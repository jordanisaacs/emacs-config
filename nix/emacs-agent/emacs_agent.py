#!@PYTHON@
"""Command-line control for coding agents hosted by the Emacs daemon."""

from __future__ import annotations

import argparse
import ast
import base64
import json
import os
import shutil
import stat
import subprocess
import sys
import time
from collections.abc import Callable, Sequence
from pathlib import Path
from typing import Any


EMACSCLIENT = "@EMACSCLIENT@"
POLL_SECONDS = 0.15
DEFAULT_WAIT_STATES = ("idle", "done", "blocked")
VALID_STATES = ("working", "blocked", "done", "idle", "unknown")
VALID_KINDS = ("claude", "codex", "cursor")
IDENTITY_EVENTS = ("SessionStart", "UserPromptSubmit")
CURSOR_IDENTITY_EVENTS = ("sessionStart", "beforeSubmitPrompt")
CODEX_PROFILE_NAME = "emacs-agent"


class AgentError(Exception):
    """A structured error safe to return to callers."""

    def __init__(
        self,
        code: str,
        message: str,
        details: dict[str, Any] | None = None,
    ) -> None:
        super().__init__(message)
        self.code = code
        self.message = message
        self.details = details

    def envelope(self) -> dict[str, Any]:
        return {
            "ok": False,
            "error": {
                "code": self.code,
                "message": self.message,
                "details": self.details,
            },
        }


class EmacsClient:
    """Base64 JSON transport over one fixed Emacs Lisp entry point."""

    def __init__(self, executable: str = EMACSCLIENT) -> None:
        self.executable = executable

    def call(self, request: dict[str, Any]) -> dict[str, Any]:
        encoded = base64.b64encode(
            json.dumps(request, separators=(",", ":")).encode()
        ).decode("ascii")
        expression = f'(emacs-agent-api-call-base64 "{encoded}")'
        argv = [self.executable]
        server = os.environ.get("EMACS_AGENT_SERVER")
        if server:
            argv.extend(("--socket-name", server))
        argv.extend(("--eval", expression))
        try:
            completed = subprocess.run(
                argv,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=10,
                check=False,
            )
        except (OSError, subprocess.TimeoutExpired) as error:
            raise AgentError("daemon_unavailable", f"cannot contact Emacs: {error}") from error
        if completed.returncode != 0:
            message = completed.stderr.strip() or "emacsclient failed"
            raise AgentError("daemon_unavailable", message)
        try:
            printed = ast.literal_eval(completed.stdout.strip())
            envelope = json.loads(base64.b64decode(printed).decode())
        except (ValueError, SyntaxError, TypeError, json.JSONDecodeError) as error:
            raise AgentError("internal_error", "invalid response from Emacs") from error
        if not envelope.get("ok"):
            payload = envelope.get("error") or {}
            raise AgentError(
                payload.get("code", "internal_error"),
                payload.get("message", "Emacs agent operation failed"),
                payload.get("details"),
            )
        return envelope["result"]


def _deadline(timeout_ms: int | None) -> float | None:
    return None if timeout_ms is None else time.monotonic() + timeout_ms / 1000


def _remaining_expired(deadline: float | None) -> bool:
    return deadline is not None and time.monotonic() >= deadline


def _get_agent(client: EmacsClient, target: str) -> dict[str, Any]:
    return client.call({"op": "get", "target": target})["agent"]


def _same_run(agent: dict[str, Any], run_id: str) -> None:
    if agent.get("run_id") != run_id:
        raise AgentError("run_replaced", "the target now refers to a different agent run")


def _poll_agent(
    client: EmacsClient,
    target: str,
    run_id: str,
    deadline: float | None,
) -> dict[str, Any]:
    if _remaining_expired(deadline):
        raise AgentError("wait_timeout", "timed out waiting for agent")
    try:
        agent = _get_agent(client, target)
    except AgentError as error:
        if error.code == "not_found":
            raise AgentError("agent_exited", "agent exited while waiting") from error
        raise
    _same_run(agent, run_id)
    return agent


def wait_for_states(
    client: EmacsClient,
    target: str,
    states: Sequence[str],
    timeout_ms: int | None,
    *,
    initial: dict[str, Any] | None = None,
    deadline: float | None = None,
) -> dict[str, Any]:
    """Wait for TARGET's captured run to enter one of STATES."""
    agent = initial or _get_agent(client, target)
    run_id = agent["run_id"]
    stop_at = deadline if deadline is not None else _deadline(timeout_ms)
    while agent.get("status") not in states:
        time.sleep(POLL_SECONDS)
        agent = _poll_agent(client, target, run_id, stop_at)
    return agent


def command_start(client: EmacsClient, args: argparse.Namespace) -> dict[str, Any]:
    provisional = client.call(
        {
            "op": "start",
            "name": args.name,
            "kind": args.kind,
            "project": args.project,
            "focus": args.focus,
            "args": args.agent_args,
        }
    )
    target = provisional["id"]
    stop_at = _deadline(args.timeout)
    while not _remaining_expired(stop_at):
        try:
            agent = _get_agent(client, target)
            if agent.get("kind") == args.kind and agent.get("name") == args.name:
                return {"type": "agent", "agent": agent}
        except AgentError as error:
            if error.code != "not_found":
                raise
        time.sleep(POLL_SECONDS)
    try:
        client.call({"op": "cancel_start", "id": target})
    except AgentError:
        pass
    raise AgentError(
        "start_timeout",
        "timed out waiting for the agent process to start",
        {"id": target, "name": args.name},
    )


def command_prompt(client: EmacsClient, args: argparse.Namespace) -> dict[str, Any]:
    result = client.call({"op": "prompt", "target": args.target, "text": args.text})
    if not args.wait:
        return result
    initial = result["agent"]
    run_id = initial["run_id"]
    stop_at = _deadline(args.timeout)
    current = initial
    if initial.get("status") != "working":
        baseline_revision = initial.get("revision", 0)
        work_deadline = time.monotonic() + 5
        if stop_at is not None:
            work_deadline = min(work_deadline, stop_at)
        while not (
            current.get("status") == "working"
            and current.get("revision", 0) > baseline_revision
        ):
            time.sleep(POLL_SECONDS)
            try:
                current = _poll_agent(client, args.target, run_id, work_deadline)
            except AgentError as error:
                if error.code == "wait_timeout" and (
                    stop_at is None or work_deadline < stop_at
                ):
                    raise AgentError(
                        "prompt_not_started", "agent did not begin new work"
                    ) from error
                raise
        if current.get("status") != "working":
            raise AgentError("prompt_not_started", "agent did not begin new work")
    accepted = args.until or DEFAULT_WAIT_STATES
    agent = wait_for_states(
        client,
        args.target,
        accepted,
        args.timeout,
        initial=current,
        deadline=stop_at,
    )
    return {"type": "agent", "agent": agent}


def command_wait(client: EmacsClient, args: argparse.Namespace) -> dict[str, Any]:
    agent = wait_for_states(
        client,
        args.target,
        args.until or DEFAULT_WAIT_STATES,
        args.timeout,
    )
    return {"type": "agent", "agent": agent}


def command_stop(client: EmacsClient, args: argparse.Namespace) -> dict[str, Any]:
    result = client.call({"op": "stop", "target": args.target})
    agent = result["agent"]
    target = agent["id"]
    run_id = agent["run_id"]
    stop_at = time.monotonic() + 5
    while time.monotonic() < stop_at:
        time.sleep(POLL_SECONDS)
        try:
            current = _get_agent(client, target)
        except AgentError as error:
            if error.code == "not_found":
                return {"type": "stop", "agent": agent}
            raise
        _same_run(current, run_id)
    raise AgentError("stop_timeout", "agent did not stop after SIGTERM")


def _xdg_data_home() -> Path:
    value = os.environ.get("XDG_DATA_HOME")
    return Path(value) if value else Path.home() / ".local" / "share"


def _reporter_path() -> Path:
    return _xdg_data_home() / "emacs-agent" / "identity-reporter.py"


def _owned_hook_stems() -> tuple[str, ...]:
    return (
        str(Path.home() / ".pm" / "hooks" / "identity-reporter"),
        str(Path.home() / ".pm" / "hooks" / "status-reporter"),
        str(_xdg_data_home() / "emacs-agent" / "identity-reporter"),
    )


REPORTER_TEMPLATE = '''#!@PYTHON@
"""Deliver coding-agent identity to its owning Emacs daemon."""
import base64
import json
import os
import select
import subprocess
import sys


def read_stdin(timeout=2.0):
    try:
        if sys.stdin.isatty():
            return ""
        if select.select([sys.stdin], [], [], timeout)[0]:
            return sys.stdin.read()
    except Exception:
        pass
    return ""


def main():
    kind = sys.argv[1] if len(sys.argv) > 1 else ""
    event = sys.argv[2] if len(sys.argv) > 2 else ""
    server = os.environ.get("EMACS_AGENT_SERVER", "")
    agent_id = os.environ.get("EMACS_AGENT_ID", "")
    if not server or not agent_id or kind not in ("claude", "codex", "cursor"):
        return
    try:
        data = json.loads(read_stdin() or "{}")
        if not isinstance(data, dict):
            data = {}
    except Exception:
        data = {}
    if kind != "cursor" and data.get("cursor_version"):
        return
    payload = {
        "kind": kind,
        "id": agent_id,
        "session_id": data.get("session_id", ""),
        "cwd": data.get("cwd") or (data.get("workspace_roots") or [""])[0],
        "hook_event_name": data.get("hook_event_name") or event,
        "source": data.get("source", ""),
        "transcript_path": data.get("transcript_path", ""),
    }
    encoded = base64.b64encode(
        json.dumps(payload, separators=(",", ":")).encode()
    ).decode("ascii")
    expression = '(emacs-agent-report-identity "%s")' % encoded
    subprocess.run(
        ["@EMACSCLIENT@", "--socket-name", server, "--eval", expression],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        timeout=2,
        check=False,
    )


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
    sys.exit(0)
'''


def _write_reporter() -> Path:
    path = _reporter_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(REPORTER_TEMPLATE)
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


def _hook_command(script: Path, kind: str, event: str) -> str:
    return f"{script} {kind} {event}"


def _nested_hooks(script: Path, kind: str) -> dict[str, list[dict[str, Any]]]:
    return {
        event: [
            {
                "matcher": "*",
                "hooks": [
                    {
                        "type": "command",
                        "command": _hook_command(script, kind, event),
                        "timeout": 5,
                    }
                ],
            }
        ]
        for event in IDENTITY_EVENTS
    }


def _cursor_hooks(script: Path) -> dict[str, list[dict[str, Any]]]:
    return {
        event: [
            {
                "command": _hook_command(script, "cursor", event),
                "timeout": 5,
            }
        ]
        for event in CURSOR_IDENTITY_EVENTS
    }


def _command_is_owned(command: object) -> bool:
    return any(stem in str(command) for stem in _owned_hook_stems())


def _read_json_config(target: Path) -> dict[str, Any]:
    target.parent.mkdir(parents=True, exist_ok=True)
    if not target.exists():
        return {}
    backup = target.with_suffix(target.suffix + ".emacs-agent-bak")
    if not backup.exists():
        shutil.copyfile(target, backup)
    try:
        data = json.loads(target.read_text() or "{}")
    except json.JSONDecodeError as error:
        raise AgentError("invalid_hook_config", f"invalid JSON in {target}") from error
    if not isinstance(data, dict):
        raise AgentError("invalid_hook_config", f"expected an object in {target}")
    return data


def _clean_events(
    hooks: dict[str, list[dict[str, Any]]],
    predicate: Callable[[dict[str, Any]], bool],
) -> None:
    for event in list(hooks):
        entries = hooks[event]
        kept = [entry for entry in entries if not predicate(entry)]
        if kept:
            hooks[event] = kept
        else:
            hooks.pop(event)


def _merge_nested_hooks(target: Path, mapping: dict[str, list[dict[str, Any]]]) -> None:
    data = _read_json_config(target)
    hooks = data.setdefault("hooks", {})
    if not isinstance(hooks, dict):
        raise AgentError("invalid_hook_config", f"hooks must be an object in {target}")
    _clean_events(
        hooks,
        lambda entry: any(
            _command_is_owned(hook.get("command", ""))
            for hook in entry.get("hooks", [])
        ),
    )
    for event, entries in mapping.items():
        hooks[event] = hooks.get(event, []) + entries
    target.write_text(json.dumps(data, indent=2) + "\n")


def _merge_cursor_hooks(target: Path, mapping: dict[str, list[dict[str, Any]]]) -> None:
    data = _read_json_config(target)
    data.setdefault("version", 1)
    hooks = data.setdefault("hooks", {})
    if not isinstance(hooks, dict):
        raise AgentError("invalid_hook_config", f"hooks must be an object in {target}")
    _clean_events(hooks, lambda entry: _command_is_owned(entry.get("command", "")))
    for event, entries in mapping.items():
        hooks[event] = hooks.get(event, []) + entries
    target.write_text(json.dumps(data, indent=2) + "\n")


def _codex_profile(script: Path) -> str:
    lines = [
        "# Managed by `emacs-agent install-hooks` — do not edit by hand.",
        f"# Layered via `codex --profile {CODEX_PROFILE_NAME}`.",
        "",
    ]
    for event in IDENTITY_EVENTS:
        lines.extend(
            (
                f"[[hooks.{event}]]",
                f"[[hooks.{event}.hooks]]",
                'type = "command"',
                f'command = "{_hook_command(script, "codex", event)}"',
                "timeout = 5",
                "",
            )
        )
    return "\n".join(lines) + "\n"


def _write_codex_profile(script: Path) -> Path:
    target = Path.home() / ".codex" / f"{CODEX_PROFILE_NAME}.config.toml"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(_codex_profile(script))
    legacy = target.parent / "pm.config.toml"
    if legacy.exists() and "Managed by `pm agent install-hooks`" in legacy.read_text():
        legacy.unlink()
    return target


def command_install_hooks(args: argparse.Namespace) -> dict[str, Any]:
    kinds = args.kind or list(VALID_KINDS)
    script = _write_reporter()
    targets: list[str] = [str(script)]
    if "claude" in kinds:
        target = Path.home() / ".claude" / "settings.json"
        _merge_nested_hooks(target, _nested_hooks(script, "claude"))
        targets.append(str(target))
    if "cursor" in kinds:
        target = Path.home() / ".cursor" / "hooks.json"
        _merge_cursor_hooks(target, _cursor_hooks(script))
        targets.append(str(target))
    if "codex" in kinds:
        targets.append(str(_write_codex_profile(script)))
    return {"type": "hooks", "kinds": kinds, "targets": targets}


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="emacs-agent")
    subparsers = parser.add_subparsers(dest="command", required=True)

    start = subparsers.add_parser("start")
    start.add_argument("name")
    start.add_argument("--kind", required=True, choices=VALID_KINDS)
    start.add_argument("--project", required=True)
    start.add_argument("--focus", action="store_true")
    start.add_argument("--timeout", type=int, default=30_000)

    get = subparsers.add_parser("get")
    get.add_argument("target")
    subparsers.add_parser("list")

    prompt = subparsers.add_parser("prompt")
    prompt.add_argument("target")
    prompt.add_argument("text")
    prompt.add_argument("--wait", action="store_true")
    prompt.add_argument("--until", action="append", choices=VALID_STATES)
    prompt.add_argument("--timeout", type=int)

    wait = subparsers.add_parser("wait")
    wait.add_argument("target")
    wait.add_argument("--until", action="append", choices=VALID_STATES)
    wait.add_argument("--timeout", type=int)

    read = subparsers.add_parser("read")
    read.add_argument("target")
    read.add_argument("--source", choices=("recent", "visible", "detection"), default="recent")
    read.add_argument("--lines", type=int, default=80)

    keys = subparsers.add_parser("send-keys")
    keys.add_argument("target")
    keys.add_argument("keys", nargs="+")

    focus = subparsers.add_parser("focus")
    focus.add_argument("target")
    stop = subparsers.add_parser("stop")
    stop.add_argument("target")

    hooks = subparsers.add_parser("install-hooks")
    hooks.add_argument("--kind", action="append", choices=VALID_KINDS)
    return parser


def parse_arguments(argv: Sequence[str] | None = None) -> argparse.Namespace:
    """Parse CLI arguments, preserving values after the agent-argument delimiter."""
    raw = list(sys.argv[1:] if argv is None else argv)
    forwarded: list[str] = []
    if raw and raw[0] == "start" and "--" in raw:
        delimiter = raw.index("--")
        forwarded = raw[delimiter + 1 :]
        raw = raw[:delimiter]
    parser = build_parser()
    args = parser.parse_args(raw)
    if forwarded and args.command != "start":
        parser.error("only start accepts arguments after --")
    args.agent_args = forwarded if args.command == "start" else []
    return args


def execute(client: EmacsClient, args: argparse.Namespace) -> dict[str, Any]:
    if args.command == "start":
        return command_start(client, args)
    if args.command == "get":
        return client.call({"op": "get", "target": args.target})
    if args.command == "list":
        return client.call({"op": "list"})
    if args.command == "prompt":
        if not args.wait and (args.until or args.timeout is not None):
            raise AgentError("invalid_request", "--until/--timeout require --wait")
        return command_prompt(client, args)
    if args.command == "wait":
        return command_wait(client, args)
    if args.command == "read":
        return client.call(
            {
                "op": "read",
                "target": args.target,
                "source": args.source,
                "lines": args.lines,
            }
        )
    if args.command == "send-keys":
        return client.call({"op": "send_keys", "target": args.target, "keys": args.keys})
    if args.command == "focus":
        return client.call({"op": "focus", "target": args.target})
    if args.command == "stop":
        return command_stop(client, args)
    if args.command == "install-hooks":
        return command_install_hooks(args)
    raise AgentError("invalid_request", f"unknown command: {args.command}")


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parse_arguments(argv)
    if hasattr(args, "timeout") and args.timeout is not None and args.timeout < 0:
        parser.error("--timeout must be non-negative")
    try:
        result = execute(EmacsClient(), args)
    except AgentError as error:
        print(json.dumps(error.envelope(), separators=(",", ":")), file=sys.stderr)
        return 1
    print(json.dumps({"ok": True, "result": result}, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
