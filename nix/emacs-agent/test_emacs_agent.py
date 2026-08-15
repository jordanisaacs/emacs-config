"""Tests for the dependency-free emacs-agent CLI."""

from __future__ import annotations

import argparse
import base64
import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import emacs_agent as agent


def record(status: str = "working", revision: int = 1, run_id: str = "run-1") -> dict:
    return {
        "id": "buffer-1",
        "run_id": run_id,
        "name": "worker",
        "kind": "codex",
        "status": status,
        "revision": revision,
    }


class FakeClient:
    def __init__(self, responses: list[object]) -> None:
        self.responses = list(responses)
        self.requests: list[dict] = []

    def call(self, request: dict) -> dict:
        self.requests.append(request)
        response = self.responses.pop(0)
        if isinstance(response, Exception):
            raise response
        return response


class ParserTests(unittest.TestCase):
    def test_start_keeps_forwarded_agent_args(self) -> None:
        parsed = agent.parse_arguments(
            [
                "start",
                "worker",
                "--kind",
                "codex",
                "--project",
                "demo",
                "--",
                "--model",
                "gpt-5",
            ]
        )
        self.assertEqual(parsed.agent_args, ["--model", "gpt-5"])

    def test_syntax_errors_exit_two(self) -> None:
        with self.assertRaises(SystemExit) as raised:
            agent.main(["get"])
        self.assertEqual(raised.exception.code, 2)


class TransportTests(unittest.TestCase):
    def test_transport_uses_fixed_base64_entry_point(self) -> None:
        payload = base64.b64encode(
            json.dumps({"ok": True, "result": {"type": "agents", "agents": []}}).encode()
        ).decode()
        completed = mock.Mock(returncode=0, stdout=repr(payload), stderr="")
        with mock.patch.object(agent.subprocess, "run", return_value=completed) as run:
            result = agent.EmacsClient("/nix/emacsclient").call({"op": "list"})
        self.assertEqual(result["type"], "agents")
        argv = run.call_args.args[0]
        self.assertEqual(argv[0], "/nix/emacsclient")
        self.assertIn("(emacs-agent-api-call-base64 ", argv[-1])
        encoded = argv[-1].split('"')[1]
        self.assertEqual(json.loads(base64.b64decode(encoded)), {"op": "list"})

    def test_daemon_failure_is_structured(self) -> None:
        completed = mock.Mock(returncode=1, stdout="", stderr="no socket")
        with mock.patch.object(agent.subprocess, "run", return_value=completed):
            with self.assertRaises(agent.AgentError) as raised:
                agent.EmacsClient().call({"op": "list"})
        self.assertEqual(raised.exception.code, "daemon_unavailable")


class LifecycleTests(unittest.TestCase):
    @mock.patch.object(agent.time, "sleep", return_value=None)
    def test_wait_uses_exact_requested_states(self, _sleep: mock.Mock) -> None:
        client = FakeClient(
            [
                {"type": "agent", "agent": record("idle", 1)},
                {"type": "agent", "agent": record("working", 2)},
                {"type": "agent", "agent": record("blocked", 3)},
            ]
        )
        result = agent.wait_for_states(client, "worker", ["blocked"], 1000)
        self.assertEqual(result["status"], "blocked")

    @mock.patch.object(agent.time, "sleep", return_value=None)
    def test_wait_rejects_replaced_run(self, _sleep: mock.Mock) -> None:
        client = FakeClient(
            [
                {"type": "agent", "agent": record("working", 1)},
                {"type": "agent", "agent": record("idle", 2, "run-2")},
            ]
        )
        with self.assertRaises(agent.AgentError) as raised:
            agent.wait_for_states(client, "worker", ["idle"], 1000)
        self.assertEqual(raised.exception.code, "run_replaced")

    def test_start_timeout_releases_provisional_name(self) -> None:
        client = FakeClient(
            [
                {"type": "start", "id": "buffer-1"},
                {"type": "cancel_start", "id": "buffer-1"},
            ]
        )
        args = argparse.Namespace(
            name="worker",
            kind="codex",
            project="demo",
            focus=False,
            agent_args=[],
            timeout=0,
        )
        with self.assertRaises(agent.AgentError) as raised:
            agent.command_start(client, args)
        self.assertEqual(raised.exception.code, "start_timeout")
        self.assertEqual(client.requests[-1], {"op": "cancel_start", "id": "buffer-1"})

    @mock.patch.object(agent.time, "sleep", return_value=None)
    def test_prompt_reports_stall(self, _sleep: mock.Mock) -> None:
        client = FakeClient(
            [
                {"type": "agent", "agent": record("idle", 1)},
            ]
        )
        args = argparse.Namespace(
            target="worker", text="go", wait=True, until=None, timeout=None
        )
        with mock.patch.object(agent.time, "monotonic", side_effect=[0.0, 6.0]):
            with self.assertRaises(agent.AgentError) as raised:
                agent.command_prompt(client, args)
        self.assertEqual(raised.exception.code, "prompt_not_started")


class HookTests(unittest.TestCase):
    def test_nested_merge_preserves_unrelated_and_removes_pm_entries(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            target = home / ".claude" / "settings.json"
            target.parent.mkdir(parents=True)
            target.write_text(
                json.dumps(
                    {
                        "theme": "dark",
                        "hooks": {
                            "Stop": [
                                {
                                    "matcher": "*",
                                    "hooks": [
                                        {
                                            "command": str(
                                                home / ".pm/hooks/status-reporter.py"
                                            )
                                        }
                                    ],
                                }
                            ],
                            "SessionStart": [
                                {
                                    "matcher": "*",
                                    "hooks": [{"command": "/usr/bin/custom-hook"}],
                                }
                            ],
                        },
                    }
                )
            )
            script = home / ".local/share/emacs-agent/identity-reporter.py"
            with mock.patch.object(Path, "home", return_value=home), mock.patch.dict(
                agent.os.environ, {"XDG_DATA_HOME": str(home / ".local/share")}
            ):
                agent._merge_nested_hooks(target, agent._nested_hooks(script, "claude"))
            result = json.loads(target.read_text())
            self.assertEqual(result["theme"], "dark")
            self.assertNotIn("Stop", result["hooks"])
            commands = [
                hook["command"]
                for entry in result["hooks"]["SessionStart"]
                for hook in entry["hooks"]
            ]
            self.assertIn("/usr/bin/custom-hook", commands)
            self.assertTrue(any("identity-reporter.py claude" in value for value in commands))

    def test_reporter_uses_emacs_owned_identity(self) -> None:
        self.assertIn("EMACS_AGENT_SERVER", agent.REPORTER_TEMPLATE)
        self.assertIn("EMACS_AGENT_ID", agent.REPORTER_TEMPLATE)
        self.assertIn("emacs-agent-report-identity", agent.REPORTER_TEMPLATE)
        self.assertNotIn("PM_META_", agent.REPORTER_TEMPLATE)

    def test_codex_profile_is_emacs_agent_profile(self) -> None:
        rendered = agent._codex_profile(Path("/tmp/identity-reporter.py"))
        self.assertIn("codex --profile emacs-agent", rendered)
        self.assertIn("hooks.SessionStart", rendered)
        self.assertNotIn("PreToolUse", rendered)


if __name__ == "__main__":
    unittest.main()
