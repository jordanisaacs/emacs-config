#!@PYTHON@
"""Resolve live coding-agent titles from vendor-owned local metadata.

The process is intentionally one-shot: Emacs batches its live sessions into a
single JSON request on stdin and receives one JSON response on stdout.  There
is no daemon, socket, or dependency on project-manager.
"""

from __future__ import annotations

import json
import os
import re
import sqlite3
import sys
from pathlib import Path
from typing import Any


MAX_TITLE_LENGTH = 200
_CURSOR_QUERY_RE = re.compile(r"<user_query>\s*(.*?)\s*</user_query>", re.DOTALL)
_CURSOR_PATH_RE = re.compile(r"[/.]+")
_SYSTEM_USER_PREFIXES = (
    "<local-command-caveat>",
    "<local-command-stdout>",
    "<local-command-stderr>",
    "<command-message>",
    "<command-name>",
    "<command-stdout>",
    "<command-stderr>",
    "<system-reminder>",
    "<bash-input>",
    "<bash-stdout>",
    "<bash-stderr>",
    "<environment_context>",
    "<user_instructions>",
)


def _normalize(value: object) -> str | None:
    if not isinstance(value, str):
        return None
    cleaned = "".join(ch for ch in value if ch >= " " and ch != "\x7f")
    collapsed = " ".join(cleaned.split())
    return collapsed[:MAX_TITLE_LENGTH] or None


def _readonly_connection(path: Path) -> sqlite3.Connection:
    return sqlite3.connect(f"file:{path}?mode=ro", uri=True, timeout=0.1)


def _codex_home() -> Path:
    configured = os.environ.get("CODEX_HOME")
    return Path(configured).expanduser() if configured else Path.home() / ".codex"


def _codex_explicit_names(path: Path, wanted: set[str]) -> dict[str, str | None]:
    names: dict[str, str | None] = {}
    try:
        with path.open(encoding="utf-8", errors="replace") as handle:
            for raw in handle:
                try:
                    row = json.loads(raw)
                except json.JSONDecodeError:
                    continue
                if not isinstance(row, dict) or row.get("id") not in wanted:
                    continue
                # The index is append-only.  A later empty value explicitly
                # clears an older rename and falls through to the native DB.
                names[str(row["id"])] = _normalize(row.get("thread_name"))
    except OSError:
        pass
    return names


def _codex_database_titles(path: Path, wanted: set[str]) -> dict[str, tuple[str, str]]:
    if not wanted or not path.is_file():
        return {}
    placeholders = ",".join("?" for _ in wanted)
    try:
        connection = _readonly_connection(path)
        try:
            rows = connection.execute(
                f"SELECT id, title, first_user_message FROM threads WHERE id IN ({placeholders})",
                tuple(wanted),
            ).fetchall()
        finally:
            connection.close()
    except sqlite3.Error:
        return {}
    titles: dict[str, tuple[str, str]] = {}
    for session_id, title, first_user_message in rows:
        if native := _normalize(title):
            titles[str(session_id)] = (native, "codex-title")
        elif fallback := _normalize(first_user_message):
            titles[str(session_id)] = (fallback, "codex-first-prompt")
    return titles


def _resolve_codex(requests: list[dict[str, Any]]) -> list[dict[str, Any]]:
    wanted = {str(request["session_id"]) for request in requests}
    root = _codex_home()
    explicit = _codex_explicit_names(root / "session_index.jsonl", wanted)
    database = _codex_database_titles(root / "state_5.sqlite", wanted)
    results: list[dict[str, Any]] = []
    for request in requests:
        session_id = str(request["session_id"])
        title = explicit.get(session_id)
        source = "codex-explicit"
        if title is None:
            title, source = database.get(session_id, (None, None))
        results.append(_result(request, title, source))
    return results


def _flatten_content(content: object, text_types: tuple[str, ...]) -> str:
    if isinstance(content, str):
        return content
    if not isinstance(content, list):
        return ""
    parts: list[str] = []
    for block in content:
        if not isinstance(block, dict) or block.get("type") not in text_types:
            continue
        if isinstance(block.get("text"), str):
            parts.append(str(block["text"]))
    return "\n".join(parts)


def _claude_path(request: dict[str, Any]) -> Path | None:
    reported = request.get("transcript_path")
    if isinstance(reported, str) and reported:
        return Path(reported).expanduser()
    cwd = request.get("cwd")
    session_id = request.get("session_id")
    if not isinstance(cwd, str) or not isinstance(session_id, str):
        return None
    configured = os.environ.get("CLAUDE_CONFIG_DIR")
    root = Path(configured).expanduser() if configured else Path.home() / ".claude"
    encoded = cwd.replace("/", "-").replace(".", "-")
    return root / "projects" / encoded / f"{session_id}.jsonl"


def _claude_candidate(row: dict[str, Any]) -> tuple[str, str] | None:
    row_type = row.get("type")
    if row_type == "custom-title":
        title = _normalize(row.get("customTitle"))
        return (title, "claude-custom") if title else None
    if row_type == "ai-title":
        title = _normalize(row.get("aiTitle"))
        return (title, "claude-ai") if title else None
    if row_type == "user":
        if (
            row.get("isMeta")
            or row.get("isSidechain")
            or row.get("promptSource", "typed") != "typed"
        ):
            return None
        message = row.get("message")
        content = message.get("content") if isinstance(message, dict) else None
        text = _flatten_content(content, ("text",)).lstrip()
        if text and not text.startswith(_SYSTEM_USER_PREFIXES):
            title = _normalize(text)
            return (title, "claude-first-prompt") if title else None
        return None
    message = row.get("message")
    if isinstance(message, dict) and message.get("model") == "<synthetic>":
        text = _flatten_content(message.get("content"), ("text",)).lstrip()
        if text.startswith("Summary:"):
            title = _normalize(text[len("Summary:") :])
            return (title, "claude-summary") if title else None
    return None


_CLAUDE_RANK = {
    "claude-first-prompt": 1,
    "claude-summary": 2,
    "claude-ai": 3,
    "claude-custom": 4,
}


def _scan_claude(
    path: Path,
    start: int,
    current_title: str | None,
    current_source: str | None,
) -> tuple[str | None, str | None, dict[str, Any]]:
    stat = path.stat()
    with path.open("rb") as handle:
        handle.seek(start)
        raw = handle.read()
        end = handle.tell()
    title = current_title
    source = current_source
    first_prompt_seen = source == "claude-first-prompt"
    for line in raw.decode("utf-8", errors="replace").splitlines():
        try:
            row = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(row, dict) or not (candidate := _claude_candidate(row)):
            continue
        next_title, next_source = candidate
        if next_source == "claude-first-prompt" and first_prompt_seen:
            continue
        if next_source == "claude-first-prompt":
            first_prompt_seen = True
        if _CLAUDE_RANK[next_source] >= _CLAUDE_RANK.get(source or "", 0):
            title, source = next_title, next_source
    cursor = {
        "path": str(path),
        "device": stat.st_dev,
        "inode": stat.st_ino,
        "offset": end,
    }
    return title, source, cursor


def _resolve_claude(request: dict[str, Any]) -> dict[str, Any]:
    path = _claude_path(request)
    if path is None or not path.is_file():
        return _result(request, None, None)
    cursor = request.get("title_cursor")
    current_title = _normalize(request.get("title"))
    current_source = request.get("title_source")
    try:
        stat = path.stat()
        valid_cursor = (
            isinstance(cursor, dict)
            and cursor.get("path") == str(path)
            and cursor.get("device") == stat.st_dev
            and cursor.get("inode") == stat.st_ino
            and isinstance(cursor.get("offset"), int)
            and 0 <= int(cursor["offset"]) <= stat.st_size
        )
        start = int(cursor["offset"]) if valid_cursor else 0
        if not valid_cursor:
            current_title = None
            current_source = None
        title, source, next_cursor = _scan_claude(path, start, current_title, current_source)
        return _result(request, title, source, next_cursor)
    except OSError:
        return _result(request, None, None)


def _cursor_config_root() -> Path:
    configured = os.environ.get("XDG_CONFIG_HOME")
    return (Path(configured).expanduser() if configured else Path.home() / ".config") / "cursor"


def _cursor_store(session_id: str) -> Path | None:
    for path in (_cursor_config_root() / "chats").glob(f"*/{session_id}/store.db"):
        if path.is_file():
            return path
    return None


def _cursor_store_title(path: Path | None) -> tuple[str | None, bool]:
    """Return (title, readable); unreadable stores must not erase live titles."""
    if path is None:
        return None, True
    try:
        connection = _readonly_connection(path)
        try:
            row = connection.execute("SELECT value FROM meta WHERE key='0'").fetchone()
        finally:
            connection.close()
        if not row:
            return None, True
        metadata = json.loads(bytes.fromhex(str(row[0])).decode("utf-8"))
        title = _normalize(metadata.get("name")) if isinstance(metadata, dict) else None
        return title, True
    except (OSError, ValueError, UnicodeDecodeError, json.JSONDecodeError, sqlite3.Error):
        return None, False


def _cursor_transcript(request: dict[str, Any]) -> Path | None:
    reported = request.get("transcript_path")
    if isinstance(reported, str) and reported:
        return Path(reported).expanduser()
    cwd = request.get("cwd")
    session_id = request.get("session_id")
    if not isinstance(cwd, str) or not isinstance(session_id, str):
        return None
    encoded = _CURSOR_PATH_RE.sub("-", cwd).lstrip("-")
    return (
        Path.home()
        / ".cursor"
        / "projects"
        / encoded
        / "agent-transcripts"
        / session_id
        / f"{session_id}.jsonl"
    )


def _cursor_first_prompt(path: Path | None) -> str | None:
    if path is None:
        return None
    try:
        with path.open(encoding="utf-8", errors="replace") as handle:
            for raw in handle:
                try:
                    row = json.loads(raw)
                except json.JSONDecodeError:
                    continue
                if not isinstance(row, dict) or row.get("role") != "user":
                    continue
                message = row.get("message")
                content = message.get("content") if isinstance(message, dict) else None
                text = _flatten_content(content, ("text",))
                if not text:
                    continue
                match = _CURSOR_QUERY_RE.search(text)
                if title := _normalize(match.group(1) if match else text):
                    return title
    except OSError:
        pass
    return None


def _resolve_cursor(request: dict[str, Any]) -> dict[str, Any]:
    session_id = str(request["session_id"])
    title, readable = _cursor_store_title(_cursor_store(session_id))
    if title:
        return _result(request, title, "cursor-native")
    if not readable:
        return _result(request, None, None)
    title = _cursor_first_prompt(_cursor_transcript(request))
    return _result(request, title, "cursor-first-prompt" if title else None)


def _result(
    request: dict[str, Any],
    title: str | None,
    source: str | None,
    cursor: dict[str, Any] | None = None,
) -> dict[str, Any]:
    result: dict[str, Any] = {
        "id": request["id"],
        "run_id": request.get("run_id"),
        "kind": request["kind"],
        "session_id": request["session_id"],
        "title": title,
        "source": source,
    }
    if cursor is not None:
        result["cursor"] = cursor
    return result


def resolve(requests: object) -> dict[str, list[dict[str, Any]]]:
    if not isinstance(requests, list):
        return {"results": []}
    valid = [
        request
        for request in requests
        if isinstance(request, dict)
        and isinstance(request.get("id"), str)
        and request.get("kind") in ("claude", "codex", "cursor")
        and isinstance(request.get("session_id"), str)
        and request.get("session_id")
    ]
    codex = [request for request in valid if request["kind"] == "codex"]
    results = _resolve_codex(codex) if codex else []
    results.extend(_resolve_claude(request) for request in valid if request["kind"] == "claude")
    results.extend(_resolve_cursor(request) for request in valid if request["kind"] == "cursor")
    return {"results": results}


def main() -> int:
    try:
        payload = json.loads(sys.stdin.readline())
        print(json.dumps(resolve(payload), separators=(",", ":")))
    except Exception as error:  # Keep the Emacs scheduler alive on corrupt native state.
        print(json.dumps({"results": [], "error": str(error)}, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
