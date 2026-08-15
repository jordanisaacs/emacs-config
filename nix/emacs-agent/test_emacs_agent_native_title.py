"""Tests for native coding-agent title resolution."""

from __future__ import annotations

import json
import os
import sqlite3
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import emacs_agent_native_title as titles


def _request(kind: str, session_id: str, **extra: object) -> dict[str, object]:
    return {
        "id": f"buffer-{kind}",
        "run_id": "run-1",
        "kind": kind,
        "session_id": session_id,
        **extra,
    }


class NativeTitleTests(unittest.TestCase):
    def test_codex_explicit_name_beats_database_title_and_last_record_wins(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            codex = root / ".codex"
            codex.mkdir()
            (codex / "session_index.jsonl").write_text(
                "\n".join(
                    [
                        json.dumps({"id": "s1", "thread_name": "old"}),
                        "not json",
                        json.dumps({"id": "s1", "thread_name": "renamed natively"}),
                    ]
                )
            )
            connection = sqlite3.connect(codex / "state_5.sqlite")
            connection.execute(
                "CREATE TABLE threads(id TEXT, title TEXT, first_user_message TEXT)"
            )
            connection.execute(
                "INSERT INTO threads VALUES(?,?,?)", ("s1", "generated", "prompt")
            )
            connection.commit()
            connection.close()
            with mock.patch.object(Path, "home", return_value=root), mock.patch.dict(
                os.environ, {}, clear=True
            ):
                result = titles.resolve([_request("codex", "s1")])["results"][0]
            self.assertEqual(result["title"], "renamed natively")
            self.assertEqual(result["source"], "codex-explicit")

    def test_codex_cleared_explicit_name_falls_back_to_database(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            codex = root / ".codex"
            codex.mkdir()
            (codex / "session_index.jsonl").write_text(
                json.dumps({"id": "s1", "thread_name": ""}) + "\n"
            )
            connection = sqlite3.connect(codex / "state_5.sqlite")
            connection.execute(
                "CREATE TABLE threads(id TEXT, title TEXT, first_user_message TEXT)"
            )
            connection.execute("INSERT INTO threads VALUES(?,?,?)", ("s1", "", "first prompt"))
            connection.commit()
            connection.close()
            with mock.patch.object(Path, "home", return_value=root), mock.patch.dict(
                os.environ, {}, clear=True
            ):
                result = titles.resolve([_request("codex", "s1")])["results"][0]
            self.assertEqual(
                (result["title"], result["source"]),
                ("first prompt", "codex-first-prompt"),
            )

    def test_claude_incrementally_observes_custom_rename(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "s1.jsonl"
            transcript.write_text(
                "\n".join(
                    [
                        json.dumps(
                            {
                                "type": "user",
                                "message": {"content": "initial prompt"},
                                "promptSource": "typed",
                            }
                        ),
                        json.dumps({"type": "ai-title", "aiTitle": "Generated title"}),
                    ]
                )
                + "\n"
            )
            request = _request("claude", "s1", transcript_path=str(transcript))
            first = titles.resolve([request])["results"][0]
            self.assertEqual((first["title"], first["source"]), ("Generated title", "claude-ai"))
            with transcript.open("a") as handle:
                handle.write(
                    json.dumps({"type": "custom-title", "customTitle": "My rename"})
                    + "\n"
                )
            request.update(
                title=first["title"],
                title_source=first["source"],
                title_cursor=first["cursor"],
            )
            second = titles.resolve([request])["results"][0]
            self.assertEqual((second["title"], second["source"]), ("My rename", "claude-custom"))
            self.assertGreater(second["cursor"]["offset"], first["cursor"]["offset"])

    def test_cursor_native_store_name_beats_transcript(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            session_id = "cursor-1"
            database = root / ".config/cursor/chats/workspace" / session_id / "store.db"
            database.parent.mkdir(parents=True)
            connection = sqlite3.connect(database)
            connection.execute("CREATE TABLE meta(key TEXT, value TEXT)")
            metadata = json.dumps({"name": "Refactor auth layer"}).encode().hex()
            connection.execute("INSERT INTO meta VALUES('0', ?)", (metadata,))
            connection.commit()
            connection.close()
            transcript = root / "cursor.jsonl"
            transcript.write_text(
                json.dumps(
                    {
                        "role": "user",
                        "message": {
                            "content": [
                                {"type": "text", "text": "<user_query>fallback</user_query>"}
                            ]
                        },
                    }
                )
                + "\n"
            )
            with mock.patch.object(Path, "home", return_value=root), mock.patch.dict(
                os.environ, {}, clear=True
            ):
                result = titles.resolve(
                    [_request("cursor", session_id, transcript_path=str(transcript))]
                )["results"][0]
            self.assertEqual(
                (result["title"], result["source"]),
                ("Refactor auth layer", "cursor-native"),
            )

    def test_missing_metadata_is_safe(self) -> None:
        with tempfile.TemporaryDirectory() as directory, mock.patch.object(
            Path, "home", return_value=Path(directory)
        ), mock.patch.dict(os.environ, {}, clear=True):
            result = titles.resolve([_request("codex", "missing")])["results"][0]
        self.assertIsNone(result["title"])
        self.assertIsNone(result["source"])


if __name__ == "__main__":
    unittest.main()
