# emacs-daemon Plugin

Claude Code skill for launching `pm agent` sessions inside the user's running Emacs daemon.

## Skills

### `pm-emacs`

Activates when the user asks to start a new agent (claude / codex / cursor) in Emacs, mentions `pm agent` + emacs, or asks for a new terminal in the running Emacs daemon.

Teaches the agent:

- The `emacsclient` handoff pattern (eval an elisp form inside the running daemon)
- How to set `default-directory` so the new shell lands in the right pm project (defaults to project root, not a worktree)
- How to send `pm agent claude|codex|cursor --project <name>` into the new buffer once the shell is ready
- Pre-flight checks (`server-running-p`, terminal backend loaded)

The terminal buffer the skill creates is a ghostel buffer (`libghostty-vt`-backed, `ghostel-mode`); the skill treats this as an implementation detail.

## Triggers

This skill is the user's default for launching coding agents — every agent runs as a `pm agent` inside their Emacs daemon, so the user rarely says "in emacs" out loud.

- Any phrasing about starting / running / spawning / kicking off / firing up / opening a new agent (claude / codex / cursor)
- "new claude session", "launch codex on `<project>`", "start cursor", etc.
- Explicit Emacs / daemon / `emacsclient` mentions also still trigger
- Does **not** trigger when "agent" just refers to the assistant in conversation ("the agent fixed X") — only on explicit requests to launch a new session
