# emacs-daemon Plugin

Claude Code skill for starting and controlling coding-agent sessions inside the user's running Emacs daemon.

## Skills

### `emacs-agent`

Activates when the user asks to start, prompt, inspect, focus, wait for, or stop a Claude, Codex, or Cursor agent in Emacs.

Teaches the agent:

- The `emacs-agent` CLI for low-level live-agent operations
- Named starts in PM projects, with optional vendor arguments
- Bounded prompt/wait workflows and structured state inspection
- Reading terminal output, sending keys, focusing, and stopping agents

The CLI calls one fixed Elisp API in the daemon. Ghostel and raw `emacsclient` forms remain implementation details.

## Triggers

This skill is the user's default for launching coding agents — every agent runs as a `pm agent` inside their Emacs daemon, so the user rarely says "in emacs" out loud.

- Any phrasing about starting / running / spawning / kicking off / firing up / opening a new agent (claude / codex / cursor)
- "new claude session", "launch codex on `<project>`", "start cursor", etc.
- Explicit Emacs / daemon / `emacsclient` mentions also still trigger
- Does **not** trigger when "agent" just refers to the assistant in conversation ("the agent fixed X") — only on explicit requests to launch a new session
