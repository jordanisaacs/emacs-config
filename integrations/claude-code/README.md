# emacs-daemon Plugin

Claude Code skill for spawning coding agents into the running Emacs daemon's ghostel terminal.

## Skills

### `ghostel-agent`

Activates when the user asks to start a new agent (claude / codex / cursor) in Emacs, mentions ghostel + pm agent, or asks for a new terminal in the running Emacs daemon.

Teaches the agent:

- The emacsclient + ghostel spawn pattern (ghostel is a libghostty-vt terminal that runs inside Emacs)
- How to set `default-directory` so the new shell lands in the right pm worktree
- How to send a command into the new buffer once the shell is ready
- How to combine the above with `pm agent claude|codex|cursor --project <name>`
- Pre-flight checks (`server-running-p`, ghostel loaded)

## Triggers

- "new claude/codex agent in emacs", "open a ghostel", "spawn a terminal in emacs"
- "pm agent" combined with emacs / ghostel
- Any request to launch an interactive agent against an existing pm project worktree
