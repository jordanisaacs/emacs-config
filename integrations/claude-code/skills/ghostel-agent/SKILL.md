---
name: ghostel-agent
description: |
  Launch a coding agent (claude / codex / cursor) inside the running Emacs daemon by
  spawning a new ghostel terminal buffer via emacsclient and running `pm agent` in it.
  Use when the user asks to start a new agent in Emacs, open a ghostel, or "launch claude
  in emacs" / "spawn a terminal in the daemon" — anything where the destination is the
  user's existing Emacs session, not the current shell.
  Triggers: "ghostel", "emacsclient", "emacs daemon", "new agent in emacs",
  "pm agent" + emacs, "open a terminal in emacs", "claude in ghostel".
---

# Ghostel Agent Launcher

Reference for spawning a new coding agent into the running Emacs daemon's ghostel terminal. Use when the user wants the agent to land in their existing Emacs session, not in the current shell.

## Mental Model

- **Emacs runs as a daemon** (`server-running-p` returns `t`). The user interacts with it through `emacsclient` from terminals or GUI frames.
- **Ghostel** is a `libghostty-vt`-backed terminal emulator that runs *inside* Emacs as a buffer in `ghostel-mode`. It is NOT the standalone Ghostty terminal — it's an Emacs terminal that uses the same VT engine.
- A **ghostel buffer** is an ordinary Emacs buffer hosting an interactive shell (or any program). Spawning one from outside Emacs means: `emacsclient -e '<elisp>'` evaluates a form that creates the buffer and starts its process.
- **`pm agent claude|codex|cursor`** is the canonical way to launch a coding agent in a `pm`-managed worktree. Inside a ghostel buffer it works exactly like in any other terminal.

The end-to-end flow is: emacsclient → eval an elisp form → new ghostel buffer with `default-directory` set to a pm worktree → shell starts → send `pm agent <agent> --project <name>` into the buffer.

## Critical Rules

1. **Never assume the daemon is running.** Pre-flight with `emacsclient -e '(server-running-p)'`. If it returns anything other than `t`, stop and tell the user — don't try to start the daemon yourself.
2. **Always set `default-directory` in the eval form.** Ghostel inherits cwd from the buffer's `default-directory` at spawn. If you don't set it, the shell lands wherever the daemon was started.
3. **Pass paths with a trailing slash.** `default-directory` must end in `/` — `"/home/.../carvedb"` is wrong, `"/home/.../carvedb/"` is correct.
4. **Sending input is async.** The shell starts in a background process; sending a command immediately races against shell startup. Use `run-at-time` with a small delay (≥0.5s) before `ghostel-send-string`.
5. **Always include `\n` in `ghostel-send-string`.** Without it the line sits in the prompt, unsent. Use `"<command>\n"`.
6. **Generate a fresh buffer per agent.** Use `generate-new-buffer "*ghostel:<name>*"` so concurrent agents don't collide on the default `*ghostel*` buffer.
7. **Quote elisp safely for the shell.** When passing `'(quote category)` or similar through `emacsclient -e '...'`, write `(quote category)` (the symbol form) inside single-quoted shell strings — never nest single quotes.
8. **Don't spawn an agent without explicit user intent.** Launching a Claude session is a visible, billable side effect. Confirm the project / worktree / agent before spawning unless the user already told you all three.

## Command Reference

| What | How |
|---|---|
| Check daemon | `emacsclient -e '(server-running-p)'` → expect `t` |
| Check ghostel loaded | `emacsclient -e '(featurep (quote ghostel))'` → expect `t` |
| Resolve a worktree path | `pm cd --print <project> <wt>` |
| Spawn a new ghostel + run a command | see "Canonical Spawn" below |

### Useful ghostel functions

| Function | Purpose |
|---|---|
| `ghostel` (interactive) | Pop up the default `*ghostel*` buffer (or new one with prefix arg) |
| `ghostel-project` (interactive) | Spawn a ghostel in `(project-root)` with project-prefixed name |
| `ghostel--init-buffer BUF` | Internal — turns BUF into a ghostel buffer and starts its shell. Required after `generate-new-buffer`. |
| `ghostel-send-string STR` | Send STR (raw bytes, include `\n`) to the buffer's shell |
| `ghostel-exec BUF PROGRAM &optional ARGS` | Spawn PROGRAM directly (no shell, no shell integration) in BUF |

## Canonical Spawn

The standard recipe — new ghostel buffer in a pm worktree, then run a command:

```bash
emacsclient -e '(let* ((default-directory "/home/jordan.isaacs/.projects/<proj>/<wt>/")
                       (buf (generate-new-buffer "*ghostel:<proj>*")))
                  (pop-to-buffer buf (append display-buffer--same-window-action
                                             (list (cons (quote category) (quote comint)))))
                  (ghostel--init-buffer buf)
                  (run-at-time 0.8 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (ghostel-send-string "pm agent claude --project <proj>\n")))))
                  (buffer-name buf))'
```

`emacsclient` returns the new buffer name on success (e.g. `"*ghostel:columnar-storage*"`). Echo it back to the user.

Substitute:
- `<proj>` — pm project name (e.g. `columnar-storage`)
- `<wt>` — worktree name within the project (e.g. `carvedb`)
- `claude` — swap for `codex` or `cursor` to launch a different agent

For codex with resume: append `-- --resume` to the `pm agent` command.

## Variations

### Background spawn (don't steal the user's frame)

Default for non-interactive launches. Drop `pop-to-buffer` entirely — the buffer is created, the shell starts, the command runs, but no client frame is touched. The user can switch to it later (`C-x b`, `M-x ghostel-other`, etc.).

```bash
emacsclient -e '(let* ((default-directory "/home/jordan.isaacs/.projects/<proj>/<wt>/")
                       (buf (generate-new-buffer "*ghostel:<proj>*")))
                  (ghostel--init-buffer buf)
                  (run-at-time 0.8 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (ghostel-send-string "pm agent claude --project <proj>\n")))))
                  (buffer-name buf))'
```

Notes specific to background mode:

- **Terminal sizing falls back to 24×80** until the buffer is first displayed, because `ghostel--init-buffer` uses `(selected-window)` (typically the daemon's minibuffer-only window) when no window shows the buffer. Ghostel auto-resizes via `window-size-change-functions` once you switch to it, so this self-corrects — but TUI programs that read `$LINES`/`$COLUMNS` at startup will see 24×80 first. Claude Code handles resize fine; if you need a specific size, display the buffer first.
- **`pop-to-buffer` is the only thing we drop.** Keep `generate-new-buffer`, `ghostel--init-buffer`, and the `run-at-time` send — all three are still required.
- **Echo the buffer name** back to the user so they can find it. `emacsclient` returns whatever the form returns; `(buffer-name buf)` makes that the new buffer's name.

Use foreground (canonical) spawn when the user explicitly says "open it" / "show it"; use background for "kick off an agent" / "start it in the background" / scripted launches.

### Agent on a specific branch

The branch must already be checked out in the worktree (use `pm stacker create` or plain `git checkout` first). Ghostel doesn't switch branches — it just spawns a shell wherever the worktree currently points. Verify with `cd <wt> && git branch --show-current` before spawning.

### Spawn without running pm agent (just a shell)

Drop the `run-at-time` block:

```bash
emacsclient -e '(let* ((default-directory "/path/to/dir/")
                       (buf (generate-new-buffer "*ghostel:scratch*")))
                  (pop-to-buffer buf (append display-buffer--same-window-action
                                             (list (cons (quote category) (quote comint)))))
                  (ghostel--init-buffer buf)
                  (buffer-name buf))'
```

### Spawn a program directly (no shell wrapper)

`ghostel-exec` execs PROGRAM via `/bin/sh -c` without sourcing the user's shell init. Use when you want a single program (not an interactive shell) and don't need PATH from `.zshrc`:

```bash
emacsclient -e '(let* ((default-directory "/path/to/dir/")
                       (buf (get-buffer-create "*claude:foo*")))
                  (pop-to-buffer buf)
                  (ghostel-exec buf "pm" (list "agent" "claude" "--project" "foo"))
                  (buffer-name buf))'
```

This skips shell integration (no prompt markers, no `EMACS_GHOSTEL_PATH`). Prefer the canonical spawn unless you explicitly want a no-shell environment.

### Reuse an existing ghostel buffer

Send into a known buffer (no new spawn):

```bash
emacsclient -e '(with-current-buffer "*ghostel:foo*"
                  (ghostel-send-string "pm agent claude --project foo\n"))'
```

Errors if the buffer doesn't exist or isn't in `ghostel-mode`.

## Pre-flight Checklist

Before the spawn:

```bash
# 1. Daemon up?
emacsclient -e '(server-running-p)'                # → t

# 2. Ghostel loaded?
emacsclient -e '(featurep (quote ghostel))'        # → t

# 3. Worktree path correct?
pm cd --print <proj> <wt>                          # → /home/.../<wt>

# 4. Branch where you expect?
cd $(pm cd --print <proj> <wt>) && git branch --show-current
```

If any step fails, stop and report — don't paper over with workarounds.

## Tripping Hazards

| Footgun | What happens | Right move |
|---|---|---|
| `default-directory` missing trailing `/` | Emacs error: "Search failed" / wrong cwd | Always end the path in `/` |
| Sending the command immediately (no `run-at-time`) | The string lands before the shell prompt; first chars get eaten | `run-at-time 0.8 nil ...` (or longer for slow shells) |
| Forgetting `\n` in `ghostel-send-string` | The command sits typed but unsent | Append `\n` |
| Reusing the default `*ghostel*` buffer name | New spawn collides with an existing session | `generate-new-buffer "*ghostel:<name>*"` |
| Nested single quotes in `emacsclient -e '...'` | Shell parse error | Use `(quote symbol)` form, or switch to a heredoc |
| Daemon not running | `emacsclient: can't find socket` | Tell the user; don't try `emacs --daemon` automatically |
| Ghostel not loaded | `void-function ghostel--init-buffer` | The user's emacs config didn't load ghostel — investigate; don't load it yourself |
| Spawning in a non-pm directory and then running `pm agent` | `pm agent` errors because cwd isn't a project | Set `default-directory` to a real `~/.projects/<p>/<wt>/` path |
| Foreground spawn during the user's active editing | `pop-to-buffer` splits or replaces the user's window | Use the background spawn (no `pop-to-buffer`); default to background unless the user said "open it" |
| Running `pm agent` from the project root (not a worktree) | Some `pm` commands need a worktree, not the project root | Spawn inside the worktree symlink, e.g. `~/.projects/foo/universe/`, not `~/.projects/foo/` |

## When to Hand Off

- Anything about creating / managing pm projects, worktrees, pool slots, or `pm agent` flags themselves → **`pm-workflow`** skill.
- Anything about stacked branches (`pm stacker ...`, `stack/*` branches) → **`pm-stacker-workflow`** skill.

This skill stops at "I have a pm worktree and want to launch an agent into it via the running Emacs daemon."
