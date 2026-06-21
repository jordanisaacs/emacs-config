---
name: pm-emacs
description: |
  Launch a coding agent (claude / codex / cursor) for the user. This is the user's
  default — every agent runs as a `pm agent` inside their running Emacs daemon, not in
  the calling shell. Trigger on ANY request to start, run, spawn, kick off, fire up,
  or open a new agent / claude / codex / cursor session, with or without the user
  mentioning Emacs. Do NOT trigger when "agent" only refers to the assistant doing
  the talking (e.g. "the agent fixed X") — only when the user wants a new session
  launched.
  Triggers: "run an agent", "start an agent", "spawn an agent", "kick off an agent",
  "fire up claude", "new claude session", "launch codex", "start cursor on <project>",
  "open an agent in <project>", and any equivalent phrasing — Emacs need not be
  mentioned.
---

# PM agent in Emacs

Reference for launching a `pm agent` inside the user's running Emacs daemon. Use when the destination is the user's existing Emacs session, not the calling shell.

## What this skill does

The user has Emacs running as a daemon and wants a coding agent (`pm agent claude|codex|cursor`) to launch *inside* that daemon — visible alongside their other Emacs buffers — rather than in the terminal you're invoked from. This skill is the recipe for that handoff.

**Default to the project root** (`~/.projects/<proj>/`). Only target a specific worktree when the user named one, or the task is worktree-bound (`pm stacker`, branch-specific work). `pm agent` resolves the project from `--project` regardless of cwd, so worktree selection only affects where the *interactive shell* lands.

## Mental Model

- **Emacs runs as a daemon** (`server-running-p` returns `t`). The user interacts with it through `emacsclient` from terminals or GUI frames.
- The handoff happens via `emacsclient -e '<elisp>'`: an elisp form runs inside the daemon, creates a new terminal buffer with `default-directory` set to a pm project (or worktree), starts a shell, and sends the `pm agent` command into it.
- `pm agent` is the canonical launcher; inside the new buffer it works exactly as it would in any other terminal.

The end-to-end flow: emacsclient → eval an elisp form → new terminal buffer with `default-directory` set → shell starts → `pm agent <agent> --project <name>` is sent into the buffer.

### Ghostel (implementation detail)

The terminal buffer this skill creates is a **ghostel buffer** — `libghostty-vt`-backed, running in `ghostel-mode` inside Emacs. You don't need to expose this to the user; it's the in-Emacs terminal the daemon ships with. References to `ghostel-*` functions and `*ghostel:*` buffer names below are the API surface for that terminal — treat them as the implementation, not the product.

## Critical Rules

1. **One `emacsclient -e` per spawn.** `emacsclient` failing to connect *is* the daemon check (no socket → non-zero exit + stderr); the ghostel feature check belongs **inside** the same elisp form (`(unless (featurep 'ghostel) (error ...))`). Don't pre-flight with a separate `(server-running-p)` call — it adds a round-trip and races the spawn. If `emacsclient` returns non-zero or prints `*ERROR*`, stop and tell the user; don't try to start the daemon yourself.
2. **Always set `default-directory` in the eval form.** The new buffer inherits cwd from `default-directory` at spawn. If you don't set it, the shell lands wherever the daemon was started.
3. **Pass paths with a trailing slash.** `default-directory` must end in `/` — `"/home/.../carvedb"` is wrong, `"/home/.../carvedb/"` is correct.
4. **Sending input is async.** The shell starts in a background process; sending a command immediately races against shell startup. Use `run-at-time` with a small delay (≥0.5s) before `ghostel-send-string`.
5. **Always include `\n` in `ghostel-send-string`.** Without it the line sits in the prompt, unsent. Use `"<command>\n"`.
6. **Generate a fresh buffer per agent.** Use `generate-new-buffer "*ghostel:<name>*"` so concurrent agents don't collide on the default `*ghostel*` buffer.
7. **Quote elisp safely for the shell.** When passing `'(quote category)` or similar through `emacsclient -e '...'`, write `(quote category)` (the symbol form) inside single-quoted shell strings — never nest single quotes.
8. **Don't spawn an agent without explicit user intent.** Launching a Claude session is a visible, billable side effect. Confirm the project and agent before spawning. Don't ask about worktree unless the user has hinted that this is worktree-specific work — default to the project root.

## Command Reference

| What | How |
|---|---|
| Resolve a project path | `pm cd --print <project>` |
| Resolve a worktree path | `pm cd --print <project> <wt>` (only if user specified a worktree) |
| Spawn a new buffer + run a command | see "Canonical Spawn" below — daemon + feature checks are inline |

Need to discover what projects / worktrees exist? Don't guess `pm` subcommands — load the **`pm-workflow`** skill, which is the authoritative reference for `pm project ls`, `pm pool ls`, etc.

### Internal terminal API (ghostel)

These are the elisp entry points the spawn recipes call. You don't need to surface them to the user.

| Function | Purpose |
|---|---|
| `ghostel--init-buffer BUF` | Turns BUF into a terminal buffer and starts its shell. Required after `generate-new-buffer`. |
| `ghostel-send-string STR` | Send STR (raw bytes, include `\n`) to the buffer's shell |
| `ghostel-exec BUF PROGRAM &optional ARGS` | Spawn PROGRAM directly (no shell, no shell integration) in BUF |
| `ghostel` (interactive) | Pop up the default terminal buffer (or new one with prefix arg) |
| `ghostel-project` (interactive) | Spawn a terminal in `(project-root)` with project-prefixed name |

## Canonical Spawn

The standard recipe — one `emacsclient -e` call that asserts ghostel is loaded, opens a new buffer at the pm **project root**, and runs the command:

```bash
emacsclient -e '(progn
                  (unless (featurep (quote ghostel))
                    (error "ghostel not loaded"))
                  (let* ((default-directory "/home/jordan.isaacs/.projects/<proj>/")
                         (buf (generate-new-buffer "*ghostel:<proj>*")))
                    (pop-to-buffer buf (append display-buffer--same-window-action
                                               (list (cons (quote category) (quote comint)))))
                    (ghostel--init-buffer buf)
                    (run-at-time 0.8 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (ghostel-send-string "pm agent claude --project <proj>\n")))))
                    (buffer-name buf)))'
```

Three possible outcomes from this single call:

1. **Daemon down** — `emacsclient` exits non-zero with `emacsclient: can't find socket`. Stop, tell the user; don't start the daemon yourself.
2. **Ghostel not loaded** — `emacsclient` prints `*ERROR*: ghostel not loaded` to stderr and exits non-zero. The user's config didn't load it — investigate; don't load it yourself.
3. **Success** — `emacsclient` prints the new buffer name (e.g. `"*ghostel:columnar-storage*"`) on stdout. Echo it back to the user so they can find it in Emacs (`C-x b`).

Substitute:
- `<proj>` — pm project name (e.g. `columnar-storage`)
- `claude` — swap for `codex` or `cursor` to launch a different agent

For codex with resume: append `-- --resume` to the `pm agent` command.

If the user explicitly asked for a worktree, use `~/.projects/<proj>/<wt>/` for `default-directory` instead — see the "Worktree-scoped spawn" variation below.

## Variations

### Background spawn (don't steal the user's frame)

Default for non-interactive launches. Drop `pop-to-buffer` entirely — the buffer is created, the shell starts, the command runs, but no client frame is touched. The user can switch to it later (`C-x b`, etc.).

```bash
emacsclient -e '(progn
                  (unless (featurep (quote ghostel))
                    (error "ghostel not loaded"))
                  (let* ((default-directory "/home/jordan.isaacs/.projects/<proj>/")
                         (buf (generate-new-buffer "*ghostel:<proj>*")))
                    (ghostel--init-buffer buf)
                    (run-at-time 0.8 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (ghostel-send-string "pm agent claude --project <proj>\n")))))
                    (buffer-name buf)))'
```

Notes specific to background mode:

- **Terminal sizing falls back to 24×80** until the buffer is first displayed, because `ghostel--init-buffer` uses `(selected-window)` (typically the daemon's minibuffer-only window) when no window shows the buffer. Auto-resize via `window-size-change-functions` corrects this once the user switches to it — but TUI programs that read `$LINES`/`$COLUMNS` at startup will see 24×80 first. Claude Code handles resize fine; if you need a specific size, display the buffer first.
- **`pop-to-buffer` is the only thing we drop.** Keep `generate-new-buffer`, `ghostel--init-buffer`, and the `run-at-time` send — all three are still required.
- **Echo the buffer name** back to the user so they can find it. `emacsclient` returns whatever the form returns; `(buffer-name buf)` makes that the new buffer's name.

Use foreground (canonical) spawn when the user explicitly says "open it" / "show it"; use background for "kick off an agent" / "start it in the background" / scripted launches.

### Worktree-scoped spawn

Use when the user explicitly named a worktree, or when the work is worktree-bound (`pm stacker`, branch-specific tasks, anything that calls `git` against a particular tree). Set `default-directory` to the worktree symlink and name the buffer after the worktree so concurrent worktree spawns don't collide:

```bash
emacsclient -e '(progn
                  (unless (featurep (quote ghostel))
                    (error "ghostel not loaded"))
                  (let* ((default-directory "/home/jordan.isaacs/.projects/<proj>/<wt>/")
                         (buf (generate-new-buffer "*ghostel:<proj>:<wt>*")))
                    (ghostel--init-buffer buf)
                    (run-at-time 0.8 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (ghostel-send-string "pm agent claude --project <proj>\n")))))
                    (buffer-name buf)))'
```

Note: `pm agent` itself runs in the project dir regardless of cwd — the worktree path only affects where the *interactive shell* lands. That's still useful when the user wants to run `git` / `pm stacker` commands in the same buffer alongside the agent.

### Agent on a specific branch

The branch must already be checked out in the worktree (use `pm stacker create` or plain `git checkout` first). The terminal doesn't switch branches — it just spawns a shell wherever the worktree currently points. Verify with `cd <wt> && git branch --show-current` before spawning. Use the worktree-scoped spawn above so the shell starts inside the right tree.

### Spawn without running pm agent (just a shell)

Drop the `run-at-time` block:

```bash
emacsclient -e '(progn
                  (unless (featurep (quote ghostel))
                    (error "ghostel not loaded"))
                  (let* ((default-directory "/path/to/dir/")
                         (buf (generate-new-buffer "*ghostel:scratch*")))
                    (pop-to-buffer buf (append display-buffer--same-window-action
                                               (list (cons (quote category) (quote comint)))))
                    (ghostel--init-buffer buf)
                    (buffer-name buf)))'
```

### Spawn a program directly (no shell wrapper)

`ghostel-exec` execs PROGRAM via `/bin/sh -c` without sourcing the user's shell init. Use when you want a single program (not an interactive shell) and don't need PATH from `.zshrc`:

```bash
emacsclient -e '(progn
                  (unless (featurep (quote ghostel))
                    (error "ghostel not loaded"))
                  (let* ((default-directory "/path/to/dir/")
                         (buf (get-buffer-create "*claude:foo*")))
                    (pop-to-buffer buf)
                    (ghostel-exec buf "pm" (list "agent" "claude" "--project" "foo"))
                    (buffer-name buf)))'
```

This skips shell integration (no prompt markers, no `EMACS_GHOSTEL_PATH`). Prefer the canonical spawn unless you explicitly want a no-shell environment.

### Reuse an existing buffer

Send into a known buffer (no new spawn). No feature check needed — if the buffer exists, ghostel is loaded:

```bash
emacsclient -e '(with-current-buffer "*ghostel:foo*"
                  (ghostel-send-string "pm agent claude --project foo\n"))'
```

Errors if the buffer doesn't exist or isn't a terminal buffer.

## Pre-flight Checklist

The spawn form does the daemon + ghostel checks itself — the only things you have to verify outside it are paths:

```bash
# Project path correct?
pm cd --print <proj>                               # → /home/.../<proj>

# Only if the user named a specific worktree:
pm cd --print <proj> <wt>                          # → /home/.../<wt>

# Branch where you expect (worktree-bound work only)?
cd $(pm cd --print <proj> <wt>) && git branch --show-current
```

Then run the spawn — `emacsclient` itself returns non-zero if the daemon is down, and the inline `(unless (featurep 'ghostel) ...)` errors out if the backend isn't loaded. If either fails, stop and report; don't paper over with workarounds.

## Tripping Hazards

| Footgun | What happens | Right move |
|---|---|---|
| `default-directory` missing trailing `/` | Emacs error: "Search failed" / wrong cwd | Always end the path in `/` |
| Sending the command immediately (no `run-at-time`) | The string lands before the shell prompt; first chars get eaten | `run-at-time 0.8 nil ...` (or longer for slow shells) |
| Forgetting `\n` in `ghostel-send-string` | The command sits typed but unsent | Append `\n` |
| Reusing the default `*ghostel*` buffer name | New spawn collides with an existing session | `generate-new-buffer "*ghostel:<name>*"` |
| Nested single quotes in `emacsclient -e '...'` | Shell parse error | Use `(quote symbol)` form, or switch to a heredoc |
| Daemon not running | `emacsclient: can't find socket` | Tell the user; don't try `emacs --daemon` automatically |
| Terminal backend not loaded | The inline `(unless (featurep 'ghostel) ...)` fires `*ERROR*: ghostel not loaded` on stderr (or `void-function ghostel--init-buffer` if you removed the guard) | The user's Emacs config didn't load it — investigate; don't load it yourself |
| Spawning in a non-pm directory and then running `pm agent` | `pm agent` errors because cwd isn't a project | Set `default-directory` to a real `~/.projects/<p>/` path |
| Foreground spawn during the user's active editing | `pop-to-buffer` splits or replaces the user's window | Use the background spawn (no `pop-to-buffer`); default to background unless the user said "open it" |
| Defaulting to a worktree when the user just named a project | Picks an arbitrary worktree, may surprise the user | Default `default-directory` to `~/.projects/<proj>/`. Only descend into a worktree when the user named one or the task is worktree-bound (stacker, branch work) |

## When to Hand Off

- Anything about creating / managing pm projects, worktrees, pool slots, `pm agent` flags themselves, or just *discovering* what projects exist → **`pm-workflow`** skill. Load it before running any `pm` subcommand you're unsure of — don't guess (e.g. it's `pm project ls`, not `list`).
- Anything about stacked branches (`pm stacker ...`, `stack/*` branches) → **`pm-stacker-workflow`** skill.

This skill stops at "I have a pm project (or, when explicitly asked, a specific worktree) and want to launch an agent into it via the running Emacs daemon."
