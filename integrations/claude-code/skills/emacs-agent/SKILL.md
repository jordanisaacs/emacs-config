---
name: emacs-agent
description: Start and control Claude, Codex, or Cursor agents inside the user's running Emacs daemon. Use when asked to launch, prompt, inspect, focus, wait for, or stop a coding agent; when asked for another agent session; or when Emacs, Ghostel, pm agent, or emacsclient is mentioned in connection with live agent management.
---

# Emacs agent operations

Use `emacs-agent` for all live agent operations. It is a safe CLI over the
running Emacs daemon's Ghostel buffers. Do not call `emacsclient`, evaluate
Elisp, or invoke Ghostel functions directly during normal operation.

## Start an agent

Choose a short unique lowercase name and the user's PM project name:

```bash
emacs-agent start reviewer --kind codex --project my-project
emacs-agent start implementer --kind claude --project my-project --focus
```

Pass vendor arguments only after `--`:

```bash
emacs-agent start reviewer --kind codex --project my-project -- --model gpt-5.6
```

Codex automatically uses the `emacs-agent` profile unless another profile is
explicitly supplied. Names must match `[a-z][a-z0-9_-]{0,31}`.

## Prompt and wait

Prefer one bounded command when assigning work:

```bash
emacs-agent prompt reviewer "Review the current changes" --wait --timeout 600000
```

Use explicit accepted states when needed:

```bash
emacs-agent wait reviewer --until done --until blocked --timeout 600000
```

Always provide `--timeout` for `wait` and `prompt --wait`. A blocked result
requires user or caller action; inspect it instead of repeatedly waiting.

## Inspect and interact

```bash
emacs-agent list
emacs-agent get reviewer
emacs-agent read reviewer --source recent --lines 120
emacs-agent read reviewer --source visible
emacs-agent send-keys reviewer esc
emacs-agent send-keys reviewer ctrl+c
emacs-agent focus reviewer
emacs-agent stop reviewer
```

Commands emit JSON. Address manually started agents by the `id` returned from
`list`; API-started agents can also be addressed by their unique name.

## Diagnostic escape hatch

Use raw `emacsclient` only when an existing `emacs-agent` operation cannot
complete the task, such as when a failed process has already disappeared from
the live registry or `send-keys` cannot express a required key. First try the
structured command and record its failure. Then evaluate the smallest possible
Elisp expression, scope it to the exact known Ghostel buffer, and prefer
read-only inspection. Do not use this exception to replace routine
`emacs-agent` operations or manipulate unrelated buffers.

PM remains the source for project names and durable history/resume. Do not use
PM to infer live terminal state or to manipulate live buffers.
