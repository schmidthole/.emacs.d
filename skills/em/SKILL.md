---
name: em
description: Open a file for editing in terminal Emacs with eml when the user asks to view or edit it in their editor.
---

Use the user's explicit file or description of a file. With no target, choose
the most recently linked or referenced file in this conversation. Preserve any
line and column reference. Ask briefly only when no usable target is identifiable.

Resolve the path against the relevant project or worktree, then execute
`~/.local/bin/eml open '/absolute/path/to/file:line:column'`, omitting the position
when absent. Quote the argument safely. Add `--split` only when requested.
The launcher creates and selects a tmux window and returns immediately.
Keep the existing tmux environment so it opens beside this agent's session.

Report the target opened briefly. Do not wait for the editor to close or make
file changes as part of opening it. If launching fails, report the failure.
