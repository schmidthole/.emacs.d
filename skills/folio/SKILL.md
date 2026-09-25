---
name: folio
description: Open a Markdown file in the terminal Emacs Folio reader with eml when the user asks to read a document in Folio.
---

Use the user's explicit file or description of a file. With no target, choose
the most recently linked or referenced Markdown file in this conversation.
Preserve any line reference. Ask briefly only when no usable target is identifiable.

Resolve the path against the relevant project or worktree, then execute
`~/.local/bin/eml folio '/absolute/path/to/file.md:line'`, omitting the position
when absent. Quote the argument safely. Add `--split` only when requested.
The launcher detects herdr (`HERDR_ENV=1`) first, then tmux, creates and selects
a tab/window, and returns immediately. Preserve the inherited `HERDR_*`, `TMUX`,
and `TMUX_PANE` environment so it opens beside this agent's session.

Report the target opened briefly. Do not wait for the reader to close or rewrite
the document. If launching fails, report the failure.
