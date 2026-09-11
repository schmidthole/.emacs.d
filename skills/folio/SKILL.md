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
The launcher creates and selects a tmux window and returns immediately.
Keep the existing tmux environment so it opens beside this agent's session.

Report the target opened briefly. Do not wait for the reader to close or rewrite
the document. If launching fails, report the failure.
