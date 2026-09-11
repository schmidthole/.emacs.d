---
name: magit
description: Open a repository's Magit status view in terminal Emacs with eml when the user asks to inspect changes in Magit.
---

Use the explicit repository or file when provided. Otherwise use the repository
containing the most recently linked or referenced file in this conversation,
falling back to this agent's current repository. Preserve the exact worktree;
do not substitute the main checkout. Ask briefly if no repository is identifiable.

Execute `~/.local/bin/eml magit '/absolute/path/to/repository-or-file'` with a
safely quoted argument. Add `--split` only when requested. The launcher creates
and selects a tmux window and returns immediately. Keep the existing tmux
environment so it opens beside this agent's session.

Report the repository opened briefly. Do not wait for the editor to close,
stage changes, commit, or alter files. If launching fails, report the failure.
