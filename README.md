# taymacs

A minimal terminal-only Emacs 31+ configuration.

## installation

With Homebrew installed and `brew` on your PATH, run:

```sh
./install.sh
```

The script adds the [d12frosted/emacs-plus tap](https://github.com/d12frosted/homebrew-emacs-plus),
installs `emacs-plus@31` with default build options, Pandoc, tmux, and herdr. It also
installs Python if `python3` is unavailable.
Already installed formulae are skipped. Automatic Homebrew updates, cleanup,
and dependent checks are disabled for this run. If an installation fails,
the script stops; rerun it to resume after resolving the error.

The installer also links Pandoc into Homebrew's `bin` directory and verifies
that Emacs can find and run it. Your shell should load Homebrew's environment
as described by `brew shellenv`. This configuration adds `/opt/homebrew/bin`
and `/usr/local/bin`, when present, to Emacs's `exec-path` and `PATH` on macOS,
including when Emacs starts outside a terminal. Restart Emacs after installation.

The installer links `bin/eml` into `~/.local/bin/eml` and the three folders under
`skills/` into both `~/.agents/skills/` and `~/.claude/skills/`. Add `~/.local/bin`
to your shell's `PATH` if needed. Existing matching links are left alone; stale
links are updated. An existing file or directory at a destination is preserved
and reported as a conflict. Your `em` alias is not changed. Keep this repository
at `~/.emacs.d` so the Emacs daemon loads its configuration normally.

### terminal configs

This repository is the authoritative source for your terminal configuration.
Each `./install.sh` run copies these files, overwriting existing files or replacing
symlinks at their destinations:

| repository source | installed copy |
| --- | --- |
| `config/tmux.conf` | `~/.tmux.conf` |
| `config/herdr/config.toml` | `~/.config/herdr/config.toml` |
| `config/ghostty/config` | `~/.config/ghostty/config` |
| `config/ghostty/shaders/cursor_warp.glsl` | `~/.config/ghostty/shaders/cursor_warp.glsl` |

Edit the repository copies, then rerun `./install.sh` to apply them. Local edits
to the installed copies are overwritten, including the herdr config on every
machine where the installer runs. Other Ghostty and herdr files are left alone.
The installer ensures tmux and herdr are installed through Homebrew, using
herdr's [documented Homebrew installation](https://herdr.dev/docs/install/).
It does not check for or install Ghostty.

Reload Ghostty with `⌘ Shift ,`. For an existing tmux server, run
`tmux source-file ~/.tmux.conf`; new servers load the file automatically.

Ghostty, herdr, and Emacs follow macOS appearance: Modus Operandi in light
mode and Modus Vivendi in dark mode. Ghostty uses its built-in theme pair;
herdr follows the host terminal with matching custom light/dark palettes.
After changing herdr's config, press `F12`, then `Shift R` to reload it.
Emacs checks macOS appearance every two seconds, including terminal and daemon
sessions, and uses the native appearance hook when available. Restarting this
watcher with `M-x tay/appearance-start` is safe. macOS System Settings →
Appearance can be Light, Dark, or Auto; these configs follow that setting.

| shortcut | action |
| --- | --- |
| `⌘ d` / `⌘ Shift d` | split side by side / top and bottom |
| `⌘ h/j/k/l` | select a pane |
| `⌘ Shift h/j/k/l` | resize a pane |
| `⌘ Return` | zoom a pane |
| `⌘ t` | create a tmux window |
| `⌘ 1–9` / `⌘ [` / `⌘ ]` | select / previous / next window |
| `⌘ r` | rename a window |
| `⌘ w` / `⌘ Shift w` | close a pane / window |

`F12` remains the manual tmux prefix. Emacs Meta bindings remain available.

## emacs launcher

```sh
eml open notes.md
eml open src/main.go:42:3
eml folio docs/plan.md
eml magit .
eml --split folio docs/plan.md
```

Each command creates and selects a fresh herdr tab or tmux window named `em:notes.md`,
`folio:plan.md`, or `magit:repo`. `--split` opens a side-by-side pane instead,
with the same label as its pane title. Pane titles are visible if your tmux
configuration displays them. The launcher returns once the multiplexer launches the view;
the terminal client stays in the new window. The first launch starts an Emacs
daemon if necessary; later launches share that server and its buffers.
For Folio, the launcher supplies its Pandoc path if the existing server cannot
find `pandoc`. An explicitly customized `folio-md-pandoc-program` is preserved.
The launcher uses herdr when `HERDR_ENV=1`, including when tmux variables were
inherited from an outer terminal; otherwise it uses tmux when `TMUX` is set.
Herdr tabs open in the caller's current workspace, and splits open beside the
caller. Keep `herdr` on `PATH`; the installer installs it through Homebrew. Agents must
preserve the inherited `HERDR_*`, `TMUX`, and `TMUX_PANE` environment. Outside
either multiplexer, an interactive terminal opens the client in place.

`open` can visit a new file in an existing directory. `folio` requires an
existing file. Both accept `file:line` or `file:line:column` references; Folio
uses the source line to position its rendered view. Magit accepts a repository
directory or a file inside it and preserves the exact Git worktree. `eml magit`
defaults to the current repository. The other CLI actions require a file.

Save edits with `C-x C-s`, then close the tab, window, or pane as usual. On your
tmux Command-key setup, `⌘ Shift w` closes a window and `⌘ w` closes a pane.
The server keeps its buffers, including unsaved edits, after the client closes.
Folio and Magit's `q` keys retain their normal buffer behavior.

### agent commands

| action | codex | claude |
| --- | --- | --- |
| edit a file | `$em [file]` | `/em [file]` |
| read markdown | `$folio [file]` | `/folio [file]` |
| inspect a repository | `$magit [repo or file]` | `/magit [repo or file]` |

These skills choose an explicit target first. Without one, they use the most
recent applicable file referenced in the conversation; Magit falls back to
the agent's current repository. Descriptions such as "the earlier proposal"
also work through the agent's conversation context. The agent passes a concrete
absolute path to `eml`; the launcher does not parse chat history. Ask for a
split when you want one. Restart the agent if newly installed skills do not
appear. Codex also exposes skills through `/skills`; its built-in `/diff`
continues to show its own diff output.

See the [Codex skill documentation](https://learn.chatgpt.com/docs/build-skills)
and [Claude skill documentation](https://code.claude.com/docs/en/skills).

For feedback, use `eml open feedback.md`, write your requested changes, save,
and return to the agent with "read feedback.md and apply the requested changes".
Feedback is submitted only when you ask the agent to read it. Folio already
refreshes visible documents as agents update them on disk; unsaved source edits
take precedence.

### checks

```sh
python3 -m unittest discover -s tests -v
emacs -Q --batch -L user-lisp -l tests/folio-md-test.el -l tests/eml-test.el -l tests/tay-appearance-test.el -f ert-run-tests-batch-and-exit
```

The reader test requires Pandoc. Format Python with
`black bin/eml scripts/install-links.py tests/test_eml.py`; use Emacs's
`indent-region` for the Emacs Lisp and shell files.

## Features

- tree-sitter syntax highlighting
- Eglot language server support
- Flymake diagnostics
- Fido completion
- tree-sitter Markdown and Go modes
- Protocol Buffer mode
- `jumpa` line navigation (`C-;`, or `F6` in any terminal)
- `folio-md` Markdown reading buffers
- Magit Git interface (`C-x g`)

Magit and Protocol Buffer mode are installed automatically with `package.el` from MELPA.

Ghostty maps `Ctrl+;` to the standard `F6` escape sequence, which Emacs binds
to `jumpa`. This works through Herdr and tmux without extended keyboard
protocol negotiation. The native Emacs `C-;` binding is also retained.
After installing the terminal configs, reload Ghostty with `⌘ Shift ,`.

## magit

Press `C-x g` in a repository to open its status buffer. Magit loads on demand.
Use `TAB` to expand a diff, `s` to stage a file or hunk, `u` to unstage,
`c c` to commit, `P` for the push menu, `?` for help, and `q` to quit.
Finish a commit message with `C-c C-c`, or cancel with `C-c C-k`.

The configuration uses Magit's defaults. For a full-frame status view, an optional
setting is `magit-display-buffer-function` = `magit-display-buffer-fullframe-status-v1`.
Pair it with `magit-bury-buffer-function` = `magit-restore-window-configuration`
to restore the previous window layout with `q`. Change these through
`M-x customize-variable`. See the [Magit display documentation](https://docs.magit.vc/magit/Modes-and-Buffers.html).

## folio-md

Emacs 31 automatically prepares `user-lisp/` at startup, making `folio-md`
and `folio-md-open` available on demand. After adding or updating a package
in a running session, use `M-x prepare-user-lisp` to pick it up immediately.

Folio requires Pandoc 3.x on Emacs's `exec-path` (on macOS with Homebrew:
`brew install pandoc`). If needed, set `folio-md-pandoc-program` to its full path
with `M-x customize-group RET folio-md RET`.

Open a Markdown file and run `M-x folio-md`, or use `M-x folio-md-open` to
choose a file. The reader opens in a separate, read-only buffer.

| key | action |
| --- | --- |
| `e` | jump to the corresponding source line |
| `i` | choose a heading with completion |
| `TAB` | fold or unfold the current section |
| `o` / `RET` | follow the link at point |
| `c` | copy the code block at point without its borders |
| `g` | refresh from the source, including unsaved edits |
| `q` | quit the reading window |

Text flows to the window width like Org and Markdown modes. Visible reading
buffers check for changes every two idle seconds. Customize
`folio-md-refresh-interval` in the `folio-md` customization group; set it to
`nil` to disable polling.
