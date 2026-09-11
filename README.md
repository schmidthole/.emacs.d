# taymacs

A minimal terminal-only Emacs 31+ configuration.

## installation

With Homebrew installed and `brew` on your PATH, run:

```sh
./install.sh
```

The script adds the [d12frosted/emacs-plus tap](https://github.com/d12frosted/homebrew-emacs-plus),
installs `emacs-plus@31` with default build options, then installs Pandoc.
Already installed formulae are skipped. Automatic Homebrew updates, cleanup,
and dependent checks are disabled for this run. If an installation fails,
the script stops; rerun it to resume after resolving the error.

The installer also links Pandoc into Homebrew's `bin` directory and verifies
that Emacs can find and run it. Your shell should load Homebrew's environment
as described by `brew shellenv`. This configuration adds `/opt/homebrew/bin`
and `/usr/local/bin`, when present, to Emacs's `exec-path` and `PATH` on macOS,
including when Emacs starts outside a terminal. Restart Emacs after installation.

## Features

- tree-sitter syntax highlighting
- Eglot language server support
- Flymake diagnostics
- Fido completion
- tree-sitter Markdown and Go modes
- Protocol Buffer mode
- `jumpa` line navigation
- `folio-md` Markdown reading buffers

Protocol Buffer mode is installed automatically with `package.el` from MELPA.

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

Visible reading buffers check for changes every two idle seconds. Customize
`folio-md-width` (default 88 columns) and `folio-md-refresh-interval` in the
`folio-md` customization group; set the interval to `nil` to disable polling.
