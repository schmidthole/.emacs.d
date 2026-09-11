#!/bin/sh
set -eu

if ! command -v brew >/dev/null 2>&1; then
    printf '%s\n' 'homebrew is required; install it and add brew to your path first.' >&2
    exit 1
fi

export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_INSTALL_CLEANUP=1
export HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1

brew tap d12frosted/emacs-plus

for formula in d12frosted/emacs-plus/emacs-plus@31 pandoc tmux; do
    if brew list --formula --versions "$formula" >/dev/null 2>&1; then
        printf '%s is already installed\n' "$formula"
    else
        brew install --formula "$formula"
    fi
done

brew link pandoc
brew_prefix=$(brew --prefix)
export PATH="$brew_prefix/bin:$PATH"

if ! command -v python3 >/dev/null 2>&1; then
    brew install --formula python
fi

pandoc --version >/dev/null
emacs_prefix=$(brew --prefix d12frosted/emacs-plus/emacs-plus@31)
"$emacs_prefix/bin/emacs" -Q --batch --eval '
(unless (and (executable-find "pandoc")
             (zerop (call-process "pandoc" nil nil nil "--version")))
  (error "pandoc is unavailable to emacs"))'
printf '%s\n' 'pandoc is on the path and available to emacs; restart emacs to use folio.'

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
python3 "$repo_dir/scripts/install-links.py"
