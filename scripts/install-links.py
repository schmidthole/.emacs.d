#!/usr/bin/env python3
"""install launcher links, agent skills, and terminal configs from this repository."""

from pathlib import Path
import shutil
import sys


def install(repo, destination):
    configs = [
        (repo / "config" / "tmux.conf", destination / ".tmux.conf"),
        (
            repo / "config" / "ghostty" / "config",
            destination / ".config" / "ghostty" / "config",
        ),
        (
            repo / "config" / "ghostty" / "shaders" / "cursor_warp.glsl",
            destination / ".config" / "ghostty" / "shaders" / "cursor_warp.glsl",
        ),
    ]
    for source, target in configs:
        if not source.is_file():
            raise ValueError(f"terminal config source is missing: {source}")
        if target.is_dir() and not target.is_symlink():
            raise ValueError(f"terminal config destination is a directory: {target}")
    links = [(repo / "bin" / "eml", destination / ".local" / "bin" / "eml")]
    for agent in (".agents", ".claude"):
        for name in ("em", "folio", "magit"):
            links.append(
                (repo / "skills" / name, destination / agent / "skills" / name)
            )
    for source, target in links:
        if not source.exists():
            raise ValueError("launcher installation source is missing")
        if target.exists() and not target.is_symlink():
            raise ValueError(f"existing installation path is not a symlink: {target}")
    for source, target in links:
        target.parent.mkdir(parents=True, exist_ok=True)
        if target.is_symlink():
            if target.resolve() == source.resolve():
                continue
            target.unlink()
        target.symlink_to(source)
    for source, target in configs:
        target.parent.mkdir(parents=True, exist_ok=True)
        if target.is_symlink():
            target.unlink()
        shutil.copyfile(source, target)


if __name__ == "__main__":
    try:
        install(Path(__file__).resolve().parent.parent, Path.home())
    except (OSError, ValueError) as error:
        sys.exit(str(error))
    print("eml and agent skills are installed; add ~/.local/bin to your path if needed")
    print("ghostty and tmux configs are installed from the repository")
