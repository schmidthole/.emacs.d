#!/usr/bin/env python3
"""install launcher and agent skill links from this repository."""

from pathlib import Path
import sys


def install(repo, destination):
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


if __name__ == "__main__":
    try:
        install(Path(__file__).resolve().parent.parent, Path.home())
    except (OSError, ValueError) as error:
        sys.exit(str(error))
    print("eml and agent skills are installed; add ~/.local/bin to your path if needed")
