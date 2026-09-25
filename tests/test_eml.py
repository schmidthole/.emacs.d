"""test launcher targets, process boundaries, and installation."""

from importlib.machinery import SourceFileLoader
from importlib.util import module_from_spec, spec_from_loader
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest
from unittest.mock import patch


REPO = Path(__file__).resolve().parent.parent


def load_module(name, path):
    spec = spec_from_loader(name, SourceFileLoader(name, str(path)))
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


eml = load_module("eml", REPO / "bin" / "eml")
installer = load_module("install_links", REPO / "scripts" / "install-links.py")


class TargetTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)

    def test_file_position_and_new_file(self):
        path = self.root / "new file.md"
        self.assertEqual(eml.resolve_target("open", f"{path}:42:3"), (path, 42, 3))
        self.assertFalse(path.exists())

    def test_existing_numeric_filename_takes_precedence(self):
        path = self.root / "notes:42"
        path.touch()
        self.assertEqual(eml.resolve_target("open", str(path)), (path, 1, 1))

    def test_folio_requires_existing_file(self):
        with self.assertRaisesRegex(ValueError, "existing file"):
            eml.resolve_target("folio", str(self.root / "missing.md"))

    def test_missing_parent_and_directory_are_rejected(self):
        for path in (self.root, self.root / "missing" / "file"):
            with self.subTest(path=path), self.assertRaises(ValueError):
                eml.resolve_target("open", str(path))

    def test_missing_repository_does_not_open_parent(self):
        with self.assertRaisesRegex(ValueError, "does not exist"):
            eml.resolve_target("magit", str(self.root / "missing"))

    def test_magit_preserves_worktree(self):
        main = self.root / "main"
        worktree = self.root / "work tree"
        subprocess.run(["git", "init", "-q", str(main)], check=True)
        subprocess.run(
            [
                "git",
                "-C",
                str(main),
                "-c",
                "user.name=test",
                "-c",
                "user.email=test@example.com",
                "-c",
                "commit.gpgsign=false",
                "commit",
                "-q",
                "--allow-empty",
                "-m",
                "initial",
            ],
            check=True,
        )
        subprocess.run(
            [
                "git",
                "-C",
                str(main),
                "worktree",
                "add",
                "-q",
                "-b",
                "test",
                str(worktree),
            ],
            check=True,
        )
        path = worktree / "notes.md"
        path.touch()
        target, _, _ = eml.resolve_target("magit", f"{path}:2")
        self.assertEqual(target.resolve(), worktree.resolve())

    def test_lisp_string_roundtrip(self):
        value = 'quotes " backslash \\ newline\n unicode λ $(touch nope)'
        result = subprocess.check_output(
            ["emacs", "-Q", "--batch", "--eval", f"(princ {eml.lisp_string(value)})"],
            text=True,
        )
        self.assertEqual(result, value)


class LaunchTests(unittest.TestCase):
    def setUp(self):
        environment = patch.dict(os.environ, {"HERDR_ENV": "", "TMUX": ""})
        environment.start()
        self.addCleanup(environment.stop)
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.path = Path(self.directory.name) / 'a "quote" $(touch nope).md'
        self.path.touch()

    @patch.dict(os.environ, {"TMUX": "/tmp/socket,123,1", "TMUX_PANE": "%7"})
    @patch.object(eml.shutil, "which", return_value="/tools/emacsclient")
    @patch.object(eml.subprocess, "check_output", return_value="$3\n")
    @patch.object(eml.subprocess, "run")
    def test_window_targets_origin_session_without_shell(self, run, output, which):
        eml.launch("open", str(self.path))
        self.assertEqual(
            output.call_args.args[0],
            ["tmux", "display-message", "-p", "-t", "%7", "#{session_id}"],
        )
        command = run.call_args.args[0]
        self.assertEqual(command[:4], ["tmux", "new-window", "-t", "$3:"])
        self.assertIn("/tools/emacsclient", command)
        self.assertIn(eml.lisp_string(self.path), command[-1])
        self.assertNotIn("shell", run.call_args.kwargs)
        self.assertNotIn("-n", command[command.index("/tools/emacsclient") :])

    @patch.dict(os.environ, {"TMUX": "/tmp/socket,123,1", "TMUX_PANE": "%7"})
    @patch.object(eml.shutil, "which", return_value="/tools/emacsclient")
    @patch.object(eml.subprocess, "check_output", return_value="%8\n")
    @patch.object(eml.subprocess, "run")
    def test_split_targets_origin_pane(self, run, output, which):
        eml.launch("folio", str(self.path), split=True)
        self.assertEqual(
            output.call_args.args[0][:5], ["tmux", "split-window", "-h", "-t", "%7"]
        )
        self.assertEqual(
            run.call_args.args[0][:5], ["tmux", "select-pane", "-t", "%8", "-T"]
        )

    @patch.dict(os.environ, {"TMUX": ""})
    @patch.object(eml.shutil, "which", return_value="/tools/emacsclient")
    @patch.object(eml.sys.stdin, "isatty", return_value=True)
    @patch.object(eml.os, "execv")
    def test_plain_terminal_executes_client(self, execute, isatty, which):
        eml.launch("open", str(self.path))
        self.assertEqual(execute.call_args.args[0], "/tools/emacsclient")
        self.assertEqual(execute.call_args.args[1][1:4], ["-t", "-a", ""])

    @patch.dict(os.environ, {"TMUX": ""})
    @patch.object(eml.shutil, "which", return_value="/tools/emacsclient")
    @patch.object(eml.sys.stdin, "isatty", return_value=False)
    def test_agent_outside_multiplexers_gets_actionable_error(self, isatty, which):
        with self.assertRaisesRegex(ValueError, "inside herdr, tmux"):
            eml.launch("open", str(self.path))

    @patch.dict(
        os.environ,
        {
            "HERDR_ENV": "1",
            "HERDR_PANE_ID": "w1:p1",
            "HERDR_WORKSPACE_ID": "w1",
            "TMUX": "inherited",
        },
    )
    @patch.object(eml.shutil, "which", side_effect=lambda name: "/tools/" + name)
    @patch.object(eml.subprocess, "check_output")
    def test_herdr_tab_uses_live_workspace_and_quotes_shell_command(
        self, output, which
    ):
        for action in ("open", "folio", "magit"):
            with self.subTest(action=action):
                output.reset_mock()
                output.side_effect = [
                    json.dumps(
                        {"result": {"pane": {"pane_id": "w2:p9", "workspace_id": "w2"}}}
                    ),
                    json.dumps({"result": {"root_pane": {"pane_id": "w2:p10"}}}),
                    "",
                ]
                with patch.object(
                    eml, "resolve_target", return_value=(self.path, 42, 3)
                ):
                    eml.launch(action, str(self.path))
                calls = [call.args[0] for call in output.call_args_list]
                self.assertEqual(
                    calls[0], ["/tools/herdr", "pane", "current", "--current"]
                )
                self.assertEqual(
                    calls[1],
                    [
                        "/tools/herdr",
                        "tab",
                        "create",
                        "--workspace",
                        "w2",
                        "--cwd",
                        str(self.path if action == "magit" else self.path.parent),
                        "--label",
                        f"{'em' if action == 'open' else action}:{self.path.name}".lower(),
                        "--focus",
                    ],
                )
                self.assertEqual(
                    calls[2][:4], ["/tools/herdr", "pane", "run", "w2:p10"]
                )
                command = shlex.split(calls[2][4])
                self.assertEqual(
                    command[:6],
                    ["exec", "/tools/emacsclient", "-t", "-a", "", "--eval"],
                )
                self.assertIn(f"{eml.lisp_string(self.path)} 42 3", command[6])
                self.assertEqual(len(command), 7)

    @patch.dict(os.environ, {"HERDR_ENV": "1", "HERDR_PANE_ID": "w1:p1"})
    @patch.object(eml.shutil, "which", side_effect=lambda name: "/tools/" + name)
    @patch.object(eml.subprocess, "check_output")
    def test_herdr_split_targets_resolved_caller_and_labels_new_pane(
        self, output, which
    ):
        output.side_effect = [
            json.dumps(
                {"result": {"pane": {"pane_id": "w2:p9", "workspace_id": "w2"}}}
            ),
            json.dumps({"result": {"pane": {"pane_id": "w2:p10"}}}),
            "",
            "",
        ]
        eml.launch("folio", str(self.path), split=True)
        calls = [call.args[0] for call in output.call_args_list]
        self.assertEqual(
            calls[1],
            [
                "/tools/herdr",
                "pane",
                "split",
                "--pane",
                "w2:p9",
                "--direction",
                "right",
                "--cwd",
                str(self.path.parent),
                "--focus",
            ],
        )
        self.assertEqual(
            calls[2],
            [
                "/tools/herdr",
                "pane",
                "rename",
                "w2:p10",
                f"folio:{self.path.name}".lower(),
            ],
        )
        self.assertEqual(calls[3][:4], ["/tools/herdr", "pane", "run", "w2:p10"])

    @patch.dict(os.environ, {"HERDR_ENV": "1", "HERDR_PANE_ID": ""})
    @patch.object(eml.shutil, "which", return_value="/tools/emacsclient")
    @patch.object(eml.subprocess, "check_output")
    def test_herdr_missing_context_never_targets_focused_pane(self, output, which):
        with self.assertRaisesRegex(ValueError, "herdr pane is unknown"):
            eml.launch("open", str(self.path))
        output.assert_not_called()

    @patch.dict(
        os.environ, {"HERDR_ENV": "1", "HERDR_PANE_ID": "w1:p1", "TMUX": "inherited"}
    )
    @patch.object(
        eml.shutil,
        "which",
        side_effect=lambda name: None if name == "herdr" else "/tools/" + name,
    )
    @patch.object(eml.subprocess, "check_output")
    def test_missing_herdr_does_not_fall_back_to_outer_tmux(self, output, which):
        with self.assertRaisesRegex(ValueError, "herdr is unavailable"):
            eml.launch("open", str(self.path))
        output.assert_not_called()

    @patch.dict(
        os.environ, {"HERDR_ENV": "1", "HERDR_PANE_ID": "w1:p1", "TMUX": "inherited"}
    )
    @patch.object(eml.shutil, "which", side_effect=lambda name: "/tools/" + name)
    @patch.object(eml.subprocess, "check_output")
    def test_herdr_failure_stops_before_creating_a_view(self, output, which):
        for response in ("not json", '{"result": {}}'):
            with self.subTest(response=response):
                output.reset_mock()
                output.return_value = response
                with self.assertRaisesRegex(ValueError, "invalid"):
                    eml.launch("open", str(self.path))
                self.assertEqual(output.call_count, 1)


class InstallTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.destination = Path(self.directory.name)

    def test_terminal_configs_are_copied_and_restored_on_reinstall(self):
        configs = {
            "config/tmux.conf": ".tmux.conf",
            "config/herdr/config.toml": ".config/herdr/config.toml",
            "config/ghostty/config": ".config/ghostty/config",
            "config/ghostty/shaders/cursor_warp.glsl": ".config/ghostty/shaders/cursor_warp.glsl",
        }
        installer.install(REPO, self.destination)
        for source, target in configs.items():
            path = self.destination / target
            self.assertFalse(path.is_symlink())
            self.assertEqual(path.read_bytes(), (REPO / source).read_bytes())
            path.write_text("machine changes")
        for _ in range(2):
            installer.install(REPO, self.destination)
            for source, target in configs.items():
                self.assertEqual(
                    (self.destination / target).read_bytes(),
                    (REPO / source).read_bytes(),
                )

    def test_terminal_config_symlinks_are_replaced_without_changing_referents(self):
        referent = self.destination / "old-config"
        referent.write_text("old config")
        target = self.destination / ".tmux.conf"
        target.symlink_to(referent)
        installer.install(REPO, self.destination)
        self.assertFalse(target.is_symlink())
        self.assertEqual(target.read_bytes(), (REPO / "config/tmux.conf").read_bytes())
        self.assertEqual(referent.read_text(), "old config")

    def test_broken_terminal_config_symlink_is_replaced(self):
        target = self.destination / ".tmux.conf"
        target.symlink_to(self.destination / "missing-config")
        installer.install(REPO, self.destination)
        self.assertFalse(target.is_symlink())
        self.assertEqual(target.read_bytes(), (REPO / "config/tmux.conf").read_bytes())

    def test_terminal_config_directory_is_reported_before_installing(self):
        (self.destination / ".tmux.conf").mkdir()
        with self.assertRaisesRegex(ValueError, "destination is a directory"):
            installer.install(REPO, self.destination)
        self.assertFalse((self.destination / ".local").exists())

    def test_install_is_idempotent_and_shared(self):
        installer.install(REPO, self.destination)
        launcher = self.destination / ".local" / "bin" / "eml"
        inode = launcher.lstat().st_ino
        installer.install(REPO, self.destination)
        self.assertEqual(launcher.lstat().st_ino, inode)
        self.assertEqual(launcher.resolve(), REPO / "bin" / "eml")
        for agent in (".agents", ".claude"):
            for name in ("em", "folio", "magit"):
                self.assertEqual(
                    (self.destination / agent / "skills" / name).resolve(),
                    REPO / "skills" / name,
                )

    def test_relocating_repo_updates_symlinks(self):
        launcher = self.destination / ".local" / "bin" / "eml"
        launcher.parent.mkdir(parents=True)
        launcher.symlink_to(self.destination / "old-repo" / "bin" / "eml")
        installer.install(REPO, self.destination)
        self.assertEqual(launcher.resolve(), REPO / "bin" / "eml")

    def test_conflicting_skill_is_preserved_before_any_links_are_created(self):
        existing = self.destination / ".claude" / "skills" / "magit"
        existing.mkdir(parents=True)
        marker = existing / "SKILL.md"
        marker.write_text("user skill")
        with self.assertRaisesRegex(ValueError, "not a symlink"):
            installer.install(REPO, self.destination)
        self.assertEqual(marker.read_text(), "user skill")
        self.assertFalse((self.destination / ".local").exists())


if __name__ == "__main__":
    unittest.main()
