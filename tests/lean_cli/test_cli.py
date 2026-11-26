"""Tests for the ./lean entry point and its argument handling (lean, tools/lean/cli.py)."""
import contextlib
import io
import subprocess
import sys
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import cli
from tools.lean.config import BuildConfig

ROOT = Path(__file__).resolve().parents[2]
COMMANDS = ('doctor', 'build', 'tables', 'fetch-upstream', 'generate')


def run_main(*argv: str) -> tuple[int, str, str]:
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        status = cli.main(list(argv))
    return status, out.getvalue(), err.getvalue()


class CliTest(unittest.TestCase):

    def test_unknown_command_exits_2(self):
        status, out, err = run_main('run', '--anything')
        self.assertEqual(status, 2)
        self.assertIn("invalid choice: 'run'", out + err)

    def test_help_lists_commands(self):
        status, out, _ = run_main('--help')
        self.assertEqual(status, 0)
        for name in COMMANDS:
            self.assertIn(name, out)

    def test_entry_point_runs_from_another_directory(self):
        done = subprocess.run([sys.executable, str(ROOT / 'lean'), '--help'], cwd='/', capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn('build', done.stdout)

    def test_build_command_passes_its_options(self):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            status, _, _ = run_main('build', '--target', 'gpu', '--profile', 'strict',
                                    '--jobs', '1', '--dry-run', '--json', '--compiler', '/x/nvfortran')
        self.assertEqual(status, 0)
        build.assert_called_once_with(BuildConfig('gpu', 'nvfortran', 'strict'), jobs=1,
                                      dry_run=True, as_json=True, compiler='/x/nvfortran', link=None)

    def test_build_defaults(self):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            run_main('build')
        build.assert_called_once_with(BuildConfig('cpu', 'gfortran', 'strict'), jobs=2, dry_run=False,
                                      as_json=False, compiler=None, link=None)

    def test_build_levels(self):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            self.assertEqual(run_main('build', '--levels', '45')[0], 0)
        build.assert_called_once_with(BuildConfig('cpu', 'gfortran', 'strict', levels=45), jobs=2, dry_run=False,
                                      as_json=False, compiler=None, link=None)
        with mock.patch('tools.lean.build.build') as build:
            for bad in ('0', '61', 'x'):
                with self.subTest(levels=bad):
                    self.assertEqual(run_main('build', '--levels', bad)[0], 2)
        build.assert_not_called()

    def test_bad_build_config_exits_2(self):
        with mock.patch('tools.lean.build.build') as build:
            status, _, err = run_main('build', '--target', 'gpu', '--fc', 'gfortran')
        self.assertEqual(status, 2)
        self.assertIn('nvfortran', err)
        build.assert_not_called()

    def test_tables_command(self):
        with mock.patch('tools.lean.tables.install', return_value=0) as install, \
                mock.patch('tools.lean.tables.rebuild', return_value=1) as rebuild:
            self.assertEqual(run_main('tables', '--from', 'build/tables')[0], 0)
            install.assert_called_once_with('build/tables')
            self.assertEqual(run_main('tables', '--build')[0], 1)
            rebuild.assert_called_once_with()
            self.assertEqual(run_main('tables', '--from', 'x', '--build')[0], 2)
            self.assertEqual(run_main('tables')[0], 2)

    def test_fetch_upstream_and_generate_commands(self):
        with mock.patch('tools.lean.upstream.fetch', return_value=0) as fetch, \
                mock.patch('tools.lean.generate.generate', return_value=1) as generate:
            self.assertEqual(run_main('fetch-upstream')[0], 0)
            fetch.assert_called_once_with()
            self.assertEqual(run_main('generate', '--check')[0], 1)
            generate.assert_called_once_with(check=True)
            run_main('generate')
            generate.assert_called_with(check=False)

    def test_doctor_command_passes_its_options(self):
        with mock.patch('tools.lean.doctor.checks', return_value=[]) as checks, \
                mock.patch('tools.lean.doctor.render', return_value=('DOCTOR OK', 0)) as render:
            status, out, _ = run_main('doctor', '--target', 'gpu', '--json')
        self.assertEqual((status, out), (0, 'DOCTOR OK\n'))
        checks.assert_called_once_with('gpu', 'nvfortran')
        render.assert_called_once_with([], as_json=True)
        self.assertEqual(run_main('doctor', '--target', 'gpu', '--fc', 'gfortran')[0], 2)


if __name__ == '__main__':
    unittest.main()
