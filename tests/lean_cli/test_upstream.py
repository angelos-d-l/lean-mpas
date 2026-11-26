"""Tests for ./lean fetch-upstream (tools/lean/upstream.py), on temporary local git repositories: nothing is cloned
from the network."""
import contextlib
import io
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import upstream

ENV = {key: value for key, value in {**os.environ, 'GIT_CONFIG_GLOBAL': os.devnull, 'GIT_CONFIG_NOSYSTEM': '1'}.items()
       if key not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_INDEX_FILE')}
EXTERNALS = """[MMM-physics]
local_path = ./physics_mmm
protocol = git
repo_url = {mmm}
tag = 20250616-MPASv8.3
required = True

[GSL_UGWP]
local_path = ./physics_noaa/UGWP
protocol = git
repo_url = {ugwp}
tag = MPAS_20241223
required = True

[externals_description]
schema_version = 1.0.0
"""


def git(repo: Path, *args: str) -> str:
    done = subprocess.run(['git', '-C', str(repo), *args], check=True, env=ENV, capture_output=True, text=True)
    return done.stdout.strip()


def make_repo(path: Path, files: dict, tag: str) -> str:
    """A git repository at path with files committed and tagged; returns the commit."""
    for rel, text in files.items():
        (path / rel).parent.mkdir(parents=True, exist_ok=True)
        (path / rel).write_text(text, encoding='utf-8')
    git(path, 'init', '-q')
    git(path, 'add', '-A')
    git(path, '-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', 'init')
    git(path, 'tag', tag)
    return git(path, 'rev-parse', 'HEAD')


def lock_for(mpas: str, mpas_url: str, mmm: str, ugwp: str) -> dict:
    return {'mpas_model': {'repo': mpas_url, 'tag': 'v8.4.2', 'commit': mpas},
            'mmm_physics': {'repo': 'unused', 'tag': '20250616-MPASv8.3', 'commit': mmm},
            'gsl_ugwp': {'repo': 'unused', 'tag': 'MPAS_20241223', 'commit': ugwp}}


class UpstreamTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name).resolve()
        patch = mock.patch.dict(os.environ, ENV, clear=True)
        patch.start()
        self.addCleanup(patch.stop)

    def fetch(self, dest: Path, lock: dict) -> tuple[int, str]:
        out = io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(io.StringIO()):
            status = upstream.fetch(dest, lock=lock)
        return status, out.getvalue()

    def test_upstream_present_ok(self):
        dest = self.tmp / 'mpas_src'
        commit = make_repo(dest, {'README.md': 'mpas\n'}, 'v8.4.2')
        status, out = self.fetch(dest, lock_for(commit, 'unused', 'x', 'y'))
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'UPSTREAM OK (already present)')

    def test_upstream_mismatch_refuses(self):
        dest = self.tmp / 'mpas_src'
        make_repo(dest, {'README.md': 'another commit\n'}, 'v8.4.2')
        (dest / 'local.txt').write_text('mine\n', encoding='utf-8')
        status, out = self.fetch(dest, lock_for('f' * 40, 'unused', 'x', 'y'))
        self.assertEqual(status, 1)
        self.assertIn('E_UPSTREAM_MISMATCH', out.splitlines()[-1])
        self.assertEqual((dest / 'local.txt').read_text(encoding='utf-8'), 'mine\n', 'nothing is overwritten')
        self.assertEqual(git(dest, 'log', '--format=%s'), 'init')

    def test_folder_inside_another_repo_is_not_taken_for_a_checkout(self):
        outer = self.tmp / 'outer'
        commit = make_repo(outer, {'README.md': 'outer\n'}, 'v8.4.2')
        dest = outer / 'reference' / 'mpas_src'
        dest.mkdir(parents=True)
        (dest / 'x.F').write_text('x\n', encoding='utf-8')
        status, out = self.fetch(dest, lock_for(commit, 'unused', 'x', 'y'))
        self.assertEqual(status, 1)
        self.assertIn('E_UPSTREAM_MISMATCH', out.splitlines()[-1])

    def test_clone_with_externals(self):
        mmm = make_repo(self.tmp / 'mmm', {'mp_wsm6.F90': 'mmm\n'}, '20250616-MPASv8.3')
        ugwp = make_repo(self.tmp / 'ugwp', {'ugwp.F90': 'ugwp\n'}, 'MPAS_20241223')
        cfg = EXTERNALS.format(mmm=(self.tmp / 'mmm').as_uri(), ugwp=(self.tmp / 'ugwp').as_uri())
        mpas = make_repo(self.tmp / 'mpas', {'src/core_atmosphere/Externals.cfg': cfg,
                                             'src/core_atmosphere/physics/Makefile': 'all:\n'}, 'v8.4.2')
        dest = self.tmp / 'reference' / 'mpas_src'
        lock = lock_for(mpas, (self.tmp / 'mpas').as_uri(), mmm, ugwp)
        status, out = self.fetch(dest, lock)
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'UPSTREAM OK')
        physics = dest / 'src' / 'core_atmosphere' / 'physics'
        self.assertEqual(git(dest, 'rev-parse', 'HEAD'), mpas)
        self.assertEqual(git(physics / 'physics_mmm', 'rev-parse', 'HEAD'), mmm)
        self.assertEqual(git(physics / 'physics_noaa' / 'UGWP', 'rev-parse', 'HEAD'), ugwp)
        self.assertEqual(self.fetch(dest, lock), (0, 'UPSTREAM OK (already present)\n'))

    def test_clone_with_a_wrong_external_commit_leaves_nothing(self):
        make_repo(self.tmp / 'mmm', {'mp_wsm6.F90': 'mmm\n'}, '20250616-MPASv8.3')
        ugwp = make_repo(self.tmp / 'ugwp', {'ugwp.F90': 'ugwp\n'}, 'MPAS_20241223')
        cfg = EXTERNALS.format(mmm=(self.tmp / 'mmm').as_uri(), ugwp=(self.tmp / 'ugwp').as_uri())
        mpas = make_repo(self.tmp / 'mpas', {'src/core_atmosphere/Externals.cfg': cfg}, 'v8.4.2')
        dest = self.tmp / 'reference' / 'mpas_src'
        status, out = self.fetch(dest, lock_for(mpas, (self.tmp / 'mpas').as_uri(), 'e' * 40, ugwp))
        self.assertEqual(status, 1)
        self.assertIn('E_UPSTREAM_MISMATCH', out.splitlines()[-1])
        self.assertIn('MMM-physics', out.splitlines()[-1])
        self.assertEqual(sorted(path.name for path in dest.parent.iterdir()), [], 'no partial checkout is left')


if __name__ == '__main__':
    unittest.main()
