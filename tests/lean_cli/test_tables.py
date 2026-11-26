"""Tests for ./lean tables (tools/lean/tables.py): copies into a temporary root, checked against a temporary lock."""
import contextlib
import hashlib
import io
import re
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import tables

GOOD = {'A.DBL': b'table a', 'B.DBL': b'table b, longer'}


def lock_of(contents: dict) -> dict:
    return {name: {'sha256': hashlib.sha256(data).hexdigest(), 'size': len(data)} for name, data in contents.items()}


class TablesTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name).resolve()
        self.root = self.tmp / 'root'
        self.root.mkdir()
        self.source = self.tmp / 'source'
        self.source.mkdir()
        for name, data in GOOD.items():
            (self.source / name).write_bytes(data)
        self.dest = self.root / 'data' / 'tables'
        patches = (mock.patch.object(tables, 'ROOT', self.root),
                   mock.patch.object(tables, 'expected', return_value=lock_of(GOOD)))
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)

    def call(self, function, *args) -> tuple[int, str]:
        out = io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(io.StringIO()):
            status = function(*args)
        return status, out.getvalue()

    def test_tables_install_ok(self):
        status, out = self.call(tables.install, str(self.source))
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'TABLES OK')
        for name, data in GOOD.items():
            self.assertEqual((self.dest / name).read_bytes(), data)
        self.assertEqual(tables.verified(), [self.dest / name for name in sorted(GOOD)])
        self.assertEqual(sorted(path.name for path in self.dest.iterdir()), sorted(GOOD), 'no temporary files left')

    def test_tables_bad_sha_removed(self):
        (self.source / 'B.DBL').write_bytes(b'table b, LONGER')     # same size, other bytes: an interrupted copy
        status, out = self.call(tables.install, str(self.source))
        self.assertEqual(status, 1)
        self.assertEqual(out.splitlines()[-1], 'TABLES FAIL: E_TABLES_SHA B.DBL')
        self.assertFalse((self.dest / 'B.DBL').exists(), 'the bad copy is removed')
        self.assertEqual(sorted(path.name for path in self.dest.iterdir()), ['A.DBL'])
        self.assertEqual(tables.verified(), [self.dest / 'A.DBL'])

    def test_verified_excludes_a_bad_file_in_place(self):
        self.dest.mkdir(parents=True)
        (self.dest / 'A.DBL').write_bytes(GOOD['A.DBL'])
        (self.dest / 'B.DBL').write_bytes(b'truncated')
        self.assertEqual(tables.verified(), [self.dest / 'A.DBL'])
        self.assertEqual(tables.status(), {'A.DBL': 'ok', 'B.DBL': 'bad'})

    def test_tables_file_url(self):
        status, out = self.call(tables.install, self.source.as_uri())
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'TABLES OK')
        self.assertEqual(len(tables.verified()), 2)

    def test_missing_source_file_fails(self):
        (self.source / 'A.DBL').unlink()
        status, out = self.call(tables.install, str(self.source))
        self.assertEqual(status, 1)
        self.assertTrue(out.splitlines()[-1].startswith('TABLES FAIL: E_TABLES_SOURCE A.DBL'), out)
        self.assertEqual(tables.verified(), [self.dest / 'B.DBL'])

    def generator(self, contents: dict) -> None:
        """Make rebuild() run a fake generator that writes contents into its directory and counts its runs."""
        (self.root / 'reference' / 'mpas_src' / 'src').mkdir(parents=True, exist_ok=True)
        (self.root / 'tests').mkdir(exist_ok=True)
        (self.root / 'tests' / 'support.f90').write_text('module services\nend module\nmodule mp_radar\nend module\n',
                                                         encoding='utf-8')
        self.runs = self.tmp / 'runs.txt'
        writes = ''.join(f"printf '%s' '{data.decode()}' > {name}; " for name, data in contents.items())
        steps = [['sh', '-c', 'true'], ['sh', '-c', f'echo run >> {self.runs}; {writes}']]
        patch = mock.patch.object(tables, '_steps', return_value=steps)
        patch.start()
        self.addCleanup(patch.stop)

    def script_tree(self) -> dict:
        """build/tables/ and build/O2/ as tools/build_tables.sh leaves them; rebuild() must not touch them."""
        files = {'build/tables/A.DBL': b'sentinel a', 'build/tables/B.DBL': b'sentinel b',
                 'build/tables/generation.log': b'script log', 'build/O2/flags.txt': b'-O2', 'build/O2/build_tables': b'x'}
        for rel, data in files.items():
            (self.root / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.root / rel).write_bytes(data)
        return files

    def assert_untouched(self, files: dict) -> None:
        for rel, data in files.items():
            self.assertEqual((self.root / rel).read_bytes(), data, rel)
        for folder in ('build/tables', 'build/O2'):
            self.assertEqual(sorted(str(path.relative_to(self.root)) for path in (self.root / folder).iterdir()),
                             sorted(rel for rel in files if rel.startswith(folder + '/')), folder)

    def test_rebuild_works_in_its_own_directory(self):
        scripts = self.script_tree()
        self.generator(GOOD)
        status, out = self.call(tables.rebuild)
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'TABLES OK')
        self.assertEqual(tables.verified(), [self.dest / name for name in sorted(GOOD)])
        work = self.root / 'build' / 'lean-tables'
        for name, data in GOOD.items():
            self.assertEqual((work / name).read_bytes(), data)
        self.assertEqual((work / 'services.f90').read_text(encoding='utf-8'), 'module services\nend module\n')
        self.assertTrue((work / 'generation.log').is_file())
        self.assert_untouched(scripts)

    def test_rebuild_regenerates_every_time(self):
        self.generator(GOOD)
        self.assertEqual(self.call(tables.rebuild)[0], 0)
        stale = self.root / 'build' / 'lean-tables' / 'stale.DBL'
        stale.write_bytes(b'left from an earlier run')
        self.assertEqual(self.call(tables.rebuild)[0], 0)
        self.assertEqual(self.runs.read_text(encoding='utf-8'), 'run\nrun\n', 'the generator runs on each call')
        self.assertFalse(stale.exists(), 'the directory is emptied first')

    def test_rebuild_checks_what_it_generated_although_installed_tables_are_good(self):
        self.assertEqual(self.call(tables.install, str(self.source))[0], 0)
        self.generator({'A.DBL': GOOD['A.DBL'], 'B.DBL': b'table b, other!'})
        status, out = self.call(tables.rebuild)
        self.assertEqual(status, 1)
        self.assertEqual(out.splitlines()[-1], 'TABLES FAIL: E_TABLES_SHA B.DBL')
        self.assertEqual(tables.verified(), [self.dest / name for name in sorted(GOOD)], 'the good tables stay')

    def test_rebuild_fails_on_a_table_not_generated(self):
        self.assertEqual(self.call(tables.install, str(self.source))[0], 0)
        self.generator({'B.DBL': GOOD['B.DBL']})
        status, out = self.call(tables.rebuild)
        self.assertEqual(status, 1)
        self.assertEqual(out.splitlines()[-1], 'TABLES FAIL: E_TABLES_SOURCE A.DBL')

    def test_rebuild_fails_on_a_failing_step(self):
        scripts = self.script_tree()
        self.generator(GOOD)
        with mock.patch.object(tables, '_steps', return_value=[['sh', '-c', 'exit 3'], ['sh', '-c', 'true']]):
            status, out = self.call(tables.rebuild)
        self.assertEqual(status, 1)
        self.assertEqual(out.splitlines()[-1], 'TABLES FAIL: E_TABLES_BUILD step 1 of 2 (sh) exited 3')
        self.assertFalse(self.dest.exists(), 'nothing is installed')
        self.assert_untouched(scripts)

    def test_rebuild_needs_reference_mpas_src(self):
        status, out = self.call(tables.rebuild)
        self.assertEqual(status, 1)
        self.assertIn('E_TABLES_BUILD reference/mpas_src/src is missing', out)
        self.assertFalse((self.root / 'build').exists(), 'nothing is written')

    def test_steps_are_those_of_build_tables_sh(self):
        """The compile lines repeat tools/build_reference.sh O2 and tools/build_tables.sh: same flags, sources and
        objects (the byte check against the lock catches any other drift)."""
        scripts = {name: (tables.config.ROOT / 'tools' / name).read_text(encoding='utf-8')
                   for name in ('build_reference.sh', 'build_tables.sh')}
        flags = re.search(r'^flags="(.*) \$opt"$', scripts['build_reference.sh'], re.M).group(1).split() + ['-O2']
        steps = tables._steps(self.root / 'build' / 'lean-tables')
        compiles = steps[:-1]
        for argv in compiles:
            self.assertEqual(argv[:1 + len(flags)], ['gfortran', *flags])
        names = lambda text: sorted(re.findall(r'[\w]+\.(?:F|f90|o)\b', text))
        script_compiles = [line for text in scripts.values() for line in text.splitlines()
                           if line.startswith('gfortran ')]
        self.assertEqual(names(' '.join(script_compiles)),
                         names(' '.join(Path(arg).name for argv in compiles for arg in argv)))
        self.assertEqual(steps[-1], ['./build_tables'])


if __name__ == '__main__':
    unittest.main()
