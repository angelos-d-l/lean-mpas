"""Tests for the vendored pristine MPAS closure (tools/lean/vendor.py)."""
import json
import re
import shutil
import tempfile
import unittest
from pathlib import Path

from tools.lean.vendor import FIXED, PACKAGE_TERMS, check, esmf_closure, noahmp_closure, order_problems

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / 'reference' / 'mpas_src' / 'src'
DEST = ROOT / 'third_party' / 'mpas-8.4.2'
REFERENCE_FIXES = ('module_cu_gf.mpas.F module_mp_thompson.F mpas_atmphys_o3climatology.F '
                   'mpas_isobaric_diagnostics.F mpas_timekeeping.F summarize_timestep.inc').split()

needs_upstream = unittest.skipUnless(SRC.is_dir(), 'reference/mpas_src is absent (./lean fetch-upstream)')


def committed_closure():
    return json.loads((DEST / 'closure.json').read_text())


def vendored_paths():
    closure = committed_closure()
    return closure['fixed'] + closure['esmf_time'] + closure['noahmp']


class ClosureTest(unittest.TestCase):

    def test_fixed_has_the_nine_files(self):
        self.assertEqual(len(FIXED), 9)
        self.assertEqual(FIXED[0], 'framework/mpas_kind_types.F')
        self.assertEqual(FIXED[-1], 'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_sw.F')

    @needs_upstream
    def test_esmf_closure_is_every_file_in_dependency_order(self):
        paths = esmf_closure(SRC)
        folder = SRC / 'external' / 'esmf_time_f90'
        every = sorted('external/esmf_time_f90/' + p.name for p in folder.iterdir()
                       if p.suffix in ('.F90', '.inc'))
        self.assertEqual(sorted(paths), every)
        f90 = [p for p in paths if p.endswith('.F90')]
        self.assertEqual(paths[:len(f90)], f90)  # the *.F90 compile order first, then the *.inc
        defined = {}
        for i, p in enumerate(f90):
            for name in re.findall(r'^\s*module\s+(\w+)\s*$', (SRC / p).read_text(), re.M | re.I):
                defined[name.lower()] = i
        for i, p in enumerate(f90):
            for name in re.findall(r'^\s*use\s+(\w+)', (SRC / p).read_text(), re.M | re.I):
                if name.lower() in defined:
                    self.assertLess(defined[name.lower()], i, f'{p} uses {name} before it is compiled')
        self.assertEqual(order_problems('esmf_time', paths, SRC), [])

    def test_order_problems_detects_a_bad_order(self):
        esmf = committed_closure()['esmf_time']
        self.assertEqual(order_problems('esmf_time', esmf, DEST / 'src'), [])
        self.assertTrue(order_problems('esmf_time', list(reversed(esmf)), DEST / 'src'))
        noahmp = committed_closure()['noahmp']
        self.assertEqual(order_problems('noahmp', noahmp, DEST / 'src'), [])
        self.assertTrue(order_problems('noahmp', list(reversed(noahmp)), DEST / 'src'))

    @needs_upstream
    def test_committed_closure_is_current(self):
        closure = committed_closure()
        self.assertEqual(list(closure), ['fixed', 'esmf_time', 'noahmp'])
        self.assertEqual(closure['fixed'], FIXED)
        # esmf_time as a set: the walker visits files in directory order, which differs between file systems
        self.assertEqual(sorted(closure['esmf_time']), sorted(esmf_closure(SRC)))
        self.assertEqual(order_problems('esmf_time', closure['esmf_time'], SRC), [])
        self.assertEqual(closure['noahmp'], noahmp_closure(SRC))


class VendoredTreeTest(unittest.TestCase):

    def test_no_errorhandlemod(self):
        self.assertFalse([p for p in vendored_paths() if Path(p).stem == 'ErrorHandleMod'])
        self.assertFalse(list(DEST.rglob('ErrorHandleMod*')))

    def test_no_overlap_with_reference_fixes(self):
        self.assertEqual(sorted(p.name for p in (ROOT / 'reference_fixes').iterdir()), REFERENCE_FIXES)
        names = {Path(p).name for p in vendored_paths()}
        self.assertEqual(names & set(REFERENCE_FIXES), set())

    def test_check_passes_on_the_committed_tree(self):
        self.assertEqual(check(DEST), [])

    def test_check_detects_modified_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            copy = Path(tmp) / 'mpas-8.4.2'
            shutil.copytree(DEST, copy)
            target = copy / 'src' / FIXED[1]
            data = bytearray(target.read_bytes())
            data[100] ^= 0x01
            target.write_bytes(bytes(data))
            problems = check(copy)
        self.assertEqual(len(problems), 1, problems)
        self.assertIn('src/' + FIXED[1], problems[0])

    def test_check_without_upstream_detects_reordered_closure(self):
        with tempfile.TemporaryDirectory() as tmp:
            copy = Path(tmp) / 'mpas-8.4.2'
            shutil.copytree(DEST, copy)
            closure = json.loads((copy / 'closure.json').read_text())
            closure['esmf_time'].reverse()
            (copy / 'closure.json').write_text(json.dumps(closure, indent=2) + '\n')
            problems = check(copy)
        self.assertTrue(any('closure.json is changed' in p for p in problems), problems)
        self.assertTrue(any('uses' in p and 'before it is compiled' in p for p in problems), problems)
        self.assertTrue(any('README.md is not the README of closure.json' in p for p in problems), problems)

    @needs_upstream
    def test_check_against_upstream_accepts_another_valid_esmf_order(self):
        # another machine's walker may give another valid order: compared as a set, not reported as a wrong closure
        with tempfile.TemporaryDirectory() as tmp:
            copy = Path(tmp) / 'mpas-8.4.2'
            shutil.copytree(DEST, copy)
            closure = json.loads((copy / 'closure.json').read_text())
            f90 = [p for p in closure['esmf_time'] if p.endswith('.F90')]
            other = f90[1:2] + f90[:1] + f90[2:]  # wrf_message.F90 and ESMF_BaseMod.F90 use nothing of each other
            self.assertEqual(order_problems('esmf_time', other, SRC), [])
            closure['esmf_time'] = other + closure['esmf_time'][len(f90):]
            (copy / 'closure.json').write_text(json.dumps(closure, indent=2) + '\n')
            problems = check(copy, SRC)
        self.assertFalse([p for p in problems if 'is not the closure computed' in p or 'before it is compiled' in p],
                         problems)
        self.assertTrue(any('closure.json is changed' in p for p in problems), problems)

    def test_check_without_upstream_detects_changed_readme(self):
        with tempfile.TemporaryDirectory() as tmp:
            copy = Path(tmp) / 'mpas-8.4.2'
            shutil.copytree(DEST, copy)
            with (copy / 'README.md').open('a') as f:
                f.write('stale line\n')
            problems = check(copy)
        self.assertEqual(sorted(problems), ['README.md is changed (its sha256 is not the one in SHA256SUMS)',
                                            'README.md is not the README of closure.json'])

    def test_sha256sums_covers_closure_and_readme(self):
        names = [line.split('  ', 1)[1] for line in (DEST / 'SHA256SUMS').read_text().splitlines()]
        self.assertIn('closure.json', names)
        self.assertIn('README.md', names)
        self.assertIn('LICENSE', names)

    def test_check_detects_missing_and_extra_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            copy = Path(tmp) / 'mpas-8.4.2'
            shutil.copytree(DEST, copy)
            (copy / 'src' / FIXED[0]).unlink()
            (copy / 'src' / 'framework' / 'stray.F').write_text('stray\n')
            problems = check(copy)
        self.assertTrue(any('src/' + FIXED[0] in p for p in problems), problems)
        self.assertTrue(any('src/framework/stray.F' in p for p in problems), problems)

    @needs_upstream
    def test_check_against_upstream_detects_changed_original(self):
        with tempfile.TemporaryDirectory() as tmp:
            src = Path(tmp) / 'src'
            shutil.copyfile(SRC.parent / 'LICENSE', Path(tmp) / 'LICENSE')
            for rel in vendored_paths() + list(PACKAGE_TERMS):
                (src / rel).parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(SRC / rel, src / rel)
            shutil.copytree(SRC / 'core_atmosphere' / 'physics' / 'physics_noahmp',
                            src / 'core_atmosphere' / 'physics' / 'physics_noahmp', dirs_exist_ok=True)
            shutil.copytree(SRC / 'external' / 'esmf_time_f90', src / 'external' / 'esmf_time_f90',
                            dirs_exist_ok=True)
            self.assertEqual(check(DEST, src), [])
            with (src / FIXED[2]).open('a') as f:
                f.write('! changed\n')
            problems = check(DEST, src)
        self.assertEqual(len(problems), 1, problems)
        self.assertIn(FIXED[2], problems[0])


if __name__ == '__main__':
    unittest.main()
