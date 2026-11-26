"""tools/gen_phys_state.py, the generator of src/phys_state.f90, and the allocatable state types it reads.

    cd tests/sizes && python3 -m unittest test_gen
"""
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GEN = ROOT / 'tools' / 'gen_phys_state.py'
sys.path.insert(0, str(ROOT / 'tools'))
# the object's name, the type's name in the routines, and its member count in the default build
TYPES = [('state', 'state', 102), ('lands', 'land', 460), ('convection', 'gf', 22), ('drag', 'gwdo', 10)]
ROUTINES = ('zero', 'to_device', 'to_host', 'copy')


def run(*args):
    return subprocess.run([sys.executable, str(GEN), *args], cwd=ROOT, capture_output=True, text=True)


def routine(text, name):
    """the lines of subroutine `name` in the generated module"""
    lines = text.split('\n')
    start = next(i for i, line in enumerate(lines) if re.match(r'^ subroutine %s\(' % name, line))
    end = next(i for i in range(start, len(lines)) if lines[i] == ' end subroutine')
    return lines[start:end]


def default_build(lines):
    """the lines outside every #ifdef block"""
    out, depth = [], 0
    for line in lines:
        if line.startswith('#if'):
            depth += 1
        elif line.startswith('#endif'):
            depth -= 1
        elif line.strip() == 'if (noahmp_opt_sfc == 2) then':
            depth += 1
        elif depth and line.strip() == 'end if':
            depth -= 1
        elif depth == 0:
            out.append(line)
    return out


def storage_names(short):
    import gen_phys_state
    out, depth = [], 0
    for f in gen_phys_state.fields(short):
        if isinstance(f, str):
            depth += 1 if f.startswith('#if') else -1 if f.startswith('#endif') else 0
        elif depth == 0:
            out.append(f['name'])
    return out


def named(lines):
    """the members the lines name, in order (a line may name its member twice: c_loc(s%a), size(s%a))"""
    out = []
    for line in lines:
        found = []
        for a, b in re.findall(r'\bs%(\w+)|\bfrom%(\w+)', line):
            if (a or b) not in found:
                found.append(a or b)
        out += found
    return out


class GeneratorTest(unittest.TestCase):
    def test_types_are_allocatable_with_original_shapes(self):
        import gen_phys_state
        for prefix, short, _ in (TYPES[0], TYPES[2]):
            with self.subTest(type=short):
                fields = [f for f in gen_phys_state.fields(short) if not isinstance(f, str)]
                self.assertTrue(all(f['allocatable'] for f in fields if f['rank']))
                filename = 'state' if short == 'state' else 'gf_state'
                text = (ROOT / 'src' / (filename + '.f90')).read_text()
                body = re.search(r' subroutine allocate_' + short + r'\(s\)(.*?) end subroutine', text, re.S)
                self.assertIsNotNone(body)
                allocated = [n for n, _ in re.findall(r'allocate\(s%(\w+)\(([^()]*)\)\)', body.group(1))]
                self.assertEqual(allocated, [f['name'] for f in fields if f['rank']])
        text = (ROOT / 'src' / 'state.f90').read_text()
        mesh = re.search(r' type mesh_type\n(.*?) end type', text, re.S).group(1)
        self.assertEqual(len(re.findall(r'allocatable', mesh)), 2)
        body = re.search(r' subroutine allocate_mesh\(mesh\)(.*?) end subroutine', text, re.S)
        self.assertIsNotNone(body)
        self.assertEqual(re.findall(r'allocate\(mesh%(\w+)\(([^()]*)\)\)', body.group(1)),
                         [('z', 'nz+1,ncells'), ('area', 'ncells'), ('lat', 'ncells'), ('lon', 'ncells'),
                          ('neighbors', 'maxedges,ncells'), ('degree', 'ncells')])

    def test_startup_allocates_then_zeros(self):
        main = (ROOT / 'src' / 'lean_model.f90').read_text()
        self.assertIn('call allocate_state(state); call zero_state(state)', main)
        self.assertIn('call allocate_gf(convection); call zero_gf(convection)', main)
        self.assertIn('call allocate_mesh(mesh)', main)

    def test_column_harnesses_allocate_and_use_generated_zeros(self):
        for name in ('sea_ice', 'land_ice'):
            with self.subTest(program=name):
                text = (ROOT / 'tests' / name / 'columns.f90').read_text()
                self.assertIn('call allocate_state(state); call zero_state(state)', text)

    def test_member_counts(self):
        import gen_phys_state
        for _, short, count in TYPES:
            with self.subTest(type=short):
                self.assertEqual(len(gen_phys_state.member_names(short)), count)

    def test_check_passes_on_the_committed_file(self):
        done = run('--check')
        self.assertEqual((done.returncode, done.stdout), (0, ''), done.stderr)

    def test_check_fails_when_a_member_is_gone(self):
        with tempfile.TemporaryDirectory() as tmp:
            for name in ('state', 'noahmp_state', 'gf_state', 'gwdo_state'):
                shutil.copy(ROOT / 'src' / (name + '.f90'), tmp)
            path = Path(tmp) / 'state.f90'
            text = path.read_text()
            line = next(l for l in text.split('\n') if re.match(r'^\s*real(\s*,\s*allocatable)?\s*::\s*tsq\(', l))   # state_type's, not the view's
            path.write_text(text.replace(line + '\n', '', 1))
            done = run('--check', '--src', tmp)
            self.assertEqual(done.returncode, 1)
            self.assertEqual(done.stdout.strip(), 'DIFFERS src/phys_state.f90')

    def test_every_routine_names_every_member(self):
        import gen_phys_state
        text = (ROOT / 'src' / 'phys_state.f90').read_text()
        for _, short, _ in TYPES:
            routines = ROUTINES + (('enter',) if gen_phys_state.is_allocatable(short) else ())
            for kind in routines:
                with self.subTest(routine=kind + '_' + short):
                    found = named(default_build(routine(text, kind + '_' + short)))
                    expected = storage_names(short)
                    self.assertEqual(found, expected)

    def test_an_allocatable_type_is_entered_object_first(self):
        import gen_phys_state
        text = (ROOT / 'src' / 'phys_state.f90').read_text()
        shorts = [(short, count) for _, short, count in TYPES if gen_phys_state.is_allocatable(short)]
        if not shorts:
            self.skipTest('every type has fixed members')
        for short, count in shorts:
            with self.subTest(type=short):
                lines = routine(text, 'enter_' + short)
                self.assertEqual(lines[2], ' !$acc enter data copyin(s)')
                self.assertEqual(len(named(default_build(lines))), count + (2 if short == 'land' else 0))
                self.assertEqual(len(named(default_build(routine(text, 'zero_' + short)))), count + (2 if short == 'land' else 0))

    def test_surface_exchange_fields_in_every_routine(self):
        text = (ROOT / 'src' / 'phys_state.f90').read_text()
        extra = ['energy_state_frictionvelvertveg', 'energy_state_frictionvelvertbare']
        self.assertTrue(set(extra) <= set(storage_names('land')))
        for kind in (*ROUTINES, 'enter'):
            with self.subTest(routine=kind + '_land'):
                lines = routine(text, kind + '_land')
                all_names = named(lines)
                self.assertTrue(set(extra) <= set(all_names))
                self.assertTrue(set(['sw_frac_dir_vis', 'sw_frac_dir_nir', 'sw_frac_dif_vis']) <= set(all_names))
                selected = named(default_build(lines))
                self.assertTrue(set(extra) <= set(selected))
                self.assertFalse(set(['sw_frac_dir_vis', 'sw_frac_dir_nir', 'sw_frac_dif_vis']) & set(selected))


if __name__ == '__main__':
    unittest.main()
