"""Tests for ./lean generate (tools/lean/generate.py): the two device-code generators on the repo and on a scratch
copy of their inputs."""
import contextlib
import io
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import generate

ROOT = Path(__file__).resolve().parents[2]
# What the generators read and write: their code, src/ (with src/device/) and the original files of third_party/.
GENERATOR_INPUTS = ('tools/gen_device_physics.py', 'tools/gen_device_radiation.py', 'tools/gen_radiation_fast_gas.py', 'tools/devgen.py', 'tools/switch_merge.py', 'tools/level_capacity.py', 'tools/thompson_card.f90',
                    'src',
                    'third_party/mpas-8.4.2/src/core_atmosphere/physics/mpas_atmphys_functions.F',
                    'third_party/mpas-8.4.2/src/core_atmosphere/physics/physics_wrf/module_ra_rrtmg_lw.F',
                    'third_party/mpas-8.4.2/src/core_atmosphere/physics/physics_wrf/module_ra_rrtmg_sw.F')


def scratch_copy(root: Path) -> Path:
    """A copy of the generators' inputs under root (no reference/mpas_src): the generators run there on their own."""
    for rel in GENERATOR_INPUTS:
        source, target = ROOT / rel, root / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, target, ignore=shutil.ignore_patterns('*.o', '*.mod'))
        else:
            shutil.copy2(source, target)
    return root


def flip_one_byte(path: Path) -> None:
    data = bytearray(path.read_bytes())
    data[-2] = ord('X') if data[-2] != ord('X') else ord('Y')
    path.write_bytes(bytes(data))


def run(check: bool, root: Path = ROOT) -> tuple[int, str]:
    out = io.StringIO()
    with mock.patch.object(generate, 'ROOT', root), contextlib.redirect_stdout(out), \
            contextlib.redirect_stderr(io.StringIO()):
        status = generate.generate(check=check)
    return status, out.getvalue()


class GenerateTest(unittest.TestCase):

    def test_generate_check_clean(self):
        status, out = run(check=True)
        self.assertEqual(status, 0, out)
        self.assertEqual(out.splitlines()[-1], 'GENERATE OK')

    def test_generators_read_third_party(self):
        for name in ('gen_device_physics.py', 'gen_device_radiation.py'):
            text = (ROOT / 'tools' / name).read_text(encoding='utf-8')
            self.assertNotIn('reference/mpas_src', text, name)
            self.assertIn('third_party/mpas-8.4.2/src/', text, name)

    def test_drift_is_found_and_generate_repairs_it(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = scratch_copy(Path(tmp).resolve())
            flip_one_byte(root / 'src/device/gf_core.f90')
            flip_one_byte(root / 'src/device/radiation_sw.f90')
            status, out = run(check=True, root=root)
            self.assertEqual(status, 1)
            self.assertEqual(out.splitlines()[-1],
                             'GENERATE DRIFT: src/device/gf_core.f90 src/device/radiation_sw.f90')
            status, out = run(check=False, root=root)
            self.assertEqual((status, out.splitlines()[-1]), (0, 'GENERATE OK'), out)
            for rel in ('src/device/gf_core.f90', 'src/device/radiation_sw.f90'):
                self.assertEqual((root / rel).read_bytes(), (ROOT / rel).read_bytes(), rel)
            self.assertEqual(run(check=True, root=root)[0], 0)

    def test_a_failing_generator_is_not_drift(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = scratch_copy(Path(tmp).resolve())
            (root / 'third_party/mpas-8.4.2/src/core_atmosphere/physics/mpas_atmphys_functions.F').unlink()
            status, out = run(check=True, root=root)
            self.assertEqual(status, 1)
            self.assertTrue(out.splitlines()[-1].startswith('GENERATE FAIL: E_GENERATOR gen_device_physics.py'), out)


if __name__ == '__main__':
    unittest.main()
