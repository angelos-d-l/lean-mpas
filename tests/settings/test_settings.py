"""Small checks of the run settings: the MPAS log comparison of the radiation decisions, the Noah-MP option's card
update, and messages written in one place only. Run from the repository root."""
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

class SettingsTest(unittest.TestCase):
    def test_radiation_log_oracle(self):
        reference = ROOT / 'data/cases/greece_60km/reference/strict_radsw15_3h/log.atmosphere.0000.out'
        if not reference.exists():
            self.skipTest(f'the MPAS reference log {reference.relative_to(ROOT)} is not released')
        text = reference.read_text()
        (ROOT / 'build').mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as work:
            actual = Path(work) / 'actual.log'
            # Stamps, boundaries and extrema remain exactly those of MPAS.
            actual.write_text(text)
            cmd = ['python3', 'tests/settings/compare_log.py', str(actual), str(reference),
                   str(len(re.findall(r'Begin timestep (\S+)', text)))]
            self.assertEqual(subprocess.run(cmd, capture_output=True).returncode, 0)
            for changed in (re.sub(r'(L_RADLW = )T', r'\1F', text, count=1),
                            re.sub(r'(L_RADSW = )T', r'\1F', text, count=1),
                            re.sub(r'^.*L_RADLW.*\n', '', text, flags=re.M)):
                actual.write_text(changed)
                self.assertNotEqual(subprocess.run(cmd, capture_output=True).returncode, 0,
                                    'wrong/missing radiation decisions accepted')

    @unittest.skipUnless(shutil.which('gfortran'), 'needs gfortran')
    def test_noahmp_device_guard(self):
        for device in (False, True):
            cmd = ['gfortran', '-E', '-cpp', '-ffree-line-length-none', 'src/lean_model.f90']
            if device:
                cmd.insert(1, '-DLEAN_PHYS_DEV')
            text = subprocess.check_output(cmd, text=True)
            self.assertEqual('!$acc update device(noahmp_opt_sfc)' in text, device)

    def test_one_place_each(self):
        model = (ROOT / 'src/lean_model.f90').read_text()
        combined = model + (ROOT / 'src/run/lean_drop_in.f90').read_text()
        # the microphysics lines come from the one message routine, the option's card copy from lean_model alone, and
        # the time of day from the clock, not from the step loop
        self.assertEqual(combined.count('--- n_microp  = '), 0, 'use one message routine')
        self.assertNotIn('!$acc update device(noahmp_opt_sfc)', (ROOT / 'src/noahmp_state.f90').read_text())
        loop = model[model.index(' do it=1,nsteps\n  step_start_s'):]
        self.assertNotIn('gmt=', loop)

if __name__ == '__main__':
    unittest.main()
