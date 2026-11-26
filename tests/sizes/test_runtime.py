"""Card-only bounds and sentinel scalars, read from the source text without a card compiler."""
import re
import unittest
from pathlib import Path
import audit

ROOT = Path(__file__).resolve().parents[2]


class RuntimeTest(unittest.TestCase):
    def test_routine_bounds(self):
        for path in list((ROOT / 'src').glob('*.f90')) + list((ROOT / 'src/device').glob('*.f90')):
            self.assertEqual(audit.routine_lines(path), [], str(path))

    def test_host_monthly_bounds(self):
        text = (ROOT / 'src/phys_dev.f90').read_text()
        for name in ('radiation_device_start', 'run_radiation', 'radiation_check'):
            body = text.split(' subroutine ' + name + '(')[1].split(' end subroutine')[0]
            self.assertIn('monthly(12,59,ncells)', body)

    def test_device_sentinel_has_value(self):
        # Loop bounds are evaluated at launch; an index inside the body needs its own device scalar.
        for name in ('dyn_acoustic', 'dyn_scalars', 'dyn_step'):
            path = ROOT / ('src/device/' + name + '.f90')   # the card's form of a kernel, where it has its own file
            if not path.exists():
                path = ROOT / ('src/' + name + '.f90')
            lines = path.read_text().splitlines()
            directive = ''
            for i, line in enumerate(lines, 1):
                if re.match(r'\s*!\$acc\s+(parallel|kernels)\b', line):
                    directive = line
                if re.match(r'\s*(rho_zz\(k,nc1\)|scalar_(old|new)\(k,nc1\)|f%theta_m[12]\((:|k),nc1\))\s*=', line):
                    self.assertIn('firstprivate(nc1)', directive, f'{name}:{i}')


if __name__ == '__main__':
    unittest.main()
