"""tools/compare_runs.py on two small synthetic run directories; needs netCDF4 and numpy (LEAN_PY), no model run."""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import netCDF4
import numpy as np

ROOT = Path(__file__).resolve().parents[1]
TOOL = ROOT / 'tools/compare_runs.py'


def write(path, t2, file_id):
    with netCDF4.Dataset(path, 'w', format='NETCDF3_64BIT_DATA') as nc:
        nc.createDimension('nCells', len(t2))
        nc.createVariable('t2m', 'f4', ('nCells',))[:] = t2
        nc.file_id = file_id


class CompareRunsTest(unittest.TestCase):
    def run_tool(self, a, b):
        return subprocess.run([sys.executable, str(TOOL), str(a), str(b)], capture_output=True, text=True)

    def test_same_values_with_another_file_id_are_identical(self):
        with tempfile.TemporaryDirectory() as tmp:
            a, b = Path(tmp, 'a'), Path(tmp, 'b')
            a.mkdir(); b.mkdir()
            values = np.array([280., float('nan'), 290.], 'f4')
            write(a / 'history.x.nc', values, 'aaaaaaaaaa')
            write(b / 'history.x.nc', values, 'bbbbbbbbbb')
            done = self.run_tool(a, b)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            self.assertIn('history.x.nc: identical', done.stdout)

    def test_a_changed_value_and_a_missing_file_are_reported(self):
        with tempfile.TemporaryDirectory() as tmp:
            a, b = Path(tmp, 'a'), Path(tmp, 'b')
            a.mkdir(); b.mkdir()
            write(a / 'history.x.nc', np.array([280., 285., 290.], 'f4'), 'aaaaaaaaaa')
            write(b / 'history.x.nc', np.array([280., 285.5, 290.], 'f4'), 'aaaaaaaaaa')
            write(a / 'diag.x.nc', np.zeros(3, 'f4'), 'aaaaaaaaaa')
            done = self.run_tool(a, b)
            self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
            self.assertIn('t2m 1 of 3 values, largest difference 0.5', done.stdout)
            self.assertIn('diag.x.nc: missing in the second run', done.stdout)


if __name__ == '__main__':
    unittest.main()
