"""tools/renumber_grid.py on a small regional grid: the renumbered grid passes the round-trip and permutation checks, and a
broken connectivity is caught."""
import contextlib
import io
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GRID = ROOT / 'data/cases/greece_60km/inputs/greece_60km.grid.nc'
try:
    import netCDF4  # noqa: F401
    import numpy as np
    import sys
    sys.path.insert(0, str(ROOT / 'tools'))
    import renumber_grid
except ImportError:   # the test needs netCDF4 and numpy
    renumber_grid = None


@unittest.skipIf(renumber_grid is None or not GRID.exists(), 'needs netCDF4 and data/cases/greece_60km')
class RenumberGridTest(unittest.TestCase):
    def test_renumbered_grid_checks(self):
        with tempfile.TemporaryDirectory() as tmp:
            new = Path(tmp) / 'g.grid.nc'
            perms = renumber_grid.renumber(GRID, new)
            self.assertFalse((perms['cell'] == np.arange(len(perms['cell']))).all())
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(renumber_grid.check(GRID, new), 0)
            # a wrong edge of a cell is caught
            with netCDF4.Dataset(new, 'a') as g:
                eoc = g['edgesOnCell'][:]
                eoc[0, 0], eoc[0, 1] = eoc[0, 1], eoc[0, 0]
                g['edgesOnCell'][:] = eoc
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(renumber_grid.check(GRID, new), 1)


if __name__ == '__main__':
    unittest.main()
