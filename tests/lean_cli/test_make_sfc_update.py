"""tools/cases/make_sfc_update.py: the surface-update file of the x1_2562 case."""
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

try:
    import netCDF4
    import numpy as np
    HAVE_ARRAYS = True
except ImportError:
    HAVE_ARRAYS = False

ROOT = Path(__file__).resolve().parents[2]
TOOL = ROOT / 'tools' / 'cases' / 'make_sfc_update.py'
INIT = ROOT / 'data' / 'cases' / 'x1_2562' / 'inputs' / 'x1.2562.init.nc'
GREECE = ROOT / 'data' / 'cases' / 'greece_60km' / 'inputs' / 'greece_60km.sfc_update.nc'
LINE = re.compile(r'record (\d\d):00 to_land (\d+) to_ocean (\d+) removed (\d+) rescaled (\d+)')


def counts_by_cell(path, xland, xice_init):
    """MPAS's branches of physics_update_sst, one cell at a time (mpas_atmphys_update_surface.F:200-301)."""
    with netCDF4.Dataset(path) as data:
        xice_records = np.array(data['xice'][:], dtype=np.float32)
    threshold = np.float32(0.02)
    xicem = [x if x >= threshold else np.float32(0) for x in xice_init]
    land = [float(x) for x in xland]
    result = []
    for record in xice_records:
        to_land = to_ocean = removed = rescaled = 0
        for c, x in enumerate(record):
            x = x if x >= threshold else np.float32(0)
            if x != xicem[c] and xicem[c] >= threshold:
                rescaled += 1
            if land[c] >= 1.5 and x >= threshold and xicem[c] < threshold:
                to_land += 1
                land[c] = 1.0
            elif land[c] < 1.5 and x < threshold and xicem[c] >= threshold:
                to_ocean += 1
                land[c] = 2.0
            elif x < threshold and xicem[c] < threshold:
                removed += 1
            xicem[c] = x
        result.append((to_land, to_ocean, removed, rescaled))
    return result


@unittest.skipUnless(HAVE_ARRAYS, 'needs netCDF4 and numpy')
@unittest.skipUnless(INIT.is_file() and GREECE.is_file(), 'needs the x1 init and greece surface file under data/cases')
class MakeSfcUpdateTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        (ROOT / 'build').mkdir(exist_ok=True)
        cls.scratch = tempfile.TemporaryDirectory(dir=ROOT / 'build')
        cls.out = Path(cls.scratch.name)
        run = subprocess.run([sys.executable, str(TOOL), str(INIT), str(cls.out)], capture_output=True, text=True)
        cls.run_ = run
        cls.printed = [tuple(int(v) for v in m.groups()) for m in LINE.finditer(run.stdout)]
        cls.path = cls.out / 'x1.2562.sfc_update.nc'

    @classmethod
    def tearDownClass(cls):
        cls.scratch.cleanup()

    def test_the_script_succeeds_and_prints_three_records(self):
        self.assertEqual(self.run_.returncode, 0, self.run_.stderr)
        self.assertEqual([p[0] for p in self.printed], [0, 3, 6])

    def test_printed_counts_equal_a_count_made_from_the_file(self):
        with netCDF4.Dataset(INIT) as init:
            xland = np.array(init['xland'][0], dtype=np.float32)
            xice = np.array(init['xice'][0], dtype=np.float32)
        independent = counts_by_cell(self.path, xland, xice)
        self.assertEqual([p[1:] for p in self.printed], independent)

    def test_every_branch_of_the_update_is_used(self):
        later = self.printed[1:]
        self.assertEqual(self.printed[0][1:3], (0, 0))
        self.assertEqual(self.printed[0][4], 0)
        self.assertEqual((later[0][1], later[0][2], later[0][4]), (12, 12, 4 + 12 + 20))
        self.assertEqual((later[1][1], later[1][2]), (6, 12))

    def test_structure_equals_greeces_surface_file(self):
        with netCDF4.Dataset(self.path) as mine, netCDF4.Dataset(GREECE) as greece:
            self.assertEqual(mine.data_model, greece.data_model)
            self.assertEqual(mine.data_model, 'NETCDF3_64BIT_DATA')
            self.assertEqual({k: (v.isunlimited(), len(v) if k != 'nCells' else 0) for k, v in mine.dimensions.items()},
                             {k: (v.isunlimited(), len(v) if k != 'nCells' else 0) for k, v in greece.dimensions.items()}
                             | {'Time': (True, 3)})
            self.assertEqual(list(mine.variables), list(greece.variables))
            for name, variable in greece.variables.items():
                got = mine[name]
                self.assertEqual((got.dtype, got.dimensions), (variable.dtype, variable.dimensions), name)
                self.assertEqual({a: got.getncattr(a) for a in got.ncattrs()},
                                 {a: variable.getncattr(a) for a in variable.ncattrs()}, name)
            self.assertEqual(set(mine.ncattrs()), set(greece.ncattrs()))

    def test_global_attributes_and_times_come_from_the_init_and_the_spacing(self):
        with netCDF4.Dataset(self.path) as mine, netCDF4.Dataset(INIT) as init:
            for name in init.ncattrs():
                self.assertEqual(mine.getncattr(name), init.getncattr(name), name)
            self.assertEqual(list(mine['Time'][:]), [0.0, 10800.0, 21600.0])
            stamps = [str(t).strip() for t in netCDF4.chartostring(mine['xtime'][:])]
            self.assertEqual(stamps, ['2026-11-08_00:00:00', '2026-11-08_03:00:00', '2026-11-08_06:00:00'])
            self.assertTrue(np.array_equal(mine['sst'][0], init['sst'][0]))
            self.assertTrue(np.array_equal(mine['xice'][0], init['xice'][0]))


if __name__ == '__main__':
    unittest.main()
