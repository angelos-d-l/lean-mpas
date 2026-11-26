"""tests/restart/compare_runs.py on small synthetic files (compare_restart.py is checked by the restart model runs), run under LEAN_PY (or PYTHON), which
needs netCDF4 and numpy."""
from pathlib import Path
import os
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
PYTHON = os.environ.get('LEAN_PY', os.environ.get('PYTHON', 'python3'))


class CompareRuns(unittest.TestCase):
    def test_values_and_atts(self):
        result = subprocess.run([PYTHON, '-c', """
from pathlib import Path
from tempfile import TemporaryDirectory
from netCDF4 import Dataset
from tests.restart.compare_runs import compare_netcdf

with TemporaryDirectory(dir='build') as directory:
    a, b = [Path(directory) / name for name in ('a.nc', 'b.nc')]
    for j, path in enumerate((a, b)):
        with Dataset(path, 'w') as ds:
            ds.createDimension('n', 2)
            ds.createVariable('field', 'f4', ('n',))[:] = [1, 2]
            for name in ('file_id', 'parent_id', 'history', 'config_do_restart',
                         'config_start_time', 'config_run_duration'):
                ds.setncattr(name, str(j))
            ds.config_dt = 120
    assert compare_netcdf(a, b) == 1
    with Dataset(b, 'a') as ds:
        ds.config_dt = 121
    try:
        compare_netcdf(a, b)
    except AssertionError as error:
        assert "global attributes differ: ['config_dt']" in str(error)
    else:
        raise AssertionError('changed config_dt accepted')
    with Dataset(b, 'a') as ds:
        ds.config_dt = 120
        ds['field'][1] = 3
    try:
        compare_netcdf(a, b)
    except AssertionError as error:
        assert 'field: values differ' in str(error)
    else:
        raise AssertionError('changed field accepted')
    with Dataset(b, 'a') as ds:
        ds['field'][1] = 2
        ds.lean_restart = 'wrong'
    try:
        compare_netcdf(a, b)
    except AssertionError as error:
        assert 'global attributes differ' in str(error)
    else:
        raise AssertionError('changed attribute accepted')
"""], cwd=ROOT, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()
