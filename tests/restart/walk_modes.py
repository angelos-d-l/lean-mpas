"""The restart round trip of every variable: gives each variable of before.nc (written by tests/restart/units.f90 in
the current directory) its own nonzero bits, runs `units restore` (read the file, write after.nc) and requires after.nc
to equal before.nc in every variable the walk counts. Run by tests/restart/run.sh."""
from pathlib import Path
import re
import subprocess
import sys

import numpy as np
from netCDF4 import Dataset

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from tests.restart.compare_runs import compare_netcdf

with Dataset('before.nc', 'r+') as ds:
    ds.set_auto_maskandscale(False)
    ds.set_auto_chartostring(False)
    assert len(ds.variables) > 700
    assert 'lean_ghost_u' in ds.variables and 'lean_ghost_qv' in ds.variables
    for j, var in enumerate(ds.variables.values(), 1):
        if var.dtype.kind == 'S':
            var[0, :] = np.frombuffer(f'variable {j}: walk_modes'.ljust(64).encode(), dtype='S1')
        elif var.name in {'lean_seaice_any'} or 'flag' in var.name:
            var[:] = j % 2
        elif var.dtype.kind in 'iu':
            var[:] = np.arange(var.size).reshape(var.shape) + j
        else:
            var[:] = (np.arange(var.size).reshape(var.shape) + j) / 128
result = subprocess.check_output(['./units', 'restore'], text=True)
count = int(re.search(r'walk variables (\d+)', result)[1])
assert compare_netcdf('before.nc', 'after.nc') == count
print('ok   walk_modes', count)
print(result.strip().splitlines()[-1])
