"""Compare lean outputs bit for bit, excluding run-identifying NetCDF attributes."""
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


RUN_ATTRIBUTES = {'file_id', 'parent_id', 'history',
                  'config_do_restart', 'config_start_time', 'config_run_duration'}


def attributes(obj, permitted=()):
    return {name: (np.asarray(obj.getncattr(name)).dtype.str,
                   np.asarray(obj.getncattr(name)).tobytes())
            for name in obj.ncattrs() if name not in permitted}


def compare_variable(left, right):
    assert left.dimensions == right.dimensions, f'{left.name}: dimensions differ'
    assert left.dtype == right.dtype, f'{left.name}: type differs'
    assert attributes(left) == attributes(right), f'{left.name}: attributes differ'
    left.set_auto_maskandscale(False)
    right.set_auto_maskandscale(False)
    left.set_auto_chartostring(False)
    right.set_auto_chartostring(False)
    x, y = np.asarray(left[:]), np.asarray(right[:])
    assert x.shape == y.shape, f'{left.name}: shapes differ'
    if x.tobytes() != y.tobytes():
        words = np.dtype((np.void, x.dtype.itemsize))
        ix = np.flatnonzero(x.ravel().view(words) != y.ravel().view(words))
        first = np.unravel_index(ix[0], x.shape)
        raise AssertionError(f'{left.name}: values differ at {len(ix)}/{x.size} values, '
                             f'first {first}: {x[first]!r} vs {y[first]!r}')


def compare_netcdf(left, right):
    with Dataset(left) as a, Dataset(right) as b:
        aa, bb = attributes(a, RUN_ATTRIBUTES), attributes(b, RUN_ATTRIBUTES)
        changed = sorted(name for name in aa.keys() | bb.keys() if aa.get(name) != bb.get(name))
        assert not changed, f'{Path(left).name}: global attributes differ: {changed}'
        assert set(a.variables) == set(b.variables) and a.variables, f'{Path(left).name}: variables differ'
        for name in a.variables:
            compare_variable(a[name], b[name])
        return len(a.variables)
