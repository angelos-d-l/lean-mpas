"""A lean restart file against MPAS's restart file of the same run: every variable both files hold (lean_* aside), bit
for bit, except the measured compiler allowances of tests/writer/t0_values.py (ANY_BITS, and GFORTRAN_BITS for
--fc gfortran) and the departures listed in KNOWN. A variable that MPAS stores without Time and lean with one record
is compared without the Time axis.

    compare_restart.py LEAN_RESTART MPAS_RESTART --fc gfortran|nvfortran     RESTART VALUES OK | FAIL, exit 0 | 1
"""
import argparse
from pathlib import Path
import sys

import numpy as np
from netCDF4 import Dataset

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from tests.writer.t0_values import ANY_BITS, GFORTRAN_BITS

# Departures from MPAS's restart file that lean makes on purpose, with the reason (docs/restart.md).
KNOWN = {'dss': "lean holds the input's copy; MPAS writes the damping profile it derives at start"}


def bit_difference(x, y):
    if x.shape != y.shape or x.dtype != y.dtype:
        return max(x.size, y.size), float('inf')
    words = np.dtype((np.void, x.dtype.itemsize))
    changed = x.ravel().view(words) != y.ravel().view(words)
    count = int(np.count_nonzero(changed))
    if not count or x.dtype.kind not in 'fiu':
        return count, 0.0
    with np.errstate(invalid='ignore'):
        delta = np.abs(x.ravel()[changed].astype('f8') - y.ravel()[changed].astype('f8'))
    return count, float(delta.max())


def values(variable):
    variable.set_auto_maskandscale(False)
    variable.set_auto_chartostring(False)
    return np.asarray(variable[:])


def paired_values(a, b):
    x, y = values(a), values(b)
    ax, by = a.dimensions, b.dimensions
    # MPAS stores some invariants once, while the lean restart walk records them.
    if ax == ('Time', *by) and x.shape[0] == 1:
        x, ax = x[0], ax[1:]
    elif by == ('Time', *ax) and y.shape[0] == 1:
        y, by = y[0], by[1:]
    return x, y, ax == by


def compare(lean, mpas, fc):
    allowed = dict(next((fields for mesh, fields in ANY_BITS.items() if mesh in str(mpas)), {}))
    if fc == 'gfortran':
        allowed.update(next((fields for mesh, fields in GFORTRAN_BITS.items() if mesh in str(mpas)), {}))
    failures = 0
    with Dataset(lean) as left, Dataset(mpas) as right:
        names = [name for name in left.variables
                 if not name.startswith('lean_') and name in right.variables]
        for name in names:
            a, b = left[name], right[name]
            x, y, same_dims = paired_values(a, b)
            count, maximum = bit_difference(x, y)
            if not same_dims:
                count, maximum = max(x.size, y.size), float('inf')
            limit, count_limit = allowed.get(name, (0, 0))
            if x.dtype.kind != 'f':
                limit, count_limit = 0, 0
            if count and name in KNOWN:
                print(f'KNOWN {name} {count} {maximum:.17g}: {KNOWN[name]}')
                continue
            if count and not (count <= count_limit and maximum <= limit):
                print(f'DIFF {name} {count} {maximum:.17g}')
                failures += 1
        omitted = len(left.variables.keys() - right.variables.keys())
    if failures or not names:
        print('RESTART VALUES FAIL')
        return False
    print(f"RESTART VALUES OK {len(names)} fields; {omitted} lean fields not in MPAS's file")
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('lean', type=Path)
    parser.add_argument('mpas', type=Path)
    parser.add_argument('--fc', required=True, choices=('gfortran', 'nvfortran'))
    args = parser.parse_args()
    try:
        return 0 if compare(args.lean, args.mpas, args.fc) else 1
    except (OSError, RuntimeError) as error:
        print(error, file=sys.stderr)
        print('RESTART VALUES FAIL')
        return 1


if __name__ == '__main__':
    sys.exit(main())
