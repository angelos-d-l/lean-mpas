"""compare_runs.py RUN_A RUN_B: the NetCDF output files of two run directories, value for value.

For every history, diag and restart file of RUN_A (or the files named with --files) it reads the file of the same name
in RUN_B and compares the dimensions and every variable bit for bit. It prints one line per file: `identical`, or the
variables that differ with the number of differing values and the largest absolute difference (the first six;
--all lists every one). Global attributes are not compared: MPAS writes a random file_id into every file, and the
build's version string changes with every commit.

Use it to see what a change did: run a case before and after the change, then compare the two run directories. A
change that should not alter the forecast must give `identical` on every file.

Needs a Python with netCDF4 and numpy (the one in LEAN_PY). Exit status 0 when every file is identical, 1 otherwise.
"""
import argparse
import sys
from pathlib import Path

import netCDF4
import numpy as np

PREFIXES = ('history.', 'diag.', 'restart.')


def output_files(run):
    return sorted(f.name for f in run.iterdir()
                  if f.is_file() and not f.is_symlink() and f.suffix == '.nc' and f.name.startswith(PREFIXES))


def compare_file(a_path, b_path):
    """The differences of two files as a list of text notes; empty when they hold the same data."""
    notes = []
    with netCDF4.Dataset(a_path) as a, netCDF4.Dataset(b_path) as b:
        a.set_auto_mask(False)
        b.set_auto_mask(False)
        dims_a = {name: len(dim) for name, dim in a.dimensions.items()}
        dims_b = {name: len(dim) for name, dim in b.dimensions.items()}
        if dims_a != dims_b:
            notes.append('dimensions differ')
        for name in sorted(set(a.variables) - set(b.variables)):
            notes.append(f'{name} only in the first')
        for name in sorted(set(b.variables) - set(a.variables)):
            notes.append(f'{name} only in the second')
        for name in sorted(set(a.variables) & set(b.variables)):
            x, y = a.variables[name][...], b.variables[name][...]
            if x.shape != y.shape or x.dtype != y.dtype:
                notes.append(f'{name} shape or type differs')
            elif x.tobytes() != y.tobytes():
                if x.dtype.kind in 'fiu':
                    # count by bit pattern, so that a NaN against the same NaN is not a difference
                    width = x.dtype.itemsize
                    count = int((x.view(f'V{width}') != y.view(f'V{width}')).sum())
                    with np.errstate(all='ignore'):
                        largest = float(np.nanmax(np.abs(x.astype('f8') - y.astype('f8'))))
                    notes.append(f'{name} {count} of {x.size} values, largest difference {largest:.3g}')
                else:
                    notes.append(f'{name} differs')
    return notes


def main():
    parser = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    parser.add_argument('run_a', type=Path)
    parser.add_argument('run_b', type=Path)
    parser.add_argument('--files', nargs='+', metavar='NAME',
                        help='compare these file names instead of the history, diag and restart files of RUN_A')
    parser.add_argument('--all', action='store_true', help='list every differing variable, not only the first six')
    args = parser.parse_args()
    names = args.files or output_files(args.run_a)
    if not names:
        sys.exit(f'no history, diag or restart files in {args.run_a}')
    differing = 0
    for name in names:
        if not (args.run_b / name).is_file():
            notes = ['missing in the second run']
        else:
            notes = compare_file(args.run_a / name, args.run_b / name)
        differing += bool(notes)
        if len(notes) > 6 and not args.all:
            notes = notes[:6] + [f'and {len(notes) - 6} more (--all lists them)']
        print(f'{name}: ' + ('; '.join(notes) if notes else 'identical'))
    extra = sorted(set(output_files(args.run_b)) - set(names)) if not args.files else []
    for name in extra:
        differing += 1
        print(f'{name}: only in the second run')
    print(f'{len(names) + len(extra) - differing} identical, {differing} differ')
    sys.exit(1 if differing else 0)


if __name__ == '__main__':
    main()
