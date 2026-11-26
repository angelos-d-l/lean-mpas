#!/usr/bin/env python3
"""A mesh file with a case's dimensions scaled to N cells and the memory check's three count fields, nothing else:
xice 0 and xland 1 on the one Time record, ivgtyp 1 (no glacier). The program's memory check reads only dimensions
and these fields, so it can be tested at sizes no real case file has, in a file of a few bytes per cell.

    make_dims_only.py CASE OUT --ncells N        CASE: greece (data/cases/greece_60km) or x1 (data/cases/x1_2562)

Needs netCDF4 (a Python with numpy and netCDF4). nEdges and nVertices keep the case's ratios to nCells.
"""
import argparse
import sys
from pathlib import Path

import netCDF4
import numpy as np

ROOT = Path(__file__).resolve().parents[2]
INITS = {'greece': 'data/cases/greece_60km/inputs/greece_60km.init.nc', 'x1': 'data/cases/x1_2562/inputs/x1.2562.init.nc'}


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    parser.add_argument('case', choices=sorted(INITS))
    parser.add_argument('out', type=Path)
    parser.add_argument('--ncells', type=int, required=True)
    args = parser.parse_args(argv)
    with netCDF4.Dataset(ROOT / INITS[args.case]) as src:
        dims = {name: (None if dim.isunlimited() else len(dim)) for name, dim in src.dimensions.items()}
    scale = args.ncells / dims['nCells']
    dims['nEdges'] = int(round(dims['nEdges'] * scale))
    dims['nVertices'] = int(round(dims['nVertices'] * scale))
    dims['nCells'] = args.ncells
    with netCDF4.Dataset(args.out, 'w', format='NETCDF3_64BIT_DATA') as out:
        for name, size in dims.items():
            out.createDimension(name, size)
        out.createVariable('xice', 'f4', ('Time', 'nCells'))[0, :] = np.zeros(args.ncells, 'f4')
        out.createVariable('xland', 'f4', ('Time', 'nCells'))[0, :] = np.ones(args.ncells, 'f4')
        out.createVariable('ivgtyp', 'i4', ('nCells',))[:] = np.ones(args.ncells, 'i4')
    return 0


if __name__ == '__main__':
    sys.exit(main())
