#!/usr/bin/env python3
"""Two small surface files for tests/surface/units.f90 (file_ice, read_record): records 00:00, 03:00, 06:00 on NCELLS cells.

  python3 tests/surface/make_units_files.py OUT_DIR NCELLS

ice.sfc_update.nc: xice 0.6 on cell 30 (1-based) at 03:00 only; noice.sfc_update.nc: no ice. sst[record k, cell c (1-based)] = 250 + k + 0.001*c.
"""
import sys
from pathlib import Path

import netCDF4
import numpy as np


def write(path, n, ice):
    with netCDF4.Dataset(path, 'w', format='NETCDF3_64BIT_DATA') as out:
        out.createDimension('StrLen', 64)
        out.createDimension('Time', None)
        out.createDimension('nCells', n)
        xtime = out.createVariable('xtime', 'S1', ('Time', 'StrLen'))
        sst = out.createVariable('sst', 'f4', ('Time', 'nCells'))
        xice = out.createVariable('xice', 'f4', ('Time', 'nCells'))
        for k, hour in enumerate((0, 3, 6)):
            xtime[k, :] = np.frombuffer(f'2026-11-08_{hour:02d}:00:00'.ljust(64).encode(), dtype='S1')
            sst[k, :] = 250 + k + 0.001 * np.arange(1, n + 1, dtype=np.float32)
            xice[k, :] = 0
            if ice and k == 1:
                xice[k, 29] = 0.6


if __name__ == '__main__':
    out, n = Path(sys.argv[1]), int(sys.argv[2])
    write(out / 'ice.sfc_update.nc', n, True)
    write(out / 'noice.sfc_update.nc', n, False)
