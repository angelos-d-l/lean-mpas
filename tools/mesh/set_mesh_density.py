"""Write meshDensity of an MPAS mesh from its cell sizes, in place, in every file given (grid, static, init of one mesh).

  python set_mesh_density.py LEN_DISP_M FILE [FILE ...]      e.g. 3000 mesh.grid.nc mesh.static.nc mesh.init.nc

MPAS (mpas_atm_core.F: meshScalingDel2/Del4/RegionalEdge; the convection driver's dx = len_disp / meshDensity^0.25)
and lean (dyn_startup.f90 mesh_scaling, cold_column.f90) take each cell's size from meshDensity. A mesh made with
mpas_tools' build_spherical_mesh can have 1 in every cell, so every cell is treated as the finest.
Here meshDensity = min(1, (LEN_DISP / dx)^4), dx = sqrt(areaCell / (sqrt(3)/2)), the spacing of a regular hexagon of the
cell's area; so len_disp / meshDensity^0.25 = dx for every cell at least LEN_DISP wide, and LEN_DISP for the finest cells.
The cells must be the same in every file (checked on areaCell).
"""
import argparse
import sys
from pathlib import Path

import numpy as np
import netCDF4 as nc

EARTH_RADIUS = 6371229.0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('len_disp', metavar='LEN_DISP_M', type=float, help='the finest cell spacing, m')
    ap.add_argument('files', metavar='FILE', nargs='+', help='grid, static and init files of one mesh, changed in place')
    a = ap.parse_args()
    len_disp, files = a.len_disp, a.files
    for f in files:
        if not Path(f).is_file():
            sys.exit(f'set_mesh_density.py: {f}: no such file')
    ref = None
    for f in files:
        with nc.Dataset(f, 'r+') as d:
            area = np.asarray(d['areaCell'][:], float)
            # a grid.nc on the unit sphere (as MPAS distributes them; static and init files are in m2): scale to the Earth
            if area.max() < 1.0:
                area = area * EARTH_RADIUS ** 2
            if ref is None:
                ref = area
                dx = np.sqrt(area / (np.sqrt(3) / 2))
                dens = np.minimum(1.0, (len_disp / dx) ** 4)
            assert np.allclose(area, ref, rtol=1e-5), f'{f}: other cells than {files[0]}'
            old = np.asarray(d['meshDensity'][:], float)
            d['meshDensity'][:] = dens
            print(f'{f}: meshDensity was {old.min():.3g}-{old.max():.3g}, now {dens.min():.3g}-{dens.max():.3g}; '
                  f'dx {dx.min():.0f}-{dx.max():.0f} m, {int((dens == 1).sum())} cells at 1')


if __name__ == '__main__':
    main()
