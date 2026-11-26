"""Fill the background albedo that MPAS 8's static step leaves at 0.08 % on small coast and island land cells, in place.

  python fix_albedo.py STATIC_NC

mpas_init_atm_static.F (MODIS albedo, about lines 1000-1097) adds the fill value 8.0 for missing pixels as a raw
integer and then scales by 0.01, so a land cell that falls only on missing (water) pixels of the 0.05 deg dataset gets
albedo12m = 0.08 (%) in every month, i.e. sfc_albbck 0.0008, and a cell with a few real pixels among the missing ones
0.3-1.1 %: a black coastline until Noah-MP sets its own albedo. On a 3 km mesh over Greece that is a few
hundred land cells, plus the diluted ones; finer meshes have more. Here land cells below 3 % (yearly mean) take, month by month, the mean
of their land neighbours with a real albedo, repeated outwards; islets with no such neighbour take the land median of
the month. Run it before the initial state is made.
"""
import argparse
import sys
from pathlib import Path

import numpy as np
import netCDF4 as nc


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('static_nc', metavar='STATIC_NC', help='the static file of the mesh, changed in place')
    a = ap.parse_args()
    if not Path(a.static_nc).is_file():
        sys.exit(f'fix_albedo.py: {a.static_nc}: no such file')
    with nc.Dataset(a.static_nc, 'r+') as s:
        a = np.asarray(s['albedo12m'][:], float)               # (nCells, nMonths)
        land = np.asarray(s['landmask'][:]) == 1
        noc = np.asarray(s['nEdgesOnCell'][:]); coc = np.asarray(s['cellsOnCell'][:]) - 1
        bad = land & (a.mean(axis=1) < 3.0)                   # 0.08 everywhere, or diluted by a few real pixels (0.3-1.1 %)
        good = land & ~bad
        todo = bad.copy()
        nb = np.where(np.arange(coc.shape[1])[None, :] < noc[:, None], coc, -1)
        for it in range(50):
            if not todo.any():
                break
            idx = np.where(todo)[0]
            n = nb[idx]
            ok = (n >= 0) & good[np.maximum(n, 0)]
            cnt = ok.sum(1)
            fill = idx[cnt > 0]
            if fill.size == 0:
                break
            sums = np.where(ok[cnt > 0, :, None], a[np.maximum(n[cnt > 0], 0)], 0.).sum(1)
            a[fill] = sums / cnt[cnt > 0, None]
            good[fill] = True; todo[fill] = False
        rest = int(todo.sum())                                 # islands with no land neighbour that has a real albedo
        if rest:
            a[todo] = np.median(a[land & ~bad], axis=0)
        s['albedo12m'][:] = a
        print(f'{int(bad.sum())} land cells had albedo12m below 3 % (yearly mean); filled {int(bad.sum() - todo.sum())} '
              f'from neighbours in {it} rings, {rest} (isolated islets) from the land median; their new October value {np.median(a[bad, 9]):.1f} % (median), '
              f'all land {np.median(a[land, 9]):.1f} %')


if __name__ == '__main__':
    main()
