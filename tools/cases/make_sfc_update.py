#!/usr/bin/env python3
"""Make x1_2562's surface-update file, a test of config_sst_update with sea ice: water that freezes, ice that melts,
ice that changes.

  python3 tools/cases/make_sfc_update.py data/cases/x1_2562/inputs/x1.2562.init.nc OUT_DIR

Writes OUT_DIR/x1.2562.sfc_update.nc the way init_atmosphere writes one: CDF-5, dimensions StrLen, Time (unlimited), nCells,
variables xtime, Time, sst, xice with the attributes of greece_60km's surface file, and the init's global attributes.
Records 00:00, 03:00 and 06:00 (the same 3 h spacing as greece's file). Cells are chosen by a fixed rule: lowest index first.

  00:00  the init's sst and xice (nothing changes).
  03:00  sst + 0.5 K on water. 12 water cells next to sea ice get xice 0.6 (water to ice). Of the sea-ice cells:
         the 4 full cells (xice 1) go to 0.5, then 6 go to 0 and 6 to 0.015 (ice to water; the threshold is 0.02), then
         20 go to 0.7 of their xice (rescale). A cell that melts gets sst 272 K when its init sst is lower (a melted cell
         must not be colder than freezing sea water).
  06:00  sst + 1.0 K on water. The 12 new cells go back to 0. 6 of the cleared cells (the ones at 0 at 03:00) get 0.4.

For each record it prints the counts MPAS's physics_update_sst sees (mpas_atmphys_update_surface.F:114-326), with the
state carried from record to record (xicem, xland) as MPAS carries it:
  to_land   xland >= 1.5, xice >= threshold, xicem < threshold  (:229-231, water becomes sea ice)
  to_ocean  xland < 1.5, xice < threshold, xicem >= threshold   (:261-263, sea ice becomes water)
  removed   xice < threshold and xicem < threshold               (:299-301, includes every land cell)
  rescaled  xice /= xicem and xicem >= threshold                 (:208-209, sfc_albedo and sfc_emiss rescaled)
"""
import argparse
import sys
from pathlib import Path

import netCDF4
import numpy as np

THRESHOLD = np.float32(0.02)   # mpas_atmphys_manager.F:741-743 with config_frac_seaice true
HOURS = (0, 3, 6)
STAMP = '2026-11-08_{:02d}:00:00'
NAME = 'x1.2562.sfc_update.nc'
ATTRIBUTES = {
    'xtime': {'units': 'YYYY-MM-DD_hh:mm:ss', 'long_name': 'Model valid time'},
    'Time': {'units': 'seconds since 2026-11-08 00:00:00', 'long_name': 'CF-compliant valid time',
             'standard_name': 'time'},
    'sst': {'units': 'K', 'long_name': 'sea-surface temperature'},
    'xice': {'units': 'unitless', 'long_name': 'fractional area coverage of sea-ice'},
}


def first(mask, count):
    """The lowest `count` indices where mask is true."""
    found = np.flatnonzero(mask)
    if len(found) < count:
        raise SystemExit(f'make_sfc_update: only {len(found)} cells for a group of {count}')
    return found[:count]


def records(init):
    """sst and xice of the three records, from the arrays of the init file."""
    sst0 = np.array(init['sst'][0], dtype=np.float32)
    xice0 = np.array(init['xice'][0], dtype=np.float32)
    xland = np.array(init['xland'][0], dtype=np.float32)
    on_cell = np.array(init['cellsOnCell'][:])
    edges = np.array(init['nEdgesOnCell'][:])
    ice = xice0 >= THRESHOLD
    water = xland >= 1.5
    full = first(ice & (xice0 == 1.0), 4)
    next_to_ice = np.zeros(len(xice0), dtype=bool)
    for cell in np.flatnonzero(water):
        neighbours = on_cell[cell, :edges[cell]] - 1           # cellsOnCell is 1-based, 0 for none
        next_to_ice[cell] = any(n >= 0 and ice[n] for n in neighbours)
    freeze = first(next_to_ice, 12)
    used = np.zeros(len(xice0), dtype=bool)
    used[full] = True
    to_zero = first(ice & ~used, 6)
    used[to_zero] = True
    to_small = first(ice & ~used, 6)
    used[to_small] = True
    to_scale = first(ice & ~used & (0.7 * xice0 >= 2 * THRESHOLD), 20)   # keep 0.7 * xice well above the threshold

    sst3 = sst0.copy()
    sst3[water] += np.float32(0.5)
    xice3 = xice0.copy()
    xice3[freeze] = 0.6
    xice3[full] = 0.5
    xice3[to_zero] = 0.0
    xice3[to_small] = 0.015
    xice3[to_scale] = np.float32(0.7) * xice0[to_scale]
    melt = np.concatenate([to_zero, to_small])
    sst3[melt] = np.maximum(sst3[melt], np.float32(272.0))

    sst6 = sst0.copy()
    sst6[water] += np.float32(1.0)
    sst6[melt] = np.maximum(sst6[melt], np.float32(272.0))
    xice6 = xice3.copy()
    xice6[freeze] = 0.0
    xice6[to_zero] = 0.4
    return [(sst0, xice0), (sst3, xice3), (sst6, xice6)], xice0, xland


def counts(xice_records, xice0, xland0):
    """Per record: to_land, to_ocean, removed, rescaled (the loops of physics_update_sst, on whole arrays)."""
    xicem = np.where(xice0 < THRESHOLD, np.float32(0), xice0)
    xland = xland0.copy()
    result = []
    for xice in xice_records:
        xice = np.where(xice < THRESHOLD, np.float32(0), xice)
        rescaled = (xice != xicem) & (xicem >= THRESHOLD)
        to_land = (xland >= 1.5) & (xice >= THRESHOLD) & (xicem < THRESHOLD)
        to_ocean = (xland < 1.5) & (xice < THRESHOLD) & (xicem >= THRESHOLD)
        removed = (xice < THRESHOLD) & (xicem < THRESHOLD)
        result.append(tuple(int(m.sum()) for m in (to_land, to_ocean, removed, rescaled)))
        xland = np.where(to_land, np.float32(1), np.where(to_ocean, np.float32(2), xland))
        xicem = xice
    return result


def write(init, path, data):
    with netCDF4.Dataset(path, 'w', format='NETCDF3_64BIT_DATA') as out:
        out.createDimension('StrLen', 64)
        out.createDimension('Time', None)
        out.createDimension('nCells', len(init.dimensions['nCells']))
        xtime = out.createVariable('xtime', 'S1', ('Time', 'StrLen'))
        time = out.createVariable('Time', 'f4', ('Time',))
        sst = out.createVariable('sst', 'f4', ('Time', 'nCells'))
        xice = out.createVariable('xice', 'f4', ('Time', 'nCells'))
        for variable in (xtime, time, sst, xice):
            variable.setncatts(ATTRIBUTES[variable.name])
        out.setncatts({name: init.getncattr(name) for name in init.ncattrs()})
        for k, hour in enumerate(HOURS):
            xtime[k, :] = np.frombuffer(STAMP.format(hour).ljust(64).encode('ascii'), dtype='S1')
            time[k] = hour * 3600
            sst[k, :] = data[k][0]
            xice[k, :] = data[k][1]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('init', type=Path)
    parser.add_argument('out_dir', type=Path)
    args = parser.parse_args(argv)
    if not args.out_dir.is_dir():
        print(f'make_sfc_update: output directory must exist: {args.out_dir}', file=sys.stderr)
        return 2
    with netCDF4.Dataset(args.init) as init:
        data, xice0, xland0 = records(init)
        write(init, args.out_dir / NAME, data)
    for hour, (to_land, to_ocean, removed, rescaled) in zip(HOURS, counts([d[1] for d in data], xice0, xland0)):
        print(f'record {hour:02d}:00 to_land {to_land} to_ocean {to_ocean} removed {removed} rescaled {rescaled}')
    print(f'wrote {args.out_dir / NAME}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
