"""Lean film stream (LEAN_FILM, src/film_output.f90) -> one diag.YYYY-MM-DD_HH.MM.SS.nc per output time, with the
variable names and layout of MPAS diag files (refl10cm_max, rainnc, rainc, u10, v10, mslp; Time x nCells, float32), so
tools that read MPAS diag files can read the film.

  python film_to_diag.py FILM OUTDIR --start YYYY-MM-DD_HH --dt SECONDS [--restart RESTART.nc]

--start is the start time of the run and --dt its time step (config_start_time and config_dt of namelist.atmosphere).
A run from a restart begins with rainc = 0: the restart's rainc (accumulated by the run that wrote it) is added, so
rainc means what it means in MPAS (accumulated since the model start). rainnc already carries it. A cold start (no
--restart) begins with both at 0.
"""
import argparse, os
from datetime import datetime, timedelta
import numpy as np
import netCDF4 as nc

NAMES = ['refl10cm_max', 'rainnc', 'rainc', 'u10', 'v10', 'mslp']
UNITS = ['dBZ', 'mm', 'mm', 'm s-1', 'm s-1', 'Pa']


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('film'); ap.add_argument('outdir'); ap.add_argument('--restart')
    ap.add_argument('--start', required=True, help='start time of the run, YYYY-MM-DD_HH')
    ap.add_argument('--dt', type=float, required=True, help='model time step in seconds (config_dt)')
    a = ap.parse_args()
    raw = open(a.film, 'rb').read()
    n = int(np.frombuffer(raw[:4], np.int32)[0])
    START = datetime.strptime(a.start, '%Y-%m-%d_%H')
    rainc0 = np.zeros(n, np.float32)
    if a.restart:
        with nc.Dataset(a.restart) as r:
            rainc0 = np.array(r['rainc'][0], np.float32)
    assert rainc0.shape == (n,), rainc0.shape
    os.makedirs(a.outdir, exist_ok=True)
    rec, off, count = 4 + 6 * n * 4, 4, 0
    while off + rec <= len(raw):
        it = int(np.frombuffer(raw[off:off + 4], np.int32)[0])
        f = np.frombuffer(raw[off + 4:off + rec], np.float32).reshape(6, n).copy()
        off += rec
        f[2] += rainc0
        # named after the 10-minute mark the step reached, as MPAS names its files (dt 18: 07:10:12 -> 07.10.00)
        t = START + timedelta(seconds=int(a.dt * it) // 600 * 600)
        with nc.Dataset(os.path.join(a.outdir, t.strftime('diag.%Y-%m-%d_%H.%M.%S.nc')), 'w') as d:
            d.createDimension('Time', None); d.createDimension('nCells', n)
            d.source = 'lean_mpas film output (src/film_output.f90), step %d' % it
            for name, unit, v in zip(NAMES, UNITS, f):
                x = d.createVariable(name, 'f4', ('Time', 'nCells')); x.units = unit; x[0, :] = v
        count += 1
    assert off == len(raw), 'trailing bytes'
    print('%d diag files, %s .. %s' % (count, (START + timedelta(minutes=10)).strftime('%H:%M'), t.strftime('%Y-%m-%d %H:%M')))


if __name__ == '__main__':
    main()
