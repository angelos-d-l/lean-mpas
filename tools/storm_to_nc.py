"""Convert LEAN_STORM: python -I tools/storm_to_nc.py STREAM MESH_NC OUT_NC.

Native-endian int32/real32 stream; see docs/storm-stream.md.
"""
from pathlib import Path
import argparse
import struct

import netCDF4
import numpy as np

# name: (units, long_name, the MPAS field it corresponds to). For the running maxima the third column is the name of
# the MPAS field (the mpas_name attribute); the instantaneous fields have no MPAS counterpart and say instead how they
# relate to one (the mpas_method attribute).
FIELDS = {
    'uh_max': ('m2 s-2', 'Maximum 2-5 km AGL updraft helicity since previous record', 'updraft_helicity_max'),
    'w_max': ('m s-1', 'Maximum vertical velocity since previous record (faces 1 through nz)', 'w_velocity_max'),
    'uh': ('m2 s-2', 'Instantaneous 2-5 km AGL updraft helicity at record step',
           'as updraft_helicity_max, instantaneous at the record step'),
    'w_col': ('m s-1', 'Instantaneous column maximum vertical velocity (faces 1 through nz)',
              'as w_velocity_max, instantaneous at the record step'),
}
INSTANTANEOUS = ('uh', 'w_col')


def read_stream(path):
    """Return (dt, names, records), with records backed by a read-only memmap."""
    path = Path(path)
    with path.open('rb') as f:
        header = f.read(12)
        if len(header) != 12:
            raise ValueError('truncated storm header')
        nc, nf, dt = struct.unpack('=iif', header)
        if nc <= 0 or not 0 < nf <= len(FIELDS) or not np.isfinite(dt) or dt <= 0:
            raise ValueError('invalid storm dimensions or dt')
        raw_names = f.read(32 * nf)
    if len(raw_names) != 32 * nf:
        raise ValueError('truncated storm names')
    try:
        names = tuple(raw_names[i:i+32].decode('ascii').rstrip() for i in range(0, len(raw_names), 32))
    except UnicodeDecodeError as e:
        raise ValueError('invalid storm field name') from e
    if len(set(names)) != nf or any(n not in FIELDS for n in names):
        raise ValueError('unknown or duplicate storm field')
    offset = 12 + 32 * nf
    record_bytes = 4 + 4 * nf * nc
    remaining = path.stat().st_size - offset
    if remaining < 0 or remaining % record_bytes:
        raise ValueError('truncated storm record')
    raw = np.memmap(path, mode='r', dtype='u1')
    dtype = np.dtype([('it', '=i4'), ('fields', '=f4', (nf, nc))])
    records = np.ndarray((remaining // record_bytes,), dtype=dtype, buffer=raw, offset=offset)
    if np.any(records['it'] <= 0) or np.any(np.diff(records['it'].astype('i8')) <= 0):
        raise ValueError('storm steps must be positive and increasing')
    return dt, names, records


def convert(stream, mesh_nc, out_nc):
    if Path(out_nc).resolve() in (Path(stream).resolve(), Path(mesh_nc).resolve()):
        raise ValueError('output must differ from input')
    dt, names, records = read_stream(stream)
    nc = records['fields'].shape[2]
    with netCDF4.Dataset(mesh_nc) as mesh:
        if len(mesh.dimensions['nCells']) != nc:
            raise ValueError('mesh nCells differs from storm stream')
        lat = np.degrees(np.asarray(mesh['latCell'][:], dtype='f8'))
        lon = np.degrees(np.asarray(mesh['lonCell'][:], dtype='f8'))
    with netCDF4.Dataset(out_nc, 'w') as ds:
        ds.createDimension('time', None)
        ds.createDimension('nCells', nc)
        ds.dt_seconds = float(dt)
        ds.description = 'lean_mpas native-mesh storm diagnostics; time from forecast start'
        for name, values, units in (('lat', lat, 'degrees_north'), ('lon', lon, 'degrees_east')):
            var = ds.createVariable(name, 'f8', ('nCells',))
            var.units = units
            var[:] = values
        var = ds.createVariable('it', 'i4', ('time',))
        var.long_name = 'Model step from forecast start'
        var[:] = records['it']
        var = ds.createVariable('time', 'f8', ('time',))
        var.units = 's'
        var.long_name = 'Elapsed time from forecast start'
        var[:] = records['it'].astype('f8') * dt
        for i, name in enumerate(names):
            var = ds.createVariable(name, 'f4', ('time', 'nCells'), fill_value=False)
            var.units, var.long_name, mpas = FIELDS[name]
            if name in INSTANTANEOUS:
                var.mpas_method = mpas
            else:
                var.mpas_name = mpas
            for t, record in enumerate(records):
                var[t] = record['fields'][i]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('stream')
    parser.add_argument('mesh_nc')
    parser.add_argument('out_nc')
    args = parser.parse_args()
    convert(args.stream, args.mesh_nc, args.out_nc)


if __name__ == '__main__':
    main()
