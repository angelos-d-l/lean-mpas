"""Convert LEAN_SURFACE output: python -I tools/surface_to_nc.py STREAM OUT_NC.

32-bit little-endian stream (every supported build machine, arm64 and x86_64, is little-endian); see
docs/surface-stream.md.
No interpolation or humidity conversion: these are the stored instantaneous values.
"""
from pathlib import Path
import argparse
import struct

import netCDF4
import numpy as np

# Header name: (lean source, units, cells, long_name), as the table at the top of src/surface_output.f90. The model
# does not read this table.
FIELDS = {
    'tsk': ('state%tsk', 'K', 'all', "Surface skin temperature; water keeps the startup SST, or the last surface record's sst with config_sst_update"),
    'xice': ('ice', '1', 'all', "Sea ice fraction: the startup one, or the last surface record's with config_sst_update"),
    'snow': ('snow', 'kg m-2', 'all', 'Radiation surface input; land refreshed each step, water startup'),
    'snowh': ('state%snowh', 'm', 'all', 'Snow depth'),
    'gsw': ('rad%sw_net', 'W m-2', 'all', 'Net downward surface SW; last radiation call, held until the next (config_radtsw_interval)'),
    'glw': ('rad%lw_down', 'W m-2', 'all', 'Downward surface LW; last radiation call, held until the next (config_radtlw_interval)'),
    'albedo': ('albedo', '1', 'all', 'Radiation surface input; land refreshed when valid, water startup'),
    'emiss': ('emiss', '1', 'all', 'Radiation surface input; land refreshed each step, water startup'),
    'hfx': ('state%hfx', 'W m-2', 'all', 'Upward sensible heat flux'),
    'qfx': ('state%qfx', 'kg m-2 s-1', 'all', 'Upward moisture flux'),
    'lh': ('state%lh', 'W m-2', 'all', 'Upward latent heat flux'),
    'ust': ('state%ust', 'm s-1', 'all', 'Friction velocity'),
    'znt': ('state%znt', 'm', 'all', 'Roughness length'),
    'hpbl': ('state%pblh', 'm', 'all', 'PBL height'),
    'chs': ('state%chs', 'm s-1', 'all', 'Surface exchange coefficient'),
    'chs2': ('state%chs2', 'm s-1', 'all', '2 m heat exchange coefficient'),
    'cqs2': ('state%cqs2', 'm s-1', 'all', '2 m moisture exchange coefficient'),
    'flhc': ('state%flhc', 'W m-2 K-1', 'all', 'Heat exchange coefficient'),
    'flqc': ('state%flqc', 'kg m-2 s-1', 'all', 'Moisture exchange coefficient'),
    'qsfc': ('state%qsfc', 'kg kg-1', 'all', 'Surface specific humidity'),
    'br': ('state%br', '1', 'all', 'Bulk Richardson number'),
    'zol': ('state%zol', '1', 'all', 'Height divided by Obukhov length'),
    'grdflx': ('lands%energy_flux_heatgroundtot', 'W m-2', 'land', 'Ground heat flux, positive into soil/snow'),
    'tslb1': ('lands%energy_state_temperaturesoilsnow(1,:)', 'K', 'land', 'Top soil temperature; snow indices -2:0 excluded'),
    'smois1': ('lands%water_state_soilmoisture(1,:)', 'm3 m-3', 'land', 'Top soil total moisture'),
    'sh2o1': ('lands%water_state_soilliqwater(1,:)', 'm3 m-3', 'land', 'Top soil liquid water'),
    'tmn': ('lands%forcing_temperaturesoilbottom', 'K', 'land', 'Deep soil boundary temperature'),
    'tv': ('lands%energy_state_temperaturecanopy', 'K', 'land', 'Vegetation temperature'),
    'tg': ('lands%energy_state_temperaturegrd', 'K', 'land', 'Ground temperature'),
    'tah': ('lands%energy_state_temperaturecanopyair', 'K', 'land', 'Canopy air temperature'),
    'eah': ('lands%energy_state_pressurevaporcanair', 'Pa', 'land', 'Canopy air vapour pressure'),
    'fveg': ('lands%energy_state_vegfrac', '1', 'land', 'Vegetation fraction'),
    'lai': ('lands%energy_state_leafareaindex', 'm2 m-2', 'land', 'Leaf area index'),
    'canwat': ('lands%water_state_canopyliqwater + lands%water_state_canopyice', 'mm', 'land', 'Canopy liquid plus ice; MPAS CANWAT'),
    't2mv': ('lands%energy_state_temperatureair2mveg', 'K', 'land', 'Vegetated tile 2 m temperature'),
    't2mb': ('lands%energy_state_temperatureair2mbare', 'K', 'land', 'Bare tile 2 m temperature'),
    'q2mv': ('lands%energy_state_spechumidity2mveg', 'kg kg-1', 'land', 'Vegetated tile 2 m specific humidity, stored value (not MPAS mixing ratio)'),
    'q2mb': ('lands%energy_state_spechumidity2mbare', 'kg kg-1', 'land', 'Bare tile 2 m specific humidity, stored value (not MPAS mixing ratio)'),
    'fsa': ('lands%energy_flux_radswabssfc', 'W m-2', 'land', 'Absorbed surface SW'),
    'fira': ('lands%energy_flux_radlwnetsfc', 'W m-2', 'land', 'Net LW, positive to atmosphere'),
    'fsh': ('lands%energy_flux_heatsensiblesfc', 'W m-2', 'land', 'Sensible heat, positive to atmosphere'),
    'fgev': ('lands%energy_flux_heatlatentgrd', 'W m-2', 'land', 'Ground latent heat, positive to atmosphere'),
    'fcev': ('lands%energy_flux_heatlatentcanopy', 'W m-2', 'land', 'Canopy evaporation latent heat, positive to atmosphere'),
    'fctr': ('lands%energy_flux_heatlatenttransp', 'W m-2', 'land', 'Transpiration latent heat, positive to atmosphere'),
    'chv': ('lands%energy_state_exchcoeffshabvcan', 'm s-1', 'land', 'Vegetated exchange coefficient above displacement height'),
    'chb': ('lands%energy_state_exchcoeffshbare', 'm s-1', 'land', 'Bare exchange coefficient above displacement height'),
    'chv2': ('lands%energy_state_exchcoeffsh2mveg', 'm s-1', 'land', 'Vegetated 2 m heat exchange coefficient'),
    'chb2': ('lands%energy_state_exchcoeffsh2mbare', 'm s-1', 'land', 'Bare 2 m heat exchange coefficient'),
    't2': ('state%t2', 'K', 'all', '2 m temperature'),
    'q2': ('state%q2', 'kg kg-1', 'all', "2 m humidity: mixing ratio over land; over water a mixing ratio with LEAN_SFC_Q2MR, else MPAS's formula, which mixes specific humidity and mixing ratio"),
    'th2': ('state%th2', 'K', 'all', '2 m potential temperature'),
    'u10': ('state%u10', 'm s-1', 'all', '10 m zonal wind'),
    'v10': ('state%v10', 'm s-1', 'all', '10 m meridional wind'),
}
STATIC = {
    'xland': ('state%xland', '1', 'Land mask (1 land, 2 water)'),
    'ivgtyp': ('startup io%ivgtyp(1)', '1', 'Vegetation type'),
    'isltyp': ('startup io%isltyp(1)', '1', 'Soil type'),
    'lat': ('mesh%lat * (180 / pi)', 'degrees_north', 'Cell latitude'),
    'lon': ('mesh%lon * (180 / pi)', 'degrees_east', 'Cell longitude'),
    'terrain': ('m%zgrid(1,1:ncells)', 'm', 'Terrain height'),
}


def read_stream(path):
    """Return (dt, names, static, records) as views of the file; reject incomplete records."""
    path = Path(path)
    with path.open('rb') as f:
        header = f.read(12)
        if len(header) != 12:
            raise ValueError('truncated surface header')
        nc, nf, dt = struct.unpack('<iif', header)
        if nc <= 0 or not 0 < nf <= len(FIELDS) or not np.isfinite(dt) or dt <= 0:
            raise ValueError('invalid surface dimensions or dt')
        raw_names = f.read(32 * nf)
    if len(raw_names) != 32 * nf:
        raise ValueError('truncated surface names')
    names = tuple(raw_names[i:i+32].decode('ascii').rstrip() for i in range(0, len(raw_names), 32))
    if len(set(names)) != nf or any(n not in FIELDS for n in names):
        raise ValueError('unknown or duplicate surface field')
    offset = 12 + 32 * nf
    record_offset = offset + 4 * len(STATIC) * nc
    record_bytes = 4 + 4 * nf * nc
    remaining = path.stat().st_size - record_offset
    if remaining < 0 or remaining % record_bytes:
        raise ValueError('truncated surface static block or record')
    raw = np.memmap(path, mode='r', dtype='u1')
    static = np.ndarray((len(STATIC), nc), dtype='<f4', buffer=raw, offset=offset)
    dtype = np.dtype([('it', '<i4'), ('fields', '<f4', (nf, nc))])
    records = np.ndarray((remaining // record_bytes,), dtype=dtype, buffer=raw, offset=record_offset)
    if np.any(records['it'] <= 0) or np.any(np.diff(records['it'].astype('i8')) <= 0):
        raise ValueError('surface steps must be positive and increasing')
    return dt, names, static, records


def convert(stream, out_nc):
    if Path(stream).resolve() == Path(out_nc).resolve():
        raise ValueError('output must differ from input')
    dt, names, static, records = read_stream(stream)
    with netCDF4.Dataset(out_nc, 'w') as ds:
        ds.createDimension('time', None)
        ds.createDimension('nCells', static.shape[1])
        ds.dt_seconds = float(dt)
        ds.description = 'Instantaneous lean_mpas surface output; time measured from forecast start'
        time = ds.createVariable('time', 'f8', ('time',))
        time.units = 's'
        time.long_name = 'Elapsed time from forecast start'
        time[:] = records['it'].astype('f8') * dt
        step = ds.createVariable('it', 'i4', ('time',))
        step.long_name = 'Model step from forecast start'
        step[:] = records['it']
        for i, (name, (source, units, long_name)) in enumerate(STATIC.items()):
            var = ds.createVariable(name, 'f4', ('nCells',), fill_value=False)
            var.lean_source, var.units, var.long_name = source, units, long_name
            var[:] = static[i]
        for i, name in enumerate(names):
            source, units, cells, long_name = FIELDS[name]
            var = ds.createVariable(name, 'f4', ('time', 'nCells'), fill_value=False)
            var.lean_source, var.units, var.long_name, var.cells = source, units, long_name, cells
            if cells == 'land':
                var.note = 'Noah-MP land cells (xland < 1.5); water values are stored zeros, not valid diagnostics'
            for t, record in enumerate(records):
                var[t] = record['fields'][i]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('stream')
    parser.add_argument('out_nc')
    args = parser.parse_args()
    convert(args.stream, args.out_nc)


if __name__ == '__main__':
    main()
