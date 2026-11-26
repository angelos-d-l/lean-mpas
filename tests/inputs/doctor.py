"""A copy of an MPAS file with one change, for tests/inputs.

Usage: doctor.py CASE SRC DST        (a Python with netCDF4 and numpy)
The source is only read. Each case below makes one fault (or one variant) that a check of tests/inputs expects lean to
stop on, warn about or accept.
"""
import shutil
import sys
import netCDF4
import numpy as np


def edit(src, dst):
    shutil.copyfile(src, dst)
    return netCDF4.Dataset(dst, 'r+')


def first_hexagon(ds):
    return int(np.argmax(np.asarray(ds['nEdgesOnCell'][:]) == 6))


def sphere_radius(src, dst):
    with edit(src, dst) as ds:
        ds.setncattr('sphere_radius', np.float32(1.0))


def no_sphere_attributes(src, dst):
    with edit(src, dst) as ds:
        ds.delncattr('on_a_sphere')
        ds.delncattr('sphere_radius')


def planar(src, dst):
    with edit(src, dst) as ds:
        ds.setncattr('on_a_sphere', 'NO')


def slot9(src, dst):
    with edit(src, dst) as ds:
        ds['cellsOnCell'][first_hexagon(ds), 8] = 1


def empty_slot_neighbour(src, dst):
    with edit(src, dst) as ds:
        ds['edgesOnCell'][first_hexagon(ds), 6] = 5


def index_order(src, dst):
    with edit(src, dst) as ds:
        ds['indexToCellID'][0:2] = [2, 1]


INVARIANT = """latCell lonCell xCell yCell zCell indexToCellID latEdge lonEdge xEdge yEdge zEdge indexToEdgeID latVertex lonVertex
xVertex yVertex zVertex indexToVertexID cellsOnEdge nEdgesOnCell nEdgesOnEdge edgesOnCell edgesOnEdge weightsOnEdge dvEdge dcEdge
angleEdge areaCell areaTriangle edgeNormalVectors localVerticalUnitVectors cellTangentPlane cellsOnCell verticesOnCell
verticesOnEdge edgesOnVertex cellsOnVertex kiteAreasOnVertex fEdge fVertex meshDensity nominalMinDc bdyMaskCell bdyMaskEdge
bdyMaskVertex cf1 cf2 cf3 zgrid rdzw dzu rdzu fzm fzp zxu zz zb zb3 dss deriv_two coeffs_reconstruct isltyp ivgtyp mminlu isice_lu
iswater_lu landmask shdmin shdmax snoalb albedo12m greenfrac var2d con oa1 oa2 oa3 oa4 ol1 ol2 ol3 ol4 soilcomp soilcl1 soilcl2
soilcl3 soilcl4""".split()   # core_atmosphere/Registry.xml:499-600
INPUT = """u_init v_init t_init qv qc qr initial_time xtime Time u w rho theta relhum rho_base theta_base surface_pressure
sfc_albbck skintemp snow snowc snowh sst tmn vegfra seaice xice xland u10 v10 q2 t2m precipw dzs sh2o smois tslb
h_oml_initial""".split()     # Registry.xml:602-648, the scalars the init file holds


def rewrite(src, dst, keep=lambda name: True, double=False):
    """A new file with src's format and attributes, the variables keep() accepts and only the dimensions they use,
    as MPAS writes a stream's file (an invariant file has no nSoilLevels and no Time)."""
    with netCDF4.Dataset(src) as a, netCDF4.Dataset(dst, 'w', format=a.data_model) as b:
        a.set_auto_maskandscale(False)
        b.set_auto_maskandscale(False)
        b.setncatts({k: a.getncattr(k) for k in a.ncattrs()})
        kept = [v for name, v in a.variables.items() if keep(name)]
        used = {d for v in kept for d in v.dimensions}
        for name, dim in a.dimensions.items():
            if name in used:
                b.createDimension(name, None if dim.isunlimited() else len(dim))
        for v in kept:
            kind = 'f8' if double and v.dtype == np.float32 else v.dtype
            b.createVariable(v.name, kind, v.dimensions).setncatts({k: v.getncattr(k) for k in v.ncattrs()})
        for v in kept:
            b[v.name][...] = v[...]


def invariant(src, dst):
    rewrite(src, dst, lambda name: name in INVARIANT)


def state(src, dst):
    rewrite(src, dst, lambda name: name in INPUT)


def double(src, dst):
    rewrite(src, dst, double=True)


def two_records(src, dst):
    with edit(src, dst) as ds:
        ds.set_auto_maskandscale(False)
        for v in ds.variables.values():
            if v.dimensions and v.dimensions[0] == 'Time':
                v[1] = v[0]
        ds['theta'][1] = np.asarray(ds['theta'][0], np.float32) + np.float32(1.0)
        ds['xtime'][1] = netCDF4.stringtoarr('2026-11-08_03:00:00'.ljust(64), 64)


def no_xtime(src, dst):
    with edit(src, dst) as ds:
        ds.renameVariable('xtime', 'xtime_was')


def missing_field(src, dst):
    rewrite(src, dst, lambda name: name != 'sfc_albbck')


def missing_diag(src, dst):
    rewrite(src, dst, lambda name: name != 'u10')


def ice_init(src, dst):
    """qi qs qg ni nr, each one nonzero value: 1e-5, 2e-5, 3e-5, 4000, 5000 at cell 7 (1-based), level 3."""
    with edit(src, dst) as ds:
        for name, value in zip('qi qs qg ni nr'.split(), (1e-5, 2e-5, 3e-5, 4000.0, 5000.0)):
            v = ds.createVariable(name, 'f4', ('Time', 'nCells', 'nVertLevels'))
            a = np.zeros(v.shape, np.float32)
            a[0, 6, 2] = value
            v[...] = a


def bad_xtime(src, dst):
    with edit(src, dst) as ds:
        ds['xtime'][0] = netCDF4.stringtoarr('garbage'.ljust(64), 64)


def initial_0630(src, dst):
    with edit(src, dst) as ds:
        ds['initial_time'][...] = netCDF4.stringtoarr('2026-11-08_06:30:00'.ljust(64), 64)
        ds['xtime'][0, :] = netCDF4.stringtoarr('2026-11-08_06:30:00'.ljust(64), 64)


def initial_2400(src, dst):
    with edit(src, dst) as ds:
        ds['initial_time'][...] = netCDF4.stringtoarr('2026-11-08_24:00:00'.ljust(64), 64)


def initial_roll(src, dst):
    """2026-11-07_24:00:00 is 2026-11-08_00:00:00 for MPAS's time manager."""
    with edit(src, dst) as ds:
        ds['initial_time'][...] = netCDF4.stringtoarr('2026-11-07_24:00:00'.ljust(64), 64)


def nan_theta(src, dst):
    with edit(src, dst) as ds:
        ds['theta'][0, 5, 3] = np.nan          # cell 6, level 4


def isice16(src, dst):
    with edit(src, dst) as ds:
        ds['isice_lu'][...] = 16


def usgs(src, dst):
    with edit(src, dst) as ds:
        ds['mminlu'][:] = netCDF4.stringtoarr('USGS'.ljust(64), 64)


def mminlu_nul(src, dst):
    with edit(src, dst) as ds:
        ds['mminlu'][:] = netCDF4.stringtoarr('MODIFIED_IGBP_MODIS_NOAH', 64)   # NUL-padded, as C tools write


def land_cells(ds, n):
    return np.flatnonzero(np.asarray(ds['landmask'][:]) == 1)[:n]


def black_land3(src, dst):
    """The MPAS 8 static fault on the first 3 land cells: albedo12m 0.5 % in every month, so sfc_albbck 0.005."""
    with edit(src, dst) as ds:
        for c in land_cells(ds, 3):
            ds['albedo12m'][c, :] = 0.5
            ds['sfc_albbck'][0, c] = 0.005


def init_albedo(src, dst):
    """sfc_albbck of those 3 cells again from albedo12m (November), as the init step derives it from a corrected static."""
    with edit(src, dst) as ds:
        for c in land_cells(ds, 3):
            ds['sfc_albbck'][0, c] = np.float32(ds['albedo12m'][c, 10]) / np.float32(100.0)


def density16(src, dst):
    with edit(src, dst) as ds:
        ds['meshDensity'][0:10] = 16.0


def density_nan(src, dst):
    with edit(src, dst) as ds:
        ds['meshDensity'][2] = np.nan


def land_ice2(src, dst):
    with edit(src, dst) as ds:
        for c in land_cells(ds, 2):
            ds['ivgtyp'][c] = 15


def sea_ice1(src, dst):
    with edit(src, dst) as ds:
        c = int(np.argmax(np.asarray(ds['landmask'][:]) == 0))
        ds['xice'][0, c] = 0.5
        # MPAS initialize_real makes a sea-ice cell land before the physics start.
        ds['landmask'][c] = 1
        ds['xland'][0, c] = 1.0
        ds['ivgtyp'][c] = 15
        ds['isltyp'][c] = 16


def multi(src, dst):
    """The five greece boundary records (00-12 UTC, the files next to SRC) as the records of one file."""
    from pathlib import Path
    with edit(src, dst) as ds:
        ds.set_auto_maskandscale(False)
        for i, hour in enumerate((3, 6, 9, 12), start=1):
            with netCDF4.Dataset(Path(src).parent / f'lbc.2026-11-08_{hour:02d}.00.00.nc') as other:
                other.set_auto_maskandscale(False)
                for name, v in ds.variables.items():
                    v[i] = other[name][0]


def lbc_ice(src, dst):
    """lbc_qi lbc_qs lbc_qg lbc_ni lbc_nr, each one nonzero value: 1e-5, 2e-5, 3e-5, 4000, 5000 at cell 7, level 3."""
    with edit(src, dst) as ds:
        for name, value in zip('qi qs qg ni nr'.split(), (1e-5, 2e-5, 3e-5, 4000.0, 5000.0)):
            v = ds.createVariable('lbc_' + name, 'f4', ('Time', 'nCells', 'nVertLevels'))
            a = np.zeros(v.shape, np.float32)
            a[0, 6, 2] = value
            v[...] = a


def lbc_no_w_qc_qr(src, dst):
    rewrite(src, dst, lambda name: name not in ('lbc_w', 'lbc_qc', 'lbc_qr'))


def lbc_nan(src, dst):
    with edit(src, dst) as ds:
        ds['lbc_theta'][0, 6, 2] = np.nan


def wrong_vertices(src, dst):
    with edit(src, dst) as ds:
        ds.createDimension('nVertices', 7)


def levels_over(src, dst):
    # No large data copy: a rejected mesh needs only dimensions and attributes.
    with netCDF4.Dataset(src) as a, netCDF4.Dataset(dst, 'w', format=a.data_model) as b:
        b.setncatts({k: a.getncattr(k) for k in a.ncattrs()})
        for name, dim in a.dimensions.items():
            n = 61 if name == 'nVertLevels' else 62 if name == 'nVertLevelsP1' else len(dim)
            b.createDimension(name, n)


CASES = {f.__name__: f for f in (sphere_radius, no_sphere_attributes, planar, slot9, empty_slot_neighbour, index_order,
                                 invariant, state, double, two_records, no_xtime, missing_field, missing_diag, ice_init,
                                 bad_xtime, initial_0630, initial_2400, initial_roll, nan_theta, isice16, usgs, mminlu_nul, black_land3, init_albedo, density16,
                                 density_nan, land_ice2, sea_ice1, multi, lbc_ice, lbc_nan, lbc_no_w_qc_qr, wrong_vertices, levels_over)}

if __name__ == '__main__':
    if len(sys.argv) != 4 or sys.argv[1] not in CASES:
        sys.exit(__doc__ + '\nCases: ' + ' '.join(CASES))
    CASES[sys.argv[1]](sys.argv[2], sys.argv[3])
