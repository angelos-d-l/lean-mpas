"""Lean product output (LEAN_PRODUCTS, src/product_output.f90) -> four NetCDF files on a regular lat-lon grid, named
like ICON output_nml files (<prefix>_DOM01_ML_0001.nc, pl_DOM01_PL_0001.nc) with the prefixes sfc, storm, pl and const,
using ICON variable names, units and its time axis ("day as %Y%m%d.%f"), so map tools written for ICON output can read
a lean run. docs/output-formats.md describes the product records and this tool's output.
The grids: --grid greece is 0.03 deg over Greece and the Aegean, 17.85-29.61 E, 33.93-42.51 N; --grid wide (default)
has the same points and goes on to 12-36 E, 30-46 N, for a mesh that reaches that far.

  python products_to_icon.py PRODUCTS MESH_NC OUTDIR --start YYYY-MM-DD_HH [--grid wide|greece] [--storm STREAM]
                             [--point NAME LAT LON]

MESH_NC is any MPAS file of the mesh with latCell/lonCell (the init file). Leads are whole hours from the start; lead 0 is not
written (the model has no product record before its first step). The specified rows of the boundary (bdyMaskCell 6-7, when the
mesh file has it) are left out, so the grid is NaN there as outside the mesh: their state is the LBC's again every step, and
Grell-Freitas rain can pile up there. Fields and how they are made:
  t_2m td_2m rh_2m      t2, and q2 with psfc (Bolton 1980 vapour pressure, water)
  u_10m v_10m           u10 v10 of the surface layer
  gust10                the largest 10 m wind speed of the 10-minute samples of the hour (lean has no gust scheme)
  pres_msl              MPAS compute_slp (as the film)
  tot_prec rain_gsp rain_con snow_gsp snow_con graupel_gsp
                        Thompson rainnc (all grid-scale precipitation) split by snownc and graupelnc, Grell-Freitas rainc;
                        snow_con = 0 (Grell-Freitas makes no snow); accumulated since the start, kg m-2
  h_snow                snow depth (Noah-MP), m
  clct clcl clcm clch   the radiation's cloud fraction of every level (Xu-Randall, radiation_cloud.f90) with the MYNN
                        boundary-layer cloud fraction, maximum overlap inside each layer (surface-800, 800-400, 400-0 hPa), random
                        between them for clct, %
  cape_ml cin_ml        a parcel with the mean theta and qv of the lowest 50 hPa, pseudo-adiabatic (Bolton theta-e), virtual
                        temperature buoyancy; CIN below the level of free convection
  dbz_ctmax             the largest column-maximum reflectivity of the 10-minute samples of the hour
  uh_max                without --storm: instantaneous 2-5 km AGL UH from interpolated winds at the hour;
                        with --storm STREAM: native-mesh MPAS UH maximum over every step since the previous record,
                        mapped by the larger of linear interpolation and a nearest-grid-point box maximum
  geopot temp u v rh    at 850 700 500 300 hPa every 3 hours, linear in ln p from model levels (below ground: the lowest level,
                        geopotential extrapolated hydrostatically)
  topography_c fr_land  the terrain height and the land fraction of the mesh
dhail_mx (ICON's hail size) is not written: lean has no hail size diagnostic.
METEOGRAM_PE000_patch001.nc: the column of the mesh cell nearest the point (--point NAME LAT LON, default Athens,
37.98 N 23.73 E) every hour, in the layout of ICON's meteogram output (for Skew-T and meteogram plots): P T U V QV QC QI
REL_HUM CLC on the model levels (heights above sea level, top first), P_SFC T2M TD2M U10M V10M VBMAX10M (the hour's
largest 10 m wind) and the accumulations and cloud covers of sfc; time_step counts 30 s steps, whatever the model's
time step (time_step x 30 s is the time since the start).
"""
import argparse
import os
from datetime import datetime, timedelta
import numpy as np
import netCDF4 as nc
from scipy.spatial import Delaunay

G = 9.80665
RD, CP = 287.0, 1004.5
GRIDS = {'greece': (17.85 + 0.03 * np.arange(393), 33.93 + 0.03 * np.arange(287)),
         'wide': (12.0 + 0.03 * np.arange(801), 30.0 + 0.03 * np.arange(534))}
LON, LAT = GRIDS['wide']
PLEV = np.array([85000., 70000., 50000., 30000.])
NAMES2 = ['t2', 'q2', 'psfc', 'u10', 'v10', 'wind10_max', 'refl_max', 'mslp', 'rainnc', 'rainc', 'snownc', 'graupelnc', 'snowh', 'xland']
NAMES3 = ['theta', 'pressure', 'qv', 'qc', 'qr', 'qi', 'qs', 'qg', 'u', 'v', 'cldfra_bl']


class Records:
    """the product file, one hourly record at a time"""
    def __init__(self, path):
        self.f = open(path, 'rb')
        self.n, self.nz = np.fromfile(self.f, '<i4', 2)
        self.dt = float(np.fromfile(self.f, '<f4', 1)[0])
        self.zgrid = np.fromfile(self.f, '<f4', (self.nz + 1) * self.n).reshape(self.n, self.nz + 1)   # (cell, level)

    def __iter__(self):
        n, nz = self.n, self.nz
        while True:
            it = np.fromfile(self.f, '<i4', 1)
            if it.size == 0:
                return
            r = {'it': int(it[0])}
            a2 = np.fromfile(self.f, '<f4', len(NAMES2) * n)
            a3 = np.fromfile(self.f, '<f4', len(NAMES3) * nz * n)
            w = np.fromfile(self.f, '<f4', (nz + 1) * n)
            if a2.size + a3.size + w.size != (len(NAMES2) + len(NAMES3) * nz + nz + 1) * n:
                print('warning: the record of step %d is incomplete (run stopped while writing): left out' % r['it'])
                return
            r.update(zip(NAMES2, a2.reshape(len(NAMES2), n)))
            r.update(zip(NAMES3, a3.reshape(len(NAMES3), n, nz)))
            r['w'] = w.reshape(n, nz + 1)
            yield r


class Regrid:
    """linear (barycentric) interpolation from the cell centres to the regular grid; NaN outside the mesh, or outside the
    cells of keep when it is given (the call still takes values of every cell)"""
    def __init__(self, lon_cell, lat_cell, keep=None):
        idx = np.arange(lon_cell.size) if keep is None else np.flatnonzero(keep)
        tri = Delaunay(np.c_[lon_cell[idx], lat_cell[idx]])
        X, Y = np.meshgrid(LON, LAT)
        q = np.c_[X.ravel(), Y.ravel()]
        s = tri.find_simplex(q)
        self.inside = s >= 0
        T = tri.transform[s[self.inside]]
        b = np.einsum('nij,nj->ni', T[:, :2], q[self.inside] - T[:, 2])
        self.w = np.c_[b, 1 - b.sum(1)].astype(np.float64)
        self.v = idx[tri.simplices[s[self.inside]]]

    def __call__(self, x):
        out = np.full(LAT.size * LON.size, np.nan, np.float32)
        out[self.inside] = (x[self.v] * self.w).sum(1)
        return out.reshape(LAT.size, LON.size)


def box_maximum(linear, values, lon, lat, keep):
    """Retain native-cell peaks in the nearest grid box, preserving linear-map NaNs."""
    dx, dy = LON[1]-LON[0], LAT[1]-LAT[0]
    inside = (keep & (lon >= LON[0]-.5*dx) & (lon <= LON[-1]+.5*dx)
              & (lat >= LAT[0]-.5*dy) & (lat <= LAT[-1]+.5*dy))
    x = np.rint((lon[inside]-LON[0])/dx).astype('i8')
    y = np.rint((lat[inside]-LAT[0])/dy).astype('i8')
    # At an exact half-step outer boundary, ties-to-even may point one slot outside.
    x = np.clip(x, 0, LON.size-1)
    y = np.clip(y, 0, LAT.size-1)
    out = linear.copy()
    np.maximum.at(out, (y, x), values[inside])
    return out


def storm_records(path, n, dt):
    # Imported here: a conversion without --storm does not need the storm reader.
    import sys
    from pathlib import Path
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    from storm_to_nc import read_stream
    storm_dt, names, records = read_stream(path)
    if records['fields'].shape[2] != n or storm_dt != dt:
        raise ValueError('storm ncells or dt differs from products')
    if 'uh_max' not in names:
        raise ValueError('storm stream has no uh_max field')
    index = names.index('uh_max')
    return {int(r['it']): r['fields'][index] for r in records}


def storm_at_step(records, it):
    if it not in records:
        raise ValueError(f'no storm record for product step {it}')
    return records[it]


def esat_water(t):
    """saturation vapour pressure over water, Pa (Bolton 1980)"""
    return 611.2 * np.exp(17.67 * (t - 273.15) / (t - 29.65))


def dewpoint(e):
    x = np.log(np.maximum(e, 1e-3) / 611.2)
    return 273.15 + 243.5 * x / (17.67 - x)


def vapour_pressure(q, p):
    """q mixing ratio"""
    return q * p / (0.622 + q)


def cloud_fraction(t, p, qv, qc, qi, qs):
    """radiation_cloud.f90 (MPAS cal_cldfra1, Xu-Randall), every cell and level"""
    esw = 1000. * 0.61078 * np.exp(17.2693882 * (t - 273.15) / (t - 35.86))
    esi = 1000. * 0.61078 * np.exp(21.8745584 * (t - 273.15) / (t - 7.66))
    ep2 = 287. / 461.6
    qvsw = ep2 * esw / (p - esw)
    qvsi = ep2 * esi / (p - esi)
    qcld = qc + qi + qs
    weight = np.where(qcld < 1e-12, 0., (qi + qs) / np.maximum(qcld, 1e-30))
    qvs = (1 - weight) * qvsw + weight * qvsi
    rhum = qv / qvs
    subsat = np.maximum(1e-10, qvs - qv)
    arg = np.maximum(-6.9, -100. * qcld / subsat ** 0.49)
    cf = np.maximum(1e-10, rhum) ** 0.25 * (1. - np.exp(arg))
    cf = np.where(cf < 0.01, 0., cf)
    return np.where(qcld < 1e-12, 0., np.where(rhum >= 1., 1., cf))


def theta_e(t, p, q):
    """Bolton (1980) equivalent potential temperature; q mixing ratio"""
    e = vapour_pressure(q, p)
    td = dewpoint(e)
    tl = 1. / (1. / (td - 56.) + np.log(t / td) / 800.) + 56.
    return t * (1e5 / p) ** (0.2854 * (1 - 0.28 * q)) * np.exp((3.376 / tl - 0.00254) * 1000 * q * (1 + 0.81 * q)), tl


def saturated_t(the, p, t0):
    """temperature on the pseudo-adiabat theta_e at pressure p (Newton from t0)"""
    t = t0.copy()
    for _ in range(12):
        qs = 0.622 * esat_water(t) / (p - esat_water(t))
        f, _ = theta_e(t, p, qs)
        t1 = t + 0.1
        qs1 = 0.622 * esat_water(t1) / (p - esat_water(t1))
        f1, _ = theta_e(t1, p, qs1)
        t = t - (f - the) / ((f1 - f) / 0.1)
    return t


def cape_cin(t, p, qv, z, psfc):
    """mixed-layer (lowest 50 hPa) CAPE and CIN, J kg-1; t p qv z (cell, level) at mass levels"""
    n, nz = t.shape
    th = t * (1e5 / p) ** (RD / CP)
    # pressure-weighted: each mass level stands for its layer (interfaces halfway between levels), clipped to psfc - 50 hPa
    pi = np.c_[psfc, 0.5 * (p[:, 1:] + p[:, :-1]), np.zeros(n)]
    ml = np.clip(np.minimum(pi[:, :-1], psfc[:, None]) - np.maximum(pi[:, 1:], psfc[:, None] - 5000.), 0., None)
    th_ml = (th * ml).sum(1) / ml.sum(1)
    q_ml = (np.maximum(qv, 0) * ml).sum(1) / ml.sum(1)
    t0 = th_ml * (p[:, 0] / 1e5) ** (RD / CP)
    the, tlcl = theta_e(t0, p[:, 0], q_ml)
    plcl = p[:, 0] * (tlcl / t0) ** (CP / RD)
    tp = np.empty_like(t)
    qp = np.empty_like(t)
    tguess = t[:, 0].copy()
    for k in range(nz):
        dry = p[:, k] >= plcl
        tdry = th_ml * (p[:, k] / 1e5) ** (RD / CP)
        tsat = saturated_t(the, p[:, k], np.where(dry, tdry, tguess))
        tp[:, k] = np.where(dry, tdry, tsat)
        qp[:, k] = np.where(dry, q_ml, 0.622 * esat_water(tsat) / (p[:, k] - esat_water(tsat)))
        tguess = tp[:, k]
    tvp = tp * (1 + 0.608 * qp)
    tve = t * (1 + 0.608 * np.maximum(qv, 0))
    b = G * (tvp - tve) / tve
    dz = np.diff(np.c_[z[:, :1], z], axis=1)
    above = p <= plcl[:, None]
    pos = (b > 0) & above
    lfc = np.where(pos.any(1), pos.argmax(1), nz)
    k = np.arange(nz)[None, :]
    cape = np.where(pos & (k >= lfc[:, None]), b * dz, 0.).sum(1)
    cin = np.where((b < 0) & (k < lfc[:, None]), b * dz, 0.).sum(1)
    return cape, np.where(lfc < nz, cin, 0.)


def to_plev(x, p, plev, below):
    """linear in ln p from mass levels (p decreasing upward), the lowest level below ground"""
    lp = np.log(p)
    out = np.empty(x.shape[0])
    target = np.log(plev)
    k = (lp > target).sum(1)                         # levels below the pressure level
    k1 = np.clip(k, 1, p.shape[1] - 1)
    i = np.arange(x.shape[0])
    f = (target - lp[i, k1 - 1]) / (lp[i, k1] - lp[i, k1 - 1])
    out = x[i, k1 - 1] + f * (x[i, k1] - x[i, k1 - 1])
    return np.where(k == 0, below, out)


def heights_agl(x, zmid, zsfc, levels):
    """values at heights above ground, linear in height"""
    h = zmid - zsfc[:, None]
    out = []
    for lv in levels:
        k = np.clip((h < lv).sum(1), 1, h.shape[1] - 1)
        i = np.arange(h.shape[0])
        f = (lv - h[i, k - 1]) / (h[i, k] - h[i, k - 1])
        out.append(x[i, k - 1] + f * (x[i, k] - x[i, k - 1]))
    return out


def day_as(t):
    return float(t.strftime('%Y%m%d')) + (t.hour * 3600 + t.minute * 60 + t.second) / 86400.


def new_file(path, times=True, extra=()):
    d = nc.Dataset(path, 'w')
    d.Conventions = 'CF-1.6'
    d.title = 'lean MPAS forecast on a regular lat-lon grid, ICON variable names'
    d.source = 'lean_mpas product output (src/product_output.f90), tools/products_to_icon.py'
    if times:
        d.createDimension('time', None)
        v = d.createVariable('time', 'f8', ('time',)); v.standard_name = 'time'; v.units = 'day as %Y%m%d.%f'
        v.calendar = 'proleptic_gregorian'; v.axis = 'T'
    d.createDimension('lon', LON.size); d.createDimension('lat', LAT.size)
    v = d.createVariable('lon', 'f8', ('lon',)); v.standard_name = 'longitude'; v.units = 'degrees_east'; v[:] = LON
    v = d.createVariable('lat', 'f8', ('lat',)); v.standard_name = 'latitude'; v.units = 'degrees_north'; v[:] = LAT
    for name, values, units in extra:
        d.createDimension(name, len(values))
        v = d.createVariable(name, 'f8', (name,)); v.units = units; v[:] = values
    return d


def var(d, name, dims, units):
    v = d.createVariable(name, 'f4', dims, zlib=True, complevel=1, fill_value=np.float32(np.nan))
    v.units = units
    return v


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('products'); ap.add_argument('mesh'); ap.add_argument('outdir')
    ap.add_argument('--storm', help='LEAN_STORM stream of native-mesh hourly UH maxima')
    ap.add_argument('--start', required=True, help='YYYY-MM-DD_HH, the model start')
    ap.add_argument('--grid', choices=sorted(GRIDS), default='wide', help='the lat-lon grid: wide (default) or greece (see above)')
    ap.add_argument('--point', nargs=3, default=['point', '37.98', '23.73'], metavar=('NAME', 'LAT', 'LON'),
                    help='the meteogram point (default Athens)')
    a = ap.parse_args()
    global LON, LAT
    LON, LAT = GRIDS[a.grid]
    start = datetime.strptime(a.start, '%Y-%m-%d_%H')
    rec = Records(a.products)
    storm_data = storm_records(a.storm, rec.n, rec.dt) if a.storm else None
    with nc.Dataset(a.mesh) as m:
        lon = np.degrees(np.array(m['lonCell'][:], np.float64)); lat = np.degrees(np.array(m['latCell'][:], np.float64))
        keep = np.asarray(m['bdyMaskCell'][:]) < 6 if 'bdyMaskCell' in m.variables else np.ones(lon.size, bool)
    lon = np.where(lon > 180, lon - 360, lon)
    assert lon.size == rec.n, (lon.size, rec.n)
    print('%d cells of the specified boundary rows left out' % (keep.size - keep.sum()))
    R = Regrid(lon, lat, keep)
    os.makedirs(a.outdir, exist_ok=True)
    zsfc = rec.zgrid[:, 0].astype(np.float64)
    zmid = 0.5 * (rec.zgrid[:, 1:] + rec.zgrid[:, :-1]).astype(np.float64)

    sfc = new_file(os.path.join(a.outdir, 'sfc_DOM01_ML_0001.nc'), extra=[('height', [2.], 'm'), ('height_2', [10.], 'm')])
    for name, values in (('plev', 800.), ('plev_2', 600.), ('plev_3', 200.)):
        sfc.createDimension(name, 1); v = sfc.createVariable(name, 'f8', (name,)); v.units = 'hPa'; v[:] = values
    sfc.createDimension('bnds', 2)
    for name, b in (('plev', (800., 0.)), ('plev_2', (400., 800.)), ('plev_3', (0., 400.))):
        sfc.createVariable(name + '_bnds', 'f8', (name, 'bnds'))[:] = [b]
    S = {}
    for n, dims, u in (('t_2m', 'height', 'K'), ('td_2m', 'height', 'K'), ('rh_2m', 'height', '%'), ('u_10m', 'height_2', 'm s-1'),
                       ('v_10m', 'height_2', 'm s-1'), ('gust10', 'height_2', 'm s-1')):
        S[n] = var(sfc, n, ('time', dims, 'lat', 'lon'), u)
    for n, u in (('pres_msl', 'Pa'), ('tot_prec', 'kg m-2'), ('rain_gsp', 'kg m-2'), ('rain_con', 'kg m-2'), ('snow_gsp', 'kg m-2'),
                 ('snow_con', 'kg m-2'), ('graupel_gsp', 'kg m-2'), ('h_snow', 'm'), ('clct', '%')):
        S[n] = var(sfc, n, ('time', 'lat', 'lon'), u)
    for n, dim in (('clcl', 'plev'), ('clcm', 'plev_2'), ('clch', 'plev_3')):
        S[n] = var(sfc, n, ('time', dim, 'lat', 'lon'), '%')
    storm = new_file(os.path.join(a.outdir, 'storm_DOM01_ML_0001.nc'))
    T = {n: var(storm, n, ('time', 'lat', 'lon'), u) for n, u in (('cape_ml', 'J kg-1'), ('cin_ml', 'J kg-1'), ('dbz_ctmax', 'dBZ'),
                                                                 ('uh_max', 'm2 s-2'))}
    pl = new_file(os.path.join(a.outdir, 'pl_DOM01_PL_0001.nc'), extra=[('plev', PLEV, 'Pa')])
    pl['plev'].standard_name = 'air_pressure'
    P = {n: var(pl, n, ('time', 'plev', 'lat', 'lon'), u) for n, u in (('geopot', 'm2 s-2'), ('temp', 'K'), ('u', 'm s-1'),
                                                                      ('v', 'm s-1'), ('rh', '%'))}
    first = True
    pname, plat, plon = a.point[0], float(a.point[1]), float(a.point[2])
    pc = int(np.argmin(np.where(keep, (lat - plat) ** 2 + ((lon - plon) * np.cos(np.radians(plat))) ** 2, np.inf)))
    mt = {'leads': [], 'prof': {n: [] for n in MT_PROF}, 'sfc': {n: [] for n in MT_SFC}}
    ns = npl = 0
    for r in rec:
        if storm_data is not None:
            storm_uh = storm_at_step(storm_data, r['it'])
        lead_s = r['it'] * rec.dt
        lead = int(round(lead_s / 3600.))
        t = start + timedelta(hours=lead)
        if first:
            const = new_file(os.path.join(a.outdir, 'const_DOM01_ML_0001.nc'), times=False)
            var(const, 'topography_c', ('lat', 'lon'), 'm')[:] = R(zsfc)
            var(const, 'fr_land', ('lat', 'lon'), '-')[:] = R((r['xland'] < 1.5).astype(np.float64))
            const.close()
            first = False
        p = r['pressure'].astype(np.float64)
        tk = r['theta'] * (p / 1e5) ** (RD / CP)
        sfc['time'][ns] = day_as(t)
        S['t_2m'][ns, 0] = R(r['t2'])
        e2 = vapour_pressure(np.maximum(r['q2'].astype(np.float64), 1e-8), r['psfc'])
        S['td_2m'][ns, 0] = R(np.minimum(dewpoint(e2), r['t2']))
        S['rh_2m'][ns, 0] = R(np.clip(100. * e2 / esat_water(r['t2'].astype(np.float64)), 0., 100.))
        S['u_10m'][ns, 0] = R(r['u10']); S['v_10m'][ns, 0] = R(r['v10'])
        S['gust10'][ns, 0] = R(np.maximum(r['wind10_max'], np.hypot(r['u10'], r['v10'])))
        S['pres_msl'][ns] = R(r['mslp'])
        gsp = r['rainnc'].astype(np.float64)
        S['tot_prec'][ns] = R(gsp + r['rainc']); S['rain_con'][ns] = R(r['rainc'])
        S['snow_gsp'][ns] = R(r['snownc']); S['graupel_gsp'][ns] = R(r['graupelnc'])
        S['rain_gsp'][ns] = R(np.maximum(gsp - r['snownc'] - r['graupelnc'], 0.)); S['snow_con'][ns] = R(np.zeros(rec.n))
        S['h_snow'][ns] = R(np.where(r['xland'] < 1.5, r['snowh'], 0.))
        cf = np.maximum(cloud_fraction(tk, p, np.maximum(r['qv'], 0), np.maximum(r['qc'], 0), np.maximum(r['qi'], 0),
                                       np.maximum(r['qs'], 0)), r['cldfra_bl'])
        layer = [np.where(sel, cf, 0.).max(1) for sel in (p >= 80000., (p < 80000.) & (p >= 40000.), p < 40000.)]
        S['clcl'][ns, 0] = R(100 * layer[0]); S['clcm'][ns, 0] = R(100 * layer[1]); S['clch'][ns, 0] = R(100 * layer[2])
        S['clct'][ns] = R(100 * (1 - (1 - layer[0]) * (1 - layer[1]) * (1 - layer[2])))
        storm['time'][ns] = day_as(t)
        cape, cin = cape_cin(tk, p, r['qv'].astype(np.float64), zmid, r['psfc'].astype(np.float64))
        T['cape_ml'][ns] = R(cape); T['cin_ml'][ns] = R(cin); T['dbz_ctmax'][ns] = R(r['refl_max'])
        if storm_data is None:
            T['uh_max'][ns] = updraft_helicity(R, r, zmid, zsfc)
        else:
            T['uh_max'][ns] = box_maximum(R(storm_uh), storm_uh, lon, lat, keep)
        ns += 1
        q = np.maximum(r['qv'][pc].astype(np.float64), 0.)
        rain_gsp = max(float(gsp[pc] - r['snownc'][pc] - r['graupelnc'][pc]), 0.)
        for n, v in zip(MT_PROF, (p[pc], tk[pc], r['u'][pc], r['v'][pc], q / (1 + q), np.maximum(r['qc'][pc], 0),
                                  np.maximum(r['qi'][pc], 0), 100. * vapour_pressure(q, p[pc]) / esat_water(tk[pc]), cf[pc])):
            mt['prof'][n].append(np.asarray(v, np.float64)[::-1])          # ICON order: top first
        for n, v in zip(MT_SFC, (r['psfc'][pc], r['t2'][pc], min(float(dewpoint(e2[pc])), float(r['t2'][pc])), r['u10'][pc],
                                 r['v10'][pc], max(float(r['wind10_max'][pc]), float(np.hypot(r['u10'][pc], r['v10'][pc]))),
                                 rain_gsp, r['snownc'][pc], r['rainc'][pc], 0., r['graupelnc'][pc],
                                 1 - (1 - layer[0][pc]) * (1 - layer[1][pc]) * (1 - layer[2][pc]), layer[0][pc], layer[1][pc],
                                 layer[2][pc])):
            mt['sfc'][n].append(float(v))
        mt['leads'].append(lead)
        if lead % 3 == 0:
            pl['time'][npl] = day_as(t)
            rh3 = 100. * vapour_pressure(np.maximum(r['qv'], 0), p) / esat_water(tk)
            for j, lev in enumerate(PLEV):
                zl = to_plev(zmid, p, lev, zmid[:, 0] + RD * tk[:, 0] / G * np.log(p[:, 0] / lev))
                P['geopot'][npl, j] = R(G * zl)
                P['temp'][npl, j] = R(to_plev(tk, p, lev, tk[:, 0]))
                P['u'][npl, j] = R(to_plev(r['u'], p, lev, r['u'][:, 0]))
                P['v'][npl, j] = R(to_plev(r['v'], p, lev, r['v'][:, 0]))
                P['rh'][npl, j] = R(np.clip(to_plev(rh3, p, lev, rh3[:, 0]), 0., 100.))
            npl += 1
        print('lead %3d (%s) written' % (lead, t.strftime('%Y-%m-%d %H:%M')), flush=True)
    for d in (sfc, storm, pl):
        d.close()
    if mt['leads']:
        write_meteogram(os.path.join(a.outdir, 'METEOGRAM_PE000_patch001.nc'), mt, pname, plat, plon, zsfc[pc],
                        zmid[pc][::-1], start)
    print('%d hourly, %d three-hourly times in %s' % (ns, npl, a.outdir))


MT_PROF = ['P', 'T', 'U', 'V', 'QV', 'QC', 'QI', 'REL_HUM', 'CLC']
MT_PROF_UNITS = ['Pa', 'K', 'm/s', 'm/s', 'kg kg-1', 'kg kg-1', 'kg kg-1', '%', '-']
MT_SFC = ['P_SFC', 'T2M', 'TD2M', 'U10M', 'V10M', 'VBMAX10M', 'RAIN_GSP', 'SNOW_GSP', 'RAIN_CON', 'SNOW_CON', 'GRAUPEL_GSP',
          'CLCT', 'CLCL', 'CLCM', 'CLCH']
MT_SFC_UNITS = ['Pa', 'K', 'K', 'm/s', 'm/s', 'm/s', 'kg/m2', 'kg/m2', 'kg/m2', 'kg/m2', 'kg/m2', '-', '-', '-', '-']
MT_DTIME = 30.   # the meteogram's step unit, s: time_step * MT_DTIME = seconds since the start


def write_meteogram(path, mt, name, lat, lon, hsurf, z_top_first, start):
    """ICON's meteogram file layout for one station"""
    nt, nz = len(mt['leads']), z_top_first.size
    d = nc.Dataset(path, 'w')
    for dim, n in (('stringlen', 128), ('nstations', 1), ('nvars', len(MT_PROF)), ('nsfcvars', len(MT_SFC)), ('max_nlevs', nz),
                   ('time', nt)):
        d.createDimension(dim, n)

    def strings(var, dims, values):
        v = d.createVariable(var, 'S1', dims)
        v[:] = np.array([np.frombuffer(s.encode().ljust(128, b'\0'), 'S1') for s in values])   # stringtochar fails on netCDF4 1.7.4
    strings('station_name', ('nstations', 'stringlen'), [name])
    d.createVariable('station_lat', 'f8', ('nstations',))[:] = [lat]
    d.createVariable('station_lon', 'f8', ('nstations',))[:] = [lon]
    d.createVariable('station_hsurf', 'f8', ('nstations',))[:] = [hsurf]
    strings('var_name', ('nvars', 'stringlen'), MT_PROF)
    strings('var_unit', ('nvars', 'stringlen'), MT_PROF_UNITS)
    d.createVariable('var_nlevs', 'i4', ('nvars',))[:] = [nz] * len(MT_PROF)
    strings('sfcvar_name', ('nsfcvars', 'stringlen'), MT_SFC)
    strings('sfcvar_unit', ('nsfcvars', 'stringlen'), MT_SFC_UNITS)
    d.createVariable('time_step', 'i4', ('time',))[:] = [int(round(l * 3600 / MT_DTIME)) for l in mt['leads']]
    strings('date', ('time', 'stringlen'), [(start + timedelta(hours=l)).strftime('%Y%m%dT%H%M%SZ') for l in mt['leads']])
    d.createVariable('heights', 'f8', ('max_nlevs', 'nvars', 'nstations'))[:] = np.repeat(z_top_first[:, None, None], len(MT_PROF), 1)
    v = np.stack([np.stack(mt['prof'][n]) for n in MT_PROF], axis=2)          # (time, level, var)
    d.createVariable('values', 'f8', ('time', 'max_nlevs', 'nvars', 'nstations'))[:] = v[..., None]
    d.createVariable('sfcvalues', 'f8', ('time', 'nsfcvars', 'nstations'))[:] = np.array([mt['sfc'][n] for n in MT_SFC]).T[..., None]
    d.close()


def updraft_helicity(R, r, zmid, zsfc):
    """2-5 km above ground: sum of max(w,0) * max(relative vorticity,0) * dz on the regular grid, m2 s-2 (each factor clipped
    per level, as MPAS's updraft_helicity_max; the vorticity is on surfaces of constant height above ground)"""
    levels = np.arange(2000., 5001., 500.)
    wmid = 0.5 * (r['w'][:, 1:] + r['w'][:, :-1])
    U = heights_agl(r['u'].astype(np.float64), zmid, zsfc, levels)
    V = heights_agl(r['v'].astype(np.float64), zmid, zsfc, levels)
    W = heights_agl(wmid.astype(np.float64), zmid, zsfc, levels)
    step = LON[1] - LON[0]
    dx = 6.371e6 * np.radians(step) * np.cos(np.radians(LAT))[:, None]
    dy = 6.371e6 * np.radians(step)
    uh = np.zeros((LAT.size, LON.size))
    weights = np.full(levels.size, 500.); weights[[0, -1]] = 250.   # trapezoid over 3000 m
    for u, v, w, dz in zip(U, V, W, weights):
        ug, vg, wg = R(u).astype(np.float64), R(v).astype(np.float64), R(w).astype(np.float64)
        zeta = np.gradient(vg, axis=1) / dx - np.gradient(ug, axis=0) / dy
        uh += np.maximum(wg, 0.) * np.maximum(zeta, 0.) * dz
    return uh.astype(np.float32)


if __name__ == '__main__':
    main()
