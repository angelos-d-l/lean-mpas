# Optional outputs (film, products, storm, surface)

Besides the files of `streams.atmosphere`, lean_mpas can write four outputs of its own, each switched on by an
environment variable that names the file (an absolute path). Unset or empty, the output is off and costs nothing.

| Variable | Output | When a record is written | Format |
|---|---|---|---|
| `LEAN_FILM` | the film: six surface fields for animations | every 10 minutes | below |
| `LEAN_PRODUCTS` | the products: surface fields, the hour's maxima and the 3-D state | every forecast hour | below |
| `LEAN_STORM` | native-mesh updraft helicity and vertical-velocity maxima | every forecast hour and the last step | [storm-output.md](storm-output.md) |
| `LEAN_SURFACE` | instantaneous surface-layer and Noah-MP fields | every forecast hour and the last step | [surface-output.md](surface-output.md) |

All four are output only: the forecast does not change when they are on. They belong to one run: they start their
sums and maxima at the run's start, and a restart run writes new files from its own start (the log warns about it).

## Common to all four

- **Binary layout.** Fortran unformatted stream access: no record markers, no padding. Integers are int32, reals are
  float32, in the byte order of the machine that ran the model (little-endian on x86-64 and on Apple silicon).
- **Cells.** Every field of a record holds `ncells` values, in the cell order of the mesh file: cell i of the file (1-based,
  as in MPAS) is value i-1 of each field (0-based, as in NumPy). Take `latCell` and `lonCell` (radians) from the init file of the run to place them.
- **Levels.** A 3-D field is stored cell by cell: for each cell its `nz` layer values from the bottom up (Fortran array
  `(nz, ncells)`, so in C order an array of shape `(ncells, nz)`). `nz` is the mesh's `nVertLevels`.
- **Time.** `it` is the step of the run, counted from 1 at the run's start. A record holds the state at the end of step
  `it`, valid at the run's start time plus `it * dt` (`dt` is `config_dt`). There is no record at step 0.
- **Records** are written at the first step that reaches each 10-minute mark (film) or each whole hour (products) after
  the run's start, as MPAS's output alarms ring: with dt 18 s the 10:10 record holds the state of 10:10:12.

## Film (`LEAN_FILM`)

Six fields every 10 minutes, the ones an animation of radar, rain, wind and pressure needs, computed as MPAS 8.4.2
computes them for its `diag` files.

| Offset (bytes) | Content |
|---:|---|
| 0 | `ncells` int32 |
| 4 | the first record |

A record is `4 + 24*ncells` bytes; record `r` (from 0) starts at `4 + r*(4 + 24*ncells)`:

| Within the record | Field | Units | What |
|---:|---|---|---|
| 0 | `it` | int32 | the step |
| 4 | `refl10cm_max` | dBZ | column maximum of the 10 cm radar reflectivity of Thompson's `calc_refl10cm`, at least -35 (MPAS's `compute_radar_reflectivity`) |
| 4 + 4*ncells | `rainnc` | mm | grid-scale (Thompson) precipitation, accumulated since the forecast's initial time |
| 4 + 8*ncells | `rainc` | mm | convective (Grell-Freitas) precipitation, accumulated since this run's start |
| 4 + 12*ncells | `u10` | m s-1 | 10 m zonal wind of the surface layer |
| 4 + 16*ncells | `v10` | m s-1 | 10 m meridional wind |
| 4 + 20*ncells | `mslp` | Pa | mean sea-level pressure, MPAS's `compute_slp` |

Reading it with NumPy:

```python
import numpy as np
raw = np.fromfile('run.film', dtype='<i4')          # '<' little-endian; the file is native order
ncells = int(raw[0])
n = (raw.size - 1) // (1 + 6 * ncells)               # complete records only
rec = raw[1:1 + n * (1 + 6 * ncells)].reshape(n, 1 + 6 * ncells)   # one row per record
steps = rec[:, 0]
fields = rec[:, 1:].view('<f4').reshape(-1, 6, ncells)
refl, rainnc, rainc, u10, v10, mslp = (fields[:, i] for i in range(6))
```

`tools/film_to_diag.py FILM OUTDIR --start YYYY-MM-DD_HH --dt DT` writes one `diag.YYYY-MM-DD_HH.MM.SS.nc` per record
with the six fields as MPAS names them (`Time x nCells`, float32); `--restart RESTART.nc` adds the `rainc` of a restart
file, so that `rainc` counts from the forecast's initial time as in MPAS.

## Products (`LEAN_PRODUCTS`)

Once an hour, the surface fields for maps, the hour's maxima and the 3-D state for soundings and pressure-level maps.

| Offset (bytes) | Content |
|---:|---|
| 0 | `ncells` int32 |
| 4 | `nz` int32 |
| 8 | `dt` float32, seconds |
| 12 | `zgrid`: `ncells x (nz+1)` float32, for each cell the heights of its `nz+1` layer interfaces, m above sea level, from the bottom up |
| 12 + 4*ncells*(nz+1) | the first record |

A record is `4 + 4*ncells*(15 + 12*nz)` bytes, in this order:

| Field | Shape | Units | What |
|---|---|---|---|
| `it` | int32 | | the step |
| `t2` | ncells | K | 2 m temperature |
| `q2` | ncells | kg kg-1 | 2 m water vapour mixing ratio (over water see `LEAN_SFC_Q2MR` in [fixes.md](fixes.md)) |
| `psfc` | ncells | Pa | surface pressure |
| `u10`, `v10` | ncells each | m s-1 | 10 m zonal and meridional wind |
| `wind10_max` | ncells | m s-1 | the largest 10 m wind speed of the 10-minute samples since the previous record |
| `refl_max` | ncells | dBZ | the largest column-maximum reflectivity of the 10-minute samples since the previous record (as the film's `refl10cm_max`) |
| `mslp` | ncells | Pa | mean sea-level pressure, MPAS's `compute_slp` |
| `rainnc` | ncells | mm | all grid-scale (Thompson) precipitation, accumulated since the forecast's initial time |
| `rainc` | ncells | mm | convective (Grell-Freitas) precipitation, accumulated since this run's start |
| `snownc` | ncells | mm | Thompson snow and ice reaching the ground, accumulated as `rainnc` (part of `rainnc`) |
| `graupelnc` | ncells | mm | Thompson graupel, accumulated as `rainnc` (part of `rainnc`) |
| `snowh` | ncells | m | snow depth |
| `xland` | ncells | 1 | 1 land, 2 water |
| `theta` | ncells x nz | K | potential temperature of dry air, `theta_m / (1 + (Rv/Rd)*qv)` |
| `pressure` | ncells x nz | Pa | full pressure |
| `qv`, `qc`, `qr`, `qi`, `qs`, `qg` | ncells x nz each | kg kg-1 | mixing ratios of vapour, cloud water, rain, ice, snow, graupel |
| `u`, `v` | ncells x nz each | m s-1 | zonal and meridional wind at the cell centres |
| `cldfra_bl` | ncells x nz | 1 | MYNN's subgrid cloud fraction |
| `w` | ncells x (nz+1) | m s-1 | vertical velocity at the layer interfaces |

The 3-D fields, `w`, `mslp` and the accumulations are the state at the end of step `it`. `t2`, `q2`, `psfc`, `u10`,
`v10`, `u`, `v` and `cldfra_bl` are those of that step's physics, which works on the state at the start of the step.

Reading it with NumPy:

```python
import numpy as np
NAMES2 = ['t2', 'q2', 'psfc', 'u10', 'v10', 'wind10_max', 'refl_max', 'mslp', 'rainnc', 'rainc',
          'snownc', 'graupelnc', 'snowh', 'xland']
NAMES3 = ['theta', 'pressure', 'qv', 'qc', 'qr', 'qi', 'qs', 'qg', 'u', 'v', 'cldfra_bl']
with open('run.prod', 'rb') as f:
    ncells, nz = (int(x) for x in np.fromfile(f, '<i4', 2))
    dt = float(np.fromfile(f, '<f4', 1)[0])
    zgrid = np.fromfile(f, '<f4', ncells * (nz + 1)).reshape(ncells, nz + 1)
    while True:
        it = np.fromfile(f, '<i4', 1)
        if it.size == 0:
            break
        surface = np.fromfile(f, '<f4', len(NAMES2) * ncells)
        state = np.fromfile(f, '<f4', len(NAMES3) * ncells * nz)
        w = np.fromfile(f, '<f4', ncells * (nz + 1))
        if (surface.size < len(NAMES2) * ncells or state.size < len(NAMES3) * ncells * nz
                or w.size < ncells * (nz + 1)):
            break                                       # a short last record: the run was killed while writing
        surface = surface.reshape(len(NAMES2), ncells)
        state = state.reshape(len(NAMES3), ncells, nz)
        w = w.reshape(ncells, nz + 1)
        record = dict(zip(NAMES2, surface)) | dict(zip(NAMES3, state)) | {'w': w, 'it': int(it[0])}
```

A run that is killed while writing leaves its last record short: stop at a record that is not complete, as both
readers above do.

```
tools/products_to_icon.py PRODUCTS MESH_NC OUTDIR --start YYYY-MM-DD_HH [--grid wide|greece] [--storm FILE]
                          [--point NAME LAT LON]
```

interpolates the records to one of two built-in 0.03 degree latitude-longitude grids over Greece and the eastern
Mediterranean (`--grid wide`, the default: 12-36 E, 30-46 N; `--grid greece`: 17.85-29.61 E, 33.93-42.51 N). A mesh
outside them gives NaN; for a mesh elsewhere the grids in `GRIDS` of the tool must be edited. It writes `sfc_`,
`storm_`, `pl_` and `const_` files named as ICON `output_nml` files, with ICON-LAM's postprocessed names, units and
time axis (surface fields, 2 m dewpoint and humidity, cloud covers, mixed-layer CAPE and CIN, pressure-level fields),
and `METEOGRAM_PE000_patch001.nc`, the column of the cell nearest `--point` (default Athens, 37.98 N 23.73 E).
`--storm FILE` takes the hourly updraft-helicity maxima from the `LEAN_STORM` output. It needs NumPy, netCDF4 and
SciPy; its docstring lists every field it makes and how.
