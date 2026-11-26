# x1_2562: MPAS's coarse global mesh

- **Mesh:** UCAR's `x1.2562.tar.gz` (1,514,620 bytes, sha256 `5359727b...c551`): 2,562 cells, 7,680 edges, 5,120
  vertices, 480 km.
- **Cycle:** GFS 2026-11-08 00 UTC, the global 0.25 degree analysis (502,827,497 bytes, sha256 `6801f0a7...bfb56`),
  ungribbed with WPS.
- **Inputs** (`init/*.txt`): the static file on 4 ranks (SoilGrids on) and the initial conditions; `inputs/l55/` has 55
  levels; `inputs/sfc_ice/` holds the surface-update file, not in the release: make it once with
  `mkdir -p data/cases/x1_2562/inputs/sfc_ice && "$LEAN_PY" tools/cases/make_sfc_update.py
  data/cases/x1_2562/inputs/x1.2562.init.nc data/cases/x1_2562/inputs/sfc_ice` (a Python with NumPy and netCDF4). 45
  levels, nominalMinDc
  480000.0, 660 land cells, 80 land-ice cells, 146 sea-ice cells.
- **Runs:** `case.txt` (dt 120 s, 24 h) and `mpas_runs/*.txt`. The references of `MPAS-nv-strict` (one rank; builds in `cases/README.md`):

| Run | maxRSS | Kept |
|---|---|---|
| `strict_24h` | 0.72 GB | 5 history (6 h), 9 diag (3 h), log, namelist, streams; CDL headers in `reference/` |
| `strict_l55_3h` (`l55`) | 0.75 GB | diag at 03:00, log |

- **Against `MPAS-nv-strict`** (lean with nvfortran on the CPU): the restart file at 12:00 (`strict_rst_a12h`) equals
  MPAS's in every shared field, the sea-ice and glacier columns included.
- **Data:** `data/cases/x1_2562/`; the inputs are a release asset (`cases/README.md`), `SHA256SUMS` lists them and
  MPAS's reference outputs.
