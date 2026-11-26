# greece_60km: a small regional case with lateral boundaries

- **Mesh:** a circle of 1100 km around 38.0 N 24.0 E, cut with MPAS-Limited-Area `create_region` from UCAR's
  x1.163842 (60 km): 2,337 cells (1,280 interior, boundary rows 1-7), 7,183 edges, 4,847 vertices, `maxEdges` 10;
  24.2-51.6 N, 6.7-41.1 E. `greece_60km.pts` is the cut.
- **Cycle:** GFS 2026-11-08 00 UTC.
- **Inputs** (`init/*.txt` on MPAS's defaults, one rank): the static file (SoilGrids, `config_noahmp_static true`), the
  initial conditions, 12 h of boundary files every 3 h, and `greece_60km.sfc_update.nc` with five records every 3 h.
  45 levels, nominalMinDc 60000.0, 1460 land cells, no ice. Variants: `inputs/v0630/` starts at 06:30 (init and
  boundary files at 06:30, 09:30, 12:30); `inputs/l55/` has 55 levels.
- **Runs:** `case.txt` (dt 120 s, 12 h, boundaries on, history every 3 h, diagnostics every hour) and
  `mpas_runs/*.txt`. The references of `MPAS-nv-strict` (one rank; builds in `cases/README.md`):

| Run | maxRSS | Kept |
|---|---|---|
| `strict_12h` | 0.68 GB | 5 history (3 h), 13 diag (1 h), log, namelist, streams; CDL headers in `reference/` |
| `strict_0630_3h` (`v0630`) | 0.68 GB | diag at 09:32 (past the 09:30 boundary record), log |
| `strict_sfc_3h` (`config_sst_update`) | 0.68 GB | diag at 03:02 (past the 03:00 surface record), log |

- **Step by step against `MPAS-nv-strict`** (lean with nvfortran on the CPU, a cold start): 0 of 143 fields differ after 1
  and after 10 steps.
- **Data:** `data/cases/greece_60km/`; the inputs are a release asset (`cases/README.md`), `SHA256SUMS` lists them and
  MPAS's reference outputs.
