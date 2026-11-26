# reference/: MPAS's own files that lean_mpas and its tests read

| Folder | What | Who reads it |
|---|---|---|
| `config/` | MPAS 8.4.2's `LANDUSE.TBL`, `VEGPARM.TBL`, `SOILPARM.TBL`, `GENPARM.TBL` and `NoahmpTable.TBL`, and an example `namelist.atmosphere` and `streams.atmosphere`: MPAS's defaults with the settings of the `greece_60km` case (`cases/greece_60km/case.txt`) | the build (it links `NoahmpTable.TBL` beside the executable), the tests that make run directories; `tests/namelist/` reads the example namelist |
| `radiation/` | RRTMG's `RRTMG_LW_DATA`, `RRTMG_SW_DATA` and the ozone climatology `OZONE_*.TBL` ([README](radiation/README.md)) | the tests that make run directories |
| `mpas_defaults/` | the default namelists, streams and stream lists that MPAS 8.4.2's build generates ([README](mpas_defaults/README.md)) | `tools/cases/run_spec.py`, which applies the case specs to them |
| `mpas_src/` | not in git: the MPAS-Model 8.4.2 checkout with its pinned externals, made by `./lean fetch-upstream` | `tools/lean/vendor.py`, `./lean tables --build`, the tests that compile MPAS's own routines |

Every file is MPAS's, unchanged, except the example `namelist.atmosphere` and `streams.atmosphere` in `config/` (MPAS's
defaults with the `greece_60km` case's settings, written by `tools/cases/run_spec.py`); each folder's `SHA256SUMS`
holds the sums. A run directory brings its own copies of the tables (README section 3); these are for the build and
the tests.
