# Test cases

Three cases made with MPAS 8.4.2's `init_atmosphere` from GFS 2026-11-08 00 UTC, and run through MPAS 8.4.2 and
through lean_mpas. Each case's README has the details.

| Case | What | Variants | Data |
|---|---|---|---|
| `greece_60km` | a 2,337-cell regional cut of MPAS's 60 km mesh around Greece, with lateral boundaries, 12 h | `v0630` (a 06:30 start), `l55` (55 levels), a surface-update file | inputs public |
| `x1_2562` | MPAS's 480 km global mesh, 24 h, with sea ice and land ice | `l55` (55 levels), a made surface-update file | inputs public |
| `athens_3km` | a 3 km regional mesh around Athens, 24 h: the case of the speed tables in `docs/acceptance.md` | | not public |

## Where the data live

The inputs of `greece_60km` and `x1_2562` (about 167 MB) are assets of the GitHub release v0.1.0 of
[github.com/angelos-d-l/lean-mpas](https://github.com/angelos-d-l/lean-mpas/releases/tag/v0.1.0). Download each case's
archive and unpack it into `data/cases/` (ignored by git), so that the files land in `data/cases/<case>/inputs/`, then
check them:

```sh
mkdir -p data/cases && tar -xzf <case>_inputs.tar.gz -C data/cases
cd data/cases/<case> && grep ' inputs/' ../../../cases/<case>/SHA256SUMS | shasum -a 256 -c
```

Each case's `SHA256SUMS` also lists MPAS's reference outputs (`reference/<run>/`) that the comparison tests read.
They are not in the release: a test that needs them says so and skips that part. `cases/<case>/reference/*.cdl` are the
`ncdump -h` headers of some of them, which the writer tests compare with lean's files.

## The specs

A case folder holds plain-text specs that `tools/cases/run_spec.py` applies to MPAS's generated default namelists and
streams (`reference/mpas_defaults/`):

- `case.txt`: the settings every run of the case shares (time step, start, the files, the physics suite);
- `init/base.txt` and `init/<stage>.txt`: one `init_atmosphere` stage (static, initial conditions, boundary files,
  surface-update file);
- `mpas_runs/<run>.txt`: one forecast, applied after `case.txt`.

A spec line is one of `nml RECORD OPTION VALUE`, `stream NAME ATTRIBUTE VALUE`, `build NAME`, `ranks N`,
`card on|off`, `minutes N` (a time limit), `inputs SUBFOLDER` (a variant of the inputs), `expect FILE` (a file the run
must write); `#` starts a comment; later lines and files override earlier ones.

`build NAME` names the MPAS 8.4.2 build that ran the reference:

| Build | What |
|---|---|
| `MPAS-nv-strict` | `atmosphere_model` with nvfortran 25.3, `-O2 -Kieee -Mnofma`: IEEE arithmetic and no fused multiply-add, the reference of the bit comparisons |
| `MPAS-nv-atm` | MPAS's OpenACC build for the card, nvfortran 25.3 |
| `MPAS-gnu-o3` | `atmosphere_model` with gfortran 13, `-O3` |
| `MPAS-gnu-init` | `init_atmosphere_model` with gfortran 13 |

The run names say what a run is for: `strict_*` the reference runs (`strict_bit10` writes a restart file at every
step for the step-by-step comparison, `strict_rst_a*` and `strict_rst_b*` are the two halves of a restart pair),
`off_*` and `opt_*` runs with a scheme `'off'` or another option, `probe_*` option sets MPAS cannot run (`off_sw_3h`
is a probe too; what MPAS does is in `docs/fixes.md`, "Scheme sets MPAS cannot run", and for `probe_sst_none`, "The
surface update"), `sfc_*` the surface update, and in `athens_3km`
`atm8_6h` (MPAS on the card) and `ref_*` (MPAS on 8 ranks of plain cores) for the speed tables.

## Running a case with lean

From the repository root: a run directory with `case.txt` and the run's spec applied, links to the inputs and to the
tables (`tests/inputs/run_dir.sh`), then lean_mpas inside it:

```sh
repo=$PWD
./lean build --flavor mpas --omp
./lean fetch-upstream && ./lean tables --build   # or: ./lean tables --from <folder with the four MP_THOMPSON_*_DATA.DBL>
root=$repo; . tests/inputs/run_dir.sh
make_run_dir "$repo/runs/<case>" "$repo/data/cases/<case>/inputs" cases/<case>/case.txt cases/<case>/mpas_runs/<run>.txt
cd "$repo/runs/<case>" && OMP_NUM_THREADS=2 OMP_STACKSIZE=256M "$repo/build/cpu-gfortran-strict-omp/lean_mpas"
```

`make_run_dir` links the Thompson tables of `data/tables/` (made by `./lean tables`, above) and the other tables from
`reference/`. Of the folder you give it, it links only the init file and the `lbc.*.nc` files. For a spec with
`inputs SUB`, give `data/cases/<case>/inputs/SUB` when that folder holds the init file (`v0630`, `l55`); otherwise link
SUB's files over the case's inputs by hand. A run with `config_sst_update` also needs
`ln -s data/cases/<case>/inputs[/SUB]/*.sfc_update.nc` in the run directory, and a restart run (`strict_rst_b*`) needs
the run-A restart file (see "The restart pairs").

## Running a case with MPAS and lean side by side

On a Linux machine with MPAS 8.4.2 builds, `linux/cases/` runs both from the same specs, inputs and tables, in new
folders under `LEAN_LAB` (`linux/cases/common.sh` describes the layout):

- `linux/cases/init.sh CASE STAGE [MET_DIR]`: one `init_atmosphere` stage;
- `linux/cases/mpas_run.sh CASE RUN`: one MPAS forecast;
- `linux/cases/lean_run.sh CASE RUN LEAN_BUILD`: the same forecast with lean_mpas.

Each prints `INIT|MPAS|LEAN OK|FAIL` with the wall time and memory; `CASE_CORES` pins both to the same cores.

## Pitfalls

- MPAS 8.4.2 writes CDF-5 files: an `ncdump` from a netCDF-C built without CDF-5 (MacPorts' 4.9.2, for one) fails on
  them with "NetCDF: Attempt to use feature that was not turned on when netCDF was built."
- MPAS reads the Thompson tables big-endian. `tools/dbl_big_endian.py` writes MPAS's from lean's tables;
  `cases/thompson_be.sha256` holds their sums.
- MPAS can exit 0 after an error: read the end of its log and look for `.err` and `.abort` files
  (`linux/cases/verdict.sh` does).
- MPAS with Noah-MP needs `soilcomp` and `soilcl1-4` in the init file: `config_noahmp_static = true` and SoilGrids in
  the geog folder. With `false`, `init_atmosphere` writes none ("the 'nSoilComps' dimension is not defined").
- A variant's inputs live in their own subfolder (`inputs v0630`), so that a run links only its own init and boundary
  files.

## How the inputs were made

- **Cycle:** GFS 2026-11-08 00 UTC at 0.25 degrees, ungribbed with WPS (`Vtable.GFS`, prefix `GFS`, interval 10800 s).
- **Meshes:** UCAR's `x1.2562.tar.gz` (sha256 `5359727bf93993d4df32dda37176279c2f52fa3f7caaa076ab0813776433c551`) for
  `x1_2562`; `greece_60km` cut from UCAR's `x1.163842` with MPAS-Limited-Area `create_region`, a circle of 1100 km
  around 38.0 N 24.0 E (`cases/greece_60km/greece_60km.pts`).
- **Static:** with SoilGrids from MPAS's `mpas_static.tar.bz2` in the geog folder of `init/base.txt`.
- **Stages:** every stage through `linux/cases/init.sh` with `MPAS-gnu-init`.

Checks of the init files (land-use categories: ice 15, water 17):

| File | nCells | Levels | nominalMinDc | Land | Land ice | Sea ice (xice >= 0.02) | Land albedo < 1 % | meshDensity > 10 % off | NaN in u, theta, qv |
|---|---|---|---|---|---|---|---|---|---|
| `greece_60km` init (and `v0630`) | 2337 | 45 | 60000.0 | 1460 | 0 | 0 | 0 | 0 | 0 |
| `x1_2562` init (and `l55`) | 2562 | 45 (55) | 480000.0 | 660 | 80 | 146 | 0 | 0 | 0 |

Every `xtime` equals its file name: the `greece_60km` boundary files every 3 h from 00 to 12 UTC, those of `v0630` at
06:30, 09:30 and 12:30, and the five records of `greece_60km.sfc_update.nc` (00 to 12 UTC). A stock MPAS static on
these meshes needs neither the albedo nor the meshDensity fix of `tools/mesh/`.

### The restart pairs

MPAS strict, one rank, 120 s steps. Run A starts at 00:00 and runs to 12:00; run B starts from A's restart file,
which it reads from `inputs/rst0600/` (or `inputs/rst0610/`), laid over the case's inputs: copy run A's
`restart.2026-11-08_06.00.00.nc` (or `restart.2026-11-08_06.10.00.nc`) there before running B.

| Mesh | Run | Start | Restart |
|---|---|---|---|
| `greece_60km` | `strict_rst_a12h` | 00:00 | writes at 06:00 and 12:00 |
| `greece_60km` | `strict_rst_b6h` | 06:00 | resumes at 06:00 |
| `greece_60km` | `strict_rst_a0610` | 00:00 | writes at 06:10, between radiation calls and boundary records |
| `greece_60km` | `strict_rst_b0610` | 06:10 | resumes at 06:10 |
| `x1_2562` | `strict_rst_a12h` | 00:00 | writes at 06:00 and 12:00 |
| `x1_2562` | `strict_rst_b6h` | 06:00 | resumes at 06:00 |

The off-cycle pair pins the boundary, history and diagnostics schedules to 00:00 with `reference_time`. The aligned
pairs agree with run A at 12:00 in every variable; MPAS's 06:10 pair does not (47 history and 95 diagnostic variables
differ), which is why lean's restart test compares an off-cycle split with lean's own continuous run.

### The surface-update file of `x1_2562`

After `mkdir -p data/cases/x1_2562/inputs/sfc_ice`,
`"$LEAN_PY" tools/cases/make_sfc_update.py data/cases/x1_2562/inputs/x1.2562.init.nc data/cases/x1_2562/inputs/sfc_ice`
(a Python with NumPy and netCDF4) writes
`x1.2562.sfc_update.nc` into `inputs/sfc_ice/`, the folder the `sfc_ice_7h` run reads, in the form
of `init_atmosphere`'s own surface file, with records at 00:00, 03:00 and 06:00 that freeze water, melt ice and change
ice fractions (its docstring lists the cells). It prints, per record, the counts of the branches of MPAS's
`physics_update_sst` (`to_land`, `to_ocean`, `removed`, `rescaled`): 0/0/2416/0, 12/12/2404/36, 6/12/2410/12.
`tests/lean_cli/test_make_sfc_update.py` checks them against the written file. The runs `sfc_upd_7h` (`greece_60km`,
its own surface file) and `sfc_ice_7h` (`x1_2562`, `inputs sfc_ice`, the made file) run 7 h with a record every 3 h.
