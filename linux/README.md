# linux/: MPAS and lean_mpas side by side

Scripts that run one case of `cases/` through MPAS 8.4.2's `atmosphere_model` and through lean_mpas with the same
namelist, streams, inputs and tables, on the same cores, and record for each run the wall time, the seconds per step
and the memory. They are written for a Linux machine (`taskset`, GNU time); a card run needs an NVIDIA card.

| script | what it does |
|---|---|
| `cases/init.sh CASE STAGE [MET_DIR]` | one `init_atmosphere_model` stage (`cases/CASE/init/base.txt` + `STAGE.txt`); its outputs go to the case's inputs only after a clean verdict |
| `cases/mpas_run.sh CASE RUN` | one MPAS forecast (`cases/CASE/case.txt` + `mpas_runs/RUN.txt`) |
| `cases/lean_run.sh CASE RUN LEAN_BUILD` | the same forecast with lean_mpas linked as `atmosphere_model` |
| `cases/verdict.sh`, `cases/common.sh` | sourced by the three: the OK/FAIL checks, the folder layout, the run directory, the timed run |
| `lean_env.sh` | sourced by the three: `LEAN_PY` and the optional NVIDIA HPC SDK paths |

## What you need

- bash, `taskset` (util-linux), GNU time at `/usr/bin/time` (the `time` package), `sha256sum`, `md5sum`, Python 3.
- `LEAN_LAB`, a folder of your choice:

  ```
  $LEAN_LAB/cases/CASE/inputs/      the case's init, static and boundary files (cases/README.md says where they come from)
  $LEAN_LAB/cases/thompson_be/      MPAS's four MP_THOMPSON_*_DATA.DBL, big-endian (sums in cases/thompson_be.sha256)
  $LEAN_LAB/mpas/BUILD/             an MPAS 8.4.2 build tree, named as the specs' `build` lines name it
  ```

  The runs go to `$LEAN_LAB/cases/CASE/runs/RUN/` (MPAS) and `$LEAN_LAB/cases/CASE/lean_runs/RUN/` (lean); a run
  directory is never reused, so remove it to run again. `$LEAN_LAB/cases/CASE/runs.log` gets one line per run.
- An MPAS build tree may hold an `env.sh`, sourced before its programs run, for what that build needs: `MPIRUN` (the
  launcher, default `mpirun`), `MPIRUN_ARGS` (words added after `-np N`), `LD_LIBRARY_PATH`. For example, a gfortran
  build with the system's MPICH and its own PnetCDF:

  ```sh
  MPIRUN=/usr/bin/mpirun.mpich
  export LD_LIBRARY_PATH=/opt/pnetcdf/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
  ```

  and an nvfortran build with the NVIDIA HPC SDK's Open MPI, which binds rank i to core i unless told not to, so that
  `taskset` keeps the ranks on `CASE_CORES`:

  ```sh
  MPIRUN_ARGS='--bind-to none'
  ```

## A comparison

```sh
export LEAN_LAB=/data/lab CASE_CORES=0-7              # the same eight cores for both runs
bash linux/cases/mpas_run.sh greece_60km strict_12h
./lean build --flavor mpas --omp --jobs 8            # ends with BUILD OK build/<folder>/lean_mpas
LEAN_THREADS=8 bash linux/cases/lean_run.sh greece_60km strict_12h build/<folder>
```

The spec `strict_12h` names its MPAS build (`build MPAS-nv-strict`): `$LEAN_LAB/mpas/MPAS-nv-strict/` must hold it,
and lean's run takes its tables from there too. cases/README.md lists the MPAS builds the specs name and how each is
compiled (`MPAS-nv-strict`: nvfortran 25.3, `-O2 -Kieee -Mnofma`). For a like-for-like timing, build lean with the same
compiler (`--fc nvfortran`) or edit the spec's `build` line.

Each prints one line, for example

```
MPAS OK greece_60km/strict_12h: OK, rc=0, wall 52 s, 360 steps at 0.1203 s, maxRSS 0.61 GB, card off
```

- **wall**: the whole process, start-up and output included.
- **steps at s**: the `time integration` row of the timer table at the end of `log.atmosphere.0000.out`, which MPAS and
  lean_mpas both write: its calls are the steps, its total over its calls the seconds per step, start-up excluded.
  `tools/speed_table.py` splits the step into components.
- **maxRSS**: GNU time's largest resident set of one process; for MPAS under `mpirun` that is one rank, so the run
  uses about ranks times as much.
- **card peak**: the card's used memory, sampled every 5 s with `nvidia-smi`; it is the whole card's reading.
  `card off` when the run used no card.

`runs.log` adds the build, the executable's md5, the ranks or threads, the cores and the load (`uptime`) at start and
end. Time one run at a time on an otherwise idle machine, and check the load in `runs.log`.

- MPAS: the spec's `ranks` is the number of MPI tasks; `card on` marks a build that runs on the card, whose ranks then
  share it through an MPS daemon that `mpas_run.sh` starts (pipe and logs under `$LEAN_LAB/mps/`) and stops.
- lean: one process. `LEAN_THREADS` (default 1) is `OMP_NUM_THREADS` of an `--omp` build; a card build runs on the
  card as `LEAN_CARD` says (unset: all of it). The optional outputs (`LEAN_FILM`, `LEAN_PRODUCTS`, `LEAN_STORM`,
  `LEAN_SURFACE`) are off, so that lean writes what MPAS writes.
- Both: `CASE_CORES` pins the run with `taskset` (unset: no pinning), the stack limit is raised to unlimited (a gfortran
  build of MPAS 8.4.2 stops with SIGSEGV with the default 8 MB), and the run directory links the same tables: those of
  the MPAS build the spec names, and the four checked Thompson tables.

## The verdict

A run is OK when the program exited 0, the rank-0 log ends with zero error and zero critical error messages, no rank
wrote an `.err` or `.abort` log, no log line holds a NaN and every `expect` file of the spec was written as a new,
non-empty file. MPAS can exit 0 after an error, which is why the log is read too. Exit status 0 OK, 1 FAIL, 2 refused
(a bad name, an existing run directory, a missing `LEAN_LAB`).

The tests in `tests/linux_cases/` run these scripts with stand-ins for the models, MPS and the system tools; they need
neither a model nor a card.
