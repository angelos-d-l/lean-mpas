# Running lean_mpas

The README says how to build, set up a run directory and start a forecast. This page has the details: what lean
checks at the start, the command line, the logs and exit status, threads and the card, the optional outputs and the
tests.

## What lean reads and checks at the start

lean starts from the files MPAS itself reads: `namelist.atmosphere` and `streams.atmosphere` with the stream list files
they name; the init file of the `input` stream (or the files of the `invariant` and `input` streams), with
`config_apply_lbcs` the boundary files of `lbc_in`, with `config_sst_update` the files of `surface`; and the tables
`LANDUSE.TBL`, `OZONE_DAT.TBL`, `OZONE_LAT.TBL`, `OZONE_PLEV.TBL`, `RRTMG_LW_DATA`, `RRTMG_SW_DATA`, `NoahmpTable.TBL` and
the four `MP_THOMPSON_*_DATA.DBL`. The mesh size (`nCells`, `nEdges`, `nVertices`, up to 60 levels), the time step,
the radiation intervals and the dynamics settings come from these files when the run starts: one build of a flavor runs
any run directory.

It reads the namelist, the streams file and the inputs as MPAS 8.4.2 does and prints MPAS's lines in the log
([streams.md](streams.md), [namelist.md](namelist.md)). Before the first step, and before any output file is opened,
it checks:

- the dimensions (up to 60 levels), the mesh attributes (a sphere of the earth's radius) and the index order;
- that the init file holds the start time and every field lean reads;
- non-finite values, the land-use table, and, as warnings (stops with `config_lean_strict_inputs = .true.`), the
  near-zero background albedo MPAS 8's static step leaves on small coastal and island land cells, and a
  `meshDensity` that does not fit the cell sizes;
- the namelist values lean has in one form only (the damping and mixing constants and the other dynamics options) and
  the scheme sets MPAS cannot run;
- the tables, and the byte order of the Thompson tables;
- every boundary and surface read of the run: the file and the record, by `xtime`, with MPAS's rules;
- the memory the run needs against the memory available (README section 6).

Each problem is one `ERROR:` line that names the file and what to do; the run then stops with
`CRITICAL ERROR: lean_mpas stops: N ... problem(s) listed above`. [fixes.md](fixes.md) lists where this differs from
MPAS.

## The command line

`lean_mpas` (or a link named `atmosphere_model`), started inside the run directory, uses `namelist.atmosphere` and
`streams.atmosphere`; `-n FILE` and `-s FILE` select other files. As in MPAS's argument loop, either order works, a
repeated option takes its last value, a trailing option without an argument is ignored and other arguments are skipped;
an empty file name, or the other option in its place, is a usage error.

The run reads, resolves and checks the namelist, sets up the streams and the clock, reads the mesh, the initial state
and the boundary records, and steps for the namelist's run length. The log has MPAS's lines in MPAS's order: the setup,
the reads, the limited-area and dynamics checks, and for each step `Begin timestep`, the boundary update, the physics
alarms, ` split dynamics-transport integration 3`, the extrema and the step's timing; the output lines when a stream
writes. Nothing else is written on standard output, except a card build's start-up lines (the stages on the card, the
column work pools, the radiation slots) and the `LEAN_BUDGET` lines.

## Exit status and logs

| Status | Meaning |
|---|---|
| 0 | the forecast finished |
| 1 | a `CRITICAL ERROR` stopped the run, or the log could not be opened |
| 2 | a usage error, or a refusal before the log is opened: an MPI launcher with more than one task, or another run holding the run directory |

A gfortran build also uses 2 for a Fortran runtime error: tell a refusal by its message on standard error.

`log.atmosphere.0000.out` has MPAS's header, line form, counts and timer table. Its header records the version, the git
description, the build name, OpenMP and OpenACC support, single precision, the profile, netCDF-C I/O, the flavor and its
switches, and one MPI task (with the OpenMP thread count of an `--omp` build). The I/O block always reports one task;
the PIO options of the namelist are ignored.

`log.atmosphere.0000.err` is opened at the first `ERROR` or `CRITICAL ERROR` and gets every later line of those levels.
A fatal error closes both logs and writes `lean_mpas stopped: <message> (see log.atmosphere.0000.err)` on standard
error. An `ERROR` without a `CRITICAL ERROR` leaves the error log without a closing line, as in MPAS. Opening a new log
removes a stale error log.

The extrema printed each step use MPAS's `summarize_timestep` bounds, ties and text, and its three printout options.

### Refusals before the log

lean is one process. The first of these launcher variables that says otherwise refuses the start:

| Variables | Refused above |
|---|---:|
| `OMPI_COMM_WORLD_SIZE`, `PMI_SIZE`, `MV2_COMM_WORLD_SIZE`, `SLURM_STEP_NUM_TASKS` | 1 |
| `OMPI_COMM_WORLD_RANK`, `PMI_RANK`, `MV2_COMM_WORLD_RANK`, `SLURM_PROCID`, `PMIX_RANK` | 0 |

Empty or non-integer values are skipped. `SLURM_NTASKS` is not read: a batch job with several tasks allocated may
still start one copy. A launcher with one task is accepted.

lean holds an exclusive `flock` on the run directory for the life of the process (no lock file; the kernel releases it
even after `kill -9`). A second run in the same directory refuses. When the file system cannot lock, the run goes on
with the warning `cannot lock the run directory: <reason>; lean_mpas cannot check that no other run uses it`.

## Threads

The same sources build serially (the OpenMP directives are comments) and with OpenMP (`./lean build --omp`). Run with
`OMP_NUM_THREADS=N OMP_STACKSIZE=256M`: `-fopenmp` implies `-frecursive`, so every local array lives on the stack, and
the radiation columns need about 1 MB per thread.

Each phase of a step (start of step, physics, dynamics with Thompson) is one `!$omp parallel` region in
`src/lean_model.f90`; the routines it calls contain orphaned worksharing loops (`!$omp do schedule(dynamic,64)`, with
the barrier at the end of each loop), so every loop stays in its own routine and is optimised like the serial loop.
`tools/omp_tool.py check|orphans|modvars|nonlocal` audits the directives: private lists, shared writes outside
worksharing loops, module variables written in parallel.

## On the card

A card build (`--target gpu`) runs the dynamics on the card and, by default, the whole physics. Environment variables
choose at run time; a CPU build reads none of them.

| Variable | Effect |
|---|---|
| `LEAN_CARD` | the physics stages on the card: a comma list of `thompson`, `inputs`, `surface`, `land`, `pbl`, `gwdo`, `gf`, `finish`, `radiation`, or `all`; unset means all; `none` keeps the physics on the host |
| `LEAN_CHECK` | a comma list of the `LEAN_CARD` stage names (no `all`): those stages run on the card and on the host from the same inputs; the two results are compared bit for bit (radiation: the largest differences are printed, the card has its own `exp` and `log`) and the run goes on with the host's |
| `LEAN_ACT=n` | check the state for non-finite values every n steps (default 10) |
| `LEAN_RAD_SLOTS`, `LEAN_MICRO_SLOTS`, `LEAN_GF_SLOTS`, `LEAN_MYNN_CHUNK`, `LEAN_MYNN_PLUMES`, `LEAN_MYNN_PLUMELEV` | sizes of the card's column work pools (defaults 2048, 8192, 8192, 12288, 8192, 16): smaller values use less card memory and run slower; the memory check counts them |
| `LEAN_MICRO_STAGED=1` | with a build that has `LEAN_MICRO_ONEKERNEL`, pick Thompson's staged form |

`LEAN_MEMORY_REPORT=1` prints the memory check and stops with status 0 right after it (`./lean doctor --build` uses
it).

## Optional outputs

`LEAN_FILM`, `LEAN_PRODUCTS`, `LEAN_STORM` and `LEAN_SURFACE`, each set to an absolute path, write lean's own outputs
beside the streams: [output-formats.md](output-formats.md). A card build with `--switch LEAN_BUDGET=on` prints the
[water and energy ledger](water-energy-ledger.md).

## The writer

The NetCDF writer closes each file after each record. A new file is written as `<name>.lean_partial` and renamed after
its first record is closed; the writer removes the partial file when it reports a write failure, and a process killed
before the close can leave it behind. Existing CDF-5 and 64-bit-offset files are appended in place, and the completed
record count stays readable if the process dies; NetCDF-4 appends have no such guarantee.

For the tests, `LEAN_WRITER_DIE=N` (N positive) kills the process inside its N-th variable write after half a slab, and
`LEAN_WRITER_DIE=-N` forces a reported write failure at the close of the record that holds that write. The log warns
when it is set; leave it unset for forecasts.

## Tests

```sh
tests/run_all.sh quick     # no compiler: the command line, the configuration, the generators, the converters
tests/run_all.sh build     # compiles and runs the unit and column tests (gfortran; at most two compile jobs)
tests/run_all.sh cases     # runs the model on the small cases of cases/ and compares with MPAS where references exist
```

Each suite prints one line, `ok`, `SKIP <why>` or `FAIL`, and the run ends with a total. A suite that needs something
this machine or this release does not have skips and says what is missing:

- MPAS's source in `reference/mpas_src/` (`./lean fetch-upstream`), for the tests that compile MPAS's own routines
  beside lean's;
- the case inputs in `data/cases/` (release assets, [cases/README.md](../cases/README.md));
- MPAS's reference outputs of the cases, which are not released;
- nvfortran and a card, for the card tests.

Set `LEAN_PY` (or `PYTHON`) to a Python with NumPy, netCDF4 and xarray for the suites that read NetCDF files, and
`NCDUMP` to an `ncdump` that reads CDF-5. [The code map](code-map.md) lists which suite needs what.
