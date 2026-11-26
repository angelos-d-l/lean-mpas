# Comparison with MPAS 8.4.2, and speed

What lean_mpas was compared against, how, and what came out; then the speed comparison and its method.
The test cases are those of [cases/](../cases/README.md).

## The reference

**MPAS strict** below (the build `MPAS-nv-strict` of `cases/README.md`) is MPAS-Atmosphere 8.4.2 built with nvfortran 25.3, `-O2 -Kieee -Mnofma` (IEEE arithmetic, no
fused multiply-add), run on one rank. lean's comparison build is the same compiler and flags on the CPU
(`./lean build --flavor mpas --fc nvfortran`), so that equal code gives equal bits.

## What was compared

| What | How | Result |
|---|---|---|
| Output files | lean's history, diag and restart files against MPAS strict's, every shared variable, on `greece_60km` and `x1_2562`: the default streams, frequent diagnostics, 17 runs with schemes `'off'` or other options (`off_*`, `opt_*` specs), a 06:30 start, a time step of 2.5 s, the surface update (`sfc_upd_7h`, `sfc_ice_7h`), restart files at 06:00 and 12:00; and one 3 km run directory (`athens_3km`) | bit-identical, apart from the differences in the last bit listed in [fixes.md](fixes.md) ("Summation order of the diagnostics") |
| The physics, step by step | flavor `mpas`, a cold start, then the restart files MPAS strict writes after 1 and after 10 steps (`strict_bit10` specs) against lean's state after the same steps | 0 of 143 fields differ on `greece_60km` and `athens_3km`; `x1_2562` is compared through its restart files instead (next row) |
| Restart files against MPAS | `strict_rst_a12h` on `x1_2562`: lean's restart file at 12:00 against MPAS strict's, every shared field | equal, the sea-ice and land-ice columns included |
| Restart, lean to lean | a run split by a restart against the continuous run: 22 cases on the CPU, on the card, both stream forms, with the surface update, from times on and off the radiation and boundary cycles | byte-identical files |
| gfortran | lean built with gfortran against MPAS strict's files at the start | the last-bit differences in the table below; MPAS's own expressions compiled with the same gfortran give lean's bits exactly |
| The card | flavor `mpas` on the card against the CPU build | not bit-identical (another math library, other summation in parallel reductions); within the spread of legitimate CPU builds of different optimisation, and the same bits from run to run |
| Reading the files | every comparison reads lean's files with netCDF4, xarray and `ncdump` beside MPAS's | read as MPAS's; `convert_mpas` itself was not run on lean's files |
| The stop messages | `tests/namelist/` (321 cases), `tests/inputs/`, `tests/restart/` (an MPAS restart file, a lean restart file of another layout), `tests/options/` (scheme sets), `tests/surface/` (the surface-update stops), `tests/memory/` (a mesh that does not fit) | every message as documented; the card's memory stop is tested through a test hook, not on a full card |
| Installing from the README | a tester (a person or an AI agent) given only the README and an MPAS run directory, with an empty home folder on a Mac with Homebrew: clone to a finished 3 h `greece_60km` forecast (`tests/stranger/`) | finished, from the README alone |

What gfortran gives differently at the start (the comparison accepts these maxima only for a gfortran build; nvfortran
must be bit-identical):

| Mesh | Field | Changed values | Largest difference | Cause |
|---|---|---:|---:|---|
| `greece_60km`, `x1_2562` | `pressure` | 835 of 105,165; 985 of 115,290 | 0.015625 Pa | MPAS's own expressions (`dynamics/mpas_atm_time_integration.F:7545-7550, 7605-7652`, `mpas_atm_core.F:962`) compiled with gfortran give lean's bits |
| `greece_60km` | `uReconstructMeridional` | 19 | 1.9e-6 m/s | identical with nvfortran |
| `x1_2562` | `sh2o` | 8 | 1.5e-8 m3/m3 | the fractional power of `NoahmpInitMainMod.F90:96-102` |

The isobaric and convective diagnostics follow from these inputs: MPAS's own diagnostic routines, compiled with the
same gfortran on the same inputs, reproduce every difference (`tests/diag/oracle_*.txt` hold the counts and maxima).

## Speed

Speed is given only as ratios: MPAS and lean on the same machine, with the same run directory and the same cores.
Absolute times depend on the hardware and are not given.

### Method

- The case is `athens_3km` (`cases/athens_3km/`): a 3 km regional mesh around Athens, GFS-driven, dt 18 s, radiation
  every 15 min, history every 3 h and diagnostics every hour (`cases/athens_3km/mpas_runs/atm8_6h.txt`). Its data are
  not public.
- The same run directory for MPAS and lean, one run at a time, the machine otherwise idle (load below 1 at each start).
- Both sides pinned with `taskset` to the same cores: 8 cores of the same kind for both sides; on a CPU with two kinds
  of cores, use the same kind for both. Check the pinning on the running processes (`taskset -cp PID`): the `mpirun`
  of NVHPC's OpenMPI ignores an outer `taskset` and binds rank i to CPU i.
- For the CPU comparison the inputs are renumbered along a Hilbert curve (`tools/renumber_run.py`) for both sides.
- The time per step and per component comes from the timers in the two logs (`tools/speed_table.py`); the tables give
  the ratios. Compilers: gfortran 13.2, NVHPC 25.3, ifx 2025.2.

### On the card

| Athens 3 km, 6 h | MPAS 8.4.2 | lean_mpas |
|---|---|---|
| Setup | MPAS's OpenACC build, 8 ranks sharing the card through MPS | flavor `lean`, 1 core and the card |
| Time for the forecast | 1 | 0.16: **6.1x faster** |
| Memory | card 5,669 MiB; host 8 x 1.44 GB | host 4.9 GB |

A whole day with lean (4,800 steps, writing only the products and storm outputs) took, per forecast hour, about 0.7 of
the time of the 6 h run above; card peak 4,616 MiB.

The lean build of these rows fixed the mesh size and the time step at compile time; a build of this release reads them
from the run directory. Only the level count can be fixed at compile time (`./lean build --levels N`). On this case,
one forecast hour on the card, a `--levels 45` build of this release ran within 1 % of the build of these rows, and a
build without `--levels` about 5 % slower.

### On plain cores

8 cores of one kind, the inputs renumbered for both sides; MPAS on 8 ranks, lean on 8 OpenMP threads (`--omp`):

| Build | lean faster by |
|---|---|
| gfortran -O3 on both sides (lean `--profile envelope --variant o3`) | 1.87x |
| gfortran: MPAS -O3 -march=native, lean `--profile fast --variant fast` | 1.97x |
| lean with ifx `--profile fast --variant fast`; MPAS as above (no ifx build of MPAS) | 2.29x |

Per component with gfortran (MPAS -O3 -march=native against lean `fast`):

| Component | lean faster by | share of MPAS's step | share of lean's step |
|---|---:|---:|---:|
| large-step tendencies | 1.6x | 18 % | 22 % |
| acoustic steps with divergence damping | 0.99x (equal) | 11 % | 21 % |
| recover | 1.14x | 5 % | 8 % |
| solve diagnostics | 1.29x | 4 % | 6 % |
| monotonic transport | 1.74x | 8 % | 9 % |
| physics with microphysics and radiation | 4.0x | 41 % | 20 % |
| the rest of the step | 1.96x | 14 % | 14 % |

- The two `fast` rows were measured with `--levels 45`. In a separate 100-step comparison on this case, not on the
  8-thread setup of the table, fixing the level count was worth about 6 % with gfortran and 5 % with ifx. The -O3 row
  takes the level count from the mesh. The `fast` variants (`config/flags.json`) are accepted by forecast
  sanity, not by bits: after 100 steps T2 differs from the -O3 build by 0.04 K rms.
- Renumbering the inputs made lean 23 % faster (MPAS 5 %): in the original order an edge's neighbours lie a median of
  29,000 indices apart.
- The acoustic steps, the recovery and the transport are bound by memory bandwidth at 8 threads (the STREAM triad on
  8 cores gives about 2.5 times the bandwidth of one core).
- MPAS's gfortran build with Ubuntu's MPICH 4.0.2 and gfortran 13 gives wrong halo values on 8 ranks through MPAS's
  `mpi_f08` interface; build it with `MPAS_MPI_F08=0`.

### Small cases

`greece_60km`, 3 h, runs on one core of a laptop with the README's gfortran build, in about 0.6 GB of memory.
