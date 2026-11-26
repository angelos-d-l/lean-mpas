# lean_mpas

lean_mpas replaces `atmosphere_model` of MPAS-Atmosphere 8.4.2 in an MPAS run directory. It reads the same
`namelist.atmosphere`, `streams.atmosphere`, stream list files, init, boundary and table files, prints MPAS's log
lines, and writes the history, diag and restart files the streams ask for. It runs as one process, on the CPU or on
one NVIDIA card, in single precision, with one physics suite: MPAS's `convection_permitting` with Noah-MP (Thompson
microphysics, Grell-Freitas convection, MYNN PBL and surface layer, Noah-MP, RRTMG longwave and shortwave, YSU
gravity-wave drag). Any scheme may be `'off'` where MPAS allows it.

It does not replace the rest of MPAS: you make the mesh, the static, init and boundary files with MPAS's own tools
(`init_atmosphere`) as usual. lean_mpas runs regional and global meshes with up to 60 vertical levels. It reads and
writes NetCDF through netCDF-C; it has no MPI.

**Version 0.1.0**, the first public release. It is an experiment by one modeller, tested on one Linux machine with an
NVIDIA card and on a Mac. It is not an official MPAS product and is not supported by NCAR.

| Athens 3 km mesh, the same machine and the same run directory on both sides | lean_mpas against MPAS 8.4.2 |
|---|---|
| On the card: MPAS's OpenACC build on 8 MPI ranks sharing the card; lean on 1 core and the same card (6 h forecast) | about 6x faster (6.1x) |
| On 8 CPU cores: MPAS on 8 MPI ranks, lean on 8 OpenMP threads, the same cores, gfortran -O3 on both sides | about 1.9x faster |
| Build options of the CPU row | lean: `./lean build --flavor lean --profile envelope --variant o3 --omp`; MPAS: its gfortran build with -O3 |

These are ratios measured on one machine, one run at a time; absolute times depend on the hardware. The card row was
measured with a build that fixed the mesh size and the time step at compile time; a build of this release reads them
from the run directory. On the same case a `--target gpu --levels 45` build of this release ran within 1 % of that
build, and a build without `--levels` about 5 % slower. How it was measured, and the other comparisons with MPAS, are in `docs/acceptance.md`.
Where lean departs from MPAS on purpose (bugs fixed, behaviour changed) is in `docs/fixes.md`.

## 1. What you need

| Tool | Version |
|---|---|
| gfortran | 13 or newer |
| CMake | 3.28 or newer |
| Ninja | 1.10 or newer |
| Python | 3.9 or newer (standard library only) |
| git | any |
| netCDF-C | built with CDF-5 (the default since 4.5); found through `NETCDF=<prefix>`, else the first `nc-config` on `PATH` |

On Ubuntu 24.04 (as root, or with `sudo` in front):

```sh
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y git gfortran cmake ninja-build python3 libnetcdf-dev
```

On macOS with Homebrew:

```sh
brew install gcc cmake ninja netcdf python git
```

MacPorts' netCDF-C 4.9.2 has no CDF-5: lean_mpas passes over it (and says so) and takes the next `nc-config` on
`PATH`, so put Homebrew's first or set `NETCDF=$(brew --prefix netcdf)`.

MPI and PnetCDF are not needed. lean_mpas reads and writes through netCDF-C even where `streams.atmosphere` says
`io_type="pnetcdf"`. You need them only to build upstream MPAS for preparing inputs.

Then, in the clone (next section), check the tools:

```sh
./lean doctor
```

It prints one line per requirement and ends with `DOCTOR OK`, or `DOCTOR FAIL: <n> missing`. A line that starts with
`E_` is a missing requirement, with the command that installs it; a line that starts with `W_` is a warning only. In a
fresh clone the doctor prints four `W_NO_TABLES` lines for lean's own copy of the Thompson tables: a run directory
brings its own (section 3), so these do not stop a forecast.

## 2. Build

```sh
git clone https://github.com/angelos-d-l/lean-mpas.git lean_mpas
cd lean_mpas
./lean build --flavor lean
```

The build uses two compile jobs (`--jobs N` to change) and takes a few minutes.
It ends with

```
BUILD OK build/cpu-gfortran-strict-lean/lean_mpas
```

That file is the executable. Beside it, `build.json` records what was built: the commit, the flags, the compiler and
netCDF-C. In a fresh clone the build also prints `W_NO_TABLES`, for the same reason as the doctor: ignore it.

- **Flavors.** `--flavor lean` is MPAS's schemes with a set of physics changes switched on at build time (the list is
  in `config/presets.json`, each change in `docs/fixes.md`). `--flavor mpas` is MPAS 8.4.2's physics unchanged, for a
  comparison with MPAS. The flavor goes into the log, into every output file and into restart files.
- **One executable for every run directory.** The mesh size, the number of levels, the time step, the radiation
  intervals and the other namelist values come from the run directory when the run starts.
- `--levels N` fixes the number of vertical levels at compile time instead. The loops over levels then have a known
  length, which the card compiler uses: on the test card an `athens_3km` run took about 5 % longer without it. Such an
  executable runs only meshes with `nVertLevels` = N and stops at the start on any other.
- `--omp` builds with OpenMP threads: then set `OMP_NUM_THREADS` and `OMP_STACKSIZE=256M` before the run.

## 3. Your run directory

Start from a run directory in which MPAS 8.4.2's `atmosphere_model` would run. In `namelist.atmosphere`, set these two
options in the `&physics` record (add a line where the option is not there yet):

```
config_physics_suite = 'convection_permitting'
config_lsm_scheme = 'sf_noahmp'
```

lean_mpas has only this suite's schemes and only Noah-MP; with another suite or land model it stops at the start and
prints these two lines. `config_physics_suite = 'none'` (no physics) also runs.
Moving from Noah (`'sf_noah'`, MPAS's default) to Noah-MP changes the forecast.

lean_mpas reads these tables from the run directory, as MPAS does:

- `LANDUSE.TBL`, `OZONE_DAT.TBL`, `OZONE_LAT.TBL`, `OZONE_PLEV.TBL`, `RRTMG_LW_DATA`, `RRTMG_SW_DATA`: MPAS's build
  links them into the top of its source tree from `src/core_atmosphere/physics/physics_wrf/files/`;
- `NoahmpTable.TBL`: linked there from `src/core_atmosphere/physics/physics_noahmp/parameters/`;
- the four `MP_THOMPSON_*_DATA.DBL` (`QRacrQG`, `QRacrQS`, `freezeH2O`, `QIautQS`): written by MPAS's `build_tables`.
  MPAS's big-endian tables and lean's own little-endian ones both work; the four must have the same byte order.

The streams file, its stream list files and the boundary files stay as for MPAS: the `input` stream names the init
file (or `invariant` and `input` name the static and init files), and with `config_apply_lbcs = true` the `lbc_in`
stream names the boundary files, one per `input_interval`, at the times MPAS would read.

Options lean_mpas reads as MPAS does: `config_start_time`, `config_run_duration`, `config_dt`, `config_len_disp`,
`config_radtlw_interval`, `config_radtsw_interval`, `config_apply_lbcs`, `config_do_restart`,
`config_noahmp_iopt_sfc` (1 or 2), `config_apvm_upwinding` (MPAS's default is 0.5; the test cases use 0.0),
`config_microp_re`, `config_radt_cld_scheme` (`cld_fraction`, `cld_incidence`, or `'off'`, which runs as
`cld_incidence`) and `config_sst_update`. With
`config_sst_update = true` the sea-surface temperature and the sea ice are updated from the records of the `surface`
stream, every `input_interval` of that stream; a `surface` stream without an `input_interval` stops the run. Each
scheme (`config_microp_scheme`, `config_convection_scheme`, `config_pbl_scheme`, `config_sfclayer_scheme`,
`config_lsm_scheme`, `config_radt_lw_scheme`, `config_radt_sw_scheme`, `config_gwdo_scheme`) may be `'off'`; the sets
MPAS 8.4.2 cannot run (microphysics `'off'` with another scheme on, convection or gravity-wave drag without the PBL,
the surface layer `'off'` with MYNN, shortwave `'off'` with longwave on) stop at the start with a message that says
what to set. Options of the dynamics that lean_mpas has in one form only must keep that value (for example
`config_horiz_mixing = '2d_smagorinsky'`); MPAS's defaults all fit, and another value stops the start with a line that
names the option and the value lean_mpas supports.

## 4. Run

```sh
cd /path/to/run_directory
ln -sf /path/to/lean_mpas/build/cpu-gfortran-strict-lean/lean_mpas atmosphere_model
./atmosphere_model
```

`./atmosphere_model -n NAMELIST -s STREAMS` reads other file names, as MPAS does. Start it directly, never under
`mpirun`, `mpiexec` or `srun`: it is one process and refuses more than one task. One run per run directory.

It writes `log.atmosphere.0000.out` as MPAS does, with lean_mpas's flavor and build in the header and the timers and
message counts at the end. Warnings that MPAS prints too, such as `Variable qi not in input file.`, mean the same as
in MPAS. The exit status is 0 when the forecast finished. When it stops, the reason is in
`log.atmosphere.0000.err` as well, every problem it found on one `ERROR:` line, and the exit status is 1 (2 when it
refuses to start before it opens the log: under MPI, or with another run in the same directory). A gfortran build
then also prints `ERROR STOP 1` and a backtrace on the terminal: that is the stop, not a crash. Section 9 lists what
the messages mean.

## 5. On an NVIDIA card

A card build needs the NVIDIA HPC SDK, NVHPC 25.3 (tested; it brings CUDA 12.8), on `PATH`, and a driver whose CUDA
version is at least the one nvfortran builds for. Then:

```sh
./lean doctor --target gpu
./lean build --flavor lean --target gpu
```

The doctor warns (`W_GPU_UNTESTED`, `W_NVHPC_UNTESTED`) on a card or an NVHPC we have not tested; the build compiles
for the card's compute capability (CUDA device 0; `--gpu-cc N` chooses another). It ends with `BUILD OK
build/gpu-nvfortran-strict-lean/lean_mpas`. Link and start it as in section 4.
A card build runs the dynamics and the whole physics on the card: an unset `LEAN_CARD` means all;
`LEAN_CARD=none` keeps the physics on the host.

## 6. Memory

Before it allocates the mesh, lean_mpas adds up the memory the run will use and prints it in the log:

```
Memory check: nCells <n>, nEdges <n>, nVertices <n>, nVertLevels <n>, <g> glacier columns
  host: <need> MiB needed (<sum> MiB and <h> MiB headroom), <avail> MiB available (<source>)
```

with a third line for the card in a card build. When the run does not fit it stops there, with both numbers, and
names a `./lean doctor` command that lists what the sum is made of: `The mesh of <file> does not fit in host memory`,
or `... does not fit on the card`. `config_lean_memory_check = .false.` in a `&lean` record of the namelist turns
the stop into a warning. The card is shared: a process that takes card memory after the check can still make the run
fail.

Before building, the doctor estimates the same sum for an init file, and with `--build` gives the sum of that build:

```sh
./lean doctor --init /path/to/run_directory/x1.40962.init.nc
./lean doctor --run-dir /path/to/run_directory --build build/cpu-gfortran-strict-lean
```

## 7. What it writes

The files the streams ask for, named and laid out as MPAS 8.4.2 writes them: the same names, dimensions and
attributes for the fields lean_mpas has, and a global attribute `lean_flavor`. A stream list that names a package of
fields, such as `scalars` in MPAS's default `stream_list.atmosphere.output`, writes each of its fields (`qv`, `qc`,
`qr`, ...) as MPAS does. A file is CDF-5 unless the stream's
`io_type` asks for `netcdf` (64-bit offset) or `netcdf4`. Double-precision inputs are read into single precision.

Four optional outputs of lean's own (a film of surface fields every 10 minutes, hourly map products, native-mesh storm
maxima, Noah-MP surface fields) are switched on by environment variables: `docs/output-formats.md`.

## 8. Restart

Set an `output_interval` on the `restart` stream to write a checkpoint. In a fresh run directory with the same inputs
and tables, link or copy the checkpoint, set `config_do_restart = .true.` under `&restart`, and set the remaining run
duration. Use the checkpoint's time as `config_start_time`, or copy its `restart_timestamp` and set
`config_start_time = 'file'`.

For a limited-area restart between boundary records, set `reference_time` on `lbc_in` to the original run's boundary
schedule anchor. Use the same lean_mpas flavor and switches. The model refuses MPAS restart files. More in
[restart setup and inventory](docs/restart.md).

## 9. When it stops

| The line contains | What to do |
|---|---|
| `To run lean_mpas, set in &physics:` | Set the suite and the land model of section 3, and put `NoahmpTable.TBL` in the run directory. |
| `: lean_mpas supports only` | An option lean_mpas has in one form only: set the value the line names. |
| `land cells with background albedo below 1 %` | A warning: MPAS 8's static step leaves small coastal and island land cells at about 0.08 % background albedo where the MODIS data has only missing pixels; `tools/mesh/fix_albedo.py` fills them. |
| `meshDensity does not match the cell sizes` | A warning: a mesh made with `build_spherical_mesh` can carry meshDensity 1 everywhere, so every cell is treated as the finest; `tools/mesh/set_mesh_density.py` corrects the mesh file. |
| `does not fit in host memory`, `does not fit on the card` | Section 6: a smaller mesh, fewer levels, or more memory. |
| `this executable was built for` | A `--levels` build and a mesh with another level count: build with the mesh's count, or without `--levels`. |
| `lean_mpas cannot read the card's memory` | The card's driver did not answer; `config_lean_memory_check = .false.` runs without the card part of the check. |
| `an MPI launcher started it with more than one task` | Start it without `mpirun` (section 4). |
| `but this executable is the` | The namelist's `config_lean_flavor` asks for the other flavor: build that flavor, or remove the option. |
| `is not a lean_mpas restart file` | lean_mpas cannot continue from an MPAS restart file: start cold, or from a lean_mpas restart. |
| `is missing: lean_mpas reads MPAS's physics tables from the run directory` | Put the table in the run directory (section 3). |
| `mix byte orders` | Use four Thompson tables of one byte order. |
| `another run holds this run directory` | One run per run directory. |
| `: no variable` | The init or boundary file lacks a field lean_mpas reads: make it with MPAS 8.4.2's `init_atmosphere`. |

A stop at the start ends with `CRITICAL ERROR: lean_mpas stops: <n> ... problem(s) listed above`.

## For developers

- `AGENTS.md`: for an AI coding agent or a person who changes the code: the checks, where a change goes, and how to see
  what it did to the forecast (`tools/compare_runs.py`).
- `docs/code-map.md`: where things are, one model step file by file, the build, what came from which MPAS file.
- `docs/running.md`: the start-up checks, the logs and exit status, threads, the card, the tests.
- `docs/output-formats.md`: lean's optional outputs (film, products, storm, surface) and how to read them.
- `docs/fixes.md`: every place where lean departs from MPAS 8.4.2, and why.
- `docs/acceptance.md`: what was compared with MPAS, and the speed tables.
- `docs/namelist.md`, `docs/streams.md`, `docs/fields.md`, `docs/restart.md`, `docs/memory.md`: every namelist option,
  the streams and the clock, the output fields, the restart file, the memory check.
- `cases/README.md`: the test cases, where their inputs are, how to run them through MPAS and lean. `linux/` holds the
  scripts that run them side by side on a Linux machine.

## Licence

lean_mpas's own code is under the BSD 3-clause licence (`LICENSE`). Code taken or translated from MPAS-Atmosphere
8.4.2 and from the packages MPAS ships keeps the terms of its source: `NOTICE.md` says which part comes from where,
and the texts are in `LICENSE-MPAS` and `licenses/`. One of them decides for the whole package: lean_mpas contains
Noah-MP, whose terms allow free use, changes and redistribution and forbid selling Noah-MP or a work that contains
it. So lean_mpas may be used, changed and passed on without charge, and may not be sold.
