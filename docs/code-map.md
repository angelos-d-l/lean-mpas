# Code map

Where things are in the tree, what one model step runs and in which file, how the build is put together, and which
MPAS 8.4.2 file each part comes from.

## Folders

| Path | What it holds |
|---|---|
| `AGENTS.md` | how to change the code without breaking it: the checks, where a change goes |
| `lean` | the command line: `./lean doctor`, `build`, `tables`, `fetch-upstream`, `generate` |
| `lean.lock.json` | the pins: MPAS-Model and its externals, tool versions, NVHPC and CUDA, the Thompson tables' sums |
| `CMakeLists.txt` | the CMake project `./lean build` configures |
| `config/` | what the build and the generators read (below) |
| `src/` | the model: host sources, one file per module |
| `src/run/` | start-up: command line, log, namelist, streams file, clock, memory check, the order of MPAS's start |
| `src/io/` | NetCDF through netCDF-C: the inputs of the run directory, the boundary records, the output writer, restart files |
| `src/diag/` | the output diagnostics: isobaric and convective fields, the diagnostics manager |
| `src/device/` | the card's copies of physics, radiation and dynamics files (below) |
| `third_party/mpas-8.4.2/` | the MPAS files compiled unchanged: kinds, constants, RRTMG, the ESMF time manager, Noah-MP |
| `reference/` | MPAS's tables and default namelists and streams; `reference/mpas_src/` after `./lean fetch-upstream` ([reference/README.md](../reference/README.md)) |
| `reference_fixes/` | MPAS source files with only the fixes of [fixes.md](fixes.md), for the tests that compile MPAS's code |
| `tools/` | the generators, the converters of the optional outputs, mesh fixes, case helpers, `compare_runs.py` (two run directories value for value) |
| `tools/lean/` | the implementation of `./lean`: configure, build, doctor, tables, upstream, vendor, memory estimate |
| `tests/` | the test suites, run by `tests/run_all.sh` |
| `cases/` | the test cases: init and run specs, MPAS reference headers, checksums ([cases/README.md](../cases/README.md)) |
| `linux/` | scripts that run MPAS 8.4.2 and lean side by side on the cases on a Linux machine |
| `docs/` | this documentation |
| `data/`, `build/`, `runs/` | made locally and ignored by git: tables and case inputs, build folders, run directories |

## Start-up

`src/run/main.f90` reads the command line and calls, in MPAS's order (`driver/mpas_subdriver.F:150-384`, then
`core_atmosphere/mpas_atm_core.F:118-251`):

1. `src/run/lean_drop_in.f90` `drop_in_start`: the launcher check and the run-directory lock (`src/run/lean_guard.f90`),
   the log (`src/run/lean_log.f90`), the namelist (`src/run/lean_namelist.f90`, its option table generated into
   `src/run/lean_namelist_options.f90`), the streams file (`src/run/lean_streams.f90`, `src/run/lean_xml.f90`, the
   Registry streams in `src/run/lean_stream_registry.f90`), the clock (`src/run/lean_clock.f90`), the memory check
   (`src/run/lean_memory.f90`), the mesh and initial state (`src/io/lean_inputs.f90`, `src/io/lean_input_fields.f90`),
   the boundary schedule (`src/io/lean_lbc.f90`), the checks of the inputs and the streams
   (`src/io/lean_run_inputs.f90`).
2. `src/lean_model.f90` `run_model_from`: the physics schemes' start (`thompson_init`, MYNN), the allocations, the
   dynamics start (`model_init`: the mesh vectors of `src/mesh_vectors.f90`, the coefficients of `src/dyn_startup.f90`,
   a cold start in `src/cold_dyn.f90` or a lean restart in `src/io/lean_restart.f90`), the physics columns (cold:
   `src/cold_column.f90`, `src/landuse_startup.f90`, `src/noahmp_cold_start.f90`, sea ice and glacier), the tables,
   the output streams and the optional outputs, the card start, then the time loop.
3. `drop_in_finish`: the timers, the message counts, the closing lines of the log.

## One model step

`src/lean_model.f90` drives the step; the time loop is in `run_model_from`. In order:

| Part | What | File (host) |
|---|---|---|
| surface updates | the daily vegetation update; the sea-surface and sea-ice update of `config_sst_update` | `src/surface_update.f90`, `src/surface_bdy.f90` |
| start of step | time levels shifted, cell winds reconstructed, Runge-Kutta setup | `src/lean_model.f90` `model_start_step`, `src/dyn_step.f90` |
| boundary | a new boundary record when `lbc_in` rings | `src/io/lean_lbc.f90`, `src/dyn_boundary.f90` |
| physics inputs | dynamics state to physics columns | `src/physics_inputs.f90` |
| radiation | RRTMG longwave and shortwave at their alarms, with cloud fraction and ozone | `src/radiation_column.f90`, `src/radiation_lw.f90`, `src/radiation_sw.f90` and their kernels, `src/radiation_cloud.f90`, `src/radiation_ozone.f90`, `src/radiation_solar.f90` |
| surface layer | MYNN surface layer; Noah sea ice on ice cells | `src/mynn_surface.f90`, `src/sea_ice.f90`, `src/sea_ice_noah.f90` |
| land | Noah-MP on land cells, its glacier driver on land ice | `src/noahmp_coupling.f90`, `src/noahmp.f90`, `src/noahmp_core.f90`, `src/land_ice.f90` |
| PBL | MYNN with EDMF | `src/mynn.f90`, `src/mynn_pbl.f90`, `src/mynn_pbl_helpers.f90`, `src/mynn_conversions.f90` |
| gravity-wave drag | YSU/Kim orographic drag | `src/gwdo.f90`, `src/gwdo_core.f90` |
| convection | Grell-Freitas | `src/gf.f90`, `src/gf_core.f90` |
| tendencies | physics tendencies to the dynamics, mass coupled | `src/pre_dynamics.f90`, `src/physics_tendency.f90`, `src/dyn_handoff.f90` |
| dynamics | 3 substeps x 3 Runge-Kutta stages: large-step tendencies, acoustic steps with divergence damping, recovery, boundary zones, solve diagnostics | `src/dyn_step.f90` `dynamics_step`, `src/dyn_tend.f90`, `src/dyn_acoustic.f90`, `src/dyn_diagnostics.f90`, `src/dyn_boundary.f90` |
| transport | 3 stages of split scalar transport, the last monotonic | `src/dyn_scalars.f90` |
| microphysics | Thompson after the dynamics, the step's precipitation | `src/dyn_micro.f90` `model_step`, `src/thompson.f90` |
| end of step | the specified zone set from the boundary data, the extrema printed | `src/dyn_step.f90` `end_of_step_boundary`, `src/run/lean_summary.f90` |
| outputs | the optional outputs; then, when a stream rings, the diagnostics and the stream files and restart | `src/storm_output.f90`, `src/film_output.f90`, `src/product_output.f90`, `src/surface_output.f90`; `src/diag/`, `src/lean_fields.f90`, `src/io/lean_writer.f90`, `src/io/lean_restart.f90` |

Per step that is 9 large-step tendency calls, 24 acoustic steps (2 + 2 + 4 per substep) and 3 transport stages, as in
MPAS's `atm_srk3`.

The data every part shares: `src/model_data.f90` (mesh, reference state, dynamics fields, boundary data), the physics
state types in `src/state.f90`, `src/noahmp_state.f90`, `src/gf_state.f90`, `src/gwdo_state.f90`, the sizes in
`src/config.f90` and `src/levels.f90`, the run's settings in `src/run_settings.f90` and `src/dyn_config.f90`, which
schemes run in `src/schemes.f90`.

## Host and card

A CPU build compiles `src/` (and `src/device/micro_pool.f90`, below). A card build (`--target gpu`, nvfortran with OpenACC) replaces some files by their
namesake in `src/device/` (`config/units.json`, `device_overrides`) and adds a few card-only files:

| Card file | Made by | What differs from `src/` |
|---|---|---|
| physics: `gf*`, `gwdo*`, `mynn*` (except `mynn_card.f90`), `noahmp*`, `sea_ice*`, `land_ice_exchange`, `physics_inputs`, `physics_tendency`, `state`, `thompson` | `tools/gen_device_physics.py` from the `src/` file of the same name | `!$acc routine seq` column routines, work arrays from pools, the card's math functions |
| `micro_pool.f90` | `tools/gen_device_physics.py` from the pooled column routines of `src/thompson.f90`, `src/gf_core.f90`, `src/gf.f90` | no `src/` namesake: the work-array pools of the card's column routines; compiled in every build (also CPU), allocated only by a card build |
| radiation: `radiation_*` (except the three below), `radiation_device_tables` | `tools/gen_device_radiation.py` from `src/radiation_*.f90` and RRTMG's data modules | the same, with the tables on the card |
| `radiation_fast_gas.f90` | `tools/gen_radiation_fast_gas.py` from the card's RRTMG kernels | the gas optics of the g-point path |
| `radiation_fast.f90`, `radiation_fast_data.f90`, `mynn_card.f90` | written by hand | RRTMG with the g-point as a thread; MYNN as stages over column chunks |
| dynamics: `dyn_acoustic`, `dyn_boundary`, `dyn_diagnostics`, `dyn_scalars`, `dyn_tend` | written by hand | the card's loop form (one thread per level and point, values recomputed where they are read); `src/dyn_*.f90` keep the CPU's (neighbour loop outside, level loop inside, values in arrays). The arithmetic is the same. |

Do not edit a generated file: change its `src/` source and run `./lean generate` (`./lean generate --check` tells
whether `src/device/` is current). `src/phys_dev.f90` runs the physics as stages on the card or the host, chosen by
`LEAN_CARD` (`src/phys_mode.f90`); `src/dyn_device.f90` puts the dynamics on the card once at the start.

On the CPU, each phase of a step is one OpenMP parallel region in `src/lean_model.f90` whose routines hold orphaned
worksharing loops ([running.md](running.md)).

Other generated sources, each regenerated or checked with `--check` by its generator:

| Generator | Writes |
|---|---|
| `tools/gen_phys_state.py` | `src/phys_state.f90`: the physics state member by member for the card |
| `tools/gen_phys_check.py` | `src/phys_check.f90`: the card-against-host comparison of `LEAN_CHECK` |
| `tools/gen_namelist.py` | `src/run/lean_namelist_options.f90`, [namelist.md](namelist.md), from `config/namelist.json` and MPAS's Registry |
| `tools/gen_stream_registry.py` | `src/run/lean_stream_registry.f90`, from MPAS's Registry |
| `tools/gen_field_catalog.py` | `src/run/lean_field_catalog.f90`, the attributes of every output field, from MPAS's Registry |
| `tools/gen_fields_doc.py` | [fields.md](fields.md) |
| `tools/gen_restart_set.py` | `src/io/lean_restart_set.f90`, `src/io/lean_restart_layout.f90`, [restart.md](restart.md), from `config/restart.json` |
| `tools/gen_memory.py` | `src/run/lean_memory_terms.f90`, `config/memory_terms.json`, [memory.md](memory.md), from `config/memory.json` |

## Build

`./lean build` (`tools/lean/cli.py`, `tools/lean/build.py`) takes the flavor's settings from `config/presets.json`,
adds `--switch` options, makes a `BuildConfig` (`tools/lean/config.py`), configures `CMakeLists.txt` and builds with
Ninja into `build/<target>-<compiler>-<profile>[-<variant>][-omp][-levels<N>][-lean | -<hash>]/`, with `build.json`
beside the executable. Flavor `mpas` adds nothing and flavor `lean` adds `-lean`. A switch set that is neither flavor
(any `--switch`, `LEAN_BUDGET` included) adds the first 8 hex digits of a hash of the build kind and its defines.

| File | What it gives the build |
|---|---|
| `config/presets.json` | the two flavors, `mpas` (no switch) and `lean` (the switches of [fixes.md](fixes.md) section 2) |
| `config/switches.json` | every build switch: its kind (on/off or a number) and what it does |
| `config/flags.json` | the flags per compiler and profile (`strict`, `debug`, `fast`, `envelope`), the variants, the compile classes |
| `config/units.json` | the source files by group in compile order, the card's replacements, the files compiled only with a switch |
| `config/namelist.json`, `config/restart.json`, `config/memory.json` | the inputs of the namelist, restart and memory generators |

`tools/lean/vendor.py` copies the MPAS files of `third_party/mpas-8.4.2/` from `reference/mpas_src/` and checks them
against `third_party/mpas-8.4.2/SHA256SUMS`; `tools/lean/tables.py` places and checks the Thompson tables.

## What came from MPAS 8.4.2

The terms of each part's source are in [NOTICE.md](../NOTICE.md).

Paths are relative to MPAS's `src/core_atmosphere/`, except those that start with `framework/`, `external/` or
`tools/` (relative to `src/`). The translated files keep MPAS's formulas and their order of operations; where they
depart, [fixes.md](fixes.md) says so.

| lean | MPAS 8.4.2 |
|---|---|
| `src/dyn_step.f90` | `dynamics/mpas_atm_time_integration.F`: `atm_srk3`, `atm_rk_integration_setup`, `atm_rk_dynamics_substep_finish` |
| `src/dyn_tend.f90` | `atm_compute_dyn_tend_work` (same file) |
| `src/dyn_acoustic.f90` | `atm_set_smlstep_pert_variables`, `atm_advance_acoustic_step`, `atm_divergence_damping_3d`, `atm_recover_large_step_variables` |
| `src/dyn_diagnostics.f90` | `atm_compute_moist_coefficients`, `atm_compute_solve_diagnostics`, `atm_compute_vert_imp_coefs` |
| `src/dyn_scalars.f90` | `atm_advance_scalars_work`, `atm_advance_scalars_mono_work` |
| `src/dyn_boundary.f90` | `dynamics/mpas_atm_boundaries.F` and the `atm_bdy_*` routines of the time integration |
| `src/dyn_startup.f90`, `src/mesh_vectors.f90`, `src/winds.f90` | `mpas_atm_core.F` (`atm_mpas_init_block`, `atm_compute_damping_coefs`), `operators/mpas_vector_operations.F`, `operators/mpas_vector_reconstruction.F` |
| `src/cold_dyn.f90` | the start from the `input` stream in `mpas_atm_core.F` and `dynamics/mpas_atm_time_integration.F` |
| `src/dyn_micro.f90`, `src/physics.f90`, `src/thompson.f90` | `physics/mpas_atmphys_driver_microphysics.F`, `physics/mpas_atmphys_interface.F`, `physics/physics_wrf/module_mp_thompson.F` |
| `src/physics_inputs.f90`, `src/pre_dynamics.f90`, `src/physics_tendency.f90`, `src/dyn_handoff.f90` | `physics/mpas_atmphys_interface.F`, `physics/mpas_atmphys_driver.F`, `physics/mpas_atmphys_todynamics.F` |
| `src/mynn*.f90` | `physics/physics_mmm/bl_mynn.F90`, `bl_mynn_subroutines.F90`, `sf_mynn.F90`, `physics/physics_wrf/bl_mynn_pre.F`, `bl_mynn_post.F`, `physics/mpas_atmphys_driver_pbl.F`, `physics/mpas_atmphys_driver_sfclayer.F` |
| `src/gf.f90`, `src/gf_core.f90` | `physics/physics_wrf/module_cu_gf.mpas.F`, `physics/mpas_atmphys_driver_convection.F` |
| `src/gwdo.f90`, `src/gwdo_core.f90` | `physics/physics_mmm/bl_gwdo.F90`, `physics/physics_wrf/module_bl_gwdo.F`, `physics/mpas_atmphys_driver_gwdo.F` |
| `src/noahmp*.f90`, `src/land_ice*.f90` | `physics/physics_noahmp/` (`src/` and `drivers/mpas/`), `physics/mpas_atmphys_driver_lsm_noahmp.F`, `physics/mpas_atmphys_lsm_noahmpinit.F` |
| `src/sea_ice.f90`, `src/sea_ice_noah.f90` | `physics/physics_wrf/module_sf_noah_seaice.F`, `module_sf_noah_seaice_drv.F`, `physics/mpas_atmphys_driver_seaice.F` |
| `src/radiation_*.f90` | `physics/physics_wrf/module_ra_rrtmg_lw.F`, `module_ra_rrtmg_sw.F`, `physics/mpas_atmphys_driver_radiation_lw.F`, `_sw.F`, `physics/mpas_atmphys_driver_cloudiness.F`, `physics/mpas_atmphys_o3climatology.F` |
| `src/landuse_startup.f90`, `src/surface_update.f90`, `src/surface_bdy.f90` | `physics/mpas_atmphys_landuse.F`, `physics/mpas_atmphys_update_surface.F` |
| `src/diag/` | `diagnostics/mpas_isobaric_diagnostics.F`, `diagnostics/mpas_convective_diagnostics.F`, `physics/mpas_atmphys_manager.F` |
| `src/run/lean_clock.f90` | `framework/mpas_timekeeping.F` (on the ESMF time manager of `third_party/`) |
| `src/run/lean_streams.f90`, `src/run/lean_xml.f90` | `framework/xml_stream_parser.c`, `framework/mpas_stream_manager.F`, `external/ezxml/ezxml.c` |
| `src/run/lean_log.f90`, `src/run/lean_messages.f90` | `framework/mpas_log.F` |
| `src/run/lean_namelist.f90` | the namelist reading of `tools/registry/gen_inc.c` and `physics/mpas_atmphys_control.F` |
| `src/io/lean_writer.f90` | the clobber and file rules of `framework/mpas_stream_manager.F` |
| `third_party/mpas-8.4.2/src/` | compiled unchanged: [its README](../third_party/mpas-8.4.2/README.md) lists the files |

Not taken from MPAS: MPI, PIO and SMIOL (lean is one process and writes through netCDF-C), the other physics suites
and schemes, the IAU and data assimilation paths, MPAS's pools and halos.

## Tests and the data they need

`tests/run_all.sh` runs the suites in three tiers ([running.md](running.md)):

| Tier | Needs | Examples |
|---|---|---|
| `quick` | the tree and Python 3.9 | `tests/lean_cli/` (command line, configuration, generators, README), the converters of the optional outputs |
| `build` | a compiler; `reference/mpas_src/` (`./lean fetch-upstream`) for the suites that compile MPAS's own routines beside lean's | `tests/namelist/`, `tests/streams/`, `tests/netcdf/`, `tests/sea_ice/`, `tests/land_ice/` |
| `cases` | the case inputs in `data/cases/` (release assets); MPAS's reference outputs of the cases for the comparisons (not released: those parts skip) | `tests/inputs/`, `tests/diag/`, `tests/writer/`, `tests/restart/`, `tests/options/`, `tests/surface/`, `tests/memory/` |

A suite that needs nvfortran and a card skips without them.
