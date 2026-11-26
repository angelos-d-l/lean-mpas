# Where lean_mpas departs from MPAS 8.4.2

lean_mpas follows MPAS-Atmosphere 8.4.2 wherever MPAS has a defined behaviour. This page lists every place where it does
something else, one entry per departure: what changes, where MPAS has the code, why, and in which builds it is on.

MPAS paths are relative to `src/core_atmosphere/` of MPAS-Model 8.4.2, except those that start with `framework/` or
`driver/` (relative to `src/`). `./lean fetch-upstream` puts the MPAS source in `reference/mpas_src/`. Where a fix is
also applied to MPAS's own source for the tests, the patched copy is in `reference_fixes/`: the complete original file
with only the listed edit, checked by `tools/reference_fixes.py`.

The groups:

1. [Bug fixes](#1-bug-fixes): in both flavors; every build except the GF cloud-base search, which is card builds only.
2. [Physics changes of the `lean` flavor](#2-physics-changes-of-the-lean-flavor): build switches, off in flavor `mpas`.
3. [Other build switches](#3-other-build-switches): off in both flavors.
4. [Start-up, input and output](#4-start-up-input-and-output): what lean reads, refuses and writes differently.
5. [Found in MPAS, left unchanged](#5-found-in-mpas-left-unchanged).

A build switch is a preprocessor define (`./lean build --switch NAME=on`, list in `config/switches.json`). Switched off,
the code is MPAS's. `config/presets.json` gives the switch set of each flavor; the flavor and the switches are written in
the log, in every output file and in restart files, and a restart refuses a build with other switches.

## 1. Bug fixes

### GF shallow qco: an undefined read above the cloud top

- **What:** the shallow plume of Grell-Freitas (`cup_gf_sh`) reads `qco(i,k+1)` at `k = ktop(i)`, one level above the
  last level it assigns. lean sets `qco` to zero on every level before the plume, as the same loop already does for
  `qrco`.
- **MPAS:** `physics/physics_wrf/module_cu_gf.mpas.F:4784` (the read); the initial loop at 4378-4389 sets `qrco` but
  not `qco`. Patched copy: `reference_fixes/module_cu_gf.mpas.F` (one added line).
- **Why:** `detup` is zero at the cloud top, but zero times an unset value is not zero when that value is a NaN: a build
  with NaN-initialised memory and floating-point traps stops there. Normal builds gave the same numbers with and without
  the fix.
- **Default:** every build. With `LEAN_GF_NOSHALLOW` (flavor `lean`) the shallow plume does not run at all.

### Ozone climatology at the last table latitude

- **What:** a column exactly at the last latitude of the ozone table (87.8638 degrees) takes the last profile, as
  columns north of it do.
- **MPAS:** `physics/mpas_atmphys_o3climatology.F:157` tests `lat .gt. lat_ozone(latsiz)`; at equality the search loop
  reads `lat_ozone(latsiz+1)` at line 165, outside the table. lean tests `.ge.`. Patched copy:
  `reference_fixes/mpas_atmphys_o3climatology.F`.
- **Why:** an out-of-bounds read; a single-precision latitude can hit the value exactly.
- **Default:** every build.

### Alarm creation reports success explicitly

- **What:** creating an alarm sets its status to success.
- **MPAS:** `framework/mpas_timekeeping.F:559-561`, `mpas_add_clock_alarm`: on the success path `ierr` is an
  `intent(out)` argument that is read (`if (ierr == ESMF_SUCCESS)`) before anything sets it. Patched copy:
  `reference_fixes/mpas_timekeeping.F` (`if (present(ierr)) ierr = 0`), used where the tests compile MPAS's clock.
- **Why:** the value read is undefined; depending on the compiler and optimisation an alarm that was created can be
  reported as failed.
- **Default:** every build.

### Thompson sedimentation: a sub-step count beyond the integer range

- **What:** the sub-step count of the fall-speed loops is raised only when `DT/delta_tp + 1` is below 2^31.
- **MPAS:** `physics/physics_wrf/module_mp_thompson.F:3269, 3328, 3367, 3395` (rain, ice, snow, graupel):
  `nstep = MAX(nstep, INT(DT/delta_tp + 1.))`. Patched copy: `reference_fixes/module_mp_thompson.F`.
- **Why:** `INT` of a real beyond the integer range is processor dependent. On x86 it gives the most negative integer and
  the `MAX` ignores it; on an NVIDIA card it saturates to the largest integer, and the loop runs about 2e9 times, until
  the card's watchdog stops the kernel. On x86 the guarded code gives the same numbers as the original for every input.
- **Default:** every build.
- **Remaining:** a finite but very large count (order 1e6, from a degenerate column) still takes long on one card
  thread. A cap on the sub-steps would change the results of such columns and is not applied.

### summarize_timestep: the location of an extremum that was not found

- **What:** the four lines that turn the offset of an extremum into a level, cell, latitude and longitude run only when
  an offset was found. Otherwise the level and index stay -1 and the position 0, and MPAS's NaN check that follows
  stops the run with its own message.
- **MPAS:** `dynamics/mpas_atm_time_integration.F:8615-8618, 8664-8667, 8713-8716, 8762-8765, 8812-8815`: each search
  starts from `offset_1d = huge(1)`; when no value equals the extremum (an all-NaN field) that sentinel is used as an
  array index, `latCell(536870912)` in the tests. Patched copy: `reference_fixes/summarize_timestep.inc`.
- **Why:** an out-of-bounds read. For finite fields the printed lines are those of MPAS.
- **Default:** every build (`src/run/lean_summary.f90`).

### Isobaric temperature requested alone

- **What:** the temperature at cell centres is computed whenever the isobaric diagnostics run.
- **MPAS:** `diagnostics/mpas_isobaric_diagnostics.F:643` computes it only when temperature, relative humidity,
  dewpoint or MSLP fields are requested, but `t_isobaric` and `meanT_500_300` read it too (`:920-959`). Patched copy:
  `reference_fixes/mpas_isobaric_diagnostics.F`.
- **Why:** a stream that asks for only those two fields reads an unset array in MPAS. MPAS's default streams request
  temperature and MSLP, so their values do not change.
- **Default:** every build.

### Additional-diagnostics alarm when a stream has no time interval

- **What:** an `output` or `diagnostics` stream whose interval is not a time interval (missing, `none`,
  `initial_only`, ...) is treated as having none; the alarm then follows the other stream.
- **MPAS:** `physics/mpas_atmphys_manager.F:612-645` builds the alarm from the record intervals of the streams `output`
  and `diagnostics`; an interval that is not a time leaves its string unset.
- **Why:** defined behaviour for an input MPAS leaves undefined; nothing changes for a time interval.
- **Default:** every build.

### restart_timestamp only after a restart file was written

- **What:** `restart_timestamp` is written after the restart record is closed (and a new file renamed to its final
  name), never when the write failed.
- **MPAS:** `mpas_atm_core.F:893-895`: the comment asks for a successful write, the test looks at the alarm alone, also
  after the clobber errors allowed at lines 865-871.
- **Why:** a timestamp that names a restart file that was not written makes the next run start from the wrong file.
- **Default:** every build.

### Card build: the GF cloud-base search

- **What:** in the card copy of Grell-Freitas (`src/device/gf_core.f90`, written by `tools/gen_device_physics.py`), the
  `GO TO` search loop of `cup_kbcon` is written with `do`, `exit` and `cycle`, with the same logic.
- **MPAS:** `cup_kbcon` in `physics/physics_wrf/module_cu_gf.mpas.F`.
- **Why:** nvfortran 25.3 at `-O2` compiled the `GO TO` form for the card without its two `IF(KBCON.GT.KBMAX+2)` exits:
  the cloud base passed the top level and arrays were read beyond it. Host builds never did.
- **Default:** card builds.

## 2. Physics changes of the `lean` flavor

Each is a build switch, off by default and on in flavor `lean`. They change the forecast; flavor `mpas` keeps MPAS's
physics. The measured effects below come from regional runs at 3 km over Greece (48 h, verified at about 600 land
stations) and from a global 120 km mesh (x1.40962, 48 h, compared with GFS and CERES); they are cases, not a
climatology.

### LEAN_MYNN_HYD_PS: the first layer's mass from the hydrostatic surface pressure

- **What:** MYNN takes the pressure thickness of the first layer, `delp(kts)`, from the hydrostatic surface pressure
  `p(kts) + g*rho*dz/2` instead of the diagnosed `psfc`.
- **MPAS:** `physics/physics_mmm/bl_mynn_subroutines.F90:3102`.
- **Why:** in the first steps an acoustic dip of the first-layer pressure (430 Pa on a 500 m mesh) can make `delp(kts)`
  zero, and `moisture_check` divides by it.
- **File:** `src/mynn_pbl_helpers.f90`.

### LEAN_MYNN_DEW: dew and frost reach the air

- **What:** a negative surface moisture flux (dew, frost) is kept, bounded so that one step takes at most
  `0.9*qv - 1e-8` out of the first layer.
- **MPAS:** `physics/physics_mmm/bl_mynn_subroutines.F90:3473`: the bound `min(0.9*sqv(kts) - 1e-8, 0.)/dtz(kts)` is
  zero for any `qv` above 1.1e-8, so every negative flux is dropped, against the comment that says the bound keeps
  vapour above 1e-8.
- **Why:** Noah-MP books the dew as surface water while MPAS's air keeps it. The bound still keeps `qv` positive in very
  dry air. Station T2 and Td2 did not change.
- **File:** `src/mynn_pbl_helpers.f90`.

### LEAN_MYNN_RHOSFC: MYNN's surface terms with the first layer's density

- **What:** the surface boundary terms of the MYNN solver use the first layer's density `rho(kts)`.
- **MPAS:** `physics/physics_mmm/bl_mynn_subroutines.F90:3096`, `rhosfc = psfc/(R_d*(tk(kts)+p608*qv(kts)))`: the sum
  adds a temperature in kelvin and a dimensionless number. The caller made the kinematic fluxes with `rho(kts)`
  (`physics/physics_mmm/bl_mynn.F90:807`), so a different density changes the flux the layer receives.
- **Why:** with `rho(kts)` the boundary terms put exactly `qfx`, `hfx/cpm` and `rho*ust**2` into the first layer.
- **File:** `src/mynn_pbl_helpers.f90`.

### LEAN_MYNN_MOIST: the PBL water conversions keep the water

- **What:** MYNN's changes of the specific contents `s = q/(1+qv)` go back to mixing ratios as `dq = (1+qv)*ds`, for
  vapour, cloud water, ice and snow; snow is converted per moist mass like cloud and ice.
- **MPAS:** `physics/physics_wrf/bl_mynn_post.F:123, 134, 145` convert back with `s*(1+s_vapor)`, and
  `physics/physics_wrf/bl_mynn_pre.F:135` converts snow with its own denominator; the two directions do not invert each
  other.
- **Why:** with this and `LEAN_MYNN_RHOSFC` the PBL step keeps its water to rounding. MPAS adds 0.7 to 1.5 % of the
  surface flux and removes 2 to 36 g/m2 of condensate a day. Station T2 and Td2 moved by less than 0.05 K.
- **File:** `src/mynn_conversions.f90`.

### LEAN_SFC_Q2MR: 2 m humidity over water as a mixing ratio

- **What:** over water, `qsfc` is turned into a mixing ratio before the 2 m humidity is diagnosed, as MYNN does over
  land.
- **MPAS:** `physics/physics_mmm/sf_mynn.F90:319` makes `qsfc` a specific humidity over water, the flux at `:843` is a
  mixing-ratio flux and land uses a mixing ratio (`:322`); the water `q2` mixes the two and is low by about `qsfc**2`.
- **Why:** one convention for `q2` everywhere. Output only.
- **File:** `src/noahmp_coupling.f90`.

### LEAN_MYNN_FILL_THL: MYNN's moisture check heats once

- **What:** `moisture_check` works on a copy of `thl`, so filling a negative `qc` no longer adds `L*dqc` to `thl`.
- **MPAS:** `physics/physics_mmm/bl_mynn_subroutines.F90`, `mynn_tendencies` and `moisture_check`: the check was
  written for potential temperature but receives `thl`, which does not change when vapour condenses; the `Dth` that
  follows adds `L*sqc2` again.
- **Why:** the fill heated twice. Water closed, energy gained 6.9 W/m2 on the global mesh. With the switch the MYNN
  energy residual fell from +7.39 to +0.71 W/m2 ([the ledger](water-energy-ledger.md)). Effect: more low cloud; on the
  global mesh the absorbed shortwave at the top fell from 241.5 to 231.9 W/m2; at 3 km the stations moved by 0.02 K.
- **File:** `src/mynn_pbl_helpers.f90`.

### LEAN_MYNN_SALT: sea-surface humidity over salt water

- **What:** over open water (`xland > 1.5`, skin at or above 271 K) the saturation vapour pressure of the skin is
  multiplied by 0.98.
- **MPAS:** `physics/physics_mmm/sf_mynn.F90:318-320` has no factor; its sibling `sf_sfclayrev.F90` has it.
- **Why:** sea water of 35 psu holds about 0.98 of the vapour pressure of fresh water, and the COARE coefficients this
  scheme uses were fitted with the factor. Sea ice (below 271 K) and land keep their own `qsfc`. Effect on the global
  mesh: ocean latent heat flux 107.0 to 101.1 W/m2 (GFS 106.6); station T2 and Td2 within 0.06 K.
- **File:** `src/mynn_surface.f90`.

### LEAN_MONO_RELAX: monotonic transport reaches the relaxation rows

- **What:** the result of the monotonic scalar update (third transport stage) is written back to the relaxation rows
  3 to 5 of a regional mesh too.
- **MPAS:** `dynamics/mpas_atm_time_integration.F:5771`, `atm_advance_scalars_mono_work`:
  `if (bdyMaskCell(iCell) <= nSpecZone)`, although the comment says only the specified zone is set after transport and
  the non-monotonic stages update every cell up to `nRelaxZone`.
- **Why:** with `nSpecZone` (2) in place of `nRelaxZone` (5) the rows 3 to 5 keep the value of the second stage (dt/2)
  and miss the full-step transport. Only those rows change, and the effect on the interior after 3 h is small.
- **File:** `src/dyn_scalars.f90`.

### LEAN_GF_NOSHALLOW: no Grell-Freitas shallow plume

- **What:** Grell-Freitas runs its deep plume only; MYNN's EDMF carries the shallow cumulus.
- **MPAS:** `physics/mpas_atmphys_vars.F:340` hard-codes `ishallow = 1` while `config_mynn_edmf = 1` also carries
  shallow cumulus. WRF refuses this pair (`share/module_check_a_mundo.F`: EDMF requires `ishallow = 0`). The switch also
  removes an undefined write-back: `cup_gf_sh` declares `pre` `intent(out)` and never sets it
  (`physics/physics_wrf/module_cu_gf.mpas.F:4197`), with the deep scheme's rain as the actual argument.
- **Why:** two schemes for the same clouds, and the shallow plume creates water: on the global mesh GF's tendencies plus
  its rain were +0.26 mm/day with the plume (0 means conserved) and -0.07 without it, about a tenth of the global rain.
- **File:** `src/gf.f90`.

### LEAN_GF_DXMIN=4000: no Grell-Freitas on convection-permitting cells

- **What:** Grell-Freitas does not run in cells finer than the given spacing (4000 m in flavor `lean`); the coarser
  cells of a variable-resolution mesh keep it.
- **MPAS:** GF runs on every cell. Its scale awareness stops at about 6 km: `frh = pi*radius**2/dx**2` is capped at 0.7
  (`physics/physics_wrf/module_cu_gf.mpas.F:752-761`), so the closure keeps 0.09 of its mass flux at 3 km, 1 km or
  500 m.
- **Why:** at a few kilometres the dynamics resolves deep convection; a parameterised plume with a fixed share on top of
  it is double counting. 4000 m is above the cell size of a 3 km mesh. The cells skipped get the outputs GF leaves when
  no plume runs (no rain, zero tendencies), as with `cu_physics = 0` on a fine WRF nest.
- **File:** `src/gf.f90`.

### LEAN_RAD_BLCLOUD: MYNN's subgrid cloud in the radiation

- **What:** RRTMG sees MYNN's subgrid cloud water and ice (`qc_bl`, `qi_bl` of the last PBL step) where the resolved
  cloud is absent, with WRF's `icloud_bl = 1` thresholds. The cloud fraction stays Xu-Randall; the model state does not
  change.
- **MPAS:** `physics/mpas_atmphys_vars.F:418`, `icloud_bl = 0`: "no coupling of subgrid-scale clouds with radiation".
- **Why:** MYNN's stratocumulus, fog and shallow cumulus are otherwise invisible to the radiation. In a column test a
  night stratus gave +18 W/m2 downward longwave and a noon cumulus -82 W/m2 surface shortwave.
- **Note:** on the 120 km global mesh, together with `LEAN_MYNN_FILL_THL`, it made the planet too bright (absorbed
  shortwave at the top 231.7 W/m2 with it, 240.8 without; CERES about 240). On coarse global meshes consider building
  with `--switch LEAN_RAD_BLCLOUD=off`.
- **File:** `src/radiation_blcloud.f90`, called from `src/lean_model.f90` and `src/phys_dev.f90`.

### LEAN_NOAHMP_SWSPLIT: Noah-MP takes RRTMG's direct fraction

- **What:** Noah-MP splits the downward shortwave into direct and diffuse with RRTMG's surface fractions instead of a
  fixed 70 % direct. The visible share stays 50 %.
- **MPAS:** `physics/physics_noahmp/src/AtmosForcingMod.F90:79-85`: `RadDirFrac = 0.7`, `RadVisFrac = 0.5`.
- **Why:** the direct fraction is known from the radiation. The visible share stays fixed because RRTMG's UV/visible
  band ends at 0.625 um and Noah-MP's at about 0.7 um: RRTMG's visible fraction (about 0.415 under clear sky) would raise
  the land albedo for a reason of band limits. At night and with zero fractions the original lines apply.
- **File:** `src/noahmp_core.f90`.

### LEAN_NOAHMP_T2Z: Noah-MP's 2 m stability from the flux's capped stability

- **What:** z/L at 2 m comes from z/L at the first level after its cap of 1, not from the uncapped Obukhov length with a
  cap of its own. In unstable air the two are the same number. Only T2 and Q2 change.
- **MPAS:** `physics/physics_noahmp/src/ResistanceAboveCanopyMostMod.F90:97-98`,
  `physics/physics_noahmp/src/ResistanceBareGroundMostMod.F90:98-99`.
- **Why:** past the cap the flux acts as if L = z, while 2 m still saw the true short L; as a calm night grew more
  stable, T2 turned back from the surface temperature towards the first level. Measured at 3 km, 48 h, 597 land
  stations: the T2 bias fell from +1.23 to +0.96 K (night hours +1.64 to +1.08 K), the afternoon unchanged.
- **File:** `src/noahmp_core.f90`.

### LEAN_NOAHMP_RSF3: Noah-MP's ground resistance by adjusted Sellers

- **What:** the soil part of the ground evaporation resistance of `iopt_rsf = 4` uses Noah-MP's option 3 (adjusted
  Sellers 1992, `exp(8.25 - 6*s)`, s the top-soil liquid water over saturation) instead of Sakaguchi and Zeng; the snow
  weighting of option 4 stays.
- **MPAS:** `physics/physics_noahmp/src/ResistanceGroundEvaporationMod.F90:63-68` (options 1 and 4) and `:76`
  (option 3).
- **Why:** at the soil moisture of a cold start Sakaguchi and Zeng give thousands of s/m on a 10 cm layer; Greek land at
  noon had a Bowen ratio of 3.5 and the afternoon Td2 bias was -2.1 K. With the switch: afternoon T2 bias +0.87 to
  +0.45 K, Td2 -2.09 to -1.42 K. Over deserts the Sellers resistance (at most about 3800 s/m) lets dry sand evaporate
  more than Sakaguchi and Zeng (about 20,000 s/m), so global deserts evaporate too much.
- **File:** `src/noahmp_core.f90`.

### LEAN_RAD_2025: present-day gases and solar constant

- **What:** RRTMG's CO2, CH4 and N2O become NOAA GML's global means of 2025 (425.62 ppm, 1936.03 ppb, 338.86 ppb), and
  the solar constant becomes 1361 W/m2, the measured total solar irradiance (SORCE, TSIS-1). The minor halocarbons stay
  at their 2005 values (their change since is a few hundredths of a W/m2).
- **MPAS:** `physics/physics_wrf/module_ra_rrtmg_lw.F:11674-11680` and `physics/physics_wrf/module_ra_rrtmg_sw.F:10145-10151`
  (379 ppm, 1774 ppb, 319 ppb, the values of 2005); `physics/mpas_atmphys_constants.F:88` (`solcon_0 = 1370.`).
- **Why:** current values. On the global mesh the absorbed shortwave fell by about 1.5 W/m2 and the OLR by 0.5 W/m2,
  the clear-sky OLR moved towards GFS; at 3 km the stations did not move.
- **File:** `src/radiation_lw.f90`, `src/radiation_sw.f90`, `src/radiation_solar.f90`.

### LEAN_PRECIPW_MOIST: precipitable water without a second division

- **What:** the column water vapour `precipw` sums `qv * rho_d * dz`.
- **MPAS:** `physics/mpas_atmphys_driver_microphysics.F:615-616` divides the density by `1 + qv`, but `rho_p` there is
  already the dry density of the dynamics.
- **Why:** with the division the column vapour is low by the factor `1 + qv`. Only the output field `precipw` changes.
- **File:** `src/dyn_micro.f90`.

## 3. Other build switches

Off in both flavors.

| Switch | What it does |
|---|---|
| `LEAN_NOAHMP_THETA` | Physics change: Noah-MP is forced with the first level's potential temperature brought to the surface pressure, `theta*(psfc/p0)**(R/cp)`, instead of the first level's temperature. MPAS gives the temperature, about 0.27 K colder over 27 m, and Noah-MP's own potential temperature then uses the same pressure twice (`physics/physics_noahmp/src/AtmosForcingMod.F90:72-73`). Not in flavor `lean`. `src/noahmp_coupling.f90`. |
| `LEAN_BUDGET` | Diagnostic: a card build prints the global water and energy terms of every scheme each step ([the ledger](water-energy-ledger.md)). No physics change. |
| `LEAN_REFL_CARD` | Card builds: the reflectivity of the products output is computed on the card; it differs from the host's in the last bits (another math library). |
| `LEAN_MICRO_ONEKERNEL` | Card builds: keeps Thompson's one-kernel column form beside the staged form; `LEAN_MICRO_STAGED=1` at run time picks the stages. Same physics. |

## 4. Start-up, input and output

### The run

- **One process.** lean has no MPI. It refuses to start under an MPI launcher with more than one task (the first of
  `OMPI_COMM_WORLD_SIZE`, `PMI_SIZE`, `MV2_COMM_WORLD_SIZE`, `SLURM_STEP_NUM_TASKS` above 1, or of
  `OMPI_COMM_WORLD_RANK`, `PMI_RANK`, `MV2_COMM_WORLD_RANK`, `SLURM_PROCID`, `PMIX_RANK` above 0; `SLURM_NTASKS` is not
  read). It holds an exclusive `flock` on the run directory, so a second run in the same directory refuses. Both stop
  before the log is opened, with exit status 2.
- **Exit status.** MPAS returns 0 after its fatal stops; lean returns 1 after a `CRITICAL ERROR` and 2 when it refuses
  to start, and prints `lean_mpas stopped: <message> (see log.atmosphere.0000.err)` on standard error.
- **Log.** lean removes a stale `log.atmosphere.0000.err` when it opens a new log, writes no `log.abort`, adds
  `(lean_mpas drop-in core)` to the version line, takes its version from `git describe --dirty --always` (MPAS has no
  `--always`), and records its own build (target, profile, netCDF-C, flavor, switches). Timers stop by name. Lean's own
  model timers and, on a card build, the stage list are outside MPAS's log format.

### Inputs

lean reads the run directory's init, invariant, boundary and surface files itself (`src/io/`). Every boundary read of
the run is resolved before the first step (which file, which record, by `xtime` with MPAS's seek rules). MPAS resolves
each read when it is due (`mpas_atm_core.F:736-771`), so a missing boundary file stops MPAS hours into the run and
lean at the start.

lean stops at the start where MPAS goes on, crashes later or has no check:

| lean stops when | MPAS |
|---|---|
| a later boundary read gets a record that is not after the previous one | divides by the zero gap (`dynamics/mpas_atm_boundaries.F:266-272`) |
| `lbc_in` has a `record_interval` but its input alarm does not ring at the start (`final_only`) | runs without boundary data |
| the mesh is not on a sphere, or not on the earth's radius | runs (`framework/mpas_bootstrapping.F:283-340`) |
| a cell has more than 8 edges, or a neighbour sits in a slot beyond the cell's degree | runs meshes of up to `maxEdges = 10` |
| `indexToCellID`, `indexToEdgeID` or `indexToVertexID` is not 1..n | reorders by the IDs |
| a non-finite value in an initial field or a boundary record | starts, and stops later in its own checks or not at all |
| `config_microphysics_top` is below the model top | limits the microphysics levels; lean runs it on every level |
| a Thompson table whose first record is not MPAS 8.4.2's, or tables of mixed byte order | a Fortran runtime error, or wrong numbers |
| a boundary file without `xtime` | forces its first record, then fails (`framework/mpas_io_streams.F:370-374`) |
| a field lean needs is missing from the init or a boundary file (`lbc_u`, `lbc_rho`, `lbc_theta`, `lbc_qv` among them) | warns `Variable <s> not in input file.` and runs with zeros |
| `qc`, `qr` or `w` is missing from the init file | warns and runs with zeros |
| `NoahmpTable.TBL` or another physics table is not in the run directory | for `NoahmpTable.TBL` warns and goes on (`physics/physics_noahmp/drivers/mpas/NoahmpReadTableMod.F90:652-659`) |
| an `xtime`, or the init file's `initial_time`, is not a time, or `initial_time` is after the start time | reads the string as it can |
| a value outside the single-precision range | MPAS's `real()` narrowing gives Inf (`framework/mpas_io.F:1964`); lean stops with `NC_ERANGE` |

Fields MPAS reads when the file has them are read the same way, with MPAS's warning and zeros when it has not: `qi`,
`qs`, `qg`, `ni`, `nr` in the init file; `lbc_w`, `lbc_qc`, `lbc_qr`, `lbc_qi`, `lbc_qs`, `lbc_qg`, `lbc_ni`, `lbc_nr`
in a boundary file (MPAS warns once per file it opens, lean at every read). Strings read from NetCDF are cut at the
first NUL, as MPAS's PIO path does.

Warnings MPAS does not give (`config_lean_strict_inputs = .true.` in a `&lean` record makes them stops):

- land cells with a background albedo below 1 %: MPAS 8's static step leaves small coastal and island land cells at
  about 0.08 % background albedo where the MODIS data has only missing pixels (`tools/mesh/fix_albedo.py` fills them);
- a `meshDensity` that does not match the cell sizes for `config_len_disp`: a mesh made with `build_spherical_mesh`
  can carry meshDensity 1 everywhere, so every cell is treated as the finest (`tools/mesh/set_mesh_density.py`).

Accepted beyond MPAS: the four Thompson tables in little-endian order (`./lean tables` writes them) as well as MPAS's
big-endian ones; the byte order is read from the first record marker.

After a failed limited-area check lean stops at once with MPAS's two closing lines; MPAS first prints its
dynamics-settings block (`mpas_atm_core.F:1608-1614`).

The departures of the streams file and the clock are listed in [streams.md](streams.md), "Departures".

### Scheme sets MPAS cannot run

lean refuses at the start, with a message that names the schemes and what to set, the sets of `'off'` schemes where
MPAS 8.4.2 reads arrays it never allocated (`check_scheme_set` in `src/run/lean_namelist.f90`). Run on MPAS 8.4.2 (the
scheme `probe_*` specs and `off_sw_3h` of `cases/greece_60km/mpas_runs/`):

| Set | MPAS 8.4.2 | lean |
|---|---|---|
| microphysics `'off'` with other schemes on | runs, but reads cloud species it never set (`mpas_atmphys_interface.F:316-335`) | refuses |
| surface layer `'off'` with MYNN | segmentation fault in the first step (`mpas_atmphys_control.F:280-281`) | refuses |
| convection without the PBL | segmentation fault in the first step (`mpas_atmphys_driver_convection.F:695-745`) | refuses |
| gravity-wave drag without the PBL | segmentation fault in the first step (`mpas_atmphys_driver_gwdo.F:525-546`) | refuses |
| shortwave `'off'` with longwave on | segmentation fault: the longwave driver writes `xlat_p` (`mpas_atmphys_driver_radiation_lw.F:519, 871`), which for RRTMG only the shortwave driver allocates (`mpas_atmphys_driver_radiation_sw.F:125`) | refuses |
| longwave `'off'`, or both `'off'` | runs | runs |

Every other set of `'off'` schemes runs as MPAS runs it.

### The surface update (`config_sst_update`)

- **One alarm.** MPAS reads the `surface` stream when the stream's input alarm rings (`mpas_atm_core.F:787-809`) and
  applies the record when its `sfcbdy` alarm rings (`physics/mpas_atmphys_manager.F:240-246`); both come from the same
  interval (`:557-565`), so with the default streams they ring on the same steps. lean has one alarm, anchored at the
  start of the run, and reads the latest record at or before the step's time and applies it on that step. A `surface`
  stream with its own `reference_time` is applied at the update times, not read at the stream's. The update itself is
  MPAS's `physics_update_sst` line for line (`physics/mpas_atmphys_update_surface.F:114-326`).
- **Restart.** The alarm's previous ring is in lean's restart file, so a restart keeps the phase (6 h + 6 h gives the
  bytes of 12 h); MPAS re-anchors both alarms at the restart time.
- **Stops.** `config_sst_update = .true.` with `input_interval = 'none'` on the `surface` stream: MPAS sets its
  alarm from that interval and its runtime aborts parsing it (nvfortran: `FIO-F-225 ... lexical error` in
  `mpas_timekeeping.F`, nothing written). An `input_interval` on the `surface` stream with
  `config_sst_update = .false.`: MPAS reads the stream and puts its raw `sst` and `xice` into the model without the
  update, so sea ice appears on cells that hold no sea-ice state. A surface file with sea ice on a land cell:
  `init_atmosphere` never writes one (`mpas_init_atm_cases.F:4833-4848`); MPAS would keep a land cell with an ice
  fraction. A restart whose surface file freezes a water cell when the run had no sea-ice cells at its start.

### Restart

- lean writes and reads its own restart files and refuses MPAS's. A lean restart file
  holds the whole model state, including what MPAS recomputes or loses at a restart, so that a split run gives the
  bytes of the continuous one. [restart.md](restart.md) lists every variable.
- **Physics alarms keep their phase.** MPAS starts the radiation, greenness and additional-diagnostics alarms at the
  restart time (`physics/mpas_atmphys_manager.F:457-495, 549-552, 631-640`) and takes the day of year from the restart
  time. lean carries the previous rings, and the day of year and elapsed radiation time of the original start. MPAS's
  own restart at 06:10 (between radiation calls) differs from its continuous run at 12:00 in 47 history and 95
  diagnostic variables; at 06:00 it agrees.
- **The maxima window.** MPAS resets and samples the maxima at a restart start (`mpas_atm_core.F:666-718`); lean
  restores them and goes on with the window of the run before.
- **First-step code.** MPAS's Noah-MP driver sets `smois = 1` on water and fully iced cells only at `itimestep == 1`
  (`physics/physics_noahmp/drivers/mpas/NoahmpDriverMainMod.F90:135-153`), and its restart runs that step again; lean
  does not.

### Output

- **Files.** The writer closes each file after each record. A new file is written as `<name>.lean_partial` and renamed
  when its first record is closed. MPAS keeps its stream files open (`framework/mpas_stream_manager.F:3150-3440`).
  A killed run keeps every completed record of CDF-5 and 64-bit-offset files; NetCDF-4 appends have no such guarantee.
- **Global attributes.** New files carry `lean_flavor`, `lean_switches` and `lean_version` before `file_id`, and
  `git_version` names lean's build. MPAS adds its own (`driver/mpas_subdriver.F:513-556`) and a random `file_id`
  (`framework/mpas_stream_manager.F:4306-4310`).
- **Units of `chvxy` and `chbxy`.** `m s^{-1}`; the Registry says `s m^{-1}`, while Noah-MP describes the exchange
  coefficient in m/s (`physics/physics_noahmp/src/EnergyVarType.F90:149`).
- **Fields lean does not have.** A stream that asks for a field lean does not compute stops the start before any output
  file is opened ([fields.md](fields.md)); MPAS writes every field it knows.
- **Summation order of the diagnostics.** lean adds the levels of `updraft_helicity_max` and `meanT_500_300` in source
  order. nvfortran vectorizes these level sums in MPAS (also with `-Kieee`) and adds them in another order, so MPAS's
  files built with nvfortran differ from lean in the last bit of a few cells (one helicity cell and one `meanT_500_300`
  cell on `x1_2562`; 283 and 20 cells at 55 levels; none on `greece_60km`). `tests/diag/` and `tests/writer/t0_values.py`
  hold the measured counts. The kite sum of `updraft_helicity_max` follows the vertex order MPAS builds for one block:
  the order in which a walk over the cells first meets the vertices (`framework/mpas_block_decomp.F:633-685`), not the
  file's vertex numbers.
- **Thompson's surface precipitation.** lean adds the step's amounts as `pr + (ps + pg + pice)`: these are the bits MPAS
  built with nvfortran in strict floating point gives for `module_mp_thompson.F:1194`, which is written left to right.
  The two orders differ in the last bit where rain falls with snow or graupel.

## 5. Found in MPAS, left unchanged

Physical inconsistencies in MPAS 8.4.2 that lean reproduces as they are. Paths are relative to
`src/core_atmosphere/physics/` unless they start with `dynamics/` or `framework/`; `mpas_atm_core.F` and
`Registry.xml` are in `src/core_atmosphere/` itself.

### Grell-Freitas: plume transport and environmental feedback do not close the water

`physics_wrf/module_cu_gf.mpas.F:1640-1748, 3749-3768, 3905-3914, 3958-3966, 3521-3578`. The below-cloud parcel humidity
is imposed and clipped instead of following the discrete transport relation used for the environmental feedback; the
lower boundary has a finite normalised updraft mass with unequal parcel and environment humidity; the cloud saturation
floors and the feedback omit matching terms. In one convecting column the water balance before the limiter was
-0.028 kg/m2 in one step, three quarters of it from the below-cloud profile. A fix needs a conservative discrete
derivation, not a redistribution of the residual.

### Grell-Freitas: two gravity constants

`physics_wrf/module_cu_gf.mpas.F:1737, 1740, 4818, 4821` (and 2358-2391, 2490-2491, 3877) use 9.81 for the cloud
feedback, while the driver passes 9.80616 for the pressure and mass relations. The mismatch is a mass-weighting error
of order 1e-6 kg/m2 a step in a column.

### RRTMG: another gravity constant

`physics_wrf/module_ra_rrtmg_lw.F:8187, 8244` and `physics_wrf/module_ra_rrtmg_sw.F:4856, 4912` set gravity 9.8066 for
`heatfac = grav*86400/(cp*100)`, while MPAS's hydrostatic pressure uses 9.80616 (`framework/mpas_constants.F:46`). The
heating normalisation and MPAS's column mass differ by 45 ppm. MPAS passes its `cp` to RRTMG at the start; passing its
gravity too would remove the mismatch.

### Noah-MP urban solar time in radians

`physics_noahmp/drivers/mpas/NoahmpDriverMainMod.F90:190-193`, `mpas_atmphys_lsm_noahmpinit.F:307`: the urban irrigation
solar time divides the longitude by 15, but MPAS passes the longitude in radians. Unreachable with
`sf_urban_physics = 0`; lean has no urban branch.

### Off-centring of the vertically implicit solve: height basis

`mpas_atm_core.F:1340-1350` (`atm_compute_damping_coefs`) compares `config_epssm_transition_bottom_z` and `_top_z`
(30 and 50 km) with heights built from the cumulative `1/rdzw`, the height of the computational coordinate above the
flat reference, while the Registry speaks of height above sea level. With a model top at or below 30 km `epssm` is its
minimum on every level under either reading.

### Perturbation Coriolis term with the reference wind

`dynamics/mpas_atm_time_integration.F`, `atm_compute_dyn_tend_work`, block `perturbation_coriolis` (its comment: "this
is correct only for constant f"). It subtracts the Coriolis term of `u_init`, `v_init`. For real-data initial files these
are zero, and lean skips the block when they are.

### The missing-neighbour cell

`atm_recover_large_step_variables_work` sets `rho_zz(k, nCells+1) = 1` at every call, and the boundary update sets
`zz(:, nCells+1) = 1` (`dynamics/mpas_atm_boundaries.F`, `mpas_atm_update_bdy_tend`), while a run without boundaries
keeps zero there. The dummy cell is read only at the outermost edges, which the specified zone overwrites. lean does
the same.

### Operator precedence in the relaxation-row test of the monotonic transport

`atm_advance_scalars_mono_work`, at `flux_tmp` and `flux_arr` of its two edge loops:
`config_apply_lbcs .and. (bdyMaskEdge(iEdge) == nRelaxZone) .or. (bdyMaskEdge(iEdge) == nRelaxZone-1)` means
`(lbcs and m == 5) or (m == 4)`. The two readings differ only for a mask of 4 without boundaries, which a mesh without
boundary zones never has. lean writes the intended form; the results are the same.

### Thompson after the dynamics

`mpas_atmphys_interface.F`, `microphysics_from_MPAS` and `microphysics_to_MPAS`:

- The vertical velocity Thompson gets for a layer is the one at its bottom interface (`w_p(i,k,j) = w(k,i)`), not the
  layer mean; droplet and ice nucleation see an updraft half a layer too low.
- `rt_diabatic_tend` is the change of the moist potential temperature `theta_m = theta*(1 + 1.61*qv)`, so it holds the
  dilution of vapour as well as the latent heating: not a pure heating rate.
- Exner and the pressure perturbation are recomputed from the new `rho_zz*theta_m` at fixed `rho_zz`, while `rho_p` is
  left alone: the state after Thompson is not exactly the one the acoustic recovery would give.

### Boundary files without ice species

`lbc_scalars` lists `qv qc qr qi qs qg ni nr` (`Registry.xml:2149-2180`), but boundary files made by `init_atmosphere`
from GFS hold only `lbc_qv`, `lbc_qc` and `lbc_qr` ("Variable lbc_qi not in input file" at every read). The missing
species and their tendencies are zero at the boundary: in the specified zone `qi`, `qs`, `qg`, `ni`, `nr` are set to
zero after every transport stage and step, and in the relaxation zone they are damped towards zero. Ice leaving through
the boundary is removed, and ice of the driving model never enters. lean reads these fields when a boundary file holds
them.
