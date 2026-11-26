! The model run of the drop-in executable: the start (cold or from a lean restart), the step loop (inputs, radiation, the
! other physics, dynamics, Thompson), the output streams, the restart and the optional outputs, each switched on by an
! environment variable that names its file (LEAN_FILM, LEAN_PRODUCTS, LEAN_SURFACE, LEAN_STORM; docs/running.md). One
! process; the physics hands its tendencies to the dynamics in memory.
module lean_model
 use config
 use micro_pool, only: micro_pool_allocate
 use run_settings, only: radt_lw, radt_sw, settings_to_device, apply_lbcs
 use model_timing
 use dyn_config
 use model_state
 use gf_state
 use noahmp_state
 use gwdo_state
 use pre_dynamics
 use physics_inputs
 use dyn_handoff, only: east_north
 use dyn_mesh_data
 use dyn_state_data
 use dyn_micro_data
 use dyn_step
 use dyn_micro
 use phys_mode
 use model_data
#ifdef LEAN_PHYS_DEV
 use phys_dev, phys_run_inputs=>run_inputs
 use lean_phys_fault, only: physics_fault_check
#endif
 use check_util, only: check_final
 use film_output, only: film_on, film_start, film_rain_step, film_write, film_fields
#if defined(LEAN_REFL_CARD) && defined(_OPENACC)
 use lean_thompson, only: calc_refl10cm_card, radar_card_start
#endif
 use product_output, only: product_on, product_start, product_rain_step, product_sample, product_write
 use surface_output
 use storm_output
 use dyn_device
 use dyn_boundary
 use dyn_acoustic, only: allocate_acoustic_work
 use dyn_lbc_data
 use initial_winds, only: reconstruct_winds, reconstruct_winds_trig
 use model_start
 use cold_dyn, only: start_mesh_vectors, cold_dyn_state
 use cold_dyn_data, only: cold_dyn_type
 use cold_column, only: cold_cell, cold_physics_column
 use iso_fortran_env, only: int64
 use lean_messages, only: message_list, real_text, add_message, has_fatal, int_text, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT
 use lean_inputs, only: lbc_reader, lbc_due, lbc_next, inputs_failed
 use lean_run_inputs, only: run_inputs
 use lean_namelist, only: run_config
 use lean_streams, only: stream_set, io_plan, plan_start, plan_step_outputs, stream_index, read_filename, record_interval
 use diag_manager
 use lean_fields
 use schemes, only: micro_on, conv_on, lsm_on, lw_on, sw_on, moist_physics
 use lean_restart, only: restart_write, restart_prepare, restart_read, restart_alarms
 use lean_restart_file, only: restart_timestamp_write
 use ESMF, only: ESMF_TimeIntervalSet
 use lean_clock, only: run_clock, elapsed_s, seconds_until, interval_from_dt, step_count, &
  advance_run_clock, add_alarm, alarm_ringing, reset_alarm, ESMF_Time, ESMF_TimeInterval, operator(+), operator(*), operator(<), time_string
 use lean_log, only: log_write, log_messages, log_fatal, log_step_begin, log_step_time, log_timer_start, log_timer_stop
 use lean_summary, only: summarize_timestep
 use mpas_kind_types, only: R8KIND
 use landuse_startup, only: surface_table, read_surface_table
 use noahmp_cold_start, only: cold_land_column
 use surface_update, only: vegetation_of_day
 use sea_ice, only: xice_threshold, sea_ice_cells => ice, ice_cell, start_sea_ice, allocate_ice, any_sea_ice
 use surface_bdy, only: ivgtyp_now, isltyp_now, allocate_surface_bdy, surface_bdy_start, read_surface_record, update_sst, surface_file_has_ice
 use land_ice_exchange, only: allocate_glacier_of
 use land_ice, only: glacier, glacier_of, n_glacier, glacier_category, start_glacier, finish_glacier_start
 implicit none
 private
 public :: run_model_from, check_lw_start
 ! The boundary reads of the run (lean_lbc): every one resolved before the first step, each record read when it is due.
 type(lbc_reader), save :: lbc_in
 type(message_list), save :: lbc_msgs   ! the lines of a boundary read (a module variable: no allocation in the routine of the step loop)
 ! The run's lines go to MPAS's log (lean_log), nothing to standard output. MPAS reads the second boundary record in
 ! its first step, lean at the start: its lines wait in lbc_held for the first "Begin timestep", with the interval they
 ! open (lbc_held_from to lbc_held_to).
 type(message_list), save :: lbc_held
 character(19), save :: lbc_held_from = '', lbc_held_to = ''
 type(ESMF_Time), save :: lbc_epoch, boundary_end
 integer(int64), save :: step_start_s, step_end_s
 real, allocatable, save :: product_refl(:), product_mslp(:)
contains
#if defined(LEAN_REFL_CARD) && defined(_OPENACC)
 ! LEAN_REFL_CARD (build switch, off by default): refl of film_fields computed on the card, from the card's film_in, with
 ! calc_refl10cm_card (the same statements; the card's exp/log/pow and complex functions differ from the host library in the
 ! last bit). Only refl comes to the host. It is here, not in film_output.f90, because film_output is host code.
 subroutine film_refl_card(film_in, refl)
 real, intent(in) :: film_in(8,nz,nc1)
 real, intent(out) :: refl(ncells)
 real :: q(max_levels,8), dbz1d(max_levels), mx
 integer :: c, k, i
 logical :: wet
 logical, save :: started = .false.
 if (.not. started) then
  call radar_card_start()
  started = .true.
 end if
 !$acc wait   ! the microphysics of the step (queue 1) has written film_in
 !$acc parallel loop gang vector default(present) private(q, dbz1d, k, i, wet, mx) copyout(refl)
 do c = 1, ncells
  ! a column where no level has rain (qr > 1e-12, Thompson's R1), snow or graupel (qs or qg > 1e-6, R2) gets -35 dBZ,
  ! as in film_fields
  wet = .false.
  do k = 1, nz
   if (film_in(3,k,c) > 1.e-12 .or. film_in(5,k,c) > 1.e-6 .or. film_in(6,k,c) > 1.e-6) wet = .true.
  end do
  if (.not. wet) then
   refl(c) = -35.
  else
   do k = 1, nz
    do i = 1, 8
     q(k,i) = film_in(i,k,c)
    end do
    dbz1d(k) = -35.
   end do
   call calc_refl10cm_card(q(:,1), q(:,2), q(:,3), q(:,4), q(:,5), q(:,6), q(:,7), q(:,8), dbz1d, 1, nz, c, 1)
   mx = max(-35., dbz1d(1))
   do k = 2, nz
    mx = max(mx, max(-35., dbz1d(k)))
   end do
   refl(c) = mx
  end if
 end do
 end subroutine
#endif
 ! The messages of an input read or check, in the log; any ERR or CRIT line stops the run.
 subroutine input_messages(msgs)
 type(message_list), intent(inout) :: msgs
 logical :: failed
 if (msgs%n == 0) return
 failed = inputs_failed(msgs)
 call log_messages(msgs)   ! a CRIT line stops here
 if (failed) call log_fatal('lean_mpas stops: input problem(s) listed above')
 end subroutine
 ! The RRTMG long-wave column adds max(nint(p_top/4 hPa),1) layers between the model top and 0 hPa, as MPAS's RRTMG LW
 ! does (module_ra_rrtmg_lw.F:11822); p_top is the base-state pressure extrapolated to zgrid(nz+1). Stops the start when
 ! that column exceeds max_lw_layers, the size this executable was built with. Run before any state goes to the card or
 ! any output stream opens.
 subroutine check_lw_start(path)
 use radiation_sizes, only: max_lw_layers
 character(*), intent(in) :: path
 type(message_list) :: msgs
 real :: z0, z1, z2, w1, w2, ptop
 integer :: c, needed
 if (nz < 2) then
  call add_message(msgs, LOG_ERR, path//': longwave radiation requires at least 2 mesh levels; found '//int_text(nz))
  call input_messages(msgs)
 end if
 needed = 0
 do c = 1, ncells
  z0 = m%zgrid(nz+1,c)
  z1 = 0.5*(m%zgrid(nz+1,c)+m%zgrid(nz,c))
  z2 = 0.5*(m%zgrid(nz,c)+m%zgrid(nz-1,c))
  w1 = (z0-z2)/(z1-z2)
  w2 = 1.-w1
  ptop = exp(w1*log(s%pressure_base(nz,c))+w2*log(s%pressure_base(nz-1,c)))
  needed = max(needed, nz + max(nint((ptop/100.)/4.), 1))
 end do
 if (needed > max_lw_layers) then
  call add_message(msgs, LOG_ERR, path//': the longwave radiation needs '//int_text(needed)// &
                   ' layers above '//int_text(nz)//' levels; this executable holds '//int_text(max_lw_layers))
  call input_messages(msgs)
 end if
 end subroutine

 ! MPAS's lines after a boundary update (dynamics/mpas_atm_boundaries.F:319-322).
 subroutine log_lbc_interval(from, to)
 character(*), intent(in) :: from, to
 call log_write(repeat('-', 70))
 call log_write('Updated lateral boundary conditions. LBCs are now valid')
 call log_write('from '//from//' to '//to)
 call log_write(repeat('-', 70))
 end subroutine
 ! The next boundary record of lbc_in into the boundary data: the state itself (first), or the new state and the
 ! tendency towards it. Times are seconds after 00 UTC of the start day; the clock supplies the remaining interval.
 ! hold: the lines of this read wait for the first step's block of the log (a read that failed stops at once).
 subroutine model_lbc_record(first, hold)
 logical, intent(in) :: first, hold
 integer(int64) :: valid_s
 integer :: sec
 type(ESMF_TimeInterval) :: gap
 call lbc_next(lbc_in, record, valid_s, lbc_msgs)
 if (hold .and. .not. inputs_failed(lbc_msgs)) then
  lbc_held = lbc_msgs
  lbc_msgs%n = 0
 else
  call input_messages(lbc_msgs)
 end if
 sec = int(lbc_start_sec + valid_s)
 call ESMF_TimeIntervalSet(gap,S=int(valid_s))
 boundary_end=lbc_epoch+gap
 if (first) then
  call lbc_first(bl, sec, record%u, record%rho, record%theta, record%qv, record%qc, record%qr, m%cellsOnEdge, m%zz, &
                 record%qi, record%qs, record%qg, record%ni, record%nr)
 else
  call lbc_update(bl, sec, record%u, record%rho, record%theta, record%qv, record%qc, record%qr, m%cellsOnEdge, m%zz, &
                  record%qi, record%qs, record%qg, record%ni, record%nr)
 end if
 end subroutine
 ! The record valid at or before the start and the next one.
 subroutine model_use_lbc()
 call bdy_lists(m%bdyMaskCell, m%nEdgesOnCell, m%edgesOnCell)   ! the cells and edges the boundary operators touch
 call lbc_allocate(bl)   ! (after the lists: the boundary data hold only their cells and edges)
 call model_lbc_record(.true., .false.)
 lbc_held_from = lbc_end_xtime()
 call model_lbc_record(.false., .true.)
 lbc_held_to = lbc_end_xtime()
 bl%remaining_s = real(bl%end_sec - lbc_start_sec)
 g%lbc => bl
 end subroutine
 ! YYYY-MM-DD_hh:mm:ss of the record that ends the present boundary interval (bl%end_sec).
 function lbc_end_xtime() result(text)
 character(19) :: text
 integer :: y, mo, d, sec
 call civil_date(day_number(st%year, st%month, st%day) + bl%end_sec/86400, y, mo, d)
 sec = mod(bl%end_sec, 86400)
 write(text,'(i4.4,"-",i2.2,"-",i2.2,"_",i2.2,":",i2.2,":",i2.2)') y, mo, d, sec/3600, mod(sec, 3600)/60, mod(sec, 60)
 end function
 ! The dynamics goes to the card (OpenACC build; a plain build has no directives and nothing happens): mesh, coefficients, state, boundary data.
 subroutine model_device_start()
#ifdef _OPENACC
 drain_queue => wait_dynamics_queue
#endif
 call settings_to_device()
#ifdef LEAN_PHYS_DEV
 ! Only the device noahmp_state declares this setting on the card.
 !$acc update device(noahmp_opt_sfc)
#endif
 call device_enter_grid(g)
 call device_enter_fields(fc)
 if(apply_lbcs)then
 call device_enter_lbc(bl)
 endif
 if (card_thompson) call micro_enter_device(g, cmp)
 end subroutine
 ! cold: the mesh m and the fields dyn are already read from the run directory, the coupled state comes from dyn
 ! (cold_dyn.f90) and the micro fields start at 0 (cold_physics_column sets nt_c, run_model_from the surface pressure);
 ! a lean restart: the restart read fills the state later.
 subroutine model_init(st, dyn)
 type(start_info), intent(in) :: st
 type(cold_dyn_type), intent(in) :: dyn
 call start_mesh_vectors(m)   ! every start, as MPAS (cold_dyn.f90)
 if(st%kind==start_lean)then
  allocate(s%rho_base(nz,nc1),s%rtheta_base(nz,nc1),s%exner_base(nz,nc1),s%pressure_base(nz,nc1))
  call setup_grid(m,s,g)
  call allocate_fields(fc)
  call allocate_micro(cmp)
 else
  call setup_grid(m, s, g)
  call cold_dyn_state(dyn, m, g%sign_cell, g%zb_cell, g%zb3_cell, s)
  call zero_micro(mb)
  call allocate_fields(fc)
  fc%u1 = s%u; fc%w1 = s%w; fc%theta_m1 = s%theta_m; fc%rho_zz1 = s%rho_zz; fc%scalars1 = s%scalars
  ! without a microphysics scheme MPAS has no cloud or number constituents (qtot sums what exists): only qv is carried
  if(.not.micro_on)fc%scalars1(i_qc:i_nr,:,:) = 0.
  fc%ru = s%ru; fc%rw = s%rw; fc%rho_p = s%rho_p; fc%rtheta_p = s%rtheta_p; fc%exner = s%exner; fc%pressure_p = s%pressure_p
  fc%rt_diabatic_tend = s%rt_diabatic_tend
  call allocate_micro(cmp)
  cmp%nt_c = mb%nt_c; cmp%surface_pressure = mb%surface_pressure; cmp%tend_sfc_pressure = mb%tend_sfc_pressure
  cmp%rainnc = mb%rainnc; cmp%rainncv = mb%rainncv; cmp%snownc = mb%snownc; cmp%snowncv = mb%snowncv
  cmp%graupelnc = mb%graupelnc; cmp%graupelncv = mb%graupelncv; cmp%sr = mb%sr
  cmp%rainprod = mb%rainprod; cmp%evapprod = mb%evapprod; cmp%re_cloud = mb%re_cloud; cmp%re_ice = mb%re_ice; cmp%re_snow = mb%re_snow
  cmp%dtheta_dt_mp = 0.
 endif
 allocate(east(3,nc1), north(3,nc1), mass_edge(nz,ne1), ux(nz,nc1), uy(nz,nc1), tend_th(nz,nc1), rbe(nz,ne1))
 call east_north(m%latCell, m%lonCell, east, north)
 end subroutine
 subroutine zero_micro(f)
 type(dyn_micro_type), intent(out) :: f
 allocate(f%nt_c(nc1), f%surface_pressure(nc1), f%rainnc(nc1), f%rainncv(nc1), f%snownc(nc1), f%snowncv(nc1), &
          f%graupelnc(nc1), f%graupelncv(nc1), f%sr(nc1), f%tend_sfc_pressure(nc1), f%rainprod(nz,nc1), f%evapprod(nz,nc1), &
          f%re_cloud(nz,nc1), f%re_ice(nz,nc1), f%re_snow(nz,nc1))
 f%nt_c = 0.; f%surface_pressure = 0.; f%rainnc = 0.; f%rainncv = 0.; f%snownc = 0.; f%snowncv = 0.
 f%graupelnc = 0.; f%graupelncv = 0.; f%sr = 0.; f%tend_sfc_pressure = 0.; f%rainprod = 0.; f%evapprod = 0.
 f%re_cloud = 0.; f%re_ice = 0.; f%re_snow = 0.
 end subroutine
 subroutine model_start_step(it)
 integer, intent(in) :: it
 integer :: e, k
 mass_edge(:,ne1) = 0.      ! the ghost edge; every other edge is set below
 ! One team of threads for the whole routine: the routines called here contain orphaned worksharing loops (!$omp do).
 !$omp parallel
 if (it > 1) then
  call detail_start(d_shift)
  call shift_time_levels(fc)
  call detail_stop(d_shift)
 else
#ifdef _OPENACC
  ! on the card, as after the transport (the cell winds of the first step); to the host only if the physics inputs run there
  call reconstruct_winds_trig(fc%u1, m%coeffs_reconstruct, m%edgesOnCell, m%nEdgesOnCell, g%cos_lat, g%sin_lat, g%cos_lon, g%sin_lon, &
                              fc%u_zonal, fc%u_meridional)
  if (host_inputs) then
   !$acc wait(1)
   !$acc update self(fc%u_zonal, fc%u_meridional)
  end if
#else
  call reconstruct_winds(fc%u1, m%coeffs_reconstruct, m%edgesOnCell, m%nEdgesOnCell, m%latCell, m%lonCell, fc%u_zonal, fc%u_meridional)
#endif
 end if
 call rk_setup(fc)
 if (it == 1) call stage_diagnostics(g, fc, .true.)
 !$acc wait(1)
 ! the edge density of the host physics chain (pre_dynamics_step); the staged chain of the card build computes its own in run_finish
 ! (phys_dev.f90), so a card run skips this host loop (it would read a stale host copy and cost time every step)
 if (.not. any_card) then
  !$omp do schedule(dynamic,64)
  do e = 1, nedges
   do k = 1, nz
    mass_edge(k,e) = 0.5 * (fc%rho_zz1(k,m%cellsOnEdge(1,e)) + fc%rho_zz1(k,m%cellsOnEdge(2,e)))
   end do
  end do
 end if
 !$omp end parallel
 end subroutine
 subroutine model_inputs(it, state, convection)
 integer, intent(in) :: it
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: convection
 type(column_view) :: a
 type(gf_column) :: f
 integer :: c
 if (card_thompson .and. it > 1) call micro_to_host(cmp)
 !$omp parallel do schedule(dynamic,64) private(a,f)
 do c = 1, ncells
  a = view_column(state, c)
  f = gf_view(convection, c)
  call physics_inputs_cell(a, f, fc%rho_zz1(:,c), fc%theta_m1(:,c), fc%scalars1(:,:,c), fc%exner(:,c), fc%pressure_p(:,c), &
                           fc%w1(:,c), fc%u_zonal(:,c), fc%u_meridional(:,c), m%zz(:,c), m%zgrid(:,c), s%pressure_base(:,c))
  if (it > 1) then
   a%rainnc = cmp%rainnc(c); a%rainncv = cmp%rainncv(c); a%snownc = cmp%snownc(c); a%snowncv = cmp%snowncv(c)
   a%graupelnc = cmp%graupelnc(c); a%graupelncv = cmp%graupelncv(c); a%sr = cmp%sr(c)
   a%rainprod = cmp%rainprod(:,c); a%evapprod = cmp%evapprod(:,c)
   a%re_cloud = cmp%re_cloud(:,c); a%re_ice = cmp%re_ice(:,c); a%re_snow = cmp%re_snow(:,c)
   f%dynamics_theta = fc%rthdynten(:,c); f%dynamics_qv = fc%rqvdynten(:,c)
  end if
 end do
 end subroutine
 subroutine model_physics(state, land_state, convection_state, drag_state, mesh, restarting)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: land_state
 type(gf_state_type), target, intent(inout) :: convection_state
 type(gwdo_state_type), target, intent(inout) :: drag_state
 type(mesh_type), intent(in) :: mesh
 logical, intent(in) :: restarting
 !$omp parallel
 call pre_dynamics_step(state, land_state, convection_state, drag_state, mesh, restarting, rad, rad_heating, fc%rho_zz1, fc%theta_m1, &
                        fc%scalars1, mass_edge, m%cellsOnEdge, m%edgeNormalVectors, east, north, ux, uy, tend_th, rbe, &
                        fc%tend_ru_physics, fc%tend_rtheta_physics, fc%tend_rho_physics, fc%tend_scalars)
 !$omp end parallel
 end subroutine
 ! The physics tendencies (computed on the host) to the card, for the dynamics of this step.
 subroutine model_tendencies_to_device()
 !$acc update device(fc%tend_ru_physics, fc%tend_rtheta_physics, fc%tend_scalars)
 end subroutine
 subroutine model_advance(it)
 integer, intent(in) :: it
 !$omp parallel
 call model_step(g, fc, cmp, it=it)
 !$omp end parallel
 end subroutine
 ! The model from inputs already opened: the mesh (moved into the model's own and left empty), the initial fields of
 ! a cold start (freed once the state is made), and run: the cold start's columns, the boundary reader, the start
 ! descriptor and the Thompson tables' byte order. Nothing here reads a namelist; input problems found here (restart, surface
 ! files, output setup) are logged and stop the run (input_messages).
 ! The run's lines go to MPAS's log in MPAS's order and nothing to standard output: the 'initialize' timer stops after
 ! the model's start; each step is the 'time integration' timer, "Begin timestep", the boundary update's lines,
 ! " split dynamics-transport integration 3", the step, MPAS's extrema (summarize_timestep, with cfg's three print
 ! options) and the step's timing (core_atmosphere/mpas_atm_core.F:754-821). The tables are the working directory's,
 ! as in MPAS.
 subroutine run_model_from(mesh_read,dyn,run,clock,cfg,set)
 use phys_state, only: zero_gwdo, zero_land, zero_state, zero_gf
 use model_startup_io, only: initialize, clear_io
 use noahmp_startup, only: initialize_noahmp_column
 use NoahmpIOVarType, only: NoahmpIO_type
 use physics_time
 use radiation_tables, only: initialize_radiation_tables
 use radiation_climatology, only: initialize_ozone_tables, monthly_ozone
 use radiation_solar, only: radiation_solar_constants
 use radiation_column, only: radiation_event
#ifdef LEAN_RAD_BLCLOUD
 use radiation_blcloud, only: bl_cloud_water
#endif
 use radiation_sizes, only: max_sw_layers, max_lw_layers
 use mynn, only: mynn_init
 use lean_thompson, only: thompson_init
 type(dyn_mesh_type), intent(inout) :: mesh_read
 type(cold_dyn_type), intent(inout) :: dyn
 type(run_inputs), intent(inout) :: run
 type(run_clock), intent(inout) :: clock
 integer :: nsteps
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(inout) :: set
 type(state_type), target, save :: state
 type(mesh_type), save :: mesh
 type(noahmp_state_type), target, save :: lands
 type(gf_state_type), target, save :: convection
 type(gwdo_state_type), target, save :: drag_state
 type(NoahmpIO_type) :: io
 type(column_view) :: a
 type(noahmp_column) :: land
 type(gf_column) :: f
 type(gwdo_column_state) :: drag
 type(io_plan) :: output_plan
 logical :: output_need(N_FETCH)
 real(kind=R8KIND) :: diagnostic_seconds, stream_seconds
 type(run_clock) :: output_clock
 type(message_list) :: clock_msgs
 logical :: do_lw, do_sw
 real :: xtime, gmt
 real, allocatable :: monthly(:,:,:)
 real :: pin(59), julian, declin, solcon, native_z(nz+1)
 type(cold_cell) :: cell
 type(surface_table) :: surface
 type(dyn_mesh_type) :: no_mesh
 type(cold_dyn_type) :: no_fields
 logical :: print_vel, print_detailed, print_sca
 character(19) :: lbc_from
 logical :: table_there
 real(kind=R8KIND) :: step_seconds
#ifdef LEAN_NOAHMP_SWSPLIT
 real :: heating(nz,2), sw(17), lw(11), sw_flux(4,max_sw_layers+1), lw_flux(4,max_lw_layers+1)
#else
 real :: heating(nz,2), sw(14), lw(11), sw_flux(4,max_sw_layers+1), lw_flux(4,max_lw_layers+1)
#endif
 real :: radii(nz,3), cloud(nz), ozone(59)
#ifdef LEAN_RAD_BLCLOUD
 real :: qc_rad(nz), qi_rad(nz)
#endif
 integer :: c, it, active
 integer :: step0,ms
 integer :: year, month, day
 real :: vegfra, shdmax
 real, allocatable :: greenfrac(:,:)
 real(kind=8) :: t0
 character(64) :: stamp
 character(512) :: film_path, product_path, surface_path, storm_path
 integer :: ig
 ! the surface update: MPAS's physics_update_sst on the records of the 'surface' stream
 logical :: sst_update, sfc_ring
 integer :: isurface
 type(ESMF_TimeInterval) :: sfc_interval
 character(len=512) :: sfc_path
 character(len=:), allocatable :: sfc_name
 character(len=19) :: sfc_valid
 real, allocatable :: sfc_sst(:), sfc_xice(:)
 integer(kind=8) :: sfc_tick0, sfc_tick1, sfc_rate
 type(message_list) :: sfc_msgs, sfc_scan
 type(ESMF_Time) :: sfc_when
 logical :: sfc_has_interval
 character(len=:), allocatable :: sfc_prev
 nsteps=step_count(clock)
 output_clock%start=clock%start
 print_vel=cfg%config_print_global_minmax_vel
 print_detailed=cfg%config_print_detailed_minmax_vel
 print_sca=cfg%config_print_global_minmax_sca
 st=run%st
 lbc_start_sec=st%lbc_origin_sec
 lbc_epoch=clock%start
 lbc_in=run%lbc
 call phys_mode_init()
 call mynn_init()
 if(micro_on)call thompson_init(big_endian=run%thompson_big_endian)   ! mpas_atmphys_init.F:268-273: only with a microphysics scheme
 call initialize(io)
 if(st%kind==start_lean)then
  call restart_prepare(run,io,clock_msgs)
  call input_messages(clock_msgs)
 endif
 io%yr=st%year;io%month=st%month;io%day=st%day
 ! the state types with allocatable members: allocated here, once; new memory is not zero
 call allocate_gwdo(drag_state); call zero_gwdo(drag_state)
 call allocate_land(lands); call zero_land(lands)
 call allocate_gf(convection); call zero_gf(convection)
 call allocate_state(state); call zero_state(state)
 call allocate_mesh(mesh)
 call allocate_radiation_cache()
 call allocate_acoustic_work()
 allocate(product_refl(ncells))
 allocate(product_mslp(ncells))
 call allocate_ice()
 call allocate_surface_bdy()
 call allocate_glacier_of()
 ! the mesh that was read becomes the model's; the copy that was read is freed
 m=mesh_read
 mesh_read=no_mesh
 call model_init(st,dyn)
 if(st%kind/=start_lean)call check_lw_start(run%cold%init_path)
 dyn=no_fields
 state%timestep=1
 mesh%z=m%zgrid(:,1:ncells);mesh%area=m%areaCell(1:ncells)
 mesh%lat=m%latCell(1:ncells);mesh%lon=m%lonCell(1:ncells)
 mesh%neighbors=m%cellsOnCell(:,1:ncells);mesh%degree=m%nEdgesOnCell(1:ncells)
 ! The physics mesh keeps file zeros; dynamics maps missing neighbors to its ghost cell.
 where(mesh%neighbors==nc1)mesh%neighbors=0
 if(apply_lbcs)call model_use_lbc()
 ! LEAN_FILM=path: the film output every 10 model minutes (film_output.f90); before the Thompson fields go to the card
 call get_environment_variable('LEAN_FILM',film_path)
 if(len_trim(film_path)>0)then
  call micro_film_start(cmp)
  call film_start(trim(film_path))
 endif
 ! LEAN_PRODUCTS=path: the hourly product fields (product_output.f90), from which tools/products_to_icon.py makes ICON-format files
 call get_environment_variable('LEAN_PRODUCTS',product_path)
 if(len_trim(product_path)>0)then
  if(.not.film_on)call micro_film_start(cmp)
  call product_start(trim(product_path),m%zgrid)
 endif
 ! LEAN_SURFACE=path: hourly surface and land fields (surface_output.f90), which tools/surface_to_nc.py converts to netCDF
 call get_environment_variable('LEAN_SURFACE',surface_path)
 if(len_trim(surface_path)>0)call surface_start(trim(surface_path))
 call get_environment_variable('LEAN_STORM',storm_path)
 call diag_setup(set,clock,clock_msgs)
 step0=0
 if(st%kind==start_lean)then
  allocate(monthly(12,59,ncells),emiss(ncells),ice(ncells),snow(ncells),albedo(ncells))
  call restart_read(run,state,lands,convection,drag_state,clock_msgs)
  call input_messages(clock_msgs)
  step0=state%timestep-1
  call check_lw_start(run%cold%init_path)
  call diag_restore_device()
  call own_restart_output_warning(film_path,'LEAN_FILM',clock,clock_msgs)
  call own_restart_output_warning(product_path,'LEAN_PRODUCTS',clock,clock_msgs)
  call own_restart_output_warning(storm_path,'LEAN_STORM',clock,clock_msgs)
  call own_restart_output_warning(surface_path,'LEAN_SURFACE',clock,clock_msgs)
 endif
 ! the surface update runs only with a moist physics (mpas_atmphys_manager.F:557-565 is in the physics initialisation, which no
 ! physics skips: mpas_atm_core.F:580-596)
 sfc_has_interval=.false.
 sst_update=cfg%config_sst_update.and.moist_physics
 if(sst_update)then
  isurface=stream_index(set,'surface')
  if(.not.read_filename(set,isurface,clock%start,sfc_name,clock_msgs))sst_update=.false.
  if(sst_update)sfc_path=sfc_name
  call input_messages(clock_msgs)
 endif
 if(sst_update)allocate(sfc_sst(nc1),sfc_xice(nc1))
 call model_device_start()
 call add_alarm(clock,"radtlw",clock%start,clock_msgs,interval_from_dt(radt_lw))
 call add_alarm(clock,"radtsw",clock%start,clock_msgs,interval_from_dt(radt_sw))
 call add_alarm(clock,"green",clock%start,clock_msgs,interval_from_dt(run%greeness_s))
 ! mpas_atmphys_manager.F:557-565: the surface-update alarm rings every record interval of the stream, from the start
 if(sst_update)then
  sfc_has_interval=record_interval(set,clock,isurface,sfc_interval)
  if(sfc_has_interval)call add_alarm(clock,"sfcbdy",clock%start,clock_msgs,sfc_interval)
 endif
 if(st%kind==start_lean)call restart_alarms(clock,clock_msgs)
 call input_messages(clock_msgs)
 call clock_fields(clock,julian,stamp)
 if(st%kind==start_lean)then
  call move_alloc(run%restart_greenfrac,greenfrac)
 else
  allocate(monthly(12,59,ncells),emiss(ncells),ice(ncells),snow(ncells),albedo(ncells),greenfrac(12,ncells))
 endif
 call initialize_radiation_tables('.')
 call initialize_ozone_tables('.',pin)
 if(st%kind==start_cold)then
  ! MPAS reads its physics tables only with physics (mpas_atm_core.F:580-596): without any, LANDUSE.TBL may be absent
  inquire(file='./LANDUSE.TBL',exist=table_there)
  if(moist_physics.or.table_there)call read_surface_table('./LANDUSE.TBL',surface)
 endif
 do c=1,ncells
  if(st%kind==start_lean)then
   if(surface_on)call surface_types(c,run%restart_ivgtyp(c),run%restart_isltyp(c))
   ! a cell that freezes at a surface update starts the sea-ice code with the input's snow albedo (sfc_input snoalb): the
   ! restart carries it for the cells that had sea ice, the static field for the others
   if(sst_update.and.sea_ice_cells%xice(c)<xice_threshold)sea_ice_cells%snoalb(c)=run%restart_snoalb(c)
   call monthly_ozone(mesh%lat(c),monthly(:,:,c))
   cycle
  endif
  a=view_column(state,c);land=noahmp_view(lands,c);f=gf_view(convection,c);drag=gwdo_view(drag_state,c)
  if(st%kind==start_cold)then
   ! the orography fields into the GWDO view (run%cold%orography rows: var2d, con, oa1..oa4, ol1..ol4, as lean_inputs
   ! read_columns reads them); the heights are those of the mesh
   drag%terrain_stddev=run%cold%orography(1,c);drag%convexity=run%cold%orography(2,c)
   drag%asymmetry=run%cold%orography(3:6,c);drag%length_fraction=run%cold%orography(7:10,c)
   native_z=m%zgrid(:,c)
   cell=run%cold%cell(c)
   call cold_physics_column(cell,a,f,native_z,m%meshDensity(c))
   cmp%nt_c(c)=a%nt;cmp%surface_pressure(c)=start_surface_pressure(c)
   ! the physics driver sets surface_pressure (mpas_atmphys_interface.F:436,959): with no physics MPAS keeps the input's
   if(.not.moist_physics)cmp%surface_pressure(c)=cell%surface_pressure
   greenfrac(:,c)=cell%greenfrac
   call clear_io(io)
   io%julian=julian
   call cold_land_column(io,cell,a,surface,st%julday,native_z)
   ! Noah-MP's start: the shortwave and convective-rain inputs are left for the first step to set; the surface albedo starts at
   ! the LANDUSE.TBL value that cold_land_column put in io%albedo.
   if(a%xland<1.5)then
    call initialize_noahmp_column(io,land)
    ! without a land-surface scheme MPAS never runs Noah-MP's start (mpas_atmphys_init.F:418-425): the liquid soil water
    ! stays the input file's, not the one NoahmpInitMainMod.F90:77-111 derives
    if(.not.lsm_on)then
     land%water_state_soilliqwater=cell%sh2o
     land%water_state_soilmoisture=cell%smois
     land%energy_state_temperaturesoilsnow(1:4)=cell%tslb
    endif
    land%surface_albedo=io%albedo(1)
   endif
   ! sea ice and land ice start from the values MPAS's Noah-MP start leaves (sea_ice.f90, land_ice.f90); MYNN reads
   ! the snow depth of the pool after that start
   if(cell%xice>=xice_threshold)then
    if(cell%xland>1.5)error stop 'sea ice on a water cell: the init file must make sea-ice cells land'
    if(lsm_on)a%snowh=io%snowh(1)
    call start_sea_ice(c,cell%xice,cell%sst,cell%snoalb,io%snow(1),io%tslb(1,:),a%znt,io%albedo(1),io%emiss(1))
   elseif(cell%xland<1.5.and.cell%ivgtyp==glacier_category)then
    if(lsm_on)a%snowh=io%snowh(1)
    call start_glacier(c,io)
   endif
   ! a cell that freezes at a surface update starts the sea-ice code with the input's snow albedo (sfc_input snoalb)
   if(sst_update)sea_ice_cells%snoalb(c)=cell%snoalb
  endif
  if(surface_on)call surface_types(c,io%ivgtyp(1),io%isltyp(1))
  ivgtyp_now(c)=io%ivgtyp(1);isltyp_now(c)=io%isltyp(1)   ! MPAS's types, changed by the surface update
  emiss(c)=io%emiss(1);ice(c)=io%xice(1);snow(c)=io%snow(1);albedo(c)=io%albedo(1)
  ! znt, sfc_emiss and sfc_albedo are set by the physics initialisation (mpas_atmphys_init.F), which no physics skips
  ! (mpas_atm_core.F:580-596): MPAS's restart holds the arrays as allocated, 0
  if(.not.moist_physics.and.st%kind/=start_lean)then
   emiss(c)=0.;albedo(c)=0.;a%znt=0.
  endif
  call monthly_ozone(mesh%lat(c),monthly(:,:,c))
 enddo
 if(st%kind/=start_lean)call finish_glacier_start()   ! the exchange records of the glacier columns
 if(st%kind/=start_lean)call surface_bdy_start(ice)   ! xicem = xice (mpas_atmphys_init.F:328-333)
 if(sst_update)then
  ! sea ice may appear: the sea-ice code, and on the card its data, are chosen once (phys_dev.f90 ice_on). A restart that
  ! turns the update on has the same need: the flag the restart gave stays and the files add to it. MPAS opens the file of
  ! each ring's time (a template with time tokens names one file per ring), so every file the run will open is looked at.
  sfc_when=clock%start
  sfc_prev=''
  do
   if(.not.read_filename(set,isurface,sfc_when,sfc_name,clock_msgs))exit
   if(sfc_name/=sfc_prev)then
    if(len(sfc_prev)==0)then
     if(surface_file_has_ice(sfc_name,state%xland,clock_msgs))any_sea_ice=.true.   ! the first file: its problems are reported
    else
     sfc_scan%n=0   ! a later file that is missing is reported when its ring comes
     if(surface_file_has_ice(sfc_name,state%xland,sfc_scan))any_sea_ice=.true.
    endif
    sfc_prev=sfc_name
   endif
   if(.not.sfc_has_interval)exit
   sfc_when=sfc_when+sfc_interval
   if(.not.(sfc_when<clock%stop))exit
  enddo
  call input_messages(clock_msgs)
 endif
 if(surface_on)call surface_static(state%xland,mesh%lat,mesh%lon,m%zgrid(1,1:ncells))
 call output_start(cfg,set,run,clock_msgs)
 call input_messages(clock_msgs)
 call output_needs(set,output_need)
 call diag_bind(state,convection)
 if(st%kind==start_cold)deallocate(run%cold%cell,run%cold%orography)
 if(st%kind==start_cold.and.card_thompson)then
  !$acc update device(cmp%nt_c,cmp%surface_pressure)   ! set by the cold loop after model_device_start copied cmp to the card
 endif
#ifdef LEAN_PHYS_DEV
 ! the radiation code of this build (host loop too) reads the copies of the RRTMG tables (src/device/radiation_device_tables.f90): fill them
 ! on the host and the card in every case, not only when the radiation is on the card
 call rrtmg_tables_to_device()
 if(any_card)call phys_device_start(state,lands,convection,drag_state)
 call micro_pool_allocate(128)   ! the pooled column routines of this build on the host use slot 1 (no-op if the card's pools exist)
 if(card_radiation)call radiation_device_start(m,monthly)
#endif
 ! LEAN_STORM=path: hourly native-mesh storm maxima (storm_output.f90), after all existing device entry.
 if(len_trim(storm_path)>0)call storm_start(trim(storm_path),m%zgrid,m%areaCell)
 elapsed=0.d0;elapsed_cpu=0.d0
 call log_timer_stop('initialize')
 call plan_start(set,clock,cfg,output_plan)
 if(st%kind==start_lean)then
  call diag_resume(g)
  call diag_written(set,output_plan)
 else
  call diag_start(g,fc,run%cold)
  call diag_written(set,output_plan)
  call diag_reset()
  call diag_update(1)
 endif
 if(output_plan%any_output)then
  call log_timer_start('diagnostic_fields')
  call output_fields_to_host(state,lands,convection,output_need)
  call diag_compute(merge(2,1,st%kind==start_lean))
  call log_timer_stop('diagnostic_fields',diagnostic_seconds)
  call log_timer_start('stream_output')
  call write_streams(set,output_plan,clock,state,lands,run,st%kind/=start_lean,clock_msgs)
  ! plan_start leaves diagnostics_rang false at either kind of start.
  call diag_reset(output_plan%diagnostics_rang)
  call write_restart_output(set,output_plan,clock,cfg,state,lands,convection,drag_state,run,clock_msgs)
  call output_messages(clock_msgs)
  call log_timer_stop('stream_output',stream_seconds)
  call log_write(' Timing for diagnostic computation: '//real_text(real(diagnostic_seconds))//' s')
  call log_write(' Timing for stream output: '//real_text(real(stream_seconds))//' s')
 endif
 gmt=real(st%init_hour)+real(st%init_minute)/60.+real(st%init_second)/3600.
 do it=1,nsteps
  step_start_s=elapsed_s(clock)
  output_clock%now=clock%now+clock%step
  step_end_s=elapsed_s(output_clock)
  ms=step0+it
  xtime=start_xtime(st,step_start_s)
  call clock_fields(clock,julian,stamp)
  call log_timer_start('time integration')
  call log_step_begin(trim(stamp))
  if(apply_lbcs.and.it==1)then
   ! the second boundary read of the start, which MPAS makes here
   call log_messages(lbc_held)
   call log_lbc_interval(lbc_held_from,lbc_held_to)
  endif
  ! MPAS reads the 'surface' stream when its input alarm rings (mpas_atm_core.F:787-809), the record at or before the time
  sfc_ring=.false.
  if(sst_update)sfc_ring=alarm_ringing(clock,"sfcbdy")
  if(sfc_ring)then
   call system_clock(sfc_tick0,sfc_rate)
   sfc_msgs%n=0
   ! the file of this ring's time (a template with time tokens names a file per ring)
   if(read_filename(set,isurface,clock%now,sfc_name,sfc_msgs))then
    sfc_path=sfc_name
    call read_surface_record(trim(sfc_path),trim(stamp),sfc_sst,sfc_xice,sfc_valid,sfc_msgs)
   endif
   call system_clock(sfc_tick1)
   if(inputs_failed(sfc_msgs))then
    ! mpas_atm_core.F:797-800
    call add_message(sfc_msgs,LOG_ERR,repeat('*',80))
    call add_message(sfc_msgs,LOG_ERR,'Error reading input stream surface')
    call add_message(sfc_msgs,LOG_CRIT,repeat('*',80))
   else
    call add_message(sfc_msgs,LOG_OUT,repeat('-',70))
    call add_message(sfc_msgs,LOG_OUT,"  Read 'surface' input stream valid at "//sfc_valid)
    call add_message(sfc_msgs,LOG_OUT,'   Timing for stream input: '//real_text(real(sfc_tick1-sfc_tick0)/real(sfc_rate))//' s')
    call add_message(sfc_msgs,LOG_OUT,repeat('-',70))
   endif
   call input_messages(sfc_msgs)
  endif
  ! MPAS's vegetation update (surface_update.f90): at the start and every 24 h after, before the physics of the step
  call timer_start(t0)
  if(moist_physics.and.alarm_ringing(clock,"green"))then   ! mpas_atmphys_manager.F: the physics time tracker is not called without physics
   call reset_alarm(clock,"green")
   call log_write('--- time to update background surface albedo, greeness fraction.')
   read(stamp,'(i4,1x,i2,1x,i2)')year,month,day
#ifdef LEAN_PHYS_DEV
   ! the loop reads the land mask and writes the two fractions on the host: on a card run the card's copies are the
   ! valid ones, so they come back first and every cell goes up unchanged
   if(any_card)then
    !$acc update self(state%xland,lands%energy_param_vegfracgreen,lands%energy_param_vegfracannmax)
   endif
#endif
   !$omp parallel do schedule(dynamic,64) private(a,land,vegfra,shdmax)
   do c=1,ncells
    a=view_column(state,c);land=noahmp_view(lands,c)
    if(a%xland<1.5)then
     call vegetation_of_day(year,month,day,greenfrac(:,c),vegfra,shdmax)
     call set_vegfra(c,vegfra)
     land%energy_param_vegfracgreen=vegfra/100.0
     land%energy_param_vegfracannmax=shdmax/100.0
     if(glacier_of(c)>0)then
      glacier(glacier_of(c))%vegfra(1)=vegfra;glacier(glacier_of(c))%gvfmax(1)=shdmax
     endif
    endif
   enddo
#ifdef LEAN_PHYS_DEV
   if(any_card)then
    !$acc update device(lands%energy_param_vegfracgreen,lands%energy_param_vegfracannmax)
   endif
#endif
  endif
  ! mpas_atmphys_manager.F:240-246: the surface update follows the greenness update, before the physics of the step
  if(sfc_ring)then
   call reset_alarm(clock,"sfcbdy")
#ifdef LEAN_PHYS_DEV
   if(any_card)call surface_update_transfer(state,lands,.true.)
#endif
   sfc_msgs%n=0
   call update_sst(sfc_sst,sfc_xice,state,lands,sfc_msgs,trim(sfc_path)//' at '//sfc_valid)
   call input_messages(sfc_msgs)
#ifdef LEAN_PHYS_DEV
   if(any_card)call surface_update_transfer(state,lands,.false.)
#endif
  endif
  ! MPAS's Noah-MP driver, in its first step, sets the soil moisture of the cells whose sea-ice fraction is exactly 1 to 1.0
  ! (NoahmpDriverMainMod.F90:135-153); the history keeps it (lean_fields.f90 smois_first). The fraction is the one after the
  ! surface update above. A lean restart brings the flags of the run before.
  if(it==1.and.st%kind/=start_lean.and.allocated(smois_first))then   ! allocated with the output setup (a run without streams writes no history)
   do c=1,ncells
    smois_first(c)=0
    if(state%xland(c)<1.5.and.ice(c)==1.)smois_first(c)=1
   enddo
  endif
  call model_start_step(ms)
  call timer_stop(start_timer,t0)
  ! the date of the glacier columns, every step as lsm_noahmp_fromMPAS (mpas_atmphys_driver_lsm_noahmp.F:121-126)
  if(n_glacier>0)then
   read(stamp,'(i4,1x,i2,1x,i2)')year,month,day
   do ig=1,n_glacier
    glacier(ig)%yr=year;glacier(ig)%month=month;glacier(ig)%day=day;glacier(ig)%julian=julian
   enddo
  endif
  call timer_start(t0)
 if(apply_lbcs)then
  if(lbc_due(lbc_in,it))then
   lbc_from=lbc_end_xtime()
   call model_lbc_record(.false.,.false.)
   call device_update_lbc(bl)
   call log_lbc_interval(lbc_from,lbc_end_xtime())
  endif
 endif
  if(associated(g%lbc))then
   bl%remaining_s=seconds_until(clock,boundary_end)
  endif
  call timer_stop(boundary_timer,t0)
#ifdef LEAN_PHYS_DEV
  if(any_card)then
   if(card_thompson.and.ms>1.and.(.not.card(st_inputs).or.check(st_inputs)))call micro_to_host(cmp)
   call run_stage(st_inputs,ms,julian,state,lands,convection,drag_state)
  else
#endif
  call model_inputs(ms,state,convection)
  !$omp parallel do schedule(dynamic,64) private(a,land)
  do c=1,ncells
   a=view_column(state,c);land=noahmp_view(lands,c)
   if(a%xland<1.5)land%config_domain_DayJulianInYear=julian   ! host copy; with the card the inputs stage sets its own (phys_dev.f90)
  enddo
#ifdef LEAN_PHYS_DEV
  endif
#endif
  call timer_start(t0)
  ! a radiation scheme that is 'off' never rings (physics/mpas_atmphys_manager.F:258-285); its alarm is kept in step anyway
  do_lw=lw_on.and.alarm_ringing(clock,"radtlw")
  do_sw=sw_on.and.alarm_ringing(clock,"radtsw")
  if(moist_physics)then
   ! physics/mpas_atmphys_manager.F:271,285, before the physics drivers: a line only for a scheme that is on.
   if(lw_on)call log_write('--- time to run the LW radiation scheme L_RADLW = '//merge('T','F',do_lw))
   if(sw_on)call log_write('--- time to run the SW radiation scheme L_RADSW = '//merge('T','F',do_sw))
  endif
  if(do_lw)call reset_alarm(clock,"radtlw")
  if(do_sw)call reset_alarm(clock,"radtsw")
  call diag_start_step(set,clock)
  if(moist_physics)then
   ! mpas_atmphys_manager.F:296-368: each line follows its scheme; with no physics at all the time tracker is not called
   ! (mpas_atm_core.F:1034-1037) and none of them is written
   if(conv_on)call log_write('--- time to run the convection scheme L_CONV    = T')
   if(do_lw.or.do_sw)call log_write('--- time to update the ozone climatology for RRTMG radiation codes')
   if(conv_on)call log_write('--- time to apply limit to accumulated rainc and rainnc L_ACRAIN   = F')
   if(lw_on.or.sw_on)call log_write('--- time to apply limit to accumulated radiation diags. L_ACRADT   = F')
   call log_write('--- time to calculate additional physics_diagnostics               = '//merge('T','F',l_diags))
  endif
  if(do_lw.or.do_sw)then
#ifdef LEAN_PHYS_DEV
  if(card_radiation.and..not.check_radiation)then
   call need_card(state,lands,convection,drag_state)
   call radiation_solar_constants(julian,declin,solcon)
   call run_radiation(ms,julian,declin,solcon,pin,monthly,state,m,xtime,gmt,do_lw,do_sw)
  else
   if(any_card)call need_host(state,lands,convection,drag_state)
#endif
   call radiation_solar_constants(julian,declin,solcon)
   ! Columns are independent; radiation_event is recursive (all work arrays on the stack). Day and night columns differ in cost.
#ifdef LEAN_RAD_BLCLOUD
   ! LEAN_RAD_BLCLOUD: MYNN's subgrid cloud water and ice reach RRTMG (radiation_blcloud.f90)
   !$omp parallel do schedule(dynamic,64) private(a,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,qc_rad,qi_rad)
   do c=1,ncells
    a=view_column(state,c)
    call bl_cloud_water(a%qc,a%qi,a%qc_bl,a%qi_bl,a%cldfra_bl,qc_rad,qi_rad)
    call radiation_event(a%theta,a%exner,a%p,a%rho_moist,a%dz,mesh%z(:,c),a%qv,qc_rad,qi_rad,a%qs, &
     a%re_cloud,a%re_ice,a%re_snow,a%tsk,emiss(c),a%xland,ice(c),snow(c),albedo(c),mesh%lat(c),mesh%lon(c), &
     st%julday,julian,xtime,gmt,declin,solcon,pin,monthly(:,:,c),heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,do_lw,do_sw)
#else
   !$omp parallel do schedule(dynamic,64) private(a,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone)
   do c=1,ncells
    a=view_column(state,c)
    call radiation_event(a%theta,a%exner,a%p,a%rho_moist,a%dz,mesh%z(:,c),a%qv,a%qc,a%qi,a%qs, &
     a%re_cloud,a%re_ice,a%re_snow,a%tsk,emiss(c),a%xland,ice(c),snow(c),albedo(c),mesh%lat(c),mesh%lon(c), &
     st%julday,julian,xtime,gmt,declin,solcon,pin,monthly(:,:,c),heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,do_lw,do_sw)
#endif
    if(do_lw)then
     rad_heating(:,1,c)=heating(:,1)
     rad(c)%lw_down=lw(1)
     rad(c)%olr=lw(2)
    endif
    if(do_sw)then
     rad_heating(:,2,c)=heating(:,2)
     rad(c)%sw_net=sw(2);rad(c)%cos_zenith=sw(1)
#ifdef LEAN_NOAHMP_SWSPLIT
    rad(c)%sw_frac_dir_vis = sw(15)
    rad(c)%sw_frac_dir_nir = sw(16)
    rad(c)%sw_frac_dif_vis = sw(17)
#endif
    endif
#ifdef LEAN_BUDGET
    ! the same net downward fluxes as the card's rad_fast_lw_out and rad_fast_sw_out (phys_dev.f90), so a budget run with radiation on the host is read alike
    if(do_sw)then
     rad(c)%flux(1)=sw(6)-sw(4);rad(c)%flux(2)=sw(10)-sw(8)
     rad(c)%flux(5)=sw_flux(2,nz+1)-sw_flux(1,nz+1);rad(c)%flux(8)=sw(7)-sw(5)
    endif
    if(do_lw)then
     rad(c)%flux(3)=lw(6)-lw(4);rad(c)%flux(4)=lw(10)-lw(8)
     rad(c)%flux(6)=lw_flux(2,nz+1)-lw_flux(1,nz+1);rad(c)%flux(7)=lw(7)-lw(5)
    endif
#endif
   enddo
#ifdef LEAN_PHYS_DEV
   if(any_card)then
    !$acc update device(rad, rad_heating)
   endif
   if(check_radiation)then
    call need_card(state,lands,convection,drag_state)
    call radiation_check(ms,julian,declin,solcon,pin,monthly,state,m,xtime,gmt,do_lw,do_sw)
   endif
  endif
#endif
  endif
  call timer_stop(radiation_timer,t0)
#ifdef LEAN_PHYS_DEV
  if(any_card)then
   call run_zero(loc==1)
   call run_stage(st_surface,ms,julian,state,lands,convection,drag_state)
   call run_stage(st_land,ms,julian,state,lands,convection,drag_state)
   call run_stage(st_pbl,ms,julian,state,lands,convection,drag_state)
   call run_stage(st_gwdo,ms,julian,state,lands,convection,drag_state)
   call run_stage(st_gf,ms,julian,state,lands,convection,drag_state)
   call diag_rain(convection%pratec,loc==1)
   call diag_snow(state,lands,loc==1)
   call run_stage(st_finish,ms,julian,state,lands,convection,drag_state)
   if(loc==1)then
    !$acc serial present(state)
    state%timestep=state%timestep+1
    !$acc end serial
   else
    state%timestep=state%timestep+1
   endif
   if(.not.card(st_finish).or.check(st_finish))call tendencies_to_device()
#ifdef LEAN_BUDGET
   call budget_step(ms,state,convection)
#endif
  else
#endif
  call timer_start(t0)
  call model_physics(state,lands,convection,drag_state,mesh,.false.)
  call diag_rain(convection%pratec,.false.)
  call diag_snow(state,lands,.false.)
#ifdef LEAN_PHYS_DEV
  ! the host path of a card binary runs the device copies of the physics, whose fatal checks only set the code
  call physics_fault_check(.false.,'physics',it)
#endif
  call timer_stop(physics_timer,t0)
  call timer_start(t0)
  call model_tendencies_to_device()
  call timer_stop(transfer_timer,t0)
#ifdef LEAN_PHYS_DEV
  endif
#endif
  ! The radiation driver consumes the land diagnostics left by the previous step.
#ifdef LEAN_PHYS_DEV
  if(.not.any_card)then
#endif
  !$omp parallel do schedule(dynamic,64) private(a,land) shared(emiss,snow,albedo)
  do c=1,ncells
   if(.not.lsm_on)cycle   ! no land-surface block: sfc_emiss, sfc_albedo and the snow stay at their start values
   a=view_column(state,c);land=noahmp_view(lands,c)
   ! sea ice: the blended surface of sea_ice.f90; land ice: the glacier column (sfc_emiss, snow, sfc_albedo pools)
   if(ice_cell(c))then
    emiss(c)=sea_ice_cells%emiss(c);snow(c)=sea_ice_cells%snow(c);albedo(c)=sea_ice_cells%albedo(c)
   elseif(glacier_of(c)>0)then
    emiss(c)=glacier(glacier_of(c))%emiss(1);snow(c)=glacier(glacier_of(c))%snow(1);albedo(c)=glacier(glacier_of(c))%albedo(1)
   elseif(a%xland<1.5)then
    emiss(c)=land%energy_state_EmissivitySfc
    snow(c)=land%water_state_SnowWaterEquiv
    if(land%energy_state_AlbedoSfc > -999.)albedo(c)=land%energy_state_AlbedoSfc
   endif
  enddo
#ifdef LEAN_PHYS_DEV
  endif
#endif
  cmp%film_capture=(film_on.or.product_on).and.ten_minute_step(it)   ! read on the host only (dyn_micro.f90), never sent to the card
  cmp%film_capture=cmp%film_capture.or.(l_diags.and.need_micro)
  cmp%sum_pw=sum_pw
  call log_write(' split dynamics-transport integration 3')   ! dynamics/mpas_atm_time_integration.F:2056
  call model_advance(ms)
  if(l_diags)call diag_after_micro(cmp)
  ! MPAS's extrema of the step (dynamics/mpas_atm_time_integration.F:8509-8931) on the new state; on the card the
  ! state arrays are present there, the coordinates are the host's
  call summarize_timestep(print_vel,print_detailed,print_sca,fc%w2,fc%u2,fc%v,fc%scalars2,m%latCell,m%lonCell, &
                          run%cold%lat_edge,run%cold%lon_edge,ncells,nedges,nz)
  call log_timer_stop('time integration',step_seconds)
  call log_step_time(step_seconds)
  call timer_start(t0)
  if(storm_on)call storm_step(it,nsteps,fc%w2,fc%vorticity,m%verticesOnCell,m%nEdgesOnCell,g%kite, &
                              m%kiteAreasOnVertex,m%areaCell,m%zgrid,step_end_s/3600>step_start_s/3600)
  if(film_on)call film_step(it,state,convection)
  if(product_on)call product_step(it,state,convection)
  if(surface_on)call surface_step(it,nsteps,state,lands)
  if(mod(it,state_check_every)==0.or.it==nsteps)call check_model_state()
  call timer_stop(output_timer,t0)
  call advance_run_clock(clock)
  call advance_cf_time()
  call plan_step_outputs(set,clock,output_plan)
  call diag_written(set,output_plan)
  call diag_update(2)
  if(output_plan%any_output)then
   call log_timer_start('diagnostic_fields')
   call output_fields_to_host(state,lands,convection,output_need)
   call diag_compute(2)
   call log_timer_stop('diagnostic_fields',diagnostic_seconds)
   call log_timer_start('stream_output')
   call write_streams(set,output_plan,clock,state,lands,run,.false.,clock_msgs)
   call diag_reset(output_plan%diagnostics_rang)
   call write_restart_output(set,output_plan,clock,cfg,state,lands,convection,drag_state,run,clock_msgs)
   call output_messages(clock_msgs)
   call log_timer_stop('stream_output',stream_seconds)
   call log_write(' Timing for diagnostic computation: '//real_text(real(diagnostic_seconds))//' s')
   call log_write(' Timing for stream output: '//real_text(real(stream_seconds))//' s')
  endif
 enddo
 if(storm_on)call storm_report(nsteps)
call check_final()
#ifdef LEAN_PHYS_DEV
 call phys_report()
#endif
 end subroutine
 ! True at the first step that reaches each 10-minute mark, as the MPAS output alarm rings (dt 30 s: every 20th step;
 ! dt 18 s: the state of 07:10:12 goes into the 07:10 file).
 logical function ten_minute_step(it)
 integer, intent(in) :: it
 ten_minute_step = step_end_s/600 > step_start_s/600
 end function
 ! the film output of one step (film_output.f90): the convective rain of the step, and at an output step the fields
 subroutine film_step(it, state, convection)
 integer, intent(in) :: it
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: convection
#ifdef LEAN_PHYS_DEV
 !$acc update self(convection%pratec) if(loc==1)
#endif
 call film_rain_step(convection%pratec)
 if(.not.ten_minute_step(it))return
 if (card_thompson) call micro_film_to_host(cmp)
 if (card_thompson) call micro_to_host(cmp)
#ifdef LEAN_PHYS_DEV
 !$acc update self(state%u10, state%v10) if(loc==1)
#endif
 !$acc update self(fc%theta_m2, fc%scalars2, fc%exner, fc%pressure_p)
 call film_write(it, cmp%film_in, cmp%rainnc, state%u10, state%v10, fc%theta_m2, fc%scalars2, fc%exner, fc%pressure_p, &
                 s%pressure_base, m%zgrid)
 end subroutine

 ! the product output of one step (product_output.f90): convective rain every step, the hour's maxima every 10 minutes, a record
 ! at the end of every forecast hour
 subroutine product_step(it, state, convection)
 integer, intent(in) :: it
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: convection
 logical :: hour_record, refl_on_card
#ifdef LEAN_PHYS_DEV
 if(.not.film_on)then
  !$acc update self(convection%pratec) if(loc==1)
 endif
#endif
 call product_rain_step(convection%pratec)
 if(.not.ten_minute_step(it))return
 ! every 10 minutes the reflectivity inputs and the 10 m wind (the hour's maxima); the state, the accumulations and mslp only
 ! at the record of the hour, the one time they are written
 hour_record = step_end_s/3600 > step_start_s/3600
 refl_on_card = .false.
#if defined(LEAN_REFL_CARD) && defined(_OPENACC)
 ! LEAN_REFL_CARD: the reflectivity on the card, only refl comes over
 if (card_thompson) then
  call film_refl_card(cmp%film_in, product_refl)
  refl_on_card = .true.
 end if
#endif
 if (card_thompson .and. .not. refl_on_card) call micro_film_to_host(cmp)
#ifdef LEAN_PHYS_DEV
 !$acc update self(state%u10, state%v10) if(loc==1)
#endif
 if (hour_record) then
  !$acc update self(fc%theta_m2, fc%scalars2, fc%exner, fc%pressure_p)
  if (card_thompson) then
   !$acc update self(cmp%rainnc, cmp%snownc, cmp%graupelnc)
  end if
 end if
 call film_fields(cmp%film_in, fc%theta_m2, fc%scalars2, fc%exner, fc%pressure_p, s%pressure_base, m%zgrid, product_refl, product_mslp, &
                  hour_record, .not. refl_on_card)
 call product_sample(product_refl, state%u10, state%v10)
 if(.not.hour_record)return
#ifdef LEAN_PHYS_DEV
 !$acc update self(state%t2, state%q2, state%psfc, state%snowh, state%xland, state%u, state%v, state%cldfra_bl) if(loc==1)
#endif
 !$acc update self(fc%w2)
 call product_write(it, state%t2, state%q2, state%psfc, state%u10, state%v10, product_mslp, cmp%rainnc, cmp%snownc, cmp%graupelnc, &
                    state%snowh, state%xland, fc%theta_m2, fc%scalars2, fc%pressure_p, s%pressure_base, state%u, state%v, &
                    state%cldfra_bl, fc%w2)
 end subroutine

 ! The surface output of one step (surface_output.f90): a record at the end of every forecast hour and at the last step; only
 ! those steps copy the fields from the card (the radiation fields by where the radiation runs, the others by loc).
 subroutine surface_step(it, nsteps, state, lands)
 integer, intent(in) :: it, nsteps
 type(state_type), intent(inout) :: state
 type(noahmp_state_type), intent(inout) :: lands
 if(.not.(step_end_s/3600 > step_start_s/3600).and.it/=nsteps)return
#ifdef LEAN_PHYS_DEV
 !$acc update self(state%tsk, state%snowh) if(loc==1)
 !$acc update self(state%hfx, state%qfx) if(loc==1)
 !$acc update self(state%lh, state%ust) if(loc==1)
 !$acc update self(state%znt, state%pblh) if(loc==1)
 !$acc update self(state%chs, state%chs2) if(loc==1)
 !$acc update self(state%cqs2, state%flhc) if(loc==1)
 !$acc update self(state%flqc, state%qsfc) if(loc==1)
 !$acc update self(state%br, state%zol) if(loc==1)
 !$acc update self(lands%energy_flux_heatgroundtot, lands%energy_state_temperaturesoilsnow) if(loc==1)
 !$acc update self(lands%water_state_soilmoisture, lands%water_state_soilliqwater) if(loc==1)
 !$acc update self(lands%forcing_temperaturesoilbottom, lands%energy_state_temperaturecanopy) if(loc==1)
 !$acc update self(lands%energy_state_temperaturegrd, lands%energy_state_temperaturecanopyair) if(loc==1)
 !$acc update self(lands%energy_state_pressurevaporcanair, lands%energy_state_vegfrac) if(loc==1)
 !$acc update self(lands%energy_state_leafareaindex, lands%water_state_canopyliqwater) if(loc==1)
 !$acc update self(lands%water_state_canopyice, lands%energy_state_temperatureair2mveg) if(loc==1)
 !$acc update self(lands%energy_state_temperatureair2mbare, lands%energy_state_spechumidity2mveg) if(loc==1)
 !$acc update self(lands%energy_state_spechumidity2mbare, lands%energy_flux_radswabssfc) if(loc==1)
 !$acc update self(lands%energy_flux_radlwnetsfc, lands%energy_flux_heatsensiblesfc) if(loc==1)
 !$acc update self(lands%energy_flux_heatlatentgrd, lands%energy_flux_heatlatentcanopy) if(loc==1)
 !$acc update self(lands%energy_flux_heatlatenttransp, lands%energy_state_exchcoeffshabvcan) if(loc==1)
 !$acc update self(lands%energy_state_exchcoeffshbare, lands%energy_state_exchcoeffsh2mveg) if(loc==1)
 !$acc update self(lands%energy_state_exchcoeffsh2mbare, state%t2) if(loc==1)
 !$acc update self(state%q2, state%th2) if(loc==1)
 !$acc update self(state%u10, state%v10) if(loc==1)
 !$acc update self(emiss, snow, albedo) if(loc==1)
 !$acc update self(rad, rad_heating) if(card_radiation.and..not.check_radiation)
#endif
 call surface_write(it,state,lands,rad,ice,snow,albedo,emiss)
 end subroutine

 subroutine own_restart_output_warning(path,name,clock,msgs)
 character(*), intent(in) :: path,name
 type(run_clock), intent(in) :: clock
 type(message_list), intent(inout) :: msgs
 if(len_trim(path)>0)call add_message(msgs,LOG_WARN,name//' starts its sums at the restart time '// &
  time_string(clock%start)//": lean's own outputs are not carried by a restart.")
 end subroutine

 subroutine write_restart_output(set,plan,clock,cfg,state,lands,convection,drag,run,msgs)
 type(stream_set), intent(in) :: set
 type(io_plan), intent(in) :: plan
 type(run_clock), intent(in) :: clock
 type(run_config), intent(in) :: cfg
 type(state_type), intent(inout) :: state
 type(noahmp_state_type), intent(inout) :: lands
 type(gf_state_type), intent(inout) :: convection
 type(gwdo_state_type), intent(inout) :: drag
 type(run_inputs), intent(in) :: run
 type(message_list), intent(inout) :: msgs
 integer :: j,i
 logical :: written
 if(.not.plan%restart_rang.or.has_fatal(msgs))return
 do j=1,size(plan%write)
  i=plan%write(j)
  if(set%s(i)%name/='restart')cycle
  call restart_to_host(state,lands,convection,drag)
  call restart_write(set,i,clock,state,lands,convection,drag,run,written,msgs)
  ! MPAS core_atmosphere/mpas_atm_core.F:893-902. A skipped record cannot publish a timestamp.
  if(written)call restart_timestamp_write(cfg,time_string(clock%now),msgs)
 enddo
 end subroutine

 subroutine restart_to_host(state,lands,convection,drag)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag
#ifdef LEAN_PHYS_DEV
 if(loc==1)call bundle_to_host(state,lands,convection,drag)
 if(card_radiation)then
  !$acc update self(rad,rad_heating)
 endif
#endif
 call device_fields_to_host(fc)
 if(card_thompson)then
  call micro_to_host(cmp)
  !$acc update self(cmp%precipw)
 endif
 call diag_to_host()
 end subroutine

 ! The output's ERR lines (for example a file the stream's clobber mode forbids overwriting) are logged and do not stop
 ! the run, as in MPAS (mpas_atm_core.F:704-710).
 subroutine output_messages(msgs)
 type(message_list),intent(inout) :: msgs
 call log_messages(msgs)
 msgs%n=0
 end subroutine

 ! The fields the output streams need (need), copied from the card to the host; nothing goes to the card and no physics
 ! stage changes where it runs.
 subroutine output_fields_to_host(state,lands,convection,need)
 type(state_type),intent(inout) :: state
 type(noahmp_state_type),intent(inout) :: lands
 type(gf_state_type),intent(inout) :: convection
 logical,intent(in) :: need(N_FETCH)
 if(need(FETCH_DYN))then
  !$acc update self(fc%u2,fc%w2,fc%theta_m2,fc%rho_zz2,fc%scalars2,fc%pressure_p,fc%u_zonal,fc%u_meridional)
 endif
 if(need(FETCH_DIAG_DYN))then
  !$acc update self(fc%exner,fc%divergence,fc%ke,fc%vorticity)
 endif
 if(need(FETCH_PRECIPW).and.card_thompson)then
  !$acc update self(cmp%precipw)
 endif
 if(need(FETCH_MICRO).and.card_thompson)call micro_to_host(cmp)
#ifdef LEAN_PHYS_DEV
 if(need(FETCH_CONV))then
  !$acc update self(convection%pratec,state%cd,state%cda,state%ck,state%cka) if(loc==1)
 endif
 if(need(FETCH_STATE))then
  !$acc update self(state%t2,state%th2,state%q2,state%u10,state%v10,state%hfx,state%qfx,state%lh) if(loc==1)
  !$acc update self(state%kpbl,state%pblh,state%tsk,state%xland,state%snowh,state%cldfra_bl) if(loc==1)
 endif
 if(need(FETCH_LAND))then
  !$acc update self(sea_ice_cells%tslb) if(loc==1.and.ice_on)
  !$acc update self(lands%energy_state_temperaturesoilsnow,lands%water_state_soilmoisture) if(loc==1)
  !$acc update self(lands%water_state_soilliqwater) if(loc==1)
  !$acc update self(lands%energy_state_spechumidity2mveg,lands%energy_state_spechumidity2mbare) if(loc==1)
  !$acc update self(lands%energy_state_exchcoeffsh2mveg,lands%energy_state_exchcoeffsh2mbare) if(loc==1)
 endif
 if(need(FETCH_RAD))then
  !$acc update self(rad) if(card_radiation.and..not.check_radiation)
  !$acc update self(snow,albedo,emiss) if(loc==1)
 endif
#endif
 end subroutine


#ifdef LEAN_BUDGET
 ! One line per step of global means, read by tools/budget_summary.py: the water and energy each scheme hands over, sampled
 ! after the physics and before the dynamics. Mass is the dry mass, as in the tendency coupling (physics_tendency.f90).
 subroutine budget_step(it,state,conv)
 integer, intent(in) :: it
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: conv
 real(kind=8), parameter :: cpd=1004.5d0,cvd=717.5d0,grav=9.80616d0,lv=2.5d6,lf=3.34d5,rvrd=461.6d0/287.d0,day=86400.d0
 integer :: c,k
 real(kind=8) :: b(48),col(48),w,area,water,mass,pi,qt,tm,z,cp_pi,diss
 !$acc update self(fc%rho_zz1)
 !$acc update self(state%theta, state%exner, state%u, state%v, state%qv, state%qc, state%qr, state%qi, state%qs, state%qg) if(loc==1)
 !$acc update self(state%t_theta, state%t_qv, state%t_qc, state%t_qi, state%t_qs, state%t_u, state%t_v) if(loc==1)
 !$acc update self(state%hfx, state%qfx, state%lh, state%xland, state%qke, state%el_pbl, state%p) if(loc==1)
 !$acc update self(conv%rthcuten, conv%rqvcuten, conv%rqccuten, conv%rqicuten, conv%pratec) if(loc==1)
 !$acc update self(rad, rad_heating)
 if (card_thompson) call micro_to_host(cmp)
 b=0.d0;area=0.d0;water=0.d0
 do c=1,ncells
  col=0.d0
  do k=1,nz
   mass=real(fc%rho_zz1(k,c),8)*real(m%zz(k,c),8)*real(m%zgrid(k+1,c)-m%zgrid(k,c),8)
   pi=state%exner(k,c);cp_pi=cpd*pi
   qt=real(state%qv(k,c),8)+state%qc(k,c)+state%qr(k,c)+state%qi(k,c)+state%qs(k,c)+state%qg(k,c)
   tm=state%theta(k,c)*pi*(1.d0+rvrd*state%qv(k,c))
   z=0.5d0*(real(m%zgrid(k,c),8)+m%zgrid(k+1,c))
   col(1)=col(1)+mass*qt
   col(2)=col(2)+mass*cvd*tm
   col(3)=col(3)+mass*(1.d0+qt)*grav*z
   col(4)=col(4)+mass*(1.d0+qt)*0.5d0*(real(state%u(k,c),8)**2+real(state%v(k,c),8)**2)
   col(5)=col(5)+mass*lv*state%qv(k,c)
   col(6)=col(6)+mass*lf*(real(state%qi(k,c),8)+state%qs(k,c)+state%qg(k,c))
   col(7)=col(7)+mass*state%t_qv(k,c)
   col(8)=col(8)+mass*state%t_qc(k,c)
   col(9)=col(9)+mass*(real(state%t_qi(k,c),8)+state%t_qs(k,c))
   col(10)=col(10)+mass*cp_pi*state%t_theta(k,c)
   col(11)=col(11)+mass*conv%rqvcuten(k,c)
   col(12)=col(12)+mass*conv%rqccuten(k,c)
   col(13)=col(13)+mass*conv%rqicuten(k,c)
   col(14)=col(14)+mass*cp_pi*conv%rthcuten(k,c)
   col(15)=col(15)+mass*cp_pi*cmp%dtheta_dt_mp(k,c)
   col(16)=col(16)+mass*cp_pi*rad_heating(k,1,c)
   col(17)=col(17)+mass*cp_pi*rad_heating(k,2,c)
   col(18)=col(18)+mass*(real(state%u(k,c),8)*state%t_u(k,c)+real(state%v(k,c),8)*state%t_v(k,c))
   ! MYNN's dissipative heating (mynn_pbl.f90, diss_heat): theta tendency in K/s, without its cap
   if(k<nz)then
    diss=real(state%qke(k,c),8)**1.5d0/(24.d0*max(0.5d0*(real(state%el_pbl(k,c),8)+state%el_pbl(k+1,c)),1.d0))
    col(30)=col(30)+mass*diss*exp(-1.d4/max(real(state%p(k,c),8),1.d0))
   endif
  enddo
  col(19)=state%qfx(c);col(20)=state%hfx(c);col(21)=state%lh(c);col(22)=conv%pratec(c)
  col(23)=cmp%rainnc(c);col(24)=cmp%snownc(c);col(25)=cmp%graupelnc(c)
  col(26:29)=real(rad(c)%flux(1:4),8)
  col(31:32)=real(rad(c)%flux(5:6),8)
  col(46:47)=real(rad(c)%flux(7:8),8)
  w=real(m%areaCell(c),8)
  area=area+w
  b(1:32)=b(1:32)+w*col(1:32)
  b(46:47)=b(46:47)+w*col(46:47)
  if(state%xland(c)>1.5)then
   water=water+w
   b(33:38)=b(33:38)+w*[col(19),col(20),col(21),col(22),col(27),col(29)]
  else
   b(39:44)=b(39:44)+w*[col(19),col(20),col(21),col(22),col(27),col(29)]
  endif
 enddo
 b(33:38)=b(33:38)/water
 b(39:44)=b(39:44)/(area-water)
 b(1:32)=b(1:32)/area
 b(45)=water/area
 b(46:47)=b(46:47)/area
 print '(a,i7,47es17.9)','budget ',it,b(1:47)
 end subroutine
#endif

 ! The state check every state_check_every steps: the run stops on a non-finite wind, density, theta or scalar, or on a
 ! dry density that is not positive. (A value is finite when abs(value) <= huge: false for NaN and infinity.)
 subroutine check_model_state()
 integer :: c,k,e,s,bad,nonpositive
 bad=0;nonpositive=0
 !$acc parallel loop gang vector collapse(2) default(present) reduction(+:bad)
 do e=1,ne1
  do k=1,nz
   if(.not.(abs(fc%u2(k,e))<=huge(1.)))bad=bad+1
  enddo
 enddo
 !$acc parallel loop gang vector collapse(2) default(present) reduction(+:bad,nonpositive)
 do c=1,nc1
  do k=1,nz+1
   if(.not.(abs(fc%w2(k,c))<=huge(1.)))bad=bad+1
   if(k<=nz)then
    if(.not.(abs(fc%rho_zz2(k,c))<=huge(1.)))bad=bad+1
    if(c<=ncells)then
     if(fc%rho_zz2(k,c)<=0.)nonpositive=nonpositive+1
    endif
   endif
  enddo
 enddo
 if(bad>0)then
  call log_fatal('lean_mpas stops: nonfinite wind/density')
  error stop 'nonfinite wind/density'
 endif
 if(nonpositive>0)then
  call log_fatal('lean_mpas stops: nonpositive dry density')
  error stop 'nonpositive dry density'
 endif
 !$acc parallel loop gang vector collapse(2) default(present) reduction(+:bad)
 do c=1,ncells
  do k=1,nz
   if(.not.(abs(fc%theta_m2(k,c))<=huge(1.)))bad=bad+1
   do s=1,num_scalars
    if(.not.(abs(fc%scalars2(s,k,c))<=huge(1.)))bad=bad+1
   enddo
  enddo
 enddo
 if(bad>0)then
  call log_fatal('lean_mpas stops: nonfinite state')
  error stop 'nonfinite state'
 endif
 end subroutine
 ! The surface pressure MPAS computes at the start of every physics step from the state (MPAS_to_physics,
 ! mpas_atmphys_interface.F:426-438): on a cold start its first value comes from the initial state, not from the init
 ! file. Lean's dyn_micro computes the same at the end of each step and Thompson uses that value (the other schemes
 ! recompute it from the state); the boundary reset after it can still change the lowest-level qv of boundary cells
 ! (dyn_step.f90), which MPAS would see and lean does not.
 real function start_surface_pressure(c)
 integer, intent(in) :: c
 real :: tem1, tem2, rho1, rho2
 tem1 = m%zgrid(2,c) - m%zgrid(1,c)
 tem2 = m%zgrid(3,c) - m%zgrid(2,c)
 rho1 = s%rho_zz(1,c) * m%zz(1,c) * (1. + max(0., s%scalars(i_qv,1,c)))
 rho2 = s%rho_zz(2,c) * m%zz(2,c) * (1. + max(0., s%scalars(i_qv,2,c)))
 start_surface_pressure = 0.5*gravity*(m%zgrid(2,c) - m%zgrid(1,c)) * (rho1 - 0.5*(rho2 - rho1)*tem1/(tem1 + tem2))
 start_surface_pressure = start_surface_pressure + s%pressure_p(1,c) + s%pressure_base(1,c)
 end function
end module
