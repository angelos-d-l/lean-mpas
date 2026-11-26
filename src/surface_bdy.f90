! The surface update: MPAS's physics_update_sst on lean's memories, the 'surface' stream's records
! it reads, and the file test for the sea ice that may appear.
!
! MPAS reads the 'surface' stream when its input alarm rings (core_atmosphere/mpas_atm_core.F:787-809; the record at or
! before the time, MPAS_STREAM_LATEST_BEFORE) and applies it in the physics time tracker when its sfcbdy alarm rings
! (physics/mpas_atmphys_manager.F:240-246), after the greenness update, before the physics of the step. Both alarms ring at
! the start and every input_interval after it (mpas_atmphys_manager.F:557-565). Lean reads and applies on its one sfcbdy
! alarm (docs/fixes.md, "The surface update (config_sst_update)", "One alarm").
!
! update_sst copies physics_update_sst (mpas_atmphys_update_surface.F:114-326) line for line, with config_frac_seaice true
! (lean has only that: the Registry default) and so xice_threshold 0.02 (mpas_atmphys_manager.F:740-744). Each field MPAS
! assigns is a row of the table below; tests/surface/units.f90 (map_complete) fails when a field of MPAS's text has no row.
!
! map xice      ice%xice(c), model_data ice(c), lean_fields xice(c): the threshold applied to every cell (:184-187)
! map xicem     xicem(c) of this module (restart row xicem), set at the end of the cell (:285)
! map xland     state%xland(c): 1 on a cell that becomes sea ice, 2 on one that melts (:222, :247)
! map tsk       state%tsk(c): sst on every water cell after the transitions (:271-272)
! map tslb      ice%tslb(:,c): tsk of a new sea-ice cell (:226), sst of a melted cell (:260), then 273.16 on every water
!               cell (:274). A land cell's soil temperature is Noah-MP's and is never changed here.
! map smois     lands%water_state_soilmoisture(1:4,c): 1 on a new sea-ice cell and on a melted one (:227, :261)
! map sh2o      lands%water_state_soilliqwater(1:4,c): 0 on a new sea-ice cell, 1 on a melted one (:228, :262)
! map snow      model_data snow(c) and ice%snow(c): 0 on a melted cell (:251)
! map snowc     ice%snowc(c): 0 on a melted cell (:250)
! map snowh     state%snowh(c): 0 on a melted cell (:252)
! map sfc_albedo  model_data albedo(c) and ice%albedo(c): the rescale of :204, 0.8*xice+0.08*(1-xice) on a new sea-ice
!               cell (:234), 0.08 on a melted one (:254)
! map sfc_emiss model_data emiss(c) and ice%emiss(c): as sfc_albedo with 0.98 (:205, :235, :256)
! map vegfra    lean_fields vegetation(c): 0 on a cell that changes type (:221, :246)
! map ivgtyp    ivgtyp_now(c) of this module (restart row ivgtyp): the sea-ice category on a new sea-ice cell (:219), water on a
!               melted one (:244). No lean scheme reads it; the restart file carries it as MPAS's restart does.
! map isltyp    isltyp_now(c) of this module (restart row isltyp): 16 on a new sea-ice cell (:220), 14 on a melted one (:245)
! map tmn       no lean target: no lean scheme reads the deep-soil temperature of a water or sea-ice column
! map sfc_albbck  no lean target: only Noah-MP's background albedo reads it, and a sea-ice or water column has none
! map sfc_emibck  no lean target: no lean scheme reads the background emissivity
! map seaice    no lean target: MPAS's flag of xice > 0 (:286-289) is read by no lean scheme; ice_cell(c) tests xice itself
module surface_bdy
 use iso_fortran_env, only: int64
 use config, only: ncells, nc1
 use model_state, only: state_type
 use noahmp_state, only: noahmp_state_type
 use sea_ice, only: sea_ice_cells => ice, xice_threshold, any_sea_ice
 use model_data, only: md_emiss => emiss, md_ice => ice, md_snow => snow, md_albedo => albedo
 use lean_fields, only: lf_sst => sst, lf_xice => xice, lf_vegetation => vegetation, tslb_by_update
 use lean_nc, only: nc_get, NC_NOERR
 use lean_input_fields, only: input_file, open_input, close_input, read_field, has_variable, dimension_length
 use lean_inputs, only: xtime_seconds
 use lean_messages, only: message_list, add_message, int_text, real_text, LOG_WARN, LOG_ERR, LOG_CRIT
 use schemes, only: moist_physics
 implicit none
 private
 public :: xicem, ivgtyp_now, isltyp_now, update_counts, allocate_surface_bdy, surface_bdy_start, read_surface_record, update_sst, &
           surface_file_has_ice
 ! diag_physics xicem (Registry.xml:854, 3478): the sea-ice fraction of the last update, a restart row
 real, allocatable, save :: xicem(:)
 ! The vegetation and soil types of every cell as MPAS's ivgtyp and isltyp: the init's, changed by the update on the cells that
 ! freeze or melt. Two restart rows in place of the copied inputs. Nothing else in lean reads them (surface_output keeps its own
 ! copy of the start's types).
 integer, allocatable, save :: ivgtyp_now(:), isltyp_now(:)
 ! MODIS categories of the init file (lean_inputs.f90 stops on any other): isice_lu and iswater_lu
 integer, parameter :: category_ice = 15, category_water = 17
 ! the cells of the last update by branch: to land (sea ice appears), to ocean (it melts), removed, rescaled
 integer, save :: update_counts(4) = 0
contains

 ! The memory of xicem: allocated in every run (the restart reads it), 0 as MPAS's arrays are allocated.
 subroutine allocate_surface_bdy()
 if (.not. allocated(xicem)) allocate(xicem(ncells), ivgtyp_now(ncells), isltyp_now(ncells))
 xicem = 0.
 ivgtyp_now = 0
 isltyp_now = 0
 end subroutine

 ! mpas_atmphys_init.F:328-333: a cold start sets xicem to xice. Without any moist physics MPAS's physics initialisation is
 ! skipped (mpas_atm_core.F:580-596) and the restart holds xicem as allocated, 0.
 subroutine surface_bdy_start(xice)
 real, intent(in) :: xice(:)
 if (.not. allocated(xicem)) call allocate_surface_bdy()
 if (moist_physics) xicem = xice(1:ncells)
 end subroutine

 ! The record of the surface file at or before `now` (YYYY-MM-DD_hh:mm:ss), as MPAS_seekStream finds it with
 ! MPAS_STREAM_LATEST_BEFORE (framework/mpas_io_streams.F:289-296): the latest record whose xtime is not after `now`.
 ! sst and xice (nc1 long) hold the record; a variable the file lacks is 0 with MPAS's warning (framework/mpas_io.F:875).
 ! valid is the record's xtime. No record at or before `now`: an ERR line with the reason and valid blank.
 subroutine read_surface_record(path, now, sst, xice, valid, msgs)
 character(*), intent(in) :: path, now
 real, intent(out), contiguous, target :: sst(:), xice(:)
 character(*), intent(out) :: valid
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 character(len=256), allocatable :: xtimes(:)
 integer(int64) :: gap, best
 integer :: n, i
 valid = ''
 sst = 0.
 xice = 0.
 call open_input(f, path, 'surface', msgs)
 if (.not. f%opened) return
 n = dimension_length(f, 'Time')
 if (n <= 0 .or. .not. has_variable(f, 'xtime')) then
  call add_message(msgs, LOG_WARN, 'File '//trim(path)//' does not contain a seekable xtime variable. '// &
                   'Forcing a read of the first time record.')
  f%record = min(n, 1)
  valid = now
 else
  allocate(xtimes(n))
  if (nc_get(f%nc, 'xtime', xtimes) /= NC_NOERR) then
   call add_message(msgs, LOG_ERR, trim(path)//': cannot read xtime')
   call close_input(f)
   return
  end if
  f%record = 0
  best = 864000000_int64            ! MPAS_seekStream starts from 10000 days
  do i = 1, n
   if (xtime_seconds(xtimes(i), xtimes(i)) /= 0) then
    call add_message(msgs, LOG_ERR, trim(path)//': xtime of record '//int_text(i)//' is not a time: '''// &
                     trim(xtimes(i))//'''')
    call close_input(f)
    return
   end if
   gap = xtime_seconds(now, xtimes(i))            ! now minus the record's time
   if (gap < 0) cycle                            ! a record after `now`
   if (gap < best) then
    best = gap
    f%record = i
   end if
  end do
  if (f%record == 0) then
   call add_message(msgs, LOG_ERR, 'File '//trim(path)//' does not contain a record at or before '//trim(now))
   call close_input(f)
   return
  end if
  valid = xtimes(f%record)(1:19)
 end if
 call read_variable(f, 'sst', sst, msgs)
 call read_variable(f, 'xice', xice, msgs)
 call close_input(f)
 end subroutine

 subroutine read_variable(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:)
 type(message_list), intent(inout) :: msgs
 a = 0.
 if (has_variable(f, name)) then
  call read_field(f, name, a, msgs)
 else
  call add_message(msgs, LOG_WARN, 'Variable '//name//' not in input file.')
 end if
 end subroutine

 ! Whether sea ice may appear in the run: any record of the file has xice at the threshold or above on a water cell
 ! (xland > 1.5; ice on a land cell stops the run at the update). True also lets the cells that are ice-free at the start take the
 ! sea-ice code (ice_on of the card, any_sea_ice), which is chosen once.
 logical function surface_file_has_ice(path, xland, msgs)
 character(*), intent(in) :: path
 real, intent(in) :: xland(:)
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 real, allocatable :: sst(:), xice(:)
 integer :: n, rec, c
 surface_file_has_ice = .false.
 allocate(sst(nc1), xice(nc1))
 call open_input(f, path, 'surface', msgs)
 if (.not. f%opened) return
 n = max(dimension_length(f, 'Time'), 1)
 if (has_variable(f, 'xice')) then
  do rec = 1, n
   f%record = merge(rec, 0, dimension_length(f, 'Time') > 0)
   call read_field(f, 'xice', xice, msgs)
   do c = 1, ncells
    if (xice(c) >= xice_threshold .and. xland(c) > 1.5) surface_file_has_ice = .true.
   end do
   if (surface_file_has_ice) exit
  end do
 end if
 call close_input(f)
 end function

 ! MPAS's physics_update_sst on lean's memories, for the record (sst_in, xice_in; nc1 long) just read. `where` names the
 ! file and the time for the stop below. A land cell that receives sea ice stops the update before any change: lean keeps
 ! no sea-ice state for a land column.
 subroutine update_sst(sst_in, xice_in, state, lands, msgs, where)
 real, intent(in) :: sst_in(:), xice_in(:)
 type(state_type), intent(inout) :: state
 type(noahmp_state_type), intent(inout) :: lands
 type(message_list), intent(inout) :: msgs
 character(*), intent(in), optional :: where
 integer :: c, n_land_ice, n_new_ice, first
 real :: x, first_xice
 character(len=512) :: where_text
 where_text = 'the surface record'
 if (present(where)) where_text = where
 ! sea ice on a land cell: init_atmosphere writes none (mpas_init_atm_cases.F:4833-4848), a hand-made file might
 n_land_ice = 0
 first = 0
 first_xice = 0.
 do c = 1, ncells
  x = xice_in(c)
  if (x < xice_threshold) x = 0.
  if (state%xland(c) < 1.5 .and. x >= xice_threshold .and. xicem(c) < xice_threshold) then
   n_land_ice = n_land_ice + 1
   if (first == 0) then
    first = c
    first_xice = x
   end if
  end if
 end do
 if (n_land_ice > 0) then
  call add_message(msgs, LOG_CRIT, trim(where_text)//': '//int_text(n_land_ice)// &
   ' land cell(s) receive sea ice (the first: cell '//int_text(first)//', xice '//real_text(first_xice)// &
   '). lean_mpas turns open water into sea ice as MPAS''s surface update does, but keeps no sea-ice state for a land '// &
   'column; init_atmosphere writes no sea ice on land (mpas_init_atm_cases.F:4833-4848).')
  return
 end if
 ! sea ice on a water cell in a run without sea-ice cells: the sea-ice code is chosen once, at the start (any_sea_ice); a water cell that freezes without it would run as a land
 ! column that was never started
 n_new_ice = 0
 first = 0
 first_xice = 0.
 if (.not. any_sea_ice) then
  do c = 1, ncells
   x = xice_in(c)
   if (x < xice_threshold) x = 0.
   if (state%xland(c) >= 1.5 .and. x >= xice_threshold .and. xicem(c) < xice_threshold) then
    n_new_ice = n_new_ice + 1
    if (first == 0) then
     first = c
     first_xice = x
    end if
   end if
  end do
 end if
 if (n_new_ice > 0) then
  call add_message(msgs, LOG_CRIT, trim(where_text)//': '//int_text(n_new_ice)// &
   ' water cell(s) receive sea ice (the first: cell '//int_text(first)//', xice '//real_text(first_xice)// &
   '), but this run set up no sea-ice cells at its start. lean_mpas chooses the sea-ice code once, at the start, from the '// &
   'init file, the restart and the records of the surface files the run will open.')
  return
 end if
 update_counts = 0
 tslb_by_update = .true.     ! the history's tslb of water and sea ice is this update's from now on (lean_fields.f90)
 do c = 1, ncells
  call update_cell(c)
 end do
 contains
  subroutine update_cell(c)
  integer, intent(in) :: c
  real :: x
  ! :184-187: the sea-ice fraction below the threshold is 0
  x = xice_in(c)
  if (x < xice_threshold) x = 0.
  ! :198-208: sfc_albedo and sfc_emiss were valid for the earlier xice, xicem; rescale them to the new one
  if (x /= xicem(c) .and. xicem(c) >= xice_threshold) then
   update_counts(4) = update_counts(4) + 1
   sea_ice_cells%albedo(c) = 0.08 + (sea_ice_cells%albedo(c) - 0.08)*x/xicem(c)
   sea_ice_cells%emiss(c) = 0.98 + (sea_ice_cells%emiss(c) - 0.98)*x/xicem(c)
   md_albedo(c) = sea_ice_cells%albedo(c)
   md_emiss(c) = sea_ice_cells%emiss(c)
  end if
  if (state%xland(c) >= 1.5 .and. x >= xice_threshold .and. xicem(c) < xice_threshold) then
   ! :216-237: water becomes sea ice (a land cell with ice, the second sea-ice case of MPAS's test, stopped above)
   update_counts(1) = update_counts(1) + 1
   lf_vegetation(c) = 0.
   ivgtyp_now(c) = category_ice
   isltyp_now(c) = 16
   state%xland(c) = 1.
   sea_ice_cells%tslb(:,c) = state%tsk(c)
   lands%water_state_soilmoisture(1:4,c) = 1.
   lands%water_state_soilliqwater(1:4,c) = 0.
   ! over newly formed ice the guesses for the albedo and emissivity are the default values of ice and water
   sea_ice_cells%albedo(c) = 0.80*x + 0.08*(1. - x)
   sea_ice_cells%emiss(c) = 0.98*x + 0.98*(1. - x)
   md_albedo(c) = sea_ice_cells%albedo(c)
   md_emiss(c) = sea_ice_cells%emiss(c)
   ! the sea-ice code starts from the cell's roughness, lean's evolved znt of the water column. MPAS's z0 on water stays the
   ! landuse value (mpas_atmphys_landuse.F:285) and only its znt evolves. The value has no effect: sflx_seaice sets its own
   ! roughness before using it (Z0BRD = 0.001 in sflx_seaice, sea_ice_noah.f90) and ice_step overwrites ice%z0 (with LSM 'off' it reaches the restart row)
   sea_ice_cells%z0(c) = state%znt(c)
  else if (state%xland(c) < 1.5 .and. x < xice_threshold .and. xicem(c) >= xice_threshold) then
   ! :239-263: sea ice turns to water
   update_counts(2) = update_counts(2) + 1
   lf_vegetation(c) = 0.
   ivgtyp_now(c) = category_water
   isltyp_now(c) = 14
   state%xland(c) = 2.
   sea_ice_cells%snowc(c) = 0.
   sea_ice_cells%snow(c) = 0.
   md_snow(c) = 0.
   state%snowh(c) = 0.
   sea_ice_cells%albedo(c) = 0.08
   sea_ice_cells%emiss(c) = 0.98
   md_albedo(c) = 0.08
   md_emiss(c) = 0.98
   sea_ice_cells%tslb(:,c) = sst_in(c)
   lands%water_state_soilmoisture(1:4,c) = 1.
   lands%water_state_soilliqwater(1:4,c) = 1.
  else if (x < xice_threshold .and. xicem(c) < xice_threshold) then
   update_counts(3) = update_counts(3) + 1     ! nb_removed: xice is already 0
  end if
  ! :271-275: water takes the sea-surface temperature, its soil temperature is 273.16
  if (state%xland(c) >= 1.5) then
   state%tsk(c) = sst_in(c)
   sea_ice_cells%tslb(:,c) = 273.16
  end if
  ! the record's values in every place lean keeps them; :283-290 the fraction for the next update
  lf_sst(c) = sst_in(c)
  sea_ice_cells%sst(c) = sst_in(c)
  lf_xice(c) = x
  md_ice(c) = x
  sea_ice_cells%xice(c) = x
  xicem(c) = x
  end subroutine
 end subroutine

end module
