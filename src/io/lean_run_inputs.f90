! The inputs of an MPAS run directory, from its namelist and streams file: the only code that joins lean_inputs (what
! is read) with lean_streams and lean_clock (which file, which record, when).
! Their routines stand for these MPAS steps (driver/mpas_subdriver.F, core_atmosphere/mpas_atm_core.F):
!   streams_inquire, packages_when   the stream inquiry and the two packages set before the suite    (mpas_subdriver.F:262-281)
!   resolve_suite, set_packages      the physics suite and the packages of the run                    (mpas_atm_core_interface.F:137-258)
!   clock_from_config                the simulation clock                                             (mpas_atm_core.F:299-363)
!   streams_mesh                     the mesh stream and its file                                     (mpas_subdriver.F:325-354)
!   streams_parse                    the stream manager's streams, alarms and field lists             (mpas_subdriver.F:370-384)
!   read_filename, stream_fields     the file a stream reads at a time; the fields it reads           (mpas_stream_manager.F:3686-3698)
!   plan_start, plan_step_inputs     the boundary reads of atm_core_init and of each step             (mpas_atm_core.F:736-771)
!   record_interval                  MPAS_STREAM_PROPERTY_RECORD_INTV of lbc_in                       (mpas_atm_boundaries.F:852-867)
!   build_stream_filename            the file one filename interval away, for a seek's retest         (mpas_stream_manager.F:3790-3870)
! lean's own reads stand for: read_mesh_inputs the bootstrap (framework/mpas_bootstrapping.F:158-340), read_dimensions
! its "Reading dimensions" block (:492-662), read_state_inputs the read of the 'input' stream (mpas_atm_core.F:134-152),
! effective_len_disp :165-199, bdy_checks mpas_atm_bdy_checks, lbc_open every read of mpas_atm_update_bdy_tend.
module lean_run_inputs
 use iso_fortran_env, only: int64
 use ESMF, only: ESMF_TimeIntervalGet, ESMF_TimeInterval
 use config, only: ncells
 use lean_messages, only: message_list, add_message, has_fatal, int_text, real_text, logical_text, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT
 use lean_namelist, only: run_config
 use lean_clock
 use lean_streams
 use lean_stream_registry, only: FIELD_NAME, FIELD_DIMS, FIELD_PACKAGES, N_PACKAGES
 use lean_nc
 use lean_input_fields, only: count_problems, input_file, open_input, close_input, read_field
 use lean_inputs
 use schemes, only: micro_on
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use model_start, only: start_info, start_cold, start_from_times, start_from_restart
 use lean_restart_file, only: restart_header, restart_check
 use lean_memory, only: check_memory
 implicit none
 private
 public :: run_inputs, read_run_mesh, open_run_inputs, bdy_checks, lbc_requests, unserved_streams

 type run_inputs
  type(cold_inputs) :: cold            ! read_mesh_inputs fills it, read_state_inputs completes it
  type(lbc_reader) :: lbc
  type(start_info) :: st               ! src/model_start.f90
  type(restart_header) :: restart
  real, allocatable :: restart_greenfrac(:,:)
  integer, allocatable :: restart_ivgtyp(:), restart_isltyp(:)
  real, allocatable :: restart_snoalb(:)
  real :: greeness_s = 86400.
  real :: len_disp = 0.
  logical :: thompson_big_endian = .false.
 end type

contains

 ! lean's stop line, after n ERR lines of its own reads and checks.
 subroutine stop_line(n, msgs)
 integer, intent(in) :: n
 type(message_list), intent(inout) :: msgs
 call add_message(msgs, LOG_CRIT, 'lean_mpas stops: '//int_text(n)//' input problem(s) listed above')
 end subroutine

 ! The mesh of the mesh stream's file, between streams_mesh and streams_parse, where MPAS bootstraps
 ! (driver/mpas_subdriver.F:356-368): the memory check right before it, then read_mesh_inputs, and the stop line after
 ! an ERR line either added. nsteps and namelist_path go into the memory check's messages.
 subroutine read_run_mesh(cfg, clock, nsteps, namelist_path, set, m, run, msgs)
 type(run_config), intent(in) :: cfg
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 character(*), intent(in) :: namelist_path
 type(stream_set), intent(in) :: set
 type(dyn_mesh_type), intent(out) :: m
 type(run_inputs), intent(inout) :: run
 type(message_list), intent(inout) :: msgs
 integer :: before
 if(set%mesh_stream=='restart')then
  call restart_check(trim(set%mesh_file),'',run%restart,msgs)
  if(has_fatal(msgs))return
 endif
 before = count_problems(msgs)
 call check_memory(cfg, clock, nsteps, namelist_path, set, msgs)
 if (has_fatal(msgs)) return
 if (count_problems(msgs) > before) then
  call stop_line(count_problems(msgs) - before, msgs)
  return
 end if
 call read_mesh_inputs(trim(set%mesh_file), m, run%cold, msgs)
 if (has_fatal(msgs)) return
 if (count_problems(msgs) > before) call stop_line(count_problems(msgs) - before, msgs)
 end subroutine

 ! Everything a start from the init file reads from the run directory, after the mesh (read_mesh_inputs) and the parse
 ! (a restart goes to open_restart_inputs):
 !  1. the start time;
 !  2. MPAS's "Reading dimensions from input streams" block;
 !  3. the initial state at the clock's start from the input stream's file, and its initial_time;
 !  4. len_disp, the checks on what was read (check_inputs), the run settings, the physics tables;
 !  5. every boundary read of nsteps steps, resolved now;
 !  6. the start descriptor.
 ! A CRIT returns at once. In steps 1-3 an ERR line added here ends the sequence with the stop line; steps 4 and 5 list
 ! all their problems, and the stop line follows them.
 subroutine open_run_inputs(cfg, set, clock, nsteps, m, dyn, run, msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 type(dyn_mesh_type), intent(inout) :: m
 type(cold_dyn_type), intent(out) :: dyn
 type(run_inputs), intent(inout) :: run
 type(message_list), intent(inout) :: msgs
 type(ESMF_Time) :: whole
 type(message_list) :: dropped
 character(len=32), allocatable :: fields(:)
 character(len=:), allocatable :: init, start
 character(len=19) :: initial, written
 real :: radt_lw_s, radt_sw_s
 integer :: before, tables, input, y, mo, d, h, mi, sec, iy, imo, id, ih, imi, is
 logical :: ok

 if(cfg%config_do_restart)then
  call open_restart_inputs(cfg,set,clock,nsteps,m,run,msgs)
  return
 endif
 before = count_problems(msgs)

 ! 1.
 ! MPAS preserves a decimal fraction (mpas_timekeeping.F:1175-1200); start_info cannot.
 ! Reject before time_string discards it and before reading any state.
 if(index(trim(cfg%config_start_time),'.')>0)then
  call add_message(msgs,LOG_ERR,"config_start_time = '"//trim(cfg%config_start_time)// &
                   "': fractional seconds are not supported")
  call stop_line(1,msgs)
  return
 endif
 start = time_string(clock%start)

 ! 2.
 call read_dimensions(set, clock, msgs)
 if (has_fatal(msgs)) return

 ! 3.
 input = stream_index(set, 'input')
 if (.not. read_filename(set, input, clock%start, init, msgs)) init = trim(set%s(input)%template)
 call stream_fields(set, input, fields)
 call read_state_inputs(init, start, m, dyn, run%cold, msgs, fields)
 if (has_fatal(msgs)) return
 if (count_problems(msgs) == before) then
  ! initial_time as MPAS's time manager reads it (framework/mpas_timekeeping.F:1143-1298): hours and days roll
  ! over, so 2026-11-08_24:00:00 is the next day's 00 UTC. The time it means is what the start descriptor takes.
  written = run%cold%initial_time
  initial = written
  if (xtime_seconds(written, written) /= 0) then
   call add_message(msgs, LOG_ERR, init//': initial_time is not a time: '''//trim(written)//'''')
  else
   ok = parse_time(trim(written), whole, dropped)
   initial = time_string(whole)
   if (xtime_seconds(start, initial) < 0 .and. initial /= written) then
    call add_message(msgs, LOG_ERR, init//': initial_time = '''//trim(written)//''' ('//initial// &
                     ') is after the start time '//start)
   else if (xtime_seconds(start, initial) < 0) then
    call add_message(msgs, LOG_ERR, init//': initial_time = '''//initial//''' is after the start time '//start)
   end if
  end if
 end if
 if (count_problems(msgs) > before) then
  call stop_line(count_problems(msgs) - before, msgs)
  return
 end if

 ! 4.
 run%len_disp = effective_len_disp(cfg%config_len_disp, run%cold%nominal_min_dc, msgs)
 call check_inputs(run%cold, m, dyn, run%len_disp, cfg%config_lean_strict_inputs, msgs)
 radt_lw_s = interval_of(cfg%config_radtlw_interval, cfg%config_dt, msgs)
 radt_sw_s = interval_of(cfg%config_radtsw_interval, cfg%config_dt, msgs)
 run%greeness_s = interval_of(cfg%config_greeness_update, cfg%config_dt, msgs)
 call apply_run_config(cfg, m, run%cold, run%len_disp, radt_lw_s, radt_sw_s, run%greeness_s, msgs)
 tables = count_problems(msgs)
 call check_run_tables('.', msgs)
 if (count_problems(msgs) == tables .and. micro_on) call thompson_table_order('.', run%thompson_big_endian, msgs)

 ! 5.
 call open_run_lbc(cfg,set,clock,nsteps,start,run,msgs)
 if(has_fatal(msgs))return

 ! 6.
 if (count_problems(msgs) > before) then
  call stop_line(count_problems(msgs) - before, msgs)
  return
 end if
 read(start, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') y, mo, d, h, mi, sec
 read(initial, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iy, imo, id, ih, imi, is
 call start_from_times(start_cold, y, mo, d, h, mi, sec, iy, imo, id, ih, imi, is, run%st)
 end subroutine

 subroutine open_restart_inputs(cfg,set,clock,nsteps,m,run,msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 type(dyn_mesh_type), intent(inout) :: m
 type(run_inputs), intent(inout) :: run
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 type(ESMF_Time) :: initial
 character(len=:), allocatable :: path
 character(len=19) :: start
 integer :: stream,before,tables
 real :: lw,sw
 integer(int64) :: first,last,rate

 if(index(trim(cfg%config_start_time),'.')>0)then
  call add_message(msgs,LOG_ERR,"config_start_time = '"//trim(cfg%config_start_time)// &
   "': fractional seconds are not supported")
  call stop_line(1,msgs)
  return
 endif
 before=count_problems(msgs)
 start=time_string(clock%start)
 stream=stream_index(set,'restart')
 if(.not.read_filename(set,stream,clock%start,path,msgs))path=trim(set%s(stream)%template)
 ! The restart supplies the mesh. Report a limited-area mismatch before checking
 ! the physics switch set and reading the state or its boundaries.
 if(cfg%config_apply_lbcs.neqv.(maxval(m%bdyMaskCell(1:ncells))>0))then
  call bdy_checks(cfg,set,m,msgs)
  return
 endif
 call restart_check(path,start,run%restart,msgs)
 if(has_fatal(msgs))return
 call read_dimensions(set,clock,msgs,lean_resume=.true.)
 if(has_fatal(msgs))return
 ! MPAS core_atmosphere/mpas_atm_core.F:134,147-152.
 call add_message(msgs,LOG_OUT,"Reading initial state from 'restart' stream")
 call system_clock(first,rate)
 call open_input(f,path,'restart',msgs)
 if(has_fatal(msgs))return
 f%record=run%restart%record
 allocate(run%restart_greenfrac(12,ncells),run%restart_ivgtyp(ncells),run%restart_isltyp(ncells),run%restart_snoalb(ncells))
 call read_field(f,'u_init',m%u_init,msgs)
 call read_field(f,'v_init',m%v_init,msgs)
 call read_field(f,'greenfrac',run%restart_greenfrac,msgs)
 call read_field(f,'ivgtyp',run%restart_ivgtyp,msgs)
 call read_field(f,'isltyp',run%restart_isltyp,msgs)
 call read_field(f,'snoalb',run%restart_snoalb,msgs)
 call close_input(f)
 if(count_problems(msgs)>before)then
  call stop_line(count_problems(msgs)-before,msgs)
  return
 endif
 run%cold%init_path=path
 run%cold%cf_time=run%restart%time_s
 run%cold%initial_time=run%restart%initial_time
 run%cold%start_time=start
 call system_clock(last)
 call add_message(msgs,LOG_OUT," Timing for read of 'restart' stream: "//real_text(real(last-first)/real(rate))//' s')
 call add_message(msgs,LOG_OUT,' ----- done reading initial state -----')
 run%len_disp=effective_len_disp(cfg%config_len_disp,run%cold%nominal_min_dc,msgs)
 lw=interval_of(cfg%config_radtlw_interval,cfg%config_dt,msgs)
 sw=interval_of(cfg%config_radtsw_interval,cfg%config_dt,msgs)
 run%greeness_s=interval_of(cfg%config_greeness_update,cfg%config_dt,msgs)
 call apply_run_config(cfg,m,run%cold,run%len_disp,lw,sw,run%greeness_s,msgs)
 tables=count_problems(msgs)
 call check_run_tables('.',msgs)
 if(count_problems(msgs)==tables.and.micro_on)call thompson_table_order('.',run%thompson_big_endian,msgs)
 call open_run_lbc(cfg,set,clock,nsteps,start,run,msgs)
 if(has_fatal(msgs))return
 if(count_problems(msgs)>before)then
  call stop_line(count_problems(msgs)-before,msgs)
  return
 endif
 if(.not.parse_time(trim(run%restart%initial_time),initial,msgs))then
  call stop_line(1,msgs)
  return
 endif
 call start_from_restart(run%restart%cold_start,time_string(initial),start,run%st)
 end subroutine

 subroutine open_run_lbc(cfg,set,clock,nsteps,start,run,msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 character(*), intent(in) :: start
 type(run_inputs), intent(inout) :: run
 type(message_list), intent(inout) :: msgs
 type(lbc_request), allocatable :: requests(:)
 type(ESMF_TimeInterval) :: interval
 integer :: lbc,n
 logical :: ok
 ! Without a record interval of lbc_in nothing is listed: bdy_checks stops the run with MPAS's lines, as
 ! mpas_atm_bdy_checks runs before any boundary read.
 if (cfg%config_apply_lbcs) then
  lbc = stream_index(set, 'lbc_in')
  if (record_interval(set, clock, lbc, interval)) then
   call lbc_requests(cfg, set, clock, nsteps, requests, n, msgs)
   ok = n > 0
   if (ok) ok = requests(1)%at_s == 0 .and. requests(1)%whence == LBC_LATEST_BEFORE
   if (.not. ok) then
    call add_message(msgs, LOG_ERR, 'lbc_in: the input alarm does not ring at the start time '//start// &
                     ' (input_interval = '''//trim(set%s(lbc)%raw_input)//'''): lean_mpas needs a recurring input_interval')
   else
    call lbc_open(run%lbc, requests(1:n), start, msgs)
   end if
  end if
 end if
 end subroutine

 ! A physics interval of the namelist in seconds; 'none' is if_none (config_dt for the radiation intervals, as
 ! physics/mpas_atmphys_manager.F:473-485). Text that is not an interval leaves parse_interval's ERR line and 0.
 real function interval_of(text, if_none, msgs)
 character(*), intent(in) :: text
 real, intent(in) :: if_none
 type(message_list), intent(inout) :: msgs
 type(ESMF_TimeInterval) :: interval
 interval_of = 0.
 if (trim(text) == 'none') then
  interval_of = if_none
 else if (parse_interval(trim(text), interval, msgs)) then
  if (.not. interval_seconds(interval, interval_of)) interval_of = 0.
 end if
 end function

 ! A field by its own Registry packages (mpas_pool_get_field_info's isActive): none, or one that is active.
 logical function registry_active(set, field)
 type(stream_set), intent(in) :: set
 integer, intent(in) :: field
 integer :: p
 registry_active = FIELD_PACKAGES(field) == 0
 do p = 1, N_PACKAGES
  if (btest(FIELD_PACKAGES(field), p-1) .and. set%package(p)) registry_active = .true.
 end do
 end function

 ! framework/mpas_bootstrapping.F:492-662, the block MPAS logs between the parse and the reads. For every input
 ! stream of the manager in its order: an active one's file at the start is opened and the dimensions of its active
 ! fields are listed in order of first appearance; a missing dimension is MPAS's block and CRIT, a dimension two
 ! streams' files give differently its CRIT (:605-607).
 subroutine read_dimensions(set, clock, msgs, lean_resume)
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 type(message_list), intent(inout) :: msgs
 logical, intent(in), optional :: lean_resume
 integer, parameter :: most = 64
 character(len=48) :: dims(most), known(most), text
 integer :: value(most), ndims, nknown, i, k, j, field, n, blank, status
 character(len=:), allocatable :: file, name
 type(nc_file) :: nc
 logical :: seen, restart_dims, resume
 resume=.false.
 if(present(lean_resume))resume=lean_resume
 call add_message(msgs, LOG_OUT, ' ')
 call add_message(msgs, LOG_OUT, ' ')
 call add_message(msgs, LOG_OUT, 'Reading dimensions from input streams ...')
 nknown = 0
 do i = 1, set%n
  if (set%s(i)%direction /= STREAM_INPUT .and. set%s(i)%direction /= STREAM_INPUT_OUTPUT) cycle
  name = trim(set%s(i)%name)
  call add_message(msgs, LOG_OUT, ' ')
  if (.not. set%s(i)%active.or.(resume.and.name=='input')) then
   call add_message(msgs, LOG_OUT, '----- skipping inactive stream '''//name//'''')
   cycle
  end if
  restart_dims=resume.and.name=='restart'
  ndims = 0
  do k = 1, set%s(i)%n_fields
   field = set%s(i)%field(k)%field
   if (.not. stream_has_field(set, i, trim(FIELD_NAME(field)))) cycle
   if (.not. registry_active(set, field)) cycle
   text = FIELD_DIMS(field)
   do while (len_trim(text) > 0)
    blank = index(text, ' ')
    seen = .false.
    do j = 1, ndims
     if (dims(j) == text(1:blank-1)) seen = .true.
    end do
    if (.not. seen .and. ndims < most) then
     ndims = ndims + 1
     dims(ndims) = text(1:blank-1)
    end if
    text = adjustl(text(blank:))
   end do
  end do
  if (.not. read_filename(set, i, clock%start, file, msgs)) file = trim(set%s(i)%template)
  if (nc_open(nc, file) /= NC_NOERR) then
   call add_message(msgs, LOG_OUT, '  *** unable to open input file '//file//' for stream '''//name//'''')
   cycle
  end if
  call add_message(msgs, LOG_OUT, '----- reading dimensions from stream '''//name//''' using file '//file)
  do k = 1, ndims
   if (nc_inq_dim(nc, trim(dims(k)), n) /= NC_NOERR) then
    ! The restart's fixed lean inventory replaces the Registry field list.
    if(restart_dims)cycle
    call add_message(msgs, LOG_OUT, '       '//trim(dims(k))//' *** not found in stream ***')
    call add_message(msgs, LOG_OUT, '')
    call add_message(msgs, LOG_ERR, 'At least one fields to be read from the '''//name//''' stream is dimensioned')
    call add_message(msgs, LOG_ERR, 'by '''//trim(dims(k))//''', but the '''//trim(dims(k))//''' dimension is not defined')
    call add_message(msgs, LOG_ERR, 'in the file '//file)
    call add_message(msgs, LOG_CRIT, 'Please check the input file(s) to be read by the '''//name//''' input stream.')
    status = nc_close(nc)
    return
   end if
   call add_message(msgs, LOG_OUT, '       '//trim(dims(k))//' = '//int_text(n))
   ! The LBC reader checks its own extents against the restart mesh.
   if(resume.and.name=='lbc_in')cycle
   seen = .false.
   do j = 1, nknown
    if (known(j) /= dims(k)) cycle
    seen = .true.
    if (value(j) /= n) then
     call add_message(msgs, LOG_CRIT, 'Dimension '//trim(dims(k))//' was read with an inconsistent value.')
     status = nc_close(nc)
     return
    end if
   end do
   if (.not. seen .and. nknown < most) then
    nknown = nknown + 1
    known(nknown) = dims(k)
    value(nknown) = n
   end if
  end do
  status = nc_close(nc)
 end do
 call add_message(msgs, LOG_OUT, ' ')
 call add_message(msgs, LOG_OUT, '----- done reading dimensions from input streams -----')
 call add_message(msgs, LOG_OUT, ' ')
 call add_message(msgs, LOG_OUT, ' ')
 end subroutine

 ! The boundary reads a run of nsteps steps makes, as MPAS makes them (mpas_atm_core.F:736-771), on a copy of the
 ! clock advanced max(nsteps, 1) steps so that both reads at the start are listed: the first, LATEST_BEFORE, when the
 ! lbc_in alarm rings at the start (plan_start), and at each step whose alarm rings EARLIEST_STRICTLY_AFTER
 ! (plan_step_inputs). file is the one MPAS opens at the read time, retest the one a filename interval earlier
 ! (LATEST_BEFORE) or later, '' without a filename interval. n requests are listed.
 subroutine lbc_requests(cfg, set, clock, nsteps, requests, n, msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 type(lbc_request), allocatable, intent(out) :: requests(:)
 integer, intent(out) :: n
 type(message_list), intent(inout) :: msgs
 type(run_clock) :: copy
 type(io_plan) :: plan
 integer :: lbc, step
 n = 0
 allocate(requests(max(nsteps, 1) + 1))
 if (.not. cfg%config_apply_lbcs) return
 lbc = stream_index(set, 'lbc_in')
 copy = clock
 copy%now = copy%start
 step = 1
 call plan_start(set, copy, cfg, plan)
 if (plan%lbc_read) call one_request(LBC_LATEST_BEFORE)
 do step = 1, max(nsteps, 1)
  call plan_step_inputs(set, copy, cfg, plan)
  if (plan%lbc_read) call one_request(LBC_EARLIEST_STRICTLY_AFTER)
  call advance_run_clock(copy)
 end do
 contains
  subroutine one_request(whence)
  integer, intent(in) :: whence
  type(ESMF_TimeInterval) :: since
  type(ESMF_Time) :: other
  character(len=:), allocatable :: name
  integer(int64) :: seconds
  integer :: days
  n = n + 1
  since = copy%now - copy%start
  call ESMF_TimeIntervalGet(since, D=days, S_i8=seconds)
  requests(n)%at_s = 86400_int64*int(days, int64) + seconds
  requests(n)%step = step
  requests(n)%whence = whence
  if (.not. read_filename(set, lbc, copy%now, name, msgs)) name = trim(set%s(lbc)%template)
  requests(n)%file = name
  requests(n)%retest = ''
  if (.not. set%s(lbc)%has_filename_interval) return
  if (whence == LBC_LATEST_BEFORE) then
   other = copy%now - set%s(lbc)%filename_interval
  else
   other = copy%now + set%s(lbc)%filename_interval
  end if
  if (build_stream_filename(set%s(lbc)%reference_time, other, set%s(lbc)%filename_interval, trim(set%s(lbc)%template), &
                            name, msgs)) requests(n)%retest = name
  end subroutine
 end subroutine

 ! Report input streams MPAS would read and lean does not serve. The plan
 ! runs on a copy of the clock, so inactive streams and alarms that never ring
 ! do not get a message.
 subroutine unserved_streams(cfg, set, clock, nsteps, msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_clock), intent(in) :: clock
 integer, intent(in) :: nsteps
 type(message_list), intent(inout) :: msgs
 type(run_clock) :: copy
 type(io_plan) :: plan
 logical :: reads(set%n), found
 character(len=:), allocatable :: name
 character(len=64) :: text
 type(ESMF_TimeInterval) :: interval
 integer :: step, i
 reads = .false.
 copy = clock
 copy%now = copy%start
 call plan_start(set, copy, cfg, plan)
 do step = 1, nsteps
  call plan_step_inputs(set, copy, cfg, plan)
  reads(plan%read) = .true.
  call advance_run_clock(copy)
 end do
 do i = 1, set%n
  name = trim(set%s(i)%name)
  if (name == 'input' .or. name == 'invariant' .or. name == 'lbc_in' .or. name == 'restart') cycle
  if (name == 'surface' .and. cfg%moist_physics) then
   ! the 'surface' stream is read for the surface update. Without config_sst_update MPAS would put the raw sst and xice
   ! into the model, with sea ice on cells that hold no sea-ice state, so lean stops instead.
   if (reads(i) .and. .not. cfg%config_sst_update) &
    call add_message(msgs, LOG_CRIT, 'The ''surface'' stream is read every '//trim(set%s(i)%raw_input)// &
     ', but config_sst_update = .false.: MPAS 8.4.2 would put its raw sst and xice into the model without the surface '// &
     'update. Set config_sst_update = .true., or input_interval = ''none'' on the ''surface'' stream.')
   cycle
  end if
  if (reads(i)) call add_message(msgs, LOG_WARN, 'Stream '''//name//''' is not read: lean_mpas reads no record '// &
                                 'of it in this run (it reads the input streams input, invariant, lbc_in, restart '// &
                                 'and, with physics, surface)')
 end do
 ! the surface-update alarm of MPAS is set from the input interval of the 'surface' stream (mpas_atmphys_manager.F:557-565)
 if (cfg%config_sst_update .and. cfg%moist_physics) then
  i = stream_index(set, 'surface')
  text = 'none'
  if (i > 0) text = trim(set%s(i)%raw_input)
  found = .false.
  if (i > 0) found = record_interval(set, clock, i, interval)
  if (.not. found) &
   call add_message(msgs, LOG_CRIT, 'config_sst_update = .true. needs an input_interval on the ''surface'' stream (it is '''// &
    trim(text)//'''): MPAS 8.4.2 sets its surface-update alarm from it (mpas_atmphys_manager.F:557-565).')
 end if
 end subroutine

 ! mpas_atm_bdy_checks (dynamics/mpas_atm_boundaries.F:775-874). After one of its ERR lines the run stops as
 ! MPAS's does: its line of mpas_atm_core.F:247-251 and the CRIT of driver/mpas_subdriver.F:393-396.
 subroutine bdy_checks(cfg, set, m, msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(dyn_mesh_type), intent(in) :: m
 type(message_list), intent(inout) :: msgs
 type(run_clock) :: no_clock
 type(ESMF_TimeInterval) :: interval
 character(len=:), allocatable :: text
 integer :: most
 logical :: failed
 call add_message(msgs, LOG_OUT, '')
 call add_message(msgs, LOG_OUT, 'Checking consistency of limited-area settings...')
 call add_message(msgs, LOG_OUT, ' - config_apply_lbcs = '//logical_text(cfg%config_apply_lbcs))
 most = maxval(m%bdyMaskCell(1:ncells))
 call add_message(msgs, LOG_OUT, ' - Maximum value in bdyMaskCell = '//int_text(most))
 failed = .false.
 if (.not. cfg%config_apply_lbcs .and. most > 0) then
  call add_message(msgs, LOG_ERR, 'Boundary cells found in the bdyMaskCell field, but config_apply_lbcs = false.')
  call add_message(msgs, LOG_ERR, 'Please ensure that config_apply_lbcs = true for limited-area simulations.')
  failed = .true.
 else if (cfg%config_apply_lbcs .and. most == 0) then
  call add_message(msgs, LOG_ERR, 'config_apply_lbcs = true, but no boundary cells found in the bdyMaskCell field.')
  call add_message(msgs, LOG_ERR, 'Please ensure that config_apply_lbcs = false for global simulations.')
  failed = .true.
 else if (cfg%config_apply_lbcs) then
  text = 'none'
  ! the stream's record_interval attribute, else its recurring input interval (lean_streams' record_interval; its
  ! clock argument only says whether alarms exist, and they do after the parse)
  no_clock%n_alarms = 1
  if (record_interval(set, no_clock, stream_index(set, 'lbc_in'), interval)) text = interval_string(interval)
  call add_message(msgs, LOG_OUT, ' - Input interval for ''lbc_in'' stream = '''//text//'''')
  if (text == 'none') then
   call add_message(msgs, LOG_ERR, 'Input interval for the ''lbc_in'' stream must be a valid interval '// &
                    'when config_apply_lbcs = true.')
   failed = .true.
  end if
 end if
 if (failed) then
  call add_message(msgs, LOG_ERR, 'Please correct issues with the model input fields and/or namelist.')
  call add_message(msgs, LOG_CRIT, 'Core init failed for core atmosphere')
  return
 end if
 call add_message(msgs, LOG_OUT, ' ----- done checking limited-area settings -----')
 call add_message(msgs, LOG_OUT, '')
 end subroutine
end module
