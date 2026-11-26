! lean_run_inputs inside an MPAS run directory of the greece_60km case:  run_inputs ROOT
! The working directory is tests/inputs/run.sh's build/inputs/run_greece: namelist.CASE and streams.CASE for each case,
! links to the init and boundary files and the tables. Each case asserts its WARN, ERR and CRIT lines exactly.
program run_inputs_check
 use iso_fortran_env, only: int32, int64
 use nc_check, only: check, finish
 use config
 use run_settings, only: radt_lw, radt_sw
 use lean_messages
 use lean_namelist, only: run_config, validate, read_namelist, resolve_suite, check_scheme_set
 use lean_clock
 use lean_streams
 use lean_inputs
 use lean_run_inputs
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use model_start, only: start_cold
 implicit none
 character(len=*), parameter :: t0 = '2026-11-08_00:00:00'
 character(len=*), parameter :: stops = 'lean_mpas stops: '
 character(len=*), parameter :: listed = ' input problem(s) listed above'
 character(len=32) :: child
 character(len=400) :: root
 character(len=600) :: want_text(40)
 integer :: want_level(40), nwant = 0
 type(message_list) :: msgs
 type(run_config) :: cfg
 type(stream_set) :: set
 type(run_clock) :: clock
 type(dyn_mesh_type) :: m, m0
 type(cold_dyn_type) :: dyn, dyn0
 type(run_inputs) :: run, run0
 type(lbc_request), allocatable :: q(:), q0(:)
 integer :: nq, nq0, i
 logical :: ok

 call get_command_argument(1, root)
 call get_command_argument(2, child)
 if(child=='sfc3')then
  call go('atmosphere',100,.false.)
  msgs%n=0
  cfg%config_noahmp_iopt_sfc=3
  call validate(cfg,'mpas',msgs)
  call want(LOG_ERR,'config_noahmp_iopt_sfc = 3: lean_mpas supports only 1 or 2; MPAS options 3 and 4 are not supported')
  call want(LOG_CRIT,'lean_mpas stops: 1 namelist problem(s) listed above; docs/namelist.md lists what lean_mpas does with each option')
  call verify('sfc3: validate')
  cfg%config_noahmp_iopt_sfc=2
  call validate(cfg,'mpas',msgs)
  call verify('sfc2: validate accepts 2')
  call finish('run_inputs sfc3')
  stop
 endif
 if(child=='no_lbcs')then
 ! no_lbcs: bdy_checks also after the input stop, for MPAS's limited-area check lines
 call go('no_lbcs', 100, .true.)
 call want_ice_warnings()
 call want(LOG_ERR, 'Boundary cells found in the bdyMaskCell field, but config_apply_lbcs = false.')
 call want(LOG_ERR, 'Please ensure that config_apply_lbcs = true for limited-area simulations.')
 call want(LOG_ERR, 'Please correct issues with the model input fields and/or namelist.')
 call want(LOG_CRIT, 'Core init failed for core atmosphere')
 call verify('no_lbcs: the limited-area check')

  call finish('run_inputs no_lbcs')
  stop
 endif
 if(child=='radt_none')then
  call go('radt_none',100,.false.)
  call want_ice_warnings()
  call verify('radt_none: accepted')
  call check('radt_none intervals',radt_lw==120..and.radt_sw==120.)
  call finish('run_inputs radt_none')
  stop
 endif


 ! default_streams
 call go('atmosphere', 100, .false.)
 open(10, file='lean.log', status='replace', action='write')
 call write_messages(10, msgs)
 close(10)
 call want_ice_warnings()
 call verify('default_streams')
 call list('default_streams', 100)
 nq0 = nq
 allocate(q0(nq0))
 do i = 1, nq0
  q0(i)%at_s = q(i)%at_s
  q0(i)%whence = q(i)%whence
  q0(i)%file = q(i)%file
  q0(i)%retest = q(i)%retest
 end do
 ok = nq == 3
 ! compared here, not in a contained function: nvfortran 25.3 reads q(k)%file wrongly there
 if (ok) ok = all(q(1:3)%at_s == [0_int64, 0_int64, 10800_int64]) .and. &
              all(q(1:3)%whence == [LBC_LATEST_BEFORE, LBC_EARLIEST_STRICTLY_AFTER, LBC_EARLIEST_STRICTLY_AFTER])
 if (ok) ok = q(1)%file == 'lbc.2026-11-08_00.00.00.nc' .and. q(1)%retest == 'lbc.2026-11-07_21.00.00.nc'
 if (ok) ok = q(2)%file == 'lbc.2026-11-08_00.00.00.nc' .and. q(2)%retest == 'lbc.2026-11-08_03.00.00.nc'
 if (ok) ok = q(3)%file == 'lbc.2026-11-08_03.00.00.nc' .and. q(3)%retest == 'lbc.2026-11-08_06.00.00.nc'
 call check('default_streams: the three requests', ok)
 if (.not. ok) then   ! what was asked, for the reader of a failed check
  print '(a,i0)', 'requests: ', nq
  do i = 1, nq
   print '(i0,1x,i0,1x,i0,4a)', i, q(i)%at_s, q(i)%whence, ' [', q(i)%file, '] [', q(i)%retest//']'
  end do
 end if
 ok = run%lbc%n == 3
 if (ok) ok = all(run%lbc%valid_s(1:3) == [0_int64, 10800_int64, 21600_int64]) .and. &
              all(run%lbc%at_s(1:3) == [0_int64, 0_int64, 10800_int64])
 call check('default_streams: valid_s 0, 10800, 21600', ok)
 call check('default_streams: mesh = input = greece_60km.init.nc', trim(set%mesh_file) == 'greece_60km.init.nc' .and. &
            run%cold%mesh_path == 'greece_60km.init.nc' .and. run%cold%init_path == 'greece_60km.init.nc')
 call check('default_streams: the start', run%st%kind == start_cold .and. run%st%year == 2026 .and. run%st%month == 11 .and. &
            run%st%day == 8 .and. run%st%hour == 0 .and. run%st%init_hour == 0 .and. run%st%xtime_offset_min == 0 .and. &
            run%st%lbc_origin_sec == 0)
 call check('default_streams: len_disp 60000, lean''s own Thompson tables', run%len_disp == 60000. .and. &
            .not. run%thompson_big_endian)
 m0 = m
 dyn0 = dyn
 run0 = run
 call log_window(80, 81)
 call log_window(245, 275)
 call log_window(319, 334)
 call go('atmosphere', 0, .false.)
 call want_ice_warnings()
 call verify('default_streams, 0 steps: no boundary stop')
 call list('default_streams, 0 steps', 0)
 ok = nq == 2 .and. run%lbc%n == 2
 if (ok) ok = same_request(q(1), q0(1)) .and. same_request(q(2), q0(2))
 call check('default_streams, 0 steps: the first two requests', ok)

 ! invariant
 call go('invariant', 100, .false.)
 call want(LOG_WARN, 'File greece_60km.invariant.nc does not contain a seekable xtime variable. '// &
           'Forcing a read of the first time record.')
 call want_ice_warnings()
 call verify('invariant')
 call check('invariant: the mesh from the invariant file, the state from the input stream''s', &
            trim(set%mesh_file) == 'greece_60km.invariant.nc' .and. run%cold%mesh_path == 'greece_60km.invariant.nc' .and. &
            run%cold%init_path == 'greece_60km.state.nc')
 call check('invariant: every array equals default_streams''', same_mesh(m, m0) .and. same_dyn(dyn, dyn0, .true.) .and. &
            same_inp(run%cold, run0%cold))

 ! late_reference: a recurring alarm always rings at the start (framework/mpas_timekeeping.F:1039)
 call go('late_reference', 100, .false.)
 call want_ice_warnings()
 call verify('late_reference')
 call list('late_reference', 100)
 ok = nq == nq0
 do i = 1, min(nq, nq0)
  if (.not. same_request(q(i), q0(i))) ok = .false.
 end do
 call check('late_reference: exactly default_streams'' requests', ok)

 ! final_only_lbc: MPAS's limited-area check stops the run (dynamics/mpas_atm_boundaries.F:861-865)
 call go('final_only_lbc', 100, .false.)
 call check('final_only_lbc: the interval line of the limited-area check', has_out(' - Input interval for ''lbc_in'' stream = ''none'''))
 call want_ice_warnings()
 call want(LOG_ERR, 'Input interval for the ''lbc_in'' stream must be a valid interval when config_apply_lbcs = true.')
 call want(LOG_ERR, 'Please correct issues with the model input fields and/or namelist.')
 call want(LOG_CRIT, 'Core init failed for core atmosphere')
 call verify('final_only_lbc: no boundary stop')

 ! final_only_record
 call go('final_only_record', 100, .false.)
 call want_ice_warnings()
 call want(LOG_ERR, 'lbc_in: the input alarm does not ring at the start time '//t0//' (input_interval = ''final_only''): '// &
           'lean_mpas needs a recurring input_interval')
 call want(LOG_CRIT, stops//'1'//listed)
 call verify('final_only_record: the boundary stop')

 ! half_hour_start
 call go('half_hour_start', 100, .false.)
 call want_ice_warnings()
 call verify('half_hour_start: accepted')
 call list('half_hour_start',100)
 call check('half_hour_start: steps 1, 1, 91', nq==3)
 if(nq==3)then
  call check('half_hour_start: request steps',all(q%step==[1,1,91]))
  call check('half_hour_start: files',q(1)%file=='v0630/lbc.2026-11-08_06.30.00.nc'.and. &
             q(2)%retest=='v0630/lbc.2026-11-08_09.30.00.nc')
 endif

 ! initial_0630
 call go('initial_0630', 100, .false.)
 call want_ice_warnings()
 call verify('initial_0630: accepted')
 call list('initial_0630',100)
 call check('initial_0630: request steps',nq==3)
 if(nq==3)call check('initial_0630: steps 1, 1, 91',all(q%step==[1,1,91]))

 ! initial_2400, initial_roll: an initial_time whose hour rolls over is a time for MPAS's time manager
 ! (framework/mpas_timekeeping.F:1143-1298) and is taken as that time: the day after, which is after the start and
 ! stops through the messages; or the start itself, which runs as default_streams
 call go('initial_2400', 100, .false.)
 call want_ice_warnings()
 call want(LOG_ERR, 'initial_2400.nc: initial_time = ''2026-11-08_24:00:00'' (2026-11-09_00:00:00) is after the start '// &
           'time '//t0)
 call want(LOG_CRIT, stops//'1'//listed)
 call verify('initial_2400: an ERR line and the input stop, no error stop')
 call go('initial_roll', 100, .false.)
 call want_ice_warnings()
 call verify('initial_roll')
 call check('initial_roll: the start descriptor of default_streams', run%st%kind == start_cold .and. &
            run%st%day == 8 .and. run%st%hour == 0 .and. run%st%init_hour == 0 .and. run%st%xtime_offset_min == 0 .and. &
            run%lbc%n == 3)

 ! missing_table
 call execute_command_line('mv OZONE_DAT.TBL OZONE_DAT.away')
 call go('atmosphere', 100, .false.)
 call execute_command_line('mv OZONE_DAT.away OZONE_DAT.TBL')
 call want_ice_warnings()
 call want(LOG_ERR, './OZONE_DAT.TBL is missing: lean_mpas reads MPAS''s physics tables from the run directory')
 call want(LOG_CRIT, stops//'1'//listed)
 call verify('missing_table: one missing-table line')

 ! past_lbc: 400 steps of 120 s need a record after 12 UTC
 call go('atmosphere', 400, .false.)
 call want_ice_warnings()
 call want(LOG_ERR, 'Could not read from ''lbc_in'' stream after the current date to update lateral boundary tendencies')
 call want(LOG_ERR, 'Failed to process LBC data at next time after 2026-11-08_12:00:00')
 call want(LOG_ERR, '    (time 2026-11-08_12:00:00; looked in lbc.2026-11-08_12.00.00.nc and lbc.2026-11-08_15.00.00.nc)')
 call want(LOG_CRIT, stops//'3'//listed)
 call verify('past_lbc: the boundary read error naming 2026-11-08_12:00:00')

 ! The invariant stream bootstraps the mesh; the restart stream still supplies the state.
 call go('restart_invariant', 100, .false.)
 call want(LOG_CRIT, 'fake_restart.nc is not a lean_mpas restart file (it has no global attribute lean_restart).')
 call verify('restart_invariant: the restart refusal with a separate mesh')

 ! missing_init
 call execute_command_line('mv greece_60km.init.nc greece_60km.init.away')
 call go('atmosphere', 100, .false.)
 call execute_command_line('mv greece_60km.init.away greece_60km.init.nc')
 ok = msgs%n > 1
 if (ok) ok = msgs%line(msgs%n)%level == LOG_CRIT .and. msgs%line(msgs%n)%text == &
              'Could not open input file ''greece_60km.init.nc'' to read mesh fields' .and. &
              msgs%line(msgs%n-1)%text == ' ** Attempting to bootstrap MPAS framework using stream: input'
 call check('missing_init: after the bootstrap line only the CRIT of the mesh read', ok)
 call want(LOG_CRIT, 'Could not open input file ''greece_60km.init.nc'' to read mesh fields')
 call verify('missing_init')

 ! missing_streams, missing_namelist
 msgs%n = 0
 call startup('namelist.atmosphere', 'streams.none', 0, cfg, set, clock, m, run, msgs)
 call want(LOG_ERR, 'Could not open file streams.none in read_and_broadcast')
 call want(LOG_CRIT, 'Initialization of streamInfo object failed for core atmosphere')
 call verify('missing_streams: the streams inquiry lines only')
 call startup('namelist.none', 'streams.atmosphere', 0, cfg, set, clock, m, run, msgs)
 call want(LOG_CRIT, 'Namelist file namelist.none does not exist.')
 call verify('missing_namelist: read_namelist''s CRIT only')

 call execute_command_line('rm -f lean.log')
 call finish('run_inputs')

contains

 include 'same.inc'

 ! The startup of atmosphere_model up to the inputs, without validate: the namelist, the streams inquiry, the packages,
 ! the clock, the mesh stream, the mesh read and the parse. It returns at the first fatal line.
 subroutine startup(namelist_path, streams_path, nsteps, cfg, set, clock, m, run, msgs)
 character(*), intent(in) :: namelist_path, streams_path
 integer, intent(in) :: nsteps
 type(run_config), intent(out) :: cfg
 type(stream_set), intent(out) :: set
 type(run_clock), intent(out) :: clock
 type(dyn_mesh_type), intent(out) :: m
 type(run_inputs), intent(out) :: run
 type(message_list), intent(inout) :: msgs
 call read_namelist(namelist_path, cfg, msgs)
 if (has_fatal(msgs)) return
 call streams_inquire(streams_path, set, msgs)
 if (has_fatal(msgs)) return
 call packages_when(cfg, set, msgs)
 call resolve_suite(cfg, msgs)
 if (has_fatal(msgs)) return
 ! the scheme combinations MPAS cannot run stop here too
 call check_scheme_set(cfg, msgs)
 if (has_fatal(msgs)) return
 call set_packages(cfg, set, msgs)
 call clock_from_config(cfg, clock, msgs)
 if (has_fatal(msgs)) return
 call streams_mesh(set, cfg, clock, msgs)
 if (has_fatal(msgs)) return
 call read_run_mesh(cfg, clock, nsteps, namelist_path, set, m, run, msgs)
 if (has_fatal(msgs)) return
 call streams_parse(set, cfg, clock, msgs)
 end subroutine

 ! One case: startup, then open_run_inputs and bdy_checks while nothing is fatal. also_bdy: bdy_checks even after
 ! open_run_inputs' stop (the mesh is read), for the lines MPAS's check gives in that state.
 subroutine go(name, nsteps, also_bdy)
 character(*), intent(in) :: name
 integer, intent(in) :: nsteps
 logical, intent(in) :: also_bdy
 logical :: started
 msgs%n = 0
 call startup('namelist.'//name, 'streams.'//name, nsteps, cfg, set, clock, m, run, msgs)
 started = .not. has_fatal(msgs)
 if (started) call open_run_inputs(cfg, set, clock, nsteps, m, dyn, run, msgs)
 if (started .and. (also_bdy .or. .not. has_fatal(msgs))) call bdy_checks(cfg, set, m, msgs)
 end subroutine

 ! The requests of the case just run, again from its streams and clock.
 subroutine list(name, nsteps)
 character(*), intent(in) :: name
 integer, intent(in) :: nsteps
 type(message_list) :: none
 call lbc_requests(cfg, set, clock, nsteps, q, nq, none)
 call check(name//': the requests are listed without a line', none%n == 0)
 end subroutine

 logical function same_request(a, b)
 type(lbc_request), intent(in) :: a, b
 same_request = a%at_s == b%at_s .and. a%whence == b%whence
 if (same_request) same_request = a%file == b%file .and. a%retest == b%retest
 end function

 logical function has_out(text)
 character(*), intent(in) :: text
 integer :: k
 has_out = .false.
 do k = 1, msgs%n
  if (msgs%line(k)%level == LOG_OUT .and. trim(msgs%line(k)%text) == text) has_out = .true.
 end do
 end function

 ! Lines first..last of the greece strict_12h log of MPAS 8.4.2 are a run of lines of lean.log (default_streams'
 ! messages as write_messages writes them); the timing of the input read is masked.
 subroutine log_window(first, last)
 integer, intent(in) :: first, last
 character(len=300), allocatable :: theirs(:), ours(:)
 character(len=300) :: line
 character(len=8) :: a, b
 integer :: unit, status, n, k, j, at
 logical :: found
 allocate(theirs(last-first+1), ours(2000))
 open(newunit=unit, file=trim(root)//'/data/cases/greece_60km/reference/strict_12h/log.atmosphere.0000.out', status='old', &
      action='read')
 do k = 1, last
  read(unit, '(a)', iostat=status) line
  if (status /= 0) exit
  if (k >= first) theirs(k-first+1) = masked(line)
 end do
 close(unit)
 n = 0
 open(newunit=unit, file='lean.log', status='old', action='read')
 do
  read(unit, '(a)', iostat=status) line
  if (status /= 0) exit
  n = n + 1
  ours(n) = masked(line)
 end do
 close(unit)
 found = .false.
 at = 0
 do k = 1, n - size(theirs) + 1
  if (ours(k) /= theirs(1)) cycle
  at = k
  found = .true.
  do j = 1, size(theirs)
   if (ours(k+j-1) /= theirs(j)) found = .false.
  end do
  if (found) exit
 end do
 write(a, '(i0)') first
 write(b, '(i0)') last
 call check('log_window: MPAS''s log lines '//trim(a)//'-'//trim(b)//' are lean''s', found)
 if (.not. found) then
  print '(a)', '   MPAS:'
  do j = 1, size(theirs)
   print '(a)', '    |'//trim(theirs(j))
  end do
  print '(a,i0,a)', '   lean (from its line ', max(at, 1), '):'
  do j = max(at, 1), min(n, max(at, 1) + size(theirs) + 5)
   print '(a)', '    |'//trim(ours(j))
  end do
 end if
 end subroutine

 function masked(line) result(text)
 character(*), intent(in) :: line
 character(len=300) :: text
 character(len=*), parameter :: timing = '  Timing for read of ''input'' stream:'
 text = line
 if (index(line, timing) == 1) text = timing
 end function

 subroutine want(level, text)
 integer, intent(in) :: level
 character(*), intent(in) :: text
 nwant = nwant + 1
 want_level(nwant) = level
 want_text(nwant) = text
 end subroutine

 subroutine want_ice_warnings()
 call want(LOG_WARN, 'Variable qi not in input file.')
 call want(LOG_WARN, 'Variable qs not in input file.')
 call want(LOG_WARN, 'Variable qg not in input file.')
 call want(LOG_WARN, 'Variable ni not in input file.')
 call want(LOG_WARN, 'Variable nr not in input file.')
 end subroutine

 ! The WARN, ERR and CRIT lines of msgs are exactly the wanted ones, in order. Both lists are then emptied.
 subroutine verify(name)
 character(*), intent(in) :: name
 integer :: k, n
 logical :: fine
 n = 0
 fine = .true.
 do k = 1, msgs%n
  if (msgs%line(k)%level == LOG_OUT) cycle
  n = n + 1
  if (n > nwant) then
   fine = .false.
  else if (msgs%line(k)%level /= want_level(n) .or. trim(msgs%line(k)%text) /= trim(want_text(n))) then
   fine = .false.
  end if
 end do
 if (n /= nwant) fine = .false.
 call check(name//': the WARN, ERR and CRIT lines', fine)
 if (.not. fine) then
  print '(a)', '   got:'
  do k = 1, msgs%n
   if (msgs%line(k)%level /= LOG_OUT) print '(a,i0,a)', '    ', msgs%line(k)%level, ' '//trim(msgs%line(k)%text)
  end do
  print '(a)', '   wanted:'
  do k = 1, nwant
   print '(a,i0,a)', '    ', want_level(k), ' '//trim(want_text(k))
  end do
 end if
 msgs%n = 0
 nwant = 0
 end subroutine
end program
