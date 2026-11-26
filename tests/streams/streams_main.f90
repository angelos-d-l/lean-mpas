! lean's stream setup (tests/streams/run.py):  streams_main MODE NAMELIST STREAMS_FILE
! Reads the namelist and the streams file and sets up the streams, packages and I/O plan in the order of MPAS's
! driver; prints the messages and the resulting streams for comparison with MPAS's parser and logs.
program streams_main
 use lean_streams
 use lean_namelist
 use lean_clock
 use lean_messages
 use lean_stream_registry, only: N_PACKAGES, PACKAGE_NAME, FIELD_NAME
 implicit none
 type(run_clock) :: clock
 type(run_config) :: cfg
 type(stream_set) :: streams
 type(message_list) :: msgs, nml_msgs
 type(io_plan) :: plan
 type(ESMF_TimeInterval) :: interval
 character(len=512) :: mode, nml_path, path
 character(len=:), allocatable :: filename
 character(len=32), allocatable :: names(:)
 integer :: i, k
 call get_command_argument(1, mode)
 call get_command_argument(2, nml_path)
 call get_command_argument(3, path)
 call read_namelist(trim(nml_path), cfg, nml_msgs)
 if (has_fatal(nml_msgs)) then
  call write_messages(6, nml_msgs)
  stop 1
 end if
 ! lean_only_restart_run: a restart run of the same files.
 if (trim(mode) == 'loop-restart') cfg%config_do_restart = .true.
 ! The order of driver/mpas_subdriver.F:263-381.
 call streams_inquire(trim(path), streams, msgs)
 call packages_when(cfg, streams, msgs)
 if (.not. has_fatal(msgs)) call resolve_suite(cfg, msgs)
 call set_packages(cfg, streams, msgs)
 if (trim(mode) == 'packages') then
  call write_messages(6, msgs)
  if (has_fatal(msgs)) stop 1
  do i = 1, N_PACKAGES
   write(6, '(a)') '@ package '//trim(PACKAGE_NAME(i))//' '//logical_text(streams%package(i))
  end do
  stop
 end if
 if (trim(mode) /= 'startup') msgs = message_list()
 call clock_from_config(cfg, clock, msgs)
 call streams_mesh(streams, cfg, clock, msgs)
 ! The mesh read's place (framework/mpas_bootstrapping.F:164).
 if (trim(mode) == 'startup' .and. .not. has_fatal(msgs)) call add_message(msgs, LOG_OUT, &
  "Bootstrapping framework with mesh fields from input file '"//trim(streams%mesh_file)//"'")
 call streams_parse(streams, cfg, clock, msgs)
 select case (trim(mode))
 case ('startup')
  call write_messages(6, msgs)
 case ('parse', 'inspect')
  call write_messages(6, msgs)
  call dump_streams(6, streams)
  if (streams%mesh_template /= '') write(6, '(a)') '@ mesh status=0 template='//trim(streams%mesh_template)// &
   ' reference_time='//trim(streams%mesh_reference)//' filename_interval='//trim(streams%mesh_interval)// &
   ' io_type='//trim(streams%mesh_io)
  if (trim(mode) == 'inspect') write(6, '(a)') '@ mesh_file '//trim(streams%mesh_file)
  write(6, '(a,i0)') '@ parser status=', merge(1,0,has_fatal(msgs))
 case ('fields')
  if (has_fatal(msgs)) call write_messages(6, msgs)
  if (has_fatal(msgs)) stop 1
  do i = 1, streams%n
   call stream_fields(streams, i, names)
   write(6, '(a,i0)') '@ fields '//trim(streams%s(i)%name)//' '//logical_text(stream_active(streams, i))//' ', size(names)
   do k = 1, streams%s(i)%n_fields
    if (stream_has_field(streams, i, FIELD_NAME(streams%s(i)%field(k)%field))) &
     write(6, '(a)') '@ has '//trim(streams%s(i)%name)//' '//trim(FIELD_NAME(streams%s(i)%field(k)%field))
   end do
  end do
 case ('loop', 'loop-restart')
  if (has_fatal(msgs)) call write_messages(6, msgs)
  if (has_fatal(msgs)) stop 1
  ! core_atmosphere/mpas_atm_core.F: atm_core_init's reads and atm_core_run's loop, without the model.
  call plan_start(streams, clock, cfg, plan)
  write(6, '(a)') '@ initial '//trim(plan%initial_stream)
  if (plan%read_invariant) write(6, '(a)') '@ invariant'
  ! MPAS looks at these two alarms only after a step (mpas_atm_core.F:877-903): never set at the start.
  if (plan%diagnostics_rang) write(6, '(a)') '@ start diagnostics_rang'
  if (plan%restart_rang) write(6, '(a)') '@ start restart_rang'
  do i = 1, streams%n
   if (record_interval(streams, clock, i, interval)) then
    write(6, '(a)') '@ record_interval '//trim(streams%s(i)%name)//' '//interval_string(interval)
   else
    write(6, '(a)') '@ record_interval '//trim(streams%s(i)%name)//' none'
   end if
  end do
  write(6, '(a)') 'start '//time_string(clock%now)
  call show_outputs()
  if (plan%lbc_read) then
   write(6, '(a)') 'lbc first'
   if (read_filename(streams, stream_index(streams, 'lbc_in'), clock%now, filename, msgs)) &
    write(6, '(a)') '@ lbc_file '//filename
  end if
  do while (.not. is_stop_time(clock))
   write(6, '(a)') 'step '//time_string(clock%now)
   call plan_step_inputs(streams, clock, cfg, plan)
   if (plan%lbc_read) write(6, '(a)') 'lbc'
   do k = 1, size(plan%read_logged)
    write(6, '(a)') 'read '//trim(streams%s(plan%read_logged(k))%name)
   end do
   do k = 1, size(plan%read)
    write(6, '(a)') '@ reads '//trim(streams%s(plan%read(k))%name)
   end do
   call advance_run_clock(clock)
   write(6, '(a)') '@ now '//time_string(clock%now)
   call plan_step_outputs(streams, clock, plan)
   call show_outputs()
   if (plan%diagnostics_rang) write(6, '(a)') '@ diagnostics_rang'
   if (plan%restart_rang) write(6, '(a)') '@ restart_rang'
  end do
  if (has_fatal(msgs)) call write_messages(6, msgs)
 end select
 if (has_fatal(msgs)) stop 1
contains
 subroutine show_outputs()
  integer :: j
  do j = 1, size(plan%write)
   if (write_filename(streams, plan%write(j), clock%now, filename, msgs)) &
    write(6, '(a)') 'write '//trim(streams%s(plan%write(j))%name)//' '//filename
  end do
  if (plan%any_output) write(6, '(a)') 'output'
  do j = 1, size(plan%rang_output)
   write(6, '(a)') '@ rang '//trim(streams%s(plan%rang_output(j))%name)
  end do
 end subroutine
end program
