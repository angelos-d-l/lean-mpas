! lean's side of the clock checks (tests/streams/run.py):  clock_main CASE_FILE
! Runs the commands of a tests/streams/clock_cases file (time parsing, interval division, alarms, stream file names)
! with lean_clock and prints one line per result.
program clock_main
 use iso_fortran_env, only: output_unit
 use ESMF, only: ESMF_TimeIntervalGet
 use lean_clock
 use lean_messages, only: message_list, write_messages, has_fatal
 use lean_namelist, only: run_config
 use lean_namelist_options, only: read_records
 use mpas_kind_types, only: RKIND, I8KIND, StrKIND
 implicit none
 type(ESMF_Time) :: t
 type(ESMF_TimeInterval) :: iv
 type(message_list) :: msgs
 type(run_clock) :: clock
 type(run_config) :: cfg
 type(ESMF_Time) :: reference, when
 type(ESMF_TimeInterval) :: numerator, denominator, remainder
 character(len=StrKIND) :: path, line, command, arg, id, interval_text, ref_word, ref_text, num_text, den_text
 character(len=:), allocatable :: step_line
 character(len=StrKIND) :: template, when_text
 character(len=:), allocatable :: name
 real(kind=RKIND) :: dt, seconds_back
 integer :: unit, status, days, sn, sd, i, n, steps, nml_unit
 integer(kind=I8KIND) :: seconds, quotient
 logical :: ok, until_stop

 call get_command_argument(1, path)
 ok = parse_time('2026-11-08_00:00:00', clock%start, msgs)
 clock%now = clock%start
 clock%step = interval_from_dt(120.0_RKIND)
 ok = parse_interval('1_00:00:00', iv, msgs)
 clock%stop = clock%start + iv
 open(newunit=unit, file=trim(path), status='old', action='read')
 do
  read(unit, '(a)', iostat=status) line
  if (status /= 0) exit
  if (len_trim(line) == 0) cycle
  if (line(1:1) == '#' .or. line(1:1) == '=') cycle
  read(line, *) command
  arg = trim(adjustl(line(index(line, ' ')+1:)))
  msgs = message_list()
  select case (trim(command))
  case ('time')
   ok = parse_time(trim(arg), t, msgs)
   call write_messages(output_unit, msgs)
   if (ok) then
    print '(a)', 'time '//trim(arg)//' -> '//time_string(t)
   else
    print '(a)', 'time '//trim(arg)//' -> error'
   end if
  case ('interval')
   ok = parse_interval(trim(arg), iv, msgs)
   call write_messages(output_unit, msgs)
   if (.not. ok) then
    print '(a)', 'interval '//trim(arg)//' -> error'
   else if (interval_has_months(iv)) then
    print '(a,i0,1x,i0,a)', 'interval '//trim(arg)//' -> ', iv%YR, iv%MM, ' months'
   else
    call ESMF_TimeIntervalGet(iv, D=days, S_i8=seconds, Sn=sn, Sd=sd)
    if (sd == 0) sd = 1
    print '(a,i0,1x,i0,1x,i0,1x,i0)', &
          'interval '//trim(arg)//' -> '//interval_string(iv)//' ', days, seconds, sn, sd
   end if
  case ('dt')
   read(arg, *) dt
   iv = interval_from_dt(dt)
   call ESMF_TimeIntervalGet(iv, D=days, S_i8=seconds, Sn=sn, Sd=sd)
   if (sd == 0) sd = 1
   ok = interval_seconds(iv, seconds_back)
   if (.not. ok) stop 2
   print '(a,i0,1x,i0,1x,i0,1x,i0,1x,es16.8)', 'dt '//trim(arg)//' -> ', days, seconds, sn, sd, seconds_back
  case ('start')
   ok = parse_time(trim(arg), clock%start, msgs)
   clock%now = clock%start
   ok = parse_interval('1_00:00:00', iv, msgs)
   clock%stop = clock%start + iv
  case ('step')
   ok = parse_interval(trim(arg), clock%step, msgs)
  case ('dtstep')
   read(arg, *) dt
   clock%step = interval_from_dt(dt)
  case ('stop')
   ok = parse_time(trim(arg), clock%stop, msgs)
  case ('duration')
   ok = parse_interval(trim(arg), iv, msgs)
   clock%stop = clock%start + iv
  case ('alarm')
   read(arg, *) id, interval_text
   reference = clock%start
   if (index(arg, ' ref ') /= 0) then
    read(arg, *) id, interval_text, ref_word, ref_text
    ok = parse_time(trim(ref_text), reference, msgs)
   end if
   ok = add_stream_alarm(clock, trim(id), 'output', trim(interval_text), reference, msgs)
   call write_messages(output_unit, msgs)
  case ('run')
   until_stop = trim(arg) == 'stop'
   n = 0
   if (.not. until_stop) read(arg, *) n
   steps = 0
   do
    if (.not. until_stop) then
     if (steps == n) exit
    end if
    step_line = time_string(clock%now)
    do i = 1, clock%n_alarms
     if (alarm_ringing(clock, clock%alarm(i)%id)) step_line = step_line//' '//trim(clock%alarm(i)%id)
    end do
    print '(a)', step_line
    do i = 1, clock%n_alarms
     call reset_alarm(clock, clock%alarm(i)%id)
    end do
    if (until_stop) then
     if (is_stop_time(clock)) exit
    end if
    call advance_run_clock(clock)
    steps = steps + 1
   end do
   if (until_stop) print '(a,i0)', 'steps ', step_count(clock)
  case ('divide')
   read(arg, *) ref_text, num_text, den_text
   ok = parse_time(trim(ref_text), reference, msgs)
   ok = parse_interval(trim(num_text), numerator, msgs)
   ok = parse_interval(trim(den_text), denominator, msgs)
   call interval_division(reference, numerator, denominator, quotient, remainder)
   print '(a,i0,1x,a)', 'divide -> ', quotient, interval_string(remainder)
  case ('config')
   cfg = run_config()
   open(newunit=nml_unit, file=trim(path(:scan(path, '/', back=.true.)))//trim(arg), status='old')
   call read_records(nml_unit, cfg, msgs)
   close(nml_unit)
   if (has_fatal(msgs)) stop 2
   msgs = message_list()
   call clock_from_config(cfg, clock, msgs)
   call write_messages(output_unit, msgs)
   if (.not. has_fatal(msgs)) then
    print '(a,i0)', 'start '//time_string(clock%start)//' stop '//time_string(clock%stop)// &
                   ' step '//interval_string(clock%step)//' steps ', step_count(clock)
   end if
  case ('expand')
   read(arg, *) ref_text, template
   ok = parse_time(trim(ref_text), reference, msgs)
   ok = expand_template(reference, trim(template), name, msgs)
   call write_messages(output_unit, msgs)
   if (ok) print '(a)', 'expand -> '//name
  case ('filename')
   read(arg, *) ref_text, when_text, interval_text, template
   ok = parse_time(trim(ref_text), reference, msgs)
   ok = parse_time(trim(when_text), when, msgs)
   ok = parse_interval(trim(interval_text), iv, msgs)
   ok = build_stream_filename(reference, when, iv, trim(template), name, msgs)
   call write_messages(output_unit, msgs)
   if (ok) print '(a)', 'filename -> '//name
  case default
   print '(a)', 'unknown command '//trim(command)
   stop 2
  end select
  if (has_fatal(msgs) .and. command /= 'config') stop 1
 end do
 close(unit)
end program
