! MPAS's side of the clock checks (tests/streams/run.py):  clock_oracle CASE_FILE
! The same commands as clock_main.f90 with MPAS's own mpas_timekeeping and mpas_build_stream_filename.
program clock_oracle
 use mpas_timekeeping
 use filename_oracle, only: mpas_build_stream_filename
 implicit none
 type(MPAS_Time_type) :: t
 type(MPAS_TimeInterval_type) :: iv
 type(MPAS_Clock_type) :: clock
 type(MPAS_Time_type) :: start_time, stop_time, now, reference, when
 type(MPAS_TimeInterval_type) :: time_step, numerator, denominator, remainder
 type(MPAS_Alarm_type), pointer :: alarm
 character(len=StrKIND) :: path, line, command, arg, text, id, interval_text, ref_word, ref_text, num_text, den_text
 character(len=:), allocatable :: step_line
 character(len=StrKIND) :: template, when_text, name
 real(kind=RKIND) :: dt, seconds_back
 integer :: unit, status, ierr, days, sn, sd, n, steps
 integer(kind=I8KIND) :: seconds, quotient
 logical :: ready, until_stop

 call mpas_timekeeping_init('gregorian')
 ready = .false.
 call mpas_set_time(start_time, dateTimeString='2026-11-08_00:00:00')
 call mpas_set_timeInterval(time_step, dt=120.0_RKIND)
 call mpas_set_timeInterval(iv, timeString='1_00:00:00')
 stop_time = start_time + iv
 call get_command_argument(1, path)
 open(newunit=unit, file=trim(path), status='old', action='read')
 do
  read(unit, '(a)', iostat=status) line
  if (status /= 0) exit
  if (len_trim(line) == 0) cycle
  if (line(1:1) == '#' .or. line(1:1) == '=') cycle
  read(line, *) command
  arg = trim(adjustl(line(index(line, ' ')+1:)))
  select case (trim(command))
  case ('time')
   call mpas_set_time(t, dateTimeString=trim(arg), ierr=ierr)
   if (ierr == 0) then
    call mpas_get_time(t, dateTimeString=text)
    print '(a)', 'time '//trim(arg)//' -> '//trim(text)
   else
    print '(a)', 'time '//trim(arg)//' -> error'
   end if
  case ('interval')
   call mpas_set_timeInterval(iv, timeString=trim(arg), ierr=ierr)
   if (ierr /= 0) then
    print '(a)', 'interval '//trim(arg)//' -> error'
   else if (iv%ti%YR /= 0 .or. iv%ti%MM /= 0) then
    print '(a,i0,1x,i0,a)', 'interval '//trim(arg)//' -> ', iv%ti%YR, iv%ti%MM, ' months'
   else
    call mpas_get_timeInterval(iv, DD=days, S_i8=seconds, S_n=sn, S_d=sd, timeString=text)
    print '(a,i0,1x,i0,1x,i0,1x,i0)', 'interval '//trim(arg)//' -> '//trim(text)//' ', days, seconds, sn, sd
   end if
  case ('dt')
   read(arg, *) dt
   call mpas_set_timeInterval(iv, dt=dt)
   call mpas_get_timeInterval(iv, DD=days, S_i8=seconds, S_n=sn, S_d=sd, dt=seconds_back)
   print '(a,i0,1x,i0,1x,i0,1x,i0,1x,es16.8)', 'dt '//trim(arg)//' -> ', days, seconds, sn, sd, seconds_back
  case ('start')
   call mpas_set_time(start_time, dateTimeString=trim(arg))
   call mpas_set_timeInterval(iv, timeString='1_00:00:00')
   stop_time = start_time + iv
  case ('step')
   call mpas_set_timeInterval(time_step, timeString=trim(arg))
  case ('dtstep')
   read(arg, *) dt
   call mpas_set_timeInterval(time_step, dt=dt)
  case ('stop')
   call mpas_set_time(stop_time, dateTimeString=trim(arg))
  case ('duration')
   call mpas_set_timeInterval(iv, timeString=trim(arg))
   stop_time = start_time + iv
  case ('alarm')
   call ensure_clock()
   read(arg, *) id, interval_text
   reference = start_time
   if (index(arg, ' ref ') /= 0) then
    read(arg, *) id, interval_text, ref_word, ref_text
    call mpas_set_time(reference, dateTimeString=trim(ref_text))
   end if
   ! framework/mpas_stream_manager.F:6242-6287, with the manager lookups supplied by the scenario.
   id = trim(id)//'_output'
   if (interval_text == 'none') cycle
   if (interval_text == 'final_only') then
    reference = stop_time
    call mpas_add_clock_alarm(clock, trim(id), stop_time)
   else if (interval_text == 'initial_only') then
    call mpas_add_clock_alarm(clock, trim(id), start_time)
   else
    call mpas_set_timeInterval(iv, timeString=trim(interval_text))
    call mpas_add_clock_alarm(clock, trim(id), start_time, alarmTimeInterval=iv)
    call mpas_adjust_alarm_to_reference_time(clock, trim(id), reference, ierr)
   end if
  case ('run')
   call ensure_clock()
   until_stop = trim(arg) == 'stop'
   n = 0
   if (.not. until_stop) read(arg, *) n
   steps = 0
   do
    if (.not. until_stop) then
     if (steps == n) exit
    end if
    now = mpas_get_clock_time(clock, MPAS_NOW)
    call mpas_get_time(now, dateTimeString=text)
    step_line = trim(text)
    alarm => clock%alarmListHead
    do while (associated(alarm))
     if (mpas_is_alarm_ringing(clock, alarm%alarmID)) step_line = step_line//' '//trim(alarm%alarmID)
     alarm => alarm%next
    end do
    print '(a)', step_line
    alarm => clock%alarmListHead
    do while (associated(alarm))
     call mpas_reset_clock_alarm(clock, alarm%alarmID)
     alarm => alarm%next
    end do
    if (until_stop) then
     if (mpas_is_clock_stop_time(clock)) exit
    end if
    call mpas_advance_clock(clock)
    steps = steps + 1
   end do
   if (until_stop) print '(a,i0)', 'steps ', steps
  case ('divide')
   read(arg, *) ref_text, num_text, den_text
   call mpas_set_time(reference, dateTimeString=trim(ref_text))
   call mpas_set_timeInterval(numerator, timeString=trim(num_text))
   call mpas_set_timeInterval(denominator, timeString=trim(den_text))
   call mpas_interval_division(reference, numerator, denominator, quotient, remainder)
   call mpas_get_timeInterval(remainder, timeString=text)
   print '(a,i0,1x,a)', 'divide -> ', quotient, trim(text)
  case ('expand')
   read(arg, *) ref_text, template
   call mpas_set_time(reference, dateTimeString=trim(ref_text))
   call mpas_get_time(reference, dateTimeString=text)
   call mpas_expand_string(trim(text), -1, trim(template), name)
   print '(a)', 'expand -> '//trim(name)
  case ('filename')
   read(arg, *) ref_text, when_text, interval_text, template
   call mpas_set_time(reference, dateTimeString=trim(ref_text))
   call mpas_set_time(when, dateTimeString=trim(when_text))
   call mpas_set_timeInterval(iv, timeString=trim(interval_text))
   call mpas_build_stream_filename(reference, when, iv, trim(template), -1, name, ierr)
   print '(a)', 'filename -> '//trim(name)
  case default
   print '(a)', 'unknown command '//trim(command)
   stop 2
  end select
 end do
 close(unit)
 if (ready) call mpas_destroy_clock(clock)
contains
 subroutine ensure_clock()
  if (ready) return
  call mpas_create_clock(clock, startTime=start_time, timeStep=time_step, stopTime=stop_time)
  ready = .true.
 end subroutine
end program
