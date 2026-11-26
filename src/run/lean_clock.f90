! The run clock, its alarms, and time and interval strings, with the rules of MPAS 8.4.2 framework/mpas_timekeeping.F.
module lean_clock
 use ESMF
 use mpas_kind_types, only: RKIND, I8KIND, StrKIND, ShortStrKIND
 use lean_messages, only: message_list, add_message, int_text, real_text, LOG_WARN, LOG_ERR, LOG_CRIT
 use lean_namelist, only: run_config
 implicit none
 private
 public :: ESMF_Time, ESMF_TimeInterval
 public :: operator(+), operator(-), operator(==), operator(/=), operator(<), operator(>)
 public :: operator(<=), operator(>=), operator(*)
 public :: parse_time, parse_interval, interval_from_dt, time_string, interval_string
 public :: interval_has_months, interval_seconds
 public :: clock_alarm, run_clock, clock_from_config, advance_run_clock, is_stop_time, step_count
 public :: add_alarm, alarm_index, adjust_alarm_to_reference, alarm_ringing, reset_alarm, add_stream_alarm
 public :: elapsed_s, seconds_until, set_alarm_previous, alarm_previous
 public :: interval_division
 public :: expand_template, build_stream_filename

 type clock_alarm
  character(len=ShortStrKIND) :: id = ''
  logical :: recurring = .false., is_set = .false.
  type(ESMF_Time) :: ring_time, prev_ring_time
  type(ESMF_TimeInterval) :: interval
 end type

 type run_clock
  type(ESMF_Time) :: start, stop, now
  type(ESMF_TimeInterval) :: step
  type(clock_alarm), allocatable :: alarm(:)
  integer :: n_alarms = 0
 end type

contains

 subroutine initialize_time()
  if (.not. ESMF_IsInitialized()) call ESMF_Initialize(defaultCalendar=ESMF_CALKIND_GREGORIAN)
 end subroutine

 ! framework/mpas_string_utils.F:34-69: keep empty tokens and trim only the end.
 subroutine split_string(text, delimiter, parts)
  character(len=*), intent(in) :: text
  character, intent(in) :: delimiter
  character(len=StrKIND), allocatable, intent(out) :: parts(:)
  integer :: i, first, n

  n = 1
  do i = 1, len_trim(text)
   if (text(i:i) == delimiter) n = n + 1
  end do
  allocate(parts(n))
  first = 1
  n = 1
  do i = 1, len_trim(text)
   if (text(i:i) == delimiter) then
    parts(n) = text(first:i-1)
    n = n + 1
    first = i + 1
   end if
  end do
  parts(n) = text(first:len_trim(text))
 end subroutine

 ! framework/mpas_timekeeping.F:1143-1298. MPAS does not check its reads or the month; here an unreadable number or a
 ! month outside 1..12 is an error.
 logical function parse_time(text, t, msgs) result(ok)
  character(len=*), intent(in) :: text
  type(ESMF_Time), intent(out) :: t
  type(message_list), intent(inout) :: msgs
  character(len=StrKIND) :: whole, date, clock_text, fraction
  character(len=StrKIND), allocatable :: parts(:)
  integer :: numerator, denominator, power, status, i
  integer :: date_value(3), time_value(3)

  ok = .false.
  call initialize_time()
  whole = text
  numerator = 0
  denominator = 1
  call split_string(whole, '.', parts)
  if (size(parts) == 2) then
   whole = parts(1)
   fraction = parts(2)(:8)
   power = len_trim(fraction)
   if (power > 0) then
    read(fraction, *, iostat=status) numerator
    if (status /= 0) go to 900
    if (numerator > 0) denominator = 10**power
   end if
  else if (size(parts) /= 1) then
   call add_message(msgs, LOG_ERR, 'Invalid DateTime string '//text)
   return
  end if

  call split_string(whole, '_', parts)
  if (size(parts) == 2) then
   date = parts(1)
   clock_text = parts(2)
   call split_string(clock_text, ':', parts)
   if (size(parts) /= 3) then
    call add_message(msgs, LOG_ERR, 'Invalid DateTime string (invalid time substring) '//text)
    return
   end if
   time_value = 0
   do i = 1, 3
    read(parts(i), *, iostat=status) time_value(i)
    if (status /= 0) go to 900
   end do
  else if (size(parts) == 1) then
   date = parts(1)
   time_value = 0
  else
   call add_message(msgs, LOG_ERR, 'Invalid DateTime string'//text)
   return
  end if

  call split_string(date, '-', parts)
  if (size(parts) /= 3) then
   call add_message(msgs, LOG_ERR, 'Invalid DateTime string (invalid date substring) '//text)
   return
  end if
  date_value = 0
  do i = 1, 3
   read(parts(i), *, iostat=status) date_value(i)
   if (status /= 0) go to 900
  end do
  if (date_value(2) < 1 .or. date_value(2) > 12) then
   call add_message(msgs, LOG_ERR, 'Invalid DateTime string (month out of range) '//text)
   return
  end if
  call ESMF_TimeSet(t, YY=date_value(1), MM=date_value(2), DD=date_value(3), &
                    H=time_value(1), M=time_value(2), S=time_value(3), Sn=numerator, Sd=denominator, rc=status)
  ok = status == ESMF_SUCCESS
  return
900 call add_message(msgs, LOG_ERR, 'Invalid DateTime string (unreadable number) '//text)
 end function

 ! framework/mpas_timekeeping.F:1477-1591. As in MPAS, a date part with more than three '-' fields (Y-M-D) is not read
 ! and counts as zero days.
 logical function parse_interval(text, iv, msgs) result(ok)
  character(len=*), intent(in) :: text
  type(ESMF_TimeInterval), intent(out) :: iv
  type(message_list), intent(inout) :: msgs
  character(len=StrKIND) :: whole, date, clock_text, fraction
  character(len=StrKIND), allocatable :: parts(:)
  integer :: numerator, denominator, power, status, i, n
  integer :: date_value(3), time_value(2)
  integer(kind=I8KIND) :: seconds

  ok = .false.
  call initialize_time()
  whole = text
  numerator = 0
  denominator = 1
  call split_string(whole, '.', parts)
  if (size(parts) == 2) then
   whole = parts(1)
   fraction = parts(2)(:8)
   power = len_trim(fraction)
   if (power > 0) then
    read(fraction, *, iostat=status) numerator
    if (status /= 0) go to 900
    if (numerator > 0) denominator = 10**power
   end if
  else if (size(parts) /= 1) then
   call add_message(msgs, LOG_ERR, 'Invalid TimeInterval string '//trim(text))
   return
  end if

  date_value = 0
  call split_string(whole, '_', parts)
  if (size(parts) == 2) then
   date = parts(1)
   clock_text = parts(2)
   call split_string(date, '-', parts)
   n = size(parts)
   if (n <= 3) then
    do i = 1, n
     read(parts(i), *, iostat=status) date_value(3-n+i)
     if (status /= 0) go to 900
    end do
   end if
  else if (size(parts) == 1) then
   clock_text = parts(1)
  else
   call add_message(msgs, LOG_ERR, 'Invalid TimeInterval string '//trim(text))
   return
  end if

  call split_string(clock_text, ':', parts)
  n = size(parts)
  if (n > 3) then
   call add_message(msgs, LOG_ERR, 'Invalid TimeInterval string (invalid time substring) '//trim(text))
   return
  end if
  time_value = 0
  do i = 1, n-1
   read(parts(i), *, iostat=status) time_value(3-n+i)
   if (status /= 0) go to 900
  end do
  seconds = 0
  read(parts(n), *, iostat=status) seconds
  if (status /= 0) go to 900
  call ESMF_TimeIntervalSet(iv, YY=date_value(1), MM=date_value(2), D=date_value(3), &
                            H=time_value(1), M=time_value(2), S_i8=seconds, Sn=numerator, Sd=denominator, rc=status)
  ok = status == ESMF_SUCCESS
  return
900 call add_message(msgs, LOG_ERR, 'Invalid TimeInterval string (unreadable number) '//trim(text))
 end function

 ! framework/mpas_timekeeping.F:1452-1474: write RKIND dt to nine places, then keep eight.
 function interval_from_dt(dt) result(iv)
  real(kind=RKIND), intent(in) :: dt
  type(ESMF_TimeInterval) :: iv
  type(message_list) :: msgs
  character(len=StrKIND) :: text
  character(len=16) :: fmt
  integer :: width
  logical :: ok

  width = 11
  if (dt /= 0.0_RKIND) width = int(max(log10(abs(dt)), 0.0_RKIND)) + 11
  if (dt < 0.0_RKIND) width = width + 1
  write(fmt, '(a,i2.2,a)') '(a,f', width, '.09)'
  write(text, trim(fmt)) '00:00:', dt
  ok = parse_interval(trim(text), iv, msgs)
 end function

 function time_string(t) result(text)
  type(ESMF_Time), intent(in) :: t
  character(len=:), allocatable :: text
  character(len=StrKIND) :: buffer

  ! framework/mpas_timekeeping.F:1325: bundled ESMF writes whole seconds.
  call ESMF_TimeGet(t, timeString=buffer)
  text = trim(buffer)
 end function

 function interval_string(iv) result(text)
  type(ESMF_TimeInterval), intent(in) :: iv
  character(len=:), allocatable :: text
  character(len=StrKIND) :: buffer

  text = ''
  if (interval_has_months(iv)) return
  ! framework/mpas_timekeeping.F:1694: preserve ESMF's component representation.
  call ESMF_TimeIntervalGet(iv, timeString=buffer)
  text = trim(buffer)
 end function

 logical function interval_has_months(iv)
  type(ESMF_TimeInterval), intent(in) :: iv

  interval_has_months = iv%YR /= 0 .or. iv%MM /= 0
 end function

 logical function interval_seconds(iv, seconds) result(ok)
  type(ESMF_TimeInterval), intent(in) :: iv
  real(kind=RKIND), intent(out) :: seconds
  integer :: days, sn, sd
  integer(kind=I8KIND) :: whole_seconds

  seconds = 0.0_RKIND
  ok = .not. interval_has_months(iv)
  if (.not. ok) return
  ! framework/mpas_timekeeping.F:1626-1654, including the zero-denominator guard.
  call ESMF_TimeIntervalGet(iv, D=days, S_i8=whole_seconds, Sn=sn, Sd=sd)
  if (sd == 0) sd = 1
  seconds = real(days, RKIND)*24.0_RKIND*60.0_RKIND*60.0_RKIND + &
            real(whole_seconds, RKIND) + real(sn, RKIND)/real(sd, RKIND)
 end function

 ! core_atmosphere/mpas_atm_core.F:299-363, driver/mpas_subdriver.F:288-291. An unreadable restart timestamp file, a
 ! time step that is not positive and a stop time before the start stop the run here.
 subroutine clock_from_config(cfg, clock, msgs)
  type(run_config), intent(in) :: cfg
  type(run_clock), intent(out) :: clock
  type(message_list), intent(inout) :: msgs
  type(ESMF_Time) :: configured_stop
  type(ESMF_TimeInterval) :: duration
  character(len=StrKIND) :: start_text, reason
  integer :: unit, status

  start_text = cfg%config_start_time
  if (trim(start_text) == 'file') then
   open(newunit=unit, file=trim(cfg%config_restart_timestamp_name), form='formatted', &
        status='old', action='read', iostat=status, iomsg=reason)
   if (status == 0) then
    read(unit, *, iostat=status, iomsg=reason) start_text
    close(unit)
   end if
   if (status /= 0) then
    call add_message(msgs, LOG_ERR, 'Cannot read the start time from file '// &
                     trim(cfg%config_restart_timestamp_name)//': '//trim(reason))
    go to 900
   end if
  end if
  if (.not. parse_time(trim(start_text), clock%start, msgs)) go to 900
  if (cfg%config_dt <= 0.0_RKIND) then
   call add_message(msgs, LOG_ERR, 'config_dt = '//real_text(cfg%config_dt)//': the time step must be positive')
   go to 900
  end if
  clock%step = interval_from_dt(cfg%config_dt)
  if (trim(cfg%config_run_duration) /= 'none') then
   if (.not. parse_interval(trim(cfg%config_run_duration), duration, msgs)) go to 900
   clock%stop = clock%start + duration
   if (trim(cfg%config_stop_time) /= 'none') then
    if (.not. parse_time(trim(cfg%config_stop_time), configured_stop, msgs)) go to 900
    if (configured_stop /= clock%stop) then
     call add_message(msgs, LOG_WARN, &
                      'config_run_duration and config_stop_time are inconsistent: using config_run_duration.')
    end if
   end if
  else if (trim(cfg%config_stop_time) /= 'none') then
   if (.not. parse_time(trim(cfg%config_stop_time), clock%stop, msgs)) go to 900
  else
   call add_message(msgs, LOG_ERR, 'Neither config_run_duration nor config_stop_time were specified.')
   go to 900
  end if
  if (clock%stop < clock%start) then
   call add_message(msgs, LOG_ERR, 'the stop time '//time_string(clock%stop)// &
                    ' is before the start time '//time_string(clock%start))
   go to 900
  end if
  clock%now = clock%start
  return
900 call add_message(msgs, LOG_CRIT, 'Clock setup failed for core atmosphere')
 end subroutine

 ! core_atmosphere/mpas_atm_core.F:1026-1029: MPAS adds s_n/s_d in integer division, which is 0 for a proper fraction,
 ! so the elapsed time is whole seconds.
 integer(kind=I8KIND) function elapsed_s(clock)
  type(run_clock), intent(in) :: clock
  type(ESMF_TimeInterval) :: gap
  integer :: days
  integer(kind=I8KIND) :: seconds
  gap = clock%now - clock%start
  call ESMF_TimeIntervalGet(gap, D=days, S_i8=seconds)
  elapsed_s = int(days,I8KIND)*86400_I8KIND + seconds
 end function

 ! dynamics/mpas_atm_boundaries.F:493-499: time remaining includes the fractional second.
 real function seconds_until(clock,t)
  type(run_clock), intent(in) :: clock
  type(ESMF_Time), intent(in) :: t
  type(ESMF_TimeInterval) :: gap
  real(kind=RKIND) :: seconds
  logical :: ok
  gap = t - clock%now
  ok = interval_seconds(gap,seconds)
  if (.not.ok) error stop 'boundary interval has months'
  seconds_until = real(seconds)
 end function

 subroutine advance_run_clock(clock)
  type(run_clock), intent(inout) :: clock

  ! framework/mpas_timekeeping.F:397-426 (forward clock only).
  clock%now = clock%now + clock%step
 end subroutine

 logical function is_stop_time(clock)
  type(run_clock), intent(in) :: clock

  ! framework/mpas_timekeeping.F:280-302; setup rejects a backwards clock.
  is_stop_time = clock%now >= clock%stop
 end function

 integer function step_count(clock) result(n)
  type(run_clock), intent(in) :: clock
  type(run_clock) :: copy

  ! Count from the start, including a last step that overshoots the stop; no rounded dt division.
  copy%start = clock%start
  copy%stop = clock%stop
  copy%now = clock%start
  copy%step = clock%step
  n = 0
  do while (.not. is_stop_time(copy))
   call advance_run_clock(copy)
   n = n + 1
  end do
 end function

 integer function alarm_index(clock, id) result(index)
  type(run_clock), intent(in) :: clock
  character(len=*), intent(in) :: id
  integer :: i

  index = 0
  do i = 1, clock%n_alarms
   if (trim(clock%alarm(i)%id) /= trim(id)) cycle
   index = i
   return
  end do
 end function

 ! framework/mpas_timekeeping.F:490-566; allocation occurs only while constructing the clock.
 subroutine add_alarm(clock, id, ring_time, msgs, interval)
  type(run_clock), intent(inout) :: clock
  character(len=*), intent(in) :: id
  type(ESMF_Time), intent(in) :: ring_time
  type(message_list), intent(inout) :: msgs
  type(ESMF_TimeInterval), intent(in), optional :: interval
  type(clock_alarm), allocatable :: grown(:)
  integer :: i

  if (len_trim(id) > ShortStrKIND) then
   call add_message(msgs, LOG_CRIT, "Length of alarmID '"//trim(id)//"' is "//int_text(len_trim(id))// &
                    ' which is longer than the allowable length of '//int_text(ShortStrKIND))
   return
  end if
  if (alarm_index(clock, id) /= 0) then
   call add_message(msgs, LOG_ERR, 'OOPS -- we have a duplicate alarmID'//trim(id))
   return
  end if
  if (.not. allocated(clock%alarm)) allocate(clock%alarm(4))
  if (clock%n_alarms == size(clock%alarm)) then
   allocate(grown(2*clock%n_alarms))
   grown(:clock%n_alarms) = clock%alarm(:clock%n_alarms)
   call move_alloc(grown, clock%alarm)
  end if
  clock%n_alarms = clock%n_alarms + 1
  i = clock%n_alarms
  clock%alarm(i)%id = trim(id)
  clock%alarm(i)%is_set = .true.
  clock%alarm(i)%ring_time = ring_time
  clock%alarm(i)%recurring = present(interval)
  if (present(interval)) then
   clock%alarm(i)%interval = interval
   clock%alarm(i)%prev_ring_time = ring_time - interval
  else
   clock%alarm(i)%prev_ring_time = ring_time
  end if
 end subroutine

 subroutine set_alarm_previous(clock,id,previous)
  type(run_clock), intent(inout) :: clock
  character(*), intent(in) :: id
  type(ESMF_Time), intent(in) :: previous
  integer :: i
  i=alarm_index(clock,id)
  if(i>0)clock%alarm(i)%prev_ring_time=previous
 end subroutine

 type(ESMF_Time) function alarm_previous(clock,id) result(previous)
  type(run_clock), intent(in) :: clock
  character(*), intent(in) :: id
  integer :: i
  i=alarm_index(clock,id)
  previous=clock%start
  if(i>0)previous=clock%alarm(i)%prev_ring_time
 end function

 ! framework/mpas_timekeeping.F:814-905.
 logical function alarm_ringing(clock, id, within) result(ringing)
  type(run_clock), intent(in) :: clock
  character(len=*), intent(in) :: id
  type(ESMF_TimeInterval), intent(in), optional :: within
  type(ESMF_Time) :: threshold
  integer :: i

  threshold = clock%now
  if (present(within)) threshold = clock%now + within
  ringing = .false.
  i = alarm_index(clock, id)
  if (i == 0) return
  if (.not. clock%alarm(i)%is_set) return
  if (clock%alarm(i)%recurring) then
   ringing = clock%alarm(i)%prev_ring_time + clock%alarm(i)%interval <= threshold
  else
   ringing = clock%alarm(i)%ring_time <= threshold
  end if
 end function

 ! framework/mpas_timekeeping.F:927-985; retain the interval's phase after a late step.
 subroutine reset_alarm(clock, id)
  type(run_clock), intent(inout) :: clock
  character(len=*), intent(in) :: id
  type(ESMF_TimeInterval) :: remainder, elapsed
  integer(kind=I8KIND) :: n
  integer :: i

  if (.not. alarm_ringing(clock, id)) return
  i = alarm_index(clock, id)
  if (.not. clock%alarm(i)%recurring) then
   clock%alarm(i)%is_set = .false.
   return
  end if
  elapsed = clock%now - clock%alarm(i)%prev_ring_time
  call interval_division(clock%alarm(i)%prev_ring_time, elapsed, clock%alarm(i)%interval, n, remainder)
  clock%alarm(i)%prev_ring_time = clock%now - remainder
 end subroutine

 ! framework/mpas_timekeeping.F:992-1062: all recurring stream alarms ring at startup.
 subroutine adjust_alarm_to_reference(clock, id, reference)
  type(run_clock), intent(inout) :: clock
  character(len=*), intent(in) :: id
  type(ESMF_Time), intent(in) :: reference
  type(ESMF_TimeInterval) :: elapsed, remainder, zero
  integer(kind=I8KIND) :: n
  integer :: i

  i = alarm_index(clock, id)
  if (i == 0) return
  if (.not. clock%alarm(i)%recurring) return
  call ESMF_TimeIntervalSet(zero, S=0)
  if (clock%now > reference) then
   elapsed = clock%now - reference
   call interval_division(reference, elapsed, clock%alarm(i)%interval, n, remainder)
  else
   elapsed = reference - clock%now
   call interval_division(reference, elapsed, clock%alarm(i)%interval, n, remainder)
   if (remainder /= zero) remainder = clock%alarm(i)%interval - remainder
  end if
  clock%alarm(i)%prev_ring_time = clock%now - clock%alarm(i)%interval - remainder
 end subroutine

 ! framework/mpas_stream_manager.F:6198-6292; the streams parser names the stream when the interval is not valid.
 logical function add_stream_alarm(clock, stream, direction, interval_text, reference, msgs) result(ok)
  type(run_clock), intent(inout) :: clock
  character(len=*), intent(in) :: stream, direction, interval_text
  type(ESMF_Time), intent(inout) :: reference
  type(message_list), intent(inout) :: msgs
  type(ESMF_TimeInterval) :: interval
  character(len=:), allocatable :: id
  integer :: before

  ok = .true.
  if (trim(interval_text) == 'none') return
  id = trim(stream)//'_'//trim(direction)
  before = clock%n_alarms
  if (trim(interval_text) == 'final_only') then
   reference = clock%stop
   call add_alarm(clock, id, clock%stop, msgs)
  else if (trim(interval_text) == 'initial_only') then
   call add_alarm(clock, id, clock%start, msgs)
  else
   ok = parse_interval(interval_text, interval, msgs)
   if (.not. ok) return
   call add_alarm(clock, id, clock%start, msgs, interval)
   if (clock%n_alarms > before) call adjust_alarm_to_reference(clock, id, reference)
  end if
  ok = clock%n_alarms > before
 end function

 ! framework/mpas_timekeeping.F:1849-1932: preserve doubling/halving, including calendar intervals.
 subroutine interval_division(ref, num, den, n, remainder, msgs)
  type(ESMF_Time), intent(in) :: ref
  type(ESMF_TimeInterval), intent(in) :: num, den
  integer(kind=I8KIND), intent(out) :: n
  type(ESMF_TimeInterval), intent(out) :: remainder
  type(message_list), intent(inout), optional :: msgs
  integer(kind=I8KIND) :: m
  type(ESMF_Time) :: target, updated
  type(ESMF_TimeInterval) :: temp, zero

  call ESMF_TimeIntervalSet(zero, S=0)
  if (den == zero) then
   if (present(msgs)) then
    call add_message(msgs, LOG_WARN, 'mpas_interval_division: Attempting to divide by zero.')
   end if
   n = 0_I8KIND
   remainder = zero
   return
  end if
  target = ref + num
  updated = ref + den
  if (target == updated) then
   n = 1_I8KIND
   remainder = zero
   return
  end if
  if (target < updated) then
   n = 0_I8KIND
   remainder = num
   return
  end if
  n = 1_I8KIND
  do while (target > updated)
   n = n * 2_I8KIND
   temp = den * n
   updated = ref + temp
  end do
  n = n / 2_I8KIND
  m = n
  temp = den * n
  updated = ref + temp
  do while (m > 0_I8KIND)
   temp = den * m
   if (updated + temp <= target) then
    updated = updated + temp
    n = n + m
   end if
   m = m / 2_I8KIND
  end do
  remainder = target - updated
 end subroutine

 ! framework/mpas_timekeeping.F:2225-2324: '$$' restarts expansion; '$S' mutates second in place.
 logical function expand_template(t, template, name, msgs) result(ok)
  type(ESMF_Time), intent(in) :: t
  character(len=*), intent(in) :: template
  character(len=:), allocatable, intent(out) :: name
  type(message_list), intent(inout) :: msgs
  integer :: year, month, day, hour, minute, second, doy, i
  character(len=ShortStrKIND) :: part
  logical :: expanding

  ! Integer components discard fractions, just as MPAS's time_string/parse_time round trip does.
  call ESMF_TimeGet(t, YY=year, MM=month, DD=day, H=hour, M=minute, S=second, dayOfYear=doy)
  name = ''
  expanding = .false.
  ok = .false.
  do i = 1, len_trim(template)
   if (template(i:i) == '$') then
    expanding = .true.
   else if (expanding) then
    select case (template(i:i))
    case ('Y')
     write(part, '(i0.4)') year
    case ('M')
     write(part, '(i0.2)') month
    case ('D')
     write(part, '(i0.2)') day
    case ('d')
     write(part, '(i0.3)') doy
    case ('h')
     write(part, '(i0.2)') hour
    case ('m')
     write(part, '(i0.2)') minute
    case ('s')
     write(part, '(i0.2)') second
    case ('S')
     second = second + 60*minute + 3600*hour
     write(part, '(i0.5)') second
    case ('B')
     part = 'all'
    case default
     call add_message(msgs, LOG_CRIT, 'ERROR: mpas_expand_string option $'//template(i:i)// &
                      ' is not a valid expansion character.')
     return
    end select
    name = trim(name)//trim(part)
    expanding = .false.
   else
    name = name//template(i:i)
   end if
  end do
  name = trim(name)
  ok = .true.
 end function

 ! framework/mpas_stream_manager.F:4136-4201; calendar intervals use the same search as alarms.
 logical function build_stream_filename(ref, when, interval, template, name, msgs) result(ok)
  type(ESMF_Time), intent(in) :: ref, when
  type(ESMF_TimeInterval), intent(in) :: interval
  character(len=*), intent(in) :: template
  character(len=:), allocatable, intent(out) :: name
  type(message_list), intent(inout) :: msgs
  type(ESMF_Time) :: file_time
  type(ESMF_TimeInterval) :: elapsed, remainder, zero
  integer(kind=I8KIND) :: n

  if (when >= ref) then
   elapsed = when - ref
  else
   elapsed = ref - when
  end if
  call interval_division(ref, elapsed, interval, n, remainder, msgs)
  call ESMF_TimeIntervalSet(zero, S=0)
  file_time = when
  if (remainder /= zero) then
   if (when >= ref) then
    file_time = when - remainder
   else
    file_time = when + remainder
    file_time = file_time - interval
   end if
  end if
  ok = expand_template(file_time, template, name, msgs)
 end function

end module
