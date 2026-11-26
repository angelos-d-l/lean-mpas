! MPAS log files and message accounting: framework/mpas_log.F:380,433-443,576-630,687-706.
module lean_log
 use iso_fortran_env, only: error_unit
 use mpas_kind_types, only: RKIND, R8KIND, StrKIND
 use lean_messages, only: message_list, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT, real_text, trim_message
 implicit none
 private
 public :: log_open, log_write, log_messages, log_fatal, log_step_begin, log_step_time, log_finish
 public :: log_timer_start, log_timer_stop, timer_header, timer_row

 character(len=*), parameter, public :: LOG_FILE = 'log.atmosphere.0000.out'
 character(len=*), parameter, public :: ERR_FILE = 'log.atmosphere.0000.err'
 ! Fixed storage keeps timer restarts out of the allocator in the time loop.
 integer, parameter :: MAX_TIMERS = 128
 type timer_record
  character(len=StrKIND) :: name = ''
  integer :: parent = 0, calls = 0
  integer(8) :: started = 0
  real(kind=R8KIND) :: total = 0.0_R8KIND
  ! The unfinished-timer sentinels follow framework/mpas_timer.F:187-188 without an RKIND overflow.
  real(kind=R8KIND) :: tmin = real(huge(RKIND), R8KIND), tmax = -real(huge(RKIND), R8KIND)
  logical :: running = .false.
 end type
 type(timer_record) :: timers(MAX_TIMERS)
 integer :: timer_count = 0, running_count = 0, running(MAX_TIMERS)
 integer :: out_unit, err_unit, counts(4) = 0
 logical :: out_open = .false., err_open = .false.

contains

 subroutine log_open(status, text)
 integer, intent(out) :: status
 character(len=:), allocatable, intent(out) :: text
 character(len=StrKIND) :: iomsg
 integer :: stale_unit
 logical :: exists

 text = ''
 if (out_open) close(out_unit)
 if (err_open) close(err_unit)
 out_open = .false.
 err_open = .false.
 open(newunit=out_unit, file=LOG_FILE, status='replace', action='write', iostat=status, iomsg=iomsg)
 if (status /= 0) then
  text = trim_message(iomsg)
  return
 end if
 out_open = .true.
 counts = 0
 timer_count = 0
 running_count = 0
 timers = timer_record()
 call file_header(out_unit, 'Output')
 ! Departure: an old error log must not make a successful new run look failed.
 inquire(file=ERR_FILE, exist=exists)
 if (exists) then
  open(newunit=stale_unit, file=ERR_FILE, status='old', iostat=status, iomsg=iomsg)
  if (status == 0) close(stale_unit, status='delete', iostat=status, iomsg=iomsg)
  if (status /= 0) then
   text = trim_message(iomsg)
   close(out_unit)
   out_open = .false.
  end if
 end if
 end subroutine

 function date_stamp() result(text)
 character(len=19) :: text
 character(len=8) :: date
 character(len=10) :: time

 call date_and_time(date, time)
 text = date(1:4)//'/'//date(5:6)//'/'//date(7:8)//' '//time(1:2)//':'//time(3:4)//':'//time(5:6)
 end function

 subroutine put_line(unit, text)
 integer, intent(in) :: unit
 character(len=*), intent(in) :: text

 write(unit, '(a)') text
 flush(unit)
 end subroutine

 subroutine file_header(unit, kind)
 integer, intent(in) :: unit
 character(len=*), intent(in) :: kind

 ! Headers bypass the counters, as framework/mpas_log.F:433-443 does.
 call put_line(unit, repeat('-', 70))
 call put_line(unit, 'Beginning MPAS-atmosphere '//kind//' Log File for task       0 of       1')
 call put_line(unit, '    Opened at '//date_stamp())
 call put_line(unit, repeat('-', 70))
 call put_line(unit, '')
 end subroutine

 recursive subroutine log_write(text, level)
 character(len=*), intent(in) :: text
 integer, intent(in), optional :: level
 character(len=16) :: prefix
 character(len=StrKIND) :: iomsg
 integer :: kind, status

 kind = LOG_OUT
 if (present(level)) kind = level
 prefix = ''
 select case (kind)
 case (LOG_WARN)
  prefix = 'WARNING:'
 case (LOG_ERR)
  prefix = 'ERROR:'
 case (LOG_CRIT)
  prefix = 'CRITICAL ERROR:'
 case default
  kind = LOG_OUT
 end select
 call put_line(out_unit, trim(prefix)//' '//trim(text))
 counts(kind) = counts(kind) + 1
 ! MPAS opens the error log lazily and repeats errors there: mpas_log.F:610-630.
 if (kind == LOG_ERR .or. kind == LOG_CRIT) then
  if (.not. err_open) then
   open(newunit=err_unit, file=ERR_FILE, status='replace', action='write', iostat=status, iomsg=iomsg)
   if (status /= 0) then
    call close_output()
    write(error_unit, '(a)') 'lean_mpas: cannot open '//ERR_FILE//': '//trim_message(iomsg)
    error stop 1
   end if
   err_open = .true.
   call file_header(err_unit, 'Error')
  end if
  call put_line(err_unit, trim(prefix)//' '//trim(text))
 end if
 if (kind == LOG_CRIT) then
  ! mpas_log.F:839-859 closes both logs; departure: return failure instead of STOP's success.
  call close_output()
  call put_line(err_unit, 'Logging complete.  Closing file at '//date_stamp())
  close(err_unit)
  err_open = .false.
  write(error_unit, '(a)') 'lean_mpas stopped: '//trim(text)//' (see '//ERR_FILE//')'
  error stop 1
 end if
 end subroutine

 subroutine log_messages(list)
 type(message_list), intent(inout) :: list
 integer :: i

 do i = 1, list%n
  call log_write(trim(list%line(i)%text), list%line(i)%level)
 end do
 list%n = 0
 end subroutine

 subroutine log_fatal(text)
 character(len=*), intent(in) :: text

 call log_write(text, LOG_CRIT)
 end subroutine

 subroutine log_step_begin(stamp)
 character(len=*), intent(in) :: stamp

 ! core_atmosphere/mpas_atm_core.F:759-760.
 call log_write('')
 call log_write('Begin timestep '//trim(stamp))
 end subroutine

 subroutine log_step_time(seconds)
 real(kind=R8KIND), intent(in) :: seconds

 ! mpas_atm_core.F:819 converts the wall time to RKIND before $r expansion.
 call log_write(' Timing for integration step: '//real_text(real(seconds, RKIND))//' s')
 end subroutine

 subroutine log_timer_start(name)
 character(len=*), intent(in) :: name
 integer :: parent, timer, i

 ! Parent and name select a timer; calls count starts: framework/mpas_timer.F:136-200,218.
 parent = 0
 timer = 0
 if (timer_count > 0) then
  parent = 1
  if (running_count > 0) parent = running(running_count)
  do i = 2, timer_count
   if (timers(i)%parent /= parent) cycle
   if (trim(timers(i)%name) /= trim(name)) cycle
   timer = i
   exit
  end do
 end if
 if (timer == 0) then
  if (timer_count == MAX_TIMERS) call log_fatal('The log''s timer storage is full (MAX_TIMERS in src/run/lean_log.f90).')
  timer_count = timer_count + 1
  timer = timer_count
  timers(timer)%name = name
  timers(timer)%parent = parent
 end if
 timers(timer)%calls = timers(timer)%calls + 1
 timers(timer)%running = .true.
 call system_clock(count=timers(timer)%started)
 running_count = running_count + 1
 running(running_count) = timer
 end subroutine

 subroutine log_timer_stop(name, seconds)
 character(len=*), intent(in) :: name
 real(kind=R8KIND), intent(out), optional :: seconds
 integer(8) :: now, rate, limit, ticks
 real(kind=R8KIND) :: elapsed
 integer :: i, timer

 if (present(seconds)) seconds = 0.0_R8KIND
 ! Departure from mpas_timer.F:271-282, which stops only the most recently started timer and ends the run with a
 ! critical error when no timer runs or the name is another one: here the innermost running timer of that name
 ! stops, and a name that is not running changes nothing.
 do i = running_count, 1, -1
  timer = running(i)
  if (trim(timers(timer)%name) /= trim(name)) cycle
  call system_clock(count=now, count_rate=rate, count_max=limit)
  ticks = now - timers(timer)%started
  if (now < timers(timer)%started) ticks = (limit - timers(timer)%started) + now + 1_8
  elapsed = 0.0_R8KIND
  if (rate > 0) elapsed = real(ticks, R8KIND) / real(rate, R8KIND)
  timers(timer)%total = timers(timer)%total + elapsed
  timers(timer)%tmin = min(timers(timer)%tmin, elapsed)
  timers(timer)%tmax = max(timers(timer)%tmax, elapsed)
  timers(timer)%running = .false.
  running(i:running_count-1) = running(i+1:running_count)
  running_count = running_count - 1
  if (present(seconds)) seconds = elapsed
  return
 end do
 end subroutine

 function timer_header() result(text)
 character(len=:), allocatable :: text
 character(len=StrKIND) :: buffer

 ! framework/mpas_timer.F:346.
 write(buffer, '(3x, a10, 34x, a15, a12, a11, a15, a15, a13, a10, a12)') &
  'timer_name', 'total', 'calls', 'min', 'max', 'avg', 'pct_tot', 'pct_par', 'par_eff'
 text = trim(buffer)
 end function

 function timer_row(level, name, total, calls, tmin, tmax, tavg, pct_tot, pct_par, efficiency) result(text)
 integer, intent(in) :: level, calls
 character(len=*), intent(in) :: name
 real(kind=RKIND), intent(in) :: total, tmin, tmax, tavg, pct_tot, pct_par, efficiency
 character(len=:), allocatable :: text
 character(len=StrKIND) :: buffer
 character(len=45) :: indented

 indented = repeat(' ', max(0, level-1))//trim(name)
 ! framework/mpas_timer.F:449; a fixed-width name preserves its left justification.
 write(buffer, '(i2, 1x, a45, f15.5, i10, 3f15.5, 1x, f8.2, 3x, f8.2, 3x, f8.2)') &
  level, indented, total, calls, tmin, tmax, tavg, pct_tot, pct_par, efficiency
 text = trim(buffer)
 end function

 recursive subroutine print_timer(timer, level)
 integer, intent(in) :: timer, level
 real(kind=RKIND) :: average, pct_tot, pct_par
 integer :: parent, child

 if (timers(timer)%running) call log_write('Timer '//trim(timers(timer)%name)//' is still running.', LOG_ERR)
 average = real(timers(timer)%total / timers(timer)%calls, RKIND)
 pct_tot = 0.0_RKIND
 pct_par = 0.0_RKIND
 ! framework/mpas_timer.F:404,429-449. Zero parents, like zero roots, give zero percent.
 if (timers(1)%total /= 0.0_R8KIND) pct_tot = real(100.0_R8KIND * timers(timer)%total / timers(1)%total, RKIND)
 parent = timers(timer)%parent
 if (parent > 0) then
  if (timers(parent)%total /= 0.0_R8KIND) &
   pct_par = real(100.0_R8KIND * timers(timer)%total / timers(parent)%total, RKIND)
 end if
 call log_write(timer_row(level, trim(timers(timer)%name), real(timers(timer)%total, RKIND), timers(timer)%calls, &
  real(timers(timer)%tmin, RKIND), real(timers(timer)%tmax, RKIND), average, pct_tot, pct_par, 1.0_RKIND))
 do child = 2, timer_count
  if (timers(child)%parent == timer) call print_timer(child, level+1)
 end do
 end subroutine

 subroutine close_output()
 character(len=StrKIND) :: buffer
 integer :: output_count

 ! framework/mpas_log.F:687-706 takes the output count before printing these lines.
 output_count = counts(LOG_OUT)
 call log_write('')
 call log_write(repeat('-', 41))
 call log_write('Total log messages printed:')
 write(buffer, '(a,i12.1)') '   Output messages =         ', output_count
 call log_write(trim(buffer))
 write(buffer, '(a,i12.1)') '   Warning messages =        ', counts(LOG_WARN)
 call log_write(trim(buffer))
 write(buffer, '(a,i12.1)') '   Error messages =          ', counts(LOG_ERR)
 call log_write(trim(buffer))
 write(buffer, '(a,i12.1)') '   Critical error messages = ', counts(LOG_CRIT)
 call log_write(trim(buffer))
 call log_write(repeat('-', 41))
 call log_write('Logging complete.  Closing file at '//date_stamp())
 close(out_unit)
 out_open = .false.
 end subroutine

 subroutine log_finish()

 call log_timer_stop('total time')
 ! These are counted OUT messages, unlike the file header: mpas_timer.F:329-348.
 call log_write('')
 call log_write('')
 call log_write(' Timer information: ')
 call log_write('    Globals are computed across all threads and processors')
 call log_write('')
 call log_write(' Columns:')
 call log_write('    total time: Global max of accumulated time spent in timer')
 call log_write('    calls: Total number of times this timer was started / stopped.')
 call log_write('    min: Global min of time spent in a single start / stop')
 call log_write('    max: Global max of time spent in a single start / stop')
 call log_write('    avg: Global max of average time spent in a single start / stop')
 call log_write('    pct_tot: Percent of the timer at level 1')
 call log_write('    pct_par: Percent of the parent timer (one level up)')
 call log_write('    par_eff: Parallel efficiency, global average total time / global max total time')
 call log_write('')
 call log_write('')
 call log_write(timer_header())
 if (timer_count > 0) call print_timer(1, 1)
 call close_output()
 if (err_open) close(err_unit)
 err_open = .false.
 end subroutine

end module
