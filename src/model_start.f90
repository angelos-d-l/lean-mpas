! When the run starts: the start descriptor of a cold start or a lean restart and the times derived from it.
! The init_* time is the forecast's initial time (MPAS initial_time); a restart starts later than it.
module model_start
 use iso_fortran_env, only: int64
 implicit none
 private
 public :: start_info, start_cold, start_lean
 public :: parse_xtime, start_from_times, start_from_restart, start_xtime, day_number, civil_date
 integer, parameter :: start_cold = 1, start_lean = 2
 type start_info
  integer :: kind = 0
  integer :: year = 0, month = 0, day = 0, hour = 0   ! start of this run (UTC)
  integer :: minute = 0, second = 0
  integer :: init_minute = 0, init_second = 0
  integer :: init_hour = 0                             ! hour of the initial time: the radiation gmt
  integer :: julday = 0                                ! day of year of the start
  integer(int64) :: cold_offset_s = 0_int64               ! seconds from the cold start to the start of this run (0 on a cold start)
  integer :: lbc_origin_sec = 0                        ! seconds from 00 UTC of the start day to the start
  real :: xtime_offset_min = 0.                      ! minutes from the initial time to the start
 end type
contains
 ! The start descriptor from the start (y mo d h) and the initial time (iy im id ih), in UTC.
 subroutine start_from_times(kind, y, mo, d, h, mi, sec, iy, im, id, ih, imi, is, st)
 integer, intent(in) :: kind, y, mo, d, h, mi, sec, iy, im, id, ih, imi, is
 type(start_info), intent(out) :: st
 st%kind = kind
 st%year = y
 st%month = mo
 st%day = d
 st%hour = h
 st%minute = mi
 st%second = sec
 st%init_minute = imi
 st%init_second = is
 call check_date(st%year, st%month, st%day, st%hour)
 call check_date(iy, im, id, ih)
 st%init_hour = ih
 st%julday = day_number(st%year, st%month, st%day) - day_number(st%year, 1, 1) + 1
 st%lbc_origin_sec = 3600*st%hour + 60*mi + sec
 st%xtime_offset_min = 60*(24*(day_number(st%year, st%month, st%day) - day_number(iy, im, id)) + st%hour - ih) + mi - imi + real(sec-is)/60.
 if (st%xtime_offset_min < 0) error stop 'the start is before the initial time'
 end subroutine

 ! On a lean restart the radiation time base (init_hour, julday) stays the cold start's; MPAS resets it at a restart
 ! (physics/mpas_atmphys_manager.F:457-463). Keeping it makes a restarted run give the bytes of the uninterrupted one
 ! (docs/fixes.md, Restart). The boundary clock starts at this run.
 subroutine start_from_restart(cold_start, initial, start, st)
 character(*), intent(in) :: cold_start, initial, start
 type(start_info), intent(out) :: st
 integer :: y, mo, d, h, mi, sec, iy, imo, id, ih, imi, is
 integer :: cold_day, cold_sec
 logical :: ok

 call parse_xtime(cold_start,y,mo,d,h,mi,sec,ok)
 if(.not.ok)error stop 'invalid lean_cold_start in restart header'
 call parse_xtime(initial,iy,imo,id,ih,imi,is,ok)
 if(.not.ok)error stop 'invalid initial_time in restart header'
 call start_from_times(start_lean,y,mo,d,h,mi,sec,iy,imo,id,ih,imi,is,st)
 cold_day=day_number(y,mo,d)
 cold_sec=st%lbc_origin_sec
 call parse_xtime(start,st%year,st%month,st%day,st%hour,st%minute,st%second,ok)
 if(.not.ok)error stop 'invalid start time in restart header'
 st%lbc_origin_sec=3600*st%hour+60*st%minute+st%second
 st%cold_offset_s=86400_int64*(day_number(st%year,st%month,st%day)-cold_day)+st%lbc_origin_sec-cold_sec
 end subroutine

 real function start_xtime(st, step_start_s)
 type(start_info), intent(in) :: st
 integer(int64), intent(in) :: step_start_s
 start_xtime=real(st%xtime_offset_min)+real(st%cold_offset_s+step_start_s)/60.
 end function

 ! An MPAS time stamp, YYYY-MM-DD_hh:mm:ss: a real date and time of day, with only blanks after it.
 subroutine parse_xtime(text, y, mo, d, h, mi, s, ok)
 character(*), intent(in) :: text
 integer, intent(out) :: y, mo, d, h, mi, s
 logical, intent(out) :: ok
 integer :: i, y2, m2, d2
 y = 0; mo = 0; d = 0; h = 0; mi = 0; s = 0
 ok = .false.
 if (len_trim(text) /= 19) return
 do i = 1, 19
  select case (i)
  case (5, 8)
   if (text(i:i) /= '-') return
  case (11)
   if (text(i:i) /= '_') return
  case (14, 17)
   if (text(i:i) /= ':') return
  case default
   if (text(i:i) < '0' .or. text(i:i) > '9') return
  end select
 end do
 read(text(1:4), '(i4)') y
 read(text(6:7), '(i2)') mo
 read(text(9:10), '(i2)') d
 read(text(12:13), '(i2)') h
 read(text(15:16), '(i2)') mi
 read(text(18:19), '(i2)') s
 if (y < 1 .or. mo < 1 .or. mo > 12 .or. d < 1 .or. h > 23 .or. mi > 59 .or. s > 59) return
 call civil_date(day_number(y, mo, d), y2, m2, d2)
 ok = d2 == d
 end subroutine

 subroutine check_date(y, m, d, h)
 integer, intent(in) :: y, m, d, h
 integer :: y2, m2, d2
 if (y < 1 .or. m < 1 .or. m > 12 .or. d < 1 .or. h < 0 .or. h > 23) error stop 'start time: date or hour out of range'
 call civil_date(day_number(y, m, d), y2, m2, d2)
 if (d2 /= d) error stop 'start time: no such day in that month'
 end subroutine

 ! Gregorian day number (days since 0000-03-01) and its inverse.
 pure integer function day_number(y, m, d)
 integer, intent(in) :: y, m, d
 integer :: yy, era, yoe, doy
 yy = y
 if (m <= 2) yy = y - 1
 era = yy/400
 yoe = yy - era*400
 doy = (153*(m + merge(-3, 9, m > 2)) + 2)/5 + d - 1
 day_number = era*146097 + yoe*365 + yoe/4 - yoe/100 + doy
 end function

 pure subroutine civil_date(n, y, m, d)
 integer, intent(in) :: n
 integer, intent(out) :: y, m, d
 integer :: era, doe, yoe, doy, mp
 era = n/146097
 doe = n - era*146097
 yoe = (doe - doe/1460 + doe/36524 - doe/146096)/365
 doy = doe - (365*yoe + yoe/4 - yoe/100)
 mp = (5*doy + 2)/153
 d = doy - (153*mp + 2)/5 + 1
 m = mp + merge(3, -9, mp < 10)
 y = yoe + era*400 + merge(1, 0, m <= 2)
 end subroutine
end module
