! Test-only stand-ins for the MPAS framework modules that unchanged MPAS source calls (the physics drivers of
! tests/sea_ice and tests/land_ice): a name -> pointer table instead of the Registry-generated pools, no logging
! machinery, no timers. The host registers its arrays by the names the original asks for;
! asking for a name that was not registered stops the program. Nothing here is lean code.
module mpas_derived_types
 use mpas_kind_types, only: RKIND, StrKIND
 implicit none
 integer, parameter :: MPAS_LOG_ERR = 1, MPAS_LOG_WARN = 2, MPAS_LOG_CRIT = 3
 integer, parameter :: pool_capacity = 160
 integer, parameter :: MPAS_NOW = 0, MPAS_POOL_SILENT = 0
 ! time stand-ins (whole seconds from 2026-10-11_00:00:00; enough for the lateral boundary reader of the original)
 integer, parameter :: MPAS_STREAM_LATEST_BEFORE = 1, MPAS_STREAM_EARLIEST_STRICTLY_AFTER = 2, MPAS_STREAM_MGR_NOERR = 0
 type MPAS_Time_type
  integer :: sec = 0
 end type
 type MPAS_TimeInterval_type
  integer :: sec = 0
 end type
 type MPAS_Clock_type
  type(MPAS_Time_type) :: now
 end type
 type MPAS_streamManager_type
  integer :: unused
 end type
 type domain_type
  integer :: unused
 end type
 type block_type
  type(domain_type), pointer :: domain => null()   ! a pointer in MPAS, so block%domain may be passed as inout
  type(mpas_pool_type), pointer :: structs => null()
 end type
 type pool_entry
  character(len=64) :: name = ''
  integer :: level = 0
  real(kind=RKIND), pointer :: r0 => null()
  real(kind=RKIND), pointer :: r1(:) => null()
  real(kind=RKIND), pointer :: r2(:,:) => null()
  real(kind=RKIND), pointer :: r3(:,:,:) => null()
  integer, pointer :: i0 => null()
  integer, pointer :: i1(:) => null()
  integer, pointer :: i2(:,:) => null()
  logical, pointer :: l0 => null()
  character(len=StrKIND), pointer :: c0 => null()
  type(mpas_pool_type), pointer :: sub => null()
 end type
 type mpas_pool_type
  integer :: n = 0
  type(pool_entry) :: e(pool_capacity)
 end type
end module

module mpas_pool_routines
 use mpas_derived_types
 implicit none
 interface mpas_pool_get_array
  module procedure get_r1, get_r2, get_r3, get_i0, get_i1, get_i2
 end interface
 interface mpas_pool_get_config
  module procedure get_config_r, get_config_l, get_config_c
 end interface
 interface pool_put
  module procedure put_r1, put_r2, put_r3, put_i1, put_i2
 end interface
contains
 integer function find(pool, name, level)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 integer, intent(in) :: level
 integer :: i
 find = 0
 do i = 1, pool%n
  if (trim(pool%e(i)%name) == name .and. pool%e(i)%level == level) then
   find = i
   return
  end if
 end do
 print *, 'pool stub: name not registered: ', name, level
 error stop 'pool stub: unexpected name'
 end function
 integer function slot(pool, name, level)
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 integer, intent(in) :: level
 pool%n = pool%n + 1
 if (pool%n > pool_capacity) error stop 'pool stub full'
 pool%e(pool%n)%name = name
 pool%e(pool%n)%level = level
 slot = pool%n
 end function

 subroutine put_r1(pool, name, a, level)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), target, intent(in) :: a(:)
 integer, intent(in), optional :: level
 integer :: lev
 lev = 0; if (present(level)) lev = level
 idx = slot(pool,name,lev); pool%e(idx)%r1 => a
 end subroutine
 subroutine put_r2(pool, name, a, level)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), target, intent(in) :: a(:,:)
 integer, intent(in), optional :: level
 integer :: lev
 lev = 0; if (present(level)) lev = level
 idx = slot(pool,name,lev); pool%e(idx)%r2 => a
 end subroutine
 subroutine put_r3(pool, name, a, level)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), target, intent(in) :: a(:,:,:)
 integer, intent(in), optional :: level
 integer :: lev
 lev = 0; if (present(level)) lev = level
 idx = slot(pool,name,lev); pool%e(idx)%r3 => a
 end subroutine
 subroutine put_i1(pool, name, a, level)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 integer, target, intent(in) :: a(:)
 integer, intent(in), optional :: level
 integer :: lev
 lev = 0; if (present(level)) lev = level
 idx = slot(pool,name,lev); pool%e(idx)%i1 => a
 end subroutine
 subroutine put_i2(pool, name, a, level)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 integer, target, intent(in) :: a(:,:)
 integer, intent(in), optional :: level
 integer :: lev
 lev = 0; if (present(level)) lev = level
 idx = slot(pool,name,lev); pool%e(idx)%i2 => a
 end subroutine
 ! mpas_pool_shift_time_levels for the arrays of a pool that has two levels: level 1 takes the data of level 2 (the real routine swaps the
 ! pointers, so level 2 keeps the old level 1; the caller overwrites level 2 completely or it holds zeros - the lateral boundary reader).
 subroutine mpas_pool_shift_time_levels(pool)
 type(mpas_pool_type), intent(inout) :: pool
 integer :: i, j
 do i = 1, pool%n
  if (pool%e(i)%level /= 2) cycle
  do j = 1, pool%n
   if (pool%e(j)%level == 1 .and. trim(pool%e(j)%name) == trim(pool%e(i)%name)) then
    if (associated(pool%e(i)%r2)) pool%e(j)%r2 = pool%e(i)%r2
    if (associated(pool%e(i)%r3)) pool%e(j)%r3 = pool%e(i)%r3
   end if
  end do
 end do
 end subroutine
 integer function mpas_pool_get_error_level()
 mpas_pool_get_error_level = 0
 end function
 subroutine mpas_pool_set_error_level(level)
 integer, intent(in) :: level
 end subroutine
 subroutine pool_put_dimension(pool, name, value)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 integer, target, intent(in) :: value
 idx = slot(pool,name,0); pool%e(idx)%i0 => value
 end subroutine
 subroutine pool_put_real(pool, name, value)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), target, intent(in) :: value
 idx = slot(pool,name,0); pool%e(idx)%r0 => value
 end subroutine
 subroutine pool_put_logical(pool, name, value)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 logical, target, intent(in) :: value
 idx = slot(pool,name,0); pool%e(idx)%l0 => value
 end subroutine
 subroutine pool_put_string(pool, name, value)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 character(len=StrKIND), target, intent(in) :: value
 idx = slot(pool,name,0); pool%e(idx)%c0 => value
 end subroutine
 subroutine pool_put_sub(pool, name, sub)
 integer :: idx
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 type(mpas_pool_type), target, intent(in) :: sub
 idx = slot(pool,name,0); pool%e(idx)%sub => sub
 end subroutine

 subroutine get_r1(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), pointer :: a(:)
 integer, intent(in), optional :: timeLevel
 integer :: lev
 lev = 0; if (present(timeLevel)) lev = timeLevel
 a => pool%e(find(pool,name,lev))%r1
 end subroutine
 subroutine get_r2(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), pointer :: a(:,:)
 integer, intent(in), optional :: timeLevel
 integer :: lev
 lev = 0; if (present(timeLevel)) lev = timeLevel
 a => pool%e(find(pool,name,lev))%r2
 end subroutine
 subroutine get_r3(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), pointer :: a(:,:,:)
 integer, intent(in), optional :: timeLevel
 integer :: lev
 lev = 0; if (present(timeLevel)) lev = timeLevel
 a => pool%e(find(pool,name,lev))%r3
 end subroutine
 subroutine get_i1(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 integer, pointer :: a(:)
 integer, intent(in), optional :: timeLevel
 integer :: lev
 lev = 0; if (present(timeLevel)) lev = timeLevel
 a => pool%e(find(pool,name,lev))%i1
 end subroutine
 subroutine get_i2(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 integer, pointer :: a(:,:)
 integer, intent(in), optional :: timeLevel
 integer :: lev
 lev = 0; if (present(timeLevel)) lev = timeLevel
 a => pool%e(find(pool,name,lev))%i2
 end subroutine
 subroutine get_i0(pool, name, a, timeLevel)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 integer, pointer :: a
 integer, intent(in), optional :: timeLevel
 a => pool%e(find(pool,name,0))%i0
 end subroutine
 subroutine get_config_c(pool, name, a)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 character(len=StrKIND), pointer :: a
 a => pool%e(find(pool,name,0))%c0
 end subroutine
 subroutine mpas_pool_get_dimension(pool, name, a)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 integer, pointer :: a
 a => pool%e(find(pool,name,0))%i0
 end subroutine
 subroutine get_config_r(pool, name, a)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), pointer :: a
 a => pool%e(find(pool,name,0))%r0
 end subroutine
 subroutine get_config_l(pool, name, a)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 logical, pointer :: a
 a => pool%e(find(pool,name,0))%l0
 end subroutine
 subroutine mpas_pool_get_subpool(pool, name, sub)
 type(mpas_pool_type), intent(in) :: pool
 character(*), intent(in) :: name
 type(mpas_pool_type), pointer :: sub
 sub => pool%e(find(pool,name,0))%sub
 end subroutine
end module

module mpas_log
 implicit none
contains
 subroutine mpas_log_write(message, messageType, realArgs, intArgs, logicArgs)
 use mpas_kind_types, only: RKIND
 use mpas_derived_types, only: MPAS_LOG_ERR, MPAS_LOG_CRIT
 character(*), intent(in) :: message
 integer, intent(in), optional :: messageType
 real(kind=RKIND), intent(in), optional :: realArgs(:)
 integer, intent(in), optional :: intArgs(:)
 logical, intent(in), optional :: logicArgs(:)
 if (present(messageType)) then
  if (messageType == MPAS_LOG_ERR .or. messageType == MPAS_LOG_CRIT) then
   print *, trim(message)
   error stop 'original raised an error message'
  end if
 end if
 end subroutine
end module

module mpas_timer
 implicit none
contains
 subroutine mpas_timer_start(name)
 character(*), intent(in) :: name
 end subroutine
 subroutine mpas_timer_stop(name)
 character(*), intent(in) :: name
 end subroutine
end module

module mpas_vector_operations
 use mpas_kind_types, only: RKIND
 implicit none
contains
 real(kind=RKIND) function mpas_fix_periodicity(a, b, c)
 real(kind=RKIND), intent(in) :: a, b, c
 error stop 'planar periodicity is not part of the spherical configuration'
 mpas_fix_periodicity = 0.
 end function
end module

module mpas_timekeeping
 use mpas_derived_types
 implicit none
 interface operator(-)
  module procedure sub_t_t
 end interface
contains
 type(MPAS_TimeInterval_type) function sub_t_t(t1, t2)
 type(MPAS_Time_type), intent(in) :: t1, t2
 sub_t_t%sec = t1%sec - t2%sec
 end function
 type(MPAS_Time_type) function mpas_get_clock_time(clock, which, ierr)
 type(MPAS_Clock_type), intent(in) :: clock
 integer, intent(in) :: which
 integer, intent(out), optional :: ierr
 mpas_get_clock_time = clock%now
 if (present(ierr)) ierr = 0
 end function
 subroutine mpas_get_timeInterval(interval, DD, S, S_n, S_d, ierr)
 type(MPAS_TimeInterval_type), intent(in) :: interval
 integer, intent(out), optional :: DD, S, S_n, S_d, ierr
 if (interval%sec < 0) error stop 'time stand-in: negative interval'
 if (present(DD)) DD = interval%sec / 86400
 if (present(S)) S = interval%sec - (interval%sec / 86400) * 86400
 if (present(S_n)) S_n = 0
 if (present(S_d)) S_d = 1
 if (present(ierr)) ierr = 0
 end subroutine
 subroutine mpas_set_time(curr_time, dateTimeString, ierr)
 type(MPAS_Time_type), intent(out) :: curr_time
 character(*), intent(in) :: dateTimeString
 integer, intent(out), optional :: ierr
 integer :: y, mo, d, h, mi, s
 read(dateTimeString, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') y, mo, d, h, mi, s
 if (y /= 2026 .or. mo /= 10) error stop 'time stand-in: only 2026-10'
 curr_time%sec = (d - 11) * 86400 + h * 3600 + mi * 60 + s
 if (present(ierr)) ierr = 0
 end subroutine
 subroutine mpas_get_time(curr_time, dateTimeString, ierr)
 type(MPAS_Time_type), intent(in) :: curr_time
 character(*), intent(out) :: dateTimeString
 integer, intent(out), optional :: ierr
 integer :: d, rest
 d = curr_time%sec / 86400 + 11; rest = curr_time%sec - (d - 11) * 86400
 write(dateTimeString, '(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)') 2026, '-', 10, '-', d, '_', rest/3600, ':', (rest-(rest/3600)*3600)/60, ':', mod(rest,60)
 if (present(ierr)) ierr = 0
 end subroutine
end module
