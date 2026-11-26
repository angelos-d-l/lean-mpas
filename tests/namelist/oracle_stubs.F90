! Serial stand-ins only; MPAS's generated reader and log expansion are unchanged.
module mpas_derived_types
 use mpas_kind_types, only: RKIND, StrKIND
 implicit none
 integer, parameter :: MPAS_LOG_OUT = 1, MPAS_LOG_WARN = 2, MPAS_LOG_ERR = 3, MPAS_LOG_CRIT = 4
 integer, parameter :: IO_NODE = 0

 type dm_info
  integer :: my_proc_id = 0
 end type

 type config_entry
  character(len=StrKIND) :: name = ''
  integer, pointer :: int_value => null()
  real(kind=RKIND), pointer :: real_value => null()
  logical, pointer :: logical_value => null()
  character(len=StrKIND), pointer :: char_value => null()
 end type

 type mpas_pool_type
  integer :: n = 0
  type(config_entry) :: entry(200)
 end type
end module

module mpas_io_units
 implicit none
end module

module mpas_dmpar
 use mpas_derived_types, only: dm_info, RKIND
 implicit none
contains
 subroutine mpas_dmpar_bcast_int(dminfo, value)
  type(dm_info), intent(in) :: dminfo
  integer, intent(inout) :: value
 end subroutine

 subroutine mpas_dmpar_bcast_real(dminfo, value)
  type(dm_info), intent(in) :: dminfo
  real(kind=RKIND), intent(inout) :: value
 end subroutine

 subroutine mpas_dmpar_bcast_logical(dminfo, value)
  type(dm_info), intent(in) :: dminfo
  logical, intent(inout) :: value
 end subroutine

 subroutine mpas_dmpar_bcast_char(dminfo, value)
  type(dm_info), intent(in) :: dminfo
  character(len=*), intent(inout) :: value
 end subroutine
end module

module mpas_log
 use mpas_kind_types, only: RKIND, I8KIND, StrKIND, ShortStrKIND
 use mpas_derived_types, only: MPAS_LOG_OUT, MPAS_LOG_WARN, MPAS_LOG_ERR, MPAS_LOG_CRIT
 implicit none
 integer :: log_unit
contains
 subroutine mpas_log_write(message, messageType, masterOnly, flushNow, &
                           intArgs, int8Args, realArgs, logicArgs, err)
  character(len=*), intent(in) :: message
  integer, intent(in), optional :: messageType
  logical, intent(in), optional :: masterOnly, flushNow
  integer, dimension(:), intent(in), optional :: intArgs
  integer(kind=I8KIND), dimension(:), intent(in), optional :: int8Args
  real(kind=RKIND), dimension(:), intent(in), optional :: realArgs
  logical, dimension(:), intent(in), optional :: logicArgs
  integer, intent(out), optional :: err
  character(len=StrKIND) :: messageExpanded
  character(len=16) :: messagePrefix
  integer :: messageTypeHere

  messageTypeHere = MPAS_LOG_OUT
  if (present(messageType)) messageTypeHere = messageType
  call log_expand_string(message, messageExpanded, intArgs=intArgs, int8Args=int8Args, &
                         logicArgs=logicArgs, realArgs=realArgs)
  messagePrefix = ''
  select case (messageTypeHere)
  case (MPAS_LOG_WARN)
   messagePrefix = 'WARNING:'
  case (MPAS_LOG_ERR)
   messagePrefix = 'ERROR:'
  case (MPAS_LOG_CRIT)
   messagePrefix = 'CRITICAL ERROR:'
  end select
  write(log_unit, '(a,a,a)') trim(messagePrefix), ' ', trim(messageExpanded)
  if (present(err)) err = 0
  if (present(flushNow)) then
   if (flushNow) flush(log_unit)
  end if
  if (messageTypeHere == MPAS_LOG_CRIT) then
   close(log_unit)
   error stop 1
  end if
 end subroutine

#include "log_pieces.inc"
end module

module mpas_abort
 use mpas_log, only: log_unit
 implicit none
contains
 subroutine mpas_dmpar_global_abort(mesg)
  character(len=*), intent(in) :: mesg

  write(log_unit, '(a)') 'ABORT: '//trim(mesg)
  close(log_unit)
  error stop 1
 end subroutine
end module

module mpas_pool_routines
 use mpas_derived_types, only: mpas_pool_type, RKIND, StrKIND
 implicit none
 private
 public :: mpas_pool_add_config, mpas_pool_get_config, mpas_pool_create_pool, mpas_pool_add_subpool, dump_pool
 interface mpas_pool_add_config
  module procedure add_int, add_real, add_logical, add_char
 end interface
 interface mpas_pool_get_config
  module procedure get_int, get_real, get_logical, get_char
 end interface
contains
 integer function add_entry(pool, name) result(i)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name

  if (pool%n == size(pool%entry)) error stop 'oracle pool full'
  pool%n = pool%n + 1
  i = pool%n
  pool%entry(i)%name = name
 end function

 integer function find_entry(pool, name) result(i)
  type(mpas_pool_type), intent(in) :: pool
  character(len=*), intent(in) :: name

  do i = 1, pool%n
   if (pool%entry(i)%name == name) return
  end do
  error stop 'oracle config name not found'
 end function

 subroutine add_int(pool, name, value)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name
  integer, intent(in) :: value
  integer :: i

  i = add_entry(pool, name)
  allocate(pool%entry(i)%int_value, source=value)
 end subroutine

 subroutine add_real(pool, name, value)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name
  real(kind=RKIND), intent(in) :: value
  integer :: i

  i = add_entry(pool, name)
  allocate(pool%entry(i)%real_value, source=value)
 end subroutine

 subroutine add_logical(pool, name, value)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name
  logical, intent(in) :: value
  integer :: i

  i = add_entry(pool, name)
  allocate(pool%entry(i)%logical_value, source=value)
 end subroutine

 subroutine add_char(pool, name, value)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name, value
  integer :: i

  i = add_entry(pool, name)
  allocate(pool%entry(i)%char_value)
  pool%entry(i)%char_value = value
 end subroutine

 subroutine get_int(pool, name, value)
  type(mpas_pool_type), intent(in) :: pool
  character(len=*), intent(in) :: name
  integer, pointer, intent(out) :: value

  value => pool%entry(find_entry(pool, name))%int_value
 end subroutine

 subroutine get_real(pool, name, value)
  type(mpas_pool_type), intent(in) :: pool
  character(len=*), intent(in) :: name
  real(kind=RKIND), pointer, intent(out) :: value

  value => pool%entry(find_entry(pool, name))%real_value
 end subroutine

 subroutine get_logical(pool, name, value)
  type(mpas_pool_type), intent(in) :: pool
  character(len=*), intent(in) :: name
  logical, pointer, intent(out) :: value

  value => pool%entry(find_entry(pool, name))%logical_value
 end subroutine

 subroutine get_char(pool, name, value)
  type(mpas_pool_type), intent(in) :: pool
  character(len=*), intent(in) :: name
  character(len=StrKIND), pointer, intent(out) :: value

  value => pool%entry(find_entry(pool, name))%char_value
 end subroutine

 subroutine mpas_pool_create_pool(pool)
  type(mpas_pool_type), pointer, intent(out) :: pool

  allocate(pool)
 end subroutine

 subroutine mpas_pool_add_subpool(pool, name, subpool)
  type(mpas_pool_type), intent(inout) :: pool
  character(len=*), intent(in) :: name
  type(mpas_pool_type), pointer, intent(in) :: subpool

  ! The atmosphere Registry has no in_subpool records; fail if that assumption changes.
  error stop 'unexpected oracle subpool'
 end subroutine

 subroutine dump_pool(unit, pool)
  integer, intent(in) :: unit
  type(mpas_pool_type), intent(in) :: pool
  character(len=32) :: value
  integer :: i

  do i = 1, pool%n
   if (associated(pool%entry(i)%char_value)) then
    write(unit, '(a)') trim(pool%entry(i)%name)//' = '//trim(pool%entry(i)%char_value)
    cycle
   else if (associated(pool%entry(i)%int_value)) then
    write(value, '(i17)') pool%entry(i)%int_value
   else if (associated(pool%entry(i)%real_value)) then
    write(value, '(ES15.8E2)') pool%entry(i)%real_value
   else if (associated(pool%entry(i)%logical_value)) then
    write(value, '(l1)') pool%entry(i)%logical_value
   else
    error stop 'oracle config has no storage'
   end if
   write(unit, '(a)') trim(pool%entry(i)%name)//' = '//trim(adjustl(value))
  end do
 end subroutine
end module
