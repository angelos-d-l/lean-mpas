! Serial stand-ins; the timekeeping, string utilities and log expansion are MPAS's own.
module mpas_derived_types
 use ESMF
 use mpas_kind_types
 implicit none
 integer, parameter :: MPAS_LOG_OUT = 1, MPAS_LOG_WARN = 2, MPAS_LOG_ERR = 3, MPAS_LOG_CRIT = 4
#include "mpas_timekeeping_types.inc"
 type MPAS_stream_list_type
  character(len=StrKIND) :: name
  type(MPAS_stream_list_type), pointer :: next => null()
 end type
end module

module mpas_dmpar
 implicit none
end module

module mpas_threading
 implicit none
contains
 integer function mpas_threading_get_thread_num()
  mpas_threading_get_thread_num = 0
 end function
end module

module mpas_log
 use iso_fortran_env, only: output_unit
 use mpas_kind_types, only: RKIND, I8KIND, StrKIND, ShortStrKIND
 use mpas_derived_types, only: MPAS_LOG_OUT, MPAS_LOG_WARN, MPAS_LOG_ERR, MPAS_LOG_CRIT
 implicit none
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
  character(len=StrKIND) :: expanded
  character(len=16) :: prefix
  integer :: level

  level = MPAS_LOG_OUT
  if (present(messageType)) level = messageType
  call log_expand_string(message, expanded, intArgs=intArgs, int8Args=int8Args, &
                         logicArgs=logicArgs, realArgs=realArgs)
  prefix = ''
  select case (level)
  case (MPAS_LOG_WARN)
   prefix = 'WARNING:'
  case (MPAS_LOG_ERR)
   prefix = 'ERROR:'
  case (MPAS_LOG_CRIT)
   prefix = 'CRITICAL ERROR:'
  end select
  ! framework/mpas_log.F:576, including embedded newlines and the leading blank.
  write(output_unit, '(a,a,a)') trim(prefix), ' ', trim(expanded)
  if (present(err)) err = 0
  if (level == MPAS_LOG_CRIT) stop 1
 end subroutine

#include "log_pieces.inc"
end module
