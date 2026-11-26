! Serial stand-ins for MPAS's unchanged summarize_timestep and log expansion.
module mpas_derived_types
 use mpas_kind_types, only: RKIND
 implicit none
 integer, parameter :: MPAS_LOG_OUT = 1, MPAS_LOG_WARN = 2, MPAS_LOG_ERR = 3, MPAS_LOG_CRIT = 4
 type dm_info
  integer :: my_proc_id = 0
 end type
 type domain_type
  type(dm_info) :: dminfo
 end type
 type mpas_pool_type
  logical, pointer :: print_vel => null(), print_detailed => null(), print_sca => null()
  type(mpas_pool_type), pointer :: state => null(), diag => null(), mesh => null()
  real(kind=RKIND), pointer :: w(:,:) => null(), u(:,:) => null(), v(:,:) => null(), scalars(:,:,:) => null()
  real(kind=RKIND), pointer :: lat_cell(:) => null(), lon_cell(:) => null(), lat_edge(:) => null(), lon_edge(:) => null()
  integer, pointer :: ncells => null(), nedges => null(), nz => null(), nscalars => null(), cell_ids(:) => null()
 end type
 type block_type
  type(mpas_pool_type), pointer :: configs => null(), structs => null()
 end type
end module

module mpas_pool_routines
 use mpas_derived_types, only: mpas_pool_type, RKIND
 implicit none
 interface mpas_pool_get_array
  module procedure get_real1, get_real2, get_real3, get_int1
 end interface
contains
 subroutine mpas_pool_get_config(pool, name, value)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 logical, pointer, intent(out) :: value
 select case (name)
 case ('config_print_global_minmax_vel')
  value => pool%print_vel
 case ('config_print_detailed_minmax_vel')
  value => pool%print_detailed
 case ('config_print_global_minmax_sca')
  value => pool%print_sca
 case default
  error stop 'unknown config'
 end select
 end subroutine

 subroutine mpas_pool_get_subpool(pool, name, value)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 type(mpas_pool_type), pointer, intent(out) :: value
 select case (name)
 case ('state')
  value => pool%state
 case ('diag')
  value => pool%diag
 case ('mesh')
  value => pool%mesh
 case default
  error stop 'unknown subpool'
 end select
 end subroutine

 subroutine mpas_pool_get_dimension(pool, name, value)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 integer, pointer, intent(out) :: value
 select case (name)
 case ('nCellsSolve')
  value => pool%ncells
 case ('nEdgesSolve')
  value => pool%nedges
 case ('nVertLevels')
  value => pool%nz
 case ('num_scalars')
  value => pool%nscalars
 case default
  error stop 'unknown dimension'
 end select
 end subroutine

 subroutine get_real1(pool, name, value, timelevel)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 real(kind=RKIND), pointer, intent(out) :: value(:)
 integer, intent(in), optional :: timelevel
 select case (name)
 case ('latCell')
  value => pool%lat_cell
 case ('lonCell')
  value => pool%lon_cell
 case ('latEdge')
  value => pool%lat_edge
 case ('lonEdge')
  value => pool%lon_edge
 case default
  error stop 'unknown real1'
 end select
 end subroutine

 subroutine get_real2(pool, name, value, timelevel)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 real(kind=RKIND), pointer, intent(out) :: value(:,:)
 integer, intent(in), optional :: timelevel
 select case (name)
 case ('w')
  value => pool%w
 case ('u')
  value => pool%u
 case ('v')
  value => pool%v
 case default
  error stop 'unknown real2'
 end select
 end subroutine

 subroutine get_real3(pool, name, value, timelevel)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 real(kind=RKIND), pointer, intent(out) :: value(:,:,:)
 integer, intent(in), optional :: timelevel
 if (name /= 'scalars') error stop 'unknown real3'
 value => pool%scalars
 end subroutine

 subroutine get_int1(pool, name, value, timelevel)
 type(mpas_pool_type), intent(in) :: pool
 character(len=*), intent(in) :: name
 integer, pointer, intent(out) :: value(:)
 integer, intent(in), optional :: timelevel
 if (name /= 'indexToCellID') error stop 'unknown int1'
 value => pool%cell_ids
 end subroutine
end module

module mpas_dmpar
 use mpas_derived_types, only: dm_info, RKIND
 implicit none
contains
 subroutine mpas_dmpar_min_real(dminfo, value, global)
 type(dm_info), intent(in) :: dminfo
 real(kind=RKIND), intent(in) :: value
 real(kind=RKIND), intent(out) :: global
 global = value
 end subroutine

 subroutine mpas_dmpar_minattributes_real(dminfo, value, local, global)
 type(dm_info), intent(in) :: dminfo
 real(kind=RKIND), intent(in) :: value, local(:)
 real(kind=RKIND), intent(out) :: global(:)
 global = local
 end subroutine

 subroutine mpas_dmpar_max_real(dminfo, value, global)
 type(dm_info), intent(in) :: dminfo
 real(kind=RKIND), intent(in) :: value
 real(kind=RKIND), intent(out) :: global
 global = value
 end subroutine

 subroutine mpas_dmpar_maxattributes_real(dminfo, value, local, global)
 type(dm_info), intent(in) :: dminfo
 real(kind=RKIND), intent(in) :: value, local(:)
 real(kind=RKIND), intent(out) :: global(:)
 global = local
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

