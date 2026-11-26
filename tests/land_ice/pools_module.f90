! The pools of the land-ice test (tests/land_ice/run.sh): the arrays and registrations generated from the MPAS Noah-MP
! driver by tests/land_ice/pools.py (pools.inc), and the stand-ins that driver needs. Nothing here is lean code.
module mpas_atmphys_manager   ! the date the original driver reads (mpas_atmphys_manager.F year, month, day, curr_julday)
 use mpas_kind_types, only: RKIND
 implicit none
 integer :: year, month, day
 real(kind=RKIND) :: curr_julday
end module

module land_ice_pools
 use mpas_kind_types, only: RKIND, StrKIND
 use mpas_derived_types, only: mpas_pool_type
 use mpas_pool_routines
 use NoahmpIOVarType, only: NoahmpIO_type
 implicit none
 integer, parameter :: ncol = 30, nlev = 45, nsoil = 4, nsnow = 3, nzsnow = 7, nscal = 8
 type pool_set
  type(mpas_pool_type) :: configs, mesh, state, diag, diag_physics, diag_physics_noahmp, output_noahmp, sfc_input
 end type
 logical, target :: do_restart = .false.
 character(len=StrKIND), target :: microp_scheme = 'mp_thompson', convection_scheme = 'cu_grell_freitas'
 interface differ
  module procedure differ_r, differ_i
 end interface
#include "pools.inc"
 subroutine clear_pools(s)
 type(pool_set), intent(inout) :: s
 s%configs%n = 0; s%mesh%n = 0; s%state%n = 0; s%diag%n = 0; s%diag_physics%n = 0; s%diag_physics_noahmp%n = 0
 s%output_noahmp%n = 0; s%sfc_input%n = 0
 call pool_put_logical(s%configs, 'config_do_restart', do_restart)
 call pool_put_string(s%configs, 'config_microp_scheme', microp_scheme)
 call pool_put_string(s%configs, 'config_convection_scheme', convection_scheme)
 end subroutine
 logical function differ_r(x, y)
 real(kind=RKIND), intent(in) :: x, y
 differ_r = transfer(x, 0) /= transfer(y, 0)
 end function
 logical function differ_i(x, y)
 integer, intent(in) :: x, y
 differ_i = x /= y
 end function
end module
