! Which schemes run, set once per run from the resolved namelist.
! A scheme that is 'off' is skipped where it is dispatched (pre_dynamics, phys_dev's stages, the time step); nothing is
! allocated differently, so the restart layout and the card's data do not depend on it. Only has_re and cloud_scheme are
! read inside kernels (the radiation column), so only those two have a card copy.
module schemes
 implicit none
 private
 public :: set_schemes
 public :: micro_on, conv_on, pbl_on, sfclayer_on, lsm_on, lw_on, sw_on, gwdo_on, moist_physics
 public :: cloud_scheme, has_re, CLOUD_FRACTION, CLOUD_INCIDENCE

 integer, parameter :: CLOUD_FRACTION = 1, CLOUD_INCIDENCE = 2
 logical, protected, save :: micro_on = .true., conv_on = .true., pbl_on = .true., sfclayer_on = .true., &
                             lsm_on = .true., lw_on = .true., sw_on = .true., gwdo_on = .true., moist_physics = .true.
 integer, protected, save :: cloud_scheme = CLOUD_FRACTION   ! config_radt_cld_scheme
 integer, protected, save :: has_re = 1                      ! has_reqc = has_reqi = has_reqs
 !$acc declare create(cloud_scheme, has_re)
 logical, save :: is_set = .false.

contains

 ! The nine resolved scheme names (the strings of resolve_suite) and config_microp_re. moist_physics is MPAS's
 ! (mpas_atmphys_control.F:310-318: the gwdo scheme does not count), has_re is MPAS's has_reqc/i/s
 ! (mpas_atmphys_manager.F:747-758: the radii of a microphysics scheme that has them, and both RRTMG schemes).
 subroutine set_schemes(microp, convection, pbl, sfclayer, lsm, lw, sw, cld, gwdo, microp_re)
 character(*), intent(in) :: microp, convection, pbl, sfclayer, lsm, lw, sw, cld, gwdo
 logical, intent(in) :: microp_re
 logical :: new_micro, new_conv, new_pbl, new_sfclayer, new_lsm, new_lw, new_sw, new_gwdo, new_moist
 integer :: new_cloud, new_re

 new_micro = trim(microp) /= 'off'
 new_conv = trim(convection) /= 'off'
 new_pbl = trim(pbl) /= 'off'
 new_sfclayer = trim(sfclayer) /= 'off'
 new_lsm = trim(lsm) /= 'off'
 new_lw = trim(lw) /= 'off'
 new_sw = trim(sw) /= 'off'
 new_gwdo = trim(gwdo) /= 'off'
 new_moist = new_micro .or. new_conv .or. new_lsm .or. new_pbl .or. new_lw .or. new_sw .or. new_sfclayer
 new_cloud = CLOUD_FRACTION
 if (trim(cld) == 'cld_incidence') new_cloud = CLOUD_INCIDENCE
 new_re = 0
 if (microp_re .and. (trim(microp) == 'mp_thompson' .or. trim(microp) == 'mp_thompson_aerosols') .and. &
     trim(lw) == 'rrtmg_lw' .and. trim(sw) == 'rrtmg_sw') new_re = 1
 if (is_set) then
  if ((new_micro .neqv. micro_on) .or. (new_conv .neqv. conv_on) .or. (new_pbl .neqv. pbl_on) .or. &
      (new_sfclayer .neqv. sfclayer_on) .or. (new_lsm .neqv. lsm_on) .or. (new_lw .neqv. lw_on) .or. &
      (new_sw .neqv. sw_on) .or. (new_gwdo .neqv. gwdo_on) .or. new_cloud /= cloud_scheme .or. new_re /= has_re) &
   error stop 'the scheme set is set once per run'
  return
 end if
 micro_on = new_micro
 conv_on = new_conv
 pbl_on = new_pbl
 sfclayer_on = new_sfclayer
 lsm_on = new_lsm
 lw_on = new_lw
 sw_on = new_sw
 gwdo_on = new_gwdo
 moist_physics = new_moist
 cloud_scheme = new_cloud
 has_re = new_re
 is_set = .true.
 !$acc update device(cloud_scheme, has_re)
 end subroutine

end module
