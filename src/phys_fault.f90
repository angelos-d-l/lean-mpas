! The fatal conditions of the physics in the device versions (src/device). Code on the card cannot stop the program, so the
! statement that stops the CPU build (MYNN/GWDO errflg, the Noah-MP budget and geometry checks) becomes `call physics_fault(code)` there
! (tools/gen_device_physics.py; the RRTMG physics_error_fatal calls, tools/gen_device_radiation.py), and the host looks at the code after
! every physics stage and stops with the original message.
module lean_phys_fault
 implicit none
 private
 public :: physics_fault, physics_fault_reset, physics_fault_check
 integer, parameter :: nfaults = 38
 character(80), parameter :: fault_text(nfaults) = [character(80) :: &
  'MYNN PBL column', 'MYNN surface column', 'GWDO column', &
  'Error in ResistanceAboveCanopyMostMod.F90', 'Error: ZeroPlaneDisp problem in NoahMP LSM', &
  'Error in ResistanceBareGroundMostMod.F90', 'Error: Longwave radiation budget problem in NoahMP LSM', &
  'Error: Water budget problem in NoahMP LSM', 'Error: Solar radiation budget problem in NoahMP LSM', &
  'Error: Energy budget problem in NoahMP LSM', &
  'RRTMG LW: ice generalized effective size out of bounds', 'RRTMG LW: snow generalized effective size out of bounds', &
  'RRTMG SW: ice generalized effective size out of bounds', 'RRTMG SW: FDELTA less than 0.0', 'RRTMG SW: FDELTA greater than 1.0', &
  'RRTMG SW: snow generalized effective size out of bounds', 'RRTMG SW: snow extinction less than 0.0', &
  'RRTMG SW: snow SSA greater than 1.0', 'RRTMG SW: snow SSA less than 0.0', 'RRTMG SW: snow asymmetry greater than 1.0', &
  'RRTMG SW: snow asymmetry less than 0.0', 'LW layer capacity exceeded', &
  'MCICA_SUBCOL: KISSVEC SEED GENERATOR REQUIRES PMID FROM BOTTOM FOUR LAYERS.', 'RRTMG LW: liquid effective radius out of bounds', &
  'RRTMG SW: ice extinction less than 0.0', 'RRTMG SW: ice SSA greater than 1.0', 'RRTMG SW: ice SSA less than 0.0', &
  'RRTMG SW: ice asymmetry greater than 1.0', 'RRTMG SW: ice asymmetry less than 0.0', 'RRTMG SW: liquid effective radius out of bounds', &
  'RRTMG SW: liquid extinction less than 0.0', 'RRTMG SW: liquid SSA greater than 1.0', 'RRTMG SW: liquid SSA less than 0.0', &
  'RRTMG SW: liquid asymmetry greater than 1.0', 'RRTMG SW: liquid asymmetry less than 0.0', &
  'RRTMG: INFLAG = 1 option not available with McICA', 'RRTMG LW: ice radius too small', 'RRTMG: ice radius out of bounds']
 integer, save :: fault_code = 0
 !$acc declare create(fault_code)
 ! the one fatal check of the Noah sea-ice scheme (src/device/sea_ice_noah.f90, tools/gen_device_physics.py)
 integer, parameter, public :: sea_ice_fault = nfaults + 1
contains
 subroutine physics_fault(code)
 !$acc routine seq
 integer, intent(in) :: code
 !$acc atomic write
 fault_code = code
 end subroutine
 subroutine physics_fault_reset()
 fault_code = 0
 !$acc update device(fault_code)
 end subroutine
 ! on_card: the stage ran on the card (the host copy of the code is then read back); a stage on the host sets the host copy directly
 subroutine physics_fault_check(on_card, stage, it)
 logical, intent(in) :: on_card
 character(*), intent(in) :: stage
 integer, intent(in) :: it
 integer :: host_code
 host_code = fault_code
 if (on_card) then
  !$acc update self(fault_code)
 end if
 if (host_code /= 0) fault_code = host_code
 if (fault_code == 0) return
 write(0, '(a,i0,3a)') 'physics fault at step ', it, ' in stage ', trim(stage), ':'
 if (fault_code >= 1 .and. fault_code <= nfaults) error stop trim(fault_text(fault_code))
 if (fault_code == sea_ice_fault) error stop 'Physical snow depth is less than snow water equiv.'
 error stop 'physics fault with an unknown code'
 end subroutine
end module
