! One Noah-MP time step of a column (NoahmpMain), after the per-step values that MPAS's input transfer routines set
! before it (the physics_noahmp/drivers/mpas files named *VarInTransferMod.F90).
module noahmp_step
 use noahmp_state
 use lean_noahmpmainmod,only: NoahmpMain
 implicit none
contains
 subroutine advance_noahmp(a)
 type(noahmp_column),intent(inout) :: a
 integer :: k
 ! The soil time step is the model time step (soiltstep 0), so the accumulators of the soil step start at zero every step.
 a%energy_flux_HeatGroundTotAcc=0.
 a%water_flux_EvapSoilSfcLiqAcc=0.
 a%water_flux_SoilSfcInflowAcc=0.
 a%water_flux_SfcWaterTotChgAcc=0.
 a%water_flux_PrecipTotAcc=0.
 a%water_flux_EvapCanopyNetAcc=0.
 a%water_flux_TranspirationAcc=0.
 a%water_flux_EvapGroundNetAcc=0.
 a%water_flux_TranspWatLossSoilAcc=0.
 a%water_state_RechargeGwDeepWT=0.
 ! Recompute the old ice fraction from the current layered snow.
 a%water_state_SnowIceFracPrev=0.
 do k=a%config_domain_NumSnowLayerNeg+1,0
  a%water_state_SnowIceFracPrev(k)=a%water_state_SnowIce(k) &
   /(a%water_state_SnowIce(k)+a%water_state_SnowLiqWater(k))
 enddo
 call NoahmpMain(a)
 end subroutine
end module
