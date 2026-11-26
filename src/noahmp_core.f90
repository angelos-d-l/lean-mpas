! Noah-MP of MPAS 8.4.2 (physics_noahmp) for 4 soil and 3 snow layers and only the options MPAS sets; each module below
! names the file it comes from.
! Noah-MP's terms (it may not be sold) are in licenses/Noah-MP-LICENSE.txt; see NOTICE.md.
! Source: physics_noahmp/src/ConstantDefineMod.F90
module lean_constantdefinemod
  use Machine
  implicit none
  private
  real(kind=kind_noahmp), public, parameter :: ConstGravityAcc = 9.80616
  real(kind=kind_noahmp), public, parameter :: ConstStefanBoltzmann = 5.67e-08
  real(kind=kind_noahmp), public, parameter :: ConstVonKarman = 0.40
  real(kind=kind_noahmp), public, parameter :: ConstFreezePoint = 273.16
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatSublim = 2.8440e06
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatEvap = 2.5104e06
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatFusion = 0.3336e06
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacWater = 4.188e06
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacIce = 2.094e06
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacAir = 1004.64
  real(kind=kind_noahmp), public, parameter :: ConstThermConductWater = 0.57
  real(kind=kind_noahmp), public, parameter :: ConstThermConductIce = 2.2
  real(kind=kind_noahmp), public, parameter :: ConstThermConductAir = 0.023
  real(kind=kind_noahmp), public, parameter :: ConstThermConductQuartz = 7.7
  real(kind=kind_noahmp), public, parameter :: ConstThermConductSoilOth = 2.0
  real(kind=kind_noahmp), public, parameter :: ConstGasDryAir = 287.04
  real(kind=kind_noahmp), public, parameter :: ConstGasWaterVapor = 461.269
  real(kind=kind_noahmp), public, parameter :: ConstDensityWater = 1000.0
  real(kind=kind_noahmp), public, parameter :: ConstDensityIce = 917.0
  real(kind=kind_noahmp), public, parameter :: ConstPI = 3.14159265
  real(kind=kind_noahmp), public, parameter :: ConstDensityGraupel = 500.0
  real(kind=kind_noahmp), public, parameter :: ConstDensityHail = 917.0
  public :: runtime_fourth_power
  contains
  ! x**4 evaluated at run time: after the snowmelt clamp the argument is the constant ConstFreezePoint, and a power the
  ! compiler folds can differ in the last bit from the one the original computes at run time.
  real(kind=kind_noahmp) function runtime_fourth_power(x)
    real(kind=kind_noahmp),intent(in) :: x
    runtime_fourth_power=x**4
  end function runtime_fourth_power
end module lean_constantdefinemod

! Source: physics_noahmp/src/AtmosForcingMod.F90
module lean_atmosforcingmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ProcessAtmosForcing(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    integer, parameter :: LoopNum = 10
    real(kind=kind_noahmp) :: PrecipFrozenTot
    real(kind=kind_noahmp) :: RadDirFrac
    real(kind=kind_noahmp) :: RadVisFrac
    real(kind=kind_noahmp) :: VapPresSat
    real(kind=kind_noahmp) :: LatHeatVap
    real(kind=kind_noahmp) :: PsychConst
    real(kind=kind_noahmp) :: TemperatureDegC
    real(kind=kind_noahmp) :: TemperatureWetBulb
    associate( CosSolarZenithAngle => noahmp%config_domain_CosSolarZenithAngle , OptRainSnowPartition => 1 , &
      & PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & PrecipConvRefHeight => noahmp%forcing_PrecipConvRefHeight , &
      & PrecipNonConvRefHeight => noahmp%forcing_PrecipNonConvRefHeight , &
      & PrecipShConvRefHeight => noahmp%forcing_PrecipShConvRefHeight , &
      & PrecipSnowRefHeight => noahmp%forcing_PrecipSnowRefHeight , &
      & PrecipGraupelRefHeight => noahmp%forcing_PrecipGraupelRefHeight , &
      & PrecipHailRefHeight => noahmp%forcing_PrecipHailRefHeight , &
      & RadSwDownRefHeight => noahmp%forcing_RadSwDownRefHeight , &
      & WindEastwardRefHeight => noahmp%forcing_WindEastwardRefHeight , &
      & WindNorthwardRefHeight => noahmp%forcing_WindNorthwardRefHeight , &
      & SnowfallDensityMax => noahmp%water_param_SnowfallDensityMax , &
      & TemperaturePotRefHeight => noahmp%energy_state_TemperaturePotRefHeight , &
      & PressureVaporRefHeight => noahmp%energy_state_PressureVaporRefHeight , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & RadSwDownDir => noahmp%energy_flux_RadSwDownDir , RadSwDownDif => noahmp%energy_flux_RadSwDownDif , &
      & RainfallRefHeight => noahmp%water_flux_RainfallRefHeight , &
      & SnowfallRefHeight => noahmp%water_flux_SnowfallRefHeight , &
      & PrecipTotRefHeight => noahmp%water_flux_PrecipTotRefHeight , &
      & PrecipConvTotRefHeight => noahmp%water_flux_PrecipConvTotRefHeight , &
      & PrecipLargeSclRefHeight => noahmp%water_flux_PrecipLargeSclRefHeight , &
      & PrecipAreaFrac => noahmp%water_state_PrecipAreaFrac , &
      & FrozenPrecipFrac => noahmp%water_state_FrozenPrecipFrac , &
      & SnowfallDensity => noahmp%water_state_SnowfallDensity )
    TemperaturePotRefHeight = TemperatureAirRefHeight * (PressureAirRefHeight / PressureAirRefHeight) ** (ConstGasDryAir / ConstHeatCapacAir)
    PressureVaporRefHeight = SpecHumidityRefHeight * PressureAirRefHeight / (0.622 + 0.378*SpecHumidityRefHeight)
    DensityAirRefHeight = (PressureAirRefHeight - 0.378*PressureVaporRefHeight) / (ConstGasDryAir * TemperatureAirRefHeight)
    RadDirFrac = 0.7
    RadVisFrac = 0.5
    if ( CosSolarZenithAngle <= 0.0 ) RadSwDownRefHeight = 0.0
    RadSwDownDir(1) = RadSwDownRefHeight * RadDirFrac * RadVisFrac
    RadSwDownDir(2) = RadSwDownRefHeight * RadDirFrac * (1.0-RadVisFrac)
    RadSwDownDif(1) = RadSwDownRefHeight * (1.0-RadDirFrac) * RadVisFrac
    RadSwDownDif(2) = RadSwDownRefHeight * (1.0-RadDirFrac) * (1.0-RadVisFrac)
#ifdef LEAN_NOAHMP_SWSPLIT
    ! Lean departure (build switch LEAN_NOAHMP_SWSPLIT, on in flavor lean, off in mpas): RRTMG's direct fraction instead of 70 %. Visible stays 50 %: RRTMG's UV/visible band ends at 0.625 um and Noah-MP's at about 0.7 um, so RRTMG's visible share (0.415 in clear sky) would move light Noah-MP counts as visible into its near-infrared band, where the surface albedo is higher, and raise the albedo only because the band edges differ. At night the lines above stay.
    if ( noahmp%sw_frac_dir_vis + noahmp%sw_frac_dir_nir + noahmp%sw_frac_dif_vis > 0.0 ) then
      RadDirFrac = noahmp%sw_frac_dir_vis + noahmp%sw_frac_dir_nir
      RadSwDownDir(1) = RadSwDownRefHeight * RadDirFrac       * RadVisFrac
      RadSwDownDir(2) = RadSwDownRefHeight * RadDirFrac       * (1.0-RadVisFrac)
      RadSwDownDif(1) = RadSwDownRefHeight * (1.0-RadDirFrac) * RadVisFrac
      RadSwDownDif(2) = RadSwDownRefHeight * (1.0-RadDirFrac) * (1.0-RadVisFrac)
    endif
#endif
    PrecipTotRefHeight = PrecipConvRefHeight + PrecipNonConvRefHeight + PrecipShConvRefHeight
    PrecipConvTotRefHeight = 0.10 * PrecipTotRefHeight
    PrecipLargeSclRefHeight = 0.90 * PrecipTotRefHeight
    PrecipAreaFrac = 0.0
    if ( (PrecipConvTotRefHeight+PrecipLargeSclRefHeight) > 0.0 ) then
      PrecipAreaFrac = (PrecipConvTotRefHeight + PrecipLargeSclRefHeight) / (10.0*PrecipConvTotRefHeight + PrecipLargeSclRefHeight)
    endif
    if ( TemperatureAirRefHeight > (ConstFreezePoint+2.5) ) then
      FrozenPrecipFrac = 0.0
    else
      if ( TemperatureAirRefHeight <= (ConstFreezePoint+0.5) ) then
        FrozenPrecipFrac = 1.0
      elseif ( TemperatureAirRefHeight <= (ConstFreezePoint+2.0) ) then
        FrozenPrecipFrac = 1.0 - (-54.632 + 0.2*TemperatureAirRefHeight)
      else
        FrozenPrecipFrac = 0.6
      endif
    endif
    SnowfallDensity = min( SnowfallDensityMax, 67.92+51.25*exp((TemperatureAirRefHeight-ConstFreezePoint)/2.59) )
    RainfallRefHeight = PrecipTotRefHeight * (1.0 - FrozenPrecipFrac)
    SnowfallRefHeight = PrecipTotRefHeight * FrozenPrecipFrac
    WindSpdRefHeight = max(sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0)
    end associate
  end subroutine ProcessAtmosForcing
end module lean_atmosforcingmod

! Source: physics_noahmp/src/GeneralInitMod.F90
module lean_generalinitmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine GeneralInit(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    associate( NumSoilLayer => 4 , DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & NumSoilLayerRoot => noahmp%water_param_NumSoilLayerRoot , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & DepthSnowSoilLayer => noahmp%config_domain_DepthSnowSoilLayer , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & TemperatureRootZone => noahmp%energy_state_TemperatureRootZone )
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( LoopInd == NumSnowLayerNeg+1 ) then
        ThicknessSnowSoilLayer(LoopInd) = - DepthSnowSoilLayer(LoopInd)
      else
        ThicknessSnowSoilLayer(LoopInd) = DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)
      endif
    enddo
    TemperatureRootZone = 0.0
    do LoopInd = 1, NumSoilLayerRoot
      TemperatureRootZone = TemperatureRootZone + TemperatureSoilSnow(LoopInd) * ThicknessSnowSoilLayer(LoopInd) / (-DepthSoilLayer(NumSoilLayerRoot))
    enddo
    end associate
  end subroutine GeneralInit
end module lean_generalinitmod

! Source: physics_noahmp/src/PhenologyMainMod.F90
module lean_phenologymainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine PhenologyMain (noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IntpMonth1,IntpMonth2
    real(kind=kind_noahmp) :: ThicknessCanBury
    real(kind=kind_noahmp) :: SnowDepthVegBury
    real(kind=kind_noahmp) :: DayCurrent
    real(kind=kind_noahmp) :: IntpWgt1,IntpWgt2
    real(kind=kind_noahmp) :: MonthCurrent
    associate( OptDynamicVeg => 4 , OptCropModel => 0 , VegType => noahmp%config_domain_VegType , &
      & CropType => noahmp%config_domain_CropType , IndexIcePoint => noahmp%config_domain_IndexIcePoint , &
      & IndexBarrenPoint => noahmp%config_domain_IndexBarrenPoint , &
      & IndexWaterPoint => noahmp%config_domain_IndexWaterPoint , FlagUrban => noahmp%config_domain_FlagUrban , &
      & FlagDynamicVeg => noahmp%config_domain_FlagDynamicVeg , &
      & FlagDynamicCrop => noahmp%config_domain_FlagDynamicCrop , Latitude => noahmp%config_domain_Latitude , &
      & NumDayInYear => noahmp%config_domain_NumDayInYear , &
      & DayJulianInYear => noahmp%config_domain_DayJulianInYear , &
      & HeightCanopyTop => noahmp%energy_param_HeightCanopyTop , &
      & HeightCanopyBot => noahmp%energy_param_HeightCanopyBot , &
      & LeafAreaIndexMon => noahmp%energy_param_LeafAreaIndexMon , &
      & StemAreaIndexMon => noahmp%energy_param_StemAreaIndexMon , &
      & VegFracAnnMax => noahmp%energy_param_VegFracAnnMax , VegFracGreen => noahmp%energy_param_VegFracGreen , &
      & TemperatureMinPhotosyn => noahmp%biochem_param_TemperatureMinPhotosyn , &
      & PlantGrowStage => noahmp%biochem_state_PlantGrowStage , SnowDepth => noahmp%water_state_SnowDepth , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & LeafAreaIndex => noahmp%energy_state_LeafAreaIndex , StemAreaIndex => noahmp%energy_state_StemAreaIndex , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , VegFrac => noahmp%energy_state_VegFrac , &
      & CanopyFracSnowBury => noahmp%energy_state_CanopyFracSnowBury , &
      & IndexGrowSeason => noahmp%biochem_state_IndexGrowSeason )
    if ( CropType == 0 ) then
      if ( Latitude >= 0.0 ) then
        DayCurrent = DayJulianInYear
      else
        DayCurrent = mod(DayJulianInYear+(0.5*NumDayInYear), real(NumDayInYear))
      endif
      MonthCurrent = 12.0 * DayCurrent / real(NumDayInYear)
      IntpMonth1 = MonthCurrent + 0.5
      IntpMonth2 = IntpMonth1 + 1
      IntpWgt1 = (IntpMonth1 + 0.5) - MonthCurrent
      IntpWgt2 = 1.0 - IntpWgt1
      if ( IntpMonth1 < 1 ) IntpMonth1 = 12
      if ( IntpMonth2 > 12 ) IntpMonth2 = 1
      LeafAreaIndex = IntpWgt1 * LeafAreaIndexMon(IntpMonth1) + IntpWgt2 * LeafAreaIndexMon(IntpMonth2)
      StemAreaIndex = IntpWgt1 * StemAreaIndexMon(IntpMonth1) + IntpWgt2 * StemAreaIndexMon(IntpMonth2)
      if ( StemAreaIndex < 0.05 ) StemAreaIndex = 0.0
      if ( (LeafAreaIndex < 0.05) .or. (StemAreaIndex == 0.0) ) LeafAreaIndex = 0.0
      if ( (VegType == IndexWaterPoint) .or. (VegType == IndexBarrenPoint) .or. (VegType == IndexIcePoint ) .or. (FlagUrban .eqv. .true.) ) then
        LeafAreaIndex = 0.0
        StemAreaIndex = 0.0
      endif
    endif
    ThicknessCanBury = min(max(SnowDepth-HeightCanopyBot,0.0), (HeightCanopyTop-HeightCanopyBot))
    CanopyFracSnowBury = ThicknessCanBury / max(1.0e-06, (HeightCanopyTop-HeightCanopyBot))
    if ( (HeightCanopyTop > 0.0) .and. (HeightCanopyTop <= 1.0) ) then
      SnowDepthVegBury = HeightCanopyTop * exp(-SnowDepth / 0.2)
      CanopyFracSnowBury = min(SnowDepth, SnowDepthVegBury) / SnowDepthVegBury
    endif
    LeafAreaIndEff = LeafAreaIndex * (1.0 - CanopyFracSnowBury)
    StemAreaIndEff = StemAreaIndex * (1.0 - CanopyFracSnowBury)
    if ( (StemAreaIndEff < 0.05) .and. (CropType == 0) ) StemAreaIndEff = 0.0
    if ( ((LeafAreaIndEff < 0.05) .or. (StemAreaIndEff == 0.0)) .and. (CropType == 0) ) LeafAreaIndEff = 0.0
    if ( ((TemperatureCanopy > TemperatureMinPhotosyn) .and. (CropType == 0)) .or. ((PlantGrowStage > 2) .and. (PlantGrowStage < 7) .and. (CropType > 0))) then
      IndexGrowSeason = 1.0
    else
      IndexGrowSeason = 0.0
    endif
    VegFrac = VegFracAnnMax
    if ( VegFrac <= 0.05 ) VegFrac = 0.05
    if ( (FlagUrban .eqv. .true.) .or. (VegType == IndexBarrenPoint) ) VegFrac = 0.0
    if ( (LeafAreaIndEff+StemAreaIndEff) == 0.0 ) VegFrac = 0.0
    FlagDynamicCrop = .false.
    FlagDynamicVeg = .false.
    end associate
  end subroutine PhenologyMain
end module lean_phenologymainmod

! Source: physics_noahmp/src/IrrigationTriggerMod.F90
module lean_irrigationtriggermod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine IrrigationTrigger(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    logical :: FlagIrri
    integer :: LoopInd
    real(kind=kind_noahmp) :: SoilMoistAvail
    real(kind=kind_noahmp) :: SoilMoistAvailMax
    real(kind=kind_noahmp) :: IrrigationWater
    associate( DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & DayJulianInYear => noahmp%config_domain_DayJulianInYear , OptIrrigation => 0 , OptIrrigationMethod => 0 , &
      & DatePlanting => noahmp%biochem_param_DatePlanting , DateHarvest => noahmp%biochem_param_DateHarvest , &
      & SoilMoistureWilt => noahmp%water_param_SoilMoistureWilt , &
      & SoilMoistureFieldCap => noahmp%water_param_SoilMoistureFieldCap , &
      & NumSoilLayerRoot => noahmp%water_param_NumSoilLayerRoot , &
      & IrriStopDayBfHarvest => noahmp%water_param_IrriStopDayBfHarvest , &
      & IrriTriggerLaiMin => noahmp%water_param_IrriTriggerLaiMin , &
      & SoilWatDeficitAllow => noahmp%water_param_SoilWatDeficitAllow , &
      & IrriFloodLossFrac => noahmp%water_param_IrriFloodLossFrac , VegFrac => noahmp%energy_state_VegFrac , &
      & LeafAreaIndex => noahmp%energy_state_LeafAreaIndex , &
      & IrrigationFracGrid => noahmp%water_state_IrrigationFracGrid , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & IrrigationFracMicro => noahmp%water_state_IrrigationFracMicro , &
      & IrrigationFracFlood => noahmp%water_state_IrrigationFracFlood , &
      & IrrigationFracSprinkler => noahmp%water_state_IrrigationFracSprinkler , &
      & IrrigationAmtMicro => noahmp%water_state_IrrigationAmtMicro , &
      & IrrigationAmtFlood => noahmp%water_state_IrrigationAmtFlood , &
      & IrrigationAmtSprinkler => noahmp%water_state_IrrigationAmtSprinkler , &
      & IrrigationCntSprinkler => noahmp%water_state_IrrigationCntSprinkler , &
      & IrrigationCntMicro => noahmp%water_state_IrrigationCntMicro , &
      & IrrigationCntFlood => noahmp%water_state_IrrigationCntFlood )
    FlagIrri = .true.
    FlagIrri = .false.
    if ( FlagIrri .eqv. .true. ) then
      SoilMoistAvail = 0.0
      SoilMoistAvailMax = 0.0
      SoilMoistAvail = (SoilLiqWater(1) - SoilMoistureWilt(1)) * (-1.0) * DepthSoilLayer(1)
      SoilMoistAvailMax = (SoilMoistureFieldCap(1) - SoilMoistureWilt(1)) * (-1.0) * DepthSoilLayer(1)
      do LoopInd = 2, NumSoilLayerRoot
        SoilMoistAvail = SoilMoistAvail + (SoilLiqWater(LoopInd) - SoilMoistureWilt(LoopInd)) * (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
        SoilMoistAvailMax = SoilMoistAvailMax + (SoilMoistureFieldCap(LoopInd) - SoilMoistureWilt(LoopInd)) * (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
      enddo
      if ( (SoilMoistAvail/SoilMoistAvailMax) <= SoilWatDeficitAllow ) then
        IrrigationWater = (SoilMoistAvailMax - SoilMoistAvail) * IrrigationFracGrid * VegFrac
        if ( (IrrigationAmtSprinkler == 0.0) .and. (IrrigationFracSprinkler > 0.0) .and. (OptIrrigationMethod == 0) ) then
          IrrigationAmtSprinkler = IrrigationFracSprinkler * IrrigationWater
          IrrigationCntSprinkler = IrrigationCntSprinkler + 1
        endif
        if ( (IrrigationAmtMicro == 0.0) .and. (IrrigationFracMicro > 0.0) .and. (OptIrrigationMethod == 0) ) then
          IrrigationAmtMicro = IrrigationFracMicro * IrrigationWater
          IrrigationCntMicro = IrrigationCntMicro + 1
        endif
        if ( (IrrigationAmtFlood == 0.0) .and. (IrrigationFracFlood > 0.0) .and. (OptIrrigationMethod == 0) ) then
          IrrigationAmtFlood = IrrigationFracFlood * IrrigationWater * (1.0/(1.0 - IrriFloodLossFrac))
          IrrigationCntFlood = IrrigationCntFlood + 1
        endif
      else
        IrrigationWater = 0.0
        IrrigationAmtSprinkler = 0.0
        IrrigationAmtMicro = 0.0
        IrrigationAmtFlood = 0.0
      endif
    endif
    end associate
  end subroutine IrrigationTrigger
end module lean_irrigationtriggermod

! Source: physics_noahmp/src/IrrigationPrepareMod.F90
module lean_irrigationpreparemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_irrigationtriggermod, only : IrrigationTrigger
  implicit none
  contains
  subroutine IrrigationPrepare(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( LandUseDataName => 'MODIFIED_IGBP_MODIS_NOAH' , VegType => noahmp%config_domain_VegType , &
      & FlagSoilProcess => noahmp%config_domain_FlagSoilProcess , OptIrrigationMethod => 0 , &
      & IrriFracThreshold => noahmp%water_param_IrriFracThreshold , &
      & IrriStopPrecipThr => noahmp%water_param_IrriStopPrecipThr , &
      & IrrigationFracGrid => noahmp%water_state_IrrigationFracGrid , &
      & IrrigationAmtSprinkler => noahmp%water_state_IrrigationAmtSprinkler , &
      & IrrigationAmtFlood => noahmp%water_state_IrrigationAmtFlood , &
      & IrrigationAmtMicro => noahmp%water_state_IrrigationAmtMicro , &
      & RainfallRefHeight => noahmp%water_flux_RainfallRefHeight , &
      & FlagCropland => noahmp%config_domain_FlagCropland , &
      & IrrigationFracSprinkler => noahmp%water_state_IrrigationFracSprinkler , &
      & IrrigationFracMicro => noahmp%water_state_IrrigationFracMicro , &
      & IrrigationFracFlood => noahmp%water_state_IrrigationFracFlood )
    FlagCropland = .false.
    if ( (VegType == 12) .or. (VegType == 14) ) FlagCropland = .true.
    if ( (OptIrrigationMethod == 0) .and. (IrrigationFracSprinkler == 0.0) .and. (IrrigationFracMicro == 0.0) .and. (IrrigationFracFlood == 0.0) .and. (IrrigationFracGrid >= IrriFracThreshold) ) then
      IrrigationFracSprinkler = 1.0
    endif
    if ( FlagSoilProcess .eqv. .true. ) then
      if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) .and. (RainfallRefHeight < (IrriStopPrecipThr/3600.0)) .and. ((IrrigationAmtSprinkler+IrrigationAmtMicro+IrrigationAmtFlood) == 0.0) ) then
        call IrrigationTrigger(noahmp)
      endif
      if ( (RainfallRefHeight >= (IrriStopPrecipThr/3600.0)) .or. (IrrigationFracGrid < IrriFracThreshold) ) then
        IrrigationAmtSprinkler = 0.0
        IrrigationAmtMicro = 0.0
        IrrigationAmtFlood = 0.0
      endif
    endif
    end associate
  end subroutine IrrigationPrepare
end module lean_irrigationpreparemod

! Source: physics_noahmp/utility/CheckNanMod.F90
module lean_checknanmod
  use Machine, only : kind_noahmp
  implicit none
  contains
  subroutine CheckRealNaN(NumIn, OutVal)
    real(kind=kind_noahmp), intent(in) :: NumIn
    logical , intent(out) :: OutVal
    OutVal = (NumIn /= NumIn)
  end subroutine CheckRealNaN
end module lean_checknanmod

! Source: physics_noahmp/src/SoilHydraulicPropertyMod.F90
module lean_soilhydraulicpropertymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SoilDiffusivityConductivityOpt1(noahmp, SoilWatDiffusivity, SoilWatConductivity, SoilMoisture, &
    & SoilImpervFrac, IndLayer)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer , intent(in) :: IndLayer
    real(kind=kind_noahmp), intent(in) :: SoilMoisture
    real(kind=kind_noahmp), intent(in) :: SoilImpervFrac
    real(kind=kind_noahmp), intent(out) :: SoilWatConductivity
    real(kind=kind_noahmp), intent(out) :: SoilWatDiffusivity
    real(kind=kind_noahmp) :: SoilExpTmp
    real(kind=kind_noahmp) :: SoilPreFac
    associate( SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , &
      & SoilWatDiffusivitySat => noahmp%water_param_SoilWatDiffusivitySat , &
      & SoilWatConductivitySat => noahmp%water_param_SoilWatConductivitySat )
    SoilPreFac = max(0.01, SoilMoisture/SoilMoistureSat(IndLayer))
    SoilExpTmp = SoilExpCoeffB(IndLayer) + 2.0
    SoilWatDiffusivity = SoilWatDiffusivitySat(IndLayer) * SoilPreFac ** SoilExpTmp
    SoilWatDiffusivity = SoilWatDiffusivity * (1.0 - SoilImpervFrac)
    SoilExpTmp = 2.0 * SoilExpCoeffB(IndLayer) + 3.0
    SoilWatConductivity = SoilWatConductivitySat(IndLayer) * SoilPreFac ** SoilExpTmp
    SoilWatConductivity = SoilWatConductivity * (1.0 - SoilImpervFrac)
    end associate
  end subroutine SoilDiffusivityConductivityOpt1
  subroutine SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, SoilMoisture, &
    & SoilIce, IndLayer)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer , intent(in) :: IndLayer
    real(kind=kind_noahmp), intent(in) :: SoilMoisture
    real(kind=kind_noahmp), intent(in) :: SoilIce
    real(kind=kind_noahmp), intent(out) :: SoilWatConductivity
    real(kind=kind_noahmp), intent(out) :: SoilWatDiffusivity
    real(kind=kind_noahmp) :: SoilExpTmp
    real(kind=kind_noahmp) :: SoilPreFac1
    real(kind=kind_noahmp) :: SoilPreFac2
    real(kind=kind_noahmp) :: SoilIceWgt
    associate( SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , &
      & SoilWatDiffusivitySat => noahmp%water_param_SoilWatDiffusivitySat , &
      & SoilWatConductivitySat => noahmp%water_param_SoilWatConductivitySat )
    SoilPreFac1 = 0.05 / SoilMoistureSat(IndLayer)
    SoilPreFac2 = max(0.01, SoilMoisture/SoilMoistureSat(IndLayer))
    SoilPreFac1 = min(SoilPreFac1, SoilPreFac2)
    SoilExpTmp = SoilExpCoeffB(IndLayer) + 2.0
    SoilWatDiffusivity = SoilWatDiffusivitySat(IndLayer) * SoilPreFac2 ** SoilExpTmp
    if ( SoilIce > 0.0 ) then
      SoilIceWgt = 1.0 / (1.0 + (500.0 * SoilIce)**3.0)
      SoilWatDiffusivity = SoilIceWgt * SoilWatDiffusivity + (1.0-SoilIceWgt) * SoilWatDiffusivitySat(IndLayer) * SoilPreFac1**SoilExpTmp
    endif
    SoilExpTmp = 2.0 * SoilExpCoeffB(IndLayer) + 3.0
    SoilWatConductivity = SoilWatConductivitySat(IndLayer) * SoilPreFac2 ** SoilExpTmp
    end associate
  end subroutine SoilDiffusivityConductivityOpt2
end module lean_soilhydraulicpropertymod

! Source: physics_noahmp/src/IrrigationInfilPhilipMod.F90
module lean_irrigationinfilphilipmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_soilhydraulicpropertymod, only : SoilDiffusivityConductivityOpt2
  implicit none
  contains
  subroutine IrrigationInfilPhilip(noahmp, TimeStep, InfilRateSfc)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in) :: TimeStep
    real(kind=kind_noahmp), intent(out) :: InfilRateSfc
    integer :: LoopInd
    integer :: IndSoilLayer
    real(kind=kind_noahmp) :: SoilSorptivity
    real(kind=kind_noahmp) :: SoilWatConductInit
    real(kind=kind_noahmp) :: SoilWatConductivity
    real(kind=kind_noahmp) :: SoilWatDiffusivity
    real(kind=kind_noahmp) :: SoilIceMaxTmp
    associate( NumSoilLayer => 4 , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilIce => noahmp%water_state_SoilIce , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilWatDiffusivitySat => noahmp%water_param_SoilWatDiffusivitySat , &
      & SoilWatConductivitySat => noahmp%water_param_SoilWatConductivitySat )
    SoilWatConductivity = 0.0
    SoilWatDiffusivity = 0.0
    SoilIceMaxTmp = 0.0
    SoilSorptivity = 0.0
    SoilWatConductInit = 0.0
    do LoopInd = 1, NumSoilLayer
      if ( SoilIce(LoopInd) > SoilIceMaxTmp ) SoilIceMaxTmp = SoilIce(LoopInd)
    enddo
    IndSoilLayer = 1
    call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, &
      & SoilLiqWater(IndSoilLayer), SoilIceMaxTmp, IndSoilLayer)
    SoilSorptivity = sqrt(2.0 * max(0.0, &
      & (SoilMoistureSat(IndSoilLayer) - SoilMoisture(IndSoilLayer))) * (SoilWatDiffusivitySat(IndSoilLayer) - SoilWatDiffusivity))
    SoilWatConductInit = min(SoilWatConductivity, (2.0/3.0) * SoilWatConductivitySat(IndSoilLayer))
    SoilWatConductInit = max(SoilWatConductInit , (1.0/3.0) * SoilWatConductivitySat(IndSoilLayer))
    InfilRateSfc = 0.5 * SoilSorptivity * (TimeStep**(-0.5)) + SoilWatConductInit
    InfilRateSfc = max(0.0, InfilRateSfc)
    end associate
  end subroutine IrrigationInfilPhilip
end module lean_irrigationinfilphilipmod

! Source: physics_noahmp/src/IrrigationSprinklerMod.F90
module lean_irrigationsprinklermod
  use Machine
  use lean_checknanmod
  use noahmp_state
  use lean_constantdefinemod
  use lean_irrigationinfilphilipmod, only : IrrigationInfilPhilip
  implicit none
  contains
  subroutine IrrigationSprinkler(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    logical :: FlagNan
    real(kind=kind_noahmp) :: InfilRateSfc
    real(kind=kind_noahmp) :: IrriRateTmp
    real(kind=kind_noahmp) :: WindSpdTot
    real(kind=kind_noahmp) :: IrriLossTmp
    real(kind=kind_noahmp) :: PressureVaporSat
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & WindEastwardRefHeight => noahmp%forcing_WindEastwardRefHeight , &
      & WindNorthwardRefHeight => noahmp%forcing_WindNorthwardRefHeight , &
      & PressureVaporRefHeight => noahmp%energy_state_PressureVaporRefHeight , &
      & IrriSprinklerRate => noahmp%water_param_IrriSprinklerRate , &
      & IrrigationFracSprinkler => noahmp%water_state_IrrigationFracSprinkler , &
      & SoilMoisture => noahmp%water_state_SoilMoisture , SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & HeatLatentIrriEvap => noahmp%energy_flux_HeatLatentIrriEvap , &
      & IrrigationAmtSprinkler => noahmp%water_state_IrrigationAmtSprinkler , &
      & EvapIrriSprinkler => noahmp%water_flux_EvapIrriSprinkler , &
      & RainfallRefHeight => noahmp%water_flux_RainfallRefHeight , &
      & IrrigationRateSprinkler => noahmp%water_flux_IrrigationRateSprinkler , &
      & IrriEvapLossSprinkler => noahmp%water_flux_IrriEvapLossSprinkler , SoilIce => noahmp%water_state_SoilIce )
    SoilIce(:) = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    call IrrigationInfilPhilip(noahmp, MainTimeStep, InfilRateSfc)
    IrriRateTmp = IrriSprinklerRate * (1.0/1000.0) * MainTimeStep / 3600.0
    IrrigationRateSprinkler = min(InfilRateSfc*MainTimeStep, IrrigationAmtSprinkler, IrriRateTmp)
    WindSpdTot = sqrt((WindEastwardRefHeight**2.0) + (WindNorthwardRefHeight**2.0))
    PressureVaporSat = 610.8 * exp((17.27*(TemperatureAirRefHeight-273.15)) / (237.3+(TemperatureAirRefHeight-273.15)))
    if ( TemperatureAirRefHeight > 273.15 ) then
      IrriLossTmp = 4.375 * (exp(0.106*WindSpdTot)) * (((PressureVaporSat-PressureVaporRefHeight)*0.01)**(-0.092)) * ((TemperatureAirRefHeight-273.15)**(-0.102))
    else
      IrriLossTmp = 4.337 * (exp(0.077*WindSpdTot)) * (((PressureVaporSat-PressureVaporRefHeight)*0.01)**(-0.098))
    endif
    call CheckRealNaN(IrriLossTmp, FlagNan)
    if ( FlagNan .eqv. .true. ) IrriLossTmp = 4.0
    if ( (IrriLossTmp > 100.0) .or. (IrriLossTmp < 0.0) ) IrriLossTmp = 4.0
    IrrigationRateSprinkler = IrrigationRateSprinkler * IrrigationFracSprinkler
    if ( IrrigationRateSprinkler >= IrrigationAmtSprinkler ) then
      IrrigationRateSprinkler = IrrigationAmtSprinkler
      IrrigationAmtSprinkler = 0.0
    else
      IrrigationAmtSprinkler = IrrigationAmtSprinkler - IrrigationRateSprinkler
    endif
    IrriEvapLossSprinkler = IrrigationRateSprinkler * IrriLossTmp * (1.0/100.0)
    IrrigationRateSprinkler = IrrigationRateSprinkler - IrriEvapLossSprinkler
    RainfallRefHeight = RainfallRefHeight + (IrrigationRateSprinkler * 1000.0 / MainTimeStep)
    HeatLatentIrriEvap = IrriEvapLossSprinkler * 1000.0 * ConstLatHeatEvap / MainTimeStep
    EvapIrriSprinkler = IrriEvapLossSprinkler * 1000.0 / MainTimeStep
    end associate
  end subroutine IrrigationSprinkler
end module lean_irrigationsprinklermod

! Source: physics_noahmp/src/CanopyWaterInterceptMod.F90
module lean_canopywaterinterceptmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine CanopyWaterIntercept(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: IceDripFacTemp
    real(kind=kind_noahmp) :: IceDripFacWind
    real(kind=kind_noahmp) :: CanopySnowDrip
    associate( SurfaceType => 1 , MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & WindEastwardRefHeight => noahmp%forcing_WindEastwardRefHeight , &
      & WindNorthwardRefHeight => noahmp%forcing_WindNorthwardRefHeight , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , VegFrac => noahmp%energy_state_VegFrac , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & CanopyLiqHoldCap => noahmp%water_param_CanopyLiqHoldCap , &
      & RainfallRefHeight => noahmp%water_flux_RainfallRefHeight , &
      & SnowfallRefHeight => noahmp%water_flux_SnowfallRefHeight , &
      & SnowfallDensity => noahmp%water_state_SnowfallDensity , &
      & PrecipAreaFrac => noahmp%water_state_PrecipAreaFrac , &
      & CanopyLiqWater => noahmp%water_state_CanopyLiqWater , CanopyIce => noahmp%water_state_CanopyIce , &
      & CanopyWetFrac => noahmp%water_state_CanopyWetFrac , &
      & CanopyTotalWater => noahmp%water_state_CanopyTotalWater , &
      & CanopyIceMax => noahmp%water_state_CanopyIceMax , &
      & CanopyLiqWaterMax => noahmp%water_state_CanopyLiqWaterMax , &
      & InterceptCanopyRain => noahmp%water_flux_InterceptCanopyRain , &
      & DripCanopyRain => noahmp%water_flux_DripCanopyRain , &
      & ThroughfallRain => noahmp%water_flux_ThroughfallRain , &
      & InterceptCanopySnow => noahmp%water_flux_InterceptCanopySnow , &
      & DripCanopySnow => noahmp%water_flux_DripCanopySnow , &
      & ThroughfallSnow => noahmp%water_flux_ThroughfallSnow , &
      & RainfallGround => noahmp%water_flux_RainfallGround , SnowfallGround => noahmp%water_flux_SnowfallGround , &
      & SnowDepthIncr => noahmp%water_flux_SnowDepthIncr )
    InterceptCanopyRain = 0.0
    DripCanopyRain = 0.0
    ThroughfallRain = 0.0
    InterceptCanopySnow = 0.0
    DripCanopySnow = 0.0
    ThroughfallSnow = 0.0
    RainfallGround = 0.0
    SnowfallGround = 0.0
    SnowDepthIncr = 0.0
    CanopySnowDrip = 0.0
    IceDripFacTemp = 0.0
    IceDripFacWind = 0.0
    CanopyLiqWaterMax = VegFrac * CanopyLiqHoldCap * (LeafAreaIndEff + StemAreaIndEff)
    if ( (LeafAreaIndEff+StemAreaIndEff) > 0.0 ) then
      InterceptCanopyRain = VegFrac * RainfallRefHeight * PrecipAreaFrac
      InterceptCanopyRain = min( InterceptCanopyRain, &
        & (CanopyLiqWaterMax-CanopyLiqWater)/MainTimeStep * (1.0-exp(-RainfallRefHeight*MainTimeStep/CanopyLiqWaterMax)) )
      InterceptCanopyRain = max( InterceptCanopyRain, 0.0 )
      DripCanopyRain = VegFrac * RainfallRefHeight - InterceptCanopyRain
      ThroughfallRain = (1.0 - VegFrac) * RainfallRefHeight
      CanopyLiqWater = max( 0.0, CanopyLiqWater + InterceptCanopyRain*MainTimeStep )
    else
      InterceptCanopyRain = 0.0
      DripCanopyRain = 0.0
      ThroughfallRain = RainfallRefHeight
      if ( CanopyLiqWater > 0.0 ) then
        DripCanopyRain = DripCanopyRain + CanopyLiqWater / MainTimeStep
        CanopyLiqWater = 0.0
      endif
    endif
    CanopyIceMax = VegFrac * 6.6 * (0.27 + 46.0/SnowfallDensity) * (LeafAreaIndEff + StemAreaIndEff)
    if ( (LeafAreaIndEff+StemAreaIndEff) > 0.0 ) then
      InterceptCanopySnow = VegFrac * SnowfallRefHeight * PrecipAreaFrac
      InterceptCanopySnow = min( InterceptCanopySnow, &
        & (CanopyIceMax-CanopyIce)/MainTimeStep * (1.0-exp(-SnowfallRefHeight*MainTimeStep/CanopyIceMax)) )
      InterceptCanopySnow = max( InterceptCanopySnow, 0.0 )
      IceDripFacTemp = max( 0.0, (TemperatureCanopy - 270.15) / 1.87e5 )
      IceDripFacWind = sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0) / 1.56e5
      CanopySnowDrip = max( 0.0, CanopyIce ) * (IceDripFacWind + IceDripFacTemp)
      CanopySnowDrip = min( CanopyIce/MainTimeStep + InterceptCanopySnow, CanopySnowDrip)
      DripCanopySnow = (VegFrac * SnowfallRefHeight - InterceptCanopySnow) + CanopySnowDrip
      ThroughfallSnow = (1.0 - VegFrac) * SnowfallRefHeight
      CanopyIce = max( 0.0, CanopyIce + (InterceptCanopySnow-CanopySnowDrip)*MainTimeStep )
    else
      InterceptCanopySnow = 0.0
      DripCanopySnow = 0.0
      ThroughfallSnow = SnowfallRefHeight
      if ( CanopyIce > 0.0 ) then
        DripCanopySnow = DripCanopySnow + CanopyIce / MainTimeStep
        CanopyIce = 0.0
      endif
    endif
    if ( CanopyIce > 0.0 ) then
      CanopyWetFrac = max( 0.0, CanopyIce ) / max( CanopyIceMax, 1.0e-06 )
    else
      CanopyWetFrac = max( 0.0, CanopyLiqWater ) / max( CanopyLiqWaterMax, 1.0e-06 )
    endif
    CanopyWetFrac = min( CanopyWetFrac, 1.0 ) ** 0.667
    CanopyTotalWater = CanopyLiqWater + CanopyIce
    RainfallGround = DripCanopyRain + ThroughfallRain
    SnowfallGround = DripCanopySnow + ThroughfallSnow
    SnowDepthIncr = SnowfallGround / SnowfallDensity
    end associate
  end subroutine CanopyWaterIntercept
end module lean_canopywaterinterceptmod

! Source: physics_noahmp/src/PrecipitationHeatAdvectMod.F90
module lean_precipitationheatadvectmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine PrecipitationHeatAdvect(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: HeatPrcpAirToCan
    real(kind=kind_noahmp) :: HeatPrcpCanToGrd
    real(kind=kind_noahmp) :: HeatPrcpAirToGrd
    associate( TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , VegFrac => noahmp%energy_state_VegFrac , &
      & RainfallRefHeight => noahmp%water_flux_RainfallRefHeight , &
      & SnowfallRefHeight => noahmp%water_flux_SnowfallRefHeight , &
      & DripCanopyRain => noahmp%water_flux_DripCanopyRain , &
      & ThroughfallRain => noahmp%water_flux_ThroughfallRain , &
      & DripCanopySnow => noahmp%water_flux_DripCanopySnow , &
      & ThroughfallSnow => noahmp%water_flux_ThroughfallSnow , &
      & HeatPrecipAdvCanopy => noahmp%energy_flux_HeatPrecipAdvCanopy , &
      & HeatPrecipAdvVegGrd => noahmp%energy_flux_HeatPrecipAdvVegGrd , &
      & HeatPrecipAdvBareGrd => noahmp%energy_flux_HeatPrecipAdvBareGrd )
    HeatPrcpAirToCan = 0.0
    HeatPrcpCanToGrd = 0.0
    HeatPrcpAirToGrd = 0.0
    HeatPrecipAdvCanopy = 0.0
    HeatPrecipAdvVegGrd = 0.0
    HeatPrecipAdvBareGrd = 0.0
    HeatPrcpAirToCan = VegFrac * RainfallRefHeight * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight-TemperatureCanopy)
    HeatPrcpCanToGrd = DripCanopyRain * (ConstHeatCapacWater/1000.0) * (TemperatureCanopy-TemperatureGrd)
    HeatPrcpAirToGrd = ThroughfallRain * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight-TemperatureGrd)
    HeatPrcpAirToCan = HeatPrcpAirToCan + VegFrac * SnowfallRefHeight * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight-TemperatureCanopy)
    HeatPrcpCanToGrd = HeatPrcpCanToGrd + DripCanopySnow * (ConstHeatCapacIce/1000.0) * (TemperatureCanopy-TemperatureGrd)
    HeatPrcpAirToGrd = HeatPrcpAirToGrd + ThroughfallSnow * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight-TemperatureGrd)
    HeatPrecipAdvCanopy = HeatPrcpAirToCan - HeatPrcpCanToGrd
    HeatPrecipAdvVegGrd = HeatPrcpCanToGrd
    HeatPrecipAdvBareGrd = HeatPrcpAirToGrd
    if ( (VegFrac > 0.0) .and. (VegFrac < 1.0) ) then
      HeatPrecipAdvVegGrd = HeatPrecipAdvVegGrd / VegFrac
      HeatPrecipAdvBareGrd = HeatPrecipAdvBareGrd / (1.0-VegFrac)
    elseif ( VegFrac <= 0.0 ) then
      HeatPrecipAdvBareGrd = HeatPrecipAdvVegGrd + HeatPrecipAdvBareGrd
      HeatPrecipAdvVegGrd = 0.0
      HeatPrecipAdvCanopy = 0.0
    elseif ( VegFrac >= 1.0 ) then
      HeatPrecipAdvBareGrd = 0.0
    endif
    HeatPrecipAdvCanopy = max(HeatPrecipAdvCanopy , -20.0)
    HeatPrecipAdvCanopy = min(HeatPrecipAdvCanopy , 20.0)
    HeatPrecipAdvVegGrd = max(HeatPrecipAdvVegGrd , -20.0)
    HeatPrecipAdvVegGrd = min(HeatPrecipAdvVegGrd , 20.0)
    HeatPrecipAdvBareGrd = max(HeatPrecipAdvBareGrd, -20.0)
    HeatPrecipAdvBareGrd = min(HeatPrecipAdvBareGrd, 20.0)
    end associate
  end subroutine PrecipitationHeatAdvect
end module lean_precipitationheatadvectmod

! Source: physics_noahmp/src/SnowCoverGroundNiu07Mod.F90
module lean_snowcovergroundniu07mod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowCoverGroundNiu07(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: SnowDensBulk
    real(kind=kind_noahmp) :: MeltFac
    associate( SnowMeltFac => noahmp%water_param_SnowMeltFac , &
      & SnowCoverFac => noahmp%water_param_SnowCoverFac , SnowDepth => noahmp%water_state_SnowDepth , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , SnowCoverFrac => noahmp%water_state_SnowCoverFrac )
    SnowCoverFrac = 0.0
    if ( SnowDepth > 0.0 ) then
      SnowDensBulk = SnowWaterEquiv / SnowDepth
      MeltFac = (SnowDensBulk / 100.0)**SnowMeltFac
      SnowCoverFrac = tanh( SnowDepth /(SnowCoverFac * MeltFac))
    endif
    end associate
  end subroutine SnowCoverGroundNiu07
end module lean_snowcovergroundniu07mod

! Source: physics_noahmp/src/GroundRoughnessPropertyMod.F90
module lean_groundroughnesspropertymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine GroundRoughnessProperty(noahmp, FlagVegSfc)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    logical , intent(in ) :: FlagVegSfc
    associate( SurfaceType => 1 , RefHeightAboveSfc => noahmp%config_domain_RefHeightAboveSfc , &
      & FlagUrban => noahmp%config_domain_FlagUrban , SnowCoverFrac => noahmp%water_state_SnowCoverFrac , &
      & SnowDepth => noahmp%water_state_SnowDepth , HeightCanopyTop => noahmp%energy_param_HeightCanopyTop , &
      & RoughLenMomVeg => noahmp%energy_param_RoughLenMomVeg , &
      & RoughLenMomSnow => noahmp%energy_param_RoughLenMomSnow , &
      & RoughLenMomSoil => noahmp%energy_param_RoughLenMomSoil , &
      & RoughLenMomLake => noahmp%energy_param_RoughLenMomLake , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & RoughLenMomSfc => noahmp%energy_state_RoughLenMomSfc , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & ZeroPlaneDispSfc => noahmp%energy_state_ZeroPlaneDispSfc , &
      & ZeroPlaneDispGrd => noahmp%energy_state_ZeroPlaneDispGrd , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd )
    RoughLenMomGrd = RoughLenMomSoil * (1.0-SnowCoverFrac) + SnowCoverFrac * RoughLenMomSnow
    ZeroPlaneDispGrd = SnowDepth
    if ( FlagVegSfc .eqv. .true. ) then
      RoughLenMomSfc = RoughLenMomVeg
      ZeroPlaneDispSfc = 0.65 * HeightCanopyTop
      if ( SnowDepth > ZeroPlaneDispSfc ) ZeroPlaneDispSfc = SnowDepth
    else
      RoughLenMomSfc = RoughLenMomGrd
      ZeroPlaneDispSfc = ZeroPlaneDispGrd
    endif
    if ( FlagUrban .eqv. .true. ) then
      RoughLenMomGrd = RoughLenMomVeg
      ZeroPlaneDispGrd = 0.65 * HeightCanopyTop
      RoughLenMomSfc = RoughLenMomGrd
      ZeroPlaneDispSfc = ZeroPlaneDispGrd
    endif
    RefHeightAboveGrd = max(ZeroPlaneDispSfc, HeightCanopyTop) + RefHeightAboveSfc
    if ( ZeroPlaneDispGrd >= RefHeightAboveGrd ) RefHeightAboveGrd = ZeroPlaneDispGrd + RefHeightAboveSfc
    end associate
  end subroutine GroundRoughnessProperty
end module lean_groundroughnesspropertymod

! Source: physics_noahmp/src/SnowThermalPropertyMod.F90
module lean_snowthermalpropertymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowThermalProperty(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp), dimension(-2:0) :: SnowDensBulk
    associate( NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , NumSnowLayerMax => 3 , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , OptSnowThermConduct => 1 , &
      & SnowIce => noahmp%water_state_SnowIce , SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & SnowIceVol => noahmp%water_state_SnowIceVol , SnowLiqWaterVol => noahmp%water_state_SnowLiqWaterVol , &
      & SnowEffPorosity => noahmp%water_state_SnowEffPorosity , &
      & HeatCapacVolSnow => noahmp%energy_state_HeatCapacVolSnow , &
      & ThermConductSnow => noahmp%energy_state_ThermConductSnow )
    SnowDensBulk = 0.0
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowIceVol(LoopInd) = min(1.0, SnowIce(LoopInd)/(ThicknessSnowSoilLayer(LoopInd)*ConstDensityIce))
      SnowEffPorosity(LoopInd) = 1.0 - SnowIceVol(LoopInd)
      SnowLiqWaterVol(LoopInd) = min(SnowEffPorosity(LoopInd), &
        & SnowLiqWater(LoopInd)/(ThicknessSnowSoilLayer(LoopInd)*ConstDensityWater))
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowDensBulk(LoopInd) = (SnowIce(LoopInd) + SnowLiqWater(LoopInd)) / ThicknessSnowSoilLayer(LoopInd)
      HeatCapacVolSnow(LoopInd) = ConstHeatCapacIce*SnowIceVol(LoopInd) + ConstHeatCapacWater*SnowLiqWaterVol(LoopInd)
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      ThermConductSnow(LoopInd) = 3.2217e-6 * SnowDensBulk(LoopInd)**2.0
    enddo
    end associate
  end subroutine SnowThermalProperty
end module lean_snowthermalpropertymod

! Source: physics_noahmp/src/SoilThermalPropertyMod.F90
module lean_soilthermalpropertymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SoilThermalProperty(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp) :: KerstenFac
    real(kind=kind_noahmp) :: SoilGamFac
    real(kind=kind_noahmp) :: ThermConductSoilDry
    real(kind=kind_noahmp) :: ThermConductSoilSat
    real(kind=kind_noahmp) :: ThermConductSolid
    real(kind=kind_noahmp) :: SoilSatRatio
    real(kind=kind_noahmp) :: SoilWatFracSat
    real(kind=kind_noahmp) :: SoilWatFrac
    real(kind=kind_noahmp), dimension(1:4) :: SoilIceTmp
    associate( NumSoilLayer => 4 , SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilHeatCapacity => noahmp%energy_param_SoilHeatCapacity , &
      & SoilQuartzFrac => noahmp%energy_param_SoilQuartzFrac , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & HeatCapacVolSoil => noahmp%energy_state_HeatCapacVolSoil , &
      & ThermConductSoil => noahmp%energy_state_ThermConductSoil )
    SoilIceTmp(:) = 0.0
    do LoopInd = 1, NumSoilLayer
      SoilIceTmp(LoopInd) = SoilMoisture(LoopInd) - SoilLiqWater(LoopInd)
      HeatCapacVolSoil(LoopInd) = SoilLiqWater(LoopInd) * ConstHeatCapacWater + (1.0 - SoilMoistureSat(LoopInd)) * SoilHeatCapacity + (SoilMoistureSat(LoopInd) - SoilMoisture(LoopInd)) * ConstHeatCapacAir + SoilIceTmp(LoopInd) * ConstHeatCapacIce
      SoilSatRatio = SoilMoisture(LoopInd) / SoilMoistureSat(LoopInd)
      ThermConductSolid = (ConstThermConductQuartz ** SoilQuartzFrac(LoopInd)) * (ConstThermConductSoilOth ** (1.0 - SoilQuartzFrac(LoopInd)))
      SoilWatFrac = 1.0
      if ( SoilMoisture(LoopInd) > 0.0 ) SoilWatFrac = SoilLiqWater(LoopInd) / SoilMoisture(LoopInd)
      SoilWatFracSat = SoilWatFrac * SoilMoistureSat(LoopInd)
      ThermConductSoilSat = ThermConductSolid ** (1.0-SoilMoistureSat(LoopInd)) * ConstThermConductIce ** (SoilMoistureSat(LoopInd)-SoilWatFracSat) * ConstThermConductWater ** (SoilWatFracSat)
      SoilGamFac = (1.0 - SoilMoistureSat(LoopInd)) * 2700.0
      ThermConductSoilDry = (0.135 * SoilGamFac + 64.7) / (2700.0 - 0.947 * SoilGamFac)
      if ( (SoilLiqWater(LoopInd)+0.0005) < SoilMoisture(LoopInd) ) then
        KerstenFac = SoilSatRatio
      else
        if ( SoilSatRatio > 0.1 ) then
          KerstenFac = log10(SoilSatRatio) + 1.0
        else
          KerstenFac = 0.0
        endif
      endif
      ThermConductSoil(LoopInd) = KerstenFac*(ThermConductSoilSat-ThermConductSoilDry) + ThermConductSoilDry
    enddo
    end associate
  end subroutine SoilThermalProperty
end module lean_soilthermalpropertymod

! Source: physics_noahmp/src/GroundThermalPropertyMod.F90
module lean_groundthermalpropertymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowthermalpropertymod, only : SnowThermalProperty
  use lean_soilthermalpropertymod, only : SoilThermalProperty
  implicit none
  contains
  subroutine GroundThermalProperty(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    associate( NumSoilLayer => 4 , SurfaceType => 1 , MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , FlagUrban => noahmp%config_domain_FlagUrban , &
      & SnowDepth => noahmp%water_state_SnowDepth , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & ThermConductSoilSnow => noahmp%energy_state_ThermConductSoilSnow , &
      & HeatCapacSoilSnow => noahmp%energy_state_HeatCapacSoilSnow , &
      & PhaseChgFacSoilSnow => noahmp%energy_state_PhaseChgFacSoilSnow , &
      & HeatCapacVolSnow => noahmp%energy_state_HeatCapacVolSnow , &
      & ThermConductSnow => noahmp%energy_state_ThermConductSnow , &
      & HeatCapacVolSoil => noahmp%energy_state_HeatCapacVolSoil , &
      & ThermConductSoil => noahmp%energy_state_ThermConductSoil )
    HeatCapacSoilSnow = 0.0
    ThermConductSoilSnow = 0.0
    call SnowThermalProperty(noahmp)
    do LoopInd = NumSnowLayerNeg+1, 0
      ThermConductSoilSnow(LoopInd) = ThermConductSnow(LoopInd)
      HeatCapacSoilSnow(LoopInd) = HeatCapacVolSnow(LoopInd)
    enddo
    call SoilThermalProperty(noahmp)
    do LoopInd = 1, NumSoilLayer
      ThermConductSoilSnow(LoopInd) = ThermConductSoil(LoopInd)
      HeatCapacSoilSnow(LoopInd) = HeatCapacVolSoil(LoopInd)
    enddo
    if ( FlagUrban .eqv. .true. ) then
      do LoopInd = 1, NumSoilLayer
        ThermConductSoilSnow(LoopInd) = 3.24
      enddo
    endif
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      PhaseChgFacSoilSnow(LoopInd) = MainTimeStep / (HeatCapacSoilSnow(LoopInd) * ThicknessSnowSoilLayer(LoopInd))
    enddo
    if ( NumSnowLayerNeg == 0 ) then
      ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + 0.35*SnowDepth) / (SnowDepth + ThicknessSnowSoilLayer(1))
    else
      ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + ThermConductSoilSnow(0)*ThicknessSnowSoilLayer(0)) / (ThicknessSnowSoilLayer(0) + ThicknessSnowSoilLayer(1))
    endif
    end associate
  end subroutine GroundThermalProperty
end module lean_groundthermalpropertymod

! Source: physics_noahmp/src/SnowAgingBatsMod.F90
module lean_snowagingbatsmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowAgingBats(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: SnowAgeFacTot
    real(kind=kind_noahmp) :: SnowAgeVapEff
    real(kind=kind_noahmp) :: SnowAgeFrzEff
    real(kind=kind_noahmp) :: SnowAgeSootEff
    real(kind=kind_noahmp) :: SnowAgeChg
    real(kind=kind_noahmp) :: SnowAgeTmp
    real(kind=kind_noahmp) :: SnowFreshFac
    real(kind=kind_noahmp) :: SnowAgeTimeFac
    real(kind=kind_noahmp) :: SnowGrowVapExp
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & SnowMassFullCoverOld => noahmp%water_param_SnowMassFullCoverOld , &
      & SnowAgeFacBats => noahmp%energy_param_SnowAgeFacBats , &
      & SnowGrowVapFacBats => noahmp%energy_param_SnowGrowVapFacBats , &
      & SnowGrowFrzFacBats => noahmp%energy_param_SnowGrowFrzFacBats , &
      & SnowSootFacBats => noahmp%energy_param_SnowSootFacBats , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & SnowWaterEquivPrev => noahmp%water_state_SnowWaterEquivPrev , &
      & SnowAgeNondim => noahmp%energy_state_SnowAgeNondim , SnowAgeFac => noahmp%energy_state_SnowAgeFac )
    if ( SnowWaterEquiv <= 0.0 ) then
      SnowAgeNondim = 0.0
    else
      SnowAgeTimeFac = MainTimeStep / SnowAgeFacBats
      SnowGrowVapExp = SnowGrowVapFacBats * (1.0/ConstFreezePoint - 1.0/TemperatureGrd)
      SnowAgeVapEff = exp(SnowGrowVapExp)
      SnowAgeFrzEff = exp(amin1(0.0, SnowGrowFrzFacBats*SnowGrowVapExp))
      SnowAgeSootEff = SnowSootFacBats
      SnowAgeFacTot = SnowAgeVapEff + SnowAgeFrzEff + SnowAgeSootEff
      SnowAgeChg = SnowAgeTimeFac * SnowAgeFacTot
      SnowFreshFac = amax1(0.0, SnowWaterEquiv-SnowWaterEquivPrev) / SnowMassFullCoverOld
      SnowAgeTmp = (SnowAgeNondim + SnowAgeChg) * (1.0 - SnowFreshFac)
      SnowAgeNondim = amax1(0.0, SnowAgeTmp)
    endif
    SnowAgeFac = SnowAgeNondim / (SnowAgeNondim + 1.0)
    end associate
  end subroutine SnowAgingBats
end module lean_snowagingbatsmod

! Source: physics_noahmp/src/SnowAlbedoBatsMod.F90
module lean_snowalbedobatsmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowAlbedoBats(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: ZenithAngFac
    real(kind=kind_noahmp) :: ZenithAngFacTmp
    real(kind=kind_noahmp) :: SolarAngleFac2
    real(kind=kind_noahmp) :: SolarAngleFac1
    real(kind=kind_noahmp) :: SolarAngleFac
    associate( NumSwRadBand => 2 , CosSolarZenithAngle => noahmp%config_domain_CosSolarZenithAngle , &
      & SolarZenithAdjBats => noahmp%energy_param_SolarZenithAdjBats , &
      & FreshSnoAlbVisBats => noahmp%energy_param_FreshSnoAlbVisBats , &
      & FreshSnoAlbNirBats => noahmp%energy_param_FreshSnoAlbNirBats , &
      & SnoAgeFacDifVisBats => noahmp%energy_param_SnoAgeFacDifVisBats , &
      & SnoAgeFacDifNirBats => noahmp%energy_param_SnoAgeFacDifNirBats , &
      & SzaFacDirVisBats => noahmp%energy_param_SzaFacDirVisBats , &
      & SzaFacDirNirBats => noahmp%energy_param_SzaFacDirNirBats , SnowAgeFac => noahmp%energy_state_SnowAgeFac , &
      & AlbedoSnowDir => noahmp%energy_state_AlbedoSnowDir , AlbedoSnowDif => noahmp%energy_state_AlbedoSnowDif )
    AlbedoSnowDir(1:NumSwRadBand) = 0.0
    AlbedoSnowDif(1:NumSwRadBand) = 0.0
    SolarAngleFac = SolarZenithAdjBats
    SolarAngleFac1 = 1.0 / SolarAngleFac
    SolarAngleFac2 = 2.0 * SolarAngleFac
    ZenithAngFacTmp = (1.0 + SolarAngleFac1) / (1.0 + SolarAngleFac2*CosSolarZenithAngle) - SolarAngleFac1
    ZenithAngFac = amax1(ZenithAngFacTmp, 0.0)
    AlbedoSnowDif(1) = FreshSnoAlbVisBats * (1.0 - SnoAgeFacDifVisBats * SnowAgeFac)
    AlbedoSnowDif(2) = FreshSnoAlbNirBats * (1.0 - SnoAgeFacDifNirBats * SnowAgeFac)
    AlbedoSnowDir(1) = AlbedoSnowDif(1) + SzaFacDirVisBats * ZenithAngFac * (1.0 - AlbedoSnowDif(1))
    AlbedoSnowDir(2) = AlbedoSnowDif(2) + SzaFacDirNirBats * ZenithAngFac * (1.0 - AlbedoSnowDif(2))
    end associate
  end subroutine SnowAlbedoBats
end module lean_snowalbedobatsmod

! Source: physics_noahmp/src/GroundAlbedoMod.F90
module lean_groundalbedomod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine GroundAlbedo(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndSwBnd
    real(kind=kind_noahmp) :: AlbedoSoilAdjWet
    associate( NumSwRadBand => 2 , SurfaceType => 1 , &
      & CosSolarZenithAngle => noahmp%config_domain_CosSolarZenithAngle , &
      & SnowCoverFrac => noahmp%water_state_SnowCoverFrac , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & AlbedoSoilSat => noahmp%energy_param_AlbedoSoilSat , AlbedoSoilDry => noahmp%energy_param_AlbedoSoilDry , &
      & AlbedoLakeFrz => noahmp%energy_param_AlbedoLakeFrz , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & AlbedoSnowDir => noahmp%energy_state_AlbedoSnowDir , AlbedoSnowDif => noahmp%energy_state_AlbedoSnowDif , &
      & AlbedoSoilDir => noahmp%energy_state_AlbedoSoilDir , AlbedoSoilDif => noahmp%energy_state_AlbedoSoilDif , &
      & AlbedoGrdDir => noahmp%energy_state_AlbedoGrdDir , AlbedoGrdDif => noahmp%energy_state_AlbedoGrdDif )
    do IndSwBnd = 1, NumSwRadBand
      AlbedoSoilAdjWet = max(0.11-0.40*SoilMoisture(1), 0.0)
      AlbedoSoilDir(IndSwBnd) = min(AlbedoSoilSat(IndSwBnd)+AlbedoSoilAdjWet, AlbedoSoilDry(IndSwBnd))
      AlbedoSoilDif(IndSwBnd) = AlbedoSoilDir(IndSwBnd)
      AlbedoGrdDir(IndSwBnd) = AlbedoSoilDir(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDir(IndSwBnd)*SnowCoverFrac
      AlbedoGrdDif(IndSwBnd) = AlbedoSoilDif(IndSwBnd)*(1.0-SnowCoverFrac) + AlbedoSnowDif(IndSwBnd)*SnowCoverFrac
    enddo
    end associate
  end subroutine GroundAlbedo
end module lean_groundalbedomod

! Source: physics_noahmp/src/CanopyRadiationTwoStreamMod.F90
module lean_canopyradiationtwostreammod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine CanopyRadiationTwoStream(noahmp, IndSwBnd, IndSwDif)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer , intent(in ) :: IndSwBnd
    integer , intent(in ) :: IndSwDif
    real(kind=kind_noahmp) :: ScatCoeffCan
    real(kind=kind_noahmp) :: ScatCoeffLeaf
    real(kind=kind_noahmp) :: UpscatCoeffCanDif
    real(kind=kind_noahmp) :: UpscatCoeffLeafDif
    real(kind=kind_noahmp) :: UpscatCoeffCanDir
    real(kind=kind_noahmp) :: UpscatCoeffLeafDir
    real(kind=kind_noahmp) :: OpticDepthDir
    real(kind=kind_noahmp) :: OpticDepthDif
    real(kind=kind_noahmp) :: CosSolarZenithAngleTmp
    real(kind=kind_noahmp) :: SingleScatAlb
    real(kind=kind_noahmp) :: LeafOrientIndex
    real(kind=kind_noahmp) :: RadSwTransDir
    real(kind=kind_noahmp) :: RadSwTransDif
    real(kind=kind_noahmp) :: RadSwReflTot
    real(kind=kind_noahmp) :: VegDensity
    real(kind=kind_noahmp) :: RadSwReflCan
    real(kind=kind_noahmp) :: RadSwReflGrd
    real(kind=kind_noahmp) :: CrownDepth
    real(kind=kind_noahmp) :: CrownRadiusVert
    real(kind=kind_noahmp) :: SolarAngleTmp
    real(kind=kind_noahmp) :: FoliageDensity
    real(kind=kind_noahmp) :: VegAreaIndTmp
    real(kind=kind_noahmp) :: Tmp0,Tmp1,Tmp2,Tmp3,Tmp4
    real(kind=kind_noahmp) :: Tmp5,Tmp6,Tmp7,Tmp8,Tmp9
    real(kind=kind_noahmp) :: P1,P2,P3,P4,S1,S2,U1,U2,U3
    real(kind=kind_noahmp) :: B,C,D,D1,D2,F,H,H1,H2,H3
    real(kind=kind_noahmp) :: H4,H5,H6,H7,H8,H9,H10
    real(kind=kind_noahmp) :: Phi1,Phi2,Sigma
    associate( OptCanopyRadiationTransfer => 3 , &
      & CosSolarZenithAngle => noahmp%config_domain_CosSolarZenithAngle , &
      & CanopyWetFrac => noahmp%water_state_CanopyWetFrac , &
      & TreeCrownRadius => noahmp%energy_param_TreeCrownRadius , &
      & HeightCanopyTop => noahmp%energy_param_HeightCanopyTop , &
      & HeightCanopyBot => noahmp%energy_param_HeightCanopyBot , TreeDensity => noahmp%energy_param_TreeDensity , &
      & CanopyOrientIndex => noahmp%energy_param_CanopyOrientIndex , &
      & ScatterCoeffSnow => noahmp%energy_param_ScatterCoeffSnow , &
      & UpscatterCoeffSnowDir => noahmp%energy_param_UpscatterCoeffSnowDir , &
      & UpscatterCoeffSnowDif => noahmp%energy_param_UpscatterCoeffSnowDif , &
      & VegAreaIndEff => noahmp%energy_state_VegAreaIndEff , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & AlbedoGrdDir => noahmp%energy_state_AlbedoGrdDir , AlbedoGrdDif => noahmp%energy_state_AlbedoGrdDif , &
      & ReflectanceVeg => noahmp%energy_state_ReflectanceVeg , &
      & TransmittanceVeg => noahmp%energy_state_TransmittanceVeg , VegFrac => noahmp%energy_state_VegFrac , &
      & AlbedoSfcDir => noahmp%energy_state_AlbedoSfcDir , AlbedoSfcDif => noahmp%energy_state_AlbedoSfcDif , &
      & VegAreaProjDir => noahmp%energy_state_VegAreaProjDir , GapBtwCanopy => noahmp%energy_state_GapBtwCanopy , &
      & GapInCanopy => noahmp%energy_state_GapInCanopy , GapCanopyDif => noahmp%energy_state_GapCanopyDif , &
      & GapCanopyDir => noahmp%energy_state_GapCanopyDir , RadSwAbsVegDir => noahmp%energy_flux_RadSwAbsVegDir , &
      & RadSwAbsVegDif => noahmp%energy_flux_RadSwAbsVegDif , &
      & RadSwDirTranGrdDir => noahmp%energy_flux_RadSwDirTranGrdDir , &
      & RadSwDirTranGrdDif => noahmp%energy_flux_RadSwDirTranGrdDif , &
      & RadSwDifTranGrdDir => noahmp%energy_flux_RadSwDifTranGrdDir , &
      & RadSwDifTranGrdDif => noahmp%energy_flux_RadSwDifTranGrdDif , &
      & RadSwReflVegDir => noahmp%energy_flux_RadSwReflVegDir , &
      & RadSwReflVegDif => noahmp%energy_flux_RadSwReflVegDif , &
      & RadSwReflGrdDir => noahmp%energy_flux_RadSwReflGrdDir , &
      & RadSwReflGrdDif => noahmp%energy_flux_RadSwReflGrdDif )
    if ( VegAreaIndEff == 0.0 ) then
      GapCanopyDir = 1.0
      GapCanopyDif = 1.0
    else
      GapCanopyDir = 1.0 - VegFrac
      GapCanopyDif = 1.0 - VegFrac
    endif
    CosSolarZenithAngleTmp = max( 0.001, CosSolarZenithAngle )
    LeafOrientIndex = min( max(CanopyOrientIndex, -0.4), 0.6 )
    if ( abs(LeafOrientIndex) <= 0.01 ) LeafOrientIndex = 0.01
    Phi1 = 0.5 - 0.633 * LeafOrientIndex - 0.330 * LeafOrientIndex * LeafOrientIndex
    Phi2 = 0.877 * (1.0 - 2.0 * Phi1)
    VegAreaProjDir = Phi1 + Phi2 * CosSolarZenithAngleTmp
    OpticDepthDir = VegAreaProjDir / CosSolarZenithAngleTmp
    OpticDepthDif = (1.0 - Phi1/Phi2 * log( (Phi1+Phi2) / Phi1 )) / Phi2
    ScatCoeffLeaf = ReflectanceVeg(IndSwBnd) + TransmittanceVeg(IndSwBnd)
    Tmp0 = VegAreaProjDir + Phi2 * CosSolarZenithAngleTmp
    Tmp1 = Phi1 * CosSolarZenithAngleTmp
    SingleScatAlb = 0.5 * ScatCoeffLeaf * VegAreaProjDir / Tmp0 * (1.0 - Tmp1/Tmp0 * log((Tmp1+Tmp0)/Tmp1) )
    UpscatCoeffLeafDir = (1.0 + OpticDepthDif * OpticDepthDir) / (ScatCoeffLeaf * OpticDepthDif * OpticDepthDir) * SingleScatAlb
    UpscatCoeffLeafDif = 0.5 * (ReflectanceVeg(IndSwBnd) + TransmittanceVeg(IndSwBnd) + (ReflectanceVeg(IndSwBnd)-TransmittanceVeg(IndSwBnd))*((1.0+LeafOrientIndex)/2.0)**2)/ScatCoeffLeaf
    if ( TemperatureCanopy > ConstFreezePoint ) then
      Tmp0 = ScatCoeffLeaf
      Tmp1 = UpscatCoeffLeafDir
      Tmp2 = UpscatCoeffLeafDif
    else
      Tmp0 = (1.0 - CanopyWetFrac) * ScatCoeffLeaf + CanopyWetFrac * ScatterCoeffSnow(IndSwBnd)
      Tmp1 = ((1.0 - CanopyWetFrac) * ScatCoeffLeaf * UpscatCoeffLeafDir + CanopyWetFrac * ScatterCoeffSnow(IndSwBnd) * UpscatterCoeffSnowDir ) / Tmp0
      Tmp2 = ((1.0 - CanopyWetFrac) * ScatCoeffLeaf * UpscatCoeffLeafDif + CanopyWetFrac * ScatterCoeffSnow(IndSwBnd) * UpscatterCoeffSnowDif ) / Tmp0
    endif
    ScatCoeffCan = Tmp0
    UpscatCoeffCanDir = Tmp1
    UpscatCoeffCanDif = Tmp2
    B = 1.0 - ScatCoeffCan + ScatCoeffCan * UpscatCoeffCanDif
    C = ScatCoeffCan * UpscatCoeffCanDif
    Tmp0 = OpticDepthDif * OpticDepthDir
    D = Tmp0 * ScatCoeffCan * UpscatCoeffCanDir
    F = Tmp0 * ScatCoeffCan * (1.0 - UpscatCoeffCanDir)
    Tmp1 = B * B - C * C
    H = sqrt(Tmp1) / OpticDepthDif
    Sigma = Tmp0 * Tmp0 - Tmp1
    if ( abs(Sigma) < 1.0e-6 ) Sigma = sign(1.0e-6, Sigma)
    P1 = B + OpticDepthDif * H
    P2 = B - OpticDepthDif * H
    P3 = B + Tmp0
    P4 = B - Tmp0
    S1 = exp( -H * VegAreaIndEff )
    S2 = exp( -OpticDepthDir * VegAreaIndEff )
    if ( IndSwDif == 0 ) then
      U1 = B - C / AlbedoGrdDir(IndSwBnd)
      U2 = B - C * AlbedoGrdDir(IndSwBnd)
      U3 = F + C * AlbedoGrdDir(IndSwBnd)
    else
      U1 = B - C / AlbedoGrdDif(IndSwBnd)
      U2 = B - C * AlbedoGrdDif(IndSwBnd)
      U3 = F + C * AlbedoGrdDif(IndSwBnd)
    endif
    Tmp2 = U1 - OpticDepthDif * H
    Tmp3 = U1 + OpticDepthDif * H
    D1 = P1 * Tmp2 / S1 - P2 * Tmp3 * S1
    Tmp4 = U2 + OpticDepthDif * H
    Tmp5 = U2 - OpticDepthDif * H
    D2 = Tmp4 / S1 - Tmp5 * S1
    H1 = -D * P4 - C * F
    Tmp6 = D - H1 * P3 / Sigma
    Tmp7 = ( D - C - H1 / Sigma * (U1+Tmp0) ) * S2
    H2 = ( Tmp6 * Tmp2 / S1 - P2 * Tmp7 ) / D1
    H3 = - ( Tmp6 * Tmp3 * S1 - P1 * Tmp7 ) / D1
    H4 = -F * P3 - C * D
    Tmp8 = H4 / Sigma
    Tmp9 = ( U3 - Tmp8 * (U2-Tmp0) ) * S2
    H5 = - ( Tmp8 * Tmp4 / S1 + Tmp9 ) / D2
    H6 = ( Tmp8 * Tmp5 * S1 + Tmp9 ) / D2
    H7 = (C * Tmp2) / (D1 * S1)
    H8 = (-C * Tmp3 * S1) / D1
    H9 = Tmp4 / (D2 * S1)
    H10 = (-Tmp5 * S1) / D2
    if ( IndSwDif == 0 ) then
      RadSwTransDir = S2 * (1.0 - GapCanopyDir) + GapCanopyDir
      RadSwTransDif = (H4 * S2 / Sigma + H5 * S1 + H6 / S1) * (1.0 - GapCanopyDir)
    else
      RadSwTransDir = 0.0
      RadSwTransDif = (H9 * S1 + H10 / S1) * (1.0 - GapCanopyDif) + GapCanopyDif
    endif
    if ( IndSwDif == 0 ) then
      RadSwDirTranGrdDir(IndSwBnd) = RadSwTransDir
      RadSwDifTranGrdDir(IndSwBnd) = RadSwTransDif
    else
      RadSwDirTranGrdDif(IndSwBnd) = RadSwTransDir
      RadSwDifTranGrdDif(IndSwBnd) = RadSwTransDif
    endif
    if ( IndSwDif == 0 ) then
      RadSwReflTot = (H1 / Sigma + H2 + H3) * (1.0 - GapCanopyDir) + AlbedoGrdDir(IndSwBnd) * GapCanopyDir
      RadSwReflCan = (H1 / Sigma + H2 + H3) * (1.0 - GapCanopyDir)
      RadSwReflGrd = AlbedoGrdDir(IndSwBnd) * GapCanopyDir
    else
      RadSwReflTot = (H7 + H8) * (1.0 - GapCanopyDif) + AlbedoGrdDif(IndSwBnd) * GapCanopyDif
      RadSwReflCan = (H7 + H8) * (1.0 - GapCanopyDif) + AlbedoGrdDif(IndSwBnd) * GapCanopyDif
      RadSwReflGrd = 0
    endif
    if ( IndSwDif == 0 ) then
      AlbedoSfcDir(IndSwBnd) = RadSwReflTot
      RadSwReflVegDir(IndSwBnd) = RadSwReflCan
      RadSwReflGrdDir(IndSwBnd) = RadSwReflGrd
    else
      AlbedoSfcDif(IndSwBnd) = RadSwReflTot
      RadSwReflVegDif(IndSwBnd) = RadSwReflCan
      RadSwReflGrdDif(IndSwBnd) = RadSwReflGrd
    endif
    if ( IndSwDif == 0 ) then
      RadSwAbsVegDir(IndSwBnd) = 1.0 - AlbedoSfcDir(IndSwBnd) - (1.0 - AlbedoGrdDir(IndSwBnd))*RadSwDirTranGrdDir(IndSwBnd) - (1.0 - AlbedoGrdDif(IndSwBnd))*RadSwDifTranGrdDir(IndSwBnd)
    else
      RadSwAbsVegDif(IndSwBnd) = 1.0 - AlbedoSfcDif(IndSwBnd) - (1.0 - AlbedoGrdDir(IndSwBnd))*RadSwDirTranGrdDif(IndSwBnd) - (1.0 - AlbedoGrdDif(IndSwBnd))*RadSwDifTranGrdDif(IndSwBnd)
    endif
    end associate
  end subroutine CanopyRadiationTwoStream
end module lean_canopyradiationtwostreammod

! Source: physics_noahmp/src/SurfaceAlbedoMod.F90
module lean_surfacealbedomod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowagingbatsmod, only : SnowAgingBats
  use lean_snowalbedobatsmod, only : SnowAlbedoBats
  use lean_groundalbedomod, only : GroundAlbedo
  use lean_canopyradiationtwostreammod, only : CanopyRadiationTwoStream
  implicit none
  contains
  subroutine SurfaceAlbedo(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndBand
    integer :: IndDif
    real(kind=kind_noahmp) :: LeafWgt
    real(kind=kind_noahmp) :: StemWgt
    real(kind=kind_noahmp) :: MinThr
    real(kind=kind_noahmp) :: LightExtDir
    associate( NumSwRadBand => 2 , CosSolarZenithAngle => noahmp%config_domain_CosSolarZenithAngle , &
      & OptSnowAlbedo => 1 , ReflectanceLeaf => noahmp%energy_param_ReflectanceLeaf , &
      & ReflectanceStem => noahmp%energy_param_ReflectanceStem , &
      & TransmittanceLeaf => noahmp%energy_param_TransmittanceLeaf , &
      & TransmittanceStem => noahmp%energy_param_TransmittanceStem , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , AlbedoGrdDir => noahmp%energy_state_AlbedoGrdDir , &
      & AlbedoGrdDif => noahmp%energy_state_AlbedoGrdDif , AlbedoSnowDir => noahmp%energy_state_AlbedoSnowDir , &
      & AlbedoSnowDif => noahmp%energy_state_AlbedoSnowDif , AlbedoSfcDir => noahmp%energy_state_AlbedoSfcDir , &
      & AlbedoSfcDif => noahmp%energy_state_AlbedoSfcDif , &
      & CanopySunlitFrac => noahmp%energy_state_CanopySunlitFrac , &
      & CanopyShadeFrac => noahmp%energy_state_CanopyShadeFrac , &
      & LeafAreaIndSunlit => noahmp%energy_state_LeafAreaIndSunlit , &
      & LeafAreaIndShade => noahmp%energy_state_LeafAreaIndShade , &
      & GapBtwCanopy => noahmp%energy_state_GapBtwCanopy , GapInCanopy => noahmp%energy_state_GapInCanopy , &
      & ReflectanceVeg => noahmp%energy_state_ReflectanceVeg , &
      & TransmittanceVeg => noahmp%energy_state_TransmittanceVeg , &
      & VegAreaIndEff => noahmp%energy_state_VegAreaIndEff , &
      & VegAreaProjDir => noahmp%energy_state_VegAreaProjDir , &
      & RadSwAbsVegDir => noahmp%energy_flux_RadSwAbsVegDir , &
      & RadSwAbsVegDif => noahmp%energy_flux_RadSwAbsVegDif , &
      & RadSwDirTranGrdDir => noahmp%energy_flux_RadSwDirTranGrdDir , &
      & RadSwDifTranGrdDir => noahmp%energy_flux_RadSwDifTranGrdDir , &
      & RadSwDifTranGrdDif => noahmp%energy_flux_RadSwDifTranGrdDif , &
      & RadSwDirTranGrdDif => noahmp%energy_flux_RadSwDirTranGrdDif , &
      & RadSwReflVegDir => noahmp%energy_flux_RadSwReflVegDir , &
      & RadSwReflVegDif => noahmp%energy_flux_RadSwReflVegDif , &
      & RadSwReflGrdDir => noahmp%energy_flux_RadSwReflGrdDir , &
      & RadSwReflGrdDif => noahmp%energy_flux_RadSwReflGrdDif )
    MinThr = 1.0e-06
    GapBtwCanopy = 0.0
    GapInCanopy = 0.0
    VegAreaProjDir = 0.0
    ReflectanceVeg = 0.0
    TransmittanceVeg = 0.0
    CanopySunlitFrac = 0.0
    do IndBand = 1, NumSwRadBand
      AlbedoSfcDir (IndBand) = 0.0
      AlbedoSfcDif (IndBand) = 0.0
      AlbedoGrdDir (IndBand) = 0.0
      AlbedoGrdDif (IndBand) = 0.0
      AlbedoSnowDir (IndBand) = 0.0
      AlbedoSnowDif (IndBand) = 0.0
      RadSwAbsVegDir (IndBand) = 0.0
      RadSwAbsVegDif (IndBand) = 0.0
      RadSwDirTranGrdDir(IndBand) = 0.0
      RadSwDirTranGrdDif(IndBand) = 0.0
      RadSwDifTranGrdDir(IndBand) = 0.0
      RadSwDifTranGrdDif(IndBand) = 0.0
      RadSwReflVegDir (IndBand) = 0.0
      RadSwReflVegDif (IndBand) = 0.0
      RadSwReflGrdDir (IndBand) = 0.0
      RadSwReflGrdDif (IndBand) = 0.0
    enddo
    VegAreaIndEff = LeafAreaIndEff + StemAreaIndEff
    if ( CosSolarZenithAngle > 0 ) then
      LeafWgt = LeafAreaIndEff / max(VegAreaIndEff, MinThr)
      StemWgt = StemAreaIndEff / max(VegAreaIndEff, MinThr)
      do IndBand = 1, NumSwRadBand
        ReflectanceVeg(IndBand) = max(ReflectanceLeaf(IndBand)*LeafWgt+ReflectanceStem(IndBand)*StemWgt, MinThr)
        TransmittanceVeg(IndBand) = max(TransmittanceLeaf(IndBand)*LeafWgt+TransmittanceStem(IndBand)*StemWgt, MinThr)
      enddo
      call SnowAgingBats(noahmp)
      call SnowAlbedoBats(noahmp)
      call GroundAlbedo(noahmp)
      do IndBand = 1, NumSwRadBand
        IndDif = 0
        call CanopyRadiationTwoStream(noahmp, IndBand, IndDif)
        IndDif = 1
        call CanopyRadiationTwoStream(noahmp, IndBand, IndDif)
      enddo
      LightExtDir = VegAreaProjDir / CosSolarZenithAngle * sqrt(1.0-ReflectanceVeg(1)-TransmittanceVeg(1))
      CanopySunlitFrac = (1.0 - exp(-LightExtDir*VegAreaIndEff)) / max(LightExtDir*VegAreaIndEff, MinThr)
      LightExtDir = CanopySunlitFrac
      if ( LightExtDir < 0.01 ) then
        LeafWgt = 0.0
      else
        LeafWgt = LightExtDir
      endif
      CanopySunlitFrac = LeafWgt
    endif
    CanopyShadeFrac = 1.0 - CanopySunlitFrac
    LeafAreaIndSunlit = LeafAreaIndEff * CanopySunlitFrac
    LeafAreaIndShade = LeafAreaIndEff * CanopyShadeFrac
    end associate
  end subroutine SurfaceAlbedo
end module lean_surfacealbedomod

! Source: physics_noahmp/src/SurfaceRadiationMod.F90
module lean_surfaceradiationmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SurfaceRadiation(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndBand
    real(kind=kind_noahmp) :: MinThr
    real(kind=kind_noahmp) :: RadSwAbsGrdTmp
    real(kind=kind_noahmp) :: RadSwReflSfcNir
    real(kind=kind_noahmp) :: RadSwReflSfcVis
    real(kind=kind_noahmp) :: LeafAreaIndFrac
    real(kind=kind_noahmp) :: RadSwTranGrdDir
    real(kind=kind_noahmp) :: RadSwTranGrdDif
    real(kind=kind_noahmp), dimension(1:2) :: RadSwAbsCanDir
    real(kind=kind_noahmp), dimension(1:2) :: RadSwAbsCanDif
    associate( NumSwRadBand => 2 , LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & VegAreaIndEff => noahmp%energy_state_VegAreaIndEff , &
      & CanopySunlitFrac => noahmp%energy_state_CanopySunlitFrac , &
      & CanopyShadeFrac => noahmp%energy_state_CanopyShadeFrac , &
      & LeafAreaIndSunlit => noahmp%energy_state_LeafAreaIndSunlit , &
      & LeafAreaIndShade => noahmp%energy_state_LeafAreaIndShade , &
      & RadSwDownDir => noahmp%energy_flux_RadSwDownDir , RadSwDownDif => noahmp%energy_flux_RadSwDownDif , &
      & RadSwAbsVegDir => noahmp%energy_flux_RadSwAbsVegDir , &
      & RadSwAbsVegDif => noahmp%energy_flux_RadSwAbsVegDif , &
      & RadSwDirTranGrdDir => noahmp%energy_flux_RadSwDirTranGrdDir , &
      & RadSwDifTranGrdDir => noahmp%energy_flux_RadSwDifTranGrdDir , &
      & RadSwDifTranGrdDif => noahmp%energy_flux_RadSwDifTranGrdDif , &
      & AlbedoGrdDir => noahmp%energy_state_AlbedoGrdDir , AlbedoGrdDif => noahmp%energy_state_AlbedoGrdDif , &
      & AlbedoSfcDir => noahmp%energy_state_AlbedoSfcDir , AlbedoSfcDif => noahmp%energy_state_AlbedoSfcDif , &
      & RadSwReflVegDir => noahmp%energy_flux_RadSwReflVegDir , &
      & RadSwReflVegDif => noahmp%energy_flux_RadSwReflVegDif , &
      & RadSwReflGrdDir => noahmp%energy_flux_RadSwReflGrdDir , &
      & RadSwReflGrdDif => noahmp%energy_flux_RadSwReflGrdDif , &
      & RadPhotoActAbsSunlit => noahmp%energy_flux_RadPhotoActAbsSunlit , &
      & RadPhotoActAbsShade => noahmp%energy_flux_RadPhotoActAbsShade , &
      & RadSwAbsVeg => noahmp%energy_flux_RadSwAbsVeg , RadSwAbsGrd => noahmp%energy_flux_RadSwAbsGrd , &
      & RadSwAbsSfc => noahmp%energy_flux_RadSwAbsSfc , RadSwReflSfc => noahmp%energy_flux_RadSwReflSfc , &
      & RadSwReflVeg => noahmp%energy_flux_RadSwReflVeg , RadSwReflGrd => noahmp%energy_flux_RadSwReflGrd )
    MinThr = 1.0e-6
    RadSwAbsCanDir = 0.0
    RadSwAbsCanDif = 0.0
    RadSwAbsGrd = 0.0
    RadSwAbsVeg = 0.0
    RadSwAbsSfc = 0.0
    RadSwReflSfc = 0.0
    RadSwReflVeg = 0.0
    RadSwReflGrd = 0.0
    RadPhotoActAbsSunlit = 0.0
    RadPhotoActAbsShade = 0.0
    do IndBand = 1, NumSwRadBand
      RadSwAbsCanDir(IndBand) = RadSwDownDir(IndBand) * RadSwAbsVegDir(IndBand)
      RadSwAbsCanDif(IndBand) = RadSwDownDif(IndBand) * RadSwAbsVegDif(IndBand)
      RadSwAbsVeg = RadSwAbsVeg + RadSwAbsCanDir(IndBand) + RadSwAbsCanDif(IndBand)
      RadSwAbsSfc = RadSwAbsSfc + RadSwAbsCanDir(IndBand) + RadSwAbsCanDif(IndBand)
      RadSwTranGrdDir = RadSwDownDir(IndBand) * RadSwDirTranGrdDir(IndBand)
      RadSwTranGrdDif = RadSwDownDir(IndBand) * RadSwDifTranGrdDir(IndBand) + RadSwDownDif(IndBand) * RadSwDifTranGrdDif(IndBand)
      RadSwAbsGrdTmp = RadSwTranGrdDir * (1.0 - AlbedoGrdDir(IndBand)) + RadSwTranGrdDif * (1.0 - AlbedoGrdDif(IndBand))
      RadSwAbsGrd = RadSwAbsGrd + RadSwAbsGrdTmp
      RadSwAbsSfc = RadSwAbsSfc + RadSwAbsGrdTmp
    enddo
    LeafAreaIndFrac = LeafAreaIndEff / max(VegAreaIndEff, MinThr)
    if ( CanopySunlitFrac > 0.0 ) then
      RadPhotoActAbsSunlit = (RadSwAbsCanDir(1) + CanopySunlitFrac * RadSwAbsCanDif(1)) * LeafAreaIndFrac / max(LeafAreaIndSunlit, MinThr)
      RadPhotoActAbsShade = (CanopyShadeFrac * RadSwAbsCanDif(1)) * LeafAreaIndFrac / max(LeafAreaIndShade, MinThr)
    else
      RadPhotoActAbsSunlit = 0.0
      RadPhotoActAbsShade = (RadSwAbsCanDir(1) + RadSwAbsCanDif(1)) * LeafAreaIndFrac / max(LeafAreaIndShade, &
        & MinThr)
    endif
    RadSwReflSfcVis = AlbedoSfcDir(1) * RadSwDownDir(1) + AlbedoSfcDif(1) * RadSwDownDif(1)
    RadSwReflSfcNir = AlbedoSfcDir(2) * RadSwDownDir(2) + AlbedoSfcDif(2) * RadSwDownDif(2)
    RadSwReflSfc = RadSwReflSfcVis + RadSwReflSfcNir
    RadSwReflVeg = RadSwReflVegDir(1)*RadSwDownDir(1) + RadSwReflVegDif(1)*RadSwDownDif(1) + RadSwReflVegDir(2)*RadSwDownDir(2) + RadSwReflVegDif(2)*RadSwDownDif(2)
    RadSwReflGrd = RadSwReflGrdDir(1)*RadSwDownDir(1) + RadSwReflGrdDif(1)*RadSwDownDif(1) + RadSwReflGrdDir(2)*RadSwDownDir(2) + RadSwReflGrdDif(2)*RadSwDownDif(2)
    end associate
  end subroutine SurfaceRadiation
end module lean_surfaceradiationmod

! Source: physics_noahmp/src/SurfaceEmissivityMod.F90
module lean_surfaceemissivitymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SurfaceEmissivity(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( IndicatorIceSfc => 0 , SurfaceType => 1 , &
      & EmissivitySnow => noahmp%energy_param_EmissivitySnow , &
      & EmissivitySoilLake => noahmp%energy_param_EmissivitySoilLake , &
      & EmissivityIceSfc => noahmp%energy_param_EmissivityIceSfc , &
      & SnowCoverFrac => noahmp%water_state_SnowCoverFrac , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , VegFrac => noahmp%energy_state_VegFrac , &
      & EmissivityVeg => noahmp%energy_state_EmissivityVeg , EmissivityGrd => noahmp%energy_state_EmissivityGrd , &
      & EmissivitySfc => noahmp%energy_state_EmissivitySfc )
    EmissivityVeg = 1.0 - exp(-(LeafAreaIndEff + StemAreaIndEff) / 1.0)
    EmissivityGrd = EmissivitySoilLake(SurfaceType) * (1.0-SnowCoverFrac) + EmissivitySnow * SnowCoverFrac
    EmissivitySfc = VegFrac * (EmissivityGrd*(1-EmissivityVeg) + EmissivityVeg + EmissivityVeg*(1-EmissivityVeg)*(1-EmissivityGrd)) + (1-VegFrac) * EmissivityGrd
    end associate
  end subroutine SurfaceEmissivity
end module lean_surfaceemissivitymod

! Source: physics_noahmp/src/SoilWaterTranspirationMod.F90
module lean_soilwatertranspirationmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SoilWaterTranspiration(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndSoil
    real(kind=kind_noahmp) :: SoilWetFac
    real(kind=kind_noahmp) :: MinThr
    associate( SurfaceType => 1 , ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , OptSoilWaterTranspiration => 1 , &
      & NumSoilLayerRoot => noahmp%water_param_NumSoilLayerRoot , &
      & SoilMoistureWilt => noahmp%water_param_SoilMoistureWilt , &
      & SoilMoistureFieldCap => noahmp%water_param_SoilMoistureFieldCap , &
      & SoilMatPotentialWilt => noahmp%water_param_SoilMatPotentialWilt , &
      & SoilMatPotentialSat => noahmp%water_param_SoilMatPotentialSat , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & SoilTranspFac => noahmp%water_state_SoilTranspFac , &
      & SoilTranspFacAcc => noahmp%water_state_SoilTranspFacAcc , &
      & SoilMatPotential => noahmp%water_state_SoilMatPotential )
    MinThr = 1.0e-6
    SoilTranspFacAcc = 0.0
    do IndSoil = 1, NumSoilLayerRoot
      SoilWetFac = (SoilLiqWater(IndSoil) - SoilMoistureWilt(IndSoil)) / (SoilMoistureFieldCap(IndSoil) - SoilMoistureWilt(IndSoil))
      SoilWetFac = min(1.0, max(0.0,SoilWetFac))
      SoilTranspFac(IndSoil) = max(MinThr, &
        & ThicknessSnowSoilLayer(IndSoil) / (-DepthSoilLayer(NumSoilLayerRoot)) * SoilWetFac)
      SoilTranspFacAcc = SoilTranspFacAcc + SoilTranspFac(IndSoil)
    enddo
    SoilTranspFacAcc = max(MinThr, SoilTranspFacAcc)
    SoilTranspFac(1:NumSoilLayerRoot) = SoilTranspFac(1:NumSoilLayerRoot) / SoilTranspFacAcc
    end associate
  end subroutine SoilWaterTranspiration
end module lean_soilwatertranspirationmod

! Source: physics_noahmp/src/ResistanceGroundEvaporationMod.F90
module lean_resistancegroundevaporationmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceGroundEvaporation(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: SoilEvapFac
    real(kind=kind_noahmp) :: DrySoilThickness
    real(kind=kind_noahmp) :: VapDiffuseRed
    real(kind=kind_noahmp) :: SoilMatPotentialSfc
    associate( SurfaceType => 1 , DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & FlagUrban => noahmp%config_domain_FlagUrban , OptGroundResistanceEvap => 4 , &
      & ResistanceSoilExp => noahmp%energy_param_ResistanceSoilExp , &
      & ResistanceSnowSfc => noahmp%energy_param_ResistanceSnowSfc , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilMoistureWilt => noahmp%water_param_SoilMoistureWilt , &
      & SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , &
      & SoilMatPotentialSat => noahmp%water_param_SoilMatPotentialSat , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SnowCoverFrac => noahmp%water_state_SnowCoverFrac , &
      & SnowDepth => noahmp%water_state_SnowDepth , TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & ResistanceGrdEvap => noahmp%energy_state_ResistanceGrdEvap , &
      & RelHumidityGrd => noahmp%energy_state_RelHumidityGrd )
    SoilEvapFac = max(0.0, SoilLiqWater(1)/SoilMoistureSat(1))
    DrySoilThickness = (-DepthSoilLayer(1)) * (exp((1.0 - min(1.0, &
      & SoilLiqWater(1)/SoilMoistureSat(1))) ** ResistanceSoilExp) - 1.0) / (2.71828-1.0)
    VapDiffuseRed = 2.2e-5 * SoilMoistureSat(1) * SoilMoistureSat(1) * (1.0 - SoilMoistureWilt(1)/SoilMoistureSat(1)) ** (2.0 + 3.0/SoilExpCoeffB(1))
#ifdef LEAN_NOAHMP_RSF3
    ! Lean departure (build switch LEAN_NOAHMP_RSF3, on in flavor lean, off in mpas; docs/fixes.md): the soil part of MPAS's option 4
    ! resistance uses Noah-MP's option 3 (Sellers 1992 adjusted to a lower resistance over wet soil) instead of Sakaguchi and Zeng;
    ! option 4's snow weighting below stays.
    ResistanceGrdEvap = exp(8.25 - 6.0*SoilEvapFac)
#else
    ResistanceGrdEvap = DrySoilThickness / VapDiffuseRed
#endif
    ResistanceGrdEvap = 1.0 / (SnowCoverFrac * (1.0/ResistanceSnowSfc) + (1.0-SnowCoverFrac) * (1.0/max(ResistanceGrdEvap,0.001)))
    if ( (SoilLiqWater(1) < 0.01) .and. (SnowDepth == 0.0) ) ResistanceGrdEvap = 1.0e6
    SoilMatPotentialSfc = -SoilMatPotentialSat(1) * (max(0.01, &
      & SoilLiqWater(1)) / SoilMoistureSat(1)) ** (-SoilExpCoeffB(1))
    RelHumidityGrd = SnowCoverFrac + (1.0-SnowCoverFrac) * exp(SoilMatPotentialSfc*ConstGravityAcc/(ConstGasWaterVapor*TemperatureGrd))
    if ( (FlagUrban .eqv. .true.) .and. (SnowDepth == 0.0) ) then
      ResistanceGrdEvap = 1.0e6
    endif
    end associate
  end subroutine ResistanceGroundEvaporation
end module lean_resistancegroundevaporationmod

! Source: physics_noahmp/src/PsychrometricVariableMod.F90
module lean_psychrometricvariablemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine PsychrometricVariable(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & LatHeatVapCanopy => noahmp%energy_state_LatHeatVapCanopy , &
      & LatHeatVapGrd => noahmp%energy_state_LatHeatVapGrd , &
      & FlagFrozenCanopy => noahmp%energy_state_FlagFrozenCanopy , &
      & FlagFrozenGround => noahmp%energy_state_FlagFrozenGround , &
      & PsychConstCanopy => noahmp%energy_state_PsychConstCanopy , &
      & PsychConstGrd => noahmp%energy_state_PsychConstGrd )
    if ( TemperatureCanopy > ConstFreezePoint ) then
      LatHeatVapCanopy = ConstLatHeatEvap
      FlagFrozenCanopy = .false.
    else
      LatHeatVapCanopy = ConstLatHeatSublim
      FlagFrozenCanopy = .true.
    endif
    PsychConstCanopy = ConstHeatCapacAir * PressureAirRefHeight / (0.622*LatHeatVapCanopy)
    if ( TemperatureGrd > ConstFreezePoint ) then
      LatHeatVapGrd = ConstLatHeatEvap
      FlagFrozenGround = .false.
    else
      LatHeatVapGrd = ConstLatHeatSublim
      FlagFrozenGround = .true.
    endif
    PsychConstGrd = ConstHeatCapacAir * PressureAirRefHeight / (0.622*LatHeatVapGrd)
    end associate
  end subroutine PsychrometricVariable
end module lean_psychrometricvariablemod

! Source: physics_noahmp/src/VaporPressureSaturationMod.F90
module lean_vaporpressuresaturationmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine VaporPressureSaturation(T, VapPresSatWat, VapPresSatIce, VapPresSatWatD, VapPresSatIceD)
    implicit none
    real(kind=kind_noahmp), intent(in) :: T
    real(kind=kind_noahmp), intent(out) :: VapPresSatWat
    real(kind=kind_noahmp), intent(out) :: VapPresSatIce
    real(kind=kind_noahmp), intent(out) :: VapPresSatWatD
    real(kind=kind_noahmp), intent(out) :: VapPresSatIceD
    real(kind=kind_noahmp), parameter :: A0 = 6.107799961
    real(kind=kind_noahmp), parameter :: A1 = 4.436518521e-01
    real(kind=kind_noahmp), parameter :: A2 = 1.428945805e-02
    real(kind=kind_noahmp), parameter :: A3 = 2.650648471e-04
    real(kind=kind_noahmp), parameter :: A4 = 3.031240396e-06
    real(kind=kind_noahmp), parameter :: A5 = 2.034080948e-08
    real(kind=kind_noahmp), parameter :: A6 = 6.136820929e-11
    real(kind=kind_noahmp), parameter :: B0 = 6.109177956
    real(kind=kind_noahmp), parameter :: B1 = 5.034698970e-01
    real(kind=kind_noahmp), parameter :: B2 = 1.886013408e-02
    real(kind=kind_noahmp), parameter :: B3 = 4.176223716e-04
    real(kind=kind_noahmp), parameter :: B4 = 5.824720280e-06
    real(kind=kind_noahmp), parameter :: B5 = 4.838803174e-08
    real(kind=kind_noahmp), parameter :: B6 = 1.838826904e-10
    real(kind=kind_noahmp), parameter :: C0 = 4.438099984e-01
    real(kind=kind_noahmp), parameter :: C1 = 2.857002636e-02
    real(kind=kind_noahmp), parameter :: C2 = 7.938054040e-04
    real(kind=kind_noahmp), parameter :: C3 = 1.215215065e-05
    real(kind=kind_noahmp), parameter :: C4 = 1.036561403e-07
    real(kind=kind_noahmp), parameter :: C5 = 3.532421810e-10
    real(kind=kind_noahmp), parameter :: C6 = -7.090244804e-13
    real(kind=kind_noahmp), parameter :: D0 = 5.030305237e-01
    real(kind=kind_noahmp), parameter :: D1 = 3.773255020e-02
    real(kind=kind_noahmp), parameter :: D2 = 1.267995369e-03
    real(kind=kind_noahmp), parameter :: D3 = 2.477563108e-05
    real(kind=kind_noahmp), parameter :: D4 = 3.005693132e-07
    real(kind=kind_noahmp), parameter :: D5 = 2.158542548e-09
    real(kind=kind_noahmp), parameter :: D6 = 7.131097725e-12
    VapPresSatWat = 100.0 * (A0 + T * (A1 + T * (A2 + T * (A3 + T * ( A4 + T * (A5 + T*A6) ) ) ) ) )
    VapPresSatIce = 100.0 * (B0 + T * (B1 + T * (B2 + T * (B3 + T * ( B4 + T * (B5 + T*B6) ) ) ) ) )
    VapPresSatWatD = 100.0 * (C0 + T * (C1 + T * (C2 + T * (C3 + T * ( C4 + T * (C5 + T*C6) ) ) ) ) )
    VapPresSatIceD = 100.0 * (D0 + T * (D1 + T * (D2 + T * (D3 + T * ( D4 + T * (D5 + T*D6) ) ) ) ) )
  end subroutine VaporPressureSaturation
end module lean_vaporpressuresaturationmod

! Source: physics_noahmp/src/ResistanceAboveCanopyMostMod.F90
module lean_resistanceabovecanopymostmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceAboveCanopyMOST(noahmp, IterationInd, HeatSensibleTmp, MoStabParaSgn)
    implicit none
    integer , intent(in ) :: IterationInd
    integer , intent(inout) :: MoStabParaSgn
    real(kind=kind_noahmp), intent(in ) :: HeatSensibleTmp
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp) :: MPE
    real(kind=kind_noahmp) :: TMPCM
    real(kind=kind_noahmp) :: TMPCH
    real(kind=kind_noahmp) :: FMNEW
    real(kind=kind_noahmp) :: FHNEW
    real(kind=kind_noahmp) :: MOZOLD
    real(kind=kind_noahmp) :: TMP1,TMP2,TMP3,TMP4,TMP5
    real(kind=kind_noahmp) :: TVIR
    real(kind=kind_noahmp) :: TMPCM2
    real(kind=kind_noahmp) :: TMPCH2
    real(kind=kind_noahmp) :: FM2NEW
    real(kind=kind_noahmp) :: FH2NEW
    real(kind=kind_noahmp) :: TMP12,TMP22,TMP32
    real(kind=kind_noahmp) :: CMFM, CHFH, CM2FM2, CH2FH2
    associate( TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & ZeroPlaneDispSfc => noahmp%energy_state_ZeroPlaneDispSfc , &
      & RoughLenShCanopy => noahmp%energy_state_RoughLenShCanopy , &
      & RoughLenMomSfc => noahmp%energy_state_RoughLenMomSfc , &
      & MoStabCorrMomAbvCan => noahmp%energy_state_MoStabCorrMomAbvCan , &
      & MoStabCorrShAbvCan => noahmp%energy_state_MoStabCorrShAbvCan , &
      & MoStabCorrMomVeg2m => noahmp%energy_state_MoStabCorrMomVeg2m , &
      & MoStabCorrShVeg2m => noahmp%energy_state_MoStabCorrShVeg2m , &
      & MoStabParaAbvCan => noahmp%energy_state_MoStabParaAbvCan , &
      & FrictionVelVeg => noahmp%energy_state_FrictionVelVeg , &
      & MoStabParaVeg2m => noahmp%energy_state_MoStabParaVeg2m , &
      & MoLengthAbvCan => noahmp%energy_state_MoLengthAbvCan , &
      & ExchCoeffMomAbvCan => noahmp%energy_state_ExchCoeffMomAbvCan , &
      & ExchCoeffShAbvCan => noahmp%energy_state_ExchCoeffShAbvCan , &
      & ExchCoeffSh2mVegMo => noahmp%energy_state_ExchCoeffSh2mVegMo , &
      & ResistanceMomAbvCan => noahmp%energy_state_ResistanceMomAbvCan , &
      & ResistanceShAbvCan => noahmp%energy_state_ResistanceShAbvCan , &
      & ResistanceLhAbvCan => noahmp%energy_state_ResistanceLhAbvCan )
    MPE = 1.0e-6
    MOZOLD = MoStabParaAbvCan
    if ( RefHeightAboveGrd <= ZeroPlaneDispSfc ) then
      write(*,*) "WARNING: critical problem: RefHeightAboveGrd <= ZeroPlaneDispSfc; model stops"
      stop "Error in ResistanceAboveCanopyMostMod.F90"
    endif
    TMPCM = log((RefHeightAboveGrd - ZeroPlaneDispSfc) / RoughLenMomSfc)
    TMPCH = log((RefHeightAboveGrd - ZeroPlaneDispSfc) / RoughLenShCanopy)
    TMPCM2 = log((2.0 + RoughLenMomSfc) / RoughLenMomSfc)
    TMPCH2 = log((2.0 + RoughLenShCanopy) / RoughLenShCanopy)
    if ( IterationInd == 1 ) then
      FrictionVelVeg = 0.0
      MoStabParaAbvCan = 0.0
      MoLengthAbvCan = 0.0
      MoStabParaVeg2m = 0.0
    else
      TVIR = (1.0 + 0.61*SpecHumidityRefHeight) * TemperatureAirRefHeight
      TMP1 = ConstVonKarman * (ConstGravityAcc/TVIR) * HeatSensibleTmp / (DensityAirRefHeight*ConstHeatCapacAir)
      if ( abs(TMP1) <= MPE ) TMP1 = MPE
      MoLengthAbvCan = -1.0 * FrictionVelVeg**3 / TMP1
      MoStabParaAbvCan = min((RefHeightAboveGrd - ZeroPlaneDispSfc) / MoLengthAbvCan, 1.0)
#ifdef LEAN_NOAHMP_T2Z
      ! Lean departure (build switch LEAN_NOAHMP_T2Z, on in flavor lean, off in mpas; docs/fixes.md): MPAS caps z/L at 1 at the first
      ! level and at 2 m apart, from the same L. Past the cap the flux acts as if L = z, while 2 m still sees the true short L, so
      ! as a calm night grows more stable T2 turns back from the surface towards the first level. Here 2 m takes the capped L of
      ! the flux. In unstable air, with no cap, this is the same number. T2 and Q2 only: nothing else reads the 2 m values.
      MoStabParaVeg2m = MoStabParaAbvCan * (2.0 + RoughLenShCanopy) / (RefHeightAboveGrd - ZeroPlaneDispSfc)
#else
      MoStabParaVeg2m = min((2.0 + RoughLenShCanopy) / MoLengthAbvCan, 1.0)
#endif
    endif
    if ( MOZOLD*MoStabParaAbvCan < 0.0 ) MoStabParaSgn = MoStabParaSgn + 1
    if ( MoStabParaSgn >= 2 ) then
      MoStabParaAbvCan = 0.0
      MoStabCorrMomAbvCan = 0.0
      MoStabCorrShAbvCan = 0.0
      MoStabParaVeg2m = 0.0
      MoStabCorrMomVeg2m = 0.0
      MoStabCorrShVeg2m = 0.0
    endif
    if ( MoStabParaAbvCan < 0.0 ) then
      TMP1 = (1.0 - 16.0 * MoStabParaAbvCan)**0.25
      TMP2 = log((1.0 + TMP1*TMP1) / 2.0)
      TMP3 = log((1.0 + TMP1) / 2.0)
      FMNEW = 2.0 * TMP3 + TMP2 - 2.0 * atan(TMP1) + 1.5707963
      FHNEW = 2 * TMP2
      TMP12 = (1.0 - 16.0 * MoStabParaVeg2m)**0.25
      TMP22 = log((1.0 + TMP12*TMP12) / 2.0)
      TMP32 = log((1.0 + TMP12) / 2.0)
      FM2NEW = 2.0 * TMP32 + TMP22 - 2.0 * atan(TMP12) + 1.5707963
      FH2NEW = 2 * TMP22
    else
      FMNEW = -5.0 * MoStabParaAbvCan
      FHNEW = FMNEW
      FM2NEW = -5.0 * MoStabParaVeg2m
      FH2NEW = FM2NEW
    endif
    if ( IterationInd == 1 ) then
      MoStabCorrMomAbvCan = FMNEW
      MoStabCorrShAbvCan = FHNEW
      MoStabCorrMomVeg2m = FM2NEW
      MoStabCorrShVeg2m = FH2NEW
    else
      MoStabCorrMomAbvCan = 0.5 * (MoStabCorrMomAbvCan + FMNEW)
      MoStabCorrShAbvCan = 0.5 * (MoStabCorrShAbvCan + FHNEW)
      MoStabCorrMomVeg2m = 0.5 * (MoStabCorrMomVeg2m + FM2NEW)
      MoStabCorrShVeg2m = 0.5 * (MoStabCorrShVeg2m + FH2NEW)
    endif
    MoStabCorrShAbvCan = min(MoStabCorrShAbvCan , 0.9*TMPCH)
    MoStabCorrMomAbvCan = min(MoStabCorrMomAbvCan, 0.9*TMPCM)
    MoStabCorrShVeg2m = min(MoStabCorrShVeg2m , 0.9*TMPCH2)
    MoStabCorrMomVeg2m = min(MoStabCorrMomVeg2m , 0.9*TMPCM2)
    CMFM = TMPCM - MoStabCorrMomAbvCan
    CHFH = TMPCH - MoStabCorrShAbvCan
    CM2FM2 = TMPCM2 - MoStabCorrMomVeg2m
    CH2FH2 = TMPCH2 - MoStabCorrShVeg2m
    if ( abs(CMFM) <= MPE ) CMFM = MPE
    if ( abs(CHFH) <= MPE ) CHFH = MPE
    if ( abs(CM2FM2) <= MPE ) CM2FM2 = MPE
    if ( abs(CH2FH2) <= MPE ) CH2FH2 = MPE
    ExchCoeffMomAbvCan = ConstVonKarman * ConstVonKarman / (CMFM * CMFM)
    ExchCoeffShAbvCan = ConstVonKarman * ConstVonKarman / (CMFM * CHFH)
    FrictionVelVeg = WindSpdRefHeight * sqrt(ExchCoeffMomAbvCan)
    ExchCoeffSh2mVegMo = ConstVonKarman * FrictionVelVeg / CH2FH2
    ResistanceMomAbvCan = max(1.0, 1.0/(ExchCoeffMomAbvCan*WindSpdRefHeight))
    ResistanceShAbvCan = max(1.0, 1.0/(ExchCoeffShAbvCan*WindSpdRefHeight))
    ResistanceLhAbvCan = ResistanceShAbvCan
    end associate
  end subroutine ResistanceAboveCanopyMOST
end module lean_resistanceabovecanopymostmod

! Also from: physics_noahmp/src/ResistanceAboveCanopyChen97Mod.F90
module lean_resistanceabovecanopychen97mod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceAboveCanopyChen97(noahmp, IterationInd)
    implicit none
    integer , intent(in ) :: IterationInd
    type(noahmp_column) , intent(inout) :: noahmp
    integer :: ILECH, ITR
    real(kind=kind_noahmp) :: ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    real(kind=kind_noahmp) :: XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    real(kind=kind_noahmp) :: ZILFC, ZU, ZT, RDZ, CXCH, DTHV, DU2
    real(kind=kind_noahmp) :: BTGH, ZSLU, ZSLT, RLOGU, RLOGT, RLMA
    real(kind=kind_noahmp) :: ZETALT, ZETALU, ZETAU, ZETAT, XLU4
    real(kind=kind_noahmp) :: XLT4, XU4, XT4, XLU, XLT, XU, XT
    real(kind=kind_noahmp) :: PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN
    integer , parameter :: ITRMX = 5
    real(kind=kind_noahmp), parameter :: WWST = 1.2
    real(kind=kind_noahmp), parameter :: WWST2 = WWST * WWST
    real(kind=kind_noahmp), parameter :: VKRM = 0.40
    real(kind=kind_noahmp), parameter :: EXCM = 0.001
    real(kind=kind_noahmp), parameter :: BETA = 1.0 / 270.0
    real(kind=kind_noahmp), parameter :: BTG = BETA * ConstGravityAcc
    real(kind=kind_noahmp), parameter :: ELFC = VKRM * BTG
    real(kind=kind_noahmp), parameter :: WOLD = 0.15
    real(kind=kind_noahmp), parameter :: WNEW = 1.0 - WOLD
    real(kind=kind_noahmp), parameter :: PIHF = 3.14159265 / 2.0
    real(kind=kind_noahmp), parameter :: EPSU2 = 1.0e-4
    real(kind=kind_noahmp), parameter :: EPSUST = 0.07
    real(kind=kind_noahmp), parameter :: EPSIT = 1.0e-4
    real(kind=kind_noahmp), parameter :: EPSA = 1.0e-8
    real(kind=kind_noahmp), parameter :: ZTMIN = -5.0
    real(kind=kind_noahmp), parameter :: ZTMAX = 1.0
    real(kind=kind_noahmp), parameter :: HPBL = 1000.0
    real(kind=kind_noahmp), parameter :: SQVISC = 258.2
    real(kind=kind_noahmp), parameter :: RIC = 0.183
    real(kind=kind_noahmp), parameter :: RRIC = 1.0 / RIC
    real(kind=kind_noahmp), parameter :: FHNEU = 0.8
    real(kind=kind_noahmp), parameter :: RFC = 0.191
    real(kind=kind_noahmp), parameter :: RFAC = RIC / ( FHNEU * RFC * RFC )
    PSLMU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLMS(ZZ) = ZZ * RRIC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSLHU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLHS(ZZ) = ZZ * RFAC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSPMU(XX) = -2.0*log( (XX+1.0)*0.5 ) - log( (XX*XX+1.0)*0.5 ) + 2.0*atan(XX) - PIHF
    PSPMS(YY) = 5.0 * YY
    PSPHU(XX) = -2.0 * log( (XX*XX + 1.0)*0.5 )
    PSPHS(YY) = 5.0 * YY
    associate( ZilitinkevichCoeff => noahmp%energy_param_ZilitinkevichCoeff , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd , &
      & TemperaturePotRefHeight => noahmp%energy_state_TemperaturePotRefHeight , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & RoughLenMomSfc => noahmp%energy_state_RoughLenMomSfc , &
      & TemperatureCanopyAir => noahmp%energy_state_TemperatureCanopyAir , &
      & ExchCoeffMomAbvCan => noahmp%energy_state_ExchCoeffMomAbvCan , &
      & ExchCoeffShAbvCan => noahmp%energy_state_ExchCoeffShAbvCan , &
      & MoStabParaAbvCan => noahmp%energy_state_MoStabParaAbvCan , &
      & FrictionVelVertVeg => noahmp%energy_state_FrictionVelVertVeg , &
      & FrictionVelVeg => noahmp%energy_state_FrictionVelVeg , &
      & ResistanceMomAbvCan => noahmp%energy_state_ResistanceMomAbvCan , &
      & ResistanceShAbvCan => noahmp%energy_state_ResistanceShAbvCan , &
      & ResistanceLhAbvCan => noahmp%energy_state_ResistanceLhAbvCan )
    ILECH = 0
    ZILFC = -ZilitinkevichCoeff * VKRM * SQVISC
    ZU = RoughLenMomSfc
    RDZ = 1.0 / RefHeightAboveGrd
    CXCH = EXCM * RDZ
    DTHV = TemperaturePotRefHeight - TemperatureCanopyAir
    DU2 = max(WindSpdRefHeight*WindSpdRefHeight, EPSU2)
    BTGH = BTG * HPBL
    if ( IterationInd == 1 ) then
      if ( (BTGH*ExchCoeffShAbvCan*DTHV) /= 0.0 ) then
        FrictionVelVertVeg = WWST2 * abs(BTGH*ExchCoeffShAbvCan*DTHV)**(2.0/3.0)
      else
        FrictionVelVertVeg = 0.0
      endif
      FrictionVelVeg = max(sqrt(ExchCoeffMomAbvCan*sqrt(DU2+FrictionVelVertVeg)), EPSUST)
      MoStabParaAbvCan = ELFC * ExchCoeffShAbvCan * DTHV / FrictionVelVeg**3
    endif
    ZT = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelVeg*RoughLenMomSfc))*RoughLenMomSfc)
    ZSLU = RefHeightAboveGrd + ZU
    ZSLT = RefHeightAboveGrd + ZT
    RLOGU = log(ZSLU/ZU)
    RLOGT = log(ZSLT/ZT)
    ZETALT = max(ZSLT*MoStabParaAbvCan, ZTMIN)
    MoStabParaAbvCan = ZETALT / ZSLT
    ZETALU = ZSLU * MoStabParaAbvCan
    ZETAU = ZU * MoStabParaAbvCan
    ZETAT = ZT * MoStabParaAbvCan
    if ( ILECH == 0 ) then
      if ( MoStabParaAbvCan < 0.0 ) then
        XLU4 = 1.0 - 16.0 * ZETALU
        XLT4 = 1.0 - 16.0 * ZETALT
        XU4 = 1.0 - 16.0 * ZETAU
        XT4 = 1.0 - 16.0 * ZETAT
        XLU = sqrt(sqrt(XLU4))
        XLT = sqrt(sqrt(XLT4))
        XU = sqrt(sqrt(XU4))
        XT = sqrt(sqrt(XT4))
        PSMZ = PSPMU(XU)
        SIMM = PSPMU(XLU) - PSMZ + RLOGU
        PSHZ = PSPHU(XT)
        SIMH = PSPHU(XLT) - PSHZ + RLOGT
      else
        ZETALU = min(ZETALU, ZTMAX)
        ZETALT = min(ZETALT, ZTMAX)
        ZETAU = min(ZETAU, ZTMAX/(ZSLU/ZU))
        ZETAT = min(ZETAT, ZTMAX/(ZSLT/ZT))
        PSMZ = PSPMS(ZETAU)
        SIMM = PSPMS(ZETALU) - PSMZ + RLOGU
        PSHZ = PSPHS(ZETAT)
        SIMH = PSPHS(ZETALT) - PSHZ + RLOGT
      endif
    else
      if ( MoStabParaAbvCan < 0.0 ) then
        PSMZ = PSLMU(ZETAU)
        SIMM = PSLMU(ZETALU) - PSMZ + RLOGU
        PSHZ = PSLHU(ZETAT)
        SIMH = PSLHU(ZETALT) - PSHZ + RLOGT
      else
        ZETALU = min(ZETALU, ZTMAX)
        ZETALT = min(ZETALT, ZTMAX)
        PSMZ = PSLMS(ZETAU)
        SIMM = PSLMS(ZETALU) - PSMZ + RLOGU
        PSHZ = PSLHS(ZETAT)
        SIMH = PSLHS(ZETALT) - PSHZ + RLOGT
      endif
    endif
    FrictionVelVeg = max(sqrt(ExchCoeffMomAbvCan*sqrt(DU2+FrictionVelVertVeg)), EPSUST)
    ZT = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelVeg*RoughLenMomSfc))*RoughLenMomSfc)
    ZSLT = RefHeightAboveGrd + ZT
    RLOGT = log(ZSLT/ZT)
    USTARK = FrictionVelVeg * VKRM
    if ( SIMM < 1.0e-6 ) SIMM = 1.0e-6
    ExchCoeffMomAbvCan = max(USTARK/SIMM, CXCH)
    if ( SIMH < 1.0e-6 ) SIMH = 1.0e-6
    ExchCoeffShAbvCan = max(USTARK/SIMH, CXCH)
    if ( (BTGH*ExchCoeffShAbvCan*DTHV) /= 0.0 ) then
      FrictionVelVertVeg = WWST2 * abs(BTGH*ExchCoeffShAbvCan*DTHV)**(2.0/3.0)
    else
      FrictionVelVertVeg = 0.0
    endif
    RLMN = ELFC * ExchCoeffShAbvCan * DTHV / FrictionVelVeg**3
    RLMA = MoStabParaAbvCan * WOLD + RLMN * WNEW
    MoStabParaAbvCan = RLMA
    ExchCoeffShAbvCan = ExchCoeffShAbvCan / WindSpdRefHeight
    ExchCoeffMomAbvCan = ExchCoeffMomAbvCan / WindSpdRefHeight
    ResistanceMomAbvCan = max(1.0, 1.0/(ExchCoeffMomAbvCan*WindSpdRefHeight))
    ResistanceShAbvCan = max(1.0, 1.0/(ExchCoeffShAbvCan*WindSpdRefHeight))
    ResistanceLhAbvCan = ResistanceShAbvCan
    end associate
  end subroutine ResistanceAboveCanopyChen97
end module lean_resistanceabovecanopychen97mod

! Source: physics_noahmp/src/ResistanceLeafToGroundMod.F90
module lean_resistanceleaftogroundmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceLeafToGround(noahmp, IndIter, VegAreaIndEff, HeatSenGrdTmp)
    implicit none
    integer , intent(in ) :: IndIter
    real(kind=kind_noahmp), intent(in ) :: HeatSenGrdTmp
    real(kind=kind_noahmp), intent(in ) :: VegAreaIndEff
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp) :: MPE
    real(kind=kind_noahmp) :: KH
    real(kind=kind_noahmp) :: TMP1
    real(kind=kind_noahmp) :: TMP2
    real(kind=kind_noahmp) :: TMPRAH2
    real(kind=kind_noahmp) :: TMPRB
    real(kind=kind_noahmp) :: FHGNEW
    associate( LeafDimLength => noahmp%energy_param_LeafDimLength , &
      & CanopyWindExtFac => noahmp%energy_param_CanopyWindExtFac , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & TemperatureCanopyAir => noahmp%energy_state_TemperatureCanopyAir , &
      & ZeroPlaneDispSfc => noahmp%energy_state_ZeroPlaneDispSfc , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , CanopyHeight => noahmp%energy_state_CanopyHeight , &
      & WindSpdCanopyTop => noahmp%energy_state_WindSpdCanopyTop , &
      & RoughLenShCanopy => noahmp%energy_state_RoughLenShCanopy , &
      & RoughLenShVegGrd => noahmp%energy_state_RoughLenShVegGrd , &
      & FrictionVelVeg => noahmp%energy_state_FrictionVelVeg , &
      & MoStabCorrShUndCan => noahmp%energy_state_MoStabCorrShUndCan , &
      & WindExtCoeffCanopy => noahmp%energy_state_WindExtCoeffCanopy , &
      & MoStabParaUndCan => noahmp%energy_state_MoStabParaUndCan , &
      & MoLengthUndCan => noahmp%energy_state_MoLengthUndCan , &
      & ResistanceMomUndCan => noahmp%energy_state_ResistanceMomUndCan , &
      & ResistanceShUndCan => noahmp%energy_state_ResistanceShUndCan , &
      & ResistanceLhUndCan => noahmp%energy_state_ResistanceLhUndCan , &
      & ResistanceLeafBoundary => noahmp%energy_state_ResistanceLeafBoundary )
    MPE = 1.0e-6
    MoStabParaUndCan = 0.0
    MoLengthUndCan = 0.0
    if ( IndIter > 1 ) then
      TMP1 = ConstVonKarman * (ConstGravityAcc / TemperatureCanopyAir) * HeatSenGrdTmp / (DensityAirRefHeight * ConstHeatCapacAir)
      if ( abs(TMP1) <= MPE ) TMP1 = MPE
      MoLengthUndCan = -1.0 * FrictionVelVeg**3 / TMP1
      MoStabParaUndCan = min((ZeroPlaneDispSfc-RoughLenMomGrd)/MoLengthUndCan, 1.0)
    endif
    if ( MoStabParaUndCan < 0.0 ) then
      FHGNEW = (1.0 - 15.0 * MoStabParaUndCan)**(-0.25)
    else
      FHGNEW = 1.0 + 4.7 * MoStabParaUndCan
    endif
    if ( IndIter == 1 ) then
      MoStabCorrShUndCan = FHGNEW
    else
      MoStabCorrShUndCan = 0.5 * (MoStabCorrShUndCan + FHGNEW)
    endif
    WindExtCoeffCanopy = (CanopyWindExtFac * VegAreaIndEff * CanopyHeight * MoStabCorrShUndCan)**0.5
    TMP1 = exp(-WindExtCoeffCanopy * RoughLenShVegGrd / CanopyHeight)
    TMP2 = exp(-WindExtCoeffCanopy * (RoughLenShCanopy + ZeroPlaneDispSfc) / CanopyHeight)
    TMPRAH2 = CanopyHeight * exp(WindExtCoeffCanopy) / WindExtCoeffCanopy * (TMP1-TMP2)
    KH = max(ConstVonKarman*FrictionVelVeg*(CanopyHeight-ZeroPlaneDispSfc), MPE)
    ResistanceMomUndCan = 0.0
    ResistanceShUndCan = TMPRAH2 / KH
    ResistanceLhUndCan = ResistanceShUndCan
    TMPRB = WindExtCoeffCanopy * 50.0 / (1.0 - exp(-WindExtCoeffCanopy/2.0))
    ResistanceLeafBoundary = TMPRB * sqrt(LeafDimLength / WindSpdCanopyTop)
    ResistanceLeafBoundary = min(max(ResistanceLeafBoundary, 5.0), 50.0)
    end associate
  end subroutine ResistanceLeafToGround
end module lean_resistanceleaftogroundmod

! Source: physics_noahmp/src/ResistanceCanopyStomataBallBerryMod.F90
module lean_resistancecanopystomataballberrymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
    implicit none
    integer , intent(in ) :: IndexShade
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndIter
    integer, parameter :: NumIter = 3
    real(kind=kind_noahmp) :: RadPhotoActAbsTmp
    real(kind=kind_noahmp) :: ResistanceStomataTmp
    real(kind=kind_noahmp) :: PhotosynLeafTmp
    real(kind=kind_noahmp) :: NitrogenFoliageFac
    real(kind=kind_noahmp) :: CarboxylRateMax
    real(kind=kind_noahmp) :: MPE
    real(kind=kind_noahmp) :: RLB
    real(kind=kind_noahmp) :: TC
    real(kind=kind_noahmp) :: CS
    real(kind=kind_noahmp) :: KC
    real(kind=kind_noahmp) :: KO
    real(kind=kind_noahmp) :: A,B,C,Q
    real(kind=kind_noahmp) :: R1,R2
    real(kind=kind_noahmp) :: PPF
    real(kind=kind_noahmp) :: WC
    real(kind=kind_noahmp) :: WJ
    real(kind=kind_noahmp) :: WE
    real(kind=kind_noahmp) :: CP
    real(kind=kind_noahmp) :: CI
    real(kind=kind_noahmp) :: AWC
    real(kind=kind_noahmp) :: J
    real(kind=kind_noahmp) :: CEA
    real(kind=kind_noahmp) :: CF
    real(kind=kind_noahmp) :: T
    real(kind=kind_noahmp) :: F1
    real(kind=kind_noahmp) :: F2
    real(kind=kind_noahmp) :: AB
    real(kind=kind_noahmp) :: BC
    F1(AB, BC) = AB**( (BC - 25.0) / 10.0 )
    F2(AB) = 1.0 + exp( (-2.2e05 + 710.0 * (AB + 273.16)) / (8.314 * (AB + 273.16)) )
    associate( PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & SoilTranspFacAcc => noahmp%water_state_SoilTranspFacAcc , &
      & IndexGrowSeason => noahmp%biochem_state_IndexGrowSeason , &
      & NitrogenConcFoliage => noahmp%biochem_state_NitrogenConcFoliage , &
      & NitrogenConcFoliageMax => noahmp%biochem_param_NitrogenConcFoliageMax , &
      & QuantumEfficiency25C => noahmp%biochem_param_QuantumEfficiency25C , &
      & CarboxylRateMax25C => noahmp%biochem_param_CarboxylRateMax25C , &
      & CarboxylRateMaxQ10 => noahmp%biochem_param_CarboxylRateMaxQ10 , &
      & PhotosynPathC3 => noahmp%biochem_param_PhotosynPathC3 , &
      & SlopeConductToPhotosyn => noahmp%biochem_param_SlopeConductToPhotosyn , &
      & Co2MmConst25C => noahmp%energy_param_Co2MmConst25C , O2MmConst25C => noahmp%energy_param_O2MmConst25C , &
      & Co2MmConstQ10 => noahmp%energy_param_Co2MmConstQ10 , O2MmConstQ10 => noahmp%energy_param_O2MmConstQ10 , &
      & ConductanceLeafMin => noahmp%energy_param_ConductanceLeafMin , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & VapPresSatCanopy => noahmp%energy_state_VapPresSatCanopy , &
      & PressureVaporCanAir => noahmp%energy_state_PressureVaporCanAir , &
      & PressureAtmosO2 => noahmp%energy_state_PressureAtmosO2 , &
      & PressureAtmosCO2 => noahmp%energy_state_PressureAtmosCO2 , &
      & ResistanceLeafBoundary => noahmp%energy_state_ResistanceLeafBoundary , &
      & VegFrac => noahmp%energy_state_VegFrac , &
      & RadPhotoActAbsSunlit => noahmp%energy_flux_RadPhotoActAbsSunlit , &
      & RadPhotoActAbsShade => noahmp%energy_flux_RadPhotoActAbsShade , &
      & ResistanceStomataSunlit => noahmp%energy_state_ResistanceStomataSunlit , &
      & ResistanceStomataShade => noahmp%energy_state_ResistanceStomataShade , &
      & PhotosynLeafSunlit => noahmp%biochem_flux_PhotosynLeafSunlit , &
      & PhotosynLeafShade => noahmp%biochem_flux_PhotosynLeafShade )
    MPE = 1.0e-6
    CF = PressureAirRefHeight / (8.314 * TemperatureAirRefHeight) * 1.0e06
    ResistanceStomataTmp = 1.0 / ConductanceLeafMin * CF
    PhotosynLeafTmp = 0.0
    if ( IndexShade == 0 ) RadPhotoActAbsTmp = RadPhotoActAbsSunlit / max(VegFrac,1.0e-6)
    if ( IndexShade == 1 ) RadPhotoActAbsTmp = RadPhotoActAbsShade / max(VegFrac,1.0e-6)
    if ( RadPhotoActAbsTmp > 0.0 ) then
      NitrogenFoliageFac = min(NitrogenConcFoliage/max(MPE, NitrogenConcFoliageMax), 1.0)
      TC = TemperatureCanopy - ConstFreezePoint
      PPF = 4.6 * RadPhotoActAbsTmp
      J = PPF * QuantumEfficiency25C
      KC = Co2MmConst25C * F1(Co2MmConstQ10, TC)
      KO = O2MmConst25C * F1(O2MmConstQ10, TC)
      AWC = KC * ( 1.0 + PressureAtmosO2 / KO )
      CP = 0.5 * KC / KO * PressureAtmosO2 * 0.21
      CarboxylRateMax = CarboxylRateMax25C / F2(TC) * NitrogenFoliageFac * SoilTranspFacAcc * F1(CarboxylRateMaxQ10, TC)
      CI = 0.7 * PressureAtmosCO2 * PhotosynPathC3 + 0.4 * PressureAtmosCO2 * (1.0 - PhotosynPathC3)
      RLB = ResistanceLeafBoundary / CF
      CEA = max(0.25*VapPresSatCanopy*PhotosynPathC3 + 0.40*VapPresSatCanopy*(1.0-PhotosynPathC3), &
        & min(PressureVaporCanAir,VapPresSatCanopy))
      do IndIter = 1, NumIter
        WJ = max(CI-CP, 0.0) * J / (CI + 2.0*CP) * PhotosynPathC3 + J * (1.0 - PhotosynPathC3)
        WC = max(CI-CP, &
          & 0.0) * CarboxylRateMax / (CI + AWC) * PhotosynPathC3 + CarboxylRateMax * (1.0 - PhotosynPathC3)
        WE = 0.5 * CarboxylRateMax * PhotosynPathC3 + 4000.0 * CarboxylRateMax * CI / PressureAirRefHeight * (1.0 - PhotosynPathC3)
        PhotosynLeafTmp = min(WJ, WC, WE) * IndexGrowSeason
        CS = max(PressureAtmosCO2-1.37*RLB*PressureAirRefHeight*PhotosynLeafTmp, MPE)
        A = SlopeConductToPhotosyn * PhotosynLeafTmp * PressureAirRefHeight * CEA / (CS * VapPresSatCanopy) + ConductanceLeafMin
        B = (SlopeConductToPhotosyn * PhotosynLeafTmp * PressureAirRefHeight / CS + ConductanceLeafMin) * RLB - 1.0
        C = -RLB
        if ( B >= 0.0 ) then
          Q = -0.5 * (B + sqrt(B*B-4.0*A*C))
        else
          Q = -0.5 * (B - sqrt(B*B-4.0*A*C))
        endif
        R1 = Q / A
        R2 = C / Q
        ResistanceStomataTmp = max(R1, R2)
        CI = max(CS-PhotosynLeafTmp*PressureAirRefHeight*1.65*ResistanceStomataTmp, 0.0)
      enddo
      ResistanceStomataTmp = ResistanceStomataTmp * CF
    endif
    if ( IndexShade == 0 ) then
      ResistanceStomataSunlit = ResistanceStomataTmp
      PhotosynLeafSunlit = PhotosynLeafTmp
    endif
    if ( IndexShade == 1 ) then
      ResistanceStomataShade = ResistanceStomataTmp
      PhotosynLeafShade = PhotosynLeafTmp
    endif
    end associate
  end subroutine ResistanceCanopyStomataBallBerry
end module lean_resistancecanopystomataballberrymod

! Source: physics_noahmp/src/SurfaceEnergyFluxVegetatedMod.F90
module lean_surfaceenergyfluxvegetatedmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_vaporpressuresaturationmod, only : VaporPressureSaturation
  use lean_resistanceabovecanopymostmod, only : ResistanceAboveCanopyMOST
  use lean_resistanceabovecanopychen97mod, only : ResistanceAboveCanopyChen97
  use lean_resistanceleaftogroundmod, only : ResistanceLeafToGround
  use lean_resistancecanopystomataballberrymod, only : ResistanceCanopyStomataBallBerry
  implicit none
  contains
  subroutine SurfaceEnergyFluxVegetated(noahmp)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer :: IndIter
    integer :: LastIter
    integer :: MoStabParaSgn
    integer :: IndexShade
    integer, parameter :: NumIterC = 20
    integer, parameter :: NumIterG = 5
    real(kind=kind_noahmp) :: ExchCoeffShAbvCanTmp
    real(kind=kind_noahmp) :: TemperatureCanChg
    real(kind=kind_noahmp) :: TemperatureGrdChg
    real(kind=kind_noahmp) :: LwCoeffAir
    real(kind=kind_noahmp) :: LwCoeffCan
    real(kind=kind_noahmp) :: ShCoeff
    real(kind=kind_noahmp) :: LhCoeff
    real(kind=kind_noahmp) :: GrdHeatCoeff
    real(kind=kind_noahmp) :: TranspHeatCoeff
    real(kind=kind_noahmp) :: TempShGhTmp
    real(kind=kind_noahmp) :: ExchCoeffShFrac
    real(kind=kind_noahmp) :: VapPresLhTot
    real(kind=kind_noahmp) :: ExchCoeffEtFrac
    real(kind=kind_noahmp) :: VapPresSatWatTmp
    real(kind=kind_noahmp) :: VapPresSatIceTmp
    real(kind=kind_noahmp) :: VapPresSatWatTmpD
    real(kind=kind_noahmp) :: VapPresSatIceTmpD
    real(kind=kind_noahmp) :: FluxTotCoeff
    real(kind=kind_noahmp) :: EnergyResTmp
    real(kind=kind_noahmp) :: ExchCoeffShLeafTmp
    real(kind=kind_noahmp) :: ExchCoeffTot
    real(kind=kind_noahmp) :: ShCanTmp
    real(kind=kind_noahmp) :: ShGrdTmp
    real(kind=kind_noahmp) :: MoistureFluxSfc
    real(kind=kind_noahmp) :: VegAreaIndTmp
    real(kind=kind_noahmp) :: LeafAreaIndSunEff
    real(kind=kind_noahmp) :: LeafAreaIndShdEff
    real(kind=kind_noahmp) :: TempTmp
    real(kind=kind_noahmp) :: TempUnitConv
    real(kind=kind_noahmp) :: HeatCapacCan
    TempUnitConv(TempTmp) = min(50.0, max(-50.0, (TempTmp - ConstFreezePoint)))
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & GridIndexI => noahmp%config_domain_GridIndexI , GridIndexJ => noahmp%config_domain_GridIndexJ , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , OptSurfaceDrag => noahmp_opt_sfc , &
      & OptStomataResistance => 1 , OptSnowSoilTempTime => 1 , &
      & WindEastwardRefHeight => noahmp%forcing_WindEastwardRefHeight , &
      & WindNorthwardRefHeight => noahmp%forcing_WindNorthwardRefHeight , &
      & RadLwDownRefHeight => noahmp%forcing_RadLwDownRefHeight , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & PressureAirSurface => noahmp%forcing_PressureAirSurface , SnowDepth => noahmp%water_state_SnowDepth , &
      & SnowCoverFrac => noahmp%water_state_SnowCoverFrac , CanopyWetFrac => noahmp%water_state_CanopyWetFrac , &
      & CanopyLiqWater => noahmp%water_state_CanopyLiqWater , CanopyIce => noahmp%water_state_CanopyIce , &
      & HeightCanopyTop => noahmp%energy_param_HeightCanopyTop , &
      & ZilitinkevichCoeff => noahmp%energy_param_ZilitinkevichCoeff , &
      & HeatCapacCanFac => noahmp%energy_param_HeatCapacCanFac , RadSwAbsVeg => noahmp%energy_flux_RadSwAbsVeg , &
      & RadSwAbsGrd => noahmp%energy_flux_RadSwAbsGrd , &
      & HeatPrecipAdvCanopy => noahmp%energy_flux_HeatPrecipAdvCanopy , &
      & HeatPrecipAdvVegGrd => noahmp%energy_flux_HeatPrecipAdvVegGrd , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd , VegFrac => noahmp%energy_state_VegFrac , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & PressureVaporRefHeight => noahmp%energy_state_PressureVaporRefHeight , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & VegAreaIndEff => noahmp%energy_state_VegAreaIndEff , &
      & LeafAreaIndSunlit => noahmp%energy_state_LeafAreaIndSunlit , &
      & LeafAreaIndShade => noahmp%energy_state_LeafAreaIndShade , &
      & ZeroPlaneDispSfc => noahmp%energy_state_ZeroPlaneDispSfc , &
      & RoughLenMomSfc => noahmp%energy_state_RoughLenMomSfc , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & EmissivityVeg => noahmp%energy_state_EmissivityVeg , EmissivityGrd => noahmp%energy_state_EmissivityGrd , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & ThermConductSoilSnow => noahmp%energy_state_ThermConductSoilSnow , &
      & ResistanceGrdEvap => noahmp%energy_state_ResistanceGrdEvap , &
      & PsychConstCanopy => noahmp%energy_state_PsychConstCanopy , &
      & LatHeatVapCanopy => noahmp%energy_state_LatHeatVapCanopy , &
      & PsychConstGrd => noahmp%energy_state_PsychConstGrd , &
      & RelHumidityGrd => noahmp%energy_state_RelHumidityGrd , &
      & SpecHumiditySfc => noahmp%energy_state_SpecHumiditySfc , &
      & PressureVaporCanAir => noahmp%energy_state_PressureVaporCanAir , &
      & TemperatureCanopyAir => noahmp%energy_state_TemperatureCanopyAir , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & TemperatureGrdVeg => noahmp%energy_state_TemperatureGrdVeg , &
      & ExchCoeffMomAbvCan => noahmp%energy_state_ExchCoeffMomAbvCan , &
      & ExchCoeffShAbvCan => noahmp%energy_state_ExchCoeffShAbvCan , &
      & WindStressEwVeg => noahmp%energy_state_WindStressEwVeg , &
      & WindStressNsVeg => noahmp%energy_state_WindStressNsVeg , &
      & TemperatureAir2mVeg => noahmp%energy_state_TemperatureAir2mVeg , &
      & ExchCoeffShLeaf => noahmp%energy_state_ExchCoeffShLeaf , &
      & ExchCoeffShUndCan => noahmp%energy_state_ExchCoeffShUndCan , &
      & ExchCoeffSh2mVeg => noahmp%energy_state_ExchCoeffSh2mVeg , &
      & SpecHumidity2mVeg => noahmp%energy_state_SpecHumidity2mVeg , &
      & ResistanceStomataSunlit => noahmp%energy_state_ResistanceStomataSunlit , &
      & ResistanceStomataShade => noahmp%energy_state_ResistanceStomataShade , &
      & FrictionVelVeg => noahmp%energy_state_FrictionVelVeg , &
      & RoughLenShCanopy => noahmp%energy_state_RoughLenShCanopy , &
      & RoughLenShVegGrd => noahmp%energy_state_RoughLenShVegGrd , &
      & ResistanceLeafBoundary => noahmp%energy_state_ResistanceLeafBoundary , &
      & ResistanceShAbvCan => noahmp%energy_state_ResistanceShAbvCan , &
      & ResistanceLhAbvCan => noahmp%energy_state_ResistanceLhAbvCan , &
      & ResistanceShUndCan => noahmp%energy_state_ResistanceShUndCan , &
      & ResistanceLhUndCan => noahmp%energy_state_ResistanceLhUndCan , &
      & ExchCoeffLhAbvCan => noahmp%energy_state_ExchCoeffLhAbvCan , &
      & ExchCoeffLhTransp => noahmp%energy_state_ExchCoeffLhTransp , &
      & ExchCoeffLhEvap => noahmp%energy_state_ExchCoeffLhEvap , &
      & ExchCoeffLhUndCan => noahmp%energy_state_ExchCoeffLhUndCan , &
      & VapPresSatCanopy => noahmp%energy_state_VapPresSatCanopy , &
      & VapPresSatGrdVeg => noahmp%energy_state_VapPresSatGrdVeg , &
      & VapPresSatCanTempD => noahmp%energy_state_VapPresSatCanTempD , &
      & VapPresSatGrdVegTempD => noahmp%energy_state_VapPresSatGrdVegTempD , &
      & CanopyHeight => noahmp%energy_state_CanopyHeight , &
      & WindSpdCanopyTop => noahmp%energy_state_WindSpdCanopyTop , &
      & MoStabParaAbvCan => noahmp%energy_state_MoStabParaAbvCan , &
      & MoStabCorrShVeg2m => noahmp%energy_state_MoStabCorrShVeg2m , &
      & RadLwNetCanopy => noahmp%energy_flux_RadLwNetCanopy , &
      & HeatSensibleCanopy => noahmp%energy_flux_HeatSensibleCanopy , &
      & HeatLatentCanEvap => noahmp%energy_flux_HeatLatentCanEvap , &
      & RadLwNetVegGrd => noahmp%energy_flux_RadLwNetVegGrd , &
      & HeatSensibleVegGrd => noahmp%energy_flux_HeatSensibleVegGrd , &
      & HeatLatentVegGrd => noahmp%energy_flux_HeatLatentVegGrd , &
      & HeatLatentCanTransp => noahmp%energy_flux_HeatLatentCanTransp , &
      & HeatCanStorageChg => noahmp%energy_flux_HeatCanStorageChg , &
      & HeatGroundVegGrd => noahmp%energy_flux_HeatGroundVegGrd )
    LastIter = 0
    FrictionVelVeg = 0.1
    TemperatureCanChg = 0.0
    TemperatureGrdChg = 0.0
    MoStabParaAbvCan = 0.0
    MoStabParaSgn = 0
    MoStabCorrShVeg2m = 0.0
    ShGrdTmp = 0.0
    ShCanTmp = 0.0
    MoistureFluxSfc = 0.0
    VegAreaIndTmp = min(6.0, VegAreaIndEff)
    LeafAreaIndSunEff = min(6.0, LeafAreaIndSunlit)
    LeafAreaIndShdEff = min(6.0, LeafAreaIndShade)
    TempTmp = TempUnitConv(TemperatureGrdVeg)
    call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, &
      & VapPresSatIceTmpD)
    if ( TempTmp > 0.0 ) then
      VapPresSatGrdVeg = VapPresSatWatTmp
    else
      VapPresSatGrdVeg = VapPresSatIceTmp
    endif
    CanopyHeight = HeightCanopyTop
    WindSpdCanopyTop = WindSpdRefHeight * log((CanopyHeight - ZeroPlaneDispSfc + RoughLenMomSfc)/RoughLenMomSfc) / log(RefHeightAboveGrd/RoughLenMomSfc)
    if ( (CanopyHeight-ZeroPlaneDispSfc) <= 0.0 ) then
      print*, "CRITICAL PROBLEM: CanopyHeight <= ZeroPlaneDispSfc"
      print*, "GridIndexI,GridIndexJ = ", GridIndexI, GridIndexJ
      print*, "CanopyHeight = " , CanopyHeight
      print*, "ZeroPlaneDispSfc = " , ZeroPlaneDispSfc
      print*, "SnowDepth = " , SnowDepth
      stop "Error: ZeroPlaneDisp problem in NoahMP LSM"
    endif
    LwCoeffAir = -EmissivityVeg * (1.0 + (1.0-EmissivityVeg)*(1.0-EmissivityGrd)) * RadLwDownRefHeight - EmissivityVeg * EmissivityGrd * ConstStefanBoltzmann * TemperatureGrdVeg**4
    LwCoeffCan = (2.0 - EmissivityVeg * (1.0-EmissivityGrd)) * EmissivityVeg * ConstStefanBoltzmann
    loop1: do IndIter = 1, NumIterC
    if ( IndIter == 1 ) then
      RoughLenShCanopy = RoughLenMomSfc
      RoughLenShVegGrd = RoughLenMomGrd
    else
      RoughLenShCanopy = RoughLenMomSfc
      RoughLenShVegGrd = RoughLenMomGrd
    endif
 if (noahmp_opt_sfc == 2) then
    call ResistanceAboveCanopyChen97(noahmp, IndIter)
 else
    call ResistanceAboveCanopyMOST(noahmp, IndIter, ShCanTmp, MoStabParaSgn)
 end if
    call ResistanceLeafToGround(noahmp, IndIter, VegAreaIndTmp, ShGrdTmp)
    TempTmp = TempUnitConv(TemperatureCanopy)
    call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, &
      & VapPresSatIceTmpD)
    if ( TempTmp > 0.0 ) then
      VapPresSatCanopy = VapPresSatWatTmp
      VapPresSatCanTempD = VapPresSatWatTmpD
    else
      VapPresSatCanopy = VapPresSatIceTmp
      VapPresSatCanTempD = VapPresSatIceTmpD
    endif
    if ( IndIter == 1 ) then
      IndexShade = 0
      call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
      IndexShade = 1
      call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
    endif
    ExchCoeffShAbvCanTmp = 1.0 / ResistanceShAbvCan
    ExchCoeffShLeafTmp = 2.0 * VegAreaIndTmp / ResistanceLeafBoundary
    GrdHeatCoeff = 1.0 / ResistanceShUndCan
    ExchCoeffTot = ExchCoeffShAbvCanTmp + ExchCoeffShLeafTmp + GrdHeatCoeff
    TempShGhTmp = (TemperatureAirRefHeight*ExchCoeffShAbvCanTmp + TemperatureGrdVeg*GrdHeatCoeff) / ExchCoeffTot
    ExchCoeffShFrac = ExchCoeffShLeafTmp / ExchCoeffTot
    ShCoeff = (1.0 - ExchCoeffShFrac) * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffShLeafTmp
    ExchCoeffLhAbvCan = 1.0 / ResistanceLhAbvCan
    ExchCoeffLhEvap = CanopyWetFrac * VegAreaIndTmp / ResistanceLeafBoundary
    ExchCoeffLhTransp = (1.0 - CanopyWetFrac) * (LeafAreaIndSunEff/(ResistanceLeafBoundary+ResistanceStomataSunlit) + LeafAreaIndShdEff/(ResistanceLeafBoundary+ResistanceStomataShade))
    ExchCoeffLhUndCan = 1.0 / (ResistanceLhUndCan + ResistanceGrdEvap)
    ExchCoeffTot = ExchCoeffLhAbvCan + ExchCoeffLhEvap + ExchCoeffLhTransp + ExchCoeffLhUndCan
    VapPresLhTot = (PressureVaporRefHeight*ExchCoeffLhAbvCan + VapPresSatGrdVeg*ExchCoeffLhUndCan ) / ExchCoeffTot
    ExchCoeffEtFrac = (ExchCoeffLhEvap + ExchCoeffLhTransp) / ExchCoeffTot
    LhCoeff = (1.0 - ExchCoeffEtFrac) * ExchCoeffLhEvap * DensityAirRefHeight * ConstHeatCapacAir / PsychConstCanopy
    TranspHeatCoeff = (1.0 - ExchCoeffEtFrac) * ExchCoeffLhTransp * DensityAirRefHeight * ConstHeatCapacAir / PsychConstCanopy
    TemperatureCanopyAir = TempShGhTmp + ExchCoeffShFrac * TemperatureCanopy
    PressureVaporCanAir = VapPresLhTot + ExchCoeffEtFrac * VapPresSatCanopy
    RadLwNetCanopy = VegFrac * (LwCoeffAir + LwCoeffCan * TemperatureCanopy**4)
    HeatSensibleCanopy = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffShLeafTmp * (TemperatureCanopy - TemperatureCanopyAir)
    HeatLatentCanEvap = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhEvap * (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy
    HeatLatentCanTransp = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhTransp * (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy
    if ( TemperatureCanopy > ConstFreezePoint ) then
      HeatLatentCanEvap = min(CanopyLiqWater*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap)
    else
      HeatLatentCanEvap = min(CanopyIce*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap)
    endif
    HeatCapacCan = HeatCapacCanFac*VegAreaIndTmp*ConstHeatCapacWater + CanopyLiqWater*ConstHeatCapacWater/ConstDensityWater + CanopyIce*ConstHeatCapacIce/ConstDensityIce
    EnergyResTmp = RadSwAbsVeg - RadLwNetCanopy - HeatSensibleCanopy - HeatLatentCanEvap - HeatLatentCanTransp + HeatPrecipAdvCanopy
    FluxTotCoeff = VegFrac * (4.0*LwCoeffCan*TemperatureCanopy**3 + ShCoeff + (LhCoeff+TranspHeatCoeff)*VapPresSatCanTempD + HeatCapacCan/MainTimeStep)
    TemperatureCanChg = EnergyResTmp / FluxTotCoeff
    RadLwNetCanopy = RadLwNetCanopy + VegFrac * 4.0 * LwCoeffCan * TemperatureCanopy**3 * TemperatureCanChg
    HeatSensibleCanopy = HeatSensibleCanopy + VegFrac * ShCoeff * TemperatureCanChg
    HeatLatentCanEvap = HeatLatentCanEvap + VegFrac * LhCoeff * VapPresSatCanTempD * TemperatureCanChg
    HeatLatentCanTransp = HeatLatentCanTransp + VegFrac * TranspHeatCoeff * VapPresSatCanTempD * TemperatureCanChg
    HeatCanStorageChg = VegFrac * HeatCapacCan / MainTimeStep * TemperatureCanChg
    TemperatureCanopy = TemperatureCanopy + TemperatureCanChg
    ShCanTmp = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureCanopyAir-TemperatureAirRefHeight) / ResistanceShAbvCan
    ShGrdTmp = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureGrdVeg-TemperatureCanopyAir) / ResistanceShUndCan
    SpecHumiditySfc = (0.622 * PressureVaporCanAir) / (PressureAirRefHeight - 0.378 * PressureVaporCanAir)
    if ( LastIter == 1 ) then
      exit loop1
    endif
    if ( (IndIter >= 5) .and. (abs(TemperatureCanChg) <= 0.01) .and. (LastIter == 0) ) then
      LastIter = 1
    endif
  enddo loop1
  LwCoeffAir = -EmissivityGrd * (1.0 - EmissivityVeg) * RadLwDownRefHeight - EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
  LwCoeffCan = EmissivityGrd * ConstStefanBoltzmann
  ShCoeff = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShUndCan
  LhCoeff = DensityAirRefHeight * ConstHeatCapacAir / (PsychConstGrd * (ResistanceLhUndCan+ResistanceGrdEvap))
  GrdHeatCoeff = 2.0 * ThermConductSoilSnow(NumSnowLayerNeg+1) / ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
  loop2: do IndIter = 1, NumIterG
  TempTmp = TempUnitConv(TemperatureGrdVeg)
  call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, &
    & VapPresSatIceTmpD)
  if ( TempTmp > 0.0 ) then
    VapPresSatGrdVeg = VapPresSatWatTmp
    VapPresSatGrdVegTempD = VapPresSatWatTmpD
  else
    VapPresSatGrdVeg = VapPresSatIceTmp
    VapPresSatGrdVegTempD = VapPresSatIceTmpD
  endif
  RadLwNetVegGrd = LwCoeffCan * TemperatureGrdVeg**4 + LwCoeffAir
  HeatSensibleVegGrd = ShCoeff * (TemperatureGrdVeg - TemperatureCanopyAir)
  HeatLatentVegGrd = LhCoeff * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir)
  HeatGroundVegGrd = GrdHeatCoeff * (TemperatureGrdVeg - TemperatureSoilSnow(NumSnowLayerNeg+1))
  EnergyResTmp = RadSwAbsGrd - RadLwNetVegGrd - HeatSensibleVegGrd - HeatLatentVegGrd - HeatGroundVegGrd + HeatPrecipAdvVegGrd
  FluxTotCoeff = 4.0 * LwCoeffCan * TemperatureGrdVeg**3 + ShCoeff + LhCoeff*VapPresSatGrdVegTempD + GrdHeatCoeff
  TemperatureGrdChg = EnergyResTmp / FluxTotCoeff
  RadLwNetVegGrd = RadLwNetVegGrd + 4.0 * LwCoeffCan * TemperatureGrdVeg**3 * TemperatureGrdChg
  HeatSensibleVegGrd = HeatSensibleVegGrd + ShCoeff * TemperatureGrdChg
  HeatLatentVegGrd = HeatLatentVegGrd + LhCoeff * VapPresSatGrdVegTempD * TemperatureGrdChg
  HeatGroundVegGrd = HeatGroundVegGrd + GrdHeatCoeff * TemperatureGrdChg
  TemperatureGrdVeg = TemperatureGrdVeg + TemperatureGrdChg
enddo loop2
if ( (SnowDepth > 0.05) .and. (TemperatureGrdVeg > ConstFreezePoint) ) then
  TemperatureGrdVeg = ConstFreezePoint
  RadLwNetVegGrd = LwCoeffCan * TemperatureGrdVeg**4 - EmissivityGrd * (1.0-EmissivityVeg) * RadLwDownRefHeight - EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
  HeatSensibleVegGrd = ShCoeff * (TemperatureGrdVeg - TemperatureCanopyAir)
  HeatLatentVegGrd = LhCoeff * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir)
  HeatGroundVegGrd = RadSwAbsGrd + HeatPrecipAdvVegGrd - (RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd)
endif
WindStressEwVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindEastwardRefHeight
WindStressNsVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindNorthwardRefHeight
ExchCoeffSh2mVeg = FrictionVelVeg * ConstVonKarman / (log((2.0+RoughLenShCanopy)/RoughLenShCanopy) - MoStabCorrShVeg2m)
if ( ExchCoeffSh2mVeg < 1.0e-5 ) then
  TemperatureAir2mVeg = TemperatureCanopyAir
  SpecHumidity2mVeg = SpecHumiditySfc
else
  TemperatureAir2mVeg = TemperatureCanopyAir - (HeatSensibleVegGrd + HeatSensibleCanopy/VegFrac) / (DensityAirRefHeight * ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mVeg
  SpecHumidity2mVeg = SpecHumiditySfc - ((HeatLatentCanEvap+HeatLatentCanTransp)/VegFrac + HeatLatentVegGrd) / (LatHeatVapCanopy * DensityAirRefHeight) * 1.0 / ExchCoeffSh2mVeg
endif
ExchCoeffShAbvCan = ExchCoeffShAbvCanTmp
ExchCoeffShLeaf = ExchCoeffShLeafTmp
ExchCoeffShUndCan = 1.0 / ResistanceShUndCan
end associate
end subroutine SurfaceEnergyFluxVegetated
end module lean_surfaceenergyfluxvegetatedmod

! Source: physics_noahmp/src/ResistanceBareGroundMostMod.F90
module lean_resistancebaregroundmostmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceBareGroundMOST(noahmp, IndIter, HeatSensibleTmp, MoStabParaSgn)
    implicit none
    integer , intent(in ) :: IndIter
    integer , intent(inout) :: MoStabParaSgn
    real(kind=kind_noahmp), intent(in ) :: HeatSensibleTmp
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp) :: MPE
    real(kind=kind_noahmp) :: TMPCM
    real(kind=kind_noahmp) :: TMPCH
    real(kind=kind_noahmp) :: FMNEW
    real(kind=kind_noahmp) :: FHNEW
    real(kind=kind_noahmp) :: MOZOLD
    real(kind=kind_noahmp) :: TMP1,TMP2,TMP3,TMP4,TMP5
    real(kind=kind_noahmp) :: TVIR
    real(kind=kind_noahmp) :: TMPCM2
    real(kind=kind_noahmp) :: TMPCH2
    real(kind=kind_noahmp) :: FM2NEW
    real(kind=kind_noahmp) :: FH2NEW
    real(kind=kind_noahmp) :: TMP12,TMP22,TMP32
    real(kind=kind_noahmp) :: CMFM, CHFH, CM2FM2, CH2FH2
    associate( TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & ZeroPlaneDispGrd => noahmp%energy_state_ZeroPlaneDispGrd , &
      & RoughLenShBareGrd => noahmp%energy_state_RoughLenShBareGrd , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & MoStabCorrMomBare => noahmp%energy_state_MoStabCorrMomBare , &
      & MoStabCorrShBare => noahmp%energy_state_MoStabCorrShBare , &
      & MoStabCorrMomBare2m => noahmp%energy_state_MoStabCorrMomBare2m , &
      & MoStabCorrShBare2m => noahmp%energy_state_MoStabCorrShBare2m , &
      & FrictionVelBare => noahmp%energy_state_FrictionVelBare , &
      & MoStabParaBare => noahmp%energy_state_MoStabParaBare , &
      & MoStabParaBare2m => noahmp%energy_state_MoStabParaBare2m , &
      & MoLengthBare => noahmp%energy_state_MoLengthBare , &
      & ExchCoeffMomBare => noahmp%energy_state_ExchCoeffMomBare , &
      & ExchCoeffShBare => noahmp%energy_state_ExchCoeffShBare , &
      & ExchCoeffSh2mBareMo => noahmp%energy_state_ExchCoeffSh2mBareMo , &
      & ResistanceMomBareGrd => noahmp%energy_state_ResistanceMomBareGrd , &
      & ResistanceShBareGrd => noahmp%energy_state_ResistanceShBareGrd , &
      & ResistanceLhBareGrd => noahmp%energy_state_ResistanceLhBareGrd )
    MPE = 1.0e-6
    MOZOLD = MoStabParaBare
    if ( RefHeightAboveGrd <= ZeroPlaneDispGrd ) then
      write(*,*) "WARNING: critical problem: RefHeightAboveGrd <= ZeroPlaneDispGrd; model stops"
      stop "Error in ResistanceBareGroundMostMod.F90"
    endif
    TMPCM = log((RefHeightAboveGrd - ZeroPlaneDispGrd) / RoughLenMomGrd)
    TMPCH = log((RefHeightAboveGrd - ZeroPlaneDispGrd) / RoughLenShBareGrd)
    TMPCM2 = log((2.0 + RoughLenMomGrd) / RoughLenMomGrd)
    TMPCH2 = log((2.0 + RoughLenShBareGrd) / RoughLenShBareGrd)
    if ( IndIter == 1 ) then
      FrictionVelBare = 0.0
      MoStabParaBare = 0.0
      MoLengthBare = 0.0
      MoStabParaBare2m = 0.0
    else
      TVIR = (1.0 + 0.61*SpecHumidityRefHeight) * TemperatureAirRefHeight
      TMP1 = ConstVonKarman * (ConstGravityAcc/TVIR) * HeatSensibleTmp / (DensityAirRefHeight*ConstHeatCapacAir)
      if ( abs(TMP1) <= MPE ) TMP1 = MPE
      MoLengthBare = -1.0 * FrictionVelBare**3 / TMP1
      MoStabParaBare = min((RefHeightAboveGrd - ZeroPlaneDispGrd) / MoLengthBare, 1.0)
#ifdef LEAN_NOAHMP_T2Z
      ! Lean departure (build switch LEAN_NOAHMP_T2Z, on in flavor lean, off in mpas; docs/fixes.md): MPAS caps z/L at 1 at the first
      ! level and at 2 m apart, from the same L. Past the cap the flux acts as if L = z, while 2 m still sees the true short L, so
      ! as a calm night grows more stable T2 turns back from the surface towards the first level. Here 2 m takes the capped L of
      ! the flux. In unstable air, with no cap, this is the same number. T2 and Q2 only: nothing else reads the 2 m values.
      MoStabParaBare2m = MoStabParaBare * (2.0 + RoughLenShBareGrd) / (RefHeightAboveGrd - ZeroPlaneDispGrd)
#else
      MoStabParaBare2m = min((2.0 + RoughLenShBareGrd) / MoLengthBare, 1.0)
#endif
    endif
    if ( MOZOLD*MoStabParaBare < 0.0 ) MoStabParaSgn = MoStabParaSgn + 1
    if ( MoStabParaSgn >= 2 ) then
      MoStabParaBare = 0.0
      MoStabCorrMomBare = 0.0
      MoStabCorrShBare = 0.0
      MoStabParaBare2m = 0.0
      MoStabCorrMomBare2m = 0.0
      MoStabCorrShBare2m = 0.0
    endif
    if ( MoStabParaBare < 0.0 ) then
      TMP1 = (1.0 - 16.0 * MoStabParaBare)**0.25
      TMP2 = log((1.0 + TMP1*TMP1) / 2.0)
      TMP3 = log((1.0 + TMP1) / 2.0)
      FMNEW = 2.0 * TMP3 + TMP2 - 2.0 * atan(TMP1) + 1.5707963
      FHNEW = 2 * TMP2
      TMP12 = (1.0 - 16.0 * MoStabParaBare2m)**0.25
      TMP22 = log((1.0 + TMP12*TMP12) / 2.0)
      TMP32 = log((1.0 + TMP12) / 2.0)
      FM2NEW = 2.0 * TMP32 + TMP22 - 2.0 * atan(TMP12) + 1.5707963
      FH2NEW = 2 * TMP22
    else
      FMNEW = -5.0 * MoStabParaBare
      FHNEW = FMNEW
      FM2NEW = -5.0 * MoStabParaBare2m
      FH2NEW = FM2NEW
    endif
    if ( IndIter == 1 ) then
      MoStabCorrMomBare = FMNEW
      MoStabCorrShBare = FHNEW
      MoStabCorrMomBare2m = FM2NEW
      MoStabCorrShBare2m = FH2NEW
    else
      MoStabCorrMomBare = 0.5 * (MoStabCorrMomBare + FMNEW)
      MoStabCorrShBare = 0.5 * (MoStabCorrShBare + FHNEW)
      MoStabCorrMomBare2m = 0.5 * (MoStabCorrMomBare2m + FM2NEW)
      MoStabCorrShBare2m = 0.5 * (MoStabCorrShBare2m + FH2NEW)
    endif
    MoStabCorrShBare = min(MoStabCorrShBare , 0.9*TMPCH )
    MoStabCorrMomBare = min(MoStabCorrMomBare , 0.9*TMPCM )
    MoStabCorrShBare2m = min(MoStabCorrShBare2m , 0.9*TMPCH2)
    MoStabCorrMomBare2m = min(MoStabCorrMomBare2m, 0.9*TMPCM2)
    CMFM = TMPCM - MoStabCorrMomBare
    CHFH = TMPCH - MoStabCorrShBare
    CM2FM2 = TMPCM2 - MoStabCorrMomBare2m
    CH2FH2 = TMPCH2 - MoStabCorrShBare2m
    if ( abs(CMFM) <= MPE ) CMFM = MPE
    if ( abs(CHFH) <= MPE ) CHFH = MPE
    if ( abs(CM2FM2) <= MPE ) CM2FM2 = MPE
    if ( abs(CH2FH2) <= MPE ) CH2FH2 = MPE
    ExchCoeffMomBare = ConstVonKarman * ConstVonKarman / (CMFM * CMFM)
    ExchCoeffShBare = ConstVonKarman * ConstVonKarman / (CMFM * CHFH)
    FrictionVelBare = WindSpdRefHeight * sqrt(ExchCoeffMomBare)
    ExchCoeffSh2mBareMo = ConstVonKarman * FrictionVelBare / CH2FH2
    ResistanceMomBareGrd = max(1.0, 1.0/(ExchCoeffMomBare*WindSpdRefHeight))
    ResistanceShBareGrd = max(1.0, 1.0/(ExchCoeffShBare*WindSpdRefHeight))
    ResistanceLhBareGrd = ResistanceShBareGrd
    end associate
  end subroutine ResistanceBareGroundMOST
end module lean_resistancebaregroundmostmod

! Also from: physics_noahmp/src/ResistanceBareGroundChen97Mod.F90
module lean_resistancebaregroundchen97mod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine ResistanceBareGroundChen97(noahmp, IndIter)
    implicit none
    integer , intent(in ) :: IndIter
    type(noahmp_column) , intent(inout) :: noahmp
    integer :: ILECH, ITR
    real(kind=kind_noahmp) :: ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    real(kind=kind_noahmp) :: XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    real(kind=kind_noahmp) :: ZILFC, ZU, ZT, RDZ, CXCH, DTHV, DU2
    real(kind=kind_noahmp) :: BTGH, ZSLU, ZSLT, RLOGU, RLOGT, RLMA
    real(kind=kind_noahmp) :: ZETALT, ZETALU, ZETAU, ZETAT, XLU4
    real(kind=kind_noahmp) :: XLT4, XU4, XT4, XLU, XLT, XU, XT
    real(kind=kind_noahmp) :: PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN
    integer , parameter :: ITRMX = 5
    real(kind=kind_noahmp), parameter :: WWST = 1.2
    real(kind=kind_noahmp), parameter :: WWST2 = WWST * WWST
    real(kind=kind_noahmp), parameter :: VKRM = 0.40
    real(kind=kind_noahmp), parameter :: EXCM = 0.001
    real(kind=kind_noahmp), parameter :: BETA = 1.0 / 270.0
    real(kind=kind_noahmp), parameter :: BTG = BETA * ConstGravityAcc
    real(kind=kind_noahmp), parameter :: ELFC = VKRM * BTG
    real(kind=kind_noahmp), parameter :: WOLD = 0.15
    real(kind=kind_noahmp), parameter :: WNEW = 1.0 - WOLD
    real(kind=kind_noahmp), parameter :: PIHF = 3.14159265 / 2.0
    real(kind=kind_noahmp), parameter :: EPSU2 = 1.0e-4
    real(kind=kind_noahmp), parameter :: EPSUST = 0.07
    real(kind=kind_noahmp), parameter :: EPSIT = 1.0e-4
    real(kind=kind_noahmp), parameter :: EPSA = 1.0e-8
    real(kind=kind_noahmp), parameter :: ZTMIN = -5.0
    real(kind=kind_noahmp), parameter :: ZTMAX = 1.0
    real(kind=kind_noahmp), parameter :: HPBL = 1000.0
    real(kind=kind_noahmp), parameter :: SQVISC = 258.2
    real(kind=kind_noahmp), parameter :: RIC = 0.183
    real(kind=kind_noahmp), parameter :: RRIC = 1.0 / RIC
    real(kind=kind_noahmp), parameter :: FHNEU = 0.8
    real(kind=kind_noahmp), parameter :: RFC = 0.191
    real(kind=kind_noahmp), parameter :: RFAC = RIC / ( FHNEU * RFC * RFC )
    PSLMU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLMS(ZZ) = ZZ * RRIC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSLHU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLHS(ZZ) = ZZ * RFAC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSPMU(XX) = -2.0*log( (XX+1.0)*0.5 ) - log( (XX*XX+1.0)*0.5 ) + 2.0*atan(XX) - PIHF
    PSPMS(YY) = 5.0 * YY
    PSPHU(XX) = -2.0 * log( (XX*XX + 1.0)*0.5 )
    PSPHS(YY) = 5.0 * YY
    associate( SnowDepth => noahmp%water_state_SnowDepth , &
      & ZilitinkevichCoeff => noahmp%energy_param_ZilitinkevichCoeff , &
      & RefHeightAboveGrd => noahmp%energy_state_RefHeightAboveGrd , &
      & TemperaturePotRefHeight => noahmp%energy_state_TemperaturePotRefHeight , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & TemperatureGrdBare => noahmp%energy_state_TemperatureGrdBare , &
      & ExchCoeffMomBare => noahmp%energy_state_ExchCoeffMomBare , &
      & ExchCoeffShBare => noahmp%energy_state_ExchCoeffShBare , &
      & MoStabParaBare => noahmp%energy_state_MoStabParaBare , &
      & FrictionVelVertBare => noahmp%energy_state_FrictionVelVertBare , &
      & FrictionVelBare => noahmp%energy_state_FrictionVelBare , &
      & ResistanceMomBareGrd => noahmp%energy_state_ResistanceMomBareGrd , &
      & ResistanceShBareGrd => noahmp%energy_state_ResistanceShBareGrd , &
      & ResistanceLhBareGrd => noahmp%energy_state_ResistanceLhBareGrd )
    ILECH = 0
    ZILFC = -ZilitinkevichCoeff * VKRM * SQVISC
    ZU = RoughLenMomGrd
    RDZ = 1.0 / RefHeightAboveGrd
    CXCH = EXCM * RDZ
    DTHV = TemperaturePotRefHeight - TemperatureGrdBare
    DU2 = max(WindSpdRefHeight*WindSpdRefHeight, EPSU2)
    BTGH = BTG * HPBL
    if ( IndIter == 1 ) then
      if ( (BTGH*ExchCoeffShBare*DTHV) /= 0.0 ) then
        FrictionVelVertBare = WWST2 * abs(BTGH*ExchCoeffShBare*DTHV)**(2.0/3.0)
      else
        FrictionVelVertBare = 0.0
      endif
      FrictionVelBare = max(sqrt(ExchCoeffMomBare*sqrt(DU2+FrictionVelVertBare)), EPSUST)
      MoStabParaBare = ELFC * ExchCoeffShBare * DTHV / FrictionVelBare**3
    endif
    ZT = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelBare*RoughLenMomGrd))*RoughLenMomGrd)
    ZSLU = RefHeightAboveGrd + ZU
    ZSLT = RefHeightAboveGrd + ZT
    RLOGU = log(ZSLU / ZU)
    RLOGT = log(ZSLT / ZT)
    ZETALT = max(ZSLT*MoStabParaBare, ZTMIN)
    MoStabParaBare = ZETALT / ZSLT
    ZETALU = ZSLU * MoStabParaBare
    ZETAU = ZU * MoStabParaBare
    ZETAT = ZT * MoStabParaBare
    if ( ILECH == 0 ) then
      if ( MoStabParaBare < 0.0 ) then
        XLU4 = 1.0 - 16.0 * ZETALU
        XLT4 = 1.0 - 16.0 * ZETALT
        XU4 = 1.0 - 16.0 * ZETAU
        XT4 = 1.0 - 16.0 * ZETAT
        XLU = sqrt(sqrt(XLU4))
        XLT = sqrt(sqrt(XLT4))
        XU = sqrt(sqrt(XU4))
        XT = sqrt(sqrt(XT4))
        PSMZ = PSPMU(XU)
        SIMM = PSPMU(XLU) - PSMZ + RLOGU
        PSHZ = PSPHU(XT)
        SIMH = PSPHU(XLT) - PSHZ + RLOGT
      else
        ZETALU = min(ZETALU, ZTMAX)
        ZETALT = min(ZETALT, ZTMAX)
        ZETAU = min(ZETAU, ZTMAX/(ZSLU/ZU))
        ZETAT = min(ZETAT, ZTMAX/(ZSLT/ZT))
        PSMZ = PSPMS(ZETAU)
        SIMM = PSPMS(ZETALU) - PSMZ + RLOGU
        PSHZ = PSPHS(ZETAT)
        SIMH = PSPHS(ZETALT) - PSHZ + RLOGT
      endif
    else
      if ( MoStabParaBare < 0.0 ) then
        PSMZ = PSLMU(ZETAU)
        SIMM = PSLMU(ZETALU) - PSMZ + RLOGU
        PSHZ = PSLHU(ZETAT)
        SIMH = PSLHU(ZETALT) - PSHZ + RLOGT
      else
        ZETALU = min(ZETALU, ZTMAX)
        ZETALT = min(ZETALT, ZTMAX)
        PSMZ = PSLMS(ZETAU)
        SIMM = PSLMS(ZETALU) - PSMZ + RLOGU
        PSHZ = PSLHS(ZETAT)
        SIMH = PSLHS(ZETALT) - PSHZ + RLOGT
      endif
    endif
    FrictionVelBare = max(sqrt(ExchCoeffMomBare*sqrt(DU2+FrictionVelVertBare)), EPSUST)
    ZT = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelBare*RoughLenMomGrd))*RoughLenMomGrd)
    ZSLT = RefHeightAboveGrd + ZT
    RLOGT = log(ZSLT/ZT)
    USTARK = FrictionVelBare * VKRM
    if ( SIMM < 1.0e-6 ) SIMM = 1.0e-6
    ExchCoeffMomBare = max(USTARK/SIMM, CXCH)
    if ( SIMH < 1.0e-6 ) SIMH = 1.0e-6
    ExchCoeffShBare = max(USTARK/SIMH, CXCH)
    if ( BTGH*ExchCoeffShBare*DTHV /= 0.0 ) then
      FrictionVelVertBare = WWST2 * abs(BTGH*ExchCoeffShBare*DTHV)**(2.0/3.0)
    else
      FrictionVelVertBare = 0.0
    endif
    RLMN = ELFC * ExchCoeffShBare * DTHV / FrictionVelBare**3
    RLMA = MoStabParaBare * WOLD + RLMN * WNEW
    MoStabParaBare = RLMA
    ExchCoeffShBare = ExchCoeffShBare / WindSpdRefHeight
    ExchCoeffMomBare = ExchCoeffMomBare / WindSpdRefHeight
    if ( SnowDepth > 0.0 ) then
      ExchCoeffMomBare = min(0.01, ExchCoeffMomBare)
      ExchCoeffShBare = min(0.01, ExchCoeffShBare)
    endif
    ResistanceMomBareGrd = max(1.0, 1.0/(ExchCoeffMomBare*WindSpdRefHeight))
    ResistanceShBareGrd = max(1.0, 1.0/(ExchCoeffShBare*WindSpdRefHeight))
    ResistanceLhBareGrd = ResistanceShBareGrd
    end associate
  end subroutine ResistanceBareGroundChen97
end module lean_resistancebaregroundchen97mod

! Source: physics_noahmp/src/SurfaceEnergyFluxBareGroundMod.F90
module lean_surfaceenergyfluxbaregroundmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_vaporpressuresaturationmod, only : VaporPressureSaturation
  use lean_resistancebaregroundmostmod, only : ResistanceBareGroundMOST
  use lean_resistancebaregroundchen97mod, only : ResistanceBareGroundChen97
  implicit none
  contains
  subroutine SurfaceEnergyFluxBareGround(noahmp)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer :: IndIter
    integer :: MoStabParaSgn
    integer, parameter :: NumIter = 5
    real(kind=kind_noahmp) :: TemperatureGrdChg
    real(kind=kind_noahmp) :: LwRadCoeff
    real(kind=kind_noahmp) :: ShCoeff
    real(kind=kind_noahmp) :: LhCoeff
    real(kind=kind_noahmp) :: GrdHeatCoeff
    real(kind=kind_noahmp) :: ExchCoeffShTmp
    real(kind=kind_noahmp) :: ExchCoeffMomTmp
    real(kind=kind_noahmp) :: MoistureFluxSfc
    real(kind=kind_noahmp) :: VapPresSatWatTmp
    real(kind=kind_noahmp) :: VapPresSatIceTmp
    real(kind=kind_noahmp) :: VapPresSatWatTmpD
    real(kind=kind_noahmp) :: VapPresSatIceTmpD
    real(kind=kind_noahmp) :: FluxTotCoeff
    real(kind=kind_noahmp) :: EnergyResTmp
    real(kind=kind_noahmp) :: HeatSensibleTmp
    real(kind=kind_noahmp) :: TempTmp
    real(kind=kind_noahmp) :: TempUnitConv
    TempUnitConv(TempTmp) = min(50.0, max(-50.0, (TempTmp-ConstFreezePoint)))
    associate( NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & FlagUrban => noahmp%config_domain_FlagUrban , OptSurfaceDrag => noahmp_opt_sfc , OptSnowSoilTempTime => 1 , &
      & RadLwDownRefHeight => noahmp%forcing_RadLwDownRefHeight , &
      & WindEastwardRefHeight => noahmp%forcing_WindEastwardRefHeight , &
      & WindNorthwardRefHeight => noahmp%forcing_WindNorthwardRefHeight , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & ZilitinkevichCoeff => noahmp%energy_param_ZilitinkevichCoeff , &
      & SnowDepth => noahmp%water_state_SnowDepth , SnowCoverFrac => noahmp%water_state_SnowCoverFrac , &
      & RadSwAbsGrd => noahmp%energy_flux_RadSwAbsGrd , &
      & HeatPrecipAdvBareGrd => noahmp%energy_flux_HeatPrecipAdvBareGrd , &
      & WindSpdRefHeight => noahmp%energy_state_WindSpdRefHeight , &
      & PressureVaporRefHeight => noahmp%energy_state_PressureVaporRefHeight , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & RelHumidityGrd => noahmp%energy_state_RelHumidityGrd , &
      & EmissivityGrd => noahmp%energy_state_EmissivityGrd , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & ThermConductSoilSnow => noahmp%energy_state_ThermConductSoilSnow , &
      & ResistanceGrdEvap => noahmp%energy_state_ResistanceGrdEvap , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & LatHeatVapGrd => noahmp%energy_state_LatHeatVapGrd , PsychConstGrd => noahmp%energy_state_PsychConstGrd , &
      & SpecHumiditySfc => noahmp%energy_state_SpecHumiditySfc , &
      & TemperatureGrdBare => noahmp%energy_state_TemperatureGrdBare , &
      & ExchCoeffMomBare => noahmp%energy_state_ExchCoeffMomBare , &
      & ExchCoeffShBare => noahmp%energy_state_ExchCoeffShBare , &
      & WindStressEwBare => noahmp%energy_state_WindStressEwBare , &
      & WindStressNsBare => noahmp%energy_state_WindStressNsBare , &
      & TemperatureAir2mBare => noahmp%energy_state_TemperatureAir2mBare , &
      & SpecHumidity2mBare => noahmp%energy_state_SpecHumidity2mBare , &
      & ExchCoeffSh2mBare => noahmp%energy_state_ExchCoeffSh2mBare , &
      & FrictionVelBare => noahmp%energy_state_FrictionVelBare , &
      & RoughLenShBareGrd => noahmp%energy_state_RoughLenShBareGrd , &
      & ResistanceLhBareGrd => noahmp%energy_state_ResistanceLhBareGrd , &
      & ResistanceShBareGrd => noahmp%energy_state_ResistanceShBareGrd , &
      & ResistanceMomBareGrd => noahmp%energy_state_ResistanceMomBareGrd , &
      & VapPresSatGrdBare => noahmp%energy_state_VapPresSatGrdBare , &
      & VapPresSatGrdBareTempD => noahmp%energy_state_VapPresSatGrdBareTempD , &
      & MoStabParaBare => noahmp%energy_state_MoStabParaBare , &
      & MoStabCorrShBare2m => noahmp%energy_state_MoStabCorrShBare2m , &
      & RadLwNetBareGrd => noahmp%energy_flux_RadLwNetBareGrd , &
      & HeatSensibleBareGrd => noahmp%energy_flux_HeatSensibleBareGrd , &
      & HeatLatentBareGrd => noahmp%energy_flux_HeatLatentBareGrd , &
      & HeatGroundBareGrd => noahmp%energy_flux_HeatGroundBareGrd )
    TemperatureGrdChg = 0.0
    MoStabParaBare = 0.0
    MoStabParaSgn = 0
    MoStabCorrShBare2m = 0.0
    MoistureFluxSfc = 0.0
    FrictionVelBare = 0.1
    HeatSensibleTmp = 0.0
    LwRadCoeff = EmissivityGrd * ConstStefanBoltzmann
    GrdHeatCoeff = 2.0*ThermConductSoilSnow(NumSnowLayerNeg+1)/ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    loop3: do IndIter = 1, NumIter
    if ( IndIter == 1 ) then
      RoughLenShBareGrd = RoughLenMomGrd
    else
      RoughLenShBareGrd = RoughLenMomGrd
    endif
 if (noahmp_opt_sfc == 2) then
    call ResistanceBareGroundChen97(noahmp, IndIter)
 else
    call ResistanceBareGroundMOST(noahmp, IndIter, HeatSensibleTmp, MoStabParaSgn)
 end if
    ExchCoeffMomTmp = 1.0 / ResistanceMomBareGrd
    ExchCoeffShTmp = 1.0 / ResistanceShBareGrd
    TempTmp = TempUnitConv(TemperatureGrdBare)
    call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, &
      & VapPresSatIceTmpD)
    if ( TempTmp > 0.0 ) then
      VapPresSatGrdBare = VapPresSatWatTmp
      VapPresSatGrdBareTempD = VapPresSatWatTmpD
    else
      VapPresSatGrdBare = VapPresSatIceTmp
      VapPresSatGrdBareTempD = VapPresSatIceTmpD
    endif
    ShCoeff = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShBareGrd
    LhCoeff = DensityAirRefHeight * ConstHeatCapacAir / PsychConstGrd / (ResistanceGrdEvap+ResistanceLhBareGrd)
    RadLwNetBareGrd = LwRadCoeff * runtime_fourth_power(TemperatureGrdBare) - EmissivityGrd * RadLwDownRefHeight
    HeatSensibleBareGrd = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)
    HeatLatentBareGrd = LhCoeff * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight)
    HeatGroundBareGrd = GrdHeatCoeff * (TemperatureGrdBare - TemperatureSoilSnow(NumSnowLayerNeg+1))
    EnergyResTmp = RadSwAbsGrd - RadLwNetBareGrd - HeatSensibleBareGrd - HeatLatentBareGrd - HeatGroundBareGrd + HeatPrecipAdvBareGrd
    FluxTotCoeff = 4.0*LwRadCoeff*TemperatureGrdBare**3 + ShCoeff + LhCoeff*VapPresSatGrdBareTempD + GrdHeatCoeff
    TemperatureGrdChg = EnergyResTmp / FluxTotCoeff
    RadLwNetBareGrd = RadLwNetBareGrd + 4.0 * LwRadCoeff * TemperatureGrdBare**3 * TemperatureGrdChg
    HeatSensibleBareGrd = HeatSensibleBareGrd + ShCoeff * TemperatureGrdChg
    HeatLatentBareGrd = HeatLatentBareGrd + LhCoeff * VapPresSatGrdBareTempD * TemperatureGrdChg
    HeatGroundBareGrd = HeatGroundBareGrd + GrdHeatCoeff * TemperatureGrdChg
    TemperatureGrdBare = TemperatureGrdBare + TemperatureGrdChg
    HeatSensibleTmp = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)
    TempTmp = TempUnitConv(TemperatureGrdBare)
    call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, &
      & VapPresSatIceTmpD)
    if ( TempTmp > 0.0 ) then
      VapPresSatGrdBare = VapPresSatWatTmp
    else
      VapPresSatGrdBare = VapPresSatIceTmp
    endif
    SpecHumiditySfc = 0.622 * (VapPresSatGrdBare*RelHumidityGrd) / (PressureAirRefHeight - 0.378 * (VapPresSatGrdBare*RelHumidityGrd))
    MoistureFluxSfc = (SpecHumiditySfc - SpecHumidityRefHeight) * LhCoeff * PsychConstGrd / ConstHeatCapacAir
  enddo loop3
  if ( (SnowDepth > 0.05) .and. (TemperatureGrdBare > ConstFreezePoint) ) then
    TemperatureGrdBare = ConstFreezePoint
    RadLwNetBareGrd = LwRadCoeff * runtime_fourth_power(TemperatureGrdBare) - EmissivityGrd * RadLwDownRefHeight
    HeatSensibleBareGrd = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)
    HeatLatentBareGrd = LhCoeff * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight)
    HeatGroundBareGrd = RadSwAbsGrd + HeatPrecipAdvBareGrd - (RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd)
  endif
  WindStressEwBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindEastwardRefHeight
  WindStressNsBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindNorthwardRefHeight
  ExchCoeffSh2mBare = FrictionVelBare * ConstVonKarman / (log((2.0+RoughLenShBareGrd)/RoughLenShBareGrd) - MoStabCorrShBare2m)
  if ( ExchCoeffSh2mBare < 1.0e-5 ) then
    TemperatureAir2mBare = TemperatureGrdBare
    SpecHumidity2mBare = SpecHumiditySfc
  else
    TemperatureAir2mBare = TemperatureGrdBare - HeatSensibleBareGrd / (DensityAirRefHeight*ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mBare
    SpecHumidity2mBare = SpecHumiditySfc - HeatLatentBareGrd / (LatHeatVapGrd*DensityAirRefHeight) * (1.0/ExchCoeffSh2mBare + ResistanceGrdEvap)
  endif
  if ( FlagUrban .eqv. .true. ) SpecHumidity2mBare = SpecHumiditySfc
  ExchCoeffShBare = ExchCoeffShTmp
  end associate
end subroutine SurfaceEnergyFluxBareGround
end module lean_surfaceenergyfluxbaregroundmod

! Source: physics_noahmp/src/MatrixSolverTriDiagonalMod.F90
module lean_matrixsolvertridiagonalmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine MatrixSolverTriDiagonal(P, A, B, C, D, Delta, IndTopLayer, NumSoilLayer, NumSnowLayerMax)
    implicit none
    integer , intent(in) :: IndTopLayer
    integer , intent(in) :: NumSoilLayer
    integer , intent(in) :: NumSnowLayerMax
    real(kind=kind_noahmp), dimension(-NumSnowLayerMax+1:NumSoilLayer), intent(in) :: A, B, D
    real(kind=kind_noahmp), dimension(-NumSnowLayerMax+1:NumSoilLayer), intent(inout) :: C,P,Delta
    integer :: K, KK
    C (NumSoilLayer) = 0.0
    P (IndTopLayer) = - C (IndTopLayer) / B (IndTopLayer)
    Delta (IndTopLayer) = D (IndTopLayer) / B (IndTopLayer)
    do K = IndTopLayer+1, NumSoilLayer
      P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
      Delta (K) = (D (K) - A (K) * Delta (K -1)) * (1.0 / (B (K) + A (K) * P (K -1)))
    enddo
    P (NumSoilLayer) = Delta (NumSoilLayer)
    do K = IndTopLayer+1, NumSoilLayer
      KK = NumSoilLayer - K + (IndTopLayer-1) + 1
      P (KK) = P (KK) * P (KK +1) + Delta (KK)
    enddo
  end subroutine MatrixSolverTriDiagonal
end module lean_matrixsolvertridiagonalmod

! Source: physics_noahmp/src/SoilSnowTemperatureSolverMod.F90
module lean_soilsnowtemperaturesolvermod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_matrixsolvertridiagonalmod, only : MatrixSolverTriDiagonal
  implicit none
  contains
  subroutine SoilSnowTemperatureSolver(noahmp, TimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in) :: TimeStep
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatRight
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft1
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft2
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft3
    integer :: LoopInd
    real(kind=kind_noahmp), dimension(-2:4) :: MatRightTmp
    real(kind=kind_noahmp), dimension(-2:4) :: MatLeft3Tmp
    associate( NumSoilLayer => 4 , NumSnowLayerMax => 3 , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow )
    MatRightTmp = 0.0
    MatLeft3Tmp = 0.0
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      MatRight(LoopInd) = MatRight(LoopInd) * TimeStep
      MatLeft1(LoopInd) = MatLeft1(LoopInd) * TimeStep
      MatLeft2(LoopInd) = 1.0 + MatLeft2(LoopInd) * TimeStep
      MatLeft3(LoopInd) = MatLeft3(LoopInd) * TimeStep
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      MatRightTmp(LoopInd) = MatRight(LoopInd)
      MatLeft3Tmp(LoopInd) = MatLeft3(LoopInd)
    enddo
    call MatrixSolverTriDiagonal(MatLeft3,MatLeft1,MatLeft2,MatLeft3Tmp,MatRightTmp, MatRight, &
      & NumSnowLayerNeg+1,NumSoilLayer,NumSnowLayerMax)
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      TemperatureSoilSnow(LoopInd) = TemperatureSoilSnow(LoopInd) + MatLeft3(LoopInd)
    enddo
    end associate
  end subroutine SoilSnowTemperatureSolver
end module lean_soilsnowtemperaturesolvermod

! Source: physics_noahmp/src/SoilSnowThermalDiffusionMod.F90
module lean_soilsnowthermaldiffusionmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SoilSnowThermalDiffusion(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatRight
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft1
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft2
    real(kind=kind_noahmp), dimension(-2:4), intent(inout) :: MatLeft3
    integer :: LoopInd
    real(kind=kind_noahmp) :: DepthSnowSoilTmp
    real(kind=kind_noahmp), dimension(-2:4) :: DepthSnowSoilInv
    real(kind=kind_noahmp), dimension(-2:4) :: HeatCapacPerArea
    real(kind=kind_noahmp), dimension(-2:4) :: TempGradDepth
    real(kind=kind_noahmp), dimension(-2:4) :: EnergyExcess
    associate( NumSoilLayer => 4 , NumSnowLayerMax => 3 , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & DepthSnowSoilLayer => noahmp%config_domain_DepthSnowSoilLayer , OptSoilTemperatureBottom => 2 , &
      & OptSnowSoilTempTime => 1 , TemperatureSoilBottom => noahmp%forcing_TemperatureSoilBottom , &
      & DepthSoilTempBotToSno => noahmp%energy_state_DepthSoilTempBotToSno , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & ThermConductSoilSnow => noahmp%energy_state_ThermConductSoilSnow , &
      & HeatCapacSoilSnow => noahmp%energy_state_HeatCapacSoilSnow , &
      & HeatGroundTotMean => noahmp%energy_flux_HeatGroundTotMean , &
      & RadSwPenetrateGrd => noahmp%energy_flux_RadSwPenetrateGrd , &
      & HeatFromSoilBot => noahmp%energy_flux_HeatFromSoilBot )
    MatRight(:) = 0.0
    MatLeft1(:) = 0.0
    MatLeft2(:) = 0.0
    MatLeft3(:) = 0.0
    DepthSnowSoilInv(:) = 0.0
    HeatCapacPerArea(:) = 0.0
    TempGradDepth(:) = 0.0
    EnergyExcess(:) = 0.0
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( LoopInd == (NumSnowLayerNeg+1) ) then
        HeatCapacPerArea(LoopInd) = - DepthSnowSoilLayer(LoopInd) * HeatCapacSoilSnow(LoopInd)
        DepthSnowSoilTmp = - DepthSnowSoilLayer(LoopInd+1)
        DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
        TempGradDepth(LoopInd) = 2.0 * (TemperatureSoilSnow(LoopInd) - TemperatureSoilSnow(LoopInd+1)) / DepthSnowSoilTmp
        EnergyExcess(LoopInd) = ThermConductSoilSnow(LoopInd) * TempGradDepth(LoopInd) - HeatGroundTotMean - RadSwPenetrateGrd(LoopInd)
      elseif ( LoopInd < NumSoilLayer ) then
        HeatCapacPerArea(LoopInd) = (DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)) * HeatCapacSoilSnow(LoopInd)
        DepthSnowSoilTmp = DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd+1)
        DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
        TempGradDepth(LoopInd) = 2.0 * (TemperatureSoilSnow(LoopInd) - TemperatureSoilSnow(LoopInd+1)) / DepthSnowSoilTmp
        EnergyExcess(LoopInd) = (ThermConductSoilSnow(LoopInd)*TempGradDepth(LoopInd) - ThermConductSoilSnow(LoopInd-1) * TempGradDepth(LoopInd-1) ) - RadSwPenetrateGrd(LoopInd)
      elseif ( LoopInd == NumSoilLayer ) then
        HeatCapacPerArea(LoopInd) = (DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)) * HeatCapacSoilSnow(LoopInd)
        DepthSnowSoilTmp = DepthSnowSoilLayer(LoopInd-1) - DepthSnowSoilLayer(LoopInd)
        TempGradDepth(LoopInd) = (TemperatureSoilSnow(LoopInd) - TemperatureSoilBottom) / (0.5*(DepthSnowSoilLayer(LoopInd-1)+DepthSnowSoilLayer(LoopInd)) - DepthSoilTempBotToSno)
        HeatFromSoilBot = -ThermConductSoilSnow(LoopInd) * TempGradDepth(LoopInd)
        EnergyExcess(LoopInd) = (-HeatFromSoilBot - ThermConductSoilSnow(LoopInd-1) * TempGradDepth(LoopInd-1)) - RadSwPenetrateGrd(LoopInd)
      endif
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( LoopInd == (NumSnowLayerNeg+1) ) then
        MatLeft1(LoopInd) = 0.0
        MatLeft3(LoopInd) = - ThermConductSoilSnow(LoopInd) * DepthSnowSoilInv(LoopInd) / HeatCapacPerArea(LoopInd)
        MatLeft2(LoopInd) = - MatLeft3(LoopInd)
      elseif ( LoopInd < NumSoilLayer ) then
        MatLeft1(LoopInd) = - ThermConductSoilSnow(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / HeatCapacPerArea(LoopInd)
        MatLeft3(LoopInd) = - ThermConductSoilSnow(LoopInd ) * DepthSnowSoilInv(LoopInd ) / HeatCapacPerArea(LoopInd)
        MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3 (LoopInd))
      elseif ( LoopInd == NumSoilLayer ) then
        MatLeft1(LoopInd) = - ThermConductSoilSnow(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / HeatCapacPerArea(LoopInd)
        MatLeft3(LoopInd) = 0.0
        MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3(LoopInd))
      endif
      MatRight(LoopInd) = EnergyExcess(LoopInd) / (-HeatCapacPerArea(LoopInd))
    enddo
    end associate
  end subroutine SoilSnowThermalDiffusion
end module lean_soilsnowthermaldiffusionmod

! Source: physics_noahmp/src/SoilSnowTemperatureMainMod.F90
module lean_soilsnowtemperaturemainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_soilsnowtemperaturesolvermod, only : SoilSnowTemperatureSolver
  use lean_soilsnowthermaldiffusionmod, only : SoilSnowThermalDiffusion
  implicit none
  contains
  subroutine SoilSnowTemperatureMain(noahmp)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), dimension(-2:4) :: MatRight
    real(kind=kind_noahmp), dimension(-2:4) :: MatLeft1
    real(kind=kind_noahmp), dimension(-2:4) :: MatLeft2
    real(kind=kind_noahmp), dimension(-2:4) :: MatLeft3
    associate( NumSoilLayer => 4 , NumSnowLayerMax => 3 , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & SoilTimeStep => noahmp%config_domain_SoilTimeStep , &
      & DepthSoilTempBottom => noahmp%config_domain_DepthSoilTempBottom , &
      & SnowDepth => noahmp%water_state_SnowDepth , &
      & DepthSoilTempBotToSno => noahmp%energy_state_DepthSoilTempBotToSno , &
      & HeatFromSoilBot => noahmp%energy_flux_HeatFromSoilBot , &
      & RadSwPenetrateGrd => noahmp%energy_flux_RadSwPenetrateGrd )
    MatRight(:) = 0.0
    MatLeft1(:) = 0.0
    MatLeft2(:) = 0.0
    MatLeft3(:) = 0.0
    RadSwPenetrateGrd(NumSnowLayerNeg+1:NumSoilLayer) = 0.0
    DepthSoilTempBotToSno = DepthSoilTempBottom - SnowDepth
    call SoilSnowThermalDiffusion(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
    call SoilSnowTemperatureSolver(noahmp, SoilTimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)
    HeatFromSoilBot = HeatFromSoilBot * SoilTimeStep
    end associate
  end subroutine SoilSnowTemperatureMain
end module lean_soilsnowtemperaturemainmod

! Source: physics_noahmp/src/SoilWaterSupercoolNiu06Mod.F90
module lean_soilwatersupercoolniu06mod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SoilWaterSupercoolNiu06(noahmp, IndSoil, SoilWatSupercool, SoilTemperature)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer , intent(in ) :: IndSoil
    real(kind=kind_noahmp), intent(in ) :: SoilTemperature
    real(kind=kind_noahmp), intent(out ) :: SoilWatSupercool
    real(kind=kind_noahmp) :: SoilWatPotFrz
    associate( SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , &
      & SoilMatPotentialSat => noahmp%water_param_SoilMatPotentialSat , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat )
    SoilWatPotFrz = ConstLatHeatFusion * (ConstFreezePoint - SoilTemperature) / (ConstGravityAcc * SoilTemperature)
    SoilWatSupercool = SoilMoistureSat(IndSoil) * (SoilWatPotFrz / SoilMatPotentialSat(IndSoil))**(-1.0/SoilExpCoeffB(IndSoil))
    end associate
  end subroutine SoilWaterSupercoolNiu06
end module lean_soilwatersupercoolniu06mod

! Source: physics_noahmp/src/SoilSnowWaterPhaseChangeMod.F90
module lean_soilsnowwaterphasechangemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_soilwatersupercoolniu06mod, only : SoilWaterSupercoolNiu06
  implicit none
  contains
  subroutine SoilSnowWaterPhaseChange(noahmp)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp) :: EnergyResLeft
    real(kind=kind_noahmp) :: SnowWaterPrev
    real(kind=kind_noahmp) :: SnowWaterRatio
    real(kind=kind_noahmp) :: HeatLhTotPhsChg
    real(kind=kind_noahmp), dimension(-2:4) :: EnergyRes
    real(kind=kind_noahmp), dimension(-2:4) :: WaterPhaseChg
    real(kind=kind_noahmp), dimension(-2:4) :: MassWatTotInit
    real(kind=kind_noahmp), dimension(-2:4) :: MassWatIceInit
    real(kind=kind_noahmp), dimension(-2:4) :: MassWatLiqInit
    real(kind=kind_noahmp), dimension(-2:4) :: MassWatIceTmp
    real(kind=kind_noahmp), dimension(-2:4) :: MassWatLiqTmp
    associate( OptSoilSupercoolWater => 1 , NumSoilLayer => 4 , NumSnowLayerMax => 3 , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & MainTimeStep => noahmp%config_domain_MainTimeStep , SurfaceType => 1 , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & SoilExpCoeffB => noahmp%water_param_SoilExpCoeffB , &
      & SoilMatPotentialSat => noahmp%water_param_SoilMatPotentialSat , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & PhaseChgFacSoilSnow => noahmp%energy_state_PhaseChgFacSoilSnow , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & SnowIce => noahmp%water_state_SnowIce , SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & SnowDepth => noahmp%water_state_SnowDepth , SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & IndexPhaseChange => noahmp%water_state_IndexPhaseChange , &
      & SoilSupercoolWater => noahmp%water_state_SoilSupercoolWater , &
      & PondSfcThinSnwMelt => noahmp%water_state_PondSfcThinSnwMelt , &
      & MeltGroundSnow => noahmp%water_flux_MeltGroundSnow )
    EnergyRes = 0.0
    WaterPhaseChg = 0.0
    MassWatTotInit = 0.0
    MassWatIceInit = 0.0
    MassWatLiqInit = 0.0
    MassWatIceTmp = 0.0
    MassWatLiqTmp = 0.0
    MeltGroundSnow = 0.0
    PondSfcThinSnwMelt = 0.0
    HeatLhTotPhsChg = 0.0
    do LoopInd = -NumSnowLayerMax+1, NumSoilLayer
      SoilSupercoolWater(LoopInd) = 0.0
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      MassWatIceTmp(LoopInd) = SnowIce(LoopInd)
      MassWatLiqTmp(LoopInd) = SnowLiqWater(LoopInd)
    enddo
    do LoopInd = 1, NumSoilLayer
      MassWatLiqTmp(LoopInd) = SoilLiqWater(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
      MassWatIceTmp(LoopInd) = (SoilMoisture(LoopInd) - SoilLiqWater(LoopInd)) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      IndexPhaseChange(LoopInd) = 0
      EnergyRes(LoopInd) = 0.0
      WaterPhaseChg(LoopInd) = 0.0
      MassWatIceInit(LoopInd) = MassWatIceTmp(LoopInd)
      MassWatLiqInit(LoopInd) = MassWatLiqTmp(LoopInd)
      MassWatTotInit(LoopInd) = MassWatIceTmp(LoopInd) + MassWatLiqTmp(LoopInd)
    enddo
    do LoopInd = 1, NumSoilLayer
      if ( TemperatureSoilSnow(LoopInd) < ConstFreezePoint ) then
        call SoilWaterSupercoolNiu06(noahmp, LoopInd, SoilSupercoolWater(LoopInd),TemperatureSoilSnow(LoopInd))
        SoilSupercoolWater(LoopInd) = SoilSupercoolWater(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
      endif
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( (MassWatIceTmp(LoopInd) > 0.0) .and. (TemperatureSoilSnow(LoopInd) >= ConstFreezePoint) ) then
        IndexPhaseChange(LoopInd) = 1
      endif
      if ( (MassWatLiqTmp(LoopInd) > SoilSupercoolWater(LoopInd)) .and. (TemperatureSoilSnow(LoopInd) < ConstFreezePoint) ) then
        IndexPhaseChange(LoopInd) = 2
      endif
      if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (LoopInd == 1) ) then
        if ( TemperatureSoilSnow(LoopInd) >= ConstFreezePoint ) then
          IndexPhaseChange(LoopInd) = 1
        endif
      endif
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( IndexPhaseChange(LoopInd) > 0 ) then
        EnergyRes(LoopInd) = (TemperatureSoilSnow(LoopInd)-ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd)
        TemperatureSoilSnow(LoopInd) = ConstFreezePoint
      endif
      if ( (IndexPhaseChange(LoopInd) == 1) .and. (EnergyRes(LoopInd) < 0.0) ) then
        EnergyRes(LoopInd) = 0.0
        IndexPhaseChange(LoopInd) = 0
      endif
      if ( (IndexPhaseChange(LoopInd) == 2) .and. (EnergyRes(LoopInd) > 0.0) ) then
        EnergyRes(LoopInd) = 0.0
        IndexPhaseChange(LoopInd) = 0
      endif
      WaterPhaseChg(LoopInd) = EnergyRes(LoopInd) * MainTimeStep / ConstLatHeatFusion
    enddo
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (WaterPhaseChg(1) > 0.0) ) then
      SnowWaterPrev = SnowWaterEquiv
      SnowWaterEquiv = max(0.0, SnowWaterPrev-WaterPhaseChg(1))
      SnowWaterRatio = SnowWaterEquiv / SnowWaterPrev
      SnowDepth = max(0.0, SnowWaterRatio*SnowDepth )
      SnowDepth = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)
      EnergyResLeft = EnergyRes(1) - ConstLatHeatFusion * (SnowWaterPrev - SnowWaterEquiv) / MainTimeStep
      if ( EnergyResLeft > 0.0 ) then
        WaterPhaseChg(1) = EnergyResLeft * MainTimeStep / ConstLatHeatFusion
        EnergyRes(1) = EnergyResLeft
      else
        WaterPhaseChg(1) = 0.0
        EnergyRes(1) = 0.0
      endif
      MeltGroundSnow = max(0.0, (SnowWaterPrev-SnowWaterEquiv)) / MainTimeStep
      HeatLhTotPhsChg = ConstLatHeatFusion * MeltGroundSnow
      PondSfcThinSnwMelt = SnowWaterPrev - SnowWaterEquiv
    endif
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      if ( (IndexPhaseChange(LoopInd) > 0) .and. (abs(EnergyRes(LoopInd)) > 0.0) ) then
        EnergyResLeft = 0.0
        if ( WaterPhaseChg(LoopInd) > 0.0 ) then
          MassWatIceTmp(LoopInd) = max(0.0, MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
          EnergyResLeft = EnergyRes(LoopInd) - ConstLatHeatFusion * (MassWatIceInit(LoopInd) - MassWatIceTmp(LoopInd)) / MainTimeStep
        elseif ( WaterPhaseChg(LoopInd) < 0.0 ) then
          if ( LoopInd <= 0 ) then
            MassWatIceTmp(LoopInd) = min(MassWatTotInit(LoopInd), MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
          else
            if ( MassWatTotInit(LoopInd) < SoilSupercoolWater(LoopInd) ) then
              MassWatIceTmp(LoopInd) = 0.0
            else
              MassWatIceTmp(LoopInd) = min(MassWatTotInit(LoopInd)-SoilSupercoolWater(LoopInd), &
                & MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
              MassWatIceTmp(LoopInd) = max(MassWatIceTmp(LoopInd), 0.0)
            endif
          endif
          EnergyResLeft = EnergyRes(LoopInd) - ConstLatHeatFusion * (MassWatIceInit(LoopInd) - MassWatIceTmp(LoopInd)) / MainTimeStep
        endif
        MassWatLiqTmp(LoopInd) = max(0.0, MassWatTotInit(LoopInd)-MassWatIceTmp(LoopInd))
        if ( abs(EnergyResLeft) > 0.0 ) then
          TemperatureSoilSnow(LoopInd) = TemperatureSoilSnow(LoopInd) + PhaseChgFacSoilSnow(LoopInd) * EnergyResLeft
          if ( LoopInd <= 0 ) then
            if ( (MassWatLiqTmp(LoopInd)*MassWatIceTmp(LoopInd)) > 0.0 ) TemperatureSoilSnow(LoopInd) = ConstFreezePoint
            if ( MassWatIceTmp(LoopInd) == 0.0 ) then
              TemperatureSoilSnow(LoopInd) = ConstFreezePoint
              EnergyRes(LoopInd+1) = EnergyRes(LoopInd+1) + EnergyResLeft
              WaterPhaseChg(LoopInd+1) = EnergyRes(LoopInd+1) * MainTimeStep / ConstLatHeatFusion
            endif
          endif
        endif
        HeatLhTotPhsChg = HeatLhTotPhsChg + ConstLatHeatFusion * (MassWatIceInit(LoopInd) - MassWatIceTmp(LoopInd)) / MainTimeStep
        if ( LoopInd < 1 ) then
          MeltGroundSnow = MeltGroundSnow + max(0.0, (MassWatIceInit(LoopInd)-MassWatIceTmp(LoopInd))) / MainTimeStep
        endif
      endif
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowLiqWater(LoopInd) = MassWatLiqTmp(LoopInd)
      SnowIce(LoopInd) = MassWatIceTmp(LoopInd)
    enddo
    do LoopInd = 1, NumSoilLayer
      SoilLiqWater(LoopInd) = MassWatLiqTmp(LoopInd) / (1000.0 * ThicknessSnowSoilLayer(LoopInd))
      SoilMoisture(LoopInd) = (MassWatLiqTmp(LoopInd)+MassWatIceTmp(LoopInd)) / (1000.0*ThicknessSnowSoilLayer(LoopInd))
    enddo
    end associate
  end subroutine SoilSnowWaterPhaseChange
end module lean_soilsnowwaterphasechangemod

! Source: physics_noahmp/src/EnergyMainMod.F90
module lean_energymainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowcovergroundniu07mod, only : SnowCoverGroundNiu07
  use lean_groundroughnesspropertymod, only : GroundRoughnessProperty
  use lean_groundthermalpropertymod, only : GroundThermalProperty
  use lean_surfacealbedomod, only : SurfaceAlbedo
  use lean_surfaceradiationmod, only : SurfaceRadiation
  use lean_surfaceemissivitymod, only : SurfaceEmissivity
  use lean_soilwatertranspirationmod, only : SoilWaterTranspiration
  use lean_resistancegroundevaporationmod, only : ResistanceGroundEvaporation
  use lean_psychrometricvariablemod, only : PsychrometricVariable
  use lean_surfaceenergyfluxvegetatedmod, only : SurfaceEnergyFluxVegetated
  use lean_surfaceenergyfluxbaregroundmod, only : SurfaceEnergyFluxBareGround
  use lean_soilsnowtemperaturemainmod, only : SoilSnowTemperatureMain
  use lean_soilsnowwaterphasechangemod, only : SoilSnowWaterPhaseChange
  implicit none
  contains
  subroutine EnergyMain(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    logical :: FlagVegSfc
    associate( PressureAirRefHeight => noahmp%forcing_PressureAirRefHeight , &
      & RadLwDownRefHeight => noahmp%forcing_RadLwDownRefHeight , &
      & RadSwDownRefHeight => noahmp%forcing_RadSwDownRefHeight , OptSnowSoilTempTime => 1 , &
      & FlagCropland => noahmp%config_domain_FlagCropland , &
      & FlagSoilProcess => noahmp%config_domain_FlagSoilProcess , &
      & NumSoilTimeStep => noahmp%config_domain_NumSoilTimeStep , &
      & SoilTimeStep => noahmp%config_domain_SoilTimeStep , &
      & IrriFracThreshold => noahmp%water_param_IrriFracThreshold , &
      & IrrigationFracGrid => noahmp%water_state_IrrigationFracGrid , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , VegFrac => noahmp%energy_state_VegFrac , &
      & HeatLatentIrriEvap => noahmp%energy_flux_HeatLatentIrriEvap , &
      & HeatPrecipAdvCanopy => noahmp%energy_flux_HeatPrecipAdvCanopy , &
      & HeatPrecipAdvVegGrd => noahmp%energy_flux_HeatPrecipAdvVegGrd , &
      & HeatPrecipAdvBareGrd => noahmp%energy_flux_HeatPrecipAdvBareGrd , &
      & TemperatureSfc => noahmp%energy_state_TemperatureSfc , &
      & TemperatureGrd => noahmp%energy_state_TemperatureGrd , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & SpecHumiditySfc => noahmp%energy_state_SpecHumiditySfc , &
      & SpecHumiditySfcMean => noahmp%energy_state_SpecHumiditySfcMean , &
      & PressureVaporCanAir => noahmp%energy_state_PressureVaporCanAir , &
      & ExchCoeffMomSfc => noahmp%energy_state_ExchCoeffMomSfc , &
      & ExchCoeffShSfc => noahmp%energy_state_ExchCoeffShSfc , &
      & HeatGroundTotAcc => noahmp%energy_flux_HeatGroundTotAcc , SnowDepth => noahmp%water_state_SnowDepth , &
      & RoughLenMomSfcToAtm => noahmp%energy_state_RoughLenMomSfcToAtm , &
      & WindStressEwSfc => noahmp%energy_state_WindStressEwSfc , &
      & WindStressNsSfc => noahmp%energy_state_WindStressNsSfc , &
      & TemperatureRadSfc => noahmp%energy_state_TemperatureRadSfc , &
      & TemperatureAir2m => noahmp%energy_state_TemperatureAir2m , &
      & ResistanceStomataSunlit => noahmp%energy_state_ResistanceStomataSunlit , &
      & ResistanceStomataShade => noahmp%energy_state_ResistanceStomataShade , &
      & TemperatureAir2mVeg => noahmp%energy_state_TemperatureAir2mVeg , &
      & TemperatureAir2mBare => noahmp%energy_state_TemperatureAir2mBare , &
      & LeafAreaIndSunlit => noahmp%energy_state_LeafAreaIndSunlit , &
      & LeafAreaIndShade => noahmp%energy_state_LeafAreaIndShade , &
      & EmissivitySfc => noahmp%energy_state_EmissivitySfc , VegAreaIndEff => noahmp%energy_state_VegAreaIndEff , &
      & RoughLenMomSfc => noahmp%energy_state_RoughLenMomSfc , &
      & RoughLenMomGrd => noahmp%energy_state_RoughLenMomGrd , &
      & WindStressEwVeg => noahmp%energy_state_WindStressEwVeg , &
      & WindStressNsVeg => noahmp%energy_state_WindStressNsVeg , &
      & WindStressEwBare => noahmp%energy_state_WindStressEwBare , &
      & WindStressNsBare => noahmp%energy_state_WindStressNsBare , &
      & SpecHumidity2mVeg => noahmp%energy_state_SpecHumidity2mVeg , &
      & SpecHumidity2mBare => noahmp%energy_state_SpecHumidity2mBare , &
      & SpecHumidity2m => noahmp%energy_state_SpecHumidity2m , &
      & TemperatureGrdVeg => noahmp%energy_state_TemperatureGrdVeg , &
      & TemperatureGrdBare => noahmp%energy_state_TemperatureGrdBare , &
      & ExchCoeffMomAbvCan => noahmp%energy_state_ExchCoeffMomAbvCan , &
      & ExchCoeffMomBare => noahmp%energy_state_ExchCoeffMomBare , &
      & ExchCoeffShAbvCan => noahmp%energy_state_ExchCoeffShAbvCan , &
      & ExchCoeffShBare => noahmp%energy_state_ExchCoeffShBare , &
      & ExchCoeffShLeaf => noahmp%energy_state_ExchCoeffShLeaf , &
      & ExchCoeffShUndCan => noahmp%energy_state_ExchCoeffShUndCan , &
      & ExchCoeffSh2mVeg => noahmp%energy_state_ExchCoeffSh2mVeg , AlbedoSfc => noahmp%energy_state_AlbedoSfc , &
      & RadSwReflSfc => noahmp%energy_flux_RadSwReflSfc , RadLwNetSfc => noahmp%energy_flux_RadLwNetSfc , &
      & HeatSensibleSfc => noahmp%energy_flux_HeatSensibleSfc , &
      & HeatLatentGrd => noahmp%energy_flux_HeatLatentGrd , &
      & HeatLatentCanopy => noahmp%energy_flux_HeatLatentCanopy , &
      & HeatLatentTransp => noahmp%energy_flux_HeatLatentTransp , &
      & RadPhotoActAbsCan => noahmp%energy_flux_RadPhotoActAbsCan , &
      & RadPhotoActAbsSunlit => noahmp%energy_flux_RadPhotoActAbsSunlit , &
      & RadPhotoActAbsShade => noahmp%energy_flux_RadPhotoActAbsShade , &
      & HeatGroundTot => noahmp%energy_flux_HeatGroundTot , &
      & HeatPrecipAdvSfc => noahmp%energy_flux_HeatPrecipAdvSfc , &
      & RadLwEmitSfc => noahmp%energy_flux_RadLwEmitSfc , RadLwNetCanopy => noahmp%energy_flux_RadLwNetCanopy , &
      & RadLwNetVegGrd => noahmp%energy_flux_RadLwNetVegGrd , &
      & RadLwNetBareGrd => noahmp%energy_flux_RadLwNetBareGrd , &
      & HeatSensibleCanopy => noahmp%energy_flux_HeatSensibleCanopy , &
      & HeatSensibleVegGrd => noahmp%energy_flux_HeatSensibleVegGrd , &
      & HeatSensibleBareGrd => noahmp%energy_flux_HeatSensibleBareGrd , &
      & HeatLatentVegGrd => noahmp%energy_flux_HeatLatentVegGrd , &
      & HeatLatentBareGrd => noahmp%energy_flux_HeatLatentBareGrd , &
      & HeatLatentCanEvap => noahmp%energy_flux_HeatLatentCanEvap , &
      & HeatLatentCanTransp => noahmp%energy_flux_HeatLatentCanTransp , &
      & HeatGroundVegGrd => noahmp%energy_flux_HeatGroundVegGrd , &
      & HeatGroundBareGrd => noahmp%energy_flux_HeatGroundBareGrd , &
      & HeatCanStorageChg => noahmp%energy_flux_HeatCanStorageChg , &
      & HeatFromSoilBot => noahmp%energy_flux_HeatFromSoilBot , &
      & HeatGroundTotMean => noahmp%energy_flux_HeatGroundTotMean , &
      & PhotosynTotal => noahmp%biochem_flux_PhotosynTotal , &
      & PhotosynLeafSunlit => noahmp%biochem_flux_PhotosynLeafSunlit , &
      & PhotosynLeafShade => noahmp%biochem_flux_PhotosynLeafShade )
    WindStressEwVeg = 0.0
    WindStressNsVeg = 0.0
    RadLwNetCanopy = 0.0
    HeatSensibleCanopy = 0.0
    RadLwNetVegGrd = 0.0
    HeatSensibleVegGrd = 0.0
    HeatLatentVegGrd = 0.0
    HeatLatentCanEvap = 0.0
    HeatLatentCanTransp = 0.0
    HeatGroundVegGrd = 0.0
    PhotosynLeafSunlit = 0.0
    PhotosynLeafShade = 0.0
    TemperatureAir2mVeg = 0.0
    SpecHumidity2mVeg = 0.0
    ExchCoeffShAbvCan = 0.0
    ExchCoeffShLeaf = 0.0
    ExchCoeffShUndCan = 0.0
    ExchCoeffSh2mVeg = 0.0
    HeatPrecipAdvSfc = 0.0
    HeatCanStorageChg = 0.0
    VegAreaIndEff = LeafAreaIndEff + StemAreaIndEff
    FlagVegSfc = .false.
    if ( VegAreaIndEff > 0.0 ) FlagVegSfc = .true.
    call SnowCoverGroundNiu07(noahmp)
    call GroundRoughnessProperty(noahmp, FlagVegSfc)
    call GroundThermalProperty(noahmp)
    call SurfaceAlbedo(noahmp)
    call SurfaceRadiation(noahmp)
    call SurfaceEmissivity(noahmp)
    call SoilWaterTranspiration(noahmp)
    call ResistanceGroundEvaporation(noahmp)
    call PsychrometricVariable(noahmp)
    if ( (FlagVegSfc .eqv. .true.) .and. (VegFrac > 0) ) then
      TemperatureGrdVeg = TemperatureGrd
      ExchCoeffMomAbvCan = ExchCoeffMomSfc
      ExchCoeffShAbvCan = ExchCoeffShSfc
      call SurfaceEnergyFluxVegetated(noahmp)
    endif
    TemperatureGrdBare = TemperatureGrd
    ExchCoeffMomBare = ExchCoeffMomSfc
    ExchCoeffShBare = ExchCoeffShSfc
    call SurfaceEnergyFluxBareGround(noahmp)
    if ( (FlagVegSfc .eqv. .true.) .and. (VegFrac > 0) ) then
      WindStressEwSfc = VegFrac * WindStressEwVeg + (1.0 - VegFrac) * WindStressEwBare
      WindStressNsSfc = VegFrac * WindStressNsVeg + (1.0 - VegFrac) * WindStressNsBare
      RadLwNetSfc = VegFrac * RadLwNetVegGrd + (1.0 - VegFrac) * RadLwNetBareGrd + RadLwNetCanopy
      HeatSensibleSfc = VegFrac * HeatSensibleVegGrd + (1.0 - VegFrac) * HeatSensibleBareGrd + HeatSensibleCanopy
      HeatLatentGrd = VegFrac * HeatLatentVegGrd + (1.0 - VegFrac) * HeatLatentBareGrd
      HeatGroundTot = VegFrac * HeatGroundVegGrd + (1.0 - VegFrac) * HeatGroundBareGrd
      HeatLatentCanopy = HeatLatentCanEvap
      HeatLatentTransp = HeatLatentCanTransp
      HeatPrecipAdvSfc = VegFrac * HeatPrecipAdvVegGrd + (1.0 - VegFrac) * HeatPrecipAdvBareGrd + HeatPrecipAdvCanopy
      TemperatureGrd = VegFrac * TemperatureGrdVeg + (1.0 - VegFrac) * TemperatureGrdBare
      TemperatureAir2m = VegFrac * TemperatureAir2mVeg + (1.0 - VegFrac) * TemperatureAir2mBare
      TemperatureSfc = VegFrac * TemperatureCanopy + (1.0 - VegFrac) * TemperatureGrdBare
      ExchCoeffMomSfc = VegFrac * ExchCoeffMomAbvCan + (1.0 - VegFrac) * ExchCoeffMomBare
      ExchCoeffShSfc = VegFrac * ExchCoeffShAbvCan + (1.0 - VegFrac) * ExchCoeffShBare
      SpecHumidity2m = VegFrac * SpecHumidity2mVeg + (1.0 - VegFrac) * SpecHumidity2mBare
      SpecHumiditySfcMean = VegFrac * (PressureVaporCanAir * 0.622 / (PressureAirRefHeight - 0.378*PressureVaporCanAir)) + (1.0 - VegFrac) * SpecHumiditySfc
      RoughLenMomSfcToAtm = RoughLenMomSfc
    else
      WindStressEwSfc = WindStressEwBare
      WindStressNsSfc = WindStressNsBare
      RadLwNetSfc = RadLwNetBareGrd
      HeatSensibleSfc = HeatSensibleBareGrd
      HeatLatentGrd = HeatLatentBareGrd
      HeatGroundTot = HeatGroundBareGrd
      TemperatureGrd = TemperatureGrdBare
      TemperatureAir2m = TemperatureAir2mBare
      HeatLatentCanopy = 0.0
      HeatLatentTransp = 0.0
      HeatPrecipAdvSfc = HeatPrecipAdvBareGrd
      TemperatureSfc = TemperatureGrd
      ExchCoeffMomSfc = ExchCoeffMomBare
      ExchCoeffShSfc = ExchCoeffShBare
      SpecHumiditySfcMean = SpecHumiditySfc
      SpecHumidity2m = SpecHumidity2mBare
      ResistanceStomataSunlit = 0.0
      ResistanceStomataShade = 0.0
      TemperatureGrdVeg = TemperatureGrdBare
      ExchCoeffShAbvCan = ExchCoeffShBare
      RoughLenMomSfcToAtm = RoughLenMomGrd
    endif
    RadLwEmitSfc = RadLwDownRefHeight + RadLwNetSfc
    if ( RadLwEmitSfc <= 0.0 ) then
      write(*,*) "emitted longwave <0; skin T may be wrong due to inconsistent"
      write(*,*) "input of VegFracGreen with LeafAreaIndex"
      write(*,*) "VegFrac = ", VegFrac, "VegAreaIndEff = ", VegAreaIndEff, "TemperatureCanopy = ", &
        & TemperatureCanopy, "TemperatureGrd = ", TemperatureGrd
      write(*,*) "RadLwDownRefHeight = ", RadLwDownRefHeight, "RadLwNetSfc = ", RadLwNetSfc, "SnowDepth = ", &
        & SnowDepth
      stop "Error: Longwave radiation budget problem in NoahMP LSM"
    endif
    TemperatureRadSfc = ((RadLwEmitSfc - (1.0-EmissivitySfc)*RadLwDownRefHeight) / (EmissivitySfc*ConstStefanBoltzmann))**0.25
    RadPhotoActAbsCan = RadPhotoActAbsSunlit * LeafAreaIndSunlit + RadPhotoActAbsShade * LeafAreaIndShade
    PhotosynTotal = PhotosynLeafSunlit * LeafAreaIndSunlit + PhotosynLeafShade * LeafAreaIndShade
    HeatFromSoilBot = 0.0
    HeatGroundTotAcc = HeatGroundTotAcc + HeatGroundTot
    if ( FlagSoilProcess .eqv. .true. ) then
      HeatGroundTotMean = HeatGroundTotAcc / NumSoilTimeStep
      call SoilSnowTemperatureMain(noahmp)
    endif
    call SoilSnowWaterPhaseChange(noahmp)
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) HeatSensibleSfc = HeatSensibleSfc - HeatLatentIrriEvap
    if ( RadSwDownRefHeight > 0.0 ) then
      AlbedoSfc = RadSwReflSfc / RadSwDownRefHeight
    else
      AlbedoSfc = undefined_real
    endif
    end associate
  end subroutine EnergyMain
end module lean_energymainmod

! Source: physics_noahmp/src/CanopyHydrologyMod.F90
module lean_canopyhydrologymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine CanopyHydrology(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & HeatLatentCanopy => noahmp%energy_flux_HeatLatentCanopy , &
      & HeatLatentTransp => noahmp%energy_flux_HeatLatentTransp , &
      & LeafAreaIndEff => noahmp%energy_state_LeafAreaIndEff , &
      & StemAreaIndEff => noahmp%energy_state_StemAreaIndEff , &
      & FlagFrozenCanopy => noahmp%energy_state_FlagFrozenCanopy , VegFrac => noahmp%energy_state_VegFrac , &
      & SnowfallDensity => noahmp%water_state_SnowfallDensity , &
      & CanopyLiqHoldCap => noahmp%water_param_CanopyLiqHoldCap , &
      & CanopyLiqWater => noahmp%water_state_CanopyLiqWater , CanopyIce => noahmp%water_state_CanopyIce , &
      & TemperatureCanopy => noahmp%energy_state_TemperatureCanopy , &
      & CanopyTotalWater => noahmp%water_state_CanopyTotalWater , &
      & CanopyWetFrac => noahmp%water_state_CanopyWetFrac , CanopyIceMax => noahmp%water_state_CanopyIceMax , &
      & CanopyLiqWaterMax => noahmp%water_state_CanopyLiqWaterMax , &
      & EvapCanopyNet => noahmp%water_flux_EvapCanopyNet , Transpiration => noahmp%water_flux_Transpiration , &
      & EvapCanopyLiq => noahmp%water_flux_EvapCanopyLiq , DewCanopyLiq => noahmp%water_flux_DewCanopyLiq , &
      & FrostCanopyIce => noahmp%water_flux_FrostCanopyIce , &
      & SublimCanopyIce => noahmp%water_flux_SublimCanopyIce , MeltCanopyIce => noahmp%water_flux_MeltCanopyIce , &
      & FreezeCanopyLiq => noahmp%water_flux_FreezeCanopyLiq )
    EvapCanopyNet = 0.0
    Transpiration = 0.0
    EvapCanopyLiq = 0.0
    DewCanopyLiq = 0.0
    FrostCanopyIce = 0.0
    SublimCanopyIce = 0.0
    MeltCanopyIce = 0.0
    FreezeCanopyLiq = 0.0
    CanopyLiqWaterMax = 0.0
    CanopyIceMax = 0.0
    CanopyWetFrac = 0.0
    CanopyTotalWater = 0.0
    CanopyLiqWaterMax = VegFrac * CanopyLiqHoldCap * (LeafAreaIndEff + StemAreaIndEff)
    if ( FlagFrozenCanopy .eqv. .false. ) then
      Transpiration = max( HeatLatentTransp/ConstLatHeatEvap, 0.0 )
      EvapCanopyLiq = max( HeatLatentCanopy/ConstLatHeatEvap, 0.0 )
      DewCanopyLiq = abs( min( HeatLatentCanopy/ConstLatHeatEvap, 0.0 ) )
      SublimCanopyIce = 0.0
      FrostCanopyIce = 0.0
    else
      Transpiration = max( HeatLatentTransp/ConstLatHeatSublim, 0.0 )
      EvapCanopyLiq = 0.0
      DewCanopyLiq = 0.0
      SublimCanopyIce = max( HeatLatentCanopy/ConstLatHeatSublim, 0.0 )
      FrostCanopyIce = abs( min( HeatLatentCanopy/ConstLatHeatSublim, 0.0 ) )
    endif
    EvapCanopyLiq = min( CanopyLiqWater/MainTimeStep, EvapCanopyLiq )
    CanopyLiqWater = max( 0.0, CanopyLiqWater+(DewCanopyLiq-EvapCanopyLiq)*MainTimeStep )
    if ( CanopyLiqWater <= 1.0e-06 ) CanopyLiqWater = 0.0
    CanopyIceMax = VegFrac * 6.6 * (0.27 + 46.0/SnowfallDensity) * (LeafAreaIndEff + StemAreaIndEff)
    SublimCanopyIce = min( CanopyIce/MainTimeStep, SublimCanopyIce )
    CanopyIce = max( 0.0, CanopyIce+(FrostCanopyIce-SublimCanopyIce)*MainTimeStep )
    if ( CanopyIce <= 1.0e-6 ) CanopyIce = 0.0
    if ( (CanopyIce > 0.0) .and. (CanopyIce >= CanopyLiqWater) ) then
      CanopyWetFrac = max(0.0,CanopyIce) / max(CanopyIceMax,1.0e-06)
    else
      CanopyWetFrac = max(0.0,CanopyLiqWater) / max(CanopyLiqWaterMax,1.0e-06)
    endif
    CanopyWetFrac = min(CanopyWetFrac, 1.0) ** 0.667
    CanopyTotalWater = CanopyLiqWater + CanopyIce
    if ( (CanopyIce > 1.0e-6) .and. (TemperatureCanopy > ConstFreezePoint) ) then
      MeltCanopyIce = min( CanopyIce/MainTimeStep, &
        & (TemperatureCanopy-ConstFreezePoint) * ConstHeatCapacIce * CanopyIce / ConstDensityIce / (MainTimeStep*ConstLatHeatFusion) )
      CanopyIce = max( 0.0, CanopyIce - MeltCanopyIce*MainTimeStep )
      CanopyLiqWater = max( 0.0, CanopyTotalWater - CanopyIce )
      TemperatureCanopy = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TemperatureCanopy
    endif
    if ( (CanopyLiqWater > 1.0e-6) .and. (TemperatureCanopy < ConstFreezePoint) ) then
      FreezeCanopyLiq = min( CanopyLiqWater/MainTimeStep, &
        & (ConstFreezePoint-TemperatureCanopy) * ConstHeatCapacWater * CanopyLiqWater / ConstDensityWater / (MainTimeStep*ConstLatHeatFusion) )
      CanopyLiqWater = max( 0.0, CanopyLiqWater - FreezeCanopyLiq*MainTimeStep )
      CanopyIce = max( 0.0, CanopyTotalWater - CanopyLiqWater )
      TemperatureCanopy = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TemperatureCanopy
    endif
    CanopyTotalWater = CanopyLiqWater + CanopyIce
    EvapCanopyNet = EvapCanopyLiq + SublimCanopyIce - DewCanopyLiq - FrostCanopyIce
    end associate
  end subroutine CanopyHydrology
end module lean_canopyhydrologymod

! Source: physics_noahmp/src/SnowfallBelowCanopyMod.F90
module lean_snowfallbelowcanopymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowfallAfterCanopyIntercept(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: IndNewSnowLayer
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & SnowfallGround => noahmp%water_flux_SnowfallGround , SnowDepthIncr => noahmp%water_flux_SnowDepthIncr , &
      & TemperatureAirRefHeight => noahmp%forcing_TemperatureAirRefHeight , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & SnowDepth => noahmp%water_state_SnowDepth , SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & SnowIce => noahmp%water_state_SnowIce , SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow )
    IndNewSnowLayer = 0
    if ( (NumSnowLayerNeg == 0) .and. (SnowfallGround > 0.0) ) then
      SnowDepth = SnowDepth + SnowDepthIncr * MainTimeStep
      SnowWaterEquiv = SnowWaterEquiv + SnowfallGround * MainTimeStep
    endif
    if ( (NumSnowLayerNeg == 0) .and. (SnowDepth >= 0.025) ) then
      NumSnowLayerNeg = -1
      IndNewSnowLayer = 1
      ThicknessSnowSoilLayer(0) = SnowDepth
      SnowDepth = 0.0
      TemperatureSoilSnow(0) = min(273.16, TemperatureAirRefHeight)
      SnowIce(0) = SnowWaterEquiv
      SnowLiqWater(0) = 0.0
    endif
    if ( (NumSnowLayerNeg < 0) .and. (IndNewSnowLayer == 0) .and. (SnowfallGround > 0.0) ) then
      SnowIce(NumSnowLayerNeg+1) = SnowIce(NumSnowLayerNeg+1) + SnowfallGround * MainTimeStep
      ThicknessSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1) + SnowDepthIncr * MainTimeStep
    endif
    end associate
  end subroutine SnowfallAfterCanopyIntercept
end module lean_snowfallbelowcanopymod

! Source: physics_noahmp/src/SnowpackCompactionMod.F90
module lean_snowpackcompactionmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowpackCompaction(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp) :: SnowBurden
    real(kind=kind_noahmp) :: SnowCompactAgeExpFac
    real(kind=kind_noahmp) :: TempDiff
    real(kind=kind_noahmp) :: SnowVoid
    real(kind=kind_noahmp) :: SnowWatTotTmp
    real(kind=kind_noahmp) :: SnowIceDens
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , SnowIce => noahmp%water_state_SnowIce , &
      & SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & IndexPhaseChange => noahmp%water_state_IndexPhaseChange , &
      & SnowIceFracPrev => noahmp%water_state_SnowIceFracPrev , &
      & SnowCompactBurdenFac => noahmp%water_param_SnowCompactBurdenFac , &
      & SnowCompactAgingFac1 => noahmp%water_param_SnowCompactAgingFac1 , &
      & SnowCompactAgingFac2 => noahmp%water_param_SnowCompactAgingFac2 , &
      & SnowCompactAgingFac3 => noahmp%water_param_SnowCompactAgingFac3 , &
      & SnowCompactAgingMax => noahmp%water_param_SnowCompactAgingMax , &
      & SnowViscosityCoeff => noahmp%water_param_SnowViscosityCoeff , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & CompactionSnowAging => noahmp%water_flux_CompactionSnowAging , &
      & CompactionSnowBurden => noahmp%water_flux_CompactionSnowBurden , &
      & CompactionSnowMelt => noahmp%water_flux_CompactionSnowMelt , &
      & CompactionSnowTot => noahmp%water_flux_CompactionSnowTot , SnowIceFrac => noahmp%water_state_SnowIceFrac )
    CompactionSnowAging(:) = 0.0
    CompactionSnowBurden(:) = 0.0
    CompactionSnowMelt(:) = 0.0
    CompactionSnowTot(:) = 0.0
    SnowIceFrac(:) = 0.0
    SnowBurden = 0.0
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowWatTotTmp = SnowIce(LoopInd) + SnowLiqWater(LoopInd)
      SnowIceFrac(LoopInd) = SnowIce(LoopInd) / SnowWatTotTmp
      SnowVoid = 1.0 - (SnowIce(LoopInd)/ConstDensityIce + SnowLiqWater(LoopInd)/ConstDensityWater) / ThicknessSnowSoilLayer(LoopInd)
      if ( (SnowVoid > 0.001) .and. (SnowIce(LoopInd) > 0.1) ) then
        SnowIceDens = SnowIce(LoopInd) / ThicknessSnowSoilLayer(LoopInd)
        TempDiff = max(0.0, ConstFreezePoint-TemperatureSoilSnow(LoopInd))
        SnowCompactAgeExpFac = exp(-SnowCompactAgingFac2 * TempDiff)
        CompactionSnowAging(LoopInd) = -SnowCompactAgingFac1 * SnowCompactAgeExpFac
        if ( SnowIceDens > SnowCompactAgingMax ) CompactionSnowAging(LoopInd) = CompactionSnowAging(LoopInd) * exp(-46.0e-3*(SnowIceDens-SnowCompactAgingMax))
        if ( SnowLiqWater(LoopInd) > (0.01*ThicknessSnowSoilLayer(LoopInd)) ) CompactionSnowAging(LoopInd) = CompactionSnowAging(LoopInd) * SnowCompactAgingFac3
        CompactionSnowBurden(LoopInd) = -(SnowBurden + 0.5*SnowWatTotTmp) * exp(-0.08*TempDiff-SnowCompactBurdenFac*SnowIceDens) / SnowViscosityCoeff
        if ( IndexPhaseChange(LoopInd) == 1 ) then
          CompactionSnowMelt(LoopInd) = max(0.0, (SnowIceFracPrev(LoopInd)-SnowIceFrac(LoopInd)) / max(1.0e-6, &
            & SnowIceFracPrev(LoopInd)))
          CompactionSnowMelt(LoopInd) = -CompactionSnowMelt(LoopInd) / MainTimeStep
        else
          CompactionSnowMelt(LoopInd) = 0.0
        endif
        CompactionSnowTot(LoopInd) = (CompactionSnowAging(LoopInd) + CompactionSnowBurden(LoopInd) + CompactionSnowMelt(LoopInd) ) * MainTimeStep
        CompactionSnowTot(LoopInd) = max(-0.5, CompactionSnowTot(LoopInd))
        ThicknessSnowSoilLayer(LoopInd) = ThicknessSnowSoilLayer(LoopInd) * (1.0 + CompactionSnowTot(LoopInd))
        ThicknessSnowSoilLayer(LoopInd) = max(ThicknessSnowSoilLayer(LoopInd), &
          & SnowIce(LoopInd)/ConstDensityIce + SnowLiqWater(LoopInd)/ConstDensityWater)
        ThicknessSnowSoilLayer(LoopInd) = min( max( ThicknessSnowSoilLayer(LoopInd), &
          & (SnowIce(LoopInd)+SnowLiqWater(LoopInd))/500.0 ), (SnowIce(LoopInd)+SnowLiqWater(LoopInd))/50.0 )
      endif
      SnowBurden = SnowBurden + SnowWatTotTmp
    enddo
    end associate
  end subroutine SnowpackCompaction
end module lean_snowpackcompactionmod

! Source: physics_noahmp/src/SnowLayerWaterComboMod.F90
module lean_snowlayerwatercombomod
  use Machine
  use lean_constantdefinemod
  implicit none
  contains
  subroutine SnowLayerWaterCombo(ThickLayer1, LiqLayer1, IceLayer1, TempLayer1, ThickLayer2, LiqLayer2, &
    & IceLayer2, TempLayer2)
    implicit none
    real(kind=kind_noahmp), intent(in) :: ThickLayer2
    real(kind=kind_noahmp), intent(in) :: LiqLayer2
    real(kind=kind_noahmp), intent(in) :: IceLayer2
    real(kind=kind_noahmp), intent(in) :: TempLayer2
    real(kind=kind_noahmp), intent(inout) :: ThickLayer1
    real(kind=kind_noahmp), intent(inout) :: LiqLayer1
    real(kind=kind_noahmp), intent(inout) :: IceLayer1
    real(kind=kind_noahmp), intent(inout) :: TempLayer1
    real(kind=kind_noahmp) :: ThickLayerComb
    real(kind=kind_noahmp) :: LiqLayerComb
    real(kind=kind_noahmp) :: IceLayerComb
    real(kind=kind_noahmp) :: TempLayerComb
    real(kind=kind_noahmp) :: EnthLayer1
    real(kind=kind_noahmp) :: EnthLayer2
    real(kind=kind_noahmp) :: EnthLayerComb
    ThickLayerComb = ThickLayer1 + ThickLayer2
    IceLayerComb = IceLayer1 + IceLayer2
    LiqLayerComb = LiqLayer1 + LiqLayer2
    EnthLayer1 = (ConstHeatCapacIce*IceLayer1 + ConstHeatCapacWater*LiqLayer1) * (TempLayer1-ConstFreezePoint) + ConstLatHeatFusion*LiqLayer1
    EnthLayer2 = (ConstHeatCapacIce*IceLayer2 + ConstHeatCapacWater*LiqLayer2) * (TempLayer2-ConstFreezePoint) + ConstLatHeatFusion*LiqLayer2
    EnthLayerComb = EnthLayer1 + EnthLayer2
    if ( EnthLayerComb < 0.0 ) then
      TempLayerComb = ConstFreezePoint + EnthLayerComb / (ConstHeatCapacIce*IceLayerComb + ConstHeatCapacWater*LiqLayerComb)
    else if ( EnthLayerComb <= (ConstLatHeatFusion*LiqLayerComb) ) then
      TempLayerComb = ConstFreezePoint
    else
      TempLayerComb = ConstFreezePoint + (EnthLayerComb-ConstLatHeatFusion*LiqLayerComb) / (ConstHeatCapacIce*IceLayerComb + ConstHeatCapacWater*LiqLayerComb)
    endif
    ThickLayer1 = ThickLayerComb
    IceLayer1 = IceLayerComb
    LiqLayer1 = LiqLayerComb
    TempLayer1 = TempLayerComb
  end subroutine SnowLayerWaterCombo
end module lean_snowlayerwatercombomod

! Source: physics_noahmp/src/SnowLayerCombineMod.F90
module lean_snowlayercombinemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowlayerwatercombomod, only: SnowLayerWaterCombo
  implicit none
  contains
  subroutine SnowLayerCombine(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: I,J,K,L
    integer :: NumSnowLayerOld
    integer :: IndLayer
    integer :: IndNeighbor
    real(kind=kind_noahmp) :: SnowIceTmp
    real(kind=kind_noahmp) :: SnowLiqTmp
    real(kind=kind_noahmp) :: SnowThickMin(3)
    data SnowThickMin /0.025, 0.025, 0.1/
    associate( NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & SnowDepth => noahmp%water_state_SnowDepth , SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & SnowIce => noahmp%water_state_SnowIce , SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilIce => noahmp%water_state_SoilIce , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & PondSfcThinSnwComb => noahmp%water_state_PondSfcThinSnwComb , &
      & PondSfcThinSnwTrans => noahmp%water_state_PondSfcThinSnwTrans )
    NumSnowLayerOld = NumSnowLayerNeg
    do J = NumSnowLayerOld+1,0
      if ( SnowIce(J) <= 0.1 ) then
        if ( J /= 0 ) then
          SnowLiqWater(J+1) = SnowLiqWater(J+1) + SnowLiqWater(J)
          SnowIce(J+1) = SnowIce(J+1) + SnowIce(J)
          ThicknessSnowSoilLayer(J+1) = ThicknessSnowSoilLayer(J+1) + ThicknessSnowSoilLayer(J)
        else
          if ( NumSnowLayerNeg < -1 ) then
            SnowLiqWater(J-1) = SnowLiqWater(J-1) + SnowLiqWater(J)
            SnowIce(J-1) = SnowIce(J-1) + SnowIce(J)
            ThicknessSnowSoilLayer(J-1) = ThicknessSnowSoilLayer(J-1) + ThicknessSnowSoilLayer(J)
          else
            if ( SnowIce(J) >= 0.0 ) then
              PondSfcThinSnwComb = SnowLiqWater(J)
              SnowWaterEquiv = SnowIce(J)
              SnowDepth = ThicknessSnowSoilLayer(J)
            else
              PondSfcThinSnwComb = SnowLiqWater(J) + SnowIce(J)
              if ( PondSfcThinSnwComb < 0.0 ) then
                SoilIce(1) = SoilIce(1) + PondSfcThinSnwComb/(ThicknessSnowSoilLayer(1)*1000.0)
                PondSfcThinSnwComb = 0.0
              endif
              SnowWaterEquiv = 0.0
              SnowDepth = 0.0
            endif
            SnowLiqWater(J) = 0.0
            SnowIce(J) = 0.0
            ThicknessSnowSoilLayer(J) = 0.0
          endif
        endif
        if ( (J > NumSnowLayerNeg+1) .and. (NumSnowLayerNeg < -1) ) then
          do I = J, NumSnowLayerNeg+2, -1
            TemperatureSoilSnow(I) = TemperatureSoilSnow(I-1)
            SnowLiqWater(I) = SnowLiqWater(I-1)
            SnowIce(I) = SnowIce(I-1)
            ThicknessSnowSoilLayer(I) = ThicknessSnowSoilLayer(I-1)
          enddo
        endif
        NumSnowLayerNeg = NumSnowLayerNeg + 1
      endif
    enddo
    if ( SoilIce(1) < 0.0) then
      SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
      SoilIce(1) = 0.0
    endif
    if ( NumSnowLayerNeg ==0 ) return
    SnowWaterEquiv = 0.0
    SnowDepth = 0.0
    SnowIceTmp = 0.0
    SnowLiqTmp = 0.0
    do J = NumSnowLayerNeg+1, 0
      SnowWaterEquiv = SnowWaterEquiv + SnowIce(J) + SnowLiqWater(J)
      SnowDepth = SnowDepth + ThicknessSnowSoilLayer(J)
      SnowIceTmp = SnowIceTmp + SnowIce(J)
      SnowLiqTmp = SnowLiqTmp + SnowLiqWater(J)
    enddo
    if ( (SnowDepth < 0.025) .and. (NumSnowLayerNeg < 0) ) then
      NumSnowLayerNeg = 0
      SnowWaterEquiv = SnowIceTmp
      PondSfcThinSnwTrans = SnowLiqTmp
      if ( SnowWaterEquiv <= 0.0 ) SnowDepth = 0.0
    endif
    if ( NumSnowLayerNeg < -1 ) then
      NumSnowLayerOld = NumSnowLayerNeg
      IndLayer = 1
      do I = NumSnowLayerOld+1, 0
        if ( ThicknessSnowSoilLayer(I) < SnowThickMin(IndLayer) ) then
          if ( I == NumSnowLayerNeg+1 ) then
            IndNeighbor = I + 1
          else if ( I == 0 ) then
            IndNeighbor = I - 1
          else
            IndNeighbor = I + 1
            if ( (ThicknessSnowSoilLayer(I-1)+ThicknessSnowSoilLayer(I)) < (ThicknessSnowSoilLayer(I+1)+ThicknessSnowSoilLayer(I)) ) IndNeighbor = I-1
          endif
          if ( IndNeighbor > I ) then
            J = IndNeighbor
            L = I
          else
            J = I
            L = IndNeighbor
          endif
          call SnowLayerWaterCombo(ThicknessSnowSoilLayer(J), SnowLiqWater(J), SnowIce(J), TemperatureSoilSnow(J), &
            & ThicknessSnowSoilLayer(L), SnowLiqWater(L), SnowIce(L), TemperatureSoilSnow(L) )
          if ( (J-1) > (NumSnowLayerNeg+1) ) then
            do K = J-1, NumSnowLayerNeg+2, -1
              TemperatureSoilSnow(K) = TemperatureSoilSnow(K-1)
              SnowIce(K) = SnowIce(K-1)
              SnowLiqWater(K) = SnowLiqWater(K-1)
              ThicknessSnowSoilLayer(K) = ThicknessSnowSoilLayer(K-1)
            enddo
          endif
          NumSnowLayerNeg = NumSnowLayerNeg + 1
          if ( NumSnowLayerNeg >= -1 ) Exit
        else
          IndLayer = IndLayer + 1
        endif
      enddo
    endif
    end associate
  end subroutine SnowLayerCombine
end module lean_snowlayercombinemod

! Source: physics_noahmp/src/SnowLayerDivideMod.F90
module lean_snowlayerdividemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowlayerwatercombomod, only: SnowLayerWaterCombo
  implicit none
  contains
  subroutine SnowLayerDivide(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    integer :: NumSnowLayerTmp
    real(kind=kind_noahmp) :: SnowThickCombTmp
    real(kind=kind_noahmp) :: SnowIceExtra
    real(kind=kind_noahmp) :: SnowLiqExtra
    real(kind=kind_noahmp) :: SnowFracExtra
    real(kind=kind_noahmp) :: SnowTempGrad
    real(kind=kind_noahmp), dimension(1:3) :: SnowThickTmp
    real(kind=kind_noahmp), dimension(1:3) :: SnowIceTmp
    real(kind=kind_noahmp), dimension(1:3) :: SnowLiqTmp
    real(kind=kind_noahmp), dimension(1:3) :: TemperatureSnowTmp
    associate( NumSnowLayerMax => 3 , NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , SnowIce => noahmp%water_state_SnowIce , &
      & SnowLiqWater => noahmp%water_state_SnowLiqWater )
    SnowIceTmp (:) = 0.0
    SnowLiqTmp (:) = 0.0
    TemperatureSnowTmp(:) = 0.0
    SnowThickTmp (:) = 0.0
    do LoopInd = 1, NumSnowLayerMax
      if ( LoopInd <= abs(NumSnowLayerNeg) ) then
        SnowThickTmp(LoopInd) = ThicknessSnowSoilLayer(LoopInd+NumSnowLayerNeg)
        SnowIceTmp(LoopInd) = SnowIce(LoopInd+NumSnowLayerNeg)
        SnowLiqTmp(LoopInd) = SnowLiqWater(LoopInd+NumSnowLayerNeg)
        TemperatureSnowTmp(LoopInd) = TemperatureSoilSnow(LoopInd+NumSnowLayerNeg)
      endif
    enddo
    NumSnowLayerTmp = abs(NumSnowLayerNeg)
    if ( NumSnowLayerTmp == 1 ) then
      if ( SnowThickTmp(1) > 0.05 ) then
        NumSnowLayerTmp = 2
        SnowThickTmp(1) = SnowThickTmp(1)/2.0
        SnowIceTmp(1) = SnowIceTmp(1)/2.0
        SnowLiqTmp(1) = SnowLiqTmp(1)/2.0
        SnowThickTmp(2) = SnowThickTmp(1)
        SnowIceTmp(2) = SnowIceTmp(1)
        SnowLiqTmp(2) = SnowLiqTmp(1)
        TemperatureSnowTmp(2) = TemperatureSnowTmp(1)
      endif
    endif
    if ( NumSnowLayerTmp > 1 ) then
      if ( SnowThickTmp(1) > 0.05 ) then
        SnowThickCombTmp = SnowThickTmp(1) - 0.05
        SnowFracExtra = SnowThickCombTmp / SnowThickTmp(1)
        SnowIceExtra = SnowFracExtra * SnowIceTmp(1)
        SnowLiqExtra = SnowFracExtra * SnowLiqTmp(1)
        SnowFracExtra = 0.05 / SnowThickTmp(1)
        SnowIceTmp(1) = SnowFracExtra*SnowIceTmp(1)
        SnowLiqTmp(1) = SnowFracExtra*SnowLiqTmp(1)
        SnowThickTmp(1) = 0.05
        call SnowLayerWaterCombo(SnowThickTmp(2), SnowLiqTmp(2), SnowIceTmp(2), TemperatureSnowTmp(2), &
          & SnowThickCombTmp, SnowLiqExtra, SnowIceExtra, TemperatureSnowTmp(1))
        if ( (NumSnowLayerTmp <= 2) .and. (SnowThickTmp(2) > 0.20) ) then
          NumSnowLayerTmp = 3
          SnowTempGrad = (TemperatureSnowTmp(1) - TemperatureSnowTmp(2)) / ((SnowThickTmp(1)+SnowThickTmp(2)) / 2.0)
          SnowThickTmp(2) = SnowThickTmp(2) / 2.0
          SnowIceTmp(2) = SnowIceTmp(2) / 2.0
          SnowLiqTmp(2) = SnowLiqTmp(2) / 2.0
          SnowThickTmp(3) = SnowThickTmp(2)
          SnowIceTmp(3) = SnowIceTmp(2)
          SnowLiqTmp(3) = SnowLiqTmp(2)
          TemperatureSnowTmp(3) = TemperatureSnowTmp(2) - SnowTempGrad * SnowThickTmp(2) / 2.0
          if ( TemperatureSnowTmp(3) >= ConstFreezePoint ) then
            TemperatureSnowTmp(3) = TemperatureSnowTmp(2)
          else
            TemperatureSnowTmp(2) = TemperatureSnowTmp(2) + SnowTempGrad * SnowThickTmp(2) / 2.0
          endif
        endif
      endif
    endif
    if ( NumSnowLayerTmp > 2 ) then
      if ( SnowThickTmp(2) > 0.2 ) then
        SnowThickCombTmp = SnowThickTmp(2) - 0.2
        SnowFracExtra = SnowThickCombTmp / SnowThickTmp(2)
        SnowIceExtra = SnowFracExtra * SnowIceTmp(2)
        SnowLiqExtra = SnowFracExtra * SnowLiqTmp(2)
        SnowFracExtra = 0.2 / SnowThickTmp(2)
        SnowIceTmp(2) = SnowFracExtra * SnowIceTmp(2)
        SnowLiqTmp(2) = SnowFracExtra * SnowLiqTmp(2)
        SnowThickTmp(2) = 0.2
        call SnowLayerWaterCombo(SnowThickTmp(3), SnowLiqTmp(3), SnowIceTmp(3), TemperatureSnowTmp(3), &
          & SnowThickCombTmp, SnowLiqExtra, SnowIceExtra, TemperatureSnowTmp(2))
      endif
    endif
    NumSnowLayerNeg = -NumSnowLayerTmp
    do LoopInd = NumSnowLayerNeg+1, 0
      ThicknessSnowSoilLayer(LoopInd) = SnowThickTmp(LoopInd-NumSnowLayerNeg)
      SnowIce(LoopInd) = SnowIceTmp(LoopInd-NumSnowLayerNeg)
      SnowLiqWater(LoopInd) = SnowLiqTmp(LoopInd-NumSnowLayerNeg)
      TemperatureSoilSnow(LoopInd) = TemperatureSnowTmp(LoopInd-NumSnowLayerNeg)
    enddo
    end associate
  end subroutine SnowLayerDivide
end module lean_snowlayerdividemod

! Source: physics_noahmp/src/SnowpackHydrologyMod.F90
module lean_snowpackhydrologymod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowlayercombinemod, only : SnowLayerCombine
  implicit none
  contains
  subroutine SnowpackHydrology(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp) :: InflowSnowLayer
    real(kind=kind_noahmp) :: OutflowSnowLayer
    real(kind=kind_noahmp) :: SnowIceTmp
    real(kind=kind_noahmp) :: SnowWaterRatio
    real(kind=kind_noahmp) :: SnowWaterTmp
    real(kind=kind_noahmp), dimension(-2:0) :: SnowLiqVol
    real(kind=kind_noahmp), dimension(-2:0) :: SnowIceVol
    associate( NumSnowLayerMax => 3 , MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & FrostSnowSfcIce => noahmp%water_flux_FrostSnowSfcIce , &
      & SublimSnowSfcIce => noahmp%water_flux_SublimSnowSfcIce , &
      & RainfallGround => noahmp%water_flux_RainfallGround , &
      & SnowLiqFracMax => noahmp%water_param_SnowLiqFracMax , &
      & SnowLiqHoldCap => noahmp%water_param_SnowLiqHoldCap , &
      & SnowLiqReleaseFac => noahmp%water_param_SnowLiqReleaseFac , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & SnowDepth => noahmp%water_state_SnowDepth , SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & SnowIce => noahmp%water_state_SnowIce , SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilIce => noahmp%water_state_SoilIce , &
      & SnowEffPorosity => noahmp%water_state_SnowEffPorosity , SnowBotOutflow => noahmp%water_flux_SnowBotOutflow )
    SnowLiqVol(:) = 0.0
    SnowIceVol(:) = 0.0
    SnowEffPorosity(:) = 0.0
    SnowBotOutflow = 0.0
    InflowSnowLayer = 0.0
    OutflowSnowLayer = 0.0
    if ( SnowWaterEquiv == 0.0 ) then
      SoilIce(1) = SoilIce(1) + (FrostSnowSfcIce-SublimSnowSfcIce) * MainTimeStep / (ThicknessSnowSoilLayer(1)*1000.0)
      if ( SoilIce(1) < 0.0 ) then
        SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
        SoilIce(1) = 0.0
      endif
    endif
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) ) then
      SnowWaterTmp = SnowWaterEquiv
      SnowWaterEquiv = SnowWaterEquiv - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
      SnowWaterRatio = SnowWaterEquiv / SnowWaterTmp
      SnowDepth = max(0.0, SnowWaterRatio*SnowDepth )
      SnowDepth = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)
      if ( SnowWaterEquiv < 0.0 ) then
        SoilIce(1) = SoilIce(1) + SnowWaterEquiv / (ThicknessSnowSoilLayer(1)*1000.0)
        SnowWaterEquiv = 0.0
        SnowDepth = 0.0
      endif
      if ( SoilIce(1) < 0.0 ) then
        SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
        SoilIce(1) = 0.0
      endif
    endif
    if ( (SnowDepth <= 1.0e-8) .or. (SnowWaterEquiv <= 1.0e-6) ) then
      SnowDepth = 0.0
      SnowWaterEquiv = 0.0
    endif
    if ( NumSnowLayerNeg < 0 ) then
      SnowIceTmp = SnowIce(NumSnowLayerNeg+1) - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
      SnowIce(NumSnowLayerNeg+1) = SnowIceTmp
      if ( (SnowIceTmp < 1.0e-6) .and. (NumSnowLayerNeg < 0) ) call SnowLayerCombine(noahmp)
      if ( NumSnowLayerNeg < 0 ) then
        SnowLiqWater(NumSnowLayerNeg+1) = SnowLiqWater(NumSnowLayerNeg+1) + RainfallGround * MainTimeStep
        SnowLiqWater(NumSnowLayerNeg+1) = max(0.0, SnowLiqWater(NumSnowLayerNeg+1))
      endif
    endif
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowIceVol(LoopInd) = min(1.0, SnowIce(LoopInd)/(ThicknessSnowSoilLayer(LoopInd)*ConstDensityIce))
      SnowEffPorosity(LoopInd) = 1.0 - SnowIceVol(LoopInd)
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      SnowLiqWater(LoopInd) = SnowLiqWater(LoopInd) + InflowSnowLayer
      SnowLiqVol(LoopInd) = SnowLiqWater(LoopInd) / (ThicknessSnowSoilLayer(LoopInd)*ConstDensityWater)
      OutflowSnowLayer = max(0.0, &
        & (SnowLiqVol(LoopInd)-SnowLiqHoldCap*SnowEffPorosity(LoopInd)) * ThicknessSnowSoilLayer(LoopInd))
      if ( LoopInd == 0 ) then
        OutflowSnowLayer = max((SnowLiqVol(LoopInd)-SnowEffPorosity(LoopInd)) * ThicknessSnowSoilLayer(LoopInd), &
          & SnowLiqReleaseFac * MainTimeStep * OutflowSnowLayer)
      endif
      OutflowSnowLayer = OutflowSnowLayer * ConstDensityWater
      SnowLiqWater(LoopInd) = SnowLiqWater(LoopInd) - OutflowSnowLayer
      if ( (SnowLiqWater(LoopInd)/(SnowIce(LoopInd)+SnowLiqWater(LoopInd))) > SnowLiqFracMax ) then
        OutflowSnowLayer = OutflowSnowLayer + (SnowLiqWater(LoopInd) - SnowLiqFracMax / (1.0-SnowLiqFracMax) * SnowIce(LoopInd))
        SnowLiqWater(LoopInd) = SnowLiqFracMax / (1.0 - SnowLiqFracMax) * SnowIce(LoopInd)
      endif
      InflowSnowLayer = OutflowSnowLayer
    enddo
    do LoopInd = NumSnowLayerNeg+1, 0
      ThicknessSnowSoilLayer(LoopInd) = max(ThicknessSnowSoilLayer(LoopInd), &
        & SnowLiqWater(LoopInd)/ConstDensityWater+SnowIce(LoopInd)/ConstDensityIce)
    enddo
    SnowBotOutflow = OutflowSnowLayer / MainTimeStep
    end associate
  end subroutine SnowpackHydrology
end module lean_snowpackhydrologymod

! Source: physics_noahmp/src/SnowWaterMainMod.F90
module lean_snowwatermainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_snowfallbelowcanopymod, only : SnowfallAfterCanopyIntercept
  use lean_snowpackcompactionmod, only : SnowpackCompaction
  use lean_snowlayercombinemod, only : SnowLayerCombine
  use lean_snowlayerdividemod, only : SnowLayerDivide
  use lean_snowpackhydrologymod, only : SnowpackHydrology
  implicit none
  contains
  subroutine SnowWaterMain(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    real(kind=kind_noahmp) :: SnowDensBulk
    associate( NumSnowLayerMax => 3 , NumSoilLayer => 4 , MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & SnoWatEqvMaxGlacier => noahmp%water_param_SnoWatEqvMaxGlacier , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & DepthSnowSoilLayer => noahmp%config_domain_DepthSnowSoilLayer , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , SnowDepth => noahmp%water_state_SnowDepth , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , SnowIce => noahmp%water_state_SnowIce , &
      & SnowLiqWater => noahmp%water_state_SnowLiqWater , &
      & TemperatureSoilSnow => noahmp%energy_state_TemperatureSoilSnow , &
      & GlacierExcessFlow => noahmp%water_flux_GlacierExcessFlow , &
      & PondSfcThinSnwComb => noahmp%water_state_PondSfcThinSnwComb , &
      & PondSfcThinSnwTrans => noahmp%water_state_PondSfcThinSnwTrans )
    GlacierExcessFlow = 0.0
    PondSfcThinSnwComb = 0.0
    PondSfcThinSnwTrans = 0.0
    call SnowfallAfterCanopyIntercept(noahmp)
    if ( NumSnowLayerNeg < 0 ) call SnowpackCompaction(noahmp)
    if ( NumSnowLayerNeg < 0 ) call SnowLayerCombine(noahmp)
    if ( NumSnowLayerNeg < 0 ) call SnowLayerDivide(noahmp)
    call SnowpackHydrology(noahmp)
    do LoopInd = -NumSnowLayerMax+1, NumSnowLayerNeg
      SnowIce(LoopInd) = 0.0
      SnowLiqWater(LoopInd) = 0.0
      TemperatureSoilSnow(LoopInd) = 0.0
      ThicknessSnowSoilLayer(LoopInd) = 0.0
      DepthSnowSoilLayer(LoopInd) = 0.0
    enddo
    if ( SnowWaterEquiv > SnoWatEqvMaxGlacier ) then
      SnowDensBulk = SnowIce(0) / ThicknessSnowSoilLayer(0)
      GlacierExcessFlow = SnowWaterEquiv - SnoWatEqvMaxGlacier
      SnowIce(0) = SnowIce(0) - GlacierExcessFlow
      ThicknessSnowSoilLayer(0) = ThicknessSnowSoilLayer(0) - GlacierExcessFlow / SnowDensBulk
      GlacierExcessFlow = GlacierExcessFlow / MainTimeStep
    endif
    if ( NumSnowLayerNeg < 0 ) then
      SnowWaterEquiv = 0.0
      do LoopInd = NumSnowLayerNeg+1, 0
        SnowWaterEquiv = SnowWaterEquiv + SnowIce(LoopInd) + SnowLiqWater(LoopInd)
      enddo
    endif
    do LoopInd = NumSnowLayerNeg+1, 0
      ThicknessSnowSoilLayer(LoopInd) = -ThicknessSnowSoilLayer(LoopInd)
    enddo
    ThicknessSnowSoilLayer(1) = DepthSoilLayer(1)
    do LoopInd = 2, NumSoilLayer
      ThicknessSnowSoilLayer(LoopInd) = DepthSoilLayer(LoopInd) - DepthSoilLayer(LoopInd-1)
    enddo
    DepthSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    do LoopInd = NumSnowLayerNeg+2, NumSoilLayer
      DepthSnowSoilLayer(LoopInd) = DepthSnowSoilLayer(LoopInd-1) + ThicknessSnowSoilLayer(LoopInd)
    enddo
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
      ThicknessSnowSoilLayer(LoopInd) = -ThicknessSnowSoilLayer(LoopInd)
    enddo
    if ( NumSnowLayerNeg < 0 ) then
      SnowDepth = 0.0
      do LoopInd = NumSnowLayerNeg+1, 0
        SnowDepth = SnowDepth + ThicknessSnowSoilLayer(LoopInd)
      enddo
    endif
    if ( (SnowDepth <= 1.0e-6) .or. (SnowWaterEquiv <= 1.0e-6) ) then
      SnowDepth = 0.0
      SnowWaterEquiv = 0.0
    endif
    end associate
  end subroutine SnowWaterMain
end module lean_snowwatermainmod

! Source: physics_noahmp/src/IrrigationFloodMod.F90
module lean_irrigationfloodmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_irrigationinfilphilipmod, only : IrrigationInfilPhilip
  implicit none
  contains
  subroutine IrrigationFlood(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: InfilRateSfc
    associate( SoilTimeStep => noahmp%config_domain_SoilTimeStep , &
      & NumSoilTimeStep => noahmp%config_domain_NumSoilTimeStep , &
      & IrriFloodRateFac => noahmp%water_param_IrriFloodRateFac , &
      & IrrigationFracFlood => noahmp%water_state_IrrigationFracFlood , &
      & IrrigationAmtFlood => noahmp%water_state_IrrigationAmtFlood , &
      & SoilSfcInflowAcc => noahmp%water_flux_SoilSfcInflowAcc , &
      & IrrigationRateFlood => noahmp%water_flux_IrrigationRateFlood )
    InfilRateSfc = 0.0
    call IrrigationInfilPhilip(noahmp, SoilTimeStep, InfilRateSfc)
    IrrigationRateFlood = InfilRateSfc * SoilTimeStep * IrriFloodRateFac
    IrrigationRateFlood = IrrigationRateFlood * IrrigationFracFlood
    if ( IrrigationRateFlood >= IrrigationAmtFlood ) then
      IrrigationRateFlood = IrrigationAmtFlood
      IrrigationAmtFlood = 0.0
    else
      IrrigationAmtFlood = IrrigationAmtFlood - IrrigationRateFlood
    endif
    SoilSfcInflowAcc = SoilSfcInflowAcc + (IrrigationRateFlood / SoilTimeStep * NumSoilTimeStep)
    end associate
  end subroutine IrrigationFlood
end module lean_irrigationfloodmod

! Source: physics_noahmp/src/IrrigationMicroMod.F90
module lean_irrigationmicromod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_irrigationinfilphilipmod, only : IrrigationInfilPhilip
  implicit none
  contains
  subroutine IrrigationMicro(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    real(kind=kind_noahmp) :: InfilRateSfc
    real(kind=kind_noahmp) :: IrriRateTmp
    associate( SoilTimeStep => noahmp%config_domain_SoilTimeStep , &
      & DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & IrrigationFracMicro => noahmp%water_state_IrrigationFracMicro , &
      & IrriMicroRate => noahmp%water_param_IrriMicroRate , SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & IrrigationAmtMicro => noahmp%water_state_IrrigationAmtMicro , &
      & IrrigationRateMicro => noahmp%water_flux_IrrigationRateMicro )
    InfilRateSfc = 0.0
    IrriRateTmp = 0.0
    call IrrigationInfilPhilip(noahmp, SoilTimeStep, InfilRateSfc)
    IrriRateTmp = IrriMicroRate * (1.0/1000.0) * SoilTimeStep/ 3600.0
    IrrigationRateMicro = min(0.5*InfilRateSfc*SoilTimeStep, IrrigationAmtMicro, IrriRateTmp)
    IrrigationRateMicro = IrrigationRateMicro * IrrigationFracMicro
    if ( IrrigationRateMicro >= IrrigationAmtMicro ) then
      IrrigationRateMicro = IrrigationAmtMicro
      IrrigationAmtMicro = 0.0
    else
      IrrigationAmtMicro = IrrigationAmtMicro - IrrigationRateMicro
    endif
    SoilLiqWater(1) = SoilLiqWater(1) + (IrrigationRateMicro / (-1.0*DepthSoilLayer(1)))
    end associate
  end subroutine IrrigationMicro
end module lean_irrigationmicromod

! Source: physics_noahmp/src/RunoffSurfaceFreeDrainMod.F90
module lean_runoffsurfacefreedrainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_soilhydraulicpropertymod, only : SoilDiffusivityConductivityOpt2
  implicit none
  contains
  subroutine RunoffSurfaceFreeDrain(noahmp, TimeStep)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in) :: TimeStep
    integer :: IndSoilFrz
    integer :: LoopInd1, LoopInd2, LoopInd3
    integer, parameter :: FrzSoilFac = 3
    real(kind=kind_noahmp) :: FracVoidRem
    real(kind=kind_noahmp) :: SoilWatHoldMaxRem
    real(kind=kind_noahmp) :: WaterInSfc
    real(kind=kind_noahmp) :: TimeStepDay
    real(kind=kind_noahmp) :: SoilWatHoldMaxAcc
    real(kind=kind_noahmp) :: SoilIceWatTmp
    real(kind=kind_noahmp) :: SoilImpervFrac
    real(kind=kind_noahmp) :: IndAcc
    real(kind=kind_noahmp) :: SoilIceCoeff
    real(kind=kind_noahmp) :: SoilWatDiffusivity
    real(kind=kind_noahmp) :: SoilWatConductivity
    real(kind=kind_noahmp) :: SoilWatHoldCap
    real(kind=kind_noahmp) :: InfilRateMax
    real(kind=kind_noahmp), dimension(1:4) :: SoilWatMaxHold
    associate( NumSoilLayer => 4 , DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & FlagUrban => noahmp%config_domain_FlagUrban , SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & SoilIce => noahmp%water_state_SoilIce , SoilIceMax => noahmp%water_state_SoilIceMax , &
      & SoilSfcInflowMean => noahmp%water_flux_SoilSfcInflowMean , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & SoilMoistureWilt => noahmp%water_param_SoilMoistureWilt , &
      & SoilInfilMaxCoeff => noahmp%water_param_SoilInfilMaxCoeff , &
      & SoilImpervFracCoeff => noahmp%water_param_SoilImpervFracCoeff , &
      & RunoffSurface => noahmp%water_flux_RunoffSurface , InfilRateSfc => noahmp%water_flux_InfilRateSfc )
    SoilWatMaxHold(1:NumSoilLayer) = 0.0
    if ( SoilSfcInflowMean > 0.0 ) then
      TimeStepDay = TimeStep / 86400.0
      SoilWatHoldCap = SoilMoistureSat(1) - SoilMoistureWilt(1)
      SoilWatMaxHold(1) = -DepthSoilLayer(1) * SoilWatHoldCap
      SoilIceWatTmp = -DepthSoilLayer(1) * SoilIce(1)
      SoilWatMaxHold(1) = SoilWatMaxHold(1) * (1.0-(SoilLiqWater(1)+SoilIce(1)-SoilMoistureWilt(1)) / SoilWatHoldCap)
      SoilWatHoldMaxAcc = SoilWatMaxHold(1)
      do LoopInd3 = 2, NumSoilLayer
        SoilIceWatTmp = SoilIceWatTmp + (DepthSoilLayer(LoopInd3-1) - DepthSoilLayer(LoopInd3))*SoilIce(LoopInd3)
        SoilWatMaxHold(LoopInd3) = (DepthSoilLayer(LoopInd3-1) - DepthSoilLayer(LoopInd3)) * SoilWatHoldCap
        SoilWatMaxHold(LoopInd3) = SoilWatMaxHold(LoopInd3) * (1.0 - (SoilLiqWater(LoopInd3) + SoilIce(LoopInd3) - SoilMoistureWilt(LoopInd3)) / SoilWatHoldCap)
        SoilWatHoldMaxAcc = SoilWatHoldMaxAcc + SoilWatMaxHold(LoopInd3)
      enddo
      FracVoidRem = 1.0 - exp(-1.0 * SoilInfilMaxCoeff * TimeStepDay)
      SoilWatHoldMaxRem = SoilWatHoldMaxAcc * FracVoidRem
      WaterInSfc = max(0.0, SoilSfcInflowMean * TimeStep)
      InfilRateMax = (WaterInSfc * (SoilWatHoldMaxRem/(WaterInSfc + SoilWatHoldMaxRem))) / TimeStep
      SoilImpervFrac = 1.0
      if ( SoilIceWatTmp > 1.0e-2 ) then
        SoilIceCoeff = FrzSoilFac * SoilImpervFracCoeff / SoilIceWatTmp
        IndAcc = 1.0
        IndSoilFrz = FrzSoilFac - 1
        do LoopInd1 = 1, IndSoilFrz
          LoopInd3 = 1
          do LoopInd2 = LoopInd1+1, IndSoilFrz
            LoopInd3 = LoopInd3 * LoopInd2
          enddo
          IndAcc = IndAcc + (SoilIceCoeff ** (FrzSoilFac-LoopInd1)) / float(LoopInd3)
        enddo
        SoilImpervFrac = 1.0 - exp(-SoilIceCoeff) * IndAcc
      endif
      InfilRateMax = InfilRateMax * SoilImpervFrac
      call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, SoilLiqWater(1), &
        & SoilIceMax, 1)
      InfilRateMax = max(InfilRateMax, SoilWatConductivity)
      InfilRateMax = min(InfilRateMax, WaterInSfc/TimeStep)
      RunoffSurface = max(0.0, SoilSfcInflowMean-InfilRateMax)
      InfilRateSfc = SoilSfcInflowMean - RunoffSurface
    endif
    end associate
  end subroutine RunoffSurfaceFreeDrain
end module lean_runoffsurfacefreedrainmod

! Source: physics_noahmp/src/RunoffSubSurfaceDrainageMod.F90
module lean_runoffsubsurfacedrainagemod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine RunoffSubSurfaceDrainage(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( DrainSoilBot => noahmp%water_flux_DrainSoilBot , &
      & RunoffSubsurface => noahmp%water_flux_RunoffSubsurface )
    RunoffSubsurface = RunoffSubsurface + DrainSoilBot
    end associate
  end subroutine RunoffSubSurfaceDrainage
end module lean_runoffsubsurfacedrainagemod

! Source: physics_noahmp/src/SoilWaterDiffusionRichardsMod.F90
module lean_soilwaterdiffusionrichardsmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_soilhydraulicpropertymod
  implicit none
  contains
  subroutine SoilWaterDiffusionRichards(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatRight
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft1
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft2
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft3
    integer :: LoopInd
    real(kind=kind_noahmp) :: DepthSnowSoilTmp
    real(kind=kind_noahmp) :: SoilMoistTmpToWT
    real(kind=kind_noahmp) :: SoilMoistBotTmp
    real(kind=kind_noahmp), dimension(1:4) :: DepthSnowSoilInv
    real(kind=kind_noahmp), dimension(1:4) :: SoilThickTmp
    real(kind=kind_noahmp), dimension(1:4) :: SoilWaterGrad
    real(kind=kind_noahmp), dimension(1:4) :: WaterExcess
    real(kind=kind_noahmp), dimension(1:4) :: SoilMoistureTmp
    associate( NumSoilLayer => 4 , DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & OptSoilPermeabilityFrozen => 1 , OptRunoffSubsurface => 3 , &
      & SoilDrainSlope => noahmp%water_param_SoilDrainSlope , InfilRateSfc => noahmp%water_flux_InfilRateSfc , &
      & EvapSoilSfcLiqMean => noahmp%water_flux_EvapSoilSfcLiqMean , &
      & TranspWatLossSoilMean => noahmp%water_flux_TranspWatLossSoilMean , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & WaterTableDepth => noahmp%water_state_WaterTableDepth , &
      & SoilImpervFrac => noahmp%water_state_SoilImpervFrac , &
      & SoilImpervFracMax => noahmp%water_state_SoilImpervFracMax , SoilIceMax => noahmp%water_state_SoilIceMax , &
      & SoilMoistureToWT => noahmp%water_state_SoilMoistureToWT , &
      & SoilWatConductivity => noahmp%water_state_SoilWatConductivity , &
      & SoilWatDiffusivity => noahmp%water_state_SoilWatDiffusivity , DrainSoilBot => noahmp%water_flux_DrainSoilBot )
    MatRight(:) = 0.0
    MatLeft1(:) = 0.0
    MatLeft2(:) = 0.0
    MatLeft3(:) = 0.0
    DepthSnowSoilInv(:) = 0.0
    SoilThickTmp(:) = 0.0
    SoilWaterGrad(:) = 0.0
    WaterExcess(:) = 0.0
    SoilMoistureTmp(:) = 0.0
    do LoopInd = 1, NumSoilLayer
      call SoilDiffusivityConductivityOpt1(noahmp,SoilWatDiffusivity(LoopInd),SoilWatConductivity(LoopInd), &
        & SoilMoisture(LoopInd),SoilImpervFrac(LoopInd),LoopInd)
      SoilMoistureTmp(LoopInd) = SoilMoisture(LoopInd)
    enddo
    do LoopInd = 1, NumSoilLayer
      if ( LoopInd == 1 ) then
        SoilThickTmp(LoopInd) = - DepthSoilLayer(LoopInd)
        DepthSnowSoilTmp = - DepthSoilLayer(LoopInd+1)
        DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
        SoilWaterGrad(LoopInd) = 2.0 * (SoilMoistureTmp(LoopInd)-SoilMoistureTmp(LoopInd+1)) / DepthSnowSoilTmp
        WaterExcess(LoopInd) = SoilWatDiffusivity(LoopInd)*SoilWaterGrad(LoopInd) + SoilWatConductivity(LoopInd) - InfilRateSfc + TranspWatLossSoilMean(LoopInd) + EvapSoilSfcLiqMean
      else if ( LoopInd < NumSoilLayer ) then
        SoilThickTmp(LoopInd) = (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
        DepthSnowSoilTmp = (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd+1))
        DepthSnowSoilInv(LoopInd) = 2.0 / DepthSnowSoilTmp
        SoilWaterGrad(LoopInd) = 2.0 * (SoilMoistureTmp(LoopInd) - SoilMoistureTmp(LoopInd+1)) / DepthSnowSoilTmp
        WaterExcess(LoopInd) = SoilWatDiffusivity(LoopInd)*SoilWaterGrad(LoopInd) + SoilWatConductivity(LoopInd) - SoilWatDiffusivity(LoopInd-1)*SoilWaterGrad(LoopInd-1) - SoilWatConductivity(LoopInd-1) + TranspWatLossSoilMean(LoopInd)
      else
        SoilThickTmp(LoopInd) = (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
        DrainSoilBot = SoilDrainSlope * SoilWatConductivity(LoopInd)
        WaterExcess(LoopInd) = -(SoilWatDiffusivity(LoopInd-1)*SoilWaterGrad(LoopInd-1)) - SoilWatConductivity(LoopInd-1) + TranspWatLossSoilMean(LoopInd) + DrainSoilBot
      endif
    enddo
    do LoopInd = 1, NumSoilLayer
      if ( LoopInd == 1 ) then
        MatLeft1(LoopInd) = 0.0
        MatLeft2(LoopInd) = SoilWatDiffusivity(LoopInd ) * DepthSnowSoilInv(LoopInd ) / SoilThickTmp(LoopInd)
        MatLeft3(LoopInd) = - MatLeft2(LoopInd)
      else if ( LoopInd < NumSoilLayer ) then
        MatLeft1(LoopInd) = - SoilWatDiffusivity(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / SoilThickTmp(LoopInd)
        MatLeft3(LoopInd) = - SoilWatDiffusivity(LoopInd ) * DepthSnowSoilInv(LoopInd ) / SoilThickTmp(LoopInd)
        MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3(LoopInd))
      else
        MatLeft1(LoopInd) = - SoilWatDiffusivity(LoopInd-1) * DepthSnowSoilInv(LoopInd-1) / SoilThickTmp(LoopInd)
        MatLeft3(LoopInd) = 0.0
        MatLeft2(LoopInd) = - (MatLeft1(LoopInd) + MatLeft3(LoopInd))
      endif
      MatRight(LoopInd) = WaterExcess(LoopInd) / (-SoilThickTmp(LoopInd))
    enddo
    end associate
  end subroutine SoilWaterDiffusionRichards
end module lean_soilwaterdiffusionrichardsmod

! Source: physics_noahmp/src/SoilMoistureSolverMod.F90
module lean_soilmoisturesolvermod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_matrixsolvertridiagonalmod, only : MatrixSolverTriDiagonal
  implicit none
  contains
  subroutine SoilMoistureSolver(noahmp, TimeStep, MatLeft1, MatLeft2, MatLeft3, MatRight)
    implicit none
    type(noahmp_column) , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in) :: TimeStep
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatRight
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft1
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft2
    real(kind=kind_noahmp), dimension(4), intent(inout) :: MatLeft3
    integer :: LoopInd
    real(kind=kind_noahmp) :: WatDefiTmp
    real(kind=kind_noahmp), dimension(1:4) :: MatRightTmp
    real(kind=kind_noahmp), dimension(1:4) :: MatLeft3Tmp
    associate( NumSoilLayer => 4 , DepthSoilLayer => noahmp%config_domain_DepthSoilLayer , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , OptRunoffSubsurface => 3 , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , &
      & WaterTableDepth => noahmp%water_state_WaterTableDepth , SoilIce => noahmp%water_state_SoilIce , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & SoilMoistureToWT => noahmp%water_state_SoilMoistureToWT , &
      & RechargeGwDeepWT => noahmp%water_state_RechargeGwDeepWT , &
      & DrainSoilBot => noahmp%water_flux_DrainSoilBot , SoilEffPorosity => noahmp%water_state_SoilEffPorosity , &
      & SoilSaturationExcess => noahmp%water_state_SoilSaturationExcess )
    MatRightTmp = 0.0
    MatLeft3Tmp = 0.0
    SoilSaturationExcess = 0.0
    SoilEffPorosity(:) = 0.0
    do LoopInd = 1, NumSoilLayer
      MatRight(LoopInd) = MatRight(LoopInd) * TimeStep
      MatLeft1(LoopInd) = MatLeft1(LoopInd) * TimeStep
      MatLeft2(LoopInd) = 1.0 + MatLeft2(LoopInd) * TimeStep
      MatLeft3(LoopInd) = MatLeft3(LoopInd) * TimeStep
    enddo
    do LoopInd = 1, NumSoilLayer
      MatRightTmp(LoopInd) = MatRight(LoopInd)
      MatLeft3Tmp(LoopInd) = MatLeft3(LoopInd)
    enddo
    call MatrixSolverTriDiagonal(MatLeft3,MatLeft1,MatLeft2,MatLeft3Tmp,MatRightTmp,MatRight,1,NumSoilLayer,0)
    do LoopInd = 1, NumSoilLayer
      SoilLiqWater(LoopInd) = SoilLiqWater(LoopInd) + MatLeft3(LoopInd)
    enddo
    do LoopInd = NumSoilLayer, 2, -1
      SoilEffPorosity(LoopInd) = max(1.0e-4, (SoilMoistureSat(LoopInd) - SoilIce(LoopInd)))
      SoilSaturationExcess = max((SoilLiqWater(LoopInd)-SoilEffPorosity(LoopInd)), &
        & 0.0) * ThicknessSnowSoilLayer(LoopInd)
      SoilLiqWater(LoopInd) = min(SoilEffPorosity(LoopInd), SoilLiqWater(LoopInd) )
      SoilLiqWater(LoopInd-1) = SoilLiqWater(LoopInd-1) + SoilSaturationExcess / ThicknessSnowSoilLayer(LoopInd-1)
    enddo
    SoilEffPorosity(1) = max(1.0e-4, (SoilMoistureSat(1)-SoilIce(1)))
    SoilSaturationExcess = max((SoilLiqWater(1)-SoilEffPorosity(1)), 0.0) * ThicknessSnowSoilLayer(1)
    SoilLiqWater(1) = min(SoilEffPorosity(1), SoilLiqWater(1))
    if ( SoilSaturationExcess > 0.0 ) then
      SoilLiqWater(2) = SoilLiqWater(2) + SoilSaturationExcess / ThicknessSnowSoilLayer(2)
      do LoopInd = 2, NumSoilLayer-1
        SoilEffPorosity(LoopInd) = max(1.0e-4, (SoilMoistureSat(LoopInd) - SoilIce(LoopInd)))
        SoilSaturationExcess = max((SoilLiqWater(LoopInd)-SoilEffPorosity(LoopInd)), &
          & 0.0) * ThicknessSnowSoilLayer(LoopInd)
        SoilLiqWater(LoopInd) = min(SoilEffPorosity(LoopInd), SoilLiqWater(LoopInd))
        SoilLiqWater(LoopInd+1) = SoilLiqWater(LoopInd+1) + SoilSaturationExcess / ThicknessSnowSoilLayer(LoopInd+1)
      enddo
      SoilEffPorosity(NumSoilLayer) = max(1.0e-4, (SoilMoistureSat(NumSoilLayer) - SoilIce(NumSoilLayer)))
      SoilSaturationExcess = max((SoilLiqWater(NumSoilLayer)-SoilEffPorosity(NumSoilLayer)), &
        & 0.0) * ThicknessSnowSoilLayer(NumSoilLayer)
      SoilLiqWater(NumSoilLayer) = min(SoilEffPorosity(NumSoilLayer), SoilLiqWater(NumSoilLayer))
    endif
    SoilMoisture = SoilLiqWater + SoilIce
    end associate
  end subroutine SoilMoistureSolver
end module lean_soilmoisturesolvermod

! Source: physics_noahmp/src/SoilWaterMainMod.F90
module lean_soilwatermainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_runoffsurfacefreedrainmod, only : RunoffSurfaceFreeDrain
  use lean_runoffsubsurfacedrainagemod, only : RunoffSubSurfaceDrainage
  use lean_soilwaterdiffusionrichardsmod, only : SoilWaterDiffusionRichards
  use lean_soilmoisturesolvermod, only : SoilMoistureSolver
  implicit none
  contains
  subroutine SoilWaterMain(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd1, LoopInd2
    integer :: IndIter
    integer :: NumIterSoilWat
    real(kind=kind_noahmp) :: TimeStepFine
    real(kind=kind_noahmp) :: SoilSatExcAcc
    real(kind=kind_noahmp) :: SoilWatConductAcc
    real(kind=kind_noahmp) :: WaterRemove
    real(kind=kind_noahmp) :: SoilWatRem
    real(kind=kind_noahmp) :: SoilWaterMin
    real(kind=kind_noahmp) :: DrainSoilBotAcc
    real(kind=kind_noahmp) :: RunoffSurfaceAcc
    real(kind=kind_noahmp) :: InfilSfcAcc
    real(kind=kind_noahmp), parameter :: SoilImpPara = 4.0
    real(kind=kind_noahmp), dimension(1:4) :: MatRight
    real(kind=kind_noahmp), dimension(1:4) :: MatLeft1
    real(kind=kind_noahmp), dimension(1:4) :: MatLeft2
    real(kind=kind_noahmp), dimension(1:4) :: MatLeft3
    real(kind=kind_noahmp), dimension(1:4) :: SoilLiqTmp
    associate( NumSoilLayer => 4 , SoilTimeStep => noahmp%config_domain_SoilTimeStep , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & FlagUrban => noahmp%config_domain_FlagUrban , OptRunoffSurface => 3 , OptRunoffSubsurface => 3 , &
      & OptTileDrainage => 0 , SoilIce => noahmp%water_state_SoilIce , &
      & TileDrainFrac => noahmp%water_state_TileDrainFrac , &
      & SoilSfcInflowMean => noahmp%water_flux_SoilSfcInflowMean , &
      & SoilMoistureSat => noahmp%water_param_SoilMoistureSat , SoilLiqWater => noahmp%water_state_SoilLiqWater , &
      & SoilMoisture => noahmp%water_state_SoilMoisture , &
      & RechargeGwDeepWT => noahmp%water_state_RechargeGwDeepWT , &
      & DrainSoilBot => noahmp%water_flux_DrainSoilBot , RunoffSurface => noahmp%water_flux_RunoffSurface , &
      & RunoffSubsurface => noahmp%water_flux_RunoffSubsurface , InfilRateSfc => noahmp%water_flux_InfilRateSfc , &
      & TileDrain => noahmp%water_flux_TileDrain , SoilImpervFracMax => noahmp%water_state_SoilImpervFracMax , &
      & SoilWatConductivity => noahmp%water_state_SoilWatConductivity , &
      & SoilEffPorosity => noahmp%water_state_SoilEffPorosity , &
      & SoilImpervFrac => noahmp%water_state_SoilImpervFrac , SoilIceFrac => noahmp%water_state_SoilIceFrac , &
      & SoilSaturationExcess => noahmp%water_state_SoilSaturationExcess , &
      & SoilIceMax => noahmp%water_state_SoilIceMax , SoilLiqWaterMin => noahmp%water_state_SoilLiqWaterMin )
    MatRight = 0.0
    MatLeft1 = 0.0
    MatLeft2 = 0.0
    MatLeft3 = 0.0
    SoilLiqTmp = 0.0
    RunoffSurface = 0.0
    RunoffSubsurface = 0.0
    InfilRateSfc = 0.0
    SoilSatExcAcc = 0.0
    InfilSfcAcc = 1.0e-06
    do LoopInd1 = 1, NumSoilLayer
      SoilEffPorosity(LoopInd1) = max(1.0e-4, (SoilMoistureSat(LoopInd1) - SoilIce(LoopInd1)))
      SoilSatExcAcc = SoilSatExcAcc + max(0.0, &
        & SoilLiqWater(LoopInd1) - SoilEffPorosity(LoopInd1)) * ThicknessSnowSoilLayer(LoopInd1)
      SoilLiqWater(LoopInd1) = min(SoilEffPorosity(LoopInd1), SoilLiqWater(LoopInd1))
    enddo
    do LoopInd1 = 1, NumSoilLayer
      SoilIceFrac(LoopInd1) = min(1.0, SoilIce(LoopInd1) / SoilMoistureSat(LoopInd1))
      SoilImpervFrac(LoopInd1) = max(0.0, &
        & exp(-SoilImpPara*(1.0-SoilIceFrac(LoopInd1))) - exp(-SoilImpPara)) / (1.0 - exp(-SoilImpPara))
    enddo
    SoilIceMax = 0.0
    SoilImpervFracMax = 0.0
    SoilLiqWaterMin = SoilMoistureSat(1)
    do LoopInd1 = 1, NumSoilLayer
      if ( SoilIce(LoopInd1) > SoilIceMax ) SoilIceMax = SoilIce(LoopInd1)
      if ( SoilImpervFrac(LoopInd1) > SoilImpervFracMax ) SoilImpervFracMax = SoilImpervFrac(LoopInd1)
      if ( SoilLiqWater(LoopInd1) < SoilLiqWaterMin ) SoilLiqWaterMin = SoilLiqWater(LoopInd1)
    enddo
    if ( FlagUrban .eqv. .true. ) SoilImpervFrac(1) = 0.95
    call RunoffSurfaceFreeDrain(noahmp,SoilTimeStep)
    NumIterSoilWat = 3
    if ( (InfilRateSfc*SoilTimeStep) > (ThicknessSnowSoilLayer(1)*SoilMoistureSat(1)) ) then
      NumIterSoilWat = NumIterSoilWat*2
    endif
    TimeStepFine = SoilTimeStep / NumIterSoilWat
    InfilSfcAcc = 1.0e-06
    DrainSoilBotAcc = 0.0
    RunoffSurfaceAcc = 0.0
    do IndIter = 1, NumIterSoilWat
      if ( SoilSfcInflowMean > 0.0 ) then
        call RunoffSurfaceFreeDrain(noahmp,TimeStepFine)
      endif
      call SoilWaterDiffusionRichards(noahmp, MatLeft1, MatLeft2, MatLeft3, MatRight)
      call SoilMoistureSolver(noahmp, TimeStepFine, MatLeft1, MatLeft2, MatLeft3, MatRight)
      SoilSatExcAcc = SoilSatExcAcc + SoilSaturationExcess
      DrainSoilBotAcc = DrainSoilBotAcc + DrainSoilBot
      RunoffSurfaceAcc = RunoffSurfaceAcc + RunoffSurface
    enddo
    DrainSoilBot = DrainSoilBotAcc / NumIterSoilWat
    RunoffSurface = RunoffSurfaceAcc / NumIterSoilWat
    RunoffSurface = RunoffSurface * 1000.0 + SoilSatExcAcc * 1000.0 / SoilTimeStep
    DrainSoilBot = DrainSoilBot * 1000.0
    do LoopInd2 = 1, NumSoilLayer
      SoilLiqTmp(LoopInd2) = SoilLiqWater(LoopInd2) * ThicknessSnowSoilLayer(LoopInd2) * 1000.0
    enddo
    SoilWaterMin = 0.01
    do LoopInd2 = 1, NumSoilLayer-1
      if ( SoilLiqTmp(LoopInd2) < 0.0 ) then
        SoilWatRem = SoilWaterMin - SoilLiqTmp(LoopInd2)
      else
        SoilWatRem = 0.0
      endif
      SoilLiqTmp(LoopInd2 ) = SoilLiqTmp(LoopInd2 ) + SoilWatRem
      SoilLiqTmp(LoopInd2+1) = SoilLiqTmp(LoopInd2+1) - SoilWatRem
    enddo
    LoopInd2 = NumSoilLayer
    if ( SoilLiqTmp(LoopInd2) < SoilWaterMin ) then
      SoilWatRem = SoilWaterMin - SoilLiqTmp(LoopInd2)
    else
      SoilWatRem = 0.0
    endif
    SoilLiqTmp(LoopInd2) = SoilLiqTmp(LoopInd2) + SoilWatRem
    RunoffSubsurface = RunoffSubsurface - SoilWatRem/SoilTimeStep
    do LoopInd2 = 1, NumSoilLayer
      SoilLiqWater(LoopInd2) = SoilLiqTmp(LoopInd2) / (ThicknessSnowSoilLayer(LoopInd2)*1000.0)
    enddo
    call RunoffSubSurfaceDrainage(noahmp)
    do LoopInd2 = 1, NumSoilLayer
      SoilMoisture(LoopInd2) = SoilLiqWater(LoopInd2) + SoilIce(LoopInd2)
    enddo
    RunoffSurface = RunoffSurface * SoilTimeStep
    RunoffSubsurface = RunoffSubsurface * SoilTimeStep
    TileDrain = TileDrain * SoilTimeStep
    end associate
  end subroutine SoilWaterMain
end module lean_soilwatermainmod

! Source: physics_noahmp/src/WaterMainMod.F90
module lean_watermainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_canopyhydrologymod, only : CanopyHydrology
  use lean_snowwatermainmod, only : SnowWaterMain
  use lean_irrigationfloodmod, only : IrrigationFlood
  use lean_irrigationmicromod, only : IrrigationMicro
  use lean_soilwatermainmod, only : SoilWaterMain
  implicit none
  contains
  subroutine WaterMain(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    associate( MainTimeStep => noahmp%config_domain_MainTimeStep , &
      & SoilTimeStep => noahmp%config_domain_SoilTimeStep , SurfaceType => 1 , &
      & FlagCropland => noahmp%config_domain_FlagCropland , FlagUrban => noahmp%config_domain_FlagUrban , &
      & FlagSoilProcess => noahmp%config_domain_FlagSoilProcess , &
      & NumSoilTimeStep => noahmp%config_domain_NumSoilTimeStep , VaporizeGrd => noahmp%water_flux_VaporizeGrd , &
      & CondenseVapGrd => noahmp%water_flux_CondenseVapGrd , RainfallGround => noahmp%water_flux_RainfallGround , &
      & SoilTranspFac => noahmp%water_state_SoilTranspFac , &
      & WaterStorageLakeMax => noahmp%water_param_WaterStorageLakeMax , &
      & NumSoilLayerRoot => noahmp%water_param_NumSoilLayerRoot , &
      & FlagFrozenGround => noahmp%energy_state_FlagFrozenGround , &
      & LatHeatVapGrd => noahmp%energy_state_LatHeatVapGrd , &
      & DensityAirRefHeight => noahmp%energy_state_DensityAirRefHeight , &
      & ExchCoeffShSfc => noahmp%energy_state_ExchCoeffShSfc , &
      & SpecHumidityRefHeight => noahmp%forcing_SpecHumidityRefHeight , &
      & HeatLatentGrd => noahmp%energy_flux_HeatLatentGrd , &
      & NumSnowLayerNeg => noahmp%config_domain_NumSnowLayerNeg , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , &
      & SnowWaterEquivPrev => noahmp%water_state_SnowWaterEquivPrev , &
      & SoilLiqWater => noahmp%water_state_SoilLiqWater , SoilIce => noahmp%water_state_SoilIce , &
      & SoilMoisture => noahmp%water_state_SoilMoisture , &
      & WaterStorageLake => noahmp%water_state_WaterStorageLake , &
      & PondSfcThinSnwMelt => noahmp%water_state_PondSfcThinSnwMelt , &
      & WaterHeadSfc => noahmp%water_state_WaterHeadSfc , &
      & IrrigationAmtFlood => noahmp%water_state_IrrigationAmtFlood , &
      & IrrigationAmtMicro => noahmp%water_state_IrrigationAmtMicro , &
      & SoilSfcInflow => noahmp%water_flux_SoilSfcInflow , EvapSoilSfcLiq => noahmp%water_flux_EvapSoilSfcLiq , &
      & DewSoilSfcLiq => noahmp%water_flux_DewSoilSfcLiq , FrostSnowSfcIce => noahmp%water_flux_FrostSnowSfcIce , &
      & SublimSnowSfcIce => noahmp%water_flux_SublimSnowSfcIce , &
      & TranspWatLossSoil => noahmp%water_flux_TranspWatLossSoil , &
      & GlacierExcessFlow => noahmp%water_flux_GlacierExcessFlow , &
      & SoilSfcInflowAcc => noahmp%water_flux_SoilSfcInflowAcc , &
      & EvapSoilSfcLiqAcc => noahmp%water_flux_EvapSoilSfcLiqAcc , &
      & TranspWatLossSoilAcc => noahmp%water_flux_TranspWatLossSoilAcc , &
      & SpecHumidity2mBare => noahmp%energy_state_SpecHumidity2mBare , &
      & SpecHumiditySfc => noahmp%energy_state_SpecHumiditySfc , &
      & EvapGroundNet => noahmp%water_flux_EvapGroundNet , Transpiration => noahmp%water_flux_Transpiration , &
      & EvapCanopyNet => noahmp%water_flux_EvapCanopyNet , RunoffSurface => noahmp%water_flux_RunoffSurface , &
      & RunoffSubsurface => noahmp%water_flux_RunoffSubsurface , TileDrain => noahmp%water_flux_TileDrain , &
      & SnowBotOutflow => noahmp%water_flux_SnowBotOutflow , &
      & WaterToAtmosTotal => noahmp%water_flux_WaterToAtmosTotal , &
      & SoilSfcInflowMean => noahmp%water_flux_SoilSfcInflowMean , &
      & EvapSoilSfcLiqMean => noahmp%water_flux_EvapSoilSfcLiqMean , &
      & TranspWatLossSoilMean => noahmp%water_flux_TranspWatLossSoilMean , &
      & PondSfcThinSnwComb => noahmp%water_state_PondSfcThinSnwComb , &
      & PondSfcThinSnwTrans => noahmp%water_state_PondSfcThinSnwTrans )
    TranspWatLossSoil = 0.0
    GlacierExcessFlow = 0.0
    RunoffSubsurface = 0.0
    RunoffSurface = 0.0
    SoilSfcInflow = 0.0
    TileDrain = 0.0
    SoilIce(:) = max(0.0, SoilMoisture(:)-SoilLiqWater(:))
    SnowWaterEquivPrev = SnowWaterEquiv
    VaporizeGrd = max(HeatLatentGrd/LatHeatVapGrd, 0.0)
    CondenseVapGrd = abs(min(HeatLatentGrd/LatHeatVapGrd, 0.0))
    EvapGroundNet = VaporizeGrd - CondenseVapGrd
    call CanopyHydrology(noahmp)
    SublimSnowSfcIce = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
      SublimSnowSfcIce = min(VaporizeGrd, SnowWaterEquiv/MainTimeStep)
    endif
    EvapSoilSfcLiq = VaporizeGrd - SublimSnowSfcIce
    FrostSnowSfcIce = 0.0
    if ( SnowWaterEquiv > 0.0 ) then
      FrostSnowSfcIce = CondenseVapGrd
    endif
    DewSoilSfcLiq = CondenseVapGrd - FrostSnowSfcIce
    call SnowWaterMain(noahmp)
    if ( FlagFrozenGround .eqv. .true. ) then
      SoilIce(1) = SoilIce(1) + (DewSoilSfcLiq-EvapSoilSfcLiq) * MainTimeStep / (ThicknessSnowSoilLayer(1)*1000.0)
      DewSoilSfcLiq = 0.0
      EvapSoilSfcLiq = 0.0
      if ( SoilIce(1) < 0.0 ) then
        SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
        SoilIce(1) = 0.0
      endif
      SoilMoisture(1) = SoilLiqWater(1) + SoilIce(1)
    endif
    EvapSoilSfcLiq = EvapSoilSfcLiq * 0.001
    do LoopInd = 1, NumSoilLayerRoot
      TranspWatLossSoil(LoopInd) = Transpiration * SoilTranspFac(LoopInd) * 0.001
    enddo
    SoilSfcInflow = (PondSfcThinSnwMelt + PondSfcThinSnwComb + PondSfcThinSnwTrans) / MainTimeStep * 0.001
    if ( NumSnowLayerNeg == 0 ) then
      SoilSfcInflow = SoilSfcInflow + (SnowBotOutflow + DewSoilSfcLiq + RainfallGround) * 0.001
    else
      SoilSfcInflow = SoilSfcInflow + (SnowBotOutflow + DewSoilSfcLiq) * 0.001
    endif
    SoilSfcInflowAcc = SoilSfcInflowAcc + SoilSfcInflow
    EvapSoilSfcLiqAcc = EvapSoilSfcLiqAcc + EvapSoilSfcLiq
    TranspWatLossSoilAcc = TranspWatLossSoilAcc + TranspWatLossSoil
    if ( FlagSoilProcess .eqv. .true. ) then
      if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtFlood > 0.0) ) call IrrigationFlood(noahmp)
      if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtMicro > 0.0) ) call IrrigationMicro(noahmp)
      SoilSfcInflowMean = SoilSfcInflowAcc / NumSoilTimeStep
      EvapSoilSfcLiqMean = EvapSoilSfcLiqAcc / NumSoilTimeStep
      TranspWatLossSoilMean = TranspWatLossSoilAcc / NumSoilTimeStep
      call SoilWaterMain(noahmp)
    endif
    RunoffSubsurface = RunoffSubsurface + GlacierExcessFlow * MainTimeStep
    WaterToAtmosTotal = Transpiration + EvapCanopyNet + EvapGroundNet
    if ( (FlagUrban .eqv. .true.) ) then
      SpecHumiditySfc = WaterToAtmosTotal / (DensityAirRefHeight*ExchCoeffShSfc) + SpecHumidityRefHeight
      SpecHumidity2mBare = SpecHumiditySfc
    endif
    end associate
  end subroutine WaterMain
end module lean_watermainmod

! Source: physics_noahmp/src/BalanceErrorCheckMod.F90
module lean_balanceerrorcheckmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  implicit none
  contains
  subroutine BalanceWaterInit(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    associate( NumSoilLayer => 4 , SurfaceType => 1 , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & CanopyLiqWater => noahmp%water_state_CanopyLiqWater , CanopyIce => noahmp%water_state_CanopyIce , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & WaterStorageAquifer => noahmp%water_state_WaterStorageAquifer , &
      & WaterStorageTotBeg => noahmp%water_state_WaterStorageTotBeg )
    WaterStorageTotBeg = CanopyLiqWater + CanopyIce + SnowWaterEquiv + WaterStorageAquifer
    do LoopInd = 1, NumSoilLayer
      WaterStorageTotBeg = WaterStorageTotBeg + SoilMoisture(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
    enddo
    end associate
  end subroutine BalanceWaterInit
  subroutine BalanceWaterCheck(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    integer :: LoopInd
    associate( NumSoilLayer => 4 , SurfaceType => 1 , GridIndexI => noahmp%config_domain_GridIndexI , &
      & GridIndexJ => noahmp%config_domain_GridIndexJ , &
      & ThicknessSnowSoilLayer => noahmp%config_domain_ThicknessSnowSoilLayer , &
      & MainTimeStep => noahmp%config_domain_MainTimeStep , FlagCropland => noahmp%config_domain_FlagCropland , &
      & FlagSoilProcess => noahmp%config_domain_FlagSoilProcess , &
      & IrriFracThreshold => noahmp%water_param_IrriFracThreshold , &
      & IrrigationFracGrid => noahmp%water_state_IrrigationFracGrid , &
      & WaterTableDepth => noahmp%water_state_WaterTableDepth , &
      & CanopyLiqWater => noahmp%water_state_CanopyLiqWater , CanopyIce => noahmp%water_state_CanopyIce , &
      & SnowWaterEquiv => noahmp%water_state_SnowWaterEquiv , SoilMoisture => noahmp%water_state_SoilMoisture , &
      & WaterStorageAquifer => noahmp%water_state_WaterStorageAquifer , &
      & WaterStorageTotBeg => noahmp%water_state_WaterStorageTotBeg , &
      & PrecipTotRefHeight => noahmp%water_flux_PrecipTotRefHeight , &
      & EvapCanopyNet => noahmp%water_flux_EvapCanopyNet , Transpiration => noahmp%water_flux_Transpiration , &
      & EvapGroundNet => noahmp%water_flux_EvapGroundNet , RunoffSurface => noahmp%water_flux_RunoffSurface , &
      & RunoffSubsurface => noahmp%water_flux_RunoffSubsurface , TileDrain => noahmp%water_flux_TileDrain , &
      & IrrigationRateSprinkler => noahmp%water_flux_IrrigationRateSprinkler , &
      & IrrigationRateMicro => noahmp%water_flux_IrrigationRateMicro , &
      & IrrigationRateFlood => noahmp%water_flux_IrrigationRateFlood , &
      & SfcWaterTotChgAcc => noahmp%water_flux_SfcWaterTotChgAcc , &
      & PrecipTotAcc => noahmp%water_flux_PrecipTotAcc , EvapCanopyNetAcc => noahmp%water_flux_EvapCanopyNetAcc , &
      & TranspirationAcc => noahmp%water_flux_TranspirationAcc , &
      & EvapGroundNetAcc => noahmp%water_flux_EvapGroundNetAcc , &
      & WaterStorageTotEnd => noahmp%water_state_WaterStorageTotEnd , &
      & WaterBalanceError => noahmp%water_state_WaterBalanceError )
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) then
      PrecipTotRefHeight = PrecipTotRefHeight + IrrigationRateSprinkler * 1000.0 / MainTimeStep
    endif
    WaterStorageTotEnd = CanopyLiqWater + CanopyIce + SnowWaterEquiv + WaterStorageAquifer
    do LoopInd = 1, NumSoilLayer
      WaterStorageTotEnd = WaterStorageTotEnd + SoilMoisture(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
    enddo
    SfcWaterTotChgAcc = SfcWaterTotChgAcc + (WaterStorageTotEnd - WaterStorageTotBeg)
    PrecipTotAcc = PrecipTotAcc + PrecipTotRefHeight * MainTimeStep
    EvapCanopyNetAcc = EvapCanopyNetAcc + EvapCanopyNet * MainTimeStep
    TranspirationAcc = TranspirationAcc + Transpiration * MainTimeStep
    EvapGroundNetAcc = EvapGroundNetAcc + EvapGroundNet * MainTimeStep
    if ( FlagSoilProcess .eqv. .true. ) then
      WaterBalanceError = SfcWaterTotChgAcc - (PrecipTotAcc + IrrigationRateMicro*1000.0 + IrrigationRateFlood*1000.0 - EvapCanopyNetAcc - TranspirationAcc - EvapGroundNetAcc - RunoffSurface - RunoffSubsurface - TileDrain)
      if ( abs(WaterBalanceError) > 0.1 ) then
        if ( WaterBalanceError > 0 ) then
          write(*,*) "The model is gaining water (WaterBalanceError is positive)"
        else
          write(*,*) "The model is losing water (WaterBalanceError is negative)"
        endif
        write(*,*) 'WaterBalanceError = ',WaterBalanceError, "kg m{-2} timestep{-1}"
        write(*, &
          & '(" GridIndexI GridIndexJ SfcWaterTotChgAcc PrecipTotRefHeightAcc IrrigationRateMicro IrrigationRateFlood EvapCanopyNetAcc EvapGroundNetAcc TranspirationAcc RunoffSurface RunoffSubsurface WaterTableDepth TileDrain")')
        write(*,'(i6,i6,f10.3,10f10.5)') GridIndexI, GridIndexJ, SfcWaterTotChgAcc, PrecipTotAcc, &
          & IrrigationRateMicro*1000.0, IrrigationRateFlood*1000.0, EvapCanopyNetAcc, EvapGroundNetAcc, &
          & TranspirationAcc, RunoffSurface, RunoffSubsurface, WaterTableDepth, TileDrain
        stop "Error: Water budget problem in NoahMP LSM"
      endif
    endif
    end associate
  end subroutine BalanceWaterCheck
  subroutine BalanceEnergyCheck(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( GridIndexI => noahmp%config_domain_GridIndexI , &
      & GridIndexJ => noahmp%config_domain_GridIndexJ , RadSwDownRefHeight => noahmp%forcing_RadSwDownRefHeight , &
      & VegFrac => noahmp%energy_state_VegFrac , RadSwAbsSfc => noahmp%energy_flux_RadSwAbsSfc , &
      & RadSwReflSfc => noahmp%energy_flux_RadSwReflSfc , RadSwReflVeg => noahmp%energy_flux_RadSwReflVeg , &
      & RadSwReflGrd => noahmp%energy_flux_RadSwReflGrd , RadLwNetSfc => noahmp%energy_flux_RadLwNetSfc , &
      & HeatSensibleSfc => noahmp%energy_flux_HeatSensibleSfc , &
      & HeatLatentCanopy => noahmp%energy_flux_HeatLatentCanopy , &
      & HeatLatentGrd => noahmp%energy_flux_HeatLatentGrd , &
      & HeatLatentTransp => noahmp%energy_flux_HeatLatentTransp , &
      & HeatGroundTot => noahmp%energy_flux_HeatGroundTot , RadSwAbsVeg => noahmp%energy_flux_RadSwAbsVeg , &
      & RadSwAbsGrd => noahmp%energy_flux_RadSwAbsGrd , HeatPrecipAdvSfc => noahmp%energy_flux_HeatPrecipAdvSfc , &
      & HeatPrecipAdvBareGrd => noahmp%energy_flux_HeatPrecipAdvBareGrd , &
      & HeatPrecipAdvVegGrd => noahmp%energy_flux_HeatPrecipAdvVegGrd , &
      & HeatPrecipAdvCanopy => noahmp%energy_flux_HeatPrecipAdvCanopy , &
      & HeatLatentIrriEvap => noahmp%energy_flux_HeatLatentIrriEvap , &
      & HeatCanStorageChg => noahmp%energy_flux_HeatCanStorageChg , &
      & EnergyBalanceError => noahmp%energy_state_EnergyBalanceError , &
      & RadSwBalanceError => noahmp%energy_state_RadSwBalanceError )
    RadSwBalanceError = RadSwDownRefHeight - (RadSwAbsSfc + RadSwReflSfc)
    if ( abs(RadSwBalanceError) > 0.01 ) then
      write(*,*) "GridIndexI, GridIndexJ = ", GridIndexI, GridIndexJ
      write(*,*) "RadSwBalanceError = ", RadSwBalanceError
      write(*,*) "VEGETATION ---------"
      write(*,*) "RadSwDownRefHeight * VegFrac = ", RadSwDownRefHeight*VegFrac
      write(*,*) "VegFrac*RadSwAbsVeg + RadSwAbsGrd = ", VegFrac*RadSwAbsVeg+RadSwAbsGrd
      write(*,*) "VegFrac*RadSwReflVeg + RadSwReflGrd = ", VegFrac*RadSwReflVeg+RadSwReflGrd
      write(*,*) "GROUND -------"
      write(*,*) "(1 - VegFrac) * RadSwDownRefHeight = ", (1.0-VegFrac)*RadSwDownRefHeight
      write(*,*) "(1 - VegFrac) * RadSwAbsGrd = ", (1.0-VegFrac)*RadSwAbsGrd
      write(*,*) "(1 - VegFrac) * RadSwReflGrd = ", (1.0-VegFrac)*RadSwReflGrd
      write(*,*) "RadSwReflVeg = ", RadSwReflVeg
      write(*,*) "RadSwReflGrd = ", RadSwReflGrd
      write(*,*) "RadSwReflSfc = ", RadSwReflSfc
      write(*,*) "RadSwAbsVeg = ", RadSwAbsVeg
      write(*,*) "RadSwAbsGrd = ", RadSwAbsGrd
      write(*,*) "RadSwAbsSfc = ", RadSwAbsSfc
      stop "Error: Solar radiation budget problem in NoahMP LSM"
    endif
    EnergyBalanceError = RadSwAbsVeg + RadSwAbsGrd + HeatPrecipAdvSfc - (RadLwNetSfc + HeatSensibleSfc + HeatLatentCanopy + HeatLatentGrd + HeatLatentTransp + HeatGroundTot + HeatLatentIrriEvap + HeatCanStorageChg)
    if ( abs(EnergyBalanceError) > 0.01 ) then
      write(*,*) 'EnergyBalanceError = ', EnergyBalanceError, ' at GridIndexI,GridIndexJ: ', GridIndexI, GridIndexJ
      write(*,'(a17,F10.4)' ) "Net solar: ", RadSwAbsSfc
      write(*,'(a17,F10.4)' ) "Net longwave: ", RadLwNetSfc
      write(*,'(a17,F10.4)' ) "Total sensible: ", HeatSensibleSfc
      write(*,'(a17,F10.4)' ) "Canopy evap: ", HeatLatentCanopy
      write(*,'(a17,F10.4)' ) "Ground evap: ", HeatLatentGrd
      write(*,'(a17,F10.4)' ) "Transpiration: ", HeatLatentTransp
      write(*,'(a17,F10.4)' ) "Total ground: ", HeatGroundTot
      write(*,'(a17,F10.4)' ) "Sprinkler: ", HeatLatentIrriEvap
      write(*,'(a17,F10.4)' ) "Canopy heat storage change: ", HeatCanStorageChg
      write(*,'(a17,4F10.4)') "Precip advected: ", HeatPrecipAdvSfc,HeatPrecipAdvCanopy,HeatPrecipAdvVegGrd, &
        & HeatPrecipAdvBareGrd
      write(*,'(a17,F10.4)' ) "Veg fraction: ", VegFrac
      stop "Error: Energy budget problem in NoahMP LSM"
    endif
    end associate
  end subroutine BalanceEnergyCheck
end module lean_balanceerrorcheckmod

! Source: physics_noahmp/src/NoahmpMainMod.F90
module lean_noahmpmainmod
  use Machine
  use noahmp_state
  use lean_constantdefinemod
  use lean_atmosforcingmod, only : ProcessAtmosForcing
  use lean_generalinitmod, only : GeneralInit
  use lean_phenologymainmod, only : PhenologyMain
  use lean_irrigationpreparemod, only : IrrigationPrepare
  use lean_irrigationsprinklermod, only : IrrigationSprinkler
  use lean_canopywaterinterceptmod, only : CanopyWaterIntercept
  use lean_precipitationheatadvectmod, only : PrecipitationHeatAdvect
  use lean_energymainmod, only : EnergyMain
  use lean_watermainmod, only : WaterMain
  use lean_balanceerrorcheckmod, only : BalanceWaterInit, BalanceWaterCheck, BalanceEnergyCheck
  implicit none
  contains
  subroutine NoahmpMain(noahmp)
    implicit none
    type(noahmp_column), intent(inout) :: noahmp
    associate( FlagDynamicVeg => noahmp%config_domain_FlagDynamicVeg , &
      & FlagDynamicCrop => noahmp%config_domain_FlagDynamicCrop , OptCropModel => 0 , &
      & IrrigationAmtSprinkler => noahmp%water_state_IrrigationAmtSprinkler , &
      & FlagCropland => noahmp%config_domain_FlagCropland )
    call ProcessAtmosForcing(noahmp)
    call GeneralInit(noahmp)
    call BalanceWaterInit(noahmp)
    call PhenologyMain(noahmp)
    call IrrigationPrepare(noahmp)
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationAmtSprinkler > 0.0) ) call IrrigationSprinkler(noahmp)
    call CanopyWaterIntercept(noahmp)
    call PrecipitationHeatAdvect(noahmp)
    call EnergyMain(noahmp)
    call WaterMain(noahmp)
    call BalanceWaterCheck(noahmp)
    call BalanceEnergyCheck(noahmp)
    end associate
  end subroutine NoahmpMain
end module lean_noahmpmainmod
