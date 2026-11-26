# MPAS-Model 8.4.2: the original files the lean build compiles

Every file under `src/` is copied unchanged from MPAS-Model v8.4.2
(https://github.com/MPAS-Dev/MPAS-Model, commit `f34984b2c89353cf31c235b08d3b4acd96b37bb3`), at its original path under `src/`.
The lean build compiles these files as-is. `LICENSE` is the MPAS license. The packages keep their own terms:
Noah-MP (`src/core_atmosphere/physics/physics_noahmp/LICENSE.txt`), the WRF physics
(`src/core_atmosphere/physics/physics_wrf/LICENSE`), RRTMG and the Mersenne Twister (notices inside
`module_ra_rrtmg_lw.F` and `module_ra_rrtmg_sw.F`) and the ESMF time manager (file headers). `NOTICE.md` at the
root of the repository has the whole list.

- `closure.json`: the three lists below, in compile order.
- `SHA256SUMS`: the sha256 of `LICENSE`, `closure.json`, this `README.md` and every file under `src/`
  (`shasum -a 256 -c SHA256SUMS`).
- `python3 -m tools.lean.vendor --check` checks this folder; `--write` makes it again from
  `reference/mpas_src` (`./lean fetch-upstream`).

None of these files is one of the patched copies in `reference_fixes/`
(used only by the tests that compile MPAS's own code; docs/fixes.md).

## fixed (9 files): compiled by name by the lean build

- `src/framework/mpas_kind_types.F`: MPAS real and string kinds
- `src/framework/mpas_constants.F`: MPAS physical constants
- `src/core_atmosphere/physics/mpas_atmphys_constants.F`: constants of the MPAS physics
- `src/core_atmosphere/physics/ccpp_kind_types.F`: CCPP real kinds (kind_phys)
- `src/core_atmosphere/physics/mpas_atmphys_functions.F`: gamma functions and saturation mixing ratios
- `src/core_atmosphere/physics/physics_wrf/module_mp_radar.F`: radar reflectivity support of Thompson microphysics
- `src/core_atmosphere/physics/physics_wrf/module_ra_rrtmg_vinterp.F`: ozone interpolation to model levels for RRTMG
- `src/core_atmosphere/physics/physics_wrf/module_ra_rrtmg_lw.F`: RRTMG longwave radiation
- `src/core_atmosphere/physics/physics_wrf/module_ra_rrtmg_sw.F`: RRTMG shortwave radiation

## esmf_time (17 files): the ESMF time manager of MPAS, the model clock

Every `*.F90` and `*.inc` of `src/external/esmf_time_f90`, compiled with `-DHIDE_MPI`.

- `src/external/esmf_time_f90/wrf_message.F90`
- `src/external/esmf_time_f90/ESMF_BaseMod.F90`
- `src/external/esmf_time_f90/ESMF_BaseTimeMod.F90`
- `src/external/esmf_time_f90/ESMF_CalendarMod.F90`
- `src/external/esmf_time_f90/ESMF_Stubs.F90`
- `src/external/esmf_time_f90/ESMF_FractionMod.F90`
- `src/external/esmf_time_f90/ESMF_ShrTimeMod.F90`
- `src/external/esmf_time_f90/ESMF_TimeIntervalMod.F90`
- `src/external/esmf_time_f90/ESMF_TimeMod.F90`
- `src/external/esmf_time_f90/ESMF_AlarmMod.F90`
- `src/external/esmf_time_f90/ESMF_ClockMod.F90`
- `src/external/esmf_time_f90/ESMF_AlarmClockMod.F90`
- `src/external/esmf_time_f90/MeatMod.F90`
- `src/external/esmf_time_f90/ESMF.F90`
- `src/external/esmf_time_f90/wrf_error_fatal.F90`
- `src/external/esmf_time_f90/ESMF_Macros.inc`
- `src/external/esmf_time_f90/ESMF_TimeMgr.inc`

## noahmp (144 files): the Noah-MP land surface model

The modules that the column driver, the table reader and the cold-start initializer use
(`noahmpdrivermainmod`, `noahmpreadtablemod`, `noahmpinitmainmod`, `noahmpiovarinitmod`), without `ErrorHandleMod` and its netCDF I/O.

- `src/core_atmosphere/physics/physics_noahmp/utility/Machine.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ForcingVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ConfigVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/EnergyVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BiochemVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/NoahmpVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpIOVarType.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ConfigVarInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/EnergyVarInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ForcingVarInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterVarInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BiochemVarInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/ConfigVarInTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/EnergyVarInTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/ForcingVarInTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/PedoTransferSR2006Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/WaterVarInTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/BiochemVarInTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/ConfigVarOutTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/ForcingVarOutTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/EnergyVarOutTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/WaterVarOutTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/BiochemVarOutTransferMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ConstantDefineMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/AtmosForcingMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GeneralInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/PhenologyMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationTriggerMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationPrepareMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/utility/CheckNanMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilHydraulicPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationInfilPhilipMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationSprinklerMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CanopyWaterInterceptMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/PrecipitationHeatAdvectMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowCoverGroundNiu07Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundRoughnessPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowThermalPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilThermalPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundThermalPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowAgingBatsMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowAlbedoBatsMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowAlbedoClassMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundAlbedoMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CanopyRadiationTwoStreamMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceAlbedoMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceRadiationMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceEmissivityMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterTranspirationMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceGroundEvaporationMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/PsychrometricVariableMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/VaporPressureSaturationMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceAboveCanopyMostMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceAboveCanopyChen97Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceLeafToGroundMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceCanopyStomataBallBerryMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/HumiditySaturationMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceCanopyStomataJarvisMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceEnergyFluxVegetatedMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceBareGroundMostMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceBareGroundChen97Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceEnergyFluxBareGroundMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/MatrixSolverTriDiagonalMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilSnowTemperatureSolverMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilSnowThermalDiffusionMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilSnowTemperatureMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterSupercoolKoren99Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterSupercoolNiu06Mod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilSnowWaterPhaseChangeMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/EnergyMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CanopyHydrologyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowfallBelowCanopyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowpackCompactionMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowLayerWaterComboMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowLayerCombineMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowLayerDivideMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowpackHydrologyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowWaterMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationFloodMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/IrrigationMicroMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceTopModelGrdMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceTopModelEquiMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceFreeDrainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceBatsMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceTopModelMmfMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceVicMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceXinAnJiangMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterInfilPhilipMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterInfilGreenAmptMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterInfilSmithParlangeMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceExcessDynamicVicMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSurfaceDynamicVicMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterTableEquilibriumMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSubSurfaceEquiWaterTableMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundWaterTopModelMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSubSurfaceGroundWaterMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSubSurfaceDrainageMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ShallowWaterTableMmfMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/RunoffSubSurfaceShallowMmfMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterDiffusionRichardsMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilMoistureSolverMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/TileDrainageSimpleMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/TileDrainageEquiDepthMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterTableDepthSearchMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/TileDrainageHooghoudtMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SoilWaterMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CarbonFluxNatureVegMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BiochemNatureVegMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CarbonFluxCropMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CropGrowDegreeDayMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/CropPhotosynthesisMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BiochemCropMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BalanceErrorCheckMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/NoahmpMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GeneralInitGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/PrecipitationHeatAdvectGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowCoverGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundRoughnessPropertyGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GlacierIceThermalPropertyMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundThermalPropertyGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GroundAlbedoGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceAlbedoGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceRadiationGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceEmissivityGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/ResistanceGroundEvaporationGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/PsychrometricVariableGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SurfaceEnergyFluxGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GlacierTemperatureSolverMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GlacierThermalDiffusionMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GlacierTemperatureMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/GlacierPhaseChangeMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/EnergyMainGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowpackHydrologyGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/SnowWaterMainGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/WaterMainGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/BalanceErrorCheckGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/src/NoahmpMainGlacierMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpDriverMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpReadTableMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpSnowInitMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpInitMainMod.F90`
- `src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpIOVarInitMod.F90`
