! Optional instantaneous surface stream (LEAN_SURFACE=path; docs/surface-stream.md). Native endian int32/real32; names are 32 bytes.
! Header: ncells, nf, dt; names; static real32 xland, ivgtyp, isltyp, lat, lon, terrain (xland and the types are the start's;
! a surface update changes them in the model, not in this header).
! Records: it; each named field (ncells). Noah-MP fields are land-only; water holds zeros.
! Name | source | units | cells | meaning
! tsk | state%tsk | K | all | Surface skin temperature; water keeps the startup SST, or the last surface record's sst with config_sst_update
! xice | ice | 1 | all | Sea ice fraction: the startup one, or the last surface record's with config_sst_update
! snow | snow | kg m-2 | all | Radiation surface input; land refreshed each step, water startup
! snowh | state%snowh | m | all | Snow depth
! gsw | rad%sw_net | W m-2 | all | Net downward surface SW; last radiation call, held until the next (config_radtsw_interval)
! glw | rad%lw_down | W m-2 | all | Downward surface LW; last radiation call, held until the next (config_radtlw_interval)
! albedo | albedo | 1 | all | Radiation surface input; land refreshed when valid, water startup
! emiss | emiss | 1 | all | Radiation surface input; land refreshed each step, water startup
! hfx | state%hfx | W m-2 | all | Upward sensible heat flux
! qfx | state%qfx | kg m-2 s-1 | all | Upward moisture flux
! lh | state%lh | W m-2 | all | Upward latent heat flux
! ust | state%ust | m s-1 | all | Friction velocity
! znt | state%znt | m | all | Roughness length
! hpbl | state%pblh | m | all | PBL height
! chs | state%chs | m s-1 | all | Surface exchange coefficient
! chs2 | state%chs2 | m s-1 | all | 2 m heat exchange coefficient
! cqs2 | state%cqs2 | m s-1 | all | 2 m moisture exchange coefficient
! flhc | state%flhc | W m-2 K-1 | all | Heat exchange coefficient
! flqc | state%flqc | kg m-2 s-1 | all | Moisture exchange coefficient
! qsfc | state%qsfc | kg kg-1 | all | Surface specific humidity
! br | state%br | 1 | all | Bulk Richardson number
! zol | state%zol | 1 | all | Height divided by Obukhov length
! grdflx | lands%energy_flux_heatgroundtot | W m-2 | land | Ground heat flux, positive into soil/snow
! tslb1 | lands%energy_state_temperaturesoilsnow(1,:) | K | land | Top soil temperature; snow indices -2:0 excluded
! smois1 | lands%water_state_soilmoisture(1,:) | m3 m-3 | land | Top soil total moisture
! sh2o1 | lands%water_state_soilliqwater(1,:) | m3 m-3 | land | Top soil liquid water
! tmn | lands%forcing_temperaturesoilbottom | K | land | Deep soil boundary temperature
! tv | lands%energy_state_temperaturecanopy | K | land | Vegetation temperature
! tg | lands%energy_state_temperaturegrd | K | land | Ground temperature
! tah | lands%energy_state_temperaturecanopyair | K | land | Canopy air temperature
! eah | lands%energy_state_pressurevaporcanair | Pa | land | Canopy air vapour pressure
! fveg | lands%energy_state_vegfrac | 1 | land | Vegetation fraction
! lai | lands%energy_state_leafareaindex | m2 m-2 | land | Leaf area index
! canwat | lands%water_state_canopyliqwater + lands%water_state_canopyice | mm | land | Canopy liquid plus ice; MPAS CANWAT
! t2mv | lands%energy_state_temperatureair2mveg | K | land | Vegetated tile 2 m temperature
! t2mb | lands%energy_state_temperatureair2mbare | K | land | Bare tile 2 m temperature
! q2mv | lands%energy_state_spechumidity2mveg | kg kg-1 | land | Vegetated tile 2 m specific humidity, stored value (not MPAS mixing ratio)
! q2mb | lands%energy_state_spechumidity2mbare | kg kg-1 | land | Bare tile 2 m specific humidity, stored value (not MPAS mixing ratio)
! fsa | lands%energy_flux_radswabssfc | W m-2 | land | Absorbed surface SW
! fira | lands%energy_flux_radlwnetsfc | W m-2 | land | Net LW, positive to atmosphere
! fsh | lands%energy_flux_heatsensiblesfc | W m-2 | land | Sensible heat, positive to atmosphere
! fgev | lands%energy_flux_heatlatentgrd | W m-2 | land | Ground latent heat, positive to atmosphere
! fcev | lands%energy_flux_heatlatentcanopy | W m-2 | land | Canopy evaporation latent heat, positive to atmosphere
! fctr | lands%energy_flux_heatlatenttransp | W m-2 | land | Transpiration latent heat, positive to atmosphere
! chv | lands%energy_state_exchcoeffshabvcan | m s-1 | land | Vegetated exchange coefficient above displacement height
! chb | lands%energy_state_exchcoeffshbare | m s-1 | land | Bare exchange coefficient above displacement height
! chv2 | lands%energy_state_exchcoeffsh2mveg | m s-1 | land | Vegetated 2 m heat exchange coefficient
! chb2 | lands%energy_state_exchcoeffsh2mbare | m s-1 | land | Bare 2 m heat exchange coefficient
! t2 | state%t2 | K | all | 2 m temperature
! q2 | state%q2 | kg kg-1 | all | 2 m humidity; land: mixing ratio; water: MPAS's formula, which mixes specific humidity and mixing ratio
! th2 | state%th2 | K | all | 2 m potential temperature
! u10 | state%u10 | m s-1 | all | 10 m zonal wind
! v10 | state%v10 | m s-1 | all | 10 m meridional wind
module surface_output
 use config, only: ncells, dt
 use model_state, only: state_type
 use noahmp_state, only: noahmp_state_type
 use pre_dynamics, only: radiation_cache
 implicit none
 private
 public :: surface_on, surface_start, surface_types, surface_static, surface_write
 logical, save :: surface_on = .false.
 integer, save :: surface_unit = -1
 real, allocatable, save :: vegetation(:), soil(:)
 integer, parameter :: nf = 53
 character(32), parameter :: names(nf) = [character(32) :: &
  'tsk', 'xice', 'snow', 'snowh', 'gsw', 'glw', 'albedo', &
  'emiss', 'hfx', 'qfx', 'lh', 'ust', 'znt', 'hpbl', &
  'chs', 'chs2', 'cqs2', 'flhc', 'flqc', 'qsfc', 'br', &
  'zol', 'grdflx', 'tslb1', 'smois1', 'sh2o1', 'tmn', 'tv', &
  'tg', 'tah', 'eah', 'fveg', 'lai', 'canwat', 't2mv', &
  't2mb', 'q2mv', 'q2mb', 'fsa', 'fira', 'fsh', 'fgev', &
  'fcev', 'fctr', 'chv', 'chb', 'chv2', 'chb2', 't2', &
  'q2', 'th2', 'u10', 'v10']
contains
 subroutine surface_start(path)
 character(*), intent(in) :: path
 surface_on = .true.
 allocate(vegetation(ncells), soil(ncells))
 open(newunit=surface_unit, file=path, access='stream', form='unformatted', status='replace')
 write(surface_unit) ncells, nf, dt
 write(surface_unit) names
 end subroutine
 subroutine surface_types(c, ivgtyp, isltyp)
 integer, intent(in) :: c, ivgtyp, isltyp
 vegetation(c) = real(ivgtyp); soil(c) = real(isltyp)
 end subroutine
 subroutine surface_static(xland, lat, lon, terrain)
 real, intent(in) :: xland(ncells), lat(ncells), lon(ncells), terrain(ncells)
 real, parameter :: degrees = 180. / acos(-1.)
 write(surface_unit) xland, vegetation, soil, lat*degrees, lon*degrees, terrain
 deallocate(vegetation, soil)
 end subroutine
 subroutine surface_write(it, state, lands, rad, ice, snow, albedo, emiss)
 integer, intent(in) :: it
 type(state_type), intent(in) :: state
 type(noahmp_state_type), intent(in) :: lands
 type(radiation_cache), intent(in) :: rad(ncells)
 real, intent(in) :: ice(ncells), snow(ncells), albedo(ncells), emiss(ncells)
 integer :: c
 write(surface_unit) it
 write(surface_unit) state%tsk
 write(surface_unit) ice
 write(surface_unit) snow
 write(surface_unit) state%snowh
 write(surface_unit) rad%sw_net
 write(surface_unit) rad%lw_down
 write(surface_unit) albedo
 write(surface_unit) emiss
 write(surface_unit) state%hfx
 write(surface_unit) state%qfx
 write(surface_unit) state%lh
 write(surface_unit) state%ust
 write(surface_unit) state%znt
 write(surface_unit) state%pblh
 write(surface_unit) state%chs
 write(surface_unit) state%chs2
 write(surface_unit) state%cqs2
 write(surface_unit) state%flhc
 write(surface_unit) state%flqc
 write(surface_unit) state%qsfc
 write(surface_unit) state%br
 write(surface_unit) state%zol
 write(surface_unit) lands%energy_flux_heatgroundtot
 write(surface_unit) lands%energy_state_temperaturesoilsnow(1,:)
 write(surface_unit) lands%water_state_soilmoisture(1,:)
 write(surface_unit) lands%water_state_soilliqwater(1,:)
 write(surface_unit) lands%forcing_temperaturesoilbottom
 write(surface_unit) lands%energy_state_temperaturecanopy
 write(surface_unit) lands%energy_state_temperaturegrd
 write(surface_unit) lands%energy_state_temperaturecanopyair
 write(surface_unit) lands%energy_state_pressurevaporcanair
 write(surface_unit) lands%energy_state_vegfrac
 write(surface_unit) lands%energy_state_leafareaindex
 write(surface_unit) (lands%water_state_canopyliqwater(c) + lands%water_state_canopyice(c), c=1,ncells)
 write(surface_unit) lands%energy_state_temperatureair2mveg
 write(surface_unit) lands%energy_state_temperatureair2mbare
 write(surface_unit) lands%energy_state_spechumidity2mveg
 write(surface_unit) lands%energy_state_spechumidity2mbare
 write(surface_unit) lands%energy_flux_radswabssfc
 write(surface_unit) lands%energy_flux_radlwnetsfc
 write(surface_unit) lands%energy_flux_heatsensiblesfc
 write(surface_unit) lands%energy_flux_heatlatentgrd
 write(surface_unit) lands%energy_flux_heatlatentcanopy
 write(surface_unit) lands%energy_flux_heatlatenttransp
 write(surface_unit) lands%energy_state_exchcoeffshabvcan
 write(surface_unit) lands%energy_state_exchcoeffshbare
 write(surface_unit) lands%energy_state_exchcoeffsh2mveg
 write(surface_unit) lands%energy_state_exchcoeffsh2mbare
 write(surface_unit) state%t2
 write(surface_unit) state%q2
 write(surface_unit) state%th2
 write(surface_unit) state%u10
 write(surface_unit) state%v10
 flush(surface_unit)
 end subroutine
end module
