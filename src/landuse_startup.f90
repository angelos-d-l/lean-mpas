! The LANDUSE.TBL start values, from core_atmosphere/physics/mpas_atmphys_landuse.F of MPAS 8.4.2 (its loop body is MPAS's,
! comments included).
! landuse_init_forMPAS for one cell at a cold start (a restart skips it). julday is the day of year of the start.
module landuse_startup
 implicit none
 private
 public :: surface_table, read_surface_table, landuse_cold
 type surface_table
  real :: albd(100,12), slmo(100,12), sfem(100,12), sfz0(100,12), therin(100,12), scfx(100,12), sfhc(100,12)
 end type
contains
 subroutine read_surface_table(path, t)
 character(*), intent(in) :: path
 type(surface_table), intent(out) :: t
 character(80) :: name
 integer :: unit, cats, seasons, i, j, category
 open(newunit=unit, file=path, status='old', action='read')
 do
  read(unit,'(a)') name
  read(unit,*) cats, seasons
  if (trim(name) == 'MODIFIED_IGBP_MODIS_NOAH') exit
  do j = 1, seasons
   read(unit,*)
   do i = 1, cats
    read(unit,*)
   end do
  end do
 end do
 t%albd = 0.; t%slmo = 0.; t%sfem = 0.; t%sfz0 = 0.; t%therin = 0.; t%scfx = 0.; t%sfhc = 0.
 do j = 1, seasons
  read(unit,*)
  do i = 1, cats
   read(unit,*) category, t%albd(i,j), t%slmo(i,j), t%sfem(i,j), t%sfz0(i,j), t%therin(i,j), t%scfx(i,j), t%sfhc(i,j)
  end do
 end do
 close(unit)
 end subroutine

 subroutine landuse_cold(t, julday, lat, vegetation, background_albedo, snow_cover, snow_albedo, ice_fraction, &
                         albedo_out, emissivity_out, roughness_out, moisture_availability_out, friction_velocity_out)
 type(surface_table), intent(in) :: t
 integer, intent(in) :: julday, vegetation
 real, intent(in) :: lat, background_albedo, snow_cover, snow_albedo, ice_fraction
 real, intent(out) :: albedo_out, emissivity_out, roughness_out, moisture_availability_out, friction_velocity_out
 integer, parameter :: nCells = 1, isice = 15, iswater = 17
 logical, parameter :: config_sfc_albedo = .true., config_frac_seaice = .true.
 ! MPAS sets the threshold before this routine runs (physics_run_init, mpas_atmphys_manager.F:740-744: 0.02 with
 ! config_frac_seaice = true).
 real, parameter :: xice_threshold = 0.02
 integer :: iCell, is, isn, ivgtyp(1)
 real :: albd(100,12), slmo(100,12), sfem(100,12), sfz0(100,12), therin(100,12), scfx(100,12)
 real :: latCell(1), albbck(1), sfc_albedo(1), snowc(1), snoalb(1), thc(1), z0(1), znt(1), embck(1), sfc_emiss(1), xice(1)
 real, target :: mavail_value(1), ust_value(1)
 real, pointer :: mavail(:), ust(:)
 mavail => mavail_value; ust => ust_value
 albd = t%albd; slmo = t%slmo; sfem = t%sfem; sfz0 = t%sfz0; therin = t%therin; scfx = t%scfx
 latCell = lat; ivgtyp = vegetation; albbck = background_albedo; snowc = snow_cover; snoalb = snow_albedo; xice = ice_fraction
 do iCell = 1, nCells

    !finds the season as function of julian day (summer=1, winter=2): summer in the Northern
    !Hemisphere is defined between March 15th and September 15th (winter otherwise).
    isn = 1
    if(julday.lt.105 .or. julday.ge.288) isn=2
    if(latCell(iCell) .lt. 0.) isn=3-isn

    is = ivgtyp(iCell)
    
    !set no data points to water:
    if(is.eq.0) is = iswater
    if(.not. config_sfc_albedo) albbck(iCell) = albd(is,isn)/100.
    sfc_albedo(iCell) = albbck(iCell)

    if(snowc(iCell) .gt. 0.5) then
       albbck(iCell) = albd(isice,isn) / 100.
       if(config_sfc_albedo) then
          sfc_albedo(iCell) = snoalb(iCell)
       else
          sfc_albedo(iCell) = albbck(iCell) / (1+scfx(is,isn))
       endif
    endif
    thc(iCell)    = therin(is,isn) / 100.
    z0(iCell)     = sfz0(is,isn) / 100.
    znt(iCell)    = z0(icell)
    embck(iCell)  = sfem(is,isn)
    sfc_emiss(iCell) = embck(iCell)

    if(associated(mavail)) mavail(iCell) = slmo(is,isn)
    if(associated(ust)) ust(iCell) = 0.0001
 
    !set sea-ice points to land with ice/snow surface properties:
    if(xice(iCell) .ge. xice_threshold) then
       albbck(iCell) = albd(isice,isn) / 100.
       embck(iCell)  = sfem(isice,isn)
       if(config_frac_seaice) then
          !0.08 is the albedo over open water.
          !0.98 is the emissivity over open water.
          sfc_albedo(iCell) = xice(iCell)*albbck(iCell) + (1-xice(iCell))*0.08
          sfc_emiss(iCell)  = xice(iCell)*embck(iCell)  + (1-xice(iCell))*0.98
       else
          sfc_albedo(iCell) = albbck(iCell)
          sfc_emiss(iCell)  = embck(iCell)
       endif
       thc(iCell) = therin(isice,isn) / 100.
       z0(icell)  = sfz0(isice,isn) / 100.
       znt(iCell) = z0(iCell)

       if(associated(mavail)) mavail(iCell) = slmo(isice,isn)
    endif

 enddo

 albedo_out = sfc_albedo(1); emissivity_out = sfc_emiss(1); roughness_out = znt(1)
 moisture_availability_out = mavail(1); friction_velocity_out = ust(1)
 end subroutine
end module
