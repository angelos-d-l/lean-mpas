! MPAS's physics_update_sst as it is: tests/surface/run.sh cuts the executable lines of
! physics/mpas_atmphys_update_surface.F:184-300 (the threshold, the albedo and emissivity rescale, the three branches,
! the xicem and seaice update) into update_body.inc, and this file compiles them around plain arrays in place of the pools.
! The text of the routine is not changed; only the declarations and the pool pointers are replaced.
module mpas_update_oracle
 use mpas_kind_types, only: RKIND
 implicit none
 private
 public :: oracle_update_sst
contains
 subroutine oracle_update_sst(nCellsSolve, nSoilLevels, config_frac_seaice, xice_threshold, isice, iswater, &
                              ivgtyp, isltyp, vegfra, sfc_albbck, sst, tmn, tsk, tslb, sh2o, smois, snow, snowc, snowh, &
                              seaice, xice, xland, sfc_albedo, sfc_emiss, sfc_emibck, xicem, nb_to_land, nb_to_ocean, nb_removed)
 integer, intent(in) :: nCellsSolve, nSoilLevels, isice, iswater
 logical, intent(in) :: config_frac_seaice
 real(kind=RKIND), intent(in) :: xice_threshold
 integer, intent(inout) :: ivgtyp(:), isltyp(:)
 real(kind=RKIND), intent(inout) :: vegfra(:), sfc_albbck(:), sst(:), tmn(:), tsk(:), snow(:), snowc(:), snowh(:), &
                                    seaice(:), xice(:), xland(:), sfc_albedo(:), sfc_emiss(:), sfc_emibck(:), xicem(:)
 real(kind=RKIND), intent(inout) :: tslb(:,:), sh2o(:,:), smois(:,:)
 integer, intent(out) :: nb_to_land, nb_to_ocean, nb_removed
 integer :: iCell, iSoil
 include 'update_body.inc'
 end subroutine
end module
