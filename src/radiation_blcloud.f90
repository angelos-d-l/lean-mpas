#ifdef LEAN_RAD_BLCLOUD
! Lean departure (build switch LEAN_RAD_BLCLOUD, on in flavor lean, off in mpas; compiled only with it, config/units.json; docs/fixes.md): the
! cloud water and ice RRTMG sees include MYNN's subgrid cloud.
! MPAS fixes icloud_bl = 0 (mpas_atmphys_vars.F:418, "no coupling of subgrid-scale clouds with radiation"), so the
! stratocumulus, fog and shallow cumulus of MYNN's PDF and EDMF (qc_bl, qi_bl, cldfra_bl of the last PBL step) are
! invisible to radiation. WRF couples them by default (icloud_bl = 1): qc_bl is added where the resolved qc < 1e-6 and
! qi_bl where qi < 1e-8, both where cldfra_bl > 0.001 (phys/module_radiation_driver.F); the same here. The cloud fraction
! stays MPAS's own (Xu-Randall, radiation_cloud.f90) on that water, so resolved cloud never loses fraction; the effective
! radii fall back to the defaults of radiation_lw/sw.f90 where Thompson has no cloud. The state keeps its qc and qi.
! Caller: the radiation loop of lean_model.f90 (host). rad_fast_prep of phys_dev.f90 (card) has the same lines written out.
module radiation_blcloud
 use config,only: nz
 implicit none
 private
 public :: bl_cloud_water
contains
 subroutine bl_cloud_water(qc,qi,qc_bl,qi_bl,cldfra_bl,qc_rad,qi_rad)
 real,intent(in) :: qc(nz),qi(nz),qc_bl(nz),qi_bl(nz),cldfra_bl(nz)
 real,intent(out) :: qc_rad(nz),qi_rad(nz)
 integer :: k
 do k=1,nz
  qc_rad(k)=qc(k)
  qi_rad(k)=qi(k)
  if(qc(k)<1.e-6.and.cldfra_bl(k)>0.001)qc_rad(k)=qc(k)+qc_bl(k)
  if(qi(k)<1.e-8.and.cldfra_bl(k)>0.001)qi_rad(k)=qi(k)+qi_bl(k)
 enddo
 end subroutine
end module
#endif
