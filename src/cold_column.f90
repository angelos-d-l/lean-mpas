! Cold start of a physics column: the column fields MPAS has before its first physics step when it starts
! from the input stream. zero_state (phys_state.f90) has set every field to 0 before; this sets the few that are not 0.
! The fields the physics inputs set from the dynamics state at every step (u v theta p exner dz w q* ni nr rho_moist
! p_hyd psfc, and the GF surface_pressure) are not touched here.
!
! Fields MPAS leaves at 0 at a cold start (not in init.nc, no init routine sets them;
! mpas_atmphys_init.F physics_init and the init_pbl/init_sfclayer/init_convection routines only set constants). The
! MYNN surface layer initialises its friction velocity, Obukhov length, surface humidity and qstar at step 1 (initflag).
!  MYNN PBL       qke(tke_pbl) tsq qsq cov el_pbl sh(sh3d) sm(sm3d) qc_bl qi_bl cldfra_bl(cldfrac_bl) exch_h exch_m
!                 pblh(hpbl) kpbl maxwidth maxmf ztop_plume ktop_plume
!  surface layer  regime hfx qfx lh mol rmol qgh qsfc zol cpm chs2 cqs2 chs ch flhc flqc gz1oz0 wspd br psim psih
!                 u10 v10 th2(th2m) t2(t2m) q2 wstar qstar ck cka cd cda ustm
!  tendencies     t_u t_v t_qv t_qc t_qi t_qr t_qs t_qg t_ni t_nr t_theta (rublten.. rthraten, all tend_physics)
!  Thompson       rainprod evapprod re_cloud re_ice re_snow rainnc rainncv snownc snowncv graupelnc graupelncv sr
!  GF             shortwave hbot htop xmb_total xmb_shallow raincv pratec k22_shallow kbcon_shallow ktop_shallow
!                 dynamics_theta dynamics_qv radiation_theta rthcuten rqvcuten rqccuten rqicuten qc_cu qi_cu
! Fields set elsewhere: znt mavail ust, set by landuse_init_forMPAS (noahmp_cold_start.f90, landuse_startup.f90).
module cold_column
 use config, only: nz
 use dyn_config, only: len_disp
 use model_state, only: column_view
 use gf_state, only: gf_column
 implicit none
 private
 public :: cold_cell, cold_physics_column
 ! One cell's start fields of the init file (lean_inputs read_columns).
 type cold_cell
  real :: skintemp, sst, xice, seaice, xland
  integer :: landmask
  real :: surface_pressure, snow, snowh, snowc, tmn, vegfra, shdmin, shdmax, sfc_albbck, snoalb
  integer :: ivgtyp, isltyp
  real :: tslb(4), smois(4), sh2o(4), dzs(4), albedo12m(12), greenfrac(12)
  real :: latCell, lonCell, areaCell
 end type
contains
 ! r: the cell's init values; a: its physics column (written); f: its GF column (area, terrain written); z: zgrid of the
 ! column; mesh_density: meshDensity of the cell.
 subroutine cold_physics_column(r, a, f, z, mesh_density)
 type(cold_cell), intent(in) :: r
 type(column_view), intent(in) :: a
 type(gf_column), intent(in) :: f
 real, intent(in) :: z(nz+1), mesh_density
 a%tsk = r%skintemp
 a%xland = r%xland
 a%snowh = r%snowh
 ! dx_p of the MPAS physics drivers (e.g. mpas_atmphys_driver_gwdo.F:533)
 a%dx = len_disp / mesh_density**0.25
 ! init_thompson_clouddroplets_forMPAS (mpas_atmphys_init_microphysics.F:81-87)
 if (r%landmask == 1) then
  a%nt = 300.e6
 elseif (r%landmask == 0) then
  a%nt = 100.e6
 endif
 a%mu = min(15., (1000.e6/a%nt + 2.))
 f%area = r%areaCell
 f%terrain = z(1)
 end subroutine
end module
