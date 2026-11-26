! The MPAS 8.4.2 side of the sea-ice test (tests/sea_ice/run.sh): the unchanged drivers driver_sfclayer, driver_seaice
! and atmphys_sfc_diagnostics, run on a block of n columns through the test pool stand-in (tests/dyn_pool.f90), in the
! order of mpas_atmphys_driver.F:264-316 (allocate, call, deallocate). The host sets the physics inputs MPAS_to_physics
! would set (the *_p arrays of mpas_atmphys_vars) and the pools; nothing here is lean code.
module sea_ice_original
 use mpas_kind_types, only: RKIND, StrKIND
 use mpas_derived_types, only: mpas_pool_type
 use mpas_pool_routines
 use mpas_atmphys_vars, only: ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte, ids, ide, jds, jde, kds, kde, &
                              num_soils, dt_pbl, xice_threshold, pres_hyd_p, pi_p, psfc_p, th_p, t_p, u_p, v_p, qv_p, &
                              qc_p, rho_p, dz_p, pres2_hyd_p
 use mpas_atmphys_driver_sfclayer, only: init_sfclayer, allocate_sfclayer, deallocate_sfclayer, driver_sfclayer
 use mpas_atmphys_driver_seaice, only: allocate_seaice, deallocate_seaice, driver_seaice
 use mpas_atmphys_sfc_diagnostics, only: atmphys_sfc_diagnostics
 implicit none
 private
 public :: surface_pools, setup, run_step
 integer, parameter :: nlev = 45
 ! the pool arrays of one block of n cells (diag_physics, sfc_input, mesh, diag, output_noahmp)
 type surface_pools
  real(kind=RKIND), allocatable :: br(:), cpm(:), chs(:), chs2(:), cqs2(:), flhc(:), flqc(:), gz1oz0(:), hfx(:), qfx(:), &
   qgh(:), qsfc(:), lh(:), mol(:), psih(:), psim(:), regime(:), rmol(:), ust(:), ustm(:), wspd(:), znt(:), zol(:), &
   q2(:), t2m(:), th2m(:), u10(:), v10(:), cd(:), cda(:), ck(:), cka(:), hpbl(:), mavail(:), ch(:), qcg(:), &
   acsnom(:), acsnow(:), glw(:), gsw(:), grdflx(:), sfc_albedo(:), sfc_emiss(:), sfcrunoff(:), z0(:), sr(:), &
   rainncv(:), raincv(:), skintemp(:), sst(:), xice(:), xland(:), snowh(:), snoalb(:), snow(:), snowc(:), &
   meshDensity(:), areaCell(:), surface_pressure(:), q2mxy(:), t2mxy(:), fh(:), fm(:)
  real(kind=RKIND), allocatable :: tslb(:,:)
  type(mpas_pool_type) :: configs, mesh, diag, diag_physics, sfc_input, output_noahmp
  logical :: frac_seaice = .true., do_restart = .false.
  character(len=StrKIND) :: sfclayer_scheme = 'sf_mynn', lsm_scheme = 'sf_noahmp', microp_scheme = 'mp_thompson', &
                            convection_scheme = 'cu_grell_freitas'
  real(kind=RKIND) :: len_disp = 120000.
  integer :: ncells_solve
 end type
contains
 subroutine setup(p, n, dt)
 type(surface_pools), target, intent(inout) :: p
 integer, intent(in) :: n
 real(kind=RKIND), intent(in) :: dt
 ids = 1; ide = n+1; jds = 1; jde = 2; kds = 1; kde = nlev+1
 ims = 1; ime = n; jms = 1; jme = 1; kms = 1; kme = nlev+1
 its = 1; ite = n; jts = 1; jte = 1; kts = 1; kte = nlev
 num_soils = 4
 dt_pbl = dt
 xice_threshold = 0.02   ! physics_run_init with config_frac_seaice = true
 allocate(pres_hyd_p(ims:ime,kms:kme,jms:jme), pi_p(ims:ime,kms:kme,jms:jme), th_p(ims:ime,kms:kme,jms:jme), &
          t_p(ims:ime,kms:kme,jms:jme), u_p(ims:ime,kms:kme,jms:jme), v_p(ims:ime,kms:kme,jms:jme), &
          qv_p(ims:ime,kms:kme,jms:jme), qc_p(ims:ime,kms:kme,jms:jme), rho_p(ims:ime,kms:kme,jms:jme), &
          dz_p(ims:ime,kms:kme,jms:jme), pres2_hyd_p(ims:ime,kms:kme,jms:jme), psfc_p(ims:ime,jms:jme))
 allocate(p%br(n), p%cpm(n), p%chs(n), p%chs2(n), p%cqs2(n), p%flhc(n), p%flqc(n), p%gz1oz0(n), p%hfx(n), p%qfx(n), &
  p%qgh(n), p%qsfc(n), p%lh(n), p%mol(n), p%psih(n), p%psim(n), p%regime(n), p%rmol(n), p%ust(n), p%ustm(n), p%wspd(n), &
  p%znt(n), p%zol(n), p%q2(n), p%t2m(n), p%th2m(n), p%u10(n), p%v10(n), p%cd(n), p%cda(n), p%ck(n), p%cka(n), p%hpbl(n), &
  p%mavail(n), p%ch(n), p%qcg(n), p%acsnom(n), p%acsnow(n), p%glw(n), p%gsw(n), p%grdflx(n), p%sfc_albedo(n), &
  p%sfc_emiss(n), p%sfcrunoff(n), p%z0(n), p%sr(n), p%rainncv(n), p%raincv(n), p%skintemp(n), p%sst(n), p%xice(n), &
  p%xland(n), p%snowh(n), p%snoalb(n), p%snow(n), p%snowc(n), p%meshDensity(n), p%areaCell(n), p%surface_pressure(n), &
  p%q2mxy(n), p%t2mxy(n), p%fh(n), p%fm(n), p%tslb(num_soils,n))
 p%ncells_solve = n
 call pool_put_logical(p%configs, 'config_frac_seaice', p%frac_seaice)
 call pool_put_logical(p%configs, 'config_do_restart', p%do_restart)
 call pool_put_string(p%configs, 'config_sfclayer_scheme', p%sfclayer_scheme)
 call pool_put_string(p%configs, 'config_lsm_scheme', p%lsm_scheme)
 call pool_put_string(p%configs, 'config_microp_scheme', p%microp_scheme)
 call pool_put_string(p%configs, 'config_convection_scheme', p%convection_scheme)
 call pool_put_real(p%configs, 'config_len_disp', p%len_disp)
 call pool_put(p%mesh, 'meshDensity', p%meshDensity)
 call pool_put(p%mesh, 'areaCell', p%areaCell)
 call pool_put_dimension(p%mesh, 'nCellsSolve', p%ncells_solve)
 call pool_put(p%diag, 'surface_pressure', p%surface_pressure)
 call pool_put(p%output_noahmp, 'q2mxy', p%q2mxy)
 call pool_put(p%output_noahmp, 't2mxy', p%t2mxy)
 call pool_put(p%sfc_input, 'skintemp', p%skintemp)
 call pool_put(p%sfc_input, 'sst', p%sst)
 call pool_put(p%sfc_input, 'xice', p%xice)
 call pool_put(p%sfc_input, 'xland', p%xland)
 call pool_put(p%sfc_input, 'snowh', p%snowh)
 call pool_put(p%sfc_input, 'snoalb', p%snoalb)
 call pool_put(p%sfc_input, 'snow', p%snow)
 call pool_put(p%sfc_input, 'snowc', p%snowc)
 call pool_put(p%sfc_input, 'tslb', p%tslb)
 call put(p%diag_physics, 'br', p%br); call put(p%diag_physics, 'cpm', p%cpm); call put(p%diag_physics, 'chs', p%chs)
 call put(p%diag_physics, 'chs2', p%chs2); call put(p%diag_physics, 'cqs2', p%cqs2); call put(p%diag_physics, 'flhc', p%flhc)
 call put(p%diag_physics, 'flqc', p%flqc); call put(p%diag_physics, 'gz1oz0', p%gz1oz0); call put(p%diag_physics, 'hfx', p%hfx)
 call put(p%diag_physics, 'qfx', p%qfx); call put(p%diag_physics, 'qgh', p%qgh); call put(p%diag_physics, 'qsfc', p%qsfc)
 call put(p%diag_physics, 'lh', p%lh); call put(p%diag_physics, 'mol', p%mol); call put(p%diag_physics, 'psih', p%psih)
 call put(p%diag_physics, 'psim', p%psim); call put(p%diag_physics, 'regime', p%regime); call put(p%diag_physics, 'rmol', p%rmol)
 call put(p%diag_physics, 'ust', p%ust); call put(p%diag_physics, 'ustm', p%ustm); call put(p%diag_physics, 'wspd', p%wspd)
 call put(p%diag_physics, 'znt', p%znt); call put(p%diag_physics, 'zol', p%zol); call put(p%diag_physics, 'q2', p%q2)
 call put(p%diag_physics, 't2m', p%t2m); call put(p%diag_physics, 'th2m', p%th2m); call put(p%diag_physics, 'u10', p%u10)
 call put(p%diag_physics, 'v10', p%v10); call put(p%diag_physics, 'cd', p%cd); call put(p%diag_physics, 'cda', p%cda)
 call put(p%diag_physics, 'ck', p%ck); call put(p%diag_physics, 'cka', p%cka); call put(p%diag_physics, 'hpbl', p%hpbl)
 call put(p%diag_physics, 'mavail', p%mavail); call put(p%diag_physics, 'ch', p%ch); call put(p%diag_physics, 'qcg', p%qcg)
 call put(p%diag_physics, 'acsnom', p%acsnom); call put(p%diag_physics, 'acsnow', p%acsnow)
 call put(p%diag_physics, 'glw', p%glw); call put(p%diag_physics, 'gsw', p%gsw); call put(p%diag_physics, 'grdflx', p%grdflx)
 call put(p%diag_physics, 'sfc_albedo', p%sfc_albedo); call put(p%diag_physics, 'sfc_emiss', p%sfc_emiss)
 call put(p%diag_physics, 'sfcrunoff', p%sfcrunoff); call put(p%diag_physics, 'z0', p%z0); call put(p%diag_physics, 'sr', p%sr)
 call put(p%diag_physics, 'rainncv', p%rainncv); call put(p%diag_physics, 'raincv', p%raincv)
 call put(p%diag_physics, 'fh', p%fh); call put(p%diag_physics, 'fm', p%fm)
 call init_sfclayer(p%configs)
 end subroutine
 subroutine put(pool, name, a)
 type(mpas_pool_type), intent(inout) :: pool
 character(*), intent(in) :: name
 real(kind=RKIND), target, intent(in) :: a(:)
 call pool_put(pool, name, a)
 end subroutine

 ! One physics step of the surface: surface layer, (Noah-MP skips these cells), sea ice, 2 m diagnostics.
 subroutine run_step(p, itimestep)
 type(surface_pools), intent(inout) :: p
 integer, intent(in) :: itimestep
 call allocate_sfclayer(p%configs)
 call driver_sfclayer(itimestep, p%configs, p%mesh, p%diag_physics, p%sfc_input, its, ite)
 call deallocate_sfclayer(p%configs)
 call allocate_seaice(p%configs)
 call driver_seaice(p%configs, p%diag_physics, p%sfc_input, its, ite)
 call deallocate_seaice(p%configs)
 call atmphys_sfc_diagnostics(p%configs, p%mesh, p%diag, p%diag_physics, p%sfc_input, p%output_noahmp, its, ite)
 end subroutine
end module
