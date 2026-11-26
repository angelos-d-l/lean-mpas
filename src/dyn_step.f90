! One dynamics step of the model for the configuration lean_mpas supports:
! rk_integration_setup, moist coefficients, three dynamics substeps (implicit coefficients, Runge-Kutta stages
! 1-3 with tendencies, acoustic steps, recovery, diagnostics, rk_dynamics_substep_finish), the three scalar transport
! stages, wind reconstruction and the post-dynamics bookkeeping (Grell-Freitas dynamics forcing, clipping).
! Order and arithmetic follow atm_srk3 of MPAS 8.4.2 ("the original" below). The physics tendencies (tend_*_physics, tend_scalars) are inputs.
!
! Lateral boundary: when g%lbc is associated the operators of src/dyn_boundary.f90 act where atm_srk3 calls them
! (tendency adjustment of the specified and relaxation zone after the perturbation form, u/ru reset after the recovery, w zero-gradient after the
! diagnostics, scalar adjustment after every transport stage, and - after Thompson, in model_step - theta_m/rtheta_p and the scalars of the specified
! zone). Without it (a global mesh) the step has no boundary operators.
!
! The state keeps the two time levels of MPAS (1 = start of the substep or step, 2 = working level).
module dyn_step
 use model_timing
 use config
 use dyn_config
 use dyn_mesh_data, only: dyn_mesh_type
 use dyn_state_data, only: dyn_state_type
 use dyn_startup
 use dyn_diagnostics
 use run_settings, only: apvm_upwinding
 use schemes, only: conv_on
 use dyn_tend, only: compute_dyn_tend, edge_trig
 use dyn_acoustic
 use dyn_scalars
 use dyn_handoff, only: gf_dynamics_forcing, clip_negative_scalars
 use initial_winds, only: reconstruct_winds, reconstruct_winds_trig
 use dyn_boundary
 implicit none

 ! everything that does not change during a step: the mesh fields and the coefficients derived from them at startup
 type dyn_grid
  type(dyn_mesh_type), pointer :: m => null()
  type(dyn_state_type), pointer :: s => null()          ! base state (rho_base, rtheta_base, exner_base, pressure_base)
  real, allocatable :: sign_vertex(:,:), sign_cell(:,:), zb_cell(:,:,:), zb3_cell(:,:,:), cos_edge(:), sin_edge(:)
  integer, allocatable :: kite(:,:), n_adv(:), adv_cells(:,:)
  real, allocatable :: inv_area_cell(:), inv_dv(:), inv_dc(:), inv_area_tri(:), del2(:), del4(:), reg_cell(:), reg_edge(:)
  real, allocatable :: adv_coefs(:,:), adv_coefs_3rd(:,:), dc2(:,:), ds2(:,:), dcs(:,:), dc(:,:), ds(:,:)
  real, allocatable :: dss(:,:), etp(:), etm(:), ewp(:), ewm(:), spec_cell(:), spec_edge(:), spec_vertex(:)
  real, allocatable :: cos_lat(:), sin_lat(:), cos_lon(:), sin_lon(:)   ! of the cell centres, for the wind reconstruction
  type(dyn_lbc), pointer :: lbc => null()       ! lateral boundary data; not associated = interior-only step
 end type

 type dyn_fields
  ! state, two time levels
  real, allocatable :: u1(:,:), u2(:,:), w1(:,:), w2(:,:), theta_m1(:,:), theta_m2(:,:), rho_zz1(:,:), rho_zz2(:,:)
  real, allocatable :: scalars1(:,:,:), scalars2(:,:,:)
  ! large-step diagnostics of the working level and their saved start-of-substep values
  real, allocatable :: ru(:,:), rw(:,:), rho_p(:,:), rtheta_p(:,:), exner(:,:), pressure_p(:,:)
  real, allocatable :: ru_save(:,:), rw_save(:,:), rho_p_save(:,:), rtheta_p_save(:,:), rho_zz_old_split(:,:)
  real, allocatable :: rho_edge(:,:), v(:,:), vorticity(:,:), divergence(:,:), ke(:,:), pv_vertex(:,:), pv_edge(:,:)
  real, allocatable :: ke_edge(:,:), ke_vertex(:,:)
  ! acoustic loop
  real, allocatable :: ru_p(:,:), rw_p(:,:), rho_pp(:,:), rtheta_pp(:,:), rtheta_pp_old(:,:)
  real, allocatable :: ru_avg(:,:), ww_avg(:,:), ru_avg_split(:,:), ww_avg_split(:,:)
  ! tendencies of the large step
  real, allocatable :: tend_u(:,:), tend_w(:,:), tend_theta(:,:), tend_rho(:,:), tend_u_euler(:,:), tend_w_euler(:,:)
  real, allocatable :: tend_theta_euler(:,:), rthdynten(:,:), eddy_visc_horz(:,:), h_divergence(:,:), dpdz(:,:)
  real, allocatable :: delsq_u(:,:), delsq_vorticity(:,:), delsq_divergence(:,:), delsq_w(:,:), delsq_theta(:,:)
  real, allocatable :: tend_w_pert(:,:)
  ! heating by the microphysics of the step before, per unit mass (written by the Thompson step, read by the large-step tendency and the recovery)
  real, allocatable :: rt_diabatic_tend(:,:)
  ! moist coefficients and coefficients of the implicit solve
  real, allocatable :: cqu(:,:), cqw(:,:), qtot(:,:)
  real, allocatable :: cofwr(:,:), cofwz(:,:), coftz(:,:), cofwt(:,:), a_tri(:,:), alpha_tri(:,:), gamma_tri(:,:), cofrz(:)
  ! physics tendencies handed to the dynamics (inputs)
  real, allocatable :: tend_ru_physics(:,:), tend_rtheta_physics(:,:), tend_rho_physics(:,:), tend_scalars(:,:,:)
  real, allocatable :: rqvdynten(:,:)
  ! scalar transport work arrays
  real, allocatable :: horiz_flux(:,:,:), rho_zz_int(:,:), sc_old(:,:), sc_new(:,:), s_max(:,:), s_min(:,:), wdtn(:,:)
  real, allocatable :: scale_arr(:,:,:), flux(:,:), flux_upwind(:,:), flux_tmp(:,:)
  ! reconstructed cell winds
  real, allocatable :: u_zonal(:,:), u_meridional(:,:)
 end type

contains

 subroutine setup_grid(m, s, g)
 type(dyn_mesh_type), target, intent(in) :: m
 type(dyn_state_type), target, intent(in) :: s
 type(dyn_grid), intent(inout) :: g
 integer :: c
 g%m => m; g%s => s
 allocate(g%sign_vertex(vertex_degree,nv1), g%sign_cell(maxedges,nc1), g%kite(maxedges,nc1))
 allocate(g%zb_cell(nz+1,maxedges,nc1), g%zb3_cell(nz+1,maxedges,nc1))
 allocate(g%inv_area_cell(nc1), g%inv_dv(ne1), g%inv_dc(ne1), g%inv_area_tri(nv1), g%del2(ne1), g%del4(ne1))
 allocate(g%reg_cell(nc1), g%reg_edge(ne1), g%n_adv(ne1), g%adv_cells(max_adv,ne1), g%adv_coefs(max_adv,ne1))
 allocate(g%adv_coefs_3rd(max_adv,ne1), g%dss(nz,nc1), g%etp(nz), g%etm(nz), g%ewp(nz+1), g%ewm(nz+1))
 allocate(g%dc2(maxedges,nc1), g%ds2(maxedges,nc1), g%dcs(maxedges,nc1), g%dc(maxedges,nc1), g%ds(maxedges,nc1))
 allocate(g%spec_cell(nc1), g%spec_edge(ne1), g%spec_vertex(nv1), g%cos_edge(ne1), g%sin_edge(ne1))
 call edge_trig(m%angleEdge, g%cos_edge, g%sin_edge)
 allocate(g%cos_lat(nc1), g%sin_lat(nc1), g%cos_lon(nc1), g%sin_lon(nc1))
 g%cos_lat = 0.; g%sin_lat = 0.; g%cos_lon = 0.; g%sin_lon = 0.
 do c = 1, ncells
  g%cos_lat(c) = cos(m%latCell(c)); g%sin_lat(c) = sin(m%latCell(c))
  g%cos_lon(c) = cos(m%lonCell(c)); g%sin_lon(c) = sin(m%lonCell(c))
 end do
 call edge_signs(m%cellsOnEdge, m%verticesOnEdge, m%edgesOnVertex, m%cellsOnVertex, m%nEdgesOnCell, m%edgesOnCell, &
                 m%verticesOnCell, m%zb, m%zb3, g%sign_vertex, g%sign_cell, g%kite, g%zb_cell, g%zb3_cell)
 call couple_zb3(g%zb3_cell)
 call inverse_geometry(m%areaCell, m%dvEdge, m%dcEdge, m%areaTriangle, g%inv_area_cell, g%inv_dv, g%inv_dc, g%inv_area_tri)
 call mesh_scaling(m%meshDensity, m%cellsOnEdge, g%del2, g%del4, g%reg_cell, g%reg_edge)
 call advection_weights(m%cellsOnEdge, m%cellsOnCell, m%nEdgesOnCell, m%dcEdge, m%dvEdge, m%deriv_two, &
                        g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd)
 call deformation_weights(m%cellsOnCell, m%nEdgesOnCell, m%cellsOnEdge, m%edgesOnCell, m%verticesOnCell, &
                          m%xCell, m%yCell, m%zCell, m%xVertex, m%yVertex, m%zVertex, g%dc2, g%ds2, g%dcs, g%dc, g%ds)
 call damping_coefs(m%zgrid, m%rdzw, g%dss, g%etp, g%etm, g%ewp, g%ewm)
 call boundary_zone_masks(m%bdyMaskCell, m%bdyMaskEdge, m%bdyMaskVertex, g%spec_cell, g%spec_edge, g%spec_vertex)
 end subroutine

 subroutine allocate_fields(f)
 type(dyn_fields), intent(inout) :: f
 allocate(f%u1(nz,ne1), f%u2(nz,ne1), f%w1(nz+1,nc1), f%w2(nz+1,nc1), f%theta_m1(nz,nc1), f%theta_m2(nz,nc1))
 allocate(f%rho_zz1(nz,nc1), f%rho_zz2(nz,nc1), f%scalars1(num_scalars,nz,nc1), f%scalars2(num_scalars,nz,nc1))
 allocate(f%ru(nz,ne1), f%rw(nz+1,nc1), f%rho_p(nz,nc1), f%rtheta_p(nz,nc1), f%exner(nz,nc1), f%pressure_p(nz,nc1))
 allocate(f%ru_save(nz,ne1), f%rw_save(nz+1,nc1), f%rho_p_save(nz,nc1), f%rtheta_p_save(nz,nc1), f%rho_zz_old_split(nz,nc1))
 allocate(f%rho_edge(nz,ne1), f%v(nz,ne1), f%vorticity(nz,nv1), f%divergence(nz,nc1), f%ke(nz,nc1), f%pv_vertex(nz,nv1))
 allocate(f%pv_edge(nz,ne1), f%ke_edge(nz,ne1), f%ke_vertex(nz,nv1))
 allocate(f%ru_p(nz,ne1), f%rw_p(nz+1,nc1), f%rho_pp(nz,nc1), f%rtheta_pp(nz,nc1), f%rtheta_pp_old(nz,nc1))
 allocate(f%ru_avg(nz,ne1), f%ww_avg(nz+1,nc1), f%ru_avg_split(nz,ne1), f%ww_avg_split(nz+1,nc1))
 allocate(f%tend_u(nz,ne1), f%tend_w(nz+1,nc1), f%tend_theta(nz,nc1), f%tend_rho(nz,nc1), f%tend_u_euler(nz,ne1))
 allocate(f%tend_w_euler(nz+1,nc1), f%tend_theta_euler(nz,nc1), f%rthdynten(nz,nc1), f%eddy_visc_horz(nz,nc1))
 allocate(f%h_divergence(nz,nc1), f%dpdz(nz,nc1), f%delsq_u(nz,ne1), f%delsq_vorticity(nz,nv1), f%delsq_divergence(nz,nc1))
 allocate(f%delsq_w(nz,nc1), f%delsq_theta(nz,nc1), f%tend_w_pert(nz+1,nc1))
 allocate(f%cqu(nz,ne1), f%cqw(nz,nc1), f%qtot(nz,nc1))
 allocate(f%cofwr(nz,nc1), f%cofwz(nz,nc1), f%coftz(nz+1,nc1), f%cofwt(nz,nc1), f%a_tri(nz,nc1), f%alpha_tri(nz,nc1))
 allocate(f%gamma_tri(nz,nc1), f%cofrz(nz))
 allocate(f%tend_ru_physics(nz,ne1), f%tend_rtheta_physics(nz,nc1), f%tend_rho_physics(nz,nc1))
 allocate(f%tend_scalars(num_scalars,nz,nc1), f%rqvdynten(nz,nc1))
 allocate(f%horiz_flux(num_scalars,nz,ne1), f%rho_zz_int(nz,nc1), f%sc_old(nz,nc1), f%sc_new(nz,nc1), f%s_max(nz,nc1))
 allocate(f%s_min(nz,nc1), f%wdtn(nz+1,nc1), f%scale_arr(nz,2,nc1), f%flux(nz,ne1), f%flux_upwind(nz,ne1), f%flux_tmp(nz,ne1))
 allocate(f%u_zonal(nz,nc1), f%u_meridional(nz,nc1), f%rt_diabatic_tend(nz,nc1))
 ! every field starts as zero: ghost columns and entries the original never writes read as 0
 f%u1 = 0.0
 f%u2 = 0.0
 f%w1 = 0.0
 f%w2 = 0.0
 f%theta_m1 = 0.0
 f%theta_m2 = 0.0
 f%rho_zz1 = 0.0
 f%rho_zz2 = 0.0
 f%scalars1 = 0.0
 f%scalars2 = 0.0
 f%ru = 0.0
 f%rw = 0.0
 f%rho_p = 0.0
 f%rtheta_p = 0.0
 f%exner = 0.0
 f%pressure_p = 0.0
 f%ru_save = 0.0
 f%rw_save = 0.0
 f%rho_p_save = 0.0
 f%rtheta_p_save = 0.0
 f%rho_zz_old_split = 0.0
 f%rho_edge = 0.0
 f%v = 0.0
 f%vorticity = 0.0
 f%divergence = 0.0
 f%ke = 0.0
 f%pv_vertex = 0.0
 f%pv_edge = 0.0
 f%ke_edge = 0.0
 f%ke_vertex = 0.0
 f%ru_p = 0.0
 f%rw_p = 0.0
 f%rho_pp = 0.0
 f%rtheta_pp = 0.0
 f%rtheta_pp_old = 0.0
 f%ru_avg = 0.0
 f%ww_avg = 0.0
 f%ru_avg_split = 0.0
 f%ww_avg_split = 0.0
 f%tend_u = 0.0
 f%tend_w = 0.0
 f%tend_theta = 0.0
 f%tend_rho = 0.0
 f%tend_u_euler = 0.0
 f%tend_w_euler = 0.0
 f%tend_theta_euler = 0.0
 f%rthdynten = 0.0
 f%eddy_visc_horz = 0.0
 f%h_divergence = 0.0
 f%dpdz = 0.0
 f%delsq_u = 0.0
 f%delsq_vorticity = 0.0
 f%delsq_divergence = 0.0
 f%delsq_w = 0.0
 f%delsq_theta = 0.0
 f%tend_w_pert = 0.0
 f%cqu = 0.0
 f%cqw = 0.0
 f%qtot = 0.0
 f%cofwr = 0.0
 f%cofwz = 0.0
 f%coftz = 0.0
 f%cofwt = 0.0
 f%a_tri = 0.0
 f%alpha_tri = 0.0
 f%gamma_tri = 0.0
 f%cofrz = 0.0
 f%tend_ru_physics = 0.0
 f%tend_rtheta_physics = 0.0
 f%tend_rho_physics = 0.0
 f%tend_scalars = 0.0
 f%rqvdynten = 0.0
 f%horiz_flux = 0.0
 f%rho_zz_int = 0.0
 f%sc_old = 0.0
 f%sc_new = 0.0
 f%s_max = 0.0
 f%s_min = 0.0
 f%wdtn = 0.0
 f%scale_arr = 0.0
 f%flux = 0.0
 f%flux_upwind = 0.0
 f%flux_tmp = 0.0
 f%u_zonal = 0.0
 f%u_meridional = 0.0
 f%rt_diabatic_tend = 0.0
 ! the APVM work array, only when config_apvm_upwinding > 0 (dyn_diagnostics.f90); not a field of the state, so not in the restart
 call apvm_work_allocate()
 end subroutine

 ! The host waits for the dynamics queue (model_timing's drain_queue in the card build).
 subroutine wait_dynamics_queue()
 !$acc wait(1)
 end subroutine

 ! End of a step: the working level becomes level 1 (mpas_pool_shift_time_levels); rk_setup then rebuilds level 2 from it.
 subroutine shift_time_levels(f)
 type(dyn_fields), intent(inout) :: f
 real(kind=8) :: clock_t
 integer :: c, e, k, s
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do e = 1, ne1
  do k = 1, nz
   f%u1(k,e) = f%u2(k,e)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do c = 1, nc1
  !$acc loop vector
  do k = 1, nz+1
   f%w1(k,c) = f%w2(k,c)
  end do
  !$acc loop vector
  do k = 1, nz
   f%theta_m1(k,c) = f%theta_m2(k,c)
   f%rho_zz1(k,c) = f%rho_zz2(k,c)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, nc1
  do k = 1, nz
   do s = 1, num_scalars
    f%scalars1(s,k,c) = f%scalars2(s,k,c)
   end do
  end do
 end do
#ifdef _OPENACC
 call timer_start(clock_t)
 if (host_inputs) then
 !$acc wait(1)
 !$acc update self(f%w1, f%theta_m1, f%rho_zz1, f%scalars1)
 end if
 call timer_stop(transfer_timer,clock_t)
#endif
 end subroutine

 ! atm_rk_integration_setup: the working level starts as a copy of level 1, the saved fields are the start values.
 subroutine rk_setup(f)
 type(dyn_fields), intent(inout) :: f
 integer :: c, e, k, s
 !$omp single
 !$acc parallel loop gang vector default(present) firstprivate(nc1) async(1)
 do k = 1, nz
  f%theta_m2(k,nc1) = 0.0
 end do
 !$omp end single
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do e = 1, nedges
  do k = 1, nz
   f%ru_save(k,e) = f%ru(k,e)
   f%u2(k,e) = f%u1(k,e)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   f%rtheta_p_save(k,c) = f%rtheta_p(k,c)
   f%rho_p_save(k,c) = f%rho_p(k,c)
   f%theta_m2(k,c) = f%theta_m1(k,c)
   f%rho_zz2(k,c) = f%rho_zz1(k,c)
   f%rho_zz_old_split(k,c) = f%rho_zz1(k,c)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz+1
   f%rw_save(k,c) = f%rw(k,c)
   f%w2(k,c) = f%w1(k,c)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   do s = 1, num_scalars
    f%scalars2(s,k,c) = f%scalars1(s,k,c)
   end do
  end do
 end do
 end subroutine

 ! atm_rk_dynamics_substep_finish: level 2 becomes the start of the next substep; the mass fluxes of each substep are
 ! summed, and after the last one averaged (they drive the scalar transport); density returns to its start-of-step value
 ! so that the transport sees old and new density.
 subroutine substep_finish(f, substep)
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: substep
 integer :: c, e, k
 real, parameter :: inv_split = 1.0 / real(3)
 !$omp single
 !$acc parallel loop gang vector default(present) firstprivate(nc1) async(1)
 do k = 1, nz
  f%theta_m1(k,nc1) = 0.0
 end do
 !$omp end single
 if (substep < 3) then
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do e = 1, nedges
   do k = 1, nz
    f%ru_save(k,e) = f%ru(k,e)
    f%u1(k,e) = f%u2(k,e)
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz
    f%rtheta_p_save(k,c) = f%rtheta_p(k,c)
    f%rho_p_save(k,c) = f%rho_p(k,c)
    f%theta_m1(k,c) = f%theta_m2(k,c)
    f%rho_zz1(k,c) = f%rho_zz2(k,c)
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz+1
    f%rw_save(k,c) = f%rw(k,c)
    f%w1(k,c) = f%w2(k,c)
   end do
  end do
 end if
 if (substep == 1) then
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do e = 1, nedges
   do k = 1, nz
    f%ru_avg_split(k,e) = f%ru_avg(k,e)
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz+1
    f%ww_avg_split(k,c) = f%ww_avg(k,c)
   end do
  end do
 else
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do e = 1, nedges
   do k = 1, nz
    f%ru_avg_split(k,e) = f%ru_avg(k,e) + f%ru_avg_split(k,e)
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz+1
    f%ww_avg_split(k,c) = f%ww_avg(k,c) + f%ww_avg_split(k,c)
   end do
  end do
 end if
 if (substep == 3) then
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do e = 1, nedges
   do k = 1, nz
    f%ru_avg(k,e) = f%ru_avg_split(k,e) * inv_split
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz+1
    f%ww_avg(k,c) = f%ww_avg_split(k,c) * inv_split
   end do
  end do
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz
    f%rho_zz1(k,c) = f%rho_zz_old_split(k,c)
   end do
  end do
 end if
 end subroutine

 ! Coefficients of the vertically implicit solve, from the state at the start of a substep.
 subroutine substep_start(g, f)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 call vert_imp_coefs(rk_sub_timestep(1), g%m%zz, f%cqw, f%qtot, f%exner, f%theta_m2, g%s%rho_base, g%s%rtheta_base, &
      g%s%exner_base, f%rtheta_p, g%m%rdzw, g%m%fzm, g%m%fzp, g%m%rdzu, g%etp, g%ewp, &
      f%cofwr, f%cofwz, f%coftz, f%cofwt, f%a_tri, f%alpha_tri, f%gamma_tri, f%cofrz)
 end subroutine

 ! Large-step tendencies of Runge-Kutta stage rk from the working level.
 subroutine stage_tendencies(g, f, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: rk
 ! the reference wind of the Coriolis term (u_init, v_init) is zero in real-data starts: u_tendency then skips the loop whose terms
 ! are all zero (q starts at +0 and is never -0, so subtracting a zero leaves it unchanged, bit for bit)
#ifdef _OPENACC
 ! compute_dyn_tend's work arrays live only inside the call, so on the card they are views of edge_work (each written before it
 ! is read; their ghost columns zero): edge fields 2, 3 its two edge work arrays; field 1 eddy_visc_horz and h_divergence (cell
 ! arrays); field 4 delsq_u; field 5 delsq_vorticity; field 6 delsq_divergence, delsq_w; field 7 delsq_theta (memory the card
 ! would otherwise hold for the whole run).
 ! dpdz is set at stage 1 only and read at every stage: it stays a field.
 call edge_work_ghosts_tend()
 call compute_dyn_tend(rk, any(g%m%u_init(1:nz) /= 0.) .or. any(g%m%v_init(1:nz) /= 0.), &
      g%m%dvEdge, g%m%dcEdge, g%inv_dc, g%inv_dv, g%inv_area_cell, g%inv_area_tri, g%del2, g%del4, g%m%fEdge, g%cos_edge, g%sin_edge, &
      g%m%weightsOnEdge, g%m%nEdgesOnEdge, g%m%edgesOnEdge, g%m%cellsOnEdge, g%m%verticesOnEdge, g%m%edgesOnCell, &
      g%m%nEdgesOnCell, g%sign_cell, g%m%edgesOnVertex, g%sign_vertex, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, &
      g%m%rdzu, g%m%rdzw, g%m%fzm, g%m%fzp, g%m%zz, g%m%zxu, g%dc2, g%ds2, g%dcs, g%m%u_init, g%m%v_init, &
      f%rho_zz2, f%u2, f%w2, f%theta_m2, f%ru, f%rw, f%rho_p, f%pressure_p, g%s%rho_base, f%cqu, f%cqw, f%qtot, &
      f%rt_diabatic_tend, f%rho_edge, f%v, f%divergence, f%vorticity, f%ke, f%pv_edge, f%theta_m1, f%ru_save, f%rw_save, &
      f%rho_p_save, f%tend_ru_physics, f%tend_rtheta_physics, f%tend_rho_physics, &
      f%tend_u, f%tend_w, f%tend_theta, f%tend_rho, f%tend_u_euler, f%tend_w_euler, f%tend_theta_euler, f%rthdynten, &
      edge_work(1), edge_work(nz*nc1+1), f%dpdz, edge_work(3*nz*ne1+1), edge_work(4*nz*ne1+1), edge_work(5*nz*ne1+1), &
      edge_work(5*nz*ne1+nz*nc1+1), edge_work(6*nz*ne1+1), edge_work(2*nz*ne1+1), edge_work(nz*ne1+1))   ! edge_work fields 3 and 2: edge_flux_w and edge_flux_theta
#else
 call compute_dyn_tend(rk, any(g%m%u_init(1:nz) /= 0.) .or. any(g%m%v_init(1:nz) /= 0.), &
      g%m%dvEdge, g%m%dcEdge, g%inv_dc, g%inv_dv, g%inv_area_cell, g%inv_area_tri, g%del2, g%del4, g%m%fEdge, g%cos_edge, g%sin_edge, &
      g%m%weightsOnEdge, g%m%nEdgesOnEdge, g%m%edgesOnEdge, g%m%cellsOnEdge, g%m%verticesOnEdge, g%m%edgesOnCell, &
      g%m%nEdgesOnCell, g%sign_cell, g%m%edgesOnVertex, g%sign_vertex, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, &
      g%m%rdzu, g%m%rdzw, g%m%fzm, g%m%fzp, g%m%zz, g%m%zxu, g%dc2, g%ds2, g%dcs, g%m%u_init, g%m%v_init, &
      f%rho_zz2, f%u2, f%w2, f%theta_m2, f%ru, f%rw, f%rho_p, f%pressure_p, g%s%rho_base, f%cqu, f%cqw, f%qtot, &
      f%rt_diabatic_tend, f%rho_edge, f%v, f%divergence, f%vorticity, f%ke, f%pv_edge, f%theta_m1, f%ru_save, f%rw_save, &
      f%rho_p_save, f%tend_ru_physics, f%tend_rtheta_physics, f%tend_rho_physics, &
      f%tend_u, f%tend_w, f%tend_theta, f%tend_rho, f%tend_u_euler, f%tend_w_euler, f%tend_theta_euler, f%rthdynten, &
      f%eddy_visc_horz, f%h_divergence, f%dpdz, f%delsq_u, f%delsq_vorticity, f%delsq_divergence, f%delsq_w, f%delsq_theta, &
      f%flux_tmp, f%flux_upwind)   ! two edge work arrays of the monotonic transport (it rewrites them completely before reading)
#endif
 end subroutine

 ! Perturbation form of the w tendency, into tend_w_pert (tend_w keeps the large-step tendency).
 subroutine stage_pert_variables(g, f)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer :: c, k
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, nc1
  do k = 1, nz+1
   f%tend_w_pert(k,c) = f%tend_w(k,c)
  end do
 end do
 call set_smlstep_pert_variables(g%m%bdyMaskCell, g%m%nEdgesOnCell, g%m%edgesOnCell, g%sign_cell, g%m%fzm, g%m%fzp, &
      g%zb_cell, g%zb3_cell, g%m%zz, f%tend_u, f%tend_w_pert)
 end subroutine

 ! One acoustic step followed by divergence damping.
 subroutine stage_acoustic_step(g, f, rk, small_step)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: rk, small_step
 call advance_acoustic_step(small_step, small_step > 1, rk_sub_timestep(rk), f%rho_zz2, f%theta_m1, f%w2, f%rw, f%rw_save, f%exner, f%cqu, &
      g%m%zz, g%m%zxu, g%m%fzm, g%m%fzp, g%m%rdzw, g%inv_dc, g%inv_area_cell, g%m%dvEdge, g%m%nEdgesOnCell, g%m%cellsOnEdge, &
      g%m%edgesOnCell, g%sign_cell, g%spec_edge, g%spec_cell, g%dss, g%etp, g%etm, g%ewp, g%ewm, f%cofwt, f%coftz, f%cofrz, &
      f%cofwr, f%cofwz, f%a_tri, f%alpha_tri, f%gamma_tri, f%tend_u, f%tend_rho, f%tend_theta, f%tend_w_pert, &
      f%ru_p, f%rw_p, f%rtheta_pp, f%rtheta_pp_old, f%rho_pp, f%ru_avg, f%ww_avg, edge_work(1), edge_work(nz*ne1+1), &
      edge_work(2*nz*ne1+1))
 ! the damping of the steps before the last of the stage is applied by the next step's first kernel (damp_first)
 if (small_step == number_sub_steps(rk)) &
  call divergence_damping(rk_sub_timestep(rk), f%theta_m1, f%rtheta_pp, f%rtheta_pp_old, g%m%cellsOnEdge, g%spec_edge, f%ru_p, &
                          edge_work(1))
 end subroutine

 ! Back to the large-step variables (stage 3 also removes the diabatic heating from rtheta_p and rebuilds Exner and the pressure
 ! perturbation).
 subroutine stage_recover(g, f, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: rk
 call recover_large_step_variables(rk, number_sub_steps(rk), rk_timestep(rk), g%m%bdyMaskCell, g%m%nEdgesOnCell, &
      g%m%edgesOnCell, g%m%cellsOnEdge, g%sign_cell, g%m%fzm, g%m%fzp, g%m%cf1, g%m%cf2, g%m%cf3, g%zb_cell, g%zb3_cell, &
      g%m%zz, g%s%rho_base, g%s%rtheta_base, g%s%exner_base, f%rt_diabatic_tend, f%rho_p_save, f%rtheta_p_save, &
      f%rw_save, f%ru_save, f%rho_pp, f%rtheta_pp, f%rw_p, f%ru_p, f%rho_p, f%rho_zz2, f%rw, f%w2, f%rtheta_p, f%theta_m2, &
      f%exner, f%pressure_p, f%ru, f%u2, f%ww_avg, f%ru_avg)
 end subroutine

 subroutine stage_diagnostics(g, f, compute_v)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 logical, intent(in) :: compute_v
 call solve_diagnostics(compute_v, f%rho_zz2, f%u2, g%m%cellsOnEdge, g%m%dcEdge, g%m%dvEdge, g%m%edgesOnVertex, g%sign_vertex, &
      g%inv_area_tri, g%m%edgesOnCell, g%sign_cell, g%m%nEdgesOnCell, g%inv_area_cell, g%m%verticesOnCell, g%kite, &
      g%m%kiteAreasOnVertex, g%m%edgesOnEdge, g%m%nEdgesOnEdge, g%m%weightsOnEdge, g%m%verticesOnEdge, g%m%fVertex, &
      f%rho_edge, f%v, f%vorticity, f%divergence, f%ke, f%pv_vertex, f%pv_edge, f%ke_edge, f%ke_vertex)
 ! upwinding of pv_edge, after it (mpas_atm_time_integration.F:7372); dt is the whole step's, as atm_srk3 passes it (:1872,2556)
 if (apvm_upwinding > 0.) call apvm_upwind(dt, f%u2, f%v, g%m%cellsOnEdge, g%m%verticesOnEdge, g%m%verticesOnCell, g%kite, &
      g%m%kiteAreasOnVertex, g%m%nEdgesOnCell, g%inv_area_cell, g%inv_dv, g%inv_dc, f%pv_vertex, pv_cell, f%pv_edge)
 end subroutine

 ! Transport stage rk (1, 2: non-monotonic; 3: monotonic) of all scalars with the substep-averaged mass fluxes.
 subroutine transport_stage(g, f, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: rk
 if (rk < 3) then
#ifdef _OPENACC
  call edge_work_ghosts(0)   ! its horiz_flux: the ghost edge zero
  call advance_scalars(rk, f%scalars1, f%scalars2, f%rho_zz1, f%rho_zz2, f%ru_avg, f%ww_avg, f%tend_scalars, &
       g%m%bdyMaskCell, g%m%bdyMaskEdge, g%m%cellsOnEdge, g%m%edgesOnCell, g%m%nEdgesOnCell, g%sign_cell, g%inv_area_cell, &
       g%m%dvEdge, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, g%m%fzm, g%m%fzp, g%m%rdzw, edge_work)
#else
  call advance_scalars(rk, f%scalars1, f%scalars2, f%rho_zz1, f%rho_zz2, f%ru_avg, f%ww_avg, f%tend_scalars, &
       g%m%bdyMaskCell, g%m%bdyMaskEdge, g%m%cellsOnEdge, g%m%edgesOnCell, g%m%nEdgesOnCell, g%sign_cell, g%inv_area_cell, &
       g%m%dvEdge, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, g%m%fzm, g%m%fzp, g%m%rdzw, f%horiz_flux)
#endif
 else
#ifdef _OPENACC
  ! (its work arrays are views of edge_work that mono_views sets up and zeroes the ghosts of)
  call advance_scalars_mono(f%scalars1, f%scalars2, f%tend_scalars, f%rho_zz1, f%ru_avg, f%ww_avg, &
       g%m%bdyMaskCell, g%m%bdyMaskEdge, g%m%cellsOnEdge, g%m%cellsOnCell, g%m%edgesOnCell, g%m%nEdgesOnCell, g%sign_cell, &
       g%inv_area_cell, g%m%dvEdge, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, g%m%fzm, g%m%fzp, g%m%rdzw, &
       f%rho_zz_int, f%sc_old, f%sc_new, f%s_max, f%s_min, f%wdtn, f%scale_arr, edge_work(1), edge_work(nz*ne1+1), edge_work(2*nz*ne1+1))
#else
  call advance_scalars_mono(f%scalars1, f%scalars2, f%tend_scalars, f%rho_zz1, f%ru_avg, f%ww_avg, &
       g%m%bdyMaskCell, g%m%bdyMaskEdge, g%m%cellsOnEdge, g%m%cellsOnCell, g%m%edgesOnCell, g%m%nEdgesOnCell, g%sign_cell, &
       g%inv_area_cell, g%m%dvEdge, g%n_adv, g%adv_cells, g%adv_coefs, g%adv_coefs_3rd, g%m%fzm, g%m%fzp, g%m%rdzw, &
       f%rho_zz_int, f%sc_old, f%sc_new, f%s_max, f%s_min, f%wdtn, f%scale_arr, f%flux, f%flux_upwind, f%flux_tmp)
#endif
 end if
 end subroutine

 ! Cell winds from the new edge wind, then the Grell-Freitas dynamics forcing and clipping of negative scalars.
 subroutine after_transport(g, f)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
#ifdef _OPENACC
 ! card: sin/cos of the cell coordinates from the host table (the card's own sin/cos differ in the last bit)
 call reconstruct_winds_trig(f%u2, g%m%coeffs_reconstruct, g%m%edgesOnCell, g%m%nEdgesOnCell, g%cos_lat, g%sin_lat, &
      g%cos_lon, g%sin_lon, f%u_zonal, f%u_meridional)
#else
 ! host: the original form (with gfortran on macOS the tabulated sin/cos are not bit-identical to the in-loop ones)
 call reconstruct_winds(f%u2, g%m%coeffs_reconstruct, g%m%edgesOnCell, g%m%nEdgesOnCell, g%m%latCell, g%m%lonCell, &
      f%u_zonal, f%u_meridional)
#endif
 ! rqvdynten and the moisture correction of rthdynten exist only for a convection scheme that reads them (Grell-Freitas, ntiedtke:
 ! mpas_atm_time_integration.F:2760-2796); without one MPAS's rthdynten stays the raw dynamics tendency of :6936 and rqvdynten 0
 if (conv_on) call gf_dynamics_forcing(f%scalars1, f%scalars2, f%theta_m2, f%rqvdynten, f%rthdynten)
 call clip_negative_scalars(f%scalars2)
 end subroutine

 ! ---- lateral boundary operators (only with g%lbc) ----
 ! Boundary tendencies in the specified zone, damping and filter toward the boundary values in the relaxation zone (after the perturbation form
 ! of the w tendency; the acoustic steps read tend_w_pert).
 subroutine stage_boundary_tendencies(g, f, substep, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: substep, rk
 real :: t
 call speczone_tend(g%m%bdyMaskCell, g%m%bdyMaskEdge, g%lbc%tend_ru, g%lbc%tend_rtheta_m, g%lbc%tend_rho_zz, &
      f%tend_u, f%tend_theta, f%tend_rho, f%tend_w_pert, f%rt_diabatic_tend)
 t = dt_dynamics*real(substep-1) + rk_timestep(rk)
 call bdy_values_2d(g%lbc, g%lbc%ru, g%lbc%tend_ru, t, g%lbc%drv_ru, .true.)
 call bdy_values_2d(g%lbc, g%lbc%rtheta_m, g%lbc%tend_rtheta_m, t, g%lbc%drv_rtheta_m, .false.)
 call bdy_values_2d(g%lbc, g%lbc%rho_zz, g%lbc%tend_rho_zz, t, g%lbc%drv_rho_zz, .false.)
 call relaxzone_tend(g%m%bdyMaskCell, g%m%bdyMaskEdge, g%reg_cell, g%reg_edge, g%m%dcEdge, g%m%dvEdge, g%inv_dc, g%inv_dv, &
      g%inv_area_cell, g%inv_area_tri, g%sign_cell, g%sign_vertex, g%m%cellsOnEdge, g%m%edgesOnCell, g%m%edgesOnVertex, &
      g%m%nEdgesOnCell, g%m%verticesOnEdge, f%ru, f%theta_m2, f%rho_zz2, g%lbc%drv_ru, g%lbc%drv_rtheta_m, g%lbc%drv_rho_zz, &
      f%tend_u, f%tend_theta, f%tend_rho)
 end subroutine

 ! After the recovery of a stage: u and ru of the specified zone from the boundary data.
 subroutine stage_boundary_winds(g, f, substep, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: substep, rk
 real :: t
 t = dt_dynamics*real(substep-1) + rk_timestep(rk)
 call bdy_values_2d(g%lbc, g%lbc%u, g%lbc%tend_u, t, g%lbc%drv_u, .true.)
 call bdy_values_2d(g%lbc, g%lbc%ru, g%lbc%tend_ru, t, g%lbc%drv_ru, .true.)
 call reset_edge_winds(g%m%bdyMaskEdge, g%lbc%drv_u, g%lbc%drv_ru, f%u2, f%ru)
 end subroutine

 ! After a transport stage (time step of that stage): relaxation and specified zone of all scalars.
 subroutine stage_boundary_scalars(g, f, rk)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 integer, intent(in) :: rk
 call bdy_values_3d(g%lbc, g%lbc%scalars, g%lbc%tend_scalars, rk_scalar_timestep(rk), g%lbc%drv_scalars)
 ! its work array (relaxation values) in edge_work, free after the transport stage (num_scalars*nz*ne1 >= num_scalars*nz*nc1)
 call edge_work_ghosts(-1)
 call adjust_scalars(rk_scalar_timestep(rk), g%m%bdyMaskCell, g%reg_cell, g%m%nEdgesOnCell, g%m%edgesOnCell, g%sign_cell, &
      g%m%cellsOnEdge, g%m%dvEdge, g%inv_dc, g%lbc%drv_scalars, f%scalars2, edge_work)
 end subroutine

 ! After Thompson, the end of the step: theta_m, rtheta_p and the scalars of the specified zone from the boundary values at the end of the step.
 subroutine end_of_step_boundary(g, f)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 call bdy_values_2d(g%lbc, g%lbc%rtheta_m, g%lbc%tend_rtheta_m, dt, g%lbc%drv_rtheta_m, .false.)
 call bdy_values_2d(g%lbc, g%lbc%rho_zz, g%lbc%tend_rho_zz, dt, g%lbc%drv_rho_zz, .false.)
 call reset_speczone_values(g%m%bdyMaskCell, g%lbc%drv_rtheta_m, g%lbc%drv_rho_zz, g%s%rtheta_base, f%theta_m2, f%rtheta_p)
 call bdy_values_3d(g%lbc, g%lbc%scalars, g%lbc%tend_scalars, dt, g%lbc%drv_scalars)
 call set_scalars(g%m%bdyMaskCell, g%lbc%drv_scalars, f%scalars2)
 !$acc wait(1)
 end subroutine

 ! The whole step, in the order of atm_srk3 (physics tendencies already in f; Thompson afterwards, by the caller).
 subroutine dynamics_step(g, f)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 real(kind=8) :: clock_t
 integer :: substep, rk, small_step
 call timer_start(clock_t)
 call detail_start(d_copies)
 call rk_setup(f)
 call detail_stop(d_copies)
 call detail_start(d_moist)
 call moist_coefficients(f%scalars2, g%m%cellsOnEdge, f%qtot, f%cqw, f%cqu)
 call detail_stop(d_moist)
 do substep = 1, 3
  call detail_start(d_coefs)
  call substep_start(g, f)
  call detail_stop(d_coefs)
  do rk = 1, 3
   call detail_start(d_tend)
   call stage_tendencies(g, f, rk)
   call detail_stop(d_tend)
   call detail_start(d_pert)
   call stage_pert_variables(g, f)
   call detail_stop(d_pert)
   call timer_stop(dynamics_timer,clock_t)
   call timer_start(clock_t)
   if (associated(g%lbc)) call stage_boundary_tendencies(g, f, substep, rk)
   call timer_stop(boundary_timer,clock_t)
   call timer_start(clock_t)
   ! the per-edge sums of the stage's acoustic steps, in three edge fields of edge_work (free between compute_dyn_tend and the
   ! transport)
   call edge_work_ghosts(-1)
   call acoustic_edge_sums(f%theta_m1, f%exner, g%m%zz, g%m%cellsOnEdge, edge_work(1), edge_work(nz*ne1+1), edge_work(2*nz*ne1+1))
   do small_step = 1, number_sub_steps(rk)
    call detail_start(d_acoustic)
    call stage_acoustic_step(g, f, rk, small_step)
    call detail_stop(d_acoustic)
   end do
   call detail_start(d_recover)
   call stage_recover(g, f, rk)
   call detail_stop(d_recover)
   call timer_stop(dynamics_timer,clock_t)
   call timer_start(clock_t)
   if (associated(g%lbc)) call stage_boundary_winds(g, f, substep, rk)
   call timer_stop(boundary_timer,clock_t)
   call timer_start(clock_t)
   call detail_start(d_diag)
   call stage_diagnostics(g, f, rk == 3)
   call detail_stop(d_diag)
   call timer_stop(dynamics_timer,clock_t)
   call timer_start(clock_t)
   if (associated(g%lbc)) call zero_gradient_w(g%m%bdyMaskCell, f%w2)
   call timer_stop(boundary_timer,clock_t)
   call timer_start(clock_t)
  end do
  call detail_start(d_copies)
  call substep_finish(f, substep)
  call detail_stop(d_copies)
 end do
 call timer_stop(dynamics_timer,clock_t)
 do rk = 1, 3
  call timer_start(clock_t)
  call detail_start(merge(d_transport, d_mono, rk < 3))
  call transport_stage(g, f, rk)
  call detail_stop(merge(d_transport, d_mono, rk < 3))
  call timer_stop(scalars_timer,clock_t)
  call timer_start(clock_t)
  if (associated(g%lbc)) call stage_boundary_scalars(g, f, rk)
  call timer_stop(boundary_timer,clock_t)
 end do
 call timer_start(clock_t)
 call detail_start(d_after)
 call after_transport(g, f)
 call detail_stop(d_after)
 ! the dynamics kernels run on queue 1 without a host wait between them; the physics and Thompson kernels that follow run on
 ! the synchronous queue, so the queue is drained here (and the dynamics timer then holds the time the card spent)
 !$acc wait(1)
 call timer_stop(dynamics_timer,clock_t)
 end subroutine
end module
