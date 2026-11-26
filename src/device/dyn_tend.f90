! HAND-WRITTEN card version of src/dyn_tend.f90 (not generated; ./lean generate --check does not see it). The card build
! compiles it in place of src/dyn_tend.f90 (config/units.json device_overrides); a change to a dynamics kernel is made in both files.
! Large-step tendencies of the split-explicit Runge-Kutta scheme (atm_compute_dyn_tend_work):
! u, w, theta_m and density, for one Runge-Kutta stage. Same arithmetic and summation order as MPAS 8.4.2
! ("the original" below), for the configuration lean_mpas supports: 2-D Smagorinsky horizontal mixing with 4th-order
! hyperviscosity, no LES, no vertical mixing, no Rayleigh/CAM damping, no planar curvature terms.
! Stage 1 also builds the "Euler" parts (pressure gradient, buoyancy, mixing) that stages 2 and 3 reuse.
! Arrays follow the original layout; the last cell/edge/vertex slot is the permanent zero column.
module dyn_tend
 use config
 use dyn_config
 implicit none
contains

 ! cos and sin of the angle of every edge (ghost edge included), once at startup, so that u_tendency does not evaluate them
 ! for each of the up to ten neighbouring edges of every edge at every Runge-Kutta stage.
 subroutine edge_trig(angle_edge, cos_edge, sin_edge)
 real, intent(in) :: angle_edge(ne1)
 real, intent(out) :: cos_edge(ne1), sin_edge(ne1)
 integer :: e
 do e = 1, ne1
  cos_edge(e) = cos(angle_edge(e))
  sin_edge(e) = sin(angle_edge(e))
 end do
 end subroutine

 subroutine compute_dyn_tend(rk_step, reference_wind, &
   ! mesh
   dv_edge, dc_edge, inv_dc_edge, inv_dv_edge, inv_area_cell, inv_area_triangle, scaling_del2, scaling_del4, f_edge, cos_edge, sin_edge, &
   weights_on_edge, degree_edge, edges_on_edge, cells_on_edge, vertices_on_edge, edges_on_cell, degree, sign_cell, &
   edges_on_vertex, sign_vertex, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, rdzu, rdzw, fzm, fzp, zz, zxu, &
   def_c2, def_s2, def_cs, u_init, v_init, &
   ! state of this stage, diagnostics of this stage, state at the start of the substep
   rho_zz, u, w, theta_m, ru, rw, rho_p, pressure_p, rho_base, cqu, cqw, qtot, rt_diabatic_tend, &
   rho_edge, v, divergence, vorticity, ke, pv_edge, theta_m_save, ru_save, rw_save, rho_p_save, &
   tend_ru_physics, tend_rtheta_physics, tend_rho_physics, &
   ! results; the Euler parts, eddy_visc_horz and tend_rho are written at stage 1 and read at stages 2 and 3
   tend_u, tend_w, tend_theta, tend_rho, tend_u_euler, tend_w_euler, tend_theta_euler, rthdynten, eddy_visc_horz, &
   h_divergence, dpdz, delsq_u, delsq_vorticity, delsq_divergence, delsq_w, delsq_theta, edge_flux_w, edge_flux_theta)
 integer, intent(in) :: rk_step
 logical, intent(in) :: reference_wind   ! u_init or v_init not zero
 real, intent(in) :: dv_edge(ne1), dc_edge(ne1), inv_dc_edge(ne1), inv_dv_edge(ne1), inv_area_cell(nc1), inv_area_triangle(nv1)
 real, intent(in) :: scaling_del2(ne1), scaling_del4(ne1), f_edge(ne1), cos_edge(ne1), sin_edge(ne1), weights_on_edge(2*maxedges,ne1)
 integer, intent(in) :: degree_edge(ne1), edges_on_edge(2*maxedges,ne1), cells_on_edge(2,ne1), vertices_on_edge(2,ne1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1), edges_on_vertex(vertex_degree,nv1)
 real, intent(in) :: sign_cell(maxedges,nc1), sign_vertex(vertex_degree,nv1)
 integer, intent(in) :: n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), rdzu(nz), rdzw(nz), fzm(nz), fzp(nz)
 real, intent(in) :: zz(nz,nc1), zxu(nz,ne1), def_c2(maxedges,nc1), def_s2(maxedges,nc1), def_cs(maxedges,nc1)
 real, intent(in) :: u_init(nz), v_init(nz)
 real, intent(in) :: rho_zz(nz,nc1), u(nz,ne1), w(nz+1,nc1), theta_m(nz,nc1), ru(nz,ne1), rw(nz+1,nc1)
 real, intent(in) :: rho_p(nz,nc1), pressure_p(nz,nc1), rho_base(nz,nc1), cqu(nz,ne1), cqw(nz,nc1), qtot(nz,nc1)
 real, intent(in) :: rt_diabatic_tend(nz,nc1), rho_edge(nz,ne1), v(nz,ne1), divergence(nz,nc1), vorticity(nz,nv1)
 real, intent(in) :: ke(nz,nc1), pv_edge(nz,ne1), theta_m_save(nz,nc1), ru_save(nz,ne1), rw_save(nz+1,nc1), rho_p_save(nz,nc1)
 real, intent(in) :: tend_ru_physics(nz,ne1), tend_rtheta_physics(nz,nc1), tend_rho_physics(nz,nc1)
 real, intent(inout) :: tend_u(nz,ne1), tend_w(nz+1,nc1), tend_theta(nz,nc1), tend_rho(nz,nc1)
 real, intent(inout) :: tend_u_euler(nz,ne1), tend_w_euler(nz+1,nc1), tend_theta_euler(nz,nc1)
 real, intent(inout) :: rthdynten(nz,nc1), eddy_visc_horz(nz,nc1)
 real, intent(inout) :: h_divergence(nz,nc1), dpdz(nz,nc1)
 real, intent(inout) :: delsq_u(nz,ne1), delsq_vorticity(nz,nv1), delsq_divergence(nz,nc1), delsq_w(nz,nc1), delsq_theta(nz,nc1)
 real, intent(inout) :: edge_flux_w(nz,ne1), edge_flux_theta(nz,ne1)   ! work: the 3rd/4th-order edge values of w and theta_m
 real :: h_mom_eddy_visc4

 if (rk_step == 1) then
  call smagorinsky_2d(u, v, def_c2, def_s2, def_cs, degree, edges_on_cell, eddy_visc_horz)
  h_mom_eddy_visc4 = visc4_2dsmag * len_disp**3
 end if
 call horizontal_divergence(ru, dv_edge, inv_area_cell, edges_on_cell, degree, sign_cell, h_divergence)
 if (rk_step == 1) call mass_tendency_and_buoyancy(rw, rdzw, rho_base, rho_p_save, qtot, h_divergence, tend_rho_physics, &
                                                   tend_rho, dpdz)
 call u_tendency(rk_step, u, ru, rw, rho_edge, ke, pv_edge, h_divergence, cqu, pressure_p, zz, zxu, dpdz, &
                 cells_on_edge, degree_edge, edges_on_edge, weights_on_edge, inv_dc_edge, cos_edge, sin_edge, f_edge, &
                 rdzw, fzm, fzp, u_init, v_init, reference_wind, tend_ru_physics, tend_u, tend_u_euler)
 if (rk_step == 1) call u_mixing(h_mom_eddy_visc4, u, divergence, vorticity, rho_edge, eddy_visc_horz, cells_on_edge, &
                                 vertices_on_edge, edges_on_cell, degree, sign_cell, edges_on_vertex, sign_vertex, &
                                 inv_area_cell, inv_area_triangle, inv_dv_edge, inv_dc_edge, dc_edge, dv_edge, scaling_del2, &
                                 scaling_del4, delsq_u, delsq_vorticity, delsq_divergence, tend_u_euler)
 ! stages 2 and 3 add the Euler and physics parts inside u_tendency (the mixing changes tend_u_euler only at stage 1)
 if (rk_step == 1) call add_to_tend_u(tend_u_euler, tend_ru_physics, tend_u)

 ! the horizontal advection of w and theta_m (and the stage 2/3 correction of theta), edge values once per edge
 call horizontal_advection_w_theta(rk_step, w, theta_m, ru, ru_save, theta_m_save, fzm, fzp, cells_on_edge, edges_on_cell, degree, &
                                   sign_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, edge_flux_w, edge_flux_theta, &
                                   tend_w, tend_theta)
 if (rk_step == 1) call w_mixing(h_mom_eddy_visc4, w, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, sign_cell, &
                                 inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_w, tend_w_euler)
 call w_vertical_advection(rk_step, w, rw, pressure_p, cqw, dpdz, rdzu, fzm, fzp, inv_area_cell, tend_w, tend_w_euler)

 ! MPAS sets h_theta_eddy_visc4 = h_mom_eddy_visc4 for 2d_smagorinsky (atm_compute_dyn_tend_work)
 if (rk_step == 1) call theta_mixing(h_mom_eddy_visc4, theta_m, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, &
                                     sign_cell, inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_theta, &
                                     tend_theta_euler)
 call theta_vertical_advection(theta_m, rw, rw_save, theta_m_save, rho_zz, rt_diabatic_tend, tend_rho, tend_rtheta_physics, &
                               tend_theta_euler, rdzw, fzm, fzp, inv_area_cell, tend_theta, rthdynten)
 end subroutine

 ! ---------------------------------------------------------------- stage-1 blocks

 ! Eddy viscosity of the horizontal mixing from the strain rate on the cell (Smagorinsky),
 ! limited so that the explicit diffusion stays stable.
 subroutine smagorinsky_2d(u, v, def_c2, def_s2, def_cs, degree, edges_on_cell, kdiff)
 real, intent(in) :: u(nz,ne1), v(nz,ne1), def_c2(maxedges,nc1), def_s2(maxedges,nc1), def_cs(maxedges,nc1)
 integer, intent(in) :: degree(nc1), edges_on_cell(maxedges,nc1)
 real, intent(inout) :: kdiff(nz,nc1)
 real :: invdt, d_11, d_22, d_12, dudx, dudy, dvdx, dvdy
 integer :: c, i, k, e
 invdt = 1.0 / dt
 ! gang = column, vector = level, the edges summed in the original order by each level (no per-column arrays)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(d_11, d_22, d_12, dudx, dudy, dvdx, dvdy, i, e) async(1)
 do c = 1, ncells
  do k = 1, nz
   dudx = 0.0
   dudy = 0.0
   dvdx = 0.0
   dvdy = 0.0
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    dudx = dudx + def_c2(i,c)*u(k,e) - def_cs(i,c)*v(k,e)
    dudy = dudy + def_cs(i,c)*u(k,e) - def_s2(i,c)*v(k,e)
    dvdx = dvdx + def_cs(i,c)*u(k,e) + def_c2(i,c)*v(k,e)
    dvdy = dvdy + def_s2(i,c)*u(k,e) + def_cs(i,c)*v(k,e)
   end do
   d_11 = 2*dudx
   d_22 = 2*dvdy
   d_12 = dudy + dvdx
   kdiff(k,c) = (smagorinsky_coef * len_disp)**2 * sqrt(0.25*(d_11-d_22)**2 + d_12**2)
   kdiff(k,c) = min(kdiff(k,c),(0.01*len_disp**2) * invdt)
  end do
 end do
 end subroutine

 ! Continuity tendency and the hydrostatic/buoyancy term dpdz (kept for the pressure-gradient force).
 subroutine mass_tendency_and_buoyancy(rw, rdzw, rho_base, rho_p_save, qtot, h_divergence, tend_rho_physics, tend_rho, dpdz)
 real, intent(in) :: rw(nz+1,nc1), rdzw(nz), rho_base(nz,nc1), rho_p_save(nz,nc1), qtot(nz,nc1), h_divergence(nz,nc1)
 real, intent(in) :: tend_rho_physics(nz,nc1)
 real, intent(inout) :: tend_rho(nz,nc1), dpdz(nz,nc1)
 integer :: c, k
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   tend_rho(k,c) = -h_divergence(k,c)-rdzw(k)*(rw(k+1,c)-rw(k,c)) + tend_rho_physics(k,c)
   dpdz(k,c) = -gravity*(rho_base(k,c)*(qtot(k,c)) + rho_p_save(k,c)*(1.+qtot(k,c)))
  end do
 end do
 end subroutine

 ! 2nd-order (Smagorinsky) and 4th-order horizontal mixing of u, added to the Euler part of tend_u.
 subroutine u_mixing(h_mom_eddy_visc4, u, divergence, vorticity, rho_edge, eddy_visc_horz, cells_on_edge, vertices_on_edge, &
                     edges_on_cell, degree, sign_cell, edges_on_vertex, sign_vertex, inv_area_cell, inv_area_triangle, &
                     inv_dv_edge, inv_dc_edge, dc_edge, dv_edge, scaling_del2, scaling_del4, &
                     delsq_u, delsq_vorticity, delsq_divergence, tend_u_euler)
 real, intent(in) :: h_mom_eddy_visc4, u(nz,ne1), divergence(nz,nc1), vorticity(nz,nv1), rho_edge(nz,ne1), eddy_visc_horz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1), vertices_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 integer, intent(in) :: edges_on_vertex(vertex_degree,nv1)
 real, intent(in) :: sign_cell(maxedges,nc1), sign_vertex(vertex_degree,nv1), inv_area_cell(nc1), inv_area_triangle(nv1)
 real, intent(in) :: inv_dv_edge(ne1), inv_dc_edge(ne1), dc_edge(ne1), dv_edge(ne1), scaling_del2(ne1), scaling_del4(ne1)
 real, intent(inout) :: delsq_u(nz,ne1), delsq_vorticity(nz,nv1), delsq_divergence(nz,nc1), tend_u_euler(nz,ne1)
 integer :: e, c, vtx, i, k, cell1, cell2, vertex1, vertex2
 real :: r_dc, r_dv, u_diffusion, kdiffu, r, edge_sign, u_mix_scale, acc
 ! one thread per (level, point) in each kernel, sums over the neighbours in the original order (in a register)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, vertex1, vertex2, r_dc, r_dv, u_diffusion, kdiffu) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   vertex1 = vertices_on_edge(1,e)
   vertex2 = vertices_on_edge(2,e)
   r_dc = inv_dc_edge(e)
   r_dv = min(inv_dv_edge(e), 4*inv_dc_edge(e))
   u_diffusion = ( divergence(k,cell2) - divergence(k,cell1) ) * r_dc - ( vorticity(k,vertex2) - vorticity(k,vertex1) ) * r_dv
   delsq_u(k,e) = 0.0 + u_diffusion
   kdiffu = 0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2))
   tend_u_euler(k,e) = tend_u_euler(k,e) + rho_edge(k,e)* kdiffu * u_diffusion * scaling_del2(e)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, edge_sign, acc) async(1)
 do vtx = 1, nvertices
  do k = 1, nz
   acc = 0.0
   do i = 1, vertex_degree
    e = edges_on_vertex(i,vtx)
    edge_sign = inv_area_triangle(vtx) * dc_edge(e) * sign_vertex(i,vtx)
    acc = acc + edge_sign * delsq_u(k,e)
   end do
   delsq_vorticity(k,vtx) = acc
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, r, edge_sign, acc) async(1)
 do c = 1, ncells
  do k = 1, nz
   acc = 0.0
   r = inv_area_cell(c)
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    edge_sign = r * dv_edge(e) * sign_cell(i,c)
    acc = acc + edge_sign * delsq_u(k,e)
   end do
   delsq_divergence(k,c) = acc
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, vertex1, vertex2, u_mix_scale, r_dc, r_dv, u_diffusion) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   vertex1 = vertices_on_edge(1,e)
   vertex2 = vertices_on_edge(2,e)
   u_mix_scale = scaling_del4(e)*h_mom_eddy_visc4
   r_dc = u_mix_scale * del4u_div_factor * inv_dc_edge(e)
   r_dv = u_mix_scale * min(inv_dv_edge(e), 4*inv_dc_edge(e))
   u_diffusion = rho_edge(k,e) * ( ( delsq_divergence(k,cell2) - delsq_divergence(k,cell1) ) * r_dc &
                                 -( delsq_vorticity(k,vertex2) - delsq_vorticity(k,vertex1) ) * r_dv )
   tend_u_euler(k,e) = tend_u_euler(k,e) - u_diffusion
  end do
 end do
 end subroutine

 ! w: horizontal mixing of w (2nd and 4th order); starts tend_w_euler.
 subroutine w_mixing(h_mom_eddy_visc4, w, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, sign_cell, &
                     inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_w, tend_w_euler)
 real, intent(in) :: h_mom_eddy_visc4, w(nz+1,nc1), rho_edge(nz,ne1), eddy_visc_horz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 real, intent(in) :: sign_cell(maxedges,nc1), inv_area_cell(nc1), inv_dc_edge(ne1), dv_edge(ne1), scaling_del2(ne1), scaling_del4(ne1)
 real, intent(inout) :: delsq_w(nz,nc1), tend_w_euler(nz+1,nc1)
 integer :: c, e, i, k, cell1, cell2
 real :: r_areacell, edge_sign, w_turb_flux, dsq, tw
 ! one thread per (level, cell), the edges in the original order, sums in registers
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, r_areacell, edge_sign, w_turb_flux, dsq, tw) async(1)
 do c = 1, ncells
  do k = 1, nz+1
   dsq = 0.0
   tw = 0.0
   if (k >= 2 .and. k <= nz) then
    r_areacell = inv_area_cell(c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     edge_sign = 0.5 * r_areacell*sign_cell(i,c) * dv_edge(e) * inv_dc_edge(e)
     cell1 = cells_on_edge(1,e)
     cell2 = cells_on_edge(2,e)
     w_turb_flux = edge_sign*(rho_edge(k,e)+rho_edge(k-1,e))*(w(k,cell2) - w(k,cell1))
     dsq = dsq + w_turb_flux
     w_turb_flux = w_turb_flux * scaling_del2(e) * 0.25 * &
                     ( eddy_visc_horz(k  ,cell1)+eddy_visc_horz(k  ,cell2) &
                      +eddy_visc_horz(k-1,cell1)+eddy_visc_horz(k-1,cell2) )
     tw = tw + w_turb_flux
    end do
   end if
   if (k <= nz) delsq_w(k,c) = dsq
   tend_w_euler(k,c) = tw
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, r_areacell, edge_sign, tw) async(1)
 do c = 1, ncells
  do k = 2, nz
   r_areacell = h_mom_eddy_visc4 * inv_area_cell(c)
   tw = tend_w_euler(k,c)
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    edge_sign = scaling_del4(e)*r_areacell*dv_edge(e)*sign_cell(i,c) * inv_dc_edge(e)
    tw = tw - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1))
   end do
   tend_w_euler(k,c) = tw
  end do
 end do
 end subroutine

 ! theta_m: horizontal mixing (2nd order with the Prandtl number, then 4th order); starts tend_theta_euler.
 subroutine theta_mixing(h_theta_eddy_visc4, theta_m, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, sign_cell, &
                         inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_theta, tend_theta_euler)
 real, intent(in) :: h_theta_eddy_visc4, theta_m(nz,nc1), rho_edge(nz,ne1), eddy_visc_horz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 real, intent(in) :: sign_cell(maxedges,nc1), inv_area_cell(nc1), inv_dc_edge(ne1), dv_edge(ne1), scaling_del2(ne1), scaling_del4(ne1)
 real, intent(inout) :: delsq_theta(nz,nc1), tend_theta_euler(nz,nc1)
 integer :: c, e, i, k, cell1, cell2
 real :: r_areacell, edge_sign, pr_scale, theta_turb_flux, prandtl_inv, dsq, tt
 prandtl_inv = 1.0 / prandtl
 ! one thread per (level, cell), the edges in the original order, sums in registers
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, r_areacell, edge_sign, pr_scale, theta_turb_flux, dsq, tt) async(1)
 do c = 1, ncells
  do k = 1, nz
   dsq = 0.0
   tt = 0.0
   r_areacell = inv_area_cell(c)
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    edge_sign = r_areacell*sign_cell(i,c) * dv_edge(e) * inv_dc_edge(e)
    pr_scale = prandtl_inv * scaling_del2(e)
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    theta_turb_flux = edge_sign*(theta_m(k,cell2) - theta_m(k,cell1))*rho_edge(k,e)
    dsq = dsq + theta_turb_flux
    theta_turb_flux = theta_turb_flux*0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2)) * pr_scale
    tt = tt + theta_turb_flux
   end do
   delsq_theta(k,c) = dsq
   tend_theta_euler(k,c) = tt
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, r_areacell, edge_sign, tt) async(1)
 do c = 1, ncells
  do k = 1, nz
   r_areacell = h_theta_eddy_visc4 * prandtl_inv * inv_area_cell(c)
   tt = tend_theta_euler(k,c)
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    edge_sign = scaling_del4(e)*r_areacell*dv_edge(e)*sign_cell(i,c)*inv_dc_edge(e)
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    tt = tt - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
   end do
   tend_theta_euler(k,c) = tt
  end do
 end do
 end subroutine

 ! ---------------------------------------------------------------- every stage

 subroutine horizontal_divergence(ru, dv_edge, inv_area_cell, edges_on_cell, degree, sign_cell, h_divergence)
 real, intent(in) :: ru(nz,ne1), dv_edge(ne1), inv_area_cell(nc1), sign_cell(maxedges,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1)
 real, intent(inout) :: h_divergence(nz,nc1)
 integer :: c, i, k, e
 real :: edge_sign, r, hd
 ! one thread per (level, cell); the sum in a register, then the division by the area
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, edge_sign, hd) async(1)
 do c = 1, ncells
  do k = 1, nz
   hd = 0.0
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    edge_sign = sign_cell(i,c) * dv_edge(e)
    hd = hd + edge_sign * ru(k,e)
   end do
   h_divergence(k,c) = hd * inv_area_cell(c)
  end do
 end do
 end subroutine

 ! u: pressure-gradient force (stage 1, into tend_u_euler), vertical transport of u by w, Coriolis/vorticity
 ! (perturbation form: valid for a constant f), kinetic-energy gradient and the divergence correction.
 subroutine u_tendency(rk_step, u, ru, rw, rho_edge, ke, pv_edge, h_divergence, cqu, pressure_p, zz, zxu, dpdz, &
                       cells_on_edge, degree_edge, edges_on_edge, weights_on_edge, inv_dc_edge, cos_edge, sin_edge, f_edge, &
                       rdzw, fzm, fzp, u_init, v_init, reference_wind, tend_ru_physics, tend_u, tend_u_euler)
 integer, intent(in) :: rk_step
 logical, intent(in) :: reference_wind
 real, intent(in) :: u(nz,ne1), ru(nz,ne1), rw(nz+1,nc1), rho_edge(nz,ne1), ke(nz,nc1), pv_edge(nz,ne1)
 real, intent(in) :: h_divergence(nz,nc1), cqu(nz,ne1), pressure_p(nz,nc1), zz(nz,nc1), zxu(nz,ne1), dpdz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1), degree_edge(ne1), edges_on_edge(2*maxedges,ne1)
 real, intent(in) :: weights_on_edge(2*maxedges,ne1), inv_dc_edge(ne1), cos_edge(ne1), sin_edge(ne1), f_edge(ne1)
 real, intent(in) :: rdzw(nz), fzm(nz), fzp(nz), u_init(nz), v_init(nz), tend_ru_physics(nz,ne1)
 real, intent(inout) :: tend_u(nz,ne1), tend_u_euler(nz,ne1)
 integer :: e, j, k, cell1, cell2, eoe
 real :: workpv, reference_u, cos_a, sin_a, q, w_low, w_up, t
 ! one thread per (level, edge), one pass: each level computes the vertical flux through its lower and upper face (wduz(k) and
 ! wduz(k+1) of the original, same expressions) and its own q, so no per-edge arrays and no barriers between the stages
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(q, w_low, w_up, t, j, eoe, workpv, reference_u, cos_a, sin_a) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   if (rk_step == 1) then
    tend_u_euler(k,e) = - cqu(k,e)*( (pressure_p(k,cell2)-pressure_p(k,cell1))*inv_dc_edge(e)/(.5*(zz(k,cell2)+zz(k,cell1))) &
                                    -0.5*zxu(k,e)*(dpdz(k,cell1)+dpdz(k,cell2)) )
   end if
   ! wduz(k)
   if (k == 1) then
    w_low = 0.
   else if (k == 2 .or. k == nz) then
    w_low = 0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,e)+fzp(k)*u(k-1,e))
   else
    w_low = flux3( u(k-2,e),u(k-1,e),u(k,e),u(k+1,e),0.5*(rw(k,cell1)+rw(k,cell2)), 1.0 )
   end if
   ! wduz(k+1)
   if (k == nz) then
    w_up = 0.
   else if (k+1 == 2 .or. k+1 == nz) then
    w_up = 0.5*( rw(k+1,cell1)+rw(k+1,cell2))*(fzm(k+1)*u(k+1,e)+fzp(k+1)*u(k,e))
   else
    w_up = flux3( u(k-1,e),u(k,e),u(k+1,e),u(k+2,e),0.5*(rw(k+1,cell1)+rw(k+1,cell2)), 1.0 )
   end if
   t = - rdzw(k)*(w_up-w_low)
   q = 0.0
   do j = 1, degree_edge(e)
    eoe = edges_on_edge(j,e)
    workpv = 0.5 * (pv_edge(k,e) + pv_edge(k,eoe))
    q = q + weights_on_edge(j,e) * u(k,eoe) * workpv
   end do
   if (reference_wind) then
    do j = 1, degree_edge(e)
     eoe = edges_on_edge(j,e)
     cos_a = cos_edge(eoe)
     sin_a = sin_edge(eoe)
     reference_u = u_init(k) * cos_a - v_init(k) * sin_a
     q = q - weights_on_edge(j,e) * reference_u * f_edge(e)
    end do
   end if
   t = t + rho_edge(k,e)* (q - (ke(k,cell2) - ke(k,cell1)) * inv_dc_edge(e)) &
         - u(k,e)*0.5*(h_divergence(k,cell1)+h_divergence(k,cell2))
   if (rk_step > 1) t = t + tend_u_euler(k,e) + tend_ru_physics(k,e)
   tend_u(k,e) = t
  end do
 end do
 end subroutine

 subroutine add_to_tend_u(tend_u_euler, tend_ru_physics, tend_u)
 real, intent(in) :: tend_u_euler(nz,ne1), tend_ru_physics(nz,ne1)
 real, intent(inout) :: tend_u(nz,ne1)
 integer :: e, k
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do e = 1, nedges
  do k = 1, nz
   tend_u(k,e) = tend_u(k,e) + tend_u_euler(k,e) + tend_ru_physics(k,e)
  end do
 end do
 end subroutine

 ! Horizontal advection of w and theta_m (w_horizontal_advection, theta_horizontal_advection and, at stages 2 and 3,
 ! theta_save_correction): the 3rd/4th-order edge value of each is computed once per edge (the cells summed it for each of their
 ! edges, so every edge twice), then each cell sums its edges in the original order. Same arithmetic, value for value.
 subroutine horizontal_advection_w_theta(rk_step, w, theta_m, ru, ru_save, theta_m_save, fzm, fzp, cells_on_edge, edges_on_cell, degree, &
                                         sign_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, flux_w, flux_theta, &
                                         tend_w, tend_theta)
 integer, intent(in) :: rk_step
 real, intent(in) :: w(nz+1,nc1), theta_m(nz,nc1), ru(nz,ne1), ru_save(nz,ne1), theta_m_save(nz,nc1), fzm(nz), fzp(nz)
 real, intent(in) :: sign_cell(maxedges,nc1), dv_edge(ne1), adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1), n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(inout) :: flux_w(nz,ne1), flux_theta(nz,ne1), tend_w(nz+1,nc1), tend_theta(nz,nc1)
 integer :: c, i, j, k, e, adv_cell, cell1, cell2
 real :: ru_edge_w, fw, ft, scalar_weight, flux, tw, tt
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(ru_edge_w, fw, ft, j, adv_cell, scalar_weight) async(1)
 do e = 1, nedges
  do k = 1, nz
   if (k > 1) then
    ru_edge_w = fzm(k)*ru(k,e) + fzp(k)*ru(k-1,e)
    fw = 0.0
    do j = 1, n_adv(e)
     adv_cell = adv_cells(j,e)
     scalar_weight = adv_coefs(j,e) + sign(1.0,ru_edge_w) * adv_coefs_3rd(j,e)
     fw = fw + scalar_weight * w(k,adv_cell)
    end do
    flux_w(k,e) = fw
   end if
   ft = 0.0
   do j = 1, n_adv(e)
    adv_cell = adv_cells(j,e)
    scalar_weight = adv_coefs(j,e) + sign(1.0,ru(k,e))*adv_coefs_3rd(j,e)
    ft = ft + scalar_weight* theta_m(k,adv_cell)
   end do
   flux_theta(k,e) = ft
  end do
 end do
 ! one thread per (level, cell): each sums the cell's edges in the original order (in a register; the original summed in tend_w and
 ! tend_theta, the same single-precision values)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, ru_edge_w, flux, tw, tt) async(1)
 do c = 1, ncells
  do k = 1, nz+1
   tw = 0.0
   if (k >= 2 .and. k <= nz) then
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     ru_edge_w = fzm(k)*ru(k,e) + fzp(k)*ru(k-1,e)
     tw = tw - sign_cell(i,c) * ru_edge_w*flux_w(k,e)
    end do
   end if
   tend_w(k,c) = tw
   if (k <= nz) then
    tt = 0.0
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     tt = tt - sign_cell(i,c) * ru(k,e) * flux_theta(k,e)
    end do
    if (rk_step > 1) then
     do i = 1, degree(c)
      e = edges_on_cell(i,c)
      cell1 = cells_on_edge(1,e)
      cell2 = cells_on_edge(2,e)
      flux = sign_cell(i,c)*dv_edge(e)*(ru_save(k,e)-ru(k,e))*0.5*(theta_m_save(k,cell2)+theta_m_save(k,cell1))
      tt = tt-flux
     end do
    end if
    tend_theta(k,c) = tt
   end if
  end do
 end do
 end subroutine

 ! w: vertical advection, the vertical pressure-gradient/buoyancy part (stage 1) and the sum with the Euler part.
 subroutine w_vertical_advection(rk_step, w, rw, pressure_p, cqw, dpdz, rdzu, fzm, fzp, inv_area_cell, tend_w, tend_w_euler)
 integer, intent(in) :: rk_step
 real, intent(in) :: w(nz+1,nc1), rw(nz+1,nc1), pressure_p(nz,nc1), cqw(nz,nc1), dpdz(nz,nc1)
 real, intent(in) :: rdzu(nz), fzm(nz), fzp(nz), inv_area_cell(nc1)
 real, intent(inout) :: tend_w(nz+1,nc1), tend_w_euler(nz+1,nc1)
 integer :: c, k
 real :: w_low, w_up
 ! gang = column, vector = level; each level computes the fluxes through its two faces (wdwz(k), wdwz(k+1) of the original), and the
 ! sum with the Euler part follows in the same pass
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(w_low, w_up) async(1)
 do c = 1, ncells
  do k = 2, nz
   if (k == 2 .or. k == nz) then
    w_low = 0.25*(rw(k,c)+rw(k-1,c))*(w(k,c)+w(k-1,c))
   else
    w_low = flux3( w(k-2,c),w(k-1,c),w(k,c),w(k+1,c),0.5*(rw(k,c)+rw(k-1,c)), 1.0 )
   end if
   if (k == nz) then
    w_up = 0.0
   else if (k+1 == nz) then
    w_up = 0.25*(rw(k+1,c)+rw(k,c))*(w(k+1,c)+w(k,c))
   else
    w_up = flux3( w(k-1,c),w(k,c),w(k+1,c),w(k+2,c),0.5*(rw(k+1,c)+rw(k,c)), 1.0 )
   end if
   tend_w(k,c) = tend_w(k,c) * inv_area_cell(c) -rdzu(k)*(w_up-w_low)
   if (rk_step == 1) then
    tend_w_euler(k,c) = tend_w_euler(k,c) - cqw(k,c)*( rdzu(k)*(pressure_p(k,c)-pressure_p(k-1,c)) &
                                                       - (fzm(k)*dpdz(k,c) + fzp(k)*dpdz(k-1,c)) )
   end if
   tend_w(k,c) = tend_w(k,c) + tend_w_euler(k,c)
  end do
 end do
 end subroutine

 ! theta_m: vertical advection, division by cell area, the dynamics-theta tendency used by the next convection call
 ! (rthdynten) and the sum with diabatic, Euler and physics parts.
 subroutine theta_vertical_advection(theta_m, rw, rw_save, theta_m_save, rho_zz, rt_diabatic_tend, tend_rho, tend_rtheta_physics, &
                                     tend_theta_euler, rdzw, fzm, fzp, inv_area_cell, tend_theta, rthdynten)
 real, intent(in) :: theta_m(nz,nc1), rw(nz+1,nc1), rw_save(nz+1,nc1), theta_m_save(nz,nc1), rho_zz(nz,nc1)
 real, intent(in) :: rt_diabatic_tend(nz,nc1), tend_rho(nz,nc1), tend_rtheta_physics(nz,nc1), tend_theta_euler(nz,nc1)
 real, intent(in) :: rdzw(nz), fzm(nz), fzp(nz), inv_area_cell(nc1)
 real, intent(inout) :: tend_theta(nz,nc1), rthdynten(nz,nc1)
 integer :: c, k
 real :: t_low, t_up
 ! gang = column, vector = level; each level computes the fluxes through its two faces (wdtz(k), wdtz(k+1) of the original), and the
 ! sum with the Euler and physics parts follows in the same pass
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(t_low, t_up) async(1)
 do c = 1, ncells
  do k = 1, nz
   ! wdtz(k)
   if (k == 1) then
    t_low = 0.0
   else if (k == 2) then
    t_low = rw(k,c)*(fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
    t_low = t_low+(rw_save(k,c)-rw(k,c))*(fzm(k)*theta_m_save(k,c)+fzp(k)*theta_m_save(k-1,c))
   else if (k == nz) then
    t_low = rw_save(k,c)*(fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
   else
    t_low = flux3( theta_m(k-2,c),theta_m(k-1,c),theta_m(k,c),theta_m(k+1,c), rw(k,c), coef_3rd_order )
    t_low = t_low + (rw_save(k,c)-rw(k,c))*(fzm(k)*theta_m_save(k,c)+fzp(k)*theta_m_save(k-1,c))
   end if
   ! wdtz(k+1)
   if (k == nz) then
    t_up = 0.0
   else if (k+1 == 2) then
    t_up = rw(k+1,c)*(fzm(k+1)*theta_m(k+1,c)+fzp(k+1)*theta_m(k,c))
    t_up = t_up+(rw_save(k+1,c)-rw(k+1,c))*(fzm(k+1)*theta_m_save(k+1,c)+fzp(k+1)*theta_m_save(k,c))
   else if (k+1 == nz) then
    t_up = rw_save(k+1,c)*(fzm(k+1)*theta_m(k+1,c)+fzp(k+1)*theta_m(k,c))
   else
    t_up = flux3( theta_m(k-1,c),theta_m(k,c),theta_m(k+1,c),theta_m(k+2,c), rw(k+1,c), coef_3rd_order )
    t_up = t_up + (rw_save(k+1,c)-rw(k+1,c))*(fzm(k+1)*theta_m_save(k+1,c)+fzp(k+1)*theta_m_save(k,c))
   end if
   tend_theta(k,c) = tend_theta(k,c)*inv_area_cell(c) -rdzw(k)*(t_up-t_low)
   rthdynten(k,c) = (tend_theta(k,c)-tend_rho(k,c)*theta_m(k,c))/rho_zz(k,c)
   tend_theta(k,c) = tend_theta(k,c) + rho_zz(k,c)*rt_diabatic_tend(k,c)
   tend_theta(k,c) = tend_theta(k,c) + tend_theta_euler(k,c) + tend_rtheta_physics(k,c)
  end do
 end do
 end subroutine

 ! Fourth-order centred flux with an optional third-order upwind correction (coef3).
 real function flux4(q_im2, q_im1, q_i, q_ip1, ua)
 !$acc routine seq
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua
 flux4 = ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0
 end function

 real function flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3)
 !$acc routine seq
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua, coef3
 flux3 = flux4(q_im2, q_im1, q_i, q_ip1, ua) + coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0
 end function
end module
