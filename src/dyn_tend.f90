! Large-step tendencies of the split-explicit Runge-Kutta scheme (atm_compute_dyn_tend_work):
! u, w, theta_m and density, for one Runge-Kutta stage. Same arithmetic and summation order as MPAS 8.4.2
! ("the original" below), for the configuration lean_mpas supports: 2-D Smagorinsky horizontal mixing with 4th-order
! hyperviscosity, no LES, no vertical mixing, no Rayleigh damping of u (config_rayleigh_damp_u) or CAM
! damping (the upper-level w damping, dss, is in the acoustic step), no planar curvature terms.
! Stage 1 also builds the "Euler" parts (pressure gradient, buoyancy, mixing) that stages 2 and 3 reuse.
! Arrays follow the original layout; the last cell/edge/vertex slot is the permanent zero column.
! The CPU's loop order (neighbour loop outside, level loop inside, sums in the arrays), with the column work of a cell or edge in
! one pass where the original had several loops (divergence and mass tendency; u and its Euler/physics sum; w and theta_m
! columns), and the advective fluxes of w and theta_m formed once per edge. The card's kernels are in src/device/dyn_tend.f90
! (one thread per (level, point), the Euler and physics sums fused into u_tendency).
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
 ! work: the advective fluxes of w and theta_m through each edge (w_theta_edge_fluxes); on the host the two edge work arrays
 ! of the monotonic transport, which rewrites them before reading
 real, intent(inout) :: edge_flux_w(nz,ne1), edge_flux_theta(nz,ne1)
 real :: h_mom_eddy_visc4

 if (rk_step == 1) then
  call smagorinsky_2d(u, v, def_c2, def_s2, def_cs, degree, edges_on_cell, eddy_visc_horz)
  h_mom_eddy_visc4 = visc4_2dsmag * len_disp**3
 end if
 call horizontal_divergence(rk_step, ru, rw, dv_edge, inv_area_cell, edges_on_cell, degree, sign_cell, rdzw, rho_base, &
                            rho_p_save, qtot, tend_rho_physics, h_divergence, tend_rho, dpdz)
 ! stages 2 and 3 add the Euler and physics parts inside u_tendency, stage 1 at the end of u_mixing (the mixing changes tend_u_euler)
 call u_tendency(rk_step, u, ru, rw, rho_edge, ke, pv_edge, h_divergence, cqu, pressure_p, zz, zxu, dpdz, &
                 cells_on_edge, degree_edge, edges_on_edge, weights_on_edge, inv_dc_edge, cos_edge, sin_edge, f_edge, &
                 rdzw, fzm, fzp, u_init, v_init, reference_wind, tend_ru_physics, tend_u, tend_u_euler)
 if (rk_step == 1) then
  call u_mixing(h_mom_eddy_visc4, u, divergence, vorticity, rho_edge, eddy_visc_horz, cells_on_edge, &
                vertices_on_edge, edges_on_cell, degree, sign_cell, edges_on_vertex, sign_vertex, &
                inv_area_cell, inv_area_triangle, inv_dv_edge, inv_dc_edge, dc_edge, dv_edge, scaling_del2, &
                scaling_del4, tend_ru_physics, delsq_u, delsq_vorticity, delsq_divergence, tend_u_euler, tend_u)
  ! the Euler parts of w and theta_m (they read only the state), before the column pass that adds them
  call w_mixing(h_mom_eddy_visc4, w, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, sign_cell, &
                inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_w, tend_w_euler)
  ! MPAS sets h_theta_eddy_visc4 = h_mom_eddy_visc4 for 2d_smagorinsky (atm_compute_dyn_tend_work)
  call theta_mixing(h_mom_eddy_visc4, theta_m, rho_edge, eddy_visc_horz, cells_on_edge, edges_on_cell, degree, &
                    sign_cell, inv_area_cell, inv_dc_edge, dv_edge, scaling_del2, scaling_del4, delsq_theta, &
                    tend_theta_euler)
 end if
 call w_theta_edge_fluxes(w, theta_m, ru, fzm, fzp, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, edge_flux_w, edge_flux_theta)
 call w_theta_columns(rk_step, w, rw, pressure_p, cqw, dpdz, theta_m, ru, ru_save, rw_save, theta_m_save, rho_zz, &
                      rt_diabatic_tend, tend_rho, tend_rtheta_physics, cells_on_edge, edges_on_cell, degree, sign_cell, &
                      dv_edge, rdzu, rdzw, fzm, fzp, inv_area_cell, edge_flux_w, edge_flux_theta, tend_w_euler, &
                      tend_theta_euler, tend_w, tend_theta, rthdynten)
 end subroutine

 ! ---------------------------------------------------------------- stage-1 blocks

 ! Eddy viscosity of the horizontal mixing from the strain rate on the cell (Smagorinsky),
 ! limited so that the explicit diffusion stays stable.
 subroutine smagorinsky_2d(u, v, def_c2, def_s2, def_cs, degree, edges_on_cell, kdiff)
 real, intent(in) :: u(nz,ne1), v(nz,ne1), def_c2(maxedges,nc1), def_s2(maxedges,nc1), def_cs(maxedges,nc1)
 integer, intent(in) :: degree(nc1), edges_on_cell(maxedges,nc1)
 real, intent(inout) :: kdiff(nz,nc1)
 real :: invdt, d_11(nz), d_22(nz), d_12(nz), dudx(nz), dudy(nz), dvdx(nz), dvdy(nz)
 integer :: c, i, k
 invdt = 1.0 / dt
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   dudx(k) = 0.0
   dudy(k) = 0.0
   dvdx(k) = 0.0
   dvdy(k) = 0.0
  end do
  do i = 1, degree(c)
   do k = 1, nz
    dudx(k) = dudx(k) + def_c2(i,c)*u(k,edges_on_cell(i,c)) - def_cs(i,c)*v(k,edges_on_cell(i,c))
    dudy(k) = dudy(k) + def_cs(i,c)*u(k,edges_on_cell(i,c)) - def_s2(i,c)*v(k,edges_on_cell(i,c))
    dvdx(k) = dvdx(k) + def_cs(i,c)*u(k,edges_on_cell(i,c)) + def_c2(i,c)*v(k,edges_on_cell(i,c))
    dvdy(k) = dvdy(k) + def_s2(i,c)*u(k,edges_on_cell(i,c)) + def_cs(i,c)*v(k,edges_on_cell(i,c))
   end do
  end do
  do k = 1, nz
   d_11(k) = 2*dudx(k)
   d_22(k) = 2*dvdy(k)
   d_12(k) = dudy(k) + dvdx(k)
   kdiff(k,c) = (smagorinsky_coef * len_disp)**2 * sqrt(0.25*(d_11(k)-d_22(k))**2 + d_12(k)**2)
   kdiff(k,c) = min(kdiff(k,c),(0.01*len_disp**2) * invdt)
  end do
 end do
 end subroutine

 ! 2nd-order (Smagorinsky) and 4th-order horizontal mixing of u, added to the Euler part of tend_u; then tend_u gets its Euler
 ! and physics parts (stage 1).
 subroutine u_mixing(h_mom_eddy_visc4, u, divergence, vorticity, rho_edge, eddy_visc_horz, cells_on_edge, vertices_on_edge, &
                     edges_on_cell, degree, sign_cell, edges_on_vertex, sign_vertex, inv_area_cell, inv_area_triangle, &
                     inv_dv_edge, inv_dc_edge, dc_edge, dv_edge, scaling_del2, scaling_del4, tend_ru_physics, &
                     delsq_u, delsq_vorticity, delsq_divergence, tend_u_euler, tend_u)
 real, intent(in) :: h_mom_eddy_visc4, u(nz,ne1), divergence(nz,nc1), vorticity(nz,nv1), rho_edge(nz,ne1), eddy_visc_horz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1), vertices_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 integer, intent(in) :: edges_on_vertex(vertex_degree,nv1)
 real, intent(in) :: sign_cell(maxedges,nc1), sign_vertex(vertex_degree,nv1), inv_area_cell(nc1), inv_area_triangle(nv1)
 real, intent(in) :: inv_dv_edge(ne1), inv_dc_edge(ne1), dc_edge(ne1), dv_edge(ne1), scaling_del2(ne1), scaling_del4(ne1)
 real, intent(in) :: tend_ru_physics(nz,ne1)
 real, intent(inout) :: delsq_u(nz,ne1), delsq_vorticity(nz,nv1), delsq_divergence(nz,nc1), tend_u_euler(nz,ne1), tend_u(nz,ne1)
 integer :: e, c, vtx, i, k, cell1, cell2, vertex1, vertex2
 real :: r_dc, r_dv, u_diffusion, kdiffu, r, edge_sign, u_mix_scale
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  vertex1 = vertices_on_edge(1,e)
  vertex2 = vertices_on_edge(2,e)
  r_dc = inv_dc_edge(e)
  r_dv = min(inv_dv_edge(e), 4*inv_dc_edge(e))
  do k = 1, nz
   u_diffusion = ( divergence(k,cell2) - divergence(k,cell1) ) * r_dc - ( vorticity(k,vertex2) - vorticity(k,vertex1) ) * r_dv
   delsq_u(k,e) = 0.0 + u_diffusion
   kdiffu = 0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2))
   tend_u_euler(k,e) = tend_u_euler(k,e) + rho_edge(k,e)* kdiffu * u_diffusion * scaling_del2(e)
  end do
 end do
 !$omp do schedule(dynamic,64)
 do vtx = 1, nvertices
  do k = 1, nz
   delsq_vorticity(k,vtx) = 0.0
  end do
  do i = 1, vertex_degree
   e = edges_on_vertex(i,vtx)
   edge_sign = inv_area_triangle(vtx) * dc_edge(e) * sign_vertex(i,vtx)
   do k = 1, nz
    delsq_vorticity(k,vtx) = delsq_vorticity(k,vtx) + edge_sign * delsq_u(k,e)
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   delsq_divergence(k,c) = 0.0
  end do
  r = inv_area_cell(c)
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   edge_sign = r * dv_edge(e) * sign_cell(i,c)
   do k = 1, nz
    delsq_divergence(k,c) = delsq_divergence(k,c) + edge_sign * delsq_u(k,e)
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  vertex1 = vertices_on_edge(1,e)
  vertex2 = vertices_on_edge(2,e)
  u_mix_scale = scaling_del4(e)*h_mom_eddy_visc4
  r_dc = u_mix_scale * del4u_div_factor * inv_dc_edge(e)
  r_dv = u_mix_scale * min(inv_dv_edge(e), 4*inv_dc_edge(e))
  do k = 1, nz
   u_diffusion = rho_edge(k,e) * ( ( delsq_divergence(k,cell2) - delsq_divergence(k,cell1) ) * r_dc &
                                 -( delsq_vorticity(k,vertex2) - delsq_vorticity(k,vertex1) ) * r_dv )
   tend_u_euler(k,e) = tend_u_euler(k,e) - u_diffusion
   ! the stage-1 sum of tend_u with its Euler and physics parts (u_tendency does it at stages 2 and 3)
   tend_u(k,e) = tend_u(k,e) + tend_u_euler(k,e) + tend_ru_physics(k,e)
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
 real :: r_areacell, edge_sign, w_turb_flux
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   delsq_w(k,c) = 0.0
  end do
  do k = 1, nz+1
   tend_w_euler(k,c) = 0.0
  end do
  r_areacell = inv_area_cell(c)
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   edge_sign = 0.5 * r_areacell*sign_cell(i,c) * dv_edge(e) * inv_dc_edge(e)
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   do k = 2, nz
    w_turb_flux = edge_sign*(rho_edge(k,e)+rho_edge(k-1,e))*(w(k,cell2) - w(k,cell1))
    delsq_w(k,c) = delsq_w(k,c) + w_turb_flux
    w_turb_flux = w_turb_flux * scaling_del2(e) * 0.25 * &
                    ( eddy_visc_horz(k  ,cell1)+eddy_visc_horz(k  ,cell2) &
                     +eddy_visc_horz(k-1,cell1)+eddy_visc_horz(k-1,cell2) )
    tend_w_euler(k,c) = tend_w_euler(k,c) + w_turb_flux
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  r_areacell = h_mom_eddy_visc4 * inv_area_cell(c)
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   edge_sign = scaling_del4(e)*r_areacell*dv_edge(e)*sign_cell(i,c) * inv_dc_edge(e)
   do k = 2, nz
    tend_w_euler(k,c) = tend_w_euler(k,c) - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1))
   end do
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
 real :: r_areacell, edge_sign, pr_scale, theta_turb_flux, prandtl_inv
 prandtl_inv = 1.0 / prandtl
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   delsq_theta(k,c) = 0.0
   tend_theta_euler(k,c) = 0.0
  end do
  r_areacell = inv_area_cell(c)
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   edge_sign = r_areacell*sign_cell(i,c) * dv_edge(e) * inv_dc_edge(e)
   pr_scale = prandtl_inv * scaling_del2(e)
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   do k = 1, nz
    theta_turb_flux = edge_sign*(theta_m(k,cell2) - theta_m(k,cell1))*rho_edge(k,e)
    delsq_theta(k,c) = delsq_theta(k,c) + theta_turb_flux
    theta_turb_flux = theta_turb_flux*0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2)) * pr_scale
    tend_theta_euler(k,c) = tend_theta_euler(k,c) + theta_turb_flux
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  r_areacell = h_theta_eddy_visc4 * prandtl_inv * inv_area_cell(c)
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   edge_sign = scaling_del4(e)*r_areacell*dv_edge(e)*sign_cell(i,c)*inv_dc_edge(e)
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   do k = 1, nz
    tend_theta_euler(k,c) = tend_theta_euler(k,c) - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
   end do
  end do
 end do
 end subroutine

 ! ---------------------------------------------------------------- every stage

 ! Horizontal divergence of the mass flux ru; at stage 1 also the continuity tendency and the hydrostatic/buoyancy term dpdz
 ! (kept for the pressure-gradient force), in the same cell iteration.
 subroutine horizontal_divergence(rk_step, ru, rw, dv_edge, inv_area_cell, edges_on_cell, degree, sign_cell, rdzw, rho_base, &
                                  rho_p_save, qtot, tend_rho_physics, h_divergence, tend_rho, dpdz)
 integer, intent(in) :: rk_step
 real, intent(in) :: ru(nz,ne1), rw(nz+1,nc1), dv_edge(ne1), inv_area_cell(nc1), sign_cell(maxedges,nc1), rdzw(nz)
 real, intent(in) :: rho_base(nz,nc1), rho_p_save(nz,nc1), qtot(nz,nc1), tend_rho_physics(nz,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1)
 real, intent(inout) :: h_divergence(nz,nc1), tend_rho(nz,nc1), dpdz(nz,nc1)
 integer :: c, i, k, e
 real :: edge_sign
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   h_divergence(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   edge_sign = sign_cell(i,c) * dv_edge(e)
   do k = 1, nz
    h_divergence(k,c) = h_divergence(k,c) + edge_sign * ru(k,e)
   end do
  end do
  do k = 1, nz
   h_divergence(k,c) = h_divergence(k,c) * inv_area_cell(c)
  end do
  if (rk_step == 1) then
   do k = 1, nz
    tend_rho(k,c) = -h_divergence(k,c)-rdzw(k)*(rw(k+1,c)-rw(k,c)) + tend_rho_physics(k,c)
    dpdz(k,c) = -gravity*(rho_base(k,c)*(qtot(k,c)) + rho_p_save(k,c)*(1.+qtot(k,c)))
   end do
  end if
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
 real :: wduz(nz+1), q(nz), workpv, reference_u, cos_a, sin_a
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  if (rk_step == 1) then
   do k = 1, nz
    tend_u_euler(k,e) = - cqu(k,e)*( (pressure_p(k,cell2)-pressure_p(k,cell1))*inv_dc_edge(e)/(.5*(zz(k,cell2)+zz(k,cell1))) &
                                    -0.5*zxu(k,e)*(dpdz(k,cell1)+dpdz(k,cell2)) )
   end do
  end if
  wduz(1) = 0.
  k = 2
  wduz(k) = 0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,e)+fzp(k)*u(k-1,e))
  do k = 3, nz-1
   wduz(k) = flux3( u(k-2,e),u(k-1,e),u(k,e),u(k+1,e),0.5*(rw(k,cell1)+rw(k,cell2)), 1.0 )
  end do
  k = nz
  wduz(k) = 0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,e)+fzp(k)*u(k-1,e))
  wduz(nz+1) = 0.
  do k = 1, nz
   tend_u(k,e) = - rdzw(k)*(wduz(k+1)-wduz(k))
  end do
  do k = 1, nz
   q(k) = 0.0
  end do
  do j = 1, degree_edge(e)
   eoe = edges_on_edge(j,e)
   do k = 1, nz
    workpv = 0.5 * (pv_edge(k,e) + pv_edge(k,eoe))
    q(k) = q(k) + weights_on_edge(j,e) * u(k,eoe) * workpv
   end do
  end do
  ! the reference wind of the Coriolis term is zero in real-data starts: its terms are all zero then (q starts at +0 and is
  ! never -0, so subtracting a zero leaves it unchanged, bit for bit) and the loop is skipped
  if (reference_wind) then
   do j = 1, degree_edge(e)
    eoe = edges_on_edge(j,e)
    cos_a = cos_edge(eoe)
    sin_a = sin_edge(eoe)
    do k = 1, nz
     reference_u = u_init(k) * cos_a - v_init(k) * sin_a
     q(k) = q(k) - weights_on_edge(j,e) * reference_u * f_edge(e)
    end do
   end do
  end if
  do k = 1, nz
   tend_u(k,e) = tend_u(k,e) + rho_edge(k,e)* (q(k) - (ke(k,cell2) - ke(k,cell1)) * inv_dc_edge(e)) &
                             - u(k,e)*0.5*(h_divergence(k,cell1)+h_divergence(k,cell2))
  end do
  ! stages 2 and 3: the sum with the Euler and physics parts (stage 1 in u_mixing, after the mixing)
  if (rk_step > 1) then
   do k = 1, nz
    tend_u(k,e) = tend_u(k,e) + tend_u_euler(k,e) + tend_ru_physics(k,e)
   end do
  end if
 end do
 end subroutine

 ! Advective fluxes of w and theta_m through each edge, once per edge (the original builds them in the cell loop for each of
 ! the cell's edges, so every edge twice): the mass flux times the 4th-order edge value with its 3rd-order upwind part on the sign of the flux.
 ! The weight of a neighbour, adv_coefs + sign(1,flux)*adv_coefs_3rd, is exactly adv_coefs + adv_coefs_3rd or
 ! adv_coefs - adv_coefs_3rd, so both are formed once per neighbour and chosen by the same sign() test. The cells subtract
 ! sign_cell*flux (w_theta_columns): sign_cell is +-1 (0 at a missing edge), so it equals the original (sign_cell*ru)*value,
 ! bit for bit. The ghost edge is not written: it keeps the work array's zero ghost column, and a cell reads it only at a
 ! missing edge, where sign_cell is 0.
 subroutine w_theta_edge_fluxes(w, theta_m, ru, fzm, fzp, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, flux_w, flux_theta)
 real, intent(in) :: w(nz+1,nc1), theta_m(nz,nc1), ru(nz,ne1), fzm(nz), fzp(nz)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1)
 integer, intent(in) :: n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(inout) :: flux_w(nz,ne1), flux_theta(nz,ne1)
 integer :: e, j, k, adv_cell
 real :: ru_edge_w(nz), sum_w(nz), sum_t(nz), cp, cm
 logical :: up_w(nz), up_t(nz)
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  ! level 1 of w is a wall: its sum is formed with the others (one loop for both fields) but never stored
  ru_edge_w(1) = 0.0
  up_w(1) = .true.
  do k = 2, nz
   ru_edge_w(k) = fzm(k)*ru(k,e) + fzp(k)*ru(k-1,e)
   up_w(k) = sign(1.0,ru_edge_w(k)) > 0.0
  end do
  do k = 1, nz
   up_t(k) = sign(1.0,ru(k,e)) > 0.0
   sum_w(k) = 0.0
   sum_t(k) = 0.0
  end do
  do j = 1, n_adv(e)
   adv_cell = adv_cells(j,e)
   cp = adv_coefs(j,e) + adv_coefs_3rd(j,e)
   cm = adv_coefs(j,e) - adv_coefs_3rd(j,e)
   do k = 1, nz
    sum_w(k) = sum_w(k) + merge(cp, cm, up_w(k)) * w(k,adv_cell)
    sum_t(k) = sum_t(k) + merge(cp, cm, up_t(k)) * theta_m(k,adv_cell)
   end do
  end do
  do k = 2, nz
   flux_w(k,e) = ru_edge_w(k)*sum_w(k)
  end do
  do k = 1, nz
   flux_theta(k,e) = ru(k,e)*sum_t(k)
  end do
 end do
 end subroutine

 ! The columns of w and theta_m, one cell at a time (the original's w and theta_m loops, each value with the same operations in
 ! the same order).
 ! w: the horizontal advection (the edges' fluxes in the cell's edge order), the vertical advection and division by the cell area,
 ! the vertical pressure-gradient/buoyancy part (stage 1, into tend_w_euler, whose mixing w_mixing made) and the sum with it.
 ! theta_m: the horizontal advection, at stages 2 and 3 the correction for ru at the start of the substep (the acoustic step
 ! redefined rtheta_pp this way: the difference to ru_save is carried with theta at the start), the vertical advection, division
 ! by the cell area, the dynamics-theta tendency rthdynten and the sum with diabatic, Euler and physics parts. rthdynten is read
 ! only after the step (Grell-Freitas forcing, convection, restart) and every call overwrites it, so only stage 3 writes it; the
 ! last stage 3 of the step leaves the same values.
 subroutine w_theta_columns(rk_step, w, rw, pressure_p, cqw, dpdz, theta_m, ru, ru_save, rw_save, theta_m_save, rho_zz, &
                            rt_diabatic_tend, tend_rho, tend_rtheta_physics, cells_on_edge, edges_on_cell, degree, sign_cell, &
                            dv_edge, rdzu, rdzw, fzm, fzp, inv_area_cell, flux_w, flux_theta, tend_w_euler, &
                            tend_theta_euler, tend_w, tend_theta, rthdynten)
 integer, intent(in) :: rk_step
 real, intent(in) :: w(nz+1,nc1), rw(nz+1,nc1), pressure_p(nz,nc1), cqw(nz,nc1), dpdz(nz,nc1), theta_m(nz,nc1), ru(nz,ne1)
 real, intent(in) :: ru_save(nz,ne1), rw_save(nz+1,nc1), theta_m_save(nz,nc1), rho_zz(nz,nc1), rt_diabatic_tend(nz,nc1)
 real, intent(in) :: tend_rho(nz,nc1), tend_rtheta_physics(nz,nc1), sign_cell(maxedges,nc1), dv_edge(ne1)
 real, intent(in) :: rdzu(nz), rdzw(nz), fzm(nz), fzp(nz), inv_area_cell(nc1), flux_w(nz,ne1), flux_theta(nz,ne1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 real, intent(inout) :: tend_w_euler(nz+1,nc1), tend_theta_euler(nz,nc1), tend_w(nz+1,nc1), tend_theta(nz,nc1)
 real, intent(inout) :: rthdynten(nz,nc1)
 integer :: c, i, k, e, cell1, cell2
 real :: s, flux, wdwz(nz+1), wdtz(nz+1)
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  ! ---- w
  do k = 1, nz+1
   tend_w(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   s = sign_cell(i,c)
   do k = 2, nz
    tend_w(k,c) = tend_w(k,c) - s*flux_w(k,e)
   end do
  end do
  wdwz(1) = 0.0
  k = 2
  wdwz(k) = 0.25*(rw(k,c)+rw(k-1,c))*(w(k,c)+w(k-1,c))
  do k = 3, nz-1
   wdwz(k) = flux3( w(k-2,c),w(k-1,c),w(k,c),w(k+1,c),0.5*(rw(k,c)+rw(k-1,c)), 1.0 )
  end do
  k = nz
  wdwz(k) = 0.25*(rw(k,c)+rw(k-1,c))*(w(k,c)+w(k-1,c))
  wdwz(nz+1) = 0.0
  do k = 2, nz
   tend_w(k,c) = tend_w(k,c) * inv_area_cell(c) -rdzu(k)*(wdwz(k+1)-wdwz(k))
  end do
  if (rk_step == 1) then
   do k = 2, nz
    tend_w_euler(k,c) = tend_w_euler(k,c) - cqw(k,c)*( rdzu(k)*(pressure_p(k,c)-pressure_p(k-1,c)) &
                                                       - (fzm(k)*dpdz(k,c) + fzp(k)*dpdz(k-1,c)) )
   end do
  end if
  do k = 2, nz
   tend_w(k,c) = tend_w(k,c) + tend_w_euler(k,c)
  end do
  ! ---- theta_m
  do k = 1, nz
   tend_theta(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   s = sign_cell(i,c)
   do k = 1, nz
    tend_theta(k,c) = tend_theta(k,c) - s*flux_theta(k,e)
   end do
  end do
  if (rk_step > 1) then
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    do k = 1, nz
     flux = sign_cell(i,c)*dv_edge(e)*(ru_save(k,e)-ru(k,e))*0.5*(theta_m_save(k,cell2)+theta_m_save(k,cell1))
     tend_theta(k,c) = tend_theta(k,c)-flux
    end do
   end do
  end if
  wdtz(1) = 0.0
  k = 2
  wdtz(k) = rw(k,c)*(fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
  wdtz(k) = wdtz(k)+(rw_save(k,c)-rw(k,c))*(fzm(k)*theta_m_save(k,c)+fzp(k)*theta_m_save(k-1,c))
  do k = 3, nz-1
   wdtz(k) = flux3( theta_m(k-2,c),theta_m(k-1,c),theta_m(k,c),theta_m(k+1,c), rw(k,c), coef_3rd_order )
   wdtz(k) = wdtz(k) + (rw_save(k,c)-rw(k,c))*(fzm(k)*theta_m_save(k,c)+fzp(k)*theta_m_save(k-1,c))
  end do
  k = nz
  wdtz(k) = rw_save(k,c)*(fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
  wdtz(nz+1) = 0.0
  do k = 1, nz
   tend_theta(k,c) = tend_theta(k,c)*inv_area_cell(c) -rdzw(k)*(wdtz(k+1)-wdtz(k))
  end do
  if (rk_step == 3) then
   do k = 1, nz
    rthdynten(k,c) = (tend_theta(k,c)-tend_rho(k,c)*theta_m(k,c))/rho_zz(k,c)
   end do
  end if
  do k = 1, nz
   tend_theta(k,c) = tend_theta(k,c) + rho_zz(k,c)*rt_diabatic_tend(k,c)
   tend_theta(k,c) = tend_theta(k,c) + tend_theta_euler(k,c) + tend_rtheta_physics(k,c)
  end do
 end do
 end subroutine

 ! Fourth-order centred flux; flux3 adds the third-order upwind correction weighted by coef3.
 real function flux4(q_im2, q_im1, q_i, q_ip1, ua)
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua
 flux4 = ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0
 end function

 real function flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3)
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua, coef3
 flux3 = flux4(q_im2, q_im1, q_i, q_ip1, ua) + coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0
 end function
end module
