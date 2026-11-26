! Per-step coefficient and diagnostic operators of the dynamics: moist coefficients,
! diagnostics of the wind field, coefficients of the vertically implicit solve.
! Same arithmetic and summation order as MPAS 8.4.2 (mpas_atm_time_integration.F, "the original" below); no halos, no pools.
! Arrays follow the original layout (level first); the last cell/edge/vertex slot is the
! permanent zero "missing neighbour" column and is never written here.
! The CPU's loop order (neighbour loop outside, level loop inside, sums in the arrays); the card's kernels are in
! src/device/dyn_diagnostics.f90 (one thread per (level, point), ke_edge and ke_vertex recomputed where read).
module dyn_diagnostics
 use config
 use dyn_config
 use run_settings, only: apvm_upwinding
 implicit none
 ! pv_cell of APVM: a work array of one call (written before it is read), outside the state objects, allocated only when
 ! config_apvm_upwinding > 0 (mpas_atm_time_integration.F:7380-7397)
 real, allocatable, save :: pv_cell(:,:)
contains

 subroutine apvm_work_allocate()
 if (apvm_upwinding > 0. .and. .not. allocated(pv_cell)) then
  allocate(pv_cell(nz,nc1))
  pv_cell = 0.
 end if
 end subroutine

 ! qtot: total water mixing ratio per cell. cqw (w levels) and cqu (edges): factors that turn
 ! the inverse dry density into the inverse moist density.
 subroutine moist_coefficients(scalars, cells_on_edge, qtot, cqw, cqu)
 real, intent(in) :: scalars(num_scalars,nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(out) :: qtot(nz,nc1), cqw(nz,nc1), cqu(nz,ne1)
 integer :: c, e, k, iq, cell1, cell2
 real :: qtotal
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   qtot(k,c) = 0.0
   do iq = moist_start, moist_end
    qtot(k,c) = qtot(k,c) + scalars(iq,k,c)
   end do
  end do
  do k = 2, nz
   qtotal = 0.5*(qtot(k,c)+qtot(k-1,c))
   cqw(k,c) = 1.0 / (1.0 + qtotal)
  end do
 end do
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   qtotal = 0.0
   do iq = moist_start, moist_end
    qtotal = qtotal + 0.5 * ( scalars(iq,k,cell1) + scalars(iq,k,cell2) )
   end do
   cqu(k,e) = 1.0 / (1.0 + qtotal)
  end do
 end do
 end subroutine

 ! Diagnostics of the wind u and density rho_zz: density on edges, tangential wind v (last
 ! Runge-Kutta stage only, compute_v), vorticity, divergence, kinetic energy and the
 ! potential vorticity pv_vertex/pv_edge (the APVM upwinding of config_apvm_upwinding > 0 follows in apvm_upwind).
 ! Three passes (the original's eight loops, each value with the same operations in the same order): the vertices
 ! (vorticity, pv_vertex, ke_vertex), the cells (divergence, ke), the edges (rho_edge, v, pv_edge). The edge kinetic energy
 ! ke_edge = dc*dv*u**2 is recomputed where it is read, as the card does; the array is still written at the last stage
 ! (compute_v), the value the step leaves in it, because the restart file holds it. ke_vertex is a work array whose
 ! missing-neighbour column must hold zero.
 subroutine solve_diagnostics(compute_v, rho_zz, u, cells_on_edge, dc_edge, dv_edge, edges_on_vertex, sign_vertex, &
                              inv_area_triangle, edges_on_cell, sign_cell, degree, inv_area_cell, vertices_on_cell, &
                              kite_for_cell, kite_areas, edges_on_edge, degree_edge, weights_on_edge, vertices_on_edge, &
                              f_vertex, rho_edge, v, vorticity, divergence, ke, pv_vertex, pv_edge, ke_edge, ke_vertex)
 logical, intent(in) :: compute_v
 real, intent(in) :: rho_zz(nz,nc1), u(nz,ne1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_vertex(vertex_degree,nv1), edges_on_cell(maxedges,nc1), degree(nc1)
 integer, intent(in) :: vertices_on_cell(maxedges,nc1), kite_for_cell(maxedges,nc1), edges_on_edge(2*maxedges,ne1)
 integer, intent(in) :: degree_edge(ne1), vertices_on_edge(2,ne1)
 real, intent(in) :: dc_edge(ne1), dv_edge(ne1), sign_vertex(vertex_degree,nv1), inv_area_triangle(nv1), sign_cell(maxedges,nc1)
 real, intent(in) :: inv_area_cell(nc1), kite_areas(vertex_degree,nv1), weights_on_edge(2*maxedges,ne1), f_vertex(nv1)
 real, intent(out) :: rho_edge(nz,ne1), vorticity(nz,nv1), divergence(nz,nc1), ke(nz,nc1)
 real, intent(inout) :: v(nz,ne1)   ! written only when compute_v
 real, intent(out) :: pv_vertex(nz,nv1), pv_edge(nz,ne1)
 real, intent(inout) :: ke_edge(nz,ne1), ke_vertex(nz,nv1)
 integer :: e, c, vtx, k, cell1, cell2, eoe, i, j, e1, e2, e3
 real :: r, s, efac, ef1, ef2, ef3, ke_fact
 ke_fact = 1.0 - .375
 ! vertices: vorticity, pv_vertex, and the kinetic energy at the vertex (Ringler et al. 2009) from its three edges
 !$omp do schedule(dynamic,64)
 do vtx = 1, nvertices
  do k = 1, nz
   vorticity(k,vtx) = 0.0
  end do
  do i = 1, vertex_degree
   e = edges_on_vertex(i,vtx)
   s = sign_vertex(i,vtx) * dc_edge(e)
   do k = 1, nz
    vorticity(k,vtx) = vorticity(k,vtx) + s * u(k,e)
   end do
  end do
  r = inv_area_triangle(vtx)
  do k = 1, nz
   vorticity(k,vtx) = vorticity(k,vtx) * r
   pv_vertex(k,vtx) = (f_vertex(vtx) + vorticity(k,vtx))
  end do
  e1 = edges_on_vertex(1,vtx)
  e2 = edges_on_vertex(2,vtx)
  e3 = edges_on_vertex(3,vtx)
  ef1 = dc_edge(e1)*dv_edge(e1)
  ef2 = dc_edge(e2)*dv_edge(e2)
  ef3 = dc_edge(e3)*dv_edge(e3)
  r = 0.25 * inv_area_triangle(vtx)
  do k = 1, nz
   ke_vertex(k,vtx) = ( ef1*u(k,e1)**2 + ef2*u(k,e2)**2 + ef3*u(k,e3)**2 ) * r
  end do
 end do
 ! cells: divergence and kinetic energy (its edge sum, then the blend with the vertices' kinetic energy); independent of
 ! the edge pass below, so no barrier between them
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   divergence(k,c) = 0.0
   ke(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   s = sign_cell(i,c) * dv_edge(e)
   efac = dc_edge(e)*dv_edge(e)
   do k = 1, nz
    divergence(k,c) = divergence(k,c) + s * u(k,e)
    ke(k,c) = ke(k,c) + 0.25 * (efac*u(k,e)**2)
   end do
  end do
  r = inv_area_cell(c)
  do k = 1, nz
   divergence(k,c) = divergence(k,c) * r
   ke(k,c) = ke_fact * (ke(k,c) * inv_area_cell(c))
  end do
  do i = 1, degree(c)
   vtx = vertices_on_cell(i,c)
   j = kite_for_cell(i,c)
   do k = 1, nz
    ke(k,c) = ke(k,c) + (1.-ke_fact)*kite_areas(j,vtx) * ke_vertex(k,vtx) * r
   end do
  end do
 end do
 !$omp end do nowait
 ! edges: density, tangential wind (Thuburn et al. 2009; it feeds the Smagorinsky coefficient and, when config_apvm_upwinding > 0, the APVM upwinding), pv_edge
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   rho_edge(k,e) = 0.5 * (rho_zz(k,cell1) + rho_zz(k,cell2))
   pv_edge(k,e) = 0.5 * (pv_vertex(k,vertices_on_edge(1,e)) + pv_vertex(k,vertices_on_edge(2,e)))
  end do
  if (compute_v) then
   efac = dc_edge(e)*dv_edge(e)
   do k = 1, nz
    ke_edge(k,e) = efac*u(k,e)**2
    v(k,e) = 0.0
   end do
   do i = 1, degree_edge(e)
    eoe = edges_on_edge(i,e)
    do k = 1, nz
     v(k,e) = v(k,e) + weights_on_edge(i,e) * u(k,eoe)
    end do
   end do
  end if
 end do
 end subroutine

 ! APVM upwinding of pv_edge, when config_apvm_upwinding > 0 (the caller asks): MPAS's three loops after pv_edge
 ! (mpas_atm_time_integration.F:7372-7428) on the pv_edge that solve_diagnostics made. dt_step is the whole step's dt,
 ! as atm_srk3 passes it (:1872,2556). pv_cell is the cell mean of the vertices' pv (:7380-7397), gradPVt and gradPVn
 ! are MPAS's tangential and normal gradients (:7417-7428). v is the tangential wind of the last stage 3: stages 1 and 2
 ! see the one the step before left, as in MPAS (reconstruct_v, :7305-7310).
 subroutine apvm_upwind(dt_step, u, v, cells_on_edge, vertices_on_edge, vertices_on_cell, kite_for_cell, kite_areas, &
                        degree, inv_area_cell, inv_dv_edge, inv_dc_edge, pv_vertex, pv_cell, pv_edge)
 real, intent(in) :: dt_step, u(nz,ne1), v(nz,ne1), kite_areas(vertex_degree,nv1), inv_area_cell(nc1)
 real, intent(in) :: inv_dv_edge(ne1), inv_dc_edge(ne1), pv_vertex(nz,nv1)
 integer, intent(in) :: cells_on_edge(2,ne1), vertices_on_edge(2,ne1), vertices_on_cell(maxedges,nc1)
 integer, intent(in) :: kite_for_cell(maxedges,nc1), degree(nc1)
 real, intent(inout) :: pv_cell(nz,nc1), pv_edge(nz,ne1)
 integer :: c, e, i, j, k, vtx
 real :: r, r1, r2, upwind, grad_t, grad_n
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   pv_cell(k,c) = 0.0
  end do
  r = inv_area_cell(c)
  do i = 1, degree(c)
   vtx = vertices_on_cell(i,c)
   j = kite_for_cell(i,c)
   do k = 1, nz
    pv_cell(k,c) = pv_cell(k,c) + kite_areas(j,vtx) * pv_vertex(k,vtx) * r
   end do
  end do
 end do
 upwind = apvm_upwinding * dt_step
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  r1 = 1.0 * inv_dv_edge(e)
  r2 = 1.0 * inv_dc_edge(e)
  do k = 1, nz
   grad_t = (pv_vertex(k,vertices_on_edge(2,e)) - pv_vertex(k,vertices_on_edge(1,e))) * r1
   grad_n = (pv_cell(k,cells_on_edge(2,e)) - pv_cell(k,cells_on_edge(1,e))) * r2
   pv_edge(k,e) = pv_edge(k,e) - upwind * (v(k,e) * grad_t + u(k,e) * grad_n)
  end do
 end do
 end subroutine

 ! Coefficients of the tridiagonal vertically implicit solve of the acoustic step (w, rho, theta
 ! coupled), for the substep length dts. cofrz is a column constant; b_tri and c_tri are column work.
 subroutine vert_imp_coefs(dts, zz, cqw, qtot, exner, theta_m, rho_base, rtheta_base, exner_base, rtheta_p, &
                           rdzw, fzm, fzp, rdzu, etp, ewp, cofwr, cofwz, coftz, cofwt, a_tri, alpha_tri, gamma_tri, cofrz)
 real, intent(in) :: dts, zz(nz,nc1), cqw(nz,nc1), qtot(nz,nc1), exner(nz,nc1), theta_m(nz,nc1), rho_base(nz,nc1)
 real, intent(in) :: rtheta_base(nz,nc1), exner_base(nz,nc1), rtheta_p(nz,nc1)
 real, intent(in) :: rdzw(nz), fzm(nz), fzp(nz), rdzu(nz), etp(nz), ewp(nz+1)
 real, intent(out) :: cofwr(nz,nc1), cofwz(nz,nc1), coftz(nz+1,nc1), cofwt(nz,nc1)
 real, intent(out) :: a_tri(nz,nc1), alpha_tri(nz,nc1), gamma_tri(nz,nc1), cofrz(nz)
 integer :: c, k
 real :: c2, qtotal, rcv
 real :: b_tri(nz), c_tri(nz)
 rcv = rgas/(cp-rgas)
 c2 = cp*rcv
 !$omp single
 do k = 1, nz
  cofrz(k) = rdzw(k)
 end do
 !$omp end single
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 2, nz
   cofwr(k,c) =.5*gravity*(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c))
  end do
  coftz(1,c) = 0.0
  do k = 2, nz
   cofwz(k,c) = c2*(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c)) &
                *rdzu(k)*cqw(k,c)*(fzm(k)*exner(k,c)+fzp(k)*exner(k-1,c))
   coftz(k,c) =  (fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
  end do
  coftz(nz+1,c) = 0.0
  do k = 1, nz
   qtotal = qtot(k,c)
   cofwt(k,c) = .5*rcv*zz(k,c)*gravity*rho_base(k,c)/(1.+qtotal) &
                *exner(k,c)/((rtheta_base(k,c)+rtheta_p(k,c))*exner_base(k,c))
  end do
  a_tri(1,c) = 0.
  b_tri(1) = 1.
  c_tri(1) = 0.
  gamma_tri(1,c) = 0.
  alpha_tri(1,c) = 0.
  do k = 2, nz
   a_tri(k,c) = -cofwz(k  ,c)* coftz(k-1,c)*rdzw(k-1)*zz(k-1,c) &
                +cofwr(k  ,c)* cofrz(k-1  ) &
                -cofwt(k-1,c)* coftz(k-1,c)*rdzw(k-1)
   a_tri(k,c) = a_tri(k,c)*etp(k-1)*ewp(k-1)
   b_tri(k) =   +cofwz(k  ,c)*coftz(k,c)* &
                    ( etp(k  )*rdzw(k  )*zz(k  ,c) &
                     +etp(k-1)*rdzw(k-1)*zz(k-1,c)) &
                -coftz(k  ,c)*( etp(k  )*cofwt(k  ,c)*rdzw(k  ) &
                               -etp(k-1)*cofwt(k-1,c)*rdzw(k-1)) &
                +cofwr(k,  c)*(etp(k)*cofrz(k    )-etp(k-1)*cofrz(k-1))
   b_tri(k) =   b_tri(k)*ewp(k)
   c_tri(k) =   -cofwz(k  ,c)* coftz(k+1,c)*rdzw(k  )*zz(k  ,c) &
                -cofwr(k  ,c)* cofrz(k    ) &
                +cofwt(k  ,c)* coftz(k+1,c)*rdzw(k  )
   c_tri(k) =   c_tri(k)*etp(k)*ewp(k+1)
  end do
  c_tri(nz) = 0.0
  ! the elimination runs upward through the column and cannot be vectorised over k
  do k = 2, nz
   alpha_tri(k,c) = 1./(1.0+(dts**2)*(b_tri(k)-a_tri(k,c)*gamma_tri(k-1,c)))
   gamma_tri(k,c) = (dts**2)*c_tri(k)*alpha_tri(k,c)
  end do
 end do
 end subroutine
end module
