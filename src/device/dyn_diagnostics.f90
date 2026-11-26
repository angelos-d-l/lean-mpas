! HAND-WRITTEN card version of src/dyn_diagnostics.f90 (not generated; ./lean generate --check does not see it). The card build
! compiles it in place of src/dyn_diagnostics.f90 (config/units.json device_overrides); a change to a dynamics kernel is made in both
! files.
! Per-step coefficient and diagnostic operators of the dynamics: moist coefficients,
! diagnostics of the wind field, coefficients of the vertically implicit solve.
! Same arithmetic and summation order as MPAS 8.4.2 (mpas_atm_time_integration.F, "the original" below); no halos, no pools.
! Arrays follow the original layout (level first); the last cell/edge/vertex slot is the
! permanent zero "missing neighbour" column and is never written here.
module dyn_diagnostics
 use config
 use dyn_config
 use run_settings, only: apvm_upwinding
 implicit none
 ! pv_cell of APVM: a work array of one call (written before it is read), outside the state objects, allocated and put on the
 ! card only when config_apvm_upwinding > 0 (mpas_atm_time_integration.F:7380-7397)
 real, allocatable, save :: pv_cell(:,:)
contains

 subroutine apvm_work_allocate()
 if (apvm_upwinding > 0. .and. .not. allocated(pv_cell)) then
  allocate(pv_cell(nz,nc1))
  pv_cell = 0.
  !$acc enter data copyin(pv_cell)
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
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do c = 1, ncells
  !$acc loop vector
  do k = 1, nz
   qtot(k,c) = 0.0
   do iq = moist_start, moist_end
    qtot(k,c) = qtot(k,c) + scalars(iq,k,c)
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do c = 1, ncells
  !$acc loop vector
  do k = 2, nz
   qtotal = 0.5*(qtot(k,c)+qtot(k-1,c))
   cqw(k,c) = 1.0 / (1.0 + qtotal)
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  !$acc loop vector
  do k = 1, nz
   qtotal = 0.0
   do iq = moist_start, moist_end
    qtotal = qtotal + 0.5 * ( scalars(iq,k,cell1) + scalars(iq,k,cell2) )
   end do
   cqu(k,e) = 1.0 / (1.0 + qtotal)
  end do
 end do
 end subroutine

 ! ke_edge(k,e) and ke_vertex(k,vtx) of solve_diagnostics, recomputed with the same expressions so that the card stores neither;
 ! the ghost edge and the ghost vertex give zero, as the zero column of a stored array would
 real function ke_e(e, k, ne, u, dc_edge, dv_edge)
 !$acc routine seq
 integer, intent(in) :: e, k, ne
 real, intent(in) :: u(nz,*), dc_edge(*), dv_edge(*)
 real :: efac
 if (e > ne) then
  ke_e = 0.0
 else
  efac = dc_edge(e)*dv_edge(e)
  ke_e = efac*u(k,e)**2
 end if
 end function
 real function ke_v(vtx, k, ne, nv, u, dc_edge, dv_edge, edges_on_vertex, inv_area_triangle)
 !$acc routine seq
 integer, intent(in) :: vtx, k, ne, nv, edges_on_vertex(vertex_degree,*)
 real, intent(in) :: u(nz,*), dc_edge(*), dv_edge(*), inv_area_triangle(*)
 real :: r
 if (vtx > nv) then
  ke_v = 0.0
 else
  r = 0.25 * inv_area_triangle(vtx)
  ke_v = ( ke_e(edges_on_vertex(1,vtx), k, ne, u, dc_edge, dv_edge) + ke_e(edges_on_vertex(2,vtx), k, ne, u, dc_edge, dv_edge) &
         + ke_e(edges_on_vertex(3,vtx), k, ne, u, dc_edge, dv_edge) ) * r
 end if
 end function

 ! Diagnostics of the wind u and density rho_zz: density on edges, tangential wind v (last
 ! Runge-Kutta stage only, compute_v), vorticity, divergence, kinetic energy and the
 ! potential vorticity pv_vertex/pv_edge (the APVM upwinding of config_apvm_upwinding > 0 follows in apvm_upwind).
 ! ke_edge and ke_vertex are work arrays whose missing-neighbour column must hold zero.
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
 integer :: e, c, vtx, k, cell1, cell2, eoe, i, j
 real :: r, s, efac, ke_fact, vort, div, kec, vt
 ! One thread per (level, point) in each kernel; every sum runs over the neighbours in the original order (in a register: the original
 ! summed in the array, the same single-precision values).
 ! edges: density on the edge, kinetic energy on the edge
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, efac) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   rho_edge(k,e) = 0.5 * (rho_zz(k,cell1) + rho_zz(k,cell2))
#ifndef _OPENACC
   efac = dc_edge(e)*dv_edge(e)
   ke_edge(k,e) = efac*u(k,e)**2   ! (the host keeps ke_edge, ke_vertex for the complete export; the kernels below recompute them)
#endif
  end do
 end do
 ! vertices: vorticity, the kinetic energy at the vertex, the potential vorticity
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, s, r, vort) async(1)
 do vtx = 1, nvertices
  do k = 1, nz
   vort = 0.0
   do i = 1, vertex_degree
    e = edges_on_vertex(i,vtx)
    s = sign_vertex(i,vtx) * dc_edge(e)
    vort = vort + s * u(k,e)
   end do
   vort = vort * inv_area_triangle(vtx)
   vorticity(k,vtx) = vort
#ifndef _OPENACC
   r = 0.25 * inv_area_triangle(vtx)
   ke_vertex(k,vtx) = ( ke_edge(k,edges_on_vertex(1,vtx)) + ke_edge(k,edges_on_vertex(2,vtx)) &
                      + ke_edge(k,edges_on_vertex(3,vtx)) ) * r
#endif
   pv_vertex(k,vtx) = (f_vertex(vtx) + vort)
  end do
 end do
 ! cells: divergence, and the kinetic energy of the cell (Ringler et al. 2009) blended with the one at the cell's vertices
 ke_fact = 1.0 - .375
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, j, e, vtx, s, r, div, kec) async(1)
 do c = 1, ncells
  do k = 1, nz
   div = 0.0
   kec = 0.0
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    s = sign_cell(i,c) * dv_edge(e)
    div = div + s * u(k,e)
    kec = kec + 0.25 * ke_e(e, k, nedges, u, dc_edge, dv_edge)
   end do
   r = inv_area_cell(c)
   divergence(k,c) = div * r
   kec = kec * inv_area_cell(c)
   kec = ke_fact * kec
   do i = 1, degree(c)
    vtx = vertices_on_cell(i,c)
    j = kite_for_cell(i,c)
    kec = kec + (1.-ke_fact)*kite_areas(j,vtx) * ke_v(vtx, k, nedges, nvertices, u, dc_edge, dv_edge, edges_on_vertex, inv_area_triangle) * r
   end do
   ke(k,c) = kec
  end do
 end do
 ! edges: tangential wind (Thuburn et al. 2009; it only feeds the Smagorinsky coefficient) and the potential vorticity
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, eoe, vt) async(1)
 do e = 1, nedges
  do k = 1, nz
   if (compute_v) then
    vt = 0.0
    do i = 1, degree_edge(e)
     eoe = edges_on_edge(i,e)
     vt = vt + weights_on_edge(i,e) * u(k,eoe)
    end do
    v(k,e) = vt
   end if
   pv_edge(k,e) = 0.5 * (pv_vertex(k,vertices_on_edge(1,e)) + pv_vertex(k,vertices_on_edge(2,e)))
  end do
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
 !$acc parallel loop gang vector_length(64) default(present)
 do c = 1, ncells
  !$acc loop vector
  do k = 1, nz
   pv_cell(k,c) = 0.0
  end do
  r = inv_area_cell(c)
  do i = 1, degree(c)
   vtx = vertices_on_cell(i,c)
   j = kite_for_cell(i,c)
   !$acc loop vector
   do k = 1, nz
    pv_cell(k,c) = pv_cell(k,c) + kite_areas(j,vtx) * pv_vertex(k,vtx) * r
   end do
  end do
 end do
 upwind = apvm_upwinding * dt_step
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present)
 do e = 1, nedges
  r1 = 1.0 * inv_dv_edge(e)
  r2 = 1.0 * inv_dc_edge(e)
  !$acc loop vector
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
 real :: c2, qtotal, rcv, b_k, c_k
 rcv = rgas/(cp-rgas)
 c2 = cp*rcv
 !$omp single
 !$acc parallel loop gang vector default(present) async(1)
 do k = 1, nz
  cofrz(k) = rdzw(k)
 end do
 !$omp end single
 !$omp do schedule(dynamic,64)
 ! gang = column, vector = level for the coefficients; the diagonal b and the upper diagonal c wait in alpha_tri and gamma_tri for the
 ! elimination
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do c = 1, ncells
  !$acc loop vector
  do k = 1, nz
   if (k > 1) then
    cofwr(k,c) =.5*gravity*(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c))
    cofwz(k,c) = c2*(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c)) &
                 *rdzu(k)*cqw(k,c)*(fzm(k)*exner(k,c)+fzp(k)*exner(k-1,c))
    coftz(k,c) =  (fzm(k)*theta_m(k,c)+fzp(k)*theta_m(k-1,c))
   else
    coftz(1,c) = 0.0
    coftz(nz+1,c) = 0.0
   end if
   qtotal = qtot(k,c)
   cofwt(k,c) = .5*rcv*zz(k,c)*gravity*rho_base(k,c)/(1.+qtotal) &
                *exner(k,c)/((rtheta_base(k,c)+rtheta_p(k,c))*exner_base(k,c))
  end do
  !$acc loop vector
  do k = 1, nz
   if (k == 1) then
    a_tri(1,c) = 0.
    gamma_tri(1,c) = 0.
    alpha_tri(1,c) = 0.
   else
    a_tri(k,c) = -cofwz(k  ,c)* coftz(k-1,c)*rdzw(k-1)*zz(k-1,c) &
                 +cofwr(k  ,c)* cofrz(k-1  ) &
                 -cofwt(k-1,c)* coftz(k-1,c)*rdzw(k-1)
    a_tri(k,c) = a_tri(k,c)*etp(k-1)*ewp(k-1)
    b_k =   +cofwz(k  ,c)*coftz(k,c)* &
                     ( etp(k  )*rdzw(k  )*zz(k  ,c) &
                      +etp(k-1)*rdzw(k-1)*zz(k-1,c)) &
                 -coftz(k  ,c)*( etp(k  )*cofwt(k  ,c)*rdzw(k  ) &
                                -etp(k-1)*cofwt(k-1,c)*rdzw(k-1)) &
                 +cofwr(k,  c)*(etp(k)*cofrz(k    )-etp(k-1)*cofrz(k-1))
    alpha_tri(k,c) =   b_k*ewp(k)
    if (k < nz) then
     c_k =   -cofwz(k  ,c)* coftz(k+1,c)*rdzw(k  )*zz(k  ,c) &
                  -cofwr(k  ,c)* cofrz(k    ) &
                  +cofwt(k  ,c)* coftz(k+1,c)*rdzw(k  )
     gamma_tri(k,c) =   c_k*etp(k)*ewp(k+1)
    else
     gamma_tri(k,c) = 0.0
    end if
   end if
  end do
 end do
 ! the elimination runs upward through the column and cannot be vectorised over k (alpha_tri and gamma_tri hold b and c on entry)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector default(present) async(1)
 do c = 1, ncells
  do k = 2, nz
   alpha_tri(k,c) = 1./(1.0+(dts**2)*(alpha_tri(k,c)-a_tri(k,c)*gamma_tri(k-1,c)))
   gamma_tri(k,c) = (dts**2)*gamma_tri(k,c)*alpha_tri(k,c)
  end do
 end do
 end subroutine
end module
