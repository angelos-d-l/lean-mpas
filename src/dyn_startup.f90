! Mesh coefficients that MPAS computes once at startup (atm_mpas_init_block) and the
! dynamics reads at every step. Same arithmetic as MPAS 8.4.2 ("the original" below); no pools, no halos.
! Every output array is set completely here (the original relies on pool arrays
! starting at zero), so a missing neighbour (index n+1) reads a defined zero.
module dyn_startup
 use config
 use dyn_config
 implicit none
contains

 ! +1/-1 orientation of each edge seen from its vertex and from its cell, the
 ! per-cell copy of the terrain-slope coefficients zb/zb3, and the kite index of
 ! each cell inside the vertex that surrounds it.
 subroutine edge_signs(cells_on_edge, vertices_on_edge, edges_on_vertex, cells_on_vertex, degree, &
                       edges_on_cell, vertices_on_cell, zb, zb3, &
                       sign_vertex, sign_cell, kite_for_cell, zb_cell, zb3_cell)
 integer, intent(in) :: cells_on_edge(2,ne1), vertices_on_edge(2,ne1), edges_on_vertex(vertex_degree,nv1)
 integer, intent(in) :: cells_on_vertex(vertex_degree,nv1), degree(nc1), edges_on_cell(maxedges,nc1)
 integer, intent(in) :: vertices_on_cell(maxedges,nc1)
 real, intent(in) :: zb(nz+1,2,ne1), zb3(nz+1,2,ne1)
 real, intent(out) :: sign_vertex(vertex_degree,nv1), sign_cell(maxedges,nc1)
 integer, intent(out) :: kite_for_cell(maxedges,nc1)
 real, intent(out) :: zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1)
 integer :: i, j, c, v, e
 sign_vertex = 0.; sign_cell = 0.; kite_for_cell = 0; zb_cell = 0.; zb3_cell = 0.
 do v = 1, nvertices
  do i = 1, vertex_degree
   if (edges_on_vertex(i,v) <= nedges) then
    if (v == vertices_on_edge(2,edges_on_vertex(i,v))) then
     sign_vertex(i,v) = 1.
    else
     sign_vertex(i,v) = -1.
    end if
   end if
  end do
 end do
 do c = 1, ncells
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   if (e <= nedges) then
    if (c == cells_on_edge(1,e)) then
     sign_cell(i,c) = 1.
     zb_cell(:,i,c) = zb(:,1,e)
     zb3_cell(:,i,c) = zb3(:,1,e)
    else
     sign_cell(i,c) = -1.
     zb_cell(:,i,c) = zb(:,2,e)
     zb3_cell(:,i,c) = zb3(:,2,e)
    end if
   end if
  end do
 end do
 do c = 1, ncells
  do i = 1, degree(c)
   v = vertices_on_cell(i,c)
   kite_for_cell(i,c) = 1
   if (v <= nvertices) then
    do j = 1, vertex_degree
     if (c == cells_on_vertex(j,v)) then
      kite_for_cell(i,c) = j
      exit
     end if
    end do
    if (j > vertex_degree) error stop 'edge_signs: cell not found around its vertex'
   end if
  end do
 end do
 end subroutine

 subroutine inverse_geometry(area_cell, dv_edge, dc_edge, area_triangle, inv_area_cell, inv_dv_edge, inv_dc_edge, inv_area_triangle)
 real, intent(in) :: area_cell(nc1), dv_edge(ne1), dc_edge(ne1), area_triangle(nv1)
 real, intent(out) :: inv_area_cell(nc1), inv_dv_edge(ne1), inv_dc_edge(ne1), inv_area_triangle(nv1)
 integer :: i
 inv_area_cell = 0.; inv_dv_edge = 0.; inv_dc_edge = 0.; inv_area_triangle = 0.
 do i = 1, ncells
  inv_area_cell(i) = 1.0 / area_cell(i)
 end do
 do i = 1, nedges
  inv_dv_edge(i) = 1.0 / dv_edge(i)
 end do
 do i = 1, nedges
  inv_dc_edge(i) = 1.0 / dc_edge(i)
 end do
 do i = 1, nvertices
  inv_area_triangle(i) = 1.0 / area_triangle(i)
 end do
 end subroutine

 ! Eddy-viscosity scaling with the mesh-density function (config_h_ScaleWithMesh = true).
 ! The edge that lacks a cell sees the zero density of the dummy neighbour, as in the original.
 subroutine mesh_scaling(mesh_density, cells_on_edge, del2, del4, regional_cell, regional_edge)
 real, intent(in) :: mesh_density(nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(out) :: del2(ne1), del4(ne1), regional_cell(nc1), regional_edge(ne1)
 integer :: e, c, cell1, cell2
 del2 = 1.0; del4 = 1.0; regional_cell = 1.0; regional_edge = 1.0
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  del2(e) = 1.0 / ( (mesh_density(cell1) + mesh_density(cell2) )/2.0)**0.25
  del4(e) = 1.0 / ( (mesh_density(cell1) + mesh_density(cell2) )/2.0)**0.75
 end do
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  regional_edge(e) = 1.0 / ( (mesh_density(cell1) + mesh_density(cell2) )/2.0)**0.25
 end do
 do c = 1, ncells
  regional_cell(c) = 1.0 / (mesh_density(c))**0.25
 end do
 end subroutine

 ! Upper-level sponge dss (cells) and the implicit-solver off-centring profiles
 ! etp/etm (mid levels) and ewp/ewm (w levels). The heights are the computational
 ! coordinate zeta built from 1/rdzw, as in the original (mpas_atm_core.F:1340-1350), although the Registry describes the
! epssm heights as heights above sea level; the choice matters only where the profile changes, between epssm_bottom and
! epssm_top.
 subroutine damping_coefs(zgrid, rdzw, dss, etp, etm, ewp, ewm)
 real, intent(in) :: zgrid(nz+1,nc1), rdzw(nz)
 real, intent(out) :: dss(nz,nc1), etp(nz), etm(nz), ewp(nz+1), ewm(nz+1)
 real :: z, zt, m1, pii, transition_width, coeff
 real :: height_u(nz), height_w(nz+1)
 integer :: c, k
 m1 = -1.0
 pii = acos(m1)
 dss = 0.0
 do c = 1, ncells
  zt = zgrid(nz+1,c)
  do k = 1, nz
   z = 0.5*(zgrid(k,c) + zgrid(k+1,c))
   if (z > zd) then
    dss(k,c) = xnutr*sin(0.5*pii*(z-zd)/(zt-zd))**2.0
   end if
  end do
 end do
 transition_width = epssm_top - epssm_bottom
 height_w(1) = 0.0
 do k = 1, nz
  height_w(k+1) = height_w(k) + 1.0/rdzw(k)
  height_u(k) = 0.5*(height_w(k+1) + height_w(k))
 end do
 do k = 1, nz
  coeff = off_centring(height_u(k))
  etp(k) = 0.5*(1.0 + coeff)
  etm(k) = 0.5*(1.0 - coeff)
 end do
 do k = 1, nz+1
  coeff = off_centring(height_w(k))
  ewp(k) = 0.5*(1.0 + coeff)
  ewm(k) = 0.5*(1.0 - coeff)
 end do
 contains
  real function off_centring(h)
  real, intent(in) :: h
  real :: zz
  if (h .le. epssm_bottom) then
   off_centring = epssm_min
  else if (h .ge. epssm_top) then
   off_centring = epssm_max
  else
   zz = (h-epssm_bottom)/transition_width
   off_centring = epssm_min + sin(0.5*pii*zz)**2*(epssm_max-epssm_min)
  end if
  end function
 end subroutine

 ! Flux-form advection weights of an edge: the cells around it (the two cell
 ! centres, then the neighbours of cell 1, then the neighbours of cell 2 that are
 ! not yet listed) and the 4th/3rd-order coefficients from deriv_two.
 ! adv_coefs_3rd is already multiplied by coef_3rd_order (atm_couple_coef_3rd_order).
 subroutine advection_weights(cells_on_edge, cells_on_cell, degree, dc_edge, dv_edge, deriv_two, &
                              n_adv, adv_cells, adv_coefs, adv_coefs_3rd)
 integer, intent(in) :: cells_on_edge(2,ne1), cells_on_cell(maxedges,nc1), degree(nc1)
 real, intent(in) :: dc_edge(ne1), dv_edge(ne1), deriv_two(max_adv,2,ne1)
 integer, intent(out) :: n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(out) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1)
 integer :: cell1, cell2, e, n, i, j, j_in, c
 integer :: cell_list(20)
 logical :: addcell
 n_adv = 0; adv_cells = 0; adv_coefs = 0.; adv_coefs_3rd = 0.
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  cell_list(1) = cell1
  cell_list(2) = cell2
  n = 2
  do i = 1, degree(cell1)
   if (cells_on_cell(i,cell1) /= cell2) then
    n = n + 1
    cell_list(n) = cells_on_cell(i,cell1)
   end if
  end do
  do c = 1, degree(cell2)
   addcell = .true.
   do i = 1, n
    if (cell_list(i) == cells_on_cell(c,cell2)) addcell = .false.
   end do
   if (addcell) then
    n = n + 1
    cell_list(n) = cells_on_cell(c,cell2)
   end if
  end do
  n_adv(e) = n
  do c = 1, n
   adv_cells(c,e) = cell_list(c)
  end do

  j_in = 0
  do j = 1, n
   if (cell_list(j) == cell1) j_in = j
  end do
  adv_coefs    (j_in,e) = adv_coefs    (j_in,e) + deriv_two(1,1,e)
  adv_coefs_3rd(j_in,e) = adv_coefs_3rd(j_in,e) + deriv_two(1,1,e)
  do c = 1, degree(cell1)
   j_in = 0
   do j = 1, n
    if (cell_list(j) == cells_on_cell(c,cell1)) j_in = j
   end do
   adv_coefs    (j_in,e) = adv_coefs    (j_in,e) + deriv_two(c+1,1,e)
   adv_coefs_3rd(j_in,e) = adv_coefs_3rd(j_in,e) + deriv_two(c+1,1,e)
  end do

  j_in = 0
  do j = 1, n
   if (cell_list(j) == cell2) j_in = j
  end do
  adv_coefs    (j_in,e) = adv_coefs    (j_in,e) + deriv_two(1,2,e)
  adv_coefs_3rd(j_in,e) = adv_coefs_3rd(j_in,e) - deriv_two(1,2,e)
  do c = 1, degree(cell2)
   j_in = 0
   do j = 1, n
    if (cell_list(j) == cells_on_cell(c,cell2)) j_in = j
   end do
   adv_coefs    (j_in,e) = adv_coefs    (j_in,e) + deriv_two(c+1,2,e)
   adv_coefs_3rd(j_in,e) = adv_coefs_3rd(j_in,e) - deriv_two(c+1,2,e)
  end do

  do j = 1, n
   adv_coefs    (j,e) = - (dc_edge(e) **2) * adv_coefs    (j,e) / 12.
   adv_coefs_3rd(j,e) = - (dc_edge(e) **2) * adv_coefs_3rd(j,e) / 12.
  end do

  ! the second-order centred part goes into the main weights
  j_in = 0
  do j = 1, n
   if (cell_list(j) == cell1) j_in = j
  end do
  adv_coefs(j_in,e) = adv_coefs(j_in,e) + 0.5
  j_in = 0
  do j = 1, n
   if (cell_list(j) == cell2) j_in = j
  end do
  adv_coefs(j_in,e) = adv_coefs(j_in,e) + 0.5

  ! multiplied by the edge length, so a flux is dt*ru times the dot product
  do j = 1, n
   adv_coefs    (j,e) = dv_edge(e) * adv_coefs    (j,e)
   adv_coefs_3rd(j,e) = dv_edge(e) * adv_coefs_3rd(j,e)
  end do
 end do
 adv_coefs_3rd = coef_3rd_order * adv_coefs_3rd
 end subroutine

 ! zb3_cell carries the 3rd-order coefficient as well (second half of atm_couple_coef_3rd_order).
 subroutine couple_zb3(zb3_cell)
 real, intent(inout) :: zb3_cell(nz+1,maxedges,nc1)
 zb3_cell = coef_3rd_order * zb3_cell
 end subroutine

 ! Cell-centred strain-rate weights for the 2-D Smagorinsky eddy viscosity, from the
 ! cell polygon projected on its tangent plane. Cells with a missing neighbour keep zeros.
 subroutine deformation_weights(cells_on_cell, degree, cells_on_edge, edges_on_cell, vertices_on_cell, &
                                xcell, ycell, zcell, xvertex, yvertex, zvertex, c2, s2, cs, cc, ss)
 integer, intent(in) :: cells_on_cell(maxedges,nc1), degree(nc1), cells_on_edge(2,ne1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), vertices_on_cell(maxedges,nc1)
 real, intent(in) :: xcell(nc1), ycell(nc1), zcell(nc1), xvertex(nv1), yvertex(nv1), zvertex(nv1)
 real, intent(out) :: c2(maxedges,nc1), s2(maxedges,nc1), cs(maxedges,nc1), cc(maxedges,nc1), ss(maxedges,nc1)
 real, allocatable :: theta_abs(:)
 real, dimension(25) :: xc, yc, zc, thetav, thetat, dl_sphere, xp, yp
 integer, dimension(25) :: cell_list
 real :: dl, pii, area_cell, sint2, cost2, sint_cost, dx, dy
 integer :: i, ip1, ip2, n, c, iv
 logical :: do_the_cell
 allocate(theta_abs(ncells))
 c2 = 0.; s2 = 0.; cs = 0.; cc = 0.; ss = 0.
 pii = 2.*asin(1.0)
 do c = 1, ncells
  cell_list(1) = c
  do i = 2, degree(c)+1
   cell_list(i) = cells_on_cell(i-1,c)
  end do
  n = degree(c) + 1
  do_the_cell = .true.
  do i = 1, n
   if (cell_list(i) > ncells) do_the_cell = .false.
  end do
  if (.not. do_the_cell) cycle

  ! xc: cell centre and its vertices on the unit sphere
  xc(1) = xcell(c)/sphere_radius
  yc(1) = ycell(c)/sphere_radius
  zc(1) = zcell(c)/sphere_radius
  do i = 2, n
   iv = vertices_on_cell(i-1,c)
   xc(i) = xvertex(iv)/sphere_radius
   yc(i) = yvertex(iv)/sphere_radius
   zc(i) = zvertex(iv)/sphere_radius
  end do
  ! angle of the first vertex seen from the centre, east = 0; a cell centre exactly at
  ! the pole would give a zero-length side, so its angle is set directly
  if (zc(1) == 1.0) then
   theta_abs(c) = pii/2.
  else
   theta_abs(c) = pii/2. - sphere_angle(xc(1), yc(1), zc(1), xc(2), yc(2), zc(2), 0.0, 0.0, 1.0)
  end if
  ! tangent-plane polygon: angle and distance from the centre to each vertex
  thetat(1) = theta_abs(c)
  do i = 1, n-1
   ip2 = i+2
   if (ip2 > n) ip2 = 2
   thetav(i) = sphere_angle(xc(1), yc(1), zc(1), xc(i+1), yc(i+1), zc(i+1), xc(ip2), yc(ip2), zc(ip2))
   dl_sphere(i) = sphere_radius*arc_length(xc(1), yc(1), zc(1), xc(i+1), yc(i+1), zc(i+1))
   if (i .gt. 1) thetat(i) = thetat(i-1)+thetav(i-1)
  end do
  do i = 1, n-1
   xp(i) = cos(thetat(i)) * dl_sphere(i)
   yp(i) = sin(thetat(i)) * dl_sphere(i)
  end do

  ! thetat is reused for the angle of each edge normal
  thetat(1) = theta_abs(c)
  do i = 2, n-1
   ip1 = i+1
   if (ip1 == n) ip1 = 1
   thetat(i) = plane_angle(0.0, 0.0, 0.0, xp(i)-xp(i-1), yp(i)-yp(i-1), 0.0, xp(ip1)-xp(i), yp(ip1)-yp(i), 0.0, &
                           0.0, 0.0, 1.0)
   thetat(i) = thetat(i) + thetat(i-1)
  end do
  area_cell = 0.
  do i = 1, n-1
   ip1 = i+1
   if (ip1 == n) ip1 = 1
   dx = xp(ip1)-xp(i)
   dy = yp(ip1)-yp(i)
   area_cell = area_cell + 0.25*(xp(i)+xp(ip1))*(yp(ip1)-yp(i)) - 0.25*(yp(i)+yp(ip1))*(xp(ip1)-xp(i))
   thetat(i) = atan2(dy,dx)-pii/2.
  end do

  do i = 1, n-1
   ip1 = i+1
   if (ip1 == n) ip1 = 1
   dl = sqrt((xp(ip1)-xp(i))**2 + (yp(ip1)-yp(i))**2)
   sint2 = (sin(thetat(i)))**2
   cost2 = (cos(thetat(i)))**2
   sint_cost = sin(thetat(i))*cos(thetat(i))
   c2(i,c) = dl*cost2/area_cell
   s2(i,c) = dl*sint2/area_cell
   cs(i,c) = dl*sint_cost/area_cell
   cc(i,c) = dl*cos(thetat(i))/area_cell
   ss(i,c) = dl*sin(thetat(i))/area_cell
   if (cells_on_edge(1,edges_on_cell(i,c)) /= c) then
    c2(i,c) = - c2(i,c)
    s2(i,c) = - s2(i,c)
    cs(i,c) = - cs(i,c)
   end if
  end do
 end do
 end subroutine

 ! 1 where the cell, edge or vertex lies in the specified (outermost) boundary zone.
 subroutine boundary_zone_masks(bdy_cell, bdy_edge, bdy_vertex, spec_cell, spec_edge, spec_vertex)
 integer, intent(in) :: bdy_cell(nc1), bdy_edge(ne1), bdy_vertex(nv1)
 real, intent(out) :: spec_cell(nc1), spec_edge(ne1), spec_vertex(nv1)
 spec_cell = 0.; spec_edge = 0.; spec_vertex = 0.
 where (bdy_cell > n_relax_zone) spec_cell = 1.0
 where (bdy_edge > n_relax_zone) spec_edge = 1.0
 where (bdy_vertex > n_relax_zone) spec_vertex = 1.0
 end subroutine

 ! Geometry helpers copied from mpas_geometry_utils with the same arithmetic.
 real function arc_length(ax, ay, az, bx, by, bz)
 real, intent(in) :: ax, ay, az, bx, by, bz
 real :: r, c, cx, cy, cz
 cx = bx - ax
 cy = by - ay
 cz = bz - az
 r = sqrt(ax*ax + ay*ay + az*az)
 c = sqrt(cx*cx + cy*cy + cz*cz)
 arc_length = r * 2.0 * asin(c/(2.0*r))
 end function

 real function sphere_angle(ax, ay, az, bx, by, bz, cx, cy, cz)
 real, intent(in) :: ax, ay, az, bx, by, bz, cx, cy, cz
 real :: a, b, c, abx, aby, abz, acx, acy, acz, dx, dy, dz, s, sin_angle
 a = arc_length(bx, by, bz, cx, cy, cz)
 b = arc_length(ax, ay, az, cx, cy, cz)
 c = arc_length(ax, ay, az, bx, by, bz)
 abx = bx - ax
 aby = by - ay
 abz = bz - az
 acx = cx - ax
 acy = cy - ay
 acz = cz - az
 dx =   (aby * acz) - (abz * acy)
 dy = -((abx * acz) - (abz * acx))
 dz =   (abx * acy) - (aby * acx)
 s = 0.5*(a + b + c)
 sin_angle = sqrt(min(1.0,max(0.0,(sin(s-b)*sin(s-c))/(sin(b)*sin(c)))))
 if ((dx*ax + dy*ay + dz*az) >= 0.0) then
  sphere_angle =  2.0 * asin(max(min(sin_angle,1.0),-1.0))
 else
  sphere_angle = -2.0 * asin(max(min(sin_angle,1.0),-1.0))
 end if
 end function

 real function plane_angle(ax, ay, az, bx, by, bz, cx, cy, cz, u, v, w)
 real, intent(in) :: ax, ay, az, bx, by, bz, cx, cy, cz, u, v, w
 real :: abx, aby, abz, mab, acx, acy, acz, mac, dx, dy, dz, cos_angle
 abx = bx - ax
 aby = by - ay
 abz = bz - az
 mab = sqrt(abx**2.0 + aby**2.0 + abz**2.0)
 acx = cx - ax
 acy = cy - ay
 acz = cz - az
 mac = sqrt(acx**2.0 + acy**2.0 + acz**2.0)
 dx =   (aby * acz) - (abz * acy)
 dy = -((abx * acz) - (abz * acx))
 dz =   (abx * acy) - (aby * acx)
 cos_angle = (abx*acx + aby*acy + abz*acz) / (mab * mac)
 if ((dx*u + dy*v + dz*w) >= 0.0) then
  plane_angle =  acos(max(min(cos_angle,1.0),-1.0))
 else
  plane_angle = -acos(max(min(cos_angle,1.0),-1.0))
 end if
 end function
end module
