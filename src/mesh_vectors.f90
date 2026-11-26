! Mesh vectors MPAS recomputes at every start (atm_mpas_init_block): the edge normals and cell tangent planes
! (mpas_initialize_vectors, operators/mpas_vector_operations.F:652) and the wind reconstruction coefficients
! (mpas_init_reconstruct, operators/mpas_vector_reconstruction.F, with the RBF routines of
! operators/mpas_rbf_interpolation.F). The statements and their order are the original's, so the values are the
! same bits as the original build with the same compiler and flags. Regional mesh on the sphere, not periodic.
module mesh_vectors
 use config, only: ncells, nc1, ne1, maxedges
 implicit none
 private
 public :: init_vectors, init_reconstruct
contains
 subroutine init_vectors(x_cell, y_cell, z_cell, x_edge, y_edge, z_edge, cells_on_edge, edges_on_cell, &
                         edge_normal_vectors, cell_tangent_plane)
 real, intent(in) :: x_cell(nc1), y_cell(nc1), z_cell(nc1), x_edge(ne1), y_edge(ne1), z_edge(ne1)
 integer, intent(in) :: cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1)
 real, intent(out) :: edge_normal_vectors(3,ne1), cell_tangent_plane(3,2,nc1)
 real, allocatable :: local_vertical(:,:)
 real :: x_hat(3), y_hat(3), r_hat(3), normal_dot_r_hat
 integer :: e, c, cell1, cell2
 allocate(local_vertical(3,nc1))
 edge_normal_vectors = 0
 local_vertical = 0
 cell_tangent_plane = 0
 do c = 1, ncells
  local_vertical(1,c) = x_cell(c)
  local_vertical(2,c) = y_cell(c)
  local_vertical(3,c) = z_cell(c)
  call unit_vec(local_vertical(:,c))
 end do
 do e = 1, ne1 - 1
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  if (cell1 == ncells+1) then
   edge_normal_vectors(1,e) = x_cell(cell2) - x_edge(e)
   edge_normal_vectors(2,e) = y_cell(cell2) - y_edge(e)
   edge_normal_vectors(3,e) = z_cell(cell2) - z_edge(e)
  else if (cell2 == ncells+1) then
   edge_normal_vectors(1,e) = x_edge(e) - x_cell(cell1)
   edge_normal_vectors(2,e) = y_edge(e) - y_cell(cell1)
   edge_normal_vectors(3,e) = z_edge(e) - z_cell(cell1)
  else
   edge_normal_vectors(1,e) = x_cell(cell2) - x_cell(cell1)
   edge_normal_vectors(2,e) = y_cell(cell2) - y_cell(cell1)
   edge_normal_vectors(3,e) = z_cell(cell2) - z_cell(cell1)
  end if
  call unit_vec(edge_normal_vectors(:,e))
 end do
 do c = 1, ncells
  e = edges_on_cell(1,c)
  r_hat = local_vertical(:,c)
  normal_dot_r_hat = sum(edge_normal_vectors(:,e)*r_hat)
  x_hat = edge_normal_vectors(:,e) - normal_dot_r_hat*r_hat
  call unit_vec(x_hat)
  call cross_product(r_hat, x_hat, y_hat)
  call unit_vec(y_hat)
  cell_tangent_plane(:,1,c) = x_hat
  cell_tangent_plane(:,2,c) = y_hat
 end do
 end subroutine

 subroutine init_reconstruct(x_cell, y_cell, z_cell, x_edge, y_edge, z_edge, edges_on_cell, n_edges_on_cell, &
                             edge_normal_vectors, cell_tangent_plane, coeffs_reconstruct)
 real, intent(in) :: x_cell(nc1), y_cell(nc1), z_cell(nc1), x_edge(ne1), y_edge(ne1), z_edge(ne1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), n_edges_on_cell(nc1)
 real, intent(in) :: edge_normal_vectors(3,ne1), cell_tangent_plane(3,2,nc1)
 real, intent(out) :: coeffs_reconstruct(3,maxedges,nc1)
 real, allocatable :: locations(:,:), normals(:,:), coeffs(:,:), locations_work(:,:), normals_work(:,:), coeffs_work(:,:)
 real :: r, cell_center(3), alpha, tangent_plane(2,3)
 integer :: i, c, e, point_count, max_edge_count
 coeffs_reconstruct = 0.0
 max_edge_count = maxval(n_edges_on_cell(1:ncells))
 allocate(locations(max_edge_count,3), normals(max_edge_count,3), coeffs(max_edge_count,3))
 do c = 1, ncells
  point_count = n_edges_on_cell(c)
  cell_center(1) = x_cell(c)
  cell_center(2) = y_cell(c)
  cell_center(3) = z_cell(c)
  do i = 1, point_count
   e = edges_on_cell(i,c)
   locations(i,1) = x_edge(e)
   locations(i,2) = y_edge(e)
   locations(i,3) = z_edge(e)
   normals(i,:) = edge_normal_vectors(:,e)
  end do
  alpha = 0.0
  do i = 1, point_count
   r = sqrt(sum((cell_center - locations(i,:))**2))
   alpha = alpha + r
  end do
  alpha = alpha/point_count
  tangent_plane(1,:) = cell_tangent_plane(:,1,c)
  tangent_plane(2,:) = cell_tangent_plane(:,2,c)
  allocate(locations_work(point_count,3), normals_work(point_count,3), coeffs_work(point_count,3))
  locations_work = locations(1:point_count,:)
  normals_work = normals(1:point_count,:)
  call plane_vec_const_dir_comp_coeffs(point_count, locations_work, normals_work, cell_center, alpha, tangent_plane, coeffs_work)
  coeffs(1:point_count,:) = coeffs_work
  deallocate(locations_work, normals_work, coeffs_work)
  do i = 1, point_count
   coeffs_reconstruct(:,i,c) = coeffs(i,:)
  end do
 end do
 end subroutine

 ! mpas_rbf_interp_func_3D_plane_vec_const_dir_comp_coeffs
 subroutine plane_vec_const_dir_comp_coeffs(point_count, source_points, unit_vectors, destination_point, alpha, &
                                            plane_basis_vectors, coefficients)
 integer, intent(in) :: point_count
 real, intent(in) :: source_points(point_count,3), unit_vectors(point_count,3), destination_point(3), alpha
 real :: plane_basis_vectors(2,3)
 real, intent(out) :: coefficients(point_count,3)
 integer :: i, matrix_size
 real :: planar_source_points(point_count,2), planar_unit_vectors(point_count,2), planar_destination_point(2)
 real :: matrix(point_count+2,point_count+2), matrix_copy(point_count+2,point_count+2), matrix_work(point_count,point_count)
 real :: rhs(point_count+2,2), coeffs(point_count+2,2), rhs_work(point_count,2)
 integer :: pivot_indices(point_count+2)
 matrix_size = point_count+2
 matrix = 0.0
 rhs = 0.0
 coeffs = 0.0
 do i = 1, point_count
  planar_source_points(i,1) = sum(source_points(i,:)*plane_basis_vectors(1,:))
  planar_source_points(i,2) = sum(source_points(i,:)*plane_basis_vectors(2,:))
  planar_unit_vectors(i,1) = sum(unit_vectors(i,:)*plane_basis_vectors(1,:))
  planar_unit_vectors(i,2) = sum(unit_vectors(i,:)*plane_basis_vectors(2,:))
 end do
 planar_destination_point(1) = sum(destination_point*plane_basis_vectors(1,:))
 planar_destination_point(2) = sum(destination_point*plane_basis_vectors(2,:))
 call vector_dirichlet_matrix_and_rhs(point_count, 2, planar_source_points, planar_unit_vectors, planar_destination_point, &
                                      alpha, matrix_work, rhs_work)
 matrix(1:point_count,1:point_count) = matrix_work
 rhs(1:point_count,:) = rhs_work
 do i = 1, point_count
  matrix(i,point_count+1:point_count+2) = planar_unit_vectors(i,:)
  matrix(point_count+1:point_count+2,i) = matrix(i,point_count+1:point_count+2)
 end do
 do i = 1, 2
  rhs(point_count+i,i) = 1.0
 end do
 matrix_copy = matrix
 call legs(matrix, matrix_size, rhs(:,1), coeffs(:,1), pivot_indices)
 call legs(matrix_copy, matrix_size, rhs(:,2), coeffs(:,2), pivot_indices)
 do i = 1, 3
  coefficients(:,i) = plane_basis_vectors(1,i)*coeffs(1:point_count,1) + plane_basis_vectors(2,i)*coeffs(1:point_count,2)
 end do
 end subroutine

 ! mpas_set_up_vector_dirichlet_rbf_matrix_and_rhs
 subroutine vector_dirichlet_matrix_and_rhs(point_count, dimensions, source_points, unit_vectors, destination_point, alpha, &
                                            matrix, rhs)
 integer, intent(in) :: point_count, dimensions
 real, intent(in) :: source_points(point_count,dimensions), unit_vectors(point_count,dimensions)
 real, intent(in) :: destination_point(dimensions), alpha
 real, intent(out) :: matrix(point_count,point_count), rhs(point_count,dimensions)
 integer :: i, j
 real :: r_squared, rbf_value, unit_vector_dot_product
 do j = 1, point_count
  do i = j, point_count
   r_squared = sum((source_points(i,:)-source_points(j,:))**2)/alpha**2
   rbf_value = evaluate_rbf(r_squared)
   unit_vector_dot_product = sum(unit_vectors(i,:)*unit_vectors(j,:))
   matrix(i,j) = rbf_value*unit_vector_dot_product
   matrix(j,i) = matrix(i,j)
  end do
 end do
 do j = 1, point_count
  r_squared = sum((destination_point-source_points(j,:))**2)/alpha**2
  rhs(j,:) = evaluate_rbf(r_squared)*unit_vectors(j,:)
 end do
 end subroutine

 real function evaluate_rbf(r_squared)
 real, intent(in) :: r_squared
 evaluate_rbf = 1/sqrt(1 + r_squared)
 end function

 ! mpas_legs and elgs: Gaussian elimination with scaled partial pivoting
 subroutine legs(a, n, b, x, indx)
 integer, intent(in) :: n
 integer, intent(out) :: indx(n)
 real, intent(inout) :: a(n,n), b(n)
 real, intent(out) :: x(n)
 integer :: i, j
 call elgs(a, n, indx)
 do i = 1, n-1
  do j = i+1, n
   b(indx(j)) = b(indx(j))-a(indx(j),i)*b(indx(i))
  end do
 end do
 x(n) = b(indx(n))/a(indx(n),n)
 do i = n-1, 1, -1
  x(i) = b(indx(i))
  do j = i+1, n
   x(i) = x(i)-a(indx(i),j)*x(j)
  end do
  x(i) = x(i)/a(indx(i),i)
 end do
 end subroutine

 subroutine elgs(a, n, indx)
 integer, intent(in) :: n
 integer, intent(out) :: indx(n)
 real, intent(inout) :: a(n,n)
 integer :: i, j, k, itmp
 real :: c1, pi, pi1, pj, c(n)
 do i = 1, n
  indx(i) = i
 end do
 do i = 1, n
  c1 = 0.0
  do j = 1, n
   c1 = max(c1,abs(a(i,j)))
  end do
  c(i) = c1
 end do
 do j = 1, n-1
  pi1 = 0.0
  do i = j, n
   pi = abs(a(indx(i),j))/c(indx(i))
   if (pi > pi1) then
    pi1 = pi
    k = i
   end if
  end do
  itmp = indx(j)
  indx(j) = indx(k)
  indx(k) = itmp
  do i = j+1, n
   pj = a(indx(i),j)/a(indx(j),j)
   a(indx(i),j) = pj
   do k = j+1, n
    a(indx(i),k) = a(indx(i),k)-pj*a(indx(j),k)
   end do
  end do
 end do
 end subroutine

 subroutine unit_vec(v)
 real, intent(inout) :: v(3)
 real :: mag
 mag = sqrt(v(1)**2+v(2)**2+v(3)**2)
 v(:) = v(:) / mag
 end subroutine

 subroutine cross_product(p1, p2, p_out)
 real, intent(in) :: p1(3), p2(3)
 real, intent(out) :: p_out(3)
 p_out(1) = p1(2)*p2(3)-p1(3)*p2(2)
 p_out(2) = p1(3)*p2(1)-p1(1)*p2(3)
 p_out(3) = p1(1)*p2(2)-p1(2)*p2(1)
 end subroutine
end module
