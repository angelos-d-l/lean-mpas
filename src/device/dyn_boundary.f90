! HAND-WRITTEN card version of src/dyn_boundary.f90 (not generated; ./lean generate --check does not see it). The card build
! compiles it in place of src/dyn_boundary.f90 (config/units.json device_overrides); a change to a dynamics kernel is made in both files.
! Lateral boundary of the regional model: the boundary records of the lbc file with their time interpolation, and the
! operators that act on the specified zone (bdyMask > n_relax_zone) and the relaxation zone (2 .. n_relax_zone) in the Runge-Kutta stages.
! Arithmetic and order as mpas_atm_boundaries.F (mpas_atm_update_bdy_tend, mpas_atm_get_bdy_tend/state) and the atm_bdy_* routines and
! inline blocks of atm_srk3 in mpas_atm_time_integration.F of MPAS 8.4.2. Only what lean_mpas consumes is kept: of the file's fields
! u, rho, theta, qv, qc, qr (and w, which MPAS never uses: w is set to zero in the specified zone) the tendencies of u, ru, rho_zz, rtheta_m
! and the scalars, and the states at the end of the interval. qi qs qg ni nr are taken when the file holds them; a file without
! them (the usual case) gives them a boundary state and tendency of zero, as in MPAS, so the specified zone holds no ice, snow,
! graupel or number concentrations and the relaxation zone damps them toward zero (docs/fixes.md, Boundary files without ice species).
module dyn_boundary
 use config
 use run_settings, only: relax_zone_divdamp_coef
 use dyn_config
 implicit none
 real, parameter, private :: rvord = rv / rgas

 type dyn_lbc
  ! state at the end of the interval (the last record read, derived) and its tendency per second
  real, allocatable :: u(:,:), ru(:,:), rho_zz(:,:), rtheta_m(:,:), scalars(:,:,:)
  real, allocatable :: tend_u(:,:), tend_ru(:,:), tend_rho_zz(:,:), tend_rtheta_m(:,:), tend_scalars(:,:,:)
  ! the boundary values an operator asks for, at a time inside the interval (work arrays, rewritten at every request)
  real, allocatable :: drv_u(:,:), drv_ru(:,:), drv_rtheta_m(:,:), drv_rho_zz(:,:), drv_scalars(:,:,:)
  real, allocatable :: u_new(:,:), ru_new(:,:), rho_zz_new(:,:), rtheta_m_new(:,:), scalars_new(:,:,:)
  integer :: end_sec = 0       ! record validity, seconds after 00 UTC of the start day
  real :: remaining_s = 0.       ! time from the clock to the end of the boundary interval
 end type

 real, allocatable, save, private :: rho_edge(:,:)
 ! The cells and edges the boundary operators can touch: the cells with bdyMaskCell >= 1 and every edge of those cells. Every
 ! operator acts only where the mask is 2 or more (relaxation zone 2..n_relax_zone and the specified zone above it) and reads
 ! its neighbours, which have mask >= 1, so the kernels loop over these lists instead of the whole mesh (most of the regional
 ! domain is interior). Until bdy_lists is called (the tests) the lists are the whole mesh.
 integer, allocatable, save :: bdy_cell(:), bdy_edge(:)
 integer, save :: n_bdy_cell = 0, n_bdy_edge = 0
 !$acc declare create(bdy_cell, bdy_edge)
 ! The boundary data (dyn_lbc's arrays) hold only the listed cells and edges: column slot_cell(c) of a cell array is cell c's,
 ! slot_edge(e) edge e's. With bdy_lists: the list position, and one more column (n_bdy_cell+1, n_bdy_edge+1) for every other
 ! cell or edge, which the boundary values never write (it stays 0).
 ! Without (the tests): the whole mesh, slot = index, nc1 and ne1 columns. A regional mesh's boundary zone is a small part of
 ! it, so the boundary data take only that part of the card memory.
 integer, allocatable, save :: slot_cell(:), slot_edge(:), cell_of_slot(:), edge_of_slot(:)
 integer, save :: n_slot_cell = 0, n_slot_edge = 0
 !$acc declare create(slot_cell, slot_edge)
contains

 subroutine bdy_lists(mask_cell, n_edges_on_cell, edges_on_cell)
 integer, intent(in) :: mask_cell(nc1), n_edges_on_cell(nc1), edges_on_cell(maxedges,nc1)
 logical, allocatable :: edge_in(:)
 integer :: c, i, e
 if (allocated(bdy_cell)) deallocate(bdy_cell, bdy_edge)
 if (allocated(slot_cell)) deallocate(slot_cell, slot_edge, cell_of_slot, edge_of_slot)
 allocate(edge_in(ne1)); edge_in = .false.
 n_bdy_cell = count(mask_cell(1:ncells) >= 1)
 allocate(bdy_cell(max(n_bdy_cell,1)))
 n_bdy_cell = 0
 do c = 1, ncells
  if (mask_cell(c) >= 1) then
   n_bdy_cell = n_bdy_cell + 1
   bdy_cell(n_bdy_cell) = c
   do i = 1, n_edges_on_cell(c)
    edge_in(edges_on_cell(i,c)) = .true.
   end do
  end if
 end do
 n_bdy_edge = count(edge_in(1:nedges))
 allocate(bdy_edge(max(n_bdy_edge,1)))
 n_bdy_edge = 0
 do e = 1, nedges
  if (edge_in(e)) then
   n_bdy_edge = n_bdy_edge + 1
   bdy_edge(n_bdy_edge) = e
  end if
 end do
 n_slot_cell = n_bdy_cell + 1; n_slot_edge = n_bdy_edge + 1
 allocate(slot_cell(nc1), slot_edge(ne1), cell_of_slot(n_slot_cell), edge_of_slot(n_slot_edge))
 slot_cell = n_slot_cell; slot_edge = n_slot_edge
 cell_of_slot(n_slot_cell) = nc1; edge_of_slot(n_slot_edge) = ne1
 do c = 1, n_bdy_cell
  slot_cell(bdy_cell(c)) = c
  cell_of_slot(c) = bdy_cell(c)
 end do
 do e = 1, n_bdy_edge
  slot_edge(bdy_edge(e)) = e
  edge_of_slot(e) = bdy_edge(e)
 end do
 !$acc update device(bdy_cell, bdy_edge, slot_cell, slot_edge)
 end subroutine

 ! the whole mesh, when no list was made
 subroutine whole_mesh_lists()
 integer :: c, e
 if (allocated(bdy_cell)) return
 allocate(bdy_cell(ncells), bdy_edge(nedges))
 do c = 1, ncells
  bdy_cell(c) = c
 end do
 do e = 1, nedges
  bdy_edge(e) = e
 end do
 n_bdy_cell = ncells; n_bdy_edge = nedges
 n_slot_cell = nc1; n_slot_edge = ne1
 allocate(slot_cell(nc1), slot_edge(ne1), cell_of_slot(nc1), edge_of_slot(ne1))
 do c = 1, nc1
  slot_cell(c) = c; cell_of_slot(c) = c
 end do
 do e = 1, ne1
  slot_edge(e) = e; edge_of_slot(e) = e
 end do
 !$acc update device(bdy_cell, bdy_edge, slot_cell, slot_edge)
 end subroutine

 subroutine lbc_allocate(b)
 type(dyn_lbc), intent(inout) :: b
 integer :: ne, nc
 call whole_mesh_lists()   ! (the program made its lists before; the tests get the whole mesh)
 ne = n_slot_edge; nc = n_slot_cell
 allocate(rho_edge(nz,ne1))
 allocate(b%u(nz,ne), b%ru(nz,ne), b%rho_zz(nz,nc), b%rtheta_m(nz,nc), b%scalars(num_scalars,nz,nc))
 allocate(b%tend_u(nz,ne), b%tend_ru(nz,ne), b%tend_rho_zz(nz,nc), b%tend_rtheta_m(nz,nc), b%tend_scalars(num_scalars,nz,nc))
 allocate(b%drv_u(nz,ne), b%drv_ru(nz,ne), b%drv_rtheta_m(nz,nc), b%drv_rho_zz(nz,nc), b%drv_scalars(num_scalars,nz,nc))
 allocate(b%u_new(nz,ne), b%ru_new(nz,ne), b%rho_zz_new(nz,nc), b%rtheta_m_new(nz,nc), b%scalars_new(num_scalars,nz,nc))
 b%u = 0.; b%ru = 0.; b%rho_zz = 0.; b%rtheta_m = 0.; b%scalars = 0.
 b%tend_u = 0.; b%tend_ru = 0.; b%tend_rho_zz = 0.; b%tend_rtheta_m = 0.; b%tend_scalars = 0.
 b%drv_u = 0.; b%drv_ru = 0.; b%drv_rtheta_m = 0.; b%drv_rho_zz = 0.; b%drv_scalars = 0.
 end subroutine

 ! The record's fields in the slots of the boundary data (derive_record on the whole mesh, then each slot's column).
 subroutine derive_slots(u, rho, theta, qv, qc, qr, cells_on_edge, zz, u_out, ru_out, rho_zz_out, rtheta_m_out, scalars_out, &
                         qi, qs, qg, ni, nr)
 real, intent(in) :: u(nz,ne1), rho(nz,nc1), theta(nz,nc1), qv(nz,nc1), qc(nz,nc1), qr(nz,nc1), zz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(out) :: u_out(nz,n_slot_edge), ru_out(nz,n_slot_edge), rho_zz_out(nz,n_slot_cell), rtheta_m_out(nz,n_slot_cell)
 real, intent(out) :: scalars_out(num_scalars,nz,n_slot_cell)
 real, intent(in), optional :: qi(nz,nc1), qs(nz,nc1), qg(nz,nc1), ni(nz,nc1), nr(nz,nc1)
 real, allocatable :: uw(:,:), ruw(:,:), rzw(:,:), rtw(:,:), sw(:,:,:)
 integer :: l
 allocate(uw(nz,ne1), ruw(nz,ne1), rzw(nz,nc1), rtw(nz,nc1), sw(num_scalars,nz,nc1))
 call derive_record(u, rho, theta, qv, qc, qr, cells_on_edge, zz, uw, ruw, rzw, rtw, sw, qi, qs, qg, ni, nr)
 do l = 1, n_slot_edge
  u_out(:,l) = uw(:,edge_of_slot(l)); ru_out(:,l) = ruw(:,edge_of_slot(l))
 end do
 do l = 1, n_slot_cell
  rho_zz_out(:,l) = rzw(:,cell_of_slot(l)); rtheta_m_out(:,l) = rtw(:,cell_of_slot(l))
  scalars_out(:,:,l) = sw(:,:,cell_of_slot(l))
 end do
 end subroutine

 ! Fields derived from one record: rho_zz = rho / zz, the density on edges, ru = u rho_edge, rtheta_m = theta rho_zz (1 + rvord qv).
 ! qi qs qg ni nr: the record's, when its file holds them (MPAS reads the whole lbc_scalars var_array). When the file lacks
 ! them, the usual case, those rows stay 0 (docs/fixes.md, Boundary files without ice species).
 subroutine derive_record(u, rho, theta, qv, qc, qr, cells_on_edge, zz, u_out, ru_out, rho_zz_out, rtheta_m_out, scalars_out, &
                          qi, qs, qg, ni, nr)
 real, intent(in) :: u(nz,ne1), rho(nz,nc1), theta(nz,nc1), qv(nz,nc1), qc(nz,nc1), qr(nz,nc1), zz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(out) :: u_out(nz,ne1), ru_out(nz,ne1), rho_zz_out(nz,nc1), rtheta_m_out(nz,nc1), scalars_out(num_scalars,nz,nc1)
 real, intent(in), optional :: qi(nz,nc1), qs(nz,nc1), qg(nz,nc1), ni(nz,nc1), nr(nz,nc1)
 integer :: c, e, k, c1, c2
 do c = 1, nc1
  do k = 1, nz
   rho_zz_out(k,c) = rho(k,c) / zz(k,c)
  end do
 end do
 rho_edge = 0.0
 do e = 1, nedges
  c1 = cells_on_edge(1,e)
  c2 = cells_on_edge(2,e)
  do k = 1, nz
   rho_edge(k,e) = 0.5 * (rho_zz_out(k,c1) + rho_zz_out(k,c2))
  end do
 end do
 do e = 1, ne1
  do k = 1, nz
   ru_out(k,e) = u(k,e) * rho_edge(k,e)
   u_out(k,e) = u(k,e)
  end do
 end do
 scalars_out = 0.0
 do c = 1, nc1
  do k = 1, nz
   scalars_out(i_qv,k,c) = qv(k,c)
   scalars_out(i_qc,k,c) = qc(k,c)
   scalars_out(i_qr,k,c) = qr(k,c)
  end do
 end do
 if (present(qi)) scalars_out(i_qi,:,:) = qi
 if (present(qs)) scalars_out(i_qs,:,:) = qs
 if (present(qg)) scalars_out(i_qg,:,:) = qg
 if (present(ni)) scalars_out(i_ni,:,:) = ni
 if (present(nr)) scalars_out(i_nr,:,:) = nr
 do c = 1, nc1
  do k = 1, nz
   rtheta_m_out(k,c) = theta(k,c) * rho_zz_out(k,c) * (1.0 + rvord * scalars_out(i_qv,k,c))
  end do
 end do
 end subroutine

 ! First call of the run (MPAS: firstCall, the latest record at or before the start): state only, tendencies zero.
 ! zz(:,nc1) = 1, as mpas_atm_update_bdy_tend (mpas_atm_boundaries.F) sets it, to avoid a division by zero; the whole model
 ! sees that value afterwards.
 subroutine lbc_first(b, sec, u, rho, theta, qv, qc, qr, cells_on_edge, zz, qi, qs, qg, ni, nr)
 type(dyn_lbc), intent(inout) :: b
 integer, intent(in) :: sec
 real, intent(in) :: u(nz,ne1), rho(nz,nc1), theta(nz,nc1), qv(nz,nc1), qc(nz,nc1), qr(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(inout) :: zz(nz,nc1)
 real, intent(in), optional :: qi(nz,nc1), qs(nz,nc1), qg(nz,nc1), ni(nz,nc1), nr(nz,nc1)
 zz(:,nc1) = 1.0
 call derive_slots(u, rho, theta, qv, qc, qr, cells_on_edge, zz, b%u, b%ru, b%rho_zz, b%rtheta_m, b%scalars, &
                    qi, qs, qg, ni, nr)
 b%end_sec = sec
 end subroutine

 ! A later record (MPAS: the earliest strictly after the present): the tendency is (new - old) / interval, the new state becomes the state.
 subroutine lbc_update(b, sec, u, rho, theta, qv, qc, qr, cells_on_edge, zz, qi, qs, qg, ni, nr)
 type(dyn_lbc), intent(inout) :: b
 integer, intent(in) :: sec
 real, intent(in) :: u(nz,ne1), rho(nz,nc1), theta(nz,nc1), qv(nz,nc1), qc(nz,nc1), qr(nz,nc1), zz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(in), optional :: qi(nz,nc1), qs(nz,nc1), qg(nz,nc1), ni(nz,nc1), nr(nz,nc1)
 real :: dt_inv
 integer :: c, e, k, j
 call derive_slots(u, rho, theta, qv, qc, qr, cells_on_edge, zz, b%u_new, b%ru_new, b%rho_zz_new, b%rtheta_m_new, b%scalars_new, &
                    qi, qs, qg, ni, nr)
 dt_inv = 1.0 / real(sec - b%end_sec)
 do e = 1, n_slot_edge
  do k = 1, nz
   b%tend_u(k,e) = (b%u_new(k,e) - b%u(k,e)) * dt_inv
   b%tend_ru(k,e) = (b%ru_new(k,e) - b%ru(k,e)) * dt_inv
  end do
 end do
 do c = 1, n_slot_cell
  do k = 1, nz
   b%tend_rtheta_m(k,c) = (b%rtheta_m_new(k,c) - b%rtheta_m(k,c)) * dt_inv
   b%tend_rho_zz(k,c) = (b%rho_zz_new(k,c) - b%rho_zz(k,c)) * dt_inv
  end do
 end do
 do c = 1, n_slot_cell
  do k = 1, nz
   do j = 1, num_scalars
    b%tend_scalars(j,k,c) = (b%scalars_new(j,k,c) - b%scalars(j,k,c)) * dt_inv
   end do
  end do
 end do
 b%u = b%u_new; b%ru = b%ru_new; b%rho_zz = b%rho_zz_new; b%rtheta_m = b%rtheta_m_new; b%scalars = b%scalars_new
 b%end_sec = sec
 end subroutine

 ! Boundary value delta_t seconds after the start of the current step: the state at the end of the interval minus the tendency times
 ! the time that is left (mpas_atm_get_bdy_state).
 ! on_edges: the columns of the fields are edges (bdy_edge), else cells (bdy_cell); the lists are read in the kernels as module arrays
 ! (passed as dummy arguments they reached the card as host addresses)
 subroutine bdy_values_2d(b, state, tend, delta_t, out, on_edges)
 type(dyn_lbc), intent(in) :: b
 real, intent(in) :: state(:,:), tend(:,:), delta_t
 real, intent(inout) :: out(:,:)   ! inout: only the listed columns are written
 logical, intent(in) :: on_edges
 real :: dt
 integer :: i, j, l, n, nk
 call whole_mesh_lists()
 dt = b%remaining_s
 dt = dt - delta_t
 n = merge(n_bdy_edge, n_bdy_cell, on_edges)
 nk = size(state, 1)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n
  if (on_edges) then
   j = slot_edge(bdy_edge(l))
  else
   j = slot_cell(bdy_cell(l))
  end if
  !$acc loop vector
  do i = 1, nk
   out(i,j) = state(i,j) - dt * tend(i,j)
  end do
 end do
 end subroutine

 subroutine bdy_values_3d(b, state, tend, delta_t, out)
 type(dyn_lbc), intent(in) :: b
 real, intent(in) :: state(num_scalars,nz,n_slot_cell), tend(num_scalars,nz,n_slot_cell), delta_t
 real, intent(inout) :: out(num_scalars,nz,n_slot_cell)
 real :: dt
 integer :: i, j, k, l
 call whole_mesh_lists()
 dt = b%remaining_s
 dt = dt - delta_t
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  k = slot_cell(bdy_cell(l))
  !$acc loop vector collapse(2)
  do j = 1, nz
   do i = 1, num_scalars
    out(i,j,k) = state(i,j,k) - dt * tend(i,j,k)
   end do
  end do
 end do
 end subroutine

 ! ---- operators ----

 ! atm_bdy_adjust_dynamics_speczone_tend: in the specified zone the dry tendencies are the boundary tendencies; w and the diabatic heating are zero.
 subroutine speczone_tend(mask_cell, mask_edge, drv_tend_ru, drv_tend_rt, drv_tend_rho, tend_ru, tend_rt, tend_rho, tend_rw, rt_diabatic_tend)
 integer, intent(in) :: mask_cell(nc1), mask_edge(ne1)
 real, intent(in) :: drv_tend_ru(nz,n_slot_edge), drv_tend_rt(nz,n_slot_cell), drv_tend_rho(nz,n_slot_cell)
 real, intent(inout) :: tend_ru(nz,ne1), tend_rt(nz,nc1), tend_rho(nz,nc1), tend_rw(nz+1,nc1), rt_diabatic_tend(nz,nc1)
 integer :: c, e, k, l
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    tend_rho(k,c) = drv_tend_rho(k,slot_cell(c))
    tend_rt(k,c) = drv_tend_rt(k,slot_cell(c))
    tend_rw(k,c) = 0.
    rt_diabatic_tend(k,c) = 0.
   end do
  end if
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_edge
  e = bdy_edge(l)
  if (mask_edge(e) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    tend_ru(k,e) = drv_tend_ru(k,slot_edge(e))
   end do
  end if
 end do
 end subroutine

 ! atm_bdy_adjust_dynamics_relaxzone_tend: in the relaxation zone the tendencies get a Rayleigh damping toward the boundary values and a
 ! second-order filter of the difference (cells: rtheta_m and rho_zz; edges: ru by divergence and vorticity of the difference).
 subroutine relaxzone_tend(mask_cell, mask_edge, reg_cell, reg_edge, dc_edge, dv_edge, inv_dc, inv_dv, inv_area_cell, inv_area_tri, &
                           sign_cell, sign_vertex, cells_on_edge, edges_on_cell, edges_on_vertex, n_edges_on_cell, vertices_on_edge, &
                           ru, theta_m, rho_zz, drv_ru, drv_rt, drv_rho, tend_ru, tend_rt, tend_rho)
 integer, intent(in) :: mask_cell(nc1), mask_edge(ne1), cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), edges_on_vertex(3,nv1)
 integer, intent(in) :: n_edges_on_cell(nc1), vertices_on_edge(2,ne1)
 real, intent(in) :: reg_cell(nc1), reg_edge(ne1), dc_edge(ne1), dv_edge(ne1), inv_dc(ne1), inv_dv(ne1), inv_area_cell(nc1)
 real, intent(in) :: inv_area_tri(nv1), sign_cell(maxedges,nc1), sign_vertex(vertex_degree,nv1)
 real, intent(in) :: ru(nz,ne1), theta_m(nz,nc1), rho_zz(nz,nc1)
 real, intent(in) :: drv_ru(nz,n_slot_edge), drv_rt(nz,n_slot_cell), drv_rho(nz,n_slot_cell)
 real, intent(inout) :: tend_ru(nz,ne1), tend_rt(nz,nc1), tend_rho(nz,nc1)
 real :: edge_sign, laplacian_filter_coef, rayleigh_damping_coef, r_dc, r_dv, invArea
 real :: divergence1(max_levels), divergence2(max_levels), vorticity1(max_levels), vorticity2(max_levels)   ! capacity: shared memory
 integer :: c, e, i, k, cell1, cell2, edge_vort, edge_div, vertex1, vertex2, v, l, s1, s2
 call whole_mesh_lists()
 ! Rayleigh damping of rho_zz, rtheta_m and ru
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > 1 .and. mask_cell(c) <= n_relax_zone) then
   rayleigh_damping_coef = (real(mask_cell(c)) - 1.)/real(n_relax_zone)/(50.*dt*reg_cell(c))
   !$acc loop vector
   do k = 1, nz
    tend_rho(k,c) = tend_rho(k,c) - rayleigh_damping_coef * (rho_zz(k,c) - drv_rho(k,slot_cell(c)))
    tend_rt(k,c)  = tend_rt(k,c)  - rayleigh_damping_coef * (rho_zz(k,c)*theta_m(k,c) - drv_rt(k,slot_cell(c)))
   end do
  end if
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_edge
  e = bdy_edge(l)
  if (mask_edge(e) > 1 .and. mask_edge(e) <= n_relax_zone) then
   rayleigh_damping_coef = (real(mask_edge(e)) - 1.)/real(n_relax_zone)/(50.*dt*reg_edge(e))
   !$acc loop vector
   do k = 1, nz
    tend_ru(k,e) = tend_ru(k,e) - rayleigh_damping_coef * (ru(k,e) - drv_ru(k,slot_edge(e)))
   end do
  end if
 end do
 ! horizontal filter of rtheta_m and rho_zz (a dimensionless Laplacian: no cell area)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(s1, s2) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > 1 .and. mask_cell(c) <= n_relax_zone) then
   laplacian_filter_coef = (real(mask_cell(c)) - 1.)/real(n_relax_zone)/(10.*dt*reg_cell(c))
   do i = 1, n_edges_on_cell(c)
    e = edges_on_cell(i,c)
    edge_sign = sign_cell(i,c) * dv_edge(e) * inv_dc(e) * laplacian_filter_coef
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    s1 = slot_cell(cell1)
    s2 = slot_cell(cell2)
    !$acc loop vector
    do k = 1, nz
     tend_rt(k,c)  = tend_rt(k,c)  + edge_sign*(   (rho_zz(k,cell2)*theta_m(k,cell2)-drv_rt(k,s2)) &
                                                 - (rho_zz(k,cell1)*theta_m(k,cell1)-drv_rt(k,s1)) )
     tend_rho(k,c) = tend_rho(k,c) + edge_sign*(   (rho_zz(k,cell2)-drv_rho(k,s2)) &
                                                 - (rho_zz(k,cell1)-drv_rho(k,s1)) )
    end do
   end do
  end if
 end do
 ! horizontal filter of ru: grad(div) - k x grad(vorticity) of the difference to the boundary value
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(divergence1, divergence2, vorticity1, vorticity2, s1) async(1)
 do l = 1, n_bdy_edge
  e = bdy_edge(l)
  if (mask_edge(e) > 1 .and. mask_edge(e) <= n_relax_zone) then
   laplacian_filter_coef = dc_edge(e)**2 * (real(mask_edge(e)) - 1.)/real(n_relax_zone)/(10.*dt*reg_edge(e))
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   vertex1 = vertices_on_edge(1,e)
   vertex2 = vertices_on_edge(2,e)
   r_dc = inv_dc(e)
   r_dv = min(inv_dv(e), 4*inv_dc(e))
   divergence1 = 0.; divergence2 = 0.; vorticity1 = 0.; vorticity2 = 0.
   invArea = inv_area_cell(cell1)
   do i = 1, n_edges_on_cell(cell1)
    edge_div = edges_on_cell(i,cell1)
    edge_sign = invArea * dv_edge(edge_div) * sign_cell(i,cell1)
    s1 = slot_edge(edge_div)
    !$acc loop vector
    do k = 1, nz
     divergence1(k) = divergence1(k) + edge_sign * (ru(k,edge_div) - drv_ru(k,s1))
    end do
   end do
   invArea = inv_area_cell(cell2)
   do i = 1, n_edges_on_cell(cell2)
    edge_div = edges_on_cell(i,cell2)
    edge_sign = invArea * dv_edge(edge_div) * sign_cell(i,cell2)
    s1 = slot_edge(edge_div)
    !$acc loop vector
    do k = 1, nz
     divergence2(k) = divergence2(k) + edge_sign * (ru(k,edge_div) - drv_ru(k,s1))
    end do
   end do
   v = vertex1
   do i = 1, vertex_degree
    edge_vort = edges_on_vertex(i,v)
    edge_sign = inv_area_tri(v) * dc_edge(edge_vort) * sign_vertex(i,v)
    s1 = slot_edge(edge_vort)
    !$acc loop vector
    do k = 1, nz
     vorticity1(k) = vorticity1(k) + edge_sign * (ru(k,edge_vort) - drv_ru(k,s1))
    end do
   end do
   v = vertex2
   do i = 1, vertex_degree
    edge_vort = edges_on_vertex(i,v)
    edge_sign = inv_area_tri(v) * dc_edge(edge_vort) * sign_vertex(i,v)
    s1 = slot_edge(edge_vort)
    !$acc loop vector
    do k = 1, nz
     vorticity2(k) = vorticity2(k) + edge_sign * (ru(k,edge_vort) - drv_ru(k,s1))
    end do
   end do
   !$acc loop vector
   do k = 1, nz
    tend_ru(k,e) = tend_ru(k,e) + laplacian_filter_coef &
                   * (relax_zone_divdamp_coef * (divergence2(k) - divergence1(k)) * r_dc - (vorticity2(k) - vorticity1(k)) * r_dv)
   end do
  end if
 end do
 end subroutine

 ! After the recovery of a stage (srk3 inline block): u and ru at the edges of the specified zone are the boundary values.
 subroutine reset_edge_winds(mask_edge, drv_u, drv_ru, u, ru)
 integer, intent(in) :: mask_edge(ne1)
 real, intent(in) :: drv_u(nz,n_slot_edge), drv_ru(nz,n_slot_edge)
 real, intent(inout) :: u(nz,ne1), ru(nz,ne1)
 integer :: e, k, l
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_edge
  e = bdy_edge(l)
  if (mask_edge(e) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    u(k,e) = drv_u(k,slot_edge(e))
   end do
  end if
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_edge
  e = bdy_edge(l)
  if (mask_edge(e) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    ru(k,e) = drv_ru(k,slot_edge(e))
   end do
  end if
 end do
 end subroutine

 ! atm_zero_gradient_w_bdy_work: w is zero inside the column in the specified zone (the surface and top values are kept).
 subroutine zero_gradient_w(mask_cell, w)
 integer, intent(in) :: mask_cell(nc1)
 real, intent(inout) :: w(nz+1,nc1)
 integer :: c, k, l
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > n_relax_zone) then
   !$acc loop vector
   do k = 2, nz
    w(k,c) = 0.0
   end do
  end if
 end do
 end subroutine

 ! atm_bdy_reset_speczone_values (after the microphysics): theta_m and rtheta_p of the specified zone from the boundary values at the end of the step.
 subroutine reset_speczone_values(mask_cell, drv_rt, drv_rho, rtheta_base, theta_m, rtheta_p)
 integer, intent(in) :: mask_cell(nc1)
 real, intent(in) :: drv_rt(nz,n_slot_cell), drv_rho(nz,n_slot_cell), rtheta_base(nz,nc1)
 real, intent(inout) :: theta_m(nz,nc1), rtheta_p(nz,nc1)
 integer :: c, k, l
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    theta_m(k,c) = drv_rt(k,slot_cell(c))/drv_rho(k,slot_cell(c))
    rtheta_p(k,c) = drv_rt(k,slot_cell(c)) - rtheta_base(k,c)
   end do
  end if
 end do
 end subroutine

 ! atm_bdy_adjust_scalars_work (after each transport stage, dt_rk = the stage's time step): relaxation zone: filter and Rayleigh damping toward the
 ! boundary values, computed from the values before the update; specified zone: the boundary values. scalars_tmp keeps the relaxation values until
 ! every cell is done, because each cell reads its neighbours' old values (a view of dyn_scalars' edge_work, free between the
 ! transport stages: written at every cell of mask > 1 before it is read there).
 subroutine adjust_scalars(dt_rk, mask_cell, reg_cell, n_edges_on_cell, edges_on_cell, sign_cell, cells_on_edge, dv_edge, inv_dc, drv, &
                           scalars, scalars_tmp)
 real, intent(in) :: dt_rk
 integer, intent(in) :: mask_cell(nc1), n_edges_on_cell(nc1), edges_on_cell(maxedges,nc1), cells_on_edge(2,ne1)
 real, intent(in) :: reg_cell(nc1), sign_cell(maxedges,nc1), dv_edge(ne1), inv_dc(ne1), drv(num_scalars,nz,n_slot_cell)
 real, intent(inout) :: scalars(num_scalars,nz,nc1), scalars_tmp(num_scalars,nz,nc1)
 real :: edge_sign, laplacian_filter_coef, rayleigh_damping_coef, filter_flux
 integer :: c, e, i, k, s, cell1, cell2, l, s0, s1, s2
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(s0, s1, s2) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  s0 = slot_cell(c)
  if (mask_cell(c) > 1 .and. mask_cell(c) <= n_relax_zone) then
   laplacian_filter_coef = dt_rk*(real(mask_cell(c)) - 1.)/real(n_relax_zone)/(10.*dt*reg_cell(c))
   rayleigh_damping_coef = laplacian_filter_coef/5.0
   !$acc loop vector
   do k = 1, nz
    do s = 1, num_scalars
     scalars_tmp(s,k,c) = scalars(s,k,c)
    end do
   end do
   do i = 1, n_edges_on_cell(c)
    e = edges_on_cell(i,c)
    edge_sign = sign_cell(i,c) * dv_edge(e) * inv_dc(e) * laplacian_filter_coef
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    s1 = slot_cell(cell1)
    s2 = slot_cell(cell2)
    !$acc loop vector
    do k = 1, nz
     do s = 1, num_scalars
      filter_flux = edge_sign*(   (scalars(s,k,cell2)-drv(s,k,s2)) - (scalars(s,k,cell1)-drv(s,k,s1)) )
      scalars_tmp(s,k,c) = scalars_tmp(s,k,c) + filter_flux
     end do
    end do
   end do
   !$acc loop vector
   do k = 1, nz
    do s = 1, num_scalars
     scalars_tmp(s,k,c) = scalars_tmp(s,k,c) - rayleigh_damping_coef * (scalars(s,k,c)-drv(s,k,s0))
    end do
   end do
  else if (mask_cell(c) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    do s = 1, num_scalars
     scalars_tmp(s,k,c) = drv(s,k,s0)
    end do
   end do
  end if
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > 1) then
   !$acc loop vector
   do k = 1, nz
    do s = 1, num_scalars
     scalars(s,k,c) = scalars_tmp(s,k,c)
    end do
   end do
  end if
 end do
 end subroutine

 ! atm_bdy_set_scalars_work (end of the step): the specified zone takes the boundary values.
 subroutine set_scalars(mask_cell, drv, scalars)
 integer, intent(in) :: mask_cell(nc1)
 real, intent(in) :: drv(num_scalars,nz,n_slot_cell)
 real, intent(inout) :: scalars(num_scalars,nz,nc1)
 integer :: c, k, s, l
 call whole_mesh_lists()
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do l = 1, n_bdy_cell
  c = bdy_cell(l)
  if (mask_cell(c) > n_relax_zone) then
   !$acc loop vector
   do k = 1, nz
    do s = 1, num_scalars
     scalars(s,k,c) = drv(s,k,slot_cell(c))
    end do
   end do
  end if
 end do
 end subroutine
end module
