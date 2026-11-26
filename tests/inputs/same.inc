 ! Bit-for-bit comparisons of the input objects, shared by checks.f90 and run_inputs.f90 (contained procedures).
 logical function er1(a, b)
 real, intent(in), allocatable :: a(:), b(:)
 er1 = allocated(a) .and. allocated(b)
 if (er1) er1 = size(a) == size(b)
 if (er1) er1 = all(transfer(a, [0_int32]) == transfer(b, [0_int32]))
 end function

 logical function er2(a, b)
 real, intent(in), allocatable :: a(:,:), b(:,:)
 er2 = allocated(a) .and. allocated(b)
 if (er2) er2 = all(shape(a) == shape(b))
 if (er2) er2 = all(transfer(a, [0_int32]) == transfer(b, [0_int32]))
 end function

 logical function er3(a, b)
 real, intent(in), allocatable :: a(:,:,:), b(:,:,:)
 er3 = allocated(a) .and. allocated(b)
 if (er3) er3 = all(shape(a) == shape(b))
 if (er3) er3 = all(transfer(a, [0_int32]) == transfer(b, [0_int32]))
 end function

 logical function ei1(a, b)
 integer, intent(in), allocatable :: a(:), b(:)
 ei1 = allocated(a) .and. allocated(b)
 if (ei1) ei1 = size(a) == size(b)
 if (ei1) ei1 = all(a == b)
 end function

 logical function ei2(a, b)
 integer, intent(in), allocatable :: a(:,:), b(:,:)
 ei2 = allocated(a) .and. allocated(b)
 if (ei2) ei2 = all(shape(a) == shape(b))
 if (ei2) ei2 = all(a == b)
 end function

 ! Bit for bit, every component.
 logical function same_mesh(a, b)
 type(dyn_mesh_type), intent(in) :: a, b
 same_mesh = er3(a%coeffs_reconstruct, b%coeffs_reconstruct) .and. er1(a%latCell, b%latCell) .and. er1(a%lonCell, b%lonCell) &
  .and. er2(a%edgeNormalVectors, b%edgeNormalVectors) .and. er1(a%xCell, b%xCell) .and. er1(a%yCell, b%yCell) &
  .and. er1(a%zCell, b%zCell) .and. er1(a%areaCell, b%areaCell) .and. er1(a%meshDensity, b%meshDensity) &
  .and. er1(a%dvEdge, b%dvEdge) .and. er1(a%dcEdge, b%dcEdge) .and. er1(a%xVertex, b%xVertex) .and. er1(a%yVertex, b%yVertex) &
  .and. er1(a%zVertex, b%zVertex) .and. er1(a%areaTriangle, b%areaTriangle) .and. ei1(a%nEdgesOnCell, b%nEdgesOnCell) &
  .and. ei2(a%cellsOnCell, b%cellsOnCell) .and. ei2(a%edgesOnCell, b%edgesOnCell) .and. ei2(a%verticesOnCell, b%verticesOnCell) &
  .and. ei2(a%cellsOnEdge, b%cellsOnEdge) .and. ei2(a%verticesOnEdge, b%verticesOnEdge) &
  .and. ei2(a%edgesOnVertex, b%edgesOnVertex) .and. ei2(a%cellsOnVertex, b%cellsOnVertex) &
  .and. ei1(a%nEdgesOnEdge, b%nEdgesOnEdge) .and. ei2(a%edgesOnEdge, b%edgesOnEdge) .and. er2(a%weightsOnEdge, b%weightsOnEdge) &
  .and. er2(a%kiteAreasOnVertex, b%kiteAreasOnVertex) .and. er1(a%fEdge, b%fEdge) .and. er1(a%fVertex, b%fVertex) &
  .and. er1(a%angleEdge, b%angleEdge) .and. er2(a%zxu, b%zxu) .and. er1(a%u_init, b%u_init) .and. er1(a%v_init, b%v_init) &
  .and. transfer(a%cf1, 0_int32) == transfer(b%cf1, 0_int32) .and. transfer(a%cf2, 0_int32) == transfer(b%cf2, 0_int32) &
  .and. transfer(a%cf3, 0_int32) == transfer(b%cf3, 0_int32) .and. er3(a%deriv_two, b%deriv_two) .and. er3(a%zb, b%zb) &
  .and. er3(a%zb3, b%zb3) .and. er2(a%zgrid, b%zgrid) .and. er2(a%zz, b%zz) .and. er1(a%rdzw, b%rdzw) .and. er1(a%rdzu, b%rdzu) &
  .and. er1(a%fzm, b%fzm) .and. er1(a%fzp, b%fzp) .and. ei1(a%bdyMaskCell, b%bdyMaskCell) .and. ei1(a%bdyMaskEdge, b%bdyMaskEdge) &
  .and. ei1(a%bdyMaskVertex, b%bdyMaskVertex) .and. er2(a%dss, b%dss) .and. er1(a%xEdge, b%xEdge) .and. er1(a%yEdge, b%yEdge) &
  .and. er1(a%zEdge, b%zEdge)
 end function

 ! ice: also qi qs qg ni nr.
 logical function same_dyn(a, b, ice)
 type(cold_dyn_type), intent(in) :: a, b
 logical, intent(in) :: ice
 same_dyn = er2(a%u, b%u) .and. er2(a%w, b%w) .and. er2(a%theta, b%theta) .and. er2(a%rho, b%rho) &
  .and. er2(a%rho_base, b%rho_base) .and. er2(a%theta_base, b%theta_base) .and. er2(a%qv, b%qv) .and. er2(a%qc, b%qc) &
  .and. er2(a%qr, b%qr)
 if (ice) same_dyn = same_dyn .and. er2(a%qi, b%qi) .and. er2(a%qs, b%qs) .and. er2(a%qg, b%qg) .and. er2(a%ni, b%ni) &
  .and. er2(a%nr, b%nr)
 end function

 ! Everything of cold_inputs but the paths.
 logical function same_inp(a, b)
 type(cold_inputs), intent(in) :: a, b
 same_inp = allocated(a%cell) .and. allocated(b%cell)
 if (same_inp) same_inp = size(a%cell) == size(b%cell)
 if (same_inp) same_inp = all(transfer(a%cell, [0_int32]) == transfer(b%cell, [0_int32]))
 same_inp = same_inp .and. er2(a%orography, b%orography) .and. er1(a%lat_edge, b%lat_edge) .and. er1(a%lon_edge, b%lon_edge) &
  .and. er1(a%lat_vertex, b%lat_vertex) .and. er1(a%lon_vertex, b%lon_vertex) .and. er1(a%dzu, b%dzu) &
  .and. er2(a%relhum, b%relhum) .and. er1(a%u10, b%u10) .and. er1(a%v10, b%v10) .and. er1(a%q2, b%q2) .and. er1(a%t2m, b%t2m) &
  .and. er1(a%precipw, b%precipw) .and. er1(a%h_oml_initial, b%h_oml_initial) .and. a%nominal_min_dc == b%nominal_min_dc &
  .and. a%mminlu == b%mminlu .and. a%isice_lu == b%isice_lu .and. a%iswater_lu == b%iswater_lu &
  .and. a%start_time == b%start_time .and. a%initial_time == b%initial_time
 end function
