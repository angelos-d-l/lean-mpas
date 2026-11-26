! lean_mpas reads an MPAS run directory's inputs itself: the mesh, the initial state, the column and orography fields
! of the init file, through lean_nc, with MPAS's rules and messages, and checks what it read.
module lean_inputs
 use noahmp_state, only: set_noahmp_opt_sfc
 use iso_fortran_env, only: int64
 use ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
 use config, only: ncells, nedges, nvertices, nz, nc1, ne1, nv1, maxedges, set_mesh_sizes, max_levels
 use levels, only: fixed_levels
 use lean_namelist, only: run_config
 use lean_nc
 use lean_messages, only: message_list, add_message, int_text, real_text, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT
 use lean_input_fields
 use lean_lbc
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use cold_column, only: cold_cell
 use run_settings, only: set_time_step, set_radiation_intervals, set_dyn_config, set_apply_lbcs, set_apvm_upwinding
 use schemes, only: set_schemes, micro_on, lsm_on, lw_on, sw_on, moist_physics
 implicit none
 private
 public :: seek_record   ! also used by the memory check, to find the start record
 public :: configure
 public :: mesh_sizes, mesh_attributes, cold_inputs, read_mesh_sizes, read_mesh_inputs, read_state_inputs
 public :: check_inputs, effective_len_disp, apply_run_config, check_run_tables, thompson_table_order, inputs_failed
 ! lean_lbc, re-exported
 public :: lbc_request, lbc_reader, LBC_LATEST_BEFORE, LBC_EARLIEST_STRICTLY_AFTER, xtime_seconds
 public :: lbc_open, lbc_due, lbc_next

 real, parameter :: earth_radius = 6371229.   ! init_atmosphere's sphere_radius (dyn_config)
 character(len=*), parameter :: soil_why = 'lean_mpas runs Noah-MP with 4 soil layers'
 ! The four Thompson tables and the length in bytes of their first record (module_mp_thompson.F's reads)
 character(len=30), parameter :: thompson_table(4) = [character(len=30) :: 'MP_THOMPSON_QIautQS_DATA.DBL', &
  'MP_THOMPSON_QRacrQG_DATA.DBL', 'MP_THOMPSON_QRacrQS_DATA.DBL', 'MP_THOMPSON_freezeH2O_DATA.DBL']
 integer, parameter :: thompson_record(4) = [28160, 8586368, 2759904, 27106200]
 ! The other physics tables lean reads from the run directory, as MPAS does
 character(len=16), parameter :: other_table(7) = [character(len=16) :: 'LANDUSE.TBL', 'OZONE_DAT.TBL', 'OZONE_LAT.TBL', &
  'OZONE_PLEV.TBL', 'RRTMG_LW_DATA', 'RRTMG_SW_DATA', 'NoahmpTable.TBL']
 character(len=*), parameter :: table_why = ' is missing: lean_mpas reads MPAS''s physics tables from the run directory'

 type mesh_sizes            ! the mesh file's dimensions, from which the model arrays are allocated
  integer :: ncells = 0, nedges = 0, nvertices = 0, maxedges = 0, nvertlevels = 0, nsoillevels = 0
 end type

 type mesh_attributes       ! read as framework/mpas_bootstrapping.F:283-340 reads them; the output streams write them
  logical :: on_a_sphere = .true.
  logical :: is_periodic = .false.
  real :: x_period = 0., y_period = 0.
  real :: sphere_radius = 1.0
  character(len=:), allocatable :: parent_id    ! file_id // new_line('A') // parent_id, as :327-331
  character(len=:), allocatable :: mesh_spec
 end type

 type cold_inputs           ! what a start from the init file reads besides the mesh (dyn_mesh_type) and the state
                            ! (cold_dyn_type)
  character(len=:), allocatable :: mesh_path, init_path   ! messages name the file a field came from
  type(cold_cell), allocatable :: cell(:)       ! (ncells): src/cold_column.f90's record
  real, allocatable :: orography(:,:)           ! (10,ncells): var2d con oa1 oa2 oa3 oa4 ol1 ol2 ol3 ol4 (gwdo_state.f90 read_orography)
  real, allocatable :: lat_edge(:), lon_edge(:), lat_vertex(:), lon_vertex(:)   ! (ne1), (nv1), ghost slot 0
  real, allocatable :: dzu(:)                   ! (nz), the mesh file's; for config_microphysics_top
  real, allocatable :: relhum(:,:)              ! (nz,nc1): the input stream's t=0 diagnostics, ghost slot 0
  real, allocatable :: u10(:), v10(:), q2(:), t2m(:), precipw(:), h_oml_initial(:)   ! (nc1), likewise
  real :: cf_time = 0.
  real :: nominal_min_dc = 0.                   ! 0 when the file has none
  character(len=64) :: mminlu = ''
  integer :: isice_lu = 0, iswater_lu = 0
  character(len=19) :: start_time = '', initial_time = ''   ! YYYY-MM-DD_hh:mm:ss
  type(mesh_sizes) :: sizes
  type(mesh_attributes) :: attributes
 end type

contains

 ! The dimensions of a file; a missing one is 0. A CRIT line if the file cannot be opened.
 subroutine read_mesh_sizes(path, sizes, msgs)
 character(*), intent(in) :: path
 type(mesh_sizes), intent(out) :: sizes
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 call open_input(f, path, 'input', msgs)
 if (.not. f%opened) return
 call file_sizes(f, sizes)
 call close_input(f)
 end subroutine

 ! Sets the model's mesh sizes after checking the limits of this executable. The fixed dimensions of the file
 ! (TWO, R3, ...) are checked by sizes_fit before this call.
 subroutine configure(sizes, msgs)
 type(mesh_sizes), intent(in) :: sizes
 type(message_list), intent(inout) :: msgs
 if (sizes%nvertlevels > max_levels) error stop 'mesh levels exceed max_levels'
 if (sizes%nsoillevels /= 0 .and. sizes%nsoillevels /= 4) call add_message(msgs, LOG_ERR, 'nSoilLevels: '//soil_why)
 if (sizes%maxedges < 1 .or. sizes%maxedges > 10) &
  call add_message(msgs, LOG_ERR, 'maxEdges: lean_mpas reads at most 10 neighbour slots and uses 8')
 if (min(sizes%ncells, sizes%nedges, sizes%nvertices) < 1) &
  call add_message(msgs, LOG_ERR, 'mesh dimensions must be positive')
 if (inputs_failed(msgs)) return
 call set_mesh_sizes(sizes%ncells, sizes%nedges, sizes%nvertices, sizes%nvertlevels)
 end subroutine

 subroutine require_mesh_size(f, name, msgs, missing)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 type(message_list), intent(inout) :: msgs
 logical, intent(inout) :: missing
 integer :: n
 if (nc_inq_dim(f%nc, name, n) /= NC_NOERR) then
  call missing_dimension(f, name, msgs)
  missing = .true.
 else if (n < 1) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': '//name//' must be positive')
 end if
 end subroutine

 subroutine file_sizes(f, sizes)
 type(input_file), intent(in) :: f
 type(mesh_sizes), intent(out) :: sizes
 sizes%ncells = dimension_length(f, 'nCells')
 sizes%nedges = dimension_length(f, 'nEdges')
 sizes%nvertices = dimension_length(f, 'nVertices')
 sizes%maxedges = dimension_length(f, 'maxEdges')
 sizes%nvertlevels = dimension_length(f, 'nVertLevels')
 sizes%nsoillevels = dimension_length(f, 'nSoilLevels')
 end subroutine

 ! The mesh of the input stream's file (or the invariant stream's), where MPAS bootstraps
 ! (framework/mpas_bootstrapping.F): the attributes, the sizes against this build, every component of dyn_mesh_type
 ! but u_init and v_init (read_state_inputs), the mesh fields the cold start and the checks use, the index order.
 subroutine read_mesh_inputs(mesh_path, m, inp, msgs)
 character(*), intent(in) :: mesh_path
 type(dyn_mesh_type), intent(out) :: m
 type(cold_inputs), intent(out) :: inp
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 inp%mesh_path = trim(mesh_path)
 f%path = mesh_path
 f%role = 'input'
 ! mpas_bootstrapping.F:158-165
 if (nc_open(f%nc, trim(mesh_path)) /= NC_NOERR) then
  call add_message(msgs, LOG_CRIT, 'Could not open input file '''//trim(mesh_path)//''' to read mesh fields')
  return
 end if
 f%opened = .true.
 if (dimension_length(f, 'Time') > 0) f%record = 1
 call add_message(msgs, LOG_OUT, 'Bootstrapping framework with mesh fields from input file '''//trim(mesh_path)//'''')
 call read_attributes(f, inp%attributes, msgs)
 call file_sizes(f, inp%sizes)
 if (sizes_fit(f, msgs)) then
  call configure(inp%sizes, msgs)
  if (inputs_failed(msgs)) then
   call close_input(f)
   return
  end if
  call read_mesh(f, m, msgs)
  call read_mesh_extras(f, inp, msgs)
  call check_index_order(f, 'indexToCellID', ncells, msgs)
  call check_index_order(f, 'indexToEdgeID', nedges, msgs)
  call check_index_order(f, 'indexToVertexID', nvertices, msgs)
 end if
 call close_input(f)
 end subroutine

 ! framework/mpas_bootstrapping.F:283-340, with its warnings and defaults; lean stops on a mesh that is not on a sphere
 ! or not on the earth's radius.
 subroutine read_attributes(f, a, msgs)
 type(input_file), intent(in) :: f
 type(mesh_attributes), intent(out) :: a
 type(message_list), intent(inout) :: msgs
 character(len=:), allocatable :: text, file_id, parent_id, sphere
 real :: radius
 sphere = ''
 if (nc_get_att(f%nc, '', 'on_a_sphere', text) /= NC_NOERR) then
  call add_message(msgs, LOG_WARN, 'Attribute on_a_sphere not found in '//trim(f%path))
  call add_message(msgs, LOG_WARN, '   Setting on_a_sphere to ''YES''')
  a%on_a_sphere = .true.
 else
  a%on_a_sphere = index(text, 'YES') /= 0
  sphere = text
 end if
 if (nc_get_att(f%nc, '', 'sphere_radius', radius) /= NC_NOERR) then
  call add_message(msgs, LOG_WARN, 'Attribute sphere_radius not found in '//trim(f%path))
  call add_message(msgs, LOG_WARN, '   Setting sphere_radius to 1.0')
  a%sphere_radius = 1.0
 else
  a%sphere_radius = radius
 end if
 if (nc_get_att(f%nc, '', 'file_id', file_id) /= NC_NOERR) then
  call add_message(msgs, LOG_WARN, 'Attribute file_id not found in '//trim(f%path))
  call add_message(msgs, LOG_WARN, '  Using '''' for the previous file_id.')
  file_id = ''
 end if
 if (nc_get_att(f%nc, '', 'parent_id', parent_id) /= NC_NOERR) then
  call add_message(msgs, LOG_WARN, 'Attribute parent_id not found in '//trim(f%path))
  call add_message(msgs, LOG_WARN, '  Setting parent_id to ''''')
  parent_id = ''
 end if
 if (len_trim(file_id) == 0) then
  a%parent_id = parent_id
 else
  a%parent_id = trim(file_id)//new_line('A')//trim(parent_id)
 end if
 if (nc_get_att(f%nc, '', 'mesh_spec', text) /= NC_NOERR) then
  call add_message(msgs, LOG_WARN, 'Attribute mesh_spec not found in '//trim(f%path))
  call add_message(msgs, LOG_WARN, '  Setting mesh_spec to ''0.0''')
  a%mesh_spec = '0.0'
 else
  a%mesh_spec = text
 end if
 ! MPAS framework/mpas_bootstrapping.F:345-380: sphere defaults; periods only on a periodic plane.
 if (.not.a%on_a_sphere) then
  if (nc_get_att(f%nc,'','is_periodic',text)/=NC_NOERR) then
   call add_message(msgs,LOG_WARN,'is_periodic attribute not found in '//trim(f%path))
   call add_message(msgs,LOG_WARN,'   Setting is_periodic to .false.')
  else
   a%is_periodic=index(text,'YES')/=0
  endif
 endif
 if (a%is_periodic) then
  if (nc_get_att(f%nc,'','x_period',a%x_period)/=NC_NOERR) &
   call add_message(msgs,LOG_ERR,"Required attribute 'x_period' missing from periodic input mesh. Exiting...")
  if (nc_get_att(f%nc,'','y_period',a%y_period)/=NC_NOERR) &
   call add_message(msgs,LOG_ERR,"Required attribute 'y_period' missing from periodic input mesh. Exiting...")
 endif
 if (.not. a%on_a_sphere) call add_message(msgs, LOG_ERR, trim(f%path)//': on_a_sphere = '''//trim(sphere)// &
                                           ''': lean_mpas runs spherical meshes only')
 if (a%sphere_radius /= earth_radius) &
  call add_message(msgs, LOG_ERR, trim(f%path)//': sphere_radius = '//real_text(a%sphere_radius)// &
                   ': lean_mpas needs the mesh on the earth''s radius, 6371229 m, as init_atmosphere writes it')
 end subroutine

 ! The file's dimensions against the mesh file's and this executable's limits. A dimension the file lacks gives MPAS's
 ! block (framework/mpas_bootstrapping.F:630-640), lean's line, and MPAS's CRIT after the last one.
 logical function sizes_fit(f, msgs)
 type(input_file), intent(in) :: f
 type(message_list), intent(inout) :: msgs
 character(len=*), parameter :: slots_why = 'lean_mpas reads at most 10 neighbour slots and uses 8'
 logical :: missing
 integer :: before
 before = count_problems(msgs)
 missing = .false.
 call require_mesh_size(f, 'nCells', msgs, missing)
 call require_mesh_size(f, 'nEdges', msgs, missing)
 call require_mesh_size(f, 'nVertices', msgs, missing)
 call one_size(f, 'maxEdges', 10, slots_why, .true., missing, msgs)
 call one_size(f, 'maxEdges2', 20, slots_why, .true., missing, msgs)
 call one_size(f, 'TWO', 2, 'MPAS 8.4.2 writes 2', .false., missing, msgs)
 call one_size(f, 'vertexDegree', 3, 'MPAS 8.4.2 writes 3', .false., missing, msgs)
 call one_size(f, 'R3', 3, 'MPAS 8.4.2 writes 3', .false., missing, msgs)
 call one_size(f, 'nMonths', 12, 'MPAS 8.4.2 writes 12', .false., missing, msgs)
 call one_size(f, 'FIFTEEN', 15, 'MPAS 8.4.2 writes 15', .false., missing, msgs)
 call require_mesh_size(f, 'nVertLevels', msgs, missing)
 if (dimension_length(f, 'nVertLevels') > max_levels) &
  call add_message(msgs, LOG_ERR, trim(f%path)//': nVertLevels = '//int_text(dimension_length(f, 'nVertLevels'))// &
                   '; this executable holds columns of at most '//int_text(max_levels)//' levels (max_levels in src/config.f90)')
 if (fixed_levels > 0 .and. dimension_length(f, 'nVertLevels') > 0 .and. dimension_length(f, 'nVertLevels') /= fixed_levels) &
  call add_message(msgs, LOG_ERR, trim(f%path)//': nVertLevels = '//int_text(dimension_length(f, 'nVertLevels'))// &
                   '; this executable was built for '//int_text(fixed_levels)//' levels only (./lean build --levels '// &
                   int_text(fixed_levels)//'): build it with --levels '//int_text(dimension_length(f, 'nVertLevels'))// &
                   ', or without --levels for any count')
 call one_size(f, 'nVertLevelsP1', dimension_length(f, 'nVertLevels') + 1, 'MPAS 8.4.2 writes nVertLevels + 1', .false., &
               missing, msgs)
 ! nSoilLevels is not asked of the mesh file: no invariant field has it (Registry.xml:499-600), so an invariant
 ! stream's file lacks it and MPAS runs; read_state_inputs checks it in the input stream's file.
 if (missing) call add_message(msgs, LOG_CRIT, 'Please check the input file(s) to be read by the '''//trim(f%role)// &
                               ''' input stream.')
 sizes_fit = count_problems(msgs) == before .and. .not. missing
 end function

 ! Every component of dyn_mesh_type, with its dimensions and neighbour remaps.
 subroutine read_mesh(f, m, msgs)
 type(input_file), intent(inout) :: f
 type(dyn_mesh_type), intent(inout) :: m
 type(message_list), intent(inout) :: msgs
 allocate(m%coeffs_reconstruct(3,maxedges,nc1), m%latCell(nc1), m%lonCell(nc1), m%edgeNormalVectors(3,ne1), m%xCell(nc1), &
          m%yCell(nc1), m%zCell(nc1), m%areaCell(nc1), m%meshDensity(nc1), m%dvEdge(ne1), m%dcEdge(ne1), m%xVertex(nv1), &
          m%yVertex(nv1), m%zVertex(nv1), m%areaTriangle(nv1), m%nEdgesOnCell(nc1), m%cellsOnCell(maxedges,nc1), &
          m%edgesOnCell(maxedges,nc1), m%verticesOnCell(maxedges,nc1), m%cellsOnEdge(2,ne1), m%verticesOnEdge(2,ne1), &
          m%edgesOnVertex(3,nv1), m%cellsOnVertex(3,nv1), m%nEdgesOnEdge(ne1), m%edgesOnEdge(2*maxedges,ne1), &
          m%weightsOnEdge(2*maxedges,ne1), m%kiteAreasOnVertex(3,nv1), m%fEdge(ne1), m%fVertex(nv1), m%angleEdge(ne1), &
          m%zxu(nz,ne1), m%u_init(nz), m%v_init(nz), m%deriv_two(15,2,ne1), m%zb(nz+1,2,ne1), m%zb3(nz+1,2,ne1), &
          m%zgrid(nz+1,nc1), m%zz(nz,nc1), m%rdzw(nz), m%rdzu(nz), m%fzm(nz), m%fzp(nz), m%bdyMaskCell(nc1), &
          m%bdyMaskEdge(ne1), m%bdyMaskVertex(nv1), m%dss(nz,nc1), m%xEdge(ne1), m%yEdge(ne1), m%zEdge(ne1))
 m%u_init = 0.
 m%v_init = 0.
 call read_field(f, 'coeffs_reconstruct', m%coeffs_reconstruct, msgs)
 call read_field(f, 'latCell', m%latCell, msgs)
 call read_field(f, 'lonCell', m%lonCell, msgs)
 call read_field(f, 'edgeNormalVectors', m%edgeNormalVectors, msgs)
 call read_field(f, 'xCell', m%xCell, msgs)
 call read_field(f, 'yCell', m%yCell, msgs)
 call read_field(f, 'zCell', m%zCell, msgs)
 call read_field(f, 'areaCell', m%areaCell, msgs)
 call read_field(f, 'meshDensity', m%meshDensity, msgs)
 call read_field(f, 'dvEdge', m%dvEdge, msgs)
 call read_field(f, 'dcEdge', m%dcEdge, msgs)
 call read_field(f, 'xVertex', m%xVertex, msgs)
 call read_field(f, 'yVertex', m%yVertex, msgs)
 call read_field(f, 'zVertex', m%zVertex, msgs)
 call read_field(f, 'areaTriangle', m%areaTriangle, msgs)
 call read_field(f, 'nEdgesOnCell', m%nEdgesOnCell, msgs)
 call read_field(f, 'cellsOnCell', m%cellsOnCell, msgs)
 call remap_neighbours(f, 'cellsOnCell', m%cellsOnCell, ncells, msgs, m%nEdgesOnCell)
 call read_field(f, 'edgesOnCell', m%edgesOnCell, msgs)
 call remap_neighbours(f, 'edgesOnCell', m%edgesOnCell, nedges, msgs, m%nEdgesOnCell)
 call read_field(f, 'verticesOnCell', m%verticesOnCell, msgs)
 call remap_neighbours(f, 'verticesOnCell', m%verticesOnCell, nvertices, msgs, m%nEdgesOnCell)
 call read_field(f, 'cellsOnEdge', m%cellsOnEdge, msgs)
 call remap_neighbours(f, 'cellsOnEdge', m%cellsOnEdge, ncells, msgs)
 call read_field(f, 'verticesOnEdge', m%verticesOnEdge, msgs)
 call remap_neighbours(f, 'verticesOnEdge', m%verticesOnEdge, nvertices, msgs)
 call read_field(f, 'edgesOnVertex', m%edgesOnVertex, msgs)
 call remap_neighbours(f, 'edgesOnVertex', m%edgesOnVertex, nedges, msgs)
 call read_field(f, 'cellsOnVertex', m%cellsOnVertex, msgs)
 call remap_neighbours(f, 'cellsOnVertex', m%cellsOnVertex, ncells, msgs)
 call read_field(f, 'nEdgesOnEdge', m%nEdgesOnEdge, msgs)
 call read_field(f, 'edgesOnEdge', m%edgesOnEdge, msgs)
 call remap_neighbours(f, 'edgesOnEdge', m%edgesOnEdge, nedges, msgs, m%nEdgesOnEdge)
 call read_field(f, 'weightsOnEdge', m%weightsOnEdge, msgs)
 call read_field(f, 'kiteAreasOnVertex', m%kiteAreasOnVertex, msgs)
 call read_field(f, 'fEdge', m%fEdge, msgs)
 call read_field(f, 'fVertex', m%fVertex, msgs)
 call read_field(f, 'angleEdge', m%angleEdge, msgs)
 call read_field(f, 'zxu', m%zxu, msgs)
 call read_field(f, 'cf1', m%cf1, msgs)
 call read_field(f, 'cf2', m%cf2, msgs)
 call read_field(f, 'cf3', m%cf3, msgs)
 call read_field(f, 'deriv_two', m%deriv_two, msgs)
 call read_field(f, 'zb', m%zb, msgs)
 call read_field(f, 'zb3', m%zb3, msgs)
 call read_field(f, 'zgrid', m%zgrid, msgs)
 call read_field(f, 'zz', m%zz, msgs)
 call read_field(f, 'rdzw', m%rdzw, msgs)
 call read_field(f, 'rdzu', m%rdzu, msgs)
 call read_field(f, 'fzm', m%fzm, msgs)
 call read_field(f, 'fzp', m%fzp, msgs)
 call read_field(f, 'bdyMaskCell', m%bdyMaskCell, msgs)
 call read_field(f, 'bdyMaskEdge', m%bdyMaskEdge, msgs)
 call read_field(f, 'bdyMaskVertex', m%bdyMaskVertex, msgs)
 call read_field(f, 'dss', m%dss, msgs)
 call read_field(f, 'xEdge', m%xEdge, msgs)
 call read_field(f, 'yEdge', m%yEdge, msgs)
 call read_field(f, 'zEdge', m%zEdge, msgs)
 end subroutine

 ! Mesh fields MPAS reads besides those of dyn_mesh_type.
 subroutine read_mesh_extras(f, inp, msgs)
 type(input_file), intent(inout) :: f
 type(cold_inputs), intent(inout) :: inp
 type(message_list), intent(inout) :: msgs
 allocate(inp%lat_edge(ne1), inp%lon_edge(ne1), inp%lat_vertex(nv1), inp%lon_vertex(nv1), inp%dzu(nz))
 call read_field(f, 'latEdge', inp%lat_edge, msgs)
 call read_field(f, 'lonEdge', inp%lon_edge, msgs)
 call read_field(f, 'latVertex', inp%lat_vertex, msgs)
 call read_field(f, 'lonVertex', inp%lon_vertex, msgs)
 call read_field(f, 'dzu', inp%dzu, msgs)
 inp%nominal_min_dc = 0.
 if (has_variable(f, 'nominalMinDc')) call read_field(f, 'nominalMinDc', inp%nominal_min_dc, msgs)
 call read_text(f, 'mminlu', inp%mminlu, msgs)
 call read_field(f, 'isice_lu', inp%isice_lu, msgs)
 call read_field(f, 'iswater_lu', inp%iswater_lu, msgs)
 end subroutine

 ! A character variable without a Time dimension (lean_nc reads NUL padding as blanks).
 subroutine read_text(f, name, text, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 character(*), intent(out) :: text
 type(message_list), intent(inout) :: msgs
 character(len=256) :: buffer     ! lean_nc wants room for the file's StrLen (64 in MPAS's files)
 text = ''
 buffer = ''
 if (.not. has_variable(f, name)) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': no variable '//name//' ('//trim(f%role)//' stream)')
 else if (nc_get(f%nc, name, buffer) /= NC_NOERR) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': '//name//': '//nc_reason(name))
 else
  text = buffer
 end if
 end subroutine

 ! lean uses positions as ids, so the file's ids must be 1..n in order.
 subroutine check_index_order(f, name, n, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 integer, intent(in) :: n
 type(message_list), intent(inout) :: msgs
 integer, allocatable :: id(:)
 integer :: i, before
 allocate(id(n))
 before = count_problems(msgs)
 call read_field(f, name, id, msgs)
 if (count_problems(msgs) /= before) return
 do i = 1, n
  if (id(i) /= i) then
   call add_message(msgs, LOG_ERR, trim(f%path)//': '//name//' is not 1..'//int_text(n)// &
                    ': lean_mpas needs the mesh in its own index order')
   return
  end if
 end do
 end subroutine
 ! The record of an opened file a read at `when` takes: the one whose xtime equals it (exact, the input stream:
 ! framework/mpas_stream_manager.F:3531) or the nearest (the invariant stream, core_atmosphere/mpas_atm_core.F:118),
 ! as MPAS_seekStream finds it (framework/mpas_io_streams.F:142-339). A file without Time records or xtime reads its
 ! first record, with MPAS's warning. An xtime that is not a time stops before the seek; found is then .false., and
 ! also when no record has the exact time.
 subroutine seek_record(f, when, exact, msgs, found, stamp)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: when
 logical, intent(in) :: exact
 type(message_list), intent(inout) :: msgs
 logical, intent(out) :: found
 character(len=19), intent(out) :: stamp
 character(len=256), allocatable :: xtimes(:)
 integer(int64) :: gap, best
 integer :: n, i
 found = .false.
 stamp = when
 n = dimension_length(f, 'Time')
 if (n <= 0 .or. .not. has_variable(f, 'xtime')) then
  call add_message(msgs, LOG_WARN, 'File '//trim(f%path)//' does not contain a seekable xtime variable. '// &
                   'Forcing a read of the first time record.')
  f%record = min(n, 1)
  found = .true.
  return
 end if
 allocate(xtimes(n))
 if (nc_get(f%nc, 'xtime', xtimes) /= NC_NOERR) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': xtime: '//nc_reason('xtime'))
  return
 end if
 do i = 1, n
  if (xtime_seconds(xtimes(i), xtimes(i)) /= 0) then
   call add_message(msgs, LOG_ERR, trim(f%path)//': xtime of record '//int_text(i)//' is not a time: '''// &
                    trim(xtimes(i))//'''')
   return
  end if
 end do
 f%record = 0
 best = 864000000_int64            ! MPAS_seekStream starts from 10000 days
 do i = 1, n
  gap = abs(xtime_seconds(xtimes(i), when))
  if (exact) then
   if (gap == 0) f%record = i      ! the last one, as MPAS's loop
  else if (gap < best) then
   best = gap
   f%record = i
  end if
 end do
 if (f%record == 0) then
  call add_message(msgs, LOG_ERR, 'File '//trim(f%path)//' does not contain the time '//trim(when))
  return
 end if
 stamp = xtimes(f%record)(1:19)
 found = .true.
 end subroutine

 ! A field MPAS's stream read leaves at 0 with its warning when the file lacks it (framework/mpas_io.F:875).
 subroutine read_or_zero_1(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:)
 type(message_list), intent(inout) :: msgs
 a = 0.
 if (has_variable(f, name)) then
  call read_field(f, name, a, msgs)
 else
  call add_message(msgs, LOG_WARN, 'Variable '//name//' not in input file.')
 end if
 end subroutine

 subroutine read_or_zero_2(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:,:)
 type(message_list), intent(inout) :: msgs
 a = 0.
 if (has_variable(f, name)) then
  call read_field(f, name, a, msgs)
 else
  call add_message(msgs, LOG_WARN, 'Variable '//name//' not in input file.')
 end if
 end subroutine

 ! The initial state of the input stream's file at start_time, where MPAS reads its 'input' stream
 ! (core_atmosphere/mpas_atm_core.F:118-152): the dynamics fields, the column fields of the cold start, the orography
 ! statistics, the t=0 diagnostics and the two times. Input fields (Registry.xml:602-648) come from init_path, the
 ! invariant fields of inp%cell and the orography (:499-600) from inp%mesh_path; the two may be one file.
 ! fields: the input stream's fields from the streams file; without it only lean's own fields are checked.
 subroutine read_state_inputs(init_path, start_time, m, dyn, inp, msgs, fields)
 character(*), intent(in) :: init_path, start_time
 type(dyn_mesh_type), intent(inout) :: m
 type(cold_dyn_type), intent(out) :: dyn
 type(cold_inputs), intent(inout) :: inp
 type(message_list), intent(inout) :: msgs
 character(*), intent(in), optional :: fields(:)
 type(input_file) :: f, g
 character(len=19) :: stamp, unused
 character(len=64) :: initial_stamp
 logical :: found, one_file
 integer :: count0, count1, rate, i

 call system_clock(count0, rate)
 call add_message(msgs, LOG_OUT, 'Reading initial state from ''input'' stream')
 call open_input(f, init_path, 'input', msgs)
 if (.not. f%opened) return
 inp%init_path = trim(init_path)
 if (.not. state_sizes_fit(f, msgs)) then
  call close_input(f)
  return
 end if
 call seek_record(f, start_time, .true., msgs, found, stamp)
 if (.not. found) then
  call close_input(f)
  return
 end if
 ! the mesh file, for the invariant fields: a second handle on the same file when the input stream's file holds them
 one_file = .true.
 if (allocated(inp%mesh_path)) one_file = inp%mesh_path == trim(init_path)
 if (one_file) then
  call open_input(g, init_path, 'input', msgs)
 else
  call open_input(g, inp%mesh_path, 'input', msgs)
 end if
 if (.not. g%opened) then
  call close_input(f)
  return
 end if
 if (one_file) then
  g%record = f%record
 else
  call seek_record(g, start_time, .false., msgs, found, unused)
  if (.not. found) then
   call close_input(f)
   call close_input(g)
   return
  end if
 end if

 ! MPAS's warning for each stream field the file lacks and lean does not read
 if (present(fields)) then
  do i = 1, size(fields)
   if (lean_reads(fields(i))) cycle
   if (.not. has_variable(f, trim(fields(i)))) &
    call add_message(msgs, LOG_WARN, 'Variable '//trim(fields(i))//' not in input file.')
  end do
 end if

 ! the dynamics fields of the initial state, and the scalars the init file may hold besides (zeros when it has not)
 allocate(dyn%u(nz,ne1), dyn%w(nz+1,nc1), dyn%theta(nz,nc1), dyn%rho(nz,nc1), dyn%rho_base(nz,nc1), dyn%theta_base(nz,nc1), &
          dyn%qv(nz,nc1), dyn%qc(nz,nc1), dyn%qr(nz,nc1), dyn%qi(nz,nc1), dyn%qs(nz,nc1), dyn%qg(nz,nc1), dyn%ni(nz,nc1), &
          dyn%nr(nz,nc1))
 call read_field(f, 'u', dyn%u, msgs)
 call read_field(f, 'w', dyn%w, msgs)
 call read_field(f, 'theta', dyn%theta, msgs)
 call read_field(f, 'rho', dyn%rho, msgs)
 call read_field(f, 'rho_base', dyn%rho_base, msgs)
 call read_field(f, 'theta_base', dyn%theta_base, msgs)
 call read_field(f, 'qv', dyn%qv, msgs)
 call read_field(f, 'qc', dyn%qc, msgs)
 call read_field(f, 'qr', dyn%qr, msgs)
 call read_or_zero_2(f, 'qi', dyn%qi, msgs)
 call read_or_zero_2(f, 'qs', dyn%qs, msgs)
 call read_or_zero_2(f, 'qg', dyn%qg, msgs)
 call read_or_zero_2(f, 'ni', dyn%ni, msgs)
 call read_or_zero_2(f, 'nr', dyn%nr, msgs)
 if (.not. allocated(m%u_init)) allocate(m%u_init(nz), m%v_init(nz))
 call read_field(f, 'u_init', m%u_init, msgs)
 call read_field(f, 'v_init', m%v_init, msgs)

 call read_columns(f, g, m, inp, msgs)
 call close_input(g)

 ! the t=0 diagnostics of the input stream, in their own arrays; the start from the init file keeps its zeros
 if (.not. allocated(inp%relhum)) allocate(inp%relhum(nz,nc1), inp%u10(nc1), inp%v10(nc1), inp%q2(nc1), inp%t2m(nc1), &
                                           inp%precipw(nc1), inp%h_oml_initial(nc1))
 call read_or_zero_2(f, 'relhum', inp%relhum, msgs)
 call read_or_zero_1(f, 'u10', inp%u10, msgs)
 call read_or_zero_1(f, 'v10', inp%v10, msgs)
 call read_or_zero_1(f, 'q2', inp%q2, msgs)
 call read_or_zero_1(f, 't2m', inp%t2m, msgs)
 call read_or_zero_1(f, 'precipw', inp%precipw, msgs)
 call read_or_zero_1(f, 'h_oml_initial', inp%h_oml_initial, msgs)

 inp%start_time = stamp
 call read_text(f, 'initial_time', initial_stamp, msgs)
 ! The descriptor stores whole seconds; do not silently truncate MPAS's fraction.
 if(index(trim(initial_stamp),'.')>0)then
  call add_message(msgs,LOG_ERR,trim(init_path)//": initial_time = '"//trim(initial_stamp)// &
                   "': fractional seconds are not supported")
 endif
 inp%initial_time = initial_stamp
 ! MPAS dynamics/mpas_atm_time_integration.F:1845-1850: retain single-precision elapsed time.
 inp%cf_time=0.
 if(nc_inq_var(f%nc,'Time')==NC_NOERR) then
  i=nc_get(f%nc,'Time',inp%cf_time,record=1)
  if(i/=NC_NOERR) call add_message(msgs,LOG_ERR,trim(init_path)//': cannot read Time record 1: '//nc_message())
 endif
 call close_input(f)
 call system_clock(count1)
 call add_message(msgs, LOG_OUT, ' Timing for read of ''input'' stream: '// &
                  real_text(real(count1 - count0)/real(max(rate, 1)))//' s')
 call add_message(msgs, LOG_OUT, ' ----- done reading initial state -----')
 end subroutine

 ! The input stream's fields read_state_inputs reads itself (a missing one is an error, or zeros with MPAS's warning).
 ! scalars is the stream's name for qv ... nr (a var_array: the file holds its constituents).
 logical function lean_reads(name)
 character(*), intent(in) :: name
 character(len=16), parameter :: mine(43) = [character(len=16) :: 'u', 'w', 'theta', 'rho', 'rho_base', 'theta_base', &
  'scalars', 'qv', 'qc', 'qr', 'qi', 'qs', 'qg', 'ni', 'nr', 'u_init', 'v_init', 'skintemp', 'sst', 'xice', 'seaice', 'xland', &
  'surface_pressure', 'snow', 'snowh', 'snowc', 'tmn', 'vegfra', 'sfc_albbck', 'tslb', 'smois', 'sh2o', 'dzs', 'relhum', &
  'u10', 'v10', 'q2', 't2m', 'precipw', 'h_oml_initial', 'initial_time', 'xtime', 'Time']
 lean_reads = any(mine == name)
 end function

 ! The input stream's file against the mesh and this executable's limits: the dimensions its fields use.
 logical function state_sizes_fit(f, msgs)
 type(input_file), intent(in) :: f
 type(message_list), intent(inout) :: msgs
 logical :: missing
 integer :: before
 before = count_problems(msgs)
 missing = .false.
 call one_size(f, 'nCells', ncells, mesh_why, .false., missing, msgs)
 call one_size(f, 'nEdges', nedges, mesh_why, .false., missing, msgs)
 if (dimension_length(f, 'nVertices') > 0) &
  call one_size(f, 'nVertices', nvertices, mesh_why, .false., missing, msgs)
 call one_size(f, 'nVertLevels', nz, mesh_why, .false., missing, msgs)
 call one_size(f, 'nVertLevelsP1', dimension_length(f, 'nVertLevels') + 1, 'MPAS 8.4.2 writes nVertLevels + 1', .false., &
               missing, msgs)
 call one_size(f, 'nSoilLevels', 4, soil_why, .false., missing, msgs)
 if (missing) call add_message(msgs, LOG_CRIT, 'Please check the input file(s) to be read by the '''//trim(f%role)// &
                               ''' input stream.')
 state_sizes_fit = count_problems(msgs) == before .and. .not. missing
 end function

 ! inp%cell (src/cold_column.f90's record), and the orography statistics.
 ! f: the input stream's file; g: the mesh file (the invariant fields); latCell, lonCell and areaCell are the mesh's.
 subroutine read_columns(f, g, m, inp, msgs)
 type(input_file), intent(inout) :: f, g
 type(dyn_mesh_type), intent(in) :: m
 type(cold_inputs), intent(inout) :: inp
 type(message_list), intent(inout) :: msgs
 character(len=5), parameter :: oro(10) = [character(len=5) :: 'var2d', 'con', 'oa1', 'oa2', 'oa3', 'oa4', 'ol1', 'ol2', &
                                           'ol3', 'ol4']
 real, allocatable :: r(:), r4(:,:), r12(:,:)
 integer, allocatable :: k(:)
 integer :: c, i
 allocate(r(ncells), r4(4,ncells), r12(12,ncells), k(ncells))
 if (.not. allocated(inp%cell)) allocate(inp%cell(ncells), inp%orography(10,ncells))
 call read_field(f, 'skintemp', r, msgs);         inp%cell%skintemp = r
 call read_field(f, 'sst', r, msgs);              inp%cell%sst = r
 call read_field(f, 'xice', r, msgs);             inp%cell%xice = r
 call read_field(f, 'seaice', r, msgs);           inp%cell%seaice = r
 call read_field(f, 'xland', r, msgs);            inp%cell%xland = r
 call read_field(g, 'landmask', k, msgs);         inp%cell%landmask = k
 call read_field(f, 'surface_pressure', r, msgs); inp%cell%surface_pressure = r
 call read_field(f, 'snow', r, msgs);             inp%cell%snow = r
 call read_field(f, 'snowh', r, msgs);            inp%cell%snowh = r
 call read_field(f, 'snowc', r, msgs);            inp%cell%snowc = r
 call read_field(f, 'tmn', r, msgs);              inp%cell%tmn = r
 call read_field(f, 'vegfra', r, msgs);           inp%cell%vegfra = r
 call read_field(g, 'shdmin', r, msgs);           inp%cell%shdmin = r
 call read_field(g, 'shdmax', r, msgs);           inp%cell%shdmax = r
 call read_field(f, 'sfc_albbck', r, msgs);       inp%cell%sfc_albbck = r
 call read_field(g, 'snoalb', r, msgs);           inp%cell%snoalb = r
 call read_field(g, 'ivgtyp', k, msgs);           inp%cell%ivgtyp = k
 call read_field(g, 'isltyp', k, msgs);           inp%cell%isltyp = k
 call read_field(f, 'tslb', r4, msgs)
 do c = 1, ncells
  inp%cell(c)%tslb = r4(:,c)
 end do
 call read_field(f, 'smois', r4, msgs)
 do c = 1, ncells
  inp%cell(c)%smois = r4(:,c)
 end do
 call read_field(f, 'sh2o', r4, msgs)
 do c = 1, ncells
  inp%cell(c)%sh2o = r4(:,c)
 end do
 call read_field(f, 'dzs', r4, msgs)
 do c = 1, ncells
  inp%cell(c)%dzs = r4(:,c)
 end do
 call read_field(g, 'albedo12m', r12, msgs)
 do c = 1, ncells
  inp%cell(c)%albedo12m = r12(:,c)
 end do
 call read_field(g, 'greenfrac', r12, msgs)
 do c = 1, ncells
  inp%cell(c)%greenfrac = r12(:,c)
 end do
 inp%cell%latCell = m%latCell(1:ncells)
 inp%cell%lonCell = m%lonCell(1:ncells)
 inp%cell%areaCell = m%areaCell(1:ncells)
 do i = 1, 10
  call read_field(g, trim(oro(i)), r, msgs)
  inp%orography(i,:) = r
 end do
 end subroutine

 ! One ERR line when a holds non-finite values; the index is that of the last dimension (the cell, the edge for u).
 subroutine finite_2(path, name, a, msgs)
 character(*), intent(in) :: path, name
 real, intent(in) :: a(:,:)
 type(message_list), intent(inout) :: msgs
 integer :: n, first, j
 n = 0
 first = 0
 do j = 1, size(a, 2)
  if (all(ieee_is_finite(a(:,j)))) cycle
  if (n == 0) first = j
  n = n + count(.not. ieee_is_finite(a(:,j)))
 end do
 if (n > 0) call add_message(msgs, LOG_ERR, path//': '//name//': '//int_text(n)//' non-finite values (first at cell '// &
                             int_text(first)//')')
 end subroutine

 subroutine finite_1(path, name, a, msgs)
 character(*), intent(in) :: path, name
 real, intent(in) :: a(:)
 type(message_list), intent(inout) :: msgs
 call finite_2(path, name, reshape(a, [1, size(a)]), msgs)
 end subroutine

 ! The checks on what was read: non-finite values, the land-use table, land cells with near-zero background albedo
 ! (a fault of MPAS 8 static files on coasts and islands), and meshDensity against the cell sizes. The last two warn;
 ! strict (config_lean_strict_inputs) makes them stops.
 subroutine check_inputs(inp, m, dyn, len_disp, strict, msgs)
 type(cold_inputs), intent(in) :: inp
 type(dyn_mesh_type), intent(in) :: m
 type(cold_dyn_type), intent(in) :: dyn
 real, intent(in) :: len_disp
 logical, intent(in) :: strict
 type(message_list), intent(inout) :: msgs
 character(len=5), parameter :: oro(10) = [character(len=5) :: 'var2d', 'con', 'oa1', 'oa2', 'oa3', 'oa4', 'ol1', 'ol2', &
                                           'ol3', 'ol4']
 character(len=:), allocatable :: mesh, init
 real, allocatable :: r(:), r4(:,:), r12(:,:)
 real(8), parameter :: radius = 6371229.0d0
 real(8) :: area, dx, dx_model, err, scale, worst_err, worst_model, worst_dx, ld
 integer :: c, i, n, worst, soft
 logical :: bad

 mesh = ''
 init = ''
 if (allocated(inp%mesh_path)) mesh = inp%mesh_path
 if (allocated(inp%init_path)) init = inp%init_path
 soft = LOG_WARN
 if (strict) soft = LOG_ERR

 ! non-finite values
 call finite_2(init, 'u', dyn%u, msgs)
 call finite_2(init, 'w', dyn%w, msgs)
 call finite_2(init, 'theta', dyn%theta, msgs)
 call finite_2(init, 'rho', dyn%rho, msgs)
 call finite_2(init, 'rho_base', dyn%rho_base, msgs)
 call finite_2(init, 'theta_base', dyn%theta_base, msgs)
 call finite_2(init, 'qv', dyn%qv, msgs)
 call finite_2(init, 'qc', dyn%qc, msgs)
 call finite_2(init, 'qr', dyn%qr, msgs)
 if (allocated(dyn%qi)) call finite_2(init, 'qi', dyn%qi, msgs)
 if (allocated(dyn%qs)) call finite_2(init, 'qs', dyn%qs, msgs)
 if (allocated(dyn%qg)) call finite_2(init, 'qg', dyn%qg, msgs)
 if (allocated(dyn%ni)) call finite_2(init, 'ni', dyn%ni, msgs)
 if (allocated(dyn%nr)) call finite_2(init, 'nr', dyn%nr, msgs)
 allocate(r(ncells), r4(4,ncells), r12(12,ncells))
 r = inp%cell%skintemp;         call finite_1(init, 'skintemp', r, msgs)
 r = inp%cell%sst;              call finite_1(init, 'sst', r, msgs)
 r = inp%cell%xice;             call finite_1(init, 'xice', r, msgs)
 r = inp%cell%seaice;           call finite_1(init, 'seaice', r, msgs)
 r = inp%cell%xland;            call finite_1(init, 'xland', r, msgs)
 r = inp%cell%surface_pressure; call finite_1(init, 'surface_pressure', r, msgs)
 r = inp%cell%snow;             call finite_1(init, 'snow', r, msgs)
 r = inp%cell%snowh;            call finite_1(init, 'snowh', r, msgs)
 r = inp%cell%snowc;            call finite_1(init, 'snowc', r, msgs)
 r = inp%cell%tmn;              call finite_1(init, 'tmn', r, msgs)
 r = inp%cell%vegfra;           call finite_1(init, 'vegfra', r, msgs)
 r = inp%cell%shdmin;           call finite_1(mesh, 'shdmin', r, msgs)
 r = inp%cell%shdmax;           call finite_1(mesh, 'shdmax', r, msgs)
 r = inp%cell%sfc_albbck;       call finite_1(init, 'sfc_albbck', r, msgs)
 r = inp%cell%snoalb;           call finite_1(mesh, 'snoalb', r, msgs)
 do c = 1, ncells
  r4(:,c) = inp%cell(c)%tslb
 end do
 call finite_2(init, 'tslb', r4, msgs)
 do c = 1, ncells
  r4(:,c) = inp%cell(c)%smois
 end do
 call finite_2(init, 'smois', r4, msgs)
 do c = 1, ncells
  r4(:,c) = inp%cell(c)%sh2o
 end do
 call finite_2(init, 'sh2o', r4, msgs)
 do c = 1, ncells
  r4(:,c) = inp%cell(c)%dzs
 end do
 call finite_2(init, 'dzs', r4, msgs)
 do c = 1, ncells
  r12(:,c) = inp%cell(c)%albedo12m
 end do
 call finite_2(mesh, 'albedo12m', r12, msgs)
 do c = 1, ncells
  r12(:,c) = inp%cell(c)%greenfrac
 end do
 call finite_2(mesh, 'greenfrac', r12, msgs)
 r = inp%cell%latCell;  call finite_1(mesh, 'latCell', r, msgs)
 r = inp%cell%lonCell;  call finite_1(mesh, 'lonCell', r, msgs)
 r = inp%cell%areaCell; call finite_1(mesh, 'areaCell', r, msgs)
 do i = 1, 10
  r = inp%orography(i,:)
  call finite_1(mesh, trim(oro(i)), r, msgs)
 end do
 call finite_2(init, 'relhum', inp%relhum, msgs)
 call finite_1(init, 'u10', inp%u10, msgs)
 call finite_1(init, 'v10', inp%v10, msgs)
 call finite_1(init, 'q2', inp%q2, msgs)
 call finite_1(init, 't2m', inp%t2m, msgs)
 call finite_1(init, 'precipw', inp%precipw, msgs)
 call finite_1(init, 'h_oml_initial', inp%h_oml_initial, msgs)
 call finite_2(mesh, 'zgrid', m%zgrid, msgs)
 call finite_2(mesh, 'zz', m%zz, msgs)
 call finite_1(mesh, 'meshDensity', m%meshDensity, msgs)

 ! the land-use table: src/landuse_startup.f90 is built for the MODIS table (ice 15, water 17)
 if (inp%isice_lu /= 15 .or. inp%iswater_lu /= 17) &
  call add_message(msgs, LOG_ERR, mesh//': isice_lu/iswater_lu = '//int_text(inp%isice_lu)//'/'//int_text(inp%iswater_lu)// &
                   ': lean_mpas supports the MODIS land-use categories only (15/17)')
 if (trim(inp%mminlu) /= 'MODIFIED_IGBP_MODIS_NOAH') &
  call add_message(msgs, LOG_ERR, mesh//': mminlu = '''//trim(inp%mminlu)// &
                   ''': lean_mpas supports only ''MODIFIED_IGBP_MODIS_NOAH''')

 ! near-zero background albedo on land
 n = count(inp%cell%landmask == 1 .and. inp%cell%sfc_albbck < 0.01)
 if (n > 0) call add_message(msgs, soft, init//': '//int_text(n)//' land cells with background albedo below 1 % '// &
                             '(the MPAS 8 static fault on coasts and islands); tools/mesh/fix_albedo.py corrects the static')

 ! meshDensity against the cell sizes, in double precision. A NaN counts as a mismatch.
 if (len_disp > 0.) then
  ld = real(len_disp, 8)
  scale = 1.0d0
  if (maxval(m%areaCell(1:ncells)) < 1.0) scale = radius**2      ! unit-sphere file: to m2
  n = 0
  worst = 0
  worst_err = -1.0d0
  worst_model = 0.0d0
  worst_dx = 0.0d0
  do c = 1, ncells
   area = real(m%areaCell(c), 8)*scale
   dx = sqrt(area/(sqrt(3.0d0)/2.0d0))
   dx_model = ld/real(m%meshDensity(c), 8)**0.25d0
   err = abs(dx_model/max(dx, ld) - 1.0d0)
   bad = .not. (err <= 0.1d0)
   if (bad) n = n + 1
   ! the worst cell: the largest error; a NaN is worse than any number, the first one stays
   if (ieee_is_nan(worst_err)) cycle
   if (ieee_is_nan(err) .or. err > worst_err) then
    worst = c
    worst_err = err
    worst_model = dx_model
    worst_dx = dx
   end if
  end do
  if (n > 0) call add_message(msgs, soft, mesh//': meshDensity does not match the cell sizes for len_disp '// &
                              real_text(len_disp)//' m in '//int_text(n)//' cells (worst: cell '//int_text(worst)//', '// &
                              real_text(real(worst_model))//' m against '//real_text(real(worst_dx))// &
                              ' m); tools/mesh/set_mesh_density.py corrects it')
 end if
 end subroutine

 ! The horizontal length scale, as core_atmosphere/mpas_atm_core.F:165-199 with its log lines; 0 after its error.
 real function effective_len_disp(config_len_disp, nominal_min_dc, msgs)
 real, intent(in) :: config_len_disp, nominal_min_dc
 type(message_list), intent(inout) :: msgs
 call add_message(msgs, LOG_OUT, '')
 if (config_len_disp > 0.0) then
  call add_message(msgs, LOG_OUT, 'Setting nominalMinDc to '//real_text(config_len_disp)// &
                   ' based on namelist option config_len_disp')
  if (nominal_min_dc > 0.0 .and. abs(nominal_min_dc - config_len_disp) > 1.0e-6 * config_len_disp) then
   call add_message(msgs, LOG_WARN, 'nominalMinDc was read from input file as a positive value ('// &
                    real_text(nominal_min_dc)//') that differs')
   call add_message(msgs, LOG_WARN, 'from the specified config_len_disp value ('//real_text(config_len_disp)//')')
  end if
  effective_len_disp = config_len_disp
 else if (nominal_min_dc > 0.0) then
  call add_message(msgs, LOG_OUT, 'Setting config_len_disp to '//real_text(nominal_min_dc)// &
                   ' based on nominalMinDc value in input file')
  effective_len_disp = nominal_min_dc
 else
  call add_message(msgs, LOG_ERR, 'Both config_len_disp and nominalMinDc are <= 0.0.')
  call add_message(msgs, LOG_ERR, 'Please either specify config_len_disp in the &nhyd_model namelist group,')
  call add_message(msgs, LOG_ERR, 'or use an input file that provides a valid value for the nominalMinDc variable.')
  effective_len_disp = 0.
 end if
 end function

 ! Hands the namelist's run settings to the model: time step, radiation intervals, dynamics settings, boundary and
 ! surface options, and the schemes that run. len_disp is the effective one; radt_lw_s, radt_sw_s and greeness_s are
 ! config_radtlw_interval, config_radtsw_interval and config_greeness_update in seconds. greeness_s is not used here:
 ! src/lean_model.f90 runs the greenness update on its own alarm from the start, as MPAS
 ! physics/mpas_atmphys_manager.F:549-552.
 subroutine apply_run_config(cfg, m, inp, len_disp, radt_lw_s, radt_sw_s, greeness_s, msgs)
 type(run_config), intent(in) :: cfg
 type(dyn_mesh_type), intent(in) :: m
 type(cold_inputs), intent(in) :: inp
 real, intent(in) :: len_disp, radt_lw_s, radt_sw_s, greeness_s
 type(message_list), intent(inout) :: msgs
 integer :: k
 real :: height
 ! MPAS 8.4.2 crashes when longwave runs without shortwave, so the longwave interval must be a multiple of the
 ! shortwave one.
 if(radt_lw_s<=0..or.radt_sw_s<=0.)then
  call add_message(msgs,LOG_ERR,'radiation intervals must be positive')
  return
 endif
 if(modulo(radt_lw_s,radt_sw_s)/=0.)then
  call add_message(msgs,LOG_ERR,'config_radtlw_interval = '//real_text(radt_lw_s)// &
   ': must be a whole multiple of config_radtsw_interval = '//real_text(radt_sw_s)//' (MPAS cannot run longwave alone)')
  return
 endif
 call set_time_step(cfg%config_dt)
 call set_radiation_intervals(radt_lw_s,radt_sw_s)
 call set_dyn_config(len_disp, cfg%config_zd, cfg%config_xnutr, cfg%config_epssm_minimum, &
  cfg%config_epssm_maximum, cfg%config_epssm_transition_bottom_z, cfg%config_epssm_transition_top_z, &
  cfg%config_coef_3rd_order, cfg%config_visc4_2dsmag, cfg%config_smagorinsky_coef, cfg%config_del4u_div_factor, &
  cfg%config_smdiv, cfg%config_relax_zone_divdamp_coef)
 call set_apply_lbcs(cfg%config_apply_lbcs)
 call set_apvm_upwinding(cfg%config_apvm_upwinding)
 call set_noahmp_opt_sfc(cfg%config_noahmp_iopt_sfc)
 ! which schemes run, and config_microp_re; resolve_suite has made the names final
 call set_schemes(cfg%config_microp_scheme, cfg%config_convection_scheme, cfg%config_pbl_scheme, &
  cfg%config_sfclayer_scheme, cfg%config_lsm_scheme, cfg%config_radt_lw_scheme, cfg%config_radt_sw_scheme, &
  cfg%config_radt_cld_scheme, cfg%config_gwdo_scheme, cfg%config_microp_re)
 ! lean runs the microphysics on every level (src/dyn_micro.f90). That matches MPAS only when the top layer's centre,
 ! summed as physics/mpas_atmphys_init.F:276-283 sums it, is at or below config_microphysics_top.
 height = 0.5/m%rdzw(1)
 do k = 2, nz
  height = height + inp%dzu(k)
 end do
 if (.not. (height <= cfg%config_microphysics_top)) &
  call add_message(msgs, LOG_ERR, 'config_microphysics_top = '//real_text(cfg%config_microphysics_top)// &
                   ': lean_mpas runs the microphysics on every level; the top layer''s centre is at '// &
                   real_text(height)//' m (summed as MPAS sums it)')
 end subroutine

 ! Whether a read or a check left a line that stops the run: an ERR (an input problem) or a CRIT. The readers return
 ! no status (lbc_next, ...): their caller asks this before it goes on.
 logical function inputs_failed(msgs)
 type(message_list), intent(in) :: msgs
 integer :: i
 inputs_failed = .false.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_ERR .or. msgs%line(i)%level == LOG_CRIT) inputs_failed = .true.
 end do
 end function

 ! An ERR line for each physics table the run directory lacks, of the schemes that run:
 ! the Thompson tables with Thompson, RRTMG_LW_DATA with the longwave scheme, RRTMG_SW_DATA with the shortwave scheme,
 ! the ozone tables with either, NoahmpTable.TBL with Noah-MP, and LANDUSE.TBL unless no physics runs at all (MPAS reads
 ! its physics tables only with moist_physics, mpas_atm_core.F:580-596).
 subroutine check_run_tables(directory, msgs)
 character(*), intent(in) :: directory
 type(message_list), intent(inout) :: msgs
 logical :: there, wanted
 integer :: i
 do i = 1, size(other_table)
  select case (trim(other_table(i)))
  case ('LANDUSE.TBL')
   wanted = moist_physics
  case ('OZONE_DAT.TBL', 'OZONE_LAT.TBL', 'OZONE_PLEV.TBL')
   wanted = lw_on .or. sw_on
  case ('RRTMG_LW_DATA')
   wanted = lw_on
  case ('RRTMG_SW_DATA')
   wanted = sw_on
  case ('NoahmpTable.TBL')
   wanted = lsm_on
  case default
   wanted = .true.
  end select
  if (.not. wanted) cycle
  inquire(file=directory//'/'//trim(other_table(i)), exist=there)
  if (.not. there) call add_message(msgs, LOG_ERR, directory//'/'//trim(other_table(i))//table_why)
 end do
 if (.not. micro_on) return
 do i = 1, size(thompson_table)
  inquire(file=directory//'/'//trim(thompson_table(i)), exist=there)
  if (.not. there) call add_message(msgs, LOG_ERR, directory//'/'//trim(thompson_table(i))//table_why)
 end do
 end subroutine

 ! The byte order of the four Thompson tables, from the length marker before each one's first record. MPAS's are
 ! big-endian (its Makefile builds with -fconvert=big-endian or -byteswapio: Makefile:150,390), the ones
 ! `./lean tables` places in data/tables little-endian. A missing table, a marker that is the length in neither order,
 ! or tables that disagree give an ERR line; big_endian is .false. after any of them.
 subroutine thompson_table_order(directory, big_endian, msgs)
 character(*), intent(in) :: directory
 logical, intent(out) :: big_endian
 type(message_list), intent(inout) :: msgs
 character(len=1) :: b(4)
 logical :: there, big(4), bad
 integer :: i, unit, status, little_value, big_value
 big_endian = .false.
 big = .false.
 bad = .false.
 do i = 1, size(thompson_table)
  inquire(file=directory//'/'//trim(thompson_table(i)), exist=there)
  if (.not. there) then
   call add_message(msgs, LOG_ERR, directory//'/'//trim(thompson_table(i))//table_why)
   bad = .true.
   cycle
  end if
  b = achar(0)
  open(newunit=unit, file=directory//'/'//trim(thompson_table(i)), access='stream', form='unformatted', status='old', &
       action='read', iostat=status)
  if (status == 0) then
   read(unit, iostat=status) b
   close(unit)
  end if
  ! the marker is below 2**31 in a real table, so the top byte is kept out of the sum when it would overflow
  little_value = -1
  big_value = -1
  if (status == 0 .and. iachar(b(4)) < 128) &
   little_value = iachar(b(1)) + 256*(iachar(b(2)) + 256*(iachar(b(3)) + 256*iachar(b(4))))
  if (status == 0 .and. iachar(b(1)) < 128) &
   big_value = iachar(b(4)) + 256*(iachar(b(3)) + 256*(iachar(b(2)) + 256*iachar(b(1))))
  if (little_value == thompson_record(i)) then
   big(i) = .false.
  else if (big_value == thompson_record(i)) then
   big(i) = .true.
  else
   call add_message(msgs, LOG_ERR, directory//'/'//trim(thompson_table(i))//': not a Thompson table of MPAS 8.4.2 '// &
                    '(first record of '//int_text(thompson_record(i))//' bytes expected)')
   bad = .true.
  end if
 end do
 if (bad) return
 if (any(big) .and. .not. all(big)) then
  call add_message(msgs, LOG_ERR, 'the Thompson tables in '//directory//' mix byte orders')
  return
 end if
 big_endian = big(1)
 end subroutine

end module
