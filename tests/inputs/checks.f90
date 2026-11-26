! The stops and warnings of lean_inputs on doctored and real files:  checks BUILD ROOT DOCTORED
! BUILD names the mesh of the cases: greece or x1. Each case asserts its WARN, ERR and CRIT lines exactly.
! ice_free_greece, ice_free_x1, land_ice2, sea_ice1: the cold start of a zero-length run in the run directory (the
! working directory), as atmosphere_model starts it, and the ice it sets up.
program checks
 use iso_fortran_env, only: int32, int64
 use lean_model, only: run_model_from
 use lean_drop_in, only: run_command, drop_in_start, drop_in_finish
 use lean_streams, only: stream_set
 use lean_clock, only: run_clock
 use lean_run_inputs, only: run_inputs
 use sea_ice, only: any_sea_ice, ice_values => ice
 use land_ice_exchange, only: n_glacier
 use noahmp_state, only: set_noahmp_opt_sfc
 use nc_check, only: check, finish
 use config
 use run_settings, only: set_time_step, radt_lw, radt_sw, n_microp, len_disp, zd, xnutr, &
  epssm_min, epssm_max, epssm_bottom, epssm_top, coef_3rd_order, visc4_2dsmag, smagorinsky_coef, &
  del4u_div_factor, smdiv, relax_zone_divdamp_coef, apply_lbcs
 use lean_messages
 use lean_inputs
 use lean_input_fields, only: count_problems
 use lean_namelist, only: run_config
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use cold_dyn, only: cold_dyn_state
 use dyn_state_data, only: dyn_state_type
 use dyn_config, only: i_qv, i_qc, i_qr, i_qi, i_qs, i_qg, i_ni, i_nr
 implicit none
 character(len=16) :: build
 character(len=400) :: root, doctored
 character(len=:), allocatable :: greece, x1, l55
 character(len=600) :: want_text(60)
 integer :: want_level(60), nwant = 0
 integer :: child_status
 character(len=600) :: executable
 type(message_list) :: msgs
 type(run_command) :: run_cmd
 type(run_config) :: run_cfg
 type(stream_set) :: run_set
 type(run_clock) :: run_clk
 type(dyn_mesh_type) :: run_mesh
 type(cold_dyn_type) :: run_dyn
 type(run_inputs) :: run_in

 call get_command_argument(1, build)
 call get_command_argument(2, root)
 call get_command_argument(3, doctored)
 greece = trim(root)//'/data/cases/greece_60km/inputs/greece_60km.init.nc'
 x1 = trim(root)//'/data/cases/x1_2562/inputs/x1.2562.init.nc'
 l55 = trim(root)//'/data/cases/x1_2562/inputs/l55/x1.2562.init.nc'
 select case (trim(build))
 case ('sfc_set_twice')
  call set_noahmp_opt_sfc(2)
  call set_noahmp_opt_sfc(1)
  error stop 'sfc_set_twice: accepted'
 case ('sfc_set_bad')
  call set_noahmp_opt_sfc(3)
  error stop 'sfc_set_bad: accepted'
 case ('set_twice')
  call set_time_step(60.)
  call set_time_step(18.)
  error stop 'set_twice: accepted'
 case ('sizes_twice')
  call set_mesh_sizes(2337, 7183, 4847, 45)
  call set_mesh_sizes(2337, 7183, 4847, 55)
  error stop 'sizes_twice: second set accepted'
 case ('ice_free_greece', 'ice_free_x1', 'land_ice2', 'sea_ice1')
  call drop_in_start(run_cmd, run_cfg, run_set, run_clk, run_mesh, run_dyn, run_in)
  call run_model_from(run_mesh, run_dyn, run_in, run_clk, run_cfg, run_set)
  call drop_in_finish()
  select case (trim(build))
  case ('ice_free_greece')
   call check('ice_free greece: no ice', .not. any_sea_ice .and. n_glacier == 0)
  case ('ice_free_x1')
   call check('ice_free x1: sea ice and 80 glaciers', any_sea_ice .and. n_glacier == 80)
  case ('land_ice2')
   call check('land_ice2: 2 glacier columns', .not. any_sea_ice .and. n_glacier == 2)
  case ('sea_ice1')
   call check('sea_ice1: 1 sea-ice cell', any_sea_ice .and. count(ice_values%xice >= 0.02) == 1 .and. n_glacier == 0)
  end select
 case ('greece')
  call mesh_cases_greece()
  call state_cases_greece()
  call input_cases_greece()
 case ('x1')
  call mesh_cases_x1()
  call input_cases_x1()
 case default
  error stop 'checks: BUILD must be greece or x1'
 end select
 call finish('checks '//trim(build))

contains

 function doc(name) result(path)
 character(*), intent(in) :: name
 character(len=:), allocatable :: path
 path = trim(doctored)//'/'//name//'.nc'
 end function

 subroutine want(level, text)
 integer, intent(in) :: level
 character(*), intent(in) :: text
 nwant = nwant + 1
 want_level(nwant) = level
 want_text(nwant) = text
 end subroutine

 ! The WARN, ERR and CRIT lines of msgs are exactly the wanted ones, in order. Both lists are then emptied.
 subroutine verify(name)
 character(*), intent(in) :: name
 integer :: i, n
 logical :: ok
 n = 0
 ok = .true.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_OUT) cycle
  n = n + 1
  if (n > nwant) then
   ok = .false.
  else if (msgs%line(i)%level /= want_level(n) .or. .not. matches(trim(msgs%line(i)%text), trim(want_text(n)))) then
   ok = .false.
  end if
 end do
 if (n /= nwant) ok = .false.
 call check(name//': the WARN, ERR and CRIT lines', ok)
 if (.not. ok) then
  print '(a)', '   got:'
  do i = 1, msgs%n
   if (msgs%line(i)%level /= LOG_OUT) print '(a,i0,a)', '    ', msgs%line(i)%level, ' '//trim(msgs%line(i)%text)
  end do
  print '(a)', '   wanted:'
  do i = 1, nwant
   print '(a,i0,a)', '    ', want_level(i), ' '//trim(want_text(i))
  end do
 end if
 msgs%n = 0
 nwant = 0
 end subroutine

 ! A wanted text may hold one *, which stands for any text.
 logical function matches(got, wanted)
 character(*), intent(in) :: got, wanted
 integer :: star, tail
 star = index(wanted, '*')
 if (star == 0) then
  matches = got == wanted
  return
 end if
 tail = len(wanted) - star
 matches = len(got) >= len(wanted) - 1
 if (matches) matches = got(1:star-1) == wanted(1:star-1) .and. got(len(got)-tail+1:) == wanted(star+1:)
 end function

 logical function has_out(text)
 character(*), intent(in) :: text
 integer :: i
 has_out = .false.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_OUT .and. trim(msgs%line(i)%text) == text) has_out = .true.
 end do
 end function

 function size_line(path, dim, n, built, why) result(text)
 character(*), intent(in) :: path, dim, why
 integer, intent(in) :: n, built
 character(len=:), allocatable :: text
 text = path//': '//dim//' = '//int_text(n)//'; this executable is built for '//int_text(built)//' ('//why//')'
 end function

 subroutine mesh_cases_greece()
 type(dyn_mesh_type) :: m, clean
 type(cold_inputs) :: inp
 type(mesh_sizes) :: sizes
 character(len=:), allocatable :: p
 integer :: hexagon

 ! greece_clean
 call read_mesh_inputs(greece, clean, inp, msgs)
 call check('greece_clean: the bootstrapping line', has_out('Bootstrapping framework with mesh fields from input file '''//greece//''''))
 call verify('greece_clean')
 call check('greece_clean: mesh read', allocated(clean%zEdge))
 call check('greece_clean: sizes', inp%sizes%ncells == 2337 .and. inp%sizes%nedges == 7183 .and. &
            inp%sizes%nvertices == 4847 .and. inp%sizes%maxedges == 10 .and. inp%sizes%nvertlevels == 45 .and. &
            inp%sizes%nsoillevels == 4)
 call check('greece_clean: attributes', inp%attributes%on_a_sphere .and. inp%attributes%sphere_radius == 6371229. .and. &
            inp%attributes%parent_id == 'iun7bgx9bs'//new_line('A')//'jqg61f9evo'//new_line('A') .and. &
            inp%attributes%mesh_spec == '0.0')
 call check('mesh_attributes: planar defaults', .not.inp%attributes%is_periodic .and. &
            inp%attributes%x_period==0. .and. inp%attributes%y_period==0.)
 call check('greece_clean: extras', inp%nominal_min_dc == 60000. .and. trim(inp%mminlu) == 'MODIFIED_IGBP_MODIS_NOAH' .and. &
            inp%isice_lu == 15 .and. inp%iswater_lu == 17 .and. size(inp%lat_edge) == ne1 .and. size(inp%lon_vertex) == nv1 &
            .and. size(inp%dzu) == nz .and. trim(inp%mesh_path) == greece)
 hexagon = findloc(clean%nEdgesOnCell(1:ncells), 6, dim=1)

 call read_mesh_sizes(x1, sizes, msgs)
 call verify('read_mesh_sizes')
 call check('read_mesh_sizes: x1', sizes%ncells == 2562 .and. sizes%nedges == 7680 .and. sizes%nvertices == 5120 .and. &
            sizes%maxedges == 10 .and. sizes%nvertlevels == 45 .and. sizes%nsoillevels == 4)
 call read_mesh_sizes(trim(doctored)//'/absent.nc', sizes, msgs)
 call check('read_mesh_sizes: a missing file is one CRIT line', msgs%n == 1 .and. has_fatal(msgs) .and. &
            index(msgs%line(1)%text, 'input stream: cannot open '//trim(doctored)//'/absent.nc: ') == 1)
 msgs%n = 0

 ! sizes_other_mesh: the mesh file, not the compiled defaults, sets all horizontal sizes.
 call clear_mesh_sizes()
 call read_mesh_inputs(x1, m, inp, msgs)
 call verify('sizes_other_mesh')
 call check('sizes_other_mesh: sizes', ncells == 2562 .and. nedges == 7680 .and. nvertices == 5120)
 call check('sizes_other_mesh: allocated', size(m%latCell) == 2563 .and. allocated(inp%lat_edge))
 call set_mesh_sizes(2562, 7680, 5120, 45)
 call check('sizes_twice: same values', nc1 == 2563 .and. ne1 == 7681 .and. nv1 == 5121)
 call get_command_argument(0, executable)
 call execute_command_line('"'//trim(executable)//'" sizes_twice > "'//trim(doctored)//'/sizes_twice.log" 2>&1', exitstat=child_status)
 call check('sizes_twice: different values stop', child_status /= 0)
 call execute_command_line('grep -q "mesh sizes are set once per run" "'//trim(doctored)//'/sizes_twice.log"', exitstat=child_status)
 call check('sizes_twice: stop text', child_status == 0)
 call clear_mesh_sizes()
 call set_mesh_sizes(2337, 7183, 4847, 45)

 ! levels_over: only a dimension is read; no field allocation is allowed.
 p = doc('levels_over')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': nVertLevels = 61; this executable holds columns of at most 60 levels (max_levels in src/config.f90)')
 call verify('levels_over')
 call check('levels_over: nothing read', .not. allocated(m%latCell))

 ! a mesh file that is not there: MPAS's CRIT, nothing of lean's own
 p = trim(doctored)//'/absent.nc'
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_CRIT, 'Could not open input file '''//p//''' to read mesh fields')
 call verify('missing_mesh_file')

 p = doc('unit_sphere')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': sphere_radius = 1.00000: lean_mpas needs the mesh on the earth''s radius, 6371229 m, '// &
           'as init_atmosphere writes it')
 call verify('unit_sphere')

 p = doc('no_sphere_attributes')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_WARN, 'Attribute on_a_sphere not found in '//p)
 call want(LOG_WARN, '   Setting on_a_sphere to ''YES''')
 call want(LOG_WARN, 'Attribute sphere_radius not found in '//p)
 call want(LOG_WARN, '   Setting sphere_radius to 1.0')
 call want(LOG_ERR, p//': sphere_radius = 1.00000: lean_mpas needs the mesh on the earth''s radius, 6371229 m, '// &
           'as init_atmosphere writes it')
 call verify('no_sphere_attributes')

 p = doc('planar')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': on_a_sphere = ''NO'': lean_mpas runs spherical meshes only')
 call verify('planar')

 p = doc('slot9')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': cellsOnCell: slots beyond 8 are used (first at index '//int_text(hexagon)// &
           '): lean_mpas supports cells of at most 8 edges')
 call verify('slot9')

 p = doc('empty_slot_neighbour')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': edgesOnCell: a neighbour in a slot beyond nEdgesOn* (first at index '//int_text(hexagon)//')')
 call verify('empty_slot_neighbour')

 p = doc('index_order')
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_ERR, p//': indexToCellID is not 1..2337: lean_mpas needs the mesh in its own index order')
 call verify('index_order')
 end subroutine

 include 'same.inc'

 subroutine want_ice_warnings()
 call want(LOG_WARN, 'Variable qi not in input file.')
 call want(LOG_WARN, 'Variable qs not in input file.')
 call want(LOG_WARN, 'Variable qg not in input file.')
 call want(LOG_WARN, 'Variable ni not in input file.')
 call want(LOG_WARN, 'Variable nr not in input file.')
 end subroutine

 subroutine dimension_block(path, name)
 character(*), intent(in) :: path, name
 call want(LOG_ERR, 'At least one fields to be read from the ''input'' stream is dimensioned')
 call want(LOG_ERR, 'by '''//name//''', but the '''//name//''' dimension is not defined')
 call want(LOG_ERR, 'in the file '//path)
 call want(LOG_ERR, path//': no dimension '//name//' (input stream; an init file made by init_atmosphere has it)')
 end subroutine

 subroutine state_cases_greece()
 character(len=*), parameter :: t0 = '2026-11-08_00:00:00'
 type(dyn_mesh_type) :: m, cm
 type(cold_dyn_type) :: dyn, cdyn
 type(cold_inputs) :: inp, cinp
 type(dyn_state_type) :: s, s0
 real, allocatable :: sign_cell(:,:), zb_cell(:,:,:), zb3_cell(:,:,:)
 character(len=:), allocatable :: p, q
 logical :: ok
 integer :: i

 ! the clean one-file read, MPAS's lines of the initial-state read
 call read_mesh_inputs(greece, cm, cinp, msgs)
 call read_state_inputs(greece, t0, cm, cdyn, cinp, msgs)
 call check('state_clean: the first line', has_out('Reading initial state from ''input'' stream'))
 call check('state_clean: the last line', has_out(' ----- done reading initial state -----'))
 ok = .false.
 do i = 1, msgs%n
  if (index(msgs%line(i)%text, ' Timing for read of ''input'' stream: ') == 1 .and. &
      index(msgs%line(i)%text, ' s', back=.true.) == len_trim(msgs%line(i)%text) - 1) ok = .true.
 end do
 call check('state_clean: the timing line', ok)
 call want_ice_warnings()
 call verify('state_clean')
 call check('mesh_attributes: cf_time', cinp%cf_time==0.)
 call check('state_clean: times', cinp%start_time == t0 .and. cinp%initial_time == t0 .and. trim(cinp%init_path) == greece)

 ! A separate state file may omit nVertices; if present it must match the mesh.
 p = doc('state_vertices')
 call read_state_inputs(p, t0, cm, dyn, cinp, msgs)
 call want(LOG_ERR, p//': nVertices = 7; the mesh file has 4847')
 call verify('state_other_vertices')
 call check('state_other_vertices: nothing read', .not. allocated(dyn%u))

 ! split: the invariant stream's file and the input stream's
 p = doc('invariant')
 q = doc('state')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(q, t0, m, dyn, inp, msgs)
 call want(LOG_WARN, 'File '//p//' does not contain a seekable xtime variable. Forcing a read of the first time record.')
 call want_ice_warnings()
 call verify('split')
 call check('split: every array equals the one-file read', same_mesh(m, cm) .and. same_dyn(dyn, cdyn, .true.) .and. &
            same_inp(inp, cinp))

 p = doc('double')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want_ice_warnings()
 call verify('double')
 call check('double: every array equals the single-precision file''s', same_mesh(m, cm) .and. same_dyn(dyn, cdyn, .true.) &
            .and. same_inp(inp, cinp))

 p = doc('two_records')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want_ice_warnings()
 call verify('two_records 00')
 call check('two_records: start 00 equals the one-record read', same_mesh(m, cm) .and. same_dyn(dyn, cdyn, .true.) .and. &
            same_inp(inp, cinp))
 call read_state_inputs(p, '2026-11-08_03:00:00', m, dyn, inp, msgs)
 call want_ice_warnings()
 call verify('two_records 03')
 call check('two_records: start 03 reads record 2 (theta + 1.0)', all(dyn%theta(:,1:ncells) == cdyn%theta(:,1:ncells) + 1.0) &
            .and. all(dyn%theta(:,nc1) == 0.) .and. er2(dyn%rho, cdyn%rho) .and. inp%start_time == '2026-11-08_03:00:00')

 ! start_not_in_file
 call read_state_inputs(greece, '2026-11-08_01:00:00', cm, dyn, inp, msgs)
 call want(LOG_ERR, 'File '//greece//' does not contain the time 2026-11-08_01:00:00')
 call verify('start_not_in_file')
 call check('start_not_in_file: nothing read', .not. allocated(dyn%u))

 p = doc('no_xtime')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want(LOG_WARN, 'File '//p//' does not contain a seekable xtime variable. Forcing a read of the first time record.')
 call want_ice_warnings()
 call verify('no_xtime')
 call check('no_xtime: the arrays equal', same_mesh(m, cm) .and. same_dyn(dyn, cdyn, .true.) .and. same_inp(inp, cinp))

 ! static_as_input, grid_as_input: a static or grid file as the input stream's file
 p = trim(root)//'/data/cases/greece_60km/inputs/greece_60km.static.nc'
 call read_mesh_inputs(p, m, inp, msgs)
 call check('static_as_input: MPAS''s not-found line', has_out('       nVertLevels *** not found in stream ***'))
 call dimension_block(p, 'nVertLevels')
 call dimension_block(p, 'nVertLevelsP1')
 call want(LOG_CRIT, 'Please check the input file(s) to be read by the ''input'' input stream.')
 call verify('static_as_input')
 call check('static_as_input: nothing read', .not. allocated(m%latCell))

 p = trim(root)//'/data/cases/greece_60km/inputs/greece_60km.grid.nc'
 call read_mesh_inputs(p, m, inp, msgs)
 call want(LOG_WARN, 'Attribute file_id not found in '//p)
 call want(LOG_WARN, '  Using '''' for the previous file_id.')
 call want(LOG_WARN, 'Attribute parent_id not found in '//p)
 call want(LOG_WARN, '  Setting parent_id to ''''')
 call want(LOG_WARN, 'Attribute mesh_spec not found in '//p)
 call want(LOG_WARN, '  Setting mesh_spec to ''0.0''')
 call want(LOG_ERR, p//': sphere_radius = 1.00000: lean_mpas needs the mesh on the earth''s radius, 6371229 m, '// &
           'as init_atmosphere writes it')
 call dimension_block(p, 'R3')
 call dimension_block(p, 'nMonths')
 call dimension_block(p, 'FIFTEEN')
 call dimension_block(p, 'nVertLevels')
 call dimension_block(p, 'nVertLevelsP1')
 call want(LOG_CRIT, 'Please check the input file(s) to be read by the ''input'' input stream.')
 call verify('grid_as_input')
 call check('grid_as_input: nothing read', .not. allocated(m%latCell))

 p = doc('missing_field')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want_ice_warnings()
 call want(LOG_ERR, p//': no variable sfc_albbck (input stream)')
 call verify('missing_field')

 p = doc('missing_diag')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want_ice_warnings()
 call want(LOG_WARN, 'Variable u10 not in input file.')
 call verify('missing_diag')
 call check('missing_diag: u10 is 0, the rest equal', all(inp%u10 == 0.) .and. er1(inp%v10, cinp%v10) .and. &
            same_dyn(dyn, cdyn, .true.))

 ! ice_init: qi qs qg ni nr are read from the file, as MPAS does
 p = doc('ice_init')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call verify('ice_init')
 call check('ice_init: dyn holds the five values', dyn%qi(3,7) == 1.e-5 .and. dyn%qs(3,7) == 2.e-5 .and. dyn%qg(3,7) == 3.e-5 &
            .and. dyn%ni(3,7) == 4000. .and. dyn%nr(3,7) == 5000. .and. count(dyn%qi /= 0.) == 1 .and. &
            count(dyn%qs /= 0.) == 1 .and. count(dyn%qg /= 0.) == 1 .and. count(dyn%ni /= 0.) == 1 .and. count(dyn%nr /= 0.) == 1)
 call check('ice_init: every other array equals the undoctored read', same_mesh(m, cm) .and. same_dyn(dyn, cdyn, .false.) &
            .and. same_inp(inp, cinp))
 allocate(sign_cell(maxedges,nc1), zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1))
 sign_cell = 0.
 zb_cell = 0.
 zb3_cell = 0.
 call cold_dyn_state(dyn, m, sign_cell, zb_cell, zb3_cell, s)
 call check('ice_init: a cold start has them in rows i_qi..i_nr of its scalars', &
            all(s%scalars(i_qi,:,:) == dyn%qi) .and. all(s%scalars(i_qs,:,:) == dyn%qs) .and. &
            all(s%scalars(i_qg,:,:) == dyn%qg) .and. all(s%scalars(i_ni,:,:) == dyn%ni) .and. &
            all(s%scalars(i_nr,:,:) == dyn%nr) .and. s%scalars(i_qi,3,7) == 1.e-5 .and. s%scalars(i_nr,3,7) == 5000. .and. &
            all(s%scalars(i_qv,:,:) == dyn%qv) .and. all(s%scalars(i_qc,:,:) == dyn%qc) .and. all(s%scalars(i_qr,:,:) == dyn%qr))
 call cold_dyn_state(cdyn, cm, sign_cell, zb_cell, zb3_cell, s0)
 call check('a cold start from a file without them leaves those rows 0', all(s0%scalars(i_qi:i_nr,:,:) == 0.))

 p = doc('bad_xtime')
 call read_mesh_inputs(p, m, inp, msgs)
 call read_state_inputs(p, t0, m, dyn, inp, msgs)
 call want(LOG_ERR, p//': xtime of record 1 is not a time: ''garbage''')
 call verify('bad_xtime')
 call check('bad_xtime: nothing read', .not. allocated(dyn%u))
 end subroutine

 ! Reads a file as the run does and gives check_inputs' lines alone.
 subroutine checked(path, start, len_disp, strict)
 character(*), intent(in) :: path, start
 real, intent(in) :: len_disp
 logical, intent(in) :: strict
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 type(cold_inputs) :: inp
 call read_mesh_inputs(path, m, inp, msgs)
 call read_state_inputs(path, start, m, dyn, inp, msgs)
 if (count_problems(msgs) > 0 .or. has_fatal(msgs)) then
  call check('read of '//path, .false.)
  return
 end if
 msgs%n = 0
 call check_inputs(inp, m, dyn, len_disp, strict, msgs)
 end subroutine

 subroutine input_cases_greece()
 character(len=*), parameter :: t0 = '2026-11-08_00:00:00'
 character(len=*), parameter :: albedo_tail = ' land cells with background albedo below 1 % (the MPAS 8 static fault on '// &
                                              'coasts and islands); tools/mesh/fix_albedo.py corrects the static'
 character(len=*), parameter :: density_tail = '; tools/mesh/set_mesh_density.py corrects it'
 character(len=:), allocatable :: p
 type(run_config) :: cfg
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 type(cold_inputs) :: inp
 real :: ld

 p = doc('nan_theta')
 call checked(p, t0, 60000., .false.)
 call want(LOG_ERR, p//': theta: 1 non-finite values (first at cell 6)')
 call verify('nan_theta')

 p = doc('isice16')
 call checked(p, t0, 60000., .false.)
 call want(LOG_ERR, p//': isice_lu/iswater_lu = 16/17: lean_mpas supports the MODIS land-use categories only (15/17)')
 call verify('isice16')

 p = doc('usgs')
 call checked(p, t0, 60000., .false.)
 call want(LOG_ERR, p//': mminlu = ''USGS'': lean_mpas supports only ''MODIFIED_IGBP_MODIS_NOAH''')
 call verify('usgs')

 call checked(doc('mminlu_nul'), t0, 60000., .false.)
 call verify('mminlu_nul')

 p = doc('black_land3')
 call checked(p, t0, 60000., .false.)
 call want(LOG_WARN, p//': 3'//albedo_tail)
 call verify('black_land3')
 call checked(p, t0, 60000., .true.)
 call want(LOG_ERR, p//': 3'//albedo_tail)
 call verify('black_land3 strict')

 p = doc('density16')
 call checked(p, t0, 60000., .false.)
 call want(LOG_WARN, p//': meshDensity does not match the cell sizes for len_disp 60000.0 m in 10 cells (worst: cell *'// &
           density_tail)
 call verify('density16')
 call checked(p, t0, 60000., .true.)
 call want(LOG_ERR, p//': meshDensity does not match the cell sizes for len_disp 60000.0 m in 10 cells (worst: cell *'// &
           density_tail)
 call verify('density16 strict')

 ! fixed_by_tools
 call checked(doc('albedo_fixed'), t0, 60000., .true.)
 call verify('fixed_by_tools: fix_albedo.py')
 call checked(doc('density_fixed'), t0, 60000., .true.)
 call verify('fixed_by_tools: set_mesh_density.py')

 p = doc('density_nan')
 call checked(p, t0, 60000., .false.)
 call want(LOG_ERR, p//': meshDensity: 1 non-finite values (first at cell 3)')
 call want(LOG_WARN, p//': meshDensity does not match the cell sizes for len_disp 60000.0 m in 1 cells (worst: cell 3, *'// &
           density_tail)
 call verify('density_nan')

 p = doc('land_ice2')
 call checked(p, t0, 60000., .false.)
 call verify('land_ice2')

 p = doc('sea_ice1')
 call checked(p, t0, 60000., .false.)
 call verify('sea_ice1')

 call checked(greece, t0, 60000., .true.)
 call verify('greece_strict')

 ! len_disp: core_atmosphere/mpas_atm_core.F:165-199
 ld = effective_len_disp(3000., 0., msgs)
 call check('len_disp (3000, 0): 3000, MPAS''s lines', ld == 3000. .and. msgs%n == 2 .and. msgs%line(1)%text == '' .and. &
            has_out('Setting nominalMinDc to 3000.00 based on namelist option config_len_disp'))
 call verify('len_disp (3000, 0)')
 ld = effective_len_disp(0., 60000., msgs)
 call check('len_disp (0, 60000): 60000, MPAS''s lines', ld == 60000. .and. msgs%n == 2 .and. msgs%line(1)%text == '' .and. &
            has_out('Setting config_len_disp to 60000.0 based on nominalMinDc value in input file'))
 call verify('len_disp (0, 60000)')
 ld = effective_len_disp(3000., 60000., msgs)
 call check('len_disp (3000, 60000): 3000', ld == 3000. .and. msgs%n == 4 .and. msgs%line(1)%text == '' .and. &
            has_out('Setting nominalMinDc to 3000.00 based on namelist option config_len_disp'))
 call want(LOG_WARN, 'nominalMinDc was read from input file as a positive value (60000.0) that differs')
 call want(LOG_WARN, 'from the specified config_len_disp value (3000.00)')
 call verify('len_disp (3000, 60000)')
 ld = effective_len_disp(0., 0., msgs)
 call check('len_disp (0, 0): 0', ld == 0. .and. msgs%n == 4 .and. msgs%line(1)%text == '' .and. &
            msgs%line(1)%level == LOG_OUT)
 call want(LOG_ERR, 'Both config_len_disp and nominalMinDc are <= 0.0.')
 call want(LOG_ERR, 'Please either specify config_len_disp in the &nhyd_model namelist group,')
 call want(LOG_ERR, 'or use an input file that provides a valid value for the nominalMinDc variable.')
 call verify('len_disp (0, 0)')

 ! build_settings: the namelist values this build fixes
 call read_mesh_inputs(greece, m, inp, msgs)
 call read_state_inputs(greece, t0, m, dyn, inp, msgs)
 msgs%n = 0
 cfg%config_zd=16000.
 cfg%config_xnutr=0.3
 cfg%config_epssm_minimum=0.2
 cfg%config_epssm_maximum=0.6
 cfg%config_epssm_transition_bottom_z=20000.
 cfg%config_epssm_transition_top_z=40000.
 cfg%config_coef_3rd_order=0.3
 cfg%config_visc4_2dsmag=0.06
 cfg%config_smagorinsky_coef=0.15
 cfg%config_del4u_div_factor=8.
 cfg%config_smdiv=0.2
 cfg%config_relax_zone_divdamp_coef=7.
 cfg%config_len_disp=0.
 ld=effective_len_disp(cfg%config_len_disp,inp%nominal_min_dc,msgs)
 msgs%n=0
 cfg%config_dt = 60.0
 call lean_schemes(cfg)
 cfg%config_apply_lbcs = .true.
 call apply_run_config(cfg, m, inp, ld, 900., 900., 86400., msgs)
 call verify('build_settings: dt and radiation')
 call check('build_settings: values',dt==60..and.radt_lw==900..and.radt_sw==900..and.n_microp==1)
 call check('build_settings: constants', &
  len_disp==60000..and.zd==16000..and.xnutr==0.3.and.epssm_min==0.2.and.epssm_max==0.6.and. &
  epssm_bottom==20000..and.epssm_top==40000..and.coef_3rd_order==0.3.and.visc4_2dsmag==0.06.and. &
  smagorinsky_coef==0.15.and.del4u_div_factor==8..and.smdiv==0.2.and.relax_zone_divdamp_coef==7..and.apply_lbcs)
 call check('len_disp_from_file',len_disp==ld)
 call apply_run_config(cfg,m,inp,ld,900.,900.,21600.,msgs)
 call verify('build_settings: greeness_s')
 cfg%config_microphysics_top = 20000.0
 call apply_run_config(cfg, m, inp, ld, 900., 900., 86400., msgs)
 call want(LOG_ERR, 'config_microphysics_top = 20000.0: lean_mpas runs the microphysics on every level; the model top is at * m')
 call verify('build_settings: config_microphysics_top')
 end subroutine

 ! The schemes of the case's own namelist, resolved (the scheme set is fixed once per run: a run of the model in this
 ! process has already set it, and apply_run_config must be given the same set)
 subroutine lean_schemes(cfg)
 type(run_config), intent(inout) :: cfg
 cfg%config_microp_scheme = 'mp_thompson'
 cfg%config_convection_scheme = 'cu_grell_freitas'
 cfg%config_pbl_scheme = 'bl_mynn'
 cfg%config_gwdo_scheme = 'bl_ysu_gwdo'
 cfg%config_radt_cld_scheme = 'cld_fraction'
 cfg%config_radt_lw_scheme = 'rrtmg_lw'
 cfg%config_radt_sw_scheme = 'rrtmg_sw'
 cfg%config_sfclayer_scheme = 'sf_mynn'
 cfg%config_lsm_scheme = 'sf_noahmp'
 cfg%config_microp_re = .true.
 cfg%config_apvm_upwinding = 0.   ! the Registry's default is 0.5
 end subroutine

 subroutine input_cases_x1()
 character(len=*), parameter :: t0 = '2026-11-08_00:00:00'
 type(run_config) :: cfg
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 type(cold_inputs) :: inp
 ! the real init: 80 land-ice and 146 sea-ice cells (cases/README.md)
 call checked(x1, t0, 480000., .false.)
 call verify('x1_ice')
 call checked(x1, t0, 480000., .true.)
 call verify('x1_strict')
 call read_mesh_inputs(x1, m, inp, msgs)
 call read_state_inputs(x1, t0, m, dyn, inp, msgs)
 msgs%n = 0
 cfg%config_dt = 120.0
 call lean_schemes(cfg)
 call apply_run_config(cfg, m, inp, 480000., 1800., 1800., 86400., msgs)
 call verify('build_settings x1')
 call check('build_settings x1: global',.not.apply_lbcs)
 end subroutine

 subroutine mesh_cases_x1()
 type(dyn_mesh_type) :: m
 type(cold_inputs) :: inp
 logical :: there
 inquire(file=l55, exist=there)
 if (.not. there) then
  print '(a)', 'SKIP levels55: missing '//l55
  return
 end if
 call clear_mesh_sizes()
 call read_mesh_inputs(l55, m, inp, msgs)
 call verify('levels55')
 call check('levels55: configured', nz == 55 .and. nz1 == 56)
 call check('levels55: mesh read', allocated(m%zgrid) .and. allocated(m%rdzw))
 if (allocated(m%zgrid)) call check('levels55: zgrid', size(m%zgrid,1) == 56)
 if (allocated(m%rdzw)) call check('levels55: rdzw', size(m%rdzw) == 55)
 call clear_mesh_sizes()
 call set_mesh_sizes(2562, 7680, 5120, 45)
 end subroutine
end program
