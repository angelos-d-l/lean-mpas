! Unit checks of the scheme 'off' sets and the namelist options on the real modules:  units CASE
! One case per process, since the scheme set and the options are set once per run (an error stop on a second, different
! call is itself a case). Prints "ok   CASE ..." and exits 0, or a FAIL line and a nonzero code.
program options_units
 use schemes
 use config, only: ncells, nedges, nz, nc1, ne1, nv1, nvertices, maxedges, vertex_degree, set_mesh_sizes
 use radiation_cloud, only: radiation_cloud_incidence
 use cloud_reference, only: calc_cldincidence
 use run_settings, only: dt, set_time_step, apvm_upwinding, set_apvm_upwinding
 use lean_messages, only: message_list
 use lean_inputs, only: read_mesh_inputs, read_state_inputs, cold_inputs, inputs_failed
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use dyn_startup, only: edge_signs, inverse_geometry
 use dyn_diagnostics, only: solve_diagnostics, apvm_upwind, apvm_work_allocate, work_pv_cell => pv_cell
 implicit none
 character(len=64) :: name
 character(len=*), parameter :: greece = 'data/cases/greece_60km/inputs/greece_60km.init.nc'

 call get_command_argument(1, name)
 select case (trim(name))
 case ('schemes_default')
  ! before any call: the default set, every scheme on
  call require(micro_on .and. conv_on .and. pbl_on .and. sfclayer_on .and. lsm_on .and. lw_on .and. sw_on .and. gwdo_on, &
   'every scheme on')
  call require(moist_physics, 'moist_physics')
  call require(cloud_scheme == CLOUD_FRACTION .and. has_re == 1, 'cloud_fraction, has_re 1')
 case ('re_lw_off')
  ! lw off with config_microp_re true: MPAS's has_req stays 0 (mpas_atmphys_manager.F:747-758)
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'off', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call require(has_re == 0, 'lw off: has_re 0')
  call require(.not. lw_on .and. sw_on, 'lw off, sw on')
 case ('re_sw_off')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'off', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call require(has_re == 0, 'sw off: has_re 0')
 case ('re_both')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  ! the same set again (a second apply_run_config of the process) is allowed
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call require(has_re == 1, 'both RRTMG with Thompson and microp_re: has_re 1')
 case ('re_false')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .false.)
  call require(has_re == 0, 'microp_re false: has_re 0')
 case ('re_micro_off')
  ! no microphysics scheme: the radii have no source (the scheme test of :749-751)
  call set_schemes('off', 'off', 'off', 'off', 'off', 'rrtmg_lw', 'rrtmg_sw', 'cld_fraction', 'off', .true.)
  call require(has_re == 0, 'microphysics off: has_re 0')
  call require(.not. micro_on .and. .not. conv_on .and. lw_on .and. sw_on, 'micro off, radiation on')
  call require(moist_physics, 'radiation on keeps moist_physics')
 case ('moist_none')
  ! mpas_atmphys_control.F:310-318: the gwdo scheme does not count
  call set_schemes('off', 'off', 'off', 'off', 'off', 'off', 'off', 'off', 'bl_ysu_gwdo', .true.)
  call require(.not. moist_physics, 'all seven off: moist_physics false')
  call set_schemes('off', 'off', 'off', 'off', 'off', 'off', 'off', 'off', 'bl_ysu_gwdo', .true.)   ! the same again: allowed
 case ('cloud_incidence')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_incidence', 'bl_ysu_gwdo', .true.)
  call require(cloud_scheme == CLOUD_INCIDENCE, 'cld_incidence')
 case ('incidence_oracle')
  call incidence_oracle()
 case ('apvm_oracle_half')
  call apvm_oracle(0.5)
 case ('apvm_oracle_one')
  call apvm_oracle(1.0)
 case ('apvm_zero')
  call apvm_zero()
 case ('apvm_second_call')
  ! a second, different value is an error stop; the same value again is allowed
  call set_apvm_upwinding(0.5)
  call set_apvm_upwinding(0.5)
  call set_apvm_upwinding(1.0)
  print '(a)', 'FAIL apvm_second_call: no error stop'
  stop 3
 case ('second_call')
  ! a second call with other values: error stop, nonzero exit (the runner checks the code)
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call set_schemes('mp_thompson', 'off', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  print '(a)', 'FAIL second_call: no error stop'
  stop 3
 case default
  print '(a)', 'FAIL: unknown case '//trim(name)
  stop 2
 end select
 print '(a)', 'ok   '//trim(name)

contains

 ! lean's cld_incidence against MPAS's own calc_cldincidence (mpas_atmphys_driver_cloudiness.F:237-281, cut out unchanged by
 ! run.sh into cloud_reference.f90): columns with qc, qi and their sums around the
 ! threshold 1.e-6, below and above it, zero, exactly on it, and the values the radiation column hands over (max(0,.)).
 subroutine incidence_oracle()
 real :: qc(nz), qi(nz), mine(nz), theirs(1,nz,1), q3c(1,nz,1), q3i(1,nz,1)
 integer :: pattern, k, differ, cloudy, clear
 call set_mesh_sizes(2337, 7183, 4847, 45)
 differ = 0; cloudy = 0; clear = 0
 do pattern = 1, 8
  do k = 1, nz
   select case (pattern)
   case (1); qc(k) = 0.; qi(k) = 0.
   case (2); qc(k) = 1.e-6; qi(k) = 0.             ! exactly the threshold: not above it
   case (3); qc(k) = 1.0000002e-6; qi(k) = 0.
   case (4); qc(k) = 0.; qi(k) = 1.0000002e-6
   case (5); qc(k) = 0.6e-6; qi(k) = 0.4e-6        ! the sum straddles the threshold
   case (6); qc(k) = 0.6e-6; qi(k) = 0.5e-6
   case (7); qc(k) = 1.e-4*real(mod(k,3)); qi(k) = 1.e-7*real(mod(k,5))
   case default; qc(k) = max(0., -1.e-6 + 2.e-6*real(mod(k*7,11))/10.); qi(k) = max(0., 1.e-6*real(mod(k*3,4))/3.)
   end select
  end do
  q3c(1,:,1) = qc; q3i(1,:,1) = qi
  theirs = -1.
  call calc_cldincidence(theirs, q3c, q3i, .true., .true., 1, 1)
  call radiation_cloud_incidence(mine, qc, qi)
  differ = differ + count(transfer(mine, [0], nz) /= transfer(theirs(1,:,1), [0], nz))
  cloudy = cloudy + count(theirs > 0.5)
  clear = clear + count(theirs < 0.5)
 end do
 call require(cloudy > 20 .and. clear > 20, 'the columns hold clouded and clear layers')
 call require(differ == 0, 'cld_incidence differs from MPAS in some values')
 end subroutine

 ! Reads greece's mesh and its t=0 winds and the startup arrays; false (and a SKIP line) when the case file is absent.
 logical function greece_fixture(m, dyn, sign_vertex, sign_cell, kite, inv_area_cell, inv_dv, inv_dc, inv_area_tri)
 type(dyn_mesh_type), intent(out) :: m
 type(cold_dyn_type), intent(out) :: dyn
 real, allocatable, intent(out) :: sign_vertex(:,:), sign_cell(:,:), inv_area_cell(:), inv_dv(:), inv_dc(:), inv_area_tri(:)
 integer, allocatable, intent(out) :: kite(:,:)
 type(message_list) :: msgs
 type(cold_inputs) :: inp
 real, allocatable :: zb_cell(:,:,:), zb3_cell(:,:,:)
 logical :: there
 inquire(file=greece, exist=there)
 greece_fixture = there
 if (.not. there) then
  print '(a)', 'SKIP '//trim(name)//': no '//greece
  return
 end if
 call read_mesh_inputs(greece, m, inp, msgs)
 call read_state_inputs(greece, '2026-11-08_00:00:00', m, dyn, inp, msgs)
 call require(.not. inputs_failed(msgs), 'greece fixture read')
 allocate(sign_vertex(vertex_degree,nv1), sign_cell(maxedges,nc1), kite(maxedges,nc1))
 allocate(zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1))
 allocate(inv_area_cell(nc1), inv_dv(ne1), inv_dc(ne1), inv_area_tri(nv1))
 call edge_signs(m%cellsOnEdge, m%verticesOnEdge, m%edgesOnVertex, m%cellsOnVertex, m%nEdgesOnCell, m%edgesOnCell, &
                 m%verticesOnCell, m%zb, m%zb3, sign_vertex, sign_cell, kite, zb_cell, zb3_cell)
 call inverse_geometry(m%areaCell, m%dvEdge, m%dcEdge, m%areaTriangle, inv_area_cell, inv_dv, inv_dc, inv_area_tri)
 end function

 ! lean's pv_edge with APVM upwinding against MPAS's own lines (mpas_atm_time_integration.F:7372-7430, extracted by
 ! run.sh into apvm_oracle.inc), on greece's mesh and t=0 winds with dt 120. The two start from the same pv_edge.
 subroutine apvm_oracle(value)
 real, intent(in) :: value
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 real, allocatable :: sign_vertex(:,:), sign_cell(:,:), inv_area_cell(:), inv_dv(:), inv_dc(:), inv_area_tri(:)
 integer, allocatable :: kite(:,:)
 real, allocatable :: rho_edge(:,:), v(:,:), vorticity(:,:), divergence(:,:), ke(:,:), pv_vertex(:,:), pv_edge(:,:)
 real, allocatable :: ke_edge(:,:), ke_vertex(:,:), pv_cell(:,:), o_pv_edge(:,:), o_pv_cell(:,:), before(:,:)
 integer :: nchanged, ndiff
 if (.not. greece_fixture(m, dyn, sign_vertex, sign_cell, kite, inv_area_cell, inv_dv, inv_dc, inv_area_tri)) return
 call set_time_step(120.)
 call set_apvm_upwinding(value)
 allocate(rho_edge(nz,ne1), v(nz,ne1), vorticity(nz,nv1), divergence(nz,nc1), ke(nz,nc1), pv_vertex(nz,nv1), pv_edge(nz,ne1))
 allocate(ke_edge(nz,ne1), ke_vertex(nz,nv1), pv_cell(nz,nc1), o_pv_edge(nz,ne1), o_pv_cell(nz,nc1), before(nz,ne1))
 rho_edge = 0.; v = 0.; vorticity = 0.; divergence = 0.; ke = 0.; pv_vertex = 0.; pv_edge = 0.
 ke_edge = 0.; ke_vertex = 0.; pv_cell = 0.; o_pv_cell = 0.
 call solve_diagnostics(.true., dyn%rho, dyn%u, m%cellsOnEdge, m%dcEdge, m%dvEdge, m%edgesOnVertex, sign_vertex, &
      inv_area_tri, m%edgesOnCell, sign_cell, m%nEdgesOnCell, inv_area_cell, m%verticesOnCell, kite, m%kiteAreasOnVertex, &
      m%edgesOnEdge, m%nEdgesOnEdge, m%weightsOnEdge, m%verticesOnEdge, m%fVertex, &
      rho_edge, v, vorticity, divergence, ke, pv_vertex, pv_edge, ke_edge, ke_vertex)
 before = pv_edge
 o_pv_edge = pv_edge
 call apvm_upwind(dt, dyn%u, v, m%cellsOnEdge, m%verticesOnEdge, m%verticesOnCell, kite, m%kiteAreasOnVertex, &
      m%nEdgesOnCell, inv_area_cell, inv_dv, inv_dc, pv_vertex, pv_cell, pv_edge)
 call oracle_apvm(value, dt, nz, 1, ncells, 1, nedges, m%cellsOnEdge, m%verticesOnEdge, m%verticesOnCell, kite, &
      m%kiteAreasOnVertex, m%nEdgesOnCell, inv_area_cell, inv_dv, inv_dc, pv_vertex, dyn%u, v, o_pv_cell, o_pv_edge)
 nchanged = count(transfer(before, [0]) /= transfer(o_pv_edge, [0]))
 ndiff = count(transfer(pv_edge, [0]) /= transfer(o_pv_edge, [0]))
 call require(nchanged > nz*nedges/2, 'the oracle changes most of pv_edge (it acts)')
 call require(ndiff == 0, 'pv_edge differs from MPAS in some values')
 call require(count(transfer(pv_cell, [0]) /= transfer(o_pv_cell, [0])) == 0, 'pv_cell differs from MPAS')
 call require(apvm_upwinding == value, 'apvm_upwinding holds the value')
 end subroutine

 ! At 0.0 nothing changes: pv_edge is the plain mean of the two vertices, and pv_cell is not allocated at full size.
 subroutine apvm_zero()
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 real, allocatable :: sign_vertex(:,:), sign_cell(:,:), inv_area_cell(:), inv_dv(:), inv_dc(:), inv_area_tri(:)
 integer, allocatable :: kite(:,:)
 real, allocatable :: rho_edge(:,:), v(:,:), vorticity(:,:), divergence(:,:), ke(:,:), pv_vertex(:,:), pv_edge(:,:)
 real, allocatable :: ke_edge(:,:), ke_vertex(:,:), mean(:,:)
 integer :: e, k
 if (.not. greece_fixture(m, dyn, sign_vertex, sign_cell, kite, inv_area_cell, inv_dv, inv_dc, inv_area_tri)) return
 call require(apvm_upwinding == 0., 'the default is 0.0')
 call set_apvm_upwinding(0.0)
 allocate(rho_edge(nz,ne1), v(nz,ne1), vorticity(nz,nv1), divergence(nz,nc1), ke(nz,nc1), pv_vertex(nz,nv1), pv_edge(nz,ne1))
 allocate(ke_edge(nz,ne1), ke_vertex(nz,nv1), mean(nz,ne1))
 rho_edge = 0.; v = 0.; vorticity = 0.; divergence = 0.; ke = 0.; pv_vertex = 0.; pv_edge = 0.; ke_edge = 0.; ke_vertex = 0.
 call solve_diagnostics(.true., dyn%rho, dyn%u, m%cellsOnEdge, m%dcEdge, m%dvEdge, m%edgesOnVertex, sign_vertex, &
      inv_area_tri, m%edgesOnCell, sign_cell, m%nEdgesOnCell, inv_area_cell, m%verticesOnCell, kite, m%kiteAreasOnVertex, &
      m%edgesOnEdge, m%nEdgesOnEdge, m%weightsOnEdge, m%verticesOnEdge, m%fVertex, &
      rho_edge, v, vorticity, divergence, ke, pv_vertex, pv_edge, ke_edge, ke_vertex)
 mean = 0.
 do e = 1, nedges
  do k = 1, nz
   mean(k,e) = 0.5 * (pv_vertex(k,m%verticesOnEdge(1,e)) + pv_vertex(k,m%verticesOnEdge(2,e)))
  end do
 end do
 call require(count(transfer(pv_edge, [0]) /= transfer(mean, [0])) == 0, 'pv_edge is the plain mean at 0.0')
 call apvm_work_allocate()
 call require(.not. allocated(work_pv_cell), 'pv_cell is not allocated at 0.0')
 end subroutine

 ! MPAS's lines, verbatim, with the names and kinds MPAS gives them (the dimensions are those of lean's arrays)
 subroutine oracle_apvm(config_apvm_upwinding, dt, nVertLevels, cellStart, cellEnd, edgeStart, edgeEnd, cellsOnEdge, &
                        verticesOnEdge, verticesOnCell, kiteForCell, kiteAreasOnVertex, nEdgesOnCell, invAreaCell, &
                        invDvEdge, invDcEdge, pv_vertex, u, v, pv_cell, pv_edge)
 use mpas_kind_types, only: RKIND
 real(kind=RKIND), intent(in) :: config_apvm_upwinding, dt
 integer, intent(in) :: nVertLevels, cellStart, cellEnd, edgeStart, edgeEnd
 integer, intent(in) :: cellsOnEdge(2,ne1), verticesOnEdge(2,ne1), verticesOnCell(maxedges,nc1), kiteForCell(maxedges,nc1)
 integer, intent(in) :: nEdgesOnCell(nc1)
 real(kind=RKIND), intent(in) :: kiteAreasOnVertex(vertex_degree,nv1), invAreaCell(nc1), invDvEdge(ne1), invDcEdge(ne1)
 real(kind=RKIND), intent(in) :: pv_vertex(nz,nv1), u(nz,ne1), v(nz,ne1)
 real(kind=RKIND), intent(inout) :: pv_cell(nz,nc1), pv_edge(nz,ne1)
 real(kind=RKIND) :: gradPVt(nz,ne1), gradPVn(nz,ne1)
 real(kind=RKIND) :: r, r1, r2
 integer :: i, j, k, iCell, iEdge, iVertex
 gradPVt = 0.; gradPVn = 0.
 include 'apvm_oracle.inc'
 end subroutine

 subroutine require(condition, text)
 logical, intent(in) :: condition
 character(*), intent(in) :: text
 if (.not. condition) then
  print '(a)', 'FAIL '//trim(name)//': '//text
  stop 1
 end if
 end subroutine
end program
