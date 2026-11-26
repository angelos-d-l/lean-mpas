! Land ice, lean against MPAS 8.4.2 (tests/land_ice/run.sh): glacier columns in five regimes, four steps, through the
! unchanged MPAS driver driver_lsm_noahmp (each column a block of one cell, with its own mpas_noahmp and pools) and
! through lean's land_ice.f90 glacier_pack, glacier_step, glacier_unpack. Both start from lean's cold start of the column (noahmp_cold_start.f90,
! then start_glacier); the MPAS pools are set from it as lsm_noahmp_fromMPAS would read them. Each step both get the
! same atmosphere, radiation, precipitation and surface-layer fluxes; then every field lsm_noahmp_toMPAS writes is
! compared bit for bit with lean's column, and the surface fields lean passes back to the atmosphere with the pools.
!  1 high plateau    ground 3000 m, air 225-240 K, snow 1500-3000 mm (the cold start caps it at 2000), low sun
!  2 polar night     no sun, air 240-255 K, snow 100-500 mm
!  3 melting margin  air 272-277 K, high sun, rain
!  4 snowfall        air 255-265 K, snow from the microphysics
!  5 bare ice        no snow in the init file (the cold start gives at least 10 mm), strong wind
program land_ice_columns
 use, intrinsic :: iso_fortran_env, only: int32
 use mpas_kind_types, only: RKIND
 use config, only: nz
 use model_state
 use phys_state, only: zero_state
 use NoahmpIOVarType, only: NoahmpIO_type
 use model_startup_io, only: initialize, clear_io
 use cold_column, only: cold_cell
 use landuse_startup, only: surface_table, read_surface_table
 use noahmp_cold_start, only: cold_land_column
 use land_ice, only: glacier, glacier_of, start_glacier, finish_glacier_start, glacier_pack, glacier_step, glacier_unpack
 use land_ice_pools
 use mpas_atmphys_vars, only: mpas_noahmp, xice_threshold
 use mpas_atmphys_manager, only: year, month, day, curr_julday
 use mpas_atmphys_driver_lsm_noahmp, only: driver_lsm_noahmp
 use land_ice_exchange, only: allocate_glacier_of
 implicit none
 integer, parameter :: nreg = 5, per = 6, n = nreg*per, nsteps = 4
 type(state_type), target, save :: state
 type(NoahmpIO_type) :: io
 type(NoahmpIO_type), allocatable :: mpas_side(:)
 type(pool_set), save :: pools
 type(surface_table) :: table
 type(cold_cell) :: cell
 type(column_view) :: a
 real :: gsw(n), glw(n), coszen(n), raincv(n)
 real, allocatable :: z(:,:), theta_m(:,:), vapour(:,:)
 integer :: bad(200,nreg), surface_bad(8,nreg), nf, c, r, it, f, total
 character(24) :: names(200)
 character(8), parameter :: surface_names(8) = [character(8) :: 'skintemp','hfx','qfx','lh','qsfc','znt','snowh','t2m']
 real :: julian
 allocate(z(nz+1,n))
 allocate(theta_m(nz,n))
 allocate(vapour(nz,n))
 call allocate_glacier_of()
 call allocate_state(state); call zero_state(state)
 xice_threshold = 0.02
 year = 2026; month = 10; day = 29
 call read_surface_table('LANDUSE.TBL', table)
 call initialize(io)
 allocate(mpas_side(n))
 bad = 0; surface_bad = 0
 do c = 1, n
  r = 1 + (c-1)/per
  call make_column(c, r, mod(c-1, per))
 end do
 call finish_glacier_start()
 do it = 1, nsteps
  julian = 301. + real((it-1)*600)/86400.
  curr_julday = julian
  do c = 1, n
   r = 1 + (c-1)/per
   call step_inputs(c, r, it)
   ! MPAS
   call register_pools(pools, c)
   mpas_noahmp = mpas_side(c)
   call driver_lsm_noahmp(pools%configs, pools%mesh, pools%state, 1, pools%diag, pools%diag_physics, &
                          pools%diag_physics_noahmp, pools%output_noahmp, pools%sfc_input, it, 1, 1)
   mpas_side(c) = mpas_noahmp
   ! lean (the date as lean_model.f90 sets it)
   a = view_column(state, c)
   glacier(glacier_of(c))%yr = year; glacier(glacier_of(c))%month = month; glacier(glacier_of(c))%day = day
   glacier(glacier_of(c))%julian = julian
   call glacier_pack(a, glacier_of(c), gsw(c), glw(c), coszen(c), raincv(c), z(:,c))
   call glacier_step(glacier_of(c), it)
   call glacier_unpack(a, glacier_of(c))
   call compare_pools(glacier(glacier_of(c)), c, bad(:,r), names, nf)
   call compare_surface(c, a, surface_bad(:,r))
  end do
 end do
 total = 0
 do r = 1, nreg
  print '(a,i0,a,i0,a,i0,a,i0,a)', 'regime ', r, ': ', per*nsteps, ' column-steps, ', nf, ' Noah-MP fields, ', &
        sum(bad(1:nf,r)) + sum(surface_bad(:,r)), ' differing values'
  do f = 1, nf
   if (bad(f,r) > 0) print '(4x,a,i0)', trim(names(f))//' ', bad(f,r)
  end do
  do f = 1, 8
   if (surface_bad(f,r) > 0) print '(4x,a,i0)', trim(surface_names(f))//' (surface) ', surface_bad(f,r)
  end do
  total = total + sum(bad(1:nf,r)) + sum(surface_bad(:,r))
 end do
 print '(a,i0,a,i0,a,i0,a)', 'land ice lean/MPAS: ', n, ' columns x ', nsteps, ' steps, ', total, ' differing values'
 call report_ranges()
 if (total > 0) error stop 1
contains
 ! The cold start of one glacier column (lean's), and the MPAS pools and mpas_noahmp from it.
 subroutine make_column(c, r, j)
 integer, intent(in) :: c, r, j
 real :: s, zs, t0, snow, tk, pk, ex, es
 integer :: k
 s = real(j)/real(per-1)
 select case (r)
 case (1); zs = 3000.; t0 = 225. + 15.*s; snow = 1500. + 1500.*s
 case (2); zs = 1500.; t0 = 240. + 15.*s; snow = 100. + 400.*s
 case (3); zs = 200.;  t0 = 272. + 5.*s;  snow = 50. + 300.*s
 case (4); zs = 1000.; t0 = 255. + 10.*s; snow = 200. + 100.*s
 case default; zs = 500.; t0 = 265.; snow = 0.
 end select
 do k = 1, nz+1
  z(k,c) = zs + 30000.*(real(k-1)/real(nz))**1.6
 end do
 pk = 101000.*exp(-zs/8000.)
 do k = 1, nz
  tk = max(205., t0 - 0.0065*(0.5*(z(k,c)+z(k+1,c)) - zs))
  if (k > 1) pk = pk*exp(-9.80616*0.5*((z(k,c)-z(k-1,c)) + (z(k+1,c)-z(k,c)))/(287.*tk))
  es = 611.2*exp(17.67*(tk-273.15)/(tk-29.65))
  vapour(k,c) = max(1.e-7, 0.622*0.75*es/(pk - 0.75*es))
  ex = (pk/1.e5)**(287./1004.5)
  theta_m(k,c) = tk/ex*(1. + 461.6/287.*vapour(k,c))
  p_diag_exner(k,c) = ex
  p_diag_pressure_base(k,c) = pk - 50.*s
  p_diag_pressure_p(k,c) = 50.*s
 end do
 p_mesh_zgrid(:,c) = z(:,c)
 ! the cold-start record of a land-ice cell (init_atmosphere: ivgtyp 15 = isice, isltyp 16 = land ice)
 cell%skintemp = t0 - 2.; cell%sst = 0.; cell%xice = 0.; cell%seaice = 0.; cell%xland = 1.; cell%landmask = 1
 cell%surface_pressure = pk; cell%snow = snow; cell%snowh = snow*0.003; cell%snowc = merge(1., 0., snow > 0.)
 cell%tmn = t0 - 5. - 10.*s; cell%vegfra = 0.; cell%shdmin = 0.; cell%shdmax = 0.; cell%sfc_albbck = 0.7
 cell%snoalb = 0.8; cell%ivgtyp = 15; cell%isltyp = 16
 cell%tslb = [t0 - 3., t0 - 4., t0 - 6., t0 - 8.]
 cell%smois = 1.; cell%sh2o = 0.; cell%dzs = [0.1, 0.3, 0.6, 1.0]
 cell%albedo12m = 70.; cell%greenfrac = 0.
 cell%latCell = -1.3 + 0.1*s; cell%lonCell = 2.0*s; cell%areaCell = 1.2e10
 a = view_column(state, c)
 call clear_io(io)
 io%julian = 301.
 call cold_land_column(io, cell, a, table, 302, z(:,c))
 a%snowh = io%snowh(1)
 call start_glacier(c, io)
 mpas_side(c) = glacier(glacier_of(c))
 call set_pools_from_io(glacier(glacier_of(c)), c)
 end subroutine

 ! The inputs of step it, the same for both sides: atmosphere, last radiation, rain and snow of the last step, and the
 ! fields the surface layer wrote since the last land step.
 subroutine step_inputs(c, r, it)
 integer, intent(in) :: c, r, it
 real :: s, uu, vv
 integer :: k
 s = real(mod(c-1, per))/real(per-1)
 coszen(c) = merge(-0.2, merge(0.25, 0.6, r == 1 .or. r == 4), r == 2) + 0.02*it
 gsw(c) = max(0., coszen(c))*merge(250., 120., r == 3)*(1. + 0.1*s)
 glw(c) = 5.67e-8*(theta_m(1,c)*p_diag_exner(1,c) - 10.)**4
 raincv(c) = merge(0.3*s, 0., r == 3)
 uu = merge(14., 4.*s, r == 5); vv = -2. + it
 do k = 1, nz
  p_state_theta_m(k,c) = theta_m(k,c) - 0.05*it
  p_state_scalars(:,k,c) = 0.; p_state_scalars(1,k,c) = vapour(k,c)
  p_diag_uReconstructZonal(k,c) = uu*(1. + 0.03*k); p_diag_uReconstructMeridional(k,c) = vv*(1. + 0.03*k)
 end do
 p_diag_physics_coszr(c) = coszen(c); p_diag_physics_gsw(c) = gsw(c); p_diag_physics_glw(c) = glw(c)
 p_diag_physics_swddir(c) = 0.6*gsw(c); p_diag_physics_swddif(c) = 0.4*gsw(c)   ! lean gives 0: read only by urban panels
 p_diag_physics_raincv(c) = raincv(c)
 p_diag_physics_sr(c) = merge(1., 0., r == 4); p_diag_physics_rainncv(c) = merge(0.5, merge(0.2*s, 0., r == 3), r == 4)
 p_diag_physics_snowncv(c) = merge(0.5, 0., r == 4); p_diag_physics_graupelncv(c) = merge(0.05, 0., r == 4)
 ! what the surface layer would have written over this land cell
 p_diag_physics_hfx(c) = 20.*s - 10.*it; p_diag_physics_qfx(c) = 1.e-6*it; p_diag_physics_lh(c) = 3.*it
 p_diag_physics_qsfc(c) = 0.9*vapour(1,c); p_diag_physics_znt(c) = 0.002 + 0.0005*it
 ! lean's column: the same, through its physics inputs (theta from theta_m, p = p' + p_base)
 a = view_column(state, c)
 do k = 1, nz
  a%qv(k) = vapour(k,c)
  a%theta(k) = p_state_theta_m(k,c)/(1. + 461.6/287.*a%qv(k))
  a%exner(k) = p_diag_exner(k,c)
  a%p(k) = p_diag_pressure_p(k,c) + p_diag_pressure_base(k,c)
  a%u(k) = p_diag_uReconstructZonal(k,c); a%v(k) = p_diag_uReconstructMeridional(k,c)
 end do
 a%sr = p_diag_physics_sr(c); a%rainncv = p_diag_physics_rainncv(c); a%snowncv = p_diag_physics_snowncv(c)
 a%graupelncv = p_diag_physics_graupelncv(c)
 a%hfx = p_diag_physics_hfx(c); a%qfx = p_diag_physics_qfx(c); a%lh = p_diag_physics_lh(c)
 a%qsfc = p_diag_physics_qsfc(c); a%znt = p_diag_physics_znt(c)
 a%psfc = p_diag_pressure_base(1,c)
 end subroutine

 subroutine compare_surface(c, a, b)
 integer, intent(in) :: c
 type(column_view), intent(in) :: a
 integer, intent(inout) :: b(8)
 if (differ(a%tsk, p_sfc_input_skintemp(c))) b(1) = b(1) + 1
 if (differ(a%hfx, p_diag_physics_hfx(c))) b(2) = b(2) + 1
 if (differ(a%qfx, p_diag_physics_qfx(c))) b(3) = b(3) + 1
 if (differ(a%lh, p_diag_physics_lh(c))) b(4) = b(4) + 1
 if (differ(a%qsfc, p_diag_physics_qsfc(c))) b(5) = b(5) + 1
 if (differ(a%znt, p_diag_physics_znt(c))) b(6) = b(6) + 1
 if (differ(a%snowh, p_sfc_input_snowh(c))) b(7) = b(7) + 1
 if (differ(a%t2, p_output_noahmp_t2mxy(c))) b(8) = b(8) + 1
 end subroutine

 subroutine report_ranges()
 integer :: rr, i0, i1
 do rr = 1, nreg
  i0 = (rr-1)*per + 1; i1 = rr*per
  print '(a,i0,a,2f9.2,a,2f9.2,a,2f8.1,a,2f9.1,a,2f7.3)', 'regime ', rr, ' skin ', minval(p_sfc_input_skintemp(i0:i1)), &
   maxval(p_sfc_input_skintemp(i0:i1)), ' t2 ', minval(p_output_noahmp_t2mxy(i0:i1)), maxval(p_output_noahmp_t2mxy(i0:i1)), &
   ' hfx ', minval(p_diag_physics_hfx(i0:i1)), maxval(p_diag_physics_hfx(i0:i1)), ' swe ', minval(p_sfc_input_snow(i0:i1)), &
   maxval(p_sfc_input_snow(i0:i1)), ' albedo ', minval(p_diag_physics_sfc_albedo(i0:i1)), maxval(p_diag_physics_sfc_albedo(i0:i1))
 end do
 end subroutine
end program
