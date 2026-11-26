! Sea ice, lean against MPAS 8.4.2 (tests/sea_ice/run.sh): synthetic columns in seven regimes, three steps each, through
! the unchanged MPAS drivers (sea_ice_original, tests/sea_ice/original.f90) and through lean's sea_ice.f90 with the 2 m
! diagnostics of pre_dynamics.f90 (surface_diagnostics on a view that says water). Every surface field both sides
! write is compared bit for bit after every step. Prints one line per regime and a total; exits 1 on any difference.
!  1 open water        xice 0, xland 2 (the open-water call of the original runs and is thrown away)
!  2 thin ice          xice 0.02-0.2, skin colder than 253 K (the skin limits of correct_tsk_over_seaice)
!  3 fractional ice    xice 0.2-0.5 (regime from the blended call)
!  4 thick ice         xice 0.5-1 (regime kept from before the call), no snow
!  5 snow on ice       xice 0.6-1, snow 5-80 mm, skin around 273 K (the snow branches of seaice_noah)
!  6 polar night       xice 0.9-1, no sun, air 225-245 K, skin 230-250 K
!  7 melting, rain     xice 0.3-0.9, air 274-279 K, rain (not frozen) and snow
program sea_ice_columns
 use, intrinsic :: iso_fortran_env, only: int32
 use config, only: nz, ncells
 use model_state
 use mynn, only: mynn_init, mynn_surface_column
 use noahmp_state, only: noahmp_state_type, noahmp_column, noahmp_view, allocate_land
 use phys_state, only: zero_land, zero_state
 use noahmp_coupling, only: surface_diagnostics
 use sea_ice, only: ice, start_sea_ice, ice_cell, open_water, ice_surface_layer, ice_step, hydrostatic_interfaces, ice_cells, allocate_ice, copy_ice
 use sea_ice_original, only: surface_pools, setup, run_step
 use mpas_atmphys_vars, only: pres_hyd_p, pi_p, psfc_p, th_p, t_p, u_p, v_p, qv_p, qc_p, rho_p, dz_p, pres2_hyd_p
 implicit none
 integer, parameter :: nreg = 7, per = 16, n = nreg*per, nsteps = 3, nfields = 51
 type(state_type), target, save :: state
 type(noahmp_state_type), target, save :: lands
 type(surface_pools), target, save :: p
 type(column_view) :: a, b
 type(noahmp_column) :: land
 type(ice_cells) :: saved_ice
 type(open_water) :: water
 real, target :: open_water_mask
 real :: gsw(n), glw(n), raincv(n)
 real, allocatable :: z(:,:), interfaces(:)
 real :: o(nfields), l(nfields)
 integer :: bad(nfields,nreg), c, r, it, k, f, total, cells_compared(nreg)
 real :: worst(nfields,nreg)   ! largest |lean - MPAS| of a field
 real, parameter :: dt_step = 600.
 character(12), parameter :: names(nfields) = [character(12) :: 'hfx','qfx','lh','skintemp','qsfc','chs','chs2','cqs2', &
  'cpm','qgh','br','flhc','flqc','gz1oz0','mol','psih','psim','rmol','ust','wspd','zol','regime','u10','v10','cd','cda', &
  'ck','cka','ustm','ch','znt','snowh','t2m','q2','th2m','snow','snowc','tslb1','tslb2','tslb3','tslb4','z0', &
  'sfc_albedo','sfc_emiss','grdflx','acsnow','acsnom','sfcrunoff','mavail','xland','xice']
 allocate(z(nz+1,n))
 allocate(interfaces(nz+1))
 if (n > ncells) error stop 'harness: more columns than the build has cells'
 call allocate_ice()
 call test_copy_ice()
 call setup(p, n, dt_step)
 call mynn_init()
 call allocate_state(state); call zero_state(state)
 call allocate_land(lands); call zero_land(lands)   ! allocate the members, then zero them
 bad = 0; cells_compared = 0; worst = 0.
 do c = 1, n
  r = 1 + (c-1)/per
  call make_column(c, r, mod(c-1, per))
 end do
 do it = 1, nsteps
  do c = 1, n
   gsw(c) = merge(0., p%gsw(c), 1 + (c-1)/per == 6)
   p%gsw(c) = gsw(c)*(1. + 0.1*(it-1)); gsw(c) = p%gsw(c)
   glw(c) = p%glw(c); raincv(c) = p%raincv(c)
  end do
  call run_step(p, it)
  do c = 1, n
   a = view_column(state, c)
   land = noahmp_view(lands, c)
   if (ice_cell(c)) then
    call ice_surface_layer(a, c, it, water)
    call ice_step(a, c, water, gsw(c), glw(c), raincv(c), z(:,c))
    b = a
    open_water_mask = 2.
    b%xland => open_water_mask
    call surface_diagnostics(b, land)
   else
    call mynn_surface_column(a, it)
    call surface_diagnostics(a, land)
   end if
   call fields_original(c, o)
   call fields_lean(c, a, l)
   r = 1 + (c-1)/per
   cells_compared(r) = cells_compared(r) + 1
   do f = 1, nfields
    if (transfer(o(f), 0_int32) /= transfer(l(f), 0_int32)) then
     bad(f,r) = bad(f,r) + 1
     worst(f,r) = max(worst(f,r), abs(l(f) - o(f)))
    end if
   end do
  end do
 end do
 total = 0
 do r = 1, nreg
  print '(a,i0,a,i0,a,i0,a)', 'regime ', r, ': ', cells_compared(r), ' column-steps, ', sum(bad(:,r)), ' differing values'
  do f = 1, nfields
   if (bad(f,r) > 0) print '(4x,a,i4,a,es10.3)', names(f)//' ', bad(f,r), '  largest |lean-MPAS| ', worst(f,r)
  end do
  total = total + sum(bad(:,r))
 end do
 print '(a,i0,a,i0,a,i0,a)', 'sea ice lean/MPAS: ', n, ' columns x ', nsteps, ' steps, ', total, ' differing values'
 call report_ranges()
 if (total > 0) error stop 1
contains

 subroutine test_copy_ice()
 if (.not. all(ice%xice == 0.)) error stop 'allocate_ice xice'
 ice%xice = 1.
 if (.not. all(ice%sst == 0.)) error stop 'allocate_ice sst'
 ice%sst = 2.
 if (.not. all(ice%tslb == 0.)) error stop 'allocate_ice tslb'
 ice%tslb = 3.
 if (.not. all(ice%snow == 0.)) error stop 'allocate_ice snow'
 ice%snow = 4.
 if (.not. all(ice%snowc == 0.)) error stop 'allocate_ice snowc'
 ice%snowc = 5.
 if (.not. all(ice%snoalb == 0.)) error stop 'allocate_ice snoalb'
 ice%snoalb = 6.
 if (.not. all(ice%z0 == 0.)) error stop 'allocate_ice z0'
 ice%z0 = 7.
 if (.not. all(ice%albedo == 0.)) error stop 'allocate_ice albedo'
 ice%albedo = 8.
 if (.not. all(ice%emiss == 0.)) error stop 'allocate_ice emiss'
 ice%emiss = 9.
 if (.not. all(ice%grdflx == 0.)) error stop 'allocate_ice grdflx'
 ice%grdflx = 10.
 if (.not. all(ice%acsnow == 0.)) error stop 'allocate_ice acsnow'
 ice%acsnow = 11.
 if (.not. all(ice%acsnom == 0.)) error stop 'allocate_ice acsnom'
 ice%acsnom = 12.
 if (.not. all(ice%sfcrunoff == 0.)) error stop 'allocate_ice sfcrunoff'
 ice%sfcrunoff = 13.
 call copy_ice(ice, saved_ice)
 ice%xice = -1.
 ice%sst = -1.
 ice%tslb = -1.
 ice%snow = -1.
 ice%snowc = -1.
 ice%snoalb = -1.
 ice%z0 = -1.
 ice%albedo = -1.
 ice%emiss = -1.
 ice%grdflx = -1.
 ice%acsnow = -1.
 ice%acsnom = -1.
 ice%sfcrunoff = -1.
 call copy_ice(saved_ice, ice)
 if (.not. all(ice%xice == 1.)) error stop 'copy_ice xice'
 if (.not. all(ice%sst == 2.)) error stop 'copy_ice sst'
 if (.not. all(ice%tslb == 3.)) error stop 'copy_ice tslb'
 if (.not. all(ice%snow == 4.)) error stop 'copy_ice snow'
 if (.not. all(ice%snowc == 5.)) error stop 'copy_ice snowc'
 if (.not. all(ice%snoalb == 6.)) error stop 'copy_ice snoalb'
 if (.not. all(ice%z0 == 7.)) error stop 'copy_ice z0'
 if (.not. all(ice%albedo == 8.)) error stop 'copy_ice albedo'
 if (.not. all(ice%emiss == 9.)) error stop 'copy_ice emiss'
 if (.not. all(ice%grdflx == 10.)) error stop 'copy_ice grdflx'
 if (.not. all(ice%acsnow == 11.)) error stop 'copy_ice acsnow'
 if (.not. all(ice%acsnom == 12.)) error stop 'copy_ice acsnom'
 if (.not. all(ice%sfcrunoff == 13.)) error stop 'copy_ice sfcrunoff'
 print '(a)', 'COPY ICE OK 13 members, both ways'
 ice%xice = 0.
 ice%sst = 0.
 ice%tslb = 0.
 ice%snow = 0.
 ice%snowc = 0.
 ice%snoalb = 0.
 ice%z0 = 0.
 ice%albedo = 0.
 ice%emiss = 0.
 ice%grdflx = 0.
 ice%acsnow = 0.
 ice%acsnom = 0.
 ice%sfcrunoff = 0.

 end subroutine

 ! A hydrostatic column and its surface: air temperature t0 at the ground falling 6.5 K/km (not below 205 K), relative
 ! humidity 70 %, wind (u0, v0), the ground at 0-300 m. Both sides get the same numbers.
 subroutine make_column(c, r, j)
 integer, intent(in) :: c, r, j
 real :: t0, u0, v0, xice, tsk, snow, sst, zs, tk, rh, es, pk, ex
 integer :: kk
 real :: s
 s = real(j)/real(per-1)                 ! 0..1 inside the regime
 zs = 0.
 select case (r)
 case (1); xice = 0.;            t0 = 285. + 8.*s;  tsk = 288. + 5.*s;   snow = 0.;           sst = tsk
 case (2); xice = 0.02 + 0.18*s; t0 = 245. + 10.*s; tsk = 240. + 12.*s;  snow = 0.;           sst = 271.0
 case (3); xice = 0.2 + 0.29*s;  t0 = 255. + 10.*s; tsk = 258. + 8.*s;   snow = 2.*s;         sst = 271.3
 case (4); xice = 0.5 + 0.5*s;   t0 = 250. + 15.*s; tsk = 248. + 18.*s;  snow = 0.;           sst = 271.2
 case (5); xice = 0.6 + 0.4*s;   t0 = 268. + 8.*s;  tsk = 270. + 5.*s;   snow = 5. + 75.*s;   sst = 271.4
 case (6); xice = 0.9 + 0.1*s;   t0 = 225. + 20.*s; tsk = 230. + 20.*s;  snow = 20.*s;        sst = 271.35
 case default; xice = 0.3 + 0.6*s; t0 = 274. + 5.*s; tsk = 272. + 3.*s; snow = 3.*s;         sst = 272.
 end select
 u0 = 2. + 10.*s; v0 = -3. + 6.*mod(real(j),3.)
 if (r == 6 .and. mod(j,4) == 0) then; u0 = 0.3; v0 = 0.2; end if   ! calm, very stable
 do kk = 1, nz+1
  z(kk,c) = zs + 30000.*(real(kk-1)/real(nz))**1.6
 end do
 a = view_column(state, c)
 ! layer temperature, pressure by layers from 1010 hPa at the ground
 pk = 101000. - 1200.*s
 do kk = 1, nz
  tk = max(205., t0 - 0.0065*(0.5*(z(kk,c)+z(kk+1,c)) - zs))
  rh = 0.7
  es = 611.2*exp(17.67*(tk-273.15)/(tk-29.65))
  a%dz(kk) = z(kk+1,c) - z(kk,c)
  if (kk > 1) pk = pk*exp(-9.80616*0.5*(a%dz(kk-1)+a%dz(kk))/(287.*tk))
  a%p(kk) = pk
  a%qv(kk) = max(1.e-7, 0.622*rh*es/(pk - rh*es))
  a%qc(kk) = 0.
  if (r == 7 .and. kk <= 3) a%qc(kk) = 2.e-4
  ex = (pk/1.e5)**(287./1004.5)
  a%exner(kk) = ex
  a%theta(kk) = tk/ex
  a%rho_moist(kk) = pk/(287.*tk*(1. + 0.608*a%qv(kk)))
  a%u(kk) = u0*(1. + 0.02*kk); a%v(kk) = v0*(1. + 0.02*kk)
 end do
 call hydrostatic_interfaces(a, z(:,c), interfaces)
 do kk = 1, nz
  a%p_hyd(kk) = 0.5*(interfaces(kk+1) + interfaces(kk))
 end do
 a%psfc = interfaces(1) + 3.*s
 a%dx = 120000.
 ! the original's physics inputs (MPAS_to_physics)
 do kk = 1, nz
  pres_hyd_p(c,kk,1) = a%p_hyd(kk); pi_p(c,kk,1) = a%exner(kk); th_p(c,kk,1) = a%theta(kk)
  t_p(c,kk,1) = th_p(c,kk,1)*pi_p(c,kk,1); u_p(c,kk,1) = a%u(kk); v_p(c,kk,1) = a%v(kk); qv_p(c,kk,1) = a%qv(kk)
  qc_p(c,kk,1) = a%qc(kk); rho_p(c,kk,1) = a%rho_moist(kk); dz_p(c,kk,1) = a%dz(kk)
 end do
 do kk = 1, nz+1
  pres2_hyd_p(c,kk,1) = interfaces(kk)
 end do
 psfc_p(c,1) = a%psfc
 ! the surface, the same on both sides (a cold start: fluxes and exchange coefficients 0)
 p%xice(c) = xice; p%sst(c) = sst; p%skintemp(c) = tsk; p%xland(c) = merge(2., 1., r == 1)
 p%snow(c) = snow; p%snowh(c) = snow*0.005; p%snowc(c) = merge(1., 0., snow > 0.); p%snoalb(c) = 0.75
 p%znt(c) = merge(0.0001, 0.001, r == 1); p%z0(c) = p%znt(c); p%mavail(c) = merge(1., 1., r == 1)
 p%hpbl(c) = 300. + 500.*s; p%qcg(c) = 0.; p%ch(c) = 0.
 p%br(c) = 0.; p%cpm(c) = 0.; p%chs(c) = 0.; p%chs2(c) = 0.; p%cqs2(c) = 0.; p%flhc(c) = 0.; p%flqc(c) = 0.
 p%gz1oz0(c) = 0.; p%hfx(c) = 0.; p%qfx(c) = 0.; p%qgh(c) = 0.; p%qsfc(c) = 0.; p%lh(c) = 0.; p%mol(c) = 0.
 p%psih(c) = 0.; p%psim(c) = 0.; p%regime(c) = 0.; p%rmol(c) = 0.; p%ust(c) = 0.0001; p%ustm(c) = 0.; p%wspd(c) = 0.
 p%zol(c) = 0.; p%q2(c) = 0.; p%t2m(c) = 0.; p%th2m(c) = 0.; p%u10(c) = 0.; p%v10(c) = 0.; p%cd(c) = 0.; p%cda(c) = 0.
 p%ck(c) = 0.; p%cka(c) = 0.; p%fh(c) = 0.; p%fm(c) = 0.
 p%acsnom(c) = 0.; p%acsnow(c) = 0.; p%grdflx(c) = 0.; p%sfcrunoff(c) = 0.
 p%glw(c) = 5.67e-8*(t0 - 8.)**4; p%gsw(c) = 250.*s
 p%sr(c) = merge(0.3, 0., r == 7); p%rainncv(c) = merge(0.4*s, 0., r == 7 .or. r == 5); p%raincv(c) = merge(0.2, 0., r == 7)
 p%sfc_albedo(c) = xice*(0.55 + 0.2*s) + (1.-xice)*0.08; p%sfc_emiss(c) = xice*0.95 + (1.-xice)*0.98
 do kk = 1, 4
  p%tslb(kk,c) = tsk + (271.4 - tsk)*(real(kk) - 0.5)/4.
 end do
 p%meshDensity(c) = 1.; p%areaCell(c) = 1.2e10; p%surface_pressure(c) = a%psfc; p%q2mxy(c) = 0.; p%t2mxy(c) = 0.
 ! lean: the column view and the sea-ice cells (sea_ice.f90)
 a%xland = p%xland(c); a%tsk = tsk; a%snowh = p%snowh(c); a%znt = p%znt(c); a%mavail = p%mavail(c)
 a%pblh = p%hpbl(c); a%ch = 0.
 a%br = 0.; a%cpm = 0.; a%chs = 0.; a%chs2 = 0.; a%cqs2 = 0.; a%flhc = 0.; a%flqc = 0.; a%gz1oz0 = 0.; a%hfx = 0.
 a%qfx = 0.; a%qgh = 0.; a%qsfc = 0.; a%lh = 0.; a%mol = 0.; a%psih = 0.; a%psim = 0.; a%regime = 0.; a%rmol = 0.
 a%ust = 0.0001; a%ustm = 0.; a%wspd = 0.; a%zol = 0.; a%q2 = 0.; a%t2 = 0.; a%th2 = 0.; a%u10 = 0.; a%v10 = 0.
 a%cd = 0.; a%cda = 0.; a%ck = 0.; a%cka = 0.; a%wstar = 0.; a%qstar = 0.
 a%sr = p%sr(c); a%rainncv = p%rainncv(c)
 call start_sea_ice(c, xice, sst, p%snoalb(c), snow, p%tslb(:,c), p%znt(c), p%sfc_albedo(c), p%sfc_emiss(c))
 end subroutine

 subroutine fields_original(c, v)
 integer, intent(in) :: c
 real, intent(out) :: v(nfields)
 v = [p%hfx(c), p%qfx(c), p%lh(c), p%skintemp(c), p%qsfc(c), p%chs(c), p%chs2(c), p%cqs2(c), p%cpm(c), p%qgh(c), &
      p%br(c), p%flhc(c), p%flqc(c), p%gz1oz0(c), p%mol(c), p%psih(c), p%psim(c), p%rmol(c), p%ust(c), p%wspd(c), &
      p%zol(c), p%regime(c), p%u10(c), p%v10(c), p%cd(c), p%cda(c), p%ck(c), p%cka(c), p%ustm(c), p%ch(c), p%znt(c), &
      p%snowh(c), p%t2m(c), p%q2(c), p%th2m(c), merge(p%snow(c), 0., p%xice(c) >= 0.02), &
      merge(p%snowc(c), 0., p%xice(c) >= 0.02), merge(p%tslb(:,c), 0., p%xice(c) >= 0.02), &
      merge(p%z0(c), 0., p%xice(c) >= 0.02), merge(p%sfc_albedo(c), 0., p%xice(c) >= 0.02), &
      merge(p%sfc_emiss(c), 0., p%xice(c) >= 0.02), merge(p%grdflx(c), 0., p%xice(c) >= 0.02), &
      merge(p%acsnow(c), 0., p%xice(c) >= 0.02), merge(p%acsnom(c), 0., p%xice(c) >= 0.02), &
      merge(p%sfcrunoff(c), 0., p%xice(c) >= 0.02), p%mavail(c), p%xland(c), p%xice(c)]
 end subroutine

 subroutine fields_lean(c, a, v)
 integer, intent(in) :: c
 type(column_view), intent(in) :: a
 real, intent(out) :: v(nfields)
 logical :: on
 on = ice_cell(c)
 v = [a%hfx, a%qfx, a%lh, a%tsk, a%qsfc, a%chs, a%chs2, a%cqs2, a%cpm, a%qgh, a%br, a%flhc, a%flqc, a%gz1oz0, a%mol, &
      a%psih, a%psim, a%rmol, a%ust, a%wspd, a%zol, a%regime, a%u10, a%v10, a%cd, a%cda, a%ck, a%cka, a%ustm, a%ch, &
      a%znt, a%snowh, a%t2, a%q2, a%th2, merge(ice%snow(c), 0., on), merge(ice%snowc(c), 0., on), &
      merge(ice%tslb(:,c), 0., on), merge(ice%z0(c), 0., on), merge(ice%albedo(c), 0., on), merge(ice%emiss(c), 0., on), &
      merge(ice%grdflx(c), 0., on), merge(ice%acsnow(c), 0., on), merge(ice%acsnom(c), 0., on), &
      merge(ice%sfcrunoff(c), 0., on), a%mavail, a%xland, ice%xice(c)]
 end subroutine

 ! The ranges the regimes reached (MPAS side, last step): the test covers what it says it covers.
 subroutine report_ranges()
 integer :: rr, i0, i1
 do rr = 1, nreg
  i0 = (rr-1)*per + 1; i1 = rr*per
  print '(a,i0,a,2f9.2,a,2f9.2,a,2f8.1,a,2f8.4,a,2f8.4)', 'regime ', rr, ' skin ', minval(p%skintemp(i0:i1)), &
   maxval(p%skintemp(i0:i1)), ' t2m ', minval(p%t2m(i0:i1)), maxval(p%t2m(i0:i1)), ' hfx ', minval(p%hfx(i0:i1)), &
   maxval(p%hfx(i0:i1)), ' albedo ', minval(p%sfc_albedo(i0:i1)), maxval(p%sfc_albedo(i0:i1)), ' ust ', &
   minval(p%ust(i0:i1)), maxval(p%ust(i0:i1))
 end do
 end subroutine
end program
