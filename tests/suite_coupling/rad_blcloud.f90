! LEAN_RAD_BLCLOUD on single columns: lean's radiation (radiation_column.f90, RRTMG tables of reference/radiation) without
! and with MYNN's subgrid cloud merged by bl_cloud_water (radiation_blcloud.f90). Synthetic 45-level columns near Athens in
! late October: night fog/stratus, daytime shallow cumulus, resolved and subgrid cloud on the same levels, a fraction below
! the gate, cold-layer ice, no subgrid cloud. Prints the effect; stops with code 1 if a check fails (the mutants must).
program rad_blcloud
 use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
 use config, only: nz
 use radiation_tables, only: initialize_radiation_tables
 use radiation_climatology, only: initialize_ozone_tables, monthly_ozone
 use radiation_solar, only: radiation_solar_constants
 use radiation_column, only: radiation_event
 use radiation_sizes, only: max_sw_layers, max_lw_layers
 use radiation_blcloud, only: bl_cloud_water
 implicit none
 character(1024) :: directory
 real, allocatable :: theta(:), exner(:), p(:), rho(:), dz(:), z(:), qv(:), qc(:), qi(:), qs(:)
 real, allocatable :: qc_bl(:), qi_bl(:), cldfra_bl(:), qc_rad(:), qi_rad(:), re(:,:)
 real :: pin(59), monthly(12,59), julian, declin, solcon, xtime, gmt
 real :: off(2), on(2)
 real, allocatable :: cloud_off(:), cloud_on(:), heat_off(:,:), heat_on(:,:)
 integer :: case_id, k, fails
 logical :: same
 allocate(theta(nz))
 allocate(exner(nz))
 allocate(p(nz))
 allocate(rho(nz))
 allocate(dz(nz))
 allocate(z(nz+1))
 allocate(qv(nz))
 allocate(qc(nz))
 allocate(qi(nz))
 allocate(qs(nz))
 allocate(qc_bl(nz))
 allocate(qi_bl(nz))
 allocate(cldfra_bl(nz))
 allocate(qc_rad(nz))
 allocate(qi_rad(nz))
 allocate(re(nz,3))
 allocate(cloud_off(nz))
 allocate(cloud_on(nz))
 allocate(heat_off(nz,2))
 allocate(heat_on(nz,2))
 call get_command_argument(1, directory)
 call initialize_radiation_tables(trim(directory))
 call initialize_ozone_tables(trim(directory), pin)
 call monthly_ozone(38.*3.14159265/180., monthly)
 fails = 0
 do case_id = 1, 6
  call column(case_id)
  ! day 293 (20 October): night at 00 UTC (02 local), day at 10 UTC (12 local)
  gmt = 0.; xtime = 0.
  if (case_id == 2) xtime = 600.
  julian = 293. + (gmt + xtime/60.)/24.
  call radiation_solar_constants(julian, declin, solcon)
  call bl_cloud_water(qc, qi, qc_bl, qi_bl, cldfra_bl, qc_rad, qi_rad)
  call run(qc, qi, off, cloud_off, heat_off)
  call run(qc_rad, qi_rad, on, cloud_on, heat_on)
  same = all(off == on) .and. all(cloud_off == cloud_on) .and. all(heat_off == heat_on)
  print '(a,i2,a,2f9.2,a,2f9.2,a,2f6.3,a,es10.2)', ' case', case_id, '  surface SW net, LW down (W/m2) off', off, ' on', on, &
   '  max cloud fraction off/on', maxval(cloud_off), maxval(cloud_on), '  max |d heating| K/s', maxval(abs(heat_on - heat_off))
  if (.not. (all(ieee_is_finite(on)) .and. all(ieee_is_finite(heat_on)) .and. all(ieee_is_finite(cloud_on)))) call fail('not finite')
  select case (case_id)
  case (1)   ! night fog/stratus: more LW down, cooling moves to the cloud top
   if (.not. on(2) > off(2) + 5.) call fail('night subgrid stratus did not raise the LW down by 5 W/m2')
  case (2)   ! shallow cumulus at noon: less SW at the surface
   if (.not. on(1) < off(1) - 5.) call fail('subgrid cumulus did not lower the surface SW by 5 W/m2')
  case (3)   ! resolved cloud on levels 3-5 keeps its water there; the subgrid cloud above it is added
   if (any(qc_rad(3:5) /= qc(3:5))) call fail('subgrid water added on top of resolved cloud')
   if (.not. all(qc_rad(6:8) > qc(6:8))) call fail('subgrid water not added above the resolved cloud')
  case (4)   ! cldfra_bl at the gate: nothing changes
   if (.not. same) call fail('a subgrid fraction <= 0.001 changed the radiation')
  case (5)   ! cold layer: qi_bl reaches the radiation
   if (.not. all(qi_rad(30:32) > qi(30:32))) call fail('subgrid ice not added')
   if (same) call fail('subgrid ice did not change the radiation')
  case (6)   ! no subgrid cloud: bit-identical
   if (.not. same) call fail('no subgrid cloud, yet the radiation changed')
  end select
 enddo
 if (fails > 0) then
  print '(a,i0,a)', 'RESULT FAIL (', fails, ' checks)'
  stop 1
 endif
 print '(a)', 'RESULT PASS'
contains
 subroutine fail(text)
 character(*), intent(in) :: text
 print '(2a)', '  FAIL: ', text
 fails = fails + 1
 end subroutine

 subroutine run(qc_in, qi_in, surface, cloud, heating)
 real, intent(in) :: qc_in(nz), qi_in(nz)
 real, intent(out) :: surface(2), cloud(nz), heating(nz,2)
 real :: sw(14), lw(11), sw_flux(4,max_sw_layers+1), lw_flux(4,max_lw_layers+1), radii(nz,3), ozone(59)
 integer :: active
 call radiation_event(theta, exner, p, rho, dz, z, qv, qc_in, qi_in, qs, re(:,1), re(:,2), re(:,3), &
  290., 0.95, 1., 0., 0., 0.18, 38.*3.14159265/180., 23.7*3.14159265/180., &
  293, julian, xtime, gmt, declin, solcon, pin, monthly, heating, sw, lw, sw_flux, lw_flux, active, radii, cloud, ozone, .true., .true.)
 surface = [sw(2), lw(1)]
 end subroutine

 ! A late-October column over Attica: 45 layers stretching from 60 m, surface 1013 hPa; T and RH by case.
 subroutine column(case_id)
 integer, intent(in) :: case_id
 real :: t, es, qsat, pint, rh
 integer :: k
 z(1) = 0.
 pint = 101300.
 do k = 1, nz
  dz(k) = 60.*1.08**(k-1)
  z(k+1) = z(k) + dz(k)
 enddo
 qc = 0.; qi = 0.; qs = 0.; qc_bl = 0.; qi_bl = 0.; cldfra_bl = 0.
 re(:,1) = 2.49e-6; re(:,2) = 4.99e-6; re(:,3) = 9.99e-6   ! Thompson's floors where it has no cloud
 do k = 1, nz
  t = 291. - 0.0065*(0.5*(z(k)+z(k+1)))
  if (case_id == 1 .and. k <= 5) t = 284. + 1.2*real(k)        ! night inversion
  t = max(t, 210.)
  p(k) = pint*exp(-9.80616*0.5*dz(k)/(287.04*t))
  pint = pint*exp(-9.80616*dz(k)/(287.04*t))
  exner(k) = (p(k)/1.e5)**(287.04/1004.5)
  theta(k) = t/exner(k)
  es = 611.2*exp(17.67*(t-273.15)/(t-29.65))
  qsat = 0.622*es/(p(k)-es)
  rh = 0.7
  if (k <= 8) rh = 0.93
  qv(k) = rh*qsat
  rho(k) = p(k)/(287.04*t*(1.+0.608*qv(k)))
 enddo
 select case (case_id)
 case (1)   ! fog and stratus in the lowest 5 layers
  qc_bl(1:5) = [2.e-5, 6.e-5, 1.e-4, 1.2e-4, 8.e-5]; cldfra_bl(1:5) = [0.3, 0.5, 0.7, 0.7, 0.5]
 case (2)   ! EDMF shallow cumulus at about 1.2-2 km
  qc_bl(14:18) = 1.5e-4; cldfra_bl(14:18) = 0.35
 case (3)   ! resolved cloud on 3-5, subgrid cloud on 3-8
  qc(3:5) = 2.e-4; qc_bl(3:8) = 8.e-5; cldfra_bl(3:8) = 0.6
 case (4)   ! subgrid water but a fraction at the gate
  qc_bl(3:8) = 8.e-5; cldfra_bl(3:8) = 0.001
 case (5)   ! subgrid ice in a cold layer
  qi_bl(30:32) = 2.e-5; cldfra_bl(30:32) = 0.4
 end select
 end subroutine
end program
