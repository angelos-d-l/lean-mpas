! LEAN_NOAHMP_SWSPLIT: the RRTMG surface split and the Noah-MP forcing, single precision. sw_split.sh adds a test-only
! probe of the four raw RRTMG surface components to a copy of src/radiation_sw.f90; the model's interfaces stay unchanged.
module sw_split_probe
 implicit none
 real :: parts(4), down
end module
subroutine sw_split_surface_probe(vdir,ndir,vdif,ndif,total)
 use sw_split_probe
 implicit none
 real, intent(in) :: vdir,ndir,vdif,ndif,total
 parts=[vdir,ndir,vdif,ndif]; down=total
end subroutine
program sw_split
 use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
 use config, only: nz
 use radiation_tables, only: initialize_radiation_tables
 use radiation_climatology, only: initialize_ozone_tables, monthly_ozone
 use radiation_solar, only: radiation_solar_constants
 use radiation_column, only: radiation_event
 use radiation_sizes, only: max_sw_layers, max_lw_layers
 use noahmp_state, only: noahmp_state_type, noahmp_column, noahmp_view, allocate_land
 use phys_state, only: zero_land
 use lean_atmosforcingmod, only: ProcessAtmosForcing
 use sw_split_probe
 implicit none
 type(noahmp_state_type), target, save :: lands
 type(noahmp_column) :: land
 character(1024) :: directory
 real, allocatable :: theta(:), exner(:), p(:), rho(:), dz(:), z(:), qv(:), qc(:), qi(:), qs(:), re(:,:)
 real :: pin(59), monthly(12,59), julian, declin, solcon, xtime, gmt
 real :: sw(17), lw(11), sw_flux(4,max_sw_layers+1), lw_flux(4,max_lw_layers+1)
 real :: ozone(59), frac(4), bands(4), expected(4), total
 real, allocatable :: heating(:,:), radii(:,:), cloud(:)
 real, parameter :: flux_tol=5.e-4, frac_tol=8.*epsilon(1.)
 integer :: case_id, k, fails, active
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
 allocate(re(nz,3))
 allocate(heating(nz,2))
 allocate(radii(nz,3))
 allocate(cloud(nz))
 call get_command_argument(1,directory)
 call initialize_radiation_tables(trim(directory))
 call initialize_ozone_tables(trim(directory),pin)
 call monthly_ozone(38.*acos(-1.)/180.,monthly)
 call allocate_land(lands); call zero_land(lands)   ! allocate the members, then zero them
 land=noahmp_view(lands,1)
 fails=0
 do case_id=1,3
  call column()
  ! Clear and optically thick overcast at noon; then the same column at midnight.
  if(case_id==2) then
   qc(8:18)=2.e-3
   do k=8,18
    qv(k)=0.622*611.2*exp(17.67*(theta(k)*exner(k)-273.15)/(theta(k)*exner(k)-29.65))/p(k)
   enddo
   re(8:18,1)=10.e-6
  endif
  gmt=0.; xtime=600.
  if(case_id==3) xtime=0.
  julian=293.+xtime/1440.
  call radiation_solar_constants(julian,declin,solcon)
  call radiation_event(theta,exner,p,rho,dz,z,qv,qc,qi,qs,re(:,1),re(:,2),re(:,3), &
   290.,0.95,1.,0.,0.,0.18,38.*acos(-1.)/180.,23.7*acos(-1.)/180., &
   293,julian,xtime,gmt,declin,solcon,pin,monthly,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,.true.,.true.)
  frac=[sw(15:17),1.-sw(15)-sw(16)-sw(17)]
  if(.not.all(ieee_is_finite(sw))) call fail('nonfinite radiation output')
  if(case_id<=2) then
   if(any(frac<0.).or.any(frac>1.)) call fail('fractions outside [0,1]')
   if(abs(sum(frac)-1.)>frac_tol) call fail('fractions do not sum to one')
   if(abs(sum(parts)-sw_flux(2,1))>flux_tol) call fail('raw four-band sum differs from surface down flux')
   if(down/=sw_flux(2,1)) call fail('probe and radiation_event surface differ')
   if(any(abs(frac-parts/down)>frac_tol)) call fail('fractions differ from raw surface components')
   if(case_id==1.and.frac(1)+frac(2)<=0.6) call fail('clear column not direct dominated')
   if(case_id==2.and.frac(3)+frac(4)<=0.8) call fail('overcast column not diffuse dominated')
   print '(a,i0,a,4f11.7,a,es12.4)', 'case ',case_id,' fractions (dir vis/NIR, dif vis/NIR): ',frac, &
    ' raw sum error W/m2: ',sum(parts)-sw_flux(2,1)
  else
   if(any(sw(15:17)/=0.).or.sw_flux(2,1)/=0.) call fail('night radiation not zero')
   print '(a,3f8.3)', 'night stored fractions: ',sw(15:17)
  endif
  call forcing(sw(1),sw(15:17),500.)
  if(case_id<=2) then
   total=500.
   ! RRTMG's direct fraction, visible kept at 50 % (src/noahmp_core.f90, the LEAN_NOAHMP_SWSPLIT block of AtmosForcing,
   ! says why)
   expected(1:2)=total*(frac(1)+frac(2))*0.5
   expected(3:4)=total*(1.-frac(1)-frac(2))*0.5
   if(any(abs(bands-expected)>1.e-3)) call fail('Noah-MP band does not equal its fraction')
   if(abs(sum(bands)-total)>flux_tol) call fail('Noah-MP bands do not conserve total')
   if(land%forcing_RadSwDownRefHeight/=total) call fail('daytime reference forcing changed')
  else
   if(any(bands/=0.)) call fail('night Noah-MP bands not zero')
  endif
 enddo
 ! Daylight with zero total RRTMG: keep the original arithmetic exactly.
 call forcing(0.5,[0.,0.,0.],500.)
 expected=[500.*0.7*0.5,500.*0.7*(1.-0.5),500.*(1.-0.7)*0.5,500.*(1.-0.7)*(1.-0.5)]
 if(any(bands/=expected)) call fail('zero-fraction guard does not preserve original split')
 if(land%forcing_RadSwDownRefHeight/=500.) call fail('fallback reference forcing changed')
 ! Night must also be safe with a held, nonzero daytime split.
 call forcing(-0.1,[0.4,0.3,0.2],500.)
 if(any(bands/=0.)) call fail('night with held fractions not zero')
 if(fails>0) then
  print '(a,i0,a)', 'RESULT FAIL (',fails,' checks)'
  stop 1
 endif
 print '(a,es10.2,a,es10.2)', 'RESULT PASS; flux tolerance W/m2=',flux_tol,' fraction tolerance=',frac_tol
contains
 subroutine fail(message)
 character(*),intent(in) :: message
 print '(2a)', 'FAIL: ',message
 fails=fails+1
 end subroutine
 subroutine forcing(cosz,f,total)
 real,intent(in) :: cosz,f(3),total
 land%config_domain_CosSolarZenithAngle=cosz
 land%forcing_PressureAirRefHeight=101300.
 land%forcing_TemperatureAirRefHeight=290.
 land%forcing_SpecHumidityRefHeight=0.008
 land%forcing_PrecipConvRefHeight=0.; land%forcing_PrecipNonConvRefHeight=0.
 land%forcing_PrecipShConvRefHeight=0.; land%forcing_PrecipSnowRefHeight=0.
 land%forcing_PrecipGraupelRefHeight=0.; land%forcing_PrecipHailRefHeight=0.
 land%forcing_WindEastwardRefHeight=2.; land%forcing_WindNorthwardRefHeight=1.
 land%water_param_SnowfallDensityMax=400.
 land%forcing_RadSwDownRefHeight=total
 land%sw_frac_dir_vis=f(1); land%sw_frac_dir_nir=f(2); land%sw_frac_dif_vis=f(3)
 call ProcessAtmosForcing(land)
 bands=[land%energy_flux_RadSwDownDir,land%energy_flux_RadSwDownDif]
 if(.not.all(ieee_is_finite(bands))) call fail('nonfinite Noah-MP bands')
 end subroutine
 subroutine column()
 real :: t,es,pint
 z(1)=0.; pint=101300.
 qc=0.; qi=0.; qs=0.
 re(:,1)=2.49e-6; re(:,2)=4.99e-6; re(:,3)=9.99e-6
 do k=1,nz
  dz(k)=60.*1.08**(k-1)
  z(k+1)=z(k)+dz(k)
  t=max(291.-0.0065*0.5*(z(k)+z(k+1)),210.)
  p(k)=pint*exp(-9.80616*0.5*dz(k)/(287.04*t))
  pint=pint*exp(-9.80616*dz(k)/(287.04*t))
  exner(k)=(p(k)/1.e5)**(287.04/1004.5); theta(k)=t/exner(k)
  es=611.2*exp(17.67*(t-273.15)/(t-29.65))
  qv(k)=0.7*0.622*es/(p(k)-es)
  rho(k)=p(k)/(287.04*t*(1.+0.608*qv(k)))
 enddo
 end subroutine
end program
