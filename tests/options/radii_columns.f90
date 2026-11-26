! The radiation columns with MPAS's own radii (has_re = 0) and with Thompson's (has_re = 1) against MPAS's wrappers
! rrtmg_lwrad and rrtmg_swrad (module_ra_rrtmg_lw.F, _sw.F, compiled as they are) with has_reqc = has_reqi = has_reqs
! equal to that value, on land, water, sea-ice and snow columns:  radii_columns re0|re1
! One value per process, since the scheme set is fixed once per run.
! Both sides get the same declination and solar constant; the tables are read by lean's reader for both.
program radii_columns
 use iso_fortran_env, only: int32
 use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
 use config, only: set_mesh_sizes
 use schemes, only: set_schemes, has_re
 use radiation_tables, only: initialize_radiation_tables
 use radiation_solar, only: radiation_solar_constants
 use radiation_scratch, only: lw_scratch, sw_scratch
 use radiation_sizes, only: max_lw_layers, max_sw_layers
 use radiation_sw, only: radiation_sw_column
 use radiation_lw, only: radiation_lw_column
 use module_ra_rrtmg_lw, only: rrtmg_lwrad
 use module_ra_rrtmg_sw, only: rrtmg_swrad
 use mpas_atmphys_constants, only: degrad
 implicit none
 integer, parameter :: nz = 45, nw = 65
 character(len=16) :: mode
 type(lw_scratch), save :: lws
 type(sw_scratch), save :: sws
 real :: p(nz), pw(nz+1), t(nz), tw(nz+1), pi(nz), dz(nz), q(nz,6), cloud(nz), re(nz,3), pin(59), ozone(59)
 real :: surface(5), lat, lon, gmt, julian, declin, solcon, xtime
 real :: heat(nz), r_heat(nz), lscalar(11), r_lscalar(11), sscalar(14), r_sscalar(14)
 real :: lflux(4,max_lw_layers+1), r_lflux(4,nw+1), sflux(4,max_sw_layers+1), r_sflux(4,nz+2), radii(nz,3), r_radii(nz,3)
 real :: sheat(nz), r_sheat(nz), coszr, gsw, swcf, beam(3), glw, olr, lwcf
 real :: o_heat(nz), o_scalar(11), o_radii(nz,3), o_flux(4,nw+1)
 integer :: o_active
 integer :: k, c, hr, active, r_active, unit, bad_lw, bad_sw, total, differ
 character(len=12) :: names(6)
 real :: land(6), ice(6), snow(6), skin(6)

 call get_command_argument(1, mode)
 hr = merge(1, 0, trim(mode) == 're1')
 if (trim(mode) /= 're0' .and. trim(mode) /= 're1') error stop 'radii_columns: re0 or re1'
 call set_mesh_sizes(2337, 7183, 4847, nz)
 call initialize_radiation_tables('reference/radiation')
 call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'rrtmg_lw', 'rrtmg_sw', &
  'cld_fraction', 'bl_ysu_gwdo', hr == 1)
 if (has_re /= hr) error stop 'radii_columns: has_re does not follow the option'
 open(newunit=unit, file='reference/radiation/OZONE_PLEV.TBL', status='old')
 do k = 1, 59
  read(unit, *) pin(k)
 end do
 close(unit)
 ozone = 3.e-8
 do k = 1, nz+1
  pw(k) = 101000.*exp(-log(101.)*real(k-1)/real(nz))
  tw(k) = 290. - 70.*real(k-1)/real(nz)
 end do
 do k = 1, nz
  p(k) = (pw(k)+pw(k+1))*.5; t(k) = (tw(k)+tw(k+1))*.5
 end do
 pi = (p/100000.)**(287./1004.5); dz = 500.
 re(:,1) = 10.e-6; re(:,2) = 25.e-6; re(:,3) = 100.e-6
 ! the layers of cloud water, ice and snow, and a column of ice and snow that reaches the warm layers too
 q = 0.; q(:,1) = .004; cloud = 0.
 q(8:15,2) = 1.e-4; q(20:25,3) = 1.e-5; q(18:25,5) = 1.e-5; cloud(8:25) = .6
 q(30:34,3) = 2.e-5; cloud(30:34) = .8
 ! land, water, sea ice (xice, xland 1), water with a little sea ice, land with snow, land with deep snow
 names = [character(len=12) :: 'land', 'water', 'sea ice', 'thin ice', 'snow', 'deep snow']
 land = [1., 2., 1., 2., 1., 1.]; ice = [0., 0., .8, .15, 0., 0.]; snow = [0., 0., 0., 0., 15., 120.]
 skin = [291., 288., 262., 271., 270., 258.]
 julian = 283.5; gmt = 12.
 call radiation_solar_constants(julian, declin, solcon)
 xtime = 0.
 lat = 38.; lon = 24.
 bad_lw = 0; bad_sw = 0; total = 0; differ = 0
 do c = 1, 6
  surface = [skin(c), .97, land(c), ice(c), snow(c)]
  ! MPAS's wrapper with has_req = hr
  call original_lw(p, pw, pi, t, tw, dz, q, cloud, [surface, lat, lon], pin, ozone, re, 284, hr, r_heat, r_lscalar, r_radii, &
   r_lflux, r_active)
  call original_sw(p, pw, pi, t, tw, dz, q, cloud, [surface(1), .18, surface(3:5), lat, lon], pin, ozone, re, 284, xtime, gmt, &
   declin, solcon, hr, r_sheat, r_sscalar, r_sflux)
  ! and with the other value, to show the two differ (the option acts on this column)
  call original_lw(p, pw, pi, t, tw, dz, q, cloud, [surface, lat, lon], pin, ozone, re, 284, 1-hr, o_heat, o_scalar, &
   o_radii, o_flux, o_active)
  if (any(o_heat /= r_heat)) differ = differ + 1
  call radiation_lw_column(p, pw, pi, t, tw, dz, q(:,1), q(:,2), q(:,3), q(:,5), cloud, surface(1), surface(2), surface(3), &
   surface(4), surface(5), lat, 284, pin, ozone, re(:,1), re(:,2), re(:,3), glw, olr, lwcf, heat, lflux, active, radii, ws=lws)
  lscalar = [glw, olr, lwcf, lflux(1,active+1), lflux(3,active+1), lflux(2,active+1), lflux(4,active+1), &
   lflux(1,1), lflux(3,1), lflux(2,1), lflux(4,1)]
  call radiation_sw_column(p, pw, pi, t, tw, dz, q(:,1), q(:,2), q(:,3), q(:,5), cloud, surface(1), .18, surface(3), &
   surface(4), surface(5), lat, lon, 284, pin, ozone, re(:,1), re(:,2), re(:,3), xtime, gmt, declin, solcon, &
   coszr, gsw, swcf, sheat, sflux, beam, ws=sws)
  sscalar = [coszr, gsw, swcf, sflux(1,nz+2), sflux(3,nz+2), sflux(2,nz+2), sflux(4,nz+2), &
   sflux(1,1), sflux(3,1), sflux(2,1), sflux(4,1), beam]
  if (active /= r_active) error stop 'radii_columns: LW layer count'
  if (.not. (all(ieee_is_finite(heat)) .and. all(ieee_is_finite(sheat)))) error stop 'radii_columns: nonfinite'
  if (coszr <= 0.) error stop 'radii_columns: the column is not sunlit'
  k = bits(heat, r_heat) + bits(lscalar, r_lscalar) + bits(reshape(lflux(:,1:active+1), [4*(active+1)]), &
      reshape(r_lflux(:,1:active+1), [4*(active+1)])) + bits(reshape(radii, [3*nz]), reshape(r_radii, [3*nz]))
  bad_lw = bad_lw + k
  if (k /= 0) print '(a,i0,a)', 'FAIL LW '//trim(names(c))//': ', k, ' differing words'
  k = bits(sheat, r_sheat) + bits(sscalar, r_sscalar) + bits(reshape(sflux(:,1:nz+2), [4*(nz+2)]), &
      reshape(r_sflux, [4*(nz+2)]))
  bad_sw = bad_sw + k
  if (k /= 0) print '(a,i0,a)', 'FAIL SW '//trim(names(c))//': ', k, ' differing words'
  total = total + 1
 end do
 if (bad_lw + bad_sw /= 0) error stop 'radii_columns: lean differs from MPAS'
 if (differ < 4) error stop 'radii_columns: the other value of has_re gives the same heating (the test cannot tell)'
 print '(a)', 'ok   radii_columns '//trim(mode)//': 6 columns, LW and SW bit for bit with MPAS (has_re '// &
  merge('1', '0', hr == 1)//'); the other value differs on '//char(48+differ)//' of 6'

contains

 integer function bits(a, b)
 real, intent(in) :: a(:), b(:)
 bits = count(transfer(a, [0_int32], size(a)) /= transfer(b, [0_int32], size(b)))
 end function

 subroutine original_lw(p, pw, exner, t, tw, dz, q, cloud, surface, pin, ozone, re, julday, hr, heat, scalar, radii, flux, nlayers)
 real, intent(in) :: p(nz), pw(nz+1), exner(nz), t(nz), tw(nz+1), dz(nz), q(nz,6), cloud(nz)
 real, intent(in) :: surface(7), pin(59), ozone(59), re(nz,3)
 integer, intent(in) :: julday, hr
 real, intent(out) :: heat(nz), scalar(*), radii(nz,3), flux(4,*)
 integer, intent(out) :: nlayers
 real :: p3(1,nw,1), pw3(1,nw,1), pi3(1,nw,1), t3(1,nw,1), tw3(1,nw,1), dz3(1,nw,1)
 real :: qv3(1,nw,1), qc3(1,nw,1), qi3(1,nw,1), qs3(1,nw,1), cl3(1,nw,1)
 real :: rc(1,nw,1), ri(1,nw,1), rs(1,nw,1), rrc(1,nw,1), rri(1,nw,1), rrs(1,nw,1), h3(1,nw,1)
 real :: glw(1,1), olr(1,1), cf(1,1), up(1,1), upc(1,1), dn(1,1), dnc(1,1)
 real :: ub(1,1), ubc(1,1), db(1,1), dbc(1,1), o3(1,59,1)
 real :: all_up(1,nw+2,1), all_dn(1,nw+2,1), clear_up(1,nw+2,1), clear_dn(1,nw+2,1)
 integer :: k
 p3=0.;pw3=0.;pi3=0.;t3=0.;tw3=0.;dz3=0.
 qv3=0.;qc3=0.;qi3=0.;qs3=0.;cl3=0.;rc=0.;ri=0.;rs=0.;rrc=0.;rri=0.;rrs=0.;h3=0.
 all_up=0.;all_dn=0.;clear_up=0.;clear_dn=0.
 p3(1,1:nz,1)=p;pw3(1,1:nz+1,1)=pw;pi3(1,1:nz,1)=exner
 t3(1,1:nz,1)=t;tw3(1,1:nz+1,1)=tw;dz3(1,1:nz,1)=dz
 qv3(1,1:nz,1)=q(:,1);qc3(1,1:nz,1)=q(:,2);qi3(1,1:nz,1)=q(:,3);qs3(1,1:nz,1)=q(:,5)
 cl3(1,1:nz,1)=cloud;rc(1,1:nz,1)=re(:,1);ri(1,1:nz,1)=re(:,2);rs(1,1:nz,1)=re(:,3)
 o3(1,:,1)=ozone
 nlayers=nz+max(nint(pw(nz+1)/100./4.),1)
 call rrtmg_lwrad(p3d=p3,p8w=pw3,pi3d=pi3,t3d=t3,t8w=tw3,dz8w=dz3, &
  qv3d=qv3,qc3d=qc3,qi3d=qi3,qs3d=qs3,cldfra3d=cl3, &
  tsk=reshape(surface(1:1),[1,1]),emiss=reshape(surface(2:2),[1,1]), &
  xland=reshape(surface(3:3),[1,1]),xice=reshape(surface(4:4),[1,1]), &
  snow=reshape(surface(5:5),[1,1]),xlat=reshape(surface(6:6),[1,1]), &
  julday=julday,icloud=1,cldovrlp=2,idcor=0,o3input=2,noznlevels=59,pin=pin,o3clim=o3, &
  glw=glw,olr=olr,lwcf=cf,rthratenlw=h3,has_reqc=hr,has_reqi=hr,has_reqs=hr, &
  re_cloud=rc,re_ice=ri,re_snow=rs,rre_cloud=rrc,rre_ice=rri,rre_snow=rrs, &
  lwupt=up,lwuptc=upc,lwdnt=dn,lwdntc=dnc,lwupb=ub,lwupbc=ubc,lwdnb=db,lwdnbc=dbc, &
  lwupflx=all_up,lwupflxc=clear_up,lwdnflx=all_dn,lwdnflxc=clear_dn, &
  ids=1,ide=1,jds=1,jde=1,kds=1,kde=nz+1,ims=1,ime=1,jms=1,jme=1,kms=1,kme=nw, &
  its=1,ite=1,jts=1,jte=1,kts=1,kte=nz)
 heat=h3(1,1:nz,1)
 scalar(1:11)=[glw(1,1),olr(1,1),cf(1,1),up(1,1),upc(1,1),dn(1,1),dnc(1,1),ub(1,1),ubc(1,1),db(1,1),dbc(1,1)]
 radii(:,1)=rrc(1,1:nz,1);radii(:,2)=rri(1,1:nz,1);radii(:,3)=rrs(1,1:nz,1)
 do k=1,nlayers+1
  flux(:,k)=[all_up(1,k,1),all_dn(1,k,1),clear_up(1,k,1),clear_dn(1,k,1)]
 enddo
 end subroutine

 subroutine original_sw(p, pw, exner, t, tw, dz, q, cloud, surface, pin, ozone, re, julday, xtime, gmt, declin, solcon, hr, &
  heat, scalar, flux)
 real, intent(in) :: p(nz), pw(nz+1), exner(nz), t(nz), tw(nz+1), dz(nz), q(nz,6), cloud(nz)
 real, intent(in) :: surface(7), pin(59), ozone(59), re(nz,3), xtime, gmt, declin, solcon
 integer, intent(in) :: julday, hr
 real, intent(out) :: heat(nz), scalar(14), flux(4,nz+2)
 real :: p3(1,nz+1,1), pw3(1,nz+1,1), pi3(1,nz+1,1), t3(1,nz+1,1), tw3(1,nz+1,1), dz3(1,nz+1,1)
 real :: qv3(1,nz+1,1), qc3(1,nz+1,1), qi3(1,nz+1,1), qs3(1,nz+1,1), cl3(1,nz+1,1)
 real :: rc(1,nz+1,1), ri(1,nz+1,1), rs(1,nz+1,1), h3(1,nz+1,1), o3(1,59,1)
 real :: coszr(1,1), gsw(1,1), cf(1,1), up(1,1), upc(1,1), dn(1,1), dnc(1,1)
 real :: ub(1,1), ubc(1,1), db(1,1), dbc(1,1), dir(1,1), dni(1,1), dif(1,1)
 real :: all_up(1,nz+3,1), all_dn(1,nz+3,1), clear_up(1,nz+3,1), clear_dn(1,nz+3,1)
 integer :: k, nwl
 nwl = nz + 1
 p3=0.;pw3=0.;pi3=0.;t3=0.;tw3=0.;dz3=0.
 qv3=0.;qc3=0.;qi3=0.;qs3=0.;cl3=0.;rc=0.;ri=0.;rs=0.;h3=0.
 all_up=0.;all_dn=0.;clear_up=0.;clear_dn=0.
 gsw=0.;cf=0.;dir=0.;dni=0.;dif=0.
 p3(1,1:nz,1)=p;pw3(1,1:nz+1,1)=pw;pi3(1,1:nz,1)=exner
 t3(1,1:nz,1)=t;tw3(1,1:nz+1,1)=tw;dz3(1,1:nz,1)=dz
 qv3(1,1:nz,1)=q(:,1);qc3(1,1:nz,1)=q(:,2);qi3(1,1:nz,1)=q(:,3);qs3(1,1:nz,1)=q(:,5)
 cl3(1,1:nz,1)=cloud;rc(1,1:nz,1)=re(:,1);ri(1,1:nz,1)=re(:,2);rs(1,1:nz,1)=re(:,3)
 o3(1,:,1)=ozone
 call rrtmg_swrad(p3d=p3,p8w=pw3,pi3d=pi3,t3d=t3,t8w=tw3,dz8w=dz3, &
  qv3d=qv3,qc3d=qc3,qi3d=qi3,qs3d=qs3,cldfra3d=cl3, &
  tsk=reshape(surface(1:1),[1,1]),albedo=reshape(surface(2:2),[1,1]), &
  xland=reshape(surface(3:3),[1,1]),xice=reshape(surface(4:4),[1,1]), &
  snow=reshape(surface(5:5),[1,1]),xlat=reshape(surface(6:6),[1,1]),xlong=reshape(surface(7:7),[1,1]), &
  xtime=xtime,gmt=gmt,declin=declin,solcon=solcon,radt=30.,degrad=degrad,coszr=coszr, &
  julday=julday,icloud=1,cldovrlp=2,idcor=0,o3input=2,noznlevels=59,pin=pin,o3clim=o3, &
  gsw=gsw,swcf=cf,rthratensw=h3,has_reqc=hr,has_reqi=hr,has_reqs=hr,re_cloud=rc,re_ice=ri,re_snow=rs,aer_opt=0, &
  swupt=up,swuptc=upc,swdnt=dn,swdntc=dnc,swupb=ub,swupbc=ubc,swdnb=db,swdnbc=dbc, &
  swupflx=all_up,swupflxc=clear_up,swdnflx=all_dn,swdnflxc=clear_dn,swddir=dir,swddni=dni,swddif=dif, &
  ids=1,ide=1,jds=1,jde=1,kds=1,kde=nwl,ims=1,ime=1,jms=1,jme=1,kms=1,kme=nwl, &
  its=1,ite=1,jts=1,jte=1,kts=1,kte=nz)
 heat=h3(1,1:nz,1)
 scalar=[coszr(1,1),gsw(1,1),cf(1,1),up(1,1),upc(1,1),dn(1,1),dnc(1,1), &
  ub(1,1),ubc(1,1),db(1,1),dbc(1,1),dir(1,1),dni(1,1),dif(1,1)]
 do k=1,nz+2
  flux(:,k)=[all_up(1,k,1),all_dn(1,k,1),clear_up(1,k,1),clear_dn(1,k,1)]
 enddo
 end subroutine
end program
