! Synthetic Grell-Freitas soundings from a fixed random seed: columns 1-2048 are varied moist
! soundings, columns 2049-4096 dry, stable controls that must give no convection.
module gf_synthetic
 use config,only:nz
 use model_state,only:column_view
 use gf_state,only:gf_column
 use gf_fixture,only:gf_sample
 implicit none
contains
 subroutine init_gf_synthetic()
 integer::i,n
 call random_seed(size=n)
 if(n/=8)error stop 'random_seed size is not 8: the fixed soundings need gfortran''s generator'
 call random_seed(put=[(12345+37*i+2000,i=1,8)])
 end subroutine

 subroutine make_gf_synthetic(c,a,s)
 integer,intent(in)::c
 type(column_view),intent(in)::a
 type(gf_sample),intent(out)::s
 real::r(16),zi,z,zw,ts,gam,ztrop,pp,tk,es,qs,rh,rhbl,rhft,wamp,dthdz,dqdz,zmoist,ttrop,pint
 integer::k,kz
 real,parameter::rd=287.,cp=1004.5,g=9.80616
 call random_number(r)
 s%xland=merge(1.,2.,r(1)<.6)
 s%ht=merge(1800.*r(2)*r(2),0.,s%xland(1,1)<1.5)
 s%dx=3000.+12000.*r(3)
 if(r(3)<.45)s%dx=3000.+300.*r(3)
 s%area=s%dx**2
 zi=250.+2600.*r(4)*r(4)
 ts=288.+18.*r(5)-.0065*s%ht(1,1)
 gam=.0052+.003*r(6);ztrop=10500.+3500.*r(7)
 rhbl=.50+.49*r(8);rhft=.15+.80*r(9)
 wamp=-.4+2.2*r(10)*r(10)
 if(r(11)<.25)wamp=.02*(r(10)-.5)
 zmoist=1500.+6000.*r(12)
 s%gsw=900.*r(13);s%hfx=-30.+400.*r(13);s%qfx=1.e-4*r(14)
 pint=101325.*exp(-s%ht(1,1)/8400.)*(.985+.03*r(15))
 s%p8w(1,1,1)=pint;z=0.;kz=1;ttrop=0.
 do k=1,nz
  s%dz(1,k,1)=60.*1.08**(k-1)
  zw=z;z=zw+.5*s%dz(1,k,1)
  if(z<zi)then
   tk=ts-.0098*z
  else if(z<ztrop)then
   tk=ts-.0098*zi-gam*(z-zi)+merge(1.5*r(16),0.,z<zi+400.)
  else
   if(ttrop==0.)ttrop=ts-.0098*zi-gam*(ztrop-zi)
   tk=ttrop+.001*(z-ztrop)
  endif
  tk=max(tk,195.)
  if(c>2048)tk=290.
  pp=pint*exp(-g*.5*s%dz(1,k,1)/(rd*tk))
  pint=pint*exp(-g*s%dz(1,k,1)/(rd*tk));s%p8w(1,k+1,1)=pint
  es=611.2*exp(17.67*(tk-273.15)/(tk-29.65));qs=.622*es/max(pp-es,1.)
  if(z<zi)then
   rh=rhbl
  else
   rh=rhft*exp(-(z-zi)/9000.)
   if(abs(z-zmoist)<1200.)rh=max(rh,.55+.44*r(9))
  endif
  if(z>ztrop)rh=.05
  s%t(1,k,1)=tk;s%q(1,k,1)=rh*qs;s%p(1,k,1)=pp;s%pi(1,k,1)=(pp/1.e5)**(rd/cp)
  if(c>2048)s%q(1,k,1)=1.e-9
  s%rho(1,k,1)=pp/(rd*tk*(1.+.608*s%q(1,k,1)))
  s%u(1,k,1)=(2.+14.*r(2))*z/(z+1500.)+3.*r(4)
  s%v(1,k,1)=(r(6)-.5)*20.*z/(z+3000.)
  s%w(1,k,1)=wamp*sin(3.14159*min(zw,12000.)/12000.)*(.6+.8*r(mod(k,16)+1))
  if(c>2048)s%w(1,k,1)=0.
  if(z<zi)kz=k
  z=zw+s%dz(1,k,1)
 enddo
 s%kpbl=kz+1
 do k=1,nz
  if(k>1.and.k<nz)then
   dthdz=(s%t(1,k+1,1)/s%pi(1,k+1,1)-s%t(1,k-1,1)/s%pi(1,k-1,1)) &
    /(s%dz(1,k,1)+.5*(s%dz(1,k-1,1)+s%dz(1,k+1,1)))
   dqdz=(s%q(1,k+1,1)-s%q(1,k-1,1))/(s%dz(1,k,1)+.5*(s%dz(1,k-1,1)+s%dz(1,k+1,1)))
  else
   dthdz=0.;dqdz=0.
  endif
  s%thdyn(1,k,1)=-s%w(1,k,1)*dthdz*(.5+r(mod(k+3,16)+1))
  s%qvdyn(1,k,1)=-s%w(1,k,1)*dqdz*(.5+r(mod(k+7,16)+1))
  s%rad(1,k,1)=-2.e-5+4.e-5*r(13)
  if(k<=s%kpbl(1,1))then
   s%thbl(1,k,1)=(-1.e-4+8.e-4*r(13))*r(14)
   s%qvbl(1,k,1)=(-1.e-7+1.2e-6*r(14))*r(15)
  else
   s%thbl(1,k,1)=0.;s%qvbl(1,k,1)=0.
  endif
 enddo
 if(c>2048)then
  s%rad=0.;s%thbl=0.;s%qvbl=0.;s%hfx=0.;s%qfx=0.
 endif
 s%u(1,nz+1,1)=0.;s%v(1,nz+1,1)=0.;s%w(1,nz+1,1)=0.
 s%t(1,nz+1,1)=s%t(1,nz,1);s%q(1,nz+1,1)=0.;s%p(1,nz+1,1)=pint
 s%pi(1,nz+1,1)=s%pi(1,nz,1);s%rho(1,nz+1,1)=s%rho(1,nz,1);s%dz(1,nz+1,1)=s%dz(1,nz,1)
 s%rad(1,nz+1,1)=0.;s%thbl(1,nz+1,1)=0.;s%qvbl(1,nz+1,1)=0.
 s%thdyn(1,nz+1,1)=0.;s%qvdyn(1,nz+1,1)=0.
 s%hbot=0.;s%htop=0.;s%xmb=0.;s%xmbs=0.;s%rain=0.;s%prate=0.;s%k22=0;s%kbcon=0;s%ktop=0
 s%thcu=0.;s%qvcu=0.;s%qccu=0.;s%qicu=0.;s%gdc=0.;s%gdc2=0.
 a%u=s%u(1,:nz,1);a%v=s%v(1,:nz,1);a%w=s%w(1,:,1)
 a%qv=s%q(1,:nz,1);a%exner=s%pi(1,:nz,1);a%theta=s%t(1,:nz,1)/a%exner
 ! Both interfaces see the same dry-theta-to-temperature conversion.
 s%t(1,:nz,1)=a%theta*a%exner
 a%p_hyd=s%p(1,:nz,1);a%p=a%p_hyd;a%rho_moist=s%rho(1,:nz,1);a%dz=s%dz(1,:nz,1)
 a%dx=s%dx(1,1);a%xland=s%xland(1,1);a%hfx=s%hfx(1,1);a%qfx=s%qfx(1,1);a%kpbl=s%kpbl(1,1)
 a%t_theta=s%thbl(1,:nz,1);a%t_qv=s%qvbl(1,:nz,1)
 end subroutine

 subroutine synthetic_gf_state(s,f)
 type(gf_sample),intent(in)::s
 type(gf_column),intent(in)::f
 f%area=s%area(1,1);f%terrain=s%ht(1,1);f%shortwave=s%gsw(1,1);f%surface_pressure=s%p8w(1,1,1)
 f%hbot=s%hbot(1,1);f%htop=s%htop(1,1);f%xmb_total=s%xmb(1,1);f%xmb_shallow=s%xmbs(1,1)
 f%raincv=s%rain(1,1);f%pratec=s%prate(1,1);f%k22_shallow=s%k22(1,1)
 f%kbcon_shallow=s%kbcon(1,1);f%ktop_shallow=s%ktop(1,1)
 f%dynamics_theta=s%thdyn(1,:nz,1);f%dynamics_qv=s%qvdyn(1,:nz,1);f%radiation_theta=s%rad(1,:nz,1)
 f%rthcuten=s%thcu(1,:nz,1);f%rqvcuten=s%qvcu(1,:nz,1)
 f%rqccuten=s%qccu(1,:nz,1);f%rqicuten=s%qicu(1,:nz,1);f%qc_cu=s%gdc(1,:nz,1);f%qi_cu=s%gdc2(1,:nz,1)
 end subroutine
end module
