! Single-column harness for the surface-moisture path: MYNN surface layer, MYNN PBL with its MPAS pre/post conversions
! and the 2 m diagnostics, lean (src/ or src/device/ sources, compiled for the CPU) against MPAS 8.4.2 (the physics_wrf
! wrappers sfclay_mynn and mynn_bl_driver, which call sf_mynn_pre/sf_mynn_run and bl_mynn_pre/bl_mynn_run/bl_mynn_post,
! and the sf_noahmp loop of mpas_atmphys_sfc_diagnostics.F). Run by tests/mynn_moisture/run.sh.
!
! Each code advances its own copy of one column for three days per regime with its own tendencies; the forcing (skin
! temperature and humidity of land, the sea temperature, a slow relaxation to the initial profiles that stands in for the
! large scale) is the same code for both. Land fluxes are MYNN's own, from a prescribed skin (no Noah-MP here). Every step
! every output of both codes is compared word for word. The lean side also writes a trace with the water budget of its
! PBL step: F = dt*qfx, W = vapour the tendencies add to the column at fixed dry density, Es = MYNN's own budget of
! specific humidity at its fixed moist density, C and Cs the same for the condensate.
!
!   column [dt=SECONDS] [mutate|perturb] [trace_lean] [trace_mpas]
! dt=SECONDS: the time step (default 30 s).
! mutate: the lean side starts each regime with theta(1) one ulp higher (the harness must see it).
! perturb: the lean side's skin is 0.01 K warmer every step, a control of the size of the switches' effects.
program mynn_moisture_column
 use config,only: dt
 use run_settings,only: set_time_step
 use model_state,only: state_type,column_view,view_column
 use mynn,only: mynn_init,mynn_surface_column,mynn_pbl_column
 use mynn_conversions,only: pbl_mixing_tendencies
 use sfc_diag_lean,only: noahmp_column,surface_diagnostics
 use module_sf_mynn,only: sfclay_mynn
 use module_bl_mynn,only: mynn_bl_driver
 use sf_mynn,only: sf_mynn_init
 use bl_mynn,only: bl_mynn_init
 use sfc_diag_mpas,only: mpas_sfc_diagnostics
 use model_state, only: allocate_state
 use phys_state, only: zero_state
 implicit none
 ! This synthetic MPAS driver fixture has 45 levels.
 integer,parameter :: nz=45
 integer,parameter :: nregimes=8,nrec=29,nfields=72
#ifdef LEAN_MYNN_MOIST
 logical,parameter :: lean_moist=.true.
#else
 logical,parameter :: lean_moist=.false.
#endif
 real,parameter :: cp=1004.5,grav=9.80616,r_d=287.,r_v=461.6,days=3.
 character(12),parameter :: regime_name(nregimes)=[character(12) :: 'day','calm_night','dew','windy','water_warm', &
  'water_cool','snow','stratus']
 type(state_type),target,save :: st
 type(column_view) :: a
 type(noahmp_column) :: land
 ! the column and its forcing
 real :: dz(nz),p(nz),exner(nz),theta0(nz),qv0(nz),qc0(nz),qi0(nz),qs0(nz),u0(nz),v0(nz),psfc
 real :: xland,start_hour,beta,tsk_mean,tsk_amp,znt_land,snowh,dx,sst
 ! the MPAS copy of the column (driver arrays, ims:ime=1, kms:kme=1:nz+1, jms:jme=1)
 real,dimension(1,nz+1,1) :: m_u,m_v,m_w,m_th,m_t,m_p,m_pi,m_rho,m_dz,m_qv,m_qc,m_qi,m_qs,m_nc,m_ni,m_nifa,m_nwfa, &
  m_rthraten,m_pattern,m_cldfra_bl,m_qc_bl,m_qi_bl,m_el_pbl,m_qke,m_qke_adv,m_cov,m_qsq,m_tsq,m_sh3d,m_sm3d, &
  m_rublten,m_rvblten,m_rthblten,m_rqvblten,m_rqcblten,m_rqiblten,m_rqsblten,m_rncblten,m_rniblten,m_rnifablten, &
  m_rnwfablten,m_edmf_a,m_edmf_w,m_edmf_qt,m_edmf_thl,m_edmf_ent,m_edmf_qc,m_sub_thl,m_sub_sqv,m_det_thl,m_det_sqv, &
  m_exch_h,m_exch_m,m_dqke,m_qwt,m_qshear,m_qbuoy,m_qdiss
 real,dimension(1,1) :: m_psfc,m_chs,m_chs2,m_cqs2,m_cpm,m_znt,m_ust,m_pblh,m_mavail,m_zol,m_mol,m_regime,m_psim, &
  m_psih,m_xland,m_hfx,m_qfx,m_lh,m_tsk,m_flhc,m_flqc,m_qgh,m_qsfc,m_rmol,m_u10,m_v10,m_th2,m_t2,m_q2,m_snowh, &
  m_gz1oz0,m_wspd,m_br,m_dx,m_ustm,m_ck,m_cka,m_cd,m_cda,m_ch,m_qcg,m_uoce,m_voce,m_maxwidth,m_maxmf,m_ztop_plume, &
  m_xice
 integer,dimension(1,1) :: m_kpbl,m_ktop_plume
 ! comparison counters per field
 character(12) :: field_name(nfields)
 integer(8) :: compared(nfields),differing(nfields),total_compared,total_differing,regime_compared
 integer :: first_step(nfields),nf
 real(8) :: rec(nrec)
 character(512) :: arg,errmsg,trace_lean,trace_mpas   ! errmsg: StrKIND of the MPAS wrappers
 logical :: mutate,perturb
 integer :: errflg,regime,step,nsteps,k,ul,um,i,initflag
 real :: hour,q2_surface_layer,t2_surface_layer,m_q2_surface_layer,m_t2_surface_layer,seconds

 call allocate_state(st); call zero_state(st)
 mutate=.false.;perturb=.false.;trace_lean='trace_lean.bin';trace_mpas='trace_mpas.bin'
 do i=1,command_argument_count()
  call get_command_argument(i,arg)
  if(arg(1:3)=='dt=') then
   read(arg(4:),*) seconds
   call set_time_step(seconds)
  elseif(arg=='mutate') then
   mutate=.true.
  elseif(arg=='perturb') then
   perturb=.true.
  elseif(index(arg,'trace_lean')>0) then
   trace_lean=arg
  elseif(index(arg,'trace_mpas')>0) then
   trace_mpas=arg
  endif
 enddo
 call conversion_unit_test()
 call mynn_init()
 call sf_mynn_init(errmsg,errflg)
 if(errflg/=0) error stop 'MPAS sf_mynn_init'
 call bl_mynn_init(cp,4.*r_v,2106.,4190.,r_v/r_d-1.,r_d/r_v,grav,0.4,1.e5,r_d,r_v,0.6112,17.67,29.65,273.15,3.5e5, &
  2.85e6,2.5e6,errmsg,errflg)
 if(errflg/=0) error stop 'MPAS bl_mynn_init'
 open(newunit=ul,file=trim(trace_lean),form='unformatted',access='stream',status='replace')
 open(newunit=um,file=trim(trace_mpas),form='unformatted',access='stream',status='replace')
 nsteps=nint(days*86400./dt)
 compared=0;differing=0;first_step=0;total_compared=0;total_differing=0
 print '(a,f5.1,a,i0,a)','dt ',dt,' s, ',nsteps,' steps per regime'
 do regime=1,nregimes
  call set_regime(regime)
  a=view_column(st,1)
  call start_lean()
  call start_mpas()
  if(mutate) a%theta(1)=nearest(a%theta(1),1.)
  if(perturb) a%tsk=a%tsk+0.01   ! water; land gets it every step below
  do step=1,nsteps
   hour=start_hour+(step-1)*dt/3600.
   initflag=merge(1,0,step==1)
   ! the land skin (both codes): the prescribed temperature, and a humidity that evaporates with efficiency beta and
   ! takes dew once the skin is below the dew point of the first layer
   if(xland<1.5) then
    a%tsk=skin_temperature(hour)+merge(0.01,0.,perturb);m_tsk=skin_temperature(hour)
    a%qsfc=skin_humidity(a%tsk,a%qv(1));m_qsfc=skin_humidity(m_tsk(1,1),m_qv(1,1,1))
    a%znt=znt_land;m_znt=znt_land
   endif
   ! lean: mynn_surface_column, mynn_pbl_column, surface_diagnostics (the order of coupled_land_column and pre_dynamics)
   call mynn_surface_column(a,step)
   q2_surface_layer=a%q2;t2_surface_layer=a%t2
   call mynn_pbl_column(a,step)
   call land_two_metre(a%q2,a%t2,a%qsfc)
   call surface_diagnostics(a,land)
   ! MPAS: the same, through its drivers
   call mpas_surface_layer(initflag)
   m_q2_surface_layer=m_q2(1,1);m_t2_surface_layer=m_t2(1,1)
   call mpas_pbl(initflag)
   call mpas_diagnostics()
   call compare_all(step)
   call budget()
   call advance_lean()
   call advance_mpas()
  enddo
  call report(regime)
 enddo
 close(ul);close(um)
 print '(a,i0,a,i0,a)','all regimes: ',total_compared,' words compared, ',total_differing,' differ'
 if(total_differing>0) stop 1

contains

 ! --- the regimes ------------------------------------------------------------------------------------------------------
 subroutine set_regime(r)
 integer,intent(in) :: r
 real :: zi,th_sfc,rh,wind,z,ztop,es,qsat,tk
 xland=1.;start_hour=6.;beta=0.4;tsk_amp=10.;znt_land=0.1;snowh=0.;dx=3000.;wind=5.;zi=300.;th_sfc=288.;rh=0.6
 qc0=0.;qi0=0.;qs0=0.;psfc=100000.;tsk_mean=2.;sst=0.
 select case(r)
 case(1)   ! day: land, clear, convective
 case(2)   ! calm night: land, light wind, dry
  start_hour=18.;wind=1.5;zi=150.;th_sfc=290.;rh=0.5;beta=0.3;tsk_amp=9.;tsk_mean=0.;znt_land=0.05
 case(3)   ! dew: land, wet skin, moist air, clear night
  start_hour=18.;wind=2.;zi=150.;th_sfc=287.;rh=0.85;beta=1.;tsk_amp=8.;tsk_mean=-1.
 case(4)   ! windy: land, strong wind, coarse cell (subgrid wind), a cloud layer
  wind=15.;zi=1000.;th_sfc=285.;rh=0.7;beta=0.5;tsk_amp=3.;tsk_mean=0.5;znt_land=0.5;dx=12000.
 case(5)   ! warm sea: water 4 K warmer than the air
  xland=2.;wind=7.;zi=600.;th_sfc=288.;rh=0.7;sst=4.
 case(6)   ! cool sea: water 3 K cooler than moist air, fog/stratus at the bottom
  xland=2.;wind=5.;zi=200.;th_sfc=292.;rh=0.95;sst=-3.
 case(7)   ! snow: land under snow, cold, snowing cloud (ice and snow)
  start_hour=0.;wind=4.;zi=400.;th_sfc=270.;rh=0.9;beta=1.;tsk_amp=4.;tsk_mean=-2.;znt_land=0.002;snowh=0.3
 case(8)   ! stratus: sea slightly warmer than the air, a liquid cloud in the mixed layer
  xland=2.;wind=6.;zi=900.;th_sfc=289.;rh=0.95;sst=0.5
 end select
 ! levels: 55 m at the ground, stretched to about 30 km
 do k=1,nz
  dz(k)=min(55.*1.09**(k-1),1500.)
 enddo
 ztop=0.
 p(1)=psfc
 do k=1,nz
  z=ztop+0.5*dz(k)
  if(z<zi) then
   theta0(k)=th_sfc
  else
   theta0(k)=th_sfc+0.004*(z-zi)+merge(0.012*(z-12000.),0.,z>12000.)
  endif
  if(r==4.and.z>800..and.z<1200.) qc0(k)=2.e-4
  if(r==6.and.z<300.) qc0(k)=3.e-4
  if(r==7.and.z<2000.) then
   qi0(k)=5.e-5;qs0(k)=2.e-4
  endif
  if(r==8.and.z>400..and.z<900.) qc0(k)=4.e-4
  ! hydrostatic pressure at the layer centres (dry, the moist correction is irrelevant here)
  if(k==1) then
   p(k)=psfc-grav*0.5*dz(k)*psfc/(r_d*theta0(k))
  else
   p(k)=p(k-1)-grav*0.5*(dz(k)+dz(k-1))*p(k-1)/(r_d*theta0(k-1)*(p(k-1)/1.e5)**(r_d/cp))
  endif
  exner(k)=(p(k)/1.e5)**(r_d/cp)
  tk=theta0(k)*exner(k)
  es=611.2*exp(17.67*(tk-273.15)/(tk-29.65))
  qsat=0.622*es/(p(k)-es)
  qv0(k)=qsat*merge(rh,rh*exp(-(z-zi)/3000.),z<zi)
  u0(k)=wind*cos(0.3);v0(k)=wind*sin(0.3)
  ztop=ztop+dz(k)
 enddo
 tsk_mean=theta0(1)*exner(1)+tsk_mean
 sst=theta0(1)*exner(1)+sst
 end subroutine

 real function skin_temperature(hour)
 real,intent(in) :: hour
 skin_temperature=tsk_mean+tsk_amp*cos(2.*3.14159265*(hour-14.)/24.)
 end function

 ! specific humidity of the land skin from the specific humidity of the first layer
 real function skin_humidity(tsk,qv1)
 real,intent(in) :: tsk,qv1
 real :: es,qsat,qair
 es=611.2*exp(17.67*(tsk-273.15)/(tsk-29.65))
 qsat=0.622*es/(psfc-0.378*es)
 qair=qv1/(1.+qv1)
 if(qsat>qair) then
  skin_humidity=qair+beta*(qsat-qair)
 else
  skin_humidity=qsat
 endif
 end function

 ! stand-in for Noah-MP's 2 m fields of a land cell (the same numbers for both codes, from each code's surface layer)
 subroutine land_two_metre(q2_layer,t2_layer,qsfc)
 real,intent(in) :: q2_layer,t2_layer,qsfc
 land%energy_state_SpecHumidity2mVeg=q2_layer/(1.+q2_layer)
 land%energy_state_SpecHumidity2mBare=0.5*(qsfc+land%energy_state_SpecHumidity2mVeg)
 land%energy_state_VegFrac=0.6
 land%energy_state_TemperatureAir2m=t2_layer
 end subroutine

 ! --- lean side --------------------------------------------------------------------------------------------------------
 subroutine start_lean()
 a%qke=0.;a%tsq=0.;a%qsq=0.;a%cov=0.;a%el_pbl=0.;a%sh=0.;a%sm=0.;a%qc_bl=0.;a%qi_bl=0.;a%cldfra_bl=0.
 a%exch_h=0.;a%exch_m=0.;a%t_u=0.;a%t_v=0.;a%t_theta=0.;a%t_qv=0.;a%t_qc=0.;a%t_qi=0.;a%t_qs=0.;a%t_ni=0.
 a%pblh=0.;a%regime=0.;a%hfx=0.;a%qfx=0.;a%lh=0.;a%mol=0.;a%rmol=0.;a%qgh=0.;a%qsfc=0.;a%zol=0.;a%ust=0.
 a%cpm=0.;a%chs2=0.;a%cqs2=0.;a%chs=0.;a%ch=0.;a%flhc=0.;a%flqc=0.;a%gz1oz0=0.;a%wspd=0.;a%br=0.;a%psim=0.
 a%psih=0.;a%u10=0.;a%v10=0.;a%th2=0.;a%t2=0.;a%q2=0.;a%wstar=0.;a%qstar=0.;a%ck=0.;a%cka=0.;a%cd=0.;a%cda=0.
 a%ustm=0.;a%maxwidth=0.;a%maxmf=0.;a%ztop_plume=0.;a%kpbl=0;a%ktop_plume=0
 a%u=u0;a%v=v0;a%w=0.;a%theta=theta0;a%qv=qv0;a%qc=qc0;a%qi=qi0;a%qs=qs0;a%qr=0.;a%qg=0.;a%nr=0.
 a%ni=merge(1.e5,0.,qi0>0.)
 a%rho_moist=moist_density(a%theta,a%qv);a%p_hyd=p;a%p=p;a%exner=exner;a%dz=dz
 a%psfc=psfc;a%xland=xland;a%snowh=snowh;a%dx=dx;a%mavail=1.
 a%tsk=merge(sst,skin_temperature(start_hour),xland>1.5);a%znt=merge(1.e-4,znt_land,xland>1.5)
 end subroutine

 subroutine advance_lean()
 a%u=a%u+dt*a%t_u
 a%v=a%v+dt*a%t_v
 a%theta=a%theta+dt*a%t_theta
 a%qv=a%qv+dt*a%t_qv
 a%qc=a%qc+dt*a%t_qc
 a%qi=a%qi+dt*a%t_qi
 a%qs=a%qs+dt*a%t_qs
 a%ni=a%ni+dt*a%t_ni
 call relax(a%u,a%v,a%theta,a%qv,a%qc,a%qi,a%qs)
 a%rho_moist=moist_density(a%theta,a%qv)
 end subroutine

 ! the density the physics sees, as in physics_inputs.f90 (dry density times 1+qv); the pressure profile is held fixed
 function moist_density(theta,qv) result(rho_m)
 real,intent(in) :: theta(nz),qv(nz)
 real :: rho_m(nz)
 rho_m=p/(r_d*theta*exner*(1.+r_v/r_d*qv))
 rho_m=rho_m*(1.+qv)
 end function

 ! the large scale, the same for both codes: winds back to the initial profile in 6 h, the rest in 1 day
 subroutine relax(u,v,theta,qv,qc,qi,qs)
 real,intent(inout) :: u(nz),v(nz),theta(nz),qv(nz),qc(nz),qi(nz),qs(nz)
 u=u+dt/21600.*(u0-u)
 v=v+dt/21600.*(v0-v)
 theta=theta+dt/86400.*(theta0-theta)
 qv=qv+dt/86400.*(qv0-qv)
 qc=qc+dt/86400.*(qc0-qc)
 qi=qi+dt/86400.*(qi0-qi)
 qs=qs+dt/86400.*(qs0-qs)
 end subroutine

 ! --- MPAS side --------------------------------------------------------------------------------------------------------
 subroutine start_mpas()
 m_u(1,1:nz,1)=u0;m_v(1,1:nz,1)=v0;m_w=0.;m_th(1,1:nz,1)=theta0;m_qv(1,1:nz,1)=qv0;m_qc(1,1:nz,1)=qc0
 m_qi(1,1:nz,1)=qi0;m_qs(1,1:nz,1)=qs0;m_nc=0.;m_ni(1,1:nz,1)=merge(1.e5,0.,qi0>0.);m_nifa=0.;m_nwfa=0.
 m_p(1,1:nz,1)=p;m_pi(1,1:nz,1)=exner;m_dz(1,1:nz,1)=dz;m_rho(1,1:nz,1)=moist_density(theta0,qv0)
 m_rthraten=0.;m_pattern=0.;m_cldfra_bl=0.;m_qc_bl=0.;m_qi_bl=0.;m_el_pbl=0.;m_qke=0.;m_qke_adv=0.;m_cov=0.
 m_qsq=0.;m_tsq=0.;m_sh3d=0.;m_sm3d=0.;m_rublten=0.;m_rvblten=0.;m_rthblten=0.;m_rqvblten=0.;m_rqcblten=0.
 m_rqiblten=0.;m_rqsblten=0.;m_rncblten=0.;m_rniblten=0.;m_rnifablten=0.;m_rnwfablten=0.;m_edmf_a=0.;m_edmf_w=0.
 m_edmf_qt=0.;m_edmf_thl=0.;m_edmf_ent=0.;m_edmf_qc=0.;m_sub_thl=0.;m_sub_sqv=0.;m_det_thl=0.;m_det_sqv=0.
 m_exch_h=0.;m_exch_m=0.;m_dqke=0.;m_qwt=0.;m_qshear=0.;m_qbuoy=0.;m_qdiss=0.
 m_psfc=psfc;m_chs=0.;m_chs2=0.;m_cqs2=0.;m_cpm=0.;m_ust=0.;m_pblh=0.;m_mavail=1.;m_zol=0.;m_mol=0.;m_regime=0.
 m_psim=0.;m_psih=0.;m_xland=xland;m_hfx=0.;m_qfx=0.;m_lh=0.;m_flhc=0.;m_flqc=0.;m_qgh=0.;m_qsfc=0.;m_rmol=0.
 m_u10=0.;m_v10=0.;m_th2=0.;m_t2=0.;m_q2=0.;m_snowh=snowh;m_gz1oz0=0.;m_wspd=0.;m_br=0.;m_dx=dx;m_ustm=0.
 m_ck=0.;m_cka=0.;m_cd=0.;m_cda=0.;m_ch=0.;m_qcg=0.;m_uoce=0.;m_voce=0.;m_maxwidth=0.;m_maxmf=0.;m_ztop_plume=0.
 m_kpbl=0;m_ktop_plume=0;m_xice=0.
 m_tsk=merge(sst,skin_temperature(start_hour),xland>1.5);m_znt=merge(1.e-4,znt_land,xland>1.5)
 end subroutine

 subroutine advance_mpas()
 m_u(1,1:nz,1)=m_u(1,1:nz,1)+dt*m_rublten(1,1:nz,1)
 m_v(1,1:nz,1)=m_v(1,1:nz,1)+dt*m_rvblten(1,1:nz,1)
 m_th(1,1:nz,1)=m_th(1,1:nz,1)+dt*m_rthblten(1,1:nz,1)
 m_qv(1,1:nz,1)=m_qv(1,1:nz,1)+dt*m_rqvblten(1,1:nz,1)
 m_qc(1,1:nz,1)=m_qc(1,1:nz,1)+dt*m_rqcblten(1,1:nz,1)
 m_qi(1,1:nz,1)=m_qi(1,1:nz,1)+dt*m_rqiblten(1,1:nz,1)
 m_qs(1,1:nz,1)=m_qs(1,1:nz,1)+dt*m_rqsblten(1,1:nz,1)
 m_ni(1,1:nz,1)=m_ni(1,1:nz,1)+dt*m_rniblten(1,1:nz,1)
 call relax(m_u(1,1:nz,1),m_v(1,1:nz,1),m_th(1,1:nz,1),m_qv(1,1:nz,1),m_qc(1,1:nz,1),m_qi(1,1:nz,1),m_qs(1,1:nz,1))
 m_rho(1,1:nz,1)=moist_density(m_th(1,1:nz,1),m_qv(1,1:nz,1))
 end subroutine

 subroutine mpas_surface_layer(initflag)
 integer,intent(in) :: initflag
 m_t(1,1:nz,1)=m_th(1,1:nz,1)*m_pi(1,1:nz,1)
 call sfclay_mynn(p3d=m_p,pi3d=m_pi,psfcpa=m_psfc,th3d=m_th,t3d=m_t,u3d=m_u,v3d=m_v,qv3d=m_qv,qc3d=m_qc,rho3d=m_rho, &
  dz8w=m_dz,cp=cp,g=grav,rovcp=r_d/cp,R=r_d,xlv=2.5e6,chs=m_chs,chs2=m_chs2,cqs2=m_cqs2,cpm=m_cpm,znt=m_znt, &
  ust=m_ust,pblh=m_pblh,mavail=m_mavail,zol=m_zol,mol=m_mol,regime=m_regime,psim=m_psim,psih=m_psih,xland=m_xland, &
  hfx=m_hfx,qfx=m_qfx,lh=m_lh,tsk=m_tsk,flhc=m_flhc,flqc=m_flqc,qgh=m_qgh,qsfc=m_qsfc,rmol=m_rmol,u10=m_u10, &
  v10=m_v10,th2=m_th2,t2=m_t2,q2=m_q2,snowh=m_snowh,gz1oz0=m_gz1oz0,wspd=m_wspd,br=m_br,isfflx=1,dx=m_dx, &
  svp1=0.6112,svp2=17.67,svp3=29.65,svpt0=273.15,ep1=r_v/r_d-1.,ep2=r_d/r_v,karman=0.4,ustm=m_ustm,ck=m_ck, &
  cka=m_cka,cd=m_cd,cda=m_cda,ch=m_ch,qcg=m_qcg,spp_pbl=0,isftcflx=0,iz0tlnd=0,itimestep=initflag, &
  errmsg=errmsg,errflg=errflg,ids=1,ide=1,jds=1,jde=1,kds=1,kde=nz+1,ims=1,ime=1,jms=1,jme=1,kms=1,kme=nz+1, &
  its=1,ite=1,jts=1,jte=1,kts=1,kte=nz)
 if(errflg/=0) error stop 'MPAS sfclay_mynn'
 end subroutine

 subroutine mpas_pbl(initflag)
 integer,intent(in) :: initflag
 call mynn_bl_driver(f_qc=.true.,f_qi=.true.,f_qs=.true.,f_qoz=.false.,f_nc=.false.,f_ni=.true.,f_nifa=.false., &
  f_nwfa=.false.,f_nbca=.false.,icloud_bl=0,delt=dt,dx=m_dx,xland=m_xland,ps=m_psfc,ts=m_tsk,qsfc=m_qsfc,ust=m_ust, &
  ch=m_ch,hfx=m_hfx,qfx=m_qfx,rmol=m_rmol,wspd=m_wspd,znt=m_znt,uoce=m_uoce,voce=m_voce,dz=m_dz,u=m_u,v=m_v,w=m_w, &
  th=m_th,tt=m_t,p=m_p,exner=m_pi,rho=m_rho,qv=m_qv,qc=m_qc,qi=m_qi,qs=m_qs,nc=m_nc,ni=m_ni,nifa=m_nifa,nwfa=m_nwfa, &
  rthraten=m_rthraten,pblh=m_pblh,kpbl=m_kpbl,cldfra_bl=m_cldfra_bl,qc_bl=m_qc_bl,qi_bl=m_qi_bl,maxwidth=m_maxwidth, &
  maxmf=m_maxmf,ktop_plume=m_ktop_plume,ztop_plume=m_ztop_plume,dqke=m_dqke,qke_adv=m_qke_adv,tsq=m_tsq,qsq=m_qsq, &
  cov=m_cov,el_pbl=m_el_pbl,rublten=m_rublten,rvblten=m_rvblten,rthblten=m_rthblten,rqvblten=m_rqvblten, &
  rqcblten=m_rqcblten,rqiblten=m_rqiblten,rqsblten=m_rqsblten,rncblten=m_rncblten,rniblten=m_rniblten, &
  rnifablten=m_rnifablten,rnwfablten=m_rnwfablten,edmf_a=m_edmf_a,edmf_w=m_edmf_w,edmf_qt=m_edmf_qt, &
  edmf_thl=m_edmf_thl,edmf_ent=m_edmf_ent,edmf_qc=m_edmf_qc,sub_thl=m_sub_thl,sub_sqv=m_sub_sqv,det_thl=m_det_thl, &
  det_sqv=m_det_sqv,exch_h=m_exch_h,exch_m=m_exch_m,qke=m_qke,qwt=m_qwt,qshear=m_qshear,qbuoy=m_qbuoy,qdiss=m_qdiss, &
  sh3d=m_sh3d,sm3d=m_sm3d,spp_pbl=0,pattern_spp=m_pattern,do_restart=.false.,do_DAcycling=.false.,initflag=initflag, &
  bl_mynn_tkeadvect=.false.,bl_mynn_tkebudget=0,bl_mynn_cloudpdf=2,bl_mynn_mixlength=2,bl_mynn_closure=2.5, &
  bl_mynn_stfunc=1,bl_mynn_topdown=0,bl_mynn_scaleaware=1,bl_mynn_dheat_opt=1,bl_mynn_edmf=1,bl_mynn_edmf_dd=0, &
  bl_mynn_edmf_mom=0,bl_mynn_edmf_tke=0,bl_mynn_output=0,bl_mynn_mixscalars=1,bl_mynn_cloudmix=1,bl_mynn_mixqt=0, &
  ids=1,ide=1,jds=1,jde=1,kds=1,kde=nz+1,ims=1,ime=1,jms=1,jme=1,kms=1,kme=nz+1,its=1,ite=1,jts=1,jte=1,kts=1, &
  kte=nz,errmsg=errmsg,errflg=errflg)
 if(errflg/=0) error stop 'MPAS mynn_bl_driver'
 end subroutine

 subroutine mpas_diagnostics()
 real :: q2veg,q2bare,fveg,q2mxy(1),t2mxy(1)
 ! EnergyVarOutTransferMod.F90:122,137-139 (Noah-MP's 2 m output), from the same stand-in fields as the lean side
 call land_two_metre(m_q2(1,1),m_t2(1,1),m_qsfc(1,1))
 q2veg=land%energy_state_SpecHumidity2mVeg/(1.0-land%energy_state_SpecHumidity2mVeg)
 q2bare=land%energy_state_SpecHumidity2mBare/(1.0-land%energy_state_SpecHumidity2mBare)
 fveg=land%energy_state_VegFrac
 q2mxy(1)=q2bare*(1-fveg)+q2veg*fveg
 t2mxy(1)=land%energy_state_TemperatureAir2m
 call mpas_sfc_diagnostics(m_psfc(:,1),m_tsk(:,1),m_xland(:,1),m_xice(:,1),m_chs2(:,1),m_cqs2(:,1),m_hfx(:,1), &
  m_qfx(:,1),m_qsfc(:,1),q2mxy,t2mxy,m_q2(:,1),m_t2(:,1),m_th2(:,1))
 end subroutine

 ! --- comparison -------------------------------------------------------------------------------------------------------
 subroutine compare_all(step)
 integer,intent(in) :: step
 nf=0
 call same('hfx',[a%hfx],m_hfx(:,1),step);call same('qfx',[a%qfx],m_qfx(:,1),step);call same('lh',[a%lh],m_lh(:,1),step)
 call same('ust',[a%ust],m_ust(:,1),step);call same('mol',[a%mol],m_mol(:,1),step)
 call same('rmol',[a%rmol],m_rmol(:,1),step);call same('zol',[a%zol],m_zol(:,1),step)
 call same('regime',[a%regime],m_regime(:,1),step);call same('psim',[a%psim],m_psim(:,1),step)
 call same('psih',[a%psih],m_psih(:,1),step);call same('qsfc',[a%qsfc],m_qsfc(:,1),step)
 call same('qgh',[a%qgh],m_qgh(:,1),step);call same('cpm',[a%cpm],m_cpm(:,1),step)
 call same('chs',[a%chs],m_chs(:,1),step);call same('chs2',[a%chs2],m_chs2(:,1),step)
 call same('cqs2',[a%cqs2],m_cqs2(:,1),step);call same('ch',[a%ch],m_ch(:,1),step)
 call same('flhc',[a%flhc],m_flhc(:,1),step);call same('flqc',[a%flqc],m_flqc(:,1),step)
 call same('gz1oz0',[a%gz1oz0],m_gz1oz0(:,1),step);call same('wspd',[a%wspd],m_wspd(:,1),step)
 call same('br',[a%br],m_br(:,1),step);call same('u10',[a%u10],m_u10(:,1),step)
 call same('v10',[a%v10],m_v10(:,1),step);call same('znt',[a%znt],m_znt(:,1),step)
 call same('ustm',[a%ustm],m_ustm(:,1),step);call same('ck',[a%ck],m_ck(:,1),step)
 call same('cka',[a%cka],m_cka(:,1),step);call same('cd',[a%cd],m_cd(:,1),step)
 call same('cda',[a%cda],m_cda(:,1),step)
 call same('t2',[a%t2],m_t2(:,1),step);call same('q2',[a%q2],m_q2(:,1),step);call same('th2',[a%th2],m_th2(:,1),step)
 call same('pblh',[a%pblh],m_pblh(:,1),step);call same('kpbl',[real(a%kpbl)],[real(m_kpbl(1,1))],step)
 call same('maxwidth',[a%maxwidth],m_maxwidth(:,1),step);call same('maxmf',[a%maxmf],m_maxmf(:,1),step)
 call same('ztop_plume',[a%ztop_plume],m_ztop_plume(:,1),step)
 call same('ktop_plume',[real(a%ktop_plume)],[real(m_ktop_plume(1,1))],step)
 call same('t_u',a%t_u,m_rublten(1,1:nz,1),step);call same('t_v',a%t_v,m_rvblten(1,1:nz,1),step)
 call same('t_theta',a%t_theta,m_rthblten(1,1:nz,1),step);call same('t_qv',a%t_qv,m_rqvblten(1,1:nz,1),step)
 call same('t_qc',a%t_qc,m_rqcblten(1,1:nz,1),step);call same('t_qi',a%t_qi,m_rqiblten(1,1:nz,1),step)
 call same('t_qs',a%t_qs,m_rqsblten(1,1:nz,1),step);call same('t_ni',a%t_ni,m_rniblten(1,1:nz,1),step)
 call same('qke',a%qke,m_qke(1,1:nz,1),step);call same('tsq',a%tsq,m_tsq(1,1:nz,1),step)
 call same('qsq',a%qsq,m_qsq(1,1:nz,1),step);call same('cov',a%cov,m_cov(1,1:nz,1),step)
 call same('el_pbl',a%el_pbl,m_el_pbl(1,1:nz,1),step);call same('sh',a%sh,m_sh3d(1,1:nz,1),step)
 call same('sm',a%sm,m_sm3d(1,1:nz,1),step);call same('qc_bl',a%qc_bl,m_qc_bl(1,1:nz,1),step)
 call same('qi_bl',a%qi_bl,m_qi_bl(1,1:nz,1),step);call same('cldfra_bl',a%cldfra_bl,m_cldfra_bl(1,1:nz,1),step)
 call same('exch_h',a%exch_h,m_exch_h(1,1:nz,1),step);call same('exch_m',a%exch_m,m_exch_m(1,1:nz,1),step)
 call same('theta',a%theta,m_th(1,1:nz,1),step);call same('qv',a%qv,m_qv(1,1:nz,1),step)
 call same('qc',a%qc,m_qc(1,1:nz,1),step);call same('qi',a%qi,m_qi(1,1:nz,1),step)
 call same('qs',a%qs,m_qs(1,1:nz,1),step);call same('u',a%u,m_u(1,1:nz,1),step);call same('v',a%v,m_v(1,1:nz,1),step)
 end subroutine

 subroutine same(name,x,y,step)
 character(*),intent(in) :: name
 real,intent(in) :: x(:),y(:)
 integer,intent(in) :: step
 integer :: n
 nf=nf+1
 field_name(nf)=name
 n=count(transfer(x,0,size(x))/=transfer(y,0,size(y)))
 compared(nf)=compared(nf)+size(x)
 differing(nf)=differing(nf)+n
 if(n>0.and.first_step(nf)==0) first_step(nf)=step
 end subroutine

 subroutine report(r)
 integer,intent(in) :: r
 integer :: f
 regime_compared=sum(compared)
 print '(a12,a,i0,a,i0,a)',regime_name(r),': ',regime_compared,' words compared, ',sum(differing),' differ'
 do f=1,nf
  if(differing(f)>0) print '(4x,a12,i10,a,i10,a,i0)',field_name(f),differing(f),' differ of',compared(f), &
   ', first at step ',first_step(f)
 enddo
 total_compared=total_compared+regime_compared;total_differing=total_differing+sum(differing)
 compared=0;differing=0;first_step=0
 end subroutine

 ! --- the water budget of the PBL step and the trace ---------------------------------------------------------------------
 ! F = dt*qfx; W = sum rho_d*dz*dt*t_qv (rho_d = rho_m/(1+qv)); C the same for qc+qi+qs; Es, Cs = sum rho_m*dz*ds, MYNN's own change of the
 ! specific contents at its fixed moist density rho_m = rho_d*(1+qv), recovered from the tendency with the conversion of
 ! the side (MPAS: s'/(1-s') and s*(1+s'); LEAN_MYNN_MOIST: (1+qv)*ds). All in double precision.
 subroutine budget()
 call record(a%rho_moist,a%qv,a%qc,a%qi,a%qs,a%t_qv,a%t_qc,a%t_qi,a%t_qs,a%qfx,a%t2,a%q2,a%th2,a%hfx,a%lh,a%ust,a%tsk,a%qsfc, &
  a%theta(1),a%pblh,a%maxmf,a%u10,a%v10,.true.)
 write(ul) rec
 call record(m_rho(1,1:nz,1),m_qv(1,1:nz,1),m_qc(1,1:nz,1),m_qi(1,1:nz,1),m_qs(1,1:nz,1),m_rqvblten(1,1:nz,1),m_rqcblten(1,1:nz,1), &
  m_rqiblten(1,1:nz,1),m_rqsblten(1,1:nz,1),m_qfx(1,1),m_t2(1,1),m_q2(1,1),m_th2(1,1),m_hfx(1,1),m_lh(1,1),m_ust(1,1), &
  m_tsk(1,1),m_qsfc(1,1),m_th(1,1,1),m_pblh(1,1),m_maxmf(1,1),m_u10(1,1),m_v10(1,1),.false.)
 write(um) rec
 end subroutine

 subroutine record(rho_m,qv,qc,qi,qs,tqv,tqc,tqi,tqs,qfx,t2,q2,th2,hfx,lh,ust,tsk,qsfc,theta1,pblh,maxmf,u10,v10,lean)
 real,intent(in) :: rho_m(nz),qv(nz),qc(nz),qi(nz),qs(nz),tqv(nz),tqc(nz),tqi(nz),tqs(nz),qfx,t2,q2,th2,hfx,lh,ust,tsk,qsfc
 real,intent(in) :: theta1,pblh,maxmf,u10,v10
 logical,intent(in) :: lean
 real(8) :: r,rn,s,sn,ds,dsc,mass_d,mass_m,w,c,es,cs,vap,cond,rhosfc,ulps
 integer :: k
 w=0;c=0;es=0;cs=0;vap=0;cond=0;ulps=0
 do k=1,nz
  mass_m=dble(rho_m(k))*dz(k)
  mass_d=mass_m/(1d0+qv(k))
  w=w+mass_d*dt*tqv(k)
  c=c+mass_d*dt*(dble(tqc(k))+tqi(k)+tqs(k))
  r=qv(k);s=r/(1d0+r)
  if(lean.and.lean_moist) then
   ds=dt*tqv(k)/(1d0+r)
   dsc=dt*(dble(tqc(k))+tqi(k)+tqs(k))/(1d0+r)
  else
   rn=r+dble(dt)*tqv(k);sn=rn/(1d0+rn);ds=sn-s
   dsc=((qc(k)+dble(dt)*tqc(k))+(qi(k)+dble(dt)*tqi(k))+(qs(k)+dble(dt)*tqs(k)))/(1d0+sn) &
    -(dble(qc(k))+qi(k)+qs(k))/(1d0+r)
  endif
  es=es+mass_m*ds
  ulps=ulps+mass_m*spacing(real(s))   ! one ulp of each layer's specific humidity, as mass
  cs=cs+mass_m*dsc
  vap=vap+mass_d*qv(k)
  cond=cond+mass_d*(dble(qc(k))+qi(k)+qs(k))
 enddo
 rhosfc=psfc/(r_d*(dble(theta1)*exner(1)+0.608d0*qv(1)))   ! MPAS's rhosfc, with qv a mixing ratio as in MYNN
 rec=[dble(t2),dble(q2),dble(th2),dble(hfx),dble(qfx),dble(lh),dble(ust),dble(tsk),dble(qsfc),dble(qv(1)), &
  dble(theta1),dble(qc(1)),dble(pblh),dble(psfc),dt*dble(qfx),w,c,es,dble(q2_surface_layer),vap,cond,dble(maxmf), &
  dble(u10),dble(v10),dble(maxval(abs(tqs))),cs,rhosfc/rho_m(1),dble(t2_surface_layer),ulps]
 if(.not.lean) rec(19)=m_q2_surface_layer
 if(.not.lean) rec(28)=m_t2_surface_layer
 end subroutine

 ! --- the conversions alone ----------------------------------------------------------------------------------------------
 ! A zero MYNN tendency must give a zero mixing-ratio tendency, and a change ds of the specific contents must change
 ! the mass rho_d*dz*q by rho_m*dz*ds. 10,000 random columns.
 subroutine conversion_unit_test()
 real :: qv(nz),qc(nz),qi(nz),qs(nz),dqv(nz),dqc(nz),dqi(nz),dqs(nz),x(4*nz)
 integer :: n,nonzero_v,nonzero_c,nseed
 integer,allocatable :: seed(:)
 real(8) :: worst_zero,worst_mass,mass_s,mass_r
 call random_seed(size=nseed)
 allocate(seed(nseed));seed=12345;call random_seed(put=seed)
 nonzero_v=0;nonzero_c=0;worst_zero=0;worst_mass=0
 do n=1,10000
  call random_number(x)
  qv=2.e-2*x(1:nz);qc=1.e-3*x(nz+1:2*nz);qi=5.e-4*x(2*nz+1:3*nz);qs=1.e-3*x(3*nz+1:4*nz)
  dqv=0.;dqc=0.;dqi=0.;dqs=0.
  call pbl_mixing_tendencies(dt,qv,qc,qi,qs,dqv,dqc,dqi,dqs)
  nonzero_v=nonzero_v+count(dqv/=0.)
  nonzero_c=nonzero_c+count(dqc/=0.)+count(dqi/=0.)+count(dqs/=0.)
  worst_zero=max(worst_zero,maxval(abs(dt*dqc)/max(qc,1e-12)),maxval(abs(dt*dqs)/max(qs,1e-12)))
  ! one unit of specific content into every level: the mass change must be rho_m*dz*ds = rho_d*(1+qv)*dz*ds
  dqv=1.e-6/dt;dqc=0.;dqi=0.;dqs=0.
  call pbl_mixing_tendencies(dt,qv,qc,qi,qs,dqv,dqc,dqi,dqs)
  mass_s=sum((1d0+qv)*1d-6)
  mass_r=sum(dble(dt)*dqv)
  worst_mass=max(worst_mass,abs(mass_r/mass_s-1d0))
 enddo
 print '(a,i0,a,i0,a,es9.2)','conversions, zero tendency in: nonzero out ',nonzero_v,' of 450000 vapour words, ', &
  nonzero_c,' of 1350000 condensate words; worst condensate change per call (relative) ',worst_zero
 print '(a,es9.2)','conversions, ds = 1e-6 into every level: vapour added / (rho_m*ds) - 1, worst ',worst_mass
 end subroutine
end program
