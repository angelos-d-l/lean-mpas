! The MYNN surface layer and PBL for one column, as MPAS's drivers call them (mpas_atmphys_driver_sfclayer.F,
! mpas_atmphys_driver_pbl.F): *_column on a column view, *_state on (state, c) for the card kernels.
module mynn
 use config,only: nz,dt
 use model_state,only: column_view,state_type
 use mynn_conversions,only: pbl_specific_contents,pbl_mixing_tendencies
 use lean_sf_mynn,only: sf_mynn_init,sf_mynn_run
 use lean_bl_mynn,only: bl_mynn_init,bl_mynn_run
 implicit none
 private
 public :: mynn_init,mynn_column,mynn_surface_column,mynn_pbl_column,mynn_pbl_state,mynn_surface_state
contains
 subroutine mynn_init()
 character(256) :: message
 integer :: error
 call sf_mynn_init(message,error)
 if(error/=0) error stop 'MYNN surface initialization'
 ! the constants MPAS passes (physics/mpas_atmphys_driver_pbl.F:746-747), with their values of
 ! physics/mpas_atmphys_constants.F and mpas_constants (R_d 287, R_v 461.6, gravity 9.80616, cp 1004.5)
 call bl_mynn_init(con_cp=1004.5,con_cpv=4.*461.6,con_cice=2106.,con_cliq=4190.,con_ep1=461.6/287.-1., &
  con_ep2=287./461.6,con_grav=9.80616,con_karman=0.4,con_p0=1.e5,con_rd=287.,con_rv=461.6, &
  con_svp1=0.6112,con_svp2=17.67,con_svp3=29.65,con_svpt0=273.15,con_xlf=3.5e5,con_xls=2.85e6,con_xlv=2.5e6, &
  errmsg=message,errflg=error)
 if(error/=0) error stop 'MYNN PBL initialization'
 end subroutine

 subroutine mynn_column(a,itimestep)
 type(column_view),intent(in) :: a
 integer,intent(in) :: itimestep
 call mynn_surface_column(a,itimestep)
 call mynn_pbl_column(a,itimestep)
 end subroutine

 subroutine mynn_surface_column(a,itimestep)
 type(column_view),intent(in) :: a
 integer,intent(in) :: itimestep
 real :: t(nz)
 t=a%theta*a%exner
 if(itimestep==1) then
  a%ust=max(0.04*sqrt(a%u(1)*a%u(1)+a%v(1)*a%v(1)),0.001)
  a%mol=0.
  a%qsfc=a%qv(1)/(1.+a%qv(1))
  a%qstar=0.
 endif
 call surface_column(a,t,itimestep)
 end subroutine

 ! mynn_surface_column for column c of the state, without the column view (see mynn_pbl_state)
 subroutine mynn_surface_state(state,c,itimestep)
 type(state_type),intent(inout) :: state
 integer,intent(in) :: c
 integer,intent(in) :: itimestep
 real :: t(nz)
 integer :: k
 do k=1,nz
  t(k)=state%theta(k,c)*state%exner(k,c)
 enddo
 if(itimestep==1) then
  state%ust(c)=max(0.04*sqrt(state%u(1,c)*state%u(1,c)+state%v(1,c)*state%v(1,c)),0.001)
  state%mol(c)=0.
  state%qsfc(c)=state%qv(1,c)/(1.+state%qv(1,c))
  state%qstar(c)=0.
 endif
 call surface_state(state,c,t,itimestep)
 end subroutine

 subroutine surface_state(state,c,t,itimestep)
 type(state_type),intent(inout) :: state
 integer,intent(in) :: c
 real,intent(in) :: t(nz)
 integer,intent(in) :: itimestep
 character(256) :: message
 integer :: error
 call sf_mynn_run( &
  u1d=state%u(1,c), &
  v1d=state%v(1,c), &
  t1d=t(1), &
  qv1d=state%qv(1,c), &
  p1d=state%p_hyd(1,c), &
  dz8w1d=state%dz(1,c), &
  rho1d=state%rho_moist(1,c), &
  u1d2=state%u(2,c), &
  v1d2=state%v(2,c), &
  dz2w1d=state%dz(2,c), &
  cp=1004.5, &
  g=9.80616, &
  rovcp=287./1004.5, &
  xlv=2.5e6, &
  psfcpa=state%psfc(c), &
  chs=state%chs(c), &
  chs2=state%chs2(c), &
  cqs2=state%cqs2(c), &
  cpm=state%cpm(c), &
  pblh=state%pblh(c), &
  rmol=state%rmol(c), &
  znt=state%znt(c), &
  ust=state%ust(c), &
  mavail=state%mavail(c), &
  zol=state%zol(c), &
  mol=state%mol(c), &
  regime=state%regime(c), &
  psim=state%psim(c), &
  psih=state%psih(c), &
  xland=state%xland(c), &
  hfx=state%hfx(c), &
  qfx=state%qfx(c), &
  tsk=state%tsk(c), &
  u10=state%u10(c), &
  v10=state%v10(c), &
  th2=state%th2(c), &
  t2=state%t2(c), &
  q2=state%q2(c), &
  flhc=state%flhc(c), &
  flqc=state%flqc(c), &
  snowh=state%snowh(c), &
  qgh=state%qgh(c), &
  qsfc=state%qsfc(c), &
  lh=state%lh(c), &
  gz1oz0=state%gz1oz0(c), &
  wspd=state%wspd(c), &
  br=state%br(c), &
  dx=state%dx(c), &
  svp1=0.6112, &
  svp2=17.67, &
  svp3=29.65, &
  svpt0=273.15, &
  ep1=461.6/287.-1., &
  ep2=287./461.6, &
  karman=0.4, &
  ch=state%ch(c), &
  itimestep=merge(1,0,itimestep==1), &   ! the original passes initflag, 0 after a cold first step (mpas_atmphys_driver_sfclayer.F:885-886,1039)
  wstar=state%wstar(c), &
  qstar=state%qstar(c), &
  ustm=state%ustm(c), &
  ck=state%ck(c), &
  cka=state%cka(c), &
  cd=state%cd(c), &
  cda=state%cda(c), &
  rstoch1d=0., &
  errmsg=message, &
  errflg=error)
 if(error/=0) error stop 'MYNN surface column'
 end subroutine

 ! mynn_pbl_column for column c of the state, without the column view: the column sections go to bl_mynn_run as they are (the
 ! arrays of state are contiguous), so the card kernel's frame holds neither the view (about 7 KB of pointers) nor the aliases.
 ! The same calls with the same values as mynn_pbl_column.
 subroutine mynn_pbl_state(state,c,itimestep)
 type(state_type),intent(inout) :: state
 integer,intent(in) :: c,itimestep
 real :: t(nz),sqv(nz),sqc(nz),sqi(nz),sqs(nz)
 character(256) :: message
 integer :: error,k
 do k=1,nz
  t(k)=state%theta(k,c)*state%exner(k,c)
 enddo
 call pbl_specific_contents(state%qv(:,c),state%qc(:,c),state%qi(:,c),state%qs(:,c),sqv,sqc,sqi,sqs)
 call bl_mynn_run( &
  initflag=merge(1,0,itimestep==1), &
  delt=dt, &
  dz=state%dz(:,c), &
  dx=state%dx(c), &
  u=state%u(:,c), &
  v=state%v(:,c), &
  w=state%w(:,c), &
  th=state%theta(:,c), &
  sqv=sqv, &
  sqc=sqc, &
  sqi=sqi, &
  sqs=sqs, &
  qni=state%ni(:,c), &
  p=state%p_hyd(:,c), &
  exner=state%exner(:,c), &
  rho=state%rho_moist(:,c), &
  tt=t, &
  xland=state%xland(c), &
  ts=state%tsk(c), &
  ps=state%psfc(c), &
  ust=state%ust(c), &
  hfx=state%hfx(c), &
  qfx=state%qfx(c), &
  rmol=state%rmol(c), &
  wspd=state%wspd(c), &
  uoce=0., &
  voce=0., &
  qke=state%qke(:,c), &
  tsq=state%tsq(:,c), &
  qsq=state%qsq(:,c), &
  cov=state%cov(:,c), &
  rublten=state%t_u(:,c), &
  rvblten=state%t_v(:,c), &
  rthblten=state%t_theta(:,c), &
  rqvblten=state%t_qv(:,c), &
  rqcblten=state%t_qc(:,c), &
  rqiblten=state%t_qi(:,c), &
  rqsblten=state%t_qs(:,c), &
  rqniblten=state%t_ni(:,c), &
  exch_h=state%exch_h(:,c), &
  exch_m=state%exch_m(:,c), &
  pblh=state%pblh(c), &
  kpbl=state%kpbl(c), &
  el_pbl=state%el_pbl(:,c), &
  sh=state%sh(:,c), &
  sm=state%sm(:,c), &
  qc_bl=state%qc_bl(:,c), &
  qi_bl=state%qi_bl(:,c), &
  cldfra_bl=state%cldfra_bl(:,c), &
  maxwidth=state%maxwidth(c), &
  maxmf=state%maxmf(c), &
  ztop_plume=state%ztop_plume(c), &
  ktop_plume=state%ktop_plume(c), &
  errmsg=message, &
  errflg=error)
 if(error/=0) error stop 'MYNN PBL column'
 call pbl_mixing_tendencies(dt,state%qv(:,c),state%qc(:,c),state%qi(:,c),state%qs(:,c),state%t_qv(:,c),state%t_qc(:,c), &
  state%t_qi(:,c),state%t_qs(:,c))
 end subroutine

 subroutine mynn_pbl_column(a,itimestep)
 type(column_view),intent(in) :: a
 integer,intent(in) :: itimestep
 real,pointer,contiguous :: col_qv(:)
 real,pointer,contiguous :: col_qc(:)
 real,pointer,contiguous :: col_qi(:)
 real,pointer,contiguous :: col_qs(:)
 real,pointer,contiguous :: col_dz(:)
 real,pointer,contiguous :: col_u(:)
 real,pointer,contiguous :: col_v(:)
 real,pointer,contiguous :: col_w(:)
 real,pointer,contiguous :: col_theta(:)
 real,pointer,contiguous :: col_ni(:)
 real,pointer,contiguous :: col_p_hyd(:)
 real,pointer,contiguous :: col_exner(:)
 real,pointer,contiguous :: col_rho_moist(:)
 real,pointer,contiguous :: col_qke(:)
 real,pointer,contiguous :: col_tsq(:)
 real,pointer,contiguous :: col_qsq(:)
 real,pointer,contiguous :: col_cov(:)
 real,pointer,contiguous :: col_t_u(:)
 real,pointer,contiguous :: col_t_v(:)
 real,pointer,contiguous :: col_t_theta(:)
 real,pointer,contiguous :: col_t_qv(:)
 real,pointer,contiguous :: col_t_qc(:)
 real,pointer,contiguous :: col_t_qi(:)
 real,pointer,contiguous :: col_t_qs(:)
 real,pointer,contiguous :: col_t_ni(:)
 real,pointer,contiguous :: col_exch_h(:)
 real,pointer,contiguous :: col_exch_m(:)
 real,pointer,contiguous :: col_el_pbl(:)
 real,pointer,contiguous :: col_sh(:)
 real,pointer,contiguous :: col_sm(:)
 real,pointer,contiguous :: col_qc_bl(:)
 real,pointer,contiguous :: col_qi_bl(:)
 real,pointer,contiguous :: col_cldfra_bl(:)
 real :: t(nz),sqv(nz),sqc(nz),sqi(nz),sqs(nz)
 character(256) :: message
 integer :: error
 ! Local contiguous aliases prevent gfortran packing guards on pointer components.
 col_qv=>a%qv
 col_qc=>a%qc
 col_qi=>a%qi
 col_qs=>a%qs
 col_dz=>a%dz
 col_u=>a%u
 col_v=>a%v
 col_w=>a%w
 col_theta=>a%theta
 col_ni=>a%ni
 col_p_hyd=>a%p_hyd
 col_exner=>a%exner
 col_rho_moist=>a%rho_moist
 col_qke=>a%qke
 col_tsq=>a%tsq
 col_qsq=>a%qsq
 col_cov=>a%cov
 col_t_u=>a%t_u
 col_t_v=>a%t_v
 col_t_theta=>a%t_theta
 col_t_qv=>a%t_qv
 col_t_qc=>a%t_qc
 col_t_qi=>a%t_qi
 col_t_qs=>a%t_qs
 col_t_ni=>a%t_ni
 col_exch_h=>a%exch_h
 col_exch_m=>a%exch_m
 col_el_pbl=>a%el_pbl
 col_sh=>a%sh
 col_sm=>a%sm
 col_qc_bl=>a%qc_bl
 col_qi_bl=>a%qi_bl
 col_cldfra_bl=>a%cldfra_bl
 t=a%theta*a%exner
 call pbl_specific_contents(col_qv,col_qc,col_qi,col_qs,sqv,sqc,sqi,sqs)
 ! PBL call
 call bl_mynn_run( &
  initflag=merge(1,0,itimestep==1), &
  delt=dt, &
  dz=col_dz, &
  dx=a%dx, &
  u=col_u, &
  v=col_v, &
  w=col_w, &
  th=col_theta, &
  sqv=sqv, &
  sqc=sqc, &
  sqi=sqi, &
  sqs=sqs, &
  qni=col_ni, &
  p=col_p_hyd, &
  exner=col_exner, &
  rho=col_rho_moist, &
  tt=t, &
  xland=a%xland, &
  ts=a%tsk, &
  ps=a%psfc, &
  ust=a%ust, &
  hfx=a%hfx, &
  qfx=a%qfx, &
  rmol=a%rmol, &
  wspd=a%wspd, &
  uoce=0., &
  voce=0., &
  qke=col_qke, &
  tsq=col_tsq, &
  qsq=col_qsq, &
  cov=col_cov, &
  rublten=col_t_u, &
  rvblten=col_t_v, &
  rthblten=col_t_theta, &
  rqvblten=col_t_qv, &
  rqcblten=col_t_qc, &
  rqiblten=col_t_qi, &
  rqsblten=col_t_qs, &
  rqniblten=col_t_ni, &
  exch_h=col_exch_h, &
  exch_m=col_exch_m, &
  pblh=a%pblh, &
  kpbl=a%kpbl, &
  el_pbl=col_el_pbl, &
  sh=col_sh, &
  sm=col_sm, &
  qc_bl=col_qc_bl, &
  qi_bl=col_qi_bl, &
  cldfra_bl=col_cldfra_bl, &
  maxwidth=a%maxwidth, &
  maxmf=a%maxmf, &
  ztop_plume=a%ztop_plume, &
  ktop_plume=a%ktop_plume, &
  errmsg=message, &
  errflg=error)
 if(error/=0) error stop 'MYNN PBL column'
 call pbl_mixing_tendencies(dt,col_qv,col_qc,col_qi,col_qs,col_t_qv,col_t_qc,col_t_qi,col_t_qs)
 end subroutine

 subroutine surface_column(a,t,itimestep)
 type(column_view),intent(in) :: a
 real,intent(in) :: t(nz)
 integer,intent(in) :: itimestep
 character(256) :: message
 integer :: error
 call sf_mynn_run( &
  u1d=a%u(1), &
  v1d=a%v(1), &
  t1d=t(1), &
  qv1d=a%qv(1), &
  p1d=a%p_hyd(1), &
  dz8w1d=a%dz(1), &
  rho1d=a%rho_moist(1), &
  u1d2=a%u(2), &
  v1d2=a%v(2), &
  dz2w1d=a%dz(2), &
  cp=1004.5, &
  g=9.80616, &
  rovcp=287./1004.5, &
  xlv=2.5e6, &
  psfcpa=a%psfc, &
  chs=a%chs, &
  chs2=a%chs2, &
  cqs2=a%cqs2, &
  cpm=a%cpm, &
  pblh=a%pblh, &
  rmol=a%rmol, &
  znt=a%znt, &
  ust=a%ust, &
  mavail=a%mavail, &
  zol=a%zol, &
  mol=a%mol, &
  regime=a%regime, &
  psim=a%psim, &
  psih=a%psih, &
  xland=a%xland, &
  hfx=a%hfx, &
  qfx=a%qfx, &
  tsk=a%tsk, &
  u10=a%u10, &
  v10=a%v10, &
  th2=a%th2, &
  t2=a%t2, &
  q2=a%q2, &
  flhc=a%flhc, &
  flqc=a%flqc, &
  snowh=a%snowh, &
  qgh=a%qgh, &
  qsfc=a%qsfc, &
  lh=a%lh, &
  gz1oz0=a%gz1oz0, &
  wspd=a%wspd, &
  br=a%br, &
  dx=a%dx, &
  svp1=0.6112, &
  svp2=17.67, &
  svp3=29.65, &
  svpt0=273.15, &
  ep1=461.6/287.-1., &
  ep2=287./461.6, &
  karman=0.4, &
  ch=a%ch, &
  itimestep=merge(1,0,itimestep==1), &   ! the original passes initflag, 0 after a cold first step (mpas_atmphys_driver_sfclayer.F:885-886,1039)
  wstar=a%wstar, &
  qstar=a%qstar, &
  ustm=a%ustm, &
  ck=a%ck, &
  cka=a%cka, &
  cd=a%cd, &
  cda=a%cda, &
  rstoch1d=0., &
  errmsg=message, &
  errflg=error)
 if(error/=0) error stop 'MYNN surface column'
 end subroutine
end module
