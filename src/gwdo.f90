! Orographic gravity-wave drag of one column: the inputs bl_gwdo_run takes, built from the model state as MPAS's
! driver_gwdo and module_bl_gwdo.F build them (dry hydrostatic pressure, temperature, vapour), then the drag.
module orographic_drag
 use config,only:nz,dt
 use model_state,only:column_view,state_type
 use gwdo_state,only:gwdo_column_state
 use lean_gwdo,only:gwdo_column
 implicit none
 private
 public::advance_gwdo,dry_pressure,advance_gwdo_state
contains
 subroutine dry_pressure(a,z,interfaces,layers)
 type(column_view),intent(in)::a
 real,intent(in)::z(nz+1)
 real,intent(out)::interfaces(nz+1),layers(nz)
 real::weight,density
 integer::k
 weight=(z(nz+1)-.5*(z(nz)+z(nz-1)))/(.5*(z(nz+1)+z(nz))-.5*(z(nz)+z(nz-1)))
 interfaces(nz+1)=exp(weight*log(a%p(nz))+(1.-weight)*log(a%p(nz-1)))
 do k=nz,1,-1
  density=a%rho_moist(k)/(1.+max(0.,a%qv(k)))
  interfaces(k)=interfaces(k+1)+9.80616*density*a%dz(k)
 enddo
 do k=nz,1,-1
  layers(k)=.5*(interfaces(k+1)+interfaces(k))
 enddo
 end subroutine
 subroutine advance_gwdo(a,drag,z)
 type(column_view),intent(in)::a
 type(gwdo_column_state),intent(in)::drag
 real,intent(in)::z(nz+1)
 real::interfaces(nz+1),pressure(nz),temperature(nz),midpoint(nz),vapor(nz)
 real,pointer,contiguous::u(:),v(:),exner(:),tendency_u(:),tendency_v(:),stress_u(:),stress_v(:)
 character(128)::message
 integer::error
 integer::kq
 real::dusfc,dvsfc,cosa,sina
 ! Local aliases keep gfortran from packing contiguous pointer components.
 u=>a%u;v=>a%v;exner=>a%exner;tendency_u=>a%t_u;tendency_v=>a%t_v
 stress_u=>drag%stress_u;stress_v=>drag%stress_v
 ! Without sub-grid orography (terrain_stddev <= 0) gwdo_column always sets ldrag, every stress is +0 and apply_drag only
 ! adds zeros: the tendencies keep their values (a zero of either sign becomes +0, as x + 0 does), the stresses are +0 and the
 ! surface stresses are the original expressions on zero sums (-0 and +0). The same values without the column work.
 if(drag%terrain_stddev<=0.)then
  cosa=1.;sina=0.
  do kq=1,nz   ! element by element (an array statement on pointers is a device malloc on the card)
   drag%increment_u(kq)=a%t_u(kq)
   drag%increment_v(kq)=a%t_v(kq)
   if(tendency_u(kq)==0.)tendency_u(kq)=0.
   if(tendency_v(kq)==0.)tendency_v(kq)=0.
   stress_u(kq)=0.;stress_v(kq)=0.
   drag%increment_u(kq)=a%t_u(kq)-drag%increment_u(kq)
   drag%increment_v(kq)=a%t_v(kq)-drag%increment_v(kq)
  enddo
  dusfc=0.;dvsfc=0.
  dusfc=(-1./9.80616)*dusfc
  dvsfc=(-1./9.80616)*dvsfc
  drag%surface_u=dusfc*cosa+dvsfc*sina
  drag%surface_v=-dusfc*sina+dvsfc*cosa
  return
 endif
 call dry_pressure(a,z,interfaces,pressure)
 temperature=a%theta*a%exner
 vapor=max(0.,a%qv)
 midpoint=.5*(z(2:nz+1)+z(1:nz))
 ! Reuse the diagnostic output storage for the pre-drag tendencies until return.
 drag%increment_u=a%t_u;drag%increment_v=a%t_v
 call gwdo_column(sina=0.,cosa=1.,rublten=tendency_u,rvblten=tendency_v, &
  dtaux3d=stress_u,dtauy3d=stress_v,dusfcg=drag%surface_u,dvsfcg=drag%surface_v, &
  uproj=u,vproj=v,t1=temperature,q1=vapor,prsi=interfaces,prsl=pressure,prslk=exner,zl=midpoint, &
  var=drag%terrain_stddev,oc1=drag%convexity, &
  oa2d1=drag%asymmetry(1),oa2d2=drag%asymmetry(2),oa2d3=drag%asymmetry(3),oa2d4=drag%asymmetry(4), &
  ol2d1=drag%length_fraction(1),ol2d2=drag%length_fraction(2),ol2d3=drag%length_fraction(3),ol2d4=drag%length_fraction(4), &
  g_=9.80616,cp_=1004.5,rd_=287.,rv_=461.6,fv_=461.6/287.-1.,pi_=3.141592653589793, &
  dxmeter=a%dx,deltim=dt,errmsg=message,errflg=error)
 if(error/=0)error stop 'GWDO column'
 drag%increment_u=a%t_u-drag%increment_u;drag%increment_v=a%t_v-drag%increment_v
 end subroutine

 ! advance_gwdo for column c of the state, without the column view (its 7 KB of pointers would be half of the card kernel's frame):
 ! the state's column sections directly. The same statements with the same values as advance_gwdo and dry_pressure.
 subroutine advance_gwdo_state(state,c,drag,z)
 type(state_type),intent(inout)::state
 integer,intent(in)::c
 type(gwdo_column_state),intent(in)::drag
 real,intent(in)::z(nz+1)
 real::interfaces(nz+1),pressure(nz),temperature(nz),midpoint(nz),vapor(nz)
 real,pointer,contiguous::stress_u(:),stress_v(:)
 character(128)::message
 integer :: error
 integer::kq
 real::dusfc,dvsfc,cosa,sina
 stress_u=>drag%stress_u;stress_v=>drag%stress_v
 if(drag%terrain_stddev<=0.)then
  cosa=1.;sina=0.
  do kq=1,nz
   drag%increment_u(kq)=state%t_u(kq,c)
   drag%increment_v(kq)=state%t_v(kq,c)
   if(state%t_u(kq,c)==0.)state%t_u(kq,c)=0.
   if(state%t_v(kq,c)==0.)state%t_v(kq,c)=0.
   stress_u(kq)=0.;stress_v(kq)=0.
   drag%increment_u(kq)=state%t_u(kq,c)-drag%increment_u(kq)
   drag%increment_v(kq)=state%t_v(kq,c)-drag%increment_v(kq)
  enddo
  dusfc=0.;dvsfc=0.
  dusfc=(-1./9.80616)*dusfc
  dvsfc=(-1./9.80616)*dvsfc
  drag%surface_u=dusfc*cosa+dvsfc*sina
  drag%surface_v=-dusfc*sina+dvsfc*cosa
  return
 endif
 call dry_pressure_state(state,c,z,interfaces,pressure)
 do kq=1,nz
  temperature(kq)=state%theta(kq,c)*state%exner(kq,c)
  vapor(kq)=max(0.,state%qv(kq,c))
  midpoint(kq)=.5*(z(kq+1)+z(kq))
  drag%increment_u(kq)=state%t_u(kq,c);drag%increment_v(kq)=state%t_v(kq,c)
 enddo
 call gwdo_column(sina=0.,cosa=1.,rublten=state%t_u(:,c),rvblten=state%t_v(:,c), &
  dtaux3d=stress_u,dtauy3d=stress_v,dusfcg=drag%surface_u,dvsfcg=drag%surface_v, &
  uproj=state%u(:,c),vproj=state%v(:,c),t1=temperature,q1=vapor,prsi=interfaces,prsl=pressure,prslk=state%exner(:,c), &
  zl=midpoint,var=drag%terrain_stddev,oc1=drag%convexity, &
  oa2d1=drag%asymmetry(1),oa2d2=drag%asymmetry(2),oa2d3=drag%asymmetry(3),oa2d4=drag%asymmetry(4), &
  ol2d1=drag%length_fraction(1),ol2d2=drag%length_fraction(2),ol2d3=drag%length_fraction(3),ol2d4=drag%length_fraction(4), &
  g_=9.80616,cp_=1004.5,rd_=287.,rv_=461.6,fv_=461.6/287.-1.,pi_=3.141592653589793, &
  dxmeter=state%dx(c),deltim=dt,errmsg=message,errflg=error)
 if(error/=0)error stop 'GWDO column'
 do kq=1,nz
  drag%increment_u(kq)=state%t_u(kq,c)-drag%increment_u(kq);drag%increment_v(kq)=state%t_v(kq,c)-drag%increment_v(kq)
 enddo
 end subroutine
 subroutine dry_pressure_state(state,c,z,interfaces,layers)
 type(state_type),intent(in)::state
 integer,intent(in)::c
 real,intent(in)::z(nz+1)
 real,intent(out)::interfaces(nz+1),layers(nz)
 real::weight,density
 integer::k
 weight=(z(nz+1)-.5*(z(nz)+z(nz-1)))/(.5*(z(nz+1)+z(nz))-.5*(z(nz)+z(nz-1)))
 interfaces(nz+1)=exp(weight*log(state%p(nz,c))+(1.-weight)*log(state%p(nz-1,c)))
 do k=nz,1,-1
  density=state%rho_moist(k,c)/(1.+max(0.,state%qv(k,c)))
  interfaces(k)=interfaces(k+1)+9.80616*density*state%dz(k,c)
 enddo
 do k=nz,1,-1
  layers(k)=.5*(interfaces(k+1)+interfaces(k))
 enddo
 end subroutine
end module
