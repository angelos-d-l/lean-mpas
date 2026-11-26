! Unit checks of the diagnostics manager on small synthetic stream sets and columns: which streams ask for diagnostics,
! when the maxima reset, the coordinate pools, precipitable water and the accumulated rain and snow. tests/diag/run.sh
! runs it twice, the second time built with LEAN_PRECIPW_MOIST.
program diagnostic_units
 use lean_clock
 use lean_messages
 use lean_streams
 use lean_stream_registry, only: field_index
 use dyn_micro, only: precipw_column
 use diag_convective
 use config, only: ncells
 use run_settings, only: model_dt=>dt
 use diag_manager
 implicit none
 type(stream_set) :: set
 type(run_clock) :: clock
 type(message_list) :: msgs
 type(io_plan) :: plan
 ! nvfortran 25.3 mishandles the default-constructor assignment set=stream_set(); fixture() copies these never-assigned
 ! variables of the same types instead.
 type(stream_set) :: blank_set
 type(run_clock) :: blank_clock
 type(message_list) :: blank_msgs
 integer :: i,j,k,dt,n,expected,counts(2)
 real :: qv(5),rho(5),zz(5),dz(5),pw,moist,expected_pw,sn(1),rain(1),frozen(1)
 real,allocatable::rates(:),rains(:),expected_rains(:)
 real::coordinates(13)
 logical :: ok
 ! Registry coordinates remain zero until their interpolated field is computed (diagnostics/mpas_isobaric_diagnostics.F:562-584).
 dt=120;n=0
 do j=1,2
  call fixture()
  set%s(1)%n_fields=1
  set%s(2)%has_output_interval=.false.;set%s(3)%has_output_interval=.false.
  k=field_index('t_iso_levels')
  if(j==2)k=field_index('z_iso_levels')
  set%s(1)%field(1)%field=k
  call diag_setup(set,clock,msgs)
  call require(diag_on,'coordinate producer recognized')
  coordinates=1.
  if(j==1)call diag_fetch('t_iso_levels',coordinates(:5))
  if(j==2)call diag_fetch('z_iso_levels',coordinates)
  k=5
  if(j==2)k=13
  call require(all(coordinates(:k)==0.),'coordinate pool starts zero')
  n=n+1
 enddo
 call require(n==2,'coordinate producers both exercised')
 ! MPAS physics_timetracker: reset at now before asking about the end of the step.
 do j=1,5
  dt=120
  if(j==5)dt=7
  call fixture()
  if(j==2)set%s(1)%has_output_interval=.false.
  if(j==3)then
   set%s(1)%has_output_interval=.false.;set%s(2)%has_output_interval=.false.
  endif
  if(j==4)set%s(1)%output_interval=interval_from_dt(1800.)
  call diag_setup(set,clock,msgs)
  n=0
  do i=1,10800/dt
   call diag_start_step(set,clock)
   k=3600
   if(j==4)k=1800
   expected=i*dt/k-(i-1)*dt/k
   if(j==3)expected=0
   call require(l_diags.eqv.(expected==1),'ldiags_steps')
   if(l_diags)n=n+1
   call advance_run_clock(clock)
  enddo
  expected=3
  if(j==3)expected=0
  if(j==4)expected=6
  if(j==5)expected=2
  call require(n==expected,'ldiags_steps count')
 enddo
 dt=120
 call fixture()
 set%s(1)%output_interval=interval_from_dt(3600.)
 clock%alarm(1)%interval=interval_from_dt(3600.)
 call diag_setup(set,clock,msgs)
 counts=0
 do i=1,60
  if(alarm_ringing(clock,set%s(1)%output_alarm))call reset_alarm(clock,set%s(1)%output_alarm)
  call diag_start_step(set,clock)
  call require(sum_pw.eqv.(mod(i,30)==0),'sum_pw_steps')
  if(sum_pw)counts(1)=counts(1)+1
  call advance_run_clock(clock)
 enddo
 call require(counts(1)==2,'sum_pw_steps count')
 ! Use the real stream planner: A at 1 h, B at 15 min, C inactive at 1 h.
 call fixture()
 set%s(1)%output_interval=interval_from_dt(3600.)
 clock%alarm(1)%interval=interval_from_dt(3600.)
 set%s(2)%output_interval=interval_from_dt(900.)
 clock%alarm(2)%interval=interval_from_dt(900.)
 call diag_setup(set,clock,msgs)
 clock%now=clock%start+interval_from_dt(900.)
 call plan_step_outputs(set,clock,plan)
 call require(size(plan%write)==1,'will_be_written 15 minute stream count')
 call diag_written(set,plan)
 call require(field_will_be_written('w_velocity_max'),'will_be_written maximum')
 call require(.not.field_will_be_written('cape'),'will_be_written cape absent')
 clock%now=clock%start+interval_from_dt(3600.)
 call plan_step_outputs(set,clock,plan)
 call require(size(plan%write)==2,'will_be_written hourly stream count')
 call diag_written(set,plan)
 call require(field_will_be_written('cape'),'will_be_written cape')
 plan%write=[3,3]
 call diag_written(set,plan)
 call require(.not.field_will_be_written('cape'),'will_be_written inactive package')
 set%s(3)%field(1)%field=field_index('updraft_helicity_max')
 set%s(1)%field(1)%field=field_index('updraft_helicity_max')
 set%s(2)%has_output_interval=.false.
 set%s(1)%has_output_interval=.false.
 call diag_setup(set,clock,msgs)
 call require(.not.need_uh_max,'inclusion none')
 set%s(1)%name='restart';set%s(1)%direction=STREAM_INPUT_OUTPUT
 set%s(1)%has_output_interval=.true.
 call diag_setup(set,clock,msgs)
 call require(.not.need_uh_max,'lean restart does not request diagnostics')
 set%s(1)%name='other'
 call diag_setup(set,clock,msgs)
 call require(need_uh_max,'inclusion input;output stream')
 ! The 15-minute carrier is not named diagnostics; reflectivity holds until diagnostics rings.
 call fixture()
 set%s(1)%name='diagnostics'
 set%s(1)%output_interval=interval_from_dt(3600.)
 clock%alarm(1)%interval=interval_from_dt(3600.)
 set%s(1)%field(1)%field=field_index('updraft_helicity_max')
 set%s(1)%field(2)%field=field_index('wind_speed_level1_max')
 set%s(2)%name='other'
 set%s(2)%output_interval=interval_from_dt(900.)
 clock%alarm(2)%interval=interval_from_dt(900.)
 set%s(2)%field(2)%field=field_index('refl10cm_1km_max')
 call diag_setup(set,clock,msgs)
 clock%now=clock%start+interval_from_dt(900.)
 call plan_step_outputs(set,clock,plan)
 call require(size(plan%write)==1.and..not.plan%diagnostics_rang,'reset_rules 15 minute plan')
 call diag_written(set,plan)
 w_max=5.;uh_max=4.;wsp_max=3.;refl10cm_1km_max=9.
 call diag_reset(plan%diagnostics_rang)
 call require(all(w_max==0.),'reset_rules 15 minute maximum')
 call require(all(uh_max==4.).and.all(wsp_max==3.),'reset_rules unwritten maxima held')
 call require(all(refl10cm_1km_max==9.),'reset_rules other stream holds reflectivity')
 clock%now=clock%start+interval_from_dt(3600.)
 call plan_step_outputs(set,clock,plan)
 call require(size(plan%write)==2.and.plan%diagnostics_rang,'reset_rules hourly plan')
 call diag_written(set,plan)
 call diag_reset(plan%diagnostics_rang)
 call require(all(uh_max==0.).and.all(wsp_max==0.).and.all(w_max==0.),'reset_rules hourly maxima')
 call require(all(refl10cm_1km_max==0.),'reset_rules diagnostics zeros reflectivity')
 call fixture()
 set%s(1)%field(1)%field=field_index('rainc')
 call diag_setup(set,clock,msgs)
 allocate(rates(ncells),rains(ncells),expected_rains(ncells));expected_rains=0.
 call require(ncells>0,'rainc_sum nonempty fixture')
 do k=1,3
  rates=real(k)*0.7
  expected_rains=expected_rains+rates*model_dt
  call diag_rain(rates,.false.)
  call diag_fetch('rainc',rains)
  call require(all(transfer(rains,[0],ncells)==transfer(expected_rains,[0],ncells)),'rainc_sum')
 enddo
 call require(all(rains>0.),'rainc_sum active values')
 qv=[0.02,0.01,0.005,0.002,0.001];rho=[1.2,1.,0.8,0.5,0.1];zz=1.
 dz=[100.,200.,300.,400.,500.]
 expected_pw=0.;moist=0.
 do k=1,5
  pw=rho(k)/(1.+qv(k))
  expected_pw=expected_pw+qv(k)*pw*dz(k)
  moist=moist+qv(k)*rho(k)*dz(k)
 enddo
 pw=precipw_column(5,qv,rho,zz,dz)
#ifdef LEAN_PRECIPW_MOIST
 call require(transfer(pw,0)==transfer(moist,0),'precipw_lean')
#else
 call require(transfer(pw,0)==transfer(expected_pw,0),'precipw_formula')
 call require(pw/=moist,'precipw_formula distinguishes density')
#endif
 sn=0.;expected_pw=0.
 do k=1,3
  rain=real(k)*0.7;frozen=real(k)*0.2
  expected_pw=expected_pw+rain(1)*frozen(1)
  sn(1)=snow_total(sn(1),rain(1),frozen(1))
  call require(transfer(sn(1),0)==transfer(expected_pw,0),'acsnow_sum')
 enddo
 print '(a)', 'DIAG TESTS OK' 
contains
 subroutine fixture()
 set=blank_set;clock=blank_clock;msgs=blank_msgs
 ok=parse_time('2026-11-08_00:00:00',clock%start,msgs)
 call require(ok,'fixture time')
 clock%now=clock%start;clock%step=interval_from_dt(real(dt))
 set%n=3;allocate(set%s(3))
 set%s(1)%name='output';set%s(2)%name='diagnostics';set%s(3)%name='C'
 set%s(3)%package_mask=1
 do k=1,3
  set%s(k)%direction=STREAM_OUTPUT
  set%s(k)%has_output_interval=.true.
  set%s(k)%output_interval=interval_from_dt(real(10800/k**1))
  if(k==2)set%s(k)%output_interval=interval_from_dt(3600.)
  set%s(k)%output_alarm='out'//achar(48+k)
  call add_alarm(clock,set%s(k)%output_alarm,clock%start,msgs,set%s(k)%output_interval)
  call reset_alarm(clock,set%s(k)%output_alarm)
  set%s(k)%n_fields=2;allocate(set%s(k)%field(2))
 enddo
 set%s(1)%field(1)%field=field_index('cape')
 set%s(1)%field(2)%field=field_index('precipw')
 set%s(3)%field(1)%field=field_index('cape')
 set%s(3)%field(2)%field=field_index('precipw')
 set%s(2)%field(1)%field=field_index('w_velocity_max')
 set%s(2)%field(2)%field=field_index('relhum')
 end subroutine
 subroutine require(test,name)
 logical,intent(in)::test
 character(*),intent(in)::name
 if(.not.test)then
  print '(a)','FAIL '//name
  stop 1
 endif
 end subroutine
end program
