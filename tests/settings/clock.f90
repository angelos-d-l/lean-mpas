! The clock units of tests/settings/run.sh on the strict cpu build's objects:  clock [CASE] (default: every case).
! Alarm steps, alarms within and before a step, fractional time steps, a start at 06:30, derived step counts, the
! checkpoint arithmetic and the Noah-MP options.
program settings_clock
 use iso_fortran_env, only: int64
 use lean_clock
 use run_settings, only: set_time_step, dt_dynamics, rk_timestep, rk_scalar_timestep, rk_sub_timestep
 use lean_namelist, only: run_config, validate
 use lean_messages
 use model_start
 use physics_time, only: clock_fields
 implicit none
 type(run_config) :: cfg
 type(run_clock) :: clock
 type(message_list) :: msgs
 type(start_info) :: st
 type(ESMF_Time) :: finish
 type(ESMF_TimeInterval) :: iv
 integer :: d, r, it, seconds, n, dt, intervals(2), steps(6)
 logical :: ok, expected, ringing
 real :: julian, gmt, xtime
 character(64) :: stamp
 character(32) :: case_name
 call get_command_argument(1,case_name)
 if(case_name=="".or.case_name=="alarm_steps")then
 steps = [3,6,18,30,120,600]
 intervals = [900,1800]
 do d=1,size(steps)
  dt=steps(d)
  do r=1,size(intervals)
   clock=run_clock()
   ok=parse_time('2026-10-11_06:00:00',clock%start,msgs)
   clock%now=clock%start
   clock%step=interval_from_dt(real(dt))
   iv=interval_from_dt(real(intervals(r)))
   call add_alarm(clock,'radtlw',clock%start,msgs,iv)
   iv=interval_from_dt(86400.)
   call add_alarm(clock,'green',clock%start,msgs,iv)
   do it=1,5*86400/dt
    seconds=(it-1)*dt
    ringing=alarm_ringing(clock,'radtlw')
    if(mod(intervals(r),dt)==0)then
     expected=mod(seconds,intervals(r))==0
    else
     expected=seconds==0
     if(seconds>0)expected=seconds/intervals(r)>(seconds-dt)/intervals(r)
    endif
    if(ringing.neqv.expected)print '(a,4i8,2l3)', 'alarm mismatch: dt/interval/step/elapsed, clock/mod ', &
     dt,intervals(r),it,seconds,ringing,expected
    call require(ringing.eqv.expected,'alarm_steps radiation')
    if(ringing)call reset_alarm(clock,'radtlw')
    ringing=alarm_ringing(clock,'green')
    call require(ringing.eqv.(mod(seconds,86400)==0),'alarm_steps green')
    if(ringing)call reset_alarm(clock,'green')
    call require(elapsed_s(clock)==seconds,'alarm_steps elapsed')
    call advance_run_clock(clock)
    do n=600,43200,600
     if(n/=600.and.n/=3600.and.n/=43200)cycle
     call require((elapsed_s(clock)/n > int(seconds,int64)/n).eqv. &
                  ((it*dt)/n > seconds/n),'alarm_steps output marks')
    enddo
   enddo
  enddo
 enddo
 print '(a)', 'ALARM STEPS OK'
 endif
 if(case_name=="".or.case_name=="alarm_within")then
 do d=1,2
  dt=120
  if(d==2)dt=7
  clock=run_clock()
  ok=parse_time('2026-11-08_00:00:00',clock%start,msgs)
  call require(ok,'alarm_within parse')
  clock%now=clock%start
  clock%step=interval_from_dt(real(dt))
  call add_alarm(clock,'within',clock%start,msgs,interval_from_dt(3600.))
  call reset_alarm(clock,'within')
  n=0
  do it=1,7200/dt+1
   seconds=(it-1)*dt
   expected=(seconds+dt)/3600>seconds/3600
   if(alarm_ringing(clock,'within'))call reset_alarm(clock,'within')
   ringing=alarm_ringing(clock,'within',within=clock%step)
   call require(ringing.eqv.expected,'alarm_within crossing')
   if(ringing)n=n+1
   call advance_run_clock(clock)
  enddo
  call require(n==2,'alarm_within count')
 enddo
 print '(a)', 'ALARM WITHIN OK'
 endif
 if(case_name=="".or.case_name=="alarm_previous")then
 clock=run_clock()
 ok=parse_time('2026-11-08_06:10:00',clock%start,msgs)
 call require(ok,'alarm_previous start')
 ok=parse_time('2026-11-08_06:00:00',finish,msgs)
 call require(ok,'alarm_previous time')
 clock%now=clock%start
 clock%step=interval_from_dt(120.)
 call add_alarm(clock,'radtlw',clock%start,msgs,interval_from_dt(1800.))
 call set_alarm_previous(clock,'radtlw',finish)
 call require(alarm_previous(clock,'radtlw')==finish,'alarm_previous value')
 do it=0,10
  call require(alarm_ringing(clock,'radtlw').eqv.(it==10),'alarm_previous phase')
  call advance_run_clock(clock)
 enddo
 call set_alarm_previous(clock,'missing',finish)
 call require(alarm_previous(clock,'missing')==clock%start,'alarm_previous missing')
 print '(a)', 'ALARM PREVIOUS OK'
 endif
 if(case_name=="".or.case_name=="fractional")then
 clock=run_clock()
 ok=parse_time('2026-11-08_00:00:00',clock%start,msgs)
 clock%now=clock%start
 clock%step=interval_from_dt(2.5)
 finish=clock%start+interval_from_dt(10800.)
 call add_alarm(clock,'rad',clock%start,msgs,interval_from_dt(900.))
 n=0
 do it=1,721
  if(it<=5)call require(elapsed_s(clock)==int(2.5*real(it-1),int64),'fractional elapsed')
  if(it==2)call require(seconds_until(clock,finish)==10797.5,'fractional seconds_until')
  if(alarm_ringing(clock,'rad'))then
   call require(it==1.or.it==361.or.it==721,'fractional alarm step')
   n=n+1
   call reset_alarm(clock,'rad')
  endif
  call advance_run_clock(clock)
 enddo
 call require(n==3,'fractional alarm count')
 print '(a)', 'FRACTIONAL OK'
 endif
 if(case_name=="".or.case_name=="start_0630")then
 clock=run_clock()
 ok=parse_time('2026-11-08_00:00:00',clock%start,msgs)
 call start_from_times(start_cold,2026,11,8,6,30,0,2026,11,8,6,30,0,st)
 call require(st%lbc_origin_sec==23400.and.st%julday==312,'start_0630 descriptor')
 clock%start=clock%start+interval_from_dt(23400.)
 clock%now=clock%start
 call clock_fields(clock,julian,stamp)
 call require(abs((julian-311.)*24.-6.5)<0.001,'start_0630 physics time')
 gmt=real(st%init_hour)+real(st%init_minute)/60.+real(st%init_second)/3600.
 xtime=st%xtime_offset_min+real(elapsed_s(clock))/60.
 call require(gmt==6.5,'start_0630 gmt')
 ! The first step starts at 06:30, with zero elapsed physics minutes.
 call require(xtime==0.,'start_0630 xtime')
 print '(a)', 'START 0630 OK'
 endif
 if(case_name=="".or.case_name=="derived_steps")then
 call set_time_step(18.)
 call require(transfer(dt_dynamics,0)==transfer(6.,0),'derived dt_dynamics')
 call require(all(transfer(rk_timestep,[0,0,0])==transfer([3.,3.,6.],[0,0,0])),'derived rk_timestep')
 call require(all(transfer(rk_scalar_timestep,[0,0,0])==transfer([9.,9.,18.],[0,0,0])),'derived rk_scalar_timestep')
 call require(all(transfer(rk_sub_timestep,[0,0,0])==transfer([1.5,1.5,1.5],[0,0,0])),'derived rk_sub_timestep')
 print '(a)', 'DERIVED STEPS OK'
 endif
 if(case_name=="".or.case_name=="checkpoint")then
 clock=run_clock()
 ok=parse_time('2026-11-08_00:00:00',clock%start,msgs)
 clock%step=interval_from_dt(7.)
 clock%now=clock%start+interval_from_dt(43197.)
 seconds=int(elapsed_s(clock))
 call advance_run_clock(clock)
 call require(elapsed_s(clock)==43204,'checkpoint first step after mark')
 call require(elapsed_s(clock)>=43200.and.seconds<43200,'checkpoint crosses mark')
 seconds=int(elapsed_s(clock))
 call advance_run_clock(clock)
 call require(.not.(elapsed_s(clock)>=43200.and.seconds<43200),'checkpoint once')
 print '(a)', 'CHECKPOINT OK: dt 7, elapsed 43204'
 endif
 if(case_name=="".or.case_name=="noahmp_options")then
 do n=3,4
  msgs=message_list()
  cfg%config_noahmp_iopt_sfc=n
  call validate(cfg,'mpas',msgs)
  ok=.false.
  do it=1,msgs%n
   if(index(msgs%line(it)%text,'MPAS options 3 and 4 are not supported')>0)ok=.true.
  enddo
  call require(ok,'Noah-MP unsupported options named')
 enddo
 print '(a)', 'NOAHMP OPTIONS OK'
 endif
 print '(a)', 'SETTINGS TESTS OK'
contains
 subroutine require(ok,name)
 logical,intent(in)::ok
 character(*),intent(in)::name
 if(.not.ok)then
  print '(a)', 'FAIL '//name
  stop 1
 endif
 end subroutine
end program
