! The physics' calendar values from the run clock (the julian day as physics_timetracker computes it) and MPAS's
! microphysics log lines. The clock is the bundled ESMF time manager MPAS uses.
module physics_time
 use run_settings, only: dt, n_microp
 use lean_messages, only: message_list, add_message, real_text, int_text, LOG_OUT
 use ESMF
 use lean_clock, only: run_clock
 implicit none
 private
 public :: clock_fields, microphysics_messages
contains
 subroutine microphysics_messages(msgs)
 type(message_list),intent(inout)::msgs
 ! the microphysics lines of MPAS's log (physics/mpas_atmphys_manager.F:696-698)
 call add_message(msgs,LOG_OUT,'--- specifics on cloud microphysics option microp_scheme = mp_thompson')
 call add_message(msgs,LOG_OUT,'--- dt_microp = '//real_text(dt/real(n_microp)))
 call add_message(msgs,LOG_OUT,'--- n_microp  = '//int_text(n_microp))
 end subroutine

 subroutine clock_fields(clock,julian,stamp)
 type(run_clock),intent(in)::clock
 real,intent(out)::julian
 character(*),intent(out)::stamp
 type(ESMF_Time)::now
 integer::yr,month,day,h,m,s,s_n,s_d,doy,error
 real::utc_h
 now=clock%now
 call ESMF_TimeGet(now,YY=yr,MM=month,DD=day,H=h,M=m,S=s,Sn=s_n,Sd=s_d,rc=error)
 if(error/=0)error stop 'physics calendar fields'
 call ESMF_TimeGet(now,dayOfYear=doy,timeString=stamp,rc=error)
 if(error/=0)error stop 'physics date stamp'
 ! The arithmetic of physics_timetracker (mpas_atmphys_manager.F), including its integer fraction s_n/s_d. ESMF_TimeSet
 ! without Sn/Sd leaves s_d = 0, and the original's s_n/s_d is then an integer division by zero (SIGFPE on x86, 0 on arm64):
 ! here the fraction is 0, the arm64 value.
 if(s_d/=0)then
  utc_h = real(h) + real(m) / 60.0 + real(s + s_n / s_d) / 3600.0
 else
  utc_h = real(h) + real(m) / 60.0 + real(s + 0) / 3600.0
 end if
 julian = real(doy-1) + utc_h / 24.0
 end subroutine
end module
