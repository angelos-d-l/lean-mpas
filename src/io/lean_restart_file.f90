! A lean restart file's header, checked before anything is read from it (layout, flavor and switches, the record of
! the start time), and MPAS's restart timestamp file, which the caller writes only after the restart record closes.
module lean_restart_file
 use lean_namelist_options, only: run_config
 use lean_messages
 use lean_nc
 use lean_build_info, only: build_flavor, build_switches
 use lean_restart_layout, only: RESTART_LAYOUT
 use mpas_kind_types, only: StrKIND
 implicit none
 integer, parameter :: SWITCHES_MAX=8192
 private
 public :: restart_header, restart_timestamp_write, restart_check
 type restart_header
  character(len=19) :: xtime='', initial_time='', cold_start=''
  real :: time_s=0.
  integer :: steps=0, record=1
  character(len=16) :: layout=''
  character(len=StrKIND) :: flavor='', version=''
  ! Fixed and long: nvfortran 25.3 loses a deferred-length component filled by move_alloc.
  character(len=SWITCHES_MAX) :: switches=''
 end type
contains
 ! Called twice. With start = '' (before the mesh is read) only the restart layout is checked; with the start time
 ! (before the state is restored), also the flavor and switches, and the record whose xtime is start.
 subroutine restart_check(path,start,header,msgs)
 character(*), intent(in) :: path,start
 type(restart_header), intent(out) :: header
 type(message_list), intent(inout) :: msgs
 type(nc_file) :: f
 character(len=:), allocatable :: text
 character(len=64) :: stamp
 integer :: status,records,j,timestep,closed
 logical :: has_flavor,matched,exists,too_long

 if(nc_open(f,path)/=NC_NOERR)then
  inquire(file=path,exist=exists)
  if(exists)then
   call add_message(msgs,LOG_ERR,trim(path)//' is not a readable NetCDF file.')
  else
   call add_message(msgs,LOG_ERR,"Could not open input file '"//trim(path)//"' to read mesh fields")
  endif
  call add_message(msgs,LOG_CRIT,'lean_mpas stops: 1 input problem(s) listed above')
  return
 endif
 has_flavor=nc_get_att(f,'','lean_flavor',text)==NC_NOERR
 if(has_flavor)header%flavor=text
 status=nc_get_att(f,'','lean_restart',text)
 if(status/=NC_NOERR)then
  if(has_flavor)then
   call add_message(msgs,LOG_CRIT,trim(path)//' is not a lean_mpas restart file (it has no global attribute lean_restart).')
  else
   call add_message(msgs,LOG_CRIT,trim(path)//' was written by MPAS, not by lean_mpas: '// &
    "lean_mpas resumes only from its own restart files. Start this run from the 'input' stream (config_do_restart = .false.).")
  endif
  goto 900
 endif
 header%layout=text
 if(text/=RESTART_LAYOUT)then
  call add_message(msgs,LOG_CRIT,trim(path)//' was written with restart layout '//text// &
   '; this lean_mpas has layout '//RESTART_LAYOUT//". Resume with the lean_mpas that wrote it, or start from the 'input' stream.")
  goto 900
 endif
 if(len_trim(start)==0)goto 900
 status=nc_get_att(f,'','lean_switches',text)
 too_long=.false.
 if(status==NC_NOERR)then
  too_long=len(text)>SWITCHES_MAX
  header%switches=text
 endif
 if(.not.has_flavor.or.status/=NC_NOERR.or.too_long.or.len(build_switches)>SWITCHES_MAX.or. &
    header%flavor/=build_flavor.or.header%switches/=build_switches)then
  call add_message(msgs,LOG_CRIT,trim(path)//" was written by lean_mpas flavor '"//trim(header%flavor)// &
   "' with switches '"//trim(header%switches)//"'; this one is flavor '"//build_flavor// &
   "' with switches '"//build_switches//"'. A restart continues the same physics: rebuild with the flavor and switches that wrote it.")
  goto 900
 endif
 status=nc_get_att(f,'','lean_version',text)
 if(status/=NC_NOERR)goto 800
 header%version=text
 status=nc_inq_dim(f,'Time',records)
 if(status/=NC_NOERR)goto 800
 matched=.false.
 do j=1,records
  status=nc_get(f,'xtime',stamp,record=j)
  if(status/=NC_NOERR)goto 800
  if(j==1)header%xtime=stamp
  if(stamp/=start)cycle
  header%record=j
  header%xtime=stamp
  matched=.true.
  exit
 enddo
 if(.not.matched)then
  call add_message(msgs,LOG_CRIT,trim(path)//' holds the state of '//trim(header%xtime)//', not of the start time '//trim(start)//'.')
  goto 900
 endif
 status=nc_get(f,'initial_time',stamp,record=header%record)
 if(status/=NC_NOERR)goto 800
 header%initial_time=stamp
 status=nc_get(f,'lean_cold_start',stamp,record=header%record)
 if(status/=NC_NOERR)goto 800
 header%cold_start=stamp
 status=nc_get(f,'Time',header%time_s,record=header%record)
 if(status/=NC_NOERR)goto 800
 status=nc_get_att(f,'','lean_restart_steps',header%steps)
 if(status/=NC_NOERR)goto 800
 if(records>1)then
  ! A stream can put several restart times in one file; the attribute lean_restart_steps holds the steps of
  ! the first record. For the selected record, use its own lean_phys_timestep (the restart walk's copy of
  ! state%timestep, the number of the next physics step, so the steps done are timestep - 1).
  status=nc_get(f,'lean_phys_timestep',timestep,record=header%record)
  if(status/=NC_NOERR)goto 800
  header%steps=timestep-1
 endif
 goto 900
800 call add_message(msgs,LOG_CRIT,trim(path)//': '//nc_message())
900 closed=nc_close(f)
 end subroutine

 subroutine restart_timestamp_write(cfg,now,msgs)
 type(run_config), intent(in) :: cfg
 character(*), intent(in) :: now
 type(message_list), intent(inout) :: msgs
 integer :: unit,status,closed
 character(len=StrKIND) :: why
 open(newunit=unit,file=trim(cfg%config_restart_timestamp_name),status='replace',action='write',iostat=status,iomsg=why)
 if(status==0) then
  ! MPAS core_atmosphere/mpas_atm_core.F:898-899.
  write(unit,*,iostat=status,iomsg=why) trim(now)
  if(status==0) then
   close(unit,iostat=status,iomsg=why)
  else
   close(unit,iostat=closed)
  endif
 endif
 if(status/=0) call add_message(msgs,LOG_CRIT, &
  'Cannot write the restart timestamp file '//trim(cfg%config_restart_timestamp_name)//': '//trim(why))
 end subroutine
end module
