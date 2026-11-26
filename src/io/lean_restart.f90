! Restart write and read on the host. One generated walk (lean_restart_set) defines, writes and reads every variable
! a lean restart carries, so the three cannot drift apart.
module lean_restart
 use iso_fortran_env, only: int64
 use lean_messages
 use lean_nc
 use lean_writer
 use lean_restart_layout, only: RESTART_LAYOUT
 use lean_restart_set, only: restart_walk, restart_mesh_vars, glacier_words
 use lean_run_inputs, only: run_inputs
 use lean_fields, only: field_class, SOURCE_HELD, globals, sst, xice, vegetation, smois_first
 use model_data, only: ice
 use config, only: ncells
 use land_ice, only: n_glacier, glacier
 use land_ice_exchange, only: allocate_exchange
 use sea_ice, only: xice_threshold
 use NoahmpIOVarType, only: NoahmpIO_type
 use lean_clock, only: run_clock, time_string, alarm_index, alarm_previous, set_alarm_previous, parse_time, ESMF_Time
 use lean_streams, only: stream_set, write_filename
 use lean_field_catalog, only: VAR_NAME, CAT_TEXT, CAT_INTEGER
 use model_state, only: state_type
 use noahmp_state, only: noahmp_state_type
 use gf_state, only: gf_state_type
 use gwdo_state, only: gwdo_state_type
 implicit none
 private
 public :: restart_file, restart_var, RESTART_DEFINE, RESTART_PUT, RESTART_GET, restart_write
 public :: restart_prepare, restart_read, restart_alarms
 type(stream_file), save :: restart_stream
 character(len=64), save :: restored_alarms(5)=''
 character(len=6), parameter :: alarms(5)=[character(len=6) :: 'radtlw','radtsw','green','diag','sfcbdy']
contains
 subroutine restart_write(set,i,clock,state,lands,convection,drag,run,written,msgs)
 type(stream_set), intent(in) :: set
 integer, intent(in) :: i
 type(run_clock), intent(in) :: clock
 type(state_type), intent(inout) :: state
 type(noahmp_state_type), intent(inout) :: lands
 type(gf_state_type), intent(inout) :: convection
 type(gwdo_state_type), intent(inout) :: drag
 type(run_inputs), intent(in) :: run
 logical, intent(out) :: written
 type(message_list), intent(inout) :: msgs
 type(restart_file) :: rf
 type(record_request) :: req
 type(config_attribute) :: attrs(size(globals)+2)
 character(len=:), allocatable :: path
 integer :: j,k,status
 integer(int64) :: first,last,rate

 written=.false.
 if(.not.write_filename(set,i,clock%now,path,msgs))return
 call system_clock(first,rate)
 req%stream='restart'
 req%path=path
 req%xtime=time_string(clock%now)
 req%time_units='seconds since '//run%cold%initial_time
 req%mesh_path=run%cold%mesh_path
 req%clobber=set%s(i)%clobber
 req%format=nc_io_type_format(trim(set%s(i)%io_type))
 req%precision=set%s(i)%precision
 if(req%precision==0)req%precision=4
 k=size(globals)
 attrs(:k)=globals
 attrs(k+1)%name='lean_restart'
 attrs(k+1)%kind=CAT_TEXT
 attrs(k+1)%text=RESTART_LAYOUT
 attrs(k+2)%name='lean_restart_steps'
 attrs(k+2)%kind=CAT_INTEGER
 ! state%timestep is the next step's number: the steps done are one fewer
 attrs(k+2)%i=state%timestep-1

 status=begin_record(restart_stream,req,[integer ::],[character(len=32) ::],[integer ::],attrs,rf%rec,msgs)
 if(status/=WRITE_OK)return

 rf%xtime=req%xtime
 rf%initial_time=run%cold%initial_time
 rf%cold_start=time_string(clock%start)
 if(len_trim(run%restart%cold_start)>0)rf%cold_start=run%restart%cold_start
 rf%precision=req%precision
 rf%time_units=req%time_units
 rf%mesh_path=req%mesh_path
 do j=1,size(alarms)
  k=alarm_index(clock,alarms(j))
  if(k>0)rf%alarms(j)=time_string(alarm_previous(clock,alarms(j)))
 enddo
 if(rf%rec%new_file)then
  rf%mode=RESTART_DEFINE
  call restart_walk(rf,state,lands,convection,drag)
  ! the copied inputs are defined here and written after the define mode ends (the copy_inputs below)
  if(.not.rf%failed)call copy_inputs(rf,run)
  if(.not.rf%failed)rf%failed=nc_end_define(rf%rec%f)/=NC_NOERR
 endif
 if(.not.rf%failed)call copy_inputs(rf,run)
 if(.not.rf%failed)then
  rf%mode=RESTART_PUT
  call restart_walk(rf,state,lands,convection,drag)
 endif
 rf%rec%failed=rf%failed
 if(rf%failed)call add_message(msgs,LOG_ERR,trim(path)//': '//trim(rf%failed_var)//' '//nc_message())
 status=end_record(restart_stream,rf%rec,msgs)
 written=status==WRITE_OK
 if(written)then
  call system_clock(last)
  call add_message(msgs,LOG_OUT," Timing for write of 'restart' stream: "//real_text(real(last-first)/real(rate))//' s')
 endif
 end subroutine

 ! Before the forecast state is allocated: checks that the restart's glacier columns hold the numbers of real and
 ! integer words this build packs them into, then allocates the glacier columns, each a copy of io (the prototype that
 ! holds the active level count, the scheme options and the Noah-MP table scalars), and the land-ice exchange.
 subroutine restart_prepare(run,io,msgs)
 type(run_inputs), intent(in) :: run
 type(NoahmpIO_type), intent(in) :: io
 type(message_list), intent(inout) :: msgs
 type(nc_file) :: file
 character(len=:), allocatable :: path
 integer :: n,k,nr,ni,status,j

 path=run%cold%init_path
 if(nc_open(file,path)/=NC_NOERR)then
  call add_message(msgs,LOG_CRIT,nc_message())
  return
 endif
 status=nc_inq_dim(file,'lean_nGlacierCells',n_glacier)
 if(status==NC_EBADDIM)n_glacier=0
 if(n_glacier>0)then
  status=nc_inq_dim(file,'lean_nGlacierReal',n)
  status=nc_inq_dim(file,'lean_nGlacierInt',k)
  call glacier_words(io,nr,ni)
  if(n/=nr.or.k/=ni)then
   call add_message(msgs,LOG_CRIT,path//': its glacier columns hold '//int_text(n)//' real and '// &
    int_text(k)//' integer words; this lean_mpas makes columns of '//int_text(nr)//' and '//int_text(ni)//'.')
  endif
 endif
 status=nc_close(file)
 if(has_fatal(msgs))return
 if(n_glacier>0)then
  allocate(glacier(n_glacier))
  do j=1,n_glacier
   glacier(j)=io
   glacier(j)%xice_threshold=xice_threshold
  enddo
 endif
 call allocate_exchange()
 end subroutine

 subroutine restart_read(run,state,lands,convection,drag,msgs)
 type(run_inputs), intent(in) :: run
 type(state_type), intent(inout) :: state
 type(noahmp_state_type), intent(inout) :: lands
 type(gf_state_type), intent(inout) :: convection
 type(gwdo_state_type), intent(inout) :: drag
 type(message_list), intent(inout) :: msgs
 type(restart_file) :: rf
 integer :: status,j
 integer(int64) :: first,last,rate

 call system_clock(first,rate)
 rf%path=run%cold%init_path
 if(nc_open(rf%nc,rf%path)/=NC_NOERR)then
  call add_message(msgs,LOG_CRIT,nc_message())
  return
 endif
 allocate(sst(ncells),xice(ncells),vegetation(ncells),smois_first(ncells))
 rf%mode=RESTART_GET
 rf%rec%record=run%restart%record
 rf%xtime=run%restart%xtime
 call restart_walk(rf,state,lands,convection,drag)
 if(rf%failed)call add_message(msgs,LOG_CRIT,trim(rf%path)//': '//trim(rf%failed_var)//' '//nc_message())
 status=nc_close(rf%nc)
 if(has_fatal(msgs))return
 if(status/=NC_NOERR)then
  call add_message(msgs,LOG_CRIT,nc_message())
  return
 endif
 if(state%timestep-1/=run%restart%steps)then
  call add_message(msgs,LOG_CRIT,trim(rf%path)//': lean_restart_steps does not match the state step counter')
  return
 endif
 xice=ice
 restored_alarms=rf%alarms
 do j=1,rf%warnings%n
  call add_message(msgs,rf%warnings%line(j)%level,rf%warnings%line(j)%text)
 enddo
 call system_clock(last)
 call add_message(msgs,LOG_OUT," Timing for restore of 'restart' state: "//real_text(real(last-first)/real(rate))//' s')
 call add_message(msgs,LOG_OUT,'lean_mpas resumes from '//trim(rf%path)//': written by lean_mpas '// &
  trim(run%restart%version)//' (layout '//run%restart%layout//'), '//int_text(run%restart%steps)// &
  ' steps after '//run%restart%initial_time//'.')
 end subroutine

 subroutine restart_alarms(clock,msgs)
 type(run_clock), intent(inout) :: clock
 type(message_list), intent(inout) :: msgs
 type(ESMF_Time) :: previous
 integer :: j
 do j=1,size(alarms)
  if(alarms(j)=='diag')cycle ! diag_setup follows the current output stream's phase.
  if(restored_alarms(j)=='')cycle
  if(alarm_index(clock,alarms(j))==0)cycle
  if(parse_time(trim(restored_alarms(j)),previous,msgs))call set_alarm_previous(clock,alarms(j),previous)
 enddo
 end subroutine

 subroutine copy_inputs(rf,run)
 type(restart_file), intent(inout) :: rf
 type(run_inputs), intent(in) :: run
 integer :: j,status
 do j=1,size(restart_mesh_vars)
  ! The initial wind profiles are read from the input stream's file, even with a
  ! separate invariant file (read_state_inputs in lean_inputs.f90).
  if(restart_mesh_vars(j)=='u_init'.or.restart_mesh_vars(j)=='v_init')then
   status=copy_var(rf%rec,run%cold%init_path,trim(restart_mesh_vars(j)))
  else
   status=copy_var(rf%rec,run%cold%mesh_path,trim(restart_mesh_vars(j)))
  endif
  if(status/=NC_NOERR)then
   rf%failed_var=restart_mesh_vars(j)
   rf%failed=.true.
   return
  endif
 enddo
 do j=1,size(VAR_NAME)
  if(field_class(VAR_NAME(j))/=SOURCE_HELD)cycle
  status=copy_var(rf%rec,run%cold%init_path,trim(VAR_NAME(j)))
  if(status/=NC_NOERR)then
   rf%failed_var=VAR_NAME(j)
   rf%failed=.true.
   return
  endif
 enddo
 end subroutine
end module
