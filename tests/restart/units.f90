! The restart unit cases of tests/restart/run.sh, one per call:  units [MODE]
!   (none)    the restart set's define and put walks on a small synthetic state agree in count; writes before.nc
!   restore   reads before.nc (each variable given its own bits by walk_modes.py) and writes after.nc; glacier packing
!   xtime_bits, switches_long, switches_only, missing_input, truncated_input, failed_diagnostics_write: the routines of
!             those names below (start times, the flavor and switches header, refusals of a bad file, a failed write)
program restart_units
 use iso_fortran_env, only: int64
 use model_start, only: start_info, start_from_times, start_from_restart, start_xtime, start_cold, start_lean
 use config, only: set_mesh_sizes, ncells, nc1, nz
 use lean_nc
 use lean_messages
 use lean_build_info, only: build_flavor, build_switches
 use lean_restart_file, only: restart_header, restart_check
 use lean_restart_layout, only: RESTART_LAYOUT
 use lean_writer, only: record_request, stream_file, field_buffer, begin_record, put_var, end_record, WRITE_OK, WRITE_FAILED
 use lean_field_catalog, only: var_index, CAT_REAL
 use lean_restart, only: restart_file, RESTART_DEFINE, RESTART_PUT, RESTART_GET
 use lean_restart_set, only: restart_walk, glacier_words, glacier_column
 use model_data, only: fc, cmp, s, emiss, ice, snow, albedo, allocate_radiation_cache
 use model_state, only: state_type, allocate_state
 use noahmp_state, only: noahmp_state_type, allocate_land
 use gf_state, only: gf_state_type, allocate_gf
 use gwdo_state, only: gwdo_state_type, allocate_gwdo
 use dyn_step, only: allocate_fields
 use dyn_micro, only: allocate_micro
 use sea_ice, only: allocate_ice
 use surface_bdy, only: allocate_surface_bdy
 use land_ice_exchange, only: allocate_glacier_of
 use lean_fields, only: sst, vegetation, smois_first
 use diag_manager, only: rainc, acsnow, relhum, refl10cm, refl10cm_max, refl10cm_1km, &
  refl10cm_1km_max, uh_max, w_max, wsp_max
 use phys_state, only: zero_state, zero_land, zero_gf, zero_gwdo
 use NoahmpIOVarType, only: NoahmpIO_type
 use model_startup_io, only: initialize
 implicit none
 type(state_type) :: state
 type(noahmp_state_type) :: lands
 type(gf_state_type) :: convection
 type(gwdo_state_type) :: drag
 type(restart_file) :: rf
 type(message_list) :: msgs
 type(NoahmpIO_type) :: input(2), restored(2)
 type(record_request) :: req
 type(stream_file) :: sf
 real, allocatable :: r(:), r2(:)
 integer, allocatable :: i(:), i2(:)
 character(len=32) :: mode
 integer :: status, count, nr, ni, j
 call get_command_argument(1,mode)
 if(mode=='xtime_bits')then
  call xtime_bits()
  stop
 endif
 if(mode=='switches_long'.or.mode=='switches_only')then
  call switches_header(mode=='switches_long')
  stop
 endif
 if(mode=='missing_input'.or.mode=='truncated_input')then
  call bad_input(mode=='truncated_input')
  stop
 endif
 if(mode=='failed_diagnostics_write')then
  call failed_diagnostics_write()
  stop
 endif
 call set_mesh_sizes(2,4,3,3)
 call allocate_state(state)
 call allocate_land(lands)
 call allocate_gf(convection)
 call allocate_gwdo(drag)
 call allocate_fields(fc)
 call allocate_micro(cmp)
 call allocate_radiation_cache()
 call allocate_surface_bdy()
 call allocate_ice()
 call allocate_glacier_of()
 allocate(s%rho_base(nz,nc1),s%exner_base(nz,nc1),s%rtheta_base(nz,nc1),s%pressure_base(nz,nc1))
 allocate(emiss(ncells),ice(ncells),snow(ncells),albedo(ncells),sst(ncells),vegetation(ncells),smois_first(ncells))
 allocate(rainc(ncells),acsnow(ncells),relhum(nz,ncells),refl10cm(nz,ncells),refl10cm_max(ncells),refl10cm_1km(ncells))
 allocate(refl10cm_1km_max(ncells),uh_max(ncells),w_max(ncells),wsp_max(ncells))
 call zero_state(state)
 call zero_land(lands)
 call zero_gf(convection)
 call zero_gwdo(drag)
 if (mode=='restore') then
  status=nc_open(rf%nc,'before.nc')
  call check(status==NC_NOERR,'open for GET')
  rf%mode=RESTART_GET
  rf%rec%record=1
  call restart_walk(rf,state,lands,convection,drag)
  call check(.not.rf%failed,'GET '//nc_message())
  count=rf%count
  status=nc_close(rf%nc)
  call check(status==NC_NOERR,'close after GET')
  req%path='after.nc'
 else
  count=0
  req%path='before.nc'
 endif
 req%stream='restart'
 req%format=NC_FORMAT_64BIT_DATA
 req%clobber=3
 status=begin_record(sf,req,[integer ::],[character(len=32) ::],[integer ::], &
                     [type_dummy()],rf%rec,msgs)
 call check(status==WRITE_OK,'begin')
 rf%mode=RESTART_DEFINE
 rf%count=0
 call restart_walk(rf,state,lands,convection,drag)
 call check(.not.rf%failed,'DEFINE '//nc_message())
 if(count>0)call check(rf%count==count,'same count GET and DEFINE')
 count=rf%count
 rf%mode=RESTART_PUT
 rf%count=0
 call restart_walk(rf,state,lands,convection,drag)
 call check(.not.rf%failed,'PUT '//nc_message())
 call check(rf%count==count.and.count>700,'same and nonempty count DEFINE and PUT')
 status=end_record(sf,rf%rec,msgs)
 call check(status==WRITE_OK,'end')
 print '(a,i0)', 'walk variables ',count
 if(mode=='restore')then
  do j=1,2
   call initialize(input(j))
   call initialize(restored(j))
  enddo
  call glacier_words(input(1),nr,ni)
  call check(nr>500.and.ni>=12,'glacier word counts')
  allocate(r(nr),r2(nr),i(ni),i2(ni))
  do j=1,2
   r=[(real(status+j)/128.,status=1,nr)]
   i=[(status+3*j,status=1,ni)]
   call glacier_column(input(j),r,i,.true.)
   r2=0.; i2=0
   call glacier_column(input(j),r2,i2,.false.)
   call glacier_column(restored(j),r2,i2,.true.)
   r2=0.; i2=0
   call glacier_column(restored(j),r2,i2,.false.)
   call check(all(transfer(r,[0],nr)==transfer(r2,[0],nr)).and.all(i==i2),'glacier_pack')
  enddo
  print '(a)', 'ok   glacier_pack'
 endif
contains
 subroutine bad_input(truncated)
 logical,intent(in) :: truncated
 type(restart_header) :: header
 character(len=*),parameter :: path='bad-restart.nc'
 character(len=100) :: expected
 integer :: unit
 if(truncated)then
  open(newunit=unit,file=path,status='new',access='stream',form='unformatted')
  write(unit)'CDF'//achar(5)
  close(unit)
  expected=path//' is not a readable NetCDF file.'
 else
  expected="Could not open input file '"//path//"' to read mesh fields"
 endif
 call restart_check(path,'',header,msgs)
 call check(msgs%n>0,'restart open refusal')
 call check(msgs%line(1)%text==expected,trim(mode)//' explanation')
 call check(msgs%line(1)%level==LOG_ERR,trim(mode)//' is ERR')
 call check(msgs%n==2.and.has_fatal(msgs),trim(mode)//' has a closing CRIT')
 call check(msgs%line(2)%level==LOG_CRIT.and. &
  msgs%line(2)%text=='lean_mpas stops: 1 input problem(s) listed above',trim(mode)//' closing text')
 if(truncated)then
  open(newunit=unit,file=path,status='old')
  close(unit,status='delete')
 endif
 print '(a)', 'ok   '//trim(mode)
 end subroutine

 subroutine failed_diagnostics_write()
 type(field_buffer) :: buffer
 integer :: time_var,code
 logical :: exists
 req%path='diagnostics.nc'
 req%stream='diagnostics'
 req%format=NC_FORMAT_64BIT_DATA
 req%time_units='seconds since 2026-11-08 00:00:00'
 time_var=var_index('Time')
 code=begin_record(sf,req,[time_var],[character(len=32) ::],[integer ::],[type_dummy()],rf%rec,msgs)
 call check(code==WRITE_OK,'begin diagnostics')
 buffer%kind=CAT_REAL
 buffer%n=1
 buffer%r=[3600.]
 code=put_var(rf%rec,time_var,buffer,req%precision)
 call check(code==WRITE_OK,'put diagnostics')
 code=end_record(sf,rf%rec,msgs)
 call check(code==WRITE_FAILED.and.has_fatal(msgs),'injected diagnostics write failure succeeded')
 inquire(file='diagnostics.nc.lean_partial',exist=exists)
 call check(.not.exists,'failed diagnostics left a partial file')
 inquire(file='diagnostics.nc',exist=exists)
 call check(.not.exists,'failed diagnostics published a final file')
 print '(a)', 'ok   failed_diagnostics_write'
 end subroutine

 subroutine switches_header(long)
 logical,intent(in) :: long
 type(nc_file) :: file
 type(restart_header) :: header
 character(len=:),allocatable :: switches
 character(len=*),parameter :: path='switches.nc'
 integer :: code
 if(long)then
  switches=repeat('LEAN_TEST ',199)//'ABCDEFGHIJ'
 else
  ! The spacing puts the extra switch past character 512, so a reader with a 512-character buffer would lose it.
  switches=build_switches//repeat(' ',max(1,512-len(build_switches)))//'LEAN_TEST_SWITCH'
 endif
 code=nc_create(file,path,NC_FORMAT_64BIT_DATA,.true.)
 call check(code==NC_NOERR,'create header fixture')
 code=nc_put_att(file,'','lean_restart',RESTART_LAYOUT)
 call check(code==NC_NOERR,'write layout')
 code=nc_put_att(file,'','lean_flavor',build_flavor)
 call check(code==NC_NOERR,'write flavor')
 code=nc_put_att(file,'','lean_switches',switches)
 call check(code==NC_NOERR,'write switches')
 code=nc_close(file)
 call check(code==NC_NOERR,'close header fixture')
 call restart_check(path,'2026-11-08_06:00:00',header,msgs)
 if(long)call check(len_trim(header%switches)==2000.and.header%switches==switches,'2000-character switches round trip')
 call check(msgs%n==1.and.has_fatal(msgs),'one critical restart refusal')
 call check(msgs%line(1)%text==path//" was written by lean_mpas flavor '"//build_flavor// &
  "' with switches '"//switches//"'; this one is flavor '"//build_flavor//"' with switches '"//build_switches// &
  "'. A restart continues the same physics: rebuild with the flavor and switches that wrote it.",'refusal: the same flavor, other switches')
 print '(a)', 'ok   '//trim(mode)
 end subroutine

 subroutine xtime_bits()
 type(start_info) :: a,b
 integer :: h,m,ic,ir,step,idt
 integer(int64) :: s0,s
 real :: dt, old
 character(len=19) :: cold, stamp
 do ic=1,2
  h=0; m=0
  if(ic==2)then
   h=6; m=30
  endif
  write(cold,'(a,i2.2,a,i2.2,a)')'2026-11-08_',h,':',m,':00'
  call start_from_times(start_cold,2026,11,8,h,m,0,2026,11,8,0,0,0,a)
  do ir=1,2
   s0=21600_int64+(ir-1)*600_int64
   write(stamp,'(a,i2.2,a,i2.2,a)')'2026-11-08_',h+6,':',m+(ir-1)*10,':00'
   call start_from_restart(cold,'2026-11-08_00:00:00',stamp,b)
   call check(b%kind==start_lean.and.b%cold_offset_s==s0,'restart descriptor')
   do idt=1,3
    dt=120.
    if(idt==2)dt=18.
    if(idt==3)dt=2.5
    do step=0,int(43200./dt)
     s=int(real(step)*dt,int64)
     old=a%xtime_offset_min+real(s)/60.
     call check(transfer(start_xtime(a,s),0)==transfer(old,0),'cold xtime bits')
     if(s+s0>43200)cycle
     call check(transfer(start_xtime(b,s),0)==transfer(start_xtime(a,s+s0),0),'split xtime bits')
    enddo
   enddo
  enddo
 enddo
 call start_from_restart('2026-11-08_20:00:00','2026-11-08_20:00:00','2026-11-09_02:00:00',b)
 call check(b%julday==312.and.b%day==9.and.b%cold_offset_s==21600_int64,'restart across midnight')
 print '(a)', 'ok   xtime_bits'
 end subroutine
 function type_dummy() result(a)
 use lean_writer, only: config_attribute
 use lean_field_catalog, only: CAT_TEXT
 type(config_attribute) :: a
 a%name='test'; a%kind=CAT_TEXT; a%text='walk_modes'
 end function
 subroutine check(ok,message)
 logical,intent(in) :: ok
 character(*),intent(in) :: message
 if(ok)return
 print '(a)', 'FAIL: '//message
 stop 1
 end subroutine
end program
