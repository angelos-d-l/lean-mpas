! Output stream records, written on the host: a new file is created under a .lean_partial name and renamed when its
! record closes; MPAS's clobber rules (framework/mpas_stream_manager.F:3150-3440); variables and attributes from the
! field catalog; and the typed define/put/get calls of the restart walk.
module lean_writer
 use iso_fortran_env, only: int64
 use iso_c_binding, only: c_int, c_char, c_null_char
 use mpas_kind_types, only: StrKIND
 use lean_inputs, only: mesh_attributes
 use lean_namelist_options, only: config_attribute, run_config, namelist_attributes
 use lean_build_info, only: build_flavor, build_switches, build_git
 use lean_nc
 use lean_field_catalog
 use lean_stream_registry, only: field_index, N_PACKAGES
 use lean_streams, only: stream_set, stream_fields, CLOBBER_NEVER_MODIFY, CLOBBER_APPEND, &
                         CLOBBER_TRUNCATE, CLOBBER_OVERWRITE
 use lean_messages
 implicit none
 private
 public :: global_attributes
 public :: copy_var
 public :: restart_file, restart_var, restart_static, RESTART_DEFINE, RESTART_PUT, RESTART_GET
 public :: field_buffer, stream_file, record_request, open_record, stream_vars, begin_record, put_var, end_record
 public :: WRITE_OK, WRITE_CLOBBER_FILE, WRITE_CLOBBER_RECORD, WRITE_FAILED, config_attribute
 public :: CLOBBER_NEVER_MODIFY, CLOBBER_APPEND, CLOBBER_TRUNCATE, CLOBBER_OVERWRITE
 integer, parameter :: WRITE_OK=0, WRITE_CLOBBER_FILE=1, WRITE_CLOBBER_RECORD=2, WRITE_FAILED=3
 character(len=*), parameter :: PARTIAL_SUFFIX='.lean_partial'
 ! netCDF's default fill values (netcdf.h NC_FILL_FLOAT, NC_FILL_DOUBLE, NC_FILL_INT)
 real, parameter :: FILL_FLOAT=9.9692099683868690e36
 double precision, parameter :: FILL_DOUBLE=9.9692099683868690d36
 integer, parameter :: FILL_INT=-2147483647
 type field_buffer
  integer :: kind=0, n=0
  real, allocatable :: r(:)
  double precision, allocatable :: d(:)
  integer, allocatable :: i(:)
  character(len=64) :: text=''
  ! fill: write netCDF's default fill values instead of the buffer; set only by the writer tests
  logical :: fill=.false.
 end type
 type stream_file
  character(len=StrKIND) :: file=''
  integer :: records=0
 end type
 type record_request
  character(len=StrKIND) :: stream='', path='', time_units='', mesh_path=''
  character(len=64) :: xtime=''
  integer :: clobber=0, format=0, precision=4
 end type
 type open_record
  type(nc_file) :: f
  integer :: record=0
  logical :: new_file=.false., failed=.false., partial_created=.false.
  character(len=StrKIND) :: path=''
 end type
 integer, parameter :: RESTART_DEFINE=1, RESTART_PUT=2, RESTART_GET=3
 ! restart_file and the restart_var routines live here, not in lean_restart, because the generated walk
 ! (lean_restart_set) uses them and lean_restart uses the walk.
 type restart_file
  integer :: mode=0
  type(nc_file) :: nc
  type(open_record) :: rec
  character(len=StrKIND) :: path=''
  logical :: failed=.false., optional_diag=.false.
  integer :: count=0, precision=4
  character(len=NC_MAX_NAME) :: failed_var=''
  character(len=64) :: xtime='', initial_time='', cold_start='', alarms(5)=''
  character(len=StrKIND) :: time_units='', mesh_path=''
  type(message_list) :: warnings
 end type
 ! restart_static: a variable without a Time dimension, as MPAS's restart holds the invariant stream (ivgtyp, isltyp)
 interface restart_static
  module procedure restart_static_int_1
 end interface
 interface restart_var
  module procedure restart_real_0, restart_real_1, restart_real_2, restart_real_3
  module procedure restart_int_0, restart_int_1, restart_int_2, restart_int_3
  module procedure restart_logical_0, restart_logical_1, restart_char
 end interface
 logical, save :: seeded=.false., die_read=.false.
 ! Test hook LEAN_WRITER_DIE=n (writer_die): the process kills itself during the n-th variable put, so the tests can
 ! check what an interrupted write leaves behind; with -n the record holding that put fails at its close instead.
 integer, save :: die_at=0, puts=0
 interface
  integer(c_int) function c_rename(old,new) bind(C,name='rename')
   import c_char,c_int
   character(c_char), intent(in) :: old(*),new(*)
  end function
  integer(c_int) function c_unlink(path) bind(C,name='unlink')
   import c_char,c_int
   character(c_char), intent(in) :: path(*)
  end function
  integer(c_int) function c_getpid() bind(C,name='getpid')
   import c_int
  end function
  integer(c_int) function c_kill(pid,sig) bind(C,name='kill')
   import c_int
   integer(c_int),value :: pid,sig
  end function
 end interface
contains
 subroutine seed_file_ids()
 integer :: n,j
 integer, allocatable :: seed(:)
 integer(int64) :: ticks,pid
 ! MPAS framework/random_id.c seeds from time(NULL); also mix in the process id so
 ! processes started in the same clock tick do not repeat the file/parent id chain.
 call system_clock(count=ticks)
 pid=int(c_getpid(),int64)
 call random_seed(size=n)
 allocate(seed(n))
 do j=1,n
  seed(j)=int(modulo(ieor(ticks,ishft(pid,16))+104729_int64*j,int(huge(1),int64)))
 enddo
 call random_seed(put=seed)
 end subroutine

 subroutine input_attribute(path,name,att,value)
 character(*),intent(in) :: path,name,att
 character(*),intent(inout) :: value
 type(nc_file) :: f
 character(len=:),allocatable :: text
 integer :: status
 status=nc_open(f,path)
 if(status/=NC_NOERR)return
 status=nc_get_att(f,name,att,text)
 if(status==NC_NOERR)value=text
 status=nc_close(f)
 end subroutine
 subroutine global_attributes(cfg,mesh_attrs,attrs)
 type(run_config),intent(in) :: cfg
 type(mesh_attributes),intent(in) :: mesh_attrs
 type(config_attribute),allocatable,intent(out) :: attrs(:)
 type(config_attribute),allocatable :: options(:)
 integer :: n
 ! MPAS driver/mpas_subdriver.F:513-556: globals precede options; file_id is added on create.
 call namelist_attributes(cfg,options)
 allocate(attrs(14+size(options)+3))
 attrs(1)%name='model_name'; attrs(1)%kind=CAT_TEXT; attrs(1)%text='mpas'
 attrs(2)%name='core_name'; attrs(2)%kind=CAT_TEXT; attrs(2)%text='atmosphere'
 attrs(3)%name='version'; attrs(3)%kind=CAT_TEXT; attrs(3)%text='8.4.2'
 attrs(4)%name='source'; attrs(4)%kind=CAT_TEXT; attrs(4)%text='MPAS'
 attrs(5)%name='Conventions'; attrs(5)%kind=CAT_TEXT; attrs(5)%text='MPAS'
 attrs(6)%name='git_version'; attrs(6)%kind=CAT_TEXT; attrs(6)%text=build_git
 attrs(7)%name='on_a_sphere'; attrs(7)%kind=CAT_TEXT
 attrs(7)%text=merge('YES','NO ',mesh_attrs%on_a_sphere)
 attrs(8)%name='sphere_radius'; attrs(8)%kind=CAT_REAL; attrs(8)%r=mesh_attrs%sphere_radius
 attrs(9)%name='is_periodic'; attrs(9)%kind=CAT_TEXT
 attrs(9)%text=merge('YES','NO ',mesh_attrs%is_periodic)
 attrs(10)%name='x_period'; attrs(10)%kind=CAT_REAL; attrs(10)%r=mesh_attrs%x_period
 attrs(11)%name='y_period'; attrs(11)%kind=CAT_REAL; attrs(11)%r=mesh_attrs%y_period
 attrs(12)%name='history'; attrs(12)%kind=CAT_TEXT; attrs(12)%text='mpirun -n 1 ./atmosphere_model'
 attrs(13)%name='parent_id'; attrs(13)%kind=CAT_TEXT
 if(allocated(mesh_attrs%parent_id)) attrs(13)%text=mesh_attrs%parent_id
 attrs(14)%name='mesh_spec'; attrs(14)%kind=CAT_TEXT
 if(allocated(mesh_attrs%mesh_spec)) attrs(14)%text=mesh_attrs%mesh_spec
 attrs(15:14+size(options))=options
 n=14+size(options)
 attrs(n+1)%name='lean_flavor'; attrs(n+1)%kind=CAT_TEXT; attrs(n+1)%text=build_flavor
 attrs(n+2)%name='lean_switches'; attrs(n+2)%kind=CAT_TEXT
 attrs(n+3)%name='lean_version'; attrs(n+3)%kind=CAT_TEXT; attrs(n+3)%text=build_git
 end subroutine
 subroutine stream_vars(set,i,names,vars)
 type(stream_set), intent(in) :: set
 integer,intent(in) :: i
 character(len=32),allocatable,intent(out) :: names(:)
 integer,allocatable,intent(out) :: vars(:)
 integer,allocatable :: found(:)
 integer :: j,k,f,n,mask,p
 call stream_fields(set,i,names)
 allocate(found(N_VARS))
 mask=0
 do p=1,N_PACKAGES
  if(set%package(p)) mask=ibset(mask,p-1)
 enddo
 n=0
 do j=1,size(names)
  f=field_index(names(j))
  if(f==0) cycle
  do k=FIELD_FIRST_VAR(f),FIELD_FIRST_VAR(f)+FIELD_VAR_COUNT(f)-1
   if(VAR_PACKAGES(k)/=0.and.iand(VAR_PACKAGES(k),mask)==0) cycle
   if(any(found(:n)==k)) cycle
   n=n+1; found(n)=k
  enddo
 enddo
 allocate(vars(n)); vars=found(:n)
 end subroutine

 integer function begin_record(sf,req,vars,dim_names,dim_lengths,globals,rec,msgs) result(status)
 type(stream_file),intent(inout) :: sf
 type(record_request),intent(in) :: req
 integer,intent(in) :: vars(:),dim_lengths(:)
 character(*),intent(in) :: dim_names(:)
 type(config_attribute),intent(in) :: globals(:)
 type(open_record),intent(out) :: rec
 type(message_list),intent(inout) :: msgs
 logical :: exists,current
 integer :: s,j,k,n,v,xtype,nrecords,match_record
 real :: random(10)
 character(len=32) :: dims(8)
 character(len=64) :: stamp
 character(len=StrKIND+len(PARTIAL_SUFFIX)) :: path
 character(len=4) :: number
 character(len=10) :: file_id
 character(len=*),parameter :: alphabet='0123456789abcdefghijklmnopqrstuvwxyz'
 status=WRITE_FAILED
 rec%path=req%path
 inquire(file=trim(req%path),exist=exists)
 current=sf%file==req%path.and.len_trim(sf%file)>0
 ! MPAS :3220-3225,3307-3312: a conflict is nonfatal and leaves the stream invalid.
 if(exists.and..not.current.and.req%clobber==CLOBBER_NEVER_MODIFY) then
  call add_message(msgs,LOG_ERR,"Writing to stream '"//trim(req%stream)//"' would clobber file '"//trim(req%path)//"',")
  call add_message(msgs,LOG_ERR,"    but clobber_mode is set to 'never_modify'.")
  sf=stream_file(); status=WRITE_CLOBBER_FILE; return
 endif
 rec%new_file=.not.exists.or.(.not.current.and.req%clobber==CLOBBER_TRUNCATE)
 rec%record=1
 path=req%path
 if(rec%new_file) then
  path=trim(req%path)//PARTIAL_SUFFIX
  s=nc_create(rec%f,trim(path),req%format,.true.)
  rec%partial_created=s==NC_NOERR
 else
  s=nc_open(rec%f,trim(path),.true.)
  if(s/=NC_NOERR) goto 900
  s=nc_inq_dim(rec%f,'Time',nrecords)
  if(s/=NC_NOERR) goto 900
  rec%record=nrecords+1
  if(req%clobber==CLOBBER_APPEND.or.req%clobber==CLOBBER_OVERWRITE) then
   match_record=0
   do j=1,nrecords
    s=nc_get(rec%f,'xtime',stamp,record=j)
    if(s/=NC_NOERR) goto 900
    if(stamp==req%xtime) then
     match_record=j; exit
    endif
   enddo
   if(match_record>0) then
    rec%record=match_record
    if(req%clobber==CLOBBER_APPEND) then
     write(number,'(i4)') match_record
     call add_message(msgs,LOG_ERR,"Writing to stream '"//trim(req%stream)//"' would overwrite record "// &
                      number//" in file '"//trim(req%path)//"',")
     call add_message(msgs,LOG_ERR,"    but clobber_mode is set to 'append'.")
     s=nc_close(rec%f)
     if(s/=NC_NOERR) goto 900
     status=WRITE_CLOBBER_RECORD; return
    endif
   endif
  endif
 endif
 if(s/=NC_NOERR) goto 900
 if(rec%new_file) then
  ! MPAS framework/mpas_io_streams.F:1699-1755: dimensions appear on first use.
  do j=1,size(vars)
   v=vars(j)
   call variable_dims(v,dims,n)
   xtype=NC_FLOAT
   if(req%precision==8) xtype=NC_DOUBLE
   if(VAR_TYPE(v)==CAT_INTEGER) xtype=NC_INT
   if(VAR_TYPE(v)==CAT_TEXT) xtype=NC_CHAR
   s=define_variable(rec,VAR_NAME(v),xtype,dims(:n),dim_names,dim_lengths)
   if(s/=NC_NOERR) goto 900
   s=catalog_attributes(rec,v,xtype,req%mesh_path,req%time_units)
   if(s/=NC_NOERR) goto 900
  enddo
  do j=1,size(globals)
   select case(globals(j)%kind)
   case(CAT_REAL)
    if(req%precision==8) then
     s=nc_put_att(rec%f,'',trim(globals(j)%name),globals(j)%r)
    else
     s=nc_put_att(rec%f,'',trim(globals(j)%name),real(globals(j)%r))
    endif
   case(CAT_INTEGER)
    s=nc_put_att(rec%f,'',trim(globals(j)%name),globals(j)%i)
   case(CAT_TEXT)
    if(globals(j)%name=='lean_switches')then
     ! The build's switch list can outgrow config_attribute's fixed text.
     s=nc_put_att(rec%f,'','lean_switches',build_switches)
    else
     s=nc_put_att(rec%f,'',trim(globals(j)%name),trim(globals(j)%text))
    endif
   case default
    s=LNC_ETYPE
   end select
   if(s/=NC_NOERR) goto 900
  enddo
  if(.not.seeded) then
   call seed_file_ids()
   seeded=.true.
  endif
  call random_number(random)
  do j=1,10
   k=1+int(36*random(j)); file_id(j:j)=alphabet(k:k)
  enddo
  s=nc_put_att(rec%f,'','file_id',file_id)
  if(s/=NC_NOERR) goto 900
 endif
 status=WRITE_OK
 return
900 continue
 call writer_failed(rec,msgs)
 end function

 integer function define_variable(rec,name,xtype,dims,dim_names,dim_lengths) result(s)
 type(open_record), intent(inout) :: rec
 character(*), intent(in) :: name,dims(:),dim_names(:)
 integer, intent(in) :: xtype,dim_lengths(:)
 integer :: a,d,k
 do k=1,size(dims)
  s=nc_inq_dim(rec%f,trim(dims(k)),d)
  if(s==NC_NOERR) cycle
  d=-1
  if(dims(k)=='Time') d=NC_UNLIMITED
  if(dims(k)=='StrLen') d=64
  do a=1,size(dim_names)
   if(dim_names(a)==dims(k).and.dims(k)/='Time'.and.dims(k)/='StrLen') d=dim_lengths(a)
  enddo
  if(d<0) then
   s=LNC_EARGS
   return
  endif
  s=nc_def_dim(rec%f,trim(dims(k)),d)
  if(s/=NC_NOERR) return
 enddo
 s=nc_def_var(rec%f,trim(name),xtype,dims)
 end function

 integer function catalog_attributes(rec,v,xtype,mesh_path,time_units) result(status)
 type(open_record), intent(inout) :: rec
 integer, intent(in) :: v,xtype
 character(*), intent(in) :: mesh_path,time_units
 character(len=StrKIND) :: units
 integer :: a,k,s,ios,ivalue
 real :: value
 s=NC_NOERR
   do a=VAR_ATT_FIRST(v),VAR_ATT_FIRST(v)+VAR_ATT_COUNT(v)-1
    if(ATT_NUMERIC(a)) then
     if(VAR_TYPE(v)==CAT_INTEGER) then
      units=ATT_TEXT(a)
      read(units,*,iostat=ios) ivalue
      if(ios/=0) then
       status=LNC_ETYPE; return
      endif
      s=nc_put_att(rec%f,trim(VAR_NAME(v)),trim(ATT_NAME(a)),ivalue)
     else
      units=ATT_TEXT(a)
      read(units,*,iostat=ios) value
      if(ios/=0) then
       status=LNC_ETYPE; return
      endif
      if(xtype==NC_DOUBLE) then
       s=nc_put_att(rec%f,trim(VAR_NAME(v)),trim(ATT_NAME(a)),real(value,8))
      else
       s=nc_put_att(rec%f,trim(VAR_NAME(v)),trim(ATT_NAME(a)),value)
      endif
     endif
    else
     units=ATT_TEXT(a)
     ! Input reads replace Registry attributes (mpas_io_streams.F:4344-4360). chvxy and chbxy keep the catalog's
     ! m s^{-1}: the input file carries MPAS's Registry units s m^{-1}, which are wrong for an exchange coefficient
     ! (physics_noahmp/src/EnergyVarType.F90:149).
     if(len_trim(mesh_path)>0.and.VAR_NAME(v)/='chvxy'.and.VAR_NAME(v)/='chbxy') &
      call input_attribute(trim(mesh_path),trim(VAR_NAME(v)),trim(ATT_NAME(a)),units)
     if(VAR_NAME(v)=='Time'.and.ATT_NAME(a)=='units') then
      units=time_units
      do k=1,len_trim(units)
       if(units(k:k)=='_') units(k:k)=' '
      enddo
     endif
     s=nc_put_att(rec%f,trim(VAR_NAME(v)),trim(ATT_NAME(a)),trim(units))
    endif
    if(s/=NC_NOERR) exit
   enddo
 status=s
 end function

 ! Copies variable name from source_path into the open record: in define mode its missing dimensions, the variable
 ! and its attributes; in data mode its values (a variable without Time only when the file is new). A variable the
 ! source does not hold is skipped (status NC_NOERR).
 integer function copy_var(rec,source_path,name) result(status)
 type(open_record), intent(inout) :: rec
 character(*), intent(in) :: source_path,name
 type(nc_file) :: source
 character(len=NC_MAX_NAME), allocatable :: dims(:)
 integer, allocatable :: lengths(:)
 integer :: s,xtype,axis,record
 logical :: unlimited

 status=nc_open(source,source_path)
 if(status/=NC_NOERR) return
 status=nc_inq_var(source,name,xtype,dims,lengths)
 if(status==NC_ENOTVAR) then
  status=NC_NOERR
 else if(status==NC_NOERR) then
  if(rec%f%define_mode) then
   do axis=1,size(dims)
    status=nc_inq_dim(rec%f,dims(axis),s)
    if(status==NC_NOERR) cycle
    status=nc_inq_dim(source,dims(axis),s,unlimited)
    if(status/=NC_NOERR) exit
    if(unlimited) s=NC_UNLIMITED
    status=nc_def_dim(rec%f,dims(axis),s)
    if(status/=NC_NOERR) exit
   enddo
   if(status==NC_NOERR) status=nc_def_var(rec%f,name,xtype,dims)
   if(status==NC_NOERR) status=nc_copy_attributes(source,rec%f,name)
  else
   record=0
   if(size(dims)>0) then
    if(dims(size(dims))=='Time') record=rec%record
   endif
   if(rec%new_file.or.record>0) status=nc_copy_values(source,rec%f,name,xtype,lengths,merge(1,0,record>0),record)
  endif
 endif
 s=nc_close(source)
 if(status==NC_NOERR) status=s
 rec%failed=status/=NC_NOERR
 end function

 logical function writer_die() result(die)
 character(len=32) :: env
 integer :: ios
 if(.not.die_read) then
  call get_environment_variable('LEAN_WRITER_DIE',env,status=ios)
  if(ios==0) read(env,*,iostat=ios) die_at
  die_read=.true.
 endif
 puts=puts+1
 die=puts==die_at
 end function

 integer function define_restart_var(rf,name,dimension_names,extents,kind) result(status)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dimension_names
 integer, intent(in) :: extents(:),kind
 character(len=32) :: dims(size(extents)+1)
 character(len=len(dimension_names)) :: remaining
 integer :: j,k,v,xtype

 remaining=dimension_names
 do j=1,size(extents)
  k=index(trim(remaining),' ')
  if(k==0) k=len_trim(remaining)+1
  dims(j)=remaining(:k-1)
  remaining=adjustl(remaining(k:))
 enddo
 dims(size(dims))='Time'
 v=findloc(VAR_NAME,name,dim=1)
 xtype=kind
 if(kind==NC_FLOAT.and.rf%precision==8.and.v>0) xtype=NC_DOUBLE
 status=define_variable(rf%rec,name,xtype,dims,dims(:size(extents)),extents)
 if(status==NC_NOERR.and.v>0) status=catalog_attributes(rf%rec,v,xtype,rf%mesh_path,rf%time_units)
 end function

 subroutine restart_status(rf,name,status)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name
 integer, intent(in) :: status
 rf%count=rf%count+1
 if(status==NC_NOERR)return
 if(status==NC_ENOTVAR.and.rf%mode==RESTART_GET.and.rf%optional_diag)then
  call add_message(rf%warnings,LOG_WARN,trim(rf%path)//" holds no '"//name//"': it starts from 0 at "// &
   trim(rf%xtime)//' (the run that wrote the file did not compute it).')
  return
 endif
 rf%failed=.true.
 rf%rec%failed=.true.
 rf%failed_var=name
 end subroutine

 subroutine restart_char(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 character(*), intent(inout) :: x
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,[len(x)],NC_CHAR)
 case(RESTART_PUT)
  die=writer_die()
  status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  status=nc_get(rf%nc,name,x,record=rf%rec%record)
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_logical_0(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 logical, intent(inout) :: x
 integer :: value
 if(rf%mode==RESTART_PUT)value=merge(1,0,x)
 call restart_int_0(rf,name,value,dims)
 if(rf%mode==RESTART_GET.and..not.rf%failed)x=value/=0
 end subroutine

 subroutine restart_logical_1(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 logical, intent(inout) :: x(:)
 integer :: values(size(x))
 if(rf%mode==RESTART_PUT)values=merge(1,0,x)
 call restart_int_1(rf,name,values,dims)
 if(rf%mode==RESTART_GET.and..not.rf%failed)x=values/=0
 end subroutine

 subroutine restart_real_0(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 real, intent(inout) :: x
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,[integer ::],NC_FLOAT)
 case(RESTART_PUT)
  die=writer_die()
  status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  status=nc_get(rf%nc,name,x,record=rf%rec%record)
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 ! lean_nc's get and put take contiguous targets. A member passed as a section (rad_heating(:,1,:)) is not contiguous;
 ! nvfortran 25.3 then reads into a temporary and does not copy it back (gfortran does). So a section goes through a
 ! whole copy here, in both directions.
 subroutine restart_real_1(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 real, intent(inout) :: x(:)
 real, allocatable :: whole(:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_FLOAT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_real_2(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 real, intent(inout) :: x(:,:)
 real, allocatable :: whole(:,:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_FLOAT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_real_3(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 real, intent(inout) :: x(:,:,:)
 real, allocatable :: whole(:,:,:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_FLOAT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_int_0(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 integer, intent(inout) :: x
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,[integer ::],NC_INT)
 case(RESTART_PUT)
  die=writer_die()
  status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  status=nc_get(rf%nc,name,x,record=rf%rec%record)
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_int_1(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 integer, intent(inout) :: x(:)
 integer, allocatable :: whole(:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_INT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 ! One dimension only. The values are written once, with the file (as copy_var does for the copied inputs)
 subroutine restart_static_int_1(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 integer, intent(inout) :: x(:)
 integer, allocatable :: whole(:)
 character(len=32) :: dimension_name(1)
 integer :: status,rc,v
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  dimension_name(1)=trim(dims)
  status=define_variable(rf%rec,name,NC_INT,dimension_name,dimension_name,shape(x))
  v=findloc(VAR_NAME,name,dim=1)
  if(status==NC_NOERR.and.v>0) status=catalog_attributes(rf%rec,v,NC_INT,rf%mesh_path,rf%time_units)
 case(RESTART_PUT)
  die=writer_die()
  status=NC_NOERR
  if(rf%rec%new_file) then
   whole=x
   status=nc_put(rf%rec%f,name,whole)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  allocate(whole,mold=x)
  status=nc_get(rf%nc,name,whole)
  x=whole
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_int_2(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 integer, intent(inout) :: x(:,:)
 integer, allocatable :: whole(:,:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_INT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine restart_int_3(rf,name,x,dims)
 type(restart_file), intent(inout) :: rf
 character(*), intent(in) :: name,dims
 integer, intent(inout) :: x(:,:,:)
 integer, allocatable :: whole(:,:,:)
 integer :: status,rc
 logical :: die
 if(rf%failed)return
 select case(rf%mode)
 case(RESTART_DEFINE)
  status=define_restart_var(rf,name,dims,shape(x),NC_INT)
 case(RESTART_PUT)
  die=writer_die()
  if(is_contiguous(x))then
   status=nc_put(rf%rec%f,name,x,record=rf%rec%record)
  else
   whole=x
   status=nc_put(rf%rec%f,name,whole,record=rf%rec%record)
  endif
  if(die)rc=c_kill(c_getpid(),9_c_int)
 case(RESTART_GET)
  if(is_contiguous(x))then
   status=nc_get(rf%nc,name,x,record=rf%rec%record)
  else
   allocate(whole,mold=x)
   status=nc_get(rf%nc,name,whole,record=rf%rec%record)
   x=whole
  endif
 case default
  status=LNC_EARGS
 end select
 call restart_status(rf,name,status)
 end subroutine

 subroutine variable_dims(var,dims,n)
 integer,intent(in) :: var
 character(len=32),intent(out) :: dims(:)
 integer,intent(out) :: n
 character(len=48) :: rest
 integer :: k
 n=0; dims=''
 if(VAR_TYPE(var)==CAT_TEXT) then
  n=1; dims(n)='StrLen'
 endif
 rest=VAR_DIMS(var)
 do while(len_trim(rest)>0)
  k=index(trim(rest),' ')
  if(k==0) k=len_trim(rest)+1
  n=n+1; dims(n)=rest(:k-1)
  rest=adjustl(rest(k:))
 enddo
 if(VAR_TIME(var)) then
  n=n+1; dims(n)='Time'
 endif
 end subroutine

 integer function put_var(rec,var,b,precision) result(status)
 type(open_record),intent(inout) :: rec
 integer,intent(in) :: var,precision
 type(field_buffer),intent(inout) :: b
 character(len=32) :: dims(8)
 character(len=64) :: stamp
 logical :: die
 integer :: n,j,s,first(8),counts(8),half,rc
 status=WRITE_FAILED
 rec%failed=.true.
 if(var<1.or.var>N_VARS) return
 if(b%kind/=VAR_TYPE(var)) return
 call variable_dims(var,dims,n)
 first=1; counts=1
 do j=1,n
  s=nc_inq_dim(rec%f,trim(dims(j)),counts(j))
  if(s/=NC_NOERR) return
  if(dims(j)=='Time') then
   first(j)=rec%record; counts(j)=1
  endif
 enddo
 die=writer_die()
 if(b%kind==CAT_TEXT) then
  stamp=b%text
  if(die) stamp=b%text(:32)
  if(VAR_TIME(var)) then
   s=nc_put(rec%f,trim(VAR_NAME(var)),stamp,record=rec%record)
  else
   s=nc_put(rec%f,trim(VAR_NAME(var)),stamp)
  endif
  if(die) rc=c_kill(c_getpid(),9_c_int)
 else
  if(b%n/=product(counts(:n))) return
  if(b%kind==CAT_REAL) then
   if(.not.allocated(b%r)) return
   if(size(b%r)<b%n) return
   if(b%fill) b%r(:b%n)=FILL_FLOAT
  else
   if(.not.allocated(b%i)) return
   if(size(b%i)<b%n) return
   if(b%fill) b%i(:b%n)=FILL_INT
  endif
  ! Test hook: kill after a proper rectangular half-slab, before close updates the classic record count.
  if(die) then
   do j=n,1,-1
    if(dims(j)=='Time'.or.counts(j)<2) cycle
    counts(j)=max(1,counts(j)/2); exit
   enddo
   half=product(counts(:n))
   if(b%kind==CAT_REAL) then
    s=nc_put(rec%f,trim(VAR_NAME(var)),b%r(:half),start=first(:n),count=counts(:n))
   else
    s=nc_put(rec%f,trim(VAR_NAME(var)),b%i(:half),start=first(:n),count=counts(:n))
   endif
   rc=c_kill(c_getpid(),9_c_int)
  endif
  if(b%kind==CAT_REAL.and.b%fill.and.precision==8) then
   b%d(:b%n)=FILL_DOUBLE
   s=nc_put(rec%f,trim(VAR_NAME(var)),b%d(:b%n),start=first(:n),count=counts(:n))
  else if(b%kind==CAT_REAL) then
   s=nc_put(rec%f,trim(VAR_NAME(var)),b%r(:b%n),start=first(:n),count=counts(:n))
  else
   s=nc_put(rec%f,trim(VAR_NAME(var)),b%i(:b%n),start=first(:n),count=counts(:n))
  endif
 endif
 if(s==NC_NOERR) then
  status=WRITE_OK
  rec%failed=.false.
 endif
 end function

 integer function end_record(sf,rec,msgs) result(status)
 type(stream_file),intent(inout) :: sf
 type(open_record),intent(inout) :: rec
 type(message_list),intent(inout) :: msgs
 integer :: s
 ! A negative test count fails the record containing that put at close.
 if(die_at<0.and.-puts<=die_at)rec%failed=.true.
 if(rec%failed) then
  status=WRITE_FAILED
  call writer_failed(rec,msgs); return
 endif
 s=nc_inq_dim(rec%f,'Time',sf%records)
 if(s/=NC_NOERR) then
  status=WRITE_FAILED
  call writer_failed(rec,msgs); return
 endif
 s=nc_close(rec%f)
 if(s==NC_NOERR.and.rec%new_file) &
  s=c_rename(trim(rec%path)//PARTIAL_SUFFIX//c_null_char,trim(rec%path)//c_null_char)
 status=WRITE_FAILED
 if(s/=NC_NOERR) then
  call writer_failed(rec,msgs); return
 endif
 rec%partial_created=.false.
 sf%file=rec%path
 status=WRITE_OK
 end function

 subroutine writer_failed(rec,msgs)
 type(open_record),intent(inout) :: rec
 type(message_list),intent(inout) :: msgs
 integer :: s
 if(rec%f%id>=0) s=nc_close(rec%f)
 if(rec%partial_created)then
  s=c_unlink(trim(rec%path)//PARTIAL_SUFFIX//c_null_char)
  rec%partial_created=.false.
 endif
 ! MPAS core_atmosphere/mpas_atm_core.F:704-710,865-871.
 call add_message(msgs,LOG_ERR,repeat('*',80))
 call add_message(msgs,LOG_ERR,'Error writing one or more output streams')
 call add_message(msgs,LOG_CRIT,repeat('*',80))
 end subroutine
end module
