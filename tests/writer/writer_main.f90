! The writer unit cases of tests/writer/check_writer.py, one per call:  writer CASE ROOT
! Writes test.nc in the current directory through lean's stream writer (formats, globals, fill values, clobber modes,
! renames, a killed child); check_writer.py checks the file. NCDUMP names the ncdump to use (default: ncdump on PATH).
program writer_main
 use mpas_kind_types, only: StrKIND
 use lean_inputs, only: cold_inputs, read_mesh_inputs, mesh_sizes, read_mesh_sizes, configure
 use dyn_mesh_data, only: dyn_mesh_type
 use lean_namelist, only: run_config, read_namelist, resolve_suite
 use lean_writer
 use lean_nc
 use lean_field_catalog
 use lean_streams, only: stream_set
 use lean_stream_registry, only: field_index, package_index
 use lean_messages
 implicit none
 type(stream_set) :: set
 character(len=32),allocatable :: names(:)
 integer,allocatable :: expanded(:)
 type(stream_file) :: sf, other
 type(record_request) :: req
 type(open_record) :: rec
 type(field_buffer) :: b
 type(message_list) :: msgs
 type(config_attribute),allocatable :: globals(:)
 type(run_config) :: cfg
 type(cold_inputs) :: inp
 type(dyn_mesh_type) :: mesh
 type(mesh_sizes) :: sizes
 character(len=StrKIND) :: root, ncdump
 character(len=32) :: mode, dims(4)
 integer :: lens(4), vars(8), n, j, s, k, nr
 logical :: exists
 call get_command_argument(1,mode)
 call get_command_argument(2,root)
 call get_environment_variable('NCDUMP',ncdump,status=s)
 if(s/=0) ncdump='ncdump'
 dims=[character(len=32) :: 'nCells','nVertLevels','nEdges','StrLen']
 lens=[2,3,4,64]
 vars(:6)=[var_index('qv'),var_index('latCell'),var_index('xtime'),var_index('Time'), &
             var_index('u'),var_index('initial_time')]
 n=6
 allocate(set%s(1))
 allocate(set%s(1)%field(1))
 set%n=1; set%s(1)%n_fields=1
 set%s(1)%field(1)%field=field_index('scalars')
 set%package(package_index('mp_thompson_in'))=.true.
 call stream_vars(set,1,names,expanded)
 call check(size(expanded)==8,'active scalar constituents')
 call check(all(VAR_NAME(expanded)==[character(len=32) :: 'qv','qc','qr','qi','qs','qg','ni','nr']), &
            'constituent order')
 req%stream='test'
 req%path='test.nc'
 req%time_units='seconds since 2026-11-08_06:30:00'
 req%format=NC_FORMAT_64BIT_DATA
 req%clobber=CLOBBER_TRUNCATE
 allocate(globals(3))
 globals(1)%name='first'; globals(1)%kind=CAT_TEXT; globals(1)%text='one'
 globals(2)%name='second'; globals(2)%kind=CAT_REAL; globals(2)%r=2.5d0
 globals(3)%name='third'; globals(3)%kind=CAT_INTEGER; globals(3)%i=3
 allocate(b%r(12),b%d(12),b%i(12))
 nr=1
 if(mode=='copy_var') then
  call copies()
  stop
 endif
 if(trim(mode)=='globals_greece') then
  call read_namelist(trim(root)//'/data/cases/greece_60km/reference/strict_12h/namelist.atmosphere',cfg,msgs)
  call resolve_suite(cfg,msgs)
  call read_mesh_sizes(trim(root)//'/data/cases/greece_60km/inputs/greece_60km.init.nc',sizes,msgs)
  call configure(sizes,msgs)
  call read_mesh_inputs(trim(root)//'/data/cases/greece_60km/inputs/greece_60km.init.nc',mesh,inp,msgs)
  call check(.not.has_fatal(msgs),'greece inputs')
  call global_attributes(cfg,inp%attributes,globals)
 endif
 select case(trim(mode))
 case('types','attributes','fill')
  vars(7:8)=[var_index('isnowxy'),var_index('qc_bl')]; n=8
 case('precision_double','fill_double')
  vars(7:8)=[var_index('isnowxy'),var_index('qc_bl')]; n=8
  req%precision=8
 case('records','clobber_append','clobber_overwrite')
  nr=3
 case('die')
  nr=1
 case('formats')
  nr=4
 end select
 do k=1,nr
  write(req%xtime,'(a,i2.2,a)') '2026-11-08_',k+5,':30:00'
  if(trim(mode)=='formats') then
   select case(k)
   case(1); req%format=nc_io_type_format('')
   case(2); req%format=nc_io_type_format('pnetcdf')
   case(3); req%format=nc_io_type_format('netcdf')
   case(4); req%format=nc_io_type_format('netcdf4')
   end select
   write(req%path,'(a,i1,a)') 'fmt',k,'.nc'
  endif
  if(k==3.and.index(mode,'clobber_')==1) sf=stream_file()
  if(trim(mode)=='clobber_append'.and.k==3) req%clobber=CLOBBER_APPEND
  if(trim(mode)=='clobber_overwrite'.and.k==3) then
   req%clobber=CLOBBER_OVERWRITE
   req%xtime='2026-11-08_07:30:00'
  endif
  if(trim(mode)=='die_child') req%clobber=CLOBBER_OVERWRITE
  if(trim(mode)=='die_child') req%xtime='2026-11-08_09:30:00'
  call record()
  ! A separate process must be able to open the closed file between writes.
  call execute_command_line(trim(ncdump)//' -h '//trim(req%path)//' > closed.cdl',exitstat=s)
  call check(s==0,'closed after record')
 enddo
 if(trim(mode)=='globals') then
  req%path='other.nc'; call record()
 endif
 if(trim(mode)=='clobber_never') then
  call execute_command_line('cp test.nc original.nc')
  sf=stream_file(); req%clobber=CLOBBER_NEVER_MODIFY
  do k=1,2
   s=begin_record(sf,req,vars(:n),dims,lens,globals,rec,msgs)
   call check(s==WRITE_CLOBBER_FILE,'never_modify return')
  enddo
  call execute_command_line('cmp test.nc original.nc',exitstat=s)
  call check(s==0,'never_modify bytes')
  call check(msgs%n==4,'never_modify message count')
  call check(trim(msgs%line(2)%text)=="    but clobber_mode is set to 'never_modify'.",'never_modify message text')
 endif
 if(trim(mode)=='clobber_append') then
  req%xtime='2026-11-08_07:30:00'
  s=begin_record(sf,req,vars(:n),dims,lens,globals,rec,msgs)
  call check(s==WRITE_CLOBBER_RECORD,'append conflict')
  call check(index(msgs%line(1)%text,'record    2')>0,'append conflict names record 2')
 endif
 if(trim(mode)=='clobber_overwrite') then
  req%xtime='2026-11-08_08:30:00'; call record()
 endif
 if(trim(mode)=='clobber_truncate') then
  sf=stream_file(); call record()
 endif
 print *, 'CASE OK '//trim(mode)
contains
 subroutine copies()
 type(nc_file) :: source
 s=nc_create(source,'source.nc',NC_FORMAT_64BIT_DATA,.true.)
 call check(s==NC_NOERR,'create source')
 s=nc_def_dim(source,'nCopy',2)
 call check(s==NC_NOERR,'copy dimension')
 s=nc_def_dim(source,'StrLen',7)
 call check(s==NC_NOERR,'copy text dimension')
 s=nc_def_var(source,'real_copy',NC_DOUBLE,['nCopy'])
 call check(s==NC_NOERR,'copy real definition')
 s=nc_def_var(source,'char_copy',NC_CHAR,['StrLen'])
 call check(s==NC_NOERR,'copy char definition')
 s=nc_put_att(source,'real_copy','units','meters')
 call check(s==NC_NOERR,'copy units')
 s=nc_put_att(source,'real_copy','missing_value',-1.25d0)
 call check(s==NC_NOERR,'copy missing value')
 s=nc_put_att(source,'char_copy','description','copied text')
 call check(s==NC_NOERR,'copy description')
 s=nc_put(source,'real_copy',[1.0d0,nearest(2.0d0,1.0d0)])
 call check(s==NC_NOERR,'source doubles')
 s=nc_put(source,'char_copy','  COPY!')
 call check(s==NC_NOERR,'source char')
 s=nc_close(source)
 call check(s==NC_NOERR,'close source')
 s=begin_record(sf,req,[var_index('Time')],dims,lens,globals,rec,msgs)
 call check(s==WRITE_OK,'begin copy')
 s=copy_var(rec,'source.nc','real_copy')
 call check(s==NC_NOERR,'define copied real')
 s=copy_var(rec,'source.nc','char_copy')
 call check(s==NC_NOERR,'define copied char')
 s=nc_end_define(rec%f)
 call check(s==NC_NOERR,'end define')
 s=copy_var(rec,'source.nc','real_copy')
 call check(s==NC_NOERR,'put copied real')
 s=copy_var(rec,'source.nc','char_copy')
 call check(s==NC_NOERR,'put copied char')
 s=end_record(sf,rec,msgs)
 call check(s==WRITE_OK,'end copy')
 end subroutine

 subroutine check(ok,text)
 logical,intent(in)::ok
 character(*),intent(in)::text
 if(.not.ok) then
  print *, 'FAIL '//text
  call write_messages(6,msgs)
  error stop 1
 endif
 end subroutine
 subroutine record()
 s=begin_record(sf,req,vars(:n),dims,lens,globals,rec,msgs)
 call check(s==WRITE_OK,'begin_record')
 if(trim(mode)=='partial_rename') then
  inquire(file='test.nc.lean_partial',exist=exists); call check(exists,'partial exists')
  inquire(file='test.nc',exist=exists); call check(.not.exists,'final absent')
 endif
 do j=1,n
  b%kind=VAR_TYPE(vars(j)); b%fill=trim(mode)=='fill'.or.trim(mode)=='fill_double'
  b%text=req%xtime
  b%n=1
  select case(trim(VAR_NAME(vars(j))))
  case('qv','qc_bl'); b%n=6
  case('latCell','isnowxy'); b%n=2
  case('u'); b%n=12
  end select
  b%r=real(k); b%i=k
  s=put_var(rec,vars(j),b,req%precision)
  call check(s==WRITE_OK,'put_var '//trim(VAR_NAME(vars(j))))
  if(trim(mode)=='partial_rename') then
   inquire(file='test.nc.lean_partial',exist=exists); call check(exists,'partial during puts')
   inquire(file='test.nc',exist=exists); call check(.not.exists,'final absent during puts')
  endif
 enddo
 s=end_record(sf,rec,msgs)
 call check(s==WRITE_OK,'end_record')
 inquire(file=trim(req%path)//'.lean_partial',exist=exists); call check(.not.exists,'partial removed')
 end subroutine
end program
