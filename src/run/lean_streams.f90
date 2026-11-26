! The streams file, as MPAS 8.4.2's stream inquiry, XML parser and stream manager read it: the streams, their alarms,
! packages and fields, the mesh stream, and what the run loop reads and writes at each point (io_plan).
module lean_streams
 use iso_c_binding, only: c_char, c_int, c_null_char
 use mpas_kind_types, only: StrKIND, ShortStrKIND
 use lean_clock
 use lean_messages
 use lean_namelist, only: run_config
 use lean_xml
 use lean_stream_registry
 implicit none
 private
 public :: stream_set, streams_inquire, packages_when, set_packages, package_active
 public :: stream_def, stream_request, streams_mesh, streams_parse, stream_index, dump_streams
 public :: stream_field, stream_active, stream_fields, stream_has_field
 public :: io_plan, plan_start, plan_step_inputs, plan_step_outputs
 public :: read_filename, write_filename, record_interval
 public :: streams_preview
 public :: STREAM_INPUT, STREAM_OUTPUT, STREAM_INPUT_OUTPUT, STREAM_NONE
 public :: CLOBBER_NEVER_MODIFY, CLOBBER_APPEND, CLOBBER_TRUNCATE, CLOBBER_OVERWRITE
 integer, parameter :: STREAM_INPUT = 1, STREAM_OUTPUT = 2, STREAM_INPUT_OUTPUT = 3, STREAM_NONE = 4
 integer, parameter :: CLOBBER_NEVER_MODIFY = 0, CLOBBER_APPEND = 1, CLOBBER_TRUNCATE = 2, CLOBBER_OVERWRITE = 3
 type stream_request
  character(len=:), allocatable :: kind, name, packages
 end type
 ! One field of a stream, with the packages attached to it in this stream (the stream's field_pkg_pool).
 type stream_field
  integer :: field = 0                    ! index into lean_stream_registry's FIELD_* tables
  character(len=StrKIND) :: packages = ''
  integer :: package_mask = 0             ! the known packages named in 'packages'; field_active reads the string
 end type
 type trace_line
  character(len=:), allocatable :: text
 end type
 type stream_def
  character(len=StrKIND) :: name = '', template = '', io_type = ''
  ! The length of the name in the element being parsed: the C parser's texts keep its trailing blanks, the
  ! stream manager trims them (mpas_c_to_f_string), so 'a' and 'a ' are one stream there.
  integer :: file_name_len = 0
  logical :: immutable = .false., in_file = .false., active = .true.
  integer :: direction = STREAM_NONE, file_direction = STREAM_NONE
  integer :: precision = 0, clobber = CLOBBER_NEVER_MODIFY, package_mask = 0, io_code = 0
  type(ESMF_Time) :: reference_time
  logical :: has_filename_interval = .false., has_record_interval = .false.
  type(ESMF_TimeInterval) :: filename_interval, record_interval
  character(len=ShortStrKIND) :: input_alarm = '', output_alarm = ''
  logical :: has_input_interval = .false., has_output_interval = .false.
  type(ESMF_TimeInterval) :: input_interval, output_interval
  character(len=StrKIND) :: raw_input = '', raw_output = '', resolved_input = '', resolved_output = ''
  character(len=StrKIND) :: raw_filename_interval = 'none', raw_reference = 'initial_time', raw_record = 'none'
  type(stream_request), allocatable :: requested(:)
  type(stream_field), allocatable :: field(:)
  integer :: n_fields = 0
 end type
 ! What MPAS's run loop reads and writes at one point (core_atmosphere/mpas_atm_core.F:118-151, 666-909).
 type io_plan
  character(len=ShortStrKIND) :: initial_stream = ''
  logical :: read_invariant = .false., lbc_read = .false., any_output = .false.
  logical :: diagnostics_rang = .false., restart_rang = .false.
  ! Stream indices in the manager's order: the Registry's immutable streams first, then the file's.
  ! What to do: 'read' and 'write' hold the streams to read and to write now, all of them stream_active.
  integer, allocatable :: read(:), write(:)
  ! What MPAS notes, whatever the stream's packages or type: 'read_logged' holds the streams whose input
  ! alarm rang, each of which MPAS logs as "Read '<s>' input stream valid at ..." even when it reads nothing
  ! (mpas_atm_core.F:787-809), and 'rang_output' the streams whose output alarm rang before the reset.
  integer, allocatable :: read_logged(:), rang_output(:)
 end type
 type stream_set
  character(len=StrKIND) :: path = ''
  type(xml_doc) :: doc
  logical :: package(N_PACKAGES) = .false.
  logical :: invariant_initial_only = .false.
  type(stream_def), allocatable :: s(:)
  integer :: n = 0
  character(len=StrKIND) :: mesh_stream = '', mesh_file = ''
  ! The mesh stream's attributes as xml_stream_get_attributes returns them (xml_stream_parser.c:1905-1972).
  character(len=StrKIND) :: mesh_template = '', mesh_reference = '', mesh_interval = '', mesh_io = ''
  type(trace_line), allocatable :: trace(:)
  integer :: n_trace = 0
 end type
 interface
  function c_access(path, mode) bind(C, name='access') result(ierr)
   import :: c_char, c_int
   character(kind=c_char), intent(in) :: path(*)
   integer(c_int), value :: mode
   integer(c_int) :: ierr
  end function
  function c_mkdir(path, mode) bind(C, name='mkdir') result(ierr)
   import :: c_char, c_int
   character(kind=c_char), intent(in) :: path(*)
   integer(c_int), value :: mode
   integer(c_int) :: ierr
  end function
 end interface
 ! set during streams_preview's parse: stream_path then creates and checks no directory
 logical, save :: previewing = .false.
contains
 subroutine streams_inquire(path, set, msgs)
  character(len=*), intent(in) :: path
  type(stream_set), intent(out) :: set
  type(message_list), intent(inout) :: msgs
  integer, allocatable :: children(:)
  integer :: kind, j, i
  character(len=:), allocatable :: name, value
  logical :: found
  ! framework/mpas_stream_inquiry.F:133; stream_inquiry.c:68,185-217.
  call add_message(msgs, LOG_OUT, 'Initializing MPAS_streamInfo from file '//path)
  set%path = path
  call xml_load(path, set%doc)
  if (.not. set%doc%read_ok) then
   call add_message(msgs, LOG_ERR, 'Could not open file '//path//' in read_and_broadcast')
   call add_message(msgs, LOG_CRIT, 'Initialization of streamInfo object failed for core atmosphere')
   return
  end if
  ! Inquiry uses ezxml's partial tree; an element without a name is skipped, where MPAS calls strcmp(NULL,...).
  do kind = 1, 2
   if (kind == 1) then
    children = xml_children(set%doc, 1, 'immutable_stream')
   else
    children = xml_children(set%doc, 1, 'stream')
   end if
   do j = 1, size(children)
    i = children(j)
    if (.not. xml_attr(set%doc, i, 'name', name)) cycle
    if (name /= 'invariant') cycle
    found = xml_attr(set%doc, i, 'input_interval', value)
    if (found) set%invariant_initial_only = value == 'initial_only'
    return
   end do
  end do
 end subroutine

 subroutine packages_when(cfg, set, msgs)
  type(run_config), intent(in) :: cfg
  type(stream_set), intent(inout) :: set
  type(message_list), intent(inout) :: msgs
  if (has_fatal(msgs)) return
  ! Registry.xml:486-491: LES_WHEN and SFCLAYER_WHEN, before suite resolution.
  set%package(package_index('les')) = trim(cfg%config_les_model) /= 'none'
  set%package(package_index('sfclayer')) = &
   (trim(cfg%config_physics_suite) /= 'none' .and. trim(cfg%config_sfclayer_scheme) == 'suite') .or. &
   any(trim(cfg%config_sfclayer_scheme) == [ character(len=StrKind) :: &
     'sf_monin_obukhov', 'sf_monin_obukhov_rev', 'sf_mynn' ])
  ! tools/registry/gen_inc.c:2680-2699,2887.
  call add_message(msgs, LOG_OUT, '')
  call add_message(msgs, LOG_OUT, 'Configuring registry-specified packages...')
  call add_message(msgs, LOG_OUT, '  les : '//logical_text(package_active(set, 'les')))
  call add_message(msgs, LOG_OUT, '  sfclayer : '//logical_text(package_active(set, 'sfclayer')))
  call add_message(msgs, LOG_OUT, '----- done configuring registry-specified packages -----')
  call add_message(msgs, LOG_OUT, '')
 end subroutine

 subroutine set_packages(cfg, set, msgs)
  type(run_config), intent(in) :: cfg
  type(stream_set), intent(inout) :: set
  type(message_list), intent(inout) :: msgs
  integer :: i, j
  integer, parameter :: log_order(10) = [1,2,3,4,5,6,7,9,8,10]
  if (has_fatal(msgs)) return
  ! core_atmosphere/mpas_atm_core_interface.F:146-258.
  set%package(package_index('iau')) = cfg%config_IAU_option /= 'off'
  set%package(package_index('limited_area')) = cfg%config_apply_lbcs
  set%package(package_index('jedi_da')) = cfg%config_jedi_da
  set%package(package_index('no_invariant_stream')) = .not. set%invariant_initial_only
  set%package(package_index('ugwp_orog_stream')) = cfg%config_gwdo_scheme == 'bl_ugwp_gwdo'
  set%package(package_index('ugwp_ngw_stream')) = cfg%config_ngw_scheme
  set%package(package_index('ugwp_diags_stream')) = cfg%config_ugwp_diags
  ! physics/mpas_atmphys_packages.F:53-194, after suite resolution.
  set%package(1) = cfg%config_microp_scheme == 'mp_kessler'
  set%package(2) = cfg%config_microp_scheme == 'mp_thompson'
  set%package(3) = cfg%config_microp_scheme == 'mp_thompson_aerosols'
  set%package(4) = cfg%config_microp_scheme == 'mp_wsm6'
  set%package(5) = cfg%config_convection_scheme == 'cu_grell_freitas'
  set%package(6) = cfg%config_convection_scheme == 'cu_kain_fritsch'
  set%package(7) = cfg%config_convection_scheme == 'cu_tiedtke' .or. cfg%config_convection_scheme == 'cu_ntiedtke'
  set%package(8) = cfg%config_pbl_scheme == 'bl_ysu'
  set%package(9) = cfg%config_pbl_scheme == 'bl_mynn'
  set%package(10) = cfg%config_lsm_scheme == 'sf_noahmp'
  call add_message(msgs, LOG_OUT, '----- Setting up package variables -----')
  call add_message(msgs, LOG_OUT, '')
  do i = 1, 10
   if (i == 10) call add_message(msgs, LOG_OUT, '')
   j = log_order(i)
   call add_message(msgs, LOG_OUT, '    '//trim(PACKAGE_NAME(j))//repeat(' ',24-len_trim(PACKAGE_NAME(j)))// &
     '= '//logical_text(set%package(j)))
  end do
  call add_message(msgs, LOG_OUT, '')
 end subroutine

 logical function package_active(set, name) result(active)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: name
  integer :: i
  i = package_index(name)
  active = .false.
  if (i /= 0) active = set%package(i)
 end function

 function attr(doc, i, name, default) result(value)
  type(xml_doc), intent(in) :: doc
  integer, intent(in) :: i
  character(len=*), intent(in) :: name
  character(len=*), intent(in), optional :: default
  character(len=:), allocatable :: value
  logical :: found
  found = xml_attr(doc, i, name, value)
  if (.not. found .and. present(default)) value = default
 end function

 logical function has_attr(doc, i, name) result(found)
  type(xml_doc), intent(in) :: doc
  integer, intent(in) :: i
  character(len=*), intent(in) :: name
  character(len=:), allocatable :: value
  found = xml_attr(doc, i, name, value)
 end function

 integer function direction(text) result(code)
  character(len=*), intent(in) :: text
  code = 0
  if (index(text,'input') /= 0) code = code + STREAM_INPUT
  if (index(text,'output') /= 0) code = code + STREAM_OUTPUT
  if (code == 0) code = STREAM_NONE
 end function

 subroutine file_message(set, msgs, level, text)
  type(stream_set), intent(in) :: set
  type(message_list), intent(inout) :: msgs
  integer, intent(in) :: level
  character(len=*), intent(in) :: text
  call add_message(msgs, level, 'In file '//trim(set%path)//', '//text)
 end subroutine

 ! framework/xml_stream_parser.c:348-577. Checks use substrings and keep their original order.
 logical function check_attributes(set, i, msgs) result(ok)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  type(message_list), intent(inout) :: msgs
  character(len=:), allocatable :: name, typ, template, fi, value, key, keyword
  integer :: k, j
  ok = .false.
  if (.not. has_attr(set%doc,i,'name')) then
   call file_message(set,msgs,LOG_ERR,'stream must have the "name" attribute.')
   return
  end if
  name = attr(set%doc,i,'name')
  do k = 1, 2
   key = 'type'
   if (k == 2) key = 'filename_template'
   if (has_attr(set%doc,i,key)) cycle
   call file_message(set,msgs,LOG_ERR,'stream "'//name//'" must have the "'//key//'" attribute.')
   return
  end do
  typ = attr(set%doc,i,'type')
  do k = 1, 2
   keyword = 'input'
   if (k == 2) keyword = 'output'
   key = keyword//'_interval'
   if (index(typ,keyword) == 0 .or. has_attr(set%doc,i,key)) cycle
   call file_message(set,msgs,LOG_ERR,'stream "'//name//'" is an '//keyword// &
    ' stream and must have the "'//key//'" attribute.')
   return
  end do
  if (direction(typ) == STREAM_INPUT .and. has_attr(set%doc,i,'output_interval')) &
   call file_message(set,msgs,LOG_WARN,'input-only stream "'//name//'" has the "output_interval" attribute.')
  if (direction(typ) == STREAM_OUTPUT .and. has_attr(set%doc,i,'input_interval')) &
   call file_message(set,msgs,LOG_WARN,'output-only stream "'//name//'" has the "input_interval" attribute.')
  fi = attr(set%doc,i,'filename_interval')
  do k = 1, 2
   key = 'input_interval'
   if (k == 2) key = 'output_interval'
   if (index(fi,key) == 0) cycle
   if (has_attr(set%doc,i,key)) cycle
   call file_message(set,msgs,LOG_ERR,'stream "'//name//'" has a value of "'//key// &
    '" for the "filename_interval" attribute, without defining the "'//key//'" attribute.')
   return
  end do
  do j = 1, 2
   keyword = 'initial_only'
   if (j == 2) keyword = 'final_only'
   do k = 1, 2
    key = 'input_interval'
    if (k == 2) key = 'output_interval'
    if (index(fi,key) == 0) cycle
    value = attr(set%doc,i,key)
    if (index(value,keyword) == 0) cycle
    call file_message(set,msgs,LOG_ERR,'stream "'//name//'" cannot have a value of "'//key// &
     '" for the "filename_interval" attribute, when "'//key//'" is set to "'//keyword//'".')
    return
   end do
  end do
  template = attr(set%doc,i,'filename_template')
  do k = len(template), 1, -1
   if (template(k:k) /= '$' .or. k == len(template)) cycle
   if (index('YMDdhmsGSB',template(k+1:k+1)) /= 0) cycle
   call file_message(set,msgs,LOG_ERR,'filename_template for stream "'//name// &
    '" contains unrecognized variable "$'//template(k+1:k+1)//'".')
   return
  end do
  ok = .true.
 end function

 logical function check_streams(set, msgs) result(ok)
  type(stream_set), intent(in) :: set
  type(message_list), intent(inout) :: msgs
  integer, allocatable :: immutable(:), mutable(:), children(:), ordered(:)
  integer :: i, j, k, a, b
  character(len=:), allocatable :: name, other, template, filename
  ok = .false.
  immutable = xml_children(set%doc,1,'immutable_stream')
  mutable = xml_children(set%doc,1,'stream')
  do k = 1, size(immutable)
   i = immutable(k)
   if (.not. check_attributes(set,i,msgs)) return
   do j = i+1, set%doc%n
    if (set%doc%el(j)%parent /= i) cycle
    ! A nested <stream> is refused too, beside MPAS's var and file check; var_array and var_struct remain ignored.
    ! Compared with ==, not select case: nvfortran 25.3 matches no case for this deferred-length component.
    other = set%doc%el(j)%name
    if (other == 'var' .or. other == 'file' .or. other == 'stream') then
     name = attr(set%doc,i,'name')
     call file_message(set,msgs,LOG_ERR,'the set of variables in stream "'//name//'" cannot be modified.')
     return
    end if
   end do
  end do
  do k = 1, size(mutable)
   i = mutable(k)
   if (.not. check_attributes(set,i,msgs)) return
   name = attr(set%doc,i,'name')
   children = xml_children(set%doc,i,'file')
   do j = 1, size(children)
    filename = attr(set%doc,children(j),'name')
    if (c_access(filename//c_null_char,4_c_int) == 0) cycle
    call file_message(set,msgs,LOG_ERR,'definition of stream "'//name//'" references file '// &
     filename//' that cannot be opened for reading.')
    return
   end do
  end do
  ordered = [mutable,immutable]
  do a = 1, size(ordered)
   i = ordered(a)
   name = attr(set%doc,i,'name')
   template = attr(set%doc,i,'filename_template')
   do b = 1, size(ordered)
    j = ordered(b)
    if (i == j) cycle
    other = attr(set%doc,j,'name')
    if (same(name,other)) then
     call file_message(set,msgs,LOG_ERR,'stream "'//name//'" is define more than once.')
     return
    end if
    if (index(attr(set%doc,i,'type'),'output') == 0 .and. index(attr(set%doc,j,'type'),'output') == 0) cycle
    if (.not. same(template,attr(set%doc,j,'filename_template'))) cycle
    call file_message(set,msgs,LOG_ERR,'Output streams "'//name//'" and "'//other// &
     '" cannot share the filename_template "'//template//'".')
    return
   end do
  end do
  ok = .true.
 end function

 subroutine streams_mesh(set, cfg, clock, msgs)
  type(stream_set), intent(inout) :: set
  type(run_config), intent(in) :: cfg
  type(run_clock), intent(in) :: clock
  type(message_list), intent(inout) :: msgs
  if (has_fatal(msgs)) return
  call add_message(msgs,LOG_OUT,'Reading streams configuration from file '//trim(set%path))
  if (mesh_setup(set,cfg,clock,msgs)) return
  if (.not. has_fatal(msgs)) call add_message(msgs,LOG_CRIT,'stream xml get attribute failed: '//trim(set%path))
 end subroutine

 logical function mesh_setup(set, cfg, clock, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  type(run_config), intent(in) :: cfg
  type(run_clock), intent(in) :: clock
  type(message_list), intent(inout) :: msgs
  type(ESMF_Time) :: reference
  type(ESMF_TimeInterval) :: interval
  integer, allocatable :: children(:)
  integer :: k, i
  character(len=:), allocatable :: template, rt, fi, io, filename, label
  ok = .false.
  ! MPAS's syntax pass, then a stop on a tree ezxml could build only in part, with the reader's detail; the
  ! inquiry itself never rejects a partial tree.
  if (.not. xml_syntax_ok(set%doc,msgs)) return
  if (.not. set%doc%tree_ok) then
   call add_message(msgs,LOG_ERR,'Problems encountered while parsing run-time I/O config file '//trim(set%path))
   call add_message(msgs,LOG_ERR,set%doc%tree_error)
   return
  end if
  if (.not. check_streams(set,msgs)) return
  set%mesh_stream = 'input'
  if (cfg%config_do_restart) set%mesh_stream = 'restart'
  if (set%invariant_initial_only) set%mesh_stream = 'invariant'
  ! core_atmosphere/mpas_atm_core_interface.F:365-406; xml_stream_parser.c:1870-1980.
  children = xml_children(set%doc,1,'immutable_stream')
  i = 0
  do k = 1, size(children)
   if (.not. same(attr(set%doc,children(k),'name'),trim(set%mesh_stream))) cycle
   i = children(k)
   exit
  end do
  if (i == 0) then
   ! lean names the missing stream; MPAS gives the CRIT alone.
   call file_message(set,msgs,LOG_ERR,'no <immutable_stream> named "'//trim(set%mesh_stream)// &
    '"; the mesh is read from it.')
   call add_message(msgs,LOG_CRIT,'stream xml get attribute failed: '//trim(set%path))
   return
  end if
  template = attr(set%doc,i,'filename_template')
  rt = attr(set%doc,i,'reference_time','initial_time')
  fi = attr(set%doc,i,'filename_interval','none')
  if (index(fi,'interval') /= 0) fi = attr(set%doc,i,fi,'none')
  call add_message(msgs,LOG_OUT,'Found mesh stream with filename template '//template)
  io = attr(set%doc,i,'io_type')
  set%mesh_template = template
  set%mesh_reference = rt
  set%mesh_interval = fi
  set%mesh_io = 'pnetcdf'
  if (.not. has_attr(set%doc,i,'io_type')) then
   call add_message(msgs,LOG_OUT,'Using default io_type for mesh stream')
  else
   label = ''
   if (index(io,'pnetcdf,cdf5') /= 0) then
    label = 'Parallel-NetCDF (CDF-5, large variable support)'
   else if (index(io,'pnetcdf') /= 0) then
    label = 'Parallel-NetCDF'
   else if (index(io,'netcdf4') /= 0) then
    label = 'NetCDF-4/HDF5'
   else if (index(io,'netcdf') /= 0) then
    label = 'Serial NetCDF'
   end if
   if (label == '') then
    call add_message(msgs,LOG_OUT,'*** unrecognized io_type specification for mesh stream; defaulting to Parallel-NetCDF')
   else
    call add_message(msgs,LOG_OUT,'Using io_type '//label//' for mesh stream')
    set%mesh_io = io
   end if
  end if
  reference = clock%start
  if (rt /= 'initial_time') then
   if (.not. parse_time(rt,reference,msgs)) then
    call bad_time(set,trim(set%mesh_stream),'reference_time',rt,.false.,msgs)
    return
   end if
  end if
  if (fi == 'none') then
   if (.not. expand_template(reference,template,filename,msgs)) return
  else
   if (.not. parse_interval(fi,interval,msgs)) then
    call bad_time(set,trim(set%mesh_stream),'filename_interval',fi,.true.,msgs)
    return
   end if
   if (.not. build_stream_filename(reference,clock%start,interval,template,filename,msgs)) return
  end if
  set%mesh_file = filename
  call add_message(msgs,LOG_OUT,' ** Attempting to bootstrap MPAS framework using stream: '//trim(set%mesh_stream))
  ok = .true.
 end function

 subroutine bad_time(set, stream, key, value, interval, msgs)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: stream, key, value
  logical, intent(in) :: interval
  type(message_list), intent(inout) :: msgs
  character(len=:), allocatable :: label
  label = 'time'
  if (interval) label = 'time interval'
  call file_message(set,msgs,LOG_ERR,'stream "'//stream//'": '//key//' "'//value//'" is not a valid '//label)
 end subroutine

 ! extract_stream_interval: framework/xml_stream_parser.c:934-1024 (one reference level only).
 logical function resolve_interval(set, i, key, value, msgs) result(ok)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  character(len=*), intent(in) :: key
  character(len=:), allocatable, intent(out) :: value
  type(message_list), intent(inout) :: msgs
  integer :: colon, k, target
  integer, allocatable :: children(:)
  character(len=:), allocatable :: name, other, attribute, tail, prefix
  value = attr(set%doc,i,key)
  ok = .true.
  if (index(value,'stream:') /= 1) return
  ok = .false.
  name = attr(set%doc,i,'name')
  tail = value(8:)
  colon = index(tail,':',back=.true.)
  other = tail(:max(0,colon-1))
  attribute = tail(colon+1:)
  if (same(name,other) .and. key == attribute) then
   call add_message(msgs,LOG_ERR,"Attribute '"//key//"' of stream '"//name//"' references itself.")
   return
  end if
  if (attribute /= 'input_interval' .and. attribute /= 'output_interval') then
   call add_message(msgs,LOG_ERR,"Attribute '"//key//"' of stream '"//name//"' references an invalid attribute: '"//attribute//"'.")
   call add_message(msgs,LOG_ERR,"       Valid attributes are 'input_interval' and 'output_interval'.")
   return
  end if
  children = [xml_children(set%doc,1,'immutable_stream'),xml_children(set%doc,1,'stream')]
  target = 0
  do k = 1, size(children)
   if (.not. same(attr(set%doc,children(k),'name'),other)) cycle
   target = children(k)
   exit
  end do
  prefix = "The '"//key//"' attribute of stream '"//name//"' "
  if (target == 0) then
   call add_message(msgs,LOG_ERR,prefix//"refers to an undefined stream named '"//other//"'.")
   return
  end if
  if (.not. xml_attr(set%doc,target,attribute,value)) then
   call add_message(msgs,LOG_ERR,prefix//"refers to an undefined attribute named '"//attribute//"' of stream '"//other//"'.")
   return
  end if
  if (value == 'input_interval' .or. value == 'output_interval' .or. index(value,'stream:') == 1) then
   call add_message(msgs,LOG_ERR,prefix//"contains an unexpandable value: '"//value//"'.")
   return
  end if
  ok = .true.
 end function

 ! C's strcmp: trailing blanks count, as ezxml keeps them (external/ezxml/ezxml.c:211).
 logical function same(a, b)
  character(len=*), intent(in) :: a, b
  same = len(a) == len(b) .and. a == b
 end function

 logical function recurring(text) result(yes)
  character(len=*), intent(in) :: text
  yes = index(text,'initial_only') == 0 .and. index(text,'final_only') == 0 .and. index(text,'none') == 0
 end function

 function filename_interval_text(set, i, input, output) result(text)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  character(len=*), intent(in) :: input, output
  character(len=:), allocatable :: text, raw_input, raw_output
  integer :: code
  ! framework/xml_stream_parser.c:1137-1195,1446-1504: decide on raw, use resolved value.
  raw_input = attr(set%doc,i,'input_interval')
  raw_output = attr(set%doc,i,'output_interval')
  text = attr(set%doc,i,'filename_interval','none')
  code = direction(attr(set%doc,i,'type'))
  if (.not. has_attr(set%doc,i,'filename_interval')) then
   if ((code == 1 .or. code == 3) .and. recurring(raw_input)) then
    text = input
   else if ((code == 2 .or. code == 3) .and. recurring(raw_output)) then
    text = output
   end if
  else if (index(text,'input_interval') /= 0) then
   text = 'none'
   if (recurring(raw_input)) text = input
  else if (index(text,'output_interval') /= 0) then
   text = 'none'
   if (recurring(raw_output)) text = output
  end if
 end function

 subroutine property(msgs, key, value)
  type(message_list), intent(inout) :: msgs
  character(len=*), intent(in) :: key, value
  call add_message(msgs,LOG_OUT,repeat(' ',8)//key//repeat(' ',max(0,20-len(key)))//value)
 end subroutine

 ! One line of the parse's trace (@ create, @ alarm, @ package, @ field, ...): the calls MPAS's C parser makes into
 ! the stream manager, which tests/streams compares with MPAS's own (dump_streams writes them).
 subroutine trace(set, text)
  type(stream_set), intent(inout) :: set
  character(len=*), intent(in) :: text
  type(trace_line), allocatable :: grown(:)
  if (.not. allocated(set%trace)) allocate(set%trace(16))
  if (set%n_trace == size(set%trace)) then
   allocate(grown(2*size(set%trace)))
   grown(:set%n_trace) = set%trace(:set%n_trace)
   call move_alloc(grown,set%trace)
  end if
  set%n_trace = set%n_trace + 1
  set%trace(set%n_trace)%text = text
 end subroutine

 ! framework/xml_stream_parser.c:1204-1339,1513-1647 (substring priority is significant).
 subroutine stream_options(set, i, stream, msgs)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  type(stream_def), intent(inout) :: stream
  type(message_list), intent(inout) :: msgs
  character(len=:), allocatable :: value, label
  integer :: j
  character(len=13), parameter :: modes(5) = &
   [character(len=13) :: 'never_modify', 'append', 'truncate', 'replace_files', 'overwrite']
  integer, parameter :: codes(5) = [0,1,2,2,3]
  value = attr(set%doc,i,'clobber_mode')
  if (has_attr(set%doc,i,'clobber_mode')) then
   do j = 1, size(modes)
    if (index(value,trim(modes(j))) == 0) cycle
    stream%clobber = codes(j)
    call property(msgs,'clobber mode:',trim(modes(j)))
    exit
   end do
   if (j > size(modes)) call add_message(msgs,LOG_OUT, &
    '        *** unrecognized clobber_mode specification; existing files will not be modified')
  end if
  stream%io_type = attr(set%doc,i,'io_type')
  value = trim(stream%io_type)
  if (has_attr(set%doc,i,'io_type')) then
   label = ''
   if (index(value,'pnetcdf,cdf5') /= 0) then
    stream%io_code = 1
    label = 'Parallel-NetCDF (CDF-5, large variable support)'
   else if (index(value,'pnetcdf') /= 0) then
    stream%io_code = 0
    label = 'Parallel-NetCDF'
   else if (index(value,'netcdf4') /= 0) then
    stream%io_code = 3
    label = 'NetCDF-4/HDF5'
   else if (index(value,'netcdf') /= 0) then
    stream%io_code = 2
    label = 'Serial NetCDF'
   end if
   if (label == '') then
    call add_message(msgs,LOG_OUT,'        *** unrecognized io_type specification; defaulting to Parallel-NetCDF')
   else
    call property(msgs,'I/O type:',label)
   end if
  end if
  label = 'none'
  select case (stream%file_direction)
  case (1); label = 'input'
  case (2); label = 'output'
  case (3); label = 'input, output'
  end select
  call property(msgs,'direction:',label)
  stream%raw_reference = attr(set%doc,i,'reference_time','initial_time')
  call property(msgs,'reference time:',trim(stream%raw_reference))
  stream%raw_record = attr(set%doc,i,'record_interval','none')
  label = '-'
  if (has_attr(set%doc,i,'record_interval')) label = trim(stream%raw_record)
  call property(msgs,'record interval:',label)
  value = attr(set%doc,i,'precision')
  if (index(value,'single') /= 0) then
   stream%precision = 4
  else if (index(value,'double') /= 0) then
   stream%precision = 8
  else if (has_attr(set%doc,i,'precision')) then
   call add_message(msgs,LOG_OUT,'        *** unrecognized precision specification; reverting to native precision')
  end if
  if (stream%precision /= 0) call property(msgs,'real precision:',int_text(stream%precision)//' bytes')
 end subroutine

 ! build_stream_path: framework/xml_stream_parser.c:787-912. No shell command or C object in the model.
 logical function stream_path(set, name, template, msgs) result(ok)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: name, template
  type(message_list), intent(inout) :: msgs
  integer :: slash, i
  integer(c_int) :: ierr
  character(len=:), allocatable :: directory, part
  logical :: writable_parent
  ok = .true.
  if (previewing) return
  slash = index(template,'/',back=.true.)
  if (slash == 0) return
  directory = template(:slash-1)
  writable_parent = .true.
  do i = 1, len(directory)
   if (directory(i:i) /= '/') cycle
   part = directory(:i-1)
   if (c_access(part//c_null_char,0_c_int) == 0) then
    writable_parent = c_access(part//c_null_char,2_c_int) == 0
   else
    ierr = c_mkdir(part//c_null_char,511_c_int)
    if (ierr /= 0 .and. .not. writable_parent) then
     call cannot_create(part)
     return
    end if
   end if
  end do
  if (c_access(directory//c_null_char,0_c_int) == 0) then
   if (c_access(directory//c_null_char,2_c_int) /= 0) then
    call file_message(set,msgs,LOG_ERR,'definition of stream "'//name//'" references directory '// &
     directory//' without write permission.')
    ok = .false.
   end if
  else
   ierr = c_mkdir(directory//c_null_char,511_c_int)
   if (ierr == 0) then
    call add_message(msgs,LOG_OUT,'        *** created directory '//directory//' for stream "'//name//'"')
   else if (.not. writable_parent) then
    call cannot_create(directory)
   end if
  end if
 contains
  subroutine cannot_create(part)
   character(len=*), intent(in) :: part
   call file_message(set,msgs,LOG_ERR,'cannot create directory '//part//' needed by stream "'// &
    name//'": parent directory is not writable.')
   ok = .false.
  end subroutine
 end function

 ! The memory check's look at the streams before the mesh is read: the parse of streams_parse into a fresh set, its
 ! alarms on private_clock (a clock the caller made for it), no directory created or checked, its messages dropped. ok is .false. on any problem: the real parse then stops the run with its message.
 subroutine streams_preview(set, cfg, private_clock, preview, ok)
  type(stream_set), intent(in) :: set
  type(run_config), intent(in) :: cfg
  type(run_clock), intent(inout) :: private_clock
  type(stream_set), intent(out) :: preview
  logical, intent(out) :: ok
  type(message_list) :: dropped
  integer :: i
  preview%path = set%path
  preview%package = set%package
  preview%invariant_initial_only = set%invariant_initial_only
  call xml_load(trim(set%path), preview%doc)
  ok = preview%doc%read_ok
  if (.not. ok) return
  previewing = .true.
  call streams_parse(preview, cfg, private_clock, dropped)
  previewing = .false.
  do i = 1, dropped%n
   if (dropped%line(i)%level >= LOG_ERR) ok = .false.
  end do
 end subroutine

 ! xml_stream_parser.c:1039-1855; mpas_stream_manager.F:1188-1198,5990-6048.
 subroutine streams_parse(set, cfg, clock, msgs)
  type(stream_set), intent(inout) :: set
  type(run_config), intent(in) :: cfg
  type(run_clock), intent(inout) :: clock
  type(message_list), intent(inout) :: msgs
  integer, allocatable :: immutable(:), mutable(:), ordered(:)
  integer :: j, i, idx, e
  logical :: ok
  character(len=:), allocatable :: name
  if (has_fatal(msgs)) return
  immutable = xml_children(set%doc,1,'immutable_stream')
  mutable = xml_children(set%doc,1,'stream')
  if (allocated(set%s)) deallocate(set%s)
  if (allocated(set%trace)) deallocate(set%trace)
  set%n_trace = 0
  allocate(set%s(N_IMMUTABLE+size(mutable)))
  set%n = N_IMMUTABLE
  ! MPAS tools/registry/gen_inc.c:2039-2205: the Registry's streams exist first, including ones omitted from the file.
  do j = 1, N_IMMUTABLE
   set%s(j)%name = IMMUTABLE_NAME(j)
   set%s(j)%template = IMMUTABLE_TEMPLATE(j)
   set%s(j)%direction = IMMUTABLE_DIRECTION(j)
   set%s(j)%immutable = .true.
   set%s(j)%reference_time = clock%start
   allocate(set%s(j)%requested(0))
   ! the Registry's entries, each through add_field; their lines come before the parse
   do e = IMMUTABLE_FIRST(j), IMMUTABLE_FIRST(j) + IMMUTABLE_COUNT(j) - 1
    ok = add_field(set,set%s(j),trim(FIELD_NAME(ENTRY_FIELD(e))),mask_packages(ENTRY_PACKAGES(e)),msgs)
   end do
  end do
  call add_message(msgs,LOG_OUT,achar(10)//'Parsing run-time I/O configuration from '//trim(set%path)//' ...')
  ordered = [immutable,mutable]
  do j = 1, size(ordered)
   i = ordered(j)
   name = attr(set%doc,i,'name')
   idx = stream_index(set,name)
   if (j > size(immutable)) then
    if (idx > N_IMMUTABLE) then
     ! Two names that differ only in trailing blanks pass C's strcmp, and are one stream in the manager.
     idx = -idx
    else if (idx /= 0) then
     ! lean stops here, before MPAS's mutable-stream callback would modify a Registry immutable stream.
     call file_message(set,msgs,LOG_ERR,'stream "'//name// &
      '" is a Registry immutable stream; define it with <immutable_stream>.')
     call add_message(msgs,LOG_CRIT,'xml stream parser failed: '//trim(set%path))
     return
    else
     set%n = set%n + 1
     idx = set%n
    end if
   end if
   ok = parse_one_stream(set,i,idx,j <= size(immutable),clock,msgs)
   if (.not. ok) then
    if (.not. has_fatal(msgs)) call add_message(msgs,LOG_CRIT,'xml stream parser failed: '//trim(set%path))
    return
   end if
  end do
  call add_message(msgs,LOG_OUT,achar(10)//' ----- done parsing run-time I/O from '//trim(set%path)//' -----'//achar(10))
  ! The initial state's stream must be read at the start: otherwise MPAS reads nothing and runs on without an
  ! initial state (mpas_stream_manager.F:3657-3685).
  idx = stream_index(set,trim(merge('restart','input  ',cfg%config_do_restart)))
  ok = stream_active(set,idx) .and. set%s(idx)%input_alarm /= ''
  if (ok) ok = alarm_ringing(clock,set%s(idx)%input_alarm)
  if (.not. ok) then
   call file_message(set,msgs,LOG_ERR,'stream "'//trim(set%s(idx)%name)// &
    '" is not read at the start time: lean_mpas cannot read the initial state.')
   call add_message(msgs,LOG_CRIT,'xml stream parser failed: '//trim(set%path))
   return
  end if
  call add_message(msgs,LOG_OUT,' ** Validating streams')
 end subroutine

 logical function parse_one_stream(set, i, idx, immutable, clock, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  integer, intent(in) :: i, idx
  logical, intent(in) :: immutable
  type(run_clock), intent(inout) :: clock
  type(message_list), intent(inout) :: msgs
  type(stream_def) :: stream, existing
  character(len=:), allocatable :: name, input, output, fi, label
  integer :: j, itype
  ok = .false.
  ! The C parser's texts keep the name as ezxml gives it; the stream manager trims it (mpas_c_to_f_string).
  name = attr(set%doc,i,'name')
  stream%name = name
  stream%file_name_len = len(name)
  stream%immutable = immutable
  stream%in_file = .true.
  stream%template = attr(set%doc,i,'filename_template')
  stream%file_direction = direction(attr(set%doc,i,'type'))
  stream%direction = stream%file_direction
  if (immutable .and. idx > 0) stream%direction = IMMUTABLE_DIRECTION(idx)
  stream%active = stream%file_direction /= STREAM_NONE
  allocate(stream%requested(0))
  if (.not. resolve_interval(set,i,'input_interval',input,msgs)) return
  if (.not. resolve_interval(set,i,'output_interval',output,msgs)) return
  stream%raw_input = attr(set%doc,i,'input_interval')
  stream%raw_output = attr(set%doc,i,'output_interval')
  stream%resolved_input = input
  stream%resolved_output = output
  fi = filename_interval_text(set,i,input,output)
  stream%raw_filename_interval = fi
  label = ''
  if (immutable) label = 'immutable '
  call add_message(msgs,LOG_OUT,achar(10)//' -----  found '//label//'stream "'//name//'" in '//trim(set%path)//'  -----')
  call property(msgs,'filename template:',trim(stream%template))
  call property(msgs,'filename interval:',fi)
  call stream_options(set,i,stream,msgs)
  if (stream%file_direction == 2 .or. stream%file_direction == 3) then
   if (.not. stream_path(set,name,trim(stream%template),msgs)) return
  end if
  call trace(set,'@ create '//name//' direction='//int_text(stream%file_direction)// &
   ' template='//trim(stream%template)//' filename_interval='//fi//' reference_time='//trim(stream%raw_reference)// &
   ' record_interval='//trim(stream%raw_record)//' immutable='//int_text(merge(1,0,immutable))// &
   ' precision='//int_text(stream%precision)//' clobber='//int_text(stream%clobber)//' io_type='//int_text(stream%io_code))
  itype = stream%file_direction
  stream%reference_time = clock%start
  if (idx < 0) then
   ! A mutable stream of that name exists: MPAS_stream_mgr_create_stream fails without a line
   ! (mpas_stream_manager.F:353-357). stream_mgr_create_stream_c then replaces that error with the result of
   ! each set_property call that follows (:6038-6052), and those find the existing stream and succeed. So the
   ! parse stops here only when none is made; otherwise this element's reference time, record interval and
   ! filename interval, then its alarms, packages and fields, go to the existing stream. Its template, clobber
   ! mode, precision and I/O type do not.
   if (trim(stream%raw_reference) == 'initial_time' .and. trim(stream%raw_record) == 'none' .and. &
       fi == 'none' .and. itype /= STREAM_NONE) return
   existing = set%s(-idx)
   existing%file_name_len = len(name)
   if (trim(stream%raw_reference) /= 'initial_time') existing%raw_reference = stream%raw_reference
   if (trim(stream%raw_record) /= 'none') existing%raw_record = stream%raw_record
   if (fi /= 'none') existing%raw_filename_interval = fi
   if (itype == STREAM_NONE) existing%active = .false.
   stream = existing
  end if
  if (idx > 0) then
   ! The stream exists in the manager from here on: a nested <stream> naming it finds it.
   set%s(idx)%name = name
   if (immutable) then
    ! An immutable stream keeps the Registry's fields. One whose fields are all deactivated (iau in a
    ! default run) has no list: assigning from an unallocated array is undefined.
    if (allocated(set%s(idx)%field)) stream%field = set%s(idx)%field
    stream%n_fields = set%s(idx)%n_fields
   end if
  end if
  if (immutable .and. idx == 0) then
   ! framework/mpas_stream_manager.F:1899,6022-6026.
   call add_message(msgs,LOG_ERR,'No stream matching '//trim(name)//' exists in call to MPAS_stream_mgr_set_property().')
   call add_message(msgs,LOG_ERR,repeat('*',80))
   call add_message(msgs,LOG_ERR," Error: Stream '"//trim(name)//"' was not defined in the Registry.xml file as ")
   call add_message(msgs,LOG_ERR,'        an immutable stream. Immutable streams may only be defined in the Registry.xml')
   call add_message(msgs,LOG_ERR,'        file for a core.')
   call add_message(msgs,LOG_ERR,repeat('*',80))
   return
  end if
  if (.not. stream_times(set,stream,clock,msgs)) return
  if (itype == 1 .or. itype == 3) then
   if (.not. stream_alarm(set,stream,clock,'input',input,trim(attr(set%doc,i,'input_interval')),msgs)) return
  end if
  if (itype == 2 .or. itype == 3) then
   if (.not. stream_alarm(set,stream,clock,'output',output,trim(attr(set%doc,i,'output_interval')),msgs)) return
  end if
  call stream_packages(set,i,stream,msgs)
  if (.not. immutable) then
   if (.not. stream_requests(set,i,stream,msgs)) return
  end if
  ! MPAS stops on a $G template only when it expands it: for a stream active by the package rule of
  ! mpas_stream_manager.F:4538-4582 (stream_active) that has an alarm, since only such a stream is read or written.
  if (stream%active .and. index(stream%template,'$G') /= 0 .and. &
      (stream%input_alarm /= '' .or. stream%output_alarm /= '')) then
   do j = 1, N_PACKAGES
    if (.not. btest(stream%package_mask,j-1)) cycle
    if (set%package(j)) exit
   end do
   if (stream%package_mask == 0 .or. j <= N_PACKAGES) then
    call add_message(msgs,LOG_CRIT,'ERROR: mpas_expand_string option $G is not a valid expansion character.')
    return
   end if
  end if
  set%s(abs(idx)) = stream
  ok = .true.
 end function

 ! The stream's name as the C parser passes it on, trailing blanks included.
 function file_name(stream) result(name)
  type(stream_def), intent(in) :: stream
  character(len=:), allocatable :: name
  name = stream%name(:stream%file_name_len)
 end function

 ! The reference time is the caller's (the start, or what an earlier element of the same name left).
 logical function stream_times(set, stream, clock, msgs) result(ok)
  type(stream_set), intent(in) :: set
  type(stream_def), intent(inout) :: stream
  type(run_clock), intent(in) :: clock
  type(message_list), intent(inout) :: msgs
  character(len=:), allocatable :: value, name
  ok = .false.
  name = trim(stream%name)
  value = trim(stream%raw_reference)
  if (value /= 'initial_time') then
   if (.not. parse_time(value,stream%reference_time,msgs)) then
    call bad_time(set,name,'reference_time',value,.false.,msgs)
    return
   end if
  end if
  value = trim(stream%raw_record)
  stream%has_record_interval = value /= 'none'
  if (stream%has_record_interval) then
   if (.not. parse_interval(value,stream%record_interval,msgs)) then
    call bad_time(set,name,'record_interval',value,.true.,msgs)
    return
   end if
  end if
  value = trim(stream%raw_filename_interval)
  stream%has_filename_interval = value /= 'none'
  if (stream%has_filename_interval) then
   if (.not. parse_interval(value,stream%filename_interval,msgs)) then
    call bad_time(set,name,'filename_interval',value,.true.,msgs)
    return
   end if
  end if
  ok = .true.
 end function

 logical function stream_alarm(set, stream, clock, which, value, raw, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  type(stream_def), intent(inout) :: stream
  type(run_clock), intent(inout) :: clock
  character(len=*), intent(in) :: which, value, raw
  type(message_list), intent(inout) :: msgs
  type(ESMF_TimeInterval) :: interval
  character(len=:), allocatable :: name, label, id
  logical :: is_recurring, on_stream
  ok = .false.
  name = trim(stream%name)
  id = name//'_'//which
  call trace(set,'@ alarm '//file_name(stream)//' '//which//' start '//value)
  is_recurring = value /= 'none' .and. value /= 'initial_only' .and. value /= 'final_only'
  if (is_recurring) then
   if (.not. parse_interval(value,interval,msgs)) then
    call bad_time(set,name,which//'_interval',value,.true.,msgs)
    return
   end if
  end if
  if (value /= 'none') then
   ! The immutable stream retains its Registry direction (mpas_stream_manager.F:1188-1198).
   if ((which == 'input' .and. stream%direction == STREAM_OUTPUT) .or. &
       (which == 'output' .and. stream%direction == STREAM_INPUT) .or. stream%direction == STREAM_NONE) then
    call add_message(msgs,LOG_ERR,'Attempting to add an alarm '//id//' to invalid direction for stream '//name)
    return
   end if
   ! An id the clock holds is this stream's alarm (the names are one after trimming): after the clock's
   ! 'OOPS' line, MPAS_stream_mgr_add_alarm finds it on the stream (mpas_stream_manager.F:1217-1230).
   on_stream = alarm_index(clock,id) /= 0
   if (.not. add_stream_alarm(clock,name,which,value,stream%reference_time,msgs)) then
    if (on_stream) call add_message(msgs,LOG_ERR,'Requested '//which//' alarm '//id//' already on stream '//name)
    return
   end if
   if (which == 'input') then
    stream%input_alarm = id
    stream%has_input_interval = is_recurring
    if (is_recurring) stream%input_interval = interval
   else
    stream%output_alarm = id
    stream%has_output_interval = is_recurring
    if (is_recurring) stream%output_interval = interval
   end if
  end if
  label = raw
  if (raw /= value) label = raw//' ('//value//')'
  call property(msgs,which//' alarm:',label)
  ok = .true.
 end function

 ! xml_stream_parser.c:1374-1408,1683-1716; mpas_stream_manager.F:6295-6336.
 subroutine stream_packages(set, i, stream, msgs)
  type(stream_set), intent(inout) :: set
  integer, intent(in) :: i
  type(stream_def), intent(inout) :: stream
  type(message_list), intent(inout) :: msgs
  character(len=:), allocatable :: list, token, label
  integer :: first, last, p
  if (.not. has_attr(set%doc,i,'packages')) return
  list = attr(set%doc,i,'packages')
  first = 1
  do
   last = index(list(first:),';')
   if (last == 0) then
    token = list(first:)
   else
    token = list(first:first+last-2)
   end if
   call trace(set,'@ package '//file_name(stream)//' '//token)
   p = package_index(token)
   if (p == 0) then
    call add_message(msgs,LOG_ERR,'Package '//trim(token)//'Active not found in call to MPAS_stream_mgr_add_pkg().')
    call file_message(set,msgs,LOG_WARN,'definition of stream "'//file_name(stream)// &
     '" references unrecognized package "'//token//'".')
   else
    stream%package_mask = ibset(stream%package_mask,p-1)
    label = token
    if (first == 1 .and. stream%immutable) label = token//achar(10)
    call property(msgs,'package:',label)
   end if
   if (last == 0) exit
   first = first + last
  end do
 end subroutine

 ! xml_stream_parser.c:1718-1849: the callbacks in the C parser's order, each applied as the stream manager does.
 logical function stream_requests(set, i, stream, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  integer, intent(in) :: i
  type(stream_def), intent(inout) :: stream
  type(message_list), intent(inout) :: msgs
  integer :: kind, k, j, u, ierr, nbytes, first, last
  integer, allocatable :: children(:)
  character(len=10), parameter :: tags(5) = &
   [character(len=10) :: 'file', 'var', 'var_array', 'var_struct', 'stream']
  character(len=:), allocatable :: name, packages, text, request_kind
  ok = .false.
  do kind = 1, size(tags)
   children = xml_children(set%doc,i,trim(tags(kind)))
   do k = 1, size(children)
    j = children(k)
    name = attr(set%doc,j,'name')
    packages = attr(set%doc,j,'packages')
    request_kind = 'field'
    if (kind == 4) request_kind = 'pool'
    if (kind == 5) request_kind = 'substream'
    if (kind /= 1) then
     if (.not. request(set,stream,request_kind,name,packages,msgs)) return
     cycle
    end if
    open(newunit=u,file=name,status='old',action='read',access='stream',form='unformatted',iostat=ierr)
    if (ierr /= 0) then
     call file_message(set,msgs,LOG_ERR,'definition of stream "'//file_name(stream)//'" references file '// &
      name//' that cannot be opened for reading.')
     return
    end if
    inquire(unit=u,size=nbytes)
    text = repeat(' ',nbytes)
    if (nbytes > 0) read(u,iostat=ierr) text
    close(u)
    if (ierr /= 0) return
    ! fscanf("%s") splits only on C whitespace; neither quotes nor comments are special.
    first = 1
    do while (first <= len(text))
     if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13),text(first:first)) /= 0) then
      first = first + 1
      cycle
     end if
     last = first
     do while (last <= len(text))
      if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13),text(last:last)) /= 0) exit
      last = last + 1
     end do
     if (.not. request(set,stream,'field',text(first:last-1),packages,msgs)) return
     first = last
    end do
   end do
  end do
  ok = .true.
 end function

 recursive logical function request(set, stream, kind, name, packages, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  type(stream_def), intent(inout) :: stream
  character(len=*), intent(in) :: kind, name, packages
  type(message_list), intent(inout) :: msgs
  type(stream_request) :: entry
  type(stream_request), allocatable :: more(:)
  integer :: m, have
  entry%kind = kind
  entry%name = name
  entry%packages = packages
  ! Grown member by member: nvfortran 25.3 loses the deferred lengths in [array, element] of this type.
  have = 0
  if (allocated(stream%requested)) have = size(stream%requested)
  allocate(more(have+1))
  do m = 1, have
   more(m)%kind = stream%requested(m)%kind
   more(m)%name = stream%requested(m)%name
   more(m)%packages = stream%requested(m)%packages
  end do
  more(have+1)%kind = kind
  more(have+1)%name = name
  more(have+1)%packages = packages
  call move_alloc(more, stream%requested)
  call trace(set,'@ '//kind//' '//file_name(stream)//' '//name//' '//packages)
  select case (kind)
  case ('field')
   ok = add_field(set,stream,name,packages,msgs)
  case ('pool')
   ok = add_pool(set,stream,name,packages,msgs)
  case default
   ok = add_substream(set,stream,name,packages,msgs)
  end select
 end function

 ! 0: not a package, 1: inactive, 2: active. The pool key is the token itself, blanks included.
 integer function package_state(set, token) result(state)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: token
  integer :: p
  state = 0
  do p = 1, N_PACKAGES
   if (len(token) /= len_trim(PACKAGE_NAME(p)) .or. token /= PACKAGE_NAME(p)) cycle
   state = merge(2,1,set%package(p))
   return
  end do
 end function

 ! parse_package_list, framework/mpas_stream_manager.F:4589-4664, copied as is: empty tokens are skipped,
 ! the scan stops at the first active package, and a last token of one character is never looked at (:4648).
 logical function package_list_active(set, packages, undefined) result(active)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: packages
  logical, intent(out) :: undefined
  integer :: i, j, slen, state
  undefined = .false.
  slen = len_trim(packages)
  active = slen == 0
  if (active) return
  i = 1
  j = index(packages,';')
  do while (j >= i)
   if (j > i) then
    state = package_state(set,packages(i:j-1))
    active = state == 2
    if (active) return
    if (state == 0) undefined = .true.
   end if
   i = j + 1
   j = index(packages(i:slen),';') + i - 1
  end do
  if (i < slen) then
   state = package_state(set,packages(i:slen))
   active = state == 2
   if (active) return
   if (state == 0) undefined = .true.
  end if
 end function

 ! A Registry entry's packages as a list; every name is a package, so the order does not change add_field or
 ! field_active.
 function mask_packages(mask) result(packages)
  integer, intent(in) :: mask
  character(len=:), allocatable :: packages
  integer :: p
  packages = ''
  do p = 1, N_PACKAGES
   if (.not. btest(mask,p-1)) cycle
   if (packages /= '') packages = packages//';'
   packages = packages//trim(PACKAGE_NAME(p))
  end do
 end function

 integer function field_position(stream, field) result(k)
  type(stream_def), intent(in) :: stream
  integer, intent(in) :: field
  do k = 1, stream%n_fields
   if (stream%field(k)%field == field) return
  end do
  k = 0
 end function

 subroutine append_field(set, stream, field, packages)
  type(stream_set), intent(in) :: set
  type(stream_def), intent(inout) :: stream
  integer, intent(in) :: field
  character(len=*), intent(in) :: packages
  type(stream_field), allocatable :: grown(:)
  integer :: first, last
  if (.not. allocated(stream%field)) allocate(stream%field(64))
  if (stream%n_fields == size(stream%field)) then
   allocate(grown(2*size(stream%field)))
   grown(:stream%n_fields) = stream%field(:stream%n_fields)
   call move_alloc(grown,stream%field)
  end if
  stream%n_fields = stream%n_fields + 1
  stream%field(stream%n_fields)%field = field
  stream%field(stream%n_fields)%packages = packages
  stream%field(stream%n_fields)%package_mask = 0
  first = 1
  do while (first <= len_trim(packages))
   last = index(packages(first:),';')
   if (last == 0) last = len_trim(packages) - first + 2
   if (package_state(set,packages(first:first+last-2)) /= 0) stream%field(stream%n_fields)%package_mask = &
    ibset(stream%field(stream%n_fields)%package_mask,package_index(packages(first:first+last-2))-1)
   first = first + last
  end do
 end subroutine

 ! MPAS_stream_mgr_add_field, framework/mpas_stream_manager.F:842-931.
 logical function add_field(set, stream, name, packages, msgs) result(ok)
  type(stream_set), intent(in) :: set
  type(stream_def), intent(inout) :: stream
  character(len=*), intent(in) :: name, packages
  type(message_list), intent(inout) :: msgs
  integer :: field, p
  logical :: active, undefined
  ok = .false.
  field = field_index(name)
  if (field == 0) then
   call add_message(msgs,LOG_ERR,'Requested field '//trim(name)//' not available')
   return
  end if
  ok = .true.
  if (field_position(stream,field) /= 0) then
   call add_message(msgs,LOG_WARN,'Requested field '//trim(name)//' already in stream '//trim(stream%name))
   return
  end if
  if (len_trim(packages) > 0) then
   active = package_list_active(set,trim(packages),undefined)
   if (undefined) call add_message(msgs,LOG_WARN,"One or more packages in '"//trim(packages)// &
    "' attached to field '"//trim(name)//"' is undefined")
  end if
  ! the field's own packages from the Registry
  active = FIELD_PACKAGES(field) == 0
  do p = 1, N_PACKAGES
   if (btest(FIELD_PACKAGES(field),p-1) .and. set%package(p)) active = .true.
  end do
  if (.not. active) then
   call add_message(msgs,LOG_OUT,' * Requested field '//trim(name)// &
    ' is deactivated due to packages, or is a scratch variable.')
   return
  end if
  ! A scratch field enters the stream's field pool like any other; MPAS leaves it out of the file later,
  ! without a line (field_active below; framework/mpas_io_streams.F:504, 926).
  call append_field(set,stream,field,packages)
 end function

 ! MPAS_stream_mgr_add_pool, framework/mpas_stream_manager.F:693-816: each member in the pool's order.
 logical function add_pool(set, stream, name, packages, msgs) result(ok)
  type(stream_set), intent(in) :: set
  type(stream_def), intent(inout) :: stream
  character(len=*), intent(in) :: name, packages
  type(message_list), intent(inout) :: msgs
  integer :: pool, field
  ok = .false.
  pool = pool_index(name)
  if (pool == 0) then
   call add_message(msgs,LOG_ERR,'Requested pool '//trim(name)//' does not exist.')
   return
  end if
  do field = 1, N_FIELDS
   if (FIELD_POOL(field) /= pool) cycle
   if (.not. add_field(set,stream,trim(FIELD_NAME(field)),packages,msgs)) return
  end do
  ok = .true.
 end function

 ! A nested <stream>: xml_stream_parser.c:1795-1848 and stream_mgr_add_immutable_stream_fields_c
 ! (framework/mpas_stream_manager.F:6145-6195), as MPAS 8.4.2 runs a nested <stream> (the nested_* cases of tests/streams).
 recursive logical function add_substream(set, stream, name, packages, msgs) result(ok)
  type(stream_set), intent(inout) :: set
  type(stream_def), intent(inout) :: stream
  character(len=*), intent(in) :: name, packages
  type(message_list), intent(inout) :: msgs
  integer, allocatable :: streams(:), children(:)
  integer :: ref, k, j, kind, field
  logical :: active, undefined
  character(len=10), parameter :: tags(3) = [character(len=10) :: 'var', 'var_array', 'var_struct']
  ok = .true.
  ref = stream_index(set,name)
  if (ref /= 0) then
   ! A mutable stream that exists already (defined earlier, or this one) adds nothing, without a line.
   if (.not. set%s(ref)%immutable) return
   ! MPAS_stream_mgr_add_stream_fields, :947-1064: the stream's fields with the nested element's packages.
   if (len_trim(packages) > 0) then
    active = package_list_active(set,trim(packages),undefined)
    if (undefined) call add_message(msgs,LOG_WARN,"One or more packages in '"//trim(packages)// &
     "' attached to fields from stream '"//trim(name)//"' is undefined")
   end if
   do k = 1, set%s(ref)%n_fields
    field = set%s(ref)%field(k)%field
    if (field_position(stream,field) /= 0) then
     call add_message(msgs,LOG_WARN,'Requested field '//trim(FIELD_NAME(field))//' already in stream '// &
      trim(stream%name))
     cycle
    end if
    call append_field(set,stream,field,packages)
   end do
   return
  end if
  ! Not in the manager (defined later in the file, or nowhere): MPAS's error (:2191); is_immutable stays
  ! unset (:6175), and MPAS 8.4.2 runs with it false, so the C parser searches the file's mutable streams
  ! and adds the inline var, var_array and var_struct of each one of that name.
  call add_message(msgs,LOG_ERR,'Stream '//trim(name)//' does not exist in call to MPAS_stream_mgr_get_property().')
  streams = xml_children(set%doc,1,'stream')
  do k = 1, size(streams)
   if (.not. same(attr(set%doc,streams(k),'name'),name)) cycle
   do kind = 1, size(tags)
    children = xml_children(set%doc,streams(k),trim(tags(kind)))
    do j = 1, size(children)
     ok = request(set,stream,trim(merge('pool ','field',kind == 3)),attr(set%doc,children(j),'name'),packages,msgs)
     if (.not. ok) return
    end do
   end do
  end do
 end function

 ! stream_active_pkg_check (framework/mpas_stream_manager.F:4538-4571): read or written only if it is
 ! active and has no known package or one active.
 logical function stream_active(set, i) result(active)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  integer :: p
  active = set%s(i)%active
  if (.not. active .or. set%s(i)%package_mask == 0) return
  active = .false.
  do p = 1, N_PACKAGES
   if (btest(set%s(i)%package_mask,p-1) .and. set%package(p)) active = .true.
  end do
 end function

 ! MPAS's field rule (mpas_stream_manager.F:4331-4333): no attached packages, or parse_package_list of them. A scratch field is
 ! never read or written: MPAS_streamAddField returns for a field that is not persistent
 ! (framework/mpas_io_streams.F:504, 613, 722, 926, 1044, 1162, 1282, 1402).
 logical function field_active(set, entry) result(active)
  type(stream_set), intent(in) :: set
  type(stream_field), intent(in) :: entry
  logical :: undefined
  active = .not. FIELD_SCRATCH(entry%field)
  if (active) active = package_list_active(set,trim(entry%packages),undefined)
 end function

 ! The fields stream i reads or writes now, in add order; none while the stream itself is inactive.
 subroutine stream_fields(set, i, names)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  character(len=32), allocatable, intent(out) :: names(:)
  character(len=32), allocatable :: found(:)
  integer :: k, n
  allocate(found(set%s(i)%n_fields))
  n = 0
  if (stream_active(set,i)) then
   do k = 1, set%s(i)%n_fields
    if (.not. field_active(set,set%s(i)%field(k))) cycle
    n = n + 1
    found(n) = FIELD_NAME(set%s(i)%field(k)%field)
   end do
  end if
  ! A second array, not names = names(:n): nvfortran 25.3 faults when the left side is also on the right.
  allocate(names(n))
  names(:) = found(:n)
 end subroutine

 ! In stream i and active by field_active's rule; the stream's own packages are not looked at.
 logical function stream_has_field(set, i, name) result(has)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  character(len=*), intent(in) :: name
  integer :: k
  has = .false.
  k = field_position(set%s(i),field_index(name))
  if (k /= 0) has = field_active(set,set%s(i)%field(k))
 end function

 logical function rings(set, clock, i, output)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(in) :: clock
  integer, intent(in) :: i
  logical, intent(in) :: output
  rings = .false.
  if (i == 0) return
  if (output) then
   if (set%s(i)%output_alarm /= '') rings = alarm_ringing(clock,set%s(i)%output_alarm)
  else
   if (set%s(i)%input_alarm /= '') rings = alarm_ringing(clock,set%s(i)%input_alarm)
  end if
 end function

 ! MPAS_stream_mgr_reset_alarms for one stream and direction; a reset acts only while the alarm rings.
 subroutine reset(set, clock, i, output)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(inout) :: clock
  integer, intent(in) :: i
  logical, intent(in) :: output
  if (i == 0) return
  if (output) then
   if (set%s(i)%output_alarm /= '') call reset_alarm(clock,set%s(i)%output_alarm)
  else
   if (set%s(i)%input_alarm /= '') call reset_alarm(clock,set%s(i)%input_alarm)
  end if
 end subroutine

 ! mpas_stream_mgr_write for every stream (mpas_stream_manager.F:2853-2866, write_stream :3130-3148), the
 ! notes atm_core_run takes from the alarms, then the reset of every output alarm that rings.
 subroutine plan_outputs(set, clock, plan)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(inout) :: clock
  type(io_plan), intent(inout) :: plan
  integer :: i
  plan%write = [integer ::]
  plan%rang_output = [integer ::]
  do i = 1, set%n
   if (.not. rings(set,clock,i,.true.)) cycle
   plan%rang_output = [plan%rang_output,i]
   if (set%s(i)%direction /= STREAM_OUTPUT .and. set%s(i)%direction /= STREAM_INPUT_OUTPUT) cycle
   if (stream_active(set,i)) plan%write = [plan%write,i]
  end do
  plan%any_output = size(plan%rang_output) > 0
  plan%diagnostics_rang = rings(set,clock,stream_index(set,'diagnostics'),.true.)
  plan%restart_rang = rings(set,clock,stream_index(set,'restart'),.true.)
  do i = 1, set%n
   call reset(set,clock,i,.true.)
  end do
 end subroutine

 ! At the start: atm_core_init's reads (mpas_atm_core.F:118-151) and the start of atm_core_run (:666-752).
 subroutine plan_start(set, clock, cfg, plan)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(inout) :: clock
  type(run_config), intent(in) :: cfg
  type(io_plan), intent(out) :: plan
  integer :: invariant, lbc
  plan%read = [integer ::]
  plan%read_logged = [integer ::]
  invariant = stream_index(set,'invariant')
  plan%read_invariant = stream_active(set,invariant) .and. rings(set,clock,invariant,.false.)
  call reset(set,clock,invariant,.false.)
  plan%initial_stream = merge('restart','input  ',cfg%config_do_restart)
  call reset(set,clock,stream_index(set,'input'),.false.)
  call reset(set,clock,stream_index(set,'restart'),.false.)
  call reset(set,clock,stream_index(set,'iau'),.false.)
  call reset(set,clock,stream_index(set,'iau'),.true.)
  call reset(set,clock,stream_index(set,'restart'),.true.)
  if (cfg%config_do_restart) then
   ! No history or diagnostics output at the start of a restart run.
   call reset(set,clock,stream_index(set,'output'),.true.)
   call reset(set,clock,stream_index(set,'diagnostics'),.true.)
  end if
  call plan_outputs(set,clock,plan)
  ! MPAS consults the 'diagnostics' and 'restart' alarms only after a step (mpas_atm_core.F:877-903); at
  ! the start it writes and resets (:677-720).
  plan%diagnostics_rang = .false.
  plan%restart_rang = .false.
  ! The first LBC read; the alarm is reset in the first step, after its update.
  lbc = stream_index(set,'lbc_in')
  plan%lbc_read = cfg%config_apply_lbcs .and. rings(set,clock,lbc,.false.)
 end subroutine

 ! Before each step (mpas_atm_core.F:754-813): the LBC update, then every input stream whose alarm rings (read_logged),
 ! of which the active ones are read (read).
 subroutine plan_step_inputs(set, clock, cfg, plan)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(inout) :: clock
  type(run_config), intent(in) :: cfg
  type(io_plan), intent(inout) :: plan
  integer :: i, lbc
  lbc = stream_index(set,'lbc_in')
  plan%lbc_read = cfg%config_apply_lbcs .and. rings(set,clock,lbc,.false.)
  call reset(set,clock,lbc,.false.)
  plan%read = [integer ::]
  plan%read_logged = [integer ::]
  do i = 1, set%n
   if (set%s(i)%direction /= STREAM_INPUT .and. set%s(i)%direction /= STREAM_INPUT_OUTPUT) cycle
   if (.not. rings(set,clock,i,.false.)) cycle
   plan%read_logged = [plan%read_logged,i]
   ! read_stream returns before any I/O for a stream that is not active (mpas_stream_manager.F:3657-3669).
   if (stream_active(set,i)) plan%read = [plan%read,i]
   call reset(set,clock,i,.false.)
  end do
 end subroutine

 ! After each step (mpas_atm_core.F:826-909), after the caller's advance_run_clock: the outputs.
 subroutine plan_step_outputs(set, clock, plan)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(inout) :: clock
  type(io_plan), intent(inout) :: plan
  call plan_outputs(set,clock,plan)
 end subroutine

 ! read_stream (mpas_stream_manager.F:3686-3698): the file read at 'when'.
 logical function read_filename(set, i, when, name, msgs) result(ok)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  type(ESMF_Time), intent(in) :: when
  character(len=:), allocatable, intent(out) :: name
  type(message_list), intent(inout) :: msgs
  if (set%s(i)%has_filename_interval) then
   ok = build_stream_filename(set%s(i)%reference_time,when,set%s(i)%filename_interval,trim(set%s(i)%template),name,msgs)
  else
   ok = expand_template(when,trim(set%s(i)%template),name,msgs)
  end if
 end function

 ! write_stream (mpas_stream_manager.F:3183-3191, 3237-3244): the file written at 'when'; without a
 ! filename interval every record goes to the file named at the stream's reference time.
 logical function write_filename(set, i, when, name, msgs) result(ok)
  type(stream_set), intent(in) :: set
  integer, intent(in) :: i
  type(ESMF_Time), intent(in) :: when
  character(len=:), allocatable, intent(out) :: name
  type(message_list), intent(inout) :: msgs
  if (set%s(i)%has_filename_interval) then
   ok = build_stream_filename(set%s(i)%reference_time,when,set%s(i)%filename_interval,trim(set%s(i)%template),name,msgs)
  else
   ok = expand_template(set%s(i)%reference_time,trim(set%s(i)%template),name,msgs)
  end if
 end function

 ! MPAS_STREAM_PROPERTY_RECORD_INTV for input (mpas_stream_manager.F:2103-2145): the record interval, else
 ! the stream's recurring input alarm interval (a one-shot alarm has none, mpas_timekeeping.F:671).
 logical function record_interval(set, clock, i, interval) result(found)
  type(stream_set), intent(in) :: set
  type(run_clock), intent(in) :: clock
  integer, intent(in) :: i
  type(ESMF_TimeInterval), intent(out) :: interval
  found = set%s(i)%has_record_interval
  if (found) then
   interval = set%s(i)%record_interval
   return
  end if
  if (clock%n_alarms == 0 .or. set%s(i)%input_alarm == '') return
  found = set%s(i)%has_input_interval
  if (found) interval = set%s(i)%input_interval
 end function

 integer function stream_index(set, name) result(idx)
  type(stream_set), intent(in) :: set
  character(len=*), intent(in) :: name
  integer :: i
  idx = 0
  do i = 1, set%n
   if (set%s(i)%name /= name) cycle
   idx = i
   return
  end do
 end function

 subroutine dump_streams(unit, set)
  integer, intent(in) :: unit
  type(stream_set), intent(in) :: set
  integer :: i
  do i = 1, set%n_trace
   write(unit,'(a)') set%trace(i)%text
  end do
 end subroutine
end module
