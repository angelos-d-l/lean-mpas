! The output streams other than the restart: where each Registry field name comes from (field_class: mesh file, model
! state, diagnostics, or held at its input or Registry default value), its values gathered from the running model
! (gather_field) and the stream records written (write_streams). Host code only.
module lean_fields
 use config
 use schemes, only: moist_physics, lsm_on
 use dyn_config, only: i_qv, i_qc, i_qr, i_qi, i_qs, i_qg, i_ni, i_nr, rv, rgas
 use model_state, only: state_type
 use noahmp_state, only: noahmp_state_type
 use sea_ice, only: ice_cell, sea_ice_cells=>ice, xice_threshold
 use land_ice, only: glacier,glacier_of
 use initial_winds, only: reconstruct_winds
 use model_data
 use model_start, only: start_lean
 use lean_run_inputs, only: run_inputs
 use lean_namelist, only: run_config
 use lean_streams
 use lean_clock, only: run_clock, time_string
 use lean_writer
 use lean_field_catalog
 use lean_nc
 use radiation_climatology, only: initialize_ozone_tables, monthly_ozone
 use diag_manager, only: diag_fetch
 use lean_messages
 use lean_log, only: log_fatal
 implicit none
 private
 public :: field_class, gather_field, output_start, output_needs, write_streams, advance_cf_time, set_vegfra
 public :: N_FETCH, FETCH_DYN, FETCH_STATE, FETCH_LAND, FETCH_RAD, FETCH_MICRO
 ! Where a field of an output stream comes from: SOURCE_NONE a field lean does not compute (the start stops); SOURCE_TIME
 ! xtime, initial_time, Time; SOURCE_MESH read from the mesh file; SOURCE_STATE the prognostic state; SOURCE_OUTPUT_DIAG
 ! theta/rho/pressure derived from it; SOURCE_LEAN surface and physics fields of the model's own arrays; SOURCE_HELD the input
 ! or Registry default value; SOURCE_DIAG the diagnostics package (diag_manager). Only the names are compared.
 integer, parameter, public :: SOURCE_NONE=0, SOURCE_TIME=1, SOURCE_MESH=2, SOURCE_STATE=3, &
  SOURCE_OUTPUT_DIAG=4, SOURCE_LEAN=5, SOURCE_HELD=6, SOURCE_DIAG=7
 public :: FETCH_DIAG_DYN,FETCH_CONV,FETCH_PRECIPW
 public :: sst, xice, vegetation, cf_time, globals, tslb_by_update, smois_first
 ! True once a surface update (surface_bdy.f90) has run: it writes the soil temperature of water and sea-ice cells itself
 ! (mpas_atmphys_update_surface.F:226, :260, :274), whatever the land-surface scheme does
 logical, save :: tslb_by_update = .false.
 ! FETCH_*: which group of card fields output_fields_to_host copies before a write.
 integer, parameter :: FETCH_DIAG_DYN=6,FETCH_CONV=7,FETCH_PRECIPW=8
 integer, parameter :: N_FETCH=8, FETCH_DYN=1, FETCH_STATE=2, FETCH_LAND=3, FETCH_RAD=4, FETCH_MICRO=5
 type output_stream
  type(stream_file) :: file
  integer, allocatable :: vars(:)
 end type
 type(output_stream), allocatable, save :: streams(:)
 type(config_attribute), allocatable, save :: globals(:)
 type(field_buffer), save :: buffer
 character(len=16), save :: dims(N_DIMS)
 integer, save :: lengths(N_DIMS), elements(N_VARS), held_slot(N_VARS)
 real, save :: cf_time, ozone_pressure(59)
 real, allocatable, save :: start_winds(:,:,:),water_temperature(:,:)
 real, allocatable, save :: initial(:,:), sst(:), xice(:), vegetation(:)
 ! 1 on a cell whose soil moisture MPAS's Noah-MP driver set to 1.0 in its first step because the sea-ice fraction was exactly 1
 ! (NoahmpDriverMainMod.F90:135-153, itimestep == 1). Nothing rewrites it when the fraction changes later: the history keeps 1.0
 ! on that cell, and the input's value on a cell that reaches the fraction 1 afterwards. The restart file carries it as the variable lean_smois_first_step.
 integer, allocatable, save :: smois_first(:)
contains
 integer function field_class(name) result(source)
 character(*), intent(in) :: name
 select case(name)
 case('xtime', 'initial_time', 'Time')
  source=SOURCE_TIME
 case('latCell', 'lonCell', 'xCell', 'yCell', 'zCell', &
   'indexToCellID', 'latEdge', 'lonEdge', 'xEdge', 'yEdge', &
   'zEdge', 'indexToEdgeID', 'latVertex', 'lonVertex', 'xVertex', &
   'yVertex', 'zVertex', 'indexToVertexID', 'cellsOnEdge', 'nEdgesOnCell', &
   'nEdgesOnEdge', 'edgesOnCell', 'edgesOnEdge', 'weightsOnEdge', 'dvEdge', &
   'dcEdge', 'angleEdge', 'areaCell', 'areaTriangle', 'cellsOnCell', &
   'verticesOnCell', 'verticesOnEdge', 'edgesOnVertex', 'cellsOnVertex', 'kiteAreasOnVertex', &
   'meshDensity', 'zgrid', 'fzm', 'fzp', 'zz')
  source=SOURCE_MESH
 case('u', 'w', 'qv', 'qc', 'qr', &
   'qi', 'qs', 'qg', 'ni', 'nr', &
   'rho_zz', 'theta_m')
  source=SOURCE_STATE
 case('theta', 'rho', 'pressure')
  source=SOURCE_OUTPUT_DIAG
 case('surface_pressure', 'uReconstructZonal', 'uReconstructMeridional', 'rainnc', 'kpbl', &
   'hpbl', 'hfx', 'qfx', 'lh', 'u10', &
   'v10', 'q2', 't2m', 'th2m', 'gsw', &
   'glw', 'xland', 'skintemp', 'snow', 'snowh', &
   'sst', 'vegfra', 'xice', 'sh2o', 'smois', &
   'tslb', 'pin', 'ozmixm', 'mminlu', 'isice_lu', &
   'iswater_lu', 'cldfrac_bl', 'sfc_albedo', 'sfc_emiss', 'q2mvxy', &
   'q2mbxy', 'chvxy', 'chbxy')
  source=SOURCE_LEAN
 case('i_rainnc', 'i_rainc', 't_oml', 't_oml_initial', 't_oml_200m_initial', &
   'h_oml', 'h_oml_initial', 'hu_oml', 'hv_oml', 'lcl', &
   'lfc')
  source=SOURCE_HELD
 case('sul','sslt','dust1','dust2','dust3','dust4', &
   'ocpho','bcpho','ocphi','bcphi','bg','volc')
  source=SOURCE_HELD
 case('mslp', 'relhum_50hPa', 'relhum_100hPa', 'relhum_200hPa', 'relhum_250hPa', &
   'relhum_500hPa', 'relhum_700hPa', 'relhum_850hPa', 'relhum_925hPa', 'dewpoint_50hPa', &
   'dewpoint_100hPa', 'dewpoint_200hPa', 'dewpoint_250hPa', 'dewpoint_500hPa', 'dewpoint_700hPa', &
   'dewpoint_850hPa', 'dewpoint_925hPa', 'temperature_50hPa', 'temperature_100hPa', 'temperature_200hPa', &
   'temperature_250hPa', 'temperature_500hPa', 'temperature_700hPa', 'temperature_850hPa', 'temperature_925hPa', &
   'height_50hPa', 'height_100hPa', 'height_200hPa', 'height_250hPa', 'height_500hPa', &
   'height_700hPa', 'height_850hPa', 'height_925hPa', 'uzonal_50hPa', 'uzonal_100hPa', &
   'uzonal_200hPa', 'uzonal_250hPa', 'uzonal_500hPa', 'uzonal_700hPa', 'uzonal_850hPa', &
   'uzonal_925hPa', 'umeridional_50hPa', 'umeridional_100hPa', 'umeridional_200hPa', 'umeridional_250hPa', &
   'umeridional_500hPa', 'umeridional_700hPa', 'umeridional_850hPa', 'umeridional_925hPa', 'w_50hPa', &
   'w_100hPa', 'w_200hPa', 'w_250hPa', 'w_500hPa', 'w_700hPa', &
   'w_850hPa', 'w_925hPa', 'vorticity_50hPa', 'vorticity_100hPa', 'vorticity_200hPa', &
   'vorticity_250hPa', 'vorticity_500hPa', 'vorticity_700hPa', 'vorticity_850hPa', 'vorticity_925hPa', &
   't_isobaric', 't_iso_levels', 'z_isobaric', 'z_iso_levels', 'meanT_500_300')
  source=SOURCE_DIAG
 case('cape', 'cin', 'srh_0_1km', 'srh_0_3km', 'uzonal_surface', &
   'uzonal_1km', 'uzonal_6km', 'umeridional_surface', 'umeridional_1km', 'umeridional_6km', &
   'temperature_surface', 'dewpoint_surface')
  source=SOURCE_DIAG
 case('updraft_helicity_max', 'w_velocity_max', 'wind_speed_level1_max', 'relhum', 'refl10cm', 'refl10cm_max', 'refl10cm_1km', 'refl10cm_1km_max')
  source=SOURCE_DIAG
 case('olrtoa', 'rainc', 'acsnow', 'precipw', 'cuprec', 'cd', 'cda', 'ck', 'cka', 'divergence', 'ke', 'vorticity')
  source=SOURCE_DIAG
 case default
  source=SOURCE_NONE
 end select
 end function

 subroutine output_start(cfg,set,run,msgs)
 type(run_config), intent(in) :: cfg
 type(stream_set), intent(in) :: set
 type(run_inputs), intent(in) :: run
 type(message_list), intent(inout) :: msgs
 type(nc_file) :: f
 character(len=32), allocatable :: names(:)
 character(len=32) :: die_env
 character(len=32) :: name
 character(len=48) :: definition
 integer :: i,j,v,d,n,status,largest,ios,nh
 logical :: unsupported
 call get_environment_variable('LEAN_WRITER_DIE',die_env,status=ios)
 if(ios==0.or.ios==-1)call add_message(msgs,LOG_WARN, &
  'LEAN_WRITER_DIE is a test switch; unset it for forecasts')
 allocate(streams(set%n))
 cf_time=run%cold%cf_time
 dims=DIM_NAME
 status=nc_open(f,run%cold%mesh_path)
 if(status/=NC_NOERR)error stop 'output mesh open'
 do d=1,N_DIMS
  status=nc_inq_dim(f,trim(dims(d)),n)
  if(status==NC_NOERR)then
   lengths(d)=n
   cycle
  endif
  definition=DIM_DEFINITION(d)
  lengths(d)=0
  read(definition,*,iostat=ios)n
  if(ios==0)lengths(d)=n
  select case(trim(dims(d)))
  case('nVertLevels'); lengths(d)=nz
  case('nVertLevelsP1'); lengths(d)=nz+1
  case('nVertLevelsP2'); lengths(d)=nz+2
  case('nMonths'); lengths(d)=12
  case('nSoilLevels'); lengths(d)=4
  case('nzSnowLevels'); lengths(d)=7
  case('nOznLevels'); lengths(d)=59
  case('nAerLevels'); lengths(d)=29
  case('cam_dim1'); lengths(d)=1
  case('nLags'); lengths(d)=0
  end select
 enddo
 status=nc_close(f)
 elements=1
 do v=1,N_VARS
  definition=VAR_DIMS(v)
  do while(len_trim(definition)>0)
   j=index(trim(definition)//' ',' ')
   d=dim_index(definition(:j-1))
   if(d>0)elements(v)=elements(v)*lengths(d)
   definition=adjustl(definition(j:))
  enddo
 enddo
 largest=ncells
 unsupported=.false.
 do i=1,set%n
  call stream_vars(set,i,names,streams(i)%vars)
  if(.not.set%s(i)%active)cycle
  if(set%s(i)%direction/=STREAM_OUTPUT.and.set%s(i)%direction/=STREAM_INPUT_OUTPUT)cycle
  if(trim(set%s(i)%name)=='restart')cycle
  do j=1,size(streams(i)%vars)
   v=streams(i)%vars(j);name=VAR_NAME(v)
   largest=max(largest,elements(v))
   select case(field_class(name))
   case(SOURCE_NONE)
    call add_message(msgs,LOG_ERR,"Stream '"//trim(set%s(i)%name)//"' asks for '"//trim(name)// &
     "', which lean_mpas does not compute")
    unsupported=.true.
   end select
  enddo
 enddo
 if(unsupported)then
  call add_message(msgs,LOG_ERR,'lean_mpas writes no values it does not compute: remove these fields from the streams file '// &
   'or its stream_list files')
  call add_message(msgs,LOG_CRIT,'Core init failed for core atmosphere')
  return
 endif
 allocate(buffer%r(largest),buffer%d(largest),buffer%i(largest))
 held_slot=0;nh=0
 do v=1,N_VARS
  name=VAR_NAME(v)
  if(field_class(name)/=SOURCE_HELD)then
   if(run%st%kind==start_lean.or..not.input_at_start(name))cycle
  endif
  if(elements(v)>ncells)cycle
  nh=nh+1;held_slot(v)=nh
 enddo
 allocate(initial(ncells,nh))
 initial=0.
 if(.not.allocated(sst))allocate(sst(ncells),xice(ncells),vegetation(ncells))
 if(.not.allocated(smois_first))then
  allocate(smois_first(ncells))
  smois_first=0
 endif
 if(run%st%kind/=start_lean)then
  do i=1,ncells
   sst(i)=run%cold%cell(i)%sst
   xice(i)=run%cold%cell(i)%xice
   vegetation(i)=run%cold%cell(i)%vegfra
  enddo
 endif
 ! MPAS holds uncomputed fields from the input stream, else their Registry defaults.
 status=nc_open(f,run%cold%init_path)
 if(status/=NC_NOERR)then
  call log_fatal('Cannot open the init file to read the fields the output holds at their input values: '// &
                 trim(run%cold%init_path))
  error stop 'init file open'
 endif
 do v=1,N_VARS
  name=VAR_NAME(v)
  if(field_class(name)/=SOURCE_HELD)then
   if(run%st%kind==start_lean.or..not.input_at_start(name))cycle
  endif
  if(elements(v)>ncells)cycle
  definition=VAR_DEFAULT(v)
  read(definition,*,iostat=ios)initial(1,held_slot(v))
  initial(:,held_slot(v))=initial(1,held_slot(v))
  if(nc_inq_var(f,trim(name))==NC_NOERR)then
   if(VAR_TYPE(v)==CAT_INTEGER)then
    status=nc_get(f,trim(name),buffer%i(:ncells),record=1)
    initial(:,held_slot(v))=real(buffer%i(:ncells))
   else
    status=nc_get(f,trim(name),initial(:,held_slot(v)),record=1)
   endif
   if(status/=NC_NOERR)error stop 'held output field read'
  endif
 enddo
 status=nc_close(f)
 if(run%st%kind/=start_lean)then
  allocate(start_winds(nz,ncells,2),water_temperature(4,ncells))
  call reconstruct_winds(fc%u1,m%coeffs_reconstruct,m%edgesOnCell,m%nEdgesOnCell,m%latCell,m%lonCell, &
   start_winds(:,:,1),start_winds(:,:,2))
  do i=1,ncells
   water_temperature(:,i)=run%cold%cell(i)%tslb
  enddo
 endif
 ! MPAS reads the ozone tables in its physics initialisation only (mpas_atm_core.F:580-596): with no physics pin and ozmixm are 0
 if(moist_physics)then
  call initialize_ozone_tables('.',ozone_pressure)
 else
  ozone_pressure=0.
 endif
 call global_attributes(cfg,run%cold%attributes,globals)
 end subroutine

 subroutine output_needs(set,need)
 type(stream_set), intent(in) :: set
 logical, intent(out) :: need(N_FETCH)
 integer :: i,j,source
 character(len=32) :: name
 need=.false.
 do i=1,set%n
  if(.not.set%s(i)%active)cycle
  if(set%s(i)%direction/=STREAM_OUTPUT.and.set%s(i)%direction/=STREAM_INPUT_OUTPUT)cycle
  if(trim(set%s(i)%name)=='restart')cycle
  do j=1,size(streams(i)%vars)
   name=VAR_NAME(streams(i)%vars(j))
   source=field_class(name)
   select case(source)
   case(SOURCE_STATE,SOURCE_OUTPUT_DIAG);need(FETCH_DYN)=.true.
   case(SOURCE_DIAG)
    select case(name)
    case('divergence','ke','vorticity')
     need(FETCH_DYN)=.true.;need(FETCH_DIAG_DYN)=.true.
    case('cuprec','cd','cda','ck','cka');need(FETCH_CONV)=.true.
    case('precipw');need(FETCH_PRECIPW)=.true.
    case('olrtoa');need(FETCH_RAD)=.true.
    case('cape','cin','srh_0_1km','srh_0_3km', &
     'uzonal_surface','uzonal_1km','uzonal_6km','umeridional_surface', &
     'umeridional_1km','umeridional_6km','temperature_surface','dewpoint_surface')
     need(FETCH_DYN)=.true.;need(FETCH_DIAG_DYN)=.true.
    case('mslp', 'relhum_50hPa', 'relhum_100hPa', 'relhum_200hPa', 'relhum_250hPa', &
   'relhum_500hPa', 'relhum_700hPa', 'relhum_850hPa', 'relhum_925hPa', 'dewpoint_50hPa', &
   'dewpoint_100hPa', 'dewpoint_200hPa', 'dewpoint_250hPa', 'dewpoint_500hPa', 'dewpoint_700hPa', &
   'dewpoint_850hPa', 'dewpoint_925hPa', 'temperature_50hPa', 'temperature_100hPa', 'temperature_200hPa', &
   'temperature_250hPa', 'temperature_500hPa', 'temperature_700hPa', 'temperature_850hPa', 'temperature_925hPa', &
   'height_50hPa', 'height_100hPa', 'height_200hPa', 'height_250hPa', 'height_500hPa', &
   'height_700hPa', 'height_850hPa', 'height_925hPa', 'uzonal_50hPa', 'uzonal_100hPa', &
   'uzonal_200hPa', 'uzonal_250hPa', 'uzonal_500hPa', 'uzonal_700hPa', 'uzonal_850hPa', &
   'uzonal_925hPa', 'umeridional_50hPa', 'umeridional_100hPa', 'umeridional_200hPa', 'umeridional_250hPa', &
   'umeridional_500hPa', 'umeridional_700hPa', 'umeridional_850hPa', 'umeridional_925hPa', 'w_50hPa', &
   'w_100hPa', 'w_200hPa', 'w_250hPa', 'w_500hPa', 'w_700hPa', &
   'w_850hPa', 'w_925hPa', 'vorticity_50hPa', 'vorticity_100hPa', 'vorticity_200hPa', &
   'vorticity_250hPa', 'vorticity_500hPa', 'vorticity_700hPa', 'vorticity_850hPa', 'vorticity_925hPa', &
   't_isobaric', 'z_isobaric', 'meanT_500_300')
     need(FETCH_DYN)=.true.;need(FETCH_DIAG_DYN)=.true.
    end select
   case(SOURCE_LEAN)
    select case(name)
    case('uReconstructZonal','uReconstructMeridional');need(FETCH_DYN)=.true.
    case('surface_pressure','rainnc');need(FETCH_MICRO)=.true.
    case('tslb','smois','sh2o','q2mvxy','q2mbxy','chvxy','chbxy')
     need(FETCH_LAND)=.true.;need(FETCH_STATE)=.true.
    case('gsw','glw','snow','sfc_albedo','sfc_emiss');need(FETCH_RAD)=.true.
    case('kpbl','hpbl','hfx','qfx','lh','u10','v10','q2','t2m','th2m','xland','skintemp','snowh','cldfrac_bl')
     need(FETCH_STATE)=.true.
    end select
   end select
  enddo
 enddo
 end subroutine

 subroutine advance_cf_time()
 cf_time=cf_time+dt
 end subroutine
 subroutine set_vegfra(c,percent)
 integer,intent(in) :: c
 real,intent(in) :: percent
 vegetation(c)=percent
 end subroutine

 ! Fields lean computes that MPAS has not yet computed at the start write: its pools still hold the input file's values
 ! there (mpas_atm_core.F:677-716 writes before the first step; surface_pressure is first set in the physics driver).
 logical function input_at_start(name)
 character(*),intent(in) :: name
 input_at_start=name=='u10'.or.name=='v10'.or.name=='q2'.or.name=='t2m'.or.name=='surface_pressure'
 end function

 subroutine gather_field(var,state,lands,run,clock,at_start,b)
 integer,intent(in) :: var
 type(state_type),intent(in) :: state
 type(noahmp_state_type),intent(in) :: lands
 type(run_inputs),intent(in) :: run
 type(run_clock),intent(in) :: clock
 logical,intent(in) :: at_start
 type(field_buffer),intent(inout) :: b
 type(nc_file) :: f
 real :: monthly(12,59)
 integer :: status,source,c,k
 character(len=32) :: name
 name=VAR_NAME(var);source=field_class(name)
 b%kind=VAR_TYPE(var);b%n=elements(var);b%fill=.false.;b%text=''
 select case(source)
 case(SOURCE_HELD)
  if(held_slot(var)>0)then
   b%r(:b%n)=initial(:b%n,held_slot(var))
  else
   ! Unforced aerosol constituents remain at their input or Registry default (Registry.xml:1752-1791).
   b%r(:b%n)=0.
   status=nc_open(f,run%cold%init_path)
   if(status/=NC_NOERR)then
    call log_fatal('Cannot open the init file to read the fields the output holds at their input values: '// &
                   trim(run%cold%init_path))
    error stop 'init file open'
   endif
   if(nc_inq_var(f,trim(name))==NC_NOERR)status=nc_get(f,trim(name),b%r(:b%n),record=1)
   status=nc_close(f)
  endif
  if(b%kind==CAT_INTEGER)b%i(:b%n)=int(b%r(:b%n))
  return
 case(SOURCE_MESH)
  status=nc_open(f,run%cold%mesh_path)
  if(b%kind==CAT_REAL)status=nc_get(f,trim(name),b%r(:b%n))
  if(b%kind==CAT_INTEGER)status=nc_get(f,trim(name),b%i(:b%n))
  if(status/=NC_NOERR)error stop 'output mesh field read'
  status=nc_close(f)
  return
 case(SOURCE_DIAG)
  call diag_fetch(VAR_NAME(var),b%r(:b%n))
  return
 end select
 ! surface_pressure is first set by the physics driver (mpas_atmphys_interface.F:436,959): with no physics it stays the input's
 if((at_start.or.(name=='surface_pressure'.and..not.moist_physics)).and.input_at_start(name))then
  b%r(:b%n)=initial(:b%n,held_slot(var))
  return
 endif
 select case(name)
 case('u')
  if(at_start)then
  b%r(:b%n)=reshape(fc%u1(:,1:nedges),[b%n])
  else
  b%r(:b%n)=reshape(fc%u2(:,1:nedges),[b%n])
  endif
 case('w')
  if(at_start)then
  b%r(:b%n)=reshape(fc%w1(:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%w2(:,1:ncells),[b%n])
  endif
 case('rho_zz')
  if(at_start)then
  b%r(:b%n)=reshape(fc%rho_zz1(:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%rho_zz2(:,1:ncells),[b%n])
  endif
 case('theta_m')
  if(at_start)then
  b%r(:b%n)=reshape(fc%theta_m1(:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%theta_m2(:,1:ncells),[b%n])
  endif
 case('theta')
  if(at_start)then
  b%r(:b%n)=reshape(fc%theta_m1(:,1:ncells)/(1.+rv/rgas*fc%scalars1(i_qv,:,1:ncells)),[b%n])
  else
  b%r(:b%n)=reshape(fc%theta_m2(:,1:ncells)/(1.+rv/rgas*fc%scalars2(i_qv,:,1:ncells)),[b%n])
  endif
 case('rho')
  if(at_start)then
  b%r(:b%n)=reshape(fc%rho_zz1(:,1:ncells)*m%zz(:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%rho_zz2(:,1:ncells)*m%zz(:,1:ncells),[b%n])
  endif
 case('pressure')
  b%r(:b%n)=reshape(s%pressure_base(:,1:ncells)+fc%pressure_p(:,1:ncells),[b%n])
 case('surface_pressure')
  b%r(:b%n)=cmp%surface_pressure(1:ncells)
 case('uReconstructZonal')
  if(at_start)then
   b%r(:b%n)=reshape(start_winds(:,:,1),[b%n])
  else
   b%r(:b%n)=reshape(fc%u_zonal(:,1:ncells),[b%n])
  endif
 case('uReconstructMeridional')
  if(at_start)then
   b%r(:b%n)=reshape(start_winds(:,:,2),[b%n])
  else
   b%r(:b%n)=reshape(fc%u_meridional(:,1:ncells),[b%n])
  endif
 case('rainnc')
  b%r(:b%n)=cmp%rainnc(1:ncells)
 case('snow')
  b%r(:b%n)=snow
 case('sfc_albedo')
  b%r(:b%n)=albedo
 case('sfc_emiss')
  b%r(:b%n)=emiss
 case('sst')
  b%r(:b%n)=sst
 case('xice')
  b%r(:b%n)=xice
 case('vegfra')
  b%r(:b%n)=vegetation
 case('gsw')
  b%r(:b%n)=rad%sw_net
 case('glw')
  b%r(:b%n)=rad%lw_down
 case('tslb')
  ! without a land-surface scheme MPAS's soil fields are the input's throughout (no Noah-MP, no sea-ice or glacier columns)
  b%r(:b%n)=reshape(lands%energy_state_temperaturesoilsnow(1:4,:),[b%n])
  do c=1,ncells
   k=4*(c-1)
   if(lsm_on.and.glacier_of(c)>0)then
    b%r(k+1:k+4)=glacier(glacier_of(c))%tslb(1,:)
   elseif((lsm_on.or.tslb_by_update).and.ice_cell(c))then
    b%r(k+1:k+4)=sea_ice_cells%tslb(:,c)
   elseif(state%xland(c)>1.5)then
    ! MPAS's Noah-MP driver sets the soil temperature of open water to 273.16 in its first step
    ! (physics_noahmp/drivers/mpas/NoahmpDriverMainMod.F90:135-144); the start record still holds the input's.
    if(.not.tslb_by_update.and.(at_start.or..not.lsm_on))then
     b%r(k+1:k+4)=water_temperature(:,c)
    else
     b%r(k+1:k+4)=273.16
    endif
   endif
  enddo
 case('sh2o')
  ! NoahmpInitMainMod.F90:77-111 sets the start record; NoahmpDriverMainMod.F90:161-165 sets sea ice to 1 each step.
  b%r(:b%n)=reshape(lands%water_state_soilliqwater,[b%n])
  do c=1,ncells
   k=4*(c-1)
   if(lsm_on.and.glacier_of(c)>0)then
    b%r(k+1:k+4)=glacier(glacier_of(c))%sh2o(1,:)
   elseif(state%xland(c)>1.5.or.(lsm_on.and..not.at_start.and.xice(c)>=xice_threshold))then
    b%r(k+1:k+4)=1.
   else
    b%r(k+1:k+4)=lands%water_state_soilliqwater(:,c)
   endif
  enddo
 case('smois')
  ! NoahmpDriverMainMod.F90:135-153 sets the soil moisture of the cells fully iced on the first step to 1 (smois_first); the
  ! sea-ice driver writes no soil water, so the value stays when the fraction changes (and an update's own 1.0 is in lands).
  b%r(:b%n)=reshape(lands%water_state_soilmoisture,[b%n])
  do c=1,ncells
   k=4*(c-1)
   if(lsm_on.and.glacier_of(c)>0)then
    b%r(k+1:k+4)=glacier(glacier_of(c))%smois(1,:)
   elseif(state%xland(c)>1.5.or.(lsm_on.and..not.at_start.and.smois_first(c)==1))then
    b%r(k+1:k+4)=1.
   else
    b%r(k+1:k+4)=lands%water_state_soilmoisture(:,c)
   endif
  enddo
 case('qv')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qv,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qv,:,1:ncells),[b%n])
  endif
 case('qc')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qc,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qc,:,1:ncells),[b%n])
  endif
 case('qr')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qr,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qr,:,1:ncells),[b%n])
  endif
 case('qi')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qi,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qi,:,1:ncells),[b%n])
  endif
 case('qs')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qs,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qs,:,1:ncells),[b%n])
  endif
 case('qg')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_qg,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_qg,:,1:ncells),[b%n])
  endif
 case('ni')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_ni,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_ni,:,1:ncells),[b%n])
  endif
 case('nr')
  if(at_start)then
  b%r(:b%n)=reshape(fc%scalars1(i_nr,:,1:ncells),[b%n])
  else
  b%r(:b%n)=reshape(fc%scalars2(i_nr,:,1:ncells),[b%n])
  endif
 case('hpbl')
  b%r(:b%n)=reshape(state%pblh,[b%n])
 case('skintemp')
  b%r(:b%n)=reshape(state%tsk,[b%n])
 case('t2m')
  b%r(:b%n)=reshape(state%t2,[b%n])
 case('th2m')
  b%r(:b%n)=reshape(state%th2,[b%n])
 case('cldfrac_bl')
  b%r(:b%n)=reshape(state%cldfra_bl,[b%n])
 case('hfx')
  b%r(:b%n)=reshape(state%hfx,[b%n])
 case('qfx')
  b%r(:b%n)=reshape(state%qfx,[b%n])
 case('lh')
  b%r(:b%n)=reshape(state%lh,[b%n])
 case('u10')
  b%r(:b%n)=reshape(state%u10,[b%n])
 case('v10')
  b%r(:b%n)=reshape(state%v10,[b%n])
 case('q2')
  b%r(:b%n)=reshape(state%q2,[b%n])
 case('xland')
  b%r(:b%n)=reshape(state%xland,[b%n])
 case('snowh')
  b%r(:b%n)=reshape(state%snowh,[b%n])
 case('q2mvxy')
  b%r(:b%n)=lands%energy_state_spechumidity2mveg/(1.-lands%energy_state_spechumidity2mveg)
 case('q2mbxy')
  b%r(:b%n)=lands%energy_state_spechumidity2mbare/(1.-lands%energy_state_spechumidity2mbare)
 case('chvxy')
  b%r(:b%n)=lands%energy_state_exchcoeffsh2mveg
 case('chbxy')
  b%r(:b%n)=lands%energy_state_exchcoeffsh2mbare
 case('kpbl')
  b%i(:b%n)=state%kpbl
 case('xtime')
  b%text=time_string(clock%now)
 case('initial_time')
  b%text=run%cold%initial_time
 case('Time')
  b%r(1)=cf_time
 case('mminlu')
  b%text=run%cold%mminlu
 case('isice_lu')
  b%i(1)=run%cold%isice_lu
 case('iswater_lu')
  b%i(1)=run%cold%iswater_lu
 case('pin')
  b%r(:b%n)=ozone_pressure
 case('ozmixm')
  do c=1,ncells
   if(moist_physics)then
    call monthly_ozone(m%latCell(c),monthly)
    b%r((c-1)*708+1:c*708)=reshape(monthly,[708])
   else
    b%r((c-1)*708+1:c*708)=0.
   endif
  enddo
 case('latCell', 'lonCell', 'xCell', 'yCell', 'zCell', &
   'indexToCellID', 'latEdge', 'lonEdge', 'xEdge', 'yEdge', &
   'zEdge', 'indexToEdgeID', 'latVertex', 'lonVertex', 'xVertex', &
   'yVertex', 'zVertex', 'indexToVertexID', 'cellsOnEdge', 'nEdgesOnCell', &
   'nEdgesOnEdge', 'edgesOnCell', 'edgesOnEdge', 'weightsOnEdge', 'dvEdge', &
   'dcEdge', 'angleEdge', 'areaCell', 'areaTriangle', 'cellsOnCell', &
   'verticesOnCell', 'verticesOnEdge', 'edgesOnVertex', 'cellsOnVertex', 'kiteAreasOnVertex', &
   'meshDensity', 'zgrid', 'fzm', 'fzp', 'zz')
  error stop 'field class dispatch'
 case('i_rainnc', 'i_rainc', 't_oml', 't_oml_initial', 't_oml_200m_initial', &
   'h_oml', 'h_oml_initial', 'hu_oml', 'hv_oml', 'lcl', &
   'lfc')
  error stop 'field class dispatch'
 case('refl10cm', 'relhum', 'divergence', 'vorticity', 'ke', 'precipw', &
   'cuprec', 'rainc', 'cd', 'cda', 'ck', &
   'cka', 'acsnow', 'mslp', 'relhum_50hPa', 'relhum_100hPa', &
   'relhum_200hPa', 'relhum_250hPa', 'relhum_500hPa', 'relhum_700hPa', 'relhum_850hPa', &
   'relhum_925hPa', 'dewpoint_50hPa', 'dewpoint_100hPa', 'dewpoint_200hPa', 'dewpoint_250hPa', &
   'dewpoint_500hPa', 'dewpoint_700hPa', 'dewpoint_850hPa', 'dewpoint_925hPa', 'temperature_50hPa', &
   'temperature_100hPa', 'temperature_200hPa', 'temperature_250hPa', 'temperature_500hPa', 'temperature_700hPa', &
   'temperature_850hPa', 'temperature_925hPa', 'height_50hPa', 'height_100hPa', 'height_200hPa', &
   'height_250hPa', 'height_500hPa', 'height_700hPa', 'height_850hPa', 'height_925hPa', &
   'uzonal_50hPa', 'uzonal_100hPa', 'uzonal_200hPa', 'uzonal_250hPa', 'uzonal_500hPa', &
   'uzonal_700hPa', 'uzonal_850hPa', 'uzonal_925hPa', 'umeridional_50hPa', 'umeridional_100hPa', &
   'umeridional_200hPa', 'umeridional_250hPa', 'umeridional_500hPa', 'umeridional_700hPa', 'umeridional_850hPa', &
   'umeridional_925hPa', 'w_50hPa', 'w_100hPa', 'w_200hPa', 'w_250hPa', &
   'w_500hPa', 'w_700hPa', 'w_850hPa', 'w_925hPa', 'vorticity_50hPa', &
   'vorticity_100hPa', 'vorticity_200hPa', 'vorticity_250hPa', 'vorticity_500hPa', 'vorticity_700hPa', &
   'vorticity_850hPa', 'vorticity_925hPa', 't_isobaric', 't_iso_levels', 'z_isobaric', &
   'z_iso_levels', 'meanT_500_300', 'olrtoa', 'refl10cm_max', 'refl10cm_1km', &
   'refl10cm_1km_max', 'cape', 'cin', 'srh_0_1km', 'srh_0_3km', &
   'uzonal_surface', 'uzonal_1km', 'uzonal_6km', 'umeridional_surface', 'umeridional_1km', &
   'umeridional_6km', 'temperature_surface', 'dewpoint_surface', 'updraft_helicity_max', 'w_velocity_max', &
   'wind_speed_level1_max')
  error stop 'field class dispatch'
 case('sul','sslt','dust1','dust2','dust3','dust4', &
   'ocpho','bcpho','ocphi','bcphi','bg','volc')
  error stop 'held aerosol dispatch'
 case default
  error stop 'unknown output field'
 end select
 end subroutine

 subroutine write_streams(set,plan,clock,state,lands,run,at_start,msgs)
 type(stream_set),intent(in) :: set
 type(io_plan),intent(in) :: plan
 type(run_clock),intent(in) :: clock
 type(state_type),intent(in) :: state
 type(noahmp_state_type),intent(in) :: lands
 type(run_inputs),intent(in) :: run
 logical,intent(in) :: at_start
 type(message_list),intent(inout) :: msgs
 type(record_request) :: req
 type(open_record) :: rec
 character(len=:),allocatable :: path
 integer :: j,i,k,v,status
 status=WRITE_OK
 do j=1,size(plan%write)
  i=plan%write(j)
  if(trim(set%s(i)%name)=='restart')cycle
  if(size(streams(i)%vars)==0)cycle
  req%mesh_path=run%cold%mesh_path
  req%stream=set%s(i)%name
  if(.not.write_filename(set,i,clock%now,path,msgs))cycle
  req%path=path
  req%xtime=time_string(clock%now)
  req%time_units='seconds since '//run%cold%initial_time
  ! CF time units want 'YYYY-MM-DD hh:mm:ss': the '_' of MPAS's time string (character 25 of the units string)
  ! becomes a space
  req%time_units(25:25)=' '
  req%clobber=set%s(i)%clobber
  req%format=nc_io_type_format(trim(set%s(i)%io_type))
  req%precision=set%s(i)%precision
  if(req%precision==0)req%precision=4
  status=begin_record(streams(i)%file,req,streams(i)%vars,dims,lengths,globals,rec,msgs)
  if(status==WRITE_CLOBBER_FILE.or.status==WRITE_CLOBBER_RECORD)cycle
  if(status/=WRITE_OK)exit
  do k=1,size(streams(i)%vars)
   v=streams(i)%vars(k)
   call gather_field(v,state,lands,run,clock,at_start,buffer)
   status=put_var(rec,v,buffer,req%precision)
   if(status/=WRITE_OK)exit
  enddo
  ! after a failed put, end_record still closes the file and logs MPAS's write-error message (lean_writer writer_failed)
  status=end_record(streams(i)%file,rec,msgs)
  if(status/=WRITE_OK)exit
 enddo
 if(status==WRITE_FAILED.and..not.has_fatal(msgs))then
  call add_message(msgs,LOG_ERR,repeat('*',80))
  call add_message(msgs,LOG_ERR,'Error writing one or more output streams')
  call add_message(msgs,LOG_CRIT,repeat('*',80))
 endif
 end subroutine
end module
