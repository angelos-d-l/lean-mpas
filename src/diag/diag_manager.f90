! MPAS diagnostics manager and physics_timetracker, for the configured suite.
module diag_manager
 use lean_clock
 use schemes, only: lsm_on, moist_physics
 use lean_messages
 use lean_streams
 use lean_stream_registry, only: N_FIELDS, field_index, FIELD_NAME
 use config, only: nz, ncells, nvertices, dt
 use dyn_step, only: dyn_grid, dyn_fields, rk_setup, stage_diagnostics
 use model_data, only: dg=>g, df=>fc, mp=>cmp, rad
 use model_state, only: state_type
 use gf_state, only: gf_state_type
 use noahmp_state, only: noahmp_state_type
 use initial_winds, only: reconstruct_winds, reconstruct_winds_trig
 use land_ice_exchange, only: glacier_of, exchange, glacier_exchange
 use sea_ice, only: sea_ice_cells=>ice, xice_threshold
 use phys_mode, only: card_thompson
 use dyn_micro, only: micro_film_start,micro_film_to_host,micro_fields
 use diag_convective, only: maxima_order,maxima_update,maxima_reset,convective_out,convective_compute
 use diag_isobaric
 use diag_micro, only: micro_diagnostics
 use lean_inputs, only: cold_inputs
 implicit none
 private
 public :: uh_max,w_max,wsp_max,refl10cm_1km_max,diag_after_micro
 public :: diag_bind, diag_rain, diag_snow, snow_total
 public :: diag_on, need_uh_max, need_w_max, need_wsp_max, need_rainc, need_acsnow, need_micro, l_diags, sum_pw
 public :: diag_resume, diag_restore_device
 public :: diag_setup, diag_start, diag_start_step, diag_written, field_will_be_written
 public :: diag_update, diag_compute, diag_reset, diag_fetch, diag_to_host
 public :: rainc, acsnow, relhum, refl10cm, refl10cm_max, refl10cm_1km
 logical, protected, save :: diag_on=.false., need_uh_max=.false., need_w_max=.false., need_wsp_max=.false.
 logical, protected, save :: need_rainc=.false., need_acsnow=.false., need_micro=.false., l_diags=.false., sum_pw=.false.
 logical, save :: included(N_FIELDS)=.false., written(N_FIELDS)=.false.
 real, allocatable, save :: rainc(:), acsnow(:)
 real, allocatable, save :: uh_max(:),w_max(:),wsp_max(:)
 real, allocatable, save :: relhum(:,:),refl10cm(:,:),refl10cm_max(:),refl10cm_1km(:),refl10cm_1km_max(:)
 type(convective_out),save :: conv_out
 logical,save :: need_convective=.false.
 type(isobaric_out),save :: iso
 logical,save :: need_iso=.false.
 integer,allocatable,save :: vertex_order(:,:)
 logical,save :: rainc_card=.false.,snow_card=.false.
 type(state_type),pointer,save :: air=>null()
 type(gf_state_type),pointer,save :: conv=>null()
contains
 subroutine diag_bind(state,convection)
 type(state_type),target,intent(in)::state
 type(gf_state_type),target,intent(in)::convection
 air=>state;conv=>convection
 end subroutine
 subroutine diag_setup(set,clock,msgs)
 type(stream_set),intent(in) :: set
 type(run_clock),intent(inout) :: clock
 type(message_list),intent(inout) :: msgs
 integer :: i,j,k,interval_stream
 type(ESMF_TimeInterval) :: interval
 character(11) :: name
 character(1024) :: value
 included=.false.;written=.false.;l_diags=.false.;sum_pw=.false.
 ! mpas_atm_diagnostics_utils.F:213-216: input;output streams count too.
 do i=1,set%n
  ! The lean restart saves allocated diagnostics. Its Registry fields do not request them.
  if(set%s(i)%name=='restart')cycle
  if(.not.stream_active(set,i).or..not.set%s(i)%has_output_interval)cycle
  if(set%s(i)%direction/=STREAM_OUTPUT.and.set%s(i)%direction/=STREAM_INPUT_OUTPUT)cycle
  do j=1,set%s(i)%n_fields
   k=set%s(i)%field(j)%field
   if(stream_has_field(set,i,trim(FIELD_NAME(k))))included(k)=.true.
  enddo
 enddo
 need_uh_max=wanted('updraft_helicity_max');need_w_max=wanted('w_velocity_max')
 need_wsp_max=wanted('wind_speed_level1_max');need_rainc=wanted('rainc');need_acsnow=wanted('acsnow')
 call isobaric_needs(included)
 need_iso=any(iso%need).or.wanted('t_iso_levels').or.wanted('z_iso_levels')
 call convective_needs(included)
 need_convective=any(conv_out%need)
 need_micro=iso%need(2).or.wanted('relhum').or.wanted('refl10cm').or.wanted('refl10cm_max').or. &
  wanted('refl10cm_1km').or.wanted('refl10cm_1km_max')
 diag_on=need_convective.or.need_iso.or.need_uh_max.or.need_w_max.or.need_wsp_max.or.need_rainc.or.need_acsnow.or.need_micro.or. &
  wanted('precipw').or.wanted('olrtoa').or.wanted('cuprec').or.wanted('cd').or.wanted('cda').or. &
  wanted('ck').or.wanted('cka').or.wanted('divergence').or.wanted('ke').or.wanted('vorticity')
 ! physics/mpas_atmphys_manager.F: diag alarm follows these two streams by name.
 interval_stream=0
 do j=1,2
  name='output'
  if(j==2)name='diagnostics'
  i=stream_index(set,trim(name))
  value='missing'
  if(i/=0)then
   value=set%s(i)%raw_output
   if(set%s(i)%has_output_interval)then
    if(interval_stream==0)then
     interval=set%s(i)%output_interval
     interval_stream=i
    else if(set%s(i)%output_interval<interval)then
     interval=set%s(i)%output_interval
     interval_stream=i
    endif
    cycle
   endif
  endif
  call add_message(msgs,LOG_OUT,"stream '"//trim(name)//"' has no time interval ("//trim(value)// &
   '); additional physics diagnostics follow the other stream')
 enddo
 if((need_uh_max.or.need_w_max.or.need_wsp_max).and..not.allocated(uh_max))then
  allocate(uh_max(ncells),w_max(ncells),wsp_max(ncells));uh_max=0.;w_max=0.;wsp_max=0.
  !$acc enter data copyin(uh_max,w_max,wsp_max)
 endif
 if(need_micro.and..not.allocated(refl10cm))then
  if(.not.allocated(relhum))allocate(relhum(nz,ncells))
  allocate(refl10cm(nz,ncells),refl10cm_max(ncells),refl10cm_1km(ncells),refl10cm_1km_max(ncells))
  relhum=0.;refl10cm=0.;refl10cm_max=0.;refl10cm_1km=0.;refl10cm_1km_max=0.
 endif
 if(need_micro.and.allocated(mp%film_in))then
  if(size(mp%film_in,3)==1)call micro_film_start(mp)
 endif
 if(need_rainc.and..not.allocated(rainc))then
  allocate(rainc(ncells));rainc=0.
  !$acc enter data copyin(rainc)
 endif
 if(need_acsnow.and..not.allocated(acsnow))then
  allocate(acsnow(ncells));acsnow=0.
  !$acc enter data copyin(acsnow)
 endif
 if(need_iso.and..not.allocated(iso%cell))call isobaric_allocate(iso,ncells,nvertices,nz)
 if(need_iso.and..not.allocated(relhum))then
  allocate(relhum(nz,ncells));relhum=0.
 endif
 if(need_convective.and..not.allocated(conv_out%values))allocate(conv_out%values(12,ncells))
 if(interval_stream>0)then
  call add_alarm(clock,'diag',clock%start,msgs,interval)
  call set_alarm_previous(clock,'diag',alarm_previous(clock,set%s(interval_stream)%output_alarm))
 endif
 end subroutine
 logical function wanted(name)
 character(*),intent(in)::name
 integer :: k
 k=field_index(name)
 wanted=.false.
 if(k>0)wanted=included(k)
 end function
 subroutine diag_start(g,fc,inp)
 type(dyn_grid),intent(in)::g
 type(dyn_fields),intent(inout)::fc
 type(cold_inputs),intent(in)::inp
 if(.not.diag_on)return
 !$omp parallel
#ifdef _OPENACC
 call reconstruct_winds_trig(fc%u1,g%m%coeffs_reconstruct,g%m%edgesOnCell,g%m%nEdgesOnCell, &
  g%cos_lat,g%sin_lat,g%cos_lon,g%sin_lon,fc%u_zonal,fc%u_meridional)
#else
 call reconstruct_winds(fc%u1,g%m%coeffs_reconstruct,g%m%edgesOnCell,g%m%nEdgesOnCell, &
  g%m%latCell,g%m%lonCell,fc%u_zonal,fc%u_meridional)
#endif
 call rk_setup(fc)
 call stage_diagnostics(g,fc,.true.)
 !$acc wait(1)
 !$omp end parallel
 if((need_micro.or.need_iso).and.allocated(inp%relhum))relhum=inp%relhum(:,:ncells)
 call diag_resume(g)
 if(allocated(inp%precipw))mp%precipw=inp%precipw
 if(card_thompson)then
  !$acc update device(mp%precipw)
 endif
 end subroutine
 subroutine diag_resume(g)
 type(dyn_grid),intent(in)::g
 if(need_uh_max.or.need_w_max.or.need_wsp_max)then
  allocate(vertex_order(size(g%m%verticesOnCell,1),size(g%m%verticesOnCell,2)))
  call maxima_order(g%m%verticesOnCell,g%m%nEdgesOnCell,g%kite,vertex_order)
  !$acc enter data copyin(vertex_order)
  call maxima_geometry_to_card(g%m%zgrid,g%m%areaCell)
 endif
 end subroutine
 subroutine diag_restore_device()
 if(allocated(uh_max))then
  !$acc update device(uh_max,w_max,wsp_max)
 endif
 if(allocated(rainc))then
  !$acc update device(rainc)
 endif
 if(allocated(acsnow))then
  !$acc update device(acsnow)
 endif
 end subroutine
 subroutine maxima_geometry_to_card(zgrid,area)
 ! The maxima kernel reads the heights and the cell areas on the card. Only the storm stream put them there
 ! (storm_output.f90:19), so a run with streams and no LEAN_STORM stopped: "data in PRESENT clause was not found".
 ! A second copyin of data that is already there only counts a reference.
 real,intent(in)::zgrid(:,:),area(:)
 !$acc enter data copyin(zgrid,area)
 end subroutine
 subroutine diag_start_step(set,clock)
 type(stream_set),intent(in)::set
 type(run_clock),intent(inout)::clock
 integer :: i
 ! physics/mpas_atmphys_manager.F:366-373: reset at now, then test within dt.
 if(alarm_ringing(clock,'diag'))call reset_alarm(clock,'diag')
 l_diags=alarm_ringing(clock,'diag',within=clock%step)
 sum_pw=.false.
 if(.not.wanted('precipw'))return
 do i=1,set%n
  if(.not.stream_active(set,i).or..not.set%s(i)%has_output_interval)cycle
  if(set%s(i)%direction/=STREAM_OUTPUT.and.set%s(i)%direction/=STREAM_INPUT_OUTPUT)cycle
  if(.not.stream_has_field(set,i,'precipw'))cycle
  if(alarm_ringing(clock,set%s(i)%output_alarm,within=clock%step))sum_pw=.true.
 enddo
 end subroutine
 subroutine diag_written(set,plan)
 type(stream_set),intent(in)::set
 type(io_plan),intent(in)::plan
 integer :: i,j,k,s
 written=.false.
 do i=1,size(plan%write)
  s=plan%write(i)
  if(.not.stream_active(set,s))cycle
  do j=1,set%s(s)%n_fields
   k=set%s(s)%field(j)%field
   if(stream_has_field(set,s,trim(FIELD_NAME(k))))written(k)=.true.
  enddo
 enddo
 end subroutine
 logical function field_will_be_written(name) result(will)
 character(*),intent(in)::name
 integer :: k
 k=field_index(name);will=.false.
 if(k>0)will=written(k)
 end function
 subroutine diag_update(level)
 integer,intent(in)::level
 if(.not.(need_uh_max.or.need_w_max.or.need_wsp_max))return
 if(level==1)then
  call maxima_update(df%w1,df%vorticity,df%u_zonal,df%u_meridional,dg%m%zgrid,dg%m%areaCell, &
   dg%m%verticesOnCell,vertex_order,dg%kite,dg%m%kiteAreasOnVertex,dg%m%nEdgesOnCell, &
   uh_max,w_max,wsp_max,need_uh_max,need_w_max,need_wsp_max)
 else
  call maxima_update(df%w2,df%vorticity,df%u_zonal,df%u_meridional,dg%m%zgrid,dg%m%areaCell, &
   dg%m%verticesOnCell,vertex_order,dg%kite,dg%m%kiteAreasOnVertex,dg%m%nEdgesOnCell, &
   uh_max,w_max,wsp_max,need_uh_max,need_w_max,need_wsp_max)
 endif
 end subroutine
 subroutine diag_to_host()
 if(allocated(uh_max))then
  !$acc update self(uh_max,w_max,wsp_max)
 endif
 if(allocated(rainc).and.rainc_card)then
  !$acc update self(rainc)
 endif
 if(allocated(acsnow).and.snow_card)then
  !$acc update self(acsnow)
 endif
 end subroutine

 subroutine diag_compute(level)
 integer,intent(in)::level
 if(field_will_be_written('updraft_helicity_max'))then
  !$acc update self(uh_max)
 endif
 if(field_will_be_written('w_velocity_max'))then
  !$acc update self(w_max)
 endif
 if(field_will_be_written('wind_speed_level1_max'))then
  !$acc update self(wsp_max)
 endif
 if(field_will_be_written('rainc').and.rainc_card)then
  !$acc update self(rainc)
 endif
 if(field_will_be_written('acsnow').and.snow_card)then
  !$acc update self(acsnow)
 endif
 if(need_iso)then
  call isobaric_needs(written)
  if(any(iso%need))then
   if(level==1)then
    call isobaric_compute(df%theta_m1,df%scalars1,df%exner,df%pressure_p,dg%s%pressure_base,dg%m%zgrid,df%w1, &
     df%u_zonal,df%u_meridional,relhum,df%vorticity,dg%m%cellsOnVertex(:,:nvertices), &
     dg%m%kiteAreasOnVertex(:,:nvertices),dg%m%areaTriangle(:nvertices),iso)
   else
    call isobaric_compute(df%theta_m2,df%scalars2,df%exner,df%pressure_p,dg%s%pressure_base,dg%m%zgrid,df%w2, &
     df%u_zonal,df%u_meridional,relhum,df%vorticity,dg%m%cellsOnVertex(:,:nvertices), &
     dg%m%kiteAreasOnVertex(:,:nvertices),dg%m%areaTriangle(:nvertices),iso)
   endif
  endif
 endif
 if(need_convective)then
  call convective_needs(written)
  if(any(conv_out%need))then
   if(level==1)then
    call convective_compute(df%theta_m1,df%scalars1,df%exner,df%pressure_p,dg%s%pressure_base, &
     dg%m%zgrid,df%u_zonal,df%u_meridional,conv_out)
   else
    call convective_compute(df%theta_m2,df%scalars2,df%exner,df%pressure_p,dg%s%pressure_base, &
     dg%m%zgrid,df%u_zonal,df%u_meridional,conv_out)
   endif
  endif
 endif
 end subroutine
 subroutine convective_needs(mask)
 logical,intent(in)::mask(:)
 conv_out%need(1)=mask(field_index('cape'))
 conv_out%need(2)=mask(field_index('cin'))
 conv_out%need(3)=mask(field_index('srh_0_1km'))
 conv_out%need(4)=mask(field_index('srh_0_3km'))
 conv_out%need(5)=mask(field_index('uzonal_surface'))
 conv_out%need(6)=mask(field_index('uzonal_1km'))
 conv_out%need(7)=mask(field_index('uzonal_6km'))
 conv_out%need(8)=mask(field_index('umeridional_surface'))
 conv_out%need(9)=mask(field_index('umeridional_1km'))
 conv_out%need(10)=mask(field_index('umeridional_6km'))
 conv_out%need(11)=mask(field_index('temperature_surface'))
 conv_out%need(12)=mask(field_index('dewpoint_surface'))
 end subroutine
 subroutine isobaric_needs(mask)
 logical,intent(in)::mask(:)
 iso%need=.false.
 iso%need(1)=mask(field_index('temperature_50hPa')).or.mask(field_index('temperature_100hPa')).or.mask(field_index('temperature_200hPa')).or.mask(field_index('temperature_250hPa')).or.mask(field_index('temperature_500hPa')).or.mask(field_index('temperature_700hPa')).or.mask(field_index('temperature_850hPa')).or.mask(field_index('temperature_925hPa'))
 iso%need(2)=mask(field_index('relhum_50hPa')).or.mask(field_index('relhum_100hPa')).or.mask(field_index('relhum_200hPa')).or.mask(field_index('relhum_250hPa')).or.mask(field_index('relhum_500hPa')).or.mask(field_index('relhum_700hPa')).or.mask(field_index('relhum_850hPa')).or.mask(field_index('relhum_925hPa'))
 iso%need(3)=mask(field_index('dewpoint_50hPa')).or.mask(field_index('dewpoint_100hPa')).or.mask(field_index('dewpoint_200hPa')).or.mask(field_index('dewpoint_250hPa')).or.mask(field_index('dewpoint_500hPa')).or.mask(field_index('dewpoint_700hPa')).or.mask(field_index('dewpoint_850hPa')).or.mask(field_index('dewpoint_925hPa'))
 iso%need(4)=mask(field_index('uzonal_50hPa')).or.mask(field_index('uzonal_100hPa')).or.mask(field_index('uzonal_200hPa')).or.mask(field_index('uzonal_250hPa')).or.mask(field_index('uzonal_500hPa')).or.mask(field_index('uzonal_700hPa')).or.mask(field_index('uzonal_850hPa')).or.mask(field_index('uzonal_925hPa'))
 iso%need(5)=mask(field_index('umeridional_50hPa')).or.mask(field_index('umeridional_100hPa')).or.mask(field_index('umeridional_200hPa')).or.mask(field_index('umeridional_250hPa')).or.mask(field_index('umeridional_500hPa')).or.mask(field_index('umeridional_700hPa')).or.mask(field_index('umeridional_850hPa')).or.mask(field_index('umeridional_925hPa'))
 iso%need(6)=mask(field_index('height_50hPa')).or.mask(field_index('height_100hPa')).or.mask(field_index('height_200hPa')).or.mask(field_index('height_250hPa')).or.mask(field_index('height_500hPa')).or.mask(field_index('height_700hPa')).or.mask(field_index('height_850hPa')).or.mask(field_index('height_925hPa'))
 iso%need(7)=mask(field_index('w_50hPa')).or.mask(field_index('w_100hPa')).or.mask(field_index('w_200hPa')).or.mask(field_index('w_250hPa')).or.mask(field_index('w_500hPa')).or.mask(field_index('w_700hPa')).or.mask(field_index('w_850hPa')).or.mask(field_index('w_925hPa'))
 iso%need(8)=mask(field_index('vorticity_50hPa')).or.mask(field_index('vorticity_100hPa')).or.mask(field_index('vorticity_200hPa')).or.mask(field_index('vorticity_250hPa')).or.mask(field_index('vorticity_500hPa')).or.mask(field_index('vorticity_700hPa')).or.mask(field_index('vorticity_850hPa')).or.mask(field_index('vorticity_925hPa'))
 iso%need(9)=mask(field_index('mslp'))
 iso%need(10)=mask(field_index('t_isobaric'))
 iso%need(11)=mask(field_index('z_isobaric'))
 iso%need(12)=mask(field_index('meanT_500_300'))
 end subroutine
 subroutine diag_reset(diagnostics_rang)
 logical,optional,intent(in)::diagnostics_rang
 if(field_will_be_written('updraft_helicity_max'))call maxima_reset(uh_max)
 if(field_will_be_written('w_velocity_max'))call maxima_reset(w_max)
 if(field_will_be_written('wind_speed_level1_max'))call maxima_reset(wsp_max)
 if(present(diagnostics_rang))then
  if(diagnostics_rang.and.allocated(refl10cm_1km_max))refl10cm_1km_max=0.
 endif
 end subroutine
 subroutine diag_after_micro(mp)
 type(micro_fields),intent(inout)::mp
 ! the physics diagnostics (relhum, refl10cm) are the physics driver's: with no physics MPAS keeps the input's relhum
 if(.not.need_micro.or..not.l_diags.or..not.moist_physics)return
 if(card_thompson)call micro_film_to_host(mp)
 call micro_diagnostics(mp%film_in,dg%m%zgrid,refl10cm,refl10cm_max,refl10cm_1km,refl10cm_1km_max,relhum)
 end subroutine
 ! physics/mpas_atmphys_driver_convection.F: update_convection_step2.
 subroutine diag_rain(pratec,on_card)
 real,intent(in)::pratec(ncells)
 logical,intent(in)::on_card
 if(.not.need_rainc)return
 call rain_accumulate(rainc,pratec,ncells,dt,on_card)
 rainc_card=on_card
 end subroutine
 subroutine rain_accumulate(values,pratec,n,dt,on_card)
 integer,intent(in)::n
 real,intent(in)::pratec(n),dt
 real,intent(inout)::values(n)
 logical,intent(in)::on_card
 integer::c
 !$acc parallel loop gang vector default(present) if(on_card)
 do c=1,n
  values(c)=values(c)+pratec(c)*dt
 enddo
 end subroutine
 ! physics_noahmp/drivers/mpas/WaterVarOutTransferMod.F90:75.
 real function snow_total(total,rain,frozen) result(value)
 !$acc routine seq
 real,intent(in)::total,rain,frozen
 value=total+rain*frozen
 end function
 subroutine diag_snow(state,lands,on_card)
 type(state_type),intent(in)::state
 type(noahmp_state_type),intent(in)::lands
 logical,intent(in)::on_card
 ! acsnow is Noah-MP's accumulation of the land-surface block (mpas_atmphys_driver.F:283-317): no scheme, no accumulation
 if(.not.need_acsnow.or..not.lsm_on)return
 ! The land forcing retains the previous GF rain after GF writes this step's raincv.
 ! pre_dynamics.f90:87 and phys_dev.f90:328 set rain_convective before the land step.
 call snow_kernel(acsnow,state%rainncv,lands%rain_convective,state%xland,lands%water_state_frozenprecipfrac, &
  glacier_of,exchange,sea_ice_cells%xice,sea_ice_cells%acsnow,ncells,on_card)
 snow_card=on_card
 end subroutine
 subroutine snow_kernel(acsnow,rainncv,rainconv,xland,frozen,glacier_of,exchange,xice,ice_snow,n,on_card)
 integer,intent(in)::n,glacier_of(n)
 real,intent(inout)::acsnow(n)
 real,intent(in)::rainncv(n),rainconv(n),xland(n),frozen(n),xice(n),ice_snow(n)
 type(glacier_exchange),intent(in)::exchange(:)
 logical,intent(in)::on_card
 integer::c,j
 real::rainbl
 !$acc parallel loop gang vector default(present) private(rainbl,j) if(on_card)
 do c=1,n
  j=glacier_of(c)
  if(xland(c)<1.5)then
   ! mpas_atmphys_manager.F:739-744 sets the fractional ice threshold.
   ! mpas_atmphys_driver_seaice.F:259,420 carries the whole accumulation, including on partly iced cells.
   if(xice(c)>=xice_threshold)then
    acsnow(c)=ice_snow(c)
   else if(j>0)then
    acsnow(c)=exchange(j)%acsnow
   else
    rainbl=(0.+rainncv(c))+rainconv(c)
    acsnow(c)=snow_total(acsnow(c),rainbl,frozen(c))
   endif
  endif
 enddo
 end subroutine
 subroutine diag_fetch(name,values)
 character(*),intent(in)::name
 real,intent(out)::values(:)
 select case(trim(name))
 case('updraft_helicity_max');values=uh_max
 case('w_velocity_max');values=w_max
 case('wind_speed_level1_max');values=wsp_max
 case('relhum');values=reshape(relhum,[size(values)])
 case('refl10cm');values=reshape(refl10cm,[size(values)])
 case('refl10cm_max');values=refl10cm_max
 case('refl10cm_1km');values=refl10cm_1km
 case('refl10cm_1km_max');values=refl10cm_1km_max
 case('divergence');values=reshape(df%divergence(:,:ncells),[size(values)])
 case('ke');values=reshape(df%ke(:,:ncells),[size(values)])
 case('vorticity');values=reshape(df%vorticity(:,:nvertices),[size(values)])
 case('cuprec');values=conv%pratec
 case('cd');values=air%cd
 case('cda');values=air%cda
 case('ck');values=air%ck
 case('cka');values=air%cka
 case('precipw');values=mp%precipw(:ncells)
 case('olrtoa');values=rad%olr
 case('rainc');values=rainc
 case('acsnow');values=acsnow
 case('temperature_50hPa');values=iso%cell(1,:,1)
 case('temperature_100hPa');values=iso%cell(2,:,1)
 case('temperature_200hPa');values=iso%cell(3,:,1)
 case('temperature_250hPa');values=iso%cell(4,:,1)
 case('temperature_500hPa');values=iso%cell(5,:,1)
 case('temperature_700hPa');values=iso%cell(6,:,1)
 case('temperature_850hPa');values=iso%cell(7,:,1)
 case('temperature_925hPa');values=iso%cell(8,:,1)
 case('relhum_50hPa');values=iso%cell(1,:,2)
 case('relhum_100hPa');values=iso%cell(2,:,2)
 case('relhum_200hPa');values=iso%cell(3,:,2)
 case('relhum_250hPa');values=iso%cell(4,:,2)
 case('relhum_500hPa');values=iso%cell(5,:,2)
 case('relhum_700hPa');values=iso%cell(6,:,2)
 case('relhum_850hPa');values=iso%cell(7,:,2)
 case('relhum_925hPa');values=iso%cell(8,:,2)
 case('dewpoint_50hPa');values=iso%cell(1,:,3)
 case('dewpoint_100hPa');values=iso%cell(2,:,3)
 case('dewpoint_200hPa');values=iso%cell(3,:,3)
 case('dewpoint_250hPa');values=iso%cell(4,:,3)
 case('dewpoint_500hPa');values=iso%cell(5,:,3)
 case('dewpoint_700hPa');values=iso%cell(6,:,3)
 case('dewpoint_850hPa');values=iso%cell(7,:,3)
 case('dewpoint_925hPa');values=iso%cell(8,:,3)
 case('uzonal_50hPa');values=iso%cell(1,:,4)
 case('uzonal_100hPa');values=iso%cell(2,:,4)
 case('uzonal_200hPa');values=iso%cell(3,:,4)
 case('uzonal_250hPa');values=iso%cell(4,:,4)
 case('uzonal_500hPa');values=iso%cell(5,:,4)
 case('uzonal_700hPa');values=iso%cell(6,:,4)
 case('uzonal_850hPa');values=iso%cell(7,:,4)
 case('uzonal_925hPa');values=iso%cell(8,:,4)
 case('umeridional_50hPa');values=iso%cell(1,:,5)
 case('umeridional_100hPa');values=iso%cell(2,:,5)
 case('umeridional_200hPa');values=iso%cell(3,:,5)
 case('umeridional_250hPa');values=iso%cell(4,:,5)
 case('umeridional_500hPa');values=iso%cell(5,:,5)
 case('umeridional_700hPa');values=iso%cell(6,:,5)
 case('umeridional_850hPa');values=iso%cell(7,:,5)
 case('umeridional_925hPa');values=iso%cell(8,:,5)
 case('height_50hPa');values=iso%cell(1,:,6)
 case('height_100hPa');values=iso%cell(2,:,6)
 case('height_200hPa');values=iso%cell(3,:,6)
 case('height_250hPa');values=iso%cell(4,:,6)
 case('height_500hPa');values=iso%cell(5,:,6)
 case('height_700hPa');values=iso%cell(6,:,6)
 case('height_850hPa');values=iso%cell(7,:,6)
 case('height_925hPa');values=iso%cell(8,:,6)
 case('w_50hPa');values=iso%cell(1,:,7)
 case('w_100hPa');values=iso%cell(2,:,7)
 case('w_200hPa');values=iso%cell(3,:,7)
 case('w_250hPa');values=iso%cell(4,:,7)
 case('w_500hPa');values=iso%cell(5,:,7)
 case('w_700hPa');values=iso%cell(6,:,7)
 case('w_850hPa');values=iso%cell(7,:,7)
 case('w_925hPa');values=iso%cell(8,:,7)
 case('vorticity_50hPa');values=iso%vorticity(1,:)
 case('vorticity_100hPa');values=iso%vorticity(2,:)
 case('vorticity_200hPa');values=iso%vorticity(3,:)
 case('vorticity_250hPa');values=iso%vorticity(4,:)
 case('vorticity_500hPa');values=iso%vorticity(5,:)
 case('vorticity_700hPa');values=iso%vorticity(6,:)
 case('vorticity_850hPa');values=iso%vorticity(7,:)
 case('vorticity_925hPa');values=iso%vorticity(8,:)
 case('mslp');values=iso%mslp
 case('t_iso_levels');values=iso%t_iso_levels
 case('z_iso_levels');values=iso%z_iso_levels
 case('meanT_500_300');values=iso%meanT
 case('t_isobaric');values=reshape(iso%t_isobaric,[size(values)])
 case('z_isobaric');values=reshape(iso%z_isobaric,[size(values)])
 case('cape');values=conv_out%values(1,:)
 case('cin');values=conv_out%values(2,:)
 case('srh_0_1km');values=conv_out%values(3,:)
 case('srh_0_3km');values=conv_out%values(4,:)
 case('uzonal_surface');values=conv_out%values(5,:)
 case('uzonal_1km');values=conv_out%values(6,:)
 case('uzonal_6km');values=conv_out%values(7,:)
 case('umeridional_surface');values=conv_out%values(8,:)
 case('umeridional_1km');values=conv_out%values(9,:)
 case('umeridional_6km');values=conv_out%values(10,:)
 case('temperature_surface');values=conv_out%values(11,:)
 case('dewpoint_surface');values=conv_out%values(12,:)
 case default
  error stop 'diag_fetch: '//trim(name)//' is not a diagnostic'
 end select
 end subroutine
end module
