! The physics of one model step as stages that run on the card (kernels over the columns) or on the host (the same column
! routines in a serial loop), chosen per stage at run time (phys_mode.f90). The column routines are the lean ones (pre_dynamics.f90 and what it calls);
! the cell routines below are its pre_dynamics_cell split at the scheme boundaries, in the same order, with the same expressions. The columns are
! independent, so doing a scheme for all columns before the next scheme gives the same numbers as doing all schemes column by column.
!
! The physics state (model_state, noahmp_state, gf_state, gwdo_state, the accumulators of the tendencies) lives on the host or on the card, `loc`
! says where the current values are. A stage on the card asks for the card, a stage on the host for the host; the whole state is copied when
! the place changes (never, once every stage and the radiation are on the card).
! A stage in check mode runs on the host and on the card from the same inputs, compares everything bit for bit and goes on with the host result.
module phys_dev
 use config
 use dyn_config
 use model_state
 use gf_state
 use noahmp_state
 use gwdo_state
 use physics_inputs, only: physics_inputs_cell, noahmp_interface_pressure
 use noahmp_coupling, only: update_land_dv, set_land_pressure_dv, surface_diagnostics_dv
 use mynn, only: mynn_pbl_state, mynn_surface_state
 use grell_freitas, only: advance_gf_columns
 use orographic_drag, only: advance_gwdo_state
 use physics_tendency, only: add_pbl, add_convection, add_radiation
 use schemes, only: conv_on, pbl_on, gwdo_on, sfclayer_on, lsm_on
 use pre_dynamics, only: radiation_cache
 use model_data
 use model_timing
 use phys_mode
 use check_util
 use phys_check
 use phys_state
 use lean_phys_fault, only: physics_fault_reset, physics_fault_check
 use radiation_sizes, only: max_lw_layers, max_sw_layers
 use radiation_fast
 use micro_pool, only: pool_slots2, micro_pool_start
 use mynn_card, only: mynn_card_start, mynn_card_run
 use radiation_fast_gas, only: rf_lw_gas, rf_sw_gas
 use parrrtm, only: ngptlw, nbndlw
 use parrrsw, only: ngptsw, nbndsw
 use lean_dev_rrtmg_tables, only: rrtmg_tables_to_device
 use sea_ice, only: sea_ice_cells => ice, ice_cells, any_sea_ice, ice_cell, open_water, copy_ice, ice_surface_layer_state, ice_step_state
 use land_ice_exchange, only: exchange, glacier_exchange, glacier_of, n_glacier, glacier_unpack, glacier_pack_state
 use land_ice, only: glacier_step, keep_glacier, put_glacier
 use NoahmpIOVarType, only: NoahmpIO_type
 implicit none
 public :: bundle_to_host
 real(kind=8), save :: stage_s(nstages) = 0.d0   ! seconds per stage (a stage in check mode counts host + card + comparison)
 logical, save :: ice_on = .false.  ! any sea-ice or glacier cell in the run: set once in phys_device_start, outside the cell loops
 integer, save :: loc = 0                       ! 0: the host holds the current physics state, 1: the card
 type(state_type), target, save :: h_state
 type(noahmp_state_type), target, save :: h_land
 type(gf_state_type), target, save :: h_conv
 type(gwdo_state_type), target, save :: h_drag
 real, allocatable, save :: h_heating(:,:,:)
 real, allocatable, save :: h_th(:,:), h_sc(:,:,:), h_ux(:,:), h_uy(:,:), h_em(:), h_sn(:), h_al(:)
 real, allocatable, save :: h_tru(:,:), h_trt(:,:), h_trr(:,:)
 ! radiation on the card: the columns in groups of rad_slots (LEAN_RAD_SLOTS), the group arrays of radiation_fast_data.f90.
 ! The work is spread over g-points, bands and layers, not one thread per column: a column's work arrays (1.5 MB) in one
 ! thread's stack frame would size the card's stack reservation for the whole program.
 integer, save :: rad_slots = 2048
 real, allocatable, save :: rf_cosz(:)
 integer, allocatable, save :: rf_day(:)
 ! Sea ice and land ice: the sea-ice fields and the glacier exchange records travel with the physics state; a check of the
 ! land stage keeps the glacier columns (host only) so that the two runs start from the same columns
 type(ice_cells), save :: h_ice
 type(glacier_exchange), allocatable, save :: h_exchange(:)
 type(NoahmpIO_type), allocatable, save :: glacier_before(:), glacier_host(:)
 ! The order in which the kernels' threads take the columns (order_columns): busy columns first (land for the land stage
 ! and the land diagnostics, sub-grid orography for GWDO), then the others. A warp takes 32 consecutive entries and costs as
 ! much as its slowest column, so a warp of sea or quiet columns finishes early instead of waiting for one busy column. The
 ! surface layer, MYNN and Grell-Freitas keep the plain order: the cost of their columns hardly depends on the kind of cell.
 ! Each column is computed by itself (no column reads another), so the order changes no value. col_count: the two fill
 ! counters (front, back).
 integer, allocatable, save :: col_order(:)
 integer, save :: col_count(2) = 0
 !$acc declare create(col_order, col_count)

contains

 ! ---- data on the card -------------------------------------------------------------------------------------------------------------
 subroutine phys_device_start(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 ice_on = any_sea_ice .or. n_glacier > 0
 !$acc update device(nz, nz1)
 if (check_radiation) allocate(h_heating(nz,2,ncells))
 !$acc enter data copyin(rad, rad_heating)
 call enter_state(state); call enter_gf(convection)
 call enter_land(lands); call enter_gwdo(drag_state)
 !$acc enter data copyin(tend_th, ux, uy, east, north, emiss, snow, albedo)   ! (rbe, mass_edge: host only, the card's finish has them as scalars)
 !$acc enter data copyin(m, s)
 !$acc enter data copyin(m%edgeNormalVectors, m%zgrid, m%zz, s%pressure_base)
 call micro_pool_start()   ! the column work-array pools (src/device/micro_pool.f90), before the pooled kernels
 call mynn_card_start()   ! MYNN's stage arrays (src/device/mynn_card.f90)
 if (.not. allocated(col_order)) allocate(col_order(ncells))   ! card copy by declare create
 call micro_enter_device(g, cmp)
 if (ice_on) then
 ! the start values (lean_model.f90): sea_ice.f90 and land_ice.f90 declare them on the card
 !$acc update device(sea_ice_cells%xice, sea_ice_cells%sst, sea_ice_cells%tslb)
 !$acc update device(sea_ice_cells%snow, sea_ice_cells%snowc, sea_ice_cells%snoalb)
 !$acc update device(sea_ice_cells%z0, sea_ice_cells%albedo, sea_ice_cells%emiss)
 !$acc update device(sea_ice_cells%grdflx, sea_ice_cells%acsnow, sea_ice_cells%acsnom)
 !$acc update device(sea_ice_cells%sfcrunoff)
 !$acc update device(any_sea_ice, glacier_of, n_glacier, exchange)
 end if
 call physics_fault_reset()
 loc = 0
 end subroutine

 subroutine bundle_to_device(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 real(kind=8) :: t0
 call timer_start(t0)
 call to_device_state(state); call to_device_land(lands); call to_device_gf(convection); call to_device_gwdo(drag_state)
 !$acc update device(tend_th, fc%tend_scalars, ux, uy, emiss, snow, albedo)
 if (ice_on) then
 !$acc update device(sea_ice_cells%xice, sea_ice_cells%sst, sea_ice_cells%tslb)
 !$acc update device(sea_ice_cells%snow, sea_ice_cells%snowc, sea_ice_cells%snoalb)
 !$acc update device(sea_ice_cells%z0, sea_ice_cells%albedo, sea_ice_cells%emiss)
 !$acc update device(sea_ice_cells%grdflx, sea_ice_cells%acsnow, sea_ice_cells%acsnom)
 !$acc update device(sea_ice_cells%sfcrunoff)
 !$acc update device(exchange)
 end if
 call timer_stop(transfer_timer, t0)
 end subroutine

 subroutine bundle_to_host(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 real(kind=8) :: t0
 call timer_start(t0)
 call to_host_state(state); call to_host_land(lands); call to_host_gf(convection); call to_host_gwdo(drag_state)
 !$acc update self(tend_th, fc%tend_scalars, ux, uy, emiss, snow, albedo)
 if (ice_on) then
 !$acc update self(sea_ice_cells%xice, sea_ice_cells%sst, sea_ice_cells%tslb)
 !$acc update self(sea_ice_cells%snow, sea_ice_cells%snowc, sea_ice_cells%snoalb)
 !$acc update self(sea_ice_cells%z0, sea_ice_cells%albedo, sea_ice_cells%emiss)
 !$acc update self(sea_ice_cells%grdflx, sea_ice_cells%acsnow, sea_ice_cells%acsnom)
 !$acc update self(sea_ice_cells%sfcrunoff)
 !$acc update self(exchange)
 end if
 call timer_stop(transfer_timer, t0)
 end subroutine

 ! The surface update (surface_bdy.f90) runs on the host between these two calls. It writes the land mask,
 ! skin temperature, roughness and snow depth of the state, the soil moisture and liquid water of the land state (the soil of a
 ! cell that freezes or melts), emiss, snow and albedo (the radiation inputs), and the sea-ice cells when the card has them
 ! (ice_on is chosen at the start from the init and the surface files). It also writes xice of `ice` for the radiation.
 ! Which side is current depends on loc, as everywhere else in this module. With loc == 1 the card holds the current bundle:
 ! it comes to the host first and goes back after the update. With loc == 0 the host holds the current bundle (a host stage ran
 ! last, or LEAN_CHECK restored it) and the card is one stage behind: nothing of the bundle moves, because the next need_card
 ! sends all of it. `ice` is not in the bundle and no kernel writes it, so the host copy is always current: it goes up whenever
 ! the radiation is on the card, whatever loc is. snoalb of the sea-ice cells is written by nobody after the start.
 subroutine surface_update_transfer(state, lands, to_host)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 logical, intent(in) :: to_host
 if (to_host) then
 if (loc == 1) then
 !$acc update self(state%xland, state%tsk, state%snowh, state%znt, lands%water_state_soilmoisture, lands%water_state_soilliqwater)
 !$acc update self(emiss, snow, albedo)
 if (ice_on) then
 !$acc update self(sea_ice_cells%xice, sea_ice_cells%sst, sea_ice_cells%tslb)
 !$acc update self(sea_ice_cells%snow, sea_ice_cells%snowc)
 !$acc update self(sea_ice_cells%z0, sea_ice_cells%albedo, sea_ice_cells%emiss)
 end if
 end if
 else
 if (loc == 1) then
 !$acc update device(state%xland, state%tsk, state%snowh, state%znt, lands%water_state_soilmoisture, lands%water_state_soilliqwater)
 !$acc update device(emiss, snow, albedo)
 if (ice_on) then
 !$acc update device(sea_ice_cells%xice, sea_ice_cells%sst, sea_ice_cells%tslb)
 !$acc update device(sea_ice_cells%snow, sea_ice_cells%snowc)
 !$acc update device(sea_ice_cells%z0, sea_ice_cells%albedo, sea_ice_cells%emiss)
 end if
 end if
 if (card_radiation) then
 !$acc update device(ice)
 end if
 end if
 end subroutine

 subroutine need_card(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 if (loc == 0) call bundle_to_device(state, lands, convection, drag_state)
 loc = 1
 end subroutine

 subroutine need_host(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 if (loc == 1) call bundle_to_host(state, lands, convection, drag_state)
 loc = 0
 end subroutine


 ! ---- the cell routines ------------------------------------------------------------------------------------------------------------
 ! dynamics state -> physics columns (model_inputs) and the day of the year of the land columns
 subroutine inputs_cell(c, it, julian, state, lands, convection, rho_zz1, theta_m1, scalars1, exner, pressure_p, w1, u_zonal, &
                        u_meridional, zz, zgrid, pressure_base, rainnc, rainncv, snownc, snowncv, graupelnc, graupelncv, sr, &
                        rainprod, evapprod, re_cloud, re_ice, re_snow, rthdynten, rqvdynten)
 !$acc routine seq
 integer, intent(in) :: c, it
 real, intent(in) :: julian
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 real, intent(in) :: rho_zz1(nz,*), theta_m1(nz,*), scalars1(num_scalars,nz,*), exner(nz,*), pressure_p(nz,*)
 real, intent(in) :: w1(nz+1,*), u_zonal(nz,*), u_meridional(nz,*), zz(nz,*), zgrid(nz+1,*), pressure_base(nz,*)
 real, intent(in) :: rainnc(*), rainncv(*), snownc(*), snowncv(*), graupelnc(*), graupelncv(*), sr(*)
 real, intent(in) :: rainprod(nz,*), evapprod(nz,*), re_cloud(nz,*), re_ice(nz,*), re_snow(nz,*)
 real, intent(in) :: rthdynten(nz,*), rqvdynten(nz,*)
 type(column_view) :: a
 type(gf_column) :: f
 call set_view_column(a, state, c)
 f = gf_view(convection, c)
 call physics_inputs_cell(a, f, rho_zz1(:,c), theta_m1(:,c), scalars1(:,:,c), exner(:,c), pressure_p(:,c), w1(:,c), u_zonal(:,c), &
                          u_meridional(:,c), zz(:,c), zgrid(:,c), pressure_base(:,c))
 if (it > 1) then
  a%rainnc = rainnc(c); a%rainncv = rainncv(c); a%snownc = snownc(c); a%snowncv = snowncv(c)
  a%graupelnc = graupelnc(c); a%graupelncv = graupelncv(c); a%sr = sr(c)
  a%rainprod = rainprod(:,c); a%evapprod = evapprod(:,c)
  a%re_cloud = re_cloud(:,c); a%re_ice = re_ice(:,c); a%re_snow = re_snow(:,c)
  f%dynamics_theta = rthdynten(:,c); f%dynamics_qv = rqvdynten(:,c)
 end if
 ! The land fields are written straight into lands, not through a noahmp_view copy: a local noahmp_column (15 KB) that the
 ! compiler cannot fold away stays on the kernel stack of every thread (with a run-time ncells it does: 15 KB per thread,
 ! about 0.7 GB of card memory).
 if (a%xland < 1.5) lands%config_domain_DayJulianInYear(c) = julian
 end subroutine

 ! radiation results of the last event into the convection and land inputs, then the MYNN surface layer
 subroutine surface_cell(c, itimestep, state, lands, convection, rad_c, heating)
 !$acc routine seq
 integer, intent(in) :: c, itimestep
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(radiation_cache), intent(in) :: rad_c
 real, intent(in) :: heating(nz,2)
 type(gf_column) :: f
 ! no column view (its 7 KB would be most of this kernel's frame): the state directly
 f = gf_view(convection, c)
 f%shortwave = rad_c%sw_net
 f%radiation_theta = heating(:,1) + heating(:,2)
 if (state%xland(c) < 1.5) then   ! straight into lands, no noahmp_view copy (see inputs_cell)
  lands%net_shortwave(c) = rad_c%sw_net
#ifdef LEAN_NOAHMP_SWSPLIT
  lands%sw_frac_dir_vis(c) = rad_c%sw_frac_dir_vis
  lands%sw_frac_dir_nir(c) = rad_c%sw_frac_dir_nir
  lands%sw_frac_dif_vis(c) = rad_c%sw_frac_dif_vis
#endif
  lands%forcing_RadLwDownRefHeight(c) = rad_c%lw_down
  lands%config_domain_CosSolarZenithAngle(c) = rad_c%cos_zenith
 end if
 ! MYNN's first-step initialisation (initflag) runs when itimestep is 1, i.e. only on the first step of a cold
 ! start: state%timestep is in the lean restart file, so a restart continues the count (MPAS instead restarts its counter
 ! at one and suppresses initflag).
 call mynn_surface_state(state, c, itimestep)
 end subroutine

 subroutine land_cell(c, state, lands, convection, zgrid)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 real, intent(in) :: zgrid(nz+1,*)
 type(column_view) :: a
 real :: p_first, p_second
 ! the Noah-MP column as (lands, c), not its 458-pointer view: copies of the view would be most of this kernel's stack frame
 call set_view_column(a, state, c)
 if (a%xland < 1.5) then
  lands%rain_convective(c) = convection%raincv(c)
  call noahmp_interface_pressure(zgrid(:,c), a%p, p_first, p_second)
  call set_land_pressure_dv(lands, c, p_first, p_second)
  call update_land_dv(lands, c, a)
 end if
 call surface_diagnostics_dv(lands, c, a)
 end subroutine

 ! MYNN PBL, and its tendencies into the shared accumulators
 subroutine pbl_cell(c, itimestep, state, mass, tend_th_, tend_scalars_)
 ! the host only: on the card run_pbl calls mynn_card_run
 integer, intent(in) :: c, itimestep
 type(state_type), target, intent(inout) :: state
 real, intent(in) :: mass(nz,*)
 real, intent(inout) :: tend_th_(nz,*), tend_scalars_(num_scalars,nz,*)
 ! no column view (its 7 KB of pointers would be half of this kernel's frame): the state's column sections directly
 call mynn_pbl_state(state, c, itimestep)
 call add_pbl(c, mass(:,c), state%t_theta(:,c), state%t_qv(:,c), state%t_qc(:,c), state%t_qi(:,c), state%t_qs(:,c), &
              state%t_ni(:,c), tend_th_, tend_scalars_)
 end subroutine

 subroutine gwdo_cell(c, state, drag_state, zgrid)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 type(gwdo_state_type), target, intent(inout) :: drag_state
 real, intent(in) :: zgrid(nz+1,*)
 type(gwdo_column_state) :: drag
 ! no column view (its 7 KB of pointers would be half of this kernel's frame): the state's column sections directly
 drag = gwdo_view(drag_state, c)
 call advance_gwdo_state(state, c, drag, zgrid(:,c))
 end subroutine

 ! Grell-Freitas, the convection and radiation tendencies into the accumulators, the wind tendency of PBL + GWDO
 subroutine gf_cell(c, itimestep, state, convection, rad_c, heating, mass, tend_th_, tend_scalars_, ux_, uy_)
 !$acc routine seq
 integer, intent(in) :: c, itimestep
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: convection
 type(radiation_cache), intent(in) :: rad_c
 real, intent(in) :: heating(nz,2)
 real, intent(in) :: mass(nz,*)
 real, intent(inout) :: tend_th_(nz,*), tend_scalars_(num_scalars,nz,*), ux_(nz,*), uy_(nz,*)
 type(gf_column) :: f
 integer :: k
 ! no column view (7 KB of pointers in this kernel's frame): the state's column sections straight to advance_gf_columns
 f = gf_view(convection, c)
 call advance_gf_columns(f, itimestep, state%dx(c), state%kpbl(c), state%xland(c), state%dz(:,c), state%exner(:,c), &
                         state%p_hyd(:,c), state%qv(:,c), state%rho_moist(:,c), state%t_qv(:,c), state%t_theta(:,c), &
                         state%theta(:,c), state%u(:,c), state%v(:,c), state%w(:,c))
 call add_convection(c, mass(:,c), f%rthcuten, f%rqvcuten, f%rqccuten, f%rqicuten, tend_th_, tend_scalars_)
 call add_radiation(c, mass(:,c), heating(:,1), heating(:,2), tend_th_)
 do k = 1, nz
  ux_(k,c) = state%t_u(k,c)
  uy_(k,c) = state%t_v(k,c)
 end do
 end subroutine

 ! The convection stage with convection 'off': no Grell-Freitas, but the radiation heating and the PBL and GWDO wind tendencies
 ! the stage also carries (as pre_dynamics_cell: add_radiation always, the winds when the PBL or GWDO ran). winds is the
 ! host's decision, so no kernel reads a scheme flag.
 subroutine nogf_cell(c, mass, heating, tend_th_, ux_, uy_, state, winds)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 real, intent(in) :: heating(nz,2)
 real, intent(in) :: mass(nz,*)
 real, intent(inout) :: tend_th_(nz,*), ux_(nz,*), uy_(nz,*)
 logical, intent(in) :: winds
 integer :: k
 ! no column view (as gf_cell): the state's wind tendencies read straight
 call add_radiation(c, mass(:,c), heating(:,1), heating(:,2), tend_th_)
 if (winds) then
  do k = 1, nz
   ux_(k,c) = state%t_u(k,c)
   uy_(k,c) = state%t_v(k,c)
  end do
 end if
 end subroutine

 ! land diagnostics that the next radiation event reads
 subroutine landdiag_cell(c, state, lands, emiss_, snow_, albedo_)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 real, intent(inout) :: emiss_(*), snow_(*), albedo_(*)
 type(column_view) :: a
 call set_view_column(a, state, c)
 if (a%xland < 1.5) then
  emiss_(c) = lands%energy_state_EmissivitySfc(c)
  snow_(c) = lands%water_state_SnowWaterEquiv(c)
  if (lands%energy_state_AlbedoSfc(c) > -999.) albedo_(c) = lands%energy_state_AlbedoSfc(c)
 end if
 end subroutine

 subroutine landdiag_ice_cell(c, state, lands, emiss_, snow_, albedo_)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 real, intent(inout) :: emiss_(*), snow_(*), albedo_(*)
 type(column_view) :: a
 integer :: g
 call set_view_column(a, state, c)
 ! sea ice: the blended surface of sea_ice.f90; land ice: the glacier column, through its exchange record
 if (ice_cell(c)) then
  emiss_(c) = sea_ice_cells%emiss(c); snow_(c) = sea_ice_cells%snow(c); albedo_(c) = sea_ice_cells%albedo(c)
 else if (glacier_of(c) > 0) then
  g = glacier_of(c)
  emiss_(c) = exchange(g)%emiss; snow_(c) = exchange(g)%snow; albedo_(c) = exchange(g)%albedo
 else if (a%xland < 1.5) then
  emiss_(c) = lands%energy_state_EmissivitySfc(c)
  snow_(c) = lands%water_state_SnowWaterEquiv(c)
  if (lands%energy_state_AlbedoSfc(c) > -999.) albedo_(c) = lands%energy_state_AlbedoSfc(c)
 end if
 end subroutine

 ! The surface stage of a sea-ice or glacier cell (the ice branches of pre_dynamics_cell). A sea-ice cell runs its whole
 ! surface step here: the fractional surface layer, then the sea-ice scheme that MPAS runs after the land model. The cell is
 ! independent of the others and nothing reads it between the two stages, so this keeps the order of its own steps. A glacier
 ! cell runs the surface layer and fills its exchange record for the land stage.
 subroutine ice_surface_cell(c, itimestep, layer_on, ice_scheme_on, state, lands, convection, rad_c, heating, zgrid)
 !$acc routine seq
 integer, intent(in) :: c, itimestep
 logical, intent(in) :: layer_on, ice_scheme_on
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(radiation_cache), intent(in) :: rad_c
 real, intent(in) :: heating(nz,2)
 real, intent(in) :: zgrid(nz+1,*)
 type(gf_column) :: f
 type(open_water) :: water
 integer :: g
 real :: sw_net, lw_down, cos_zenith, raincv
 ! no column view (its 7 KB of pointers would be most of this kernel's frame): the *_state routines take the state and c
 f = gf_view(convection, c)
 f%shortwave = rad_c%sw_net
 f%radiation_theta = heating(:,1) + heating(:,2)
 if (state%xland(c) < 1.5) then   ! straight into lands, no noahmp_view copy (see inputs_cell)
  lands%net_shortwave(c) = rad_c%sw_net
  lands%forcing_RadLwDownRefHeight(c) = rad_c%lw_down
  lands%config_domain_CosSolarZenithAngle(c) = rad_c%cos_zenith
 end if
 ! plain local scalars as actual arguments (see the note on module variables at the stages below)
 sw_net = rad_c%sw_net; lw_down = rad_c%lw_down; cos_zenith = rad_c%cos_zenith; raincv = f%raincv
 ! as the host form (the ice branches of pre_dynamics_cell, pre_dynamics.f90) and MPAS's driver (mpas_atmphys_driver.F:268-317): the surface layer only with
 ! the surface layer on, the sea-ice step and the glacier pack only with the land-surface block (lsm_on). The two flags are the
 ! host's, so no kernel reads a scheme flag.
 if (ice_cell(c)) then
  if (layer_on) call ice_surface_layer_state(state, c, itimestep, water)
  if (ice_scheme_on) call ice_step_state(state, c, water, sw_net, lw_down, raincv, zgrid(:,c))
 else
  g = glacier_of(c)
  if (layer_on) call mynn_surface_state(state, c, itimestep)
  if (ice_scheme_on) call glacier_pack_state(state, c, g, sw_net, lw_down, cos_zenith, raincv, zgrid(:,c))
 end if
 end subroutine

 ! The land stage of a sea-ice cell: only its 2 m diagnostics, with the water formula (a view whose land mask says water)
 subroutine ice_land_cell(c, state, lands)
 !$acc routine seq
 integer, intent(in) :: c
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(column_view) :: b
 real, target :: open_water_mask
 call set_view_column(b, state, c)
 open_water_mask = 2.
 b%xland => open_water_mask
 call surface_diagnostics_dv(lands, c, b)
 end subroutine

 ! The results of glacier column g into its cell
 subroutine glacier_cell(g, state)
 !$acc routine seq
 integer, intent(in) :: g
 type(state_type), target, intent(inout) :: state
 type(column_view) :: a
 integer :: c
 c = exchange(g)%c
 call set_view_column(a, state, c)
 call glacier_unpack(a, g)
 end subroutine

 ! ---- the stages -------------------------------------------------------------------------------------------------------------------
 ! The model fields enter the stage routines as dummy arguments (fx, mx, mh, sx), never as members of the module variables fc, cmp, m,
 ! s: a member of a module variable passed as an actual argument inside a compute region is passed by its HOST address (nvfortran 25.3),
 ! and on a system with HMM addressing the card then reads and writes host memory without any error.
 subroutine run_inputs(on_card, it, julian, state, lands, convection, fx, mx, mh, sx)
 type(dyn_fields), intent(inout) :: fx
 type(micro_fields), intent(inout) :: mx
 type(dyn_mesh_type), intent(inout) :: mh
 type(dyn_state_type), intent(inout) :: sx
 logical, intent(in) :: on_card
 integer, intent(in) :: it
 real, intent(in) :: julian
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c, k
 real :: tem1, tem2, rho1, rho2, correction, z0, z1, z2, w1, w2, int_up, int_k
 if (on_card) then
  ! the card: inputs_cell split into a kernel over (level, column) for the point work and one over the columns for the surface
  ! pressure and the hydrostatic integral, with the expressions of physics_inputs_cell: the point work then has a thread per
  ! level and column instead of one per column
  !$acc parallel loop gang vector collapse(2) default(present)
  do c = 1, ncells
   do k = 1, nz+1
    if (k <= nz) then
     state%qv(k,c) = max(0., fx%scalars1(i_qv,k,c))
     state%qc(k,c) = max(0., fx%scalars1(i_qc,k,c))
     state%qr(k,c) = max(0., fx%scalars1(i_qr,k,c))
     state%qi(k,c) = max(0., fx%scalars1(i_qi,k,c))
     state%qs(k,c) = max(0., fx%scalars1(i_qs,k,c))
     state%qg(k,c) = max(0., fx%scalars1(i_qg,k,c))
     state%ni(k,c) = max(0., fx%scalars1(i_ni,k,c))
     state%nr(k,c) = max(0., fx%scalars1(i_nr,k,c))
     state%u(k,c) = fx%u_zonal(k,c)
     state%v(k,c) = fx%u_meridional(k,c)
     state%rho_moist(k,c) = mh%zz(k,c) * fx%rho_zz1(k,c)
     state%rho_moist(k,c) = state%rho_moist(k,c) * (1. + state%qv(k,c))
     state%theta(k,c) = fx%theta_m1(k,c) / (1. + rv/rgas * state%qv(k,c))
     state%exner(k,c) = fx%exner(k,c)
     state%p(k,c) = fx%pressure_p(k,c) + sx%pressure_base(k,c)
     state%dz(k,c) = mh%zgrid(k+1,c) - mh%zgrid(k,c)
     if (it > 1) then
      state%rainprod(k,c) = mx%rainprod(k,c); state%evapprod(k,c) = mx%evapprod(k,c)
      state%re_cloud(k,c) = mx%re_cloud(k,c); state%re_ice(k,c) = mx%re_ice(k,c); state%re_snow(k,c) = mx%re_snow(k,c)
      convection%dynamics_theta(k,c) = fx%rthdynten(k,c); convection%dynamics_qv(k,c) = fx%rqvdynten(k,c)
     end if
    end if
    state%w(k,c) = fx%w1(k,c)
   end do
  end do
  !$acc parallel loop gang vector default(present) private(tem1, tem2, rho1, rho2, correction, z0, z1, z2, w1, w2, int_up, int_k)
  do c = 1, ncells
   ! surface pressure from the first two layers
   tem1 = mh%zgrid(2,c) - mh%zgrid(1,c)
   tem2 = mh%zgrid(3,c) - mh%zgrid(2,c)
   rho1 = fx%rho_zz1(1,c) * mh%zz(1,c) * (1. + state%qv(1,c))
   rho2 = fx%rho_zz1(2,c) * mh%zz(2,c) * (1. + state%qv(2,c))
   correction = 0.5*9.80616*(mh%zgrid(2,c)-mh%zgrid(1,c)) * (rho1 - 0.5*(rho2-rho1)*tem1/(tem1+tem2))
   state%psfc(c) = correction + fx%pressure_p(1,c) + sx%pressure_base(1,c)
   ! hydrostatic pressure: interfaces integrated down from the top value extrapolated in log pressure
   z0 = mh%zgrid(nz+1,c)
   z1 = 0.5*(mh%zgrid(nz+1,c)+mh%zgrid(nz,c))
   z2 = 0.5*(mh%zgrid(nz,c)+mh%zgrid(nz-1,c))
   w1 = (z0-z2)/(z1-z2)
   w2 = 1. - w1
   int_up = exp(w1*log(state%p(nz,c)) + w2*log(state%p(nz-1,c)))
   do k = nz, 1, -1
    int_k = int_up + 9.80616*state%rho_moist(k,c)*state%dz(k,c)
    state%p_hyd(k,c) = 0.5*(int_up+int_k)
    int_up = int_k
   end do
   convection%surface_pressure(c) = int_up
   if (it > 1) then
    state%rainnc(c) = mx%rainnc(c); state%rainncv(c) = mx%rainncv(c); state%snownc(c) = mx%snownc(c)
    state%snowncv(c) = mx%snowncv(c); state%graupelnc(c) = mx%graupelnc(c); state%graupelncv(c) = mx%graupelncv(c)
    state%sr(c) = mx%sr(c)
   end if
   if (state%xland(c) < 1.5) lands%config_domain_DayJulianInYear(c) = julian
  end do
  return
 end if
 do c = 1, ncells
  call inputs_cell(c, it, julian, state, lands, convection, fx%rho_zz1, fx%theta_m1, fx%scalars1, fx%exner, fx%pressure_p, fx%w1, &
                   fx%u_zonal, fx%u_meridional, mh%zz, mh%zgrid, sx%pressure_base, mx%rainnc, mx%rainncv, mx%snownc, mx%snowncv, &
                   mx%graupelnc, mx%graupelncv, mx%sr, mx%rainprod, mx%evapprod, mx%re_cloud, mx%re_ice, mx%re_snow, &
                   fx%rthdynten, fx%rqvdynten)
 end do
 end subroutine

 ! col_order for the next stage kernel: mode 1, v < 1.5 first (xland: land); mode 2, v > 0 or w > 0 first.
 subroutine order_columns(mode, v, w)
 integer, intent(in) :: mode
 real, intent(in) :: v(ncells), w(ncells)
 integer :: c, slot
 logical :: busy
 !$acc serial default(present)
 col_count(1) = 0
 col_count(2) = 0
 !$acc end serial
 !$acc parallel loop gang vector default(present) private(busy, slot)
 do c = 1, ncells
  if (mode == 1) then
   busy = v(c) < 1.5
  else
   busy = v(c) > 0. .or. w(c) > 0.
  end if
  if (busy) then
   !$acc atomic capture
   col_count(1) = col_count(1) + 1
   slot = col_count(1)
   !$acc end atomic
  else
   !$acc atomic capture
   col_count(2) = col_count(2) + 1
   slot = col_count(2)
   !$acc end atomic
   slot = ncells + 1 - slot
  end if
  col_order(slot) = c
 end do
 end subroutine

 subroutine run_surface(on_card, itimestep, state, lands, convection)
 logical, intent(in) :: on_card
 integer, intent(in) :: itimestep
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c
 !$acc parallel loop gang vector default(present) if(on_card)
 do c = 1, ncells
  call surface_cell(c, itimestep, state, lands, convection, rad(c), rad_heating(:,:,c))
 end do
 end subroutine

 subroutine run_land(on_card, state, lands, convection, mh)
 type(dyn_mesh_type), intent(inout) :: mh
 logical, intent(in) :: on_card
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c, n
 if (on_card) call order_columns(1, state%xland, state%xland)   ! land first
 !$acc parallel loop gang vector default(present) private(c) if(on_card)
 do n = 1, ncells
  c = n
  if (on_card) c = col_order(n)
  call land_cell(c, state, lands, convection, mh%zgrid)
 end do
 end subroutine

 ! run_surface with the sea-ice and glacier cells (ice_surface_cell)
 subroutine run_surface_ice(on_card, itimestep, state, lands, convection, mh)
 type(dyn_mesh_type), intent(inout) :: mh
 logical, intent(in) :: on_card
 integer, intent(in) :: itimestep
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c
 logical :: layer_on, ice_scheme_on
 ! plain local scalars for the kernels (see the note on module variables at the head of the stages above)
 layer_on = sfclayer_on
 ice_scheme_on = lsm_on
 ! two kernels, the ice cells and the others: in one, the card compiler adds the two cells' stack frames; the cells are
 ! independent, so the order does not matter
 !$acc parallel loop gang vector default(present) if(on_card)
 do c = 1, ncells
  if (ice_cell(c) .or. glacier_of(c) > 0) then
   call ice_surface_cell(c, itimestep, layer_on, ice_scheme_on, state, lands, convection, rad(c), rad_heating(:,:,c), mh%zgrid)
  end if
 end do
 ! the other cells have the surface layer only: without it there is nothing for them to run
 if (.not. layer_on) return
 !$acc parallel loop gang vector default(present) if(on_card)
 do c = 1, ncells
  if (.not. (ice_cell(c) .or. glacier_of(c) > 0)) then
   call surface_cell(c, itimestep, state, lands, convection, rad(c), rad_heating(:,:,c))
  end if
 end do
 end subroutine

 ! run_land with the sea-ice cells (ice_land_cell) and the glacier cells. A glacier column is MPAS's own Noah-MP
 ! driver (land_ice.f90), which cannot run on the card: its exchange records go to the host, the host runs the driver on
 ! each, the records come back and the card writes the results into the cells. n_glacier records each way (1.5 KB each).
 subroutine run_land_ice(on_card, itimestep, state, lands, convection, mh)
 type(dyn_mesh_type), intent(inout) :: mh
 logical, intent(in) :: on_card
 integer, intent(in) :: itimestep
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c, g, n
 ! two kernels (see run_surface_ice)
 if (on_card) call order_columns(1, state%xland, state%xland)   ! land first
 !$acc parallel loop gang vector default(present) private(c) if(on_card)
 do n = 1, ncells
  c = n
  if (on_card) c = col_order(n)
  if (ice_cell(c)) then
   call ice_land_cell(c, state, lands)
  end if
 end do
 !$acc parallel loop gang vector default(present) private(c) if(on_card)
 do n = 1, ncells
  c = n
  if (on_card) c = col_order(n)
  if (.not. ice_cell(c) .and. glacier_of(c) == 0) then
   call land_cell(c, state, lands, convection, mh%zgrid)
  end if
 end do
 if (n_glacier > 0) then
  !$acc update self(exchange) if(on_card)
  do g = 1, n_glacier
   call glacier_step(g, itimestep)
  end do
  !$acc update device(exchange) if(on_card)
  !$acc parallel loop gang vector default(present) if(on_card)
  do g = 1, n_glacier
   call glacier_cell(g, state)
  end do
 end if
 end subroutine

 subroutine run_pbl(on_card, itimestep, state, fx)
 type(dyn_fields), intent(inout) :: fx
 logical, intent(in) :: on_card
 integer, intent(in) :: itimestep
 type(state_type), target, intent(inout) :: state
 integer :: c
 if (on_card) then
  ! the card: MYNN in stages over (column, level) arrays (src/device/mynn_card.f90), the same arithmetic as pbl_cell
  call mynn_card_run(merge(1,0,itimestep==1), state, fx%rho_zz1, tend_th, fx%tend_scalars)
  return
 end if
 do c = 1, ncells
  call pbl_cell(c, itimestep, state, fx%rho_zz1, tend_th, fx%tend_scalars)
 end do
 end subroutine

 subroutine run_gwdo(on_card, state, drag_state, mh)
 type(dyn_mesh_type), intent(inout) :: mh
 logical, intent(in) :: on_card
 type(state_type), target, intent(inout) :: state
 type(gwdo_state_type), target, intent(inout) :: drag_state
 integer :: c, n
 ! columns with sub-grid orography first (order_columns): the others take advance_gwdo's quiet path, a warp of them together
 if (on_card) call order_columns(2, drag_state%terrain_stddev, drag_state%terrain_stddev)
 !$acc parallel loop gang vector default(present) private(c) if(on_card)
 do n = 1, ncells
  c = n
  if (on_card) c = col_order(n)
  call gwdo_cell(c, state, drag_state, mh%zgrid)
 end do
 end subroutine

 subroutine run_gf(on_card, itimestep, state, convection, fx)
 type(dyn_fields), intent(inout) :: fx
 logical, intent(in) :: on_card
 integer, intent(in) :: itimestep
 type(state_type), target, intent(inout) :: state
 type(gf_state_type), target, intent(inout) :: convection
 integer :: c
 ! the grid capped at the work-array pool of GF's CUP_gf (src/device/micro_pool.f90): each thread takes several columns in turn
 !$acc parallel loop gang vector num_gangs(pool_slots2/128) vector_length(128) default(present) if(on_card)
 do c = 1, ncells
  call gf_cell(c, itimestep, state, convection, rad(c), rad_heating(:,:,c), fx%rho_zz1, tend_th, fx%tend_scalars, ux, uy)
 end do
 end subroutine

 ! The convection stage of a run without convection (nogf_cell)
 subroutine run_nogf(on_card, state, fx)
 type(dyn_fields), intent(inout) :: fx
 logical, intent(in) :: on_card
 type(state_type), target, intent(inout) :: state
 integer :: c
 logical :: winds
 winds = pbl_on .or. gwdo_on
 !$acc parallel loop gang vector default(present) if(on_card)
 do c = 1, ncells
  call nogf_cell(c, fx%rho_zz1, rad_heating(:,:,c), tend_th, ux, uy, state, winds)
 end do
 end subroutine



 ! finish_physics_tendencies of physics_tendency.f90 (the expressions copied), then the land diagnostics for the radiation
 subroutine run_finish(on_card, state, lands)
 logical, intent(in) :: on_card
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 integer :: e, k, c, n, cell1, cell2
 real :: coeff, me, rb
 !$acc parallel loop gang vector collapse(2) default(present) if(on_card)
 do e = 1, ne1
  do k = 1, nz
   fc%tend_ru_physics(k,e) = 0.
  end do
 end do
 !$acc parallel loop gang vector collapse(2) default(present) if(on_card)
 do c = 1, nc1
  do k = 1, nz
   fc%tend_rtheta_physics(k,c) = 0.
   fc%tend_rho_physics(k,c) = 0.
  end do
 end do
 ! the edge density, tend_to_edges (the cell wind tendencies to the edge normal component) and their product into the edge
 ! tendency, one kernel: the edge density and the edge tendency are scalars of the thread, so no edge arrays are needed for them
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, me, rb) if(on_card)
 do e = 1, nedges
  do k = 1, nz
   me = 0.5 * (fc%rho_zz1(k,m%cellsOnEdge(1,e)) + fc%rho_zz1(k,m%cellsOnEdge(2,e)))
   cell1 = m%cellsOnEdge(1,e)
   cell2 = m%cellsOnEdge(2,e)
   rb = ux(k,cell1) * 0.5 * (m%edgeNormalVectors(1,e) * east(1,cell1) &
                                 + m%edgeNormalVectors(2,e) * east(2,cell1) &
                                 + m%edgeNormalVectors(3,e) * east(3,cell1)) &
            + uy(k,cell1) * 0.5 * (m%edgeNormalVectors(1,e) * north(1,cell1) &
                                 + m%edgeNormalVectors(2,e) * north(2,cell1) &
                                 + m%edgeNormalVectors(3,e) * north(3,cell1)) &
            + ux(k,cell2) * 0.5 * (m%edgeNormalVectors(1,e) * east(1,cell2) &
                                 + m%edgeNormalVectors(2,e) * east(2,cell2) &
                                 + m%edgeNormalVectors(3,e) * east(3,cell2)) &
            + uy(k,cell2) * 0.5 * (m%edgeNormalVectors(1,e) * north(1,cell2) &
                                 + m%edgeNormalVectors(2,e) * north(2,cell2) &
                                 + m%edgeNormalVectors(3,e) * north(3,cell2))
   fc%tend_ru_physics(k,e) = fc%tend_ru_physics(k,e) + rb*me
  end do
 end do
 !$acc parallel loop gang vector collapse(2) default(present) if(on_card)
 do c = 1, ncells
  do k = 1, nz
   coeff = (1. + rv/rgas * fc%scalars1(i_qv,k,c))
   tend_th(k,c) = coeff * tend_th(k,c) + rv/rgas * fc%theta_m1(k,c) * fc%tend_scalars(i_qv,k,c) / coeff
   fc%tend_rtheta_physics(k,c) = fc%tend_rtheta_physics(k,c) + tend_th(k,c)
  end do
 end do
 ! the land diagnostics the radiation reads come from the land-surface block; without it they stay at their start values
 ! (MPAS: sfc_emiss, sfc_albedo and the snow of the input); land columns first (order_columns)
 if (lsm_on .and. on_card) call order_columns(1, state%xland, state%xland)
 if (.not. lsm_on) then
  continue
 else if (ice_on) then
  !$acc parallel loop gang vector default(present) private(c) if(on_card)
  do n = 1, ncells
   c = n
   if (on_card) c = col_order(n)
   call landdiag_ice_cell(c, state, lands, emiss, snow, albedo)
  end do
 else
 !$acc parallel loop gang vector default(present) private(c) if(on_card)
 do n = 1, ncells
  c = n
  if (on_card) c = col_order(n)
  call landdiag_cell(c, state, lands, emiss, snow, albedo)
 end do
 end if
 end subroutine

 ! zero the accumulators at the start of the pre-dynamics chain (pre_dynamics_step)
 subroutine run_zero(on_card)
 logical, intent(in) :: on_card
 integer :: c, k, n
 !$acc parallel loop gang vector collapse(2) default(present) if(on_card)
 do c = 1, nc1
  do k = 1, nz
   tend_th(k,c) = 0.
   ux(k,c) = 0.
   uy(k,c) = 0.
  end do
 end do
 !$acc parallel loop gang vector collapse(3) default(present) if(on_card)
 do c = 1, nc1
  do k = 1, nz
   do n = 1, num_scalars
    fc%tend_scalars(n,k,c) = 0.
   end do
  end do
 end do
 end subroutine

 ! ---- running a stage, with the location bookkeeping and the check ------------------------------------------------------------------
 subroutine exec_stage(ist, on_card, it, julian, state, lands, convection, drag_state)
 integer, intent(in) :: ist, it
 logical, intent(in) :: on_card
 real, intent(in) :: julian
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 ! A scheme that is 'off' has no stage, as MPAS's driver does not call it; the flags are read on the host only. The surface stage
 ! is the surface layer, the land stage Noah-MP, the sea-ice scheme and the 2 m diagnostics (MPAS's land-surface block).
 select case (ist)
 case (st_inputs)
  call run_inputs(on_card, it, julian, state, lands, convection, fc, cmp, m, s)
 case (st_surface)
  if (ice_on) then
   ! the sea-ice step and the glacier pack belong to the land-surface block: the stage has work with either scheme on
   if (.not. sfclayer_on .and. .not. lsm_on) return
   call run_surface_ice(on_card, it, state, lands, convection, m)
  else
   if (.not. sfclayer_on) return
   call run_surface(on_card, it, state, lands, convection)
  end if
 case (st_land)
  if (.not. lsm_on) return
  if (ice_on) then
   call run_land_ice(on_card, it, state, lands, convection, m)
  else
   call run_land(on_card, state, lands, convection, m)
  end if
 case (st_pbl)
  if (pbl_on) call run_pbl(on_card, it, state, fc)
 case (st_gwdo)
  if (gwdo_on) call run_gwdo(on_card, state, drag_state, m)
 case (st_gf)
  if (conv_on) then
   call run_gf(on_card, it, state, convection, fc)
  else
   call run_nogf(on_card, state, fc)
  end if
 case (st_finish)
  call run_finish(on_card, state, lands)
 end select
 end subroutine

 ! it: the model step number (counted from the cold start), equal to state%timestep here; the MYNN and Grell-Freitas stages use it
 ! as itimestep
 subroutine run_stage(ist, it, julian, state, lands, convection, drag_state)
 integer, intent(in) :: ist, it
 real, intent(in) :: julian
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 real(kind=8) :: t0
 call timer_start(t0)
 if (check(ist)) then
  call need_host(state, lands, convection, drag_state)
  call bundle_to_device(state, lands, convection, drag_state)
  if (ice_on .and. ist == st_land) call keep_glacier(glacier_before)
  call exec_stage(ist, .false., it, julian, state, lands, convection, drag_state)
  ! the host result of the glacier columns and records is kept; the card run starts from the columns before the host run
  if (ice_on) h_exchange = exchange
  if (ice_on .and. ist == st_land) then
   call keep_glacier(glacier_host)
   call put_glacier(glacier_before)
  end if
  call exec_stage(ist, .true., it, julian, state, lands, convection, drag_state)
  call stash_host(state, lands, convection, drag_state)
  call bundle_to_host(state, lands, convection, drag_state)
  if (ist == st_finish) call stash_finish(.true.)
  call check_open(trim(stage_name(ist)), it)
  call check_state('state%', h_state, state)
  call check_land('land%', h_land, lands)
  call check_gf('gf%', h_conv, convection)
  call check_gwdo('gwdo%', h_drag, drag_state)
  if (ice_on) call check_ice()
  call compare_report('tend_th', h_th, tend_th)
  call compare_report('tend_scalars', h_sc, fc%tend_scalars)
  call compare_report('ux', h_ux, ux)
  call compare_report('uy', h_uy, uy)
  call compare_report('emiss', h_em, emiss)
  call compare_report('snow', h_sn, snow)
  call compare_report('albedo', h_al, albedo)
  if (ist == st_finish) then
   call compare_report('tend_ru_physics', h_tru, fc%tend_ru_physics)
   call compare_report('tend_rtheta_physics', h_trt, fc%tend_rtheta_physics)
   call compare_report('tend_rho_physics', h_trr, fc%tend_rho_physics)
  end if
  call check_close()
  call restore_host(state, lands, convection, drag_state)
  if (ice_on) exchange = h_exchange
  if (ice_on .and. ist == st_land) call put_glacier(glacier_host)
  if (ist == st_finish) call stash_finish(.false.)
  loc = 0
 else if (card(ist)) then
  call need_card(state, lands, convection, drag_state)
  call exec_stage(ist, .true., it, julian, state, lands, convection, drag_state)
 else
  call need_host(state, lands, convection, drag_state)
  call exec_stage(ist, .false., it, julian, state, lands, convection, drag_state)
 end if
 call physics_fault_check(card(ist) .or. check(ist), trim(stage_name(ist)), it)
 call timer_stop(physics_timer, t0)
 call timer_stop_stage(ist, t0)
 end subroutine

 subroutine timer_stop_stage(ist, t0)
 integer, intent(in) :: ist
 real(kind=8), intent(in) :: t0
 real(kind=8) :: now
 call timer_start(now)
 stage_s(ist) = stage_s(ist) + now - t0
 end subroutine

 subroutine phys_report()
 integer :: i
 do i = 2, nstages
  if (card(i) .or. any_card) print '(a,a8,a,f10.3,a,l1,a,l1)', 'stage seconds ', stage_name(i), ' ', stage_s(i), '  card=', card(i), ' check=', check(i)
 end do
 end subroutine

 ! the host result of a check, kept while the card result is brought to the host
 subroutine stash_host(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 if (.not. allocated(h_th)) then
  allocate(h_th(nz,nc1), h_sc(num_scalars,nz,nc1), h_ux(nz,nc1), h_uy(nz,nc1), h_em(ncells), h_sn(ncells), h_al(ncells))
  call allocate_state(h_state); call allocate_gf(h_conv)
  call allocate_land(h_land); call allocate_gwdo(h_drag)
 end if
 call copy_state(state, h_state); call copy_land(lands, h_land); call copy_gf(convection, h_conv); call copy_gwdo(drag_state, h_drag)
 if (ice_on) call copy_ice(sea_ice_cells, h_ice)
 h_th = tend_th; h_sc = fc%tend_scalars; h_ux = ux; h_uy = uy; h_em = emiss; h_sn = snow; h_al = albedo
 end subroutine

 subroutine restore_host(state, lands, convection, drag_state)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: lands
 type(gf_state_type), target, intent(inout) :: convection
 type(gwdo_state_type), target, intent(inout) :: drag_state
 call copy_state(h_state, state); call copy_land(h_land, lands); call copy_gf(h_conv, convection); call copy_gwdo(h_drag, drag_state)
 if (ice_on) call copy_ice(h_ice, sea_ice_cells)
 tend_th = h_th; fc%tend_scalars = h_sc; ux = h_ux; uy = h_uy; emiss = h_em; snow = h_sn; albedo = h_al
 end subroutine

 ! the finish stage also writes the dynamics tendencies, the edge tendency and the edge density: kept and compared like the bundle.
 ! (called with .true. after the host run and the card run: the host values are saved before the card result is brought over them;
 ! with .false. at the end: the host values come back.)
 subroutine stash_finish(save_host)
 logical, intent(in) :: save_host
 if (save_host) then
  if (.not. allocated(h_tru)) allocate(h_tru(nz,ne1), h_trt(nz,nc1), h_trr(nz,nc1))
  ! the card result is on the device; the host arrays still hold the host result: keep it, then bring the card values
  h_tru = fc%tend_ru_physics; h_trt = fc%tend_rtheta_physics; h_trr = fc%tend_rho_physics
  !$acc update self(fc%tend_ru_physics, fc%tend_rtheta_physics, fc%tend_rho_physics)
 else
  fc%tend_ru_physics = h_tru; fc%tend_rtheta_physics = h_trt; fc%tend_rho_physics = h_trr
 end if
 end subroutine

 ! The check of a stage with sea ice or land ice: the sea-ice fields and the glacier exchange records, host result against card result
 subroutine check_ice()
 integer :: n, g
 real, allocatable :: h(:,:), d(:,:)   ! the level fields of the records, host and card (check mode only)
 call compare_report('ice%tslb', h_ice%tslb, sea_ice_cells%tslb)
 call compare_report('ice%snow', h_ice%snow, sea_ice_cells%snow)
 call compare_report('ice%snowc', h_ice%snowc, sea_ice_cells%snowc)
 call compare_report('ice%z0', h_ice%z0, sea_ice_cells%z0)
 call compare_report('ice%albedo', h_ice%albedo, sea_ice_cells%albedo)
 call compare_report('ice%emiss', h_ice%emiss, sea_ice_cells%emiss)
 call compare_report('ice%grdflx', h_ice%grdflx, sea_ice_cells%grdflx)
 call compare_report('ice%acsnow', h_ice%acsnow, sea_ice_cells%acsnow)
 call compare_report('ice%acsnom', h_ice%acsnom, sea_ice_cells%acsnom)
 call compare_report('ice%sfcrunoff', h_ice%sfcrunoff, sea_ice_cells%sfcrunoff)
 n = n_glacier
 if (n == 0) return
 call compare_report('exchange%tsk', h_exchange(1:n)%tsk, exchange(1:n)%tsk)
 call compare_report('exchange%hfx', h_exchange(1:n)%hfx, exchange(1:n)%hfx)
 call compare_report('exchange%qfx', h_exchange(1:n)%qfx, exchange(1:n)%qfx)
 call compare_report('exchange%lh', h_exchange(1:n)%lh, exchange(1:n)%lh)
 call compare_report('exchange%qsfc', h_exchange(1:n)%qsfc, exchange(1:n)%qsfc)
 call compare_report('exchange%znt', h_exchange(1:n)%znt, exchange(1:n)%znt)
 call compare_report('exchange%snowh', h_exchange(1:n)%snowh, exchange(1:n)%snowh)
 call compare_report('exchange%t2', h_exchange(1:n)%t2, exchange(1:n)%t2)
 call compare_report('exchange%q2', h_exchange(1:n)%q2, exchange(1:n)%q2)
 call compare_report('exchange%th2', h_exchange(1:n)%th2, exchange(1:n)%th2)
 call compare_report('exchange%emiss', h_exchange(1:n)%emiss, exchange(1:n)%emiss)
 call compare_report('exchange%snow', h_exchange(1:n)%snow, exchange(1:n)%snow)
 call compare_report('exchange%albedo', h_exchange(1:n)%albedo, exchange(1:n)%albedo)
 ! the inputs the surface stage packs
 call compare_report('exchange%sr', h_exchange(1:n)%sr, exchange(1:n)%sr)
 call compare_report('exchange%rainncv', h_exchange(1:n)%rainncv, exchange(1:n)%rainncv)
 call compare_report('exchange%snowncv', h_exchange(1:n)%snowncv, exchange(1:n)%snowncv)
 call compare_report('exchange%graupelncv', h_exchange(1:n)%graupelncv, exchange(1:n)%graupelncv)
 call compare_report('exchange%raincv', h_exchange(1:n)%raincv, exchange(1:n)%raincv)
 call compare_report('exchange%gsw', h_exchange(1:n)%gsw, exchange(1:n)%gsw)
 call compare_report('exchange%glw', h_exchange(1:n)%glw, exchange(1:n)%glw)
 call compare_report('exchange%coszen', h_exchange(1:n)%coszen, exchange(1:n)%coszen)
 call compare_report('exchange%psfc', h_exchange(1:n)%psfc, exchange(1:n)%psfc)
 allocate(h(nz+1,n), d(nz+1,n))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%qv(1:nz); d(1:nz,g) = exchange(g)%qv(1:nz)
 end do
 call compare_report('exchange%qv', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%theta(1:nz); d(1:nz,g) = exchange(g)%theta(1:nz)
 end do
 call compare_report('exchange%theta', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%exner(1:nz); d(1:nz,g) = exchange(g)%exner(1:nz)
 end do
 call compare_report('exchange%exner', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%u(1:nz); d(1:nz,g) = exchange(g)%u(1:nz)
 end do
 call compare_report('exchange%u', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%v(1:nz); d(1:nz,g) = exchange(g)%v(1:nz)
 end do
 call compare_report('exchange%v', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(1:nz,g) = h_exchange(g)%p(1:nz); d(1:nz,g) = exchange(g)%p(1:nz)
 end do
 call compare_report('exchange%p', h(1:nz,:), d(1:nz,:))
 do g = 1, n
  h(:,g) = h_exchange(g)%z(1:nz+1); d(:,g) = exchange(g)%z(1:nz+1)
 end do
 call compare_report('exchange%z', h, d)
 end subroutine

 ! host tendencies to the card (finish stage on the host or checked)
 subroutine tendencies_to_device()
 !$acc update device(fc%tend_ru_physics, fc%tend_rtheta_physics, fc%tend_scalars)
 end subroutine
 ! ---- radiation --------------------------------------------------------------------------------------------------------------------
 ! RRTMG on the clock alarms on the card, by g-points (src/device/radiation_fast.f90, hand-written): the expressions of the generated
 ! device column code, split into stages over groups of rad_slots columns. The inputs are those of the host loop in lean_model.f90 (the
 ! same expressions); emiss, snow and albedo are the land diagnostics of run_finish, already on the card.
 subroutine radiation_device_start(mh, monthly)
 type(dyn_mesh_type), intent(inout) :: mh
 real, intent(in) :: monthly(12,59,ncells)
 character(32) :: sv
 integer :: n
 call get_environment_variable('LEAN_RAD_SLOTS', sv, status=n)
 if (n == 0) read(sv, *, iostat=n) rad_slots
 rad_slots = max(1, min(rad_slots, ncells))
 !$acc enter data copyin(mh%latCell, mh%lonCell, ice)   ! (the ozone of a column is computed on the card: ozone_at_lat_time)
 call rf_allocate(rad_slots)
 allocate(rf_cosz(ncells), rf_day(ncells))
 !$acc enter data create(rf_cosz, rf_day)
 print '(a,i0,a,f6.2,a)', ' radiation on the card (g-point path): ', rad_slots, ' columns in flight, ', rf_bytes()/2.**30, ' GiB of group arrays'
 end subroutine

 ! ---- radiation by g-points (radiation_fast.f90) ----
 ! inputs of a column as the host radiation loop of lean_model.f90 takes them (LEAN_RAD_BLCLOUD: MYNN's subgrid cloud added
 ! as bl_cloud_water does)
 ! (the state's columns directly, no column view: a view is 7 KB of the card's per-thread stack frame)
 subroutine rad_fast_prep(lw, j, c, coszrs, julian, solcon, state, mh, pin, emiss_c, albedo_c)
 !$acc routine seq
 logical, intent(in) :: lw
 integer, intent(in) :: j, c
 real, intent(in) :: coszrs, julian, solcon, pin(59), emiss_c, albedo_c
 type(state_type), target, intent(inout) :: state
 type(dyn_mesh_type), intent(inout) :: mh
 real :: qc_rad(max_levels), qi_rad(max_levels)
 integer :: k
 do k = 1, nz
  qc_rad(k) = state%qc(k,c)
  qi_rad(k) = state%qi(k,c)
 end do
#ifdef LEAN_RAD_BLCLOUD
 do k = 1, nz
  if (state%qc(k,c) < 1.e-6 .and. state%cldfra_bl(k,c) > 0.001) qc_rad(k) = state%qc(k,c) + state%qc_bl(k,c)
  if (state%qi(k,c) < 1.e-8 .and. state%cldfra_bl(k,c) > 0.001) qi_rad(k) = state%qi(k,c) + state%qi_bl(k,c)
 end do
#endif
 if (lw) then
  call rf_lw_prep(j,state%theta(:,c),state%exner(:,c),state%p(:,c),state%rho_moist(:,c),state%dz(:,c),mh%zgrid(:,c),state%qv(:,c),qc_rad,qi_rad,state%qs(:,c), &
   state%re_cloud(:,c),state%re_ice(:,c),state%re_snow(:,c),state%tsk(c),emiss_c,state%xland(c),mh%latCell(c),julian,pin)
 else
  call rf_sw_prep(j,coszrs,state%theta(:,c),state%exner(:,c),state%p(:,c),state%rho_moist(:,c),state%dz(:,c),mh%zgrid(:,c),state%qv(:,c),qc_rad,qi_rad,state%qs(:,c), &
   state%re_cloud(:,c),state%re_ice(:,c),state%re_snow(:,c),state%tsk(c),albedo_c,state%xland(c),mh%latCell(c),julian,solcon,pin)
 end if
 end subroutine

 ! the LW outputs, stored as the host radiation loop stores them
 subroutine rad_fast_lw_out(j, c, state, r, heating_c)
 !$acc routine seq
 integer, intent(in) :: j, c
 type(state_type), target, intent(inout) :: state
 type(radiation_cache), intent(inout) :: r
 real, intent(inout) :: heating_c(nz,2)
 real :: heating(max_levels), lw(11), lw_flux(4,max_lw_layers+1)
 call rf_lw_finish(j, state%exner(:,c), heating(1:nz), lw, lw_flux)
 heating_c(:,1) = heating(1:nz)
 r%lw_down = lw(1)
 r%olr = lw(2)
#ifdef LEAN_BUDGET
 r%flux(3) = lw(6) - lw(4)
 r%flux(4) = lw(10) - lw(8)
 r%flux(6) = lw_flux(2,nz+1) - lw_flux(1,nz+1)
 r%flux(7) = lw(7) - lw(5)
#endif
 end subroutine

 ! the SW outputs of a day column, stored as the host radiation loop stores them
 subroutine rad_fast_sw_out(j, c, state, r, heating_c)
 !$acc routine seq
 integer, intent(in) :: j, c
 type(state_type), target, intent(inout) :: state
 type(radiation_cache), intent(inout) :: r
 real, intent(inout) :: heating_c(nz,2)
#ifdef LEAN_NOAHMP_SWSPLIT
 real :: heating(max_levels), sw(17), sw_flux(4,max_sw_layers+1)
#else
 real :: heating(max_levels), sw(14), sw_flux(4,max_sw_layers+1)
#endif
 call rf_sw_finish(j, state%exner(:,c), heating(1:nz), sw, sw_flux)
 heating_c(:,2) = heating(1:nz)
 r%sw_net = sw(2); r%cos_zenith = sw(1)
#ifdef LEAN_BUDGET
 r%flux(1) = sw(6) - sw(4)
 r%flux(2) = sw(10) - sw(8)
 r%flux(5) = sw_flux(2,nz+1) - sw_flux(1,nz+1)
 r%flux(8) = sw(7) - sw(5)
#endif
#ifdef LEAN_NOAHMP_SWSPLIT
 r%sw_frac_dir_vis = sw(15)
 r%sw_frac_dir_nir = sw(16)
 r%sw_frac_dif_vis = sw(17)
#endif
 end subroutine

 ! the SW of a night column (radiation_sw_column with coszrs <= 0: everything zero but the cosine)
 subroutine rad_fast_sw_night(coszrs, r, heating_c)
 !$acc routine seq
 real, intent(in) :: coszrs
 type(radiation_cache), intent(inout) :: r
 real, intent(inout) :: heating_c(nz,2)
 heating_c(:,2) = 0.
 r%sw_net = 0.; r%cos_zenith = coszrs
#ifdef LEAN_BUDGET
 r%flux(1) = 0.; r%flux(2) = 0.; r%flux(5) = 0.; r%flux(8) = 0.
#endif
#ifdef LEAN_NOAHMP_SWSPLIT
 r%sw_frac_dir_vis = 0.; r%sw_frac_dir_nir = 0.; r%sw_frac_dif_vis = 0.
#endif
 end subroutine

 subroutine run_radiation(it, julian, declin, solcon, pin, monthly, state, mh, xtime0, gmt, do_lw, do_sw)
 logical, intent(in) :: do_lw, do_sw
 integer, intent(in) :: it
 real, intent(in) :: julian, declin, solcon, pin(59), monthly(12,59,ncells)
 type(state_type), target, intent(inout) :: state
 type(dyn_mesh_type), intent(inout) :: mh
 real, intent(in) :: xtime0, gmt
 integer :: c0, j, n, c, g, b, l, nday, i
 if (do_lw) then
  do c0 = 1, ncells, rad_slots
   n = min(rad_slots, ncells - c0 + 1)
   !$acc parallel loop gang vector vector_length(32) default(present) copyin(pin)
   do j = 1, n
    call rad_fast_prep(.true., j, c0+j-1, 0., julian, solcon, state, mh, pin, emiss(c0+j-1), albedo(c0+j-1))
   end do
   !$acc parallel loop gang vector collapse(2) default(present)
   do g = 1, ngptlw
    do j = 1, n
     call rf_lw_cloud(j, g)
    end do
   end do
   !$acc parallel loop gang vector collapse(3) default(present)
   do b = 1, nbndlw
    do l = 1, max_lw_layers
     do j = 1, n
      call rf_lw_gas(j, b, l)
     end do
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present)
   do g = 1, ngptlw
    do j = 1, n
     call rf_lw_transfer(j, g)
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present)
   do l = 0, max_lw_layers
    do j = 1, n
     call rf_lw_sums(j, l)
    end do
   end do
   !$acc parallel loop gang vector vector_length(32) default(present)
   do j = 1, n
    call rad_fast_lw_out(j, c0+j-1, state, rad(c0+j-1), rad_heating(:,:,c0+j-1))
   end do
  end do
 end if
 if (do_sw) then
  !$acc parallel loop gang vector default(present)
  do c = 1, ncells
   rf_cosz(c) = rf_sw_zenith(mh%latCell(c), mh%lonCell(c), xtime0, gmt, declin)
   if (.not. rf_cosz(c) > 0.) call rad_fast_sw_night(rf_cosz(c), rad(c), rad_heating(:,:,c))
  end do
  !$acc update self(rf_cosz)
  nday = 0
  do c = 1, ncells
   if (rf_cosz(c) > 0.) then
    nday = nday + 1
    rf_day(nday) = c
   end if
  end do
  !$acc update device(rf_day)
  do i = 1, nday, rad_slots
   n = min(rad_slots, nday - i + 1)
   !$acc parallel loop gang vector vector_length(32) default(present) copyin(pin)
   do j = 1, n
    call rad_fast_prep(.false., j, rf_day(i+j-1), rf_cosz(rf_day(i+j-1)), julian, solcon, state, mh, pin, &
     emiss(rf_day(i+j-1)), albedo(rf_day(i+j-1)))
   end do
   !$acc parallel loop gang vector collapse(3) default(present)
   do b = 1, nbndsw
    do l = 1, max_sw_layers
     do j = 1, n
      call rf_sw_gas(j, b, l)
     end do
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present)
   do g = 1, ngptsw
    do j = 1, n
     call rf_sw_transfer(j, g)
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present)
   do l = 1, max_sw_layers+1
    do j = 1, n
     call rf_sw_sums(j, l)
    end do
   end do
   !$acc parallel loop gang vector vector_length(32) default(present)
   do j = 1, n
    call rad_fast_sw_out(j, rf_day(i+j-1), state, rad(rf_day(i+j-1)), rad_heating(:,:,rf_day(i+j-1)))
   end do
  end do
 end if
 call physics_fault_check(.true., 'radiation', it)
 end subroutine

 ! LEAN_CHECK=radiation: rad holds the host result; the card result is compared with it, then the host result is put back
 subroutine radiation_check(it, julian, declin, solcon, pin, monthly, state, mh, xtime0, gmt, do_lw, do_sw)
 real, intent(in) :: xtime0, gmt
 logical, intent(in) :: do_lw, do_sw
 integer, intent(in) :: it
 real, intent(in) :: julian, declin, solcon, pin(59), monthly(12,59,ncells)
 type(state_type), target, intent(inout) :: state
 type(dyn_mesh_type), intent(inout) :: mh
 type(radiation_cache), allocatable :: h(:)
 real :: dlw, dsw, dnet, ddown, dcos, slw, ssw
 integer :: c
 h = rad
 h_heating = rad_heating
 call run_radiation(it, julian, declin, solcon, pin, monthly, state, mh, xtime0, gmt, do_lw, do_sw)
 !$acc update self(rad, rad_heating)
 dlw = 0.; dsw = 0.; dnet = 0.; ddown = 0.; dcos = 0.; slw = 0.; ssw = 0.
#ifdef LEAN_NOAHMP_SWSPLIT
 print '(a,i0,a,es10.3)', 'CHECK radiation step ', it, ': max |card-host| direct SW fraction ', &
  maxval([(abs(rad(c)%sw_frac_dir_vis + rad(c)%sw_frac_dir_nir - h(c)%sw_frac_dir_vis - h(c)%sw_frac_dir_nir), c = 1, ncells)])
#endif
 do c = 1, ncells
  dlw = max(dlw, maxval(abs(rad_heating(:,1,c) - h_heating(:,1,c)))); slw = max(slw, maxval(abs(h_heating(:,1,c))))
  dsw = max(dsw, maxval(abs(rad_heating(:,2,c) - h_heating(:,2,c)))); ssw = max(ssw, maxval(abs(h_heating(:,2,c))))
  dnet = max(dnet, abs(rad(c)%sw_net - h(c)%sw_net)); ddown = max(ddown, abs(rad(c)%lw_down - h(c)%lw_down))
  dcos = max(dcos, abs(rad(c)%cos_zenith - h(c)%cos_zenith))
 end do
 print '(a,i0,a,2(es10.3,a,es10.3,a),3(a,es10.3))', 'CHECK radiation step ', it, ': max |card-host| LW heating ', dlw, ' (of ', slw, &
  ')  SW heating ', dsw, ' (of ', ssw, ') theta/s', '  sw_net ', dnet, '  lw_down ', ddown, '  cos_zenith ', dcos
 rad(:) = h
 rad_heating = h_heating
 !$acc update device(rad, rad_heating)
 end subroutine
end module
