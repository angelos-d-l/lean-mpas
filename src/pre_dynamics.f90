! The physics that runs BEFORE the dynamics in one model step, as in the MPAS driver: surface layer -> Noah-MP -> MYNN PBL ->
! GWDO -> Grell-Freitas, with the long/short-wave heating of the last RRTMG call (RRTMG itself runs at its times, outside this
! module). No scheme is applied to the state here: each one only writes its tendency, and physics_tendency.f90 couples the
! tendencies with the density into the arrays the dynamics reads. Thompson runs after the dynamics (dyn_micro.f90).
! The atmosphere (theta, qv, u, v, p, exner, rho, ...) in the column views is the input of the step and is not changed.
module pre_dynamics
 use config
 use dyn_config
 use model_state
 use gf_state
 use noahmp_state
 use noahmp_coupling,only:update_land,set_land_pressure,surface_diagnostics
 use physics_inputs,only:noahmp_interface_pressure
 use mynn,only:mynn_surface_column,mynn_pbl_column
 use grell_freitas,only:advance_gf
 use gwdo_state
 use orographic_drag,only:advance_gwdo
 use physics_tendency
 use sea_ice, only: ice_cell, open_water, ice_surface_layer, ice_step
 use land_ice, only: glacier_of, glacier_pack, glacier_step, glacier_unpack
 use schemes, only: conv_on, pbl_on, sfclayer_on, lsm_on, gwdo_on
 implicit none
 ! Result of the last radiation call of one cell. Heating (long wave, short wave) and the surface fluxes are applied at every
 ! step until the next call, one radiation interval later.
 type radiation_cache
  real :: sw_net, lw_down, cos_zenith, olr
#ifdef LEAN_NOAHMP_SWSPLIT
  real :: sw_frac_dir_vis,sw_frac_dir_nir,sw_frac_dif_vis
#endif
#ifdef LEAN_BUDGET
  real :: flux(8)   ! net downward W/m2: SW top, SW surface, LW top, LW surface, SW and LW at the model top, clear-sky LW and SW top
#endif
 end type
contains

 ! One cell. mass is rho_zz of the column, rad the cached radiation. ux, uy get the wind tendency (PBL + GWDO) in the
 ! cell layout of the dynamics, tend_th and tend_scalars get the coupled heating and moisture tendencies.
 subroutine pre_dynamics_cell(c, a, land, f, drag, z, itimestep, restarting, mass, rad, heating, tend_th, tend_scalars, ux, uy)
 integer, intent(in) :: c, itimestep
 type(column_view), intent(inout) :: a   ! (inout: ice_surface_layer points some of its fields at locals and back)
 type(noahmp_column), intent(inout) :: land
 type(gf_column), intent(in) :: f
 type(gwdo_column_state), intent(in) :: drag
 real, intent(in) :: z(nz+1), mass(nz)
 type(radiation_cache), intent(in) :: rad
 real, intent(in) :: heating(nz,2)
 logical, intent(in) :: restarting
 real, intent(inout) :: tend_th(nz,nc1), tend_scalars(num_scalars,nz,nc1), ux(nz,nc1), uy(nz,nc1)
 integer :: mynn_step
 real :: p_first, p_second
 type(open_water) :: water
 type(column_view) :: b
 real, target :: open_water_mask
 ! MYNN's first-step initialisation (initflag) runs when itimestep is 1, i.e. only on the first step of a cold start:
 ! state%timestep is in the lean restart file, so a restart continues the count (MPAS instead restarts its counter at
 ! one and suppresses initflag). restarting is .false. at every call.
 mynn_step = itimestep
 if (restarting) mynn_step = 0
 f%shortwave = rad%sw_net
 f%radiation_theta = heating(:,1) + heating(:,2)
 if (a%xland < 1.5) then
  land%net_shortwave = rad%sw_net
#ifdef LEAN_NOAHMP_SWSPLIT
  land%sw_frac_dir_vis = rad%sw_frac_dir_vis
  land%sw_frac_dir_nir = rad%sw_frac_dir_nir
  land%sw_frac_dif_vis = rad%sw_frac_dif_vis
#endif
  land%forcing_RadLwDownRefHeight = rad%lw_down
  land%config_domain_CosSolarZenithAngle = rad%cos_zenith
 end if
 ! Sea ice (sea_ice.f90) and land ice (land_ice.f90) as MPAS: a sea-ice cell is land with xice >= xice_threshold, a
 ! glacier cell land of the ice category without sea ice; every other cell takes the last branch.
 ! A scheme that is 'off' is not called, as MPAS's driver does not call it (mpas_atmphys_driver.F:268-369): the surface
 ! layer (:268-277); the land-surface block (:283-317), which holds Noah-MP, the sea-ice
 ! scheme and the 2 m diagnostics; the PBL (:320-330); GWDO (:333-342); convection (:351-369). The radiation heating is
 ! always added: it is zero when the radiation never ran (mpas_atmphys_todynamics.F:469-485).
 if (ice_cell(c)) then
  if (sfclayer_on) call ice_surface_layer(a, c, mynn_step, water)
  if (lsm_on) then
   call ice_step(a, c, water, rad%sw_net, rad%lw_down, f%raincv, z)
   ! the water formula of the 2 m diagnostics (mpas_atmphys_sfc_diagnostics.F:100): surface_diagnostics on a view
   ! whose land mask says water
   b = a
   open_water_mask = 2.
   b%xland => open_water_mask
   call surface_diagnostics(b, land)
  end if
 else if (glacier_of(c) > 0) then
  if (sfclayer_on) call mynn_surface_column(a, mynn_step)
  if (lsm_on) then
   call glacier_pack(a, glacier_of(c), rad%sw_net, rad%lw_down, rad%cos_zenith, f%raincv, z)
   call glacier_step(glacier_of(c), itimestep)
   call glacier_unpack(a, glacier_of(c))
  end if
 else
  if (sfclayer_on) call mynn_surface_column(a, mynn_step)
  if (lsm_on) then
   if (a%xland < 1.5) then
    land%rain_convective = f%raincv
    call noahmp_interface_pressure(z, a%p, p_first, p_second)
    call set_land_pressure(land, p_first, p_second)
    call update_land(a, land)
   end if
   call surface_diagnostics(a, land)
  end if
 end if
 if (pbl_on) then
  call mynn_pbl_column(a, mynn_step)
 end if
 if (gwdo_on) call advance_gwdo(a, drag, z)
 ! Grell-Freitas reads the PBL theta and qv tendencies (per unit mass) before they are coupled.
 if (pbl_on) call add_pbl(c, mass, a%t_theta, a%t_qv, a%t_qc, a%t_qi, a%t_qs, a%t_ni, tend_th, tend_scalars)
 if (conv_on) then
  call advance_gf(a, f, itimestep)
  call add_convection(c, mass, f%rthcuten, f%rqvcuten, f%rqccuten, f%rqicuten, tend_th, tend_scalars)
 end if
 call add_radiation(c, mass, heating(:,1), heating(:,2), tend_th)
 ! the wind tendency is the PBL's and GWDO's; with both off it stays at the zero set at the start of the step
 if (pbl_on .or. gwdo_on) then
  ux(:,c) = a%t_u
  uy(:,c) = a%t_v
 end if
 end subroutine

 ! The whole domain: every cell, then the edge winds and the modified-theta tendency. rho_zz, theta_m and scalars are the
 ! state at the start of the step; mass_edge the edge density; east, north the unit vectors at the cell centres.
 subroutine pre_dynamics_step(state, land_state, convection_state, drag_state, mesh, restarting, rad, heating, rho_zz, theta_m, scalars, &
                              mass_edge, cells_on_edge, edge_normal, east, north, ux, uy, tend_th, rublten_edge, &
                              tend_ru_physics, tend_rtheta_physics, tend_rho_physics, tend_scalars)
 type(state_type), target, intent(inout) :: state
 type(noahmp_state_type), target, intent(inout) :: land_state
 type(gf_state_type), target, intent(inout) :: convection_state
 type(gwdo_state_type), target, intent(inout) :: drag_state
 type(mesh_type), intent(in) :: mesh
 logical, intent(in) :: restarting
 type(radiation_cache), intent(in) :: rad(ncells)
 real, intent(in) :: heating(nz,2,ncells)
 real, intent(in) :: rho_zz(nz,nc1), theta_m(nz,nc1), scalars(num_scalars,nz,nc1), mass_edge(nz,ne1)
 real, intent(in) :: edge_normal(3,ne1), east(3,nc1), north(3,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(inout) :: ux(nz,nc1), uy(nz,nc1), tend_th(nz,nc1), tend_scalars(num_scalars,nz,nc1)
 real, intent(out) :: rublten_edge(nz,ne1), tend_ru_physics(nz,ne1), tend_rtheta_physics(nz,nc1), tend_rho_physics(nz,nc1)
 type(noahmp_column) :: land
 type(gf_column) :: f
 type(gwdo_column_state) :: drag
 type(column_view) :: a
 integer :: c
 !$omp do schedule(dynamic,64)
 do c = 1, nc1
  tend_th(:,c) = 0.
  tend_scalars(:,:,c) = 0.
  ux(:,c) = 0.
  uy(:,c) = 0.
 end do
 ! The cells are independent: every cell reads and writes only its own columns of the shared arrays and its own views of the
 ! schemes' state. The chunks of cells must be large: smaller chunks let threads write neighbouring per-cell scalars of the
 ! state, which share cache lines (false sharing).
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  land = noahmp_view(land_state, c)
  f = gf_view(convection_state, c)
  drag = gwdo_view(drag_state, c)
  a = view_column(state, c)
  call pre_dynamics_cell(c, a, land, f, drag, mesh%z(:,c), state%timestep, restarting, rho_zz(:,c), &
                         rad(c), heating(:,:,c), tend_th, tend_scalars, ux, uy)
 end do
 !$omp single
 state%timestep = state%timestep + 1
 !$omp end single
 call finish_physics_tendencies(ux, uy, tend_th, scalars, theta_m, mass_edge, cells_on_edge, edge_normal, east, north, &
                                rublten_edge, tend_ru_physics, tend_rtheta_physics, tend_rho_physics, tend_scalars)
 end subroutine
end module
