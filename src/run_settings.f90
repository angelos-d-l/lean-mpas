! The run's time step, radiation intervals and dynamics settings, set once from the namelist at the start (the values
! below are placeholders until then).
module run_settings
 implicit none
 private
 public :: dt, radt_lw, radt_sw, n_microp, dt_dynamics, rk_timestep, rk_scalar_timestep, rk_sub_timestep
 public :: set_time_step, set_radiation_intervals, settings_to_device
 public :: apvm_upwinding, set_apvm_upwinding
 real, protected, save :: len_disp=3000.
 real, protected, save :: zd=22000.
 real, protected, save :: xnutr=0.2
 real, protected, save :: epssm_min=0.1
 real, protected, save :: epssm_max=0.5
 real, protected, save :: epssm_bottom=30000.
 real, protected, save :: epssm_top=50000.
 real, protected, save :: coef_3rd_order=0.25
 real, protected, save :: visc4_2dsmag=0.05
 real, protected, save :: smagorinsky_coef=0.125
 real, protected, save :: del4u_div_factor=10.
 real, protected, save :: smdiv=0.1
 real, protected, save :: relax_zone_divdamp_coef=6.0
 logical, protected, save :: apply_lbcs=.true.
 logical, save :: dynamics_set=.false., boundary_set=.false.
 ! config_apvm_upwinding: MPAS's Registry range is 0 to 1; 0 turns the upwinding of the potential vorticity off
 real, protected, save :: apvm_upwinding=0.
 logical, save :: apvm_set=.false.
 !$acc declare create(apvm_upwinding)
 public :: len_disp, zd, xnutr, epssm_min, epssm_max, epssm_bottom, epssm_top, &
  coef_3rd_order, visc4_2dsmag, smagorinsky_coef, del4u_div_factor, smdiv, relax_zone_divdamp_coef
 public :: apply_lbcs, set_dyn_config, set_apply_lbcs
 !$acc declare create(len_disp,zd,xnutr,epssm_min,epssm_max,epssm_bottom,epssm_top, &
 !$acc& coef_3rd_order,visc4_2dsmag,smagorinsky_coef,del4u_div_factor,smdiv,relax_zone_divdamp_coef,apply_lbcs)
 real, protected, save :: dt=30., radt_lw=1800., radt_sw=1800.
 integer, protected, save :: n_microp=1
 real, protected, save :: dt_dynamics=10.
 real, protected, save :: rk_timestep(3)= (/ 5., 5., 10. /)
 real, protected, save :: rk_scalar_timestep(3)= (/ 15., 15., 30. /)
 real, protected, save :: rk_sub_timestep(3)=2.5
 logical, save :: time_set=.false., radiation_set=.false.
 !$acc declare create(dt,radt_lw,radt_sw,n_microp,dt_dynamics,rk_timestep,rk_scalar_timestep,rk_sub_timestep)
contains
 subroutine set_time_step(seconds)
 real, intent(in) :: seconds
 if(time_set)then
  if(seconds/=dt)error stop 'the time step is set once per run'
  return
 endif
 if(.not.(seconds>0.))error stop 'the time step must be positive'
 dt=seconds
 ! n_microp: one Thompson substep per 90 s of the step (physics/mpas_atmphys_manager.F:686-693). The dynamics split
 ! (config_dynamics_split_steps = 3) and the acoustic steps (config_number_of_sub_steps = 4) are fixed: they are the only
 ! values lean accepts (docs/namelist.md), and the time steps below are the ones atm_srk3 derives from them
 ! (dynamics/mpas_atm_time_integration.F).
 n_microp=max(nint(dt/90.),1)
 dt_dynamics=dt/3.
 rk_timestep=[dt_dynamics/2.,dt_dynamics/2.,dt_dynamics]
 rk_scalar_timestep=[dt/2.,dt/2.,dt]
 rk_sub_timestep=dt_dynamics/4.
 time_set=.true.
 call settings_to_device()
 end subroutine

 subroutine set_radiation_intervals(lw,sw)
 real, intent(in) :: lw,sw
 if(radiation_set)then
  if(lw/=radt_lw.or.sw/=radt_sw)error stop 'the radiation intervals are set once per run'
  return
 endif
 if(.not.(lw>0..and.sw>0.))error stop 'the radiation intervals must be positive'
 radt_lw=lw
 radt_sw=sw
 radiation_set=.true.
 call settings_to_device()
 end subroutine

 subroutine set_dyn_config(len_disp_value,zd_value,xnutr_value,epssm_min_value,epssm_max_value,epssm_bottom_value, &
  epssm_top_value,coef_3rd_order_value,visc4_2dsmag_value,smagorinsky_coef_value,del4u_div_factor_value,smdiv_value,relax_zone_divdamp_coef_value)
 real, intent(in) :: len_disp_value,zd_value,xnutr_value,epssm_min_value,epssm_max_value,epssm_bottom_value, &
  epssm_top_value,coef_3rd_order_value,visc4_2dsmag_value,smagorinsky_coef_value,del4u_div_factor_value,smdiv_value,relax_zone_divdamp_coef_value
 if(.not.time_set)error stop 'set the time step before the dynamics settings'
 if(dynamics_set)then
  if(len_disp_value/=len_disp.or. &
   zd_value/=zd.or. &
   xnutr_value/=xnutr.or. &
   epssm_min_value/=epssm_min.or. &
   epssm_max_value/=epssm_max.or. &
   epssm_bottom_value/=epssm_bottom.or. &
   epssm_top_value/=epssm_top.or. &
   coef_3rd_order_value/=coef_3rd_order.or. &
   visc4_2dsmag_value/=visc4_2dsmag.or. &
   smagorinsky_coef_value/=smagorinsky_coef.or. &
   del4u_div_factor_value/=del4u_div_factor.or. &
   smdiv_value/=smdiv.or. &
   relax_zone_divdamp_coef_value/=relax_zone_divdamp_coef)error stop 'the dynamics settings are set once per run'
  return
 endif
 len_disp=len_disp_value
 zd=zd_value
 xnutr=xnutr_value
 epssm_min=epssm_min_value
 epssm_max=epssm_max_value
 epssm_bottom=epssm_bottom_value
 epssm_top=epssm_top_value
 coef_3rd_order=coef_3rd_order_value
 visc4_2dsmag=visc4_2dsmag_value
 smagorinsky_coef=smagorinsky_coef_value
 del4u_div_factor=del4u_div_factor_value
 smdiv=smdiv_value
 relax_zone_divdamp_coef=relax_zone_divdamp_coef_value
 dynamics_set=.true.
 call settings_to_device()
 end subroutine

 subroutine set_apply_lbcs(flag)
 logical, intent(in) :: flag
 if(boundary_set)then
  if(flag.neqv.apply_lbcs)error stop 'the boundary setting is set once per run'
  return
 endif
 apply_lbcs=flag
 boundary_set=.true.
 call settings_to_device()
 end subroutine

 ! Set once per run from the namelist's config_apvm_upwinding (mpas_atm_time_integration.F:7004); the same value again is allowed.
 subroutine set_apvm_upwinding(value)
 real, intent(in) :: value
 if(apvm_set)then
  if(value/=apvm_upwinding)error stop 'config_apvm_upwinding is set once per run'
  return
 endif
 if(.not.(value>=0..and.value<=1.))error stop 'config_apvm_upwinding must be in 0 to 1'
 apvm_upwinding=value
 apvm_set=.true.
 call settings_to_device()
 end subroutine

 subroutine settings_to_device()
 !$acc update device(len_disp,zd,xnutr,epssm_min,epssm_max,epssm_bottom,epssm_top, &
 !$acc& coef_3rd_order,visc4_2dsmag,smagorinsky_coef,del4u_div_factor,smdiv,relax_zone_divdamp_coef,apply_lbcs)
 !$acc update device(dt,radt_lw,radt_sw,n_microp,dt_dynamics,rk_timestep,rk_scalar_timestep,rk_sub_timestep)
 !$acc update device(apvm_upwinding)
 end subroutine
end module
