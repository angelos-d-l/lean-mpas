! Dynamics settings of the one configuration lean_mpas supports (namelist.atmosphere and Registry defaults).
! The run settings are applied once before the dynamics starts.
module dyn_config
 use run_settings, only: dt, dt_dynamics, rk_timestep, rk_scalar_timestep, rk_sub_timestep
 use run_settings, only: len_disp, zd, xnutr, epssm_min, epssm_max, epssm_bottom, epssm_top, &
  coef_3rd_order, visc4_2dsmag, smagorinsky_coef, del4u_div_factor, smdiv
 implicit none
 real, parameter :: sphere_radius = 6371229.
 ! lateral boundary zones counted in cells from the outer edge
 integer, parameter :: n_spec_zone = 2, n_relax_zone = 5

 ! constants of mpas_constants.F
 real, parameter :: gravity = 9.80616, rgas = 287., cp = 7.0*rgas/2.0, rv = 461.6
 ! scalar layout: qv qc qr qi qs qg are the moist species (qtot), then ni nr
 integer, parameter :: num_scalars = 8, moist_start = 1, moist_end = 6
 integer, parameter :: i_qv = 1, i_qc = 2, i_qr = 3, i_qi = 4, i_qs = 5, i_qg = 6, i_ni = 7, i_nr = 8
 ! number_sub_steps: the acoustic steps of each Runge-Kutta stage, n/2, n/2, n for config_number_of_sub_steps = 4 and order 2
 ! (atm_srk3); the dynamics runs 3 substeps per step (config_dynamics_split_steps); the stage lengths rk_timestep
 ! (dt_dynamics/2, dt_dynamics/2, dt_dynamics) and rk_scalar_timestep (dt/2, dt/2, dt) come from run_settings.
 integer, parameter :: number_sub_steps(3) = (/ 2, 2, 4 /)

 ! horizontal mixing: 2-D Smagorinsky with 4th-order hyperviscosity
 real, parameter :: prandtl = 1.0

 ! True when the physics inputs are computed on the host, which then reads the dynamics state every step; phys_mode sets
 ! it false when the inputs are computed on the card and not checked against the host
 logical, save :: host_inputs = .true.
end module
