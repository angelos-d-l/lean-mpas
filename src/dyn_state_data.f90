! The dynamics state type: the prognostic and diagnostic fields of the dynamics (model_data's s).
module dyn_state_data
 use config
 implicit none
 type dyn_state_type
  real, allocatable :: u(:,:)
  real, allocatable :: w(:,:)
  real, allocatable :: theta_m(:,:)
  real, allocatable :: rho_zz(:,:)
  real, allocatable :: scalars(:,:,:)
  real, allocatable :: exner(:,:)
  real, allocatable :: exner_base(:,:)
  real, allocatable :: rtheta_base(:,:)
  real, allocatable :: rho_base(:,:)
  real, allocatable :: rtheta_p(:,:)
  real, allocatable :: rho_p(:,:)
  real, allocatable :: pressure_p(:,:)
  real, allocatable :: pressure_base(:,:)
  real, allocatable :: ru(:,:)
  real, allocatable :: rw(:,:)
  real, allocatable :: ru_p(:,:)
  real, allocatable :: rw_p(:,:)
  real, allocatable :: rt_diabatic_tend(:,:)
 end type
end module
