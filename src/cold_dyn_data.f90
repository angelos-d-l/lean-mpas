! The initial dynamics fields of a cold start, as lean_inputs read_state_inputs fills them from the init file.
module cold_dyn_data
 use config
 implicit none
 type cold_dyn_type
  real, allocatable :: u(:,:)
  real, allocatable :: w(:,:)
  real, allocatable :: theta(:,:)
  real, allocatable :: rho(:,:)
  real, allocatable :: rho_base(:,:)
  real, allocatable :: theta_base(:,:)
  real, allocatable :: qv(:,:)
  real, allocatable :: qc(:,:)
  real, allocatable :: qr(:,:)
  ! The ice scalars: zeros when the init file has none, as MPAS reads them.
  real, allocatable :: qi(:,:), qs(:,:), qg(:,:), ni(:,:), nr(:,:)
 end type
end module
