! One record of the lateral boundary fields (lean_lbc lbc_next fills it from the lbc_in stream).
module dyn_lbc_data
 use config
 implicit none
 type dyn_lbc_type
  real, allocatable :: u(:,:)
  real, allocatable :: w(:,:)
  real, allocatable :: rho(:,:)
  real, allocatable :: theta(:,:)
  real, allocatable :: qv(:,:)
  real, allocatable :: qc(:,:)
  real, allocatable :: qr(:,:)
  ! The ice scalars: zeros when the lbc file has none, as MPAS reads them.
  real, allocatable :: qi(:,:), qs(:,:), qg(:,:), ni(:,:), nr(:,:)
 end type
end module
