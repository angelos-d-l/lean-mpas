! The microphysics fields carried between steps (model_data's mb at a cold start).
module dyn_micro_data
 use config
 implicit none
 type dyn_micro_type
  real, allocatable :: nt_c(:)
  real, allocatable :: surface_pressure(:)
  real, allocatable :: rainnc(:)
  real, allocatable :: rainncv(:)
  real, allocatable :: snownc(:)
  real, allocatable :: snowncv(:)
  real, allocatable :: graupelnc(:)
  real, allocatable :: graupelncv(:)
  real, allocatable :: sr(:)
  real, allocatable :: tend_sfc_pressure(:)
  real, allocatable :: rainprod(:,:)
  real, allocatable :: evapprod(:,:)
  real, allocatable :: re_cloud(:,:)
  real, allocatable :: re_ice(:,:)
  real, allocatable :: re_snow(:,:)
 end type
end module
