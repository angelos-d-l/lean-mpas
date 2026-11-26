! The data of the running model that more than one module needs: mesh, reference state, dynamics state and tendencies, boundary data, the
! Thompson fields and the physics scratch arrays. They live in one module so that the card kernels in phys_dev.f90 and the driver in
! lean_model.f90 see the same objects.
module model_data
 use config
 use dyn_mesh_data
 use dyn_state_data
 use dyn_micro_data
 use dyn_step
 use dyn_micro
 use dyn_lbc_data
 use dyn_boundary
 use pre_dynamics, only: radiation_cache
 use model_start, only: start_info
 implicit none
 type(dyn_mesh_type), target, save :: m
 type(dyn_state_type), target, save :: s
 type(dyn_micro_type), save :: mb
 type(dyn_grid), save :: g
 type(dyn_fields), save :: fc
 type(micro_fields), save :: cmp
 type(dyn_lbc_type), save :: record
 type(dyn_lbc), target, save :: bl
 type(start_info), save :: st                ! start descriptor of the run (model_start.f90)
 integer, save :: lbc_start_sec              ! the start on the clock of the boundary files: seconds after 00 UTC of the start day
 type(radiation_cache), allocatable, target, save :: rad(:)
 real, allocatable, target, save :: rad_heating(:,:,:) ! (nz,2,ncells): LW and SW heating
 ! rbe: the PBL+GWDO wind tendency at the edges (rublten_edge of pre_dynamics_step), host only
 real, allocatable, save :: east(:,:), north(:,:), mass_edge(:,:), ux(:,:), uy(:,:), tend_th(:,:), rbe(:,:)
 ! the surface inputs of the radiation: emiss, snow, albedo from the land diagnostics of every step (land cells; water keeps
 ! its start values); ice the sea-ice fraction (start or surface update)
 real, allocatable, save :: emiss(:), ice(:), snow(:), albedo(:)
contains
 subroutine allocate_radiation_cache()
 integer :: c
 allocate(rad(ncells), rad_heating(nz,2,ncells))
 rad_heating = 0.
 rad%olr = 0.
 rad%sw_net = 0.; rad%lw_down = 0.; rad%cos_zenith = 0.
#ifdef LEAN_NOAHMP_SWSPLIT
 rad%sw_frac_dir_vis = 0.; rad%sw_frac_dir_nir = 0.; rad%sw_frac_dif_vis = 0.
#endif
 do c = 1, ncells
#ifdef LEAN_BUDGET
  rad(c)%flux = 0.
#endif
 end do
 end subroutine
end module
