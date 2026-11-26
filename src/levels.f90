! The number of model levels nz (and nz1 = nz + 1). It is set once per run from the mesh file through config's
! set_mesh_sizes; set_levels also updates the card's copy (acc declare).
! LEAN_NZ (./lean build --levels N, defined for this unit only) makes the count a constant of the build: the loops over
! levels then have a known trip count, which makes the card's dynamics faster. Such a build runs only meshes with that
! nVertLevels; the mesh read refuses another count (fixed_levels, src/io/lean_inputs.f90 sizes_fit).
module levels
 implicit none
 private
 public :: nz, nz1, fixed_levels, set_levels, reset_levels
#ifdef LEAN_NZ
 integer, parameter :: fixed_levels = LEAN_NZ
 integer, parameter :: default_levels = fixed_levels
 integer, parameter :: nz = fixed_levels, nz1 = fixed_levels + 1
#else
 integer, parameter :: fixed_levels = 0   ! the count comes from the mesh file
 integer, parameter :: default_levels = 45   ! placeholder until the mesh is read: the athens_3km level count, as config's default_* sizes
 integer, protected, save :: nz = default_levels, nz1 = default_levels + 1
 !$acc declare create(nz, nz1)
#endif
contains
 subroutine set_levels(n)
 integer, intent(in) :: n
#ifdef LEAN_NZ
 if (n /= nz) error stop 'the mesh levels differ from the level count of this build (./lean build --levels)'
#else
 nz = n
 nz1 = n + 1
 !$acc update device(nz, nz1)
#endif
 end subroutine

 subroutine reset_levels()
 call set_levels(default_levels)
 end subroutine
end module
