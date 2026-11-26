! The orographic-drag fields of every cell (gwdo_state_type) and the one-column view of them (gwdo_column_state);
! gwdo_view points a view at cell c.
module gwdo_state
 use config,only:nz,ncells
 implicit none
 type gwdo_column_state
  real,pointer::terrain_stddev,convexity
  real,pointer,contiguous::asymmetry(:),length_fraction(:)
  real,pointer,contiguous::stress_u(:),stress_v(:),increment_u(:),increment_v(:)
  real,pointer::surface_u,surface_v
 end type
 ! Allocated once at start by allocate_gwdo; the memory is not zero until zero_gwdo (phys_state).
 type gwdo_state_type
  real,allocatable::terrain_stddev(:),convexity(:),asymmetry(:,:),length_fraction(:,:)
  real,allocatable::stress_u(:,:),stress_v(:,:),increment_u(:,:),increment_v(:,:)
  real,allocatable::surface_u(:),surface_v(:)
 end type
contains
 subroutine allocate_gwdo(s)
 type(gwdo_state_type),intent(inout)::s
 allocate(s%terrain_stddev(ncells))
 allocate(s%convexity(ncells))
 allocate(s%asymmetry(4,ncells))
 allocate(s%length_fraction(4,ncells))
 allocate(s%stress_u(nz,ncells))
 allocate(s%stress_v(nz,ncells))
 allocate(s%increment_u(nz,ncells))
 allocate(s%increment_v(nz,ncells))
 allocate(s%surface_u(ncells))
 allocate(s%surface_v(ncells))
 end subroutine
 function gwdo_view(state,c)result(column)
 type(gwdo_state_type),target,intent(inout)::state
 integer,intent(in)::c
 type(gwdo_column_state)::column
 column%terrain_stddev=>state%terrain_stddev(c);column%convexity=>state%convexity(c)
 column%asymmetry=>state%asymmetry(:,c);column%length_fraction=>state%length_fraction(:,c)
 column%stress_u=>state%stress_u(:,c);column%stress_v=>state%stress_v(:,c)
 column%increment_u=>state%increment_u(:,c);column%increment_v=>state%increment_v(:,c)
 column%surface_u=>state%surface_u(c);column%surface_v=>state%surface_v(c)
 end function
end module
