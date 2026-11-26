! Land ice: the exchange record of a glacier cell, what one land step passes between the model
! column and the cell's NoahmpIO column (land_ice.f90). Separate from land_ice.f90 so that the card build compiles only
! this part as device code (src/device/land_ice_exchange.f90, tools/gen_device_physics.py): the card fills and reads the
! records, the host runs MPAS's glacier driver on them (phys_dev.f90).
module land_ice_exchange
 use config, only: nz, ncells, max_levels
 use model_state, only: column_view, state_type
 implicit none
 private
 public :: glacier_of, n_glacier, glacier_exchange, exchange, allocate_exchange, glacier_pack, glacier_pack_state, glacier_unpack, allocate_glacier_of
 integer, allocatable, save :: glacier_of(:)   ! the column of a glacier cell in land_ice's glacier(:), 0 for every other cell
 integer, save :: n_glacier = 0
 ! One step of a glacier cell c: the forcing (sr ... z), the surface fields the other schemes change between two land
 ! steps (tsk ... snowh, in and out), the 2 m diagnostics and the radiation surface inputs (out).
 type glacier_exchange
  integer :: c
  real :: sr, rainncv, snowncv, graupelncv, raincv, gsw, glw, coszen, psfc
  real :: qv(max_levels), theta(max_levels), exner(max_levels), u(max_levels), v(max_levels), p(max_levels), z(max_levels+1)
  real :: tsk, hfx, qfx, lh, qsfc, znt, snowh
  real :: t2, q2, th2, emiss, snow, albedo, acsnow
 end type
 type(glacier_exchange), allocatable, save :: exchange(:)
contains
 subroutine allocate_glacier_of()
 allocate(glacier_of(ncells))
 glacier_of = 0
 end subroutine

 ! One record per glacier cell, at the end of the start (land_ice.f90 finish_glacier_start). Here, not there: in the
 ! card build this unit is compiled for the card, and the allocation also makes the card copy (declare create).
 ! Every word is set at allocation: the level arrays hold max_levels values, of which a run fills nz, and the restart
 ! writes the whole record.
 subroutine allocate_exchange()
 integer :: g
 allocate(exchange(max(n_glacier, 1)))
 do g = 1, size(exchange)
  exchange(g)%c = 0
  exchange(g)%sr = 0.; exchange(g)%rainncv = 0.; exchange(g)%snowncv = 0.; exchange(g)%graupelncv = 0.
  exchange(g)%raincv = 0.; exchange(g)%gsw = 0.; exchange(g)%glw = 0.; exchange(g)%coszen = 0.; exchange(g)%psfc = 0.
  exchange(g)%qv = 0.; exchange(g)%theta = 0.; exchange(g)%exner = 0.; exchange(g)%u = 0.; exchange(g)%v = 0.
  exchange(g)%p = 0.; exchange(g)%z = 0.
  exchange(g)%tsk = 0.; exchange(g)%hfx = 0.; exchange(g)%qfx = 0.; exchange(g)%lh = 0.; exchange(g)%qsfc = 0.
  exchange(g)%znt = 0.; exchange(g)%snowh = 0.
  exchange(g)%t2 = 0.; exchange(g)%q2 = 0.; exchange(g)%th2 = 0.; exchange(g)%emiss = 0.; exchange(g)%snow = 0.
  exchange(g)%albedo = 0.; exchange(g)%acsnow = 0.
 end do
 end subroutine

 ! The inputs of a step of glacier column g (cell column a): gsw, glw, coszen: net short-wave, downward long-wave and
 ! cosine of the zenith angle of the last radiation call; raincv: the convective rain of the last step; z: zgrid.
 subroutine glacier_pack(a, g, gsw, glw, coszen, raincv, z)
 type(column_view), intent(in) :: a
 integer, intent(in) :: g
 real, intent(in) :: gsw, glw, coszen, raincv, z(nz+1)
 integer :: k
 exchange(g)%sr = a%sr; exchange(g)%rainncv = a%rainncv; exchange(g)%snowncv = a%snowncv
 exchange(g)%graupelncv = a%graupelncv; exchange(g)%raincv = raincv
 exchange(g)%gsw = gsw; exchange(g)%glw = glw; exchange(g)%coszen = coszen; exchange(g)%psfc = a%psfc
 do k = 1, nz
  exchange(g)%qv(k) = a%qv(k); exchange(g)%theta(k) = a%theta(k); exchange(g)%exner(k) = a%exner(k)
  exchange(g)%u(k) = a%u(k); exchange(g)%v(k) = a%v(k); exchange(g)%p(k) = a%p(k); exchange(g)%z(k) = z(k)
 end do
 exchange(g)%z(nz+1) = z(nz+1)
 exchange(g)%tsk = a%tsk; exchange(g)%hfx = a%hfx; exchange(g)%qfx = a%qfx; exchange(g)%lh = a%lh
 exchange(g)%qsfc = a%qsfc; exchange(g)%znt = a%znt; exchange(g)%snowh = a%snowh
 end subroutine
 ! glacier_pack for column c of the state, without the column view (the same values)
 subroutine glacier_pack_state(state, c, g, gsw, glw, coszen, raincv, z)
 type(state_type), intent(in) :: state
 integer, intent(in) :: c
 integer, intent(in) :: g
 real, intent(in) :: gsw, glw, coszen, raincv, z(nz+1)
 integer :: k
 exchange(g)%sr = state%sr(c); exchange(g)%rainncv = state%rainncv(c); exchange(g)%snowncv = state%snowncv(c)
 exchange(g)%graupelncv = state%graupelncv(c); exchange(g)%raincv = raincv
 exchange(g)%gsw = gsw; exchange(g)%glw = glw; exchange(g)%coszen = coszen; exchange(g)%psfc = state%psfc(c)
 do k = 1, nz
  exchange(g)%qv(k) = state%qv(k,c); exchange(g)%theta(k) = state%theta(k,c); exchange(g)%exner(k) = state%exner(k,c)
  exchange(g)%u(k) = state%u(k,c); exchange(g)%v(k) = state%v(k,c); exchange(g)%p(k) = state%p(k,c); exchange(g)%z(k) = z(k)
 end do
 exchange(g)%z(nz+1) = z(nz+1)
 exchange(g)%tsk = state%tsk(c); exchange(g)%hfx = state%hfx(c); exchange(g)%qfx = state%qfx(c); exchange(g)%lh = state%lh(c)
 exchange(g)%qsfc = state%qsfc(c); exchange(g)%znt = state%znt(c); exchange(g)%snowh = state%snowh(c)
 end subroutine

 ! The results of the step of glacier column g into its cell column a.
 subroutine glacier_unpack(a, g)
 type(column_view), intent(in) :: a
 integer, intent(in) :: g
 a%tsk = exchange(g)%tsk; a%hfx = exchange(g)%hfx; a%qfx = exchange(g)%qfx; a%lh = exchange(g)%lh
 a%qsfc = exchange(g)%qsfc; a%znt = exchange(g)%znt; a%snowh = exchange(g)%snowh
 a%t2 = exchange(g)%t2; a%q2 = exchange(g)%q2; a%th2 = exchange(g)%th2
 end subroutine
end module
