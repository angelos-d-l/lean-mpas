! Optional storm output (LEAN_STORM=path; docs/storm-stream.md), on the native mesh. Every step: the 2-5 km AGL updraft
! helicity (positive layer-mean w times positive cell-centre relative vorticity, integrated between 2000 and 5000 m above
! ground) and the column maximum w over interfaces 1..nz, as MPAS 8.4.2 diagnostics/mpas_convective_diagnostics.F
! (convective_diagnostics_update: 154-158 for w, 169-189 for updraft helicity; integral_zstaggered: 548-565). At each
! record time and at the last step, one record: the maxima since the previous record (uh_max, w_max, at least 0) and the
! values of that step (uh, w_col); the maxima then restart from 0.
! Stream file, machine byte order, int32/real32: header ncells, nf, dt and nf 32-byte names; records it, uh_max, w_max,
! uh, w_col (ncells each).
module storm_output
 use config, only: ncells, nc1, nv1, nz, maxedges, vertex_degree, dt
 implicit none
 private
 public :: storm_on, storm_start, storm_step, storm_report
 logical, save :: storm_on = .false.
 integer, save :: storm_unit
 real, allocatable, save :: uh_max(:), w_max(:), uh(:), w_col(:)
 real(kind=8), save :: wall = 0.d0
contains
 subroutine storm_start(path, zgrid, area)
 character(*), intent(in) :: path
 real, intent(in) :: zgrid(nz+1,nc1), area(nc1)
 integer, parameter :: nf = 4
 character(32), parameter :: names(nf) = [character(32) :: 'uh_max', 'w_max', 'uh', 'w_col']
 storm_on = .true.
 allocate(uh_max(ncells), w_max(ncells), uh(ncells), w_col(ncells))
 !$acc enter data copyin(zgrid, area)
 !$acc enter data create(uh_max, w_max, uh, w_col)
 call storm_reset(uh_max, w_max)
 open(newunit=storm_unit, file=path, access='stream', form='unformatted', status='replace')
 write(storm_unit) ncells, nf, dt
 write(storm_unit) names
 end subroutine

 subroutine storm_reset(umax, wmax)
 real, intent(out) :: umax(ncells), wmax(ncells)
 integer :: c
 !$acc parallel loop gang vector default(present)
 do c = 1, ncells
  umax(c) = 0.
  wmax(c) = 0.
 end do
 end subroutine

 subroutine storm_step(it, nsteps, w, vorticity, vertices, degree, kite, kite_area, area, zgrid, record_due)
 logical, intent(in) :: record_due
 integer, intent(in) :: it, nsteps
 real, intent(in) :: w(nz+1,nc1), vorticity(nz,nv1), kite_area(vertex_degree,nv1), area(nc1), zgrid(nz+1,nc1)
 integer, intent(in) :: vertices(maxedges,nc1), degree(nc1), kite(maxedges,nc1)
 integer(kind=8) :: first, last, rate
 call system_clock(first, rate)
 call storm_kernel(w, vorticity, vertices, degree, kite, kite_area, area, zgrid, uh_max, w_max, uh, w_col)
 if (record_due.or.it==nsteps) then
  !$acc update self(uh_max, w_max, uh, w_col)
  write(storm_unit) it, uh_max, w_max, uh, w_col
  flush(storm_unit)
  call storm_reset(uh_max, w_max)
 end if
 call system_clock(last)
 wall = wall + real(last-first,8)/real(rate,8)
 end subroutine

 subroutine storm_kernel(w, vorticity, vertices, degree, kite, kite_area, area, zgrid, umax, wmax, u, wc)
 real, intent(in) :: w(nz+1,nc1), vorticity(nz,nv1), kite_area(vertex_degree,nv1), area(nc1), zgrid(nz+1,nc1)
 integer, intent(in) :: vertices(maxedges,nc1), degree(nc1), kite(maxedges,nc1)
 real, intent(inout) :: umax(ncells), wmax(ncells)
 real, intent(out) :: u(ncells), wc(ncells)
 integer :: c, k, i, v, j
 real :: s, uph, wmid, wpeak, zb, zt
 !$omp parallel do schedule(dynamic,64) private(k,i,v,j,s,uph,wmid,wpeak,zb,zt)
 !$acc parallel loop gang vector default(present)
 do c = 1, ncells
  uph = 0.
  wpeak = w(1,c)
  !$acc loop seq
  do k = 1, nz
   s = 0.
   ! cell-centre relative vorticity: the vertex vorticities weighted by the kite areas inside this cell, divided by the cell area.
   ! kite(i,c) is the slot of cell c in cellsOnVertex of its i-th vertex, found once at startup (dyn_step.f90). Each cell gathers
   ! its own share, so no thread scatters from a vertex into the cells (a race on the card) and no step searches cellsOnVertex.
   !$acc loop seq
   do i = 1, degree(c)
    v = vertices(i,c)
    j = kite(i,c)
    s = s + kite_area(j,v)*vorticity(k,v)
   end do
   wpeak = max(wpeak, w(k,c))
   wmid = 0.5*(w(k,c)+w(k+1,c))
   zb = max(zgrid(k,c)-zgrid(1,c), 2000.)
   zt = min(zgrid(k+1,c)-zgrid(1,c), 5000.)
   uph = uph + (max(0.,wmid)*max(0.,s/area(c)))*max(0.,zt-zb)
  end do
  u(c) = uph
  wc(c) = wpeak
  umax(c) = max(umax(c), uph)
  wmax(c) = max(wmax(c), wpeak)
 end do
 end subroutine

 subroutine storm_report(nsteps)
 integer, intent(in) :: nsteps
 print '(a,2es23.14)', 'storm wall total/per_step ', wall, wall/real(max(1,nsteps),8)
 end subroutine
end module
