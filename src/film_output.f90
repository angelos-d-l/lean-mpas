! The film output (LEAN_FILM=path): a few surface fields every 10 model minutes, for animations, computed the way MPAS 8.4.2
! computes its diag fields. Output only: the model reads nothing from here.
!   refl10cm_max  compute_radar_reflectivity (mpas_atmphys_driver_microphysics.F), from the eight column inputs that dyn_micro keeps
!                 at an output step (after mp_gt_driver, before microphysics_to_MPAS), calc_refl10cm of the lean Thompson module
!   rainnc        accumulated Thompson precipitation (dyn_micro)
!   rainc         update_convection_step2 (mpas_atmphys_driver_convection.F): rainc = rainc + cuprec*dt_dyn at every step
!   u10, v10      MYNN surface layer
!   mslp          compute_slp of diag_isobaric (shared with the history diagnostics, called without its log), with the temperature
!                 and the pressure in hPa as that file builds them
! File (unformatted stream, native byte order): ncells (4-byte integer), then per output step: it (4-byte integer, the model
! step), refl10cm_max, rainnc, rainc, u10, v10, mslp (ncells 4-byte reals each). tools/film_to_diag.py turns it into
! MPAS-style diag.*.nc files.
module film_output
 use mpas_kind_types, only: RKIND
 use config, only: nz, ncells, nc1, dt
 use dyn_config, only: rgas, rv, num_scalars, i_qv
 use diag_isobaric, only: compute_slp
 use lean_thompson, only: calc_refl10cm
 implicit none
 private
 public :: film_on, film_start, film_rain_step, film_write, film_fields
 real(kind=RKIND), parameter :: rvord = rv/rgas
 logical, save :: film_on = .false.
 integer, save :: film_unit = -1
 real, allocatable, save :: rainc(:), refl(:), mslp(:)
contains
 subroutine film_start(path)
 character(*), intent(in) :: path
 film_on = .true.
 allocate(rainc(ncells)); rainc = 0.
 allocate(refl(ncells))
 allocate(mslp(ncells))
 open(newunit=film_unit, file=path, access='stream', form='unformatted', status='replace')
 write(film_unit) ncells
 end subroutine

 ! every step, after the convection of the step
 subroutine film_rain_step(pratec)
 real, intent(in) :: pratec(ncells)
 integer :: c
 do c = 1, ncells
  rainc(c) = rainc(c) + pratec(c) * dt
 end do
 end subroutine

 ! at an output step, after the step: film_in from dyn_micro of this step, the state of the end of the step
 subroutine film_write(it, film_in, rainnc, u10, v10, theta_m, scalars, exner, pressure_p, pressure_base, zgrid)
 integer, intent(in) :: it
 real, intent(in) :: film_in(8,nz,nc1), rainnc(nc1), u10(ncells), v10(ncells)
 real, intent(in) :: theta_m(nz,nc1), scalars(num_scalars,nz,nc1), exner(nz,nc1), pressure_p(nz,nc1), pressure_base(nz,nc1)
 real, intent(in) :: zgrid(nz+1,nc1)
 call film_fields(film_in, theta_m, scalars, exner, pressure_p, pressure_base, zgrid, refl, mslp, .true., .true.)
 write(film_unit) it
 write(film_unit) refl, rainnc(1:ncells), rainc, u10, v10, mslp
 flush(film_unit)
 end subroutine

 ! refl10cm_max and mslp of every cell, as MPAS computes its diag fields (also the product output, product_output.f90)
 ! with_mslp false: refl only (the product output samples refl every 10 minutes and writes mslp once an hour); with_refl false:
 ! mslp only (refl computed on the card by film_refl_card of lean_model.f90)
 subroutine film_fields(film_in, theta_m, scalars, exner, pressure_p, pressure_base, zgrid, refl, mslp, with_mslp, with_refl)
 logical, intent(in) :: with_mslp, with_refl
 real, intent(in) :: film_in(8,nz,nc1)
 real, intent(in) :: theta_m(nz,nc1), scalars(num_scalars,nz,nc1), exner(nz,nc1), pressure_p(nz,nc1), pressure_base(nz,nc1)
 real, intent(in) :: zgrid(nz+1,nc1)
 real, intent(out) :: refl(ncells), mslp(ncells)
 real :: dbz1d(nz)
 real, allocatable :: temperature(:,:), pressure(:,:)
 integer :: c, k
 if (with_refl) then
 do c = 1, ncells
  ! film_in(:,k,c) = qv qc qr nr qs qg t p (dyn_micro.f90). A column where no level has rain (qr > 1e-12, R1 of
  ! calc_refl10cm) or snow or graupel (qs, qg > 1e-6, R2) has ze = 3e-22 at every level, about -35.2 dBZ, so its maximum
  ! is the floor -35: the same result as the call, which is skipped because most columns are dry at an output time.
  if (.not. any(film_in(3,:,c) > 1.e-12 .or. film_in(5,:,c) > 1.e-6 .or. film_in(6,:,c) > 1.e-6)) then
   refl(c) = -35._RKIND
   cycle
  end if
  dbz1d = -35._RKIND
  call calc_refl10cm(film_in(1,:,c), film_in(2,:,c), film_in(3,:,c), film_in(4,:,c), film_in(5,:,c), film_in(6,:,c), &
                     film_in(7,:,c), film_in(8,:,c), dbz1d, 1, nz, c, 1)
  do k = 1, nz
   dbz1d(k) = max(-35._RKIND, dbz1d(k))
  end do
  refl(c) = maxval(dbz1d(:))
 end do
 end if
 if (.not. with_mslp) return
 allocate(temperature(nz,ncells), pressure(nz,ncells))
 do c = 1, ncells
  do k = 1, nz
   pressure(k,c) = (pressure_p(k,c) + pressure_base(k,c)) / 100._RKIND
   temperature(k,c) = (theta_m(k,c)/(1._RKIND+rvord*scalars(i_qv,k,c)))*exner(k,c)
  end do
 end do
 call compute_slp(ncells, nz, num_scalars, temperature, zgrid(:,1:ncells), pressure, i_qv, scalars(:,:,1:ncells), mslp)
 mslp(:) = mslp(:) * 100.0
 end subroutine


end module
