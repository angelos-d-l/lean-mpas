! The product output (LEAN_PRODUCTS=path): the raw fields from which tools/products_to_icon.py makes NetCDF files on a regular
! lat-lon grid with ICON-LAM's variable names and units. Output only: the model reads nothing from here. A stream file in the
! machine's byte order, integers int32 and reals real32.
! Every 10 minutes the hour's maxima are sampled: the column-maximum reflectivity (as the film's refl10cm_max) and the 10 m wind
! speed. At the end of every forecast hour one record:
!   it; 2-D (ncells): t2 q2 psfc u10 v10 wind10_max refl_max mslp rainnc rainc snownc graupelnc snowh xland;
!   3-D (nz,ncells): theta (dry) pressure qv qc qr qi qs qg u v (cell winds of the physics) cldfra_bl; w (nz+1,ncells).
! Header: ncells nz dt, then zgrid (nz+1,ncells). Accumulations are since the start; the maxima since the previous record.
module product_output
 use config, only: nz, ncells, nc1, dt
 use dyn_config, only: rgas, rv, num_scalars, i_qv, i_qc, i_qr, i_qi, i_qs, i_qg
 implicit none
 private
 public :: product_on, product_start, product_rain_step, product_sample, product_write
 logical, save :: product_on = .false.
 integer, save :: product_unit = -1
 real, allocatable, save :: rainc(:), refl_max(:), wind_max(:)
contains
 subroutine product_start(path, zgrid)
 character(*), intent(in) :: path
 real, intent(in) :: zgrid(nz+1,nc1)
 product_on = .true.
 allocate(rainc(ncells), refl_max(ncells), wind_max(ncells)); rainc = 0.; refl_max = -35.; wind_max = 0.
 open(newunit=product_unit, file=path, access='stream', form='unformatted', status='replace')
 write(product_unit) ncells, nz, dt
 write(product_unit) zgrid(:,1:ncells)
 end subroutine

 ! every step, after the convection of the step (as the film's rainc)
 subroutine product_rain_step(pratec)
 real, intent(in) :: pratec(ncells)
 rainc = rainc + pratec * dt
 end subroutine

 ! every 10 minutes
 subroutine product_sample(refl, u10, v10)
 real, intent(in) :: refl(ncells), u10(ncells), v10(ncells)
 refl_max = max(refl_max, refl)
 wind_max = max(wind_max, sqrt(u10*u10 + v10*v10))
 end subroutine

 subroutine product_write(it, t2, q2, psfc, u10, v10, mslp, rainnc, snownc, graupelnc, snowh, xland, &
                          theta_m, scalars, pressure_p, pressure_base, u, v, cldfra_bl, w)
 integer, intent(in) :: it
 real, intent(in) :: t2(ncells), q2(ncells), psfc(ncells), u10(ncells), v10(ncells), mslp(ncells)
 real, intent(in) :: rainnc(nc1), snownc(nc1), graupelnc(nc1), snowh(ncells), xland(ncells)
 real, intent(in) :: theta_m(nz,nc1), scalars(num_scalars,nz,nc1), pressure_p(nz,nc1), pressure_base(nz,nc1)
 real, intent(in) :: u(nz,ncells), v(nz,ncells), cldfra_bl(nz,ncells), w(nz+1,nc1)
 integer, parameter :: species(6) = [i_qv, i_qc, i_qr, i_qi, i_qs, i_qg]
 integer :: q
 write(product_unit) it
 write(product_unit) t2, q2, psfc, u10, v10, wind_max, refl_max, mslp, rainnc(1:ncells), rainc, snownc(1:ncells), &
                     graupelnc(1:ncells), snowh, xland
 write(product_unit) theta_m(:,1:ncells) / (1. + rv/rgas*scalars(i_qv,:,1:ncells)), pressure_p(:,1:ncells) + pressure_base(:,1:ncells)
 do q = 1, 6
  write(product_unit) scalars(species(q),:,1:ncells)
 end do
 write(product_unit) u, v, cldfra_bl, w(:,1:ncells)
 flush(product_unit)
 refl_max = -35.; wind_max = 0.
 end subroutine
end module
