! Daily vegetation update: physics_update_surface of MPAS (physics/mpas_atmphys_update_surface.F) as a Noah-MP
! run uses it. MPAS rings an alarm at the start time and every 24 h after (mpas_atmphys_manager.F:233-236, :550-553) and,
! before the physics of that step, interpolates the monthly greenness fraction to the day. Noah-MP takes
! VegFracGreen = vegfra/100 and VegFracAnnMax = gvfmax/100 (EnergyVarInTransferMod.F90:99-100), the land driver passing
! shdmax as gvfmax (mpas_atmphys_driver_lsm_noahmp.F:157-159). The background albedo and shdmin, which the update also
! sets, feed only the Noah (not Noah-MP) driver and are not computed here.
module surface_update
 implicit none
 private
 public :: vegetation_of_day
contains
 ! monthly_interp_to_date and the maximum of monthly_min_max (physics/mpas_atmphys_date_time.F) for one cell, with the
 ! original's dates (year*1000 + day of year, the 15th of each month as the monthly nodes) and its order of operations.
 subroutine vegetation_of_day(year, month, day, greenfrac, vegfra, shdmax)
 integer, intent(in) :: year, month, day
 real, intent(in) :: greenfrac(12)
 real, intent(out) :: vegfra, shdmax
 integer :: middle(0:13), target_date, l, month1, month2
 do l = 1, 12
  middle(l) = year*1000 + day_of_year(year, l, 15)
 end do
 middle(0) = middle(1) - 31
 middle(13) = middle(12) + 31
 target_date = year*1000 + day_of_year(year, month, day)
 vegfra = 0.
 do l = 0, 12
  if ((middle(l) < target_date) .and. (middle(l+1) >= target_date)) then
   if ((l == 0) .or. (l == 12)) then
    month1 = 12
    month2 = 1
   else
    month1 = l
    month2 = month1 + 1
   end if
   vegfra = (greenfrac(month2) * (target_date - middle(l)) + greenfrac(month1) * (middle(l+1) - target_date)) &
            / (middle(l+1) - middle(l))
   exit
  end if
 end do
 shdmax = greenfrac(1)
 do l = 2, 12
  if (greenfrac(l) > shdmax) shdmax = greenfrac(l)
 end do
 end subroutine

 ! get_julgmt's day of year, with its leap-year rule
 integer function day_of_year(year, month, day)
 integer, intent(in) :: year, month, day
 integer :: mmd(12), m
 mmd = [31,28,31,30,31,30,31,31,30,31,30,31]
 if (mod(year,4) == 0 .and. mod(year,100) /= 0 .or. mod(year,400) == 0) mmd(2) = 29
 day_of_year = day
 do m = 1, month - 1
  day_of_year = day_of_year + mmd(m)
 end do
 end function
end module
