! The solar declination and the solar constant corrected for the Earth-Sun distance on a day of the year: radconst of MPAS 8.4.2
! (physics/mpas_atmphys_driver_radiation_sw.F), with the same expressions. Called before each LW or SW radiation call.
module radiation_solar
 use mpas_atmphys_constants,only: degrad,dpd,solcon_0
 implicit none
 private
 public :: radiation_solar_constants
contains
 recursive subroutine radiation_solar_constants(julian,declin,solcon)
 real,intent(in) :: julian
 real,intent(out) :: declin,solcon
 real :: obecl,sinob,sxlong,arg,djul,rjul,eccfac
 obecl=23.5*degrad
 sinob=sin(obecl)
 if(julian>=80.) sxlong=dpd*(julian-80.)
 if(julian<80.) sxlong=dpd*(julian+285.)
 sxlong=sxlong*degrad
 arg=sinob*sin(sxlong)
 declin=asin(arg)
 djul=julian*360./365.
 rjul=djul*degrad
 eccfac=1.000110+0.034221*cos(rjul)+0.001280*sin(rjul)+0.000719* &
  cos(2*rjul)+0.000077*sin(2*rjul)
#ifdef LEAN_RAD_2025
 ! Lean departure (build switch LEAN_RAD_2025, on in flavor lean, off in mpas; docs/fixes.md): the measured total solar irradiance, 1361 W/m2
 ! (SORCE and TSIS-1), instead of MPAS's 1370 (mpas_atmphys_constants.F)
 solcon=1361.*eccfac
#else
 solcon=solcon_0*eccfac
#endif
 end subroutine
end module
