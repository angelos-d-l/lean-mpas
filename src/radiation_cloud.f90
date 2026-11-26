! The cloud inputs of RRTMG: the Xu-Randall cloud fraction (radiation_cloud_fraction, calc_cldfraction) and cloud incidence
! (radiation_cloud_incidence, calc_cldincidence) of physics/mpas_atmphys_driver_cloudiness.F, and the effective radii relcalc and
! reicalc of physics/physics_wrf/module_ra_rrtmg_lw.F; formulas and order as in MPAS 8.4.2.
module radiation_cloud
 use config, only: nz
 implicit none
 private
 public :: radiation_cloud_fraction, radiation_cloud_incidence, relcalc, reicalc, retab
 real,parameter :: ep_2=287./461.6
 ! module_ra_rrtmg_lw.F retab, the table of reicalc (also used by radiation_lw.f90)
 real, parameter :: retab(95)=[                                                &
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234,   &
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,  &
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,  &
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,  &
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,  &
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,  &
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,  &
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,  &
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,  &
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,  &
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,  &
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,  &
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,  &
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,  &
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,  &
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639]
contains
 subroutine radiation_cloud_fraction(cldfrac,t_p,pres_p,qv,qc,qi,qs)
!=================================================================================================================

!input arguments:
 real,intent(in),dimension(nz):: qv,qc,qi,qs
 real,intent(in),dimension(nz):: t_p,pres_p

!output arguments:
 real,intent(out),dimension(nz):: cldfrac

!local variables:
 integer:: k

 real,parameter:: alpha0  = 100.
 real,parameter:: gamma   = 0.49
 real,parameter:: qcldmin = 1.e-12
 real,parameter:: pexp    = 0.25
 real,parameter:: rhgrid  = 1.0

 real,parameter:: svp1  = 0.61078
 real,parameter:: svp2  = 17.2693882
 real,parameter:: svpi2 = 21.8745584
 real,parameter:: svp3  = 35.86
 real,parameter:: svpi3 = 7.66
 real,parameter:: svpt0 = 273.15

 real:: esi,esw,qvsi,qvsw
 real:: arg,denom,qcld,qvs,rhum,subsat,weight

!-----------------------------------------------------------------------------------------------------------------

 do k = 1,nz
    cldfrac(k) = 0.
 enddo
 
 do k = 1,nz

!... calculation of the saturation mixing ratios over water and over ice (Murray, 1966):
    esw = 1000. * svp1 * exp(svp2 * (t_p(k) - svpt0) / (t_p(k) - svp3))
    esi = 1000. * svp1 * exp(svpi2 * (t_p(k) - svpt0) / (t_p(k) - svpi3))

    qvsw = ep_2 * esw / (pres_p(k) - esw)
    qvsi = ep_2 * esi / (pres_p(k) - esi)

    qcld = qc(k) + qi(k) + qs(k)
    if(qcld .lt. qcldmin) then
       weight = 0.
    else
       weight = (qi(k) + qs(k)) / qcld
    endif

    qvs = (1-weight) * qvsw + weight * qvsi
    rhum = qv(k) / qvs

    if(qcld .lt. qcldmin) then

       !assume that the cloud fraction is equal to 0. when the cloudy mixing ratio equals 0.
       cldfrac(k) = 0.

    elseif(rhum .ge. rhgrid) then
       !assume that the cloud fraction is equal to 1. when the relative humidity equal 100%.
       cldfrac(k) = 1.

    else
       !computation of the cloud fraction:
       subsat = max(1.e-10,rhgrid*qvs-qv(k))
       denom  = subsat**gamma
       arg    = max(-6.9,-alpha0*qcld/denom) ! exp(-6.9) = 0.001

       rhum = max(1.e-10,rhum)
       cldfrac(k) = (rhum/rhgrid)**pexp*(1.-exp(arg))
       if(cldfrac(k) .lt. 0.01) cldfrac(k) = 0.

    endif

 enddo

 end subroutine radiation_cloud_fraction
 subroutine radiation_cloud_incidence(cldfrac,qc,qi)
!input arguments (mpas_atmphys_driver_cloudiness.F calc_cldincidence, f_qc and f_qi true):
 real,intent(in),dimension(nz):: qc,qi
!output arguments:
 real,intent(out),dimension(nz):: cldfrac
 integer:: k
 real,parameter:: thresh = 1.e-06
 do k = 1,nz
    cldfrac(k) = 0.
 enddo
 do k = 1,nz
    if(qc(k)+qi(k) .gt. thresh) cldfrac(k) = 1.0
 enddo
 end subroutine radiation_cloud_incidence

  subroutine relcalc(ncol, pcols, pver, t, landfrac, landm, icefrac, rel, snowh)
    implicit none
    integer, intent(in) :: ncol
    integer, intent(in) :: pcols, pver
    real, intent(in) :: landfrac(pcols)      ! Land fraction
    real, intent(in) :: icefrac(pcols)       ! Ice fraction
    real, intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)
    real, intent(in) :: landm(pcols)         ! Land fraction ramping to zero over ocean
    real, intent(in) :: t(pcols,pver)        ! Temperature
    real, intent(out) :: rel(pcols,pver)      ! Liquid effective drop size (microns)
    integer i,k           ! Lon, lev indices
    real tmelt            ! freezing temperature of fresh water (K)
    real rliqland         ! liquid drop size if over land
    real rliqocean        ! liquid drop size if over ocean
    real rliqice          ! liquid drop size if over sea ice
    tmelt = 273.16
    rliqocean = 14.0
    rliqice   = 14.0
    rliqland  = 8.0
    do k=1,pver
       do i=1,ncol
          rel(i,k) = rliqland + (rliqocean-rliqland) * min(1.0,max(0.0,(tmelt-t(i,k))*0.05))
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0,max(0.0,snowh(i)*10.))
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0,max(0.0,1.0-landm(i)))
          rel(i,k) = rel(i,k) + (rliqice-rel(i,k)) * min(1.0,max(0.0,icefrac(i)))
       end do
    end do
  end subroutine relcalc
  subroutine reicalc(ncol, pcols, pver, t, re)

    integer, intent(in) :: ncol, pcols, pver
    real, intent(out) :: re(pcols,pver)
    real, intent(in) :: t(pcols,pver)
    real corr
    integer i
    integer k
    integer index
    do k=1,pver
       do i=1,ncol
          index = int(t(i,k)-179.)
          index = min(max(index,1),94)
          corr = t(i,k) - int(t(i,k))
          re(i,k) = retab(index)*(1.-corr)              &
               +retab(index+1)*corr
       end do
    end do
    return
  end subroutine reicalc

end module
