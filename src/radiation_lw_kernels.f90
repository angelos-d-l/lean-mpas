! The RRTMG longwave kernels, translated from MPAS 8.4.2.
! Translated from RRTMG as MPAS 8.4.2 ships it (physics_wrf/module_ra_rrtmg_lw.F). RRTMG's notice (NOTICE.md), with
! the years as its modules state them: 2006-2008 in mcica_subcol_gen_lw and _sw, 2002-2009 in
! rrtmg_lw_cldprmc and rrtmg_lw_taumol, 2002-2008 in the others:
! Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).
! Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).
! Copyright 2006-2008, Atmospheric & Environmental Research, Inc. (AER).
! This software may be used, copied, or redistributed as long as it is
! not sold and this copyright notice is reproduced on each copy made.
! This model is provided as is without any express or implied warranties.
! (http://www.rtweb.aer.com/)
! Startup-only coefficient modules are shared read-only, not copied per column.
module lean_mcica_subcol_gen_lw
use radiation_sizes, only: max_lw_layers
use parkind, only : im => kind_im, rb => kind_rb
use parrrtm, only : nbndlw, ngptlw
use rrlw_con, only: grav, pi
use rrlw_wvn, only: ngb
use rrlw_vsn
implicit none
public :: mcica_subcol_lw, generate_stochastic_clouds
contains
recursive subroutine mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play,  cldfrac, ciwp, clwp, cswp, rei, rel, res, tauc,  hgt, idcor, juldat, lat,  cldfmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, taucmcl, ws)
 use radiation_scratch, only: lw_scratch
 type(lw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
integer(kind=im), intent(in) :: iplon
integer(kind=im), intent(in) :: ncol
integer(kind=im), intent(in) :: nlay
integer(kind=im), intent(in) :: icld
integer(kind=im), intent(in) :: permuteseed
integer(kind=im), intent(inout) :: irng
real(kind=rb), intent(in) :: play(:,:)
real(kind=rb), intent(in) :: hgt(:,:)
real(kind=rb), intent(in) :: cldfrac(:,:)
real(kind=rb), intent(in) :: tauc(:,:,:)
real(kind=rb), intent(in) :: ciwp(:,:)
real(kind=rb), intent(in) :: clwp(:,:)
real(kind=rb), intent(in) :: cswp(:,:)
real(kind=rb), intent(in) :: rei(:,:)
real(kind=rb), intent(in) :: rel(:,:)
real(kind=rb), intent(in) :: res(:,:)
integer(kind=im), intent(in) :: idcor
integer(kind=im), intent(in) :: juldat
real(kind=rb),    intent(in) :: lat
real(kind=rb), intent(out) :: cldfmcl(:,:,:)
real(kind=rb), intent(out) :: ciwpmcl(:,:,:)
real(kind=rb), intent(out) :: clwpmcl(:,:,:)
real(kind=rb), intent(out) :: cswpmcl(:,:,:)
real(kind=rb), intent(out) :: relqmcl(:,:)
real(kind=rb), intent(out) :: reicmcl(:,:)
real(kind=rb), intent(out) :: resnmcl(:,:)
real(kind=rb), intent(out) :: taucmcl(:,:,:)
integer(kind=im), parameter :: nsubclw = ngptlw
integer(kind=im) :: ilev
real(kind=rb) :: pmid(1, max_lw_layers)
real(kind=rb), parameter :: am1 = 1.4315_rb
real(kind=rb), parameter :: am2 = 2.1219_rb
real(kind=rb), parameter :: am4 = -25.584_rb
real(kind=rb), parameter :: amr = 7._rb
real(kind=rb) :: am3
real(kind=rb) :: decorr_len(1)
real(kind=rb), parameter :: Zo_default = 2500._rb
reicmcl(:ncol,:nlay) = rei(:ncol,:nlay)
relqmcl(:ncol,:nlay) = rel(:ncol,:nlay)
resnmcl(:ncol,:nlay) = res(:ncol,:nlay)
pmid(:ncol,:nlay) = play(:ncol,:nlay)*1.e2_rb
decorr_len(:) = Zo_default
call generate_stochastic_clouds (ncol, nlay, nsubclw, icld, irng, pmid, cldfrac, clwp, ciwp, cswp, tauc,  hgt, decorr_len,  cldfmcl, clwpmcl, ciwpmcl, cswpmcl, taucmcl, permuteseed, ws=ws)
end subroutine mcica_subcol_lw
recursive subroutine generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, ciwp, cswp, tauc,  hgt, decorr_len,  cld_stoch, clwp_stoch, ciwp_stoch, cswp_stoch, tauc_stoch, changeSeed, ws)
 use radiation_scratch, only: lw_scratch
!  CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
 type(lw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
integer(kind=im), intent(in) :: ncol
integer(kind=im), intent(in) :: nlay
integer(kind=im), intent(in) :: icld
integer(kind=im), intent(inout) :: irng
integer(kind=im), intent(in) :: nsubcol
integer(kind=im), optional, intent(in) :: changeSeed
real(kind=rb), intent(in) :: pmid(:,:)
real(kind=rb), intent(in) :: hgt(:,:)
real(kind=rb), intent(in) :: cld(:,:)
real(kind=rb), intent(in) :: clwp(:,:)
real(kind=rb), intent(in) :: ciwp(:,:)
real(kind=rb), intent(in) :: cswp(:,:)
real(kind=rb), intent(in) :: tauc(:,:,:)
real(kind=rb), intent(in) :: decorr_len(:)
real(kind=rb), intent(out) :: cld_stoch(:,:,:)
real(kind=rb), intent(out) :: clwp_stoch(:,:,:)
real(kind=rb), intent(out) :: ciwp_stoch(:,:,:)
real(kind=rb), intent(out) :: cswp_stoch(:,:,:)
real(kind=rb), intent(out) :: tauc_stoch(:,:,:)
real(kind=rb) :: cldf(1,max_lw_layers)
integer(kind=im) :: overlap
real(kind=rb)                   :: Zo_inv(1)
real(kind=rb), dimension(1,max_lw_layers) :: alpha
real(kind=rb), parameter :: cldmin = 1.0e-20_rb
integer(kind=im), dimension(1) :: seed1, seed2, seed3, seed4
real(kind=rb), dimension(1) :: rand_num
integer(kind=im) :: iseed
real(kind=rb) :: rand_num_mt
integer(kind=im) :: ilev, isubcol, i, n
overlap = icld
Zo_inv(:) = 1._rb / decorr_len(:)
do ilev = 1, nlay
do i = 1, ncol
cldf(i,ilev) = cld(i,ilev)
if (cldf(i,ilev) < cldmin) then
cldf(i,ilev) = 0._rb
endif
enddo
enddo
do i=1,ncol
if (pmid(i,1).lt.pmid(i,2)) then
stop 'MCICA_SUBCOL: KISSVEC SEED GENERATOR REQUIRES PMID FROM BOTTOM FOUR LAYERS.'
endif
seed1(i) = (pmid(i,1) - int(pmid(i,1)))  * 1000000000_im
seed2(i) = (pmid(i,2) - int(pmid(i,2)))  * 1000000000_im
seed3(i) = (pmid(i,3) - int(pmid(i,3)))  * 1000000000_im
seed4(i) = (pmid(i,4) - int(pmid(i,4)))  * 1000000000_im
enddo
do i=1,changeSeed
call kissvec(seed1, seed2, seed3, seed4, rand_num)
enddo
do isubcol = 1,nsubcol
do ilev = 1,nlay
call kissvec(seed1, seed2, seed3, seed4, rand_num)
ws%cdf(isubcol,:,ilev) = rand_num
enddo
enddo
do ilev = 2,nlay
do i = 1, ncol
do isubcol = 1, nsubcol
if (ws%cdf(isubcol, i, ilev-1) > 1._rb - cldf(i,ilev-1) ) then
ws%cdf(isubcol,i,ilev) = ws%cdf(isubcol,i,ilev-1)
else
ws%cdf(isubcol,i,ilev) = ws%cdf(isubcol,i,ilev) * (1._rb - cldf(i,ilev-1))
endif
enddo
enddo
enddo
do ilev = 1,nlay
do i=1,ncol
do isubcol=1,nsubcol
ws%iscloudy(isubcol,i,ilev) = (ws%cdf(isubcol,i,ilev) >= 1._rb - cldf(i,ilev))
enddo
enddo
enddo
do ilev = 1,nlay
do i = 1, ncol
do isubcol = 1, nsubcol
if (ws%iscloudy(isubcol,i,ilev) ) then
cld_stoch(isubcol,i,ilev) = 1._rb
clwp_stoch(isubcol,i,ilev) = clwp(i,ilev)
ciwp_stoch(isubcol,i,ilev) = ciwp(i,ilev)
cswp_stoch(isubcol,i,ilev) = cswp(i,ilev)
n = ngb(isubcol)
tauc_stoch(isubcol,i,ilev) = tauc(n,i,ilev)
else
cld_stoch(isubcol,i,ilev) = 0._rb
clwp_stoch(isubcol,i,ilev) = 0._rb
ciwp_stoch(isubcol,i,ilev) = 0._rb
cswp_stoch(isubcol,i,ilev) = 0._rb
tauc_stoch(isubcol,i,ilev) = 0._rb
endif
enddo
enddo
enddo
end subroutine generate_stochastic_clouds
recursive subroutine kissvec(seed1,seed2,seed3,seed4,ran_arr)
real(kind=rb), dimension(:), intent(inout)  :: ran_arr
integer(kind=im), dimension(:), intent(inout) :: seed1,seed2,seed3,seed4
integer(kind=im) :: i,sz,kiss
integer(kind=im) :: m, k, n
m(k, n) = ieor (k, ishft (k, n) )
sz = size(ran_arr)
do i = 1, sz
seed1(i) = 69069_im * seed1(i) + 1327217885_im
seed2(i) = m (m (m (seed2(i), 13_im), - 17_im), 5_im)
seed3(i) = 18000_im * iand (seed3(i), 65535_im) + ishft (seed3(i), - 16_im)
seed4(i) = 30903_im * iand (seed4(i), 65535_im) + ishft (seed4(i), - 16_im)
kiss = seed1(i) + seed2(i) + ishft (seed3(i), 16_im) + seed4(i)
ran_arr(i) = kiss*2.328306e-10_rb + 0.5_rb
end do
end subroutine kissvec
end module lean_mcica_subcol_gen_lw

module lean_rrtmg_lw_cldprmc
use radiation_sizes, only: max_lw_layers
use mpas_atmphys_utilities,only: physics_error_fatal
use parkind, only : im => kind_im, rb => kind_rb
use parrrtm, only : ngptlw, nbndlw
use rrlw_cld, only: abscld1, absliq0, absliq1,  absice0, absice1, absice2, absice3
use rrlw_wvn, only: ngb
use rrlw_vsn, only: hvrclc, hnamclc
implicit none
contains
recursive subroutine cldprmc(nlayers, inflag, iceflag, liqflag, cldfmc,  ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, ncbands, taucmc)
integer(kind=im), intent(in) :: nlayers
integer(kind=im), intent(in) :: inflag
integer(kind=im), intent(in) :: iceflag
integer(kind=im), intent(in) :: liqflag
real(kind=rb), intent(in) :: cldfmc(:,:)
real(kind=rb), intent(in) :: ciwpmc(:,:)
real(kind=rb), intent(in) :: clwpmc(:,:)
real(kind=rb), intent(in) :: cswpmc(:,:)
real(kind=rb), intent(in) :: relqmc(:)
real(kind=rb), intent(in) :: reicmc(:)
real(kind=rb), intent(in) :: resnmc(:)
integer(kind=im), intent(out) :: ncbands
real(kind=rb), intent(inout) :: taucmc(:,:)
integer(kind=im) :: lay
integer(kind=im) :: ib
integer(kind=im) :: ig
integer(kind=im) :: index
integer(kind=im) :: icb(nbndlw)
real(kind=rb) :: abscoice(ngptlw)
real(kind=rb) :: abscoliq(ngptlw)
real(kind=rb) :: abscosno(ngptlw)
real(kind=rb) :: cwp
real(kind=rb) :: radice
real(kind=rb) :: factor
real(kind=rb) :: fint
real(kind=rb) :: radliq
real(kind=rb) :: radsno
real(kind=rb), parameter :: eps = 1.e-6_rb
real(kind=rb), parameter :: cldmin = 1.e-20_rb
character*80 errmess
!                     User's Guide, Cooperative Institute for
data icb /1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5/
ncbands = 1
do lay = 1, nlayers
do ig = 1, ngptlw
cwp = ciwpmc(ig,lay) + clwpmc(ig,lay) + cswpmc(ig,lay)
if (cldfmc(ig,lay) .ge. cldmin .and.  (cwp .ge. cldmin .or. taucmc(ig,lay) .ge. cldmin)) then
if (inflag .eq. 0) then
return
elseif(inflag .eq. 1) then
stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'
elseif(inflag .ge. 2) then
radice = reicmc(lay)
if ((ciwpmc(ig,lay)+cswpmc(ig,lay)) .eq. 0.0_rb) then
abscoice(ig) = 0.0_rb
abscosno(ig) = 0.0_rb
elseif (iceflag .eq. 0) then
if (radice .lt. 10.0_rb) stop 'ICE RADIUS TOO SMALL'
abscoice(ig) = absice0(1) + absice0(2)/radice
abscosno(ig) = 0.0_rb
elseif (iceflag .eq. 1) then
if (radice .lt. 13.0_rb .or. radice .gt. 130._rb) stop  'ICE RADIUS OUT OF BOUNDS'
ncbands = 5
ib = icb(ngb(ig))
abscoice(ig) = absice1(1,ib) + absice1(2,ib)/radice
abscosno(ig) = 0.0_rb
elseif (iceflag .eq. 2) then
if (radice .lt. 5.0_rb .or. radice .gt. 131.0_rb) stop 'ICE RADIUS OUT OF BOUNDS'
ncbands = 16
factor = (radice - 2._rb)/3._rb
index = int(factor)
if (index .eq. 43) index = 42
fint = factor - float(index)
ib = ngb(ig)
abscoice(ig) =  absice2(index,ib) + fint *  (absice2(index+1,ib) - (absice2(index,ib)))
abscosno(ig) = 0.0_rb
elseif (iceflag .ge. 3) then
if (radice .lt. 5.0_rb .or. radice .gt. 140.0_rb) then
write(errmess,'(A,i5,i5,f8.2,f8.2)' )          'ERROR: ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'    ,ig, lay, ciwpmc(ig,lay), radice
call physics_error_fatal( errmess )
endif
ncbands = 16
factor = (radice - 2._rb)/3._rb
index = int(factor)
if (index .eq. 46) index = 45
fint = factor - float(index)
ib = ngb(ig)
abscoice(ig) =  absice3(index,ib) + fint *  (absice3(index+1,ib) - (absice3(index,ib)))
abscosno(ig) = 0.0_rb
endif
if (cswpmc(ig,lay).gt.0.0_rb .and. iceflag .eq. 5) then
radsno = resnmc(lay)
if (radsno .lt. 5.0_rb .or. radsno .gt. 140.0_rb) then
write(errmess,'(A,i5,i5,f8.2,f8.2)' )          'ERROR: SNOW GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'    ,ig, lay, cswpmc(ig,lay), radsno
call physics_error_fatal( errmess )
endif
ncbands = 16
factor = (radsno - 2._rb)/3._rb
index = int(factor)
if (index .eq. 46) index = 45
fint = factor - float(index)
ib = ngb(ig)
abscosno(ig) =  absice3(index,ib) + fint *  (absice3(index+1,ib) - (absice3(index,ib)))
endif
if (clwpmc(ig,lay) .eq. 0.0_rb) then
abscoliq(ig) = 0.0_rb
elseif (liqflag .eq. 0) then
abscoliq(ig) = absliq0
elseif (liqflag .eq. 1) then
radliq = relqmc(lay)
if (radliq .lt. 2.5_rb .or. radliq .gt. 60._rb) stop  'LIQUID EFFECTIVE RADIUS OUT OF BOUNDS'
index = int(radliq - 1.5_rb)
if (index .eq. 0) index = 1
if (index .eq. 58) index = 57
fint = radliq - 1.5_rb - float(index)
ib = ngb(ig)
abscoliq(ig) =  absliq1(index,ib) + fint *  (absliq1(index+1,ib) - (absliq1(index,ib)))
endif
taucmc(ig,lay) = ciwpmc(ig,lay) * abscoice(ig) +  clwpmc(ig,lay) * abscoliq(ig) +  cswpmc(ig,lay) * abscosno(ig)
endif
endif
enddo
enddo
end subroutine cldprmc
end module lean_rrtmg_lw_cldprmc

module lean_rrtmg_lw_rtrnmc
use radiation_sizes, only: max_lw_layers
use parkind, only : im => kind_im, rb => kind_rb
use parrrtm, only : mg, nbndlw, ngptlw
use rrlw_con, only: fluxfac, heatfac
use rrlw_wvn, only: delwave, ngb, ngs
use rrlw_tbl, only: tblint, bpade, tau_tbl, exp_tbl, tfn_tbl
use rrlw_vsn, only: hvrrtc, hnamrtc
implicit none
real(kind=rb),parameter :: wtdiff=0.5_rb
real(kind=rb),parameter :: rec_6=0.166667_rb
real(kind=rb),parameter :: a0(nbndlw)=[ 1.66_rb,  1.55_rb,  1.58_rb,  1.66_rb,  1.54_rb, 1.454_rb,  1.89_rb,  1.33_rb,  1.668_rb,  1.66_rb,  1.66_rb,  1.66_rb,  1.66_rb,  1.66_rb,  1.66_rb,  1.66_rb ]
real(kind=rb),parameter :: a1(nbndlw)=[ 0.00_rb,  0.25_rb,  0.22_rb,  0.00_rb,  0.13_rb, 0.446_rb, -0.10_rb,  0.40_rb,  -0.006_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb ]
real(kind=rb),parameter :: a2(nbndlw)=[ 0.00_rb, -12.0_rb, -11.7_rb,  0.00_rb,  -0.72_rb,-0.243_rb,  0.19_rb,-0.062_rb,  0.414_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb,  0.00_rb ]
contains
recursive subroutine rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands,  cldfmc, taucmc, planklay, planklev, plankbnd,  pwvcm, fracs, taut,  totuflux, totdflux, fnet, htr,  totuclfl, totdclfl, fnetc, htrc , ws)
 use radiation_scratch, only: lw_scratch
 type(lw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
integer(kind=im), intent(in) :: nlayers
integer(kind=im), intent(in) :: istart
integer(kind=im), intent(in) :: iend
integer(kind=im), intent(in) :: iout
real(kind=rb), intent(in) :: pz(0:)
real(kind=rb), intent(in) :: pwvcm
real(kind=rb), intent(in) :: semiss(:)
real(kind=rb), intent(in) :: planklay(:,:)
real(kind=rb), intent(in) :: planklev(0:,:)
real(kind=rb), intent(in) :: plankbnd(:)
real(kind=rb), intent(in) :: fracs(:,:)
real(kind=rb), intent(in) :: taut(:,:)
integer(kind=im), intent(in) :: ncbands
real(kind=rb), intent(in) :: cldfmc(:,:)
real(kind=rb), intent(in) :: taucmc(:,:)
real(kind=rb), intent(out) :: totuflux(0:)
real(kind=rb), intent(out) :: totdflux(0:)
real(kind=rb), intent(out) :: fnet(0:)
real(kind=rb), intent(out) :: htr(0:)
real(kind=rb), intent(out) :: totuclfl(0:)
real(kind=rb), intent(out) :: totdclfl(0:)
real(kind=rb), intent(out) :: fnetc(0:)
real(kind=rb), intent(out) :: htrc(0:)
real(kind=rb) :: atot(max_lw_layers)
real(kind=rb) :: atrans(max_lw_layers)
real(kind=rb) :: bbugas(max_lw_layers)
real(kind=rb) :: bbutot(max_lw_layers)
real(kind=rb) :: clrurad(0:max_lw_layers)
real(kind=rb) :: clrdrad(0:max_lw_layers)
real(kind=rb) :: uflux(0:max_lw_layers)
real(kind=rb) :: dflux(0:max_lw_layers)
real(kind=rb) :: urad(0:max_lw_layers)
real(kind=rb) :: drad(0:max_lw_layers)
real(kind=rb) :: uclfl(0:max_lw_layers)
real(kind=rb) :: dclfl(0:max_lw_layers)
real(kind=rb) :: secdiff(nbndlw)
real(kind=rb) :: transcld, radld, radclrd, plfrac, blay, dplankup, dplankdn
real(kind=rb) :: odepth, odtot, odepth_rec, odtot_rec, gassrc
real(kind=rb) :: tblind, tfactot, bbd, bbdtot, tfacgas, transc, tausfac
real(kind=rb) :: rad0, reflect, radlu, radclru
integer(kind=im) :: icldlyr(max_lw_layers)
integer(kind=im) :: ibnd, ib, iband, lay, lev, l, ig
integer(kind=im) :: igc
integer(kind=im) :: iclddn
integer(kind=im) :: ittot, itgas, itr
do ibnd = 1,nbndlw
if (ibnd.eq.1 .or. ibnd.eq.4 .or. ibnd.ge.10) then
secdiff(ibnd) = 1.66_rb
else
secdiff(ibnd) = a0(ibnd) + a1(ibnd)*exp(a2(ibnd)*pwvcm)
if (secdiff(ibnd) .gt. 1.80_rb) secdiff(ibnd) = 1.80_rb
if (secdiff(ibnd) .lt. 1.50_rb) secdiff(ibnd) = 1.50_rb
endif
enddo
urad(0) = 0.0_rb
drad(0) = 0.0_rb
totuflux(0) = 0.0_rb
totdflux(0) = 0.0_rb
clrurad(0) = 0.0_rb
clrdrad(0) = 0.0_rb
totuclfl(0) = 0.0_rb
totdclfl(0) = 0.0_rb
do lay = 1, nlayers
urad(lay) = 0.0_rb
drad(lay) = 0.0_rb
totuflux(lay) = 0.0_rb
totdflux(lay) = 0.0_rb
clrurad(lay) = 0.0_rb
clrdrad(lay) = 0.0_rb
totuclfl(lay) = 0.0_rb
totdclfl(lay) = 0.0_rb
icldlyr(lay) = 0
do ig = 1, ngptlw
if (cldfmc(ig,lay) .eq. 1._rb) then
ib = ngb(ig)
ws%odcld(lay,ig) = secdiff(ib) * taucmc(ig,lay)
transcld = exp(-ws%odcld(lay,ig))
ws%abscld(lay,ig) = 1._rb - transcld
ws%efclfrac(lay,ig) = ws%abscld(lay,ig) * cldfmc(ig,lay)
icldlyr(lay) = 1
else
ws%odcld(lay,ig) = 0.0_rb
ws%abscld(lay,ig) = 0.0_rb
ws%efclfrac(lay,ig) = 0.0_rb
endif
enddo
enddo
igc = 1
do iband = istart, iend
if (iout.gt.0.and.iband.ge.2) igc = ngs(iband-1)+1
1000    continue
radld = 0._rb
radclrd = 0._rb
iclddn = 0
do lev = nlayers, 1, -1
plfrac = fracs(lev,igc)
blay = planklay(lev,iband)
dplankup = planklev(lev,iband) - blay
dplankdn = planklev(lev-1,iband) - blay
odepth = secdiff(iband) * taut(lev,igc)
if (odepth .lt. 0.0_rb) odepth = 0.0_rb
if (icldlyr(lev).eq.1) then
iclddn = 1
odtot = odepth + ws%odcld(lev,igc)
if (odtot .lt. 0.06_rb) then
atrans(lev) = odepth - 0.5_rb*odepth*odepth
odepth_rec = rec_6*odepth
gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
atot(lev) =  odtot - 0.5_rb*odtot*odtot
odtot_rec = rec_6*odtot
bbdtot =  plfrac * (blay+dplankdn*odtot_rec)
bbd = plfrac*(blay+dplankdn*odepth_rec)
radld = radld - radld * (atrans(lev) +  ws%efclfrac(lev,igc) * (1. - atrans(lev))) +  gassrc + cldfmc(igc,lev) *  (bbdtot * atot(lev) - gassrc)
drad(lev-1) = drad(lev-1) + radld
bbugas(lev) =  plfrac * (blay+dplankup*odepth_rec)
bbutot(lev) =  plfrac * (blay+dplankup*odtot_rec)
elseif (odepth .le. 0.06_rb) then
atrans(lev) = odepth - 0.5_rb*odepth*odepth
odepth_rec = rec_6*odepth
gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
odtot = odepth + ws%odcld(lev,igc)
tblind = odtot/(bpade+odtot)
ittot = tblint*tblind + 0.5_rb
tfactot = tfn_tbl(ittot)
bbdtot = plfrac * (blay + tfactot*dplankdn)
bbd = plfrac*(blay+dplankdn*odepth_rec)
atot(lev) = 1. - exp_tbl(ittot)
radld = radld - radld * (atrans(lev) +  ws%efclfrac(lev,igc) * (1._rb - atrans(lev))) +  gassrc + cldfmc(igc,lev) *  (bbdtot * atot(lev) - gassrc)
drad(lev-1) = drad(lev-1) + radld
bbugas(lev) = plfrac * (blay + dplankup*odepth_rec)
bbutot(lev) = plfrac * (blay + tfactot * dplankup)
else
tblind = odepth/(bpade+odepth)
itgas = tblint*tblind+0.5_rb
odepth = tau_tbl(itgas)
atrans(lev) = 1._rb - exp_tbl(itgas)
tfacgas = tfn_tbl(itgas)
gassrc = atrans(lev) * plfrac * (blay + tfacgas*dplankdn)
odtot = odepth + ws%odcld(lev,igc)
tblind = odtot/(bpade+odtot)
ittot = tblint*tblind + 0.5_rb
tfactot = tfn_tbl(ittot)
bbdtot = plfrac * (blay + tfactot*dplankdn)
bbd = plfrac*(blay+tfacgas*dplankdn)
atot(lev) = 1._rb - exp_tbl(ittot)
radld = radld - radld * (atrans(lev) +  ws%efclfrac(lev,igc) * (1._rb - atrans(lev))) +  gassrc + cldfmc(igc,lev) *  (bbdtot * atot(lev) - gassrc)
drad(lev-1) = drad(lev-1) + radld
bbugas(lev) = plfrac * (blay + tfacgas * dplankup)
bbutot(lev) = plfrac * (blay + tfactot * dplankup)
endif
else
if (odepth .le. 0.06_rb) then
atrans(lev) = odepth-0.5_rb*odepth*odepth
odepth = rec_6*odepth
bbd = plfrac*(blay+dplankdn*odepth)
bbugas(lev) = plfrac*(blay+dplankup*odepth)
else
tblind = odepth/(bpade+odepth)
itr = tblint*tblind+0.5_rb
transc = exp_tbl(itr)
atrans(lev) = 1._rb-transc
tausfac = tfn_tbl(itr)
bbd = plfrac*(blay+tausfac*dplankdn)
bbugas(lev) = plfrac * (blay + tausfac * dplankup)
endif
radld = radld + (bbd-radld)*atrans(lev)
drad(lev-1) = drad(lev-1) + radld
endif
if (iclddn.eq.1) then
radclrd = radclrd + (bbd-radclrd) * atrans(lev)
clrdrad(lev-1) = clrdrad(lev-1) + radclrd
else
radclrd = radld
clrdrad(lev-1) = drad(lev-1)
endif
enddo
rad0 = fracs(1,igc) * plankbnd(iband)
reflect = 1._rb - semiss(iband)
radlu = rad0 + reflect * radld
radclru = rad0 + reflect * radclrd
urad(0) = urad(0) + radlu
clrurad(0) = clrurad(0) + radclru
do lev = 1, nlayers
if (icldlyr(lev) .eq. 1) then
gassrc = bbugas(lev) * atrans(lev)
radlu = radlu - radlu * (atrans(lev) +  ws%efclfrac(lev,igc) * (1._rb - atrans(lev))) +  gassrc + cldfmc(igc,lev) *  (bbutot(lev) * atot(lev) - gassrc)
urad(lev) = urad(lev) + radlu
else
radlu = radlu + (bbugas(lev)-radlu)*atrans(lev)
urad(lev) = urad(lev) + radlu
endif
if (iclddn.eq.1) then
radclru = radclru + (bbugas(lev)-radclru)*atrans(lev)
clrurad(lev) = clrurad(lev) + radclru
else
radclru = radlu
clrurad(lev) = urad(lev)
endif
enddo
igc = igc + 1
if (igc .le. ngs(iband)) go to 1000
do lev = nlayers, 0, -1
uflux(lev) = urad(lev)*wtdiff
dflux(lev) = drad(lev)*wtdiff
urad(lev) = 0.0_rb
drad(lev) = 0.0_rb
totuflux(lev) = totuflux(lev) + uflux(lev) * delwave(iband)
totdflux(lev) = totdflux(lev) + dflux(lev) * delwave(iband)
uclfl(lev) = clrurad(lev)*wtdiff
dclfl(lev) = clrdrad(lev)*wtdiff
clrurad(lev) = 0.0_rb
clrdrad(lev) = 0.0_rb
totuclfl(lev) = totuclfl(lev) + uclfl(lev) * delwave(iband)
totdclfl(lev) = totdclfl(lev) + dclfl(lev) * delwave(iband)
enddo
enddo
totuflux(0) = totuflux(0) * fluxfac
totdflux(0) = totdflux(0) * fluxfac
fnet(0) = totuflux(0) - totdflux(0)
totuclfl(0) = totuclfl(0) * fluxfac
totdclfl(0) = totdclfl(0) * fluxfac
fnetc(0) = totuclfl(0) - totdclfl(0)
do lev = 1, nlayers
totuflux(lev) = totuflux(lev) * fluxfac
totdflux(lev) = totdflux(lev) * fluxfac
fnet(lev) = totuflux(lev) - totdflux(lev)
totuclfl(lev) = totuclfl(lev) * fluxfac
totdclfl(lev) = totdclfl(lev) * fluxfac
fnetc(lev) = totuclfl(lev) - totdclfl(lev)
l = lev - 1
htr(l)=heatfac*(fnet(l)-fnet(lev))/(pz(l)-pz(lev))
htrc(l)=heatfac*(fnetc(l)-fnetc(lev))/(pz(l)-pz(lev))
enddo
htr(nlayers) = 0.0_rb
htrc(nlayers) = 0.0_rb
end subroutine rtrnmc
end module lean_rrtmg_lw_rtrnmc

module lean_rrtmg_lw_setcoef
use radiation_sizes, only: max_lw_layers
use parkind, only : im => kind_im, rb => kind_rb
use parrrtm, only : nbndlw, mg, maxxsec, mxmol
use rrlw_wvn, only: totplnk, totplk16
use rrlw_ref
use rrlw_vsn, only: hvrset, hnamset
implicit none
contains
recursive subroutine setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss,  coldry, wkl, wbroad,  laytrop, jp, jt, jt1, planklay, planklev, plankbnd,  colh2o, colco2, colo3, coln2o, colco, colch4, colo2,  colbrd, fac00, fac01, fac10, fac11,  rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,  rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,  rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,  selffac, selffrac, indself, forfac, forfrac, indfor,  minorfrac, scaleminor, scaleminorn2, indminor)
integer(kind=im), intent(in) :: nlayers
integer(kind=im), intent(in) :: istart
real(kind=rb), intent(in) :: pavel(:)
real(kind=rb), intent(in) :: tavel(:)
real(kind=rb), intent(in) :: tz(0:)
real(kind=rb), intent(in) :: tbound
real(kind=rb), intent(in) :: coldry(:)
real(kind=rb), intent(in) :: wbroad(:)
real(kind=rb), intent(in) :: wkl(:,:)
real(kind=rb), intent(in) :: semiss(:)
integer(kind=im), intent(out) :: laytrop
integer(kind=im), intent(out) :: jp(:)
integer(kind=im), intent(out) :: jt(:)
integer(kind=im), intent(out) :: jt1(:)
real(kind=rb), intent(out) :: planklay(:,:)
real(kind=rb), intent(out) :: planklev(0:,:)
real(kind=rb), intent(out) :: plankbnd(:)
real(kind=rb), intent(out) :: colh2o(:)
real(kind=rb), intent(out) :: colco2(:)
real(kind=rb), intent(out) :: colo3(:)
real(kind=rb), intent(out) :: coln2o(:)
real(kind=rb), intent(out) :: colco(:)
real(kind=rb), intent(out) :: colch4(:)
real(kind=rb), intent(out) :: colo2(:)
real(kind=rb), intent(out) :: colbrd(:)
integer(kind=im), intent(out) :: indself(:)
integer(kind=im), intent(out) :: indfor(:)
real(kind=rb), intent(out) :: selffac(:)
real(kind=rb), intent(out) :: selffrac(:)
real(kind=rb), intent(out) :: forfac(:)
real(kind=rb), intent(out) :: forfrac(:)
integer(kind=im), intent(out) :: indminor(:)
real(kind=rb), intent(out) :: minorfrac(:)
real(kind=rb), intent(out) :: scaleminor(:)
real(kind=rb), intent(out) :: scaleminorn2(:)
real(kind=rb), intent(out) ::  fac00(:), fac01(:),  fac10(:), fac11(:)
real(kind=rb), intent(out) ::  rat_h2oco2(:),rat_h2oco2_1(:),  rat_h2oo3(:),rat_h2oo3_1(:),  rat_h2on2o(:),rat_h2on2o_1(:),  rat_h2och4(:),rat_h2och4_1(:),  rat_n2oco2(:),rat_n2oco2_1(:),  rat_o3co2(:),rat_o3co2_1(:)
integer(kind=im) :: indbound, indlev0
integer(kind=im) :: lay, indlay, indlev, iband
integer(kind=im) :: jp1
real(kind=rb) :: stpfac, tbndfrac, t0frac, tlayfrac, tlevfrac
real(kind=rb) :: dbdtlev, dbdtlay
real(kind=rb) :: plog, fp, ft, ft1, water, scalefac, factor, compfp
stpfac = 296._rb/1013._rb
indbound = tbound - 159._rb
if (indbound .lt. 1) then
indbound = 1
elseif (indbound .gt. 180) then
indbound = 180
endif
tbndfrac = tbound - 159._rb - float(indbound)
indlev0 = tz(0) - 159._rb
if (indlev0 .lt. 1) then
indlev0 = 1
elseif (indlev0 .gt. 180) then
indlev0 = 180
endif
t0frac = tz(0) - 159._rb - float(indlev0)
laytrop = 0
do lay = 1, nlayers
indlay = tavel(lay) - 159._rb
if (indlay .lt. 1) then
indlay = 1
elseif (indlay .gt. 180) then
indlay = 180
endif
tlayfrac = tavel(lay) - 159._rb - float(indlay)
indlev = tz(lay) - 159._rb
if (indlev .lt. 1) then
indlev = 1
elseif (indlev .gt. 180) then
indlev = 180
endif
tlevfrac = tz(lay) - 159._rb - float(indlev)
do iband = 1, 15
if (lay.eq.1) then
dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
plankbnd(iband) = semiss(iband) *  (totplnk(indbound,iband) + tbndfrac * dbdtlev)
dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
endif
dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
enddo
iband = 16
if (istart .eq. 16) then
if (lay.eq.1) then
dbdtlev = totplk16(indbound+1) - totplk16(indbound)
plankbnd(iband) = semiss(iband) *  (totplk16(indbound) + tbndfrac * dbdtlev)
dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
planklev(0,iband) = totplk16(indlev0) +  t0frac * dbdtlev
endif
dbdtlev = totplk16(indlev+1) - totplk16(indlev)
dbdtlay = totplk16(indlay+1) - totplk16(indlay)
planklay(lay,iband) = totplk16(indlay) + tlayfrac * dbdtlay
planklev(lay,iband) = totplk16(indlev) + tlevfrac * dbdtlev
else
if (lay.eq.1) then
dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
plankbnd(iband) = semiss(iband) *  (totplnk(indbound,iband) + tbndfrac * dbdtlev)
dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
endif
dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
endif
plog = log(pavel(lay))
jp(lay) = int(36._rb - 5*(plog+0.04_rb))
if (jp(lay) .lt. 1) then
jp(lay) = 1
elseif (jp(lay) .gt. 58) then
jp(lay) = 58
endif
jp1 = jp(lay) + 1
fp = 5._rb *(preflog(jp(lay)) - plog)
jt(lay) = int(3._rb + (tavel(lay)-tref(jp(lay)))/15._rb)
if (jt(lay) .lt. 1) then
jt(lay) = 1
elseif (jt(lay) .gt. 4) then
jt(lay) = 4
endif
ft = ((tavel(lay)-tref(jp(lay)))/15._rb) - float(jt(lay)-3)
jt1(lay) = int(3._rb + (tavel(lay)-tref(jp1))/15._rb)
if (jt1(lay) .lt. 1) then
jt1(lay) = 1
elseif (jt1(lay) .gt. 4) then
jt1(lay) = 4
endif
ft1 = ((tavel(lay)-tref(jp1))/15._rb) - float(jt1(lay)-3)
water = wkl(1,lay)/coldry(lay)
scalefac = pavel(lay) * stpfac / tavel(lay)
if (plog .le. 4.56_rb) go to 5300
laytrop =  laytrop + 1
forfac(lay) = scalefac / (1.+water)
factor = (332.0_rb-tavel(lay))/36.0_rb
indfor(lay) = min(2, max(1, int(factor)))
forfrac(lay) = factor - float(indfor(lay))
selffac(lay) = water * forfac(lay)
factor = (tavel(lay)-188.0_rb)/7.2_rb
indself(lay) = min(9, max(1, int(factor)-7))
selffrac(lay) = factor - float(indself(lay) + 7)
scaleminor(lay) = pavel(lay)/tavel(lay)
scaleminorn2(lay) = (pavel(lay)/tavel(lay))  *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
factor = (tavel(lay)-180.8_rb)/7.2_rb
indminor(lay) = min(18, max(1, int(factor)))
minorfrac(lay) = factor - float(indminor(lay))
rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)
rat_h2oo3(lay)=chi_mls(1,jp(lay))/chi_mls(3,jp(lay))
rat_h2oo3_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(3,jp(lay)+1)
rat_h2on2o(lay)=chi_mls(1,jp(lay))/chi_mls(4,jp(lay))
rat_h2on2o_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(4,jp(lay)+1)
rat_h2och4(lay)=chi_mls(1,jp(lay))/chi_mls(6,jp(lay))
rat_h2och4_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(6,jp(lay)+1)
rat_n2oco2(lay)=chi_mls(4,jp(lay))/chi_mls(2,jp(lay))
rat_n2oco2_1(lay)=chi_mls(4,jp(lay)+1)/chi_mls(2,jp(lay)+1)
colh2o(lay) = 1.e-20_rb * wkl(1,lay)
colco2(lay) = 1.e-20_rb * wkl(2,lay)
colo3(lay) = 1.e-20_rb * wkl(3,lay)
coln2o(lay) = 1.e-20_rb * wkl(4,lay)
colco(lay) = 1.e-20_rb * wkl(5,lay)
colch4(lay) = 1.e-20_rb * wkl(6,lay)
colo2(lay) = 1.e-20_rb * wkl(7,lay)
if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
if (colco(lay) .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
colbrd(lay) = 1.e-20_rb * wbroad(lay)
go to 5400
5300    continue
forfac(lay) = scalefac / (1.+water)
factor = (tavel(lay)-188.0_rb)/36.0_rb
indfor(lay) = 3
forfrac(lay) = factor - 1.0_rb
selffac(lay) = water * forfac(lay)
scaleminor(lay) = pavel(lay)/tavel(lay)
scaleminorn2(lay) = (pavel(lay)/tavel(lay))  * (wbroad(lay)/(coldry(lay)+wkl(1,lay)))
factor = (tavel(lay)-180.8_rb)/7.2_rb
indminor(lay) = min(18, max(1, int(factor)))
minorfrac(lay) = factor - float(indminor(lay))
rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)
rat_o3co2(lay)=chi_mls(3,jp(lay))/chi_mls(2,jp(lay))
rat_o3co2_1(lay)=chi_mls(3,jp(lay)+1)/chi_mls(2,jp(lay)+1)
colh2o(lay) = 1.e-20_rb * wkl(1,lay)
colco2(lay) = 1.e-20_rb * wkl(2,lay)
colo3(lay) = 1.e-20_rb * wkl(3,lay)
coln2o(lay) = 1.e-20_rb * wkl(4,lay)
colco(lay) = 1.e-20_rb * wkl(5,lay)
colch4(lay) = 1.e-20_rb * wkl(6,lay)
colo2(lay) = 1.e-20_rb * wkl(7,lay)
if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
if (colco(lay)  .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
colbrd(lay) = 1.e-20_rb * wbroad(lay)
5400    continue
compfp = 1. - fp
fac10(lay) = compfp * ft
fac00(lay) = compfp * (1._rb - ft)
fac11(lay) = fp * ft1
fac01(lay) = fp * (1._rb - ft1)
selffac(lay) = colh2o(lay)*selffac(lay)
forfac(lay) = colh2o(lay)*forfac(lay)
enddo
end subroutine setcoef
end module lean_rrtmg_lw_setcoef

module lean_rrtmg_lw_taumol
use radiation_sizes, only: max_lw_layers
use parkind, only : im => kind_im, rb => kind_rb
use parrrtm, only : mg, nbndlw, maxxsec, ngptlw
use rrlw_con, only: oneminus
use rrlw_wvn, only: nspa, nspb
use rrlw_vsn, only: hvrtau, hnamtau
implicit none
contains
recursive subroutine taumol(nlayers, pavel, wx, coldry,  laytrop, jp, jt, jt1, planklay, planklev, plankbnd,  colh2o, colco2, colo3, coln2o, colco, colch4, colo2,  colbrd, fac00, fac01, fac10, fac11,  rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,  rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,  rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,  selffac, selffrac, indself, forfac, forfrac, indfor,  minorfrac, scaleminor, scaleminorn2, indminor,  fracs, taug)
integer(kind=im), intent(in) :: nlayers
real(kind=rb), intent(in) :: pavel(:)
real(kind=rb), intent(in) :: wx(:,:)
real(kind=rb), intent(in) :: coldry(:)
integer(kind=im), intent(in) :: laytrop
integer(kind=im), intent(in) :: jp(:)
integer(kind=im), intent(in) :: jt(:)
integer(kind=im), intent(in) :: jt1(:)
real(kind=rb), intent(in) :: planklay(:,:)
real(kind=rb), intent(in) :: planklev(0:,:)
real(kind=rb), intent(in) :: plankbnd(:)
real(kind=rb), intent(in) :: colh2o(:)
real(kind=rb), intent(in) :: colco2(:)
real(kind=rb), intent(in) :: colo3(:)
real(kind=rb), intent(in) :: coln2o(:)
real(kind=rb), intent(in) :: colco(:)
real(kind=rb), intent(in) :: colch4(:)
real(kind=rb), intent(in) :: colo2(:)
real(kind=rb), intent(in) :: colbrd(:)
integer(kind=im), intent(in) :: indself(:)
integer(kind=im), intent(in) :: indfor(:)
real(kind=rb), intent(in) :: selffac(:)
real(kind=rb), intent(in) :: selffrac(:)
real(kind=rb), intent(in) :: forfac(:)
real(kind=rb), intent(in) :: forfrac(:)
integer(kind=im), intent(in) :: indminor(:)
real(kind=rb), intent(in) :: minorfrac(:)
real(kind=rb), intent(in) :: scaleminor(:)
real(kind=rb), intent(in) :: scaleminorn2(:)
real(kind=rb), intent(in) ::  fac00(:), fac01(:),  fac10(:), fac11(:)
real(kind=rb), intent(in) ::  rat_h2oco2(:),rat_h2oco2_1(:),  rat_h2oo3(:),rat_h2oo3_1(:),  rat_h2on2o(:),rat_h2on2o_1(:),  rat_h2och4(:),rat_h2och4_1(:),  rat_n2oco2(:),rat_n2oco2_1(:),  rat_o3co2(:),rat_o3co2_1(:)
real(kind=rb), intent(out) :: fracs(:,:)
real(kind=rb), intent(out) :: taug(:,:)
call taugb1
call taugb2
call taugb3
call taugb4
call taugb5
call taugb6
call taugb7
call taugb8
call taugb9
call taugb10
call taugb11
call taugb12
call taugb13
call taugb14
call taugb15
call taugb16
contains
recursive subroutine taugb1
use parrrtm, only : ng1
use rrlw_kg01, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mn2, kb_mn2, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
real(kind=rb) :: pp, corradj, scalen2, tauself, taufor, taun2
do lay = 1, laytrop
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(1) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(1) + 1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
pp = pavel(lay)
corradj =  1.
if (pp .lt. 250._rb) then
corradj = 1._rb - 0.15_rb * (250._rb-pp) / 154.4_rb
endif
scalen2 = colbrd(lay) * scaleminorn2(lay)
do ig = 1, ng1
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) -  forref(indf,ig)))
taun2 = scalen2*(ka_mn2(indm,ig) +  minorfrac(lay) * (ka_mn2(indm+1,ig) - ka_mn2(indm,ig)))
taug(lay,ig) = corradj * (colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +  fac11(lay) * absa(ind1+1,ig))  + tauself + taufor + taun2)
fracs(lay,ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(1) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(1) + 1
indf = indfor(lay)
indm = indminor(lay)
pp = pavel(lay)
corradj =  1._rb - 0.15_rb * (pp / 95.6_rb)
scalen2 = colbrd(lay) * scaleminorn2(lay)
do ig = 1, ng1
taufor = forfac(lay) * (forref(indf,ig) +  forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig)))
taun2 = scalen2*(kb_mn2(indm,ig) +  minorfrac(lay) * (kb_mn2(indm+1,ig) - kb_mn2(indm,ig)))
taug(lay,ig) = corradj * (colh2o(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))  + taufor + taun2)
fracs(lay,ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb1
recursive subroutine taugb2
use parrrtm, only : ng2, ngs1
use rrlw_kg02, only : fracrefa, fracrefb, absa, ka, absb, kb,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
real(kind=rb) :: pp, corradj, tauself, taufor
do lay = 1, laytrop
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(2) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(2) + 1
inds = indself(lay)
indf = indfor(lay)
pp = pavel(lay)
corradj = 1._rb - .05_rb * (pp - 100._rb) / 900._rb
do ig = 1, ng2
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
taug(lay,ngs1+ig) = corradj * (colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +  fac11(lay) * absa(ind1+1,ig))  + tauself + taufor)
fracs(lay,ngs1+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(2) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(2) + 1
indf = indfor(lay)
do ig = 1, ng2
taufor =  forfac(lay) * (forref(indf,ig) +  forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig)))
taug(lay,ngs1+ig) = colh2o(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))  + taufor
fracs(lay,ngs1+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb2
recursive subroutine taugb3
use parrrtm, only : ng3, ngs2
use rrlw_ref, only : chi_mls
use rrlw_kg03, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mn2o, kb_mn2o, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmn2o, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mn2o, specparm_mn2o, specmult_mn2o,  fmn2o, fmn2omf, chi_n2o, ratn2o, adjfac, adjcoln2o
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor, n2om1, n2om2, absn2o
real(kind=rb) :: refrat_planck_a, refrat_planck_b, refrat_m_a, refrat_m_b
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,9)/chi_mls(2,9)
refrat_planck_b = chi_mls(1,13)/chi_mls(2,13)
refrat_m_a = chi_mls(1,3)/chi_mls(2,3)
refrat_m_b = chi_mls(1,13)/chi_mls(2,13)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mn2o = colh2o(lay) + refrat_m_a*colco2(lay)
specparm_mn2o = colh2o(lay)/speccomb_mn2o
if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
specmult_mn2o = 8._rb*specparm_mn2o
jmn2o = 1 + int(specmult_mn2o)
fmn2o = mod(specmult_mn2o,1.0_rb)
fmn2omf = minorfrac(lay)*fmn2o
chi_n2o = coln2o(lay)/coldry(lay)
ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
if (ratn2o .gt. 1.5_rb) then
adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcoln2o = coln2o(lay)
endif
speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(3) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(3) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng3
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o *  (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o *  (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +   fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs2+ig) = tau_major + tau_major1  + tauself + taufor  + adjcoln2o*absn2o
fracs(lay,ngs2+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 4._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 4._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
speccomb_mn2o = colh2o(lay) + refrat_m_b*colco2(lay)
specparm_mn2o = colh2o(lay)/speccomb_mn2o
if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
specmult_mn2o = 4._rb*specparm_mn2o
jmn2o = 1 + int(specmult_mn2o)
fmn2o = mod(specmult_mn2o,1.0_rb)
fmn2omf = minorfrac(lay)*fmn2o
chi_n2o = coln2o(lay)/coldry(lay)
ratn2o = 1.e20*chi_n2o/chi_mls(4,jp(lay)+1)
if (ratn2o .gt. 1.5_rb) then
adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcoln2o = coln2o(lay)
endif
speccomb_planck = colh2o(lay)+refrat_planck_b*colco2(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 4._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(3) + js
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(3) + js1
indf = indfor(lay)
indm = indminor(lay)
do ig = 1, ng3
taufor = forfac(lay) * (forref(indf,ig) +  forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig)))
n2om1 = kb_mn2o(jmn2o,indm,ig) + fmn2o *  (kb_mn2o(jmn2o+1,indm,ig)-kb_mn2o(jmn2o,indm,ig))
n2om2 = kb_mn2o(jmn2o,indm+1,ig) + fmn2o *  (kb_mn2o(jmn2o+1,indm+1,ig)-kb_mn2o(jmn2o,indm+1,ig))
absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
taug(lay,ngs2+ig) = speccomb *  (fac000 * absb(ind0,ig) +  fac100 * absb(ind0+1,ig) +  fac010 * absb(ind0+5,ig) +  fac110 * absb(ind0+6,ig))  + speccomb1 *  (fac001 * absb(ind1,ig) +   fac101 * absb(ind1+1,ig) +  fac011 * absb(ind1+5,ig) +  fac111 * absb(ind1+6,ig))   + taufor  + adjcoln2o*absn2o
fracs(lay,ngs2+ig) = fracrefb(ig,jpl) + fpl *  (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
enddo
enddo
end subroutine taugb3
recursive subroutine taugb4
use parrrtm, only : ng4, ngs3
use rrlw_ref, only : chi_mls
use rrlw_kg04, only : fracrefa, fracrefb, absa, ka, absb, kb,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
integer(kind=im) :: js, js1, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor
real(kind=rb) :: refrat_planck_a, refrat_planck_b
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,11)/chi_mls(2,11)
refrat_planck_b = chi_mls(3,13)/chi_mls(2,13)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(4) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(4) + js1
inds = indself(lay)
indf = indfor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng4
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +   fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs3+ig) = tau_major + tau_major1  + tauself + taufor
fracs(lay,ngs3+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
speccomb = colo3(lay) + rat_o3co2(lay)*colco2(lay)
specparm = colo3(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 4._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colo3(lay) + rat_o3co2_1(lay)*colco2(lay)
specparm1 = colo3(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 4._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
specparm_planck = colo3(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 4._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(4) + js
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(4) + js1
do ig = 1, ng4
taug(lay,ngs3+ig) =  speccomb *  (fac000 * absb(ind0,ig) +  fac100 * absb(ind0+1,ig) +  fac010 * absb(ind0+5,ig) +  fac110 * absb(ind0+6,ig))  + speccomb1 *  (fac001 * absb(ind1,ig) +   fac101 * absb(ind1+1,ig) +  fac011 * absb(ind1+5,ig) +  fac111 * absb(ind1+6,ig))
fracs(lay,ngs3+ig) = fracrefb(ig,jpl) + fpl *  (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
enddo
taug(lay,ngs3+8)=taug(lay,ngs3+8)*0.92
taug(lay,ngs3+9)=taug(lay,ngs3+9)*0.88
taug(lay,ngs3+10)=taug(lay,ngs3+10)*1.07
taug(lay,ngs3+11)=taug(lay,ngs3+11)*1.1
taug(lay,ngs3+12)=taug(lay,ngs3+12)*0.99
taug(lay,ngs3+13)=taug(lay,ngs3+13)*0.88
taug(lay,ngs3+14)=taug(lay,ngs3+14)*0.943
enddo
end subroutine taugb4
recursive subroutine taugb5
use parrrtm, only : ng5, ngs4
use rrlw_ref, only : chi_mls
use rrlw_kg05, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mo3, selfref, forref, ccl4
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmo3, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mo3, specparm_mo3, specmult_mo3, fmo3
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor, o3m1, o3m2, abso3
real(kind=rb) :: refrat_planck_a, refrat_planck_b, refrat_m_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,5)/chi_mls(2,5)
refrat_planck_b = chi_mls(3,43)/chi_mls(2,43)
refrat_m_a = chi_mls(1,7)/chi_mls(2,7)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mo3 = colh2o(lay) + refrat_m_a*colco2(lay)
specparm_mo3 = colh2o(lay)/speccomb_mo3
if (specparm_mo3 .ge. oneminus) specparm_mo3 = oneminus
specmult_mo3 = 8._rb*specparm_mo3
jmo3 = 1 + int(specmult_mo3)
fmo3 = mod(specmult_mo3,1.0_rb)
speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(5) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(5) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng5
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
o3m1 = ka_mo3(jmo3,indm,ig) + fmo3 *  (ka_mo3(jmo3+1,indm,ig)-ka_mo3(jmo3,indm,ig))
o3m2 = ka_mo3(jmo3,indm+1,ig) + fmo3 *  (ka_mo3(jmo3+1,indm+1,ig)-ka_mo3(jmo3,indm+1,ig))
abso3 = o3m1 + minorfrac(lay)*(o3m2-o3m1)
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs4+ig) = tau_major + tau_major1  + tauself + taufor  + abso3*colo3(lay)  + wx(1,lay) * ccl4(ig)
fracs(lay,ngs4+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
speccomb = colo3(lay) + rat_o3co2(lay)*colco2(lay)
specparm = colo3(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 4._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colo3(lay) + rat_o3co2_1(lay)*colco2(lay)
specparm1 = colo3(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 4._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
specparm_planck = colo3(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 4._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(5) + js
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(5) + js1
do ig = 1, ng5
taug(lay,ngs4+ig) = speccomb *  (fac000 * absb(ind0,ig) +  fac100 * absb(ind0+1,ig) +  fac010 * absb(ind0+5,ig) +  fac110 * absb(ind0+6,ig))  + speccomb1 *  (fac001 * absb(ind1,ig) +  fac101 * absb(ind1+1,ig) +  fac011 * absb(ind1+5,ig) +  fac111 * absb(ind1+6,ig))   + wx(1,lay) * ccl4(ig)
fracs(lay,ngs4+ig) = fracrefb(ig,jpl) + fpl *  (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
enddo
enddo
end subroutine taugb5
recursive subroutine taugb6
use parrrtm, only : ngs5
use rrlw_ref, only : chi_mls
use rrlw_kg06
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
real(kind=rb) :: tauself, taufor, absco2
do lay = 1, laytrop
chi_co2 = colco2(lay)/(coldry(lay))
ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
if (ratco2 .gt. 3.0_rb) then
adjfac = 2.0_rb+(ratco2-2.0_rb)**0.77_rb
adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(6) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(6) + 1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
do ig = 1, ng6
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
absco2 =  (ka_mco2(indm,ig) + minorfrac(lay) *  (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
taug(lay,ngs5+ig) = colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +   fac11(lay) * absa(ind1+1,ig))   + tauself + taufor  + adjcolco2 * absco2  + wx(2,lay) * cfc11adj(ig)  + wx(3,lay) * cfc12(ig)
fracs(lay,ngs5+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
do ig = 1, ng6
taug(lay,ngs5+ig) = 0.0_rb  + wx(2,lay) * cfc11adj(ig)  + wx(3,lay) * cfc12(ig)
fracs(lay,ngs5+ig) = fracrefa(ig)
enddo
enddo
end subroutine taugb6
recursive subroutine taugb7
use parrrtm, only : ng7, ngs6
use rrlw_ref, only : chi_mls
use rrlw_kg07, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mco2, kb_mco2, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmco2, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor, co2m1, co2m2, absco2
real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
real(kind=rb) :: refrat_planck_a, refrat_m_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,3)/chi_mls(3,3)
refrat_m_a = chi_mls(1,3)/chi_mls(3,3)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2oo3(lay)*colo3(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oo3_1(lay)*colo3(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mco2 = colh2o(lay) + refrat_m_a*colo3(lay)
specparm_mco2 = colh2o(lay)/speccomb_mco2
if (specparm_mco2 .ge. oneminus) specparm_mco2 = oneminus
specmult_mco2 = 8._rb*specparm_mco2
jmco2 = 1 + int(specmult_mco2)
fmco2 = mod(specmult_mco2,1.0_rb)
chi_co2 = colco2(lay)/(coldry(lay))
ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
if (ratco2 .gt. 3.0_rb) then
adjfac = 3.0_rb+(ratco2-3.0_rb)**0.79_rb
adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
speccomb_planck = colh2o(lay)+refrat_planck_a*colo3(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(7) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(7) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng7
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 *  (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 *  (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +   fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs6+ig) = tau_major + tau_major1  + tauself + taufor  + adjcolco2*absco2
fracs(lay,ngs6+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
chi_co2 = colco2(lay)/(coldry(lay))
ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
if (ratco2 .gt. 3.0_rb) then
adjfac = 2.0_rb+(ratco2-2.0_rb)**0.79_rb
adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(7) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(7) + 1
indm = indminor(lay)
do ig = 1, ng7
absco2 = kb_mco2(indm,ig) + minorfrac(lay) *  (kb_mco2(indm+1,ig) - kb_mco2(indm,ig))
taug(lay,ngs6+ig) = colo3(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))  + adjcolco2 * absco2
fracs(lay,ngs6+ig) = fracrefb(ig)
enddo
taug(lay,ngs6+6)=taug(lay,ngs6+6)*0.92_rb
taug(lay,ngs6+7)=taug(lay,ngs6+7)*0.88_rb
taug(lay,ngs6+8)=taug(lay,ngs6+8)*1.07_rb
taug(lay,ngs6+9)=taug(lay,ngs6+9)*1.1_rb
taug(lay,ngs6+10)=taug(lay,ngs6+10)*0.99_rb
taug(lay,ngs6+11)=taug(lay,ngs6+11)*0.855_rb
enddo
end subroutine taugb7
recursive subroutine taugb8
use parrrtm, only : ng8, ngs7
use rrlw_ref, only : chi_mls
use rrlw_kg08, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mco2, ka_mn2o, ka_mo3, kb_mco2, kb_mn2o,  selfref, forref, cfc12, cfc22adj
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
real(kind=rb) :: tauself, taufor, absco2, abso3, absn2o
real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
do lay = 1, laytrop
chi_co2 = colco2(lay)/(coldry(lay))
ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
if (ratco2 .gt. 3.0_rb) then
adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(8) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(8) + 1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
do ig = 1, ng8
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
absco2 =  (ka_mco2(indm,ig) + minorfrac(lay) *  (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
abso3 =  (ka_mo3(indm,ig) + minorfrac(lay) *  (ka_mo3(indm+1,ig) - ka_mo3(indm,ig)))
absn2o =  (ka_mn2o(indm,ig) + minorfrac(lay) *  (ka_mn2o(indm+1,ig) - ka_mn2o(indm,ig)))
taug(lay,ngs7+ig) = colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +   fac11(lay) * absa(ind1+1,ig))  + tauself + taufor  + adjcolco2*absco2  + colo3(lay) * abso3  + coln2o(lay) * absn2o  + wx(3,lay) * cfc12(ig)  + wx(4,lay) * cfc22adj(ig)
fracs(lay,ngs7+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
chi_co2 = colco2(lay)/coldry(lay)
ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
if (ratco2 .gt. 3.0_rb) then
adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
adjcolco2 = adjfac*chi_mls(2,jp(lay)+1) * coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(8) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(8) + 1
indm = indminor(lay)
do ig = 1, ng8
absco2 =  (kb_mco2(indm,ig) + minorfrac(lay) *  (kb_mco2(indm+1,ig) - kb_mco2(indm,ig)))
absn2o =  (kb_mn2o(indm,ig) + minorfrac(lay) *  (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig)))
taug(lay,ngs7+ig) = colo3(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))  + adjcolco2*absco2  + coln2o(lay)*absn2o  + wx(3,lay) * cfc12(ig)  + wx(4,lay) * cfc22adj(ig)
fracs(lay,ngs7+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb8
recursive subroutine taugb9
use parrrtm, only : ng9, ngs8
use rrlw_ref, only : chi_mls
use rrlw_kg09, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mn2o, kb_mn2o, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmn2o, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, fmn2o
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor, n2om1, n2om2, absn2o
real(kind=rb) :: chi_n2o, ratn2o, adjfac, adjcoln2o
real(kind=rb) :: refrat_planck_a, refrat_m_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,9)/chi_mls(6,9)
refrat_m_a = chi_mls(1,3)/chi_mls(6,3)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mn2o = colh2o(lay) + refrat_m_a*colch4(lay)
specparm_mn2o = colh2o(lay)/speccomb_mn2o
if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
specmult_mn2o = 8._rb*specparm_mn2o
jmn2o = 1 + int(specmult_mn2o)
fmn2o = mod(specmult_mn2o,1.0_rb)
chi_n2o = coln2o(lay)/(coldry(lay))
ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
if (ratn2o .gt. 1.5_rb) then
adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcoln2o = coln2o(lay)
endif
speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(9) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(9) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng9
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o *  (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o *  (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs8+ig) = tau_major + tau_major1  + tauself + taufor  + adjcoln2o*absn2o
fracs(lay,ngs8+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
chi_n2o = coln2o(lay)/(coldry(lay))
ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
if (ratn2o .gt. 1.5_rb) then
adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
else
adjcoln2o = coln2o(lay)
endif
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(9) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(9) + 1
indm = indminor(lay)
do ig = 1, ng9
absn2o = kb_mn2o(indm,ig) + minorfrac(lay) *  (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig))
taug(lay,ngs8+ig) = colch4(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +   fac11(lay) * absb(ind1+1,ig))  + adjcoln2o*absn2o
fracs(lay,ngs8+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb9
recursive subroutine taugb10
use parrrtm, only : ng10, ngs9
use rrlw_kg10, only : fracrefa, fracrefb, absa, ka, absb, kb,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
real(kind=rb) :: tauself, taufor
do lay = 1, laytrop
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(10) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(10) + 1
inds = indself(lay)
indf = indfor(lay)
do ig = 1, ng10
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
taug(lay,ngs9+ig) = colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +  fac11(lay) * absa(ind1+1,ig))   + tauself + taufor
fracs(lay,ngs9+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(10) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(10) + 1
indf = indfor(lay)
do ig = 1, ng10
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
taug(lay,ngs9+ig) = colh2o(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +   fac11(lay) * absb(ind1+1,ig))  + taufor
fracs(lay,ngs9+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb10
recursive subroutine taugb11
use parrrtm, only : ng11, ngs10
use rrlw_kg11, only : fracrefa, fracrefb, absa, ka, absb, kb,  ka_mo2, kb_mo2, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
real(kind=rb) :: scaleo2, tauself, taufor, tauo2
do lay = 1, laytrop
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(11) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(11) + 1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
scaleo2 = colo2(lay)*scaleminor(lay)
do ig = 1, ng11
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
tauo2 =  scaleo2 * (ka_mo2(indm,ig) + minorfrac(lay) *  (ka_mo2(indm+1,ig) - ka_mo2(indm,ig)))
taug(lay,ngs10+ig) = colh2o(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +  fac11(lay) * absa(ind1+1,ig))  + tauself + taufor  + tauo2
fracs(lay,ngs10+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(11) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(11) + 1
indf = indfor(lay)
indm = indminor(lay)
scaleo2 = colo2(lay)*scaleminor(lay)
do ig = 1, ng11
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
tauo2 =  scaleo2 * (kb_mo2(indm,ig) + minorfrac(lay) *  (kb_mo2(indm+1,ig) - kb_mo2(indm,ig)))
taug(lay,ngs10+ig) = colh2o(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))   + taufor  + tauo2
fracs(lay,ngs10+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb11
recursive subroutine taugb12
use parrrtm, only : ng12, ngs11
use rrlw_ref, only : chi_mls
use rrlw_kg12, only : fracrefa, absa, ka,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
integer(kind=im) :: js, js1, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor
real(kind=rb) :: refrat_planck_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,10)/chi_mls(2,10)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(12) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(12) + js1
inds = indself(lay)
indf = indfor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng12
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs11+ig) = tau_major + tau_major1  + tauself + taufor
fracs(lay,ngs11+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
do ig = 1, ng12
taug(lay,ngs11+ig) = 0.0_rb
fracs(lay,ngs11+ig) = 0.0_rb
enddo
enddo
end subroutine taugb12
recursive subroutine taugb13
use parrrtm, only : ng13, ngs12
use rrlw_ref, only : chi_mls
use rrlw_kg13, only : fracrefa, fracrefb, absa, ka,  ka_mco2, ka_mco, kb_mo3, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmco2, jmco, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
real(kind=rb) :: speccomb_mco, specparm_mco, specmult_mco, fmco
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor, co2m1, co2m2, absco2
real(kind=rb) :: com1, com2, absco, abso3
real(kind=rb) :: chi_co2, ratco2, adjfac, adjcolco2
real(kind=rb) :: refrat_planck_a, refrat_m_a, refrat_m_a3
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,5)/chi_mls(4,5)
refrat_m_a = chi_mls(1,1)/chi_mls(4,1)
refrat_m_a3 = chi_mls(1,3)/chi_mls(4,3)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2on2o(lay)*coln2o(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2on2o_1(lay)*coln2o(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mco2 = colh2o(lay) + refrat_m_a*coln2o(lay)
specparm_mco2 = colh2o(lay)/speccomb_mco2
if (specparm_mco2 .ge. oneminus) specparm_mco2 = oneminus
specmult_mco2 = 8._rb*specparm_mco2
jmco2 = 1 + int(specmult_mco2)
fmco2 = mod(specmult_mco2,1.0_rb)
chi_co2 = colco2(lay)/(coldry(lay))
ratco2 = 1.e20_rb*chi_co2/3.55e-4_rb
if (ratco2 .gt. 3.0_rb) then
adjfac = 2.0_rb+(ratco2-2.0_rb)**0.68_rb
adjcolco2 = adjfac*3.55e-4*coldry(lay)*1.e-20_rb
else
adjcolco2 = colco2(lay)
endif
speccomb_mco = colh2o(lay) + refrat_m_a3*coln2o(lay)
specparm_mco = colh2o(lay)/speccomb_mco
if (specparm_mco .ge. oneminus) specparm_mco = oneminus
specmult_mco = 8._rb*specparm_mco
jmco = 1 + int(specmult_mco)
fmco = mod(specmult_mco,1.0_rb)
speccomb_planck = colh2o(lay)+refrat_planck_a*coln2o(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(13) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(13) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng13
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 *  (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 *  (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)
com1 = ka_mco(jmco,indm,ig) + fmco *  (ka_mco(jmco+1,indm,ig) - ka_mco(jmco,indm,ig))
com2 = ka_mco(jmco,indm+1,ig) + fmco *  (ka_mco(jmco+1,indm+1,ig) - ka_mco(jmco,indm+1,ig))
absco = com1 + minorfrac(lay) * (com2 - com1)
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs12+ig) = tau_major + tau_major1  + tauself + taufor  + adjcolco2*absco2  + colco(lay)*absco
fracs(lay,ngs12+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
indm = indminor(lay)
do ig = 1, ng13
abso3 = kb_mo3(indm,ig) + minorfrac(lay) *  (kb_mo3(indm+1,ig) - kb_mo3(indm,ig))
taug(lay,ngs12+ig) = colo3(lay)*abso3
fracs(lay,ngs12+ig) =  fracrefb(ig)
enddo
enddo
end subroutine taugb13
recursive subroutine taugb14
use parrrtm, only : ng14, ngs13
use rrlw_kg14, only : fracrefa, fracrefb, absa, ka, absb, kb,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
real(kind=rb) :: tauself, taufor
do lay = 1, laytrop
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(14) + 1
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(14) + 1
inds = indself(lay)
indf = indfor(lay)
do ig = 1, ng14
tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
taug(lay,ngs13+ig) = colco2(lay) *  (fac00(lay) * absa(ind0,ig) +  fac10(lay) * absa(ind0+1,ig) +  fac01(lay) * absa(ind1,ig) +  fac11(lay) * absa(ind1+1,ig))  + tauself + taufor
fracs(lay,ngs13+ig) = fracrefa(ig)
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(14) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(14) + 1
do ig = 1, ng14
taug(lay,ngs13+ig) = colco2(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))
fracs(lay,ngs13+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb14
recursive subroutine taugb15
use parrrtm, only : ng15, ngs14
use rrlw_ref, only : chi_mls
use rrlw_kg15, only : fracrefa, absa, ka,  ka_mn2, selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
integer(kind=im) :: js, js1, jmn2, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_mn2, specparm_mn2, specmult_mn2, fmn2
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: scalen2, tauself, taufor, n2m1, n2m2, taun2
real(kind=rb) :: refrat_planck_a, refrat_m_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(4,1)/chi_mls(2,1)
refrat_m_a = chi_mls(4,1)/chi_mls(2,1)
do lay = 1, laytrop
speccomb = coln2o(lay) + rat_n2oco2(lay)*colco2(lay)
specparm = coln2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = coln2o(lay) + rat_n2oco2_1(lay)*colco2(lay)
specparm1 = coln2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_mn2 = coln2o(lay) + refrat_m_a*colco2(lay)
specparm_mn2 = coln2o(lay)/speccomb_mn2
if (specparm_mn2 .ge. oneminus) specparm_mn2 = oneminus
specmult_mn2 = 8._rb*specparm_mn2
jmn2 = 1 + int(specmult_mn2)
fmn2 = mod(specmult_mn2,1.0_rb)
speccomb_planck = coln2o(lay)+refrat_planck_a*colco2(lay)
specparm_planck = coln2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(15) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(15) + js1
inds = indself(lay)
indf = indfor(lay)
indm = indminor(lay)
scalen2 = colbrd(lay)*scaleminor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng15
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
n2m1 = ka_mn2(jmn2,indm,ig) + fmn2 *  (ka_mn2(jmn2+1,indm,ig) - ka_mn2(jmn2,indm,ig))
n2m2 = ka_mn2(jmn2,indm+1,ig) + fmn2 *  (ka_mn2(jmn2+1,indm+1,ig) - ka_mn2(jmn2,indm+1,ig))
taun2 = scalen2 * (n2m1 + minorfrac(lay) * (n2m2 - n2m1))
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs14+ig) = tau_major + tau_major1  + tauself + taufor  + taun2
fracs(lay,ngs14+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
do ig = 1, ng15
taug(lay,ngs14+ig) = 0.0_rb
fracs(lay,ngs14+ig) = 0.0_rb
enddo
enddo
end subroutine taugb15
recursive subroutine taugb16
use parrrtm, only : ng16, ngs15
use rrlw_ref, only : chi_mls
use rrlw_kg16, only : fracrefa, fracrefb, absa, ka, absb, kb,  selfref, forref
integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
integer(kind=im) :: js, js1, jpl
real(kind=rb) :: speccomb, specparm, specmult, fs
real(kind=rb) :: speccomb1, specparm1, specmult1, fs1
real(kind=rb) :: speccomb_planck, specparm_planck, specmult_planck, fpl
real(kind=rb) :: p, p4, fk0, fk1, fk2
real(kind=rb) :: fac000, fac100, fac200, fac010, fac110, fac210
real(kind=rb) :: fac001, fac101, fac201, fac011, fac111, fac211
real(kind=rb) :: tauself, taufor
real(kind=rb) :: refrat_planck_a
real(kind=rb) :: tau_major, tau_major1
refrat_planck_a = chi_mls(1,6)/chi_mls(6,6)
do lay = 1, laytrop
speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
specparm = colh2o(lay)/speccomb
if (specparm .ge. oneminus) specparm = oneminus
specmult = 8._rb*(specparm)
js = 1 + int(specmult)
fs = mod(specmult,1.0_rb)
speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
specparm1 = colh2o(lay)/speccomb1
if (specparm1 .ge. oneminus) specparm1 = oneminus
specmult1 = 8._rb*(specparm1)
js1 = 1 + int(specmult1)
fs1 = mod(specmult1,1.0_rb)
speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
specparm_planck = colh2o(lay)/speccomb_planck
if (specparm_planck .ge. oneminus) specparm_planck=oneminus
specmult_planck = 8._rb*specparm_planck
jpl= 1 + int(specmult_planck)
fpl = mod(specmult_planck,1.0_rb)
ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(16) + js
ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(16) + js1
inds = indself(lay)
indf = indfor(lay)
if (specparm .lt. 0.125_rb) then
p = fs - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else if (specparm .gt. 0.875_rb) then
p = -fs
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac000 = fk0*fac00(lay)
fac100 = fk1*fac00(lay)
fac200 = fk2*fac00(lay)
fac010 = fk0*fac10(lay)
fac110 = fk1*fac10(lay)
fac210 = fk2*fac10(lay)
else
fac000 = (1._rb - fs) * fac00(lay)
fac010 = (1._rb - fs) * fac10(lay)
fac100 = fs * fac00(lay)
fac110 = fs * fac10(lay)
endif
if (specparm1 .lt. 0.125_rb) then
p = fs1 - 1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else if (specparm1 .gt. 0.875_rb) then
p = -fs1
p4 = p**4
fk0 = p4
fk1 = 1 - p - 2.0_rb*p4
fk2 = p + p4
fac001 = fk0*fac01(lay)
fac101 = fk1*fac01(lay)
fac201 = fk2*fac01(lay)
fac011 = fk0*fac11(lay)
fac111 = fk1*fac11(lay)
fac211 = fk2*fac11(lay)
else
fac001 = (1._rb - fs1) * fac01(lay)
fac011 = (1._rb - fs1) * fac11(lay)
fac101 = fs1 * fac01(lay)
fac111 = fs1 * fac11(lay)
endif
do ig = 1, ng16
tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *  (selfref(inds+1,ig) - selfref(inds,ig)))
taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *  (forref(indf+1,ig) - forref(indf,ig)))
if (specparm .lt. 0.125_rb) then
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac200 * absa(ind0+2,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig) +  fac210 * absa(ind0+11,ig))
else if (specparm .gt. 0.875_rb) then
tau_major = speccomb *  (fac200 * absa(ind0-1,ig) +  fac100 * absa(ind0,ig) +  fac000 * absa(ind0+1,ig) +  fac210 * absa(ind0+8,ig) +  fac110 * absa(ind0+9,ig) +  fac010 * absa(ind0+10,ig))
else
tau_major = speccomb *  (fac000 * absa(ind0,ig) +  fac100 * absa(ind0+1,ig) +  fac010 * absa(ind0+9,ig) +  fac110 * absa(ind0+10,ig))
endif
if (specparm1 .lt. 0.125_rb) then
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac201 * absa(ind1+2,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig) +  fac211 * absa(ind1+11,ig))
else if (specparm1 .gt. 0.875_rb) then
tau_major1 = speccomb1 *  (fac201 * absa(ind1-1,ig) +  fac101 * absa(ind1,ig) +  fac001 * absa(ind1+1,ig) +  fac211 * absa(ind1+8,ig) +  fac111 * absa(ind1+9,ig) +  fac011 * absa(ind1+10,ig))
else
tau_major1 = speccomb1 *  (fac001 * absa(ind1,ig) +  fac101 * absa(ind1+1,ig) +  fac011 * absa(ind1+9,ig) +  fac111 * absa(ind1+10,ig))
endif
taug(lay,ngs15+ig) = tau_major + tau_major1  + tauself + taufor
fracs(lay,ngs15+ig) = fracrefa(ig,jpl) + fpl *  (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
enddo
enddo
do lay = laytrop+1, nlayers
ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(16) + 1
ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(16) + 1
do ig = 1, ng16
taug(lay,ngs15+ig) = colch4(lay) *  (fac00(lay) * absb(ind0,ig) +  fac10(lay) * absb(ind0+1,ig) +  fac01(lay) * absb(ind1,ig) +  fac11(lay) * absb(ind1+1,ig))
fracs(lay,ngs15+ig) = fracrefb(ig)
enddo
enddo
end subroutine taugb16
end subroutine taumol
end module lean_rrtmg_lw_taumol

module lean_rrtmg_lw_rad
use radiation_sizes, only: max_lw_layers
use parkind, only : im => kind_im, rb => kind_rb
use rrlw_vsn
use lean_mcica_subcol_gen_lw, only: mcica_subcol_lw
use lean_rrtmg_lw_cldprmc, only: cldprmc
use lean_rrtmg_lw_rtrnmc, only: rtrnmc
use lean_rrtmg_lw_setcoef, only: setcoef
use lean_rrtmg_lw_taumol, only: taumol
implicit none
public :: rrtmg_lw, inatm
contains
recursive subroutine rrtmg_lw  (ncol    ,nlay    ,icld    ,  play    ,plev    ,tlay    ,tlev    ,tsfc    ,  h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr ,  cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    ,  inflglw ,iceflglw,liqflglw,cldfmcl ,  taucmcl ,ciwpmcl ,clwpmcl , cswpmcl ,reicmcl ,relqmcl , resnmcl ,  tauaer  ,  uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc,  uflxcln ,dflxcln, calc_clean_atm_diag , ws)
 use radiation_scratch, only: lw_scratch
use parrrtm, only : nbndlw, ngptlw, maxxsec, mxmol
use rrlw_con, only: fluxfac, heatfac, oneminus, pi
use rrlw_wvn, only: ng, ngb, nspa, nspb, wavenum1, wavenum2, delwave
 type(lw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
integer(kind=im), intent(in) :: ncol
integer(kind=im), intent(in) :: nlay
integer(kind=im), intent(inout) :: icld
real(kind=rb), intent(in) :: play(:,:)
real(kind=rb), intent(in) :: plev(:,:)
real(kind=rb), intent(in) :: tlay(:,:)
real(kind=rb), intent(in) :: tlev(:,:)
real(kind=rb), intent(in) :: tsfc(:)
real(kind=rb), intent(in) :: h2ovmr(:,:)
real(kind=rb), intent(in) :: o3vmr(:,:)
real(kind=rb), intent(in) :: co2vmr(:,:)
real(kind=rb), intent(in) :: ch4vmr(:,:)
real(kind=rb), intent(in) :: n2ovmr(:,:)
real(kind=rb), intent(in) :: o2vmr(:,:)
real(kind=rb), intent(in) :: cfc11vmr(:,:)
real(kind=rb), intent(in) :: cfc12vmr(:,:)
real(kind=rb), intent(in) :: cfc22vmr(:,:)
real(kind=rb), intent(in) :: ccl4vmr(:,:)
real(kind=rb), intent(in) :: emis(:,:)
integer(kind=im), intent(in) :: inflglw
integer(kind=im), intent(in) :: iceflglw
integer(kind=im), intent(in) :: liqflglw
real(kind=rb), intent(in) :: cldfmcl(:,:,:)
real(kind=rb), intent(in) :: ciwpmcl(:,:,:)
real(kind=rb), intent(in) :: clwpmcl(:,:,:)
real(kind=rb), intent(in) :: cswpmcl(:,:,:)
real(kind=rb), intent(in) :: reicmcl(:,:)
real(kind=rb), intent(in) :: relqmcl(:,:)
real(kind=rb), intent(in) :: resnmcl(:,:)
real(kind=rb), intent(in) :: taucmcl(:,:,:)
real(kind=rb), intent(in) :: tauaer(:,:,:)
integer, intent(in) :: calc_clean_atm_diag
real(kind=rb), intent(out) :: uflx(:,:)
real(kind=rb), intent(out) :: dflx(:,:)
real(kind=rb), intent(out) :: hr(:,:)
real(kind=rb), intent(out) :: uflxc(:,:)
real(kind=rb), intent(out) :: dflxc(:,:)
real(kind=rb), intent(out) :: hrc(:,:)
real(kind=rb), intent(out) :: uflxcln(:,:)
real(kind=rb), intent(out) :: dflxcln(:,:)
integer(kind=im) :: nlayers
integer(kind=im) :: istart
integer(kind=im) :: iend
integer(kind=im) :: iout
integer(kind=im) :: iaer
integer(kind=im) :: iplon
integer(kind=im) :: imca
integer(kind=im) :: ims
integer(kind=im) :: k
integer(kind=im) :: ig
real(kind=rb) :: pavel(max_lw_layers+1)
real(kind=rb) :: tavel(max_lw_layers+1)
real(kind=rb) :: pz(0:max_lw_layers+1)
real(kind=rb) :: tz(0:max_lw_layers+1)
real(kind=rb) :: tbound
real(kind=rb) :: coldry(max_lw_layers+1)
real(kind=rb) :: wbrodl(max_lw_layers+1)
real(kind=rb) :: wx(maxxsec,max_lw_layers+1)
real(kind=rb) :: pwvcm
real(kind=rb) :: semiss(nbndlw)
integer(kind=im) :: laytrop
integer(kind=im) :: jp(max_lw_layers+1)
integer(kind=im) :: jt(max_lw_layers+1)
integer(kind=im) :: jt1(max_lw_layers+1)
real(kind=rb) :: plankbnd(nbndlw)
real(kind=rb) :: colh2o(max_lw_layers+1)
real(kind=rb) :: colco2(max_lw_layers+1)
real(kind=rb) :: colo3(max_lw_layers+1)
real(kind=rb) :: coln2o(max_lw_layers+1)
real(kind=rb) :: colco(max_lw_layers+1)
real(kind=rb) :: colch4(max_lw_layers+1)
real(kind=rb) :: colo2(max_lw_layers+1)
real(kind=rb) :: colbrd(max_lw_layers+1)
integer(kind=im) :: indself(max_lw_layers+1)
integer(kind=im) :: indfor(max_lw_layers+1)
real(kind=rb) :: selffac(max_lw_layers+1)
real(kind=rb) :: selffrac(max_lw_layers+1)
real(kind=rb) :: forfac(max_lw_layers+1)
real(kind=rb) :: forfrac(max_lw_layers+1)
integer(kind=im) :: indminor(max_lw_layers+1)
real(kind=rb) :: minorfrac(max_lw_layers+1)
real(kind=rb) :: scaleminor(max_lw_layers+1)
real(kind=rb) :: scaleminorn2(max_lw_layers+1)
real(kind=rb) ::  fac00(max_lw_layers+1), fac01(max_lw_layers+1),  fac10(max_lw_layers+1), fac11(max_lw_layers+1)
real(kind=rb) ::  rat_h2oco2(max_lw_layers+1),rat_h2oco2_1(max_lw_layers+1),  rat_h2oo3(max_lw_layers+1),rat_h2oo3_1(max_lw_layers+1),  rat_h2on2o(max_lw_layers+1),rat_h2on2o_1(max_lw_layers+1),  rat_h2och4(max_lw_layers+1),rat_h2och4_1(max_lw_layers+1),  rat_n2oco2(max_lw_layers+1),rat_n2oco2_1(max_lw_layers+1),  rat_o3co2(max_lw_layers+1),rat_o3co2_1(max_lw_layers+1)
integer(kind=im) :: ncbands
integer(kind=im) :: inflag
integer(kind=im) :: iceflag
integer(kind=im) :: liqflag
real(kind=rb) :: relqmc(max_lw_layers+1)
real(kind=rb) :: reicmc(max_lw_layers+1)
real(kind=rb) :: resnmc(max_lw_layers+1)
real(kind=rb) :: totuflux(0:max_lw_layers+1)
real(kind=rb) :: totdflux(0:max_lw_layers+1)
real(kind=rb) :: fnet(0:max_lw_layers+1)
real(kind=rb) :: htr(0:max_lw_layers+1)
real(kind=rb) :: totuclfl(0:max_lw_layers+1)
real(kind=rb) :: totdclfl(0:max_lw_layers+1)
real(kind=rb) :: fnetc(0:max_lw_layers+1)
real(kind=rb) :: htrc(0:max_lw_layers+1)
real(kind=rb) :: totuclnlfl(0:max_lw_layers+1)
real(kind=rb) :: totdclnlfl(0:max_lw_layers+1)
real(kind=rb) :: fnetcln(0:max_lw_layers+1)
real(kind=rb) :: htrcln(0:max_lw_layers+1)
istart = 1
iend = 16
iout = 0
ims = 1
iaer = 10
do iplon = 1, ncol
call inatm (iplon, nlay, icld, iaer,  play, plev, tlay, tlev, tsfc, h2ovmr,  o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr,  cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw,  cldfmcl, taucmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, tauaer,  nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry,  ws%wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag,  ws%cldfmc, ws%taucmc, ws%ciwpmc, ws%clwpmc, ws%cswpmc, reicmc, relqmc, resnmc, ws%taua)
call cldprmc(nlayers, inflag, iceflag, liqflag, ws%cldfmc, ws%ciwpmc,  ws%clwpmc, ws%cswpmc, reicmc, relqmc, resnmc, ncbands, ws%taucmc)
call setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss,  coldry, ws%wkl, wbrodl,  laytrop, jp, jt, jt1, ws%planklay, ws%planklev, plankbnd,  colh2o, colco2, colo3, coln2o, colco, colch4, colo2,  colbrd, fac00, fac01, fac10, fac11,  rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,  rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,  rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,  selffac, selffrac, indself, forfac, forfrac, indfor,  minorfrac, scaleminor, scaleminorn2, indminor)
call taumol(nlayers, pavel, wx, coldry,  laytrop, jp, jt, jt1, ws%planklay, ws%planklev, plankbnd,  colh2o, colco2, colo3, coln2o, colco, colch4, colo2,  colbrd, fac00, fac01, fac10, fac11,  rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,  rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,  rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,  selffac, selffrac, indself, forfac, forfrac, indfor,  minorfrac, scaleminor, scaleminorn2, indminor,  ws%fracs, ws%taug)
do k = 1, nlayers
do ig = 1, ngptlw
ws%taut(k,ig) = ws%taug(k,ig) + ws%taua(k,ngb(ig))
enddo
enddo
do k = 0, nlayers
totuclnlfl(k) = 0.0
totdclnlfl(k) = 0.0
end do
call rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands,  ws%cldfmc, ws%taucmc, ws%planklay, ws%planklev, plankbnd,  pwvcm, ws%fracs, ws%taut,  totuflux, totdflux, fnet, htr,  totuclfl, totdclfl, fnetc, htrc , ws=ws)
do k = 0, nlayers
uflx(iplon,k+1) = totuflux(k)
dflx(iplon,k+1) = totdflux(k)
uflxc(iplon,k+1) = totuclfl(k)
dflxc(iplon,k+1) = totdclfl(k)
uflxcln(iplon,k+1) = totuclnlfl(k)
dflxcln(iplon,k+1) = totdclnlfl(k)
enddo
do k = 0, nlayers-1
hr(iplon,k+1) = htr(k)
hrc(iplon,k+1) = htrc(k)
enddo
enddo
end subroutine rrtmg_lw
recursive subroutine inatm (iplon, nlay, icld, iaer,  play, plev, tlay, tlev, tsfc, h2ovmr,  o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr,  cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw,  cldfmcl, taucmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, tauaer,  nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry,  wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag,  cldfmc, taucmc, ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, taua)
use parrrtm, only : nbndlw, ngptlw, nmol, maxxsec, mxmol
use rrlw_con, only: fluxfac, heatfac, oneminus, pi, grav, avogad
use rrlw_wvn, only: ng, nspa, nspb, wavenum1, wavenum2, delwave, ixindx
integer(kind=im), intent(in) :: iplon
integer(kind=im), intent(in) :: nlay
integer(kind=im), intent(in) :: icld
integer(kind=im), intent(in) :: iaer
real(kind=rb), intent(in) :: play(:,:)
real(kind=rb), intent(in) :: plev(:,:)
real(kind=rb), intent(in) :: tlay(:,:)
real(kind=rb), intent(in) :: tlev(:,:)
real(kind=rb), intent(in) :: tsfc(:)
real(kind=rb), intent(in) :: h2ovmr(:,:)
real(kind=rb), intent(in) :: o3vmr(:,:)
real(kind=rb), intent(in) :: co2vmr(:,:)
real(kind=rb), intent(in) :: ch4vmr(:,:)
real(kind=rb), intent(in) :: n2ovmr(:,:)
real(kind=rb), intent(in) :: o2vmr(:,:)
real(kind=rb), intent(in) :: cfc11vmr(:,:)
real(kind=rb), intent(in) :: cfc12vmr(:,:)
real(kind=rb), intent(in) :: cfc22vmr(:,:)
real(kind=rb), intent(in) :: ccl4vmr(:,:)
real(kind=rb), intent(in) :: emis(:,:)
integer(kind=im), intent(in) :: inflglw
integer(kind=im), intent(in) :: iceflglw
integer(kind=im), intent(in) :: liqflglw
real(kind=rb), intent(in) :: cldfmcl(:,:,:)
real(kind=rb), intent(in) :: ciwpmcl(:,:,:)
real(kind=rb), intent(in) :: clwpmcl(:,:,:)
real(kind=rb), intent(in) :: cswpmcl(:,:,:)
real(kind=rb), intent(in) :: relqmcl(:,:)
real(kind=rb), intent(in) :: reicmcl(:,:)
real(kind=rb), intent(in) :: resnmcl(:,:)
real(kind=rb), intent(in) :: taucmcl(:,:,:)
real(kind=rb), intent(in) :: tauaer(:,:,:)
integer(kind=im), intent(out) :: nlayers
real(kind=rb), intent(out) :: pavel(:)
real(kind=rb), intent(out) :: tavel(:)
real(kind=rb), intent(out) :: pz(0:)
real(kind=rb), intent(out) :: tz(0:)
real(kind=rb), intent(out) :: tbound
real(kind=rb), intent(out) :: coldry(:)
real(kind=rb), intent(out) :: wbrodl(:)
real(kind=rb), intent(out) :: wkl(:,:)
real(kind=rb), intent(out) :: wx(:,:)
real(kind=rb), intent(out) :: pwvcm
real(kind=rb), intent(out) :: semiss(:)
integer(kind=im), intent(out) :: inflag
integer(kind=im), intent(out) :: iceflag
integer(kind=im), intent(out) :: liqflag
real(kind=rb), intent(out) :: cldfmc(:,:)
real(kind=rb), intent(out) :: ciwpmc(:,:)
real(kind=rb), intent(out) :: clwpmc(:,:)
real(kind=rb), intent(out) :: cswpmc(:,:)
real(kind=rb), intent(out) :: relqmc(:)
real(kind=rb), intent(out) :: reicmc(:)
real(kind=rb), intent(out) :: resnmc(:)
real(kind=rb), intent(out) :: taucmc(:,:)
real(kind=rb), intent(out) :: taua(:,:)
real(kind=rb), parameter :: amd = 28.9660_rb
real(kind=rb), parameter :: amw = 18.0160_rb
real(kind=rb), parameter :: amdw = 1.607793_rb
real(kind=rb), parameter :: amdc = 0.658114_rb
real(kind=rb), parameter :: amdo = 0.603428_rb
real(kind=rb), parameter :: amdm = 1.805423_rb
real(kind=rb), parameter :: amdn = 0.658090_rb
real(kind=rb), parameter :: amdo2 = 0.905140_rb
real(kind=rb), parameter :: amdc1 = 0.210852_rb
real(kind=rb), parameter :: amdc2 = 0.239546_rb
integer(kind=im) :: isp, l, ix, n, imol, ib, ig
real(kind=rb) :: amm, amttl, wvttl, wvsh, summol
nlayers = nlay
wkl(:,:) = 0.0_rb
wx(:,:) = 0.0_rb
cldfmc(:,:) = 0.0_rb
taucmc(:,:) = 0.0_rb
ciwpmc(:,:) = 0.0_rb
clwpmc(:,:) = 0.0_rb
cswpmc(:,:) = 0.0_rb
reicmc(:) = 0.0_rb
relqmc(:) = 0.0_rb
resnmc(:) = 0.0_rb
taua(:,:) = 0.0_rb
amttl = 0.0_rb
wvttl = 0.0_rb
tbound = tsfc(iplon)
pz(0) = plev(iplon,1)
tz(0) = tlev(iplon,1)
do l = 1, nlayers
pavel(l) = play(iplon,l)
tavel(l) = tlay(iplon,l)
pz(l) = plev(iplon,l+1)
tz(l) = tlev(iplon,l+1)
wkl(1,l) = h2ovmr(iplon,l)
wkl(2,l) = co2vmr(iplon,l)
wkl(3,l) = o3vmr(iplon,l)
wkl(4,l) = n2ovmr(iplon,l)
wkl(6,l) = ch4vmr(iplon,l)
wkl(7,l) = o2vmr(iplon,l)
amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw
coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad /  (1.e2_rb * grav * amm * (1._rb + wkl(1,l)))
enddo
do l=1, nlayers
wx(1,l) = ccl4vmr(iplon,l)
wx(2,l) = cfc11vmr(iplon,l)
wx(3,l) = cfc12vmr(iplon,l)
wx(4,l) = cfc22vmr(iplon,l)
enddo
do l = 1, nlayers
summol = 0.0_rb
do imol = 2, nmol
summol = summol + wkl(imol,l)
enddo
wbrodl(l) = coldry(l) * (1._rb - summol)
do imol = 1, nmol
wkl(imol,l) = coldry(l) * wkl(imol,l)
enddo
amttl = amttl + coldry(l)+wkl(1,l)
wvttl = wvttl + wkl(1,l)
do ix = 1,maxxsec
if (ixindx(ix) .ne. 0) then
wx(ixindx(ix),l) = coldry(l) * wx(ix,l) * 1.e-20_rb
endif
enddo
enddo
wvsh = (amw * wvttl) / (amd * amttl)
pwvcm = wvsh * (1.e3_rb * pz(0)) / (1.e2_rb * grav)
do n=1,nbndlw
semiss(n) = emis(iplon,n)
enddo
do l = 1, nlayers
do ib = 1, nbndlw
taua(l,ib) = tauaer(iplon,l,ib)
enddo
enddo
inflag = inflglw
iceflag = iceflglw
liqflag = liqflglw
do l = 1, nlayers
do ig = 1, ngptlw
cldfmc(ig,l) = cldfmcl(ig,iplon,l)
taucmc(ig,l) = taucmcl(ig,iplon,l)
ciwpmc(ig,l) = ciwpmcl(ig,iplon,l)
clwpmc(ig,l) = clwpmcl(ig,iplon,l)
cswpmc(ig,l) = cswpmcl(ig,iplon,l)
enddo
reicmc(l) = reicmcl(iplon,l)
relqmc(l) = relqmcl(iplon,l)
resnmc(l) = resnmcl(iplon,l)
enddo
end subroutine inatm
end module lean_rrtmg_lw_rad
