! HAND-WRITTEN device code (not generated; tools/gen_device_radiation.py neither writes nor checks this file). Card build only.
! RRTMG on the card with the g-point as a parallel dimension.
! The radiative-transfer code is RRTMG's, rearranged. RRTMG's notice (NOTICE.md), with
! the years as its modules state them: 2006-2008 in mcica_subcol_gen_lw and _sw, 2002-2009 in
! rrtmg_lw_cldprmc and rrtmg_lw_taumol, 2002-2008 in the others:
! Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).
! Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).
! Copyright 2006-2008, Atmospheric & Environmental Research, Inc. (AER).
! This software may be used, copied, or redistributed as long as it is
! not sold and this copyright notice is reproduced on each copy made.
! This model is provided as is without any express or implied warranties.
! (http://www.rtweb.aer.com/)
!
! Run as one LW + SW column per thread (the generated device code), RRTMG needs a 1.5 MB scratch per column and, 2048 columns
! at a time, keeps only a few percent of the card busy. Here the same code is split into stages, each a kernel over a group of
! columns (index j) and, where the work is, over g-points or bands:
!   LW  prep (j): radiation_lw_column up to McICA, inatm, setcoef, McICA's random stream (the one truly serial part)
!       cloud (j,g): McICA overlap of g-point g -> its cloudy layers (a bit mask) and the cloudy-layer flags of rtrnmc
!       taumol (j,band): taugb1..16 of the device kernels
!       transfer (j,g): cldprmc + rtrnmc of one g-point; its up/down fluxes per level are stored
!       sums (j,level): the stored fluxes added band by band, g-point by g-point, in rtrnmc's order
!       finish (j): fluxfac, heating rates, the outputs of radiation_lw_column / radiation_event_ws
!   SW  prep (j), taumol (j,band), transfer (j,g: McICA overlap + cldprmc_sw + spcvmc_sw of one g-point), sums, finish.
! The expressions are those of the generated device code (src/device/radiation_lw.f90, radiation_sw.f90, radiation_column.f90,
! radiation_lw_kernels.f90, radiation_sw_kernels.f90), in the same order; the sums over g-points keep the original order, so the
! results are those of the generated column code. The k-distribution routines (taugbN, taumolNN), setcoef,
! setcoef_sw, reftra_sw and vrtqdr_sw are called as they are.
! Group arrays carry the slot index first, so the threads of a warp (consecutive j) touch consecutive words.
module radiation_fast
 use lean_phys_fault, only: physics_fault
 use config, only: nz, nz1, kte=>nz, max_levels
 use run_settings, only: radt_sw
 use radiation_sizes, only: max_lw_layers, max_sw_layers
 use mpas_atmphys_constants, only: gravity, degrad
 use parkind, only: im => kind_im, rb => kind_rb
 use parrrtm, only: nbndlw, ngptlw, maxxsec, nmol_lw => nmol
 use parrrsw, only: nbndsw, ngptsw, nmol_sw => nmol, rrsw_scon
 use radiation_prepare, only: radiation_interfaces
 use radiation_cloud, only: radiation_cloud_fraction
 use radiation_climatology, only: ozone_at_lat_time
 use radiation_ozone, only: ozone_to_layers
 use lean_dev_retab, only: retab_sw => retab
 use lean_rrtmg_lw_rtrnmc, only: wtdiff, rec_6, a0, a1, a2
 use radiation_fast_data
 use radiation_fast_gas, only: rf_setcoef, rf_setcoef_sw
 implicit none
 private
 public :: rf_allocate, rf_bytes, rf_lw_prep, rf_lw_cloud, rf_lw_transfer, rf_lw_sums, rf_lw_finish, &
  rf_sw_zenith, rf_sw_prep, rf_sw_transfer, rf_sw_sums, rf_sw_finish
 real, parameter :: deltap = 4.
 ! retab of src/device/radiation_lw.f90 (the LW column's own copy; the SW column reads lean_dev_retab)
 real, parameter :: retab_lw(95)=[                                                 5.92779, 6.26422, 6.61973, 6.99539, 7.39234,    7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,   10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,   15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,   20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,   27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,   31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,   34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,   38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,   42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,   50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,   65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,   93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,   124.954, 130.630, 136.457, 142.446, 148.608, 154.956,   161.503, 168.262, 175.248, 182.473, 189.952, 197.699,   205.728, 214.055, 222.694, 231.661, 240.971, 250.639]
#ifdef LEAN_RAD_2025
 real, parameter :: co2 = 425.62e-6, ch4 = 1936.03e-9, n2o = 338.86e-9
#else
 real, parameter :: co2 = 379.e-6, ch4 = 1774.e-9, n2o = 319.e-9
#endif
 real, parameter :: cfc11 = 0.251e-9, cfc12 = 0.538e-9, cfc22 = 0.169e-9, ccl4 = 0.093e-9, o2 = 0.209488
 real, parameter :: amdw = 1.607793
contains

 ! radiation_event_ws before the SW and LW columns (src/device/radiation_column.f90)
 subroutine prelude(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs,julian,lat_radians,temperature,interface_t,hydro_layer,hydro_interface,water,cloud,ozone)
 !$acc routine seq
 real,intent(in) :: theta(nz),exner(nz),pressure(nz),rho(nz),dz(nz),z(nz+1),qv(nz),qc(nz),qi(nz),qs(nz),julian,lat_radians
 real,intent(out) :: temperature(max_levels),interface_t(max_levels+1),hydro_layer(max_levels),hydro_interface(max_levels+1)
 real,intent(out) :: water(max_levels,4),cloud(max_levels),ozone(59)
 integer :: k
 do k=1,nz
  temperature(k)=theta(k)*exner(k)
  water(k,1)=max(0.,qv(k));water(k,2)=max(0.,qc(k));water(k,3)=max(0.,qi(k))
  water(k,4)=max(0.,qs(k))
 enddo
 call radiation_interfaces(temperature(1:nz),pressure,rho,dz,z,interface_t(1:nz+1),hydro_layer(1:nz),hydro_interface(1:nz+1))
 call radiation_cloud_fraction(cloud(1:nz),temperature(1:nz),pressure,water(1:nz,1),water(1:nz,2),water(1:nz,3),water(1:nz,4))
 call ozone_at_lat_time(lat_radians,julian,ozone)   ! monthly_ozone + ozone_at_time for this column
 end subroutine

 ! generate_stochastic_clouds(_sw): the seeds from the digits of pmid, changeSeed draws thrown away, then one number per
 ! (subcolumn, layer), subcolumn outermost. Stored as cdf(j, layer, subcolumn).
 subroutine mcica_stream(nlay, nsubcol, changeseed, play, cdf, j)
 !$acc routine seq
 integer, intent(in) :: nlay, nsubcol, changeseed, j
 real(rb), intent(in) :: play(:)
 real, intent(inout) :: cdf(:,:,:)
 integer(im) :: seed1, seed2, seed3, seed4, kiss, i, isubcol, ilev
 real(rb) :: pmid1, pmid2, pmid3, pmid4, rand_num
 pmid1 = play(1)*1.e2_rb; pmid2 = play(2)*1.e2_rb; pmid3 = play(3)*1.e2_rb; pmid4 = play(4)*1.e2_rb
 if (pmid1.lt.pmid2) call physics_fault(23)
 seed1 = (pmid1 - int(pmid1))  * 1000000000_im
 seed2 = (pmid2 - int(pmid2))  * 1000000000_im
 seed3 = (pmid3 - int(pmid3))  * 1000000000_im
 seed4 = (pmid4 - int(pmid4))  * 1000000000_im
 do i = 1, changeseed
   seed1 = 69069_im * seed1 + 1327217885_im
   seed2 = ieor(seed2, ishft(seed2, 13_im))
   seed2 = ieor(seed2, ishft(seed2, - 17_im))
   seed2 = ieor(seed2, ishft(seed2, 5_im))
   seed3 = 18000_im * iand (seed3, 65535_im) + ishft (seed3, - 16_im)
   seed4 = 30903_im * iand (seed4, 65535_im) + ishft (seed4, - 16_im)
   kiss = seed1 + seed2 + ishft (seed3, 16_im) + seed4
   rand_num = kiss*2.328306e-10_rb + 0.5_rb
 enddo
 do isubcol = 1, nsubcol
  do ilev = 1, nlay
    seed1 = 69069_im * seed1 + 1327217885_im
    seed2 = ieor(seed2, ishft(seed2, 13_im))
    seed2 = ieor(seed2, ishft(seed2, - 17_im))
    seed2 = ieor(seed2, ishft(seed2, 5_im))
    seed3 = 18000_im * iand (seed3, 65535_im) + ishft (seed3, - 16_im)
    seed4 = 30903_im * iand (seed4, 65535_im) + ishft (seed4, - 16_im)
    kiss = seed1 + seed2 + ishft (seed3, 16_im) + seed4
    rand_num = kiss*2.328306e-10_rb + 0.5_rb
   cdf(j,ilev,isubcol) = rand_num
  enddo
 enddo
 end subroutine

 ! ================================================================== LW ==================================================================
 ! radiation_lw_column up to mcica_subcol_lw, then inatm and setcoef of rrtmg_lw (src/device/radiation_lw.f90, radiation_lw_kernels.f90)
 subroutine rf_lw_prep(j,theta,exner,pressure,rho,dz,z,qv,qc,qi,qs,re_cloud,re_ice,re_snow,skin,emiss,xland,latitude,julian,pin)
 use lean_dev_rrlw_con, only: grav_lw => grav, avogad_lw => avogad
 use lean_dev_rrlw_wvn, only: ixindx
 !$acc routine seq
 integer,intent(in) :: j
 real,intent(in) :: theta(nz),exner(nz),pressure(nz),rho(nz),dz(nz),z(nz+1),qv(nz),qc(nz),qi(nz),qs(nz)
 real,intent(in) :: re_cloud(nz),re_ice(nz),re_snow(nz),skin,emiss,xland,latitude,julian,pin(59)
 real :: temperature(max_levels),interface_t(max_levels+1),hydro_layer(max_levels),hydro_interface(max_levels+1)
 real :: water(max_levels,4),cloud(max_levels),ozone(59)
 integer,parameter :: kts=1,nproflevs=60
 real,parameter :: amd=28.9660_rb,amw=18.0160_rb
 integer :: nlayers,k,kk,klev,idx_rei,l,imol,ix,n,laytrop,istart
 real :: pw1d(kts:max_levels+1),p1d(kts:max_levels),qv1d(kts:max_levels),qc1d(kts:max_levels),qi1d(kts:max_levels),qs1d(kts:max_levels)
 real :: plev(kts:max_lw_layers+1),tlev(kts:max_lw_layers+1),play(kts:max_lw_layers),tlay(kts:max_lw_layers),pdel(kts:max_levels)
 real :: h2ovmr(kts:max_lw_layers),o3vmr(kts:max_lw_layers)
 real :: cldfrac(kts:max_lw_layers),reliq(kts:max_levels),reice(kts:max_levels),resnow(kts:max_levels)
 real :: cliqwp(kts:max_levels),cicewp(kts:max_levels),csnowp(kts:max_levels),o3mmr(kts:max_lw_layers),varint(kts:max_lw_layers+1)
 real :: play1(kts:max_lw_layers),tsfc,corr,gliqwp,gicewp,gsnowp,gravmks,snow_mass_factor,wght,vark,vark1
 real(rb), pointer, contiguous :: pavel(:),tavel(:),coldry(:),wbrodl(:),wkl(:,:)   ! slot j of rf_dv, rf_wk
 real(rb) :: pz(0:max_lw_layers+1),tz(0:max_lw_layers+1)
 real(rb) :: tbound,wx(maxxsec)   ! wx: the cross sections of one layer (they go straight to lw_wx)
 real(rb) :: semiss(nbndlw),amm,amttl,wvttl,wvsh,summol,pwvcm
 real :: pprof(nproflevs),tprof(nproflevs)
 data pprof /1000.00,855.47,731.82,626.05,535.57,458.16,  391.94,335.29,286.83,245.38,209.91,179.57,  153.62,131.41,112.42, 96.17, 82.27, 70.38,  60.21, 51.51, 44.06, 37.69, 32.25, 27.59,  23.60, 20.19, 17.27, 14.77, 12.64, 10.81,  9.25,  7.91, 6.77,   5.79,  4.95,  4.24,  3.63,  3.10, 2.65,   2.27,  1.94,  1.66,  1.42,  1.22, 1.04,   0.89,  0.76,  0.65,  0.56,  0.48, 0.41,   0.35,  0.30,  0.26,  0.22,  0.19, 0.16,   0.14,  0.12,  0.10/
 data tprof /286.96,281.07,275.16,268.11,260.56,253.02,   245.62,238.41,231.57,225.91,221.72,217.79,   215.06,212.74,210.25,210.16,210.69,212.14,   213.74,215.37,216.82,217.94,219.03,220.18,   221.37,222.64,224.16,225.88,227.63,229.51,   231.50,233.73,236.18,238.78,241.60,244.44,   247.35,250.33,253.32,256.30,259.22,262.12,   264.80,266.50,267.59,268.44,268.69,267.76,   266.13,263.96,261.54,258.93,256.15,253.23,   249.89,246.67,243.48,240.25,236.66,233.86/
 pavel => rf_dv(1:max_lw_layers+1,1,j); tavel => rf_dv(1:max_lw_layers+1,2,j)
 coldry => rf_dv(1:max_lw_layers+1,3,j); wbrodl => rf_dv(1:max_lw_layers+1,4,j)
 wkl => rf_wk(:,1:max_lw_layers+1,j)
 call prelude(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs,julian,latitude,temperature,interface_t,hydro_layer,hydro_interface,water,cloud,ozone)
 ! radiation_lw_column: p3d=hydro_layer, p8w=hydro_interface, t3d=temperature, t8w=interface_t, dz8w=dz, q*3d=water, cldfra3d=cloud
 do k = kts, kte+1
  pw1d(k) = hydro_interface(k) / 100.
 enddo
 do k = kts, kte
  p1d(k)  = hydro_layer(k) / 100.
  qv1d(k) = amax1(water(k,1),1.e-12)
 enddo
 do k = kts,kte
  qc1d(k) = amax1(water(k,2),0.)
  qi1d(k) = amax1(water(k,3),0.)
  qs1d(k) = amax1(water(k,4),0.)
 enddo
 nlayers = kte + max(nint(pw1d(kte+1)/deltap), 1)
 if(nlayers>max_lw_layers) call physics_fault(22)
 do k = 1, nlayers
  cldfrac(k) = 0.
 enddo
 plev(1) = pw1d(1)
 tlev(1) = interface_t(1)
 tsfc    = skin
 do k = kts, kte
  plev(k+1)   = pw1d(k+1)
  tlev(k+1)   = interface_t(k+1)
  play(k)     = p1d(k)
  pdel(k)     = plev(k) - plev(k+1)
  tlay(k)     = temperature(k)
  h2ovmr(k)   = qv1d(k) * amdw
 enddo
 do k=kte+1,nlayers,1
  plev(k+1) = max(plev(k) - deltap, 0.00)
  play(k) = 0.5*(plev(k) + plev(k+1))
 enddo
 plev(nlayers+1) = 0.00
 play(nlayers) =  0.5*(plev(nlayers) + plev(nlayers+1))
 do k = 1, nlayers+1, 1
  if(pprof(nproflevs) .lt. plev(k)) then
   do kk = 2, nproflevs, 1
    if(pprof(kk) .lt. plev(k)) then
     klev = kk - 1
     exit
    endif
   enddo
  else
   klev = nproflevs
  endif
  if(klev .ne. nproflevs) then
   vark  = tprof(klev)
   vark1 = tprof(klev+1)
   wght  = (plev(k)-pprof(klev) )/( pprof(klev+1)-pprof(klev))
  else
   vark  = tprof(klev)
   vark1 = tprof(klev)
   wght  = 0.0
  endif
  varint(k) = wght*(vark1-vark)+vark
 enddo
 do k = kte+1, nlayers+1, 1
  tlev(k) = varint(k) + (tlev(kte) - varint(kte))
  tlay(k-1) = 0.5*(tlev(k) + tlev(k-1))
 enddo
 do k = kte+1, nlayers, 1
  h2ovmr(k)   = h2ovmr(kte)
 enddo
 play1(kts:nlayers) = play(kts:nlayers)
 call ozone_to_layers(nlayers,play1,pin,ozone,o3mmr)
 do k = kts,nlayers
  o3vmr(k) = o3mmr(k)
 enddo
 do k = kts, kte
  cldfrac(k) = cloud(k)
 enddo
 gravmks = gravity
 do k = kts, kte
  reliq(k) = max(2.5,re_cloud(k)*1.e6)
  if(reliq(k).le.2.5 .and. cldfrac(k).gt.0. .and.  (xland-1.5).gt.0.) then
   reliq(k) = 10.5
  elseif(reliq(k).le.2.5 .and. cldfrac(k).gt.0. .and.  (xland-1.5).lt.0.) then
   reliq(k) = 7.5
  endif
 enddo
 do k = kts, kte
  reice(k) = max(5.,re_ice(k)*1.e6)
  if(reice(k).le.5. .and. cldfrac(k).gt.0.) then
   idx_rei = int(temperature(k)-179.)
   idx_rei = min(max(idx_rei,1),75)
   corr = temperature(k) - int(temperature(k))
   reice(k) = retab_lw(idx_rei)*(1.-corr) + retab_lw(idx_rei+1)*corr
   reice(k) = max(reice(k),5.0)
  endif
 enddo
 do k = kts, kte
  resnow(k) = max(10.,re_snow(k)*1.e6)
 enddo
 do k = kts, kte
  gliqwp = qc1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  cliqwp(k) = gliqwp / max(0.01,cldfrac(k))
 enddo
 do k = kts, kte
  gicewp = qi1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  cicewp(k) = gicewp / max(0.01,cldfrac(k))
 enddo
 do k = kts, kte
  snow_mass_factor = 1.0
  if(resnow(k) .gt. 130.)then
   snow_mass_factor = (130.0/resnow(k))*(130.0/resnow(k))
   resnow(k)   = 130.0
  endif
  gsnowp = qs1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  csnowp(k) = snow_mass_factor * gsnowp / max(0.01,cldfrac(k))
 enddo
 ! McICA inputs (cldfrac, ciwp, clwp, cswp, rei, rel, res of mcica_subcol_lw)
 do k = kts, kte
  lw_cl(j,k,1) = cldfrac(k)
  lw_cl(j,k,2) = cicewp(k)
  lw_cl(j,k,3) = cliqwp(k)
  lw_cl(j,k,4) = csnowp(k)
  lw_cl(j,k,5) = reice(k)
  lw_cl(j,k,6) = reliq(k)
  lw_cl(j,k,7) = resnow(k)
 enddo
 do k = kte+1, nlayers
  lw_cl(j,k,1) = 0.
  lw_cl(j,k,2) = 0.
  lw_cl(j,k,3) = 0.
  lw_cl(j,k,4) = 0.
  lw_cl(j,k,5) = 10.
  lw_cl(j,k,6) = 10.
  lw_cl(j,k,7) = 10.
 enddo
 ! McICA's random stream (permuteseed 150)
 call mcica_stream(nlayers, ngptlw, 150, play, lw_cdf, j)
 ! inatm (iplon 1, tauaer 0)
 do l = 1, max_lw_layers+1
  do imol = 1, nmol_lw
   wkl(imol,l) = 0.0_rb
  enddo
 enddo
 amttl = 0.0_rb
 wvttl = 0.0_rb
 tbound = tsfc
 pz(0) = plev(1)
 tz(0) = tlev(1)
 do l = 1, nlayers
  pavel(l) = play(l)
  tavel(l) = tlay(l)
  pz(l) = plev(l+1)
  tz(l) = tlev(l+1)
  wkl(1,l) = h2ovmr(l)
  wkl(2,l) = co2   ! the gas has this value at every level
  wkl(3,l) = o3vmr(l)
  wkl(4,l) = n2o   ! the gas has this value at every level
  wkl(6,l) = ch4   ! the gas has this value at every level
  wkl(7,l) = o2   ! the gas has this value at every level
  amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw
  coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad_lw /  (1.e2_rb * grav_lw * amm * (1._rb + wkl(1,l)))
 enddo
 do l = 1, nlayers
  summol = 0.0_rb
  do imol = 2, nmol_lw
   summol = summol + wkl(imol,l)
  enddo
  wbrodl(l) = coldry(l) * (1._rb - summol)
  do imol = 1, nmol_lw
   wkl(imol,l) = coldry(l) * wkl(imol,l)
  enddo
  amttl = amttl + coldry(l)+wkl(1,l)
  wvttl = wvttl + wkl(1,l)
  do ix = 1, maxxsec
   wx(ix) = 0.0_rb
  enddo
  wx(1) = ccl4   ! the gas has this value at every level
  wx(2) = cfc11   ! the gas has this value at every level
  wx(3) = cfc12   ! the gas has this value at every level
  wx(4) = cfc22   ! the gas has this value at every level
  do ix = 1,maxxsec
   if (ixindx(ix) .ne. 0) then
    wx(ixindx(ix)) = coldry(l) * wx(ix) * 1.e-20_rb
   endif
  enddo
  do ix = 1, maxxsec
   lw_wx(j,ix,l) = wx(ix)
  enddo
 enddo
 wvsh = (amw * wvttl) / (amd * amttl)
 pwvcm = wvsh * (1.e3_rb * pz(0)) / (1.e2_rb * grav_lw)
 do n=1,nbndlw
  semiss(n) = emiss
 enddo
 ! setcoef straight into the group arrays
 istart = 1
 call rf_setcoef(j, nlayers, istart, pavel, tavel, tz, tbound, semiss, coldry, wkl, wbrodl)
 lw_nlay(j) = nlayers
 lw_pwvcm(j) = pwvcm
 do l = 1, nlayers
  lw_lay(j,l,1) = pavel(l)
  lw_lay(j,l,3) = coldry(l)
  lw_icld(j,l) = 0
 enddo
 do l = 0, nlayers
  lw_pz(j,l) = pz(l)
 enddo
 do n = 1, nbndlw
  lw_semiss(j,n) = semiss(n)
 enddo
 end subroutine

 ! generate_stochastic_clouds, maximum-random overlap (icld 2), for subcolumn ig: its cloudy layers as bits of a mask, and
 ! rtrnmc's icldlyr (a layer is cloudy if any g-point is: every writer stores 1)
 subroutine rf_lw_cloud(j, ig)
 !$acc routine seq
 integer, intent(in) :: j, ig
 real(rb), parameter :: cldmin = 1.0e-20_rb
 integer :: nlay, ilev
 integer(8) :: mask
 real(rb) :: cur, cldf_prev, cldf_cur
 nlay = lw_nlay(j)
 mask = 0_8
 cldf_cur = lw_cl(j,1,1)
 if (cldf_cur < cldmin) cldf_cur = 0._rb
 cur = lw_cdf(j,1,ig)
 if (cur >= 1._rb - cldf_cur) then
  mask = ibset(mask, 0)
  lw_icld(j,1) = 1
 endif
 do ilev = 2, nlay
  cldf_prev = cldf_cur
  cldf_cur = lw_cl(j,ilev,1)
  if (cldf_cur < cldmin) cldf_cur = 0._rb
  if (.not. (cur > 1._rb - cldf_prev)) cur = lw_cdf(j,ilev,ig) * (1._rb - cldf_prev)
  if (cur >= 1._rb - cldf_cur) then
   mask = ibset(mask, ilev-1)
   lw_icld(j,ilev) = 1
  endif
 enddo
 lw_mask(j,ig) = mask
 end subroutine

 ! cldprmc and rtrnmc for g-point igc: its sweeps down and up; the per-level contributions that rtrnmc adds into drad, urad,
 ! clrdrad, clrurad are stored (lw_gout 2, 1, 4, 3) and added in rf_lw_sums
 subroutine rf_lw_transfer(j, igc)
 use lean_dev_rrlw_wvn, only: ngb
 use lean_dev_rrlw_tbl, only: tblint, bpade, tau_tbl, exp_tbl, tfn_tbl
 use lean_dev_rrlw_cld, only: absliq1, absice3
 !$acc routine seq
 integer, intent(in) :: j, igc
 real(rb), parameter :: cldmin = 1.e-20_rb
 real(rb) :: atrans(max_lw_layers), atot(max_lw_layers), bbugas(max_lw_layers), bbutot(max_lw_layers)
 real(rb) :: odcld(max_lw_layers), efclfrac(max_lw_layers), cldfmc(max_lw_layers)
 integer :: icldlyr(max_lw_layers)
 real(rb) :: secdiff, transcld, abscld, radld, radclrd, plfrac, blay, dplankup, dplankdn
 real(rb) :: odepth, odtot, odepth_rec, odtot_rec, gassrc, tblind, tfactot, bbd, bbdtot, tfacgas, transc, tausfac
 real(rb) :: rad0, reflect, radlu, radclru, taucmc, cwp, radice, radsno, radliq, factor, fint, abscoice, abscosno, abscoliq
 real(rb) :: ciwp, clwp, cswp
 integer :: nlayers, iband, lay, lev, iclddn, ittot, itgas, itr, index
 integer(8) :: mask
 nlayers = lw_nlay(j)
 iband = ngb(igc)
 mask = lw_mask(j,igc)
 if (iband.eq.1 .or. iband.eq.4 .or. iband.ge.10) then
  secdiff = 1.66_rb
 else
  secdiff = a0(iband) + a1(iband)*exp(a2(iband)*lw_pwvcm(j))
  if (secdiff .gt. 1.80_rb) secdiff = 1.80_rb
  if (secdiff .lt. 1.50_rb) secdiff = 1.50_rb
 endif
 do lay = 1, nlayers
  icldlyr(lay) = lw_icld(j,lay)
  taucmc = 0.0_rb
  if (btest(mask, lay-1)) then
   cldfmc(lay) = 1._rb
   ! cldprmc (inflag 2, iceflag 3, liqflag 1 of the device code), the subcolumn's cloud: the layer's water paths
   ciwp = lw_cl(j,lay,2); clwp = lw_cl(j,lay,3); cswp = lw_cl(j,lay,4)
   cwp = ciwp + clwp + cswp
   if (cldfmc(lay) .ge. cldmin .and.  (cwp .ge. cldmin .or. taucmc .ge. cldmin)) then
    radice = lw_cl(j,lay,5)
    if ((ciwp+cswp) .eq. 0.0_rb) then
     abscoice = 0.0_rb
     abscosno = 0.0_rb
    else
     if (radice .lt. 5.0_rb .or. radice .gt. 140.0_rb) then
      call physics_fault(11)
     endif
     factor = (radice - 2._rb)/3._rb
     index = int(factor)
     if (index .eq. 46) index = 45
     fint = factor - float(index)
     abscoice =  absice3(index,iband) + fint *  (absice3(index+1,iband) - (absice3(index,iband)))
     abscosno = 0.0_rb
    endif
    if (cswp.gt.0.0_rb) then
     radsno = lw_cl(j,lay,7)
     if (radsno .lt. 5.0_rb .or. radsno .gt. 140.0_rb) then
      call physics_fault(12)
     endif
     factor = (radsno - 2._rb)/3._rb
     index = int(factor)
     if (index .eq. 46) index = 45
     fint = factor - float(index)
     abscosno =  absice3(index,iband) + fint *  (absice3(index+1,iband) - (absice3(index,iband)))
    endif
    if (clwp .eq. 0.0_rb) then
     abscoliq = 0.0_rb
    else
     radliq = lw_cl(j,lay,6)
     if (radliq .lt. 2.5_rb .or. radliq .gt. 60._rb) call physics_fault(24)
     index = int(radliq - 1.5_rb)
     if (index .eq. 0) index = 1
     if (index .eq. 58) index = 57
     fint = radliq - 1.5_rb - float(index)
     abscoliq =  absliq1(index,iband) + fint *  (absliq1(index+1,iband) - (absliq1(index,iband)))
    endif
    taucmc = ciwp * abscoice +  clwp * abscoliq +  cswp * abscosno
   endif
  else
   cldfmc(lay) = 0._rb
  endif
  if (cldfmc(lay) .eq. 1._rb) then
   odcld(lay) = secdiff * taucmc
   transcld = exp(-odcld(lay))
   abscld = 1._rb - transcld
   efclfrac(lay) = abscld * cldfmc(lay)
  else
   odcld(lay) = 0.0_rb
   efclfrac(lay) = 0.0_rb
  endif
 enddo
 radld = 0._rb
 radclrd = 0._rb
 iclddn = 0
 do lev = nlayers, 1, -1
  plfrac = lw_fracs(j,lev,igc)
  blay = lw_planklay(j,lev,iband)
  dplankup = lw_planklev(j,lev,iband) - blay
  dplankdn = lw_planklev(j,lev-1,iband) - blay
  odepth = secdiff * (lw_taug(j,lev,igc) + aer(1))
  if (odepth .lt. 0.0_rb) odepth = 0.0_rb
  if (icldlyr(lev).eq.1) then
   iclddn = 1
   odtot = odepth + odcld(lev)
   if (odtot .lt. 0.06_rb) then
    atrans(lev) = odepth - 0.5_rb*odepth*odepth
    odepth_rec = rec_6*odepth
    gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
    atot(lev) =  odtot - 0.5_rb*odtot*odtot
    odtot_rec = rec_6*odtot
    bbdtot =  plfrac * (blay+dplankdn*odtot_rec)
    bbd = plfrac*(blay+dplankdn*odepth_rec)
    radld = radld - radld * (atrans(lev) +  efclfrac(lev) * (1. - atrans(lev))) +  gassrc + cldfmc(lev) *  (bbdtot * atot(lev) - gassrc)
    bbugas(lev) =  plfrac * (blay+dplankup*odepth_rec)
    bbutot(lev) =  plfrac * (blay+dplankup*odtot_rec)
   elseif (odepth .le. 0.06_rb) then
    atrans(lev) = odepth - 0.5_rb*odepth*odepth
    odepth_rec = rec_6*odepth
    gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
    odtot = odepth + odcld(lev)
    tblind = odtot/(bpade+odtot)
    ittot = tblint*tblind + 0.5_rb
    tfactot = tfn_tbl(ittot)
    bbdtot = plfrac * (blay + tfactot*dplankdn)
    bbd = plfrac*(blay+dplankdn*odepth_rec)
    atot(lev) = 1. - exp_tbl(ittot)
    radld = radld - radld * (atrans(lev) +  efclfrac(lev) * (1._rb - atrans(lev))) +  gassrc + cldfmc(lev) *  (bbdtot * atot(lev) - gassrc)
    bbugas(lev) = plfrac * (blay + dplankup*odepth_rec)
    bbutot(lev) = plfrac * (blay + tfactot * dplankup)
   else
    tblind = odepth/(bpade+odepth)
    itgas = tblint*tblind+0.5_rb
    odepth = tau_tbl(itgas)
    atrans(lev) = 1._rb - exp_tbl(itgas)
    tfacgas = tfn_tbl(itgas)
    gassrc = atrans(lev) * plfrac * (blay + tfacgas*dplankdn)
    odtot = odepth + odcld(lev)
    tblind = odtot/(bpade+odtot)
    ittot = tblint*tblind + 0.5_rb
    tfactot = tfn_tbl(ittot)
    bbdtot = plfrac * (blay + tfactot*dplankdn)
    bbd = plfrac*(blay+tfacgas*dplankdn)
    atot(lev) = 1._rb - exp_tbl(ittot)
    radld = radld - radld * (atrans(lev) +  efclfrac(lev) * (1._rb - atrans(lev))) +  gassrc + cldfmc(lev) *  (bbdtot * atot(lev) - gassrc)
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
  endif
  lw_gout(j,lev-1,igc,2) = radld
#ifdef LEAN_BUDGET
  if (iclddn.eq.1) then
   radclrd = radclrd + (bbd-radclrd) * atrans(lev)
  else
   radclrd = radld
  endif
  lw_gout(j,lev-1,igc,4) = radclrd
#endif
 enddo
 rad0 = lw_fracs(j,1,igc) * lw_plankbnd(j,iband)
 reflect = 1._rb - lw_semiss(j,iband)
 radlu = rad0 + reflect * radld
 lw_gout(j,0,igc,1) = radlu
#ifdef LEAN_BUDGET
 radclru = rad0 + reflect * radclrd
 lw_gout(j,0,igc,3) = radclru
#endif
 do lev = 1, nlayers
  if (icldlyr(lev) .eq. 1) then
   gassrc = bbugas(lev) * atrans(lev)
   radlu = radlu - radlu * (atrans(lev) +  efclfrac(lev) * (1._rb - atrans(lev))) +  gassrc + cldfmc(lev) *  (bbutot(lev) * atot(lev) - gassrc)
  else
   radlu = radlu + (bbugas(lev)-radlu)*atrans(lev)
  endif
  lw_gout(j,lev,igc,1) = radlu
#ifdef LEAN_BUDGET
  if (iclddn.eq.1) then
   radclru = radclru + (bbugas(lev)-radclru)*atrans(lev)
  else
   radclru = radlu
  endif
  lw_gout(j,lev,igc,3) = radclru
#endif
 enddo
 end subroutine

 ! rtrnmc's band loop at level l: urad/drad (and the clear-sky ones) added over the band's g-points in order, times wtdiff, into
 ! the totals times delwave, band by band. Clear sky as rtrnmc: where no cloud lies above (down) / in the column (up), the
 ! clear-sky sum is the all-sky sum.
 subroutine rf_lw_sums(j, l)
 use lean_dev_rrlw_wvn, only: ngs, delwave
 !$acc routine seq
 integer, intent(in) :: j, l
 integer :: nlayers, ib, ig, g0, k
 logical :: cdn, cup
 real(rb) :: u, d, cu, cd, uf, df, ucf, dcf, tu, td, tcu, tcd
 nlayers = lw_nlay(j)
 if (l > nlayers) return
 cup = .false.
 do k = 1, nlayers
  if (lw_icld(j,k) == 1) cup = .true.
 enddo
 cdn = .false.
 do k = l+1, nlayers
  if (lw_icld(j,k) == 1) cdn = .true.
 enddo
 tu = 0.0_rb; td = 0.0_rb; tcu = 0.0_rb; tcd = 0.0_rb
 g0 = 0
 do ib = 1, nbndlw
  u = 0.0_rb; d = 0.0_rb; cu = 0.0_rb; cd = 0.0_rb
  do ig = g0+1, ngs(ib)
   u = u + lw_gout(j,l,ig,1)
#ifdef LEAN_BUDGET
   if (l == 0 .or. cup) then
    cu = cu + lw_gout(j,l,ig,3)
   else
    cu = u
   endif
#endif
   if (l < nlayers) then
    d = d + lw_gout(j,l,ig,2)
#ifdef LEAN_BUDGET
    if (cdn) then
     cd = cd + lw_gout(j,l,ig,4)
    else
     cd = d
    endif
#endif
   endif
  enddo
  g0 = ngs(ib)
  uf = u*wtdiff
  df = d*wtdiff
  tu = tu + uf * delwave(ib)
  td = td + df * delwave(ib)
  ucf = cu*wtdiff
  dcf = cd*wtdiff
  tcu = tcu + ucf * delwave(ib)
  tcd = tcd + dcf * delwave(ib)
 enddo
 lw_tot(j,l,1) = tu
 lw_tot(j,l,2) = td
 lw_tot(j,l,3) = tcu
 lw_tot(j,l,4) = tcd
 end subroutine

 ! the end of rtrnmc, rrtmg_lw, radiation_lw_column and radiation_event_ws's lw(:)
 subroutine rf_lw_finish(j, pi3d, rthratenlw, lw, flux)
 use lean_dev_rrlw_con, only: fluxfac, heatfac
 !$acc routine seq
 integer, intent(in) :: j
 real, intent(in) :: pi3d(nz)
 real, intent(out) :: rthratenlw(nz), lw(11), flux(4,max_lw_layers+1)
 real(rb) :: totuflux(0:max_lw_layers+1), totdflux(0:max_lw_layers+1), fnet(0:max_lw_layers+1), htr(0:max_lw_layers+1)
 real(rb) :: totuclfl(0:max_lw_layers+1), totdclfl(0:max_lw_layers+1)
 real :: tten1d, glw, olr, lwcf
 integer :: nlayers, lev, l, k
 nlayers = lw_nlay(j)
 totuflux(0) = lw_tot(j,0,1) * fluxfac
 totdflux(0) = lw_tot(j,0,2) * fluxfac
 fnet(0) = totuflux(0) - totdflux(0)
 totuclfl(0) = lw_tot(j,0,3) * fluxfac
 totdclfl(0) = lw_tot(j,0,4) * fluxfac
 do lev = 1, nlayers
  totuflux(lev) = lw_tot(j,lev,1) * fluxfac
  totdflux(lev) = lw_tot(j,lev,2) * fluxfac
  fnet(lev) = totuflux(lev) - totdflux(lev)
  totuclfl(lev) = lw_tot(j,lev,3) * fluxfac
  totdclfl(lev) = lw_tot(j,lev,4) * fluxfac
  l = lev - 1
  htr(l)=heatfac*(fnet(l)-fnet(lev))/(lw_pz(j,l)-lw_pz(j,lev))
 enddo
 htr(nlayers) = 0.0_rb
 glw  = totdflux(0)
 olr  = totuflux(nlayers)
 lwcf = totuclfl(nlayers) - totuflux(nlayers)
 do k = 1, kte
  tten1d = htr(k-1)/86400.
  rthratenlw(k) = tten1d/pi3d(k)
 enddo
 do k = 1, nlayers+1
  flux(1,k) = totuflux(k-1)
  flux(2,k) = totdflux(k-1)
  flux(3,k) = totuclfl(k-1)
  flux(4,k) = totdclfl(k-1)
 enddo
 lw=[glw,olr,lwcf,flux(1,nlayers+1),flux(3,nlayers+1),flux(2,nlayers+1),flux(4,nlayers+1), &
  flux(1,1),flux(3,1),flux(2,1),flux(4,1)]
 end subroutine

 ! ================================================================== SW ==================================================================
 ! cos of the zenith angle as radiation_sw_column computes it (lat, lon in degrees as radiation_event_ws passes them)
 real function rf_sw_zenith(latitude, longitude, xtime, gmt, declin)
 !$acc routine seq
 real, intent(in) :: latitude, longitude, xtime, gmt, declin
 real :: lat, lon, xt24, tloctm, hrang, xxlat
 lat = latitude/degrad
 lon = longitude/degrad
 xt24 = mod(xtime+(radt_sw/60.)*0.5,1440.)
 tloctm = gmt + xt24/60. + lon/15.
 hrang = 15. * (tloctm-12.) * degrad
 xxlat = lat * degrad
 rf_sw_zenith = sin(xxlat) * sin(declin) + cos(xxlat) * cos(declin) * cos(hrang)
 end function

 ! radiation_sw_column (a day column) up to mcica_subcol_sw, then inatm_sw and setcoef_sw of rrtmg_sw
 subroutine rf_sw_prep(j,coszrs,theta,exner,pressure,rho,dz,z,qv,qc,qi,qs,re_cloud,re_ice,re_snow,skin,albedo,xland,latitude,julian,solcon,pin)
 use lean_dev_rrsw_con, only: grav_sw => grav, avogad_sw => avogad
 !$acc routine seq
 integer,intent(in) :: j
 real,intent(in) :: coszrs,theta(nz),exner(nz),pressure(nz),rho(nz),dz(nz),z(nz+1),qv(nz),qc(nz),qi(nz),qs(nz)
 real,intent(in) :: re_cloud(nz),re_ice(nz),re_snow(nz),skin,albedo,xland,latitude,julian,solcon,pin(59)
 real :: temperature(max_levels),interface_t(max_levels+1),hydro_layer(max_levels),hydro_interface(max_levels+1)
 real :: water(max_levels,4),cloud(max_levels),ozone(59)
 integer,parameter :: kts=1
 real,parameter :: amd=28.9660_rb,amw=18.0160_rb,zepzen=1.e-10_rb
 integer :: nlay,nlayers,k,idx_rei,l,imol,laytrop,layswtch,laylow
 real :: pw1d(kts:max_sw_layers),p1d(kts:max_sw_layers-1),qv1d(kts:max_sw_layers-1),qc1d(kts:max_sw_layers-1)
 real :: qi1d(kts:max_sw_layers-1),qs1d(kts:max_sw_layers-1),pdel(kts:max_sw_layers-1)
 real :: reliq(kts:max_sw_layers-1),reice(kts:max_sw_layers-1),resnow(kts:max_sw_layers-1)
 real :: cliqwp(kts:max_sw_layers-1),cicewp(kts:max_sw_layers-1),csnowp(kts:max_sw_layers-1)
 real :: play(kts:max_sw_layers),tlay(kts:max_sw_layers),h2ovmr(kts:max_sw_layers),o3vmr(kts:max_sw_layers)
 real :: cldfrac(kts:max_sw_layers)
 real :: o3mmr(kts:max_sw_layers),play1(kts:max_sw_layers),plev(kts:max_sw_layers+1),tlev(kts:max_sw_layers+1)
 real :: tsfc,corr,gliqwp,gicewp,gsnowp,gravmks,snow_mass_factor,scon,adjes,cossza
 real(rb), pointer, contiguous :: pavel(:),tavel(:),coldry(:),wkl(:,:)   ! slot j of rf_dv, rf_wk
 real(rb) :: pz(0:max_sw_layers+1),tz(0:max_sw_layers+1)
 real(rb) :: tbound,amm,adjflx,solvar
 pavel => rf_dv(1:max_sw_layers+1,1,j); tavel => rf_dv(1:max_sw_layers+1,2,j)
 coldry => rf_dv(1:max_sw_layers+1,3,j)
 wkl => rf_wk(1:nmol_sw,1:max_sw_layers+1,j)
 call prelude(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs,julian,latitude,temperature,interface_t,hydro_layer,hydro_interface,water,cloud,ozone)
 do k = kts, kte+1
  pw1d(k) = hydro_interface(k) / 100.
 enddo
 do k = kts, kte
  p1d(k)  = hydro_layer(k) / 100.
  qv1d(k) = amax1(water(k,1),1.e-12)
 enddo
 do k = kts,kte
  qc1d(k) = amax1(water(k,2),0.)
  qi1d(k) = amax1(water(k,3),0.)
  qs1d(k) = amax1(water(k,4),0.)
 enddo
 nlay = (kte-kts+1) + 1
 plev(1) = pw1d(1)
 tlev(1) = interface_t(1)
 tsfc    = skin
 do k = kts, kte
  plev(k+1)   = pw1d(k+1)
  tlev(k+1)   = interface_t(k+1)
  play(k)     = p1d(k)
  pdel(k)     = plev(k) - plev(k+1)
  tlay(k)     = temperature(k)
  h2ovmr(k)   = qv1d(k) * amdw
 enddo
 plev(kte+2)   = 1.0e-5
 tlev(kte+2)   = tlev(kte+1) + 0.0
 play(kte+1)   = 0.5 * plev(kte+1)
 tlay(kte+1)   = tlev(kte+1) + 0.0
 h2ovmr(kte+1) = h2ovmr(kte)
 play1(kts:nlay) = play(kts:nlay)
 call ozone_to_layers(nz1,play1,pin,ozone,o3mmr)
 do k = kts,kte+1
  o3vmr(k) = o3mmr(k)
 enddo
 scon = solcon
 adjes  = 1.0
 do k = kts, kte
  cldfrac(k) = cloud(k)
 enddo
 gravmks = gravity
 do k = kts, kte
  reliq(k) = max(2.5,re_cloud(k)*1.e6)
  if(reliq(k).le.2.5 .and. cldfrac(k).gt.0. .and.  (xland-1.5).gt.0.) then
   reliq(k) = 10.5
  elseif(reliq(k).le.2.5 .and. cldfrac(k).gt.0. .and.  (xland-1.5).lt.0.) then
   reliq(k) = 7.5
  endif
 enddo
 do k = kts, kte
  reice(k) = max(5.,re_ice(k)*1.e6)
  if(reice(k).le.5. .and. cldfrac(k).gt.0.) then
   idx_rei = int(temperature(k)-179.)
   idx_rei = min(max(idx_rei,1),75)
   corr = temperature(k) - int(temperature(k))
   reice(k) = retab_sw(idx_rei)*(1.-corr) + retab_sw(idx_rei+1)*corr
   reice(k) = max(reice(k),5.0)
  endif
 enddo
 do k = kts, kte
  resnow(k) = max(10.,re_snow(k)*1.e6)
 enddo
 do k = kts, kte
  gliqwp = qc1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  cliqwp(k) = gliqwp / max(0.01,cldfrac(k))
 enddo
 do k = kts, kte
  gicewp = qi1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  cicewp(k) = gicewp / max(0.01,cldfrac(k))
 enddo
 do k = kts, kte
  snow_mass_factor = 1.0
  if(resnow(k) .gt. 130.)then
   snow_mass_factor = (130.0/resnow(k))*(130.0/resnow(k))
   resnow(k)   = 130.0
  endif
  gsnowp = qs1d(k) * pdel(k)*100.0 / gravmks * 1000.0
  csnowp(k) = snow_mass_factor * gsnowp / max(0.01,cldfrac(k))
 enddo
 do k = kts, kte
  sw_cl(j,k,1) = cldfrac(k)
  sw_cl(j,k,2) = cicewp(k)
  sw_cl(j,k,3) = cliqwp(k)
  sw_cl(j,k,4) = csnowp(k)
  sw_cl(j,k,5) = reice(k)
  sw_cl(j,k,6) = reliq(k)
  sw_cl(j,k,7) = resnow(k)
 enddo
 sw_cl(j,kte+1,1) = 0.
 sw_cl(j,kte+1,2) = 0.
 sw_cl(j,kte+1,3) = 0.
 sw_cl(j,kte+1,4) = 0.
 sw_cl(j,kte+1,5) = 10.
 sw_cl(j,kte+1,6) = 10.
 sw_cl(j,kte+1,7) = 10.
 ! McICA's random stream (permuteseed 1)
 call mcica_stream(nlay, ngptsw, 1, play, lw_cdf, j)
 ! inatm_sw (iplon 1, tauaer 0, ssaaer 1, asmaer 0)
 nlayers = nlay
 do l = 1, max_sw_layers+1
  do imol = 1, nmol_sw
   wkl(imol,l) = 0.0_rb
  enddo
 enddo
 adjflx = adjes
 solvar = scon / rrsw_scon
 tbound = tsfc
 pz(0) = plev(1)
 tz(0) = tlev(1)
 do l = 1, nlayers
  pavel(l) = play(l)
  tavel(l) = tlay(l)
  pz(l) = plev(l+1)
  tz(l) = tlev(l+1)
  sw_pdp(j,l) = pz(l-1) - pz(l)
  wkl(1,l) = h2ovmr(l)
  wkl(2,l) = co2   ! the gas has this value at every level
  wkl(3,l) = o3vmr(l)
  wkl(4,l) = n2o   ! the gas has this value at every level
  wkl(6,l) = ch4   ! the gas has this value at every level
  wkl(7,l) = o2   ! the gas has this value at every level
  amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw
  coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad_sw /  (1.e2_rb * grav_sw * amm * (1._rb + wkl(1,l)))
 enddo
 do l = 1, nlayers
  do imol = 1, nmol_sw
   wkl(imol,l) = coldry(l) * wkl(imol,l)
  enddo
 enddo
 call rf_setcoef_sw(j, nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl)
 do k = 1, ngptsw
  sw_sflx(j,k) = 0.0
 enddo
 cossza = coszrs
 if (cossza .le. zepzen) cossza = zepzen
 sw_nlay(j) = nlayers
 sw_prmu0(j) = cossza
 sw_coszrs(j) = coszrs
 sw_alb(j) = albedo
 sw_adjflux(j) = adjflx * solvar
 end subroutine

 ! cldprmc_sw of g-point (band ib) in layer lay of column slot j, with inatm_sw's copies of the McICA subcolumn: cloudy or
 ! clear; the cloud inputs tauc/ssac/asmc/fsfc are 0/1/0/0
 subroutine sw_cloud_optics(j, lay, ib, cloudy, cldf, taucmc, taormc, asmcmc, ssacmc)
 use lean_dev_rrsw_cld, only: extliq1, ssaliq1, asyliq1, extice3, ssaice3, asyice3, fdlice3
 !$acc routine seq
 integer, intent(in) :: j, lay, ib
 logical, intent(in) :: cloudy
 real(rb), intent(out) :: cldf, taucmc, taormc, asmcmc, ssacmc
 real(rb), parameter :: cldmin = 1.0e-20_rb
 real(rb) :: cwp, ciwp, clwp, cswp, radice, radsno, radliq, factor, fint, fdelta
 real(rb) :: extcoice, gice, ssacoice, forwice, extcoliq, gliq, ssacoliq, forwliq, extcosno, gsno, ssacosno, forwsno
 real(rb) :: tauiceorig, scatice, ssaice, tauice, tauliqorig, scatliq, ssaliq, tauliq, tausnoorig, scatsno, ssasno, tausno
 integer :: index, istr
  if (cloudy) then
   cldf = 1._rb
   ciwp = sw_cl(j,lay,2); clwp = sw_cl(j,lay,3); cswp = sw_cl(j,lay,4)
  else
   cldf = 0._rb
   ciwp = 0._rb; clwp = 0._rb; cswp = 0._rb
  endif
  taucmc = 0._rb
  ssacmc = 1._rb
  asmcmc = 0._rb
  taormc = taucmc
  cwp = ciwp + clwp + cswp
  if (cldf .ge. cldmin .and.  (cwp .ge. cldmin .or. taucmc .ge. cldmin)) then
   radice = sw_cl(j,lay,5)
   if ((ciwp+cswp) .eq. 0.0_rb) then
    extcoice = 0.0_rb
    ssacoice = 0.0_rb
    gice     = 0.0_rb
    forwice  = 0.0_rb
    extcosno = 0.0_rb
    ssacosno = 0.0_rb
    gsno     = 0.0_rb
    forwsno  = 0.0_rb
   else
    if (radice .lt. 5.0_rb .or. radice .gt. 140.0_rb) then
     call physics_fault(13)
    endif
    factor = (radice - 2._rb)/3._rb
    index = int(factor)
    if (index .eq. 46) index = 45
    fint = factor - float(index)
    extcoice = extice3(index,ib) + fint *  (extice3(index+1,ib) - extice3(index,ib))
    ssacoice = ssaice3(index,ib) + fint *  (ssaice3(index+1,ib) - ssaice3(index,ib))
    gice = asyice3(index,ib) + fint *  (asyice3(index+1,ib) - asyice3(index,ib))
    fdelta = fdlice3(index,ib) + fint *  (fdlice3(index+1,ib) - fdlice3(index,ib))
    if (fdelta .lt. 0.0_rb) then
     call physics_fault(14)
    endif
    if (fdelta .gt. 1.0_rb) then
     call physics_fault(15)
    endif
    forwice = fdelta + 0.5_rb / ssacoice
    if (forwice .gt. gice) forwice = gice
    if (extcoice .lt. 0.0_rb) call physics_fault(25)
    if (ssacoice .gt. 1.0_rb) call physics_fault(26)
    if (ssacoice .lt. 0.0_rb) call physics_fault(27)
    if (gice .gt. 1.0_rb) call physics_fault(28)
    if (gice .lt. 0.0_rb) call physics_fault(29)
   endif
   if (cswp.gt.0.0_rb) then
    radsno = sw_cl(j,lay,7)
    if (radsno .lt. 5.0_rb .or. radsno .gt. 140.0_rb) then
     call physics_fault(16)
    endif
    factor = (radsno - 2._rb)/3._rb
    index = int(factor)
    if (index .eq. 46) index = 45
    fint = factor - float(index)
    extcosno = extice3(index,ib) + fint *  (extice3(index+1,ib) - extice3(index,ib))
    ssacosno = ssaice3(index,ib) + fint *  (ssaice3(index+1,ib) - ssaice3(index,ib))
    gsno = asyice3(index,ib) + fint *  (asyice3(index+1,ib) - asyice3(index,ib))
    fdelta = fdlice3(index,ib) + fint *  (fdlice3(index+1,ib) - fdlice3(index,ib))
    if (fdelta .lt. 0.0_rb) then
     call physics_fault(14)
    endif
    if (fdelta .gt. 1.0_rb) then
     call physics_fault(15)
    endif
    forwsno = fdelta + 0.5_rb / ssacosno
    if (forwsno .gt. gsno) forwsno = gsno
    if (extcosno .lt. 0.0_rb) then
     call physics_fault(17)
    endif
    if (ssacosno .gt. 1.0_rb) then
     call physics_fault(18)
    endif
    if (ssacosno .lt. 0.0_rb)  then
     call physics_fault(19)
    endif
    if (gsno .gt. 1.0_rb)  then
     call physics_fault(20)
    endif
    if (gsno .lt. 0.0_rb)  then
     call physics_fault(21)
    endif
   else
    extcosno = 0.0_rb
    ssacosno = 0.0_rb
    gsno     = 0.0_rb
    forwsno  = 0.0_rb
   endif
   if (clwp .eq. 0.0_rb) then
    extcoliq = 0.0_rb
    ssacoliq = 0.0_rb
    gliq = 0.0_rb
    forwliq = 0.0_rb
   else
    radliq = sw_cl(j,lay,6)
    if (radliq .lt. 1.5_rb .or. radliq .gt. 60._rb) call physics_fault(30)
    index = int(radliq - 1.5_rb)
    if (index .eq. 0) index = 1
    if (index .eq. 58) index = 57
    fint = radliq - 1.5_rb - float(index)
    extcoliq = extliq1(index,ib) + fint *  (extliq1(index+1,ib) - extliq1(index,ib))
    ssacoliq = ssaliq1(index,ib) + fint *  (ssaliq1(index+1,ib) - ssaliq1(index,ib))
    if (fint .lt. 0._rb .and. ssacoliq .gt. 1._rb)  ssacoliq = ssaliq1(index,ib)
    gliq = asyliq1(index,ib) + fint *  (asyliq1(index+1,ib) - asyliq1(index,ib))
    forwliq = gliq*gliq
    if (extcoliq .lt. 0.0_rb) call physics_fault(31)
    if (ssacoliq .gt. 1.0_rb) call physics_fault(32)
    if (ssacoliq .lt. 0.0_rb) call physics_fault(33)
    if (gliq .gt. 1.0_rb) call physics_fault(34)
    if (gliq .lt. 0.0_rb) call physics_fault(35)
   endif
   tauliqorig = clwp * extcoliq
   tauiceorig = ciwp * extcoice
   tausnoorig = cswp * extcosno
   taormc = tauliqorig + tauiceorig + tausnoorig
   ssaliq = ssacoliq * (1._rb - forwliq) /  (1._rb - forwliq * ssacoliq)
   tauliq = (1._rb - forwliq * ssacoliq) * tauliqorig
   ssaice = ssacoice * (1._rb - forwice) /  (1._rb - forwice * ssacoice)
   tauice = (1._rb - forwice * ssacoice) * tauiceorig
   ssasno = ssacosno * (1._rb - forwsno) /  (1._rb - forwsno * ssacosno)
   tausno = (1._rb - forwsno * ssacosno) * tausnoorig
   scatliq = ssaliq * tauliq
   scatice = ssaice * tauice
   scatsno = ssasno * tausno
   taucmc = tauliq + tauice + tausno
   if(taucmc.eq.0.) taucmc = cldmin
   if(scatice.eq.0.) scatice = cldmin
   if(scatsno.eq.0.) scatsno = cldmin
   ssacmc = (scatliq + scatice + scatsno) / taucmc
   istr = 1
   asmcmc = (1.0_rb/(scatliq+scatice+scatsno))                                *  (scatliq*(gliq**istr - forwliq)/(1.0_rb - forwliq)   + scatice * ((gice-forwice)/(1.0_rb - forwice))         + scatsno * ((gsno-forwsno)/(1.0_rb - forwsno))**istr)
  endif
 end subroutine

 ! reftra_sw for one layer (kmodts 2, the only case reftra_sw runs), same expressions
 subroutine reftra1(lrtchk, zg, prmuz, zto1, zw, pref, prefd, ptra, ptrad)
 use lean_dev_rrsw_tbl, only: tblint, bpade, od_lo, exp_tbl
 !$acc routine seq
 logical, intent(in) :: lrtchk
 real(rb), intent(in) :: zg, prmuz, zto1, zw
 real(rb), intent(out) :: pref, prefd, ptra, ptrad
 real(rb), parameter :: eps = 1.e-08_rb, zwcrit = 0.9999995_rb
 integer :: itind
 real(rb) :: tblind, za, za1, za2, zbeta, zdend, zdenr, zdent, ze1, ze2, zem1, zem2, zemm, zep1, zep2
 real(rb) :: zg3, zgamma1, zgamma2, zgamma3, zgamma4, zgt, zr1, zr2, zr3, zr4, zr5, zrk, zrk2, zrkg, zrm1, zrp, zrp1, zrpp
 real(rb) :: zt1, zt2, zt3, zt4, zt5, zwo, denom
 if (.not.lrtchk) then
  pref =0._rb
  ptra =1._rb
  prefd=0._rb
  ptrad=1._rb
 else
  zg3= 3._rb * zg
  zgamma1= (8._rb - zw * (5._rb + zg3)) * 0.25_rb
  zgamma2=  3._rb *(zw * (1._rb - zg )) * 0.25_rb
  zgamma3= (2._rb - zg3 * prmuz ) * 0.25_rb
  zgamma4= 1._rb - zgamma3
  zwo = 0._rb
  denom = 1._rb
  if (zg .ne. 1._rb) denom = (1._rb - (1._rb - zw) * (zg / (1._rb - zg))**2)
  if (zw .gt. 0._rb .and. denom .ne. 0._rb) zwo = zw / denom
  if (zwo >= zwcrit) then
   za  = zgamma1 * prmuz
   za1 = za - zgamma3
   zgt = zgamma1 * zto1
   ze1 = min ( zto1 / prmuz , 500._rb)
   if (ze1 .le. od_lo) then
    ze2 = 1._rb - ze1 + 0.5_rb * ze1 * ze1
   else
    tblind = ze1 / (bpade + ze1)
    itind = tblint * tblind + 0.5_rb
    ze2 = exp_tbl(itind)
   endif
   pref = (zgt - za1 * (1._rb - ze2)) / (1._rb + zgt)
   ptra = 1._rb - pref
   prefd = zgt / (1._rb + zgt)
   ptrad = 1._rb - prefd
   if (ze2 .eq. 1.0_rb) then
    pref = 0.0_rb
    ptra = 1.0_rb
    prefd = 0.0_rb
    ptrad = 1.0_rb
   endif
  else
   za1 = zgamma1 * zgamma4 + zgamma2 * zgamma3
   za2 = zgamma1 * zgamma3 + zgamma2 * zgamma4
   zrk = sqrt ( zgamma1**2 - zgamma2**2)
   zrp = zrk * prmuz
   zrp1 = 1._rb + zrp
   zrm1 = 1._rb - zrp
   zrk2 = 2._rb * zrk
   zrpp = 1._rb - zrp*zrp
   zrkg = zrk + zgamma1
   zr1  = zrm1 * (za2 + zrk * zgamma3)
   zr2  = zrp1 * (za2 - zrk * zgamma3)
   zr3  = zrk2 * (zgamma3 - za2 * prmuz )
   zr4  = zrpp * zrkg
   zr5  = zrpp * (zrk - zgamma1)
   zt1  = zrp1 * (za1 + zrk * zgamma4)
   zt2  = zrm1 * (za1 - zrk * zgamma4)
   zt3  = zrk2 * (zgamma4 + za1 * prmuz )
   zt4  = zr4
   zt5  = zr5
   zbeta = (zgamma1 - zrk) / zrkg
   ze1 = min ( zrk * zto1, 500._rb)
   ze2 = min ( zto1 / prmuz , 500._rb)
   if (ze1 .le. od_lo) then
    zem1 = 1._rb - ze1 + 0.5_rb * ze1 * ze1
    zep1 = 1._rb / zem1
   else
    tblind = ze1 / (bpade + ze1)
    itind = tblint * tblind + 0.5_rb
    zem1 = exp_tbl(itind)
    zep1 = 1._rb / zem1
   endif
   if (ze2 .le. od_lo) then
    zem2 = 1._rb - ze2 + 0.5_rb * ze2 * ze2
    zep2 = 1._rb / zem2
   else
    tblind = ze2 / (bpade + ze2)
    itind = tblint * tblind + 0.5_rb
    zem2 = exp_tbl(itind)
    zep2 = 1._rb / zem2
   endif
   zdenr = zr4*zep1 + zr5*zem1
   zdent = zt4*zep1 + zt5*zem1
   if (zdenr .ge. -eps .and. zdenr .le. eps) then
    pref = eps
    ptra = zem2
   else
    pref = zw * (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
    ptra = zem2 - zem2 * zw * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent
   endif
   zemm = zem1*zem1
   zdend = 1._rb / ( (1._rb - zbeta*zemm ) * zrkg)
   prefd =  zgamma2 * (1._rb - zemm) * zdend
   ptrad =  zrk2*zem1*zdend
  endif
 endif
 end subroutine

 ! vrtqdr_sw with the fluxes written to lw_gout(j,:,iw,ku/kd); prup/prupd at klev+1 are the albedo, as spcvmc_sw sets them
 subroutine vrtqdr_g(klev, alb, pref, prefd, ptra, ptrad, pdbt, ptdbt, j, iw, ku, kd)
 !$acc routine seq
 integer, intent(in) :: klev, j, iw, ku, kd
 real(rb), intent(in) :: alb, pref(:), prefd(:), ptra(:), ptrad(:), pdbt(:), ptdbt(:)
 real(rb) :: prup(max_sw_layers+1), prupd(max_sw_layers+1), prdnd(max_sw_layers+1), ztdn(max_sw_layers+1), zreflect
 integer :: ikp, ikx, jk
 prup(klev+1) = alb
 prupd(klev+1) = alb
 zreflect = 1._rb / (1._rb - prefd(klev+1) * prefd(klev))
 prup(klev) = pref(klev) + (ptrad(klev) *  ((ptra(klev) - pdbt(klev)) * prefd(klev+1) +  pdbt(klev) * pref(klev+1))) * zreflect
 prupd(klev) = prefd(klev) + ptrad(klev) * ptrad(klev) *  prefd(klev+1) * zreflect
 do jk = 1,klev-1
  ikp = klev+1-jk
  ikx = ikp-1
  zreflect = 1._rb / (1._rb -prupd(ikp) * prefd(ikx))
  prup(ikx) = pref(ikx) + (ptrad(ikx) *  ((ptra(ikx) - pdbt(ikx)) * prupd(ikp) +  pdbt(ikx) * prup(ikp))) * zreflect
  prupd(ikx) = prefd(ikx) + ptrad(ikx) * ptrad(ikx) *  prupd(ikp) * zreflect
 enddo
 ztdn(1) = 1._rb
 prdnd(1) = 0._rb
 ztdn(2) = ptra(1)
 prdnd(2) = prefd(1)
 do jk = 2,klev
  ikp = jk+1
  zreflect = 1._rb / (1._rb - prefd(jk) * prdnd(jk))
  ztdn(ikp) = ptdbt(jk) * ptra(jk) +  (ptrad(jk) * ((ztdn(jk) - ptdbt(jk)) +  ptdbt(jk) * pref(jk) * prdnd(jk))) * zreflect
  prdnd(ikp) = prefd(jk) + ptrad(jk) * ptrad(jk) *  prdnd(jk) * zreflect
 enddo
 do jk = 1,klev+1
  zreflect = 1._rb / (1._rb - prdnd(jk) * prupd(jk))
  lw_gout(j,jk,iw,ku) = (ptdbt(jk) * prup(jk) +  (ztdn(jk) - ptdbt(jk)) * prupd(jk)) * zreflect
  lw_gout(j,jk,iw,kd) = ptdbt(jk) + (ztdn(jk) - ptdbt(jk)+  ptdbt(jk) * prup(jk) * prdnd(jk)) * zreflect
 enddo
 end subroutine

 ! subcolumn iw: generate_stochastic_clouds_sw's overlap (cloudy layers as bits), then spcvmc_sw for g-point iw with its
 ! layer loops fused into one (cldprmc_sw, optical depths, delta scaling, reftra_sw and the beam of a layer together); its
 ! fluxes per level and its incoming flux are stored for rf_sw_sums
 subroutine rf_sw_transfer(j, iw)
 use lean_dev_rrsw_wvn, only: ngc, ngb
 use lean_dev_rrsw_tbl, only: tblint, bpade, od_lo, exp_tbl
 !$acc routine seq
 integer, intent(in) :: j, iw
 real(rb), parameter :: cldmin = 1.0e-20_rb, repclc = 1.e-12_rb
 real(rb) :: zrefc(max_sw_layers+1), zrefdc(max_sw_layers+1), ztrac(max_sw_layers+1), ztradc(max_sw_layers+1)
 real(rb) :: zdbtc(max_sw_layers+1), ztdbtc(max_sw_layers+1)
 real(rb) :: zref(max_sw_layers+1), zrefd(max_sw_layers+1), ztra(max_sw_layers+1), ztrad(max_sw_layers+1)
 real(rb) :: zdbt(max_sw_layers+1), ztdbt(max_sw_layers+1)
 real(rb) :: zincflx, ze1, tblind, zclear, zcloud, zdbtmc, zdbtmo, zf, zwf, tauorig, prmu0, taua, omga, asya, alb
 real(rb) :: cur, cldf_prev, cldf_cur, cldf, taucmc, taormc, asmcmc, ssacmc
 real(rb) :: ztauc, zomcc, zgcc, ztauo, zomco, zgco, zrefo, zrefdo, ztrao, ztrado
 real(rb) :: tdbt_nodel, tdbtc_nodel, zdbt_nodel, zdbtc_nodel
 integer :: klev, nlay, ilev, jk, ikl, ibm, ib, itind, g1
 integer(8) :: mask
 nlay = sw_nlay(j)
 klev = nlay
 ibm = 1
 g1 = ngc(1)
 do while (iw > g1)
  ibm = ibm + 1
  g1 = g1 + ngc(ibm)
 enddo
 ib = ngb(iw)
 ! generate_stochastic_clouds_sw, overlap 2, subcolumn iw
 mask = 0_8
 cldf_cur = sw_cl(j,1,1)
 if (cldf_cur < cldmin) cldf_cur = 0._rb
 cur = lw_cdf(j,1,iw)
 if (cur >= 1._rb - cldf_cur) mask = ibset(mask, 0)
 do ilev = 2, nlay
  cldf_prev = cldf_cur
  cldf_cur = sw_cl(j,ilev,1)
  if (cldf_cur < cldmin) cldf_cur = 0._rb
  if (.not. (cur > 1._rb - cldf_prev)) cur = lw_cdf(j,ilev,iw) * (1._rb - cldf_prev)
  if (cur >= 1._rb - cldf_cur) mask = ibset(mask, ilev-1)
 enddo
 ! spcvmc_sw, g-point iw (icpr 1)
 prmu0 = sw_prmu0(j)
 alb = sw_alb(j)
 taua = aer(1); omga = aer(2); asya = aer(3)
 zincflx = sw_adjflux(j) * sw_sflx(j,iw) * prmu0
 ztdbtc(1)=1.0_rb
 zdbtc(klev+1) =0.0_rb
 ztrac(klev+1) =0.0_rb
 ztradc(klev+1)=0.0_rb
 zrefc(klev+1) =alb
 zrefdc(klev+1)=alb
 ztdbt(1)=1.0_rb
 zdbt(klev+1) =0.0_rb
 ztra(klev+1) =0.0_rb
 ztrad(klev+1)=0.0_rb
 zref(klev+1) =alb
 zrefd(klev+1)=alb
 tdbt_nodel = 1.0_rb
 tdbtc_nodel = 1.0_rb
 sw_dir(j,1,iw,1) = tdbt_nodel
#ifdef LEAN_BUDGET
 sw_dir(j,1,iw,2) = tdbtc_nodel
#endif
 do jk=1,klev
  ikl=klev+1-jk
  call sw_cloud_optics(j, ikl, ib, btest(mask, ikl-1), cldf, taucmc, taormc, asmcmc, ssacmc)
  ztauc = lw_fracs(j,ikl,iw) + lw_taug(j,ikl,iw) + taua
  zomcc = lw_fracs(j,ikl,iw) * 1.0_rb + taua * omga
  zgcc = asya * omga * taua / zomcc
  zomcc = zomcc / ztauc
  zclear = 1.0_rb - cldf
  zcloud = cldf
  ze1 = ztauc / prmu0
  if (ze1 .le. od_lo) then
   zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
  else
   tblind = ze1 / (bpade + ze1)
   itind = tblint * tblind + 0.5_rb
   zdbtmc = exp_tbl(itind)
  endif
  zdbtc_nodel = zdbtmc
  tdbtc_nodel = zdbtc_nodel * tdbtc_nodel
  tauorig = ztauc + taormc
  ze1 = tauorig / prmu0
  if (ze1 .le. od_lo) then
   zdbtmo = 1._rb - ze1 + 0.5_rb * ze1 * ze1
  else
   tblind = ze1 / (bpade + ze1)
   itind = tblint * tblind + 0.5_rb
   zdbtmo = exp_tbl(itind)
  endif
  zdbt_nodel = zclear*zdbtmc + zcloud*zdbtmo
  tdbt_nodel = zdbt_nodel * tdbt_nodel
  sw_dir(j,jk+1,iw,1) = tdbt_nodel
#ifdef LEAN_BUDGET
  sw_dir(j,jk+1,iw,2) = tdbtc_nodel
#endif
  zf = zgcc * zgcc
  zwf = zomcc * zf
  ztauc = (1.0_rb - zwf) * ztauc
  zomcc = (zomcc - zwf) / (1.0_rb - zwf)
  zgcc  = (zgcc - zf) / (1.0_rb - zf)
  ztauo = ztauc + taucmc
  zomco = ztauc * zomcc + taucmc * ssacmc
  zgco  = (taucmc * ssacmc * asmcmc +  ztauc * zomcc * zgcc) / zomco
  zomco = zomco / ztauo
  call reftra1(.true., zgcc, prmu0, ztauc, zomcc, zrefc(jk), zrefdc(jk), ztrac(jk), ztradc(jk))
  call reftra1(cldf > repclc, zgco, prmu0, ztauo, zomco, zrefo, zrefdo, ztrao, ztrado)
  zref(jk) = zclear*zrefc(jk) + zcloud*zrefo
  zrefd(jk)= zclear*zrefdc(jk) + zcloud*zrefdo
  ztra(jk) = zclear*ztrac(jk) + zcloud*ztrao
  ztrad(jk)= zclear*ztradc(jk) + zcloud*ztrado
  ze1 = ztauc / prmu0
  if (ze1 .le. od_lo) then
   zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
  else
   tblind = ze1 / (bpade + ze1)
   itind = tblint * tblind + 0.5_rb
   zdbtmc = exp_tbl(itind)
  endif
  zdbtc(jk) = zdbtmc
  ztdbtc(jk+1) = zdbtc(jk)*ztdbtc(jk)
  ze1 = ztauo / prmu0
  if (ze1 .le. od_lo) then
   zdbtmo = 1._rb - ze1 + 0.5_rb * ze1 * ze1
  else
   tblind = ze1 / (bpade + ze1)
   itind = tblint * tblind + 0.5_rb
   zdbtmo = exp_tbl(itind)
  endif
  zdbt(jk) = zclear*zdbtmc + zcloud*zdbtmo
  ztdbt(jk+1) = zdbt(jk)*ztdbt(jk)
 enddo
#ifdef LEAN_BUDGET
 call vrtqdr_g(klev, alb, zrefc, zrefdc, ztrac, ztradc, zdbtc, ztdbtc, j, iw, 3, 4)
#endif
 call vrtqdr_g(klev, alb, zref, zrefd, ztra, ztrad, zdbt, ztdbt, j, iw, 1, 2)
 sw_zinc(j,iw) = zincflx
 end subroutine

 ! spcvmc_sw's sums at output level ikl, g-point by g-point in its order (sw_tot: bbfd bbfu bbcd bbcu uvfd uvcd nifd nicd
 ! bbfddir bbcddir uvfddir uvcddir nifddir nicddir)
 subroutine rf_sw_sums(j, ikl)
 use lean_dev_rrsw_wvn, only: ngc
 !$acc routine seq
 integer, intent(in) :: j, ikl
 integer :: klev, jk, ibm, jg, iw
 real(rb) :: zi, pbbfd, pbbfu, pbbcd, pbbcu, puvfd, puvcd, pnifd, pnicd, pbbfddir, pbbcddir, puvfddir, puvcddir, pnifddir, pnicddir
 klev = sw_nlay(j)
 if (ikl > klev+1) return
 jk = klev+2-ikl
 pbbfd=0._rb; pbbfu=0._rb; pbbcd=0._rb; pbbcu=0._rb; puvfd=0._rb; puvcd=0._rb; pnifd=0._rb; pnicd=0._rb
 pbbfddir=0._rb; pbbcddir=0._rb; puvfddir=0._rb; puvcddir=0._rb; pnifddir=0._rb; pnicddir=0._rb
 iw = 0
 do ibm = 1, nbndsw
  do jg = 1, ngc(ibm)
   iw = iw + 1
   zi = sw_zinc(j,iw)
   pbbfu = pbbfu + zi*lw_gout(j,jk,iw,1)
   pbbfd = pbbfd + zi*lw_gout(j,jk,iw,2)
#ifdef LEAN_BUDGET
   pbbcu = pbbcu + zi*lw_gout(j,jk,iw,3)
   pbbcd = pbbcd + zi*lw_gout(j,jk,iw,4)
#endif
   pbbfddir = pbbfddir + zi*sw_dir(j,jk,iw,1)
#ifdef LEAN_BUDGET
   pbbcddir = pbbcddir + zi*sw_dir(j,jk,iw,2)
#endif
   if (ibm >= 10 .and. ibm <= 13) then
#ifdef LEAN_BUDGET
    puvcd = puvcd + zi*lw_gout(j,jk,iw,4)
#endif
    puvfd = puvfd + zi*lw_gout(j,jk,iw,2)
#ifdef LEAN_BUDGET
    puvcddir = puvcddir + zi*sw_dir(j,jk,iw,2)
#endif
    puvfddir = puvfddir + zi*sw_dir(j,jk,iw,1)
   else if (ibm == 14 .or. ibm <= 9) then
#ifdef LEAN_BUDGET
    pnicd = pnicd + zi*lw_gout(j,jk,iw,4)
#endif
    pnifd = pnifd + zi*lw_gout(j,jk,iw,2)
#ifdef LEAN_BUDGET
    pnicddir = pnicddir + zi*sw_dir(j,jk,iw,2)
#endif
    pnifddir = pnifddir + zi*sw_dir(j,jk,iw,1)
   endif
  enddo
 enddo
 sw_tot(j,ikl,1) = pbbfd; sw_tot(j,ikl,2) = pbbfu; sw_tot(j,ikl,3) = pbbcd; sw_tot(j,ikl,4) = pbbcu
 sw_tot(j,ikl,5) = puvfd; sw_tot(j,ikl,6) = puvcd; sw_tot(j,ikl,7) = pnifd; sw_tot(j,ikl,8) = pnicd
 sw_tot(j,ikl,9) = pbbfddir; sw_tot(j,ikl,10) = pbbcddir; sw_tot(j,ikl,11) = puvfddir; sw_tot(j,ikl,12) = puvcddir
 sw_tot(j,ikl,13) = pnifddir; sw_tot(j,ikl,14) = pnicddir
 end subroutine

 ! the end of rrtmg_sw, radiation_sw_column (day column) and radiation_event_ws's sw(:)
 subroutine rf_sw_finish(j, pi3d, rthratensw, sw, flux)
 use lean_dev_rrsw_con, only: heatfac
 !$acc routine seq
 integer, intent(in) :: j
 real, intent(in) :: pi3d(nz)
#ifdef LEAN_NOAHMP_SWSPLIT
 real, intent(out) :: rthratensw(nz), sw(17), flux(4,max_sw_layers+1)
 real :: beam(6)
#else
 real, intent(out) :: rthratensw(nz), sw(14), flux(4,max_sw_layers+1)
 real :: beam(3)
#endif
 real(rb) :: swnflx(max_sw_layers+2), swnflxc(max_sw_layers+2), swhr(max_sw_layers+1), zdpgcp
 real :: swuflx(max_sw_layers+1), swdflx(max_sw_layers+1), swuflxc(max_sw_layers+1), swdflxc(max_sw_layers+1)
 real :: sibvisdir1, sibvisdif1, sibnirdir1, swdkdir1, swdkdif1, dirdflux, difdflux, dirdnuv, difdnuv, dirdnir
 real :: gsw, swcf, coszrs, tten1d
 integer :: nlayers, i, k
 nlayers = sw_nlay(j)
 do i = 1, nlayers+1
  swuflxc(i) = sw_tot(j,i,4)
  swdflxc(i) = sw_tot(j,i,3)
  swuflx(i) = sw_tot(j,i,2)
  swdflx(i) = sw_tot(j,i,1)
 enddo
 ! surface values of the direct/diffuse split (rrtmg_sw's loop at i = 1)
 dirdflux = sw_tot(j,1,9)
 difdflux = swdflx(1) - dirdflux
 swdkdir1 = dirdflux
 swdkdif1 = difdflux
 dirdnuv = sw_tot(j,1,11)
 difdnuv = sw_tot(j,1,5) - dirdnuv
 sibvisdir1 = dirdnuv
 sibvisdif1 = difdnuv
 dirdnir = sw_tot(j,1,13)
 sibnirdir1 = dirdnir
 do i = 1, nlayers+1
  swnflxc(i) = swdflxc(i) - swuflxc(i)
  swnflx(i) = swdflx(i) - swuflx(i)
 enddo
 do i = 1, nlayers
  zdpgcp = heatfac / sw_pdp(j,i)
  swhr(i) = (swnflx(i+1) - swnflx(i)) * zdpgcp
 enddo
 swhr(nlayers) = 0._rb
 coszrs = sw_coszrs(j)
 gsw  = swdflx(1) - swuflx(1)
 swcf = (swdflx(kte+2)-swuflx(kte+2)) - (swdflxc(kte+2)-swuflxc(kte+2))
 beam = 0.
 beam(1)  = swdkdir1
 beam(2)  = beam(1) / coszrs
 beam(3)  = swdkdif1
#ifdef LEAN_NOAHMP_SWSPLIT
 if (swdflx(1)>0.) then
  beam(4) = sibvisdir1/swdflx(1)
  beam(5) = sibnirdir1/swdflx(1)
  beam(6) = sibvisdif1/swdflx(1)
 endif
#endif
 flux = 0.
 do k = 1, kte+2
  flux(1,k) = swuflx(k)
  flux(3,k) = swuflxc(k)
  flux(2,k) = swdflx(k)
  flux(4,k) = swdflxc(k)
 enddo
 do k = 1, kte
  tten1d = swhr(k)/86400.
  rthratensw(k) = tten1d/pi3d(k)
 enddo
 sw=[coszrs,gsw,swcf,flux(1,nz1+1),flux(3,nz1+1),flux(2,nz1+1), &
  flux(4,nz1+1),flux(1,1),flux(3,1),flux(2,1),flux(4,1),beam]
 end subroutine
end module
