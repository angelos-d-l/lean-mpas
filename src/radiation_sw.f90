! The RRTMG shortwave column driver, translated from MPAS 8.4.2 (physics_wrf/module_ra_rrtmg_sw.F; terms in NOTICE.md).
module radiation_sw
 use run_settings,only: radt_sw
 use config,only: nz,nz1,kte=>nz
 use radiation_sizes,only: max_sw_layers
 use mpas_atmphys_constants,only: g=>gravity,degrad
 use parrrsw,only: nbndsw,ngptsw,naerec
 use module_ra_rrtmg_lw,only: retab
 use radiation_cloud,only: relcalc,reicalc
 use schemes,only: has_re
 use radiation_ozone,only: ozone_to_layers
 use lean_mcica_subcol_gen_sw,only: mcica_subcol_sw
 use lean_rrtmg_sw_rad,only: rrtmg_sw
 implicit none
 private
 public :: radiation_sw_column
contains
 recursive subroutine radiation_sw_column(p3d,p8w,pi3d,t3d,t8w,dz8w,qv3d,qc3d,qi3d,qs3d,cldfra3d, &
  tsk,albedo,xland,xice,snow,xlat,xlong,julday,pin,o3clim,re_cloud,re_ice,re_snow, &
  xtime,gmt,declin,solcon,coszr,gsw,swcf,rthratensw,flux,beam, ws)
 use radiation_scratch, only: sw_scratch
 type(sw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
 real,intent(in) :: p3d(nz),p8w(nz+1),pi3d(nz),t3d(nz),t8w(nz+1),dz8w(nz)
 real,intent(in) :: qv3d(nz),qc3d(nz),qi3d(nz),qs3d(nz),cldfra3d(nz)
 real,intent(in) :: tsk,albedo,xland,xice,snow,xlat,xlong,pin(59),o3clim(59)
 real,intent(in) :: re_cloud(nz),re_ice(nz),re_snow(nz),xtime,gmt,declin,solcon
 integer,intent(in) :: julday
#ifdef LEAN_NOAHMP_SWSPLIT
 real,intent(out) :: coszr,gsw,swcf,rthratensw(nz),flux(4,max_sw_layers+1),beam(6)
#else
 real,intent(out) :: coszr,gsw,swcf,rthratensw(nz),flux(4,max_sw_layers+1),beam(3)
#endif
 integer,parameter :: kts=1,noznlevels=59,cldovrlp=2,idcor=0,aer_opt=0
logical:: dorrsw
integer:: calc_clean_atm_diag
integer:: na,nb,ncol,nlay,icld,inflgsw,iceflgsw,liqflgsw
integer:: dyofyr
integer:: iplon,irng,permuteseed
integer:: pcols,pver
integer:: idx_rei
integer:: i,j,k,n
real:: coszrs,xt24,tloctm,hrang,xxlat,adjes,scon
real:: ro,dz
real:: corr
real:: gliqwp,gicewp,gsnowp,gravmks
real:: snow_mass_factor
real:: dzsum,lat
real,dimension(1):: tsfc,landfrac,landm,snowh,icefrac
real,dimension(1):: asdir,asdif,aldir,aldif,coszen
real,dimension(1,1:max_sw_layers-1):: pdel,cicewp,cliqwp,csnowp,reliq,reice,resnow
real,dimension(kts:max_sw_layers-1)::  tten1d,p1d,qv1d,qc1d,qi1d,qs1d
real,dimension(kts:max_sw_layers):: pw1d
real,dimension(kts:max_sw_layers):: o3mmr
real,dimension(1,kts:max_sw_layers):: play,hlay,tlay,h2ovmr,o3vmr,co2vmr,o2vmr,ch4vmr,n2ovmr
real,dimension(1,kts:max_sw_layers):: clwpth,ciwpth,cswpth,rel,rei,res,cldfrac,relqmcl,reicmcl,resnmcl
real,dimension(1,kts:max_sw_layers):: swhr,swhrc
real,dimension(1,kts:max_sw_layers+1):: plev,tlev
real,dimension(1,kts:max_sw_layers+1):: swuflx,swdflx,swuflxc,swdflxc
real,dimension(1,kts:max_sw_layers+1):: swuflxcln,swdflxcln
real,dimension(1,kts:max_sw_layers+1):: sibvisdir,sibvisdif,sibnirdir,sibnirdif
real,dimension(1,kts:max_sw_layers+1):: swdkdir,swdkdif,swdkdirc
real :: co2
#ifdef LEAN_RAD_2025
! Lean departure (build switch LEAN_RAD_2025, on in flavor lean, off in mpas; docs/fixes.md): NOAA GML's global mean of 2025, not WRF RRTMG's 2005
data co2 / 425.62e-6 /
#else
data co2 / 379.e-6 /
#endif
real :: ch4
#ifdef LEAN_RAD_2025
data ch4 / 1936.03e-9 /
#else
data ch4 / 1774.e-9 /
#endif
real :: n2o
#ifdef LEAN_RAD_2025
data n2o / 338.86e-9 /
#else
data n2o / 319.e-9 /
#endif
real :: o2
data o2 / 0.209488 /
real :: amdw
real :: amdo
real :: amdo2
data amdw /  1.607793 /
data amdo /  0.603461 /
data amdo2 / 0.905190 /
gsw=0.
swcf=0.
rthratensw=0.
flux=0.
beam=0.
ncol = 1
icld = cldovrlp
inflgsw  = 2
iceflgsw = 3
liqflgsw = 1
calc_clean_atm_diag = 0
dorrsw = .true.
xt24 = mod(xtime+(radt_sw/60.)*0.5,1440.)
tloctm = gmt + xt24/60. + xlong/15.
hrang = 15. * (tloctm-12.) * degrad
xxlat = xlat * degrad
coszrs = sin(xxlat) * sin(declin) + cos(xxlat) * cos(declin) * cos(hrang)
coszr = coszrs
if (coszrs.le.0.0) dorrsw = .false.
if(dorrsw) then
call pressure_and_contents()
call initialize_workspace()
call model_and_upper_profiles()
call cloud_properties()
call solve_and_return()
endif
contains
recursive subroutine pressure_and_contents()
lat = xlat
do k = kts, kte+1
pw1d(k) = p8w(k) / 100.
enddo
do k = kts, kte
p1d(k)      = p3d(k) / 100.
qv1d(k)     = amax1(qv3d(k),1.e-12)
qc1d(k)     = 0.
qi1d(k)     = 0.
qs1d(k)     = 0.
enddo
do k = kts,kte
qc1d(k) = amax1(qc3d(k),0.)
qi1d(k) = amax1(qi3d(k),0.)
qs1d(k) = amax1(qs3d(k),0.)
enddo
do k = kts, kte
tten1d(k) = 0.
enddo
end subroutine pressure_and_contents
recursive subroutine initialize_workspace()
nlay = (kte-kts+1) + 1
do n = 1, ncol
do k = kts, kte
reliq(ncol,k)  = 10.
reice(ncol,k)  = 10.
resnow(ncol,k) = 10.
cliqwp(ncol,k) = 0.
cicewp(ncol,k) = 0.
csnowp(ncol,k) = 0.
enddo
do k = 1, nlay
clwpth(n,k)    = 0.
ciwpth(n,k)    = 0.
cswpth(n,k)    = 0.
rel(n,k)       = 0.
rei(n,k)       = 0.
res(n,k)       = 0.
cldfrac(n,k)   = 0.
relqmcl(n,k)   = 0.
reicmcl(n,k)   = 0.
resnmcl(n,k)   = 0.
swuflx(n,k)    = 0.
swuflxc(n,k)   = 0.
swuflxcln(n,k) = 0.
swdflx(n,k)    = 0.
swdflxc(n,k)   = 0.
swdflxcln(n,k) = 0.
swhr(n,k)      = 0.
swhrc(n,k)     = 0.
ws%taucld(1:nbndsw,n,k)  = 0.
ws%tauaer(n,k,1:nbndsw)  = 0.
ws%ssaaer(n,k,1:nbndsw)  = 0.
ws%asmaer(n,k,1:nbndsw)  = 0.
ws%cldfmcl(1:ngptsw,n,k) = 0.
ws%clwpmcl(1:ngptsw,n,k) = 0.
ws%ciwpmcl(1:ngptsw,n,k) = 0.
ws%cswpmcl(1:ngptsw,n,k) = 0.
ws%taucmcl(1:ngptsw,n,k) = 0.
enddo
do k = 1, nlay + 1
sibvisdir(ncol,k) = 0.
sibvisdif(ncol,k) = 0.
sibnirdir(ncol,k) = 0.
sibnirdif(ncol,k) = 0.
swdkdir(n,k)      = 0.
swdkdif(n,k)      = 0.
swdkdirc(n,k)     = 0.
enddo
swuflx(n,nlay+1)    = 0.
swuflxc(n,nlay+1)   = 0.
swuflxcln(n,nlay+1) = 0.
swdflx(n,nlay+1)    = 0.
swdflxc(n,nlay+1)   = 0.
swdflxcln(n,nlay+1) = 0.
enddo
do n = 1, ncol
do k = 1, nlay
do na = 1, naerec
ws%ecaer(n,k,na)   = 0.
enddo
enddo
enddo
end subroutine initialize_workspace
recursive subroutine model_and_upper_profiles()
plev(ncol,1) = pw1d(1)
tlev(ncol,1) = t8w(1)
tsfc(ncol)   = tsk
do k = kts, kte
plev(ncol,k+1)   = pw1d(k+1)
tlev(ncol,k+1)   = t8w(k+1)
play(ncol,k)     = p1d(k)
pdel(ncol,k)     = plev(ncol,k) - plev(ncol,k+1)
tlay(ncol,k)     = t3d(k)
h2ovmr(ncol,k)   = qv1d(k) * amdw
co2vmr(ncol,k)   = co2
o2vmr(ncol,k)    = o2
ch4vmr(ncol,k)   = ch4
n2ovmr(ncol,k)   = n2o
enddo
plev(ncol,kte+2)   = 1.0e-5
tlev(ncol,kte+2)   = tlev(ncol,kte+1) + 0.0
play(ncol,kte+1)   = 0.5 * plev(ncol,kte+1)
tlay(ncol,kte+1)   = tlev(ncol,kte+1) + 0.0
h2ovmr(ncol,kte+1) = h2ovmr(ncol,kte)
co2vmr(ncol,kte+1) = co2vmr(ncol,kte)
o2vmr(ncol,kte+1)  = o2vmr(ncol,kte)
ch4vmr(ncol,kte+1) = ch4vmr(ncol,kte)
n2ovmr(ncol,kte+1) = n2ovmr(ncol,kte)
dzsum = 0.
do k = kts, kte
hlay(ncol,k) = dzsum + 0.5*dz8w(k)
dzsum = dzsum + dz8w(k)
enddo
hlay(ncol,kte+1) = dzsum + 0.5*dz8w(kte)
call ozone_to_layers(nz1,play(1,:),pin,o3clim,o3mmr)
do k = kts,kte+1
o3vmr(ncol,k) = o3mmr(k)
enddo
asdir(ncol) = albedo
asdif(ncol) = albedo
aldir(ncol) = albedo
aldif(ncol) = albedo
scon = solcon
coszen(ncol) = coszrs
dyofyr = 0
adjes  = 1.0
end subroutine model_and_upper_profiles
recursive subroutine cloud_properties()
if(inflgsw .gt. 0) then
do k = kts, kte
cldfrac(ncol,k) = cldfra3d(k)
enddo
do k = kts, kte
do nb = 1, nbndsw
ws%taucld(nb,ncol,k) = 0.0
ws%ssacld(nb,ncol,k) = 1.0
ws%asmcld(nb,ncol,k) = 0.0
ws%fsfcld(nb,ncol,k) = 0.0
enddo
enddo
pcols = ncol
pver  = kte - kts + 1
gravmks = g
if(has_re .ne. 0) then
inflgsw = 3
do k = kts, kte
reliq(ncol,k) = max(2.5,re_cloud(k)*1.e6)
if(reliq(ncol,k).le.2.5 .and. cldfrac(ncol,k).gt.0. .and.  (xland-1.5).gt.0.) then
reliq(ncol,k) = 10.5
elseif(reliq(ncol,k).le.2.5 .and. cldfrac(ncol,k).gt.0. .and.  (xland-1.5).lt.0.) then
reliq(ncol,k) = 7.5
endif
enddo
else
landfrac(ncol) = 2.-xland
landm(ncol)    = landfrac(ncol)
snowh(ncol)    = 0.001*snow
icefrac(ncol)  = xice
call relcalc(ncol,pcols,pver,tlay,landfrac,landm,icefrac,reliq,snowh)
endif
if(has_re .ne. 0) then
inflgsw  = 4
iceflgsw = 4
do k = kts, kte
reice(ncol,k) = max(5.,re_ice(k)*1.e6)
if(reice(ncol,k).le.5. .and. cldfrac(ncol,k).gt.0.) then
idx_rei = int(t3d(k)-179.)
idx_rei = min(max(idx_rei,1),75)
corr = t3d(k) - int(t3d(k))
reice(ncol,k) = retab(idx_rei)*(1.-corr) + retab(idx_rei+1)*corr
reice(ncol,k) = max(reice(ncol,k),5.0)
endif
enddo
else
call reicalc(ncol,pcols,pver,tlay,reice)
if(iceflgsw .eq. 3) then
do k = kts, kte
reice(ncol,k) = reice(ncol,k) * 1.0315
reice(ncol,k) = min(140.0,reice(ncol,k))
enddo
endif
endif
if(has_re .ne. 0) then
inflgsw  = 5
iceflgsw = 5
do k = kts, kte
resnow(ncol,k) = max(10.,re_snow(k)*1.e6)
enddo
else
do k = kts, kte
resnow(ncol,k) = 10.
enddo
endif
do k = kts, kte
gliqwp = qc1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0
cliqwp(ncol,k) = gliqwp / max(0.01,cldfrac(ncol,k))
enddo
if(iceflgsw .eq. 3)then
do k = kts, kte
gicewp = (qi1d(k)+qs1d(k)) * pdel(ncol,k)*100.0 / gravmks * 1000.0
cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))
enddo
elseif(iceflgsw .ge. 4) then
do k = kts, kte
gicewp = qi1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0
cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))
enddo
endif
if(iceflgsw.eq.5)then
do k = kts, kte
snow_mass_factor = 1.0
if(resnow(ncol,k) .gt. 130.)then
snow_mass_factor = (130.0/resnow(ncol,k))*(130.0/resnow(ncol,k))
resnow(ncol,k)   = 130.0
endif
gsnowp = qs1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0
csnowp(ncol,k) = snow_mass_factor * gsnowp / max(0.01,cldfrac(ncol,k))
enddo
endif
do k = kts, kte
clwpth(ncol,k) = cliqwp(ncol,k)
ciwpth(ncol,k) = cicewp(ncol,k)
cswpth(ncol,k) = csnowp(ncol,k)
rel(ncol,k) = reliq(ncol,k)
rei(ncol,k) = reice(ncol,k)
res(ncol,k) = resnow(ncol,k)
enddo
clwpth(ncol,kte+1)  = 0.
ciwpth(ncol,kte+1)  = 0.
cswpth(ncol,kte+1)  = 0.
rel(ncol,kte+1)     = 10.
rei(ncol,kte+1)     = 10.
res(ncol,kte+1)     = 10.
cldfrac(ncol,kte+1) = 0.
do nb = 1, nbndsw
ws%taucld(nb,ncol,kte+1) = 0.
ws%ssacld(nb,ncol,kte+1) = 1.
ws%asmcld(nb,ncol,kte+1) = 0.
ws%fsfcld(nb,ncol,kte+1) = 0.
enddo
endif
end subroutine cloud_properties
recursive subroutine solve_and_return()
iplon = 1
irng  = 0
permuteseed = 1
call mcica_subcol_sw  (iplon   , ncol    , nlay    , icld    , permuteseed , irng    , play    ,  cldfrac , ciwpth  , clwpth  , cswpth  , rei         , rel     , res     ,  ws%taucld  , ws%ssacld  , ws%asmcld  , ws%fsfcld  , hlay        , idcor   , julday  ,  lat     , ws%cldfmcl , ws%ciwpmcl , ws%clwpmcl , ws%cswpmcl     , reicmcl , relqmcl ,  resnmcl , ws%taucmcl , ws%ssacmcl , ws%asmcmcl , ws%fsfcmcl, ws=ws)
do nb = 1, nbndsw
do k = kts, kte+1
ws%tauaer(ncol,k,nb) = 0.
ws%ssaaer(ncol,k,nb) = 1.
ws%asmaer(ncol,k,nb) = 0.
enddo
enddo
do na = 1, naerec
do k = kts, kte+1
ws%ecaer(ncol,k,na) = 0.
enddo
enddo
call rrtmg_sw  (ncol      , nlay      , icld      , play      , plev      , tlay      ,  tlev      , tsfc      , h2ovmr    , o3vmr     , co2vmr    , ch4vmr    ,  n2ovmr    , o2vmr     , asdir     , asdif     , aldir     , aldif     ,  coszen    , adjes     , dyofyr    , scon      , inflgsw   , iceflgsw  ,  liqflgsw  , ws%cldfmcl   , ws%taucmcl   , ws%ssacmcl   , ws%asmcmcl   , ws%fsfcmcl   ,  ws%ciwpmcl   , ws%clwpmcl   , ws%cswpmcl   , reicmcl   , relqmcl   , resnmcl   ,  ws%tauaer    , ws%ssaaer    , ws%asmaer    , ws%ecaer     , swuflx    , swdflx    ,  swhr      , swuflxc   , swdflxc   , swhrc     , swuflxcln , swdflxcln ,  aer_opt   , sibvisdir , sibvisdif , sibnirdir , sibnirdif , swdkdir   ,  swdkdif   , swdkdirc  , calc_clean_atm_diag, ws=ws)
gsw  = swdflx(1,1) - swuflx(1,1)
swcf = (swdflx(1,kte+2)-swuflx(1,kte+2)) - (swdflxc(1,kte+2)-swuflxc(1,kte+2))
beam(1)  = swdkdir(1,1)
beam(2)  = beam(1) / coszrs
beam(3)  = swdkdif(1,1)
#ifdef LEAN_NOAHMP_SWSPLIT
if (swdflx(1,1)>0.) then
beam(4) = sibvisdir(1,1)/swdflx(1,1)
beam(5) = sibnirdir(1,1)/swdflx(1,1)
beam(6) = sibvisdif(1,1)/swdflx(1,1)
endif
#endif
do k = kts, kte+2
flux(1,k)  = swuflx(1,k)
flux(3,k) = swuflxc(1,k)
flux(2,k)  = swdflx(1,k)
flux(4,k) = swdflxc(1,k)
enddo
do k = kts, kte
tten1d(k) = swhr(ncol,k)/86400.
rthratensw(k) = tten1d(k)/pi3d(k)
enddo
end subroutine solve_and_return
end subroutine
end module
