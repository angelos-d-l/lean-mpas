! The RRTMG longwave column driver, translated from MPAS 8.4.2 (physics_wrf/module_ra_rrtmg_lw.F; terms in NOTICE.md).
! One column per call: the work arrays have fixed sizes (radiation_scratch.f90, radiation_sizes.f90), and the RRTMG tables are
! read once at startup (radiation_tables.f90).
module radiation_lw
 use config, only: nz,kte=>nz
 use radiation_sizes, only: max_lw_layers
 use mpas_atmphys_constants, only: g=>gravity
 use radiation_ozone, only: ozone_to_layers
 use radiation_cloud, only: relcalc, reicalc, retab
 use schemes, only: has_re
 use parrrtm, only: nbndlw,ngptlw
 use lean_rrtmg_lw_rad, only: rrtmg_lw
 use lean_mcica_subcol_gen_lw, only: mcica_subcol_lw
 implicit none
 private
 public :: radiation_lw_column,max_lw_layers
 real, parameter :: deltap=4.
contains
 recursive subroutine radiation_lw_column(p3d,p8w,pi3d,t3d,t8w,dz8w,qv3d,qc3d,qi3d,qs3d, &
  cldfra3d,tsk,emiss,xland,xice,snow,xlat,julday,pin,o3clim,re_cloud,re_ice,re_snow, &
  glw,olr,lwcf,rthratenlw,flux,active_layers,radii, ws)
 use radiation_scratch, only: lw_scratch
 type(lw_scratch), intent(inout) :: ws   ! large work arrays of the column (radiation_scratch.f90)
 real, intent(in) :: p3d(nz),p8w(nz+1),pi3d(nz),t3d(nz),t8w(nz+1),dz8w(nz)
 real, intent(in) :: qv3d(nz),qc3d(nz),qi3d(nz),qs3d(nz),cldfra3d(nz)
 real, intent(in) :: tsk,emiss,xland,xice,snow,xlat,pin(59),o3clim(59)
 real, intent(in) :: re_cloud(nz),re_ice(nz),re_snow(nz)
 integer, intent(in) :: julday
 real, intent(out) :: glw,olr,lwcf,rthratenlw(nz),flux(4,max_lw_layers+1)
 real, intent(out) :: radii(nz,3)
 integer, intent(out) :: active_layers
 integer, parameter :: kts=1,noznlevels=59,cldovrlp=2,idcor=0
integer:: calc_clean_atm_diag
integer:: nb,ncol,nlay,icld,inflglw,iceflglw,liqflglw
integer:: iplon,irng,permuteseed
integer:: pcols,pver
integer:: idx_rei
integer:: i,j,k,kk,n
real:: ro,dz
real:: corr
real:: gliqwp,gicewp,gsnowp,gravmks
real:: snow_mass_factor
real:: dzsum,lat
real,dimension(1):: tsfc,landfrac,landm,snowh,icefrac
real,dimension(1,1:kte-kts+1):: pdel,cliqwp,cicewp,csnowp,reliq,reice,resnow
real,dimension(1,nbndlw):: emis
real,dimension(kts:kte)::  tten1d,p1d,qv1d,qc1d,qi1d,qs1d
real,dimension(kts:kte+1):: pw1d
real :: o3mmr(kts:max_lw_layers)
real :: varint(kts:max_lw_layers+1)
real :: plev(1,kts:max_lw_layers+1)
real :: tlev(1,kts:max_lw_layers+1)
real :: play(1,kts:max_lw_layers)
real :: hlay(1,kts:max_lw_layers)
real :: tlay(1,kts:max_lw_layers)
real :: h2ovmr(1,kts:max_lw_layers)
real :: o3vmr(1,kts:max_lw_layers)
real :: co2vmr(1,kts:max_lw_layers)
real :: o2vmr(1,kts:max_lw_layers)
real :: ch4vmr(1,kts:max_lw_layers)
real :: n2ovmr(1,kts:max_lw_layers)
real :: cfc11vmr(1,kts:max_lw_layers)
real :: cfc12vmr(1,kts:max_lw_layers)
real :: cfc22vmr(1,kts:max_lw_layers)
real :: ccl4vmr(1,kts:max_lw_layers)
real :: clwpth(1,kts:max_lw_layers)
real :: ciwpth(1,kts:max_lw_layers)
real :: cswpth(1,kts:max_lw_layers)
real :: rel(1,kts:max_lw_layers)
real :: rei(1,kts:max_lw_layers)
real :: res(1,kts:max_lw_layers)
real :: cldfrac(1,kts:max_lw_layers)
real :: relqmcl(1,kts:max_lw_layers)
real :: reicmcl(1,kts:max_lw_layers)
real :: resnmcl(1,kts:max_lw_layers)
real :: uflx(1,kts:max_lw_layers+1)
real :: dflx(1,kts:max_lw_layers+1)
real :: uflxc(1,kts:max_lw_layers+1)
real :: dflxc(1,kts:max_lw_layers+1)
real :: hr(1,kts:max_lw_layers)
real :: hrc(1,kts:max_lw_layers)
real :: uflxcln(1,kts:max_lw_layers+1)
real :: dflxcln(1,kts:max_lw_layers+1)
integer:: nlayers
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
real :: cfc11
data cfc11 / 0.251e-9 /
real :: cfc12
data cfc12 / 0.538e-9 /
real :: cfc22
data cfc22 / 0.169e-9 /
real :: ccl4
data ccl4 / 0.093e-9 /
real :: o2
data o2 / 0.209488 /
real :: amdw
real :: amdo
real :: amdo2
data amdw /  1.607793 /
data amdo /  0.603461 /
data amdo2 / 0.905190 /
integer,parameter:: nproflevs = 60
integer:: klev
real:: wght,vark,vark1
real,dimension(nproflevs):: pprof,tprof
data pprof /1000.00,855.47,731.82,626.05,535.57,458.16,  391.94,335.29,286.83,245.38,209.91,179.57,  153.62,131.41,112.42, 96.17, 82.27, 70.38,  60.21, 51.51, 44.06, 37.69, 32.25, 27.59,  23.60, 20.19, 17.27, 14.77, 12.64, 10.81,  9.25,  7.91, 6.77,   5.79,  4.95,  4.24,  3.63,  3.10, 2.65,   2.27,  1.94,  1.66,  1.42,  1.22, 1.04,   0.89,  0.76,  0.65,  0.56,  0.48, 0.41,   0.35,  0.30,  0.26,  0.22,  0.19, 0.16,   0.14,  0.12,  0.10/
data tprof /286.96,281.07,275.16,268.11,260.56,253.02,   245.62,238.41,231.57,225.91,221.72,217.79,   215.06,212.74,210.25,210.16,210.69,212.14,   213.74,215.37,216.82,217.94,219.03,220.18,   221.37,222.64,224.16,225.88,227.63,229.51,   231.50,233.73,236.18,238.78,241.60,244.44,   247.35,250.33,253.32,256.30,259.22,262.12,   264.80,266.50,267.59,268.44,268.69,267.76,   266.13,263.96,261.54,258.93,256.15,253.23,   249.89,246.67,243.48,240.25,236.66,233.86/
call pressure_and_contents()
call initialize_workspace()
call model_profiles()
call upper_profiles()
call cloud_properties()
call solve_and_return()
contains
recursive subroutine pressure_and_contents()
radii=0.
ncol = 1
icld = cldovrlp
inflglw  = 2
iceflglw = 3
liqflglw = 1
calc_clean_atm_diag = 0
lat = xlat
do nb = 1, nbndlw
emis(ncol,nb) = emiss
enddo
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
nlayers = kte + max(nint(pw1d(kte+1)/deltap), 1)
if(nlayers>max_lw_layers) error stop 'LW layer capacity exceeded'
active_layers=nlayers
nlay = nlayers
do n = 1, ncol
do k = kts, kte
reliq(ncol,k)  = 10.
reice(ncol,k)  = 10.
resnow(ncol,k) = 10.
cliqwp(ncol,k) = 0.
cicewp(ncol,k) = 0.
csnowp(ncol,k) = 0.
enddo
do k = 1, nlayers
clwpth(n,k)  = 0.
ciwpth(n,k)  = 0.
cswpth(n,k)  = 0.
rel(n,k)     = 0.
rei(n,k)     = 0.
res(n,k)     = 0.
cldfrac(n,k) = 0.
relqmcl(n,k) = 0.
reicmcl(n,k) = 0.
resnmcl(n,k) = 0.
uflx(n,k)    = 0.
uflxc(n,k)   = 0.
dflx(n,k)    = 0.
dflxc(n,k)   = 0.
uflxcln(n,k) = 0.
dflxcln(n,k) = 0.
hr(n,k)      = 0.
hrc(n,k)     = 0.
ws%taucld(1:nbndlw,n,k)  = 0.
ws%tauaer(n,k,1:nbndlw)  = 0.
ws%cldfmcl(1:ngptlw,n,k) = 0.
ws%clwpmcl(1:ngptlw,n,k) = 0.
ws%ciwpmcl(1:ngptlw,n,k) = 0.
ws%cswpmcl(1:ngptlw,n,k) = 0.
ws%taucmcl(1:ngptlw,n,k) = 0.
enddo
uflx(n,nlayers+1)    = 0.
uflxc(n,nlayers+1)   = 0.
uflxcln(n,nlayers+1) = 0.
dflx(n,nlayers+1)    = 0.
dflxc(n,nlayers+1)   = 0.
dflxcln(n,nlayers+1) = 0.
enddo
end subroutine initialize_workspace
recursive subroutine model_profiles()
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
cfc11vmr(ncol,k) = cfc11
cfc12vmr(ncol,k) = cfc12
cfc22vmr(ncol,k) = cfc22
ccl4vmr(ncol,k)  = ccl4
enddo
dzsum = 0.
do k = kts, kte
hlay(ncol,k) = dzsum + 0.5*dz8w(k)
dzsum = dzsum + dz8w(k)
enddo
do k = kte+1,nlayers
hlay(ncol,k) = dzsum + 0.5*dz8w(kte)
dzsum = dzsum + dz8w(kte)
enddo
end subroutine model_profiles
recursive subroutine upper_profiles()
!--- the sourcecode below follows Steven Cavallo's method to "fill" the atmospheric layers between the
do k=kte+1,nlayers,1
plev(ncol,k+1) = max(plev(ncol,k) - deltap, 0.00)
play(ncol,k) = 0.5*(plev(ncol,k) + plev(ncol,k+1))
enddo
plev(ncol,nlayers+1) = 0.00
play(ncol,nlayers) =  0.5*(plev(ncol,nlayers) + plev(ncol,nlayers+1))
do k = 1, nlayers+1, 1
if(pprof(nproflevs) .lt. plev(ncol,k)) then
do kk = 2, nproflevs, 1
if(pprof(kk) .lt. plev(ncol,k)) then
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
wght  = (plev(ncol,k)-pprof(klev) )/( pprof(klev+1)-pprof(klev))
else
vark  = tprof(klev)
vark1 = tprof(klev)
wght  = 0.0
endif
varint(k) = wght*(vark1-vark)+vark
enddo
do k = kte+1, nlayers+1, 1
tlev(ncol,k) = varint(k) + (tlev(ncol,kte) - varint(kte))
tlay(ncol,k-1) = 0.5*(tlev(ncol,k) + tlev(ncol,k-1))
enddo
do k = kte+1, nlayers, 1
h2ovmr(ncol,k)   = h2ovmr(ncol,kte)
co2vmr(ncol,k)   = co2vmr(ncol,kte)
o2vmr(ncol,k)    = o2vmr(ncol,kte)
ch4vmr(ncol,k)   = ch4vmr(ncol,kte)
n2ovmr(ncol,k)   = n2ovmr(ncol,kte)
cfc11vmr(ncol,k) = cfc11vmr(ncol,kte)
cfc12vmr(ncol,k) = cfc12vmr(ncol,kte)
cfc22vmr(ncol,k) = cfc22vmr(ncol,kte)
ccl4vmr(ncol,k)  = ccl4vmr(ncol,kte)
enddo
call ozone_to_layers(nlayers,play(1,:),pin,o3clim,o3mmr)
do k = kts,nlayers
o3vmr(ncol,k) = o3mmr(k)
enddo
end subroutine upper_profiles
recursive subroutine cloud_properties()
if(inflglw .gt. 0) then
do k = kts, kte
cldfrac(ncol,k) = cldfra3d(k)
enddo
do k = kts, kte
do nb = 1, nbndlw
ws%taucld(nb,ncol,k) = 0.0
enddo
enddo
pcols = ncol
pver  = kte - kts + 1
gravmks = g
if(has_re .ne. 0) then
inflglw = 3
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
inflglw  = 4
iceflglw = 4
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
if(iceflglw .eq. 3) then
do k = kts, kte
reice(ncol,k) = reice(ncol,k) * 1.0315
reice(ncol,k) = min(140.0,reice(ncol,k))
enddo
endif
endif
if(has_re .ne. 0) then
inflglw  = 5
iceflglw = 5
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
if(iceflglw .eq. 3)then
do k = kts, kte
gicewp = (qi1d(k)+qs1d(k)) * pdel(ncol,k)*100.0 / gravmks * 1000.0
cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))
enddo
elseif(iceflglw .ge. 4) then
do k = kts, kte
gicewp = qi1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0
cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))
enddo
endif
if(iceflglw.eq.5)then
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
do k = kte+1, nlayers
clwpth(ncol,k)  = 0.
ciwpth(ncol,k)  = 0.
cswpth(ncol,k)  = 0.
rel(ncol,k)     = 10.
rei(ncol,k)     = 10.
res(ncol,k)     = 10.
cldfrac(ncol,k) = 0.
do nb = 1, nbndlw
ws%taucld(nb,ncol,k) = 0.
enddo
enddo
endif
end subroutine cloud_properties
recursive subroutine solve_and_return()
iplon = 1
irng  = 0
permuteseed = 150
call mcica_subcol_lw  (iplon   , ncol    , nlay    , icld    , permuteseed , irng    , play    ,  cldfrac , ciwpth  , clwpth  , cswpth  , rei         , rel     , res     ,  ws%taucld  , hlay    , idcor   , julday  , lat         , ws%cldfmcl , ws%ciwpmcl ,  ws%clwpmcl , ws%cswpmcl , reicmcl , relqmcl , resnmcl     , ws%taucmcl, ws=ws)
do nb = 1, nbndlw
do k = kts, nlayers
ws%tauaer(ncol,k,nb) = 0.
enddo
enddo
call rrtmg_lw  (ncol    , nlay    , icld     , play     , plev     , tlay    ,  tlev    , tsfc    , h2ovmr   , o3vmr    , co2vmr   , ch4vmr  ,  n2ovmr  , o2vmr   , cfc11vmr , cfc12vmr , cfc22vmr , ccl4vmr ,  emis    , inflglw , iceflglw , liqflglw , ws%cldfmcl  , ws%taucmcl ,  ws%ciwpmcl , ws%clwpmcl , ws%cswpmcl  , reicmcl  , relqmcl  , resnmcl ,  ws%tauaer  , uflx    , dflx     , hr       , uflxc    , dflxc   ,  hrc     , uflxcln , dflxcln  , calc_clean_atm_diag, ws=ws)
glw  = dflx(1,1)
olr  = uflx(1,nlayers+1)
lwcf = uflxc(1,nlayers+1) - uflx(1,nlayers+1)
do k = kts, kte
tten1d(k) = hr(ncol,k)/86400.
rthratenlw(k) = tten1d(k)/pi3d(k)
enddo
do k = kts, kte
if(cldfra3d(k) .gt. 0.) then
radii(k,1) = rel(ncol,k)
radii(k,2)   = rei(ncol,k)
radii(k,3)  = res(ncol,k)
endif
enddo
flux(1,1:nlayers+1)=uflx(1,1:nlayers+1)
flux(2,1:nlayers+1)=dflx(1,1:nlayers+1)
flux(3,1:nlayers+1)=uflxc(1,1:nlayers+1)
flux(4,1:nlayers+1)=dflxc(1,1:nlayers+1)
end subroutine solve_and_return
end subroutine
end module
