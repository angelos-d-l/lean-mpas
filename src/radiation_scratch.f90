! The large RRTMG work arrays, taken out of the column routines into one scratch argument.
module radiation_scratch
 use radiation_sizes, only: max_lw_layers, max_sw_layers
 use parkind, only: rb => kind_rb
 use parrrsw, only: ngptsw, nbndsw, naerec, mxmol_sw => mxmol
 use parrrtm, only: ngptlw, nbndlw, mxmol_lw => mxmol
 implicit none
 private
 public :: lw_scratch, sw_scratch
 type lw_scratch
  ! radiation_lw_column
  real :: taucld(nbndlw,1,1:max_lw_layers)
  real :: cldfmcl(ngptlw,1,1:max_lw_layers)
  real :: clwpmcl(ngptlw,1,1:max_lw_layers)
  real :: ciwpmcl(ngptlw,1,1:max_lw_layers)
  real :: cswpmcl(ngptlw,1,1:max_lw_layers)
  real :: taucmcl(ngptlw,1,1:max_lw_layers)
  real :: tauaer(1,1:max_lw_layers,nbndlw)
  ! generate_stochastic_clouds
  real(kind=rb), dimension(ngptlw, 1, max_lw_layers) :: CDF
  logical,  dimension(ngptlw, 1, max_lw_layers) :: iscloudy
  ! rtrnmc
  real(kind=rb) :: abscld(max_lw_layers,ngptlw)
  real(kind=rb) :: efclfrac(max_lw_layers,ngptlw)
  real(kind=rb) :: odcld(max_lw_layers,ngptlw)
  ! rrtmg_lw
  real(kind=rb) :: wkl(mxmol_lw,max_lw_layers+1)
  real(kind=rb) :: fracs(max_lw_layers+1,ngptlw)
  real(kind=rb) :: taug(max_lw_layers+1,ngptlw)
  real(kind=rb) :: taut(max_lw_layers+1,ngptlw)
  real(kind=rb) :: taua(max_lw_layers+1,nbndlw)
  real(kind=rb) :: planklay(max_lw_layers+1,nbndlw)
  real(kind=rb) :: planklev(0:max_lw_layers+1,nbndlw)
  real(kind=rb) :: cldfmc(ngptlw,max_lw_layers+1)
  real(kind=rb) :: ciwpmc(ngptlw,max_lw_layers+1)
  real(kind=rb) :: clwpmc(ngptlw,max_lw_layers+1)
  real(kind=rb) :: cswpmc(ngptlw,max_lw_layers+1)
  real(kind=rb) :: taucmc(ngptlw,max_lw_layers+1)
 end type
 type sw_scratch
  ! radiation_sw_column
  real,dimension(1,1:max_sw_layers,nbndsw) :: tauaer
  real,dimension(1,1:max_sw_layers,nbndsw) :: ssaaer
  real,dimension(1,1:max_sw_layers,nbndsw) :: asmaer
  real,dimension(nbndsw,1,1:max_sw_layers) :: taucld
  real,dimension(nbndsw,1,1:max_sw_layers) :: ssacld
  real,dimension(nbndsw,1,1:max_sw_layers) :: asmcld
  real,dimension(nbndsw,1,1:max_sw_layers) :: fsfcld
  real,dimension(ngptsw,1,1:max_sw_layers) :: cldfmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: clwpmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: ciwpmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: cswpmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: taucmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: ssacmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: asmcmcl
  real,dimension(ngptsw,1,1:max_sw_layers) :: fsfcmcl
  real,dimension(1,1:max_sw_layers,naerec) :: ecaer
  ! generate_stochastic_clouds_sw
  real(kind=rb), dimension(ngptsw, 1, max_sw_layers) :: CDF
  logical,  dimension(ngptsw, 1, max_sw_layers) :: isCloudy
  ! spcvmc_sw
  real(kind=rb) :: ztaug(max_sw_layers,ngptsw)
  real(kind=rb) :: ztaur(max_sw_layers,ngptsw)
  real(kind=rb) :: zcd(max_sw_layers+1,ngptsw)
  real(kind=rb) :: zcu(max_sw_layers+1,ngptsw)
  real(kind=rb) :: zfd(max_sw_layers+1,ngptsw)
  real(kind=rb) :: zfu(max_sw_layers+1,ngptsw)
  ! rrtmg_sw
  real(kind=rb) :: wkl(mxmol_sw,max_sw_layers+1)
  real(kind=rb) :: taua(max_sw_layers+1,nbndsw)
  real(kind=rb) :: ssaa(max_sw_layers+1,nbndsw)
  real(kind=rb) :: asma(max_sw_layers+1,nbndsw)
  real(kind=rb) :: cldfmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: ciwpmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: clwpmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: cswpmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: taucmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: taormc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: ssacmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: asmcmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: fsfcmc(ngptsw,max_sw_layers+1)
  real(kind=rb) :: ztaua(max_sw_layers+1,nbndsw)
  real(kind=rb) :: ztauacln(max_sw_layers+1,nbndsw)
  real(kind=rb) :: zasya(max_sw_layers+1,nbndsw)
  real(kind=rb) :: zomga(max_sw_layers+1,nbndsw)
  real(kind=rb) :: zcldfmc(max_sw_layers+1,ngptsw)
  real(kind=rb) :: ztaucmc(max_sw_layers+1,ngptsw)
  real(kind=rb) :: ztaormc(max_sw_layers+1,ngptsw)
  real(kind=rb) :: zasycmc(max_sw_layers+1,ngptsw)
  real(kind=rb) :: zomgcmc(max_sw_layers+1,ngptsw)
 end type
end module
