! HAND-WRITTEN device code: the group arrays of the g-point radiation path (radiation_fast.f90, radiation_fast_gas.f90), in a
! module of their own so that both can use them. Slot index (column in flight) first.
module radiation_fast_data
 use radiation_sizes, only: max_lw_layers, max_sw_layers
 use parrrtm, only: nbndlw, ngptlw, maxxsec, nmol_lw => nmol
 use parkind, only: rb => kind_rb
 use parrrsw, only: nbndsw, ngptsw
 implicit none
 public
 integer, parameter :: ll = max_lw_layers + 1, ls = max_sw_layers + 1
 ! the clear-sky fluxes per g-point (lw_gout 3, 4; sw_dir 2) are kept only for LEAN_BUDGET: no other output reads them
#ifdef LEAN_BUDGET
 integer, parameter :: ngout = 4
#else
 integer, parameter :: ngout = 2
#endif
 ! LW group: layer arrays of inatm/setcoef, cloud inputs, McICA's random numbers, gas optics, per-g-point fluxes, level totals
 integer, allocatable, save :: lw_nlay(:), lw_laytrop(:), lw_ji(:,:,:), lw_icld(:,:)
 integer(8), allocatable, save :: lw_mask(:,:)
 real, allocatable, save :: lw_pwvcm(:), lw_lay(:,:,:), lw_pz(:,:), lw_wx(:,:,:), lw_semiss(:,:), lw_plankbnd(:,:)
 real, allocatable, save :: lw_planklay(:,:,:), lw_planklev(:,:,:), lw_jr(:,:,:), lw_cl(:,:,:), lw_cdf(:,:,:)
 real, allocatable, save :: lw_taug(:,:,:), lw_fracs(:,:,:), lw_gout(:,:,:,:), lw_tot(:,:,:)
 !$acc declare create(lw_nlay, lw_laytrop, lw_ji, lw_icld, lw_mask, lw_pwvcm, lw_lay, lw_pz, lw_wx, lw_semiss, lw_plankbnd)
 !$acc declare create(lw_planklay, lw_planklev, lw_jr, lw_cl, lw_cdf, lw_taug, lw_fracs, lw_gout, lw_tot)
 ! SW group (day columns only)
 integer, allocatable, save :: sw_nlay(:), sw_laytrop(:), sw_ji(:,:,:)
 real, allocatable, save :: sw_prmu0(:), sw_coszrs(:), sw_alb(:), sw_adjflux(:), sw_pdp(:,:), sw_jr(:,:,:), sw_cl(:,:,:)
 ! the SW stages reuse lw_cdf, lw_taug (taug), lw_fracs (taur) and lw_gout (fluxes): the LW of a group ends before its SW starts
 real, allocatable, save :: sw_sflx(:,:), sw_zinc(:,:), sw_dir(:,:,:,:), sw_tot(:,:,:)
 !$acc declare create(sw_nlay, sw_laytrop, sw_ji, sw_prmu0, sw_coszrs, sw_alb, sw_adjflux, sw_pdp, sw_jr, sw_cl)
 !$acc declare create(sw_sflx, sw_zinc, sw_dir, sw_tot)
 ! the aerosol optical properties of the column code (tauaer 0, ssaaer 1, asmaer 0) as data, not literals: no folding
 real, save :: aer(3) = [0., 1., 0.]
 !$acc declare create(aer)
 ! the prep's double-precision layer arrays (inatm: wkl; pavel, tavel, coldry, wbrodl), one set per slot and contiguous
 ! per slot, so that they are not in the prep kernels' stack frames (the LW prep and the SW prep take turns)
 real(rb), allocatable, target, save :: rf_wk(:,:,:), rf_dv(:,:,:)
 !$acc declare create(rf_wk, rf_dv)
contains

 ! host: the group arrays for `slots` columns in flight (declare create: the card copies come with the allocation)
 subroutine rf_allocate(slots)
 integer, intent(in) :: slots
 integer :: s
 s = slots
 allocate(lw_nlay(s), lw_laytrop(s), lw_ji(s,ll,6), lw_icld(s,ll), lw_mask(s,ngptlw), lw_pwvcm(s), lw_lay(s,ll,3))
 allocate(lw_pz(s,0:ll), lw_wx(s,maxxsec,ll), lw_semiss(s,nbndlw), lw_plankbnd(s,nbndlw), lw_planklay(s,ll,nbndlw))
 allocate(lw_planklev(s,0:ll,nbndlw), lw_jr(s,ll,31), lw_cl(s,ll,7), lw_cdf(s,ll,ngptlw), lw_taug(s,ll,ngptlw))
 allocate(lw_fracs(s,ll,ngptlw), lw_gout(s,0:max_lw_layers,ngptlw,ngout), lw_tot(s,0:max_lw_layers,4))
 allocate(sw_nlay(s), sw_laytrop(s), sw_ji(s,ls,5), sw_prmu0(s), sw_coszrs(s), sw_alb(s), sw_adjflux(s), sw_pdp(s,ls))
 allocate(sw_jr(s,ls,16), sw_cl(s,ls,7), sw_sflx(s,ngptsw))
 allocate(sw_zinc(s,ngptsw), sw_dir(s,ls,ngptsw,ngout/2), sw_tot(s,ls,14))
 allocate(rf_wk(nmol_lw,ll,s), rf_dv(ll,4,s))
 !$acc update device(aer)
 end subroutine

 real function rf_bytes()
 rf_bytes = real(storage_size(lw_nlay(1)))/8.*(size(lw_nlay)+size(lw_laytrop)+size(lw_ji)+size(lw_icld)+size(sw_nlay)+size(sw_laytrop)+size(sw_ji)) &
  + 8.*size(lw_mask) + 4.*(size(lw_pwvcm)+size(lw_lay)+size(lw_pz)+size(lw_wx)+size(lw_semiss)+size(lw_plankbnd)+size(lw_planklay) &
  + size(lw_planklev)+size(lw_jr)+size(lw_cl)+size(lw_cdf)+size(lw_taug)+size(lw_fracs)+size(lw_gout)+size(lw_tot) &
  + size(sw_prmu0)+size(sw_coszrs)+size(sw_alb)+size(sw_adjflux)+size(sw_pdp)+size(sw_jr)+size(sw_cl) &
  + size(sw_sflx)+size(sw_zinc)+size(sw_dir)+size(sw_tot))
 end function

end module
