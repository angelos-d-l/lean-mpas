! HAND-WRITTEN device code (not generated; tools/gen_device_physics.py neither writes nor checks this file). Card build only.
! MYNN PBL (bl_mynn_run of src/mynn_pbl.f90 with its helpers, the wrapper mynn_pbl_state of src/mynn.f90 and add_pbl) on the
! card in stages. The generated column path (one thread per column, the column's arrays contiguous per thread, so a warp's
! loads hit 32 lines) takes about an eighth of a step of the athens_3km case (cases/athens_3km, dt 18 s, the test card). Here every
! stage array has the column index first, ms(column, level, field), so the threads of a warp read neighbouring words, and the
! work is split:
!   load      (column, level): the state's MYNN inputs transposed, the specific contents, thl, sqw; zw and zagl (each point
!             forms the column's sum in its order)
!   columns   (column): the condensation's k_tropo; GET_PBLH, SCALE_AWARE, the surface buoyancy
!   [init]    (column, first cold step only): bl_mynn_run's start values and the generated mym_initialize itself
!   cloud     (column, level): mym_condensation (bl_mynn_cloudpdf 2)
!   plumes0   (column): DMP_mf before the plumes, the column list of the plume columns; mym_length's integral (elt, vsc)
!   plumes    (plume column, plume): one plume ascent per thread, its profile kept (compact, mf_cap plume columns at a time)
!   sums      (plume column, level): DMP_mf's sums over the plumes in its order, the flux limit, edmf
!   plumes1   (plume column): DMP_mf's ktop, ztop, maxmf and the plume clouds
!   (the three stages above take under 1 % of a step of that case; all of a column's plumes in one thread, adding into the
!   sums as they rise, take about 2.7 times as long)
!   turb      (column, level): mym_level2, mym_length's level part, mym_turbulence
!   pred0     (column, level): mym_predict's tsq, qsq, cov
!   pred1     (column): mym_predict's qke solve
!   tend      (column, variable): mynn_tendencies' seven solves (u, v, thl, qc, qv, qi, ni)
!   moist     (column, level): moisture_check and Dth per point; a column in which any level fills vapour is redone top-down,
!             one thread per column (moisture), as moisture_check does
!   store     (column, level): the outputs into the state, pbl_mixing_tendencies, retrieve_exchange_coeffs, add_pbl
! Every grid point's arithmetic is the generated device code's (src/device/mynn_pbl_helpers.f90, mynn_pbl.f90, mynn.f90,
! mynn_conversions.f90, physics_tendency.f90), statement by statement and in the same order; sums keep their order. Values
! that the column code read from arrays it had zeroed (sub_*, det_*, sd_*, s_awu, s_awv, s_awqke, kzero, rstoch_col,
! TKEprodTD) are read here from zr, a zero in card memory, so that no expression is folded differently. Work the column code
! did whose results nothing read is left out: DMP_mf's environment updates (envm_*; env_subs is off), the plume winds, TKE
! and entrainment profiles, edmf_thl/edmf_ent, An2, the zeroing of outputs that are written in full later.
! Card memory: the stages run over chunks of nch columns (LEAN_MYNN_CHUNK, default 12288, equal chunks): ms (nms fields of nch x (nz+1)),
! the per-column scalars and the plume profiles of at most mf_cap plume columns at a time (LEAN_MYNN_PLUMES, default 8192, LEAN_MYNN_PLUMELEV levels, default 16;
! more plume columns are done in turns). No work-array pool.
module mynn_card
 use config, only: ncells, nc1, nz, max_levels, dt
 use dyn_config, only: num_scalars, i_qv, i_qc, i_qi, i_qs, i_ni
 use model_state, only: state_type
 use ccpp_kind_types, only: kind_phys, kind_phys8
 use lean_bl_mynn_common, only: cp, cpv, cliq, cice, ep_2, grav, gtr, karman, onethird, twothirds, p1000mb, p608, r_d, r_v, &
  rcp, rvovrd, t0c, tice, tv0, xlf, xls, xlv, xlvcp, xlscp, b1, b2
 use lean_bl_mynn_subroutines, only: phim, phih, scale_aware, mym_initialize, get_pblh
 use lean_mynn_shared, only: qsat_blend, xl_blend
 implicit none
 private
 public :: mynn_card_start, mynn_card_run, mynn_card_bytes
 ! the private parameters of lean_bl_mynn_subroutines (src/device/mynn_pbl_helpers.f90), as they are there
 real(kind_phys), parameter :: pr = 0.74, g1 = 0.235, c2 = 0.729, c3 = 0.340, c4 = 0.0, c5 = 0.2, &
   & a1 = b1*( 1.0-3.0*g1 )/6.0, c1 = g1 -1.0/( 3.0*a1*2.88449914061481660), a2 = a1*( g1-c1 )/( g1*pr ), &
   & g2 = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )
 real(kind_phys), parameter :: cc2 = 1.0-c2, cc3 = 1.0-c3, e1c = 3.0*a2*b2*cc3, e2c = 9.0*a1*a2*cc2, &
   & e3c = 9.0*a2*a2*cc2*( 1.0-c5 ), e4c = 12.0*a1*a2*cc2, e5c = 6.0*a1*a1
 real(kind_phys), parameter :: qmin=0.0, zmax=1.0, Sqfac=3.0
 real(kind_phys), parameter :: qkemin=1.e-12
 real(kind_phys), parameter :: tliq = 269.
 real(kind_phys), parameter :: CKmod=1.
 integer, parameter :: nup = 8
 ! stage fields (third index of ms); the later names reuse fields that are dead by then (see each stage)
 integer, parameter :: i_dz=1, i_u=2, i_v=3, i_th=4, i_ex=5, i_p=6, i_rho=7, i_qni=8, i_sqv=9, i_sqc=10, i_sqi=11, &
  i_thl=12, i_sqw=13, i_zw=14, i_zagl=15, i_qke=16, i_qsq=17, i_el=18, i_sh=19, i_sm=20, &
  i_qcbl=21, i_qibl=22, i_cld=23, i_vt=24, i_vq=25, i_ea=26, i_ew=27, i_saw=28, i_sawthl=29, i_sawqv=30, i_sawqc=31, &
  i_sawqni=32, i_dfm=33, i_dfh=34, i_pdk=35, i_pdt=36, i_pdq=37, i_pdc=38, i_tcd=39, nms=39
 ! (thetav is recomputed where it is read, thv_at; the snow content sqs is read from the state in cloud, sqs_at)
 ! mym_turbulence's qcd = elq*gamq and tcd = elq*gamt with gamq = gamt = 0.0: the same bits, one field (tcd) for both
 integer, parameter :: i_qcd=i_tcd
 ! mym_predict's tsq, cov (pred0 to store) in zw and zagl, dead after turb and cloud (mym_initialize's tsq, cov are not
 ! kept: mym_predict overwrites them before any read)
 integer, parameter :: i_tsq=i_zw, i_cov=i_zagl
 ! the solves' results (tend) in fields dead after pred1 / turb / cloud; moist writes dqc, dqi, dth over sqc2, sqi2, thl2 and
 ! the final qv over sqv2
 integer, parameter :: i_du=i_pdk, i_dv=i_pdt, i_thl2=i_pdq, i_sqc2=i_pdc, i_sqv2=i_vt, i_sqi2=i_vq, i_dqni=i_sqw, &
  i_dqv=i_ea, i_dqs=i_ew, i_dqc=i_sawthl, i_dqi=i_sawqv, i_dth=i_saw
 ! per-column reals (mc) and integers (mi)
 integer, parameter :: c_psbl=1, c_pssh=2, c_flt=3, c_fltv=4, c_flq=5, c_flqv=6, c_rmol=7, c_pmz=8, c_phh=9, c_thsfc=10, &
  c_qv1=11, c_elt=12, c_vsc=13, c_fltv2=14, c_minw=15, c_psw=16, c_wspd=17, nmc=17
 integer, parameter :: m_ktropo=1, m_mf=2, m_moist=3, m_deep=4, nmi=4
 ! the list of the plume columns (mf_col(1:mf_n)); the plume profiles (slot, level, plume, field) of mf_cap of them at a time,
 ! per (slot, plume) the last level with values (plev), the plume area (pa1) and DMP_mf's nup2 = 0 (pzero)
 integer, parameter :: p_w=1, p_thl=2, p_qt=3, p_qc=4, p_qni=5, npf=5
 real, allocatable, save :: ms(:,:,:), mc(:,:), pp(:,:,:,:), pa1(:,:)
 integer, allocatable, save :: mi(:,:), mf_col(:), plev(:,:), pzero(:,:)
 integer, save :: mf_n(1) = 0, ldc = 0, mf_cap = 0, nch = 0, lplume = 0
 ! thread order measured on the athens_3km case, the test card: load tiled (kept), level-fast 1.3 times as long; store
 ! level-fast (kept), tiled 1.3 times as long, column-fast 24 times as long
 real, save :: zr = 0.
 !$acc declare create(ms, mc, mi, mf_col, mf_n, zr, pp, pa1, plev, pzero, nch, lplume)
contains

 ! ---- memory ------------------------------------------------------------------------------------------------------------------
 subroutine mynn_card_start()
 character(32) :: sv
 integer :: n
 if (allocated(ms)) return
 ! LEAN_MYNN_CHUNK: at most that many columns a chunk; the columns go in the fewest such chunks of equal size (a multiple of
 ! 32), so that no small last chunk runs a whole pass (the 46,251 columns of athens_3km with 12,288: 4 x 11,584)
 nch = 12288
 call get_environment_variable('LEAN_MYNN_CHUNK', sv, status=n)
 if (n == 0) read(sv, *, iostat=n) nch
 nch = max(128, min(nch, ncells))
 n = (ncells + nch - 1)/nch
 nch = (((ncells + n - 1)/n + 31)/32)*32
 ldc = ((nch + 31)/32)*32
 mf_cap = 8192
 call get_environment_variable('LEAN_MYNN_PLUMES', sv, status=n)
 if (n == 0) read(sv, *, iostat=n) mf_cap
 mf_cap = max(128, min(mf_cap, ldc))
 allocate(ms(ldc, nz+1, nms), mc(ldc, nmc), mi(ldc, nmi), mf_col(ldc))
 ! the plume profiles' levels (LEAN_MYNN_PLUMELEV, default 16): a column with a plume above them goes to plumes_column
 lplume = 16
 call get_environment_variable('LEAN_MYNN_PLUMELEV', sv, status=n)
 if (n == 0) read(sv, *, iostat=n) lplume
 lplume = max(2, min(lplume, nz-1))
 allocate(pp(mf_cap, lplume, nup, npf), pa1(mf_cap, nup), plev(mf_cap, nup), pzero(mf_cap, nup))
 zr = 0.
 !$acc update device(zr, nch, lplume)
 print '(a,f7.1,a,i0,a,i0,a,i0)', ' MYNN on the card in stages: ', mynn_card_bytes()/2.**20, ' MiB, plume columns at a time ', mf_cap, ', plume levels ', lplume, ', columns a chunk ', nch
 end subroutine

 real function mynn_card_bytes()
 mynn_card_bytes = 4.*(real(size(ms)) + size(mc) + size(mi) + size(mf_col) + size(pp) + size(pa1) + size(plev) + size(pzero))
 end function

 ! ---- the stages ----------------------------------------------------------------------------------------------------------------
 ! initflag: bl_mynn_run's (the first step of a cold start). mass, tend_th, tend_scalars: add_pbl's accumulators.
 subroutine mynn_card_run(initflag, state, mass, tend_th, tend_scalars)
 integer, intent(in) :: initflag
 type(state_type), intent(inout) :: state
 real, intent(in) :: mass(nz,nc1)
 real, intent(inout) :: tend_th(nz,nc1), tend_scalars(num_scalars,nz,nc1)
 integer :: c, k, j, v, i, off, nb, nchk, coff
 logical :: init
 init = initflag > 0
 ! the columns in chunks of nch (LEAN_MYNN_CHUNK): the stage arrays hold one chunk (the column c is row mod(c-1,nch)+1).
 ! Every kernel on queue 1, one wait at the end: no host round trip between the kernels (the plume batches are bounded on
 ! the card by mf_n).
 do coff = 0, ncells-1, nch
  nchk = min(nch, ncells-coff)
  !$acc parallel loop tile(8,4) default(present) async(1)
  do c = coff+1, coff+nchk
   do k = 1, nz
    call load_point(state, c, k)
   end do
  end do
  !$acc parallel loop gang vector default(present) async(1)
  do c = coff+1, coff+nchk
   call column_start(state, c, init)
  end do
  if (init) then
   !$acc parallel loop gang vector default(present) async(1)
   do c = coff+1, coff+nchk
    call init_column(state, c)
   end do
   !$acc parallel loop gang vector default(present) async(1)
   do c = coff+1, coff+nchk
    call column_scalars(state, c)
   end do
  end if
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do k = 1, nz
   do c = coff+1, coff+nchk
    call cloud_point(state, c, k)
   end do
  end do
  !$acc serial default(present) async(1)
  mf_n(1) = 0
  !$acc end serial
  !$acc parallel loop gang vector default(present) async(1)
  do c = coff+1, coff+nchk
   call plumes_start(state, c)
  end do
  ! one thread per plume, mf_cap plume columns at a time
  do off = 0, nchk - 1, mf_cap
   nb = min(mf_cap, nchk - off)
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do i = 1, nup
    do j = 1, nb
     if (off+j <= mf_n(1)) call plume_ascent(state, j, mf_col(off+j), i)
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do k = 1, nz+1
    do j = 1, nb
     if (off+j <= mf_n(1)) call plume_sums(j, mf_col(off+j), k)
    end do
   end do
   !$acc parallel loop gang vector default(present) async(1)
   do j = 1, nb
    if (off+j <= mf_n(1)) call plumes_close(state, j, mf_col(off+j))
   end do
   !$acc parallel loop gang vector default(present) async(1)
   do j = 1, nb
    if (off+j <= mf_n(1)) then
     if (mi(mod(mf_col(off+j)-1,nch)+1,m_deep) /= 0) call plumes_column(state, mf_col(off+j))
    end if
   end do
  end do
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do k = 1, nz
   do c = coff+1, coff+nchk
    call turbulence_point(state, c, k)
   end do
  end do
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do k = 1, nz
   do c = coff+1, coff+nchk
    call predict_point(c, k)
   end do
  end do
  !$acc parallel loop gang vector default(present) async(1)
  do c = coff+1, coff+nchk
   call predict_solve(state, c)
  end do
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do v = 1, 7
   do c = coff+1, coff+nchk
    call tendency_solve(state, c, v)
   end do
  end do
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do k = 1, nz
   do c = coff+1, coff+nchk
    call moisture_point(state, c, k)
   end do
  end do
  !$acc parallel loop gang vector default(present) async(1)
  do c = coff+1, coff+nchk
   if (mi(mod(c-1,nch)+1,m_moist) /= 0) call moisture(state, c)
  end do
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = coff+1, coff+nchk
   do k = 1, nz
    call store_point(state, c, k, init, mass, tend_th, tend_scalars)
   end do
  end do
 end do
 !$acc wait(1)
 end subroutine

 ! ---- load: mynn_pbl_state's t and pbl_specific_contents, bl_mynn_run's prepare_column point work ---------------------------
 subroutine load_point(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 real :: sqv, sqc, sqi
 real(kind_phys) :: zw, zagl, dzm1
 integer :: j
 ms(mod(c-1,nch)+1,k,i_dz) = state%dz(k,c)
 ms(mod(c-1,nch)+1,k,i_u) = state%u(k,c)
 ms(mod(c-1,nch)+1,k,i_v) = state%v(k,c)
 ms(mod(c-1,nch)+1,k,i_th) = state%theta(k,c)
 ms(mod(c-1,nch)+1,k,i_ex) = state%exner(k,c)
 ms(mod(c-1,nch)+1,k,i_p) = state%p_hyd(k,c)
 ms(mod(c-1,nch)+1,k,i_rho) = state%rho_moist(k,c)
 ms(mod(c-1,nch)+1,k,i_qni) = state%ni(k,c)
 sqv=state%qv(k,c)/(1.+state%qv(k,c))
 sqc=state%qc(k,c)/(1.+state%qv(k,c))
 sqi=state%qi(k,c)/(1.+state%qv(k,c))
 ms(mod(c-1,nch)+1,k,i_sqv) = sqv
 ms(mod(c-1,nch)+1,k,i_sqc) = sqc
 ms(mod(c-1,nch)+1,k,i_sqi) = sqi
 ms(mod(c-1,nch)+1,k,i_sqw) = sqv+sqc+sqi
 ms(mod(c-1,nch)+1,k,i_thl) = state%theta(k,c) - xlvcp/state%exner(k,c)*sqc - xlscp/state%exner(k,c)*(sqi)
 ms(mod(c-1,nch)+1,k,i_qke) = state%qke(k,c)
 ms(mod(c-1,nch)+1,k,i_qsq) = state%qsq(k,c)
 ! bl_mynn_run's zw and mym_condensation's zagl at k: each point forms the column's sum in its order
 zw = 0._kind_phys
 do j = 1, k-1
  zw = zw + state%dz(j,c)
 end do
 ms(mod(c-1,nch)+1,k,i_zw) = zw
 if (k == nz) ms(mod(c-1,nch)+1,k+1,i_zw) = zw + state%dz(k,c)
 if (k <= nz-1) then
  zagl = 0.
  dzm1 = 0.
  do j = 1, k
   zagl = zagl + 0.5*(state%dz(j,c) + dzm1)
   dzm1 = state%dz(j,c)
  end do
  ms(mod(c-1,nch)+1,k,i_zagl) = zagl
 end if
 end subroutine

 ! bl_mynn_run's thetav at level k, as prepare_column computes it
 real(kind_phys) function thv_at(c, k)
 !$acc routine seq
 integer, intent(in) :: c, k
 thv_at = ms(mod(c-1,nch)+1,k,i_th)*(1.+0.608*ms(mod(c-1,nch)+1,k,i_sqv))
 end function

 ! mynn_pbl_state's sqs (pbl_specific_contents) at level k
 real(kind_phys) function sqs_at(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
#ifdef LEAN_MYNN_MOIST
 sqs_at=state%qs(k,c)/(1.+state%qv(k,c))
#else
 sqs_at=state%qs(k,c)/(1.+state%qs(k,c))
#endif
 end function

 ! ---- columns: zw, zagl, k_tropo (mym_condensation), and without init the scalars ---------------------------------------------
 subroutine column_start(state, c, init)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c
 logical, intent(in) :: init
 integer :: k
 real(kind_phys) :: zagl, dzm1, theta1, theta2, ht1, ht2
 do k = nz-3, 1, -1
  theta1 = ms(mod(c-1,nch)+1,k,i_th)
  theta2 = ms(mod(c-1,nch)+1,k+2,i_th)
  ht1 = 44307.692 * (1.0 - (ms(mod(c-1,nch)+1,k,i_p)/101325.)**0.190)
  ht2 = 44307.692 * (1.0 - (ms(mod(c-1,nch)+1,k+2,i_p)/101325.)**0.190)
  if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND. (ht1.lt.19000.) .and. (ht1.gt.4000.) ) then
   exit
  endif
 enddo
 mi(mod(c-1,nch)+1,m_ktropo) = MAX(1+2, k+2)
 if (.not. init) call column_scalars(state, c)
 end subroutine

 ! GET_PBLH on the stage arrays (src/device/mynn_pbl_helpers.f90)
 subroutine pblh_column(c, landsea, zi, kzi)
 !$acc routine seq
 integer, intent(in) :: c
 real(kind_phys), intent(in) :: landsea
 real(kind_phys), intent(out) :: zi
 integer, intent(out) :: kzi
 real(kind_phys)::PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
 real(kind_phys)::delt_thv
 real(kind_phys), parameter ::sbl_lim = 200.
 real(kind_phys), parameter ::sbl_damp = 400.
 integer ::K,kthv,ktke
 kzi = 2
 k = 1+1
 kthv = 1
 minthv = 9.E9
 DO WHILE (ms(mod(c-1,nch)+1,k,i_zw) .LE. 200.)
  IF (minthv > thv_at(c,k)) then
   minthv = thv_at(c,k)
   kthv = k
  endif
  k = k+1
 ENDDO
 zi=0.
 k = kthv+1
 IF((landsea-1.5).GE.0)THEN
  delt_thv = 1.0
 else
  delt_thv = 1.25
 endif
 zi=0.
 k = kthv+1
 DO k=1+1,nz-1
  IF (thv_at(c,k) .GE. (minthv + delt_thv))THEN
   zi = ms(mod(c-1,nch)+1,k,i_zw) - ms(mod(c-1,nch)+1,k-1,i_dz)* MIN((thv_at(c,k)-(minthv + delt_thv))/ MAX(thv_at(c,k)-thv_at(c,k-1),1E-6),1.0)
  endif
  IF (k .EQ. nz-1) zi = ms(mod(c-1,nch)+1,1+1,i_zw)
  IF (zi .NE. 0.0) exit
 ENDDO
 ktke = 1
 maxqke = MAX(ms(mod(c-1,nch)+1,1,i_qke),0.)
 TKEeps = maxqke/40.
 TKEeps = MAX(TKEeps,0.02)
 PBLH_TKE=0.
 k = ktke+1
 DO k=1+1,nz-1
  qtke =MAX(ms(mod(c-1,nch)+1,k,i_qke)/2.,0.)
  qtkem1=MAX(ms(mod(c-1,nch)+1,k-1,i_qke)/2.,0.)
  IF (qtke .LE. TKEeps) THEN
   PBLH_TKE = ms(mod(c-1,nch)+1,k,i_zw) - ms(mod(c-1,nch)+1,k-1,i_dz)* MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
   PBLH_TKE = MAX(PBLH_TKE,ms(mod(c-1,nch)+1,1+1,i_zw))
  endif
  IF (k .EQ. nz-1) PBLH_TKE = ms(mod(c-1,nch)+1,1+1,i_zw)
  IF (PBLH_TKE .NE. 0.) exit
 ENDDO
 PBLH_TKE = MIN(PBLH_TKE,zi+350.)
 PBLH_TKE = MAX(PBLH_TKE,MAX(zi-350.,10.))
 wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
 IF (maxqke <= 0.05) THEN
 else
  zi=PBLH_TKE*(1.-wt) + zi*wt
 endif
 DO k=1+1,nz-1
  IF ( ms(mod(c-1,nch)+1,k,i_zw) >= zi) THEN
   kzi = k-1
   exit
  endif
 ENDDO
 end subroutine

 ! bl_mynn_run surface_buoyancy: GET_PBLH, SCALE_AWARE and the surface fluxes of the column
 subroutine column_scalars(state, c)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c
 real(kind_phys) :: cpm,flq,flqc,flqv,flt,fltv,phh,pmz,psig_bl,psig_shcu,phi_m,th_sfc,zet,rmol1,qv1,pblh
 integer :: kpbl
 call pblh_column(c, state%xland(c), pblh, kpbl)
 state%pblh(c) = pblh
 state%kpbl(c) = kpbl
 call scale_aware(state%dx(c),pblh,psig_bl,psig_shcu)
 qv1 = ms(mod(c-1,nch)+1,1,i_sqv)/(1.-ms(mod(c-1,nch)+1,1,i_sqv))
 cpm = cp*(1.+0.84*qv1)
 flqv = state%qfx(c)/ms(mod(c-1,nch)+1,1,i_rho)
 flqc = 0.0
 th_sfc = state%tsk(c)/ms(mod(c-1,nch)+1,1,i_ex)
 flq = flqv + flqc
 flt = state%hfx(c)/(ms(mod(c-1,nch)+1,1,i_rho)*cpm ) - xlvcp*flqc/ms(mod(c-1,nch)+1,1,i_ex)
 fltv = flt + flqv*p608*th_sfc
 rmol1 = -karman*gtr*fltv/max(state%ust(c)**3,1.0e-6)
 zet = 0.5*ms(mod(c-1,nch)+1,1,i_dz)*rmol1
 zet = max(zet, -20.)
 zet = min(zet, 20.)
 phi_m = phim(zet)
 pmz = phi_m - zet
 phh = phih(zet)
 mc(mod(c-1,nch)+1,c_psbl) = psig_bl
 mc(mod(c-1,nch)+1,c_pssh) = psig_shcu
 mc(mod(c-1,nch)+1,c_flt) = flt
 mc(mod(c-1,nch)+1,c_fltv) = fltv
 mc(mod(c-1,nch)+1,c_flq) = flq
 mc(mod(c-1,nch)+1,c_flqv) = flqv
 mc(mod(c-1,nch)+1,c_rmol) = rmol1
 mc(mod(c-1,nch)+1,c_pmz) = pmz
 mc(mod(c-1,nch)+1,c_phh) = phh
 mc(mod(c-1,nch)+1,c_thsfc) = th_sfc
 mc(mod(c-1,nch)+1,c_qv1) = qv1
 end subroutine

 ! ---- init: bl_mynn_run initialize_turbulence, the generated mym_initialize on contiguous copies of the column -----------------
 subroutine init_column(state, c)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c
 real(kind_phys) :: dz(max_levels), zw(max_levels+1), u(max_levels), v(max_levels), thl(max_levels), qw(max_levels)
 real(kind_phys) :: th(max_levels), zz(max_levels), sh(max_levels), sm(max_levels), el(max_levels), qke(max_levels)
 real(kind_phys) :: tsq(max_levels), qsq(max_levels), cov(max_levels)
 real(kind_phys) :: pblh, psig_bl, psig_shcu, ust
 integer :: k, kpbl
 ust = state%ust(c)
 do k = 1, nz
  dz(k) = ms(mod(c-1,nch)+1,k,i_dz); u(k) = ms(mod(c-1,nch)+1,k,i_u); v(k) = ms(mod(c-1,nch)+1,k,i_v); thl(k) = ms(mod(c-1,nch)+1,k,i_thl); qw(k) = ms(mod(c-1,nch)+1,k,i_sqv)
  th(k) = thv_at(c,k); zz(k) = zr
 end do
 do k = 1, nz+1
  zw(k) = ms(mod(c-1,nch)+1,k,i_zw)
 end do
 do k = 1, nz
  sh(k) = 0._kind_phys
  sm(k) = 0._kind_phys
  el(k) = 0._kind_phys
  tsq(k) = 0._kind_phys
  qsq(k) = 0._kind_phys
  cov(k) = 0._kind_phys
  ms(mod(c-1,nch)+1,k,i_cld) = 0._kind_phys
  ms(mod(c-1,nch)+1,k,i_qcbl) = 0._kind_phys
  ms(mod(c-1,nch)+1,k,i_qibl) = 0._kind_phys
  qke(k) = 0._kind_phys
 enddo
 do k = 1, nz
  qke(k)=5.*ust*max((ust*700.-zw(k))/(max(ust,0.01)*700.),0.01)
 enddo
 ! get_pblh reads thetav (th holds it here); mym_initialize's theta and thetav are not read with bl_mynn_mixlength 2
 call get_pblh(pblh,th(1:nz),qke(1:nz),zw(1:nz+1),dz(1:nz),state%xland(c),kpbl)
 call scale_aware(state%dx(c),pblh,psig_bl,psig_shcu)
 call mym_initialize(state%xland(c),dz(1:nz),state%dx(c),zw(1:nz+1),u(1:nz),v(1:nz),thl(1:nz),qw(1:nz),pblh,th(1:nz), &
  th(1:nz),sh(1:nz),sm(1:nz),ust,state%rmol(c),el(1:nz),qke(1:nz),tsq(1:nz),qsq(1:nz),cov(1:nz),psig_bl,zz(1:nz),zz(1:nz), &
  zz(1:nz),.true.)
 do k = 1, nz
  ms(mod(c-1,nch)+1,k,i_sh) = sh(k); ms(mod(c-1,nch)+1,k,i_sm) = sm(k); ms(mod(c-1,nch)+1,k,i_el) = el(k); ms(mod(c-1,nch)+1,k,i_qke) = qke(k)
  ms(mod(c-1,nch)+1,k,i_qsq) = qsq(k)
 end do
 end subroutine

 ! ---- cloud: mym_condensation, bl_mynn_cloudpdf 2, at level k (kte takes vt, vq of kte-1) -----------------------------------
 subroutine cloud_point(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 real(kind_phys) :: qcbl, qibl, cld, vt, vq
 if (k < nz) then
  call condensation_level(state, c, k, qcbl, qibl, cld, vt, vq)
  ms(mod(c-1,nch)+1,k,i_qcbl) = qcbl
  ms(mod(c-1,nch)+1,k,i_qibl) = qibl
  ms(mod(c-1,nch)+1,k,i_cld) = cld
 else
  call condensation_level(state, c, nz-1, qcbl, qibl, cld, vt, vq)
  ms(mod(c-1,nch)+1,k,i_qcbl) = 0.
  ms(mod(c-1,nch)+1,k,i_qibl) = 0.
  ms(mod(c-1,nch)+1,k,i_cld) = 0.
 end if
 ms(mod(c-1,nch)+1,k,i_vt) = vt
 ms(mod(c-1,nch)+1,k,i_vq) = vq
 end subroutine

 subroutine condensation_level(state, c, k, qc_bl1D, qi_bl1D, cldfra_bl1D, vt, vq)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 real(kind_phys), intent(out) :: qc_bl1D, qi_bl1D, cldfra_bl1D, vt, vq
 integer,parameter ::spp_pbl=0
 real(kind_phys) :: alp, a, bet, b, q1, RH, sgm
 real(kind_phys)::dqsl,q1k,qt,t,xl,rsl,cpm,Fng,qww,alpha,beta,bb,wt,wt2,qpct, &
   & cld_factor,fac_damp,ql_ice,ql_water,qmq,qsat_tk,q1_rh,rh_hack,zsl,maxqc
 real(kind_phys), parameter ::qpct_sfc=0.025
 real(kind_phys), parameter ::qpct_pbl=0.030
 real(kind_phys), parameter ::qpct_trp=0.040
 real(kind_phys), parameter ::rhcrit =0.83
 real(kind_phys), parameter ::rhmax =1.02
 real(kind_phys)::liq_frac
 real(kind_phys)::r3sq
 real(kind_phys) ::zagl,PBLH2,xland
 real(kind_phys) ::cfmax
 real(kind_phys) ::qw_pert
 integer :: k_tropo
 xland = state%xland(c)
 pblh2=MAX(10._kind_phys,state%pblh(c))
 k_tropo = mi(mod(c-1,nch)+1,m_ktropo)
 zagl = ms(mod(c-1,nch)+1,k,i_zagl)
 t = ms(mod(c-1,nch)+1,k,i_th)*ms(mod(c-1,nch)+1,k,i_ex)
 xl = xl_blend(t,t0c,tice,cice,cliq,cpv,xls,xlv)
 qsat_tk= qsat_blend(t,t0c,tice,ms(mod(c-1,nch)+1,k,i_p))
 rh = MAX(MIN(rhmax, ms(mod(c-1,nch)+1,k,i_sqw)/MAX(1.E-10,qsat_tk)),0.001_kind_phys)
 dqsl = qsat_tk*ep_2*xlv/( r_d*t**2 )
 alp = 1.0/( 1.0+dqsl*xlvcp )
 bet = dqsl*ms(mod(c-1,nch)+1,k,i_ex)
 rsl = xl*qsat_tk / (r_v*t**2)
 cpm = cp + ms(mod(c-1,nch)+1,k,i_sqw)*cpv
 a = 1./(1. + xl*rsl/cpm)
 b = a*rsl
 qw_pert= ms(mod(c-1,nch)+1,k,i_sqw) + ms(mod(c-1,nch)+1,k,i_sqw)*0.5*zr*real(spp_pbl)
 qmq = qw_pert - qsat_tk
 r3sq = max( ms(mod(c-1,nch)+1,k,i_qsq), 0.0 )
 sgm = SQRT( r3sq )
 sgm = min( sgm, qsat_tk*0.666 )
 wt = max(500. - max(ms(mod(c-1,nch)+1,k,i_dz)-100.,0.0), 0.0_kind_phys)/500.
 sgm = sgm + sgm*0.2*(1.0-wt)
 qpct = qpct_pbl*wt + qpct_trp*(1.0-wt)
 qpct = min(qpct, max(qpct_sfc, qpct_pbl*zagl/500.) )
 sgm = max( sgm, qsat_tk*qpct )
 q1 = qmq / sgm
 rh_hack= rh
 wt2 = min(max( zagl - pblh2, 0.0 )/300., 1.0)
 if ((ms(mod(c-1,nch)+1,k,i_sqi)+sqs_at(state,c,k))>1.e-9 .and. (zagl .gt. pblh2)) then
  rh_hack =min(rhmax, rhcrit + wt2*0.045*(9.0 + log10(ms(mod(c-1,nch)+1,k,i_sqi)+sqs_at(state,c,k))))
  rh =max(rh, rh_hack)
  q1_rh =-3. + 3.*(rh-rhcrit)/(1.-rhcrit)
  q1 =max(q1_rh, q1 )
 endif
 if (ms(mod(c-1,nch)+1,k,i_sqc)>1.e-6 .and. (zagl .gt. pblh2)) then
  rh_hack =min(rhmax, rhcrit + wt2*0.08*(6.0 + log10(ms(mod(c-1,nch)+1,k,i_sqc))))
  rh =max(rh, rh_hack)
  q1_rh =-3. + 3.*(rh-rhcrit)/(1.-rhcrit)
  q1 =max(q1_rh, q1 )
 endif
 q1k = q1
 cldfra_bl1D = max(0., min(1., 0.5+0.36*atan(1.8*(q1+0.2))))
 maxqc = max(ms(mod(c-1,nch)+1,k,i_sqw) - qsat_tk, 0.0)
 if (q1k < 0.) then
  ql_water = sgm*exp(1.2*q1k-1.)
  ql_ice = sgm*exp(1.2*q1k-1.)
 elseif (q1k > 2.) then
  ql_water = min(sgm*q1k, maxqc)
  ql_ice = sgm*q1k
 else
  ql_water = min(sgm*(exp(-1.) + 0.66*q1k + 0.086*q1k**2), maxqc)
  ql_ice = sgm*(exp(-1.) + 0.66*q1k + 0.086*q1k**2)
 endif
 if (cldfra_bl1D < 0.001) then
  ql_ice = 0.0
  ql_water = 0.0
  cldfra_bl1D = 0.0
 endif
 liq_frac = MIN(1.0, MAX(0.0, (t-tice)/(tliq-tice)))
 qc_bl1D = liq_frac*ql_water
 qi_bl1D = (1.0-liq_frac)*ql_ice
 if (k .ge. k_tropo) then
  cldfra_bl1D = 0.
  qc_bl1D = 0.
  qi_bl1D = 0.
 endif
 if ((xland-1.5).GE.0) then
  q1k=max(Q1,-2.5)
 else
  q1k=max(Q1,-2.0)
 endif
 if (q1k .ge. 1.0) then
  Fng = 1.0
 elseif (q1k .ge. -1.7 .and. q1k .lt. 1.0) then
  Fng = exp(-0.4*(q1k-1.0))
 elseif (q1k .ge. -2.5 .and. q1k .lt. -1.7) then
  Fng = 3.0 + exp(-3.8*(q1k+1.7))
 else
  Fng = min(23.9 + exp(-1.6*(q1k+2.5)), 60._kind_phys)
 endif
 cfmax = min(cldfra_bl1D, 0.6_kind_phys)
 zsl = min(max(25., 0.1*pblh2), 100.)
 wt = min(zagl/zsl, 1.0)
 cfmax = cfmax*wt
 bb = b*t/ms(mod(c-1,nch)+1,k,i_th)
 qww = 1.+0.61*ms(mod(c-1,nch)+1,k,i_sqw)
 alpha = 0.61*ms(mod(c-1,nch)+1,k,i_th)
 beta = (ms(mod(c-1,nch)+1,k,i_th)/t)*(xl/cp) - 1.61*ms(mod(c-1,nch)+1,k,i_th)
 vt = qww - cfmax*beta*bb*Fng - 1.
 vq = alpha + cfmax*beta*a*Fng - tv0
 fac_damp = min(zagl * 0.0025, 1.0)
 cld_factor = 1.0 + fac_damp*min((max(0.0, ( RH - 0.92 )) / 0.145)**2, 0.37)
 cldfra_bl1D = min( 1., cld_factor*cldfra_bl1D )
 end subroutine

 ! ---- plumes0: DMP_mf up to the plumes; mym_length's integral of the column (elt, vsc) ----------------------------------------
 subroutine plumes_start(state, c)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c
 integer :: k, k50, j
 real(kind_phys)::fltv2,Psig_w,maxw,wpbl,qc_sgs,cloud_base,hux,tvs,maxwidth,minwidth,wspd_pbl,width_flx,pblh,fltv,landsea,dx
 logical :: superadiabatic
 real(kind_phys), parameter ::lmax = 1000.
 real(kind_phys), parameter ::lmin = 300.
 real(kind_phys), parameter ::dlmin = 0.
 real(kind_phys), parameter ::dcut = 1.2
 real(kind_phys) :: zi2, h1, elt, vsc, PBLH_PLUS_ENT, zwk, dzk, qdz, afk, abk, qkw, alp1, vflx
 pblh = state%pblh(c)
 fltv = mc(mod(c-1,nch)+1,c_fltv)
 landsea = state%xland(c)
 dx = state%dx(c)
 maxw = 0.0
 cloud_base = 9000.0
 do k=1,nz-1
  if (ms(mod(c-1,nch)+1,k,i_zw) > pblh + 500.) exit
  wpbl = state%w(k,c)
  if (state%w(k,c) < 0.)wpbl = 2.*state%w(k,c)
  maxw = max(maxw,abs(wpbl))
  if (ms(mod(c-1,nch)+1,k,i_zw)<=50.)k50=k
  qc_sgs = max(ms(mod(c-1,nch)+1,k,i_sqc), ms(mod(c-1,nch)+1,k,i_qcbl))
  if (qc_sgs> 1E-5 .and. (ms(mod(c-1,nch)+1,k,i_cld) .ge. 0.5) .and. cloud_base == 9000.0) then
   cloud_base = 0.5*(ms(mod(c-1,nch)+1,k,i_zw)+ms(mod(c-1,nch)+1,k+1,i_zw))
  endif
 enddo
 maxw = max(0.,maxw - 1.0)
 Psig_w = max(0.0, 1.0 - maxw)
 Psig_w = min(Psig_w, mc(mod(c-1,nch)+1,c_pssh))
 fltv2 = fltv
 if(Psig_w == 0.0 .and. fltv > 0.0) fltv2 = -1.*fltv
 superadiabatic = .false.
 if ((landsea-1.5).ge.0) then
  hux = -0.001
 else
  hux = -0.005
 endif
 tvs = mc(mod(c-1,nch)+1,c_thsfc)*(1.0+p608*ms(mod(c-1,nch)+1,1,i_sqv))
 do k=1,max(1,k50-1)
  if (k == 1) then
   if ((thv_at(c,k)-tvs)/(0.5*ms(mod(c-1,nch)+1,k,i_dz)) < hux) then
    superadiabatic = .true.
   else
    superadiabatic = .false.
    exit
   endif
  else
   if ((thv_at(c,k)-thv_at(c,k-1))/(0.5*(ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz))) < hux) then
    superadiabatic = .true.
   else
    superadiabatic = .false.
    exit
   endif
  endif
 enddo
 maxwidth = min(dx*dcut, lmax)
 maxwidth = min(maxwidth, 1.1_kind_phys*PBLH)
 if ((landsea-1.5) .lt. 0) then
  maxwidth = MIN(maxwidth, 0.5_kind_phys*cloud_base)
 else
  maxwidth = MIN(maxwidth, 0.9_kind_phys*cloud_base)
 endif
 wspd_pbl=SQRT(MAX(ms(mod(c-1,nch)+1,1,i_u)**2 + ms(mod(c-1,nch)+1,1,i_v)**2, 0.01_kind_phys))
 if ((landsea-1.5).LT.0) then
  width_flx = MAX(MIN(1000.*(0.6*tanh((fltv - 0.040)/0.04) + .5),1000._kind_phys), 0._kind_phys)
 else
  width_flx = MAX(MIN(1000.*(0.6*tanh((fltv - 0.007)/0.02) + .5),1000._kind_phys), 0._kind_phys)
 endif
 maxwidth = MIN(maxwidth, width_flx)
 minwidth = lmin
 if (maxwidth .ge. (lmax - 1.0) .and. fltv .gt. 0.2)minwidth = lmin + dlmin*min((fltv-0.2)/0.3, 1._kind_phys)
 if (maxwidth .le. minwidth) then
  maxwidth = 0.0
 endif
 state%maxwidth(c) = maxwidth
 state%ktop_plume(c) = 0
 state%ztop_plume(c) = 0.0
 state%maxmf(c) = 0.0
 mi(mod(c-1,nch)+1,m_mf) = 0
 mi(mod(c-1,nch)+1,m_moist) = 0
 mi(mod(c-1,nch)+1,m_deep) = 0
 if ( fltv2 > 0.002 .AND. (maxwidth > minwidth) .AND. superadiabatic) then
  mc(mod(c-1,nch)+1,c_fltv2) = fltv2
  mc(mod(c-1,nch)+1,c_minw) = minwidth
  mc(mod(c-1,nch)+1,c_psw) = Psig_w
  mc(mod(c-1,nch)+1,c_wspd) = wspd_pbl
  !$acc atomic capture
  mf_n(1) = mf_n(1) + 1
  j = mf_n(1)
  !$acc end atomic
  mf_col(j) = c
 end if
 ! mym_length (bl_mynn_mixlength 2): the integral up to PBLH_PLUS_ENT
 alp1 = 0.22
 zi2=MAX(pblh, 300.)
 h1=MAX(0.3*zi2,300.)
 h1=MIN(h1,600.)
 elt = 1.0e-5
 vsc = 1.0e-5
 PBLH_PLUS_ENT = MAX(pblh+h1, 100.)
 k = 1+1
 zwk = ms(mod(c-1,nch)+1,k,i_zw)
 DO WHILE (zwk .LE. PBLH_PLUS_ENT)
  dzk = 0.5*( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
  afk = ms(mod(c-1,nch)+1,k,i_dz)/( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
  abk = 1.0 -afk
  qkw = SQRT(MAX(ms(mod(c-1,nch)+1,k,i_qke)*abk+ms(mod(c-1,nch)+1,k-1,i_qke)*afk,1.0e-3))
  qdz = min(max( qkw-qmin, 0.03 ), 30.0)*dzk
  elt = elt +qdz*zwk
  vsc = vsc +qdz
  k = k+1
  zwk = ms(mod(c-1,nch)+1,k,i_zw)
 END DO
 elt = MIN( MAX(alp1*elt/vsc, 10.), 400.)
 vflx = fltv
 vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**onethird
 mc(mod(c-1,nch)+1,c_elt) = elt
 mc(mod(c-1,nch)+1,c_vsc) = vsc
 end subroutine

 ! condensation_edmf of src/device/mynn_pbl_helpers.f90
 subroutine condensation_edmf(qt,thl,p,zagl,thv,qc)
 !$acc routine seq
 real(kind_phys),intent(in) ::QT,THL,P,zagl
 real(kind_phys),intent(out) ::THV
 real(kind_phys),intent(inout)::QC
 integer ::niter,i
 real(kind_phys)::diff,exn,t,qs,qcold
 niter=50
 diff=1.e-6
 EXN=(P/p1000mb)**rcp
 do i=1,NITER
  T=EXN*THL + xlvcp*QC
  QS=qsat_blend(T,t0c,tice,P)
  QCOLD=QC
  QC=0.5*QC + 0.5*MAX((QT-QS),0.)
  if (abs(QC-QCOLD)<Diff) exit
 enddo
 T=EXN*THL + xlvcp*QC
 QS=qsat_blend(T,t0c,tice,P)
 QC=max(QT-QS,0.)
 if(zagl < 100.)QC=0.
 THV=(THL+xlvcp*QC)*(1.+QT*(rvovrd-1.)-rvovrd*QC)
 end subroutine

 ! ---- deep plumes: DMP_mf from the plumes on for a plume column with a plume above the profiles' lplume levels: its eight
 ! plumes in turn in one thread, then what follows the plume block (the same results as plume_ascent, plume_sums, plumes_close)
 ! DMP_mf sums the plumes' fluxes over i for each level after all plumes: here plume i adds its terms level by level as it rises,
 ! after plume i-1 has added all of its own, so every sum has the same terms in the same order. A plume's levels above its last
 ! one are DMP_mf's zeros (+0 terms, which leave a sum as it is), so they are not added. The flux limit (adjustment) needs only
 ! s_aw(kts+1), the sum of the plumes' first-level terms, so it is known before the ascents; the edmf sums use the scaled areas
 ! as DMP_mf's do. Accumulators: s_aw.. at the faces (k+1) and edmf_a, edmf_w at the levels in their own fields; s_awqt,
 ! edmf_qt, edmf_qc in pdk, pdt, pdq (turb writes those later).
 subroutine plumes_column(state, c)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c
 integer, parameter :: i_sawqt=i_pdk, i_eqt=i_pdt, i_eqc=i_pdq
 integer :: k, i, ii, ktop
 logical :: nofluxes, adjusted
 real(kind_phys)::fltv2,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,pwmin,pwmax,wmin,wmax,maxwidth,minwidth,pblh,dx,landsea
 real(kind_phys)::B,QTn,THLn,THVn,QCn,QNIn,Wn,EntEXP,BCOEFF,THVk,Pk,ent,wminp
 real(kind_phys), parameter ::Atot = 0.10
 real(kind_phys), parameter ::fluxportion=0.75
 real(kind_phys), parameter ::cf_thresh = 0.5
 real(kind_phys)::dl,d,cn,cc,l,n,acfac,ac_wsp,csigma,exc_fac,exc_heat,exc_moist,wspd_pbl,flt,flq,psig_w
 real(kind_phys)::upw,upthl,upqt,upqc,upqni,upa,upas,rhoz,thl1,qt1,qni1,saw2,flx1,adjustment,dzi1,maxmf,ztop,maxqc
 real(kind_phys) :: edmf_a, edmf_w, edmf_qt, edmf_qc
 real(kind_phys) :: eqt(max_levels), eqc(max_levels), ea(max_levels)
 real(kind_phys)::sigq,xl,rsl,cpm,a,qmq,mf_cf,Aup,Q1,qsat_tk,Fng,qww,alpha,beta,bb,pt,q2p,b9,QTp,QCp,dzm,dzk,tk
 pblh = state%pblh(c)
 dx = state%dx(c)
 landsea = state%xland(c)
 fltv2 = mc(mod(c-1,nch)+1,c_fltv2)
 minwidth = mc(mod(c-1,nch)+1,c_minw)
 maxwidth = state%maxwidth(c)
 wspd_pbl = mc(mod(c-1,nch)+1,c_wspd)
 flt = mc(mod(c-1,nch)+1,c_flt)
 flq = mc(mod(c-1,nch)+1,c_flq)
 psig_w = mc(mod(c-1,nch)+1,c_psw)
 cn = 0.
 d =-1.9
 dl = (maxwidth - minwidth)/real(nup-1,kind=kind_phys)
 do ii=1,NUP
  l = minwidth + dl*real(ii-1)
  cn = cn + l**d * (l*l)/(dx*dx) * dl
 enddo
 cc = Atot/cn
 if ((landsea-1.5).LT.0) then
  acfac = .5*tanh((fltv2 - 0.02)/0.05) + .5
 else
  acfac = .5*tanh((fltv2 - 0.01)/0.03) + .5
 endif
 if (wspd_pbl .le. 10.) then
  ac_wsp = 1.0
 else
  ac_wsp = 1.0 - min((wspd_pbl - 10.0)/15., 1.0)
 endif
 acfac = acfac * ac_wsp
 z0=50.
 pwmin=0.1
 pwmax=0.4
 wstar=max(1.E-2,(gtr*fltv2*pblh)**(onethird))
 qstar=max(flq,1.0E-5)/wstar
 thstar=flt/wstar
 csigma = 1.34
 if ((landsea-1.5).GE.0) then
  exc_fac = 0.58*4.0
 else
  exc_fac = 0.58
 endif
 exc_fac = exc_fac * ac_wsp
 sigmaW =csigma*wstar*(z0/pblh)**(onethird)*(1 - 0.8*z0/pblh)
 sigmaQT=csigma*qstar*(z0/pblh)**(onethird)
 sigmaTH=csigma*thstar*(z0/pblh)**(onethird)
 wmin=MIN(sigmaW*pwmin,0.1)
 wmax=MIN(sigmaW*pwmax,0.5)
 thl1 = (ms(mod(c-1,nch)+1,1,i_thl)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_thl)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
 qt1 = (ms(mod(c-1,nch)+1,1,i_sqw)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_sqw)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
 qni1 = (ms(mod(c-1,nch)+1,1,i_qni)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_qni)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
 ! the flux limit from the first-level terms
 rhoz = (ms(mod(c-1,nch)+1,1,i_rho)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_rho)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,1,i_dz))
 saw2 = 0.
 do i = 1, nup
  l = minwidth + dl*real(i-1)
  N = cc*l**d
  upa = N*l*l/(dx*dx) * dl
  upa = upa*acfac
  upw = wmin + real(i)/real(NUP)*(wmax-wmin)
  saw2 = saw2 + rhoz*upa*upw*Psig_w
 end do
 IF (saw2 /= 0.) THEN
  dzi1 = 0.5*(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,1+1,i_dz))
  flx1 = MAX(saw2*(ms(mod(c-1,nch)+1,1,i_th)-ms(mod(c-1,nch)+1,1+1,i_th))/dzi1,1.0e-5)
 else
  flx1 = 0.0
 endif
 adjustment=1.0
 adjusted = .false.
 IF (flx1 > fluxportion*flt/ms(mod(c-1,nch)+1,1,i_dz) .AND. flx1>0.0) THEN
  adjustment= fluxportion*flt/ms(mod(c-1,nch)+1,1,i_dz)/flx1
  adjusted = .true.
 endif
 do k = 1, nz+1
  ms(mod(c-1,nch)+1,k,i_saw) = 0.; ms(mod(c-1,nch)+1,k,i_sawthl) = 0.; ms(mod(c-1,nch)+1,k,i_sawqt) = 0.; ms(mod(c-1,nch)+1,k,i_sawqc) = 0.; ms(mod(c-1,nch)+1,k,i_sawqni) = 0.
  ms(mod(c-1,nch)+1,k,i_ea) = 0.; ms(mod(c-1,nch)+1,k,i_ew) = 0.; ms(mod(c-1,nch)+1,k,i_eqt) = 0.; ms(mod(c-1,nch)+1,k,i_eqc) = 0.
 end do
 ktop = 0
 nofluxes = .false.
 do i = 1, nup
  l = minwidth + dl*real(i-1)
  N = cc*l**d
  upa = N*l*l/(dx*dx) * dl
  upa = upa*acfac
  upas = upa
  if (adjusted) upas = upa*adjustment
  upw = wmin + real(i)/real(NUP)*(wmax-wmin)
  exc_heat = exc_fac*upw*sigmaTH/sigmaW
  upthl = thl1 + exc_heat
  exc_moist=exc_fac*upw*sigmaQT/sigmaW
  upqt = qt1 +exc_moist
  upqni = qni1
  upqc = 0.0
  call plume_level(c, 1, upa, upas, psig_w, upw, upthl, upqt, upqc, upqni)
  QCn = 0.
  do k=1+1,nz-1
   wminp = 0.3 + l*0.0005   ! DMP_mf's wmin, reused in its plume loop after the start velocities were set
   ent = 0.33/(MIN(MAX(upw,wminp),0.9)*l)
   ent = max(ent,0.0003)
   IF(ms(mod(c-1,nch)+1,k,i_zw) >= MIN(pblh+1500., 4000.))THEN
    ent=ent + (ms(mod(c-1,nch)+1,k,i_zw)-MIN(pblh+1500.,4000.))*5.0E-6
   endif
   ent = ent * (1.0 - zr)
   ent = min(ent,0.9/(ms(mod(c-1,nch)+1,k+1,i_zw)-ms(mod(c-1,nch)+1,k,i_zw)))
   EntExp= ent*(ms(mod(c-1,nch)+1,k+1,i_zw)-ms(mod(c-1,nch)+1,k,i_zw))
   QTn =upqt *(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_sqw)*EntExp
   THLn=upthl*(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_thl)*EntExp
   QNIn=upqni*(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_qni)*EntExp
   Pk =(ms(mod(c-1,nch)+1,k,i_p)*ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k+1,i_p)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
   call condensation_edmf(QTn,THLn,Pk,ms(mod(c-1,nch)+1,k+1,i_zw),THVn,QCn)
   THVk =(thv_at(c,k)*ms(mod(c-1,nch)+1,k+1,i_dz)+thv_at(c,k+1)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
   B=grav*(THVn/THVk - 1.0)
   IF(B>0.)THEN
    BCOEFF = 0.15
   else
    BCOEFF = 0.2
   endif
   IF (upw < 0.2 ) THEN
    Wn = upw + (-2. * ent * upw + BCOEFF*B / MAX(upw,0.2)) * MIN(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw), 250.)
   else
    Wn = upw + (-2. * ent * upw + BCOEFF*B / upw) * MIN(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw), 250.)
   endif
   IF(Wn > upw + MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0) ) THEN
    Wn = upw + MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0)
   endif
   IF(Wn < upw - MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0) ) THEN
    Wn = upw - MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0)
   endif
   Wn = MIN(MAX(Wn,0.0), 3.0)
   IF (k==1+1 .AND. Wn == 0.) THEN
    nofluxes = .true.
    exit
   endif
   IF (Wn > 0.) THEN
    upw=Wn
    upthl=THLn
    upqt=QTn
    upqc=QCn
    upqni=QNIn
    ktop = MAX(ktop,k)
    call plume_level(c, k, upa, upas, psig_w, upw, upthl, upqt, upqc, upqni)
   else
    exit
   endif
  ENDDO
 ENDDO
 ktop=MIN(ktop,nz-1)
 IF (ktop == 0) THEN
  ztop = 0.0
 else
  ztop=ms(mod(c-1,nch)+1,ktop,i_zw)
 endif
 maxmf = 0.0
 if (.not. nofluxes) then
  do k = 1, nz+1
   ms(mod(c-1,nch)+1,k,i_sawqv) = ms(mod(c-1,nch)+1,k,i_sawqt) - ms(mod(c-1,nch)+1,k,i_sawqc)
  end do
  if (adjusted) then
   do k = 1, nz+1
    ms(mod(c-1,nch)+1,k,i_saw) = ms(mod(c-1,nch)+1,k,i_saw)*adjustment
    ms(mod(c-1,nch)+1,k,i_sawthl) = ms(mod(c-1,nch)+1,k,i_sawthl)*adjustment
    ms(mod(c-1,nch)+1,k,i_sawqc) = ms(mod(c-1,nch)+1,k,i_sawqc)*adjustment
    ms(mod(c-1,nch)+1,k,i_sawqv) = ms(mod(c-1,nch)+1,k,i_sawqv)*adjustment
    ms(mod(c-1,nch)+1,k,i_sawqni) = ms(mod(c-1,nch)+1,k,i_sawqni)*adjustment
   end do
  end if
  do k = 1, nz-1
   edmf_a = ms(mod(c-1,nch)+1,k,i_ea)
   edmf_w = ms(mod(c-1,nch)+1,k,i_ew)
   edmf_qt = ms(mod(c-1,nch)+1,k,i_eqt)
   edmf_qc = ms(mod(c-1,nch)+1,k,i_eqc)
   if (edmf_a>0.) then
    edmf_w=edmf_w/edmf_a
    edmf_qt=edmf_qt/edmf_a
    edmf_qc=edmf_qc/edmf_a
    edmf_a=edmf_a*Psig_w
    if(edmf_a*edmf_w > maxmf) maxmf = edmf_a*edmf_w
   endif
   ms(mod(c-1,nch)+1,k,i_ea) = edmf_a
   ms(mod(c-1,nch)+1,k,i_ew) = edmf_w
   ea(k) = edmf_a
   eqt(k) = edmf_qt
   eqc(k) = edmf_qc
  end do
  do k=1+1,nz-2
   if (k > KTOP) exit
   if(0.5*(eqc(k)+eqc(k-1))>0.0 .and. (ms(mod(c-1,nch)+1,k,i_cld) < cf_thresh))THEN
    dzm = 0.5*(ms(mod(c-1,nch)+1,k-1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
    dzk = 0.5*(ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k+1,i_dz))
    Aup = (ea(k)*dzm+ea(k-1)*dzk)/(dzm+dzk)
    QTp = (eqt(k)*dzm+eqt(k-1)*dzk)/(dzm+dzk)
    if (eqc(k)>0.0 .and. eqc(k-1)>0.0) then
     QCp = (eqc(k)*dzm+eqc(k-1)*dzk)/(dzm+dzk)
    else
     QCp = max(eqc(k),eqc(k-1))
    endif
    tk = ms(mod(c-1,nch)+1,k,i_th)*ms(mod(c-1,nch)+1,k,i_ex)
    xl = xl_blend(tk,t0c,tice,cice,cliq,cpv,xls,xlv)
    qsat_tk = qsat_blend(tk,t0c,tice,ms(mod(c-1,nch)+1,k,i_p))
    rsl = xl*qsat_tk / (r_v*tk**2)
    cpm = cp + ms(mod(c-1,nch)+1,k,i_sqw)*cpv
    a = 1./(1. + xl*rsl/cpm)
    b9 = a*rsl
    q2p = xlvcp/ms(mod(c-1,nch)+1,k,i_ex)
    pt = ms(mod(c-1,nch)+1,k,i_thl) +q2p*QCp*Aup
    bb = b9*tk/pt
    qww = 1.+0.61*ms(mod(c-1,nch)+1,k,i_sqw)
    alpha = 0.61*pt
    beta = pt*xl/(tk*cp) - 1.61*pt
    sigq = 10. * Aup * (QTp - ms(mod(c-1,nch)+1,k,i_sqw))
    sigq = max(sigq, qsat_tk*0.02 )
    sigq = min(sigq, qsat_tk*0.25 )
    qmq = a * (ms(mod(c-1,nch)+1,k,i_sqw) - qsat_tk)
    Q1 = qmq/sigq
    if ((landsea-1.5).GE.0) then
     mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
     mf_cf = max(mf_cf, 1.2 * Aup)
     mf_cf = min(mf_cf, 5.0 * Aup)
    else
     mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
     mf_cf = max(mf_cf, 1.8 * Aup)
     mf_cf = min(mf_cf, 5.0 * Aup)
    endif
    if (QCp * Aup > 5e-5) then
     ms(mod(c-1,nch)+1,k,i_qcbl) = 1.86 * (QCp * Aup) - 2.2e-5
    else
     ms(mod(c-1,nch)+1,k,i_qcbl) = 1.18 * (QCp * Aup)
    endif
    ms(mod(c-1,nch)+1,k,i_cld) = mf_cf
    Q1=max(Q1,-2.25)
    if (Q1 .ge. 1.0) then
     Fng = 1.0
    elseif (Q1 .ge. -1.7 .and. Q1 .lt. 1.0) then
     Fng = EXP(-0.4*(Q1-1.0))
    elseif (Q1 .ge. -2.5 .and. Q1 .lt. -1.7) then
     Fng = 3.0 + EXP(-3.8*(Q1+1.7))
    else
     Fng = min(23.9 + EXP(-1.6*(Q1+2.5)), 60.)
    endif
    ms(mod(c-1,nch)+1,k,i_vt) = qww - (1.5*Aup)*beta*bb*Fng - 1.
    ms(mod(c-1,nch)+1,k,i_vq) = alpha + (1.5*Aup)*beta*a*Fng - tv0
   endif
  enddo
  mi(mod(c-1,nch)+1,m_mf) = 1
 else
  do k = 1, ktop
   eqc(k) = 0.
  end do
 end if
 if (ktop > 0) then
  maxqc = eqc(1)
  do k = 2, ktop
   maxqc = max(maxqc, eqc(k))
  end do
  if ( maxqc < 1.E-8) maxmf = -1.0*maxmf
 endif
 state%ktop_plume(c) = ktop
 state%ztop_plume(c) = ztop
 state%maxmf(c) = maxmf
 end subroutine

 ! a plume's terms at level k (face k+1 for s_aw..) added to the column's sums, as DMP_mf adds them after its plume block
 subroutine plume_level(c, k, upa, upas, psig_w, upw, upthl, upqt, upqc, upqni)
 !$acc routine seq
 integer, intent(in) :: c, k
 real(kind_phys), intent(in) :: upa, upas, psig_w, upw, upthl, upqt, upqc, upqni
 integer, parameter :: i_sawqt=i_pdk, i_eqt=i_pdt, i_eqc=i_pdq
 real(kind_phys) :: rhoz
 rhoz = (ms(mod(c-1,nch)+1,k,i_rho)*ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k+1,i_rho)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
 ms(mod(c-1,nch)+1,k+1,i_saw) = ms(mod(c-1,nch)+1,k+1,i_saw) + rhoz*upa*upw*Psig_w
 ms(mod(c-1,nch)+1,k+1,i_sawthl)= ms(mod(c-1,nch)+1,k+1,i_sawthl) + rhoz*upa*upw*upthl*Psig_w
 ms(mod(c-1,nch)+1,k+1,i_sawqt) = ms(mod(c-1,nch)+1,k+1,i_sawqt) + rhoz*upa*upw*upqt*Psig_w
 ms(mod(c-1,nch)+1,k+1,i_sawqc) = ms(mod(c-1,nch)+1,k+1,i_sawqc) + rhoz*upa*upw*upqc*Psig_w
 ms(mod(c-1,nch)+1,k+1,i_sawqni) = ms(mod(c-1,nch)+1,k+1,i_sawqni) + rhoz*upa*upw*upqni*Psig_w
 ms(mod(c-1,nch)+1,k,i_ea) =ms(mod(c-1,nch)+1,k,i_ea) +upas
 ms(mod(c-1,nch)+1,k,i_ew) =ms(mod(c-1,nch)+1,k,i_ew) +rhoz*upas*upw
 ms(mod(c-1,nch)+1,k,i_eqt) =ms(mod(c-1,nch)+1,k,i_eqt) +rhoz*upas*upqt
 ms(mod(c-1,nch)+1,k,i_eqc) =ms(mod(c-1,nch)+1,k,i_eqc) +rhoz*upas*upqc
 end subroutine

 ! ---- plumes: one DMP_mf plume (i) of plume column j (column c); its profile into pp up to its last level plev ---------------------
 subroutine plume_ascent(state, j, c, i)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: j, c, i
 integer :: k, ii, lev
 real(kind_phys)::fltv2,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,pwmin,pwmax,wmin,wmax,maxwidth,minwidth,pblh,dx,landsea
 real(kind_phys)::B,QTn,THLn,THVn,QCn,QNIn,Wn,EntEXP,BCOEFF,THVk,Pk,ent
 real(kind_phys), parameter ::Atot = 0.10
 real(kind_phys)::dl,d,cn,cc,l,n,acfac,ac_wsp,csigma,exc_fac,exc_heat,exc_moist,wspd_pbl,flt,flq
 real(kind_phys)::upw,upthl,upqt,upqni,upa
 pblh = state%pblh(c)
 dx = state%dx(c)
 landsea = state%xland(c)
 fltv2 = mc(mod(c-1,nch)+1,c_fltv2)
 minwidth = mc(mod(c-1,nch)+1,c_minw)
 maxwidth = state%maxwidth(c)
 wspd_pbl = mc(mod(c-1,nch)+1,c_wspd)
 flt = mc(mod(c-1,nch)+1,c_flt)
 flq = mc(mod(c-1,nch)+1,c_flq)
 cn = 0.
 d =-1.9
 dl = (maxwidth - minwidth)/real(nup-1,kind=kind_phys)
 do ii=1,NUP
  l = minwidth + dl*real(ii-1)
  cn = cn + l**d * (l*l)/(dx*dx) * dl
 enddo
 cc = Atot/cn
 if ((landsea-1.5).LT.0) then
  acfac = .5*tanh((fltv2 - 0.02)/0.05) + .5
 else
  acfac = .5*tanh((fltv2 - 0.01)/0.03) + .5
 endif
 if (wspd_pbl .le. 10.) then
  ac_wsp = 1.0
 else
  ac_wsp = 1.0 - min((wspd_pbl - 10.0)/15., 1.0)
 endif
 acfac = acfac * ac_wsp
 l = minwidth + dl*real(i-1)
 N = cc*l**d
 upa = N*l*l/(dx*dx) * dl
 upa = upa*acfac
 z0=50.
 pwmin=0.1
 pwmax=0.4
 wstar=max(1.E-2,(gtr*fltv2*pblh)**(onethird))
 qstar=max(flq,1.0E-5)/wstar
 thstar=flt/wstar
 csigma = 1.34
 if ((landsea-1.5).GE.0) then
  exc_fac = 0.58*4.0
 else
  exc_fac = 0.58
 endif
 exc_fac = exc_fac * ac_wsp
 sigmaW =csigma*wstar*(z0/pblh)**(onethird)*(1 - 0.8*z0/pblh)
 sigmaQT=csigma*qstar*(z0/pblh)**(onethird)
 sigmaTH=csigma*thstar*(z0/pblh)**(onethird)
 wmin=MIN(sigmaW*pwmin,0.1)
 wmax=MIN(sigmaW*pwmax,0.5)
 upw=wmin + real(i)/real(NUP)*(wmax-wmin)
 exc_heat = exc_fac*upw*sigmaTH/sigmaW
 upthl=(ms(mod(c-1,nch)+1,1,i_thl)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_thl)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz)) + exc_heat
 exc_moist=exc_fac*upw*sigmaQT/sigmaW
 upqt=(ms(mod(c-1,nch)+1,1,i_sqw)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_sqw)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz)) +exc_moist
 upqni=(ms(mod(c-1,nch)+1,1,i_qni)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_qni)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
 pp(j,1,i,p_w) = upw
 pp(j,1,i,p_thl) = upthl
 pp(j,1,i,p_qt) = upqt
 pp(j,1,i,p_qc) = 0.0
 pp(j,1,i,p_qni) = upqni
 pa1(j,i) = upa
 pzero(j,i) = 0
 lev = 1
 QCn = 0.
 do k=1+1,nz-1
  wmin = 0.3 + l*0.0005
  ent = 0.33/(MIN(MAX(upw,wmin),0.9)*l)
  ent = max(ent,0.0003)
  IF(ms(mod(c-1,nch)+1,k,i_zw) >= MIN(pblh+1500., 4000.))THEN
   ent=ent + (ms(mod(c-1,nch)+1,k,i_zw)-MIN(pblh+1500.,4000.))*5.0E-6
  endif
  ent = ent * (1.0 - zr)
  ent = min(ent,0.9/(ms(mod(c-1,nch)+1,k+1,i_zw)-ms(mod(c-1,nch)+1,k,i_zw)))
  EntExp= ent*(ms(mod(c-1,nch)+1,k+1,i_zw)-ms(mod(c-1,nch)+1,k,i_zw))
  QTn =upqt *(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_sqw)*EntExp
  THLn=upthl*(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_thl)*EntExp
  QNIn=upqni*(1.-EntExp) + ms(mod(c-1,nch)+1,k,i_qni)*EntExp
  Pk =(ms(mod(c-1,nch)+1,k,i_p)*ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k+1,i_p)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
  call condensation_edmf(QTn,THLn,Pk,ms(mod(c-1,nch)+1,k+1,i_zw),THVn,QCn)
  THVk =(thv_at(c,k)*ms(mod(c-1,nch)+1,k+1,i_dz)+thv_at(c,k+1)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
  B=grav*(THVn/THVk - 1.0)
  IF(B>0.)THEN
   BCOEFF = 0.15
  else
   BCOEFF = 0.2
  endif
  IF (upw < 0.2 ) THEN
   Wn = upw + (-2. * ent * upw + BCOEFF*B / MAX(upw,0.2)) * MIN(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw), 250.)
  else
   Wn = upw + (-2. * ent * upw + BCOEFF*B / upw) * MIN(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw), 250.)
  endif
  IF(Wn > upw + MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0) ) THEN
   Wn = upw + MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0)
  endif
  IF(Wn < upw - MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0) ) THEN
   Wn = upw - MIN(1.25*(ms(mod(c-1,nch)+1,k,i_zw)-ms(mod(c-1,nch)+1,k-1,i_zw))/200., 2.0)
  endif
  Wn = MIN(MAX(Wn,0.0), 3.0)
  IF (k==1+1 .AND. Wn == 0.) THEN
   pzero(j,i) = 1
   exit
  endif
  IF (Wn > 0.) THEN
   if (k > lplume) then   ! above the profiles: the column goes to plumes_column
    mi(mod(c-1,nch)+1,m_deep) = 1
    exit
   end if
   upw=Wn
   upthl=THLn
   upqt=QTn
   upqni=QNIn
   pp(j,k,i,p_w) = Wn
   pp(j,k,i,p_thl) = THLn
   pp(j,k,i,p_qt) = QTn
   pp(j,k,i,p_qc) = QCn
   pp(j,k,i,p_qni) = QNIn
   lev = k
  else
   exit
  endif
 ENDDO
 plev(j,i) = lev
 end subroutine

 ! ---- plume sums (plume column j, face/level k): DMP_mf's sums over the plumes after its plume block, in its order over i. A
 ! plume's values above its last level are DMP_mf's zeros: their terms (+0) leave every sum as it is, so they are not added. The
 ! flux limit (adjustment) needs only s_aw(kts+1), the sum of the first-level terms, which each thread forms for itself. Face k:
 ! the s_aw.. of level k-1; level k: edmf_a, edmf_w (fields ea, ew), edmf_qt, edmf_qc (in pdt, pdq: turb writes those later).
 subroutine plume_sums(j, c, k)
 !$acc routine seq
 integer, intent(in) :: j, c, k
 integer, parameter :: i_eqt=i_pdt, i_eqc=i_pdq
 real(kind_phys), parameter ::fluxportion=0.75
 integer :: i, kl
 logical :: adjusted
 real(kind_phys) :: psig_w, flt, rhoz, upa, s_aw, s_awthl, s_awqt, s_awqc, s_awqni, flx1, adjustment, dzi1, saw2
 real(kind_phys) :: edmf_a, edmf_w, edmf_qt, edmf_qc
 if (mi(mod(c-1,nch)+1,m_deep) /= 0) return   ! done by plumes_column
 do i = 1, nup
  if (pzero(j,i) /= 0) return   ! DMP_mf's nup2 = 0: no fluxes
 end do
 psig_w = mc(mod(c-1,nch)+1,c_psw)
 flt = mc(mod(c-1,nch)+1,c_flt)
 rhoz = (ms(mod(c-1,nch)+1,1,i_rho)*ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,2,i_rho)*ms(mod(c-1,nch)+1,1,i_dz))/(ms(mod(c-1,nch)+1,2,i_dz)+ms(mod(c-1,nch)+1,1,i_dz))
 saw2 = 0.
 do i = 1, nup
  saw2 = saw2 + rhoz*pa1(j,i)*pp(j,1,i,p_w)*Psig_w
 end do
 IF (saw2 /= 0.) THEN
  dzi1 = 0.5*(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,1+1,i_dz))
  flx1 = MAX(saw2*(ms(mod(c-1,nch)+1,1,i_th)-ms(mod(c-1,nch)+1,1+1,i_th))/dzi1,1.0e-5)
 else
  flx1 = 0.0
 endif
 adjustment=1.0
 adjusted = .false.
 IF (flx1 > fluxportion*flt/ms(mod(c-1,nch)+1,1,i_dz) .AND. flx1>0.0) THEN
  adjustment= fluxportion*flt/ms(mod(c-1,nch)+1,1,i_dz)/flx1
  adjusted = .true.
 endif
 kl = k-1
 s_aw = 0.; s_awthl = 0.; s_awqt = 0.; s_awqc = 0.; s_awqni = 0.
 if (kl >= 1 .and. kl <= nz-1) then
  rhoz = (ms(mod(c-1,nch)+1,kl,i_rho)*ms(mod(c-1,nch)+1,kl+1,i_dz)+ms(mod(c-1,nch)+1,kl+1,i_rho)*ms(mod(c-1,nch)+1,kl,i_dz))/(ms(mod(c-1,nch)+1,kl+1,i_dz)+ms(mod(c-1,nch)+1,kl,i_dz))
  do i = 1, nup
   if (kl > plev(j,i)) cycle
   upa = pa1(j,i)
   s_aw = s_aw + rhoz*upa*pp(j,kl,i,p_w)*Psig_w
   s_awthl= s_awthl + rhoz*upa*pp(j,kl,i,p_w)*pp(j,kl,i,p_thl)*Psig_w
   s_awqt = s_awqt + rhoz*upa*pp(j,kl,i,p_w)*pp(j,kl,i,p_qt)*Psig_w
   s_awqc = s_awqc + rhoz*upa*pp(j,kl,i,p_w)*pp(j,kl,i,p_qc)*Psig_w
  end do
  do i = 1, nup
   if (kl > plev(j,i)) cycle
   upa = pa1(j,i)
   s_awqni = s_awqni + rhoz*upa*pp(j,kl,i,p_w)*pp(j,kl,i,p_qni)*Psig_w
  end do
 end if
 if (adjusted) then
  ms(mod(c-1,nch)+1,k,i_saw) = s_aw*adjustment
  ms(mod(c-1,nch)+1,k,i_sawthl) = s_awthl*adjustment
  ms(mod(c-1,nch)+1,k,i_sawqc) = s_awqc*adjustment
  ms(mod(c-1,nch)+1,k,i_sawqv) = (s_awqt - s_awqc)*adjustment
  ms(mod(c-1,nch)+1,k,i_sawqni) = s_awqni*adjustment
 else
  ms(mod(c-1,nch)+1,k,i_saw) = s_aw
  ms(mod(c-1,nch)+1,k,i_sawthl) = s_awthl
  ms(mod(c-1,nch)+1,k,i_sawqc) = s_awqc
  ms(mod(c-1,nch)+1,k,i_sawqv) = s_awqt - s_awqc
  ms(mod(c-1,nch)+1,k,i_sawqni) = s_awqni
 end if
 if (k > nz) return
 edmf_a = 0.; edmf_w = 0.; edmf_qt = 0.; edmf_qc = 0.
 if (k <= nz-1) then
  rhoz = (ms(mod(c-1,nch)+1,k,i_rho)*ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k+1,i_rho)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k+1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
  do i = 1, nup
   if (k > plev(j,i)) cycle
   upa = pa1(j,i)
   if (adjusted) upa = upa*adjustment
   edmf_a =edmf_a +upa
   edmf_w =edmf_w +rhoz*upa*pp(j,k,i,p_w)
   edmf_qt =edmf_qt +rhoz*upa*pp(j,k,i,p_qt)
   edmf_qc =edmf_qc +rhoz*upa*pp(j,k,i,p_qc)
  end do
  if (edmf_a>0.) then
   edmf_w=edmf_w/edmf_a
   edmf_qt=edmf_qt/edmf_a
   edmf_qc=edmf_qc/edmf_a
   edmf_a=edmf_a*Psig_w
  endif
 end if
 ms(mod(c-1,nch)+1,k,i_ea) = edmf_a
 ms(mod(c-1,nch)+1,k,i_ew) = edmf_w
 ms(mod(c-1,nch)+1,k,i_eqt) = edmf_qt
 ms(mod(c-1,nch)+1,k,i_eqc) = edmf_qc
 end subroutine

 ! ---- plume column j (column c) after the sums: ktop, ztop, maxmf and the plume clouds of DMP_mf -------------------------------
 subroutine plumes_close(state, j, c)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: j, c
 integer, parameter :: i_eqt=i_pdt, i_eqc=i_pdq
 real(kind_phys), parameter ::cf_thresh = 0.5
 integer :: k, i, ktop
 logical :: nofluxes
 real(kind_phys) :: maxmf, ztop, maxqc, landsea
 real(kind_phys)::sigq,xl,rsl,cpm,a,qmq,mf_cf,Aup,Q1,qsat_tk,Fng,qww,alpha,beta,bb,pt,q2p,b9,QTp,QCp,dzm,dzk,tk
 real(kind_phys)::qc0,qc1,ea0,ea1,qt0,qt1
 if (mi(mod(c-1,nch)+1,m_deep) /= 0) return   ! done by plumes_column
 landsea = state%xland(c)
 ktop = 0
 nofluxes = .false.
 do i = 1, nup
  if (plev(j,i) > 1) ktop = MAX(ktop, plev(j,i))
  if (pzero(j,i) /= 0) nofluxes = .true.
 end do
 ktop=MIN(ktop,nz-1)
 IF (ktop == 0) THEN
  ztop = 0.0
 else
  ztop=ms(mod(c-1,nch)+1,ktop,i_zw)
 endif
 maxmf = 0.0
 if (.not. nofluxes) then
  do k = 1, nz-1
   if (ms(mod(c-1,nch)+1,k,i_ea)>0.) then
    if(ms(mod(c-1,nch)+1,k,i_ea)*ms(mod(c-1,nch)+1,k,i_ew) > maxmf) maxmf = ms(mod(c-1,nch)+1,k,i_ea)*ms(mod(c-1,nch)+1,k,i_ew)
   endif
  end do
  do k=1+1,nz-2
   if (k > KTOP) exit
   qc0 = ms(mod(c-1,nch)+1,k-1,i_eqc); qc1 = ms(mod(c-1,nch)+1,k,i_eqc)
   if(0.5*(qc1+qc0)>0.0 .and. (ms(mod(c-1,nch)+1,k,i_cld) < cf_thresh))THEN
    ea0 = ms(mod(c-1,nch)+1,k-1,i_ea); ea1 = ms(mod(c-1,nch)+1,k,i_ea); qt0 = ms(mod(c-1,nch)+1,k-1,i_eqt); qt1 = ms(mod(c-1,nch)+1,k,i_eqt)
    dzm = 0.5*(ms(mod(c-1,nch)+1,k-1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
    dzk = 0.5*(ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k+1,i_dz))
    Aup = (ea1*dzm+ea0*dzk)/(dzm+dzk)
    QTp = (qt1*dzm+qt0*dzk)/(dzm+dzk)
    if (qc1>0.0 .and. qc0>0.0) then
     QCp = (qc1*dzm+qc0*dzk)/(dzm+dzk)
    else
     QCp = max(qc1,qc0)
    endif
    tk = ms(mod(c-1,nch)+1,k,i_th)*ms(mod(c-1,nch)+1,k,i_ex)
    xl = xl_blend(tk,t0c,tice,cice,cliq,cpv,xls,xlv)
    qsat_tk = qsat_blend(tk,t0c,tice,ms(mod(c-1,nch)+1,k,i_p))
    rsl = xl*qsat_tk / (r_v*tk**2)
    cpm = cp + ms(mod(c-1,nch)+1,k,i_sqw)*cpv
    a = 1./(1. + xl*rsl/cpm)
    b9 = a*rsl
    q2p = xlvcp/ms(mod(c-1,nch)+1,k,i_ex)
    pt = ms(mod(c-1,nch)+1,k,i_thl) +q2p*QCp*Aup
    bb = b9*tk/pt
    qww = 1.+0.61*ms(mod(c-1,nch)+1,k,i_sqw)
    alpha = 0.61*pt
    beta = pt*xl/(tk*cp) - 1.61*pt
    sigq = 10. * Aup * (QTp - ms(mod(c-1,nch)+1,k,i_sqw))
    sigq = max(sigq, qsat_tk*0.02 )
    sigq = min(sigq, qsat_tk*0.25 )
    qmq = a * (ms(mod(c-1,nch)+1,k,i_sqw) - qsat_tk)
    Q1 = qmq/sigq
    if ((landsea-1.5).GE.0) then
     mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
     mf_cf = max(mf_cf, 1.2 * Aup)
     mf_cf = min(mf_cf, 5.0 * Aup)
    else
     mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
     mf_cf = max(mf_cf, 1.8 * Aup)
     mf_cf = min(mf_cf, 5.0 * Aup)
    endif
    if (QCp * Aup > 5e-5) then
     ms(mod(c-1,nch)+1,k,i_qcbl) = 1.86 * (QCp * Aup) - 2.2e-5
    else
     ms(mod(c-1,nch)+1,k,i_qcbl) = 1.18 * (QCp * Aup)
    endif
    ms(mod(c-1,nch)+1,k,i_cld) = mf_cf
    Q1=max(Q1,-2.25)
    if (Q1 .ge. 1.0) then
     Fng = 1.0
    elseif (Q1 .ge. -1.7 .and. Q1 .lt. 1.0) then
     Fng = EXP(-0.4*(Q1-1.0))
    elseif (Q1 .ge. -2.5 .and. Q1 .lt. -1.7) then
     Fng = 3.0 + EXP(-3.8*(Q1+1.7))
    else
     Fng = min(23.9 + EXP(-1.6*(Q1+2.5)), 60.)
    endif
    ms(mod(c-1,nch)+1,k,i_vt) = qww - (1.5*Aup)*beta*bb*Fng - 1.
    ms(mod(c-1,nch)+1,k,i_vq) = alpha + (1.5*Aup)*beta*a*Fng - tv0
   endif
  enddo
  mi(mod(c-1,nch)+1,m_mf) = 1
  if (ktop > 0) then
   maxqc = ms(mod(c-1,nch)+1,1,i_eqc)
   do k = 2, ktop
    maxqc = max(maxqc, ms(mod(c-1,nch)+1,k,i_eqc))
   end do
   if ( maxqc < 1.E-8) maxmf = -1.0*maxmf
  endif
 else
  if (ktop > 0) maxmf = -1.0*maxmf   ! edmf_qc all zero: maxqc 0 < 1e-8
 end if
 state%ktop_plume(c) = ktop
 state%ztop_plume(c) = ztop
 state%maxmf(c) = maxmf
 end subroutine

 ! ---- turb: mym_level2, mym_length (bl_mynn_mixlength 2) and mym_turbulence at level k -----------------------------------------
 subroutine level2_level(c, k, i_qw, dtl, dqw, dtv, gm, gh, sm, sh)
 !$acc routine seq
 integer, intent(in) :: c, k, i_qw
 real(kind_phys), intent(out) :: dtl, dqw, dtv, gm, gh, sm, sh
 real(kind_phys)::rfc,f1,f2,rf1,rf2,smc,shc,ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf
 real(kind_phys)::a2fac
 dzk = 0.5 *( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
 afk = ms(mod(c-1,nch)+1,k,i_dz)/( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
 abk = 1.0 -afk
 duz = ( ms(mod(c-1,nch)+1,k,i_u)-ms(mod(c-1,nch)+1,k-1,i_u) )**2 +( ms(mod(c-1,nch)+1,k,i_v)-ms(mod(c-1,nch)+1,k-1,i_v) )**2
 duz = duz /dzk**2
 dtz = ( ms(mod(c-1,nch)+1,k,i_thl)-ms(mod(c-1,nch)+1,k-1,i_thl) )/( dzk )
 dqz = ( ms(mod(c-1,nch)+1,k,i_qw)-ms(mod(c-1,nch)+1,k-1,i_qw) )/( dzk )
 vtt = 1.0 +ms(mod(c-1,nch)+1,k,i_vt)*abk +ms(mod(c-1,nch)+1,k-1,i_vt)*afk
 vqq = tv0 +ms(mod(c-1,nch)+1,k,i_vq)*abk +ms(mod(c-1,nch)+1,k-1,i_vq)*afk
 dtq = vtt*dtz +vqq*dqz
 dtl = dtz
 dqw = dqz
 dtv = dtq
 gm = duz
 gh = -dtq*gtr
 ri = -gh/MAX( duz, 1.0e-10 )
 IF (CKmod .eq. 1) THEN
  a2fac = 1./(1. + MAX(ri,0.0))
 else
  a2fac = 1.
 endif
 rfc = g1/( g1+g2 )
 f1 = b1*( g1-c1 ) +3.0*a2*a2fac *( 1.0 -c2 )*( 1.0-c5 ) +2.0*a1*( 3.0-2.0*c2 )
 f2 = b1*( g1+g2 ) -3.0*a1*( 1.0 -c2 )
 rf1 = b1*( g1-c1 )/f1
 rf2 = b1* g1 /f2
 smc = a1 /(a2*a2fac)* f1/f2
 shc = 3.0*(a2*a2fac)*( g1+g2 )
 ri1 = 0.5/smc
 ri2 = rf1*smc
 ri3 = 4.0*rf2*smc -2.0*ri2
 ri4 = ri2**2
 rf = MIN( ri1*( ri + ri2-SQRT(ri**2 - ri3*ri + ri4) ), rfc )
 sh = shc*( rfc-rf )/( 1.0-rf )
 sm = smc*( rf1-rf )/( rf2-rf ) * sh
 end subroutine

 subroutine turbulence_point(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 real(kind_phys) :: dtl, dqw, dtv, gm, gh, sm, sh, qkw, el, afk, abk, qtke, ea_km1, ew_km1, ea_k, ew_k
 real(kind_phys)::rmo,zi,elt,vsc,Psig_bl,vflx
 real(kind_phys)::cns,alp2,alp3,alp4,alp5,alp6,zi2,h1,h2
 real(kind_phys)::zwk,dzk,bv,tau_cloud,wstar,elb,els,elf,elb_mf,wt,el_les
 real(kind_phys), parameter ::ctau = 1000.
 real(kind_phys)::gamt,gamq,gamv,elq,elh
 real(kind_phys)::cldavg
 real(kind_phys)::a2fac,duz,ri
 real(kind_phys)::sh25max,sh25min
 real(kind=kind_phys8)::q2sq,elsq,gmel,ghel
 real(kind=kind_phys8)::q3sq,dlsq,qdiv
 real(kind=kind_phys8)::e1,e2,e3,e4,eden
 real(kind_phys)::shb
 real(kind_phys), parameter ::Prlimit = 5.0
 if (k == 1) then
  ms(mod(c-1,nch)+1,k,i_el) = 0.0
  ms(mod(c-1,nch)+1,k,i_dfm) = 0.0
  ms(mod(c-1,nch)+1,k,i_dfh) = 0.0
  ms(mod(c-1,nch)+1,k,i_tcd) = 0.0
  return
 end if
 call level2_level(c, k, i_sqw, dtl, dqw, dtv, gm, gh, sm, sh)
 ! mym_length at k
 if (mi(mod(c-1,nch)+1,m_mf) /= 0) then
  ea_km1 = ms(mod(c-1,nch)+1,k-1,i_ea); ew_km1 = ms(mod(c-1,nch)+1,k-1,i_ew); ea_k = ms(mod(c-1,nch)+1,k,i_ea); ew_k = ms(mod(c-1,nch)+1,k,i_ew)
 else
  ea_km1 = 0.; ew_km1 = 0.; ea_k = 0.; ew_k = 0.
 end if
 rmo = mc(mod(c-1,nch)+1,c_rmol)
 zi = state%pblh(c)
 elt = mc(mod(c-1,nch)+1,c_elt)
 vsc = mc(mod(c-1,nch)+1,c_vsc)
 Psig_bl = mc(mod(c-1,nch)+1,c_psbl)
 vflx = mc(mod(c-1,nch)+1,c_fltv)
 cns = 3.5
 alp2 = 0.30
 alp3 = 2.0
 alp4 = 5.0
 alp5 = alp2
 alp6 = 50.0
 zi2=MAX(zi, 300.)
 h1=MAX(0.3*zi2,300.)
 h1=MIN(h1,600.)
 h2=h1*0.5
 afk = ms(mod(c-1,nch)+1,k,i_dz)/( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
 abk = 1.0 -afk
 qkw = SQRT(MAX(ms(mod(c-1,nch)+1,k,i_qke)*abk+ms(mod(c-1,nch)+1,k-1,i_qke)*afk,1.0e-3))
 qtke = 0.5*qkw**2
 zwk = ms(mod(c-1,nch)+1,k,i_zw)
 dzk = 0.5*( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
 IF ( dtv .GT. 0.0 ) THEN
  bv = MAX( SQRT( gtr*dtv ), 0.001)
  elb_mf = MAX(alp2*qkw, alp6*ea_km1*ew_km1) / bv *( 1.0 + alp3*SQRT( vsc/( bv*elt ) ) )
  elb = MIN(MAX(alp5*qkw, alp6*ea_k*ew_k)/bv, zwk)
  wstar = 1.25*(gtr*zi*MAX(vflx,1.0e-4))**onethird
  tau_cloud = MIN(MAX(ctau * wstar/grav, 30.), 150.)
  wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
  tau_cloud = tau_cloud*(1.-wt) + 50.*wt
  elf = MIN(MAX(tau_cloud*SQRT(MIN(qtke,40.)), alp6*ea_k*ew_k/bv), zwk)
 else
  wstar = 1.25*(gtr*zi*MAX(vflx,1.0e-4))**onethird
  tau_cloud = MIN(MAX(ctau * wstar/grav, 50.), 200.)
  wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
  tau_cloud = tau_cloud*(1.-wt) + MAX(100.,dzk*0.25)*wt
  elb = MIN(tau_cloud*SQRT(MIN(qtke,40.)), zwk)
  elf = elb
  elb_mf = elb
 endif
 elf = elf/(1. + (elf/800.))
 elb_mf = MAX(elb_mf, 0.01)
 IF ( rmo .GT. 0.0 ) THEN
  els = karman*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
 else
  els = karman*zwk*( 1.0 - alp4* zwk*rmo )**0.2
 endif
 wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
 el = SQRT( els**2/(1. + (els**2/elt**2) +(els**2/elb_mf**2)))
 el = el*(1.-wt) + elf*wt
 el_les= MIN(els/(1. + (els/12.)), elb_mf)
 el = el*Psig_bl + (1.-Psig_bl)*el_les
 ! mym_turbulence at k
 dzk = 0.5 *( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
 elsq = el**2
 q3sq = qkw**2
 q2sq = b1*elsq*( sm*gm+sh*gh )
 sh= MAX(sh, 1e-5)
 duz = ( ms(mod(c-1,nch)+1,k,i_u)-ms(mod(c-1,nch)+1,k-1,i_u) )**2 +( ms(mod(c-1,nch)+1,k,i_v)-ms(mod(c-1,nch)+1,k-1,i_v) )**2
 duz = duz /dzk**2
 ri = -gh/MAX( duz, 1.0e-10 )
 IF (CKmod .eq. 1) THEN
  a2fac = 1./(1. + MAX(ri,0.0))
 else
  a2fac = 1.
 endif
 gmel = gm*elsq
 ghel = gh*elsq
 dlsq = elsq
 IF ( q3sq/dlsq .LT. -gh ) q3sq = -dlsq*gh
 IF ( q3sq .LT. q2sq ) THEN
  qdiv = SQRT( q3sq/q2sq )
  sh = sh * qdiv
  sm = sm * qdiv
  e1 = q3sq - e1c*ghel*a2fac * qdiv**2
  e2 = q3sq - e2c*ghel*a2fac * qdiv**2
  e3 = e1 + e3c*ghel*a2fac**2 * qdiv**2
  e4 = e1 - e4c*ghel*a2fac * qdiv**2
  eden = e2*e4 + e3*e5c*gmel * qdiv**2
  eden = MAX( eden, 1.0d-20 )
 else
  e1 = q3sq - e1c*ghel*a2fac
  e2 = q3sq - e2c*ghel*a2fac
  e3 = e1 + e3c*ghel*a2fac**2
  e4 = e1 - e4c*ghel*a2fac
  eden = e2*e4 + e3*e5c*gmel
  eden = MAX( eden, 1.0d-20 )
  qdiv = 1.0
  sm = q3sq*a1*( e3-3.0*c1*e4 )/eden
  sh = q3sq*(a2*a2fac)*( e2+3.0*c1*e5c*gmel )/eden
 endif
 sh25max = 4.
 sh25min = 0.0
 IF ( sh > sh25max ) sh = sh25max
 IF ( sh < sh25min ) sh = sh25min
 shb = max(sh, 0.002)
 sm = MIN(sm, Prlimit*shb)
 gamt = 0.0
 gamq = 0.0
 gamv = 0.0
 cldavg = 0.5*(ms(mod(c-1,nch)+1,k-1,i_cld) + ms(mod(c-1,nch)+1,k,i_cld))
 IF (ea_k > 0.001 .OR. cldavg > 0.02) THEN
  sm = MAX(sm, 0.03*MIN(10.*ea_k*ew_k,1.0) )
  sh = MAX(sh, 0.03*MIN(10.*ea_k*ew_k,1.0) )
  sm = MAX(sm, 0.05*MIN(cldavg,1.0) )
  sh = MAX(sh, 0.05*MIN(cldavg,1.0) )
 endif
 elq = el*qkw
 elh = elq*qdiv
 ms(mod(c-1,nch)+1,k,i_pdk) = elq*( sm*gm +sh*gh+gamv ) + 0.5*zr
 ms(mod(c-1,nch)+1,k,i_pdt) = elh*( sh*dtl+gamt )*dtl
 ms(mod(c-1,nch)+1,k,i_pdq) = elh*( sh*dqw+gamq )*dqw
 ms(mod(c-1,nch)+1,k,i_pdc) = elh*( sh*dtl+gamt ) *dqw*0.5 + elh*( sh*dqw+gamq )*dtl*0.5
 if (k == nz) then
  ms(mod(c-1,nch)+1,k,i_tcd) = 0.0
 else
  ms(mod(c-1,nch)+1,k,i_tcd) = elq*gamt
 end if
 ms(mod(c-1,nch)+1,k,i_dfm) = elq*sm / dzk
 ms(mod(c-1,nch)+1,k,i_dfh) = elq*sh / dzk
 ms(mod(c-1,nch)+1,k,i_el) = el
 ms(mod(c-1,nch)+1,k,i_sh) = sh
 ms(mod(c-1,nch)+1,k,i_sm) = sm
 end subroutine

 ! ---- pred0: mym_predict's tsq, qsq, cov (from qke before the solve) -------------------------------------------------------------
 subroutine predict_point(c, k)
 !$acc routine seq
 integer, intent(in) :: c, k
 integer :: kk
 real(kind_phys) :: qkw, b2l, pdq0, pdt0, pdc0
 kk = min(k, nz-1)
 qkw = SQRT( MAX( ms(mod(c-1,nch)+1,kk,i_qke), 0.0 ) )
 if (kk == 1) then
  pdq0 = ms(mod(c-1,nch)+1,2,i_pdq); pdt0 = ms(mod(c-1,nch)+1,2,i_pdt); pdc0 = ms(mod(c-1,nch)+1,2,i_pdc)
 else
  pdq0 = ms(mod(c-1,nch)+1,kk,i_pdq); pdt0 = ms(mod(c-1,nch)+1,kk,i_pdt); pdc0 = ms(mod(c-1,nch)+1,kk,i_pdc)
 end if
 IF ( qkw .LE. 0.0 ) THEN
  b2l = 0.0
 else
  b2l = b2*0.25*( ms(mod(c-1,nch)+1,kk+1,i_el)+ms(mod(c-1,nch)+1,kk,i_el) )/qkw
 endif
 ms(mod(c-1,nch)+1,k,i_qsq) = b2l*( ms(mod(c-1,nch)+1,kk+1,i_pdq)+pdq0 )
 ms(mod(c-1,nch)+1,k,i_tsq) = b2l*( ms(mod(c-1,nch)+1,kk+1,i_pdt)+pdt0 )
 ms(mod(c-1,nch)+1,k,i_cov) = b2l*( ms(mod(c-1,nch)+1,kk+1,i_pdc)+pdc0 )
 end subroutine

 ! mym_predict's rhoz(k) (k <= kte) and the diffusivities of mynn_tendencies and mym_predict at k (k <= kte+1)
 real(kind_phys) function rhoz_at(c, k)
 !$acc routine seq
 integer, intent(in) :: c, k
 if (k == 1) then
  rhoz_at = ms(mod(c-1,nch)+1,1,i_rho)
 else
  rhoz_at =(ms(mod(c-1,nch)+1,k,i_rho)*ms(mod(c-1,nch)+1,k-1,i_dz) + ms(mod(c-1,nch)+1,k-1,i_rho)*ms(mod(c-1,nch)+1,k,i_dz))/(ms(mod(c-1,nch)+1,k-1,i_dz)+ms(mod(c-1,nch)+1,k,i_dz))
  rhoz_at = MAX(rhoz_at,1E-4)
 end if
 end function

 ! kdz(k) = rhoz(k)*df(k) with MYNN's lower bounds from s_aw at 2..kte-1; kte+1: rhoz(kte)*df(kte). f: i_dfm or i_dfh, fac 1 or Sqfac
 real(kind_phys) function kdz_at(c, k, f, sq, mf)
 !$acc routine seq
 integer, intent(in) :: c, k, f
 logical, intent(in) :: sq, mf
 real(kind_phys) :: df, saw0, saw1
 if (k == nz+1) then
  if (sq) then
   df = Sqfac*ms(mod(c-1,nch)+1,nz,f)
  else
   df = ms(mod(c-1,nch)+1,nz,f)
  end if
  kdz_at = rhoz_at(c,nz)*df
  return
 end if
 if (sq) then
  df = Sqfac*ms(mod(c-1,nch)+1,k,f)
 else
  df = ms(mod(c-1,nch)+1,k,f)
 end if
 kdz_at = rhoz_at(c,k)*df
 if (k >= 2 .and. k <= nz-1) then
  if (mf) then
   saw0 = ms(mod(c-1,nch)+1,k,i_saw); saw1 = ms(mod(c-1,nch)+1,k+1,i_saw)
  else
   saw0 = 0.; saw1 = 0.
  end if
  kdz_at = MAX(kdz_at, 0.5* saw0)
  kdz_at = MAX(kdz_at, -0.5*(saw0-saw1))
 end if
 end function

 real(kind_phys) function rhoinv_at(c, k)
 !$acc routine seq
 integer, intent(in) :: c, k
 if (k == 1) then
  rhoinv_at = 1./ms(mod(c-1,nch)+1,1,i_rho)
 else
  rhoinv_at = 1./MAX(ms(mod(c-1,nch)+1,k,i_rho),1E-4)
 end if
 end function

 real(kind_phys) function saw_at(c, k, f, mf)
 !$acc routine seq
 integer, intent(in) :: c, k, f
 logical, intent(in) :: mf
 if (mf) then
  saw_at = ms(mod(c-1,nch)+1,k,f)
 else
  saw_at = 0.
 end if
 end function

 ! ---- pred1: mym_predict's qke solve (tridiag2: forward sweep with the coefficients of each level, then back) -----------------
 subroutine predict_solve(state, c)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c
 real(kind_phys) :: tri_cp(max_levels), tri_dp(max_levels)
 real(kind_phys) :: delt, ust, pmz, vkz, pdk1, onoff, dtz, rhoinv, kq0, kq1, s0, s1, qkw, b1l, bp, rp, pdk0, pdk2
 real(kind_phys) :: a, b, cc, d, m, x, qke1
 integer :: k
 logical :: mf
 mf = mi(mod(c-1,nch)+1,m_mf) /= 0
 delt = dt
 ust = state%ust(c)
 pmz = mc(mod(c-1,nch)+1,c_pmz)
 onoff=0.0
 vkz = karman*0.5*ms(mod(c-1,nch)+1,1,i_dz)
 pdk1 = 2.0*ust**3*pmz/( vkz )
 kq1 = kdz_at(c, 1, i_dfm, .true., mf)
 s1 = saw_at(c, 1, i_saw, mf)
 do k = 1, nz
  kq0 = kq1
  s0 = s1
  if (k < nz) then
   kq1 = kdz_at(c, k+1, i_dfm, .true., mf)
   s1 = saw_at(c, k+1, i_saw, mf)
   dtz = delt/ms(mod(c-1,nch)+1,k,i_dz)
   rhoinv = rhoinv_at(c,k)
   qkw = SQRT( MAX( ms(mod(c-1,nch)+1,k,i_qke), 0.0 ) )
   b1l = b1*0.5*( ms(mod(c-1,nch)+1,k+1,i_el)+ms(mod(c-1,nch)+1,k,i_el) )
   bp = 2.*qkw / b1l
   if (k == 1) then
    pdk0 = pdk1 - ms(mod(c-1,nch)+1,2,i_pdk)
   else
    pdk0 = ms(mod(c-1,nch)+1,k,i_pdk)
   end if
   pdk2 = ms(mod(c-1,nch)+1,k+1,i_pdk)
   rp = pdk2 + pdk0
   a= - dtz*kq0*rhoinv + 0.5*dtz*rhoinv*s0*onoff
   b=1. + dtz*(kq0+kq1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1)*onoff + bp*delt
   cc= - dtz*kq1*rhoinv - 0.5*dtz*rhoinv*s1*onoff
   d=rp*delt + ms(mod(c-1,nch)+1,k,i_qke) + dtz*rhoinv*(zr-zr)*onoff
  else
   a=0.
   b=1.
   cc=0.
   d=ms(mod(c-1,nch)+1,nz,i_qke)
  end if
  if (k == 1) then
   tri_cp(1) = cc/b
   tri_dp(1) = d/b
  else
   m = b-tri_cp(k-1)*a
   tri_cp(k) = cc/m
   tri_dp(k) = (d-tri_dp(k-1)*a)/m
  end if
 end do
 x = tri_dp(nz)
 qke1 = max(x, 1.e-4)
 ms(mod(c-1,nch)+1,nz,i_qke) = min(qke1, 150.)
 do k = nz-1, 1, -1
  x = tri_dp(k)-tri_cp(k)*x
  qke1 = max(x, 1.e-4)
  ms(mod(c-1,nch)+1,k,i_qke) = min(qke1, 150.)
 end do
 end subroutine

 ! ---- tend: one of mynn_tendencies' solves (v: 1 u, 2 v, 3 thl, 4 sqc, 5 sqv, 6 sqi, 7 qni) ---------------------------------------
 subroutine tendency_solve(state, c, v)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, v
 real(kind_phys) :: tri_cp(max_levels), tri_dp(max_levels)
 real(kind_phys) :: delt, ust, wspd, flt, flqv, flqc, uoce, voce, onoff, rhosfc, qvflux, dtz, rhoinv, kd0, kd1
 real(kind_phys) :: s0, s1, f0, f1, a, b, cc, d, m, x, tcd, qcd, diss_heat, zero, tk1, qv1
 real(kind_phys), parameter ::nonloc = 1.0
 integer :: k, fdf
 logical :: mf
 mf = mi(mod(c-1,nch)+1,m_mf) /= 0
 delt = dt
 ust = state%ust(c)
 wspd = state%wspd(c)
 flt = mc(mod(c-1,nch)+1,c_flt)
 flqv = mc(mod(c-1,nch)+1,c_flqv)
 flqc = 0.0
 uoce = 0.
 voce = 0.
 zero = zr
 onoff=0.0
#ifdef LEAN_MYNN_RHOSFC
 rhosfc = ms(mod(c-1,nch)+1,1,i_rho)
#else
 tk1 = ms(mod(c-1,nch)+1,1,i_th)*ms(mod(c-1,nch)+1,1,i_ex)
 qv1 = mc(mod(c-1,nch)+1,c_qv1)
 rhosfc = state%psfc(c)/(R_d*(tk1+p608*qv1))
#endif
 if (v <= 2) then
  fdf = i_dfm
 else
  fdf = i_dfh
 end if
 kd1 = kdz_at(c, 1, fdf, .false., mf)
 s1 = saw_at(c, 1, i_saw, mf)
 select case (v)
 case (3); f1 = saw_at(c, 1, i_sawthl, mf)
 case (4); f1 = saw_at(c, 1, i_sawqc, mf)
 case (5); f1 = saw_at(c, 1, i_sawqv, mf)
 case (7); f1 = saw_at(c, 1, i_sawqni, mf)
 case default; f1 = 0.
 end select
 do k = 1, nz
  kd0 = kd1
  s0 = s1
  f0 = f1
  kd1 = kdz_at(c, k+1, fdf, .false., mf)
  if (k < nz) then
   s1 = saw_at(c, k+1, i_saw, mf)
   select case (v)
   case (3); f1 = saw_at(c, k+1, i_sawthl, mf)
   case (4); f1 = saw_at(c, k+1, i_sawqc, mf)
   case (5); f1 = saw_at(c, k+1, i_sawqv, mf)
   case (7); f1 = saw_at(c, k+1, i_sawqni, mf)
   case default; f1 = 0.
   end select
  end if
  dtz = delt/ms(mod(c-1,nch)+1,k,i_dz)
  rhoinv = rhoinv_at(c,k)
  if (k == nz) then
   a = 0.
   b = 1.
   cc = 0.
   select case (v)
   case (1); d = ms(mod(c-1,nch)+1,k,i_u)
   case (2); d = ms(mod(c-1,nch)+1,k,i_v)
   case (3); d = ms(mod(c-1,nch)+1,k,i_thl)
   case (4); d = ms(mod(c-1,nch)+1,k,i_sqc)
   case (5); d = ms(mod(c-1,nch)+1,k,i_sqv)
   case (6); d = ms(mod(c-1,nch)+1,k,i_sqi)
   case default; d = ms(mod(c-1,nch)+1,k,i_qni)
   end select
  else if (k == 1) then
   select case (v)
   case (1)
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+rhosfc*ust**2/wspd)*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    d=ms(mod(c-1,nch)+1,k,i_u) + dtz*uoce*ust**2/wspd - dtz*rhoinv*zero*onoff + dtz*rhoinv*zero*onoff + zero*delt + zero*delt
   case (2)
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1 + rhosfc*ust**2/wspd)*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    d=ms(mod(c-1,nch)+1,k,i_v) + dtz*voce*ust**2/wspd - dtz*rhoinv*zero*onoff + dtz*rhoinv*zero*onoff + zero*delt + zero*delt
   case (3)
    tcd = (tcd_raw(c,k+1,i_tcd)-tcd_raw(c,k,i_tcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    diss_heat = diss_at(c,k)
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+kd0)*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_thl) + dtz*rhosfc*flt*rhoinv + tcd*delt - dtz*rhoinv*f1 -dtz*rhoinv*zero + diss_heat*delt + zero*delt + zero*delt
   case (4)
    qcd = (tcd_raw(c,k+1,i_qcd)-tcd_raw(c,k,i_qcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+kd0)*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_sqc) + dtz*rhosfc*flqc*rhoinv + qcd*delt - dtz*rhoinv*f1 - dtz*rhoinv*zero + zero*delt
   case (5)
    qcd = (tcd_raw(c,k+1,i_qcd)-tcd_raw(c,k,i_qcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    qvflux = flqv
    if (qvflux < 0.0) then
#ifdef LEAN_MYNN_DEW
     qvflux = max(qvflux, -max(0.9*ms(mod(c-1,nch)+1,1,i_sqv) - 1e-8, 0.0)/(dtz*rhosfc*rhoinv))
#else
     qvflux = max(qvflux, (min(0.9*ms(mod(c-1,nch)+1,1,i_sqv) - 1e-8, 0.0)/dtz))
#endif
    endif
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+kd0)*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_sqv) + dtz*rhosfc*qvflux*rhoinv + qcd*delt - dtz*rhoinv*f1 - dtz*rhoinv*zero + zero*delt + zero*delt
   case (6)
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+kd0)*rhoinv
    cc= -dtz*kd1*rhoinv
    d=ms(mod(c-1,nch)+1,k,i_sqi)
   case default
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd1+kd0)*rhoinv - 0.5*dtz*rhoinv*s1*nonloc
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*nonloc
    d=ms(mod(c-1,nch)+1,k,i_qni) - dtz*rhoinv*f1*nonloc
   end select
  else
   select case (v)
   case (1)
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0*onoff + 0.5*dtz*rhoinv*zero*onoff
    b=1.+ dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1)*onoff + 0.5*dtz*rhoinv*(zero-zero)*onoff
    cc= - dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    d=ms(mod(c-1,nch)+1,k,i_u) + dtz*rhoinv*(zero-zero)*onoff - dtz*rhoinv*(zero-zero)*onoff + zero*delt + zero*delt
   case (2)
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0*onoff + 0.5*dtz*rhoinv*zero*onoff
    b=1.+dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1)*onoff + 0.5*dtz*rhoinv*(zero-zero)*onoff
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*onoff - 0.5*dtz*rhoinv*zero*onoff
    d=ms(mod(c-1,nch)+1,k,i_v) + dtz*rhoinv*(zero-zero)*onoff - dtz*rhoinv*(zero-zero)*onoff + zero*delt + zero*delt
   case (3)
    tcd = (tcd_raw(c,k+1,i_tcd)-tcd_raw(c,k,i_tcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    diss_heat = diss_at(c,k)
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0 + 0.5*dtz*rhoinv*zero
    b=1.+dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1) + 0.5*dtz*rhoinv*(zero-zero)
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_thl) + tcd*delt + dtz*rhoinv*(f0-f1) + dtz*rhoinv*(zero-zero) + diss_heat*delt + zero*delt + zero*delt
   case (4)
    qcd = (tcd_raw(c,k+1,i_qcd)-tcd_raw(c,k,i_qcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0 + 0.5*dtz*rhoinv*zero
    b=1.+dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1) + 0.5*dtz*rhoinv*(zero-zero)
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_sqc) + qcd*delt + dtz*rhoinv*(f0-f1) + dtz*rhoinv*(zero-zero) + zero*delt
   case (5)
    qcd = (tcd_raw(c,k+1,i_qcd)-tcd_raw(c,k,i_qcd))/( ms(mod(c-1,nch)+1,k,i_dz) )
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0 + 0.5*dtz*rhoinv*zero
    b=1.+dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1) + 0.5*dtz*rhoinv*(zero-zero)
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1 - 0.5*dtz*rhoinv*zero
    d=ms(mod(c-1,nch)+1,k,i_sqv) + qcd*delt + dtz*rhoinv*(f0-f1) + dtz*rhoinv*(zero-zero) + zero*delt + zero*delt
   case (6)
    a= -dtz*kd0*rhoinv
    b=1.+dtz*(kd0+kd1)*rhoinv
    cc= -dtz*kd1*rhoinv
    d=ms(mod(c-1,nch)+1,k,i_sqi)
   case default
    a= -dtz*kd0*rhoinv + 0.5*dtz*rhoinv*s0*nonloc
    b=1.+dtz*(kd0+kd1)*rhoinv + 0.5*dtz*rhoinv*(s0-s1)*nonloc
    cc= -dtz*kd1*rhoinv - 0.5*dtz*rhoinv*s1*nonloc
    d=ms(mod(c-1,nch)+1,k,i_qni) + dtz*rhoinv*(f0-f1)*nonloc
   end select
  end if
  if (k == 1) then
   tri_cp(1) = cc/b
   tri_dp(1) = d/b
  else
   m = b-tri_cp(k-1)*a
   tri_cp(k) = cc/m
   tri_dp(k) = (d-tri_dp(k-1)*a)/m
  end if
 end do
 x = tri_dp(nz)
 call tendency_result(c, nz, v, x, delt)
 do k = nz-1, 1, -1
  x = tri_dp(k)-tri_cp(k)*x
  call tendency_result(c, k, v, x, delt)
 end do
 end subroutine

 subroutine tendency_result(c, k, v, x, delt)
 !$acc routine seq
 integer, intent(in) :: c, k, v
 real(kind_phys), intent(in) :: x, delt
 select case (v)
 case (1); ms(mod(c-1,nch)+1,k,i_du)=(x-ms(mod(c-1,nch)+1,k,i_u))/delt
 case (2); ms(mod(c-1,nch)+1,k,i_dv)=(x-ms(mod(c-1,nch)+1,k,i_v))/delt
 case (3); ms(mod(c-1,nch)+1,k,i_thl2)=x
 case (4); ms(mod(c-1,nch)+1,k,i_sqc2)=x
 case (5); ms(mod(c-1,nch)+1,k,i_sqv2)=x
 case (6); ms(mod(c-1,nch)+1,k,i_sqi2)=x
 case default; ms(mod(c-1,nch)+1,k,i_dqni)=(x-ms(mod(c-1,nch)+1,k,i_qni))/delt
 end select
 end subroutine

 ! mym_turbulence's tcd/qcd before its difference loop (f: i_tcd or i_qcd)
 real(kind_phys) function tcd_raw(c, k, f)
 !$acc routine seq
 integer, intent(in) :: c, k, f
 tcd_raw = ms(mod(c-1,nch)+1,k,f)
 end function

 ! bl_mynn_run's diss_heat at k (k <= kte-1), from qke after mym_predict
 real(kind_phys) function diss_at(c, k)
 !$acc routine seq
 integer, intent(in) :: c, k
 diss_at = min(max(1.0*(ms(mod(c-1,nch)+1,k,i_qke)**1.5)/(b1*max(0.5*(ms(mod(c-1,nch)+1,k,i_el)+ms(mod(c-1,nch)+1,k+1,i_el)),1.))/cp,0.0),0.002)
 diss_at = diss_at * exp(-10000./MAX(ms(mod(c-1,nch)+1,k,i_p),1.))
 end function

 ! ---- moist: mynn_tendencies after the solves: Dqv..Dqni, moisture_check (from the top down), Dth --------------------------------
 real(kind_phys) function delp_at(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 integer :: kk
 kk = min(k, nz-1)
 if (kk == 1) then
#ifdef LEAN_MYNN_HYD_PS
  delp_at = ms(mod(c-1,nch)+1,1,i_p) + 0.5*grav*ms(mod(c-1,nch)+1,1,i_rho)*ms(mod(c-1,nch)+1,1,i_dz) - (ms(mod(c-1,nch)+1,2,i_p)*ms(mod(c-1,nch)+1,1,i_dz) + ms(mod(c-1,nch)+1,1,i_p)*ms(mod(c-1,nch)+1,2,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
#else
  delp_at = state%psfc(c) - (ms(mod(c-1,nch)+1,2,i_p)*ms(mod(c-1,nch)+1,1,i_dz) + ms(mod(c-1,nch)+1,1,i_p)*ms(mod(c-1,nch)+1,2,i_dz))/(ms(mod(c-1,nch)+1,1,i_dz)+ms(mod(c-1,nch)+1,2,i_dz))
#endif
 else
  delp_at = (ms(mod(c-1,nch)+1,kk,i_p)*ms(mod(c-1,nch)+1,kk-1,i_dz) + ms(mod(c-1,nch)+1,kk-1,i_p)*ms(mod(c-1,nch)+1,kk,i_dz))/(ms(mod(c-1,nch)+1,kk,i_dz)+ms(mod(c-1,nch)+1,kk-1,i_dz)) - &
   (ms(mod(c-1,nch)+1,kk+1,i_p)*ms(mod(c-1,nch)+1,kk,i_dz) + ms(mod(c-1,nch)+1,kk,i_p)*ms(mod(c-1,nch)+1,kk+1,i_dz))/(ms(mod(c-1,nch)+1,kk,i_dz)+ms(mod(c-1,nch)+1,kk+1,i_dz))
 end if
 end function

 ! moist per point: moisture_check's level k with the corrections from the level above as the column loop forms them when
 ! that level filled no vapour (dqv2 = +0, here zr). A level that fills vapour (dqv2 > 0) marks its column, and the column
 ! is done again by moisture (from the top down, as moisture_check), which writes every result of the column again.
 subroutine moisture_point(state, c, k)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c, k
 real(kind_phys) :: delt, qv, qc, qi, qs, th, dqv, dqc, dqi, dqs, dth, dpk, dpk1, carry, carry_d, sqs, thl0
 real(kind_phys)::dqc2,dqi2,dqs2,dqv2,t
 real(kind_phys), parameter ::qvmin = 1e-20,qcmin = 0.0,qimin = 0.0
 delt = dt
 dqv=(ms(mod(c-1,nch)+1,k,i_sqv2) - ms(mod(c-1,nch)+1,k,i_sqv))/delt
 dqc=(ms(mod(c-1,nch)+1,k,i_sqc2) - ms(mod(c-1,nch)+1,k,i_sqc))/delt
 dqi=(ms(mod(c-1,nch)+1,k,i_sqi2) - ms(mod(c-1,nch)+1,k,i_sqi))/delt
 dqs = 0.
 dth = 0.0
 qv = ms(mod(c-1,nch)+1,k,i_sqv2)
 qc = ms(mod(c-1,nch)+1,k,i_sqc2)
 qi = ms(mod(c-1,nch)+1,k,i_sqi2)
 qs = zr
 thl0 = ms(mod(c-1,nch)+1,k,i_thl2)
 th = thl0
 if (k /= nz) then
  dpk1 = delp_at(state, c, k+1)
  dpk = delp_at(state, c, k)
  carry = zr*dpk1/dpk
  carry_d = zr*dpk1/dpk/delt
  qv = qv - carry
  dqv = dqv - carry_d
 end if
 dqc2 = max(0.0, qcmin-qc)
 dqi2 = max(0.0, qimin-qi)
 dqs2 = max(0.0, qimin-qs)
 dqc = dqc + dqc2/delt
 dqi = dqi + dqi2/delt
 dqs = dqs + dqs2/delt
 dqv = dqv - (dqc2+dqi2+dqs2)/delt
 dth = dth + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*(dqc2/delt) + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*((dqi2+dqs2)/delt)
 qc = qc + dqc2
 qi = qi + dqi2
 qs = qs + dqs2
 qv = qv - dqc2 - dqi2 - dqs2
 th = th + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*dqc2 + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*(dqi2+dqs2)
 dqv2 = max(0.0, qvmin-qv)
 if (dqv2 > 0.) mi(mod(c-1,nch)+1,m_moist) = 1
 dqv = dqv + dqv2/delt
 qc = max(qc,qcmin)
 qi = max(qi,qimin)
#ifdef LEAN_MYNN_FILL_THL
 t = thl0
#else
 t = th
#endif
 sqs = zr
 ms(mod(c-1,nch)+1,k,i_dth)=(t + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*qc + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*(qi+sqs) - ms(mod(c-1,nch)+1,k,i_th))/delt
 ms(mod(c-1,nch)+1,k,i_dqc) = dqc
 ms(mod(c-1,nch)+1,k,i_dqi) = dqi
 ms(mod(c-1,nch)+1,k,i_dqs) = dqs
 ms(mod(c-1,nch)+1,k,i_dqv) = dqv
 end subroutine

 subroutine moisture(state, c)
 !$acc routine seq
 type(state_type), intent(in) :: state
 integer, intent(in) :: c
 integer :: k
 real(kind_phys) :: delt, qv, qc, qi, qs, th, dqv, dqc, dqi, dqs, dth, dpk, dpkm1, carry, carry_d, sqs, thl0
 real(kind_phys)::dqc2,dqi2,dqs2,dqv2,sum,aa,dum,t
 real(kind_phys), parameter ::qvmin = 1e-20,qcmin = 0.0,qimin = 0.0
 delt = dt
 carry = 0.
 carry_d = 0.
 dpk = delp_at(state, c, nz)
 do k = nz, 1, -1
  ! Dqv..Dqs of mynn_tendencies, then dqv(k) and qv(k) as the level above left them
  dqv=(ms(mod(c-1,nch)+1,k,i_sqv2) - ms(mod(c-1,nch)+1,k,i_sqv))/delt
  dqc=(ms(mod(c-1,nch)+1,k,i_sqc2) - ms(mod(c-1,nch)+1,k,i_sqc))/delt
  dqi=(ms(mod(c-1,nch)+1,k,i_sqi2) - ms(mod(c-1,nch)+1,k,i_sqi))/delt
  dqs = 0.
  dth = 0.0
  qv = ms(mod(c-1,nch)+1,k,i_sqv2)
  qc = ms(mod(c-1,nch)+1,k,i_sqc2)
  qi = ms(mod(c-1,nch)+1,k,i_sqi2)
  qs = zr
  thl0 = ms(mod(c-1,nch)+1,k,i_thl2)
  th = thl0
  if (k /= nz) then
   qv = qv - carry
   dqv = dqv - carry_d
  end if
  dqc2 = max(0.0, qcmin-qc)
  dqi2 = max(0.0, qimin-qi)
  dqs2 = max(0.0, qimin-qs)
  dqc = dqc + dqc2/delt
  dqi = dqi + dqi2/delt
  dqs = dqs + dqs2/delt
  dqv = dqv - (dqc2+dqi2+dqs2)/delt
  dth = dth + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*(dqc2/delt) + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*((dqi2+dqs2)/delt)
  qc = qc + dqc2
  qi = qi + dqi2
  qs = qs + dqs2
  qv = qv - dqc2 - dqi2 - dqs2
  th = th + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*dqc2 + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*(dqi2+dqs2)
  dqv2 = max(0.0, qvmin-qv)
  dqv = dqv + dqv2/delt
  qv = qv + dqv2
  if( k .ne. 1 ) then
   dpkm1 = delp_at(state, c, k-1)
   carry = dqv2*dpk/dpkm1
   carry_d = dqv2*dpk/dpkm1/delt
   dpk = dpkm1
  endif
  qv = max(qv,qvmin)
  qc = max(qc,qcmin)
  qi = max(qi,qimin)
  qs = max(qs,qimin)
  ! Dth of mynn_tendencies (thl after the check, or without it under LEAN_MYNN_FILL_THL), the results of the level
#ifdef LEAN_MYNN_FILL_THL
  t = thl0
#else
  t = th
#endif
  sqs = zr
  ms(mod(c-1,nch)+1,k,i_dth)=(t + xlvcp/ms(mod(c-1,nch)+1,k,i_ex)*qc + xlscp/ms(mod(c-1,nch)+1,k,i_ex)*(qi+sqs) - ms(mod(c-1,nch)+1,k,i_th))/delt
  ms(mod(c-1,nch)+1,k,i_dqc) = dqc
  ms(mod(c-1,nch)+1,k,i_dqi) = dqi
  ms(mod(c-1,nch)+1,k,i_dqs) = dqs
  ms(mod(c-1,nch)+1,k,i_dqv) = dqv
  ms(mod(c-1,nch)+1,k,i_sqv2) = qv
 end do
 if( dqv2 .gt. 1.e-20 ) then
  sum = 0.0
  do k = 1, nz
   if( ms(mod(c-1,nch)+1,k,i_sqv2) .gt. 2.0*qvmin ) sum = sum + ms(mod(c-1,nch)+1,k,i_sqv2)*delp_at(state, c, k)
  enddo
  aa = dqv2*delp_at(state, c, 1)/max(1.e-20,sum)
  if( aa .lt. 0.5 ) then
   do k = 1, nz
    if( ms(mod(c-1,nch)+1,k,i_sqv2) .gt. 2.0*qvmin ) then
     dum = aa*ms(mod(c-1,nch)+1,k,i_sqv2)
     ms(mod(c-1,nch)+1,k,i_sqv2) = ms(mod(c-1,nch)+1,k,i_sqv2) - dum
     ms(mod(c-1,nch)+1,k,i_dqv) = ms(mod(c-1,nch)+1,k,i_dqv) - dum/delt
    endif
   enddo
  endif
 endif
 end subroutine

 ! ---- store: the state's outputs, pbl_mixing_tendencies, retrieve_exchange_coeffs, add_pbl at level k --------------------------
 subroutine store_point(state, c, k, init, mass, tend_th, tend_scalars)
 !$acc routine seq
 type(state_type), intent(inout) :: state
 integer, intent(in) :: c, k
 logical, intent(in) :: init
 real, intent(in) :: mass(nz,*)
 real, intent(inout) :: tend_th(nz,*), tend_scalars(num_scalars,nz,*)
 real(kind_phys) :: dzk, dqv, dqc, dqi, dqs, delt
#ifndef LEAN_MYNN_MOIST
 real :: sq, rq, sqv
#endif
 delt = dt
 state%qke(k,c) = ms(mod(c-1,nch)+1,k,i_qke)
 state%tsq(k,c) = ms(mod(c-1,nch)+1,k,i_tsq)
 state%qsq(k,c) = ms(mod(c-1,nch)+1,k,i_qsq)
 state%cov(k,c) = ms(mod(c-1,nch)+1,k,i_cov)
 state%el_pbl(k,c) = ms(mod(c-1,nch)+1,k,i_el)
 if (k >= 2 .or. init) then
  state%sh(k,c) = ms(mod(c-1,nch)+1,k,i_sh)
  state%sm(k,c) = ms(mod(c-1,nch)+1,k,i_sm)
 end if
 state%qc_bl(k,c) = ms(mod(c-1,nch)+1,k,i_qcbl)
 state%qi_bl(k,c) = ms(mod(c-1,nch)+1,k,i_qibl)
 state%cldfra_bl(k,c) = ms(mod(c-1,nch)+1,k,i_cld)
 if (k == 1) then
  state%exch_m(k,c)=0.
  state%exch_h(k,c)=0.
 else
  dzk = 0.5 *( ms(mod(c-1,nch)+1,k,i_dz)+ms(mod(c-1,nch)+1,k-1,i_dz) )
  state%exch_m(k,c)=ms(mod(c-1,nch)+1,k,i_dfm)*dzk
  state%exch_h(k,c)=ms(mod(c-1,nch)+1,k,i_dfh)*dzk
 end if
 state%t_u(k,c) = ms(mod(c-1,nch)+1,k,i_du)
 state%t_v(k,c) = ms(mod(c-1,nch)+1,k,i_dv)
 state%t_theta(k,c) = ms(mod(c-1,nch)+1,k,i_dth)
 state%t_ni(k,c) = ms(mod(c-1,nch)+1,k,i_dqni)
 dqv = ms(mod(c-1,nch)+1,k,i_dqv)
 dqc = ms(mod(c-1,nch)+1,k,i_dqc)
 dqi = ms(mod(c-1,nch)+1,k,i_dqi)
 dqs = ms(mod(c-1,nch)+1,k,i_dqs)
#ifdef LEAN_MYNN_MOIST
 dqv=dqv*(1.+state%qv(k,c))
 dqc=dqc*(1.+state%qv(k,c))
 dqi=dqi*(1.+state%qv(k,c))
 dqs=dqs*(1.+state%qv(k,c))
#else
 sq=state%qv(k,c)/(1.+state%qv(k,c))
 sqv=sq+dqv*delt
 rq=sqv/(1.-sqv)
 dqv=(rq-state%qv(k,c))/delt
 sq=state%qc(k,c)/(1.+state%qv(k,c))
 rq=(sq+dqc*delt)*(1.+sqv)
 dqc=(rq-state%qc(k,c))/delt
 sq=state%qi(k,c)/(1.+state%qv(k,c))
 rq=(sq+dqi*delt)*(1.+sqv)
 dqi=(rq-state%qi(k,c))/delt
 sq=state%qs(k,c)/(1.+state%qv(k,c))
 rq=(sq+dqs*delt)*(1.+sqv)
 dqs=(rq-state%qs(k,c))/delt
#endif
 state%t_qv(k,c) = dqv
 state%t_qc(k,c) = dqc
 state%t_qi(k,c) = dqi
 state%t_qs(k,c) = dqs
 tend_th(k,c) = tend_th(k,c) + ms(mod(c-1,nch)+1,k,i_dth)*mass(k,c)
 tend_scalars(i_qv,k,c) = tend_scalars(i_qv,k,c) + dqv*mass(k,c)
 tend_scalars(i_qc,k,c) = tend_scalars(i_qc,k,c) + dqc*mass(k,c)
 tend_scalars(i_qi,k,c) = tend_scalars(i_qi,k,c) + dqi*mass(k,c)
 tend_scalars(i_qs,k,c) = tend_scalars(i_qs,k,c) + dqs*mass(k,c)
 tend_scalars(i_ni,k,c) = tend_scalars(i_ni,k,c) + ms(mod(c-1,nch)+1,k,i_dqni)*mass(k,c)
 end subroutine

end module mynn_card
