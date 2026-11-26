!! PUBLIC: thompson_card_start, th_effect_rad, th_reset, th_marks_a, th_marks_b, th_moments, th_n0_level, th_n0_scan, th_graupel_level, th_fall_level, th_fall_species, th_sediment
!! DECLARATIONS
      ! HAND-WRITTEN (not generated from the original): tools/thompson_card.f90, put into this module by tools/gen_device_physics.py
      ! (thompson_card). The column parts of the Thompson stages (mp_thompson_a2, c1, c2) as point and column pieces that the card
      ! runs over many threads (src/dyn_micro.f90 micro_staged). Every statement is the generated stage's statement on the same
      ! values (the pools of micro_pool, named tp_/td_/tl_/tm_ there); only the loops are cut:
      ! - the moments (a2): snow moments, the graupel N0 of each level and the rain moments per (column, level) point; the running
      !   minimum of the graupel N0 from the top per column (a MIN, exact); lam_exp, ilamg, N0_g per point where a later statement
      !   reads them (rg > R1 or L_qg: every read of ilamg and N0_g is under rg > R1, rg >= r_g(1) > R1 or L_qg).
      ! - the fall speeds (c2): the transcendental part per point; per column the values a level without the species takes from
      !   the level above, ksed1 and nstep (maxima, exact); the sedimentation per (column, species): the species are independent
      !   there, each sub-step goes top-down with the flux of the level above in a register (a level's flux is formed from its value
      !   at the start of the sub-step, as in the original's sed_r array).
      ! - the rain moments of a level without rain (rr = R1, nr = R2) and the graupel N0 of a level without graupel or rain to
      !   count (rg <= 5e-5, xslw1 = 0.01) are the same numbers at every such point: computed once on the card with the same
      !   expressions (thompson_card_start), taken where the inputs are exactly those.
      ! th_n0 the graupel N0 of a level, then the running minimum (double); th_vt the fall speeds (slot, level: 1 vtrk 2 vtnrk
      ! 3 vtik 4 vtnik 5 vtsk 6 vtgk; level kte+1 zero); th_k0 the column's k_0; th_on onstep(1:4); th_ks ksed1(1:4); th_any
      ! ANY(L_qr, L_qi, L_qs, L_qg) after the update (b1; b2 changes only L_qc)
      DOUBLE PRECISION, ALLOCATABLE:: th_n0(:,:)
      REAL, ALLOCATABLE:: th_vt(:,:,:), th_on(:,:), th_xs(:,:), th_odz(:,:), th_orho(:,:)
      INTEGER, ALLOCATABLE:: th_k0(:), th_k0b(:), th_ks(:,:)
      LOGICAL, ALLOCATABLE:: th_any(:,:)
      DOUBLE PRECISION:: th_dry_ilamr, th_dry_n0r, th_dry_n0g
      REAL:: th_dry_mvdr
      INTEGER:: th_dry_idxn
      !$acc declare create(th_n0, th_xs, th_odz, th_orho, th_vt, th_on, th_k0, th_k0b, th_ks, th_any, th_dry_ilamr, th_dry_n0r, th_dry_n0g, th_dry_mvdr, th_dry_idxn)
!! ROUTINES
      ! the arrays of the hand-written stages for slots columns, and the constants of the dry points (rain_size_moments for
      ! rr = R1, nr = R2; the graupel N0 of snow_graupel_moments for xslw1 = 0.01, rg = R1; b1's idx_n for nc = 2), computed on
      ! the card
      subroutine thompson_card_start(slots)
      integer, intent(in) :: slots
      DOUBLE PRECISION:: lamr, N0_exp
      REAL:: rr1, nr1, rg1, xslw1, ygra1, zans1, mvd, nc1
      INTEGER:: idx_n
      if (allocated(th_n0)) return
      allocate(th_n0(slots, kte+1), th_xs(slots, kte), th_odz(slots, kte), th_orho(slots, kte), th_vt(slots, kte+1, 6), th_on(slots, 4), th_k0(slots), th_k0b(slots), th_ks(slots, 4), th_any(slots, 4))
      !$acc serial
      rr1 = R1
      nr1 = R2
      lamr = (am_r*crg(3)*org2*nr1/rr1)**obmr
      th_dry_ilamr = 1./lamr
      mvd = (3.0 + mu_r + 0.672) / lamr
      th_dry_mvdr = mvd
      th_dry_n0r = nr1*org2*lamr**cre(2)
      rg1 = R1
      xslw1 = 0.01
      ygra1 = 4.31 + alog10(max(5.E-5, rg1))
      zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
      N0_exp = 10.**(zans1)
      N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
      th_dry_n0g = N0_exp
      nc1 = 2.
      idx_n = NINT(1.0 + FLOAT(nbc) * DLOG(nc1/t_Nc(1)) / nic1)
      th_dry_idxn = idx_n
      !$acc end serial
      end subroutine thompson_card_start

      ! the graupel N0 of one level before the running minimum (snow_graupel_moments, update_size_distributions)
      subroutine th_n0_level(slot_, k, kb)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      logical, intent(in) :: kb   ! k_0 of c1 (th_k0b), else of a2 (th_k0)
      REAL:: xslw1, ygra1, zans1
      DOUBLE PRECISION:: N0_exp
      integer :: k_0
      k_0 = th_k0(slot_)
      if (kb) k_0 = th_k0b(slot_)
      if (.not. kb) then   ! a2: the rain's term from a1's mvd_r, kept by th_moments (th_xs, -1 without)
         if (k.gt.k_0 .and. th_xs(slot_,k) .gt. 0.) then
            xslw1 = th_xs(slot_,k)
         else
            if (pool_pr(slot_,k,tp_rg) .le. 5.E-5) then
               th_n0(slot_,k) = th_dry_n0g
               return
            endif
            xslw1 = 0.01
         endif
      elseif (k.gt.k_0 .and. pool_pl(slot_,k,tl_l_qr) .and. pool_pr(slot_,k,tp_mvd_r).gt.100.E-6) then
         xslw1 = 4.01 + alog10(pool_pr(slot_,k,tp_mvd_r))
      else
         if (pool_pr(slot_,k,tp_rg) .le. 5.E-5) then   ! max(5.E-5, rg) = 5.E-5, xslw1 = 0.01: the dry constant
            th_n0(slot_,k) = th_dry_n0g
            return
         endif
         xslw1 = 0.01
      endif
      ygra1 = 4.31 + alog10(max(5.E-5, pool_pr(slot_,k,tp_rg)))
      zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
      N0_exp = 10.**(zans1)
      N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
      th_n0(slot_,k) = N0_exp
      end subroutine th_n0_level

      ! the running minimum of the graupel N0 from the top (N0_min = MIN(N0_exp, N0_min); N0_exp = N0_min)
      subroutine th_n0_scan(slot_)
      !$acc routine seq
      integer, intent(in) :: slot_
      integer :: k
      DOUBLE PRECISION:: N0_min
      N0_min = gonv_max
      do k = kte, 1, -1
         N0_min = MIN(th_n0(slot_,k), N0_min)
         th_n0(slot_,k) = N0_min
      enddo
      end subroutine th_n0_scan

      ! lam_exp, ilamg, N0_g of one level from the running minimum, where they are read (rg > R1 or L_qg)
      subroutine th_graupel_level(slot_, k)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      DOUBLE PRECISION:: N0_exp, lam_exp, lamg
      if (.not. (pool_pr(slot_,k,tp_rg) .gt. R1 .or. pool_pl(slot_,k,tl_l_qg))) return
      N0_exp = th_n0(slot_,k)
      lam_exp = (N0_exp*am_g*cgg(1)/pool_pr(slot_,k,tp_rg))**oge1
      lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
      pool_pd(slot_,k,td_ilamg) = 1./lamg
      pool_pd(slot_,k,td_n0_g) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      end subroutine th_graupel_level

      ! mp_thompson_a2 at one level: the snow moments (L_qs), the graupel N0 before the minimum (it reads mvd_r before the rain
      ! moments renew it, as in a2), the rain moments (the dry constants where rr = R1 and nr = R2)
      subroutine th_moments(slot_, k)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      REAL:: a_, b_, loga_, tc0
      DOUBLE PRECISION:: lamr
      if (pool_pl(slot_,k,tl_l_qs)) then
         tc0 = MIN(-0.1, pool_pr(slot_,k,tp_temp)-273.15)
         pool_pr(slot_,k,tp_smob) = pool_pr(slot_,k,tp_rs)*oams
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            pool_pr(slot_,k,tp_smo2) = pool_pr(slot_,k,tp_smob)
         else
            loga_ = sa(1) + sa(2)*tc0 + sa(3)*bm_s &
               + sa(4)*tc0*bm_s + sa(5)*tc0*tc0 &
               + sa(6)*bm_s*bm_s + sa(7)*tc0*tc0*bm_s &
               + sa(8)*tc0*bm_s*bm_s + sa(9)*tc0*tc0*tc0 &
               + sa(10)*bm_s*bm_s*bm_s
            a_ = 10.0**loga_
            b_ = sb(1) + sb(2)*tc0 + sb(3)*bm_s &
               + sb(4)*tc0*bm_s + sb(5)*tc0*tc0 &
               + sb(6)*bm_s*bm_s + sb(7)*tc0*tc0*bm_s &
               + sb(8)*tc0*bm_s*bm_s + sb(9)*tc0*tc0*tc0 &
               + sb(10)*bm_s*bm_s*bm_s
            pool_pr(slot_,k,tp_smo2) = (pool_pr(slot_,k,tp_smob)/a_)**(1./b_)
         endif
         loga_ = sa(1) + sa(2)*tc0 + sa(5)*tc0*tc0 + sa(9)*tc0*tc0*tc0
         a_ = 10.0**loga_
         b_ = sb(1) + sb(2)*tc0 + sb(5)*tc0*tc0 + sb(9)*tc0*tc0*tc0
         pool_pr(slot_,k,tp_smo0) = a_ * pool_pr(slot_,k,tp_smo2)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3) &
               + sa(4)*tc0 + sa(5)*tc0*tc0 &
               + sa(6) + sa(7)*tc0*tc0 &
               + sa(8)*tc0 + sa(9)*tc0*tc0*tc0 &
               + sa(10)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3) + sb(4)*tc0 &
              + sb(5)*tc0*tc0 + sb(6) &
              + sb(7)*tc0*tc0 + sb(8)*tc0 &
              + sb(9)*tc0*tc0*tc0 + sb(10)
         pool_pr(slot_,k,tp_smo1) = a_ * pool_pr(slot_,k,tp_smo2)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(1) &
               + sa(4)*tc0*cse(1) + sa(5)*tc0*tc0 &
               + sa(6)*cse(1)*cse(1) + sa(7)*tc0*tc0*cse(1) &
               + sa(8)*tc0*cse(1)*cse(1) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(1)*cse(1)*cse(1)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(1) + sb(4)*tc0*cse(1) &
              + sb(5)*tc0*tc0 + sb(6)*cse(1)*cse(1) &
              + sb(7)*tc0*tc0*cse(1) + sb(8)*tc0*cse(1)*cse(1) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(1)*cse(1)*cse(1)
         pool_pr(slot_,k,tp_smoc) = a_ * pool_pr(slot_,k,tp_smo2)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(13) &
               + sa(4)*tc0*cse(13) + sa(5)*tc0*tc0 &
               + sa(6)*cse(13)*cse(13) + sa(7)*tc0*tc0*cse(13) &
               + sa(8)*tc0*cse(13)*cse(13) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(13)*cse(13)*cse(13)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(13) + sb(4)*tc0*cse(13) &
              + sb(5)*tc0*tc0 + sb(6)*cse(13)*cse(13) &
              + sb(7)*tc0*tc0*cse(13) + sb(8)*tc0*cse(13)*cse(13) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(13)*cse(13)*cse(13)
         pool_pr(slot_,k,tp_smoe) = a_ * pool_pr(slot_,k,tp_smo2)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(16) &
               + sa(4)*tc0*cse(16) + sa(5)*tc0*tc0 &
               + sa(6)*cse(16)*cse(16) + sa(7)*tc0*tc0*cse(16) &
               + sa(8)*tc0*cse(16)*cse(16) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(16)*cse(16)*cse(16)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(16) + sb(4)*tc0*cse(16) &
              + sb(5)*tc0*tc0 + sb(6)*cse(16)*cse(16) &
              + sb(7)*tc0*tc0*cse(16) + sb(8)*tc0*cse(16)*cse(16) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(16)*cse(16)*cse(16)
         pool_pr(slot_,k,tp_smof) = a_ * pool_pr(slot_,k,tp_smo2)**b_
      endif
      ! the rain's term of the graupel N0 (snow_graupel_moments) from a1's mvd_r, before the rain moments renew it; the k_0 test
      ! comes later (th_n0_level), when the column's k_0 is known
      if (pool_pl(slot_,k,tl_l_qr) .and. pool_pr(slot_,k,tp_mvd_r).gt.100.E-6) then
         th_xs(slot_,k) = 4.01 + alog10(pool_pr(slot_,k,tp_mvd_r))
      else
         th_xs(slot_,k) = -1.
      endif
      if (pool_pr(slot_,k,tp_rr) .eq. R1 .and. pool_pr(slot_,k,tp_nr) .eq. R2) then
         pool_pd(slot_,k,td_ilamr) = th_dry_ilamr
         pool_pr(slot_,k,tp_mvd_r) = th_dry_mvdr
         pool_pd(slot_,k,td_n0_r) = th_dry_n0r
      else
         lamr = (am_r*crg(3)*org2*pool_pr(slot_,k,tp_nr)/pool_pr(slot_,k,tp_rr))**obmr
         pool_pd(slot_,k,td_ilamr) = 1./lamr
         pool_pr(slot_,k,tp_mvd_r) = (3.0 + mu_r + 0.672) / lamr
         pool_pd(slot_,k,td_n0_r) = pool_pr(slot_,k,tp_nr)*org2*lamr**cre(2)
      endif
      end subroutine th_moments

      ! k_0 of a2 (th_k0) and of c1 (th_k0b) and the columns' species (th_any) as marks of the points: reset per column, then
      ! each point raises k_0 (an atomic MAX) and sets the flags of the species it has (th_marks_a after a1, th_marks_b after b1u)
      subroutine th_reset(slot_)
      !$acc routine seq
      integer, intent(in) :: slot_
      th_k0(slot_) = 1
      th_k0b(slot_) = 1
      th_any(slot_,1) = .false.
      th_any(slot_,2) = .false.
      th_any(slot_,3) = .false.
      th_any(slot_,4) = .false.
      end subroutine th_reset

      ! after a1: the temperature of the level is t1d (a1's temp)
      subroutine th_marks_a(slot_, k)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      if (pool_mc(slot_,k,tm_t1d).ge.270.65) then
         !$acc atomic update
         th_k0(slot_) = MAX(th_k0(slot_), k)
      endif
      end subroutine th_marks_a

      ! after b1u (the updated temperature and species; b2 changes only L_qc)
      subroutine th_marks_b(slot_, k)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      if (pool_pr(slot_,k,tp_temp).ge.270.65) then
         !$acc atomic update
         th_k0b(slot_) = MAX(th_k0b(slot_), k)
      endif
      if (pool_pl(slot_,k,tl_l_qr)) th_any(slot_,1) = .true.
      if (pool_pl(slot_,k,tl_l_qi)) th_any(slot_,2) = .true.
      if (pool_pl(slot_,k,tl_l_qs)) th_any(slot_,3) = .true.
      if (pool_pl(slot_,k,tl_l_qg)) th_any(slot_,4) = .true.
      end subroutine th_marks_b

      ! mp_thompson_c2's fall speeds at one level (after b2): rhof renewed where the column has rain (the rain loop renews it at
      ! every level), then rain, ice, snow and graupel where the species is at this level. Snow above T_0+0.1 and graupel above
      ! T_0 keep their own fall speed here; th_fall_species mixes in the rain's (vtrk of this level, taken from above where needed).
      subroutine th_fall_level(slot_, k)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, k
      DOUBLE PRECISION:: lamr, lami, ilami
      REAL:: vtr, vti, vts, vtg, xDs, Mrat, ils1, ils2, t1_vts, t2_vts, t3_vts, t4_vts
      if (th_any(slot_,1)) then
         pool_pr(slot_,k,tp_rhof) = SQRT(RHO_NOT/pool_pr(slot_,k,tp_rho))
         if (pool_pr(slot_,k,tp_rr).gt. R1) then
          lamr = (am_r*crg(3)*org2*pool_pr(slot_,k,tp_nr)/pool_pr(slot_,k,tp_rr))**obmr
          vtr = pool_pr(slot_,k,tp_rhof)*av_r*crg(6)*org3 * lamr**cre(3)                 &
                      *((lamr+fv_r)**(-cre(6)))
          th_vt(slot_,k,1) = vtr
          vtr = pool_pr(slot_,k,tp_rhof)*av_r*crg(7)/crg(12) * lamr**cre(12)             &
                      *((lamr+fv_r)**(-cre(7)))
          th_vt(slot_,k,2) = vtr
         endif
      endif
      if (th_any(slot_,2)) then
          if (pool_pr(slot_,k,tp_ri).gt. R1) then
           lami = (am_i*cig(2)*oig1*pool_pr(slot_,k,tp_ni)/pool_pr(slot_,k,tp_ri))**obmi
           ilami = 1./lami
           vti = pool_pr(slot_,k,tp_rhof)*av_i*cig(3)*oig2 * ilami**bv_i
           th_vt(slot_,k,3) = vti
           vti = pool_pr(slot_,k,tp_rhof)*av_i*cig(6)/cig(7) * ilami**bv_i
           th_vt(slot_,k,4) = vti
          endif
      endif
      if (th_any(slot_,3)) then
          if (pool_pr(slot_,k,tp_rs).gt. R1) then
           xDs = pool_pr(slot_,k,tp_smoc) / pool_pr(slot_,k,tp_smob)
           Mrat = 1./xDs
           ils1 = 1./(Mrat*Lam0 + fv_s)
           ils2 = 1./(Mrat*Lam1 + fv_s)
           t1_vts = Kap0*csg(4)*ils1**cse(4)
           t2_vts = Kap1*Mrat**mu_s*csg(10)*ils2**cse(10)
           ils1 = 1./(Mrat*Lam0)
           ils2 = 1./(Mrat*Lam1)
           t3_vts = Kap0*csg(1)*ils1**cse(1)
           t4_vts = Kap1*Mrat**mu_s*csg(7)*ils2**cse(7)
           vts = pool_pr(slot_,k,tp_rhof)*av_s * (t1_vts+t2_vts)/(t3_vts+t4_vts)
           if (pool_pr(slot_,k,tp_temp).gt. (T_0+0.1)) then
            th_vt(slot_,k,5) = vts
           else
            th_vt(slot_,k,5) = vts*pool_pr(slot_,k,tp_vts_boost)
           endif
          endif
      endif
      if (th_any(slot_,4)) then
          if (pool_pr(slot_,k,tp_rg).gt. R1) then
           vtg = pool_pr(slot_,k,tp_rhof)*av_g*cgg(6)*ogg3 * pool_pd(slot_,k,td_ilamg)**bv_g
           th_vt(slot_,k,6) = vtg
          endif
      endif
      ! 1/dzq and 1/rho of the level for the sedimentation (c2 forms them again at every level of every sub-step: the same
      ! expressions on values the sedimentation does not change)
      th_odz(slot_,k) = 1./pool_mc(slot_,k,tm_dzq)
      th_orho(slot_,k) = 1./pool_pr(slot_,k,tp_rho)
      end subroutine th_fall_level

      ! mp_thompson_c2's fall-speed loop of one species n (1 rain, 2 ice, 3 snow, 4 graupel) without its transcendental part
      ! (th_fall_level): a level without the species takes the fall speed of the level above, snow and graupel mix in the rain's,
      ! ksed1 and nstep. One thread per (column, species) (micro_staged); snow and graupel take the rain's fall speed from above in
      ! a register from th_vt's rain values at the levels with rain, which the rain's thread leaves as they are (it writes only the
      ! levels without rain), so the threads of one column need no order.
      subroutine th_fall_species(slot_, n, dt)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, n
      REAL, intent(in) :: dt
      integer :: k, nstep, ks, iv
      REAL:: delta_tp, SR, vts, vr, vrn
      th_on(slot_,n) = 1.0
      th_ks(slot_,n) = 1
      if (.not. th_any(slot_,n)) return
      nstep = 0
      ks = 1
      vr = 0.
      vrn = 0.
      iv = 1
      if (n .eq. 2) iv = 3
      if (n .eq. 3) iv = 5
      if (n .eq. 4) iv = 6
      th_vt(slot_,kte+1,iv) = 0.
      if (n .le. 2) th_vt(slot_,kte+1,iv+1) = 0.
      do k = kte, 1, -1
         if (n .eq. 1) then
            if (.not. (pool_pr(slot_,k,tp_rr).gt. R1)) then
             th_vt(slot_,k,1) = th_vt(slot_,k+1,1)
             th_vt(slot_,k,2) = th_vt(slot_,k+1,2)
            endif
            if (MAX(th_vt(slot_,k,1),th_vt(slot_,k,2)) .gt. 1.E-3) then
               ks = MAX(ks, k)
               delta_tp = pool_mc(slot_,k,tm_dzq)/(MAX(th_vt(slot_,k,1),th_vt(slot_,k,2)))
               if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
            endif
         elseif (n .eq. 2) then
            if (.not. (pool_pr(slot_,k,tp_ri).gt. R1)) then
             th_vt(slot_,k,3) = th_vt(slot_,k+1,3)
             th_vt(slot_,k,4) = th_vt(slot_,k+1,4)
            endif
            if (th_vt(slot_,k,3) .gt. 1.E-3) then
               ks = MAX(ks, k)
               delta_tp = pool_mc(slot_,k,tm_dzq)/th_vt(slot_,k,3)
               if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
            endif
         else
            ! the rain's fall speed of this level (vtrk of c2: zero without rain in the column, else from above)
            if (th_any(slot_,1)) then
               if (pool_pr(slot_,k,tp_rr).gt. R1) vr = th_vt(slot_,k,1)
            endif
            if (n .eq. 3) then
               if (pool_pr(slot_,k,tp_rs).gt. R1) then
                if (pool_pr(slot_,k,tp_temp).gt. (T_0+0.1)) then
                 vts = th_vt(slot_,k,5)
                 SR = pool_pr(slot_,k,tp_rs)/(pool_pr(slot_,k,tp_rs)+pool_pr(slot_,k,tp_rr))
                 th_vt(slot_,k,5) = vts*SR + (1.-SR)*vr
                endif
               else
                th_vt(slot_,k,5) = th_vt(slot_,k+1,5)
               endif
               if (th_vt(slot_,k,5) .gt. 1.E-3) then
                  ks = MAX(ks, k)
                  delta_tp = pool_mc(slot_,k,tm_dzq)/th_vt(slot_,k,5)
                  if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
               endif
            else
               if (pool_pr(slot_,k,tp_rg).gt. R1) then
                if (pool_pr(slot_,k,tp_temp).gt. T_0) then
                 th_vt(slot_,k,6) = MAX(th_vt(slot_,k,6), vr)
                endif
               else
                th_vt(slot_,k,6) = th_vt(slot_,k+1,6)
               endif
               if (th_vt(slot_,k,6) .gt. 1.E-3) then
                  ks = MAX(ks, k)
                  delta_tp = pool_mc(slot_,k,tm_dzq)/th_vt(slot_,k,6)
                  if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
               endif
            endif
         endif
      enddo
      if (ks .eq. kte) ks = kte-1
      th_ks(slot_,n) = ks
      if (nstep .gt. 0) th_on(slot_,n) = 1./REAL(nstep)
      end subroutine th_fall_species

      ! mp_thompson_c2's sedimentation of one species s (1 rain, 2 cloud water, 3 ice, 4 snow, 5 graupel) of one column; ppt the
      ! column's (pptrain, pptsnow, pptgraul, pptice). Each sub-step top-down: sa the flux of the level above at the start of the
      ! sub-step, sk this level's (the original's sed_*(k+1), sed_*(k)).
      subroutine th_sediment(slot_, s, dt, ppt)
      use micro_pool
      !$acc routine seq
      integer, intent(in) :: slot_, s
      REAL, intent(in) :: dt
      REAL, intent(inout) :: ppt(4)
      integer :: k, n, it, nstep, ks, iv, ir, ivn, inn, iten, inten, ip
      REAL:: onstep, odzq, orho, sa, sk, san, skn, hgt_agl, vtc, pp
      DOUBLE PRECISION:: lamc, ilamc
      INTEGER:: nu_c
      LOGICAL:: any_qc
      if (s .eq. 2) then   ! cloud water: fall speeds of the lowest 500 m and one pass (no sub-steps)
       any_qc = .false.
       do k = 1, kte
          if (pool_pl(slot_,k,tl_l_qc)) any_qc = .true.
       enddo
       if (.not. any_qc) return
       ks = 1
       hgt_agl = 0.
       do k = 1, kte-1
          if (pool_pr(slot_,k,tp_rc) .gt. R2) ks = k
          hgt_agl = hgt_agl + pool_mc(slot_,k,tm_dzq)
          if (hgt_agl .gt. 500.0) exit
       enddo
       ! the level above ks has vtck = vtnck = 0 (sed_c = 0*rc, sed_n = 0*nc)
       sa = 0.*pool_pr(slot_,ks+1,tp_rc)
       san = 0.*pool_pr(slot_,ks+1,tp_nc)
       do k = ks, 1, -1
          sk = 0.
          skn = 0.
          if (pool_pr(slot_,k,tp_rc) .gt. R1 .and. pool_mc(slot_,k,tm_w1d) .lt. 1.E-1) then
           nu_c = MIN(15, NINT(1000.E6/pool_pr(slot_,k,tp_nc)) + 2)
           lamc = (pool_pr(slot_,k,tp_nc)*am_r*ccg(2,nu_c)*ocg1(nu_c)/pool_pr(slot_,k,tp_rc))**obmr
           ilamc = 1./lamc
           vtc = pool_pr(slot_,k,tp_rhof)*av_c*ccg(5,nu_c)*ocg2(nu_c) * ilamc**bv_c
           sk = vtc*pool_pr(slot_,k,tp_rc)
           vtc = pool_pr(slot_,k,tp_rhof)*av_c*ccg(4,nu_c)*ocg1(nu_c) * ilamc**bv_c
           skn = vtc*pool_pr(slot_,k,tp_nc)
          else
           sk = 0.*pool_pr(slot_,k,tp_rc)
           skn = 0.*pool_pr(slot_,k,tp_nc)
          endif
          odzq = th_odz(slot_,k)
          orho = th_orho(slot_,k)
          pool_pr(slot_,k,tp_qcten) = pool_pr(slot_,k,tp_qcten) + (sa-sk) *odzq*orho
          pool_pr(slot_,k,tp_ncten) = pool_pr(slot_,k,tp_ncten) + (san-skn) *odzq*orho
          pool_pr(slot_,k,tp_rc) = MAX(R1, pool_pr(slot_,k,tp_rc) + (sa-sk) *odzq*DT)
          pool_pr(slot_,k,tp_nc) = MAX(10., pool_pr(slot_,k,tp_nc) + (san-skn) *odzq*DT)
          sa = sk
          san = skn
       enddo
       return
      endif
      ! rain (1), ice (3), snow (4), graupel (5): n their index in th_any, th_on, th_ks; iv, ivn their fall speeds in th_vt (number
      ! 0: none); ir, inn the mass and number fields, iten, inten their tendencies; ip their sum in ppt
      select case (s)
      case (1)
       n = 1; iv = 1; ivn = 2; ir = tp_rr; inn = tp_nr; iten = tp_qrten; inten = tp_nrten; ip = 1
      case (3)
       n = 2; iv = 3; ivn = 4; ir = tp_ri; inn = tp_ni; iten = tp_qiten; inten = tp_niten; ip = 4
      case (4)
       n = 3; iv = 5; ivn = 0; ir = tp_rs; inn = 0; iten = tp_qsten; inten = 0; ip = 2
      case default
       n = 4; iv = 6; ivn = 0; ir = tp_rg; inn = 0; iten = tp_qgten; inten = 0; ip = 3
      end select
      if (.not. th_any(slot_,n)) return
      onstep = th_on(slot_,n)
      ks = th_ks(slot_,n)
      pp = ppt(ip)
      nstep = NINT(1./onstep)
      do it = 1, nstep
         ! the top level: its own flux out (sed(kte) at the start of the sub-step); the flux into ks from above
         k = kte
         sk = th_vt(slot_,k,iv)*pool_pr(slot_,k,ir)
         if (ks+1 .eq. kte) then
            sa = sk
         else
            sa = th_vt(slot_,ks+1,iv)*pool_pr(slot_,ks+1,ir)
         endif
         if (ivn .gt. 0) then
            skn = th_vt(slot_,k,ivn)*pool_pr(slot_,k,inn)
            if (ks+1 .eq. kte) then
               san = skn
            else
               san = th_vt(slot_,ks+1,ivn)*pool_pr(slot_,ks+1,inn)
            endif
         endif
         odzq = th_odz(slot_,k)
         orho = th_orho(slot_,k)
         pool_pr(slot_,k,iten) = pool_pr(slot_,k,iten) - sk*odzq*onstep*orho
         if (ivn .gt. 0) pool_pr(slot_,k,inten) = pool_pr(slot_,k,inten) - skn*odzq*onstep*orho
         pool_pr(slot_,k,ir) = MAX(R1, pool_pr(slot_,k,ir) - sk*odzq*DT*onstep)
         if (ivn .gt. 0) pool_pr(slot_,k,inn) = MAX(R2, pool_pr(slot_,k,inn) - skn*odzq*DT*onstep)
         do k = ks, 1, -1
            sk = th_vt(slot_,k,iv)*pool_pr(slot_,k,ir)
            odzq = th_odz(slot_,k)
            orho = th_orho(slot_,k)
            pool_pr(slot_,k,iten) = pool_pr(slot_,k,iten) + (sa-sk)                 &
                                               *odzq*onstep*orho
            pool_pr(slot_,k,ir) = MAX(R1, pool_pr(slot_,k,ir) + (sa-sk) &
                                           *odzq*DT*onstep)
            sa = sk
            if (ivn .gt. 0) then
               skn = th_vt(slot_,k,ivn)*pool_pr(slot_,k,inn)
               pool_pr(slot_,k,inten) = pool_pr(slot_,k,inten) + (san-skn)                 &
                                               *odzq*onstep*orho
               pool_pr(slot_,k,inn) = MAX(R2, pool_pr(slot_,k,inn) + (san-skn) &
                                           *odzq*DT*onstep)
               san = skn
            endif
         enddo
         ! sk: sed(kts) at the start of the sub-step
         if (pool_pr(slot_,1,ir).gt.R1*1000.) &
         pp = pp + sk*DT*onstep
      enddo
      ppt(ip) = pp
      end subroutine th_sediment

      ! calc_effectRad at one level: its statements on the level's values. A level without the species takes the value
      ! calc_effectRad leaves there (cloud 2.49e-6; ice 2.49e-6 or 4.99e-6, snow 4.99e-6 or 9.99e-6, by the column's has_q*);
      ! after the caller's bounds (ice at least 4.99e-6, snow at least 9.99e-6) both give the same number, so the column flags
      ! are not needed: ice 4.99e-6 and snow 9.99e-6 here.
      subroutine th_effect_rad(t, p, qv, qc, qi, ni1, qs, Nt_c, re_qc, re_qi, re_qs)
      !$acc routine seq
      REAL, INTENT(IN) :: t, p, qv, qc, qi, ni1, qs, Nt_c
      REAL, INTENT(OUT) :: re_qc, re_qi, re_qs
      REAL:: rho, rc, nc, ri, ni, rs
      REAL:: smo2, smob, smoc
      REAL:: tc0, loga_, a_, b_
      DOUBLE PRECISION:: lamc, lami
      INTEGER:: inu_c
      real, dimension(15), parameter:: g_ratio = (/24,60,120,210,336,   &
     &                504,720,990,1320,1716,2184,2730,3360,4080,4896/)
      rho = 0.622*p/(R*t*(qv+0.622))
      rc = MAX(R1, qc*rho)
      nc = Nt_c
      ri = MAX(R1, qi*rho)
      ni = MAX(R2, ni1*rho)
      rs = MAX(R1, qs*rho)
      re_qc = 2.49E-6
      if (.not. (rc.le.R1 .or. nc.le.R2)) then
         if (nc.lt.100) then
            inu_c = 15
         elseif (nc.gt.1.E10) then
            inu_c = 2
         else
            inu_c = MIN(15, NINT(1000.E6/nc) + 2)
         endif
         lamc = (nc*am_r*g_ratio(inu_c)/rc)**obmr
         re_qc = MAX(2.51E-6, MIN(SNGL(0.5D0 * DBLE(3.+inu_c)/lamc), 50.E-6))
      endif
      re_qi = 4.99E-6
      if (.not. (ri.le.R1 .or. ni.le.R2)) then
         lami = (am_i*cig(2)*oig1*ni/ri)**obmi
         re_qi = MAX(2.51E-6, MIN(SNGL(0.5D0 * DBLE(3.+mu_i)/lami), 125.E-6))
      endif
      re_qs = 9.99E-6
      if (.not. (rs.le.R1)) then
         tc0 = MIN(-0.1, t-273.15)
         smob = rs*oams
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2 = smob
         else
            loga_ = sa(1) + sa(2)*tc0 + sa(3)*bm_s &
     &         + sa(4)*tc0*bm_s + sa(5)*tc0*tc0 &
     &         + sa(6)*bm_s*bm_s + sa(7)*tc0*tc0*bm_s &
     &         + sa(8)*tc0*bm_s*bm_s + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*bm_s*bm_s*bm_s
            a_ = 10.0**loga_
            b_ = sb(1) + sb(2)*tc0 + sb(3)*bm_s &
     &         + sb(4)*tc0*bm_s + sb(5)*tc0*tc0 &
     &         + sb(6)*bm_s*bm_s + sb(7)*tc0*tc0*bm_s &
     &         + sb(8)*tc0*bm_s*bm_s + sb(9)*tc0*tc0*tc0 &
     &         + sb(10)*bm_s*bm_s*bm_s
            smo2 = (smob/a_)**(1./b_)
         endif
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(1) &
     &         + sa(4)*tc0*cse(1) + sa(5)*tc0*tc0 &
     &         + sa(6)*cse(1)*cse(1) + sa(7)*tc0*tc0*cse(1) &
     &         + sa(8)*tc0*cse(1)*cse(1) + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*cse(1)*cse(1)*cse(1)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(1) + sb(4)*tc0*cse(1) &
     &        + sb(5)*tc0*tc0 + sb(6)*cse(1)*cse(1) &
     &        + sb(7)*tc0*tc0*cse(1) + sb(8)*tc0*cse(1)*cse(1) &
     &        + sb(9)*tc0*tc0*tc0 + sb(10)*cse(1)*cse(1)*cse(1)
         smoc = a_ * smo2**b_
         re_qs = MAX(5.01E-6, MIN(0.5*(smoc/smob), 999.E-6))
      endif
      end subroutine th_effect_rad
