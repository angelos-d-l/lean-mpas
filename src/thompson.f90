! Derived from MPAS 8.4.2 physics/physics_wrf/module_mp_thompson.F (WRF physics, public domain: NOTICE.md; the original with lean's fixes is in reference_fixes/).
! The column code is split into routines of one level or one process block, each headed 'Reference module_mp_thompson.F:a-b' with
! the lines it reproduces; arithmetic order is retained. Lean changes are marked (lean).
      MODULE lean_thompson
      use mpas_log
      use mpas_kind_types
      use mpas_atmphys_functions, only: gammp,wgamma,rslf,rsif
      use mpas_atmphys_utilities
      use mpas_io_units, only : mpas_new_unit, mpas_release_unit
      use mp_radar
      use config, only: nz, kte => nz
      implicit none
      private
      public :: thompson_init, mp_thompson, calc_effectRad, naIN1, calc_refl10cm
      INTEGER, PARAMETER, PRIVATE:: IFDRY = 0
      REAL, PARAMETER, PRIVATE:: T_0 = 273.15
      REAL, PARAMETER, PRIVATE:: PI = 3.1415926536
      REAL, PARAMETER, PRIVATE:: rho_w = 1000.0
      REAL, PARAMETER, PRIVATE:: rho_s = 100.0
      REAL, PARAMETER, PRIVATE:: rho_g = 500.0
      REAL, PARAMETER, PRIVATE:: rho_i = 890.0
      REAL, PARAMETER, PRIVATE:: Nt_c_max = 1999.E6
      REAL, PARAMETER:: naIN0 = 1.5E6
      REAL, PARAMETER:: naIN1 = 0.5E6
      REAL, PARAMETER:: naCCN0 = 300.0E6
      REAL, PARAMETER:: naCCN1 = 50.0E6
      REAL, PARAMETER, PRIVATE:: mu_r = 0.0
      REAL, PARAMETER, PRIVATE:: mu_g = 0.0
      REAL, PARAMETER, PRIVATE:: mu_i = 0.0
      REAL, PARAMETER, PRIVATE:: mu_s = 0.6357
      REAL, PARAMETER, PRIVATE:: Kap0 = 490.6
      REAL, PARAMETER, PRIVATE:: Kap1 = 17.46
      REAL, PARAMETER, PRIVATE:: Lam0 = 20.78
      REAL, PARAMETER, PRIVATE:: Lam1 = 3.29
      REAL, PARAMETER, PRIVATE:: gonv_min = 1.E4
      REAL, PARAMETER, PRIVATE:: gonv_max = 3.E6
      REAL, PARAMETER, PRIVATE:: am_r = PI*rho_w/6.0
      REAL, PARAMETER, PRIVATE:: bm_r = 3.0
      REAL, PARAMETER, PRIVATE:: am_s = 0.069
      REAL, PARAMETER, PRIVATE:: bm_s = 2.0
      REAL, PARAMETER, PRIVATE:: am_g = PI*rho_g/6.0
      REAL, PARAMETER, PRIVATE:: bm_g = 3.0
      REAL, PARAMETER, PRIVATE:: am_i = PI*rho_i/6.0
      REAL, PARAMETER, PRIVATE:: bm_i = 3.0
      REAL, PARAMETER, PRIVATE:: av_r = 4854.0
      REAL, PARAMETER, PRIVATE:: bv_r = 1.0
      REAL, PARAMETER, PRIVATE:: fv_r = 195.0
      REAL, PARAMETER, PRIVATE:: av_s = 40.0
      REAL, PARAMETER, PRIVATE:: bv_s = 0.55
      REAL, PARAMETER, PRIVATE:: fv_s = 100.0
      REAL, PARAMETER, PRIVATE:: av_g = 442.0
      REAL, PARAMETER, PRIVATE:: bv_g = 0.89
      REAL, PARAMETER, PRIVATE:: av_i = 1847.5
      REAL, PARAMETER, PRIVATE:: bv_i = 1.0
      REAL, PARAMETER, PRIVATE:: av_c = 0.316946E8
      REAL, PARAMETER, PRIVATE:: bv_c = 2.0
      REAL, PARAMETER, PRIVATE:: C_cube = 0.5
      REAL, PARAMETER, PRIVATE:: C_sqrd = 0.15
      REAL, PARAMETER, PRIVATE:: Ef_si = 0.05
      REAL, PARAMETER, PRIVATE:: Ef_rs = 0.95
      REAL, PARAMETER, PRIVATE:: Ef_rg = 0.75
      REAL, PARAMETER, PRIVATE:: Ef_ri = 0.95
      REAL, PARAMETER, PRIVATE:: R1 = 1.E-12
      REAL, PARAMETER, PRIVATE:: R2 = 1.E-6
      REAL, PARAMETER, PRIVATE:: eps = 1.E-15
      REAL, PARAMETER, PRIVATE:: TNO = 5.0
      REAL, PARAMETER, PRIVATE:: ATO = 0.304
      REAL, PARAMETER, PRIVATE:: rho_not = 101325.0/(287.05*298.0)
      REAL, PARAMETER, PRIVATE:: Sc = 0.632
      REAL, PRIVATE:: Sc3
      REAL, PARAMETER, PRIVATE:: HGFR = 235.16
      REAL, PARAMETER, PRIVATE:: Rv = 461.5
      REAL, PARAMETER, PRIVATE:: oRv = 1./Rv
      REAL, PARAMETER, PRIVATE:: R = 287.04
      REAL, PARAMETER, PRIVATE:: Cp = 1004.0
      REAL, PARAMETER, PRIVATE:: R_uni = 8.314                           ! J (mol K)-1
      DOUBLE PRECISION, PARAMETER, PRIVATE:: k_b = 1.38065E-23           ! Boltzmann constant [J/K]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: M_w = 18.01528E-3           ! molecular mass of water [kg/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: M_a = 28.96E-3              ! molecular mass of air [kg/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: N_avo = 6.022E23            ! Avogadro number [1/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: ma_w = M_w / N_avo          ! mass of water molecule [kg]
      REAL, PARAMETER, PRIVATE:: ar_volume = 4./3.*PI*(2.5e-6)**3        ! assume radius of 0.025 micrometer, 2.5e-6 cm
      REAL, PARAMETER, PRIVATE:: lsub = 2.834E6
      REAL, PARAMETER, PRIVATE:: lvap0 = 2.5E6
      REAL, PARAMETER, PRIVATE:: lfus = lsub - lvap0
      REAL, PARAMETER, PRIVATE:: olfus = 1./lfus
      REAL, PARAMETER, PRIVATE:: xm0i = 1.E-12
      REAL, PARAMETER, PRIVATE:: D0c = 1.E-6
      REAL, PARAMETER, PRIVATE:: D0r = 50.E-6
      REAL, PARAMETER, PRIVATE:: D0s = 200.E-6
      REAL, PARAMETER, PRIVATE:: D0g = 250.E-6
      REAL, PRIVATE:: D0i, xm0s, xm0g
      INTEGER, PARAMETER, PRIVATE:: nbins = 100
      INTEGER, PARAMETER, PRIVATE:: nbc = nbins
      INTEGER, PARAMETER, PRIVATE:: nbi = nbins
      INTEGER, PARAMETER, PRIVATE:: nbr = nbins
      INTEGER, PARAMETER, PRIVATE:: nbs = nbins
      INTEGER, PARAMETER, PRIVATE:: nbg = nbins
      INTEGER, PARAMETER, PRIVATE:: ntb_c = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_i = 64
      INTEGER, PARAMETER, PRIVATE:: ntb_r = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_s = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_g = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_g1 = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_r1 = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_i1 = 55
      INTEGER, PARAMETER, PRIVATE:: ntb_t = 9
      INTEGER, PRIVATE:: nic1, nic2, nii2, nii3, nir2, nir3, nis2, nig2, nig3
      INTEGER, PARAMETER:: ntb_arc = 7
      INTEGER, PARAMETER:: ntb_arw = 9
      INTEGER, PARAMETER:: ntb_art = 7
      INTEGER, PARAMETER:: ntb_arr = 5
      INTEGER, PARAMETER:: ntb_ark = 4
      INTEGER, PARAMETER:: ntb_IN = 55
      INTEGER, PRIVATE:: niIN2
      DOUBLE PRECISION, DIMENSION(nbins+1):: xDx
      DOUBLE PRECISION, DIMENSION(nbc):: Dc, dtc
      DOUBLE PRECISION, DIMENSION(nbi):: Di, dti
      DOUBLE PRECISION, DIMENSION(nbr):: Dr, dtr
      DOUBLE PRECISION, DIMENSION(nbs):: Ds, dts
      DOUBLE PRECISION, DIMENSION(nbg):: Dg, dtg
      DOUBLE PRECISION, DIMENSION(nbc):: t_Nc
      REAL, DIMENSION(ntb_c), PARAMETER, PRIVATE:: &
      r_c = (/1.e-6,2.e-6,3.e-6,4.e-6,5.e-6,6.e-6,7.e-6,8.e-6,9.e-6, &
              1.e-5,2.e-5,3.e-5,4.e-5,5.e-5,6.e-5,7.e-5,8.e-5,9.e-5, &
              1.e-4,2.e-4,3.e-4,4.e-4,5.e-4,6.e-4,7.e-4,8.e-4,9.e-4, &
              1.e-3,2.e-3,3.e-3,4.e-3,5.e-3,6.e-3,7.e-3,8.e-3,9.e-3, &
              1.e-2/)
      REAL, DIMENSION(ntb_i), PARAMETER, PRIVATE:: &
      r_i = (/1.e-10,2.e-10,3.e-10,4.e-10, &
              5.e-10,6.e-10,7.e-10,8.e-10,9.e-10, &
              1.e-9,2.e-9,3.e-9,4.e-9,5.e-9,6.e-9,7.e-9,8.e-9,9.e-9, &
              1.e-8,2.e-8,3.e-8,4.e-8,5.e-8,6.e-8,7.e-8,8.e-8,9.e-8, &
              1.e-7,2.e-7,3.e-7,4.e-7,5.e-7,6.e-7,7.e-7,8.e-7,9.e-7, &
              1.e-6,2.e-6,3.e-6,4.e-6,5.e-6,6.e-6,7.e-6,8.e-6,9.e-6, &
              1.e-5,2.e-5,3.e-5,4.e-5,5.e-5,6.e-5,7.e-5,8.e-5,9.e-5, &
              1.e-4,2.e-4,3.e-4,4.e-4,5.e-4,6.e-4,7.e-4,8.e-4,9.e-4, &
              1.e-3/)
      REAL, DIMENSION(ntb_r), PARAMETER, PRIVATE:: &
      r_r = (/1.e-6,2.e-6,3.e-6,4.e-6,5.e-6,6.e-6,7.e-6,8.e-6,9.e-6, &
              1.e-5,2.e-5,3.e-5,4.e-5,5.e-5,6.e-5,7.e-5,8.e-5,9.e-5, &
              1.e-4,2.e-4,3.e-4,4.e-4,5.e-4,6.e-4,7.e-4,8.e-4,9.e-4, &
              1.e-3,2.e-3,3.e-3,4.e-3,5.e-3,6.e-3,7.e-3,8.e-3,9.e-3, &
              1.e-2/)
      REAL, DIMENSION(ntb_g), PARAMETER, PRIVATE:: &
      r_g = (/1.e-5,2.e-5,3.e-5,4.e-5,5.e-5,6.e-5,7.e-5,8.e-5,9.e-5, &
              1.e-4,2.e-4,3.e-4,4.e-4,5.e-4,6.e-4,7.e-4,8.e-4,9.e-4, &
              1.e-3,2.e-3,3.e-3,4.e-3,5.e-3,6.e-3,7.e-3,8.e-3,9.e-3, &
              1.e-2/)
      REAL, DIMENSION(ntb_s), PARAMETER, PRIVATE:: &
      r_s = (/1.e-5,2.e-5,3.e-5,4.e-5,5.e-5,6.e-5,7.e-5,8.e-5,9.e-5, &
              1.e-4,2.e-4,3.e-4,4.e-4,5.e-4,6.e-4,7.e-4,8.e-4,9.e-4, &
              1.e-3,2.e-3,3.e-3,4.e-3,5.e-3,6.e-3,7.e-3,8.e-3,9.e-3, &
              1.e-2/)
      REAL, DIMENSION(ntb_r1), PARAMETER, PRIVATE:: &
      N0r_exp = (/1.e6,2.e6,3.e6,4.e6,5.e6,6.e6,7.e6,8.e6,9.e6, &
                  1.e7,2.e7,3.e7,4.e7,5.e7,6.e7,7.e7,8.e7,9.e7, &
                  1.e8,2.e8,3.e8,4.e8,5.e8,6.e8,7.e8,8.e8,9.e8, &
                  1.e9,2.e9,3.e9,4.e9,5.e9,6.e9,7.e9,8.e9,9.e9, &
                  1.e10/)
      REAL, DIMENSION(ntb_g1), PARAMETER, PRIVATE:: &
      N0g_exp = (/1.e4,2.e4,3.e4,4.e4,5.e4,6.e4,7.e4,8.e4,9.e4, &
                  1.e5,2.e5,3.e5,4.e5,5.e5,6.e5,7.e5,8.e5,9.e5, &
                  1.e6,2.e6,3.e6,4.e6,5.e6,6.e6,7.e6,8.e6,9.e6, &
                  1.e7/)
      REAL, DIMENSION(ntb_i1), PARAMETER, PRIVATE:: &
      Nt_i = (/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0, &
               1.e1,2.e1,3.e1,4.e1,5.e1,6.e1,7.e1,8.e1,9.e1, &
               1.e2,2.e2,3.e2,4.e2,5.e2,6.e2,7.e2,8.e2,9.e2, &
               1.e3,2.e3,3.e3,4.e3,5.e3,6.e3,7.e3,8.e3,9.e3, &
               1.e4,2.e4,3.e4,4.e4,5.e4,6.e4,7.e4,8.e4,9.e4, &
               1.e5,2.e5,3.e5,4.e5,5.e5,6.e5,7.e5,8.e5,9.e5, &
               1.e6/)
      REAL, DIMENSION(ntb_arc), PARAMETER, PRIVATE:: &
      ta_Na = (/10.0, 31.6, 100.0, 316.0, 1000.0, 3160.0, 10000.0/)
      REAL, DIMENSION(ntb_arw), PARAMETER, PRIVATE:: &
      ta_Ww = (/0.01, 0.0316, 0.1, 0.316, 1.0, 3.16, 10.0, 31.6, 100.0/)
      REAL, DIMENSION(ntb_art), PARAMETER, PRIVATE:: &
      ta_Tk = (/243.15, 253.15, 263.15, 273.15, 283.15, 293.15, 303.15/)
      REAL, DIMENSION(ntb_arr), PARAMETER, PRIVATE:: &
      ta_Ra = (/0.01, 0.02, 0.04, 0.08, 0.16/)
      REAL, DIMENSION(ntb_ark), PARAMETER, PRIVATE:: &
      ta_Ka = (/0.2, 0.4, 0.6, 0.8/)
      REAL, DIMENSION(ntb_IN), PARAMETER, PRIVATE:: &
      Nt_IN = (/1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0, &
               1.e1,2.e1,3.e1,4.e1,5.e1,6.e1,7.e1,8.e1,9.e1, &
               1.e2,2.e2,3.e2,4.e2,5.e2,6.e2,7.e2,8.e2,9.e2, &
               1.e3,2.e3,3.e3,4.e3,5.e3,6.e3,7.e3,8.e3,9.e3, &
               1.e4,2.e4,3.e4,4.e4,5.e4,6.e4,7.e4,8.e4,9.e4, &
               1.e5,2.e5,3.e5,4.e5,5.e5,6.e5,7.e5,8.e5,9.e5, &
               1.e6/)
      REAL, DIMENSION(10), PARAMETER, PRIVATE:: &
      sa = (/ 5.065339, -0.062659, -3.032362, 0.029469, -0.000285, &
              0.31255,   0.000204,  0.003199, 0.0,      -0.015952/)
      REAL, DIMENSION(10), PARAMETER, PRIVATE:: &
      sb = (/ 0.476221, -0.015896,  0.165977, 0.007468, -0.000141, &
              0.060366,  0.000079,  0.000594, 0.0,      -0.003577/)
      REAL, DIMENSION(ntb_t), PARAMETER, PRIVATE:: &
      Tc = (/-0.01, -5., -10., -15., -20., -25., -30., -35., -40./)
      INTEGER, PARAMETER, PRIVATE:: R8SIZE = R8KIND
      INTEGER, PARAMETER, PRIVATE:: R4SIZE = R4KIND
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tcg_racg, tmr_racg, tcr_gacr, tmg_gacr,                 &
                tnr_racg, tnr_gacr
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tcs_racs1, tmr_racs1, tcs_racs2, tmr_racs2,             &
                tcr_sacr1, tms_sacr1, tcr_sacr2, tms_sacr2,             &
                tnr_racs1, tnr_racs2, tnr_sacr1, tnr_sacr2
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tpi_qcfz, tni_qcfz
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tpi_qrfz, tpg_qrfz, tni_qrfz, tnr_qrfz
! the freezing tables at the one ice-nuclei bin mp_thompson uses (xni = 1 per litre, the default of the scheme):
! the column code reads these, so the card holds 1/ntb_IN of the full tables (lean)
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:)::               &
                tpi_qcfz_in, tni_qcfz_in, tpi_qrfz_in, tpg_qrfz_in, tni_qrfz_in, tnr_qrfz_in
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:)::                 &
                tps_iaus, tni_iaus, tpi_ide
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:):: t_Efrw
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:):: t_Efsw
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:):: tnr_rev
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:)::               &
                tpc_wev, tnc_wev
      REAL (KIND=R4SIZE), ALLOCATABLE, DIMENSION(:,:,:,:,:):: tnccn_act
      REAL, DIMENSION(5,15), PRIVATE:: cce, ccg
      REAL, DIMENSION(15), PRIVATE::  ocg1, ocg2
      REAL, DIMENSION(7), PRIVATE:: cie, cig
      REAL, PRIVATE:: oig1, oig2, obmi
      REAL, DIMENSION(13), PRIVATE:: cre, crg
      REAL, PRIVATE:: ore1, org1, org2, org3, obmr
      REAL, DIMENSION(18), PRIVATE:: cse, csg
      REAL, PRIVATE:: oams, obms, ocms
      REAL, DIMENSION(12), PRIVATE:: cge, cgg
      REAL, PRIVATE:: oge1, ogg1, ogg2, ogg3, oamg, obmg, ocmg
      REAL:: t1_qr_qc, t1_qr_qi, t2_qr_qi, t1_qg_qc, t1_qs_qc, t1_qs_qi
      REAL:: t1_qr_ev, t2_qr_ev
      REAL:: t1_qs_sd, t2_qs_sd, t1_qg_sd, t2_qg_sd
      REAL:: t1_qs_me, t2_qs_me, t1_qg_me, t2_qg_me
      CONTAINS
 subroutine thompson_init(big_endian)
 implicit none
 logical, intent(in), optional :: big_endian
 logical :: swap_tables
 integer,parameter:: open_OK = 0
 integer:: i,j,k,l,m,n
 integer:: istat
 logical:: micro_init
 integer:: mp_unit
      swap_tables = .false.
      if (present(big_endian)) swap_tables = big_endian
      micro_init = .FALSE.
      if (.NOT. ALLOCATED(tcg_racg) ) then
         ALLOCATE(tcg_racg(ntb_g1,ntb_g,ntb_r1,ntb_r))
         micro_init = .TRUE.
      endif
      if (.NOT. ALLOCATED(tmr_racg)) ALLOCATE(tmr_racg(ntb_g1,ntb_g,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tcr_gacr)) ALLOCATE(tcr_gacr(ntb_g1,ntb_g,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tmg_gacr)) ALLOCATE(tmg_gacr(ntb_g1,ntb_g,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_racg)) ALLOCATE(tnr_racg(ntb_g1,ntb_g,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_gacr)) ALLOCATE(tnr_gacr(ntb_g1,ntb_g,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tcs_racs1)) ALLOCATE(tcs_racs1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tmr_racs1)) ALLOCATE(tmr_racs1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tcs_racs2)) ALLOCATE(tcs_racs2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tmr_racs2)) ALLOCATE(tmr_racs2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tcr_sacr1)) ALLOCATE(tcr_sacr1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tms_sacr1)) ALLOCATE(tms_sacr1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tcr_sacr2)) ALLOCATE(tcr_sacr2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tms_sacr2)) ALLOCATE(tms_sacr2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_racs1)) ALLOCATE(tnr_racs1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_racs2)) ALLOCATE(tnr_racs2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_sacr1)) ALLOCATE(tnr_sacr1(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tnr_sacr2)) ALLOCATE(tnr_sacr2(ntb_s,ntb_t,ntb_r1,ntb_r))
      if (.NOT. ALLOCATED(tpi_qcfz)) ALLOCATE(tpi_qcfz(ntb_c,nbc,45,ntb_IN))
      if (.NOT. ALLOCATED(tni_qcfz)) ALLOCATE(tni_qcfz(ntb_c,nbc,45,ntb_IN))
      if (.NOT. ALLOCATED(tpi_qrfz)) ALLOCATE(tpi_qrfz(ntb_r,ntb_r1,45,ntb_IN))
      if (.NOT. ALLOCATED(tpg_qrfz)) ALLOCATE(tpg_qrfz(ntb_r,ntb_r1,45,ntb_IN))
      if (.NOT. ALLOCATED(tni_qrfz)) ALLOCATE(tni_qrfz(ntb_r,ntb_r1,45,ntb_IN))
      if (.NOT. ALLOCATED(tnr_qrfz)) ALLOCATE(tnr_qrfz(ntb_r,ntb_r1,45,ntb_IN))
      if (.NOT. ALLOCATED(tps_iaus)) ALLOCATE(tps_iaus(ntb_i,ntb_i1))
      if (.NOT. ALLOCATED(tni_iaus)) ALLOCATE(tni_iaus(ntb_i,ntb_i1))
      if (.NOT. ALLOCATED(tpi_ide)) ALLOCATE(tpi_ide(ntb_i,ntb_i1))
      if (.NOT. ALLOCATED(t_Efrw)) ALLOCATE(t_Efrw(nbr,nbc))
      if (.NOT. ALLOCATED(t_Efsw)) ALLOCATE(t_Efsw(nbs,nbc))
      if (.NOT. ALLOCATED(tnr_rev)) ALLOCATE(tnr_rev(nbr, ntb_r1, ntb_r))
      if (.NOT. ALLOCATED(tpc_wev)) ALLOCATE(tpc_wev(nbc,ntb_c,nbc))
      if (.NOT. ALLOCATED(tnc_wev)) ALLOCATE(tnc_wev(nbc,ntb_c,nbc))
      if (.NOT. ALLOCATED(tnccn_act))                                   &
            ALLOCATE(tnccn_act(ntb_arc,ntb_arw,ntb_art,ntb_arr,ntb_ark))
      if (micro_init) then
      Sc3 = Sc**(1./3.)
      D0i = (xm0i/am_i)**(1./bm_i)
      xm0s = am_s * D0s**bm_s
      xm0g = am_g * D0g**bm_g
      do n = 1, 15
         cce(1,n) = n + 1.
         cce(2,n) = bm_r + n + 1.
         cce(3,n) = bm_r + n + 4.
         cce(4,n) = n + bv_c + 1.
         cce(5,n) = bm_r + n + bv_c + 1.
         ccg(1,n) = WGAMMA(cce(1,n))
         ccg(2,n) = WGAMMA(cce(2,n))
         ccg(3,n) = WGAMMA(cce(3,n))
         ccg(4,n) = WGAMMA(cce(4,n))
         ccg(5,n) = WGAMMA(cce(5,n))
         ocg1(n) = 1./ccg(1,n)
         ocg2(n) = 1./ccg(2,n)
      enddo
      cie(1) = mu_i + 1.
      cie(2) = bm_i + mu_i + 1.
      cie(3) = bm_i + mu_i + bv_i + 1.
      cie(4) = mu_i + bv_i + 1.
      cie(5) = mu_i + 2.
      cie(6) = bm_i*0.5 + mu_i + bv_i + 1.
      cie(7) = bm_i*0.5 + mu_i + 1.
      cig(1) = WGAMMA(cie(1))
      cig(2) = WGAMMA(cie(2))
      cig(3) = WGAMMA(cie(3))
      cig(4) = WGAMMA(cie(4))
      cig(5) = WGAMMA(cie(5))
      cig(6) = WGAMMA(cie(6))
      cig(7) = WGAMMA(cie(7))
      oig1 = 1./cig(1)
      oig2 = 1./cig(2)
      obmi = 1./bm_i
      cre(1) = bm_r + 1.
      cre(2) = mu_r + 1.
      cre(3) = bm_r + mu_r + 1.
      cre(4) = bm_r*2. + mu_r + 1.
      cre(5) = mu_r + bv_r + 1.
      cre(6) = bm_r + mu_r + bv_r + 1.
      cre(7) = bm_r*0.5 + mu_r + bv_r + 1.
      cre(8) = bm_r + mu_r + bv_r + 3.
      cre(9) = mu_r + bv_r + 3.
      cre(10) = mu_r + 2.
      cre(11) = 0.5*(bv_r + 5. + 2.*mu_r)
      cre(12) = bm_r*0.5 + mu_r + 1.
      cre(13) = bm_r*2. + mu_r + bv_r + 1.
      do n = 1, 13
         crg(n) = WGAMMA(cre(n))
      enddo
      obmr = 1./bm_r
      ore1 = 1./cre(1)
      org1 = 1./crg(1)
      org2 = 1./crg(2)
      org3 = 1./crg(3)
      cse(1) = bm_s + 1.
      cse(2) = bm_s + 2.
      cse(3) = bm_s*2.
      cse(4) = bm_s + bv_s + 1.
      cse(5) = bm_s*2. + bv_s + 1.
      cse(6) = bm_s*2. + 1.
      cse(7) = bm_s + mu_s + 1.
      cse(8) = bm_s + mu_s + 2.
      cse(9) = bm_s + mu_s + 3.
      cse(10) = bm_s + mu_s + bv_s + 1.
      cse(11) = bm_s*2. + mu_s + bv_s + 1.
      cse(12) = bm_s*2. + mu_s + 1.
      cse(13) = bv_s + 2.
      cse(14) = bm_s + bv_s
      cse(15) = mu_s + 1.
      cse(16) = 1.0 + (1.0 + bv_s)/2.
      cse(17) = cse(16) + mu_s + 1.
      cse(18) = bv_s + mu_s + 3.
      do n = 1, 18
         csg(n) = WGAMMA(cse(n))
      enddo
      oams = 1./am_s
      obms = 1./bm_s
      ocms = oams**obms
      cge(1) = bm_g + 1.
      cge(2) = mu_g + 1.
      cge(3) = bm_g + mu_g + 1.
      cge(4) = bm_g*2. + mu_g + 1.
      cge(5) = bm_g*2. + mu_g + bv_g + 1.
      cge(6) = bm_g + mu_g + bv_g + 1.
      cge(7) = bm_g + mu_g + bv_g + 2.
      cge(8) = bm_g + mu_g + bv_g + 3.
      cge(9) = mu_g + bv_g + 3.
      cge(10) = mu_g + 2.
      cge(11) = 0.5*(bv_g + 5. + 2.*mu_g)
      cge(12) = 0.5*(bv_g + 5.) + mu_g
      do n = 1, 12
         cgg(n) = WGAMMA(cge(n))
      enddo
      oamg = 1./am_g
      obmg = 1./bm_g
      ocmg = oamg**obmg
      oge1 = 1./cge(1)
      ogg1 = 1./cgg(1)
      ogg2 = 1./cgg(2)
      ogg3 = 1./cgg(3)
      t1_qr_qc = PI*.25*av_r * crg(9)
      t1_qr_qi = PI*.25*av_r * crg(9)
      t2_qr_qi = PI*.25*am_r*av_r * crg(8)
      t1_qg_qc = PI*.25*av_g * cgg(9)
      t1_qs_qc = PI*.25*av_s
      t1_qs_qi = PI*.25*av_s
      t1_qr_ev = 0.78 * crg(10)
      t2_qr_ev = 0.308*Sc3*SQRT(av_r) * crg(11)
      t1_qs_sd = 0.86
      t2_qs_sd = 0.28*Sc3*SQRT(av_s)
      t1_qs_me = PI*4.*C_sqrd*olfus * 0.86
      t2_qs_me = PI*4.*C_sqrd*olfus * 0.28*Sc3*SQRT(av_s)
      t1_qg_sd = 0.86 * cgg(10)
      t2_qg_sd = 0.28*Sc3*SQRT(av_g) * cgg(11)
      t1_qg_me = PI*4.*C_cube*olfus * 0.86 * cgg(10)
      t2_qg_me = PI*4.*C_cube*olfus * 0.28*Sc3*SQRT(av_g) * cgg(11)
      nic2 = NINT(ALOG10(r_c(1)))
      nii2 = NINT(ALOG10(r_i(1)))
      nii3 = NINT(ALOG10(Nt_i(1)))
      nir2 = NINT(ALOG10(r_r(1)))
      nir3 = NINT(ALOG10(N0r_exp(1)))
      nis2 = NINT(ALOG10(r_s(1)))
      nig2 = NINT(ALOG10(r_g(1)))
      nig3 = NINT(ALOG10(N0g_exp(1)))
      niIN2 = NINT(ALOG10(Nt_IN(1)))
      Dc(1) = D0c*1.0d0
      dtc(1) = D0c*1.0d0
      do n = 2, nbc
         Dc(n) = Dc(n-1) + 1.0D-6
         dtc(n) = (Dc(n) - Dc(n-1))
      enddo
      xDx(1) = D0i*1.0d0
      xDx(nbi+1) = 5.0d0*D0s
      do n = 2, nbi
         xDx(n) = DEXP(REAL(n-1,KIND=R8SIZE)/REAL(nbi,KIND=R8SIZE) &
                  *DLOG(xDx(nbi+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbi
         Di(n) = DSQRT(xDx(n)*xDx(n+1))
         dti(n) = xDx(n+1) - xDx(n)
      enddo
      xDx(1) = D0r*1.0d0
      xDx(nbr+1) = 0.005d0
      do n = 2, nbr
         xDx(n) = DEXP(REAL(n-1,KIND=R8SIZE)/REAL(nbr,KIND=R8SIZE) &
                  *DLOG(xDx(nbr+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbr
         Dr(n) = DSQRT(xDx(n)*xDx(n+1))
         dtr(n) = xDx(n+1) - xDx(n)
      enddo
      xDx(1) = D0s*1.0d0
      xDx(nbs+1) = 0.02d0
      do n = 2, nbs
         xDx(n) = DEXP(REAL(n-1,KIND=R8SIZE)/REAL(nbs,KIND=R8SIZE) &
                  *DLOG(xDx(nbs+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbs
         Ds(n) = DSQRT(xDx(n)*xDx(n+1))
         dts(n) = xDx(n+1) - xDx(n)
      enddo
      xDx(1) = D0g*1.0d0
      xDx(nbg+1) = 0.05d0
      do n = 2, nbg
         xDx(n) = DEXP(REAL(n-1,KIND=R8SIZE)/REAL(nbg,KIND=R8SIZE) &
                  *DLOG(xDx(nbg+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbg
         Dg(n) = DSQRT(xDx(n)*xDx(n+1))
         dtg(n) = xDx(n+1) - xDx(n)
      enddo
      xDx(1) = 1.0d0
      xDx(nbc+1) = 3000.0d0
      do n = 2, nbc
         xDx(n) = DEXP(REAL(n-1,KIND=R8SIZE)/REAL(nbc,KIND=R8SIZE)                          &
                  *DLOG(xDx(nbc+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbc
         t_Nc(n) = DSQRT(xDx(n)*xDx(n+1)) * 1.D6
      enddo
      nic1 = DLOG(t_Nc(nbc)/t_Nc(1))
      do m = 1, ntb_r
         do k = 1, ntb_r1
            do j = 1, ntb_g
               do i = 1, ntb_g1
                  tcg_racg(i,j,k,m) = 0.0d0
                  tmr_racg(i,j,k,m) = 0.0d0
                  tcr_gacr(i,j,k,m) = 0.0d0
                  tmg_gacr(i,j,k,m) = 0.0d0
                  tnr_racg(i,j,k,m) = 0.0d0
                  tnr_gacr(i,j,k,m) = 0.0d0
               enddo
            enddo
         enddo
      enddo
      do m = 1, ntb_r
         do k = 1, ntb_r1
            do j = 1, ntb_t
               do i = 1, ntb_s
                  tcs_racs1(i,j,k,m) = 0.0d0
                  tmr_racs1(i,j,k,m) = 0.0d0
                  tcs_racs2(i,j,k,m) = 0.0d0
                  tmr_racs2(i,j,k,m) = 0.0d0
                  tcr_sacr1(i,j,k,m) = 0.0d0
                  tms_sacr1(i,j,k,m) = 0.0d0
                  tcr_sacr2(i,j,k,m) = 0.0d0
                  tms_sacr2(i,j,k,m) = 0.0d0
                  tnr_racs1(i,j,k,m) = 0.0d0
                  tnr_racs2(i,j,k,m) = 0.0d0
                  tnr_sacr1(i,j,k,m) = 0.0d0
                  tnr_sacr2(i,j,k,m) = 0.0d0
               enddo
            enddo
         enddo
      enddo
      do m = 1, ntb_IN
         do k = 1, 45
            do j = 1, ntb_r1
               do i = 1, ntb_r
                  tpi_qrfz(i,j,k,m) = 0.0d0
                  tni_qrfz(i,j,k,m) = 0.0d0
                  tpg_qrfz(i,j,k,m) = 0.0d0
                  tnr_qrfz(i,j,k,m) = 0.0d0
               enddo
            enddo
            do j = 1, nbc
               do i = 1, ntb_c
                  tpi_qcfz(i,j,k,m) = 0.0d0
                  tni_qcfz(i,j,k,m) = 0.0d0
               enddo
            enddo
         enddo
      enddo
      do j = 1, ntb_i1
         do i = 1, ntb_i
            tps_iaus(i,j) = 0.0d0
            tni_iaus(i,j) = 0.0d0
            tpi_ide(i,j) = 0.0d0
         enddo
      enddo
      do j = 1, nbc
         do i = 1, nbr
            t_Efrw(i,j) = 0.0
         enddo
         do i = 1, nbs
            t_Efsw(i,j) = 0.0
         enddo
      enddo
      do k = 1, ntb_r
         do j = 1, ntb_r1
            do i = 1, nbr
               tnr_rev(i,j,k) = 0.0d0
            enddo
         enddo
      enddo
      do k = 1, nbc
         do j = 1, ntb_c
            do i = 1, nbc
               tpc_wev(i,j,k) = 0.0d0
               tnc_wev(i,j,k) = 0.0d0
            enddo
         enddo
      enddo
      do m = 1, ntb_ark
         do l = 1, ntb_arr
            do k = 1, ntb_art
               do j = 1, ntb_arw
                  do i = 1, ntb_arc
                     tnccn_act(i,j,k,l,m) = 1.0
                  enddo
               enddo
            enddo
         enddo
      enddo
      call table_Efrw
      call table_Efsw
      call table_dropEvap
      call mpas_new_unit(mp_unit, unformatted = .true.)
      if (swap_tables) then
      open(unit=mp_unit,file='MP_THOMPSON_QRacrQG_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           convert='big_endian', iostat = istat)
      else
      open(unit=mp_unit,file='MP_THOMPSON_QRacrQG_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           iostat = istat)
      endif
      if(istat /= open_OK) &
         call physics_error_fatal('subroutine thompson_init: ' // &
                                  'failure opening MP_THOMPSON_QRacrQG.DBL')
      read(mp_unit) tcg_racg
      read(mp_unit) tmr_racg
      read(mp_unit) tcr_gacr
      read(mp_unit) tmg_gacr
      read(mp_unit) tnr_racg
      read(mp_unit) tnr_gacr
      close(unit=mp_unit)
      if (swap_tables) then
      open(unit=mp_unit,file='MP_THOMPSON_QRacrQS_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           convert='big_endian', iostat=istat)
      else
      open(unit=mp_unit,file='MP_THOMPSON_QRacrQS_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           iostat=istat)
      endif
      if(istat /= open_OK) &
         call physics_error_fatal('subroutine thompson_init: ' // &
                                  'failure opening MP_THOMPSON_QRacrQS.DBL')
      read(mp_unit) tcs_racs1
      read(mp_unit) tmr_racs1
      read(mp_unit) tcs_racs2
      read(mp_unit) tmr_racs2
      read(mp_unit) tcr_sacr1
      read(mp_unit) tms_sacr1
      read(mp_unit) tcr_sacr2
      read(mp_unit) tms_sacr2
      read(mp_unit) tnr_racs1
      read(mp_unit) tnr_racs2
      read(mp_unit) tnr_sacr1
      read(mp_unit) tnr_sacr2
      close(unit=mp_unit)
      if (swap_tables) then
      open(unit=mp_unit,file='MP_THOMPSON_freezeH2O_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           convert='big_endian', iostat=istat)
      else
      open(unit=mp_unit,file='MP_THOMPSON_freezeH2O_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           iostat=istat)
      endif
      if(istat /= open_OK) &
         call physics_error_fatal('subroutine thompson_init: ' // &
                                  'failure opening MP_THOMPSON_freezeH2O.DBL')
      read(mp_unit) tpi_qrfz
      read(mp_unit) tni_qrfz
      read(mp_unit) tpg_qrfz
      read(mp_unit) tnr_qrfz
      read(mp_unit) tpi_qcfz
      read(mp_unit) tni_qcfz
      close(unit=mp_unit)
      if (swap_tables) then
      open(unit=mp_unit,file='MP_THOMPSON_QIautQS_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           convert='big_endian', iostat=istat)
      else
      open(unit=mp_unit,file='MP_THOMPSON_QIautQS_DATA.DBL',form='UNFORMATTED',status='OLD',action='READ', &
           iostat=istat)
      endif
      if(istat /= open_OK) &
         call physics_error_fatal('subroutine thompson_init: ' // &
                                  'failure opening MP_THOMPSON_QIautQS.DBL')
      read(mp_unit) tpi_ide
      read(mp_unit) tps_iaus
      read(mp_unit) tni_iaus
      close(unit=mp_unit)
      call mpas_release_unit(mp_unit)
      xam_r = am_r
      xbm_r = bm_r
      xmu_r = mu_r
      xam_s = am_s
      xbm_s = bm_s
      xmu_s = mu_s
      xam_g = am_g
      xbm_g = bm_g
      xmu_g = mu_g
      call freezing_tables_at_default_in
      call radar_init
      endif
      END SUBROUTINE thompson_init
      subroutine freezing_tables_at_default_in
! the bin idx_IN of mp_thompson's ice nucleation (module_mp_thompson.F:2307-2318, same expressions), and that slice of the tables
      integer:: idx_IN, niin, n, nn
      real:: xni
           xni = 1.0 *1000.
          if (xni.gt. Nt_IN(1)) then
           niin = NINT(ALOG10(xni))
           do nn = niin-1, niin+1
              n = nn
              if ( (xni/10.**nn).ge.1.0 .and. &
                   (xni/10.**nn).lt.10.0) goto 149
           enddo
 149       continue
           idx_IN = INT(xni/10.**n) + 10*(n-niin2) - (n-niin2)
           idx_IN = MAX(1, MIN(idx_IN, ntb_IN))
      else
           idx_IN = 1
          endif
      if (allocated(tpi_qcfz_in)) deallocate(tpi_qcfz_in, tni_qcfz_in, tpi_qrfz_in, tpg_qrfz_in, tni_qrfz_in, tnr_qrfz_in)
      allocate(tpi_qcfz_in, source=tpi_qcfz(:,:,:,idx_IN))
      allocate(tni_qcfz_in, source=tni_qcfz(:,:,:,idx_IN))
      allocate(tpi_qrfz_in, source=tpi_qrfz(:,:,:,idx_IN))
      allocate(tpg_qrfz_in, source=tpg_qrfz(:,:,:,idx_IN))
      allocate(tni_qrfz_in, source=tni_qrfz(:,:,:,idx_IN))
      allocate(tnr_qrfz_in, source=tnr_qrfz(:,:,:,idx_IN))
      end subroutine freezing_tables_at_default_in
      subroutine mp_thompson (qv1d, qc1d, qi1d, qr1d, qs1d, qg1d, ni1d,   &
                          nr1d, nc1d, nwfa1d, nifa1d, t1d, p1d, w1d, dzq, &
                          pptrain, pptsnow, pptgraul, pptice,             &
                          rainprod, evapprod,                             &
                          dt, ii, jj, Nt_c)
      implicit none
      INTEGER, PARAMETER :: kts=1
      INTEGER, INTENT(IN):: ii, jj
      REAL, INTENT(IN) :: Nt_c
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: &
                          qv1d, qc1d, qi1d, qr1d, qs1d, qg1d, ni1d, &
                          nr1d, nc1d, nwfa1d, nifa1d, t1d
      REAL, DIMENSION(kts:kte), INTENT(IN):: p1d, w1d, dzq
      REAL, INTENT(INOUT):: pptrain, pptsnow, pptgraul, pptice
      REAL, INTENT(IN):: dt
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: &
                          rainprod, evapprod
      REAL, DIMENSION(kts:kte):: tten, qvten, qcten, qiten, &
           qrten, qsten, qgten, niten, nrten, ncten, nwfaten, nifaten
      DOUBLE PRECISION, DIMENSION(kts:kte):: prw_vcd, pnc_wcd, prv_rev, pnr_rev
! the process rates of one level are scalars: rates, limits and tendencies run in one level loop (lean)
      DOUBLE PRECISION:: pnc_wau, pnc_rcw, pnc_scw, pnc_gcw, pna_rca, pna_sca, pna_gca, pnd_rcd, &
            pnd_scd, pnd_gcd, prr_wau, prr_rcw, prr_rcs, prr_rcg, prr_sml, prr_gml, prr_rci, &
            pnr_wau, pnr_rcs, pnr_rcg, pnr_rci, pnr_sml, pnr_gml, pnr_rcr, pnr_rfz, pri_inu, &
            pni_inu, pri_ihm, pni_ihm, pri_wfz, pni_wfz, pri_rfz, pni_rfz, pri_ide, pni_ide, &
            pri_rci, pni_rci, pni_sci, pni_iau, pri_iha, pni_iha, prs_iau, prs_sci, prs_rcs, &
            prs_scw, prs_sde, prs_ihm, prs_ide, prg_scw, prg_rfz, prg_gde, prg_gcw, prg_rci, &
            prg_rcs, prg_rcg, prg_ihm
      DOUBLE PRECISION, DIMENSION(kts:kte):: evap_ice   ! min(0,prs_sde) + min(0,prg_gde) of each level, for evapprod
      LOGICAL, DIMENSION(kts:kte):: L_gml               ! prr_gml > 0 of each level, for the rain evaporation
      DOUBLE PRECISION, PARAMETER:: zeroD0 = 0.0d0
      REAL, DIMENSION(kts:kte):: temp, pres, qv
      REAL, DIMENSION(kts:kte):: rc, ri, rr, rs, rg, ni, nr, nc, nwfa, nifa
      REAL, DIMENSION(kts:kte):: rho, rhof, rhof2
      REAL, DIMENSION(kts:kte):: qvs, qvsi, delQvs
      REAL, DIMENSION(kts:kte):: satw, sati, ssatw, ssati
      REAL, DIMENSION(kts:kte):: diffu, visco, vsc2, &
           tcond, lvap, ocp, lvt2
      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilamg, N0_r, N0_g
      REAL, DIMENSION(kts:kte):: mvd_r, mvd_c
      REAL, DIMENSION(kts:kte):: smob, smo2, smo1, smo0, &
           smoc, smod, smoe, smof
      REAL, DIMENSION(kts:kte):: sed_r, sed_s, sed_g, sed_i, sed_n,sed_c
      REAL:: rgvm, delta_tp, orho, lfus2
      REAL, DIMENSION(5):: onstep
      DOUBLE PRECISION:: N0_exp, N0_min, lam_exp, lamc, lamr, lamg
      DOUBLE PRECISION:: lami, ilami, ilamc
      REAL:: xDc, Dc_b, Dc_g, xDi, xDr, xDs, xDg, Ds_m, Dg_m
      DOUBLE PRECISION:: Dr_star, Dc_star
      REAL:: zeta1, zeta, taud, tau
      REAL:: stoke_r, stoke_s, stoke_g, stoke_i
      REAL:: vti, vtr, vts, vtg, vtc
      REAL, DIMENSION(kts:kte+1):: vtik, vtnik, vtrk, vtnrk, vtsk, vtgk,  &
           vtck, vtnck
      REAL, DIMENSION(kts:kte):: vts_boost
      REAL:: Mrat, ils1, ils2, t1_vts, t2_vts, t3_vts, t4_vts, C_snow
      REAL:: a_, b_, loga_, A1, A2, tf
      REAL:: tempc, tc0, r_mvd1, r_mvd2, xkrat
      REAL:: xnc, xri, xni, xmi, oxmi, xrc, xrr, xnr
      REAL:: xsat, rate_max, sump, ratio
      REAL:: clap, fcd, dfcd
      REAL:: otemp, rvs, rvs_p, rvs_pp, gamsc, alphsc, t1_evap, t1_subl
      REAL:: r_frac, g_frac
      REAL:: Ef_rw, Ef_sw, Ef_gw, Ef_rr
      REAL:: Ef_ra, Ef_sa, Ef_ga
      REAL:: dtsave, odts, odt, odzq, hgt_agl, SR
      REAL:: xslw1, ygra1, zans1, eva_factor
      INTEGER:: i, k, k2, n, nn, nstep, k_0, kbot, IT, iexfrq
      INTEGER, DIMENSION(5):: ksed1
      INTEGER:: nir, nis, nig, nii, nic, niin
      INTEGER:: idx_tc, idx_t, idx_s, idx_g1, idx_g, idx_r1, idx_r,     &
                idx_i1, idx_i, idx_c, idx, idx_d, idx_n, idx_in
      LOGICAL:: melti, no_micro
      LOGICAL, DIMENSION(kts:kte):: L_qc, L_qi, L_qr, L_qs, L_qg
      LOGICAL:: debug_flag
      character(len=256):: mp_debug
      INTEGER:: nu_c
      LOGICAL, DIMENSION(kts:kte):: L_nifa,L_nwfa
      REAL:: tem
      call initialize_rates
      call prepare_hydrometeors
      call thermodynamics
      if (no_micro) return
      call snow_graupel_moments
      call rain_size_moments
      do k = kts, kte
      call zero_rates
      call warm_rain_rates
      call frozen_rates
      call limit_process_rates
      call assemble_tendencies
      enddo
      call predict_species
      call update_size_distributions
      call condense_cloud
      call evaporate_rain
      call fall_speeds
      call sediment_rain_cloud_ice
      call sediment_snow_graupel_and_freeze
      call apply_tendencies
      contains
      subroutine initialize_rates
! Reference module_mp_thompson.F:1513-1622
      debug_flag = .false.
      if(debug_flag) then
        write(mp_debug, *) 'DEBUG INFO, mp_thompson at (i,j) ', ii, ', ', jj
      endif
      no_micro = .true.
      dtsave = dt
      odt = 1./dt
      odts = 1./dtsave
      iexfrq = 1
      do k = kts, kte
         tten(k) = 0.
         qvten(k) = 0.
         qcten(k) = 0.
         qiten(k) = 0.
         qrten(k) = 0.
         qsten(k) = 0.
         qgten(k) = 0.
         niten(k) = 0.
         nrten(k) = 0.
         ncten(k) = 0.
         nwfaten(k) = 0.
         nifaten(k) = 0.
         prw_vcd(k) = 0.
         pnc_wcd(k) = 0.
         prv_rev(k) = 0.
         pnr_rev(k) = 0.
      enddo
      end subroutine initialize_rates
      subroutine prepare_hydrometeors
! Reference module_mp_thompson.F:1623-1764
      do k = kts, kte
         rainprod(k) = 0.
         evapprod(k) = 0.
      enddo
      do k = kts, kte
         smo0(k) = 0.
         smo1(k) = 0.
         smo2(k) = 0.
         smob(k) = 0.
         smoc(k) = 0.
         smod(k) = 0.
         smoe(k) = 0.
         smof(k) = 0.
         mvd_r(k) = 0.
         mvd_c(k) = 0.
      enddo
      do k = kts, kte
         temp(k) = t1d(k)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
         nwfa(k) = MAX(11.1E6, MIN(9999.E6, nwfa1d(k)*rho(k)))
         nifa(k) = MAX(naIN1*0.01, MIN(9999.E6, nifa1d(k)*rho(k)))
         if (qc1d(k) .gt. R1) then
            no_micro = .false.
            rc(k) = qc1d(k)*rho(k)
            nc(k) = MAX(2., MIN(nc1d(k)*rho(k), Nt_c_max))
            L_qc(k) = .true.
            nu_c = MIN(15, NINT(1000.E6/nc(k)) + 2)
            lamc = (nc(k)*am_r*ccg(2,nu_c)*ocg1(nu_c)/rc(k))**obmr
            xDc = (bm_r + nu_c + 1.) / lamc
            if (xDc.lt. D0c) then
             lamc = cce(2,nu_c)/D0c
            elseif (xDc.gt. D0r*2.) then
             lamc = cce(2,nu_c)/(D0r*2.)
            endif
            nc(k) = MIN( DBLE(Nt_c_max), ccg(1,nu_c)*ocg2(nu_c)*rc(k)   &
                  / am_r*lamc**bm_r)
      nc(k) = Nt_c
      else
            qc1d(k) = 0.0
            nc1d(k) = 0.0
            rc(k) = R1
            nc(k) = 2.
            L_qc(k) = .false.
         endif
         if (qi1d(k) .gt. R1) then
            no_micro = .false.
            ri(k) = qi1d(k)*rho(k)
            ni(k) = MAX(R2, ni1d(k)*rho(k))
            L_qi(k) = .true.
            if (ni(k).le. R2) then
               lami = cie(2)/5.E-6
               ni(k) = MIN(9999.D3, cig(1)*oig2*ri(k)/am_i*lami**bm_i)
            endif
            lami = (am_i*cig(2)*oig1*ni(k)/ri(k))**obmi
            ilami = 1./lami
            xDi = (bm_i + mu_i + 1.) * ilami
            if (xDi.lt. 5.E-6) then
             lami = cie(2)/5.E-6
             ni(k) = MIN(9999.D3, cig(1)*oig2*ri(k)/am_i*lami**bm_i)
            elseif (xDi.gt. 300.E-6) then
             lami = cie(2)/300.E-6
             ni(k) = cig(1)*oig2*ri(k)/am_i*lami**bm_i
            endif
      else
            qi1d(k) = 0.0
            ni1d(k) = 0.0
            ri(k) = R1
            ni(k) = R2
            L_qi(k) = .false.
         endif
         if (qr1d(k) .gt. R1) then
            no_micro = .false.
            rr(k) = qr1d(k)*rho(k)
            nr(k) = MAX(R2, nr1d(k)*rho(k))
            if (nr(k).le. R2) then
               mvd_r(k) = 1.0E-3
               lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
               nr(k) = crg(2)*org3*rr(k)*lamr**bm_r / am_r
            endif
            L_qr(k) = .true.
            lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
            mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
            if (mvd_r(k) .gt. 2.5E-3) then
               mvd_r(k) = 2.5E-3
               lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
               nr(k) = crg(2)*org3*rr(k)*lamr**bm_r / am_r
            elseif (mvd_r(k) .lt. D0r*0.75) then
               mvd_r(k) = D0r*0.75
               lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
               nr(k) = crg(2)*org3*rr(k)*lamr**bm_r / am_r
            endif
      else
            qr1d(k) = 0.0
            nr1d(k) = 0.0
            rr(k) = R1
            nr(k) = R2
            L_qr(k) = .false.
         endif
         if (qs1d(k) .gt. R1) then
            no_micro = .false.
            rs(k) = qs1d(k)*rho(k)
            L_qs(k) = .true.
      else
            qs1d(k) = 0.0
            rs(k) = R1
            L_qs(k) = .false.
         endif
         if (qg1d(k) .gt. R1) then
            no_micro = .false.
            rg(k) = qg1d(k)*rho(k)
            L_qg(k) = .true.
      else
            qg1d(k) = 0.0
            rg(k) = R1
            L_qg(k) = .false.
         endif
      enddo
      end subroutine prepare_hydrometeors
      subroutine thermodynamics
! Reference module_mp_thompson.F:1765-1818
      do k = kts, kte
         tempc = temp(k) - 273.15
         rhof(k) = SQRT(RHO_NOT/rho(k))
         rhof2(k) = SQRT(rhof(k))
         qvs(k) = rslf(pres(k), temp(k))
         delQvs(k) = MAX(0.0, rslf(pres(k), 273.15)-qv(k))
         if (tempc .le. 0.0) then
          qvsi(k) = rsif(pres(k), temp(k))
      else
          qvsi(k) = qvs(k)
         endif
         satw(k) = qv(k)/qvs(k)
         sati(k) = qv(k)/qvsi(k)
         ssatw(k) = satw(k) - 1.
         ssati(k) = sati(k) - 1.
         if (abs(ssatw(k)).lt. eps) ssatw(k) = 0.0
         if (abs(ssati(k)).lt. eps) ssati(k) = 0.0
         if (no_micro .and. ssati(k).gt. 0.0) no_micro = .false.
         diffu(k) = 2.11E-5*(temp(k)/273.15)**1.94 * (101325./pres(k))
         if (tempc .ge. 0.0) then
            visco(k) = (1.718+0.0049*tempc)*1.0E-5
      else
            visco(k) = (1.718+0.0049*tempc-1.2E-5*tempc*tempc)*1.0E-5
         endif
         ocp(k) = 1./(Cp*(1.+0.887*qv(k)))
         vsc2(k) = SQRT(rho(k)/visco(k))
         lvap(k) = lvap0 + (2106.0 - 4218.0)*tempc
         tcond(k) = (5.69 + 0.0168*tempc)*1.0E-5 * 418.936
      enddo
      end subroutine thermodynamics
      subroutine snow_graupel_moments
! Reference module_mp_thompson.F:1820-1935
      do k = kts, kte
         if (.not. L_qs(k)) CYCLE
         tc0 = MIN(-0.1, temp(k)-273.15)
         smob(k) = rs(k)*oams
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2(k) = smob(k)
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
            smo2(k) = (smob(k)/a_)**(1./b_)
         endif
         loga_ = sa(1) + sa(2)*tc0 + sa(5)*tc0*tc0 + sa(9)*tc0*tc0*tc0
         a_ = 10.0**loga_
         b_ = sb(1) + sb(2)*tc0 + sb(5)*tc0*tc0 + sb(9)*tc0*tc0*tc0
         smo0(k) = a_ * smo2(k)**b_
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
         smo1(k) = a_ * smo2(k)**b_
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
         smoc(k) = a_ * smo2(k)**b_
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
         smoe(k) = a_ * smo2(k)**b_
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
         smof(k) = a_ * smo2(k)**b_
      enddo
      N0_min = gonv_max
      k_0 = kts
      do k = kte, kts, -1
         if (temp(k).ge.270.65) k_0 = MAX(k_0, k)
      enddo
      do k = kte, kts, -1
         if (k.gt.k_0 .and. L_qr(k) .and. mvd_r(k).gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r(k))
      else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, rg(k)))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0_exp = 10.**(zans1)
         N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
         N0_min = MIN(N0_exp, N0_min)
         N0_exp = N0_min
         lam_exp = (N0_exp*am_g*cgg(1)/rg(k))**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg(k) = 1./lamg
         N0_g(k) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      enddo
      end subroutine snow_graupel_moments
      subroutine rain_size_moments
! Reference module_mp_thompson.F:1936-2017
      do k = kte, kts, -1
         lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
         ilamr(k) = 1./lamr
         mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
         N0_r(k) = nr(k)*org2*lamr**cre(2)
      enddo
      end subroutine rain_size_moments
      subroutine zero_rates
      pnc_wau = 0.
      pnc_rcw = 0.
      pnc_scw = 0.
      pnc_gcw = 0.
      pna_rca = 0.
      pna_sca = 0.
      pna_gca = 0.
      pnd_rcd = 0.
      pnd_scd = 0.
      pnd_gcd = 0.
      prr_wau = 0.
      prr_rcw = 0.
      prr_rcs = 0.
      prr_rcg = 0.
      prr_sml = 0.
      prr_gml = 0.
      prr_rci = 0.
      pnr_wau = 0.
      pnr_rcs = 0.
      pnr_rcg = 0.
      pnr_rci = 0.
      pnr_sml = 0.
      pnr_gml = 0.
      pnr_rcr = 0.
      pnr_rfz = 0.
      pri_inu = 0.
      pni_inu = 0.
      pri_ihm = 0.
      pni_ihm = 0.
      pri_wfz = 0.
      pni_wfz = 0.
      pri_rfz = 0.
      pni_rfz = 0.
      pri_ide = 0.
      pni_ide = 0.
      pri_rci = 0.
      pni_rci = 0.
      pni_sci = 0.
      pni_iau = 0.
      pri_iha = 0.
      pni_iha = 0.
      prs_iau = 0.
      prs_sci = 0.
      prs_rcs = 0.
      prs_scw = 0.
      prs_sde = 0.
      prs_ihm = 0.
      prs_ide = 0.
      prg_scw = 0.
      prg_rfz = 0.
      prg_gde = 0.
      prg_gcw = 0.
      prg_rci = 0.
      prg_rcs = 0.
      prg_rcg = 0.
      prg_ihm = 0.
      end subroutine zero_rates
      subroutine warm_rain_rates
! one level k (the caller's loop)
         if (L_qr(k) .and. mvd_r(k).gt. D0r) then
             Ef_rr = 1.0 - EXP(2300.0*(mvd_r(k)-1950.0E-6))
          pnr_rcr = Ef_rr * 2.0*nr(k)*rr(k)
         endif
         mvd_c(k) = D0c
         if (L_qc(k)) then
          nu_c = MIN(15, NINT(1000.E6/nc(k)) + 2)
          xDc = MAX(D0c*1.E6, ((rc(k)/(am_r*nc(k)))**obmr) * 1.E6)
          lamc = (nc(k)*am_r* ccg(2,nu_c) * ocg1(nu_c) / rc(k))**obmr
          mvd_c(k) = (3.0+nu_c+0.672) / lamc
         endif
         if (rc(k).gt. 0.01e-3) then
          Dc_g = ((ccg(3,nu_c)*ocg2(nu_c))**obmr / lamc) * 1.E6
          Dc_b = (xDc*xDc*xDc*Dc_g*Dc_g*Dc_g - xDc*xDc*xDc*xDc*xDc*xDc) &
                 **(1./6.)
          zeta1 = 0.5*((6.25E-6*xDc*Dc_b*Dc_b*Dc_b - 0.4) &
                     + abs(6.25E-6*xDc*Dc_b*Dc_b*Dc_b - 0.4))
          zeta = 0.027*rc(k)*zeta1
          taud = 0.5*((0.5*Dc_b - 7.5) + abs(0.5*Dc_b - 7.5)) + R1
          tau  = 3.72/(rc(k)*taud)
          prr_wau = zeta/tau
          prr_wau = MIN(DBLE(rc(k)*odts), prr_wau)
          pnr_wau = prr_wau / (am_r*nu_c*200.*D0r*D0r*D0r)         ! RAIN2M
          pnc_wau = MIN(DBLE(nc(k)*odts), prr_wau                 &
                     / (am_r*mvd_c(k)*mvd_c(k)*mvd_c(k)))                   ! Qc2M
         endif
         if (L_qr(k) .and. mvd_r(k).gt. D0r .and. mvd_c(k).gt. D0c) then
          lamr = 1./ilamr(k)
          idx = 1 + INT(nbr*DLOG(mvd_r(k)/Dr(1))/DLOG(Dr(nbr)/Dr(1)))
          idx = MIN(idx, nbr)
          Ef_rw = t_Efrw(idx, INT(mvd_c(k)*1.E6))
          prr_rcw = rhof(k)*t1_qr_qc*Ef_rw*rc(k)*N0_r(k) &
                         *((lamr+fv_r)**(-cre(9)))
          prr_rcw = MIN(DBLE(rc(k)*odts), prr_rcw)
          pnc_rcw = rhof(k)*t1_qr_qc*Ef_rw*nc(k)*N0_r(k)             &
                         *((lamr+fv_r)**(-cre(9)))                          ! Qc2M
          pnc_rcw = MIN(DBLE(nc(k)*odts), pnc_rcw)
         endif
         if (L_qr(k) .and. mvd_r(k).gt. D0r) then
          Ef_ra = Eff_aero(mvd_r(k),0.04E-6,visco(k),rho(k),temp(k),'r')
          lamr = 1./ilamr(k)
          pna_rca = rhof(k)*t1_qr_qc*Ef_ra*nwfa(k)*N0_r(k)           &
                         *((lamr+fv_r)**(-cre(9)))
          pna_rca = MIN(DBLE(nwfa(k)*odts), pna_rca)
          Ef_ra = Eff_aero(mvd_r(k),0.8E-6,visco(k),rho(k),temp(k),'r')
          pnd_rcd = rhof(k)*t1_qr_qc*Ef_ra*nifa(k)*N0_r(k)           &
                         *((lamr+fv_r)**(-cre(9)))
          pnd_rcd = MIN(DBLE(nifa(k)*odts), pnd_rcd)
         endif
      end subroutine warm_rain_rates
      subroutine frozen_rates
! Reference module_mp_thompson.F:2018-2567 (one level k)
         vts_boost(k) = 1.0
      call frozen_lookup_indices
      call frozen_collection
         if (temp(k).lt.T_0) then
      call ice_nucleation
      call ice_growth_collection
      call riming_and_splintering
      else
      call melt_snow_graupel
         endif
      end subroutine frozen_rates
      subroutine limit_process_rates
! Reference module_mp_thompson.F:2568-2668 (one level k)
         sump = pri_inu + pri_ide + prs_ide &
              + prs_sde + prg_gde + pri_iha
         rate_max = (qv(k)-qvsi(k))*rho(k)*odts*0.999
         if ( (sump.gt. eps .and. sump.gt. rate_max) .or. &
              (sump.lt. -eps .and. sump.lt. rate_max) ) then
          ratio = rate_max/sump
          pri_inu = pri_inu * ratio
          pri_ide = pri_ide * ratio
          pni_ide = pni_ide * ratio
          prs_ide = prs_ide * ratio
          prs_sde = prs_sde * ratio
          prg_gde = prg_gde * ratio
          pri_iha = pri_iha * ratio
         endif
         sump = -prr_wau - pri_wfz - prr_rcw &
                - prs_scw - prg_scw - prg_gcw
         rate_max = -rc(k)*odts
         if (sump.lt. rate_max .and. L_qc(k)) then
          ratio = rate_max/sump
          prr_wau = prr_wau * ratio
          pri_wfz = pri_wfz * ratio
          prr_rcw = prr_rcw * ratio
          prs_scw = prs_scw * ratio
          prg_scw = prg_scw * ratio
          prg_gcw = prg_gcw * ratio
         endif
         sump = pri_ide - prs_iau - prs_sci &
                - pri_rci
         rate_max = -ri(k)*odts
         if (sump.lt. rate_max .and. L_qi(k)) then
          ratio = rate_max/sump
          pri_ide = pri_ide * ratio
          prs_iau = prs_iau * ratio
          prs_sci = prs_sci * ratio
          pri_rci = pri_rci * ratio
         endif
         sump = -prg_rfz - pri_rfz - prr_rci &
                + prr_rcs + prr_rcg
         rate_max = -rr(k)*odts
         if (sump.lt. rate_max .and. L_qr(k)) then
          ratio = rate_max/sump
          prg_rfz = prg_rfz * ratio
          pri_rfz = pri_rfz * ratio
          prr_rci = prr_rci * ratio
          prr_rcs = prr_rcs * ratio
          prr_rcg = prr_rcg * ratio
         endif
         sump = prs_sde - prs_ihm - prr_sml &
                + prs_rcs
         rate_max = -rs(k)*odts
         if (sump.lt. rate_max .and. L_qs(k)) then
          ratio = rate_max/sump
          prs_sde = prs_sde * ratio
          prs_ihm = prs_ihm * ratio
          prr_sml = prr_sml * ratio
          prs_rcs = prs_rcs * ratio
         endif
         sump = prg_gde - prg_ihm - prr_gml &
              + prg_rcg
         rate_max = -rg(k)*odts
         if (sump.lt. rate_max .and. L_qg(k)) then
          ratio = rate_max/sump
          prg_gde = prg_gde * ratio
          prg_ihm = prg_ihm * ratio
          prr_gml = prr_gml * ratio
          prg_rcg = prg_rcg * ratio
         endif
         pri_ihm = prs_ihm + prg_ihm
         ratio = MIN( ABS(prr_rcg), ABS(prg_rcg) )
         prr_rcg = ratio * SIGN(1.0, SNGL(prr_rcg))
         prg_rcg = -prr_rcg
         if (temp(k).gt.T_0) then
            ratio = MIN( ABS(prr_rcs), ABS(prs_rcs) )
            prr_rcs = ratio * SIGN(1.0, SNGL(prr_rcs))
            prs_rcs = -prr_rcs
         endif
      end subroutine limit_process_rates
      subroutine assemble_tendencies
! Reference module_mp_thompson.F:2669-2836 (one level k)
         orho = 1./rho(k)
         lfus2 = lsub - lvap(k)
         qvten(k) = qvten(k) + (-pri_inu - pri_iha - pri_ide   &
                      - prs_ide - prs_sde - prg_gde) &
                      * orho
         qcten(k) = qcten(k) + (-prr_wau - pri_wfz &
                      - prr_rcw - prs_scw - prg_scw &
                      - prg_gcw) &
                      * orho
         ncten(k) = ncten(k) + (-pnc_wau - pnc_rcw &
                      - pni_wfz - pnc_scw - pnc_gcw) &
                      * orho
         xrc=MAX(R1, (qc1d(k) + qcten(k)*dtsave)*rho(k))
         xnc=MAX(2., (nc1d(k) + ncten(k)*dtsave)*rho(k))
         if (xrc .gt. R1) then
          nu_c = MIN(15, NINT(1000.E6/xnc) + 2)
          lamc = (xnc*am_r*ccg(2,nu_c)*ocg1(nu_c)/rc(k))**obmr
          xDc = (bm_r + nu_c + 1.) / lamc
          if (xDc.lt. D0c) then
           lamc = cce(2,nu_c)/D0c
           xnc = ccg(1,nu_c)*ocg2(nu_c)*xrc/am_r*lamc**bm_r
           ncten(k) = (xnc-nc1d(k)*rho(k))*odts*orho
          elseif (xDc.gt. D0r*2.) then
           lamc = cce(2,nu_c)/(D0r*2.)
           xnc = ccg(1,nu_c)*ocg2(nu_c)*xrc/am_r*lamc**bm_r
           ncten(k) = (xnc-nc1d(k)*rho(k))*odts*orho
          endif
      else
          ncten(k) = -nc1d(k)*odts
         endif
         xnc=MAX(0.,(nc1d(k) + ncten(k)*dtsave)*rho(k))
         if (xnc.gt.Nt_c_max) &
                ncten(k) = (Nt_c_max-nc1d(k)*rho(k))*odts*orho
         qiten(k) = qiten(k) + (pri_inu + pri_iha + pri_ihm    &
                      + pri_wfz + pri_rfz + pri_ide &
                      - prs_iau - prs_sci - pri_rci) &
                      * orho
         niten(k) = niten(k) + (pni_inu + pni_iha + pni_ihm    &
                      + pni_wfz + pni_rfz + pni_ide &
                      - pni_iau - pni_sci - pni_rci) &
                      * orho
         xri=MAX(R1,(qi1d(k) + qiten(k)*dtsave)*rho(k))
         xni=MAX(R2,(ni1d(k) + niten(k)*dtsave)*rho(k))
         if (xri.gt. R1) then
           lami = (am_i*cig(2)*oig1*xni/xri)**obmi
           ilami = 1./lami
           xDi = (bm_i + mu_i + 1.) * ilami
           if (xDi.lt. 5.E-6) then
            lami = cie(2)/5.E-6
            xni = MIN(9999.D3, cig(1)*oig2*xri/am_i*lami**bm_i)
            niten(k) = (xni-ni1d(k)*rho(k))*odts*orho
           elseif (xDi.gt. 300.E-6) then
            lami = cie(2)/300.E-6
            xni = cig(1)*oig2*xri/am_i*lami**bm_i
            niten(k) = (xni-ni1d(k)*rho(k))*odts*orho
           endif
      else
          niten(k) = -ni1d(k)*odts
         endif
         xni=MAX(0.,(ni1d(k) + niten(k)*dtsave)*rho(k))
         if (xni.gt.9999.E3) &
                niten(k) = (9999.E3-ni1d(k)*rho(k))*odts*orho
         qrten(k) = qrten(k) + (prr_wau + prr_rcw &
                      + prr_sml + prr_gml + prr_rcs &
                      + prr_rcg - prg_rfz &
                      - pri_rfz - prr_rci) &
                      * orho
         nrten(k) = nrten(k) + (pnr_wau + pnr_sml + pnr_gml    &
                      - (pnr_rfz + pnr_rcr + pnr_rcg           &
                      + pnr_rcs + pnr_rci + pni_rfz) )         &
                      * orho
         xrr=MAX(R1,(qr1d(k) + qrten(k)*dtsave)*rho(k))
         xnr=MAX(R2,(nr1d(k) + nrten(k)*dtsave)*rho(k))
         if (xrr.gt. R1) then
           lamr = (am_r*crg(3)*org2*xnr/xrr)**obmr
           mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
           if (mvd_r(k) .gt. 2.5E-3) then
              mvd_r(k) = 2.5E-3
              lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
              xnr = crg(2)*org3*xrr*lamr**bm_r / am_r
              nrten(k) = (xnr-nr1d(k)*rho(k))*odts*orho
           elseif (mvd_r(k) .lt. D0r*0.75) then
              mvd_r(k) = D0r*0.75
              lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
              xnr = crg(2)*org3*xrr*lamr**bm_r / am_r
              nrten(k) = (xnr-nr1d(k)*rho(k))*odts*orho
           endif
      else
           qrten(k) = -qr1d(k)*odts
           nrten(k) = -nr1d(k)*odts
         endif
         qsten(k) = qsten(k) + (prs_iau + prs_sde &
                      + prs_sci + prs_scw + prs_rcs &
                      + prs_ide - prs_ihm - prr_sml) &
                      * orho
         qgten(k) = qgten(k) + (prg_scw + prg_rfz &
                      + prg_gde + prg_rcg + prg_gcw &
                      + prg_rci + prg_rcs - prg_ihm &
                      - prr_gml) &
                      * orho
         if (temp(k).lt.T_0) then
          tten(k) = tten(k) &
                    + ( lsub*ocp(k)*(pri_inu + pri_ide &
                                     + prs_ide + prs_sde &
                                     + prg_gde + pri_iha) &
                     + lfus2*ocp(k)*(pri_wfz + pri_rfz &
                                     + prg_rfz + prs_scw &
                                     + prg_scw + prg_gcw &
                                     + prg_rcs + prs_rcs &
                                     + prr_rci + prg_rcg) &
                       )*orho * (1-IFDRY)
      else
          tten(k) = tten(k) &
                    + ( lfus*ocp(k)*(-prr_sml - prr_gml &
                                     - prr_rcg - prr_rcs) &
                      + lsub*ocp(k)*(prs_sde + prg_gde) &
                       )*orho * (1-IFDRY)
         endif
! what the rain evaporation and the production diagnostics need of this level's rates (module_mp_thompson.F:3171, 3213-3218)
         L_gml(k) = prr_gml.gt.0.0
         evap_ice(k) = min(zeroD0,prs_sde) + min(zeroD0,prg_gde)
         rainprod(k) = prr_wau + prr_rcw + prs_scw + &
                                    prg_scw + prs_iau + &
                                    prg_gcw + prs_sci + &
                                    pri_rci
      end subroutine assemble_tendencies
      subroutine predict_species
! Reference module_mp_thompson.F:2837-2925
      do k = kts, kte
         temp(k) = t1d(k) + DT*tten(k)
         otemp = 1./temp(k)
         tempc = temp(k) - 273.15
         qv(k) = MAX(1.E-10, qv1d(k) + DT*qvten(k))
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
         rhof(k) = SQRT(RHO_NOT/rho(k))
         rhof2(k) = SQRT(rhof(k))
         qvs(k) = rslf(pres(k), temp(k))
         ssatw(k) = qv(k)/qvs(k) - 1.
         if (abs(ssatw(k)).lt. eps) ssatw(k) = 0.0
         diffu(k) = 2.11E-5*(temp(k)/273.15)**1.94 * (101325./pres(k))
         if (tempc .ge. 0.0) then
            visco(k) = (1.718+0.0049*tempc)*1.0E-5
      else
            visco(k) = (1.718+0.0049*tempc-1.2E-5*tempc*tempc)*1.0E-5
         endif
         vsc2(k) = SQRT(rho(k)/visco(k))
         lvap(k) = lvap0 + (2106.0 - 4218.0)*tempc
         tcond(k) = (5.69 + 0.0168*tempc)*1.0E-5 * 418.936
         ocp(k) = 1./(Cp*(1.+0.887*qv(k)))
         lvt2(k)=lvap(k)*lvap(k)*ocp(k)*oRv*otemp*otemp
         nwfa(k) = MAX(11.1E6, (nwfa1d(k) + nwfaten(k)*DT)*rho(k))
      enddo
      do k = kts, kte
         if ((qc1d(k) + qcten(k)*DT) .gt. R1) then
            rc(k) = (qc1d(k) + qcten(k)*DT)*rho(k)
            nc(k) = MAX(2., MIN((nc1d(k)+ncten(k)*DT)*rho(k), Nt_c_max))
      nc(k) = Nt_c
            L_qc(k) = .true.
      else
            rc(k) = R1
            nc(k) = 2.
            L_qc(k) = .false.
         endif
         if ((qi1d(k) + qiten(k)*DT) .gt. R1) then
            ri(k) = (qi1d(k) + qiten(k)*DT)*rho(k)
            ni(k) = MAX(R2, (ni1d(k) + niten(k)*DT)*rho(k))
            L_qi(k) = .true.
      else
            ri(k) = R1
            ni(k) = R2
            L_qi(k) = .false.
         endif
         if ((qr1d(k) + qrten(k)*DT) .gt. R1) then
            rr(k) = (qr1d(k) + qrten(k)*DT)*rho(k)
            nr(k) = MAX(R2, (nr1d(k) + nrten(k)*DT)*rho(k))
            L_qr(k) = .true.
            lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
            mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
            if (mvd_r(k) .gt. 2.5E-3) then
               mvd_r(k) = 2.5E-3
               lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
               nr(k) = crg(2)*org3*rr(k)*lamr**bm_r / am_r
            elseif (mvd_r(k) .lt. D0r*0.75) then
               mvd_r(k) = D0r*0.75
               lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
               nr(k) = crg(2)*org3*rr(k)*lamr**bm_r / am_r
            endif
      else
            rr(k) = R1
            nr(k) = R2
            L_qr(k) = .false.
         endif
         if ((qs1d(k) + qsten(k)*DT) .gt. R1) then
            rs(k) = (qs1d(k) + qsten(k)*DT)*rho(k)
            L_qs(k) = .true.
      else
            rs(k) = R1
            L_qs(k) = .false.
         endif
         if ((qg1d(k) + qgten(k)*DT) .gt. R1) then
            rg(k) = (qg1d(k) + qgten(k)*DT)*rho(k)
            L_qg(k) = .true.
      else
            rg(k) = R1
            L_qg(k) = .false.
         endif
      enddo
      end subroutine predict_species
      subroutine update_size_distributions
! Reference module_mp_thompson.F:2926-3015
      do k = kts, kte
         smo2(k) = 0.
         smob(k) = 0.
         smoc(k) = 0.
         smod(k) = 0.
      enddo
      do k = kts, kte
         if (.not. L_qs(k)) CYCLE
         tc0 = MIN(-0.1, temp(k)-273.15)
         smob(k) = rs(k)*oams
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2(k) = smob(k)
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
            smo2(k) = (smob(k)/a_)**(1./b_)
         endif
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
         smoc(k) = a_ * smo2(k)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(14) &
               + sa(4)*tc0*cse(14) + sa(5)*tc0*tc0 &
               + sa(6)*cse(14)*cse(14) + sa(7)*tc0*tc0*cse(14) &
               + sa(8)*tc0*cse(14)*cse(14) + sa(9)*tc0*tc0*tc0 &
               + sa(10)*cse(14)*cse(14)*cse(14)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(14) + sb(4)*tc0*cse(14) &
              + sb(5)*tc0*tc0 + sb(6)*cse(14)*cse(14) &
              + sb(7)*tc0*tc0*cse(14) + sb(8)*tc0*cse(14)*cse(14) &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(14)*cse(14)*cse(14)
         smod(k) = a_ * smo2(k)**b_
      enddo
      N0_min = gonv_max
      k_0 = kts
      do k = kte, kts, -1
         if (temp(k).ge.270.65) k_0 = MAX(k_0, k)
      enddo
      do k = kte, kts, -1
         if (k.gt.k_0 .and. L_qr(k) .and. mvd_r(k).gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r(k))
      else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, rg(k)))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0_exp = 10.**(zans1)
         N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
         N0_min = MIN(N0_exp, N0_min)
         N0_exp = N0_min
         lam_exp = (N0_exp*am_g*cgg(1)/rg(k))**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg(k) = 1./lamg
         N0_g(k) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      enddo
      end subroutine update_size_distributions
      subroutine condense_cloud
! Reference module_mp_thompson.F:3016-3131
      do k = kte, kts, -1
         lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
         ilamr(k) = 1./lamr
         mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
         N0_r(k) = nr(k)*org2*lamr**cre(2)
      enddo
      do k = kts, kte
         orho = 1./rho(k)
         if ( (ssatw(k).gt. eps) .or. (ssatw(k).lt. -eps .and. &
                   L_qc(k)) ) then
          clap = (qv(k)-qvs(k))/(1. + lvt2(k)*qvs(k))
          do n = 1, 3
             fcd = qvs(k)* EXP(lvt2(k)*clap) - qv(k) + clap
             dfcd = qvs(k)*lvt2(k)* EXP(lvt2(k)*clap) + 1.
             clap = clap - fcd/dfcd
          enddo
          xrc = rc(k) + clap*rho(k)
          xnc = 0.
          if (xrc.gt. R1) then
           prw_vcd(k) = clap*odt
           if (clap .gt. eps) then
               xnc = Nt_c
            pnc_wcd(k) = 0.5*(xnc-nc(k) + abs(xnc-nc(k)))*odts*orho
           endif
      else
           prw_vcd(k) = -rc(k)*orho*odt
           pnc_wcd(k) = -nc(k)*orho*odt
          endif
          qvten(k) = qvten(k) - prw_vcd(k)
          qcten(k) = qcten(k) + prw_vcd(k)
          ncten(k) = ncten(k) + pnc_wcd(k)
          nwfaten(k) = nwfaten(k) - pnc_wcd(k)
          tten(k) = tten(k) + lvap(k)*ocp(k)*prw_vcd(k)*(1-IFDRY)
          rc(k) = MAX(R1, (qc1d(k) + DT*qcten(k))*rho(k))
          if (rc(k).eq.R1) L_qc(k) = .false.
          nc(k) = MAX(2., MIN((nc1d(k)+ncten(k)*DT)*rho(k), Nt_c_max))
      nc(k) = Nt_c
          qv(k) = MAX(1.E-10, qv1d(k) + DT*qvten(k))
          temp(k) = t1d(k) + DT*tten(k)
          rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
          qvs(k) = rslf(pres(k), temp(k))
          ssatw(k) = qv(k)/qvs(k) - 1.
         endif
      enddo
      end subroutine condense_cloud
      subroutine evaporate_rain
! Reference module_mp_thompson.F:3132-3219
      do k = kts, kte
         if ( (ssatw(k).lt. -eps) .and. L_qr(k) &
                     .and. (.not.(prw_vcd(k).gt. 0.)) ) then
          tempc = temp(k) - 273.15
          otemp = 1./temp(k)
          orho = 1./rho(k)
          rhof(k) = SQRT(RHO_NOT*orho)
          rhof2(k) = SQRT(rhof(k))
          diffu(k) = 2.11E-5*(temp(k)/273.15)**1.94 * (101325./pres(k))
          if (tempc .ge. 0.0) then
             visco(k) = (1.718+0.0049*tempc)*1.0E-5
      else
             visco(k) = (1.718+0.0049*tempc-1.2E-5*tempc*tempc)*1.0E-5
          endif
          vsc2(k) = SQRT(rho(k)/visco(k))
          lvap(k) = lvap0 + (2106.0 - 4218.0)*tempc
          tcond(k) = (5.69 + 0.0168*tempc)*1.0E-5 * 418.936
          ocp(k) = 1./(Cp*(1.+0.887*qv(k)))
          rvs = rho(k)*qvs(k)
          rvs_p = rvs*otemp*(lvap(k)*otemp*oRv - 1.)
          rvs_pp = rvs * ( otemp*(lvap(k)*otemp*oRv - 1.) &
                          *otemp*(lvap(k)*otemp*oRv - 1.) &
                          + (-2.*lvap(k)*otemp*otemp*otemp*oRv) &
                          + otemp*otemp)
          gamsc = lvap(k)*diffu(k)/tcond(k) * rvs_p
          alphsc = 0.5*(gamsc/(1.+gamsc))*(gamsc/(1.+gamsc)) &
                     * rvs_pp/rvs_p * rvs/rvs_p
          alphsc = MAX(1.E-9, alphsc)
          xsat   = MIN(-1.E-9, ssatw(k))
          t1_evap = 2.*PI*( 1.0 - alphsc*xsat  &
                 + 2.*alphsc*alphsc*xsat*xsat  &
                 - 5.*alphsc*alphsc*alphsc*xsat*xsat*xsat ) &
                 / (1.+gamsc)
          lamr = 1./ilamr(k)
          if (qv(k)/qvs(k) .lt. 0.95 .AND. rr(k)*orho.le.1.E-8) then
          prv_rev(k) = rr(k)*orho*odts
      else
          prv_rev(k) = t1_evap*diffu(k)*(-ssatw(k))*N0_r(k)*rvs &
              * (t1_qr_ev*ilamr(k)**cre(10) &
              + t2_qr_ev*vsc2(k)*rhof2(k)*((lamr+0.5*fv_r)**(-cre(11))))
          rate_max = MIN((rr(k)*orho*odts), (qvs(k)-qv(k))*odts)
          prv_rev(k) = MIN(DBLE(rate_max), prv_rev(k)*orho)
          IF (L_gml(k)) THEN
             eva_factor = MIN(1.0, 0.01+(0.99-0.01)*(tempc/20.0))
             prv_rev(k) = prv_rev(k)*eva_factor
          ENDIF
          endif
          pnr_rev(k) = MIN(DBLE(nr(k)*0.99*orho*odts),                  &   ! RAIN2M
                       prv_rev(k) * nr(k)/rr(k))
          qrten(k) = qrten(k) - prv_rev(k)
          qvten(k) = qvten(k) + prv_rev(k)
          nrten(k) = nrten(k) - pnr_rev(k)
          nwfaten(k) = nwfaten(k) + pnr_rev(k)
          tten(k) = tten(k) - lvap(k)*ocp(k)*prv_rev(k)*(1-IFDRY)
          rr(k) = MAX(R1, (qr1d(k) + DT*qrten(k))*rho(k))
          qv(k) = MAX(1.E-10, qv1d(k) + DT*qvten(k))
          nr(k) = MAX(R2, (nr1d(k) + DT*nrten(k))*rho(k))
          temp(k) = t1d(k) + DT*tten(k)
          rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
         endif
      enddo
      do k = kts, kte
         evapprod(k) = prv_rev(k) - evap_ice(k)
      enddo
      end subroutine evaporate_rain
      subroutine fall_speeds
! Reference module_mp_thompson.F:3220-3401
      nstep = 0
      onstep(:) = 1.0
      ksed1(:) = 1
      do k = kte+1, kts, -1
         vtrk(k) = 0.
         vtnrk(k) = 0.
         vtik(k) = 0.
         vtnik(k) = 0.
         vtsk(k) = 0.
         vtgk(k) = 0.
         vtck(k) = 0.
         vtnck(k) = 0.
      enddo
      if (ANY(L_qr .eqv. .true.)) then
      do k = kte, kts, -1
         vtr = 0.
         rhof(k) = SQRT(RHO_NOT/rho(k))
         if (rr(k).gt. R1) then
          lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
          vtr = rhof(k)*av_r*crg(6)*org3 * lamr**cre(3)                 &
                      *((lamr+fv_r)**(-cre(6)))
          vtrk(k) = vtr
          vtr = rhof(k)*av_r*crg(7)/crg(12) * lamr**cre(12)             &
                      *((lamr+fv_r)**(-cre(7)))
          vtnrk(k) = vtr
      else
          vtrk(k) = vtrk(k+1)
          vtnrk(k) = vtnrk(k+1)
         endif
         if (MAX(vtrk(k),vtnrk(k)) .gt. 1.E-3) then
            ksed1(1) = MAX(ksed1(1), k)
            delta_tp = dzq(k)/(MAX(vtrk(k),vtnrk(k)))
            if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
         endif
      enddo
      if (ksed1(1) .eq. kte) ksed1(1) = kte-1
      if (nstep .gt. 0) onstep(1) = 1./REAL(nstep)
      endif
      if (ANY(L_qc .eqv. .true.)) then
      hgt_agl = 0.
      do k = kts, kte-1
         if (rc(k) .gt. R2) ksed1(5) = k
         hgt_agl = hgt_agl + dzq(k)
         if (hgt_agl .gt. 500.0) goto 151
      enddo
 151  continue
      do k = ksed1(5), kts, -1
         vtc = 0.
         if (rc(k) .gt. R1 .and. w1d(k) .lt. 1.E-1) then
          nu_c = MIN(15, NINT(1000.E6/nc(k)) + 2)
          lamc = (nc(k)*am_r*ccg(2,nu_c)*ocg1(nu_c)/rc(k))**obmr
          ilamc = 1./lamc
          vtc = rhof(k)*av_c*ccg(5,nu_c)*ocg2(nu_c) * ilamc**bv_c
          vtck(k) = vtc
          vtc = rhof(k)*av_c*ccg(4,nu_c)*ocg1(nu_c) * ilamc**bv_c
          vtnck(k) = vtc
         endif
      enddo
      endif
       if (ANY(L_qi .eqv. .true.)) then
       nstep = 0
       do k = kte, kts, -1
          vti = 0.
          if (ri(k).gt. R1) then
           lami = (am_i*cig(2)*oig1*ni(k)/ri(k))**obmi
           ilami = 1./lami
           vti = rhof(k)*av_i*cig(3)*oig2 * ilami**bv_i
           vtik(k) = vti
           vti = rhof(k)*av_i*cig(6)/cig(7) * ilami**bv_i
           vtnik(k) = vti
      else
           vtik(k) = vtik(k+1)
           vtnik(k) = vtnik(k+1)
          endif
          if (vtik(k) .gt. 1.E-3) then
             ksed1(2) = MAX(ksed1(2), k)
             delta_tp = dzq(k)/vtik(k)
             if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
          endif
       enddo
       if (ksed1(2) .eq. kte) ksed1(2) = kte-1
       if (nstep .gt. 0) onstep(2) = 1./REAL(nstep)
       endif
       if (ANY(L_qs .eqv. .true.)) then
       nstep = 0
       do k = kte, kts, -1
          vts = 0.
          if (rs(k).gt. R1) then
           xDs = smoc(k) / smob(k)
           Mrat = 1./xDs
           ils1 = 1./(Mrat*Lam0 + fv_s)
           ils2 = 1./(Mrat*Lam1 + fv_s)
           t1_vts = Kap0*csg(4)*ils1**cse(4)
           t2_vts = Kap1*Mrat**mu_s*csg(10)*ils2**cse(10)
           ils1 = 1./(Mrat*Lam0)
           ils2 = 1./(Mrat*Lam1)
           t3_vts = Kap0*csg(1)*ils1**cse(1)
           t4_vts = Kap1*Mrat**mu_s*csg(7)*ils2**cse(7)
           vts = rhof(k)*av_s * (t1_vts+t2_vts)/(t3_vts+t4_vts)
           if (temp(k).gt. (T_0+0.1)) then
            SR = rs(k)/(rs(k)+rr(k))
            vtsk(k) = vts*SR + (1.-SR)*vtrk(k)
      else
            vtsk(k) = vts*vts_boost(k)
           endif
      else
            vtsk(k) = vtsk(k+1)
          endif
          if (vtsk(k) .gt. 1.E-3) then
             ksed1(3) = MAX(ksed1(3), k)
             delta_tp = dzq(k)/vtsk(k)
             if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
          endif
       enddo
       if (ksed1(3) .eq. kte) ksed1(3) = kte-1
       if (nstep .gt. 0) onstep(3) = 1./REAL(nstep)
       endif
       if (ANY(L_qg .eqv. .true.)) then
       nstep = 0
       do k = kte, kts, -1
          vtg = 0.
          if (rg(k).gt. R1) then
           vtg = rhof(k)*av_g*cgg(6)*ogg3 * ilamg(k)**bv_g
           if (temp(k).gt. T_0) then
            vtgk(k) = MAX(vtg, vtrk(k))
      else
            vtgk(k) = vtg
           endif
      else
            vtgk(k) = vtgk(k+1)
          endif
          if (vtgk(k) .gt. 1.E-3) then
             ksed1(4) = MAX(ksed1(4), k)
             delta_tp = dzq(k)/vtgk(k)
             if (DT/delta_tp + 1. .lt. 2147483648.) nstep = MAX(nstep, INT(DT/delta_tp + 1.))
          endif
       enddo
       if (ksed1(4) .eq. kte) ksed1(4) = kte-1
       if (nstep .gt. 0) onstep(4) = 1./REAL(nstep)
       endif
      end subroutine fall_speeds
      subroutine sediment_rain_cloud_ice
! Reference module_mp_thompson.F:3402-3490
      if (ANY(L_qr .eqv. .true.)) then
      nstep = NINT(1./onstep(1))
      do n = 1, nstep
         do k = kte, kts, -1
            sed_r(k) = vtrk(k)*rr(k)
            sed_n(k) = vtnrk(k)*nr(k)
         enddo
         k = kte
         odzq = 1./dzq(k)
         orho = 1./rho(k)
         qrten(k) = qrten(k) - sed_r(k)*odzq*onstep(1)*orho
         nrten(k) = nrten(k) - sed_n(k)*odzq*onstep(1)*orho
         rr(k) = MAX(R1, rr(k) - sed_r(k)*odzq*DT*onstep(1))
         nr(k) = MAX(R2, nr(k) - sed_n(k)*odzq*DT*onstep(1))
         do k = ksed1(1), kts, -1
            odzq = 1./dzq(k)
            orho = 1./rho(k)
            qrten(k) = qrten(k) + (sed_r(k+1)-sed_r(k))                 &
                                               *odzq*onstep(1)*orho
            nrten(k) = nrten(k) + (sed_n(k+1)-sed_n(k))                 &
                                               *odzq*onstep(1)*orho
            rr(k) = MAX(R1, rr(k) + (sed_r(k+1)-sed_r(k)) &
                                           *odzq*DT*onstep(1))
            nr(k) = MAX(R2, nr(k) + (sed_n(k+1)-sed_n(k)) &
                                           *odzq*DT*onstep(1))
         enddo
         if (rr(kts).gt.R1*1000.) &
         pptrain = pptrain + sed_r(kts)*DT*onstep(1)
      enddo
      endif
      if (ANY(L_qc .eqv. .true.)) then
      do k = kte, kts, -1
         sed_c(k) = vtck(k)*rc(k)
         sed_n(k) = vtnck(k)*nc(k)
      enddo
      do k = ksed1(5), kts, -1
         odzq = 1./dzq(k)
         orho = 1./rho(k)
         qcten(k) = qcten(k) + (sed_c(k+1)-sed_c(k)) *odzq*orho
         ncten(k) = ncten(k) + (sed_n(k+1)-sed_n(k)) *odzq*orho
         rc(k) = MAX(R1, rc(k) + (sed_c(k+1)-sed_c(k)) *odzq*DT)
         nc(k) = MAX(10., nc(k) + (sed_n(k+1)-sed_n(k)) *odzq*DT)
      enddo
      endif
      if (ANY(L_qi .eqv. .true.)) then
      nstep = NINT(1./onstep(2))
      do n = 1, nstep
         do k = kte, kts, -1
            sed_i(k) = vtik(k)*ri(k)
            sed_n(k) = vtnik(k)*ni(k)
         enddo
         k = kte
         odzq = 1./dzq(k)
         orho = 1./rho(k)
         qiten(k) = qiten(k) - sed_i(k)*odzq*onstep(2)*orho
         niten(k) = niten(k) - sed_n(k)*odzq*onstep(2)*orho
         ri(k) = MAX(R1, ri(k) - sed_i(k)*odzq*DT*onstep(2))
         ni(k) = MAX(R2, ni(k) - sed_n(k)*odzq*DT*onstep(2))
         do k = ksed1(2), kts, -1
            odzq = 1./dzq(k)
            orho = 1./rho(k)
            qiten(k) = qiten(k) + (sed_i(k+1)-sed_i(k))                 &
                                               *odzq*onstep(2)*orho
            niten(k) = niten(k) + (sed_n(k+1)-sed_n(k))                 &
                                               *odzq*onstep(2)*orho
            ri(k) = MAX(R1, ri(k) + (sed_i(k+1)-sed_i(k)) &
                                           *odzq*DT*onstep(2))
            ni(k) = MAX(R2, ni(k) + (sed_n(k+1)-sed_n(k)) &
                                           *odzq*DT*onstep(2))
         enddo
         if (ri(kts).gt.R1*1000.) &
         pptice = pptice + sed_i(kts)*DT*onstep(2)
      enddo
      endif
      end subroutine sediment_rain_cloud_ice
      subroutine sediment_snow_graupel_and_freeze
! Reference module_mp_thompson.F:3491-3572
      if (ANY(L_qs .eqv. .true.)) then
      nstep = NINT(1./onstep(3))
      do n = 1, nstep
         do k = kte, kts, -1
            sed_s(k) = vtsk(k)*rs(k)
         enddo
         k = kte
         odzq = 1./dzq(k)
         orho = 1./rho(k)
         qsten(k) = qsten(k) - sed_s(k)*odzq*onstep(3)*orho
         rs(k) = MAX(R1, rs(k) - sed_s(k)*odzq*DT*onstep(3))
         do k = ksed1(3), kts, -1
            odzq = 1./dzq(k)
            orho = 1./rho(k)
            qsten(k) = qsten(k) + (sed_s(k+1)-sed_s(k))                 &
                                               *odzq*onstep(3)*orho
            rs(k) = MAX(R1, rs(k) + (sed_s(k+1)-sed_s(k)) &
                                           *odzq*DT*onstep(3))
         enddo
         if (rs(kts).gt.R1*1000.) &
         pptsnow = pptsnow + sed_s(kts)*DT*onstep(3)
      enddo
      endif
      if (ANY(L_qg .eqv. .true.)) then
      nstep = NINT(1./onstep(4))
      do n = 1, nstep
         do k = kte, kts, -1
            sed_g(k) = vtgk(k)*rg(k)
         enddo
         k = kte
         odzq = 1./dzq(k)
         orho = 1./rho(k)
         qgten(k) = qgten(k) - sed_g(k)*odzq*onstep(4)*orho
         rg(k) = MAX(R1, rg(k) - sed_g(k)*odzq*DT*onstep(4))
         do k = ksed1(4), kts, -1
            odzq = 1./dzq(k)
            orho = 1./rho(k)
            qgten(k) = qgten(k) + (sed_g(k+1)-sed_g(k))                 &
                                               *odzq*onstep(4)*orho
            rg(k) = MAX(R1, rg(k) + (sed_g(k+1)-sed_g(k)) &
                                           *odzq*DT*onstep(4))
         enddo
         if (rg(kts).gt.R1*1000.) &
         pptgraul = pptgraul + sed_g(kts)*DT*onstep(4)
      enddo
      endif
      do k = kts, kte
         xri = MAX(0.0, qi1d(k) + qiten(k)*DT)
         if ( (temp(k).gt. T_0) .and. (xri.gt. 0.0) ) then
          qcten(k) = qcten(k) + xri*odt
          ncten(k) = ncten(k) + ni1d(k)*odt
          qiten(k) = qiten(k) - xri*odt
          niten(k) = -ni1d(k)*odt
          tten(k) = tten(k) - lfus*ocp(k)*xri*odt*(1-IFDRY)
         endif
         xrc = MAX(0.0, qc1d(k) + qcten(k)*DT)
         if ( (temp(k).lt. HGFR) .and. (xrc.gt. 0.0) ) then
          lfus2 = lsub - lvap(k)
          xnc = nc1d(k) + ncten(k)*DT
          qiten(k) = qiten(k) + xrc*odt
          niten(k) = niten(k) + xnc*odt
          qcten(k) = qcten(k) - xrc*odt
          ncten(k) = ncten(k) - xnc*odt
          tten(k) = tten(k) + lfus2*ocp(k)*xrc*odt*(1-IFDRY)
         endif
      enddo
      end subroutine sediment_snow_graupel_and_freeze
      subroutine apply_tendencies
! Reference module_mp_thompson.F:3573-3641
      do k = kts, kte
         t1d(k)  = t1d(k) + tten(k)*DT
         qv1d(k) = MAX(1.E-10, qv1d(k) + qvten(k)*DT)
         qc1d(k) = qc1d(k) + qcten(k)*DT
         nc1d(k) = MAX(2./rho(k), MIN(nc1d(k) + ncten(k)*DT, Nt_c_max))
         nwfa1d(k) = MAX(11.1E6, MIN(9999.E6,                           &
                       (nwfa1d(k)+nwfaten(k)*DT)))
         nifa1d(k) = MAX(naIN1*0.01, MIN(9999.E6,                       &
                       (nifa1d(k)+nifaten(k)*DT)))
         if (qc1d(k) .le. R1) then
           qc1d(k) = 0.0
           nc1d(k) = 0.0
      else
           nu_c = MIN(15, NINT(1000.E6/(nc1d(k)*rho(k))) + 2)
           lamc = (am_r*ccg(2,nu_c)*ocg1(nu_c)*nc1d(k)/qc1d(k))**obmr
           xDc = (bm_r + nu_c + 1.) / lamc
           if (xDc.lt. D0c) then
            lamc = cce(2,nu_c)/D0c
           elseif (xDc.gt. D0r*2.) then
            lamc = cce(2,nu_c)/(D0r*2.)
           endif
           nc1d(k) = MIN(ccg(1,nu_c)*ocg2(nu_c)*qc1d(k)/am_r*lamc**bm_r,&
                         DBLE(Nt_c_max)/rho(k))
         endif
         qi1d(k) = qi1d(k) + qiten(k)*DT
         ni1d(k) = MAX(R2/rho(k), ni1d(k) + niten(k)*DT)
         if (qi1d(k) .le. R1) then
           qi1d(k) = 0.0
           ni1d(k) = 0.0
      else
           lami = (am_i*cig(2)*oig1*ni1d(k)/qi1d(k))**obmi
           ilami = 1./lami
           xDi = (bm_i + mu_i + 1.) * ilami
           if (xDi.lt. 5.E-6) then
            lami = cie(2)/5.E-6
           elseif (xDi.gt. 300.E-6) then
            lami = cie(2)/300.E-6
           endif
           ni1d(k) = MIN(cig(1)*oig2*qi1d(k)/am_i*lami**bm_i,           &
                         9999.D3/rho(k))
         endif
         qr1d(k) = qr1d(k) + qrten(k)*DT
         nr1d(k) = MAX(R2/rho(k), nr1d(k) + nrten(k)*DT)
         if (qr1d(k) .le. R1) then
           qr1d(k) = 0.0
           nr1d(k) = 0.0
      else
           lamr = (am_r*crg(3)*org2*nr1d(k)/qr1d(k))**obmr
           mvd_r(k) = (3.0 + mu_r + 0.672) / lamr
           if (mvd_r(k) .gt. 2.5E-3) then
              mvd_r(k) = 2.5E-3
           elseif (mvd_r(k) .lt. D0r*0.75) then
              mvd_r(k) = D0r*0.75
           endif
           lamr = (3.0 + mu_r + 0.672) / mvd_r(k)
           nr1d(k) = crg(2)*org3*qr1d(k)*lamr**bm_r / am_r
         endif
         qs1d(k) = qs1d(k) + qsten(k)*DT
         if (qs1d(k) .le. R1) qs1d(k) = 0.0
         qg1d(k) = qg1d(k) + qgten(k)*DT
         if (qg1d(k) .le. R1) qg1d(k) = 0.0
      enddo
      end subroutine apply_tendencies
      subroutine frozen_lookup_indices
! Reference module_mp_thompson.F:2027-2157
         xDs = 0.0
         if (L_qs(k)) xDs = smoc(k) / smob(k)
         tempc = temp(k) - 273.15
         idx_tc = MAX(1, MIN(NINT(-tempc), 45) )
         idx_t = INT( (tempc-2.5)/5. ) - 1
         idx_t = MAX(1, -idx_t)
         idx_t = MIN(idx_t, ntb_t)
         IT = MAX(1, MIN(NINT(-tempc), 31) )
         if (rc(k).gt. r_c(1)) then
          nic = NINT(ALOG10(rc(k)))
          do nn = nic-1, nic+1
             n = nn
             if ( (rc(k)/10.**nn).ge.1.0 .and. &
                  (rc(k)/10.**nn).lt.10.0) goto 141
          enddo
 141      continue
          idx_c = INT(rc(k)/10.**n) + 10*(n-nic2) - (n-nic2)
          idx_c = MAX(1, MIN(idx_c, ntb_c))
      else
          idx_c = 1
         endif
         idx_n = NINT(1.0 + FLOAT(nbc) * DLOG(nc(k)/t_Nc(1)) / nic1)
         idx_n = MAX(1, MIN(idx_n, nbc))
         if (ri(k).gt. r_i(1)) then
          nii = NINT(ALOG10(ri(k)))
          do nn = nii-1, nii+1
             n = nn
             if ( (ri(k)/10.**nn).ge.1.0 .and. &
                  (ri(k)/10.**nn).lt.10.0) goto 142
          enddo
 142      continue
          idx_i = INT(ri(k)/10.**n) + 10*(n-nii2) - (n-nii2)
          idx_i = MAX(1, MIN(idx_i, ntb_i))
      else
          idx_i = 1
         endif
         if (ni(k).gt. Nt_i(1)) then
          nii = NINT(ALOG10(ni(k)))
          do nn = nii-1, nii+1
             n = nn
             if ( (ni(k)/10.**nn).ge.1.0 .and. &
                  (ni(k)/10.**nn).lt.10.0) goto 143
          enddo
 143      continue
          idx_i1 = INT(ni(k)/10.**n) + 10*(n-nii3) - (n-nii3)
          idx_i1 = MAX(1, MIN(idx_i1, ntb_i1))
      else
          idx_i1 = 1
         endif
         if (rr(k).gt. r_r(1)) then
          nir = NINT(ALOG10(rr(k)))
          do nn = nir-1, nir+1
             n = nn
             if ( (rr(k)/10.**nn).ge.1.0 .and. &
                  (rr(k)/10.**nn).lt.10.0) goto 144
          enddo
 144      continue
          idx_r = INT(rr(k)/10.**n) + 10*(n-nir2) - (n-nir2)
          idx_r = MAX(1, MIN(idx_r, ntb_r))
          lamr = 1./ilamr(k)
          lam_exp = lamr * (crg(3)*org2*org1)**bm_r
          N0_exp = org1*rr(k)/am_r * lam_exp**cre(1)
          nir = NINT(DLOG10(N0_exp))
          do nn = nir-1, nir+1
             n = nn
             if ( (N0_exp/10.**nn).ge.1.0 .and. &
                  (N0_exp/10.**nn).lt.10.0) goto 145
          enddo
 145      continue
          idx_r1 = INT(N0_exp/10.**n) + 10*(n-nir3) - (n-nir3)
          idx_r1 = MAX(1, MIN(idx_r1, ntb_r1))
      else
          idx_r = 1
          idx_r1 = ntb_r1
         endif
         if (rs(k).gt. r_s(1)) then
          nis = NINT(ALOG10(rs(k)))
          do nn = nis-1, nis+1
             n = nn
             if ( (rs(k)/10.**nn).ge.1.0 .and. &
                  (rs(k)/10.**nn).lt.10.0) goto 146
          enddo
 146      continue
          idx_s = INT(rs(k)/10.**n) + 10*(n-nis2) - (n-nis2)
          idx_s = MAX(1, MIN(idx_s, ntb_s))
      else
          idx_s = 1
         endif
         if (rg(k).gt. r_g(1)) then
          nig = NINT(ALOG10(rg(k)))
          do nn = nig-1, nig+1
             n = nn
             if ( (rg(k)/10.**nn).ge.1.0 .and. &
                  (rg(k)/10.**nn).lt.10.0) goto 147
          enddo
 147      continue
          idx_g = INT(rg(k)/10.**n) + 10*(n-nig2) - (n-nig2)
          idx_g = MAX(1, MIN(idx_g, ntb_g))
          lamg = 1./ilamg(k)
          lam_exp = lamg * (cgg(3)*ogg2*ogg1)**bm_g
          N0_exp = ogg1*rg(k)/am_g * lam_exp**cge(1)
          nig = NINT(DLOG10(N0_exp))
          do nn = nig-1, nig+1
             n = nn
             if ( (N0_exp/10.**nn).ge.1.0 .and. &
                  (N0_exp/10.**nn).lt.10.0) goto 148
          enddo
 148      continue
          idx_g1 = INT(N0_exp/10.**n) + 10*(n-nig3) - (n-nig3)
          idx_g1 = MAX(1, MIN(idx_g1, ntb_g1))
      else
          idx_g = 1
          idx_g1 = ntb_g1
         endif
      end subroutine frozen_lookup_indices
      subroutine frozen_collection
! Reference module_mp_thompson.F:2158-2299
         otemp = 1./temp(k)
         rvs = rho(k)*qvsi(k)
         rvs_p = rvs*otemp*(lsub*otemp*oRv - 1.)
         rvs_pp = rvs * ( otemp*(lsub*otemp*oRv - 1.) &
                         *otemp*(lsub*otemp*oRv - 1.) &
                         + (-2.*lsub*otemp*otemp*otemp*oRv) &
                         + otemp*otemp)
         gamsc = lsub*diffu(k)/tcond(k) * rvs_p
         alphsc = 0.5*(gamsc/(1.+gamsc))*(gamsc/(1.+gamsc)) &
                    * rvs_pp/rvs_p * rvs/rvs_p
         alphsc = MAX(1.E-9, alphsc)
         xsat = ssati(k)
         if (abs(xsat).lt. 1.E-9) xsat=0.
         t1_subl = 4.*PI*( 1.0 - alphsc*xsat &
                + 2.*alphsc*alphsc*xsat*xsat &
                - 5.*alphsc*alphsc*alphsc*xsat*xsat*xsat ) &
                / (1.+gamsc)
         if (L_qc(k) .and. mvd_c(k).gt. D0c) then
          if (xDs .gt. D0s) then
           idx = 1 + INT(nbs*DLOG(xDs/Ds(1))/DLOG(Ds(nbs)/Ds(1)))
           idx = MIN(idx, nbs)
           Ef_sw = t_Efsw(idx, INT(mvd_c(k)*1.E6))
           prs_scw = rhof(k)*t1_qs_qc*Ef_sw*rc(k)*smoe(k)
           prs_scw = MIN(DBLE(rc(k)*odts), prs_scw)
           pnc_scw = rhof(k)*t1_qs_qc*Ef_sw*nc(k)*smoe(k)                ! Qc2M
           pnc_scw = MIN(DBLE(nc(k)*odts), pnc_scw)
          endif
          if (rg(k).ge. r_g(1) .and. mvd_c(k).gt. D0c) then
           xDg = (bm_g + mu_g + 1.) * ilamg(k)
           vtg = rhof(k)*av_g*cgg(6)*ogg3 * ilamg(k)**bv_g
           stoke_g = mvd_c(k)*mvd_c(k)*vtg*rho_w/(9.*visco(k)*xDg)
           if (xDg.gt. D0g) then
            if (stoke_g.ge.0.4 .and. stoke_g.le.10.) then
             Ef_gw = 0.55*ALOG10(2.51*stoke_g)
            elseif (stoke_g.lt.0.4) then
             Ef_gw = 0.0
            elseif (stoke_g.gt.10) then
             Ef_gw = 0.77
            endif
            prg_gcw = rhof(k)*t1_qg_qc*Ef_gw*rc(k)*N0_g(k) &
                          *ilamg(k)**cge(9)
            pnc_gcw = rhof(k)*t1_qg_qc*Ef_gw*nc(k)*N0_g(k)           &
                          *ilamg(k)**cge(9)                                 ! Qc2M
            pnc_gcw = MIN(DBLE(nc(k)*odts), pnc_gcw)
           endif
          endif
         endif
         if (rs(k) .gt. r_s(1)) then
          Ef_sa = Eff_aero(xDs,0.04E-6,visco(k),rho(k),temp(k),'s')
          pna_sca = rhof(k)*t1_qs_qc*Ef_sa*nwfa(k)*smoe(k)
          pna_sca = MIN(DBLE(nwfa(k)*odts), pna_sca)
          Ef_sa = Eff_aero(xDs,0.8E-6,visco(k),rho(k),temp(k),'s')
          pnd_scd = rhof(k)*t1_qs_qc*Ef_sa*nifa(k)*smoe(k)
          pnd_scd = MIN(DBLE(nifa(k)*odts), pnd_scd)
         endif
         if (rg(k) .gt. r_g(1)) then
          xDg = (bm_g + mu_g + 1.) * ilamg(k)
          Ef_ga = Eff_aero(xDg,0.04E-6,visco(k),rho(k),temp(k),'g')
          pna_gca = rhof(k)*t1_qg_qc*Ef_ga*nwfa(k)*N0_g(k)           &
                        *ilamg(k)**cge(9)
          pna_gca = MIN(DBLE(nwfa(k)*odts), pna_gca)
          Ef_ga = Eff_aero(xDg,0.8E-6,visco(k),rho(k),temp(k),'g')
          pnd_gcd = rhof(k)*t1_qg_qc*Ef_ga*nifa(k)*N0_g(k)           &
                        *ilamg(k)**cge(9)
          pnd_gcd = MIN(DBLE(nifa(k)*odts), pnd_gcd)
         endif
         if (rr(k).ge. r_r(1)) then
          if (rs(k).ge. r_s(1)) then
           if (temp(k).lt.T_0) then
            prr_rcs = -(tmr_racs2(idx_s,idx_t,idx_r1,idx_r) &
                           + tcr_sacr2(idx_s,idx_t,idx_r1,idx_r) &
                           + tmr_racs1(idx_s,idx_t,idx_r1,idx_r) &
                           + tcr_sacr1(idx_s,idx_t,idx_r1,idx_r))
            prs_rcs = tmr_racs2(idx_s,idx_t,idx_r1,idx_r) &
                         + tcr_sacr2(idx_s,idx_t,idx_r1,idx_r) &
                         - tcs_racs1(idx_s,idx_t,idx_r1,idx_r) &
                         - tms_sacr1(idx_s,idx_t,idx_r1,idx_r)
            prg_rcs = tmr_racs1(idx_s,idx_t,idx_r1,idx_r) &
                         + tcr_sacr1(idx_s,idx_t,idx_r1,idx_r) &
                         + tcs_racs1(idx_s,idx_t,idx_r1,idx_r) &
                         + tms_sacr1(idx_s,idx_t,idx_r1,idx_r)
            prr_rcs = MAX(DBLE(-rr(k)*odts), prr_rcs)
            prs_rcs = MAX(DBLE(-rs(k)*odts), prs_rcs)
            prg_rcs = MIN(DBLE((rr(k)+rs(k))*odts), prg_rcs)
            pnr_rcs = tnr_racs1(idx_s,idx_t,idx_r1,idx_r)            &   ! RAIN2M
                         + tnr_racs2(idx_s,idx_t,idx_r1,idx_r)          &
                         + tnr_sacr1(idx_s,idx_t,idx_r1,idx_r)          &
                         + tnr_sacr2(idx_s,idx_t,idx_r1,idx_r)
            pnr_rcs = MIN(DBLE(nr(k)*odts), pnr_rcs)
      else
            prs_rcs = -tcs_racs1(idx_s,idx_t,idx_r1,idx_r)           &
                         - tms_sacr1(idx_s,idx_t,idx_r1,idx_r)          &
                         + tmr_racs2(idx_s,idx_t,idx_r1,idx_r)          &
                         + tcr_sacr2(idx_s,idx_t,idx_r1,idx_r)
            prs_rcs = MAX(DBLE(-rs(k)*odts), prs_rcs)
            prr_rcs = -prs_rcs
           endif
          endif
          if (rg(k).ge. r_g(1)) then
           if (temp(k).lt.T_0) then
            prg_rcg = tmr_racg(idx_g1,idx_g,idx_r1,idx_r) &
                         + tcr_gacr(idx_g1,idx_g,idx_r1,idx_r)
            prg_rcg = MIN(DBLE(rr(k)*odts), prg_rcg)
            prr_rcg = -prg_rcg
            pnr_rcg = tnr_racg(idx_g1,idx_g,idx_r1,idx_r)            &   ! RAIN2M
                         + tnr_gacr(idx_g1,idx_g,idx_r1,idx_r)
            pnr_rcg = MIN(DBLE(nr(k)*odts), pnr_rcg)
      else
            prr_rcg = tcg_racg(idx_g1,idx_g,idx_r1,idx_r)
            prr_rcg = MIN(DBLE(rg(k)*odts), prr_rcg)
            prg_rcg = -prr_rcg
              pnr_rcg = -5.0*tnr_gacr(idx_g1,idx_g,idx_r1,idx_r)       ! RAIN2M
           endif
          endif
         endif
      end subroutine frozen_collection
      subroutine ice_nucleation
! Reference module_mp_thompson.F:2307-2394
          vts_boost(k) = 1.0
          rate_max = (qv(k)-qvsi(k))*rho(k)*odts*0.999
! xni = 1 per litre (the default): its bin idx_IN is fixed, the *_in tables hold that slice (freezing_tables_at_default_in)
          if (rr(k).gt. r_r(1)) then
           prg_rfz = tpg_qrfz_in(idx_r,idx_r1,idx_tc)*odts
           pri_rfz = tpi_qrfz_in(idx_r,idx_r1,idx_tc)*odts
           pni_rfz = tni_qrfz_in(idx_r,idx_r1,idx_tc)*odts
           pnr_rfz = tnr_qrfz_in(idx_r,idx_r1,idx_tc)*odts          ! RAIN2M
           pnr_rfz = MIN(DBLE(nr(k)*odts), pnr_rfz)
          elseif (rr(k).gt. R1 .and. temp(k).lt.HGFR) then
           pri_rfz = rr(k)*odts
           pni_rfz = nr(k)*odts                                         ! RAIN2M
          endif
          if (rc(k).gt. r_c(1)) then
           pri_wfz = tpi_qcfz_in(idx_c,idx_n,idx_tc)*odts
           pri_wfz = MIN(DBLE(rc(k)*odts), pri_wfz)
           pni_wfz = tni_qcfz_in(idx_c,idx_n,idx_tc)*odts
           pni_wfz = MIN(DBLE(nc(k)*odts), pri_wfz/(2.*xm0i),     &
                                pni_wfz)
          elseif (rc(k).gt. R1 .and. temp(k).lt.HGFR) then
           pri_wfz = rc(k)*odts
           pni_wfz = nc(k)*odts
          endif
          if ( (ssati(k).ge. 0.25) .or. (ssatw(k).gt. eps &
                                .and. temp(k).lt.253.15) ) then
            xnc = MIN(250.E3, TNO*EXP(ATO*(T_0-temp(k))))
           xni = ni(k) + (pni_rfz+pni_wfz)*dtsave
           pni_inu = 0.5*(xnc-xni + abs(xnc-xni))*odts
           pri_inu = MIN(DBLE(rate_max), xm0i*pni_inu)
           pni_inu = pri_inu/xm0i
          endif
          xni = smo0(k)+ni(k) + (pni_rfz+pni_wfz+pni_inu)*dtsave
      end subroutine ice_nucleation
      subroutine ice_growth_collection
! Reference module_mp_thompson.F:2395-2483
          if (L_qi(k)) then
           lami = (am_i*cig(2)*oig1*ni(k)/ri(k))**obmi
           ilami = 1./lami
           xDi = MAX(DBLE(D0i), (bm_i + mu_i + 1.) * ilami)
           xmi = am_i*xDi**bm_i
           oxmi = 1./xmi
           pri_ide = C_cube*t1_subl*diffu(k)*ssati(k)*rvs &
                  *oig1*cig(5)*ni(k)*ilami
           if (pri_ide .lt. 0.0) then
            pri_ide = MAX(DBLE(-ri(k)*odts), pri_ide, DBLE(rate_max))
            pni_ide = pri_ide*oxmi
            pni_ide = MAX(DBLE(-ni(k)*odts), pni_ide)
      else
            pri_ide = MIN(pri_ide, DBLE(rate_max))
            prs_ide = (1.0D0-tpi_ide(idx_i,idx_i1))*pri_ide
            pri_ide = tpi_ide(idx_i,idx_i1)*pri_ide
           endif
           if ( (idx_i.eq. ntb_i) .or. (xDi.gt. 5.0*D0s) ) then
            prs_iau = ri(k)*.99*odts
            pni_iau = ni(k)*.95*odts
           elseif (xDi.lt. 0.1*D0s) then
            prs_iau = 0.
            pni_iau = 0.
      else
            prs_iau = tps_iaus(idx_i,idx_i1)*odts
            prs_iau = MIN(DBLE(ri(k)*.99*odts), prs_iau)
            pni_iau = tni_iaus(idx_i,idx_i1)*odts
            pni_iau = MIN(DBLE(ni(k)*.95*odts), pni_iau)
           endif
          endif
          if (L_qs(k)) then
           C_snow = C_sqrd + (tempc+1.5)*(C_cube-C_sqrd)/(-30.+1.5)
           C_snow = MAX(C_sqrd, MIN(C_snow, C_cube))
           prs_sde = C_snow*t1_subl*diffu(k)*ssati(k)*rvs &
                        * (t1_qs_sd*smo1(k) &
                         + t2_qs_sd*rhof2(k)*vsc2(k)*smof(k))
           if (prs_sde.lt. 0.) then
            prs_sde = MAX(DBLE(-rs(k)*odts), prs_sde, DBLE(rate_max))
      else
            prs_sde = MIN(prs_sde, DBLE(rate_max))
           endif
          endif
          if (L_qg(k) .and. ssati(k).lt. -eps) then
           prg_gde = C_cube*t1_subl*diffu(k)*ssati(k)*rvs &
               * N0_g(k) * (t1_qg_sd*ilamg(k)**cge(10) &
               + t2_qg_sd*vsc2(k)*rhof2(k)*ilamg(k)**cge(11))
           if (prg_gde.lt. 0.) then
            prg_gde = MAX(DBLE(-rg(k)*odts), prg_gde, DBLE(rate_max))
      else
            prg_gde = MIN(prg_gde, DBLE(rate_max))
           endif
          endif
          if (L_qi(k)) then
           lami = (am_i*cig(2)*oig1*ni(k)/ri(k))**obmi
           ilami = 1./lami
           xDi = MAX(DBLE(D0i), (bm_i + mu_i + 1.) * ilami)
           xmi = am_i*xDi**bm_i
           oxmi = 1./xmi
           if (rs(k).ge. r_s(1)) then
            prs_sci = t1_qs_qi*rhof(k)*Ef_si*ri(k)*smoe(k)
            pni_sci = prs_sci * oxmi
           endif
           if (rr(k).ge. r_r(1) .and. mvd_r(k).gt. 4.*xDi) then
            lamr = 1./ilamr(k)
            pri_rci = rhof(k)*t1_qr_qi*Ef_ri*ri(k)*N0_r(k) &
                           *((lamr+fv_r)**(-cre(9)))
            pnr_rci = rhof(k)*t1_qr_qi*Ef_ri*ni(k)*N0_r(k)           &   ! RAIN2M
                           *((lamr+fv_r)**(-cre(9)))
            pni_rci = pri_rci * oxmi
            prr_rci = rhof(k)*t2_qr_qi*Ef_ri*ni(k)*N0_r(k) &
                           *((lamr+fv_r)**(-cre(8)))
            prr_rci = MIN(DBLE(rr(k)*odts), prr_rci)
            prg_rci = pri_rci + prr_rci
           endif
          endif
      end subroutine ice_growth_collection
      subroutine riming_and_splintering
! Reference module_mp_thompson.F:2484-2512
          if (prg_gcw.gt. eps .and. tempc.gt.-8.0) then
           tf = 0.
           if (tempc.ge.-5.0 .and. tempc.lt.-3.0) then
            tf = 0.5*(-3.0 - tempc)
           elseif (tempc.gt.-8.0 .and. tempc.lt.-5.0) then
            tf = 0.33333333*(8.0 + tempc)
           endif
           pni_ihm = 3.5E8*tf*prg_gcw
           pri_ihm = xm0i*pni_ihm
           prs_ihm = prs_scw/(prs_scw+prg_gcw) &
                          * pri_ihm
           prg_ihm = prg_gcw/(prs_scw+prg_gcw) &
                          * pri_ihm
          endif
          if (prs_scw.gt.2.0*prs_sde .and. &
                         prs_sde.gt.eps) then
           r_frac = MIN(30.0D0, prs_scw/prs_sde)
           g_frac = MIN(0.95, 0.15 + (r_frac-2.)*.028)
           vts_boost(k) = MIN(1.5, 1.1 + (r_frac-2.)*.014)
           prg_scw = g_frac*prs_scw
           prs_scw = (1. - g_frac)*prs_scw
          endif
      end subroutine riming_and_splintering
      subroutine melt_snow_graupel
! Reference module_mp_thompson.F:2514-2563
          if (L_qs(k)) then
           prr_sml = (tempc*tcond(k)-lvap0*diffu(k)*delQvs(k))       &
                      * (t1_qs_me*smo1(k) + t2_qs_me*rhof2(k)*vsc2(k)*smof(k))
           if (prr_sml .gt. 0.) then
              prr_sml = prr_sml + 4218.*olfus*tempc               &
                                      * (prr_rcs+prs_scw)
           endif
           prr_sml = MIN(DBLE(rs(k)*odts), MAX(0.D0, prr_sml))
           pnr_sml = smo0(k)/rs(k)*prr_sml * 10.0**(-0.25*tempc)      ! RAIN2M
           pnr_sml = MIN(DBLE(smo0(k)*odts), pnr_sml)
           if (ssati(k).lt. 0.) then
            prs_sde = C_cube*t1_subl*diffu(k)*ssati(k)*rvs &
                         * (t1_qs_sd*smo1(k) &
                          + t2_qs_sd*rhof2(k)*vsc2(k)*smof(k))
            prs_sde = MAX(DBLE(-rs(k)*odts), prs_sde)
           endif
          endif
          if (L_qg(k)) then
           prr_gml = (tempc*tcond(k)-lvap0*diffu(k)*delQvs(k))       &
                      * N0_g(k)*(t1_qg_me*ilamg(k)**cge(10)             &
                      + t2_qg_me*rhof2(k)*vsc2(k)*ilamg(k)**cge(11))
           prr_gml = MIN(DBLE(rg(k)*odts), MAX(0.D0, prr_gml))
           pnr_gml = N0_g(k)*cgg(2)*ilamg(k)**cge(2) / rg(k)         &   ! RAIN2M
                      * prr_gml * 10.0**(-0.5*tempc)
           if (ssati(k).lt. 0.) then
            prg_gde = C_cube*t1_subl*diffu(k)*ssati(k)*rvs &
                * N0_g(k) * (t1_qg_sd*ilamg(k)**cge(10) &
                + t2_qg_sd*vsc2(k)*rhof2(k)*ilamg(k)**cge(11))
            prg_gde = MAX(DBLE(-rg(k)*odts), prg_gde)
           endif
          endif
          if (dt .gt. 120.) then
             prr_rcw=prr_rcw+prs_scw+prg_gcw
             prs_scw=0.
             prg_gcw=0.
          endif
      end subroutine melt_snow_graupel
      end subroutine mp_thompson
      subroutine table_Efrw
      implicit none
      DOUBLE PRECISION:: vtr, stokes, reynolds, Ef_rw
      DOUBLE PRECISION:: p, yc0, F, G, H, z, K0, X
      INTEGER:: i, j
      do j = 1, nbc
      do i = 1, nbr
         Ef_rw = 0.0
         p = Dc(j)/Dr(i)
         if (Dr(i).lt.50.E-6 .or. Dc(j).lt.3.E-6) then
          t_Efrw(i,j) = 0.0
         elseif (p.gt.0.25) then
          X = Dc(j)*1.D6
          if (Dr(i) .lt. 75.e-6) then
             Ef_rw = 0.026794*X - 0.20604
          elseif (Dr(i) .lt. 125.e-6) then
             Ef_rw = -0.00066842*X*X + 0.061542*X - 0.37089
          elseif (Dr(i) .lt. 175.e-6) then
             Ef_rw = 4.091e-06*X*X*X*X - 0.00030908*X*X*X               &
                   + 0.0066237*X*X - 0.0013687*X - 0.073022
          elseif (Dr(i) .lt. 250.e-6) then
             Ef_rw = 9.6719e-5*X*X*X - 0.0068901*X*X + 0.17305*X        &
                   - 0.65988
          elseif (Dr(i) .lt. 350.e-6) then
             Ef_rw = 9.0488e-5*X*X*X - 0.006585*X*X + 0.16606*X         &
                   - 0.56125
          else
             Ef_rw = 0.00010721*X*X*X - 0.0072962*X*X + 0.1704*X        &
                   - 0.46929
          endif
         else
          vtr = -0.1021 + 4.932E3*Dr(i) - 0.9551E6*Dr(i)*Dr(i) &
              + 0.07934E9*Dr(i)*Dr(i)*Dr(i) &
              - 0.002362E12*Dr(i)*Dr(i)*Dr(i)*Dr(i)
          stokes = Dc(j)*Dc(j)*vtr*rho_w/(9.*1.718E-5*Dr(i))
          reynolds = 9.*stokes/(p*p*rho_w)
          F = DLOG(reynolds)
          G = -0.1007D0 - 0.358D0*F + 0.0261D0*F*F
          K0 = DEXP(G)
          z = DLOG(stokes/(K0+1.D-15))
          H = 0.1465D0 + 1.302D0*z - 0.607D0*z*z + 0.293D0*z*z*z
          yc0 = 2.0D0/PI * ATAN(H)
          Ef_rw = (yc0+p)*(yc0+p) / ((1.+p)*(1.+p))
         endif
         t_Efrw(i,j) = MAX(0.0, MIN(SNGL(Ef_rw), 0.95))
      enddo
      enddo
      end subroutine table_Efrw
      subroutine table_Efsw
      implicit none
      DOUBLE PRECISION:: Ds_m, vts, vtc, stokes, reynolds, Ef_sw
      DOUBLE PRECISION:: p, yc0, F, G, H, z, K0
      INTEGER:: i, j
      do j = 1, nbc
      vtc = 1.19D4 * (1.0D4*Dc(j)*Dc(j)*0.25D0)
      do i = 1, nbs
         vts = av_s*Ds(i)**bv_s * DEXP(-fv_s*Ds(i)) - vtc
         Ds_m = (am_s*Ds(i)**bm_s / am_r)**obmr
         p = Dc(j)/Ds_m
         if (p.gt.0.25 .or. Ds(i).lt.D0s .or. Dc(j).lt.6.E-6 &
               .or. vts.lt.1.E-3) then
          t_Efsw(i,j) = 0.0
         else
          stokes = Dc(j)*Dc(j)*vts*rho_w/(9.*1.718E-5*Ds_m)
          reynolds = 9.*stokes/(p*p*rho_w)
          F = DLOG(reynolds)
          G = -0.1007D0 - 0.358D0*F + 0.0261D0*F*F
          K0 = DEXP(G)
          z = DLOG(stokes/(K0+1.D-15))
          H = 0.1465D0 + 1.302D0*z - 0.607D0*z*z + 0.293D0*z*z*z
          yc0 = 2.0D0/PI * ATAN(H)
          Ef_sw = (yc0+p)*(yc0+p) / ((1.+p)*(1.+p))
          t_Efsw(i,j) = MAX(0.0, MIN(SNGL(Ef_sw), 0.95))
         endif
      enddo
      enddo
      end subroutine table_Efsw
      real function Eff_aero(D, Da, visc,rhoa,Temp,species)
      implicit none
      real:: D, Da, visc, rhoa, Temp
      character(LEN=1):: species
      real:: aval, Cc, diff, Re, Sc, St, St2, vt, Eff
      real, parameter:: boltzman = 1.3806503E-23
      real, parameter:: meanPath = 0.0256E-6
      vt = 1.
      if (species .eq. 'r') then
         vt = -0.1021 + 4.932E3*D - 0.9551E6*D*D                        &
              + 0.07934E9*D*D*D - 0.002362E12*D*D*D*D
      elseif (species .eq. 's') then
         vt = av_s*D**bv_s
      elseif (species .eq. 'g') then
         vt = av_g*D**bv_g
      endif
      Cc    = 1. + 2.*meanPath/Da *(1.257+0.4*exp(-0.55*Da/meanPath))
      diff  = boltzman*Temp*Cc/(3.*PI*visc*Da)
      Re    = 0.5*rhoa*D*vt/visc
      Sc    = visc/(rhoa*diff)
      St    = Da*Da*vt*1000./(9.*visc*D)
      aval  = 1.+LOG(1.+Re)
      St2   = (1.2 + 1./12.*aval)/(1.+aval)
      Eff = 4./(Re*Sc) * (1. + 0.4*SQRT(Re)*Sc**0.3333                  &
                             + 0.16*SQRT(Re)*SQRT(Sc))                  &
               + 4.*Da/D * (0.02 + Da/D*(1.+2.*SQRT(Re)))
      if (St.gt.St2) Eff = Eff  + ( (St-St2)/(St-St2+0.666667))**1.5
      Eff_aero = MAX(1.E-5, MIN(Eff, 1.0))
      end function Eff_aero
      subroutine table_dropEvap
      implicit none
      INTEGER:: i, j, k, n
      DOUBLE PRECISION, DIMENSION(nbc):: N_c, massc
      DOUBLE PRECISION:: summ, summ2, lamc, N0_c
      INTEGER:: nu_c
      do n = 1, nbc
         massc(n) = am_r*Dc(n)**bm_r
      enddo
      do k = 1, nbc
         nu_c = MIN(15, NINT(1000.E6/t_Nc(k)) + 2)
         do j = 1, ntb_c
            lamc = (t_Nc(k)*am_r* ccg(2,nu_c)*ocg1(nu_c) / r_c(j))**obmr
            N0_c = t_Nc(k)*ocg1(nu_c) * lamc**cce(1,nu_c)
            do i = 1, nbc
               N_c(i) = N0_c* Dc(i)**nu_c*EXP(-lamc*Dc(i))*dtc(i)
               summ = 0.
               summ2 = 0.
               do n = 1, i
                  summ = summ + massc(n)*N_c(n)
                  summ2 = summ2 + N_c(n)
               enddo
               tpc_wev(i,j,k) = summ
               tnc_wev(i,j,k) = summ2
            enddo
         enddo
      enddo
      end subroutine table_dropEvap
      subroutine calc_effectRad (t1d, p1d, qv1d, qc1d, nc1d, qi1d, ni1d, qs1d,   &
     &                re_qc1d, re_qi1d, re_qs1d, Nt_c)
      IMPLICIT NONE
      REAL, INTENT(IN) :: Nt_c
      INTEGER, PARAMETER :: kts=1
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
     &                    t1d, p1d, qv1d, qc1d, nc1d, qi1d, ni1d, qs1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: re_qc1d, re_qi1d, re_qs1d
      INTEGER:: k
      REAL, DIMENSION(kts:kte):: rho, rc, nc, ri, ni, rs
      REAL:: smo2, smob, smoc
      REAL:: tc0, loga_, a_, b_
      DOUBLE PRECISION:: lamc, lami
      LOGICAL:: has_qc, has_qi, has_qs
      INTEGER:: inu_c
      real, dimension(15), parameter:: g_ratio = (/24,60,120,210,336,   &
     &                504,720,990,1320,1716,2184,2730,3360,4080,4896/)
      has_qc = .false.
      has_qi = .false.
      has_qs = .false.
      do k = kts, kte
         rho(k) = 0.622*p1d(k)/(R*t1d(k)*(qv1d(k)+0.622))
         rc(k) = MAX(R1, qc1d(k)*rho(k))
         nc(k) = MAX(2., MIN(nc1d(k)*rho(k), Nt_c_max))
         nc(k) = Nt_c
         if (rc(k).gt.R1 .and. nc(k).gt.R2) has_qc = .true.
         ri(k) = MAX(R1, qi1d(k)*rho(k))
         ni(k) = MAX(R2, ni1d(k)*rho(k))
         if (ri(k).gt.R1 .and. ni(k).gt.R2) has_qi = .true.
         rs(k) = MAX(R1, qs1d(k)*rho(k))
         if (rs(k).gt.R1) has_qs = .true.
      enddo
      if (has_qc) then
      do k = kts, kte
         re_qc1d(k) = 2.49E-6
         if (rc(k).le.R1 .or. nc(k).le.R2) CYCLE
         if (nc(k).lt.100) then
            inu_c = 15
         elseif (nc(k).gt.1.E10) then
            inu_c = 2
         else
            inu_c = MIN(15, NINT(1000.E6/nc(k)) + 2)
         endif
         lamc = (nc(k)*am_r*g_ratio(inu_c)/rc(k))**obmr
         re_qc1d(k) = MAX(2.51E-6, MIN(SNGL(0.5D0 * DBLE(3.+inu_c)/lamc), 50.E-6))
      enddo
      endif
      if (has_qi) then
      do k = kts, kte
         re_qi1d(k) = 2.49E-6
         if (ri(k).le.R1 .or. ni(k).le.R2) CYCLE
         lami = (am_i*cig(2)*oig1*ni(k)/ri(k))**obmi
         re_qi1d(k) = MAX(2.51E-6, MIN(SNGL(0.5D0 * DBLE(3.+mu_i)/lami), 125.E-6))
      enddo
      endif
      if (has_qs) then
      do k = kts, kte
         re_qs1d(k) = 4.99E-6
         if (rs(k).le.R1) CYCLE
         tc0 = MIN(-0.1, t1d(k)-273.15)
         smob = rs(k)*oams
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
         re_qs1d(k) = MAX(5.01E-6, MIN(0.5*(smoc/smob), 999.E-6))
      enddo
      endif
      end subroutine calc_effectRad
      subroutine calc_refl10cm (qv1d, qc1d, qr1d, nr1d, qs1d, qg1d,     &
                          t1d, p1d, dBZ, kts, kte, ii, jj)
      IMPLICIT NONE
      LOGICAL, PARAMETER:: iiwarm = .false.
      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                          qv1d, qc1d, qr1d, nr1d, qs1d, qg1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ
      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho, rhof
      REAL, DIMENSION(kts:kte):: rc, rr, nr, rs, rg
      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilamg, N0_r, N0_g
      REAL, DIMENSION(kts:kte):: mvd_r
      REAL, DIMENSION(kts:kte):: smob, smo2, smoc, smoz
      REAL:: oM3, M0, Mrat, slam1, slam2, xDs
      REAL:: ils1, ils2, t1_vts, t2_vts, t3_vts, t4_vts
      REAL:: vtr_dbz_wt, vts_dbz_wt, vtg_dbz_wt
      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel
      DOUBLE PRECISION:: N0_exp, N0_min, lam_exp, lamr, lamg
      REAL:: a_, b_, loga_, tc0
      DOUBLE PRECISION:: fmelt_s, fmelt_g
      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg
      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL:: xslw1, ygra1, zans1
      do k = kts, kte
         dBZ(k) = -35.0
      enddo
      do k = kts, kte
         temp(k) = t1d(k)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
         rhof(k) = SQRT(RHO_NOT/rho(k))
         rc(k) = MAX(R1, qc1d(k)*rho(k))
         if (qr1d(k) .gt. R1) then
            rr(k) = qr1d(k)*rho(k)
            nr(k) = MAX(R2, nr1d(k)*rho(k))
            lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
            ilamr(k) = 1./lamr
            N0_r(k) = nr(k)*org2*lamr**cre(2)
            mvd_r(k) = (3.0 + mu_r + 0.672) * ilamr(k)
            L_qr(k) = .true.
         else
            rr(k) = R1
            nr(k) = R1
            mvd_r(k) = 50.E-6
            L_qr(k) = .false.
         endif
         if (qs1d(k) .gt. R2) then
            rs(k) = qs1d(k)*rho(k)
            L_qs(k) = .true.
         else
            rs(k) = R1
            L_qs(k) = .false.
         endif
         if (qg1d(k) .gt. R2) then
            rg(k) = qg1d(k)*rho(k)
            L_qg(k) = .true.
         else
            rg(k) = R1
            L_qg(k) = .false.
         endif
      enddo
      do k = kts, kte
         smo2(k) = 0.
         smob(k) = 0.
         smoc(k) = 0.
         smoz(k) = 0.
      enddo
      if (ANY(L_qs .eqv. .true.)) then
      do k = kts, kte
         if (.not. L_qs(k)) CYCLE
         tc0 = MIN(-0.1, temp(k)-273.15)
         smob(k) = rs(k)*oams
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2(k) = smob(k)
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
            smo2(k) = (smob(k)/a_)**(1./b_)
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
         smoc(k) = a_ * smo2(k)**b_
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3) &
     &         + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0 &
     &         + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3) &
     &         + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*cse(3)*cse(3)*cse(3)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) &
     &        + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3) &
     &        + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3) &
     &        + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)
         smoz(k) = a_ * smo2(k)**b_
      enddo
      endif
      if (ANY(L_qg .eqv. .true.)) then
      N0_min = gonv_max
      k_0 = kts
      do k = kte, kts, -1
         if (temp(k).ge.270.65) k_0 = MAX(k_0, k)
      enddo
      do k = kte, kts, -1
         if (k.gt.k_0 .and. L_qr(k) .and. mvd_r(k).gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r(k))
         else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, rg(k)))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0_exp = 10.**(zans1)
         N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
         N0_min = MIN(N0_exp, N0_min)
         N0_exp = N0_min
         lam_exp = (N0_exp*am_g*cgg(1)/rg(k))**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg(k) = 1./lamg
         N0_g(k) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      enddo
      endif
      melti = .false.
      k_0 = kts
      do k = kte-1, kts, -1
         if ( (temp(k).gt.273.15) .and. L_qr(k)                         &
     &                            .and. (L_qs(k+1).or.L_qg(k+1)) ) then
            k_0 = MAX(k+1, k_0)
            melti=.true.
            goto 195
         endif
      enddo
 195  continue
      do k = kts, kte
         ze_rain(k) = 1.e-22
         ze_snow(k) = 1.e-22
         ze_graupel(k) = 1.e-22
         if (L_qr(k)) ze_rain(k) = N0_r(k)*crg(4)*ilamr(k)**cre(4)
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)     &
     &                           * (am_s/900.0)*(am_s/900.0)*smoz(k)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
     &                              * (am_g/900.0)*(am_g/900.0)         &
     &                              * N0_g(k)*cgg(4)*ilamg(k)**cge(4)
      enddo
      if (.not. iiwarm .and. melti .and. k_0.ge.2) then
       do k = k_0-1, kts, -1
          if (L_qs(k) .and. L_qs(k_0) ) then
           fmelt_s = MAX(0.05d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           eta = 0.d0
           oM3 = 1./smoc(k)
           M0 = (smob(k)*oM3)
           Mrat = smob(k)*M0*M0*M0
           slam1 = M0 * Lam0
           slam2 = M0 * Lam1
           do n = 1, nrbins
              x = am_s * xxDs(n)**bm_s
              call rayleigh_soak_wetgraupel (x, DBLE(ocms), DBLE(obms), &
     &              fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
     &              CBACK, mixingrulestring_s, matrixstring_s,          &
     &              inclusionstring_s, hoststring_s,                    &
     &              hostmatrixstring_s, hostinclusionstring_s)
              f_d = Mrat*(Kap0*DEXP(-slam1*xxDs(n))                     &
     &              + Kap1*(M0*xxDs(n))**mu_s * DEXP(-slam2*xxDs(n)))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif
          if (L_qg(k) .and. L_qg(k_0) ) then
           fmelt_g = MAX(0.05d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
           eta = 0.d0
           lamg = 1./ilamg(k)
           do n = 1, nrbins
              x = am_g * xxDg(n)**bm_g
              call rayleigh_soak_wetgraupel (x, DBLE(ocmg), DBLE(obmg), &
     &              fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
     &              CBACK, mixingrulestring_g, matrixstring_g,          &
     &              inclusionstring_g, hoststring_g,                    &
     &              hostmatrixstring_g, hostinclusionstring_g)
              f_d = N0_g(k)*xxDg(n)**mu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif
       enddo
      endif
      do k = kte, kts, -1
         dBZ(k) = 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.d18)
      enddo
      end subroutine calc_refl10cm
end module lean_thompson
