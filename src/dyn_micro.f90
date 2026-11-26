! Thompson microphysics after the dynamics, as atm_srk3 calls it (lines 2806-2816 of mpas_atm_time_integration.F):
! microphysics_from_MPAS -> mp_gt_driver -> precip_to_MPAS -> microphysics_to_MPAS of MPAS 8.4.2, for the configuration
! lean_mpas supports (mp_thompson, n_microp sub-steps per dynamics step, effective radii on, no accumulation limit).
! Working level (2) of the dynamics state in, the same level out. The lean Thompson column (thompson.f90, the column code
! of MPAS's module_mp_thompson.F) is used unchanged; everything else here is the conversion between the dynamics state and a column and back:
! theta from theta_m, pressure from base + perturbation, layer thickness from zgrid, and afterwards theta_m, the diabatic
! heating rt_diabatic_tend that the NEXT step's dynamics reads, rtheta_p, Exner, perturbation pressure, surface pressure.
! Not carried (diagnostics that nothing in the model reads): the per-scheme tendencies rthmpten .. rnrmpten. precipw (column
! water vapour) only when an output stream asks for it (sum_pw). Radar reflectivity (film and product outputs,
! refl10cm diagnostics): when film_capture is set, the column keeps the eight inputs of calc_refl10cm as
! compute_radar_reflectivity of MPAS takes them (after mp_gt_driver, before microphysics_to_MPAS).
module dyn_micro
 use model_timing
 use config, only: nz, ncells, nc1, dt, max_levels
 use dyn_config
 use dyn_step, only: dyn_grid, dyn_fields, dynamics_step, end_of_step_boundary
 use lean_thompson, only: mp_thompson, calc_effectRad, naIN1
#ifdef _OPENACC
 use lean_thompson, only: mp_thompson_a1, mp_thompson_b1, mp_thompson_b1u, mp_thompson_b2, mp_thompson_b3
 use lean_thompson, only: thompson_card_start, th_reset, th_marks_a, th_marks_b, th_moments, th_n0_level, th_n0_scan, th_graupel_level, &
                          th_fall_level, th_fall_species, th_sediment, th_effect_rad
#endif
 use micro_pool, only: pool_slots, micro_pool_allocate, pool_staged, pool_mc, pool_pl
 use phys_mode
 use check_util
 use run_settings, only: n_microp
 use schemes, only: has_re, micro_on
 implicit none
 real, parameter :: rvord = rv/rgas, p0 = 100000., rcv = rgas/(cp-rgas)
 ! The order in which the card's threads take the columns: the columns that hold condensate first, then the dry ones. A warp takes
 ! 32 consecutive entries, and a column with cloud costs the warp many times a dry one; grouped, the dry warps finish early. Every
 ! column is computed by itself (no column reads another), so the order changes no value. micro_wet(c): column c holds
 ! condensate; micro_blk(b): the number of wet columns before block b of 256 columns (a prefix sum); micro_count(1): the
 ! number of wet columns, after which the dry ones start in micro_order (an array of one, so that the kernels read the
 ! card's copy and not a firstprivate copy of a scalar).
 integer, allocatable, save :: micro_order(:)
 integer, save :: micro_count(1) = 0
 logical, allocatable, save :: micro_wet(:)
 integer, allocatable, save :: micro_blk(:)
 !$acc declare create(micro_order, micro_count, micro_wet, micro_blk)
 ! Thompson in stages on the card (the card's default path; the one column kernel only with the build switch
 ! LEAN_MICRO_ONEKERNEL). The columns in chunks of pool_slots, in micro_order; per sub-step the generated level stages
 ! mp_thompson_a1, b1, b1u, b2, b3 (one (column, level) point each) and the hand-written pieces of tools/thompson_card.f90 for the
 ! column parts (moments, graupel N0 scan, fall speeds, sedimentation per column and species), all on async queue 1.
 ! The column arrays of micro_cell between the kernels: micro_pool's pool_mc(slot, level, field), slot first so that the level
 ! kernels' loads coalesce; mc_s(8, slot) the precipitation sums; mc_skip(slot) mp_thompson's no_micro; mc_nm(level, slot)
 ! the no_micro mark of each level (their AND is mc_skip).
 real, allocatable, save :: mc_s(:,:)
 logical, allocatable, save :: mc_skip(:), mc_nm(:,:)
 !$acc declare create(mc_s, mc_skip, mc_nm)
 integer, save :: micro_staged_mode = -1
 ! b1's points in a list, those with a species first (pt_list, pt_count)
 integer, allocatable, save :: pt_list(:)
 integer, save :: pt_count(2) = 0
 !$acc declare create(pt_list, pt_count)
 ! MPAS runs the microphysics only below config_microphysics_top (default 45 km). lean_mpas has no such cut, so the start
 ! refuses a mesh whose top layer is above it (lean_inputs.f90, apply_run_config).

 ! what the Thompson step keeps from step to step, besides the dynamics state
 type micro_fields
  real, allocatable :: nt_c(:)                                  ! cloud droplet number concentration of the column (input)
  real, allocatable :: surface_pressure(:), tend_sfc_pressure(:)
  real, allocatable :: rainnc(:), rainncv(:), snownc(:), snowncv(:), graupelnc(:), graupelncv(:), sr(:)   ! accumulated / of the last step
  real, allocatable :: dtheta_dt_mp(:,:), rainprod(:,:), evapprod(:,:), re_cloud(:,:), re_ice(:,:), re_snow(:,:)
  logical :: sum_pw = .false.
  real, allocatable :: precipw(:)
  ! film_capture: this step keeps the reflectivity inputs: an output step of the film or product output, or a diagnostics
  ! step whose streams need the reflectivity
  logical :: film_capture = .false.
  real, allocatable :: film_in(:,:,:)                           ! (qv, qc, qr, nr, qs, qg, t, p, level, cell); one dummy cell when off
 end type

 ! What Thompson reads and writes, as host copies: the card result and the host input of a check.
 type micro_snapshot
  real, allocatable :: scalars2(:,:,:), theta_m2(:,:), exner(:,:), rtheta_p(:,:), pressure_p(:,:), rt_diabatic_tend(:,:)
  real, allocatable :: nt_c(:), surface_pressure(:), tend_sfc_pressure(:), rainnc(:), rainncv(:), snownc(:), snowncv(:), graupelnc(:)
  real, allocatable :: graupelncv(:), sr(:), dtheta_dt_mp(:,:), rainprod(:,:), evapprod(:,:), re_cloud(:,:), re_ice(:,:), re_snow(:,:)
 end type
 type(micro_snapshot), save :: snap_in, snap_card

contains
 ! Column water vapour as MPAS's precip_to_MPAS (mpas_atmphys_driver_microphysics.F:614-616): its rho_p is already the dry
 ! density zz*rho_zz (mpas_atmphys_interface.F:664), and MPAS still divides it by 1+qv. The build switch LEAN_PRECIPW_MOIST
 ! drops that division.
 real function precipw_column(n,qv,rho_zz,zz,dz) result(pw)
 !$acc routine seq
 integer,intent(in)::n
 real,intent(in)::qv(*),rho_zz(*),zz(*),dz(*)
 real :: rho_a
 integer :: k
 pw=0.
 do k=1,n
  rho_a=rho_zz(k)*zz(k)
#ifndef LEAN_PRECIPW_MOIST
  rho_a=rho_a/(1.+qv(k))
#endif
  pw=pw+qv(k)*rho_a*dz(k)
 enddo
 end function

 subroutine allocate_micro(mp)
 type(micro_fields), intent(inout) :: mp
 integer :: c
 if (.not. allocated(micro_order)) then
  allocate(micro_order(nc1), micro_wet(nc1), micro_blk(nc1/256 + 2))
  do c = 1, nc1
   micro_order(c) = c
  end do
  !$acc update device(micro_order)
 end if
 allocate(mp%precipw(nc1));mp%precipw=0.
 allocate(mp%nt_c(nc1), mp%surface_pressure(nc1), mp%tend_sfc_pressure(nc1), mp%rainnc(nc1), mp%rainncv(nc1), mp%snownc(nc1))
 allocate(mp%snowncv(nc1), mp%graupelnc(nc1), mp%graupelncv(nc1), mp%sr(nc1))
 allocate(mp%dtheta_dt_mp(nz,nc1), mp%rainprod(nz,nc1), mp%evapprod(nz,nc1), mp%re_cloud(nz,nc1), mp%re_ice(nz,nc1), mp%re_snow(nz,nc1))
 mp%nt_c = 0.; mp%surface_pressure = 0.; mp%tend_sfc_pressure = 0.; mp%rainnc = 0.; mp%rainncv = 0.; mp%snownc = 0.
 mp%snowncv = 0.; mp%graupelnc = 0.; mp%graupelncv = 0.; mp%sr = 0.
 mp%dtheta_dt_mp = 0.; mp%rainprod = 0.; mp%evapprod = 0.; mp%re_cloud = 0.; mp%re_ice = 0.; mp%re_snow = 0.
 allocate(mp%film_in(8,nz,1)); mp%film_in = 0.
 end subroutine

 ! the reflectivity inputs for every cell (film output); before micro_enter_device
 subroutine micro_film_start(mp)
 type(micro_fields), intent(inout) :: mp
 deallocate(mp%film_in); allocate(mp%film_in(8,nz,nc1)); mp%film_in = 0.
 end subroutine

 subroutine micro_film_to_host(mp)
 type(micro_fields), intent(inout) :: mp
 !$acc update self(mp%film_in)
 end subroutine

 ! One Thompson step on every column of the working level (on the card: the staged kernels of micro_staged, or with
 ! LEAN_MICRO_ONEKERNEL the column routine as one kernel; nothing crosses the bus).
 subroutine microphysics_step(g, f, mp, on_card)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 logical, optional, intent(in) :: on_card
 logical :: card
 card = .false.
 if (present(on_card)) card = on_card
 call microphysics_columns(card, f, mp, g%m%zgrid, g%m%zz, g%s%pressure_base, g%s%rtheta_base, g%s%exner_base)
 end subroutine

 ! The mesh arrays are arguments: the compute region then needs the arrays on the card, not the grid structure.
 subroutine microphysics_columns(on_card, f, mp, zgrid, zz, pressure_base, rtheta_base, exner_base)
 logical, intent(in) :: on_card
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 real, intent(in) :: zgrid(nz+1,nc1), zz(nz,nc1), pressure_base(nz,nc1), rtheta_base(nz,nc1), exner_base(nz,nc1)
 integer :: c, ncap, k, n, slot, b, nb
 logical :: cap, sum_pw, wet
 character(8) :: sv
 sum_pw = mp%sum_pw
 cap = mp%film_capture
 ncap = size(mp%film_in, 3)
 if (on_card) then
  ! the order of the columns (see micro_order): condensate anywhere in the column puts it in the front group; inside each
  ! group the mesh's order (a stable partition: per block of 256 columns the wet count, their offsets, then each block in
  ! turn), so that neighbouring threads touch neighbouring columns of the state
  !$acc parallel loop gang vector default(present) private(k, wet)
  do c = 1, ncells
   wet = .false.
   do k = 1, nz
    if (f%scalars2(i_qc,k,c) > 0. .or. f%scalars2(i_qr,k,c) > 0. .or. f%scalars2(i_qi,k,c) > 0. .or. &
        f%scalars2(i_qs,k,c) > 0. .or. f%scalars2(i_qg,k,c) > 0.) wet = .true.
   end do
   micro_wet(c) = wet
  end do
  nb = (ncells + 255)/256
  !$acc parallel loop gang default(present) private(n)
  do b = 1, nb
   n = 0
   !$acc loop vector reduction(+:n)
   do c = (b-1)*256 + 1, min(b*256, ncells)
    if (micro_wet(c)) n = n + 1
   end do
   micro_blk(b) = n
  end do
  !$acc serial default(present)
  n = 0
  do b = 1, nb
   k = micro_blk(b)
   micro_blk(b) = n
   n = n + k
  end do
  micro_count(1) = n
  !$acc end serial
  !$acc parallel loop gang vector default(present) private(c, n, slot)
  do b = 1, nb
   n = micro_blk(b)                   ! wet columns before this block
   slot = (b-1)*256 - micro_blk(b)    ! dry columns before this block
   do c = (b-1)*256 + 1, min(b*256, ncells)
    if (micro_wet(c)) then
     n = n + 1
     micro_order(n) = c
    else
     slot = slot + 1
     micro_order(micro_count(1) + slot) = c
    end if
   end do
  end do
#ifdef _OPENACC
  if (micro_staged_mode < 0) then
   micro_staged_mode = 0
   if (pool_staged) micro_staged_mode = 1   ! always in the default card build; with LEAN_MICRO_ONEKERNEL only when
                                            ! LEAN_MICRO_STAGED=1 (micro_pool_start)
   if (micro_staged_mode == 1) then
    allocate(mc_s(8, pool_slots), mc_skip(pool_slots), mc_nm(max_levels, pool_slots))
    call thompson_card_start(pool_slots)
    allocate(pt_list(pool_slots*nz))
   end if
  end if
  if (micro_staged_mode == 1) then
   call micro_staged(f%scalars2, f%theta_m2, f%rho_zz2, f%pressure_p, f%exner, f%w2, f%rtheta_p, f%rt_diabatic_tend, &
                     zgrid, zz, pressure_base, rtheta_base, exner_base, mp%nt_c, mp%surface_pressure, &
                     mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv, mp%sr, &
                     mp%dtheta_dt_mp, mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow, cap, ncap, mp%film_in, sum_pw, &
                     mp%precipw, mc_s, mc_skip, mc_nm)
   return
  end if
#endif
#ifdef LEAN_MICRO_ONEKERNEL
  ! the one column kernel (build switch LEAN_MICRO_ONEKERNEL, run time LEAN_MICRO_STAGED unset): the grid capped at the
  ! work-array pool of mp_thompson (src/device/micro_pool.f90), each thread takes several columns in turn. Not in the default binary:
  ! its 13.3 KB frame would set the card's stack reservation.
  !$acc parallel loop gang vector num_gangs(pool_slots/128) vector_length(128) default(present) private(c)
  do n = 1, ncells
   c = micro_order(n)
   call micro_cell(c, f%scalars2, f%theta_m2, f%rho_zz2, f%pressure_p, f%exner, f%w2, f%rtheta_p, f%rt_diabatic_tend, &
                   zgrid, zz, pressure_base, rtheta_base, exner_base, mp%nt_c, mp%surface_pressure, &
                   mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv, mp%sr, &
                   mp%dtheta_dt_mp, mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow, cap, ncap, mp%film_in, sum_pw, mp%precipw)
  end do
#endif
 else
  call micro_pool_allocate(128)   ! a card build's mp_thompson keeps its work arrays in the pool (slot 1 on the host)
  !$omp do schedule(dynamic,64)
  do c = 1, ncells
   call micro_cell(c, f%scalars2, f%theta_m2, f%rho_zz2, f%pressure_p, f%exner, f%w2, f%rtheta_p, f%rt_diabatic_tend, &
                   zgrid, zz, pressure_base, rtheta_base, exner_base, mp%nt_c, mp%surface_pressure, &
                   mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv, mp%sr, &
                   mp%dtheta_dt_mp, mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow, cap, ncap, mp%film_in, sum_pw, mp%precipw)
  end do
 end if
 end subroutine

#ifdef _OPENACC
 ! micro_cell in stages (see mc_s): the same statements on the same values, in kernels over the chunk's columns (slot j is the
 ! column's position in its chunk of micro_order and its index in the pools of the Thompson stages).
 subroutine micro_staged(scalars2, theta_m2, rho_zz2, pressure_p, exner, w2, rtheta_p, rt_diabatic_tend, zgrid, zz, pressure_base, &
                         rtheta_base, exner_base, nt_c, surface_pressure, tend_sfc_pressure, rainnc, rainncv, snownc, snowncv, &
                         graupelnc, graupelncv, sr, dtheta_dt_mp, rainprod, evapprod, re_cloud, re_ice, re_snow, capture, ncap, film_in, &
                         sum_pw, precipw, mcs, skip, nm)
 real, intent(inout) :: scalars2(num_scalars,nz,nc1), theta_m2(nz,nc1), exner(nz,nc1), rtheta_p(nz,nc1), pressure_p(nz,nc1)
 real, intent(inout) :: rt_diabatic_tend(nz,nc1)
 real, intent(in) :: rho_zz2(nz,nc1), w2(nz+1,nc1), zgrid(nz+1,nc1), zz(nz,nc1), pressure_base(nz,nc1), rtheta_base(nz,nc1)
 real, intent(in) :: exner_base(nz,nc1)
 real, intent(inout) :: nt_c(nc1), surface_pressure(nc1), tend_sfc_pressure(nc1), rainnc(nc1), rainncv(nc1), snownc(nc1)
 real, intent(inout) :: snowncv(nc1), graupelnc(nc1), graupelncv(nc1), sr(nc1)
 real, intent(inout) :: dtheta_dt_mp(nz,nc1), rainprod(nz,nc1), evapprod(nz,nc1), re_cloud(nz,nc1), re_ice(nz,nc1), re_snow(nz,nc1)
 logical, intent(in) :: sum_pw, capture
 real, intent(inout) :: precipw(nc1)
 integer, intent(in) :: ncap
 real, intent(inout) :: film_in(8,nz,ncap)
 real, intent(inout) :: mcs(8,pool_slots)
 logical, intent(inout) :: skip(pool_slots), nm(max_levels,pool_slots)
 integer :: n0, nn, j, c, k, isub, isp, p, t, nt8, nz4
 real :: rec, rei, res
 logical :: wet, last, hasre
 real :: rho1, rho2, tem1, tem2
 ! pool_mc fields (slot, level, field): 1 qv 2 qc 3 qi 4 qr 5 qs 6 qg 7 ni 8 nr 9 nc 10 nwfa 11 nifa 12 t 13 p 14 w 15 dz 16 theta 17 rho
 ! mcs: 1 pr 2 ps 3 pg 4 pice 5 sum_pr 6 sum_ps 7 sum_pg 8 sum_pice
 hasre = has_re /= 0   ! (a scalar of the routine: the kernels take it as a value)
 do n0 = 1, ncells, pool_slots
  nn = min(pool_slots, ncells - n0 + 1)
  nt8 = (nn + 7)/8*8   ! the tiles (load, end)
  nz4 = (nz + 3)/4*4
  ! ---- state -> column (microphysics_from_MPAS)
  !$acc parallel loop gang vector default(present) private(c, j, k) async(1)
  do t = 0, nt8*nz4 - 1   ! tiles of 8 columns x 4 levels: state (level, column) and pools (column, level) both in short runs
                          ! (measured about 40 % faster than either order alone)
   j = (mod(t/32, nt8/8))*8 + mod(t, 8) + 1
   k = (t/32/(nt8/8))*4 + mod(t/8, 4) + 1
   if (j <= nn .and. k <= nz) then
    c = micro_order(n0+j-1)
    pool_mc(j,k,1) = scalars2(i_qv,k,c); pool_mc(j,k,2) = scalars2(i_qc,k,c); pool_mc(j,k,4) = scalars2(i_qr,k,c)
    pool_mc(j,k,3) = scalars2(i_qi,k,c); pool_mc(j,k,5) = scalars2(i_qs,k,c); pool_mc(j,k,6) = scalars2(i_qg,k,c)
    pool_mc(j,k,7) = scalars2(i_ni,k,c); pool_mc(j,k,8) = scalars2(i_nr,k,c)
    pool_mc(j,k,16) = theta_m2(k,c) / (1. + rvord*max(0., pool_mc(j,k,1)))
    pool_mc(j,k,13) = pressure_base(k,c) + pressure_p(k,c)
    pool_mc(j,k,15) = zgrid(k+1,c) - zgrid(k,c)
    pool_mc(j,k,14) = w2(k,c)
    if (k == 1) then
     mcs(5,j) = 0.; mcs(6,j) = 0.; mcs(7,j) = 0.; mcs(8,j) = 0.
     mcs(1,j) = 0.; mcs(2,j) = 0.; mcs(3,j) = 0.; mcs(4,j) = 0.
     skip(j) = .true.
     call th_reset(j)
    end if
   end if
  end do
  do isub = 1, n_microp
   last = isub == n_microp
   ! the levels' inputs (mp_thompson_a1), the column index fastest; no_micro of the column (the AND of the levels') and k_0 as
   ! marks of the points (skip, th_k0: reset at the end of the sub-step before)
   !$acc parallel loop gang vector collapse(2) default(present) private(c) async(1)
   do k = 1, nz
    do j = 1, nn
     c = micro_order(n0+j-1)
     pool_mc(j,k,12) = pool_mc(j,k,16)*exner(k,c)
     pool_mc(j,k,17) = 0.622*pool_mc(j,k,13)/(287.04*pool_mc(j,k,12)*(pool_mc(j,k,1)+0.622))
     pool_mc(j,k,9) = nt_c(c)/pool_mc(j,k,17)
     pool_mc(j,k,10) = 11.1E6/pool_mc(j,k,17)
     pool_mc(j,k,11) = naIN1*0.01/pool_mc(j,k,17)
     call mp_thompson_a1(mcs(1,j), mcs(2,j), mcs(3,j), mcs(4,j), rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c), &
                         j, k, nm(k,j))
     if (.not. nm(k,j)) skip(j) = .false.
     call th_marks_a(j, k)
     ! the moments of mp_thompson_a2 per level (also on columns that turn out to skip: nothing reads them there)
     call th_moments(j, k)
    end do
   end do
   ! the graupel N0 of the levels (k_0 now known), the running minimum per column (tools/thompson_card.f90)
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do k = 1, nz
    do j = 1, nn
     if (.not. skip(j)) call th_n0_level(j, k, .false.)
     if (k == 1 .and. j == 1) then   ! the point list of b1 starts empty
      pt_count(1) = 0
      pt_count(2) = 0
     end if
    end do
   end do
   !$acc parallel loop gang vector default(present) async(1)
   do j = 1, nn
    if (.not. skip(j)) call th_n0_scan(j)
   end do
   ! the graupel moments of the level, then the level work (mp_thompson_b1)
   ! the points with a species first (pt_list, for b1 and b2): a warp then takes points of one kind, which measured faster
   ! for b1 and b2 (b3 was slower in this order and keeps the level loop); each point is computed by itself, so the order
   ! changes no value
   !$acc parallel loop gang vector collapse(2) default(present) private(p, wet) async(1)
   do k = 1, nz
    do j = 1, nn
     wet = pool_pl(j,k,2) .or. pool_pl(j,k,3) .or. pool_pl(j,k,4) .or. pool_pl(j,k,5) .or. pool_pl(j,k,6)
     if (wet) then
      !$acc atomic capture
      pt_count(1) = pt_count(1) + 1
      p = pt_count(1)
      !$acc end atomic
     else
      !$acc atomic capture
      pt_count(2) = pt_count(2) + 1
      p = pt_count(2)
      !$acc end atomic
      p = nn*nz + 1 - p
     end if
     pt_list(p) = (k-1)*nn + j
    end do
   end do
   !$acc parallel loop gang vector default(present) private(c, j, k) async(1)
   do p = 1, nn*nz
    j = mod(pt_list(p)-1, nn) + 1
    k = (pt_list(p)-1)/nn + 1
    if (.not. skip(j)) then
     c = micro_order(n0+j-1)
     call th_graupel_level(j, k)
     call mp_thompson_b1(mcs(1,j), mcs(2,j), mcs(3,j), mcs(4,j), rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c), &
                        j, k)
     ! the prediction and the size distributions of the level (mp_thompson_b1u), the same point after b1
     call mp_thompson_b1u(mcs(1,j), mcs(2,j), mcs(3,j), mcs(4,j), rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c), &
                          j, k)
     call th_marks_b(j, k)
    end if
   end do
   ! mp_thompson_c1: the graupel N0 again (after the update; k_0 th_k0b)
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do k = 1, nz
    do j = 1, nn
     if (.not. skip(j)) call th_n0_level(j, k, .true.)
    end do
   end do
   !$acc parallel loop gang vector default(present) async(1)
   do j = 1, nn
    if (.not. skip(j)) call th_n0_scan(j)
   end do
   ! the graupel moments of the level (c1), condensation and evaporation (mp_thompson_b2), the fall speeds of the level (c2)
   !$acc parallel loop gang vector default(present) private(c, j, k) async(1)
   do p = 1, nn*nz
    j = mod(pt_list(p)-1, nn) + 1
    k = (pt_list(p)-1)/nn + 1
    if (.not. skip(j)) then
     c = micro_order(n0+j-1)
     call th_graupel_level(j, k)
     call mp_thompson_b2(mcs(1,j), mcs(2,j), mcs(3,j), mcs(4,j), rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c), &
                        j, k)
     call th_fall_level(j, k)
    end if
   end do
   ! the fall speeds per (column, species) (from above, mixed, ksed1, nstep), then the sedimentation per (column, species)
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do isp = 1, 4
    do j = 1, nn
     if (.not. skip(j)) call th_fall_species(j, isp, dt/real(n_microp))
    end do
   end do
   !$acc parallel loop gang vector collapse(2) default(present) async(1)
   do isp = 1, 5
    do j = 1, nn
     if (.not. skip(j)) call th_sediment(j, isp, dt/real(n_microp), mcs(1:4,j))
    end do
   end do
   ! the freezing of the remaining cloud and ice, and the tendencies applied (mp_thompson_b3)
   !$acc parallel loop gang vector collapse(2) default(present) private(c) async(1)
   do k = 1, nz
    do j = 1, nn
     if (.not. skip(j)) then
      c = micro_order(n0+j-1)
      call mp_thompson_b3(mcs(1,j), mcs(2,j), mcs(3,j), mcs(4,j), rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c), &
                          j, k)
     end if
     ! theta from the new temperature (micro_cell after mp_thompson), on every column
     pool_mc(j,k,16) = pool_mc(j,k,12)/exner(k,micro_order(n0+j-1))
    end do
   end do
   !$acc parallel loop gang vector default(present) private(c, k) async(1)
   do j = 1, nn
    c = micro_order(n0+j-1)
    do k = 1, nz
     if (pool_mc(j,k,1) < 0.) then
      if (k < nz-2 .and. k > 2) then
       pool_mc(j,k,1) = max(1.e-7, 0.5*(pool_mc(j,k-1,1)+pool_mc(j,k+1,1)))
      else
       pool_mc(j,k,1) = 1.e-7
      end if
     end if
    end do
    mcs(5,j) = mcs(5,j) + mcs(1,j); mcs(6,j) = mcs(6,j) + mcs(2,j); mcs(7,j) = mcs(7,j) + mcs(3,j); mcs(8,j) = mcs(8,j) + mcs(4,j)
    ! the next sub-step's precipitation, no_micro and marks start here (after the last sub-step skip stays: the end reads it)
    mcs(1,j) = 0.; mcs(2,j) = 0.; mcs(3,j) = 0.; mcs(4,j) = 0.
    if (.not. last) skip(j) = .true.
    call th_reset(j)
   end do
  end do
  ! the end of micro_cell: its level statements per (column, level) (the reflectivity inputs, calc_effectRad per level
  ! (th_effect_rad), column -> state), then its column statements (precipw, the precipitation, the surface pressure)
  !$acc parallel loop gang vector collapse(2) default(present) private(c, rec, rei, res) async(1)
  do j = 1, nn
   do k = 1, nz   ! the level fastest: the state's (level, column) arrays are written in order (measured faster here than
                  ! the column fastest or tiles of 8 x 4)
    c = micro_order(n0+j-1)
    if (capture) then
     film_in(1,k,c) = pool_mc(j,k,1); film_in(2,k,c) = pool_mc(j,k,2); film_in(3,k,c) = pool_mc(j,k,4); film_in(4,k,c) = pool_mc(j,k,8)
     film_in(5,k,c) = pool_mc(j,k,5); film_in(6,k,c) = pool_mc(j,k,6); film_in(7,k,c) = pool_mc(j,k,16)*exner(k,c)
     film_in(8,k,c) = pool_mc(j,k,13)
    end if
    ! the step's rainprod and evapprod are the last sub-step's: b1 and b2 wrote them on the columns that did not skip
    if (skip(j)) then
     rainprod(k,c) = 0.
     evapprod(k,c) = 0.
    end if
    ! module_mp_thompson.F:1353-1365: the radii only when the radiation takes them (as micro_cell)
    if (hasre) then
     call th_effect_rad(pool_mc(j,k,12), pool_mc(j,k,13), pool_mc(j,k,1), pool_mc(j,k,2), pool_mc(j,k,3), pool_mc(j,k,7), &
                        pool_mc(j,k,5), nt_c(c), rec, rei, res)
     re_cloud(k,c) = max(2.49e-6, min(rec, 50.e-6))
     re_ice(k,c) = max(4.99e-6, min(rei, 125.e-6))
     re_snow(k,c) = max(9.99e-6, min(res, 999.e-6))
    end if
    rt_diabatic_tend(k,c) = theta_m2(k,c)
    dtheta_dt_mp(k,c) = theta_m2(k,c)/(1. + rvord*scalars2(i_qv,k,c))
    scalars2(i_qv,k,c) = pool_mc(j,k,1); scalars2(i_qc,k,c) = pool_mc(j,k,2); scalars2(i_qr,k,c) = pool_mc(j,k,4)
    theta_m2(k,c) = pool_mc(j,k,16) * (1. + rvord*pool_mc(j,k,1))
    rt_diabatic_tend(k,c) = (theta_m2(k,c) - rt_diabatic_tend(k,c)) / dt
    dtheta_dt_mp(k,c) = (theta_m2(k,c)/(1. + rvord*scalars2(i_qv,k,c)) - dtheta_dt_mp(k,c)) / dt
    rtheta_p(k,c) = rho_zz2(k,c) * theta_m2(k,c) - rtheta_base(k,c)
    exner(k,c) = (zz(k,c)*(rgas/p0)*(rtheta_p(k,c) + rtheta_base(k,c)))**rcv
    pressure_p(k,c) = zz(k,c)*rgas*(exner(k,c)*rtheta_p(k,c) + (exner(k,c) - exner_base(k,c))*rtheta_base(k,c))
    scalars2(i_qi,k,c) = pool_mc(j,k,3); scalars2(i_qs,k,c) = pool_mc(j,k,5); scalars2(i_qg,k,c) = pool_mc(j,k,6)
    scalars2(i_ni,k,c) = pool_mc(j,k,7); scalars2(i_nr,k,c) = pool_mc(j,k,8)
   end do
  end do
  !$acc parallel loop gang vector default(present) private(c, k, rho1, rho2, tem1, tem2) async(1)
  do j = 1, nn
   c = micro_order(n0+j-1)
   call micro_column_end(c, j, rho_zz2, pressure_p, zgrid, zz, pressure_base, surface_pressure, tend_sfc_pressure, rainnc, &
                         rainncv, snownc, snowncv, graupelnc, graupelncv, sr, sum_pw, precipw, mcs)
  end do
 end do
 !$acc wait(1)
 end subroutine

 ! the column statements of the end of micro_cell for column c (slot j of pool_mc, mcs), after its level statements: precipw
 ! (from the column's qv and dz, which those do not change), the precipitation of the step, the surface pressure (from the new
 ! pressure_p of level 1)
 subroutine micro_column_end(c, j, rho_zz2, pressure_p, zgrid, zz, pressure_base, surface_pressure, tend_sfc_pressure, rainnc, &
                             rainncv, snownc, snowncv, graupelnc, graupelncv, sr, sum_pw, precipw, mcs)
 !$acc routine seq
 integer, intent(in) :: c, j
 real, intent(in) :: rho_zz2(nz,*), pressure_p(nz,*), zgrid(nz+1,*), zz(nz,*), pressure_base(nz,*)
 real, intent(inout) :: surface_pressure(*), tend_sfc_pressure(*), rainnc(*), rainncv(*), snownc(*), snowncv(*), graupelnc(*)
 real, intent(inout) :: graupelncv(*), sr(*), precipw(*)
 logical, intent(in) :: sum_pw
 real, intent(inout) :: mcs(8,*)
 real :: pr, ps, pg, pice, rho1, rho2, tem1, tem2
 real :: qv(max_levels), dz(max_levels)
 integer :: k
 pr = mcs(5,j); ps = mcs(6,j); pg = mcs(7,j); pice = mcs(8,j)
 if (sum_pw) then
  do k = 1, nz
   qv(k) = pool_mc(j,k,1); dz(k) = pool_mc(j,k,15)
  end do
  precipw(c)=precipw_column(nz,qv,rho_zz2(:,c),zz(:,c),dz)
 end if
 rainncv(c) = pr + (ps + pg + pice)
 rainnc(c) = rainnc(c) + rainncv(c)
 snowncv(c) = ps + pice
 graupelncv(c) = pg
 sr(c) = (snowncv(c) + graupelncv(c)) / (rainncv(c) + 1.e-12)
 snownc(c) = snownc(c) + snowncv(c)
 graupelnc(c) = graupelnc(c) + graupelncv(c)
 tem1 = zgrid(2,c) - zgrid(1,c)
 tem2 = zgrid(3,c) - zgrid(2,c)
 rho1 = rho_zz2(1,c) * zz(1,c) * (1. + pool_mc(j,1,1))
 rho2 = rho_zz2(2,c) * zz(2,c) * (1. + pool_mc(j,2,1))
 tend_sfc_pressure(c) = surface_pressure(c)
 surface_pressure(c) = 0.5*gravity*(zgrid(2,c) - zgrid(1,c)) * (rho1 - 0.5*(rho2 - rho1)*tem1/(tem1 + tem2))
 surface_pressure(c) = surface_pressure(c) + pressure_p(1,c) + pressure_base(1,c)
 tend_sfc_pressure(c) = (surface_pressure(c) - tend_sfc_pressure(c)) / dt
 end subroutine

#endif

 ! One column.
 subroutine micro_cell(c, scalars2, theta_m2, rho_zz2, pressure_p, exner, w2, rtheta_p, rt_diabatic_tend, zgrid, zz, pressure_base, &
                       rtheta_base, exner_base, nt_c, surface_pressure, tend_sfc_pressure, rainnc, rainncv, snownc, snowncv, &
                       graupelnc, graupelncv, sr, dtheta_dt_mp, rainprod, evapprod, re_cloud, re_ice, re_snow, capture, ncap, film_in, sum_pw, precipw)
 !$acc routine seq
 integer, intent(in) :: c
 real, intent(inout) :: scalars2(num_scalars,nz,*), theta_m2(nz,*), exner(nz,*), rtheta_p(nz,*), pressure_p(nz,*)
 real, intent(inout) :: rt_diabatic_tend(nz,*)
 real, intent(in) :: rho_zz2(nz,*), w2(nz+1,*), zgrid(nz+1,*), zz(nz,*), pressure_base(nz,*), rtheta_base(nz,*)
 real, intent(in) :: exner_base(nz,*)
 real, intent(inout) :: nt_c(*), surface_pressure(*), tend_sfc_pressure(*), rainnc(*), rainncv(*), snownc(*)
 real, intent(inout) :: snowncv(*), graupelnc(*), graupelncv(*), sr(*)
 real, intent(inout) :: dtheta_dt_mp(nz,*), rainprod(nz,*), evapprod(nz,*), re_cloud(nz,*), re_ice(nz,*), re_snow(nz,*)
 logical, intent(in) :: sum_pw
 real, intent(inout) :: precipw(*)
 logical, intent(in) :: capture
 integer, intent(in) :: ncap
 real, intent(inout) :: film_in(8,nz,ncap)
 real :: qv(max_levels), qc(max_levels), qr(max_levels), qi(max_levels), qs(max_levels), qg(max_levels), ni(max_levels), nr(max_levels)
 real :: theta(max_levels), t(max_levels), p(max_levels), dz(max_levels), w(max_levels), rho(max_levels), nc(max_levels), nwfa(max_levels), nifa(max_levels)
 real :: pr, ps, pg, pice, rho1, rho2, tem1, tem2
 integer :: k
 real :: sum_pr, sum_ps, sum_pg, sum_pice
 integer :: isub
 ! ---- state -> column (microphysics_from_MPAS)
 do k = 1, nz
  qv(k) = scalars2(i_qv,k,c); qc(k) = scalars2(i_qc,k,c); qr(k) = scalars2(i_qr,k,c)
  qi(k) = scalars2(i_qi,k,c); qs(k) = scalars2(i_qs,k,c); qg(k) = scalars2(i_qg,k,c)
  ni(k) = scalars2(i_ni,k,c); nr(k) = scalars2(i_nr,k,c)
  theta(k) = theta_m2(k,c) / (1. + rvord*max(0., qv(k)))
  p(k) = pressure_base(k,c) + pressure_p(k,c)
  dz(k) = zgrid(k+1,c) - zgrid(k,c)
  w(k) = w2(k,c)
 end do
 ! ---- the column (mp_gt_driver; temperature, density and the droplet / aerosol numbers as mp_gt_driver builds them)
 ! MPAS calls mp_gt_driver n_microp = max(nint(dt/90),1) times with dt/n_microp (mpas_atmphys_manager.F:686-693): 7 calls
 ! at dt = 600 s, one at dt below 135 s.
 ! Its step precipitation is the sum of the calls (rainnc_p, zeroed once a step by precip_from_MPAS); rainprod and evapprod are the
 ! last call's, as here.
 sum_pr = 0.; sum_ps = 0.; sum_pg = 0.; sum_pice = 0.
 do isub = 1, n_microp
 do k = 1, nz
  t(k) = theta(k)*exner(k,c)
  rho(k) = 0.622*p(k)/(287.04*t(k)*(qv(k)+0.622))
  nc(k) = nt_c(c)/rho(k)
  nwfa(k) = 11.1E6/rho(k)
  nifa(k) = naIN1*0.01/rho(k)
 end do
 pr = 0.; ps = 0.; pg = 0.; pice = 0.
 rainprod(:,c) = 0.
 evapprod(:,c) = 0.
 call mp_thompson(qv, qc, qi, qr, qs, qg, ni, nr, nc, nwfa, nifa, t, p, w, dz, pr, ps, pg, pice, &
                  rainprod(:,c), evapprod(:,c), dt/real(n_microp), 1, 1, nt_c(c))
 do k = 1, nz
  theta(k) = t(k)/exner(k,c)
  if (qv(k) < 0.) then
   if (k < nz-2 .and. k > 2) then
    qv(k) = max(1.e-7, 0.5*(qv(k-1)+qv(k+1)))
   else
    qv(k) = 1.e-7
   end if
  end if
 end do
 sum_pr = sum_pr + pr; sum_ps = sum_ps + ps; sum_pg = sum_pg + pg; sum_pice = sum_pice + pice
 end do
 pr = sum_pr; ps = sum_ps; pg = sum_pg; pice = sum_pice
 ! physics/mpas_atmphys_driver_microphysics.F:605-617, before microphysics_to_MPAS.
 if(sum_pw)precipw(c)=precipw_column(nz,qv,rho_zz2(:,c),zz(:,c),dz)
 ! the inputs of calc_refl10cm in compute_radar_reflectivity: t1d = th_p*pi_p, p1d = pres_p, the species after the column
 if (capture) then
  do k = 1, nz
   film_in(1,k,c) = qv(k); film_in(2,k,c) = qc(k); film_in(3,k,c) = qr(k); film_in(4,k,c) = nr(k)
   film_in(5,k,c) = qs(k); film_in(6,k,c) = qg(k); film_in(7,k,c) = theta(k)*exner(k,c); film_in(8,k,c) = p(k)
  end do
 end if
 ! module_mp_thompson.F:1353-1365: the radii only when the radiation takes them (has_reqc, has_reqi and has_reqs, all one value)
 if (has_re /= 0) then
  re_cloud(:,c) = 2.49e-6; re_ice(:,c) = 4.99e-6; re_snow(:,c) = 9.99e-6
  call calc_effectRad(t, p, qv, qc, nc, qi, ni, qs, re_cloud(:,c), re_ice(:,c), re_snow(:,c), nt_c(c))
  re_cloud(:,c) = max(2.49e-6, min(re_cloud(:,c), 50.e-6))
  re_ice(:,c) = max(4.99e-6, min(re_ice(:,c), 125.e-6))
  re_snow(:,c) = max(9.99e-6, min(re_snow(:,c), 999.e-6))
 end if
 ! ---- precipitation of the step (precip_to_MPAS: this step's amounts, then the accumulations)
 ! MPAS built with nvfortran in strict floating point (-Kieee -Mnofma) adds the rain to the bracket it also divides by in sr
 ! (module_mp_thompson.F 1194 and 1204); a left-to-right sum differs in the last bit where rain falls with snow or graupel
 ! (docs/fixes.md)
 rainncv(c) = pr + (ps + pg + pice)
 rainnc(c) = rainnc(c) + rainncv(c)
 snowncv(c) = ps + pice
 graupelncv(c) = pg
 ! MPAS's state sr is microphysics_to_MPAS's (mpas_atmphys_driver_microphysics.F:642): snow (with the ice) plus
 ! graupel over the step's total. The land schemes read this one, not Thompson's SR.
 sr(c) = (snowncv(c) + graupelncv(c)) / (rainncv(c) + 1.e-12)
 snownc(c) = snownc(c) + snowncv(c)
 graupelnc(c) = graupelnc(c) + graupelncv(c)
 ! ---- column -> state (microphysics_to_MPAS)
 do k = 1, nz
  rt_diabatic_tend(k,c) = theta_m2(k,c)
  dtheta_dt_mp(k,c) = theta_m2(k,c)/(1. + rvord*scalars2(i_qv,k,c))
  scalars2(i_qv,k,c) = qv(k); scalars2(i_qc,k,c) = qc(k); scalars2(i_qr,k,c) = qr(k)
  theta_m2(k,c) = theta(k) * (1. + rvord*qv(k))
  rt_diabatic_tend(k,c) = (theta_m2(k,c) - rt_diabatic_tend(k,c)) / dt
  dtheta_dt_mp(k,c) = (theta_m2(k,c)/(1. + rvord*scalars2(i_qv,k,c)) - dtheta_dt_mp(k,c)) / dt
  rtheta_p(k,c) = rho_zz2(k,c) * theta_m2(k,c) - rtheta_base(k,c)
  exner(k,c) = (zz(k,c)*(rgas/p0)*(rtheta_p(k,c) + rtheta_base(k,c)))**rcv
  pressure_p(k,c) = zz(k,c)*rgas*(exner(k,c)*rtheta_p(k,c) + (exner(k,c) - exner_base(k,c))*rtheta_base(k,c))
 end do
 tem1 = zgrid(2,c) - zgrid(1,c)
 tem2 = zgrid(3,c) - zgrid(2,c)
 rho1 = rho_zz2(1,c) * zz(1,c) * (1. + qv(1))
 rho2 = rho_zz2(2,c) * zz(2,c) * (1. + qv(2))
 tend_sfc_pressure(c) = surface_pressure(c)
 surface_pressure(c) = 0.5*gravity*(zgrid(2,c) - zgrid(1,c)) * (rho1 - 0.5*(rho2 - rho1)*tem1/(tem1 + tem2))
 surface_pressure(c) = surface_pressure(c) + pressure_p(1,c) + pressure_base(1,c)
 tend_sfc_pressure(c) = (surface_pressure(c) - tend_sfc_pressure(c)) / dt
 do k = 1, nz
  scalars2(i_qi,k,c) = qi(k); scalars2(i_qs,k,c) = qs(k); scalars2(i_qg,k,c) = qg(k)
  scalars2(i_ni,k,c) = ni(k); scalars2(i_nr,k,c) = nr(k)
 end do
 end subroutine

 ! The Thompson fields and the static arrays Thompson needs go to the card, once, at startup.
 subroutine micro_enter_device(g, mp)
 use micro_pool, only: micro_pool_start
 type(dyn_grid), intent(in) :: g
 type(micro_fields), intent(inout) :: mp
 !$acc enter data copyin(g%m%zgrid, g%m%zz, g%s%pressure_base, g%s%rtheta_base, g%s%exner_base)
 !$acc enter data copyin(mp)
 !$acc enter data copyin(mp%nt_c, mp%surface_pressure, mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv)
 !$acc enter data copyin(mp%graupelnc, mp%graupelncv, mp%sr, mp%dtheta_dt_mp, mp%rainprod, mp%evapprod)
 !$acc enter data copyin(mp%re_cloud, mp%re_ice, mp%re_snow, mp%film_in, mp%precipw)
 call micro_pool_start()   ! mp_thompson's work arrays on the card (src/device/micro_pool.f90)
 end subroutine

 ! The Thompson fields on the card come to the host (inputs of the host physics of the next step, restart, output).
 subroutine micro_to_host(mp)
 type(micro_fields), intent(inout) :: mp
 !$acc update self(mp%nt_c, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv, mp%sr)
 !$acc update self(mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow, mp%surface_pressure, mp%tend_sfc_pressure, mp%dtheta_dt_mp)
 end subroutine

 subroutine micro_outputs_to_host(f, mp)
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 !$acc update self(f%scalars2, f%theta_m2, f%exner, f%rtheta_p, f%pressure_p, f%rt_diabatic_tend)
 !$acc update self(mp%surface_pressure, mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv)
 !$acc update self(mp%sr, mp%dtheta_dt_mp, mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow)
 end subroutine

 subroutine micro_outputs_to_device(f, mp)
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 !$acc update device(f%scalars2, f%theta_m2, f%exner, f%rtheta_p, f%pressure_p, f%rt_diabatic_tend)
 !$acc update device(mp%surface_pressure, mp%tend_sfc_pressure, mp%rainnc, mp%rainncv, mp%snownc, mp%snowncv, mp%graupelnc, mp%graupelncv)
 !$acc update device(mp%sr, mp%dtheta_dt_mp, mp%rainprod, mp%evapprod, mp%re_cloud, mp%re_ice, mp%re_snow)
 end subroutine

 subroutine micro_take(f, mp, s)
 type(dyn_fields), intent(in) :: f
 type(micro_fields), intent(in) :: mp
 type(micro_snapshot), intent(inout) :: s
 if (.not. allocated(s%scalars2)) then
  allocate(s%scalars2, source=f%scalars2); allocate(s%theta_m2, source=f%theta_m2); allocate(s%exner, source=f%exner)
  allocate(s%rtheta_p, source=f%rtheta_p); allocate(s%pressure_p, source=f%pressure_p); allocate(s%rt_diabatic_tend, source=f%rt_diabatic_tend)
  allocate(s%nt_c, source=mp%nt_c); allocate(s%surface_pressure, source=mp%surface_pressure)
  allocate(s%tend_sfc_pressure, source=mp%tend_sfc_pressure); allocate(s%rainnc, source=mp%rainnc)
  allocate(s%rainncv, source=mp%rainncv); allocate(s%snownc, source=mp%snownc); allocate(s%snowncv, source=mp%snowncv)
  allocate(s%graupelnc, source=mp%graupelnc); allocate(s%graupelncv, source=mp%graupelncv); allocate(s%sr, source=mp%sr)
  allocate(s%dtheta_dt_mp, source=mp%dtheta_dt_mp); allocate(s%rainprod, source=mp%rainprod); allocate(s%evapprod, source=mp%evapprod)
  allocate(s%re_cloud, source=mp%re_cloud); allocate(s%re_ice, source=mp%re_ice); allocate(s%re_snow, source=mp%re_snow)
 end if
 s%scalars2 = f%scalars2; s%theta_m2 = f%theta_m2; s%exner = f%exner; s%rtheta_p = f%rtheta_p; s%pressure_p = f%pressure_p
 s%rt_diabatic_tend = f%rt_diabatic_tend
 s%nt_c = mp%nt_c; s%surface_pressure = mp%surface_pressure; s%tend_sfc_pressure = mp%tend_sfc_pressure; s%rainnc = mp%rainnc
 s%rainncv = mp%rainncv; s%snownc = mp%snownc; s%snowncv = mp%snowncv; s%graupelnc = mp%graupelnc; s%graupelncv = mp%graupelncv
 s%sr = mp%sr; s%dtheta_dt_mp = mp%dtheta_dt_mp; s%rainprod = mp%rainprod; s%evapprod = mp%evapprod
 s%re_cloud = mp%re_cloud; s%re_ice = mp%re_ice; s%re_snow = mp%re_snow
 end subroutine

 subroutine micro_put(s, f, mp)
 type(micro_snapshot), intent(in) :: s
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 f%scalars2 = s%scalars2; f%theta_m2 = s%theta_m2; f%exner = s%exner; f%rtheta_p = s%rtheta_p; f%pressure_p = s%pressure_p
 f%rt_diabatic_tend = s%rt_diabatic_tend
 mp%nt_c = s%nt_c; mp%surface_pressure = s%surface_pressure; mp%tend_sfc_pressure = s%tend_sfc_pressure; mp%rainnc = s%rainnc
 mp%rainncv = s%rainncv; mp%snownc = s%snownc; mp%snowncv = s%snowncv; mp%graupelnc = s%graupelnc; mp%graupelncv = s%graupelncv
 mp%sr = s%sr; mp%dtheta_dt_mp = s%dtheta_dt_mp; mp%rainprod = s%rainprod; mp%evapprod = s%evapprod
 mp%re_cloud = s%re_cloud; mp%re_ice = s%re_ice; mp%re_snow = s%re_snow
 end subroutine

 ! Card against host, same inputs: the card result is compared with the host result and the model continues with the host result.
 subroutine micro_check(g, f, mp, it)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 integer, intent(in) :: it
 !$acc update self(f%scalars2, f%theta_m2, f%pressure_p, f%w2, f%exner, f%rho_zz2)
 call micro_take(f, mp, snap_in)
 call microphysics_step(g, f, mp, .true.)
 call micro_outputs_to_host(f, mp)
 call micro_take(f, mp, snap_card)
 call micro_put(snap_in, f, mp)
 call microphysics_step(g, f, mp, .false.)
 call check_open('thompson', it)
 call compare_report('thompson scalars2', f%scalars2, snap_card%scalars2)
 call compare_report('thompson theta_m2', f%theta_m2, snap_card%theta_m2)
 call compare_report('thompson exner', f%exner, snap_card%exner)
 call compare_report('thompson rtheta_p', f%rtheta_p, snap_card%rtheta_p)
 call compare_report('thompson pressure_p', f%pressure_p, snap_card%pressure_p)
 call compare_report('thompson rt_diabatic_tend', f%rt_diabatic_tend, snap_card%rt_diabatic_tend)
 call compare_report('thompson surface_pressure', mp%surface_pressure, snap_card%surface_pressure)
 call compare_report('thompson tend_sfc_pressure', mp%tend_sfc_pressure, snap_card%tend_sfc_pressure)
 call compare_report('thompson rainnc', mp%rainnc, snap_card%rainnc)
 call compare_report('thompson rainncv', mp%rainncv, snap_card%rainncv)
 call compare_report('thompson snownc', mp%snownc, snap_card%snownc)
 call compare_report('thompson snowncv', mp%snowncv, snap_card%snowncv)
 call compare_report('thompson graupelnc', mp%graupelnc, snap_card%graupelnc)
 call compare_report('thompson graupelncv', mp%graupelncv, snap_card%graupelncv)
 call compare_report('thompson sr', mp%sr, snap_card%sr)
 call compare_report('thompson dtheta_dt_mp', mp%dtheta_dt_mp, snap_card%dtheta_dt_mp)
 call compare_report('thompson rainprod', mp%rainprod, snap_card%rainprod)
 call compare_report('thompson evapprod', mp%evapprod, snap_card%evapprod)
 call compare_report('thompson re_cloud', mp%re_cloud, snap_card%re_cloud)
 call compare_report('thompson re_ice', mp%re_ice, snap_card%re_ice)
 call compare_report('thompson re_snow', mp%re_snow, snap_card%re_snow)
 call check_close()
 call micro_outputs_to_device(f, mp)
 end subroutine

 ! One whole model step: the dynamics, then Thompson on the updated state (the order of atm_srk3).
 subroutine model_step(g, f, mp, it)
 type(dyn_grid), intent(in) :: g
 type(dyn_fields), intent(inout) :: f
 type(micro_fields), intent(inout) :: mp
 integer, optional, intent(in) :: it
 real(kind=8) :: clock_t
 call dynamics_step(g, f)
 if (card_thompson) then
  ! Thompson on the card. When the physics inputs are computed on the host (host_inputs), the next step reads these fields
  ! from the host copies.
  call timer_start(clock_t)
  if (host_inputs) then
  !$acc update self(f%u_zonal, f%u_meridional, f%rthdynten, f%rqvdynten)
  end if
  call timer_stop(transfer_timer,clock_t)
  call timer_start(clock_t)
  ! microphysics 'off': not called (mpas_atm_time_integration.F:2806-2817); the negative scalars were clipped before (:2799-2800)
  if (.not. micro_on) then
   continue
  else if (check_thompson) then
   call micro_check(g, f, mp, it)
  else
   call microphysics_step(g, f, mp, .true.)
  end if
  call timer_stop(thompson_timer,clock_t)
  call timer_start(clock_t)
  if (associated(g%lbc)) call end_of_step_boundary(g, f)
  call timer_stop(boundary_timer,clock_t)
  call timer_start(clock_t)
  if (host_inputs) then
  !$acc update self(f%scalars2, f%theta_m2, f%exner, f%pressure_p)
  end if
  call timer_stop(transfer_timer,clock_t)
  return
 end if
 ! Thompson and the physics of the next step run on the host (OpenACC build: the dynamics lives on the card). What they read comes
 ! to the host here, what Thompson wrote goes back, and after the boundary operator the two fields the next physics step reads.
 call timer_start(clock_t)
 !$acc update self(f%scalars2, f%theta_m2, f%pressure_p, f%w2, f%exner, f%rho_zz2, f%u_zonal, f%u_meridional, f%rthdynten, f%rqvdynten)
 call timer_stop(transfer_timer,clock_t)
 call timer_start(clock_t)
 if (micro_on) call microphysics_step(g, f, mp)
 call timer_stop(thompson_timer,clock_t)
 call timer_start(clock_t)
 !$acc update device(f%scalars2, f%theta_m2, f%rt_diabatic_tend, f%rtheta_p, f%exner, f%pressure_p)
 call timer_stop(transfer_timer,clock_t)
 call timer_start(clock_t)
 if (associated(g%lbc)) call end_of_step_boundary(g, f)
 call timer_stop(boundary_timer,clock_t)
 call timer_start(clock_t)
 !$acc update self(f%scalars2, f%theta_m2)
 call timer_stop(transfer_timer,clock_t)
 end subroutine
end module
