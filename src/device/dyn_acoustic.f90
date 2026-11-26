! HAND-WRITTEN card version of src/dyn_acoustic.f90 (not generated; ./lean generate --check does not see it). The card build
! compiles it in place of src/dyn_acoustic.f90 (config/units.json device_overrides); a change to a dynamics kernel is made in both files.
! Acoustic (small) steps of the split-explicit scheme and the return to the large-step variables:
! set_smlstep_pert_variables, advance_acoustic_step, divergence_damping and recover_large_step_variables.
! Same arithmetic and summation order as MPAS 8.4.2 (dynamics/mpas_atm_time_integration.F, "the original" below); no halos, no pools. Arrays follow the original layout
! (level first); the last cell/edge slot is the permanent zero column and is never written, with one exception
! kept from the original: recover sets the density in the missing-neighbour cell to 1.
module dyn_acoustic
 use config
 use dyn_config
 implicit none
 ! per-column results of the acoustic step that live between its kernels (rs, ts of the original); on the card from the start
 real, allocatable, save :: ts_work(:,:), rs_work(:,:)
 !$acc declare create(ts_work, rs_work)
 ! exner power, exact variant: the cells whose card result is too close to a rounding midpoint to be trusted go to the host library
 integer, allocatable, save :: flag_index(:)
 real, allocatable, save :: flag_arg(:), flag_val(:)
 !$acc declare create(flag_index, flag_arg, flag_val)
contains
 subroutine allocate_acoustic_work()
 allocate(ts_work(nz,nc1))
 allocate(rs_work(nz,nc1))
 allocate(flag_index(nz*nc1))
 allocate(flag_arg(nz*nc1))
 allocate(flag_val(nz*nc1))
 ts_work = 0.; rs_work = 0.; flag_index = 0; flag_arg = 0.; flag_val = 0.
 end subroutine

 ! Converts the vertical-momentum tendency to a perturbation form that includes the terrain-following slope
 ! terms of the horizontal momentum tendency. Not done in the specified boundary zone.
 subroutine set_smlstep_pert_variables(bdy_mask_cell, degree, edges_on_cell, sign_cell, fzm, fzp, zb_cell, zb3_cell, zz, u_tend, w_tend)
 integer, intent(in) :: bdy_mask_cell(nc1), degree(nc1), edges_on_cell(maxedges,nc1)
 real, intent(in) :: sign_cell(maxedges,nc1), fzm(nz), fzp(nz), zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1)
 real, intent(in) :: zz(nz,nc1), u_tend(nz,ne1)
 real, intent(inout) :: w_tend(nz+1,nc1)
 integer :: c, i, k, e
 real :: flux, wt
 ! one thread per (level, cell); the edges in the original order, summed in a register
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, flux, wt) async(1)
 do c = 1, ncells
  do k = 2, nz
   if (bdy_mask_cell(c) <= n_relax_zone) then
    wt = w_tend(k,c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     flux = sign_cell(i,c) * (fzm(k) * u_tend(k,e) + fzp(k) * u_tend(k-1,e))
     wt = wt - (zb_cell(k,i,c) + sign(1.0, u_tend(k,e)) * zb3_cell(k,i,c)) * flux
    end do
    w_tend(k,c) = ( fzm(k) * zz(k,c) + fzp(k) * zz(k-1,c) ) * wt
   end if
  end do
 end do
 end subroutine

 ! One acoustic step of length dts: horizontal momentum with the pressure gradient of the previous step,
 ! then, column by column, the vertically implicit solve for w (tridiagonal) and the updates of rho_pp and
 ! rtheta_pp. small_step = 1 starts the perturbation variables from zero. In the specified zone only the
 ! tendencies are integrated.
 ! damp_first: the divergence damping of the step before (divergence_damping) is applied in the first kernel, before the update
 ! (the caller then damps only after the last step of a stage); every ru_p is the same as with the separate kernel.
 subroutine advance_acoustic_step(small_step, damp_first, dts, rho_zz, theta_m, w, rw, rw_save, exner, cqu, zz, zxu, fzm, fzp, rdzw, &
                                  inv_dc_edge, inv_area_cell, dv_edge, degree, cells_on_edge, edges_on_cell, sign_cell, &
                                  spec_edge, spec_cell, dss, etp, etm, ewp, ewm, cofwt, coftz, cofrz, cofwr, cofwz, &
                                  a_tri, alpha_tri, gamma_tri, tend_ru, tend_rho, tend_rt, tend_rw, &
                                  ru_p, rw_p, rtheta_pp, rtheta_pp_old, rho_pp, ru_avg, ww_avg, th_sum, ex_sum, zz_sum)
 integer, intent(in) :: small_step
 logical, intent(in) :: damp_first
 real, intent(in) :: dts
 real, intent(in) :: rho_zz(nz,nc1), theta_m(nz,nc1), w(nz+1,nc1), rw(nz+1,nc1), rw_save(nz+1,nc1), exner(nz,nc1)
 real, intent(in) :: cqu(nz,ne1), zz(nz,nc1), zxu(nz,ne1), fzm(nz), fzp(nz), rdzw(nz), inv_dc_edge(ne1), inv_area_cell(nc1)
 real, intent(in) :: dv_edge(ne1), sign_cell(maxedges,nc1), spec_edge(ne1), spec_cell(nc1), dss(nz,nc1)
 real, intent(in) :: etp(nz), etm(nz), ewp(nz+1), ewm(nz+1)
 integer, intent(in) :: degree(nc1), cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1)
 real, intent(in) :: cofwt(nz,nc1), coftz(nz+1,nc1), cofrz(nz), cofwr(nz,nc1), cofwz(nz,nc1)
 real, intent(in) :: a_tri(nz,nc1), alpha_tri(nz,nc1), gamma_tri(nz,nc1)
 real, intent(in) :: tend_ru(nz,ne1), tend_rho(nz,nc1), tend_rt(nz,nc1), tend_rw(nz+1,nc1)
 real, intent(inout) :: ru_p(nz,ne1), rw_p(nz+1,nc1), rtheta_pp(nz,nc1), rtheta_pp_old(nz,nc1), rho_pp(nz,nc1)
 real, intent(inout) :: ru_avg(nz,ne1), ww_avg(nz+1,nc1)
 ! the per-edge sums of theta_m, exner and zz of the stage (acoustic_edge_sums: the same additions)
 real, intent(in) :: th_sum(nz,ne1), ex_sum(nz,ne1), zz_sum(nz,ne1)
 integer :: cell1, cell2, e, c, i, k
 real :: c2, rcv, pgrad, flux, rs, ts, rpp, tpp, rwk, rwk1, rup, coef_divdamp, div_cell1, div_cell2, rdts
 rcv = rgas / (cp - rgas)
 c2 = cp * rcv
 rdts = 1.0 / dts   ! as in divergence_damping
 coef_divdamp = 2.0 * smdiv * len_disp * rdts
 ! edges, one thread per (level, edge): every lane busy (a 64-lane gang per edge left 19 of 64 idle with 45 levels)
 if (small_step /= 1) then
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, pgrad, rup, div_cell1, div_cell2) async(1)
  do e = 1, nedges
   do k = 1, nz
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    rup = ru_p(k,e)
    if (damp_first) then
     div_cell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))
     div_cell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))
     rup = rup + coef_divdamp*(div_cell2-div_cell1)*(1.0 - spec_edge(e)) &
                 /th_sum(k,e)
    end if
    pgrad = ((rtheta_pp(k,cell2)-rtheta_pp(k,cell1))*inv_dc_edge(e) )/(.5*zz_sum(k,e))
    pgrad = cqu(k,e)*0.5*c2*ex_sum(k,e)*pgrad
    pgrad = pgrad + 0.5*zxu(k,e)*gravity*(rho_pp(k,cell1)+rho_pp(k,cell2))
    rup = rup + dts*(tend_ru(k,e) - (1.0 - spec_edge(e))*pgrad)
    ru_p(k,e) = rup
    ru_avg(k,e) = ru_avg(k,e) + rup
   end do
  end do
 else
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do e = 1, nedges
   do k = 1, nz
    ru_p(k,e) = dts*tend_ru(k,e)
    ru_avg(k,e) = ru_p(k,e)
   end do
  end do
 end if
 ! The columns in four kernels, one thread per (level, cell) except the solve (one thread per column): the saved
 ! rtheta_pp, the start of the sequence of acoustic steps (the perturbations are zero), rho and rtheta as the horizontal fluxes and the
 ! vertical divergence of the old rw_p change them (rs, ts: kept per column in the scratch arrays until the new rw_p is known); the new
 ! rw_p before the implicit solve; the solve; the damping and the new rho_pp, rtheta_pp. Each value is computed as in the original:
 ! a level reads the old rw_p of the level above, which no thread writes in that kernel except at the first acoustic step,
 ! where the original read the zero it had just written; divergence_damping, the next reader of rtheta_pp_old, follows.
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, cell1, cell2, flux, rs, ts, rpp, tpp, rwk, rwk1) async(1)
 do c = 1, ncells
  do k = 1, nz
   if (small_step == 1) then
    rtheta_pp_old(k,c) = 0.0
    ww_avg(k,c) = 0.0
    rho_pp(k,c) = 0.0
    rtheta_pp(k,c) = 0.0
    rw_p(k,c) = 0.0
    if (k == nz) then
     ww_avg(nz+1,c) = 0.0
     rw_p(nz+1,c) = 0.0
    end if
    rpp = 0.0
    tpp = 0.0
    rwk = 0.0
    rwk1 = 0.0
   else
    rtheta_pp_old(k,c) = rtheta_pp(k,c)
    rpp = rho_pp(k,c)
    tpp = rtheta_pp(k,c)
    rwk = rw_p(k,c)
    rwk1 = rw_p(k+1,c)
   end if
   if (spec_cell(c) == 0.0) then
    ts = 0.0
    rs = 0.0
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     flux = sign_cell(i,c)*dts*dv_edge(e)*ru_p(k,e) * inv_area_cell(c)
     rs = rs-flux
     ts = ts-flux*0.5*th_sum(k,e)   ! theta_m(cell2)+theta_m(cell1), the same sum
    end do
    rs_work(k,c) = rpp + dts*tend_rho(k,c) + rs &
            - dts*cofrz(k)*(ewm(k+1)*rwk1-ewm(k)*rwk)
    ts_work(k,c) = tpp + dts*tend_rt(k,c) + ts &
            - dts*rdzw(k)*( ewm(k+1)*coftz(k+1,c)*rwk1 &
                           -ewm(k  )*coftz(k,c)*rwk)
   end if
  end do
 end do
 ! the new rw_p before the implicit solve
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 2, nz
   if (spec_cell(c) == 0.0) then
    ww_avg(k,c) = ww_avg(k,c) + ewm(k)*rw_p(k,c)
    rw_p(k,c) = rw_p(k,c) + dts*(tend_rw(k,c) &
                - cofwz(k,c)*(( etp(k  )*zz(k  ,c)*ts_work(k,c) &
                               -etp(k-1)*zz(k-1,c)*ts_work(k-1,c)) &
                            + ( etm(k  )*zz(k  ,c)*rtheta_pp(k  ,c) &
                               -etm(k-1)*zz(k-1,c)*rtheta_pp(k-1,c))) &
                - cofwr(k,c)*((etp(k)*rs_work(k,c)+etp(k-1)*rs_work(k-1,c)) &
                             +( etm(k  )*rho_pp(k  ,c) &
                               +etm(k-1)*rho_pp(k-1,c))) &
                + cofwt(k  ,c)*(etp(k  )*ts_work(k  ,c)+etm(k  )*rtheta_pp(k  ,c)) &
                + cofwt(k-1,c)*(etp(k-1)*ts_work(k-1,c)+etm(k-1)*rtheta_pp(k-1,c)))
   end if
  end do
 end do
 ! tridiagonal solve: forward elimination, then back substitution (each level needs its neighbour: one thread per column, in a
 ! kernel of its own; inside a (level, cell) kernel the solve would run on one lane of a 64-lane gang, and the card would hold too
 ! few columns at a time to hide the latency)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector default(present) async(1)
 do c = 1, ncells
  if (spec_cell(c) == 0.0) then
   do k = 2, nz
    rw_p(k,c) = (rw_p(k,c)-(dts**2)*a_tri(k,c)*rw_p(k-1,c))*alpha_tri(k,c)
   end do
   do k = nz, 1, -1
    rw_p(k,c) = rw_p(k,c) - gamma_tri(k,c)*rw_p(k+1,c)
   end do
  end if
 end do
 ! the Rayleigh damping of w in the upper layers (dss), then the new rho_pp and rtheta_pp from the new rw_p; in the specified zone only
 ! the tendencies are integrated (one thread per (level, cell) in both)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 2, nz
   if (spec_cell(c) == 0.0) then
    rw_p(k,c) = (rw_p(k,c) + (rw_save(k  ,c) - rw(k  ,c)) -dts*dss(k,c)* &
                (fzm(k)*zz (k,c)+fzp(k)*zz (k-1,c)) &
                *(fzm(k)*rho_zz(k,c)+fzp(k)*rho_zz(k-1,c)) &
                *w(k,c)    )/(1.0+dts*dss(k,c)) &
                - (rw_save(k  ,c) - rw(k  ,c))
    ww_avg(k,c) = ww_avg(k,c) + ewp(k)*rw_p(k,c)
   end if
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   if (spec_cell(c) == 0.0) then
    rho_pp(k,c) = rs_work(k,c) - dts*cofrz(k) *( ewp(k+1)*rw_p(k+1,c) &
                                         -ewp(k  )*rw_p(k  ,c))
    rtheta_pp(k,c) = ts_work(k,c) - dts*rdzw(k)*( ewp(k+1)*coftz(k+1,c)*rw_p(k+1,c) &
                                          -ewp(k  )*coftz(k  ,c)*rw_p(k  ,c))
   else
    rho_pp(k,c) = rho_pp(k,c) + dts*tend_rho(k,c)
    rtheta_pp(k,c) = rtheta_pp(k,c) + dts*tend_rt(k,c)
    rw_p(k,c) = rw_p(k,c) + dts*tend_rw(k,c)
    ww_avg(k,c) = ww_avg(k,c) + ewp(k)*rw_p(k,c)
   end if
  end do
 end do
 end subroutine

 ! The per-edge sums the acoustic steps of a stage read (theta_m and exner of the two cells, zz of the two cells): constant during the
 ! acoustic steps (theta_m is the step's first time level, exner and zz change only in recover), so formed once per stage. Each
 ! is the addition the acoustic kernels made (the column kernel's theta_m(cell2)+theta_m(cell1) is the same sum). The real edges:
 ! the kernels read no other (the ghost edge's entries are not read).
 subroutine acoustic_edge_sums(theta_m, exner, zz, cells_on_edge, th_sum, ex_sum, zz_sum)
 real, intent(in) :: theta_m(nz,nc1), exner(nz,nc1), zz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(inout) :: th_sum(nz,ne1), ex_sum(nz,ne1), zz_sum(nz,ne1)
 integer :: e, k, cell1, cell2
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   th_sum(k,e) = theta_m(k,cell1)+theta_m(k,cell2)
   ex_sum(k,e) = exner(k,cell1)+exner(k,cell2)
   zz_sum(k,e) = zz(k,cell2)+zz(k,cell1)
  end do
 end do
 end subroutine

 ! Damps the divergence of the acoustic mode: pushes ru_p along the change of rtheta_pp during the last step.
 subroutine divergence_damping(dts, theta_m, rtheta_pp, rtheta_pp_old, cells_on_edge, spec_edge, ru_p, th_sum)
 real, intent(in) :: dts, theta_m(nz,nc1), rtheta_pp(nz,nc1), rtheta_pp_old(nz,nc1), spec_edge(ne1), th_sum(nz,ne1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(inout) :: ru_p(nz,ne1)
 integer :: e, k, cell1, cell2
 real :: div_cell1, div_cell2, rdts, coef_divdamp
 rdts = 1.0 / dts
 coef_divdamp = 2.0 * smdiv * len_disp * rdts
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   div_cell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))
   div_cell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))
   ru_p(k,e) = ru_p(k,e) + coef_divdamp*(div_cell2-div_cell1)*(1.0 - spec_edge(e)) &
                           /th_sum(k,e)
  end do
 end do
 end subroutine

 ! exner = argument**rcv, for the cells (the ghost column is left alone). The power is the one operation in the dynamics whose result on the
 ! card differs from the host library: measured with nvfortran 25.3 on the test card (x86_64 Linux host), the card's single-precision
 ! power is off by one unit in the last place for 6 % of the arguments. Two branches, chosen when compiling: the host build uses the
 ! original expression; the card build defines EXNER_EXACT (config/flags.json) and computes the power in double precision, sending
 ! the arguments within 0.01 spacings of a rounding midpoint to the host library, so the result equals the host power.
 subroutine exner_power(exner, rcv)
 real, intent(inout) :: exner(nz,nc1)
 real, intent(in) :: rcv
 integer :: c, k
#if defined(_OPENACC) && !defined(EXNER_EXACT)
#error "the card build of exner_power needs EXNER_EXACT (config/flags.json)"
#endif
#if defined(_OPENACC) && defined(EXNER_EXACT)
 integer :: nflag, n, bits
 real :: x, f0, lo, hi
 real(8) :: d, dist

 ! The host library returns the correctly rounded value except when that value lies within 0.0016 float spacings of a rounding midpoint
 ! (measured on the same machine, the host math library of the nvfortran 25.3 build, over 2e7 arguments of this range against the
 ! correctly rounded double-precision value). Every argument within 0.01 spacings of a midpoint (2 % of them) is
 ! evaluated by the host library, the rest by double precision on the card: the result equals the host power for every argument.
 nflag = 0
 ! the kernels before this one run on queue 1; this block runs on the synchronous queue (the count comes back to the host)
 !$acc wait(1)
 !$acc data copy(nflag)
 !$acc parallel loop gang vector collapse(2) default(present)
 do c = 1, ncells
  do k = 1, nz
   x = exner(k,c)
   ! exp(rcv log x) in double instead of the double power: about half the double-precision work (the card does double at 1/64
   ! of the single rate). Its error, a few units of 1e-16, is 1e-8 of a float spacing: every value that is not flagged is the
   ! correctly rounded one either way, and the flagged ones take the host library's value either way.
   d = exp(real(rcv, 8)*log(real(x, 8)))
   f0 = real(d, 4)
   bits = transfer(f0, 0)
   if (real(f0, 8) > d) then
    lo = transfer(bits - 1, 1.0); hi = f0
   else
    lo = f0; hi = transfer(bits + 1, 1.0)
   end if
   dist = abs(d - 0.5d0*(real(lo, 8) + real(hi, 8))) / (real(hi, 8) - real(lo, 8))
   exner(k,c) = f0
   if (dist < 0.01d0) then
    !$acc atomic capture
    nflag = nflag + 1
    n = nflag
    !$acc end atomic
    flag_index(n) = k + nz*(c-1)
    flag_arg(n) = x
   end if
  end do
 end do
 !$acc end data
 if (nflag > 0) then
  !$acc update self(flag_arg(1:nflag))
  do n = 1, nflag
   flag_val(n) = flag_arg(n)**rcv
  end do
  !$acc update device(flag_val(1:nflag))
  !$acc parallel loop gang vector default(present)
  do n = 1, nflag
   exner(mod(flag_index(n)-1, nz) + 1, (flag_index(n)-1)/nz + 1) = flag_val(n)
  end do
 end if
#else
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   exner(k,c) = exner(k,c)**rcv
  end do
 end do
#endif
 end subroutine

 ! After the acoustic steps of one Runge-Kutta stage: add the perturbations to the saved large-step values and
 ! rebuild density, w, rtheta, theta_m, u and (stage 3) the Exner function and pressure perturbation; then w is
 ! corrected for the terrain slope. ns is the number of acoustic steps, dt the stage length.
 subroutine recover_large_step_variables(rk_step, ns, dt, bdy_mask_cell, degree, edges_on_cell, cells_on_edge, sign_cell, &
                                         fzm, fzp, cf1, cf2, cf3, zb_cell, zb3_cell, zz, rho_base, rtheta_base, exner_base, &
                                         rt_diabatic_tend, rho_p_save, rtheta_p_save, rw_save, ru_save, rho_pp, rtheta_pp, &
                                         rw_p, ru_p, rho_p, rho_zz, rw, w, rtheta_p, theta_m, exner, pressure_p, ru, u, &
                                         ww_avg, ru_avg)
 integer, intent(in) :: rk_step, ns
 real, intent(in) :: dt
 integer, intent(in) :: bdy_mask_cell(nc1), degree(nc1), edges_on_cell(maxedges,nc1), cells_on_edge(2,ne1)
 real, intent(in) :: sign_cell(maxedges,nc1), fzm(nz), fzp(nz), cf1, cf2, cf3, zb_cell(nz+1,maxedges,nc1)
 real, intent(in) :: zb3_cell(nz+1,maxedges,nc1), zz(nz,nc1), rho_base(nz,nc1), rtheta_base(nz,nc1), exner_base(nz,nc1)
 real, intent(in) :: rt_diabatic_tend(nz,nc1), rho_p_save(nz,nc1), rtheta_p_save(nz,nc1), rw_save(nz+1,nc1), ru_save(nz,ne1)
 real, intent(in) :: rho_pp(nz,nc1), rtheta_pp(nz,nc1), rw_p(nz+1,nc1), ru_p(nz,ne1)
 real, intent(inout) :: rho_p(nz,nc1), rho_zz(nz,nc1), rw(nz+1,nc1), w(nz+1,nc1), rtheta_p(nz,nc1), theta_m(nz,nc1)
 real, intent(inout) :: exner(nz,nc1), pressure_p(nz,nc1), ru(nz,ne1), u(nz,ne1), ww_avg(nz+1,nc1), ru_avg(nz,ne1)
 integer :: i, c, e, k, cell1, cell2
 real :: inv_ns, rcv, p0, flux, wk
 rcv = rgas/(cp-rgas)
 p0 = 1.0e+05
 !$omp single
 !$acc parallel loop gang vector default(present) firstprivate(nc1) async(1)
 do k = 1, nz
  rho_zz(k,nc1) = 1.0
 end do
 !$omp end single
 inv_ns = 1 / real(ns)
 ! columns: density, w and rtheta_p, theta_m (and at stage 3 the argument of the Exner power), one kernel, one thread per (level, cell)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz+1
   if (k <= nz) then
    rho_p(k,c) = rho_p_save(k,c) + rho_pp(k,c)
    rho_zz(k,c) = rho_p(k,c) + rho_base(k,c)
    if (rk_step == 3) then
     rtheta_p(k,c) = rtheta_p_save(k,c) + rtheta_pp(k,c) - dt * rho_zz(k,c) * rt_diabatic_tend(k,c)
     theta_m(k,c) = (rtheta_p(k,c) + rtheta_base(k,c))/rho_zz(k,c)
     exner(k,c) = zz(k,c)*(rgas/p0)*(rtheta_p(k,c)+rtheta_base(k,c))
    else
     rtheta_p(k,c) = rtheta_p_save(k,c) + rtheta_pp(k,c)
     theta_m(k,c) = (rtheta_p(k,c) + rtheta_base(k,c))/rho_zz(k,c)
    end if
   end if
   if (k == 1 .or. k == nz+1) then
    rw(k,c) = 0.0
    w(k,c) = 0.0
   else
    ww_avg(k,c) = rw_save(k,c) + (ww_avg(k,c) * inv_ns)
    rw(k,c) = rw_save(k,c) + rw_p(k,c)
    w(k,c) = rw(k,c)/(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c))
   end if
  end do
 end do
 if (rk_step == 3) call exner_power(exner, rcv)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2) async(1)
 do e = 1, nedges
  do k = 1, nz
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   ru_avg(k,e) = ru_save(k,e) + (ru_avg(k,e) * inv_ns)
   ru(k,e) = ru_save(k,e) + ru_p(k,e)
   u(k,e) = 2.*ru(k,e)/(rho_zz(k,cell1)+rho_zz(k,cell2))
  end do
 end do
 ! columns: the pressure perturbation (stage 3), and w from the slope terms of the horizontal momentum (not in the specified zone).
 ! One thread per (level, cell): every level sums its edges in the original order, so the reads are not strided by level.
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) private(i, e, flux, wk) async(1)
 do c = 1, ncells
  do k = 1, nz
   if (rk_step == 3) then
    pressure_p(k,c) = zz(k,c) * rgas * (exner(k,c)*rtheta_p(k,c)+rtheta_base(k,c) * (exner(k,c)-exner_base(k,c)))
   end if
   if (bdy_mask_cell(c) <= n_relax_zone) then
    wk = w(k,c)
    if (k == 1) then
     do i = 1, degree(c)
      e = edges_on_cell(i,c)
      flux = (cf1*ru(1,e) + cf2*ru(2,e) + cf3*ru(3,e))
      wk = wk + sign_cell(i,c) * (zb_cell(1,i,c) + sign(1.0,flux)*zb3_cell(1,i,c))*flux
     end do
     w(1,c) = wk/(cf1*rho_zz(1,c)+cf2*rho_zz(2,c)+cf3*rho_zz(3,c))
    else
     do i = 1, degree(c)
      e = edges_on_cell(i,c)
      flux = (fzm(k)*ru(k,e)+fzp(k)*ru(k-1,e))
      wk = wk + sign_cell(i,c) * (zb_cell(k,i,c)+sign(1.0,flux)*zb3_cell(k,i,c))*flux
     end do
     w(k,c) = wk/(fzm(k)*rho_zz(k,c)+fzp(k)*rho_zz(k-1,c))
    end if
   end if
  end do
 end do
 end subroutine
end module
