! Acoustic (small) steps of the split-explicit scheme and the return to the large-step variables:
! set_smlstep_pert_variables, advance_acoustic_step, divergence_damping and recover_large_step_variables.
! Same arithmetic and summation order as MPAS 8.4.2 (dynamics/mpas_atm_time_integration.F, called "the original"
! below); no halos, no pools. Arrays follow the original layout (level first); the last cell/edge slot is the permanent
! zero column and is never written, with one exception kept from the original: recover sets the density in the
! missing-neighbour cell to 1.
! The CPU's loop order: the original's (one loop over the columns with the column's work private, the two cells' values
! gathered per edge), so that each array is streamed as few times as the bandwidth-bound loops allow; the card's kernels are in
! src/device/dyn_acoustic.f90 (one thread per (level, point), the exner power through double precision).
module dyn_acoustic
 use config
 use dyn_config
 implicit none
contains
 ! Nothing to allocate on the CPU: the acoustic step keeps rs and ts per column, as the original does (the card's form has
 ! per-cell work arrays; this routine keeps the call the start-up shares with it).
 subroutine allocate_acoustic_work()
 end subroutine

 ! Converts the vertical-momentum tendency to a perturbation form that includes the terrain-following slope
 ! terms of the horizontal momentum tendency. Not done in the specified boundary zone.
 subroutine set_smlstep_pert_variables(bdy_mask_cell, degree, edges_on_cell, sign_cell, fzm, fzp, zb_cell, zb3_cell, zz, u_tend, w_tend)
 integer, intent(in) :: bdy_mask_cell(nc1), degree(nc1), edges_on_cell(maxedges,nc1)
 real, intent(in) :: sign_cell(maxedges,nc1), fzm(nz), fzp(nz), zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1)
 real, intent(in) :: zz(nz,nc1), u_tend(nz,ne1)
 real, intent(inout) :: w_tend(nz+1,nc1)
 integer :: c, i, k, e
 real :: flux
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  if (bdy_mask_cell(c) <= n_relax_zone) then
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    do k = 2, nz
     flux = sign_cell(i,c) * (fzm(k) * u_tend(k,e) + fzp(k) * u_tend(k-1,e))
     w_tend(k,c) = w_tend(k,c) - (zb_cell(k,i,c) + sign(1.0, u_tend(k,e)) * zb3_cell(k,i,c)) * flux
    end do
   end do
   do k = 2, nz
    w_tend(k,c) = ( fzm(k) * zz(k,c) + fzp(k) * zz(k-1,c) ) * w_tend(k,c)
   end do
  end if
 end do
 end subroutine

 ! One acoustic step of length dts: horizontal momentum with the pressure gradient of the previous step,
 ! then, column by column, the vertically implicit solve for w (tridiagonal) and the updates of rho_pp and
 ! rtheta_pp. small_step = 1 starts the perturbation variables from zero. In the specified zone only the
 ! tendencies are integrated.
 ! damp_first: the divergence damping of the step before (divergence_damping) is applied first, edge by edge, before the update
 ! (the caller then damps only after the last step of a stage); every ru_p is the same as with the separate pass.
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
 ! the card's per-edge sums (acoustic_edge_sums); not read on the CPU, which gathers theta_m, exner and zz of the two cells
 ! as the original does (an edge array costs more memory traffic than the gathers)
 real, intent(in) :: th_sum(nz,ne1), ex_sum(nz,ne1), zz_sum(nz,ne1)
 integer :: cell1, cell2, e, c, i, k
 real :: c2, rcv, pgrad, flux, coef_divdamp, div_cell1, div_cell2, rdts
 real :: ts(nz), rs(nz)   ! the column's rtheta and rho before the new rw_p (private: each thread has its own call frame)
 rcv = rgas / (cp - rgas)
 c2 = cp * rcv
 rdts = 1.0 / dts   ! as in divergence_damping
 coef_divdamp = 2.0 * smdiv * len_disp * rdts
 if (small_step /= 1) then
  !$omp do schedule(dynamic,64)
  do e = 1, nedges
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   if (damp_first) then
    do k = 1, nz
     div_cell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))
     div_cell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))
     ru_p(k,e) = ru_p(k,e) + coef_divdamp*(div_cell2-div_cell1)*(1.0 - spec_edge(e)) &
                             /(theta_m(k,cell1)+theta_m(k,cell2))
    end do
   end if
   do k = 1, nz
    pgrad = ((rtheta_pp(k,cell2)-rtheta_pp(k,cell1))*inv_dc_edge(e) )/(.5*(zz(k,cell2)+zz(k,cell1)))
    pgrad = cqu(k,e)*0.5*c2*(exner(k,cell1)+exner(k,cell2))*pgrad
    pgrad = pgrad + 0.5*zxu(k,e)*gravity*(rho_pp(k,cell1)+rho_pp(k,cell2))
    ru_p(k,e) = ru_p(k,e) + dts*(tend_ru(k,e) - (1.0 - spec_edge(e))*pgrad)
    ru_avg(k,e) = ru_avg(k,e) + ru_p(k,e)
   end do
  end do
 else
  !$omp do schedule(dynamic,64)
  do e = 1, nedges
   do k = 1, nz
    ru_p(k,e) = dts*tend_ru(k,e)
    ru_avg(k,e) = ru_p(k,e)
   end do
  end do
 end if
 ! The columns, one cell at a time as in the original (rs, ts: the column's private work). The edge loop above has ended (its
 ! barrier), so rtheta_pp_old and the perturbations of a cell can be overwritten here: no edge reads them any more in this step.
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  if (small_step == 1) then
   ! the start of the sequence of acoustic steps: the perturbations are zero
   do k = 1, nz
    rtheta_pp_old(k,c) = 0.0
    ww_avg(k,c) = 0.0
    rho_pp(k,c) = 0.0
    rtheta_pp(k,c) = 0.0
    rw_p(k,c) = 0.0
   end do
   ww_avg(nz+1,c) = 0.0
   rw_p(nz+1,c) = 0.0
  else
   do k = 1, nz
    rtheta_pp_old(k,c) = rtheta_pp(k,c)
   end do
  end if
  if (spec_cell(c) == 0.0) then
   ! rho and rtheta as the horizontal fluxes and the vertical divergence of the old rw_p change them
   do k = 1, nz
    ts(k) = 0.0
    rs(k) = 0.0
   end do
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    do k = 1, nz
     flux = sign_cell(i,c)*dts*dv_edge(e)*ru_p(k,e) * inv_area_cell(c)
     rs(k) = rs(k)-flux
     ts(k) = ts(k)-flux*0.5*(theta_m(k,cell2)+theta_m(k,cell1))
    end do
   end do
   do k = 1, nz
    rs(k) = rho_pp(k,c) + dts*tend_rho(k,c) + rs(k) &
            - dts*cofrz(k)*(ewm(k+1)*rw_p(k+1,c)-ewm(k)*rw_p(k,c))
    ts(k) = rtheta_pp(k,c) + dts*tend_rt(k,c) + ts(k) &
            - dts*rdzw(k)*( ewm(k+1)*coftz(k+1,c)*rw_p(k+1,c) &
                           -ewm(k  )*coftz(k,c)*rw_p(k,c))
   end do
   ! the new rw_p before the implicit solve
   do k = 2, nz
    ww_avg(k,c) = ww_avg(k,c) + ewm(k)*rw_p(k,c)
    rw_p(k,c) = rw_p(k,c) + dts*(tend_rw(k,c) &
                - cofwz(k,c)*(( etp(k  )*zz(k  ,c)*ts(k) &
                               -etp(k-1)*zz(k-1,c)*ts(k-1)) &
                            + ( etm(k  )*zz(k  ,c)*rtheta_pp(k  ,c) &
                               -etm(k-1)*zz(k-1,c)*rtheta_pp(k-1,c))) &
                - cofwr(k,c)*((etp(k)*rs(k)+etp(k-1)*rs(k-1)) &
                             +( etm(k  )*rho_pp(k  ,c) &
                               +etm(k-1)*rho_pp(k-1,c))) &
                + cofwt(k  ,c)*(etp(k  )*ts(k  )+etm(k  )*rtheta_pp(k  ,c)) &
                + cofwt(k-1,c)*(etp(k-1)*ts(k-1)+etm(k-1)*rtheta_pp(k-1,c)))
   end do
   ! tridiagonal solve: forward elimination, then back substitution
   do k = 2, nz
    rw_p(k,c) = (rw_p(k,c)-(dts**2)*a_tri(k,c)*rw_p(k-1,c))*alpha_tri(k,c)
   end do
   do k = nz, 1, -1
    rw_p(k,c) = rw_p(k,c) - gamma_tri(k,c)*rw_p(k+1,c)
   end do
   ! Rayleigh damping of w in the upper layers (dss)
   do k = 2, nz
    rw_p(k,c) = (rw_p(k,c) + (rw_save(k  ,c) - rw(k  ,c)) -dts*dss(k,c)* &
                (fzm(k)*zz (k,c)+fzp(k)*zz (k-1,c)) &
                *(fzm(k)*rho_zz(k,c)+fzp(k)*rho_zz(k-1,c)) &
                *w(k,c)    )/(1.0+dts*dss(k,c)) &
                - (rw_save(k  ,c) - rw(k  ,c))
    ww_avg(k,c) = ww_avg(k,c) + ewp(k)*rw_p(k,c)
   end do
   ! the new rho_pp and rtheta_pp from the new rw_p
   do k = 1, nz
    rho_pp(k,c) = rs(k) - dts*cofrz(k) *( ewp(k+1)*rw_p(k+1,c) &
                                         -ewp(k  )*rw_p(k  ,c))
    rtheta_pp(k,c) = ts(k) - dts*rdzw(k)*( ewp(k+1)*coftz(k+1,c)*rw_p(k+1,c) &
                                          -ewp(k  )*coftz(k  ,c)*rw_p(k  ,c))
   end do
  else
   ! the specified zone: only the tendencies are integrated
   do k = 1, nz
    rho_pp(k,c) = rho_pp(k,c) + dts*tend_rho(k,c)
    rtheta_pp(k,c) = rtheta_pp(k,c) + dts*tend_rt(k,c)
    rw_p(k,c) = rw_p(k,c) + dts*tend_rw(k,c)
    ww_avg(k,c) = ww_avg(k,c) + ewp(k)*rw_p(k,c)
   end do
  end if
 end do
 end subroutine

 ! The card forms per-edge sums of theta_m, exner and zz once per stage (src/device/dyn_acoustic.f90); the CPU gathers the two
 ! cells' values in the acoustic step instead, as MPAS does. Here only the barrier that ends the card version's edge loop is
 ! kept, so both builds synchronise at the same point (the shared driver calls it between the boundary tendencies and the
 ! first acoustic step).
 subroutine acoustic_edge_sums(theta_m, exner, zz, cells_on_edge, th_sum, ex_sum, zz_sum)
 real, intent(in) :: theta_m(nz,nc1), exner(nz,nc1), zz(nz,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(inout) :: th_sum(nz,ne1), ex_sum(nz,ne1), zz_sum(nz,ne1)
 !$omp barrier
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
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   div_cell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))
   div_cell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))
   ru_p(k,e) = ru_p(k,e) + coef_divdamp*(div_cell2-div_cell1)*(1.0 - spec_edge(e)) &
                           /(theta_m(k,cell1)+theta_m(k,cell2))
  end do
 end do
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
 real :: inv_ns, rcv, p0, flux
 rcv = rgas/(cp-rgas)
 p0 = 1.0e+05
 !$omp single
 do k = 1, nz
  rho_zz(k,nc1) = 1.0
 end do
 !$omp end single
 inv_ns = 1 / real(ns)
 ! one loop over the columns: density, w from rw, then rtheta_p and theta_m (and at stage 3 the Exner function and the
 ! pressure perturbation, in one expression each as in the original) from the column's new density
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   rho_p(k,c) = rho_p_save(k,c) + rho_pp(k,c)
   rho_zz(k,c) = rho_p(k,c) + rho_base(k,c)
  end do
  rw(1,c) = 0.0
  w(1,c) = 0.0
  do k = 2, nz
   ww_avg(k,c) = rw_save(k,c) + (ww_avg(k,c) * inv_ns)
   rw(k,c) = rw_save(k,c) + rw_p(k,c)
   w(k,c) = rw(k,c)/(fzm(k)*zz(k,c)+fzp(k)*zz(k-1,c))
  end do
  rw(nz+1,c) = 0.0
  w(nz+1,c) = 0.0
  if (rk_step == 3) then
   do k = 1, nz
    rtheta_p(k,c) = rtheta_p_save(k,c) + rtheta_pp(k,c) - dt * rho_zz(k,c) * rt_diabatic_tend(k,c)
    theta_m(k,c) = (rtheta_p(k,c) + rtheta_base(k,c))/rho_zz(k,c)
    exner(k,c) = (zz(k,c)*(rgas/p0)*(rtheta_p(k,c)+rtheta_base(k,c)))**rcv
    pressure_p(k,c) = zz(k,c) * rgas * (exner(k,c)*rtheta_p(k,c)+rtheta_base(k,c) * (exner(k,c)-exner_base(k,c)))
   end do
  else
   do k = 1, nz
    rtheta_p(k,c) = rtheta_p_save(k,c) + rtheta_pp(k,c)
    theta_m(k,c) = (rtheta_p(k,c) + rtheta_base(k,c))/rho_zz(k,c)
   end do
  end if
 end do
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   ru_avg(k,e) = ru_save(k,e) + (ru_avg(k,e) * inv_ns)
   ru(k,e) = ru_save(k,e) + ru_p(k,e)
   u(k,e) = 2.*ru(k,e)/(rho_zz(k,cell1)+rho_zz(k,cell2))
  end do
 end do
 ! w from the slope terms of the horizontal momentum (not in the specified zone)
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  if (bdy_mask_cell(c) <= n_relax_zone) then
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    flux = (cf1*ru(1,e) + cf2*ru(2,e) + cf3*ru(3,e))
    w(1,c) = w(1,c) + sign_cell(i,c) * (zb_cell(1,i,c) + sign(1.0,flux)*zb3_cell(1,i,c))*flux
    do k = 2, nz
     flux = (fzm(k)*ru(k,e)+fzp(k)*ru(k-1,e))
     w(k,c) = w(k,c) + sign_cell(i,c) * (zb_cell(k,i,c)+sign(1.0,flux)*zb3_cell(k,i,c))*flux
    end do
   end do
   w(1,c) = w(1,c)/(cf1*rho_zz(1,c)+cf2*rho_zz(2,c)+cf3*rho_zz(3,c))
   do k = 2, nz
    w(k,c) = w(k,c)/(fzm(k)*rho_zz(k,c)+fzp(k)*rho_zz(k-1,c))
   end do
  end if
 end do
 end subroutine
end module
