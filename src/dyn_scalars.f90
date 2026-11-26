! Split scalar transport (atm_advance_scalars_work and atm_advance_scalars_mono_work of MPAS 8.4.2, "the original" below)
! for the configuration lean_mpas supports:
! order-2 Runge-Kutta in three stages with the mass fluxes averaged over the dynamics substeps, stages 1 and 2
! non-monotonic (3rd-order horizontal, vertical flux3), stage 3 monotonic (flux-corrected, limiter from the
! min/max of the old values around the cell). Density is advanced inside the transport (advance_density true).
! Same arithmetic and summation order as the original; no halos, no pools. Arrays follow the original layout
! (scalar index first in the state, level first in the work arrays); the last cell/edge slot is the permanent
! zero column. Regional: cells with bdy_mask_cell > n_relax_zone (specified zone) are not touched by the
! non-monotonic update, and the monotonic one copies back only the cells with mask <= n_spec_zone, as the original
! does (see the LEAN_MONO_RELAX note in mono_scalars).
! The CPU's loop order (neighbour loop outside, level and scalar loops inside, one scalar at a time in the monotonic
! transport, through per-scalar copies taken from and written back to the state in one pass each); the card's kernels are
! in src/device/dyn_scalars.f90.
module dyn_scalars
 use config
 use dyn_config
 implicit none
 ! A work buffer of num_scalars edge fields, shared by users that never live at the same time: on the host the monotonic transport's
 ! per-scalar copies (advance_scalars_mono) and adjust_scalars' relaxation values (acoustic_edge_sums receives it but writes
 ! nothing on the CPU; the card has more users of it, see src/device/dyn_scalars.f90). Every user writes what
 ! it reads; allocated by the first edge_work_ghosts call.
 real, allocatable, save :: edge_work(:)
contains
 ! Allocates the edge work buffer at the first call. views names the card's views whose ghost edge src/device/dyn_scalars.f90
 ! zeroes; the host calls it with views < 0 only (its users write every entry they read), so here it only allocates. Every
 ! thread of the step's team calls it: one allocates, the others wait at the end of the single block.
 subroutine edge_work_ghosts(views)
 integer, intent(in) :: views
 !$omp single
 if (.not. allocated(edge_work)) then
  allocate(edge_work(num_scalars*nz*ne1))
  edge_work = 0.0
 end if
 !$omp end single
 if (views >= 0) error stop 'edge_work_ghosts: ghost zeroing is card only (src/device/dyn_scalars.f90)'
 end subroutine

 ! Stage 1 or 2. scalar_new holds the previous stage result (the start state at stage 1) and is overwritten for the
 ! cells outside the specified zone; scalar_old, the densities, the mass fluxes and the tendency are read.
 ! horiz_flux is a work array (scalar value at each edge), its ghost edge must be zero.
 subroutine advance_scalars(rk_step, scalar_old, scalar_new, rho_zz_old, rho_zz_new, uh_avg, ww_avg, scalar_tend, &
                            bdy_mask_cell, bdy_mask_edge, cells_on_edge, edges_on_cell, degree, sign_cell, inv_area_cell, dv_edge, &
                            n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, horiz_flux)
 integer, intent(in) :: rk_step
 real, intent(in) :: scalar_old(num_scalars,nz,nc1), rho_zz_old(nz,nc1), rho_zz_new(nz,nc1), uh_avg(nz,ne1), ww_avg(nz+1,nc1)
 real, intent(in) :: scalar_tend(num_scalars,nz,nc1), sign_cell(maxedges,nc1), inv_area_cell(nc1), dv_edge(ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), fzm(nz), fzp(nz), rdzw(nz)
 integer, intent(in) :: bdy_mask_cell(nc1), bdy_mask_edge(ne1), cells_on_edge(2,ne1), edges_on_cell(maxedges,nc1), degree(nc1)
 integer, intent(in) :: n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(inout) :: scalar_new(num_scalars,nz,nc1), horiz_flux(num_scalars,nz,ne1)
 integer :: i, j, c, e, k, s, cell1, cell2, ica(10)
 real :: dts, weight_time_new, weight_time_old, rho_zz_new_inv, scalar_weight, scalar_weight2(nz,10)
 real :: u_direction, u_positive, u_negative
 real :: wdtn(num_scalars,nz+1), tend_column(num_scalars,nz)

 dts = rk_scalar_timestep(rk_step)
 ! time weight of the new density at stages 1 and 2 (order 2)
 weight_time_new = 1./2
 weight_time_old = 1. - weight_time_new

 ! scalar value at the edges: 3rd-order upwind-biased, in the two outermost relaxation rows plain upwind
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  if (bdy_mask_edge(e) < n_relax_zone-1) then
   select case (n_adv(e))
   case (10)
    do j = 1, 10
     do k = 1, nz
      scalar_weight2(k,j) = adv_coefs(j,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(j,e)
     end do
    end do
    do j = 1, 10
     ica(j) = adv_cells(j,e)
    end do
    do k = 1, nz
     do s = 1, num_scalars
      horiz_flux(s,k,e) = &
         scalar_weight2(k,1)  * scalar_new(s,k,ica(1)) + &
         scalar_weight2(k,2)  * scalar_new(s,k,ica(2)) + &
         scalar_weight2(k,3)  * scalar_new(s,k,ica(3)) + &
         scalar_weight2(k,4)  * scalar_new(s,k,ica(4)) + &
         scalar_weight2(k,5)  * scalar_new(s,k,ica(5)) + &
         scalar_weight2(k,6)  * scalar_new(s,k,ica(6)) + &
         scalar_weight2(k,7)  * scalar_new(s,k,ica(7)) + &
         scalar_weight2(k,8)  * scalar_new(s,k,ica(8)) + &
         scalar_weight2(k,9)  * scalar_new(s,k,ica(9)) + &
         scalar_weight2(k,10) * scalar_new(s,k,ica(10))
     end do
    end do
   case default
    do k = 1, nz
     do s = 1, num_scalars
      horiz_flux(s,k,e) = 0.0
     end do
    end do
    do j = 1, n_adv(e)
     c = adv_cells(j,e)
     do k = 1, nz
      scalar_weight = adv_coefs(j,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(j,e)
      do s = 1, num_scalars
       horiz_flux(s,k,e) = horiz_flux(s,k,e) + scalar_weight * scalar_new(s,k,c)
      end do
     end do
    end do
   end select
  else if (bdy_mask_edge(e) >= n_relax_zone-1 .and. bdy_mask_edge(e) <= n_relax_zone) then
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   do k = 1, nz
    u_direction = sign(0.5,uh_avg(k,e))
    u_positive = dv_edge(e)*abs(u_direction + 0.5)
    u_negative = dv_edge(e)*abs(u_direction - 0.5)
    do s = 1, num_scalars
     horiz_flux(s,k,e) = u_positive*scalar_new(s,k,cell1) + u_negative*scalar_new(s,k,cell2)
    end do
   end do
  else   ! the specified zone: zero (the array's initial zero, written so that no other user can leave a value there)
   do k = 1, nz
    do s = 1, num_scalars
     horiz_flux(s,k,e) = 0.0
    end do
   end do
  end if
 end do

 ! flux divergence of each column (horizontal, then vertical) and the update; the specified zone is left alone
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  if (bdy_mask_cell(c) <= n_relax_zone) then
   do k = 1, nz
    do s = 1, num_scalars
     tend_column(s,k) = 0.0
    end do
   end do
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    do k = 1, nz
     do s = 1, num_scalars
      tend_column(s,k) = tend_column(s,k) - sign_cell(i,c) * uh_avg(k,e)*horiz_flux(s,k,e)
     end do
    end do
   end do
   do k = 1, nz
    do s = 1, num_scalars
     tend_column(s,k) = tend_column(s,k) * inv_area_cell(c) + scalar_tend(s,k,c)
    end do
   end do

   do s = 1, num_scalars
    wdtn(s,1) = 0.0
    wdtn(s,2) = ww_avg(2,c)*(fzm(2)*scalar_new(s,2,c)+fzp(2)*scalar_new(s,2-1,c))
    wdtn(s,nz) = ww_avg(nz,c) * ( fzm(nz)*scalar_new(s,nz,c) + fzp(nz)*scalar_new(s,nz-1,c) )
    wdtn(s,nz+1) = 0.0
   end do
   do k = 3, nz-1
    do s = 1, num_scalars
     wdtn(s,k) = flux3(scalar_new(s,k-2,c), scalar_new(s,k-1,c), scalar_new(s,k,c), scalar_new(s,k+1,c), ww_avg(k,c), coef_3rd_order)
    end do
   end do

   do k = 1, nz
    rho_zz_new_inv = 1.0 / (weight_time_old*rho_zz_old(k,c) + weight_time_new*rho_zz_new(k,c))
    do s = 1, num_scalars
     scalar_new(s,k,c) = ( scalar_old(s,k,c)*rho_zz_old(k,c) &
           + dts*( tend_column(s,k) - rdzw(k)*(wdtn(s,k+1)-wdtn(s,k)) ) ) * rho_zz_new_inv
    end do
   end do
  end if
 end do
 end subroutine

 ! Stage 3: monotonic transport of all scalars. scalars_old (start state) first receives the physics tendency
 ! (scalar_tend, then zeroed), as the original does. The work arrays are caller scratch with a zero ghost column.
 subroutine advance_scalars_mono(scalars_old, scalars_new, scalar_tend, rho_zz_old, uh_avg, ww_avg, &
                                 bdy_mask_cell, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
                                 inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
                                 rho_zz_int, scalar_old, scalar_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp)
 real, intent(in) :: rho_zz_old(nz,nc1), uh_avg(nz,ne1), ww_avg(nz+1,nc1), sign_cell(maxedges,nc1), inv_area_cell(nc1), dv_edge(ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), fzm(nz), fzp(nz), rdzw(nz)
 integer, intent(in) :: bdy_mask_cell(nc1), bdy_mask_edge(ne1), cells_on_edge(2,ne1), cells_on_cell(maxedges,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1), n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(inout) :: scalars_old(num_scalars,nz,nc1), scalars_new(num_scalars,nz,nc1), scalar_tend(num_scalars,nz,nc1)
 real, intent(inout) :: rho_zz_int(nz,nc1), scalar_old(nz,nc1), scalar_new(nz,nc1), s_max(nz,nc1), s_min(nz,nc1)
 real, intent(inout) :: wdtn(nz+1,nc1), scale_arr(nz,2,nc1), flux(nz,ne1), flux_upwind(nz,ne1), flux_tmp(nz,ne1)
 real :: dts

 dts = rk_scalar_timestep(3)

 ! The per-scalar copies of the old and new values: species 1 to num_scalars-1 in edge_work (free during the transport,
 ! see the edge_work comment), the last species in the caller's scalar_old and scalar_new. Taken in one pass over the
 ! state and written back in one pass at the end, so that the (species, level, cell) arrays are streamed once per call,
 ! not once per species (one cache line holds 2 levels of all 8 species).
 call edge_work_ghosts(-1)
 if (2*(num_scalars-1)*nz*nc1 > size(edge_work)) error stop 'advance_scalars_mono: edge_work too small for the scalar copies'
 call mono_scalars(dts, scalars_old, scalars_new, scalar_tend, rho_zz_old, uh_avg, ww_avg, &
      bdy_mask_cell, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
      inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
      rho_zz_int, scalar_old, scalar_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp, &
      edge_work(1), edge_work((num_scalars-1)*nz*nc1+1))
 end subroutine

 ! advance_scalars_mono with the per-scalar copies old_t, new_t of species 1 to num_scalars-1 (level, cell, species).
 subroutine mono_scalars(dts, scalars_old, scalars_new, scalar_tend, rho_zz_old, uh_avg, ww_avg, &
                         bdy_mask_cell, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
                         inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
                         rho_zz_int, scalar_old, scalar_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp, old_t, new_t)
 real, intent(in) :: dts
 real, intent(in) :: rho_zz_old(nz,nc1), uh_avg(nz,ne1), ww_avg(nz+1,nc1), sign_cell(maxedges,nc1), inv_area_cell(nc1), dv_edge(ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), fzm(nz), fzp(nz), rdzw(nz)
 integer, intent(in) :: bdy_mask_cell(nc1), bdy_mask_edge(ne1), cells_on_edge(2,ne1), cells_on_cell(maxedges,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1), n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(inout) :: scalars_old(num_scalars,nz,nc1), scalars_new(num_scalars,nz,nc1), scalar_tend(num_scalars,nz,nc1)
 real, intent(inout) :: rho_zz_int(nz,nc1), scalar_old(nz,nc1), scalar_new(nz,nc1), s_max(nz,nc1), s_min(nz,nc1)
 real, intent(inout) :: wdtn(nz+1,nc1), scale_arr(nz,2,nc1), flux(nz,ne1), flux_upwind(nz,ne1), flux_tmp(nz,ne1)
 real, intent(inout) :: old_t(nz,nc1,num_scalars-1), new_t(nz,nc1,num_scalars-1)
 integer :: i, c, e, k, s

 ! the part of the update that is not transport (physics tendency), positive by construction; then the per-scalar copies
 ! and the density after the transport step
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do k = 1, nz
   do s = 1, num_scalars
    scalars_old(s,k,c) = scalars_old(s,k,c) + dts*scalar_tend(s,k,c) / rho_zz_old(k,c)
    scalar_tend(s,k,c) = 0.0
   end do
  end do
  do s = 1, num_scalars-1
   do k = 1, nz
    old_t(k,c,s) = scalars_old(s,k,c)
    new_t(k,c,s) = scalars_new(s,k,c)
   end do
  end do
  do k = 1, nz
   scalar_old(k,c) = scalars_old(num_scalars,k,c)
   scalar_new(k,c) = scalars_new(num_scalars,k,c)
  end do
  ! density after the transport step, from the same mass fluxes
  do k = 1, nz
   rho_zz_int(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   do k = 1, nz
    rho_zz_int(k,c) = rho_zz_int(k,c) - sign_cell(i,c) * uh_avg(k,e) * dv_edge(e) * inv_area_cell(c)
   end do
  end do
  do k = 1, nz
   rho_zz_int(k,c) = rho_zz_old(k,c) + dts*(rho_zz_int(k,c) - rdzw(k)*(ww_avg(k+1,c)-ww_avg(k,c)))
  end do
 end do
 ! the ghost column of every copy is zero (edge_work holds other users' values between calls)
 !$omp single
 do s = 1, num_scalars-1
  do k = 1, nz
   old_t(k,nc1,s) = 0.0
   new_t(k,nc1,s) = 0.0
  end do
 end do
 do k = 1, nz
  scalar_old(k,nc1) = 0.0
  scalar_new(k,nc1) = 0.0
 end do
 !$omp end single

 ! every scalar through the monotonic transport, then all of them back into the state
 do s = 1, num_scalars-1
  call mono_one(dts, rho_zz_old, uh_avg, ww_avg, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
       inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
       rho_zz_int, old_t(1,1,s), new_t(1,1,s), s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp)
 end do
 call mono_one(dts, rho_zz_old, uh_avg, ww_avg, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
      inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
      rho_zz_int, scalar_old, scalar_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp)

 ! back into the state (negative round-off removed); only cells with mask <= n_spec_zone, as in the original
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
#ifdef LEAN_MONO_RELAX
  ! Lean departure (build switch LEAN_MONO_RELAX, default off): the original tests <= nSpecZone (2) here, so relaxation
  ! rows 3-5 never receive the monotonic update and keep the stage-2 (dt/2) value, although its own comment says that only
  ! the specified zone (rows 6-7) is set after transport. The switch writes the relaxation rows back too.
  if (bdy_mask_cell(c) <= n_relax_zone) then
#else
  if (bdy_mask_cell(c) <= n_spec_zone) then
#endif
   do s = 1, num_scalars-1
    do k = 1, nz
     scalars_new(s,k,c) = max(0.0,new_t(k,c,s))
    end do
   end do
   do k = 1, nz
    scalars_new(num_scalars,k,c) = max(0.0,scalar_new(k,c))
   end do
  end if
 end do
 end subroutine

 ! The monotonic transport of one scalar: scalar_old holds its old values (with the physics tendency), scalar_new its
 ! stage-2 values on entry and the transported value (not yet clipped at zero) on return; both have a zero ghost column.
 subroutine mono_one(dts, rho_zz_old, uh_avg, ww_avg, bdy_mask_edge, cells_on_edge, cells_on_cell, edges_on_cell, degree, sign_cell, &
                     inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, adv_coefs_3rd, fzm, fzp, rdzw, &
                     rho_zz_int, scalar_old, scalar_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp)
 real, intent(in) :: dts
 real, intent(in) :: rho_zz_old(nz,nc1), uh_avg(nz,ne1), ww_avg(nz+1,nc1), sign_cell(maxedges,nc1), inv_area_cell(nc1), dv_edge(ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), fzm(nz), fzp(nz), rdzw(nz)
 integer, intent(in) :: bdy_mask_edge(ne1), cells_on_edge(2,ne1), cells_on_cell(maxedges,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1), n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(in) :: rho_zz_int(nz,nc1), scalar_old(nz,nc1)
 real, intent(inout) :: scalar_new(nz,nc1), s_max(nz,nc1), s_min(nz,nc1)
 real, intent(inout) :: wdtn(nz+1,nc1), scale_arr(nz,2,nc1), flux(nz,ne1), flux_upwind(nz,ne1), flux_tmp(nz,ne1)
 integer, parameter :: scale_in = 1, scale_out = 2
 real, parameter :: eps = 1.e-20
 real :: scale_factor, scalar_weight, f, flux_upwind_arr(nz), swa(10,2)
 integer :: i, j, c, e, k, cell1, cell2, ii, ica(10)

 ! high-order horizontal flux (mass flux times 3rd-order edge value), then the upwind flux and the antidiffusive
 ! remainder of the same edge; the two outermost relaxation rows keep upwind only
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  select case (n_adv(e))
  case (10)
   do j = 1, 10
    ica(j) = adv_cells(j,e)
    swa(j,1) = adv_coefs(j,e) + adv_coefs_3rd(j,e)
    swa(j,2) = adv_coefs(j,e) - adv_coefs_3rd(j,e)
   end do
   do k = 1, nz
    ii = merge(1, 2, uh_avg(k,e) > 0)
    flux(k,e) = uh_avg(k,e)*( &
         swa(1,ii)*scalar_new(k,ica(1)) + swa(2,ii)*scalar_new(k,ica(2)) + &
         swa(3,ii)*scalar_new(k,ica(3)) + swa(4,ii)*scalar_new(k,ica(4)) + &
         swa(5,ii)*scalar_new(k,ica(5)) + swa(6,ii)*scalar_new(k,ica(6)) + &
         swa(7,ii)*scalar_new(k,ica(7)) + swa(8,ii)*scalar_new(k,ica(8)) + &
         swa(9,ii)*scalar_new(k,ica(9)) + swa(10,ii)*scalar_new(k,ica(10)))
   end do
  case default
   do k = 1, nz
    flux(k,e) = 0.0
   end do
   do i = 1, n_adv(e)
    c = adv_cells(i,e)
    do k = 1, nz
     scalar_weight = uh_avg(k,e)*(adv_coefs(i,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(i,e))
     flux(k,e) = flux(k,e) + scalar_weight* scalar_new(k,c)
    end do
   end do
  end select
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   flux_upwind(k,e) = dv_edge(e) * dts * (max(0.0,uh_avg(k,e))*scalar_old(k,cell1) + min(0.0,uh_avg(k,e))*scalar_old(k,cell2))
   flux_tmp(k,e) = dts * flux(k,e) - flux_upwind(k,e)
  end do
  if (bdy_mask_edge(e) == n_relax_zone .or. bdy_mask_edge(e) == n_relax_zone-1) then
   do k = 1, nz
    flux_tmp(k,e) = 0.0
    flux(k,e) = flux_upwind(k,e)
   end do
  end if
 end do

 ! per column (after the edge loop, which reads the stage-2 values of the neighbours): vertical flux and the limiter's
 ! bounds; first-order upwind update (into scalar_new), vertical then horizontal, with the incoming/outgoing
 ! antidiffusive flux; the limiter (the fraction of the antidiffusive flux that keeps the new value inside
 ! [s_min, s_max]) and the limited vertical fluxes
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  wdtn(1,c) = 0.0
  wdtn(nz+1,c) = 0.0
  k = 1
  s_max(k,c) = max(scalar_old(1,c),scalar_old(2,c))
  s_min(k,c) = min(scalar_old(1,c),scalar_old(2,c))
  k = 2
  wdtn(k,c) = ww_avg(k,c)*(fzm(k)*scalar_new(k,c)+fzp(k)*scalar_new(k-1,c))
  s_max(k,c) = max(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
  s_min(k,c) = min(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
  do k = 3, nz-1
   wdtn(k,c) = flux3(scalar_new(k-2,c), scalar_new(k-1,c), scalar_new(k,c), scalar_new(k+1,c), ww_avg(k,c), coef_3rd_order)
   s_max(k,c) = max(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
   s_min(k,c) = min(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
  end do
  k = nz
  wdtn(k,c) = ww_avg(k,c)*(fzm(k)*scalar_new(k,c)+fzp(k)*scalar_new(k-1,c))
  s_max(k,c) = max(scalar_old(k,c),scalar_old(k-1,c))
  s_min(k,c) = min(scalar_old(k,c),scalar_old(k-1,c))
  select case (degree(c))
  case (6)
   do k = 1, nz
    s_max(k,c) = max(s_max(k,c), &
         scalar_old(k, cells_on_cell(1,c)), scalar_old(k, cells_on_cell(2,c)), scalar_old(k, cells_on_cell(3,c)), &
         scalar_old(k, cells_on_cell(4,c)), scalar_old(k, cells_on_cell(5,c)), scalar_old(k, cells_on_cell(6,c)))
    s_min(k,c) = min(s_min(k,c), &
         scalar_old(k, cells_on_cell(1,c)), scalar_old(k, cells_on_cell(2,c)), scalar_old(k, cells_on_cell(3,c)), &
         scalar_old(k, cells_on_cell(4,c)), scalar_old(k, cells_on_cell(5,c)), scalar_old(k, cells_on_cell(6,c)))
   end do
  case default
   do i = 1, degree(c)
    do k = 1, nz
     s_max(k,c) = max(s_max(k,c),scalar_old(k, cells_on_cell(i,c)))
     s_min(k,c) = min(s_min(k,c),scalar_old(k, cells_on_cell(i,c)))
    end do
   end do
  end select
  k = 1
  scalar_new(k,c) = scalar_old(k,c) * rho_zz_old(k,c)
  do k = 2, nz
   scalar_new(k,c) = scalar_old(k,c)*rho_zz_old(k,c)
   flux_upwind_arr(k) = dts*(max(0.0,ww_avg(k,c))*scalar_old(k-1,c) + min(0.0,ww_avg(k,c))*scalar_old(k,c))
  end do
  do k = 1, nz-1
   scalar_new(k,c) = scalar_new(k,c) - flux_upwind_arr(k+1)*rdzw(k)
  end do
  do k = 2, nz
   scalar_new(k  ,c) = scalar_new(k  ,c) + flux_upwind_arr(k)*rdzw(k)
   wdtn(k,c) = dts*wdtn(k,c) - flux_upwind_arr(k)
  end do
  do k = 1, nz
   scale_arr(k,scale_in, c) = - rdzw(k)*(min(0.0,wdtn(k+1,c))-max(0.0,wdtn(k,c)))
   scale_arr(k,scale_out,c) = - rdzw(k)*(max(0.0,wdtn(k+1,c))-min(0.0,wdtn(k,c)))
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   do k = 1, nz
    scalar_new(k,c) = scalar_new(k,c) - sign_cell(i,c) * flux_upwind(k,e) * inv_area_cell(c)
    scale_arr(k,scale_out,c) = scale_arr(k,scale_out,c) - max(0.0,sign_cell(i,c)*flux_tmp(k,e)) * inv_area_cell(c)
    scale_arr(k,scale_in, c) = scale_arr(k,scale_in, c) - min(0.0,sign_cell(i,c)*flux_tmp(k,e)) * inv_area_cell(c)
   end do
  end do
  do k = 1, nz
   scale_factor = (s_max(k,c)*rho_zz_int(k,c) - scalar_new(k,c)) / (scale_arr(k,scale_in,c) + eps)
   scale_arr(k,scale_in,c) = min(1.0, max(0.0, scale_factor))
   scale_factor = (s_min(k,c)*rho_zz_int(k,c) - scalar_new(k,c)) / (scale_arr(k,scale_out,c) - eps)
   scale_arr(k,scale_out,c) = min(1.0, max(0.0, scale_factor))
  end do
  do k = 2, nz
   f = wdtn(k,c)
   f = max(0.0,f) * min(scale_arr(k-1,scale_out,c), scale_arr(k  ,scale_in,c)) &
     + min(0.0,f) * min(scale_arr(k  ,scale_out,c), scale_arr(k-1,scale_in,c))
   wdtn(k,c) = f
  end do
 end do

 ! limited horizontal fluxes: the antidiffusive remainder flux_tmp (the original recomputes it here as dts*flux minus the
 ! upwind flux, the same operations on the same operands, and zeroes it in the same two relaxation rows) times the limiter
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   f = flux_tmp(k,e)
   f = max(0.0,f) * min(scale_arr(k,scale_out,cell1), scale_arr(k,scale_in, cell2)) &
     + min(0.0,f) * min(scale_arr(k,scale_in, cell1), scale_arr(k,scale_out,cell2))
   flux(k,e) = f
  end do
 end do

 ! final update with the limited fluxes, divided by the new density
 !$omp do schedule(dynamic,64)
 do c = 1, ncells
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   do k = 1, nz
    scalar_new(k,c) = scalar_new(k,c) - sign_cell(i,c)*flux(k,e) * inv_area_cell(c)
   end do
  end do
  do k = 1, nz
   scalar_new(k,c) = (scalar_new(k,c) + (-rdzw(k)*(wdtn(k+1,c)-wdtn(k,c)) ) )/rho_zz_int(k,c)
  end do
 end do
 end subroutine

 ! Third-order vertical flux (4th-order centred plus an upwind-biased correction).
 pure real function flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3)
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua, coef3
 flux3 = ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0 + coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0
 end function
end module
