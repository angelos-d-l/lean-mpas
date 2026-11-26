! HAND-WRITTEN card version of src/dyn_scalars.f90 (not generated; ./lean generate --check does not see it). The card build
! compiles it in place of src/dyn_scalars.f90 (config/units.json device_overrides); a change to a dynamics kernel is made in both files.
! Split scalar transport (atm_advance_scalars_work and atm_advance_scalars_mono_work of MPAS 8.4.2, "the original" below)
! for the configuration lean_mpas supports:
! order-2 Runge-Kutta in three stages with the mass fluxes averaged over the dynamics substeps, stages 1 and 2
! non-monotonic (3rd-order horizontal, vertical flux3), stage 3 monotonic (flux-corrected, limiter from the
! min/max of the old values around the cell). Density is advanced inside the transport (advance_density true).
! Same arithmetic and summation order as the original; no halos, no pools. Arrays follow the original layout
! (scalar index first in the state, level first in the work arrays); the last cell/edge slot is the permanent
! zero column. Regional: cells with bdy_mask_cell > n_relax_zone (specified zone) are not touched by the
! non-monotonic update, and the monotonic one copies back only the cells with mask <= n_spec_zone, as the original
! does (see the LEAN_MONO_RELAX note in advance_scalars_mono and docs/fixes.md, LEAN_MONO_RELAX).
module dyn_scalars
 use config
 use dyn_config
 implicit none
 ! On the card one edge work buffer (edge_work, num_scalars edge fields) serves as
 ! advance_scalars' horiz_flux, as the monotonic transport's flux, flux_upwind, flux_tmp and as the two edge work arrays of
 ! compute_dyn_tend: they never live at the same time. Every user writes all its real edges before it
 ! reads them; the ghost edge column of the user's view is zeroed before each use (edge_work_ghosts).
 real, allocatable, save :: edge_work(:)
 !$acc declare create(edge_work)
 ! the monotonic transport's group of scalars and the offsets of its views in edge_work (mono_views)
 integer, parameter :: mono_g = 2
 integer, save :: mono_off(7) = 0
contains
 ! the ghost columns of compute_dyn_tend's views of the edge work buffer (see its call in dyn_step), one kernel on queue 1
 subroutine edge_work_ghosts_tend()
 integer :: i, v, n, base(9)
 call edge_work_ghosts(-1)
 base(1) = nz*ne1 + (ne1-1)*nz                 ! edge field 2
 base(2) = 2*nz*ne1 + (ne1-1)*nz               ! edge field 3
 base(3) = (nc1-1)*nz                          ! eddy_visc_horz
 base(4) = nz*nc1 + (nc1-1)*nz                 ! h_divergence
 base(5) = 3*nz*ne1 + (ne1-1)*nz               ! delsq_u
 base(6) = 4*nz*ne1 + (nv1-1)*nz               ! delsq_vorticity
 base(7) = 5*nz*ne1 + (nc1-1)*nz               ! delsq_divergence
 base(8) = 5*nz*ne1 + nz*nc1 + (nc1-1)*nz      ! delsq_w
 base(9) = 6*nz*ne1 + (nc1-1)*nz               ! delsq_theta
 n = nz
 !$acc parallel loop gang vector collapse(2) default(present) copyin(base) async(1)
 do v = 1, 9
  do i = 1, n
   edge_work(base(v)+i) = 0.0
  end do
 end do
 end subroutine
 ! the ghost edge of the edge work buffer (see edge_work) as its next user sees it: views = 0, advance_scalars' horiz_flux
 ! (num_scalars, nz, ne1); views = n, n edge fields (nz, ne1) one after the other. On the card's queue 1.
 subroutine edge_work_ghosts(views)
 integer, intent(in) :: views
 integer :: i, v, n, base
 if (.not. allocated(edge_work)) then
  allocate(edge_work(num_scalars*nz*ne1))
  edge_work = 0.0
  !$acc update device(edge_work)
 end if
 if (views < 0) return   ! (allocation only)
 if (views == 0) then
  base = (ne1-1)*nz*num_scalars
  n = nz*num_scalars
  !$acc parallel loop gang vector default(present) async(1)
  do i = 1, n
   edge_work(base+i) = 0.0
  end do
 else
  ! the views' ghost columns in one kernel
  n = nz
  base = (ne1-1)*nz
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do v = 0, views-1
   do i = 1, n
    edge_work(v*nz*ne1 + base + i) = 0.0
   end do
  end do
 end if
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
 ! gang-private work of the kernels below, sized by the build's level capacity (a compile-time size keeps them in the card's shared
 ! memory; a run-time size would put them in global memory, set up at every launch)
 real :: dts, weight_time_new, weight_time_old, rho_zz_new_inv, scalar_weight, scalar_weight2(max_levels,10)
 real :: u_direction, u_positive, u_negative
 real :: wdtn(num_scalars,max_levels+1), tend_column(num_scalars,max_levels)

 dts = rk_scalar_timestep(rk_step)
 ! time weight of the new density at stages 1 and 2 (order 2)
 weight_time_new = 1./2
 weight_time_old = 1. - weight_time_new

 ! scalar value at the edges: 3rd-order upwind-biased, in the two outermost relaxation rows plain upwind. The vector runs over
 ! (level, scalar), scalar fastest: the threads of a warp read consecutive words of scalar_new and write consecutive horiz_flux.
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(ica, scalar_weight2) async(1)
 do e = 1, nedges
  if (bdy_mask_edge(e) < n_relax_zone-1) then
   select case (n_adv(e))
   case (10)
    do j = 1, 10
     !$acc loop vector
     do k = 1, nz
      scalar_weight2(k,j) = adv_coefs(j,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(j,e)
     end do
    end do
    do j = 1, 10
     ica(j) = adv_cells(j,e)
    end do
    !$acc loop vector collapse(2)
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
    !$acc loop vector collapse(2)
    do k = 1, nz
     do s = 1, num_scalars
      horiz_flux(s,k,e) = 0.0
     end do
    end do
    do j = 1, n_adv(e)
     c = adv_cells(j,e)
     !$acc loop vector collapse(2)
     do k = 1, nz
      do s = 1, num_scalars
       scalar_weight = adv_coefs(j,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(j,e)
       horiz_flux(s,k,e) = horiz_flux(s,k,e) + scalar_weight * scalar_new(s,k,c)
      end do
     end do
    end do
   end select
  else if (bdy_mask_edge(e) >= n_relax_zone-1 .and. bdy_mask_edge(e) <= n_relax_zone) then
   cell1 = cells_on_edge(1,e)
   cell2 = cells_on_edge(2,e)
   !$acc loop vector collapse(2)
   do k = 1, nz
    do s = 1, num_scalars
     u_direction = sign(0.5,uh_avg(k,e))
     u_positive = dv_edge(e)*abs(u_direction + 0.5)
     u_negative = dv_edge(e)*abs(u_direction - 0.5)
     horiz_flux(s,k,e) = u_positive*scalar_new(s,k,cell1) + u_negative*scalar_new(s,k,cell2)
    end do
   end do
  else   ! the specified zone: zero
   !$acc loop vector collapse(2)
   do k = 1, nz
    do s = 1, num_scalars
     horiz_flux(s,k,e) = 0.0
    end do
   end do
  end if
 end do

 ! flux divergence of each column (horizontal, then vertical) and the update; the specified zone is left alone
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(tend_column, wdtn) async(1)
 do c = 1, ncells
  if (bdy_mask_cell(c) <= n_relax_zone) then
   !$acc loop vector collapse(2)
   do k = 1, nz
    do s = 1, num_scalars
     tend_column(s,k) = 0.0
    end do
   end do
   do i = 1, degree(c)
    e = edges_on_cell(i,c)
    !$acc loop vector collapse(2)
    do k = 1, nz
     do s = 1, num_scalars
      tend_column(s,k) = tend_column(s,k) - sign_cell(i,c) * uh_avg(k,e)*horiz_flux(s,k,e)
     end do
    end do
   end do
   !$acc loop vector collapse(2)
   do k = 1, nz
    do s = 1, num_scalars
     tend_column(s,k) = tend_column(s,k) * inv_area_cell(c) + scalar_tend(s,k,c)
    end do
   end do

   !$acc loop vector
   do s = 1, num_scalars
    wdtn(s,1) = 0.0
    wdtn(s,2) = ww_avg(2,c)*(fzm(2)*scalar_new(s,2,c)+fzp(2)*scalar_new(s,2-1,c))
    wdtn(s,nz) = ww_avg(nz,c) * ( fzm(nz)*scalar_new(s,nz,c) + fzp(nz)*scalar_new(s,nz-1,c) )
    wdtn(s,nz+1) = 0.0
   end do
   !$acc loop vector collapse(2)
   do k = 3, nz-1
    do s = 1, num_scalars
     wdtn(s,k) = flux3(scalar_new(s,k-2,c), scalar_new(s,k-1,c), scalar_new(s,k,c), scalar_new(s,k+1,c), ww_avg(k,c), coef_3rd_order)
    end do
   end do

   !$acc loop vector collapse(2)
   do k = 1, nz
    do s = 1, num_scalars
     rho_zz_new_inv = 1.0 / (weight_time_old*rho_zz_old(k,c) + weight_time_new*rho_zz_new(k,c))
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
 integer, parameter :: scale_in = 1, scale_out = 2
 real, parameter :: eps = 1.e-20
 real :: dts, scale_factor, scalar_weight, flux_up, f, f_low, f_up, sn, s_out, s_in, swa(10,2)
 integer :: i, j, c, e, k, s, cell1, cell2, ii, ica(10)

 dts = rk_scalar_timestep(3)

 ! the part of the update that is not transport (physics tendency), positive by construction
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   do s = 1, num_scalars
    scalars_old(s,k,c) = scalars_old(s,k,c) + dts*scalar_tend(s,k,c) / rho_zz_old(k,c)
    scalar_tend(s,k,c) = 0.0
   end do
  end do
 end do

 ! density after the transport step, from the same mass fluxes
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) async(1)
 do c = 1, ncells
  !$acc loop vector
  do k = 1, nz
   rho_zz_int(k,c) = 0.0
  end do
  do i = 1, degree(c)
   e = edges_on_cell(i,c)
   !$acc loop vector
   do k = 1, nz
    rho_zz_int(k,c) = rho_zz_int(k,c) - sign_cell(i,c) * uh_avg(k,e) * dv_edge(e) * inv_area_cell(c)
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   rho_zz_int(k,c) = rho_zz_old(k,c) + dts*(rho_zz_int(k,c) - rdzw(k)*(ww_avg(k+1,c)-ww_avg(k,c)))
  end do
 end do

 ! the scalars in groups of mono_g: each kernel below works on (point, scalar of the group), mono_g scalars sharing
 ! the indirect reads and the launches. The work arrays of a group are views of edge_work
 ! (mono_views); flux_upwind and flux_tmp are recomputed where they are read and the limiter's bounds in the limiter (the same
 ! expressions, the same values), and the scalars are read from the state (its ghost column is the permanent zero). Each
 ! value is computed as in the original's loop over the scalars, in the same order.
#ifdef _OPENACC
 call mono_views()
 do s = 1, num_scalars, mono_g
  call mono_group(s, dts, scalars_old, scalars_new, rho_zz_old, uh_avg, ww_avg, bdy_mask_cell, bdy_mask_edge, cells_on_edge, &
                  cells_on_cell, edges_on_cell, degree, sign_cell, inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, &
                  adv_coefs_3rd, fzm, fzp, rdzw, rho_zz_int, edge_work(mono_off(1)+1), edge_work(mono_off(2)+1), &
                  edge_work(mono_off(3)+1), edge_work(mono_off(4)+1), edge_work(mono_off(5)+1), edge_work(mono_off(6)+1), &
                  edge_work(mono_off(7)+1))
 end do
#else
 ! the host: one scalar at a time through the work arrays of the caller
 do s = 1, num_scalars
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz
    scalar_old(k,c) = scalars_old(s,k,c)
    scalar_new(k,c) = scalars_new(s,k,c)
   end do
  end do
  !$omp single
  !$acc parallel loop gang vector default(present) firstprivate(nc1) async(1)
  do k = 1, nz
   scalar_old(k,nc1) = 0.0
   scalar_new(k,nc1) = 0.0
  end do
  !$omp end single

  ! vertical flux, and the bounds for the limiter from the old values around the cell
  !$omp do schedule(dynamic,64)
  ! gang = column, vector = level (one thread per column read the levels with a stride)
  !$acc parallel loop gang vector_length(64) default(present) async(1)
  do c = 1, ncells
   !$acc loop vector private(i)
   do k = 1, nz
    if (k == 1) then
     wdtn(1,c) = 0.0
     wdtn(nz+1,c) = 0.0
     s_max(k,c) = max(scalar_old(1,c),scalar_old(2,c))
     s_min(k,c) = min(scalar_old(1,c),scalar_old(2,c))
    else if (k == 2) then
     wdtn(k,c) = ww_avg(k,c)*(fzm(k)*scalar_new(k,c)+fzp(k)*scalar_new(k-1,c))
     s_max(k,c) = max(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
     s_min(k,c) = min(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
    else if (k < nz) then
     wdtn(k,c) = flux3(scalar_new(k-2,c), scalar_new(k-1,c), scalar_new(k,c), scalar_new(k+1,c), ww_avg(k,c), coef_3rd_order)
     s_max(k,c) = max(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
     s_min(k,c) = min(scalar_old(k-1,c),scalar_old(k,c),scalar_old(k+1,c))
    else
     wdtn(k,c) = ww_avg(k,c)*(fzm(k)*scalar_new(k,c)+fzp(k)*scalar_new(k-1,c))
     s_max(k,c) = max(scalar_old(k,c),scalar_old(k-1,c))
     s_min(k,c) = min(scalar_old(k,c),scalar_old(k-1,c))
    end if
    if (degree(c) == 6) then
     s_max(k,c) = max(s_max(k,c), &
          scalar_old(k, cells_on_cell(1,c)), scalar_old(k, cells_on_cell(2,c)), scalar_old(k, cells_on_cell(3,c)), &
          scalar_old(k, cells_on_cell(4,c)), scalar_old(k, cells_on_cell(5,c)), scalar_old(k, cells_on_cell(6,c)))
     s_min(k,c) = min(s_min(k,c), &
          scalar_old(k, cells_on_cell(1,c)), scalar_old(k, cells_on_cell(2,c)), scalar_old(k, cells_on_cell(3,c)), &
          scalar_old(k, cells_on_cell(4,c)), scalar_old(k, cells_on_cell(5,c)), scalar_old(k, cells_on_cell(6,c)))
    else
     do i = 1, degree(c)
      s_max(k,c) = max(s_max(k,c),scalar_old(k, cells_on_cell(i,c)))
      s_min(k,c) = min(s_min(k,c),scalar_old(k, cells_on_cell(i,c)))
     end do
    end if
   end do
  end do

  ! high-order horizontal flux (mass flux times 3rd-order edge value)
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector_length(64) default(present) private(ica, swa) async(1)
  do e = 1, nedges
   select case (n_adv(e))
   case (10)
    do j = 1, 10
     ica(j) = adv_cells(j,e)
     swa(j,1) = adv_coefs(j,e) + adv_coefs_3rd(j,e)
     swa(j,2) = adv_coefs(j,e) - adv_coefs_3rd(j,e)
    end do
    !$acc loop vector
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
    !$acc loop vector
    do k = 1, nz
     flux(k,e) = 0.0
    end do
    do i = 1, n_adv(e)
     c = adv_cells(i,e)
     !$acc loop vector
     do k = 1, nz
      scalar_weight = uh_avg(k,e)*(adv_coefs(i,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(i,e))
      flux(k,e) = flux(k,e) + scalar_weight* scalar_new(k,c)
     end do
    end do
   end select
  end do

  ! first-order upwind update (into scalar_new), and the vertical part of the incoming/outgoing antidiffusive flux
  !$omp do schedule(dynamic,64)
  ! gang = column, vector = level: the upwind fluxes through the level's lower and upper face are each computed where they are needed
  ! (same expression, same value), so the levels need not wait for each other
  !$acc parallel loop gang vector_length(64) default(present) async(1)
  do c = 1, ncells
   !$acc loop vector private(f_low, f_up, sn)
   do k = 1, nz
    sn = scalar_old(k,c)*rho_zz_old(k,c)
    if (k < nz) then
     f_up = dts*(max(0.0,ww_avg(k+1,c))*scalar_old(k,c) + min(0.0,ww_avg(k+1,c))*scalar_old(k+1,c))
     sn = sn - f_up*rdzw(k)
    end if
    if (k > 1) then
     f_low = dts*(max(0.0,ww_avg(k,c))*scalar_old(k-1,c) + min(0.0,ww_avg(k,c))*scalar_old(k,c))
     sn = sn + f_low*rdzw(k)
     wdtn(k,c) = dts*wdtn(k,c) - f_low
    end if
    scalar_new(k,c) = sn
   end do
   !$acc loop vector
   do k = 1, nz
    scale_arr(k,scale_in, c) = - rdzw(k)*(min(0.0,wdtn(k+1,c))-max(0.0,wdtn(k,c)))
    scale_arr(k,scale_out,c) = - rdzw(k)*(max(0.0,wdtn(k+1,c))-min(0.0,wdtn(k,c)))
   end do
  end do

  ! upwind flux and the antidiffusive remainder; the two outermost relaxation rows keep upwind only (one thread per (level, edge))
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2) async(1)
  do e = 1, nedges
   do k = 1, nz
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    flux_upwind(k,e) = dv_edge(e) * dts * (max(0.0,uh_avg(k,e))*scalar_old(k,cell1) + min(0.0,uh_avg(k,e))*scalar_old(k,cell2))
    flux_tmp(k,e) = dts * flux(k,e) - flux_upwind(k,e)
    if (bdy_mask_edge(e) == n_relax_zone .or. bdy_mask_edge(e) == n_relax_zone-1) then
     flux_tmp(k,e) = 0.0
     flux(k,e) = flux_upwind(k,e)
    end if
   end do
  end do

  ! horizontal part of the upwind update and of the incoming/outgoing antidiffusive flux (one thread per (level, cell), the edges in
  ! the original order, summed in registers)
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) private(i, e, sn, s_out, s_in) async(1)
  do c = 1, ncells
   do k = 1, nz
    sn = scalar_new(k,c)
    s_out = scale_arr(k,scale_out,c)
    s_in = scale_arr(k,scale_in, c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     sn = sn - sign_cell(i,c) * flux_upwind(k,e) * inv_area_cell(c)
     s_out = s_out - max(0.0,sign_cell(i,c)*flux_tmp(k,e)) * inv_area_cell(c)
     s_in = s_in - min(0.0,sign_cell(i,c)*flux_tmp(k,e)) * inv_area_cell(c)
    end do
    scalar_new(k,c) = sn
    scale_arr(k,scale_out,c) = s_out
    scale_arr(k,scale_in, c) = s_in
   end do
  end do

  ! limiter: the fraction of the antidiffusive flux that keeps the new value inside [s_min, s_max]
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 1, nz
    scale_factor = (s_max(k,c)*rho_zz_int(k,c) - scalar_new(k,c)) / (scale_arr(k,scale_in,c) + eps)
    scale_arr(k,scale_in,c) = min(1.0, max(0.0, scale_factor))
    scale_factor = (s_min(k,c)*rho_zz_int(k,c) - scalar_new(k,c)) / (scale_arr(k,scale_out,c) - eps)
    scale_arr(k,scale_out,c) = min(1.0, max(0.0, scale_factor))
   end do
  end do

  ! limited horizontal fluxes (one thread per (level, edge))
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) private(cell1, cell2, flux_up, f) async(1)
  do e = 1, nedges
   do k = 1, nz
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    flux_up = dv_edge(e) * dts * (max(0.0,uh_avg(k,e))*scalar_old(k,cell1) + min(0.0,uh_avg(k,e))*scalar_old(k,cell2))
    f = dts*flux(k,e) - flux_up
    if (bdy_mask_edge(e) == n_relax_zone .or. bdy_mask_edge(e) == n_relax_zone-1) f = 0.0
    f = max(0.0,f) * min(scale_arr(k,scale_out,cell1), scale_arr(k,scale_in, cell2)) &
      + min(0.0,f) * min(scale_arr(k,scale_in, cell1), scale_arr(k,scale_out,cell2))
    flux(k,e) = f
   end do
  end do

  ! limited vertical fluxes
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) async(1)
  do c = 1, ncells
   do k = 2, nz
    f = wdtn(k,c)
    f = max(0.0,f) * min(scale_arr(k-1,scale_out,c), scale_arr(k  ,scale_in,c)) &
      + min(0.0,f) * min(scale_arr(k  ,scale_out,c), scale_arr(k-1,scale_in,c))
    wdtn(k,c) = f
   end do
  end do

  ! final update with the limited fluxes, divided by the new density, and back into the state (negative round-off removed; only cells
  ! with mask <= n_spec_zone, as in the original). One thread per (level, cell).
  !$omp do schedule(dynamic,64)
  !$acc parallel loop gang vector collapse(2) default(present) private(i, e, sn) async(1)
  do c = 1, ncells
   do k = 1, nz
    sn = scalar_new(k,c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     sn = sn - sign_cell(i,c)*flux(k,e) * inv_area_cell(c)
    end do
    sn = (sn + (-rdzw(k)*(wdtn(k+1,c)-wdtn(k,c)) ) )/rho_zz_int(k,c)
    scalar_new(k,c) = sn
#ifdef LEAN_MONO_RELAX
    ! Lean departure (build switch LEAN_MONO_RELAX, default off; docs/fixes.md, LEAN_MONO_RELAX): the original tests
    ! <= nSpecZone (2) here, so relaxation rows 3-5 never receive the monotonic update and keep the stage-2 (dt/2) value,
    ! although its own comment says that only the specified zone (rows 6-7) is set after transport. The switch writes the
    ! relaxation rows back too.
    if (bdy_mask_cell(c) <= n_relax_zone) then
#else
    if (bdy_mask_cell(c) <= n_spec_zone) then
#endif
     scalars_new(s,k,c) = max(0.0,sn)
    end if
   end do
  end do
 end do
#endif
 end subroutine

 ! the views of edge_work for a group of the monotonic transport, level first: flx (nz, mono_g, ne1) the high-order flux, then the
 ! limited one; snw, sin, sout (nz, mono_g, nc1) the upwind update and the incoming/outgoing scale; wd (nz+1, mono_g, nc1) the
 ! vertical flux; so, sn (nz, mono_g, nc1) copies of the group's old and new scalars. The
 ! ghost edge of flx and the ghost cell of sin, sout, so, sn are zeroed (no kernel writes them).
 subroutine mono_views()
 integer :: i, n, o1, o3, o4, o6, o7
 if (.not. allocated(edge_work)) then
  allocate(edge_work(num_scalars*nz*ne1))
  edge_work = 0.0
  !$acc update device(edge_work)
 end if
 mono_off(1) = 0
 mono_off(2) = mono_off(1) + mono_g*nz*ne1
 mono_off(3) = mono_off(2) + mono_g*nz*nc1
 mono_off(4) = mono_off(3) + mono_g*nz*nc1
 mono_off(5) = mono_off(4) + mono_g*nz*nc1
 mono_off(6) = mono_off(5) + mono_g*(nz+1)*nc1
 mono_off(7) = mono_off(6) + mono_g*nz*nc1
 if (mono_off(7) + mono_g*nz*nc1 > size(edge_work)) error stop 'mono_views: edge_work too small for the group'
 n = mono_g*nz
 o1 = mono_off(1) + (ne1-1)*n
 o3 = mono_off(3) + (nc1-1)*n
 o4 = mono_off(4) + (nc1-1)*n
 o6 = mono_off(6) + (nc1-1)*n
 o7 = mono_off(7) + (nc1-1)*n
 !$acc parallel loop gang vector default(present) firstprivate(n, o1, o3, o4, o6, o7) async(1)
 do i = 1, n
  edge_work(o1 + i) = 0.0
  edge_work(o3 + i) = 0.0
  edge_work(o4 + i) = 0.0
  edge_work(o6 + i) = 0.0
  edge_work(o7 + i) = 0.0
 end do
 end subroutine

 subroutine mono_group(s0, dts, scalars_old, scalars_new, rho_zz_old, uh_avg, ww_avg, bdy_mask_cell, bdy_mask_edge, cells_on_edge, &
                       cells_on_cell, edges_on_cell, degree, sign_cell, inv_area_cell, dv_edge, n_adv, adv_cells, adv_coefs, &
                       adv_coefs_3rd, fzm, fzp, rdzw, rho_zz_int, flx, snw, sin, sout, wd, so, sn)
 integer, intent(in) :: s0
 real, intent(in) :: dts
 real, intent(in) :: rho_zz_old(nz,nc1), uh_avg(nz,ne1), ww_avg(nz+1,nc1), sign_cell(maxedges,nc1), inv_area_cell(nc1), dv_edge(ne1)
 real, intent(in) :: adv_coefs(max_adv,ne1), adv_coefs_3rd(max_adv,ne1), fzm(nz), fzp(nz), rdzw(nz), rho_zz_int(nz,nc1)
 integer, intent(in) :: bdy_mask_cell(nc1), bdy_mask_edge(ne1), cells_on_edge(2,ne1), cells_on_cell(maxedges,nc1)
 integer, intent(in) :: edges_on_cell(maxedges,nc1), degree(nc1), n_adv(ne1), adv_cells(max_adv,ne1)
 real, intent(in) :: scalars_old(num_scalars,nz,nc1)
 real, intent(inout) :: scalars_new(num_scalars,nz,nc1)
 real, intent(inout) :: flx(nz,mono_g,ne1), snw(nz,mono_g,nc1), sin(nz,mono_g,nc1), sout(nz,mono_g,nc1), wd(nz+1,mono_g,nc1)
 real, intent(inout) :: so(nz,mono_g,nc1), sn(nz,mono_g,nc1)
 real, parameter :: eps = 1.e-20
 real :: scale_factor, scalar_weight, flux_up, f, f_low, f_up, sv, s_out, s_in, fu, ft, s_max, s_min, swa(10,2)
 integer :: i, j, c, e, k, g, cell1, cell2, ii, ica(10)

 ! the group's scalars, level first
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    so(k,g,c) = scalars_old(s0+g-1,k,c)
    sn(k,g,c) = scalars_new(s0+g-1,k,c)
   end do
  end do
 end do

 ! vertical flux (the limiter's bounds are formed in the limiter)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    if (k == 1) then
     wd(1,g,c) = 0.0
     wd(nz+1,g,c) = 0.0
    else if (k == 2) then
     wd(k,g,c) = ww_avg(k,c)*(fzm(k)*sn(k,g,c)+fzp(k)*sn(k-1,g,c))
    else if (k < nz) then
     wd(k,g,c) = flux3(sn(k-2,g,c), sn(k-1,g,c), sn(k,g,c), sn(k+1,g,c), ww_avg(k,c), coef_3rd_order)
    else
     wd(k,g,c) = ww_avg(k,c)*(fzm(k)*sn(k,g,c)+fzp(k)*sn(k-1,g,c))
    end if
   end do
  end do
 end do

 ! high-order horizontal flux (mass flux times 3rd-order edge value): a gang per edge holds its stencil, the vector runs over
 ! (scalar, level)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector_length(64) default(present) private(ica, swa) async(1)
 do e = 1, nedges
  if (n_adv(e) == 10) then
   do j = 1, 10
    ica(j) = adv_cells(j,e)
    swa(j,1) = adv_coefs(j,e) + adv_coefs_3rd(j,e)
    swa(j,2) = adv_coefs(j,e) - adv_coefs_3rd(j,e)
   end do
   !$acc loop vector collapse(2) private(ii)
   do g = 1, mono_g
    do k = 1, nz
     ii = merge(1, 2, uh_avg(k,e) > 0)
     flx(k,g,e) = uh_avg(k,e)*( &
          swa(1,ii)*sn(k,g,ica(1)) + swa(2,ii)*sn(k,g,ica(2)) + &
          swa(3,ii)*sn(k,g,ica(3)) + swa(4,ii)*sn(k,g,ica(4)) + &
          swa(5,ii)*sn(k,g,ica(5)) + swa(6,ii)*sn(k,g,ica(6)) + &
          swa(7,ii)*sn(k,g,ica(7)) + swa(8,ii)*sn(k,g,ica(8)) + &
          swa(9,ii)*sn(k,g,ica(9)) + swa(10,ii)*sn(k,g,ica(10)))
    end do
   end do
  else
   !$acc loop vector collapse(2) private(f, i, c, scalar_weight)
   do g = 1, mono_g
    do k = 1, nz
     f = 0.0
     do i = 1, n_adv(e)
      c = adv_cells(i,e)
      scalar_weight = uh_avg(k,e)*(adv_coefs(i,e) + sign(1.0,uh_avg(k,e))*adv_coefs_3rd(i,e))
      f = f + scalar_weight* sn(k,g,c)
     end do
     flx(k,g,e) = f
    end do
   end do
  end if
 end do

 ! first-order upwind update (into snw), and the vertical part of the incoming/outgoing antidiffusive flux (two kernels: the
 ! second reads the vertical fluxes of the levels above and below as the first leaves them)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(f_low, f_up, sv) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    sv = so(k,g,c)*rho_zz_old(k,c)
    if (k < nz) then
     f_up = dts*(max(0.0,ww_avg(k+1,c))*so(k,g,c) + min(0.0,ww_avg(k+1,c))*so(k+1,g,c))
     sv = sv - f_up*rdzw(k)
    end if
    if (k > 1) then
     f_low = dts*(max(0.0,ww_avg(k,c))*so(k-1,g,c) + min(0.0,ww_avg(k,c))*so(k,g,c))
     sv = sv + f_low*rdzw(k)
     wd(k,g,c) = dts*wd(k,g,c) - f_low
    end if
    snw(k,g,c) = sv
   end do
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    sin(k,g,c) = - rdzw(k)*(min(0.0,wd(k+1,g,c))-max(0.0,wd(k,g,c)))
    sout(k,g,c) = - rdzw(k)*(max(0.0,wd(k+1,g,c))-min(0.0,wd(k,g,c)))
   end do
  end do
 end do

 ! horizontal part of the upwind update and of the incoming/outgoing antidiffusive flux: the upwind flux and the antidiffusive
 ! remainder of each edge as the edge kernel formed them (the two outermost relaxation rows keep upwind only; the ghost edge's
 ! were zero)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(i, e, sv, s_out, s_in, cell1, cell2, fu, ft) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    sv = snw(k,g,c)
    s_out = sout(k,g,c)
    s_in = sin(k,g,c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     if (e <= nedges) then
      cell1 = cells_on_edge(1,e)
      cell2 = cells_on_edge(2,e)
      fu = dv_edge(e) * dts * (max(0.0,uh_avg(k,e))*so(k,g,cell1) + min(0.0,uh_avg(k,e))*so(k,g,cell2))
      ft = dts * flx(k,g,e) - fu
      if (bdy_mask_edge(e) == n_relax_zone .or. bdy_mask_edge(e) == n_relax_zone-1) ft = 0.0
     else
      fu = 0.0
      ft = 0.0
     end if
     sv = sv - sign_cell(i,c) * fu * inv_area_cell(c)
     s_out = s_out - max(0.0,sign_cell(i,c)*ft) * inv_area_cell(c)
     s_in = s_in - min(0.0,sign_cell(i,c)*ft) * inv_area_cell(c)
    end do
    snw(k,g,c) = sv
    sout(k,g,c) = s_out
    sin(k,g,c) = s_in
   end do
  end do
 end do

 ! limiter: the fraction of the antidiffusive flux that keeps the new value inside [s_min, s_max] (the bounds from the old values
 ! around the cell, as the bounds kernel formed them)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(i, s_max, s_min, scale_factor) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    if (k == 1) then
     s_max = max(so(1,g,c),so(2,g,c))
     s_min = min(so(1,g,c),so(2,g,c))
    else if (k < nz) then
     s_max = max(so(k-1,g,c),so(k,g,c),so(k+1,g,c))
     s_min = min(so(k-1,g,c),so(k,g,c),so(k+1,g,c))
    else
     s_max = max(so(k,g,c),so(k-1,g,c))
     s_min = min(so(k,g,c),so(k-1,g,c))
    end if
    if (degree(c) == 6) then
     s_max = max(s_max, &
          so(k,g, cells_on_cell(1,c)), so(k,g, cells_on_cell(2,c)), so(k,g, cells_on_cell(3,c)), &
          so(k,g, cells_on_cell(4,c)), so(k,g, cells_on_cell(5,c)), so(k,g, cells_on_cell(6,c)))
     s_min = min(s_min, &
          so(k,g, cells_on_cell(1,c)), so(k,g, cells_on_cell(2,c)), so(k,g, cells_on_cell(3,c)), &
          so(k,g, cells_on_cell(4,c)), so(k,g, cells_on_cell(5,c)), so(k,g, cells_on_cell(6,c)))
    else
     do i = 1, degree(c)
      s_max = max(s_max,so(k,g, cells_on_cell(i,c)))
      s_min = min(s_min,so(k,g, cells_on_cell(i,c)))
     end do
    end if
    scale_factor = (s_max*rho_zz_int(k,c) - snw(k,g,c)) / (sin(k,g,c) + eps)
    sin(k,g,c) = min(1.0, max(0.0, scale_factor))
    scale_factor = (s_min*rho_zz_int(k,c) - snw(k,g,c)) / (sout(k,g,c) - eps)
    sout(k,g,c) = min(1.0, max(0.0, scale_factor))
   end do
  end do
 end do

 ! limited horizontal fluxes (in flx)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(cell1, cell2, flux_up, f) async(1)
 do e = 1, nedges
  do g = 1, mono_g
   do k = 1, nz
    cell1 = cells_on_edge(1,e)
    cell2 = cells_on_edge(2,e)
    flux_up = dv_edge(e) * dts * (max(0.0,uh_avg(k,e))*so(k,g,cell1) + min(0.0,uh_avg(k,e))*so(k,g,cell2))
    f = dts*flx(k,g,e) - flux_up
    if (bdy_mask_edge(e) == n_relax_zone .or. bdy_mask_edge(e) == n_relax_zone-1) f = 0.0
    f = max(0.0,f) * min(sout(k,g,cell1), sin(k,g,cell2)) &
      + min(0.0,f) * min(sin(k,g,cell1), sout(k,g,cell2))
    flx(k,g,e) = f
   end do
  end do
 end do

 ! limited vertical fluxes
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(f) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 2, nz
    f = wd(k,g,c)
    f = max(0.0,f) * min(sout(k-1,g,c), sin(k,g,c)) &
      + min(0.0,f) * min(sout(k,g,c), sin(k-1,g,c))
    wd(k,g,c) = f
   end do
  end do
 end do

 ! final update with the limited fluxes, divided by the new density, and back into the state (negative round-off removed; only cells
 ! with mask <= n_spec_zone, as in the original)
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(3) default(present) private(i, e, sv) async(1)
 do c = 1, ncells
  do g = 1, mono_g
   do k = 1, nz
    sv = snw(k,g,c)
    do i = 1, degree(c)
     e = edges_on_cell(i,c)
     sv = sv - sign_cell(i,c)*flx(k,g,e) * inv_area_cell(c)
    end do
    sv = (sv + (-rdzw(k)*(wd(k+1,g,c)-wd(k,g,c)) ) )/rho_zz_int(k,c)
#ifdef LEAN_MONO_RELAX
    ! Lean departure (build switch LEAN_MONO_RELAX, default off; docs/fixes.md, LEAN_MONO_RELAX): the original tests
    ! <= nSpecZone (2) here, so relaxation rows 3-5 never receive the monotonic update and keep the stage-2 (dt/2) value,
    ! although its own comment says that only the specified zone (rows 6-7) is set after transport. The switch writes the
    ! relaxation rows back too.
    if (bdy_mask_cell(c) <= n_relax_zone) then
#else
    if (bdy_mask_cell(c) <= n_spec_zone) then
#endif
     scalars_new(s0+g-1,k,c) = max(0.0,sv)
    end if
   end do
  end do
 end do
 end subroutine

 ! Third-order vertical flux (4th-order centred plus an upwind-biased correction).
 pure real function flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3)
 !$acc routine seq
 real, intent(in) :: q_im2, q_im1, q_i, q_ip1, ua, coef3
 flux3 = ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0 + coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0
 end function
end module
