! Physics-to-dynamics handoff for the supported physics suite (MYNN PBL, Grell-Freitas, RRTMG long/short wave; Thompson acts
! after the dynamics and is not part of this handoff): tend_toEdges of mpas_atmphys_todynamics, the east/north unit vectors
! of init_dirs_forphys, and the Grell-Freitas dynamics forcing (rqvdynten, rthdynten) and negative-scalar clipping that
! atm_srk3 does after the scalar transport.
! Same arithmetic and summation order as MPAS 8.4.2 ("the original" below); arrays follow the original layout.
! The coupling of each scheme's tendency with the density (physics_get_tend_work) is in physics_tendency.f90.
module dyn_handoff
 use config
 use dyn_config
 implicit none
contains

 ! Unit vectors pointing east and north at each cell centre (the last slot stays zero).
 subroutine east_north(lat_cell, lon_cell, east, north)
 real, intent(in) :: lat_cell(nc1), lon_cell(nc1)
 real, intent(out) :: east(3,nc1), north(3,nc1)
 integer :: c
 real :: mi
 east = 0.
 north = 0.
 do c = 1, ncells
  east(1,c) = -sine(lon_cell(c))
  east(2,c) =  cosine(lon_cell(c))
  east(3,c) =  0.0
  mi = 1.0 / sqrt(east(1,c)**2 + east(2,c)**2 + east(3,c)**2)
  east(1,c) = east(1,c) * mi
  east(2,c) = east(2,c) * mi
  east(3,c) = east(3,c) * mi

  north(1,c) = -cosine(lon_cell(c))*sine(lat_cell(c))
  north(2,c) = -sine(lon_cell(c))*sine(lat_cell(c))
  north(3,c) =  cosine(lat_cell(c))
  mi = 1.0 / sqrt(north(1,c)**2 + north(2,c)**2 + north(3,c)**2)
  north(1,c) = north(1,c) * mi
  north(2,c) = north(2,c) * mi
  north(3,c) = north(3,c) * mi
 end do
 end subroutine

 ! sin and cos as separate calls. The original keeps them separate (its arrays may alias, so it reloads the angle);
 ! with the angle known to be unchanged an optimising compiler merges sin(x) and cos(x) into one sincos call whose last
 ! bit can differ from the separate calls, depending on the compiler and math library. Startup code, the call cost does not matter.
 function sine(x)
 !GCC$ ATTRIBUTES noinline :: sine
 real, intent(in) :: x
 real :: sine
 sine = sin(x)
 end function
 function cosine(x)
 !GCC$ ATTRIBUTES noinline :: cosine
 real, intent(in) :: x
 real :: cosine
 cosine = cos(x)
 end function

 ! Cell-centre zonal and meridional tendencies to the edge-normal component (half of each neighbour's projection).
 subroutine tend_to_edges(ux_tend, uy_tend, cells_on_edge, edge_normal, east, north, u_tend)
 real, intent(in) :: ux_tend(nz,nc1), uy_tend(nz,nc1), edge_normal(3,ne1), east(3,nc1), north(3,nc1)
 integer, intent(in) :: cells_on_edge(2,ne1)
 real, intent(out) :: u_tend(nz,ne1)
 integer :: e, k, cell1, cell2
 !$omp do schedule(dynamic,64)
 do e = 1, nedges
  cell1 = cells_on_edge(1,e)
  cell2 = cells_on_edge(2,e)
  do k = 1, nz
   u_tend(k,e) = ux_tend(k,cell1) * 0.5 * (edge_normal(1,e) * east(1,cell1) &
                                         + edge_normal(2,e) * east(2,cell1) &
                                         + edge_normal(3,e) * east(3,cell1)) &
               + uy_tend(k,cell1) * 0.5 * (edge_normal(1,e) * north(1,cell1) &
                                         + edge_normal(2,e) * north(2,cell1) &
                                         + edge_normal(3,e) * north(3,cell1)) &
               + ux_tend(k,cell2) * 0.5 * (edge_normal(1,e) * east(1,cell2) &
                                         + edge_normal(2,e) * east(2,cell2) &
                                         + edge_normal(3,e) * east(3,cell2)) &
               + uy_tend(k,cell2) * 0.5 * (edge_normal(1,e) * north(1,cell2) &
                                         + edge_normal(2,e) * north(2,cell2) &
                                         + edge_normal(3,e) * north(3,cell2))
  end do
 end do
 end subroutine

 ! After the scalar transport: the tendency of water vapour due to the dynamics (Grell-Freitas input) and the
 ! correction of the dynamics heating for the change of moisture. scalars_start is the time-level-1 state after the
 ! transport (the monotonic routine updated it), scalars_end the transported state, theta_m the working level.
 subroutine gf_dynamics_forcing(scalars_start, scalars_end, theta_m, rqvdynten, rthdynten)
 real, intent(in) :: scalars_start(num_scalars,nz,nc1), scalars_end(num_scalars,nz,nc1), theta_m(nz,nc1)
 real, intent(out) :: rqvdynten(nz,nc1)
 real, intent(inout) :: rthdynten(nz,nc1)
 integer :: k, c
 real :: fac_m, theta_local
 ! the transport is monotonic in the configuration lean_mpas supports, so the water-vapour tendency is the difference over the step
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, nc1
  do k = 1, nz
   rqvdynten(k,c) = ( scalars_end(i_qv,k,c) - scalars_start(i_qv,k,c) ) / dt
  end do
 end do
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, ncells
  do k = 1, nz
   fac_m = 1./(1. + rv/rgas*scalars_end(i_qv,k,c))
   theta_local = theta_m(k,c)*fac_m
   rthdynten(k,c) = fac_m*(rthdynten(k,c)-theta_local*rv/rgas*rqvdynten(k,c))
  end do
 end do
 end subroutine

 ! Negative mixing ratios and number concentrations after the transport are set to zero.
 subroutine clip_negative_scalars(scalars)
 real, intent(inout) :: scalars(num_scalars,nz,nc1)
 integer :: c, k, iq
 !$omp do schedule(dynamic,64)
 !$acc parallel loop gang vector collapse(2) default(present) async(1)
 do c = 1, nc1
  do k = 1, nz
   do iq = 1, num_scalars
    if (scalars(iq,k,c) < 0.0) scalars(iq,k,c) = 0.0
   end do
  end do
 end do
 end subroutine
end module
