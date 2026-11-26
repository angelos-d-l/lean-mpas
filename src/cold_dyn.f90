! Cold start of the dynamics: what MPAS computes at a start from the input stream instead of the restart.
! - start_mesh_vectors: edge normals and wind reconstruction coefficients, recomputed as at every MPAS start, a restart
!   too (mpas_atm_core.F:543-544: mpas_rbf_interp_initialize, mpas_init_reconstruct; mesh_vectors.f90). The values a
!   restart file holds come from the build that wrote it and can differ from a fresh computation in the last bits.
! - cold_dyn_state: the coupled state of atm_init_coupled_diagnostics (dynamics/mpas_atm_time_integration.F:7440-7665)
!   from theta, rho, the base state, u, w and the scalars of the init file (qv qc qr; qi qs qg ni nr when the file holds
!   them, as MPAS's read of the scalars var_array, otherwise 0). ru_p, rw_p and rt_diabatic_tend start at 0, as fields
!   the input stream does not set.
module cold_dyn
 use config, only: ncells, nedges, nz, nc1, ne1, maxedges
 use dyn_config, only: rgas, cp, rv, num_scalars, i_qv, i_qc, i_qr, i_qi, i_qs, i_qg, i_ni, i_nr
 use dyn_mesh_data, only: dyn_mesh_type
 use dyn_state_data, only: dyn_state_type
 use cold_dyn_data, only: cold_dyn_type
 use mesh_vectors, only: init_vectors, init_reconstruct
 implicit none
 private
 public :: start_mesh_vectors, cold_dyn_state
contains
 subroutine start_mesh_vectors(m)
 type(dyn_mesh_type), intent(inout) :: m
 real, allocatable :: planes(:,:,:)
 allocate(planes(3,2,nc1))
 call init_vectors(m%xCell, m%yCell, m%zCell, m%xEdge, m%yEdge, m%zEdge, m%cellsOnEdge, m%edgesOnCell, m%edgeNormalVectors, planes)
 call init_reconstruct(m%xCell, m%yCell, m%zCell, m%xEdge, m%yEdge, m%zEdge, m%edgesOnCell, m%nEdgesOnCell, &
                       m%edgeNormalVectors, planes, m%coeffs_reconstruct)
 end subroutine

 ! sign_cell, zb_cell and zb3_cell (the latter times coef_3rd_order) as setup_grid makes them (dyn_startup edge_signs,
 ! couple_zb3): MPAS couples zb3_cell before this routine runs. f: the fields lean_inputs read from the init file.
 subroutine cold_dyn_state(f, m, sign_cell, zb_cell, zb3_cell, s)
 type(cold_dyn_type), intent(in) :: f
 type(dyn_mesh_type), intent(in) :: m
 real, intent(in) :: sign_cell(maxedges,nc1), zb_cell(nz+1,maxedges,nc1), zb3_cell(nz+1,maxedges,nc1)
 type(dyn_state_type), intent(inout) :: s
 real, parameter :: rvord = rv / rgas
 real :: rcv, p0, flux
 integer :: i, k, c, e, cell1, cell2
 allocate(s%u(nz,ne1), s%w(nz+1,nc1), s%theta_m(nz,nc1), s%rho_zz(nz,nc1), s%scalars(num_scalars,nz,nc1), s%exner(nz,nc1), &
          s%exner_base(nz,nc1), s%rtheta_base(nz,nc1), s%rho_base(nz,nc1), s%rtheta_p(nz,nc1), s%rho_p(nz,nc1), &
          s%pressure_p(nz,nc1), s%pressure_base(nz,nc1), s%ru(nz,ne1), s%rw(nz+1,nc1), s%ru_p(nz,ne1), s%rw_p(nz+1,nc1), &
          s%rt_diabatic_tend(nz,nc1))
 s%u = f%u; s%w = f%w; s%rho_base = f%rho_base
 s%scalars = 0.; s%scalars(i_qv,:,:) = f%qv; s%scalars(i_qc,:,:) = f%qc; s%scalars(i_qr,:,:) = f%qr
 if (allocated(f%qi)) s%scalars(i_qi,:,:) = f%qi
 if (allocated(f%qs)) s%scalars(i_qs,:,:) = f%qs
 if (allocated(f%qg)) s%scalars(i_qg,:,:) = f%qg
 if (allocated(f%ni)) s%scalars(i_ni,:,:) = f%ni
 if (allocated(f%nr)) s%scalars(i_nr,:,:) = f%nr
 s%theta_m = 0.; s%rho_zz = 0.; s%exner = 0.; s%exner_base = 0.; s%rtheta_base = 0.; s%rtheta_p = 0.; s%rho_p = 0.
 s%pressure_p = 0.; s%pressure_base = 0.; s%ru = 0.; s%rw = 0.; s%ru_p = 0.; s%rw_p = 0.; s%rt_diabatic_tend = 0.

 rcv = rgas / (cp-rgas)
 p0 = 1.e5
 do c = 1, ncells
  do k = 1, nz
   s%theta_m(k,c) = f%theta(k,c) * (1.0 + rvord * s%scalars(i_qv,k,c))
   s%rho_zz(k,c) = f%rho(k,c) / m%zz(k,c)
  end do
 end do
 do e = 1, nedges
  cell1 = m%cellsOnEdge(1,e)
  cell2 = m%cellsOnEdge(2,e)
  do k = 1, nz
   s%ru(k,e) = 0.5 * s%u(k,e) * (s%rho_zz(k,cell1) + s%rho_zz(k,cell2))
  end do
 end do
 do c = 1, ncells
  s%rw(1,c) = 0.0
  s%rw(nz+1,c) = 0.0
  do k = 2, nz
   s%rw(k,c) = s%w(k,c) * (m%fzp(k) * s%rho_zz(k-1,c) + m%fzm(k) * s%rho_zz(k,c)) &
                        * (m%fzp(k) * m%zz(k-1,c) + m%fzm(k) * m%zz(k,c))
  end do
 end do
 do c = 1, ncells
  do i = 1, m%nEdgesOnCell(c)
   e = m%edgesOnCell(i,c)
   do k = 2, nz
    flux = (m%fzm(k)*s%ru(k,e) + m%fzp(k)*s%ru(k-1,e))
    s%rw(k,c) = s%rw(k,c) - sign_cell(i,c) * (zb_cell(k,i,c) + sign(1.0,flux) * zb3_cell(k,i,c))*flux &
                            * (m%fzp(k) * m%zz(k-1,c) + m%fzm(k) * m%zz(k,c))
   end do
  end do
 end do
 do c = 1, ncells
  do k = 1, nz
   s%rho_p(k,c) = s%rho_zz(k,c) - s%rho_base(k,c)
  end do
 end do
 do c = 1, ncells
  do k = 1, nz
   s%rtheta_base(k,c) = f%theta_base(k,c) * s%rho_base(k,c)
  end do
 end do
 do c = 1, ncells
  do k = 1, nz
   s%rtheta_p(k,c) = s%theta_m(k,c) * s%rho_p(k,c) + s%rho_base(k,c) * (s%theta_m(k,c) - f%theta_base(k,c))
  end do
 end do
 do c = 1, ncells
  do k = 1, nz
   s%exner(k,c) = (m%zz(k,c) * (rgas/p0) * (s%rtheta_p(k,c) + s%rtheta_base(k,c)))**rcv
   s%exner_base(k,c) = (m%zz(k,c) * (rgas/p0) * (s%rtheta_base(k,c)))**rcv
  end do
 end do
 do c = 1, ncells
  do k = 1, nz
   s%pressure_p(k,c) = m%zz(k,c) * rgas * (s%exner(k,c) * s%rtheta_p(k,c) + s%rtheta_base(k,c) * (s%exner(k,c) - s%exner_base(k,c)))
   s%pressure_base(k,c) = m%zz(k,c) * rgas * s%exner_base(k,c) * s%rtheta_base(k,c)
  end do
 end do
 end subroutine
end module
