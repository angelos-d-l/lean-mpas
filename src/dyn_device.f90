! Device residency of the dynamics (OpenACC).
! The mesh, the derived coefficients, the state, all work arrays and the lateral boundary data are put on the card once, at startup;
! the kernels run with default(present) and never copy. During the run only what the host-side physics needs crosses the bus
! (model_step in dyn_micro.f90, model_physics in lean_model.f90).
module dyn_device
 use dyn_step, only: dyn_grid, dyn_fields
 use dyn_boundary, only: dyn_lbc
 implicit none
contains

 subroutine device_enter_grid(g)
 type(dyn_grid), intent(in) :: g
 !$acc enter data copyin(g%m%dvEdge, g%m%dcEdge, g%m%fEdge, g%m%angleEdge)
 !$acc enter data copyin(g%m%weightsOnEdge, g%m%nEdgesOnEdge, g%m%edgesOnEdge, g%m%cellsOnEdge)
 !$acc enter data copyin(g%m%verticesOnEdge, g%m%edgesOnCell, g%m%nEdgesOnCell, g%m%edgesOnVertex)
 !$acc enter data copyin(g%m%verticesOnCell, g%m%cellsOnCell, g%m%kiteAreasOnVertex, g%m%fVertex)
 !$acc enter data copyin(g%m%rdzu, g%m%rdzw, g%m%fzm, g%m%fzp)
 !$acc enter data copyin(g%m%zz, g%m%zxu, g%m%u_init, g%m%v_init)
 !$acc enter data copyin(g%m%bdyMaskCell, g%m%bdyMaskEdge, g%m%coeffs_reconstruct)
 !$acc enter data copyin(g%sign_vertex, g%sign_cell, g%zb_cell, g%zb3_cell)
 !$acc enter data copyin(g%kite, g%n_adv, g%adv_cells, g%inv_area_cell)
 !$acc enter data copyin(g%inv_dv, g%inv_dc, g%inv_area_tri, g%del2)
 !$acc enter data copyin(g%del4, g%reg_cell, g%reg_edge, g%adv_coefs)
 !$acc enter data copyin(g%adv_coefs_3rd, g%dc2, g%ds2, g%dcs)
 !$acc enter data copyin(g%dss, g%etp, g%etm, g%ewp)
 !$acc enter data copyin(g%ewm, g%spec_cell, g%spec_edge, g%cos_lat)
 !$acc enter data copyin(g%sin_lat, g%cos_lon, g%sin_lon, g%cos_edge)
 !$acc enter data copyin(g%sin_edge)
 !$acc enter data copyin(g%s%rho_base, g%s%rtheta_base, g%s%exner_base)
 end subroutine

 subroutine device_enter_fields(f)
 type(dyn_fields), intent(inout) :: f
 !$acc enter data copyin(f)
 !$acc enter data copyin(f%u1, f%u2, f%w1, f%w2)
 !$acc enter data copyin(f%theta_m1, f%theta_m2, f%rho_zz1, f%rho_zz2)
 !$acc enter data copyin(f%scalars1, f%scalars2, f%ru, f%rw)
 !$acc enter data copyin(f%rho_p, f%rtheta_p, f%exner, f%pressure_p)
 !$acc enter data copyin(f%ru_save, f%rw_save, f%rho_p_save, f%rtheta_p_save)
 !$acc enter data copyin(f%rho_zz_old_split, f%rho_edge, f%v, f%vorticity)
 !$acc enter data copyin(f%divergence, f%ke, f%pv_vertex, f%pv_edge)
 !$acc enter data copyin(f%ru_p, f%rw_p)   ! (ke_edge, ke_vertex: host only, solve_diagnostics recomputes them on the card)
 !$acc enter data copyin(f%rho_pp, f%rtheta_pp, f%rtheta_pp_old, f%ru_avg)
 !$acc enter data copyin(f%ww_avg, f%ru_avg_split, f%ww_avg_split, f%tend_u)
 !$acc enter data copyin(f%tend_w, f%tend_theta, f%tend_rho, f%tend_u_euler)
 !$acc enter data copyin(f%tend_w_euler, f%tend_theta_euler, f%rthdynten)
 !$acc enter data copyin(f%dpdz)   ! (h_divergence, eddy_visc_horz, delsq_*: host only; the card's are views of edge_work)
 !$acc enter data copyin(f%tend_w_pert)
 !$acc enter data copyin(f%rt_diabatic_tend, f%cqu, f%cqw, f%qtot)
 !$acc enter data copyin(f%cofwr, f%cofwz, f%coftz, f%cofwt)
 !$acc enter data copyin(f%a_tri, f%alpha_tri, f%gamma_tri, f%cofrz)
 !$acc enter data copyin(f%tend_ru_physics, f%tend_rtheta_physics, f%tend_rho_physics, f%tend_scalars)
 !$acc enter data copyin(f%rqvdynten, f%rho_zz_int)   ! (horiz_flux: host only; the card has dyn_scalars edge_work)
 ! (sc_old, sc_new, s_max, s_min, wdtn, scale_arr, flux, flux_upwind, flux_tmp: host only; the card's monotonic transport works
 ! in views of dyn_scalars edge_work, mono_views)
 !$acc enter data copyin(f%u_zonal, f%u_meridional)
 end subroutine

 subroutine device_enter_lbc(b)
 type(dyn_lbc), intent(inout) :: b
 !$acc enter data copyin(b)
 !$acc enter data copyin(b%u, b%ru, b%rho_zz, b%rtheta_m)
 !$acc enter data copyin(b%scalars, b%tend_u, b%tend_ru, b%tend_rho_zz)
 !$acc enter data copyin(b%tend_rtheta_m, b%tend_scalars, b%drv_u, b%drv_ru)
 !$acc enter data copyin(b%drv_rtheta_m, b%drv_rho_zz, b%drv_scalars)
 end subroutine

 ! After lbc_update on the host: the boundary state and tendencies to the card.
 subroutine device_update_lbc(b)
 type(dyn_lbc), intent(inout) :: b
 !$acc update device(b%u, b%ru, b%rho_zz, b%rtheta_m)
 !$acc update device(b%scalars, b%tend_u, b%tend_ru, b%tend_rho_zz)
 !$acc update device(b%tend_rtheta_m, b%tend_scalars)
 end subroutine

 ! The whole dynamics state back to the host (restart write, tests).
 subroutine device_fields_to_host(f)
 type(dyn_fields), intent(inout) :: f
 !$acc update self(f%u1, f%u2, f%w1, f%w2)
 !$acc update self(f%theta_m1, f%theta_m2, f%rho_zz1, f%rho_zz2)
 !$acc update self(f%scalars1, f%scalars2, f%ru, f%rw)
 !$acc update self(f%rho_p, f%rtheta_p, f%exner, f%pressure_p)
 !$acc update self(f%ru_save, f%rw_save, f%rho_p_save, f%rtheta_p_save)
 !$acc update self(f%rho_zz_old_split, f%rho_edge, f%v, f%vorticity)
 !$acc update self(f%divergence, f%ke, f%pv_vertex, f%pv_edge)
 !$acc update self(f%ru_p, f%rw_p)
 !$acc update self(f%rho_pp, f%rtheta_pp, f%rtheta_pp_old, f%ru_avg)
 !$acc update self(f%ww_avg, f%ru_avg_split, f%ww_avg_split, f%tend_u)
 !$acc update self(f%tend_w, f%tend_theta, f%tend_rho, f%tend_u_euler)
 !$acc update self(f%tend_w_euler, f%tend_theta_euler, f%rthdynten)
 !$acc update self(f%dpdz)
 !$acc update self(f%tend_w_pert)
 !$acc update self(f%rt_diabatic_tend, f%cqu, f%cqw, f%qtot)
 !$acc update self(f%cofwr, f%cofwz, f%coftz, f%cofwt)
 !$acc update self(f%a_tri, f%alpha_tri, f%gamma_tri, f%cofrz)
 !$acc update self(f%tend_ru_physics, f%tend_rtheta_physics, f%tend_rho_physics, f%tend_scalars)
 !$acc update self(f%rqvdynten, f%rho_zz_int)
 !$acc update self(f%u_zonal, f%u_meridional)
 end subroutine

end module
