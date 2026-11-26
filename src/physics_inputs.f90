! Dynamics state -> the inputs of the physics columns, at the start of every model step: the subset of MPAS_to_physics
! (mpas_atmphys_interface.F) that our schemes read, with the same expressions and operation order. One call per cell.
! Real conversions only (modified theta -> theta, mixing ratios limited to >= 0, density with the vapour, the hydrostatic
! pressures); everything else the physics keeps between steps (turbulence, land, convection memory) is not touched here.
module physics_inputs
 use config
 use dyn_config, only: num_scalars, i_qv, i_qc, i_qr, i_qi, i_qs, i_qg, i_ni, i_nr, rgas, rv
 use model_state
 use gf_state
 implicit none
contains

 ! Pressure at the two lowest interfaces from the pressure of the first two layers, the way lsm_noahmp_sounding_fromMPAS does it (mpas_atmphys_driver_lsm_noahmp.F
 ! 612-639): extrapolated to the surface with the weights of the layer-centre heights, and interpolated to the top of the first layer.
 subroutine noahmp_interface_pressure(z, p, p_first, p_second)
 real, intent(in) :: z(nz+1), p(nz)
 real, intent(out) :: p_first, p_second
 real :: z0, z1, z2, w1, w2, mult, fzm, fzp
 z0 = z(1)
 z1 = 0.5*(z(1)+z(2))
 z2 = 0.5*(z(2)+z(3))
 w1 = (z0-z2)/(z1-z2)
 w2 = 1. - w1
 p_first = w1*p(1) + w2*p(2)
 mult = 1./(z(3)-z(1))
 fzm = mult*(z(2)-z(1))
 fzp = mult*(z(3)-z(2))
 p_second = fzm*p(2) + fzp*p(1)
 end subroutine

 ! rho_zz, theta_m, scalars, exner, pressure_p, w and the reconstructed winds are the dynamics fields of the column; zz, zgrid,
 ! pressure_base the static mesh arrays. The column view a receives the physics inputs, f the surface pressure of Grell-Freitas.
 subroutine physics_inputs_cell(a, f, rho_zz, theta_m, scalars, exner, pressure_p, w, u_zonal, u_meridional, zz, zgrid, pressure_base)
 type(column_view), intent(in) :: a
 type(gf_column), intent(in) :: f
 real, intent(in) :: rho_zz(nz), theta_m(nz), scalars(num_scalars,nz), exner(nz), pressure_p(nz), w(nz+1)
 real, intent(in) :: u_zonal(nz), u_meridional(nz), zz(nz), zgrid(nz+1), pressure_base(nz)
 real :: interfaces(nz+1), rho1, rho2, tem1, tem2, correction, z0, z1, z2, w1, w2
 integer :: k
 do k = 1, nz
  a%qv(k) = max(0., scalars(i_qv,k))
  a%qc(k) = max(0., scalars(i_qc,k))
  a%qr(k) = max(0., scalars(i_qr,k))
  a%qi(k) = max(0., scalars(i_qi,k))
  a%qs(k) = max(0., scalars(i_qs,k))
  a%qg(k) = max(0., scalars(i_qg,k))
  a%ni(k) = max(0., scalars(i_ni,k))
  a%nr(k) = max(0., scalars(i_nr,k))
  a%u(k) = u_zonal(k)
  a%v(k) = u_meridional(k)
  a%rho_moist(k) = zz(k) * rho_zz(k)
  a%rho_moist(k) = a%rho_moist(k) * (1. + a%qv(k))
  a%theta(k) = theta_m(k) / (1. + rv/rgas * a%qv(k))
  a%exner(k) = exner(k)
  a%p(k) = pressure_p(k) + pressure_base(k)
  a%dz(k) = zgrid(k+1) - zgrid(k)
 end do
 a%w = w
 ! surface pressure from the first two layers (the original reads rho_zz and zz again, the same numbers)
 tem1 = zgrid(2) - zgrid(1)
 tem2 = zgrid(3) - zgrid(2)
 rho1 = rho_zz(1) * zz(1) * (1. + a%qv(1))
 rho2 = rho_zz(2) * zz(2) * (1. + a%qv(2))
 correction = 0.5*9.80616*(zgrid(2)-zgrid(1)) * (rho1 - 0.5*(rho2-rho1)*tem1/(tem1+tem2))
 a%psfc = correction + pressure_p(1) + pressure_base(1)
 ! hydrostatic pressure: interfaces integrated down from the top value extrapolated in log pressure
 z0 = zgrid(nz+1)
 z1 = 0.5*(zgrid(nz+1)+zgrid(nz))
 z2 = 0.5*(zgrid(nz)+zgrid(nz-1))
 w1 = (z0-z2)/(z1-z2)
 w2 = 1. - w1
 interfaces(nz+1) = exp(w1*log(a%p(nz)) + w2*log(a%p(nz-1)))
 do k = nz, 1, -1
  interfaces(k) = interfaces(k+1) + 9.80616*a%rho_moist(k)*a%dz(k)
 end do
 do k = nz, 1, -1
  a%p_hyd(k) = 0.5*(interfaces(k+1)+interfaces(k))
 end do
 f%surface_pressure = interfaces(1)
 end subroutine
end module
