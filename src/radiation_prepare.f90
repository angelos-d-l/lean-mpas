! The interface temperatures and hydrostatic pressures RRTMG needs, from the layer temperature, pressure and moist density
! of a column (the expressions of MPAS_to_physics, physics/mpas_atmphys_interface.F:480-540).
module radiation_prepare
 use config,only: nz
 use mpas_atmphys_constants,only: gravity
 implicit none
 private
 public :: radiation_interfaces
contains
 recursive subroutine radiation_interfaces(temperature,pressure,rho,dz,z,interface_t,hydro_layer,hydro_interface)
 ! In: dry temperature, full pressure, moist density and layer thickness. Out: interface temperatures (linear in height,
 ! extrapolated at the bottom and top), the hydrostatic interface pressures integrated down from the extrapolated top
 ! value, and the layer means. recursive: called from the OpenMP radiation loop, its locals on each thread's stack.
 real,intent(in) :: temperature(nz),pressure(nz),rho(nz),dz(nz),z(nz+1)
 real,intent(out) :: interface_t(nz+1),hydro_layer(nz),hydro_interface(nz+1)
 real :: tem1,fzm,fzp,z0,z1,z2,w1,w2
 integer :: k
 do k=2,nz
  tem1=1./(z(k+1)-z(k-1))
  fzm=(z(k)-z(k-1))*tem1
  fzp=(z(k+1)-z(k))*tem1
  interface_t(k)=fzm*temperature(k)+fzp*temperature(k-1)
 enddo
 k=nz+1
 z0=z(k)
 z1=0.5*(z(k)+z(k-1))
 z2=0.5*(z(k-1)+z(k-2))
 w1=(z0-z2)/(z1-z2)
 w2=1.-w1
 interface_t(k)=w1*temperature(k-1)+w2*temperature(k-2)
 hydro_interface(k)=exp(w1*log(pressure(k-1))+w2*log(pressure(k-2)))
 k=1
 z0=z(k)
 z1=0.5*(z(k)+z(k+1))
 z2=0.5*(z(k+1)+z(k+2))
 w1=(z0-z2)/(z1-z2)
 w2=1.-w1
 interface_t(k)=w1*temperature(k)+w2*temperature(k+1)
 do k=nz,1,-1
  hydro_interface(k)=hydro_interface(k+1)+gravity*rho(k)*dz(k)
 enddo
 do k=nz,1,-1
  hydro_layer(k)=0.5*(hydro_interface(k+1)+hydro_interface(k))
 enddo
 end subroutine
end module
