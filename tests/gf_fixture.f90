! One GF column in the (1, nz+1, 1) layout of MPAS's cu_grell_freitas arguments, for the synthetic soundings of
! tests/gf_synthetic.f90; 45 levels, the level count of those soundings.
module gf_fixture
 implicit none
 integer,parameter,private :: nz=45
 type gf_sample
  real :: area(1,1),dx(1,1),ht(1,1),xland(1,1),hfx(1,1),qfx(1,1),gsw(1,1)
  integer :: kpbl(1,1),k22(1,1),kbcon(1,1),ktop(1,1)
  real :: hbot(1,1),htop(1,1),xmb(1,1),xmbs(1,1),rain(1,1),prate(1,1)
  real :: u(1,nz+1,1),v(1,nz+1,1),t(1,nz+1,1),w(1,nz+1,1),q(1,nz+1,1)
  real :: p(1,nz+1,1),pi(1,nz+1,1),rho(1,nz+1,1),dz(1,nz+1,1),p8w(1,nz+1,1)
  real :: thbl(1,nz+1,1),qvbl(1,nz+1,1),thdyn(1,nz+1,1),qvdyn(1,nz+1,1),rad(1,nz+1,1)
  real :: thcu(1,nz+1,1),qvcu(1,nz+1,1),qccu(1,nz+1,1),qicu(1,nz+1,1)
  real :: gdc(1,nz+1,1),gdc2(1,nz+1,1)
 end type
end module
