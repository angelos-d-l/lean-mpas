! Shared inputs: poisoned top/ghost values expose wrong loop bounds in either implementation.
module summary_data
 use mpas_kind_types, only: RKIND
 use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
 implicit none
 integer, parameter :: nz = 3, ncells = 4, nedges = 5
 real(kind=RKIND), target :: w(4,5), u(4,6), v(4,6), scalars(8,3,5)
 real(kind=RKIND), target :: lat_cell(5), lon_cell(5), lat_edge(6), lon_edge(6)
 integer, target :: cell_ids(5) = [1,2,3,4,5]
 logical, target :: print_vel, print_detailed, print_sca
contains
 subroutine fill_summary_data(name)
 character(len=*), intent(in) :: name
 integer :: k, c, e, i

 do c = 1, 5
  lat_cell(c) = 0.1*c
  lon_cell(c) = 6.0 - 0.5*c
  do k = 1, 4
   w(k,c) = 0.5*k - 0.25*c
  end do
  do k = 1, nz
   do i = 1, 8
    scalars(i,k,c) = 0.001*i*k*c
   end do
  end do
 end do
 lon_cell(1) = 2.0
 w(4,:) = 99.0
 w(:,5) = -99.0
 scalars(1,1,1) = -1.0e-5
 scalars(:,:,5) = 5.0
 do e = 1, 6
  lat_edge(e) = -0.05*e
  lon_edge(e) = 3.5 + 0.25*e
  do k = 1, 4
   u(k,e) = 10.0*k - 3.0*e
   v(k,e) = 2.0*e - k
  end do
 end do
 lon_edge(2) = 2.5
 ! The u offset must use its actual leading dimension, rather than nz.
 u(4,:) = 888.0
 v(4,:) = 777.0
 u(:,6) = 999.0
 print_vel = .false.
 print_detailed = .false.
 print_sca = .false.
 select case (name)
 case ('vel', 'vel_positive', 'vel_log', 'sca_vel', 'nan_default')
  print_vel = .true.
 case ('detailed', 'detailed_ties', 'sca_detailed', 'nan_w', 'nan_u', 'nan_all_w')
  print_detailed = .true.
  print_vel = name == 'detailed'
 case ('sca_only', 'none')
 case default
  error stop 'unknown summary case'
 end select
 print_sca = name == 'sca_only' .or. name == 'sca_vel' .or. name == 'sca_detailed'
 select case (name)
 case ('vel_positive')
  w(1:nz,1:ncells) = abs(w(1:nz,1:ncells)) + 0.5
 case ('vel_log')
  ! MPAS greece_60km strict_12h log:410-411; samples for its number format only.
  w(1:nz,1:ncells) = 0.0
  w(1,1) = -0.287564
  w(nz,ncells) = 0.266599
  u(1:nz,1:nedges) = 0.0
  u(1,1) = -56.6575
  u(nz,nedges) = 56.9641
 case ('detailed_ties')
  w(2,1) = -3.0
  w(1,3) = -3.0
  w(3,2) = 4.0
  w(1,4) = 4.0
  u(2,2) = -40.0
  u(1,4) = -40.0
  v(2,2) = 0.0
  v(1,4) = 0.0
  u(3,3) = 40.0
  u(1,5) = 40.0
  v(3,3) = 30.0
  v(1,5) = 30.0
 case ('nan_w', 'nan_default')
  w(2,2) = ieee_value(0.0_RKIND, ieee_quiet_nan)
 case ('nan_u')
  u(1,2) = ieee_value(0.0_RKIND, ieee_quiet_nan)
 case ('nan_all_w')
  w(1:nz,1:ncells) = ieee_value(0.0_RKIND, ieee_quiet_nan)
 end select
 end subroutine
end module
