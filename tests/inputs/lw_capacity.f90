! check_lw_start (src/lean_model.f90) on a one-cell column:  lw_capacity TOP_PRESSURE_PA [NLEVELS, default 55].
! RRTMG LW uses the model levels plus about one added layer per 4 hPa of top pressure, at most the executable's
! max_lw_layers in all; tests/inputs/run.sh expects 3600 Pa (64 layers) to start, 4000 Pa (65) to stop, and a 1-level
! mesh to stop before any interpolation.
program lw_capacity
 use config
 use model_data, only: m, s
 use lean_model, only: check_lw_start
 implicit none
 character(16) :: arg
 real :: pressure
 integer :: k, nlevels
 character(512) :: path
 call get_command_argument(1, arg)
 read(arg,*) pressure
 nlevels=55
 path='capacity.init.nc'
 call get_command_argument(2,arg)
 if (len_trim(arg)>0) read(arg,*) nlevels
 call set_mesh_sizes(1, 1, 1, nlevels)
 ! One-level rejection must not inspect even an allocated mesh profile.
 if (nlevels < 2) call check_lw_start(trim(path))
 allocate(m%zgrid(nz+1,nc1), s%pressure_base(nz,nc1))
 do k = 1, nz+1
  m%zgrid(k,:) = real(k-1)*500.
 end do
 s%pressure_base = pressure
 call check_lw_start(trim(path))
 print '(a)', 'LW CAPACITY START OK'
end program
