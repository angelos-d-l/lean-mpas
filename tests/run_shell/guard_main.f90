! lean_guard (tests/run_shell/run.py):  guard_main classify|guard
!   classify  prints the lock state of each errno the lock can return
!   guard     the MPI launch check and the run-directory lock; writes proof.txt when both pass
program guard_main
 use iso_c_binding, only: c_int
 use iso_fortran_env, only: error_unit
 use mpas_kind_types, only: StrKIND
 use lean_build_info, only: errno_would_block
 use lean_guard
 implicit none
 character(len=StrKIND) :: mode
 character(len=:), allocatable :: text
 logical :: refused
 integer :: state, unit, i
 integer(c_int) :: errors(9)

 call get_command_argument(1, mode)
 if (trim(mode) == 'classify') then
  ! 0 (lock taken), EWOULDBLOCK (held by another run), then the errors that mean no lock is possible: EBADF 9,
  ! ENOLCK 37 (Linux) / 77 (macOS), ENOSYS 38 / 78, EOPNOTSUPP 95 / 45. Printed states: 0 taken, 1 held, 2 unavailable.
  errors = [0_c_int, errno_would_block, 9_c_int, 37_c_int, 38_c_int, 45_c_int, 77_c_int, 78_c_int, 95_c_int]
  write(*, '(9(i0,1x))') lock_state_of(0_c_int, errors(1)), (lock_state_of(-1_c_int, errors(i)), i=2,9)
  stop
 end if
 if (trim(mode) /= 'guard') error stop 99
 call mpi_launch_check(refused, text)
 if (refused) then
  write(error_unit, '(a)') text
  error stop 2
 end if
 call lock_run_directory(state, text)
 select case (state)
 case (LOCK_HELD)
  write(error_unit, '(a)') 'lean_mpas: another run holds this run directory (its lock is taken). '// &
   'One run per directory. Nothing was written.'
  error stop 2
 case (LOCK_UNAVAILABLE)
  write(*, '(a)') 'unavailable: '//text
 end select
 open(newunit=unit, file='proof.txt', status='replace', action='write')
 write(unit, '(a)') 'guard passed'
 close(unit)
end program
