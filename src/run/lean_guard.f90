! The two refusals before a run writes anything. lean_mpas is one process that writes MPAS's output names in the run
! directory, so it refuses an MPI launch of more than one task (each task would write the same files) and takes an
! advisory lock on the directory, so that a second run there stops instead of overwriting the first one's files.
module lean_guard
 use iso_c_binding, only: c_int, c_ptr, c_char, c_size_t, c_null_ptr, c_null_char, c_associated, c_f_pointer
 use mpas_kind_types, only: StrKIND
 use lean_build_info, only: errno_location, errno_would_block
 implicit none
 private
 public :: mpi_launch_check, lock_state_of, lock_run_directory
 integer, parameter, public :: LOCK_TAKEN = 0, LOCK_HELD = 1, LOCK_UNAVAILABLE = 2

 type(c_ptr) :: run_directory = c_null_ptr
 logical :: directory_locked = .false.

 interface
  function c_opendir(path) bind(C, name='opendir') result(directory)
   import :: c_ptr, c_char
   character(kind=c_char), intent(in) :: path(*)
   type(c_ptr) :: directory
  end function

  integer(c_int) function c_dirfd(directory) bind(C, name='dirfd')
   import :: c_int, c_ptr
   type(c_ptr), value :: directory
  end function

  integer(c_int) function c_flock(fd, operation) bind(C, name='flock')
   import :: c_int
   integer(c_int), value :: fd, operation
  end function

  function c_strerror(err) bind(C, name='strerror') result(text)
   import :: c_int, c_ptr
   integer(c_int), value :: err
   type(c_ptr) :: text
  end function

  function c_strlen(text) bind(C, name='strlen') result(length)
   import :: c_ptr, c_size_t
   type(c_ptr), value :: text
   integer(c_size_t) :: length
  end function
 end interface

contains

 subroutine mpi_launch_check(refused, text)
 logical, intent(out) :: refused
 character(len=:), allocatable, intent(out) :: text
 character(len=StrKIND), parameter :: variables(9) = [character(len=StrKIND) :: &
  'OMPI_COMM_WORLD_SIZE', 'PMI_SIZE', 'MV2_COMM_WORLD_SIZE', 'SLURM_STEP_NUM_TASKS', &
  'OMPI_COMM_WORLD_RANK', 'PMI_RANK', 'MV2_COMM_WORLD_RANK', 'SLURM_PROCID', 'PMIX_RANK']
 integer, parameter :: allowed(9) = [1, 1, 1, 1, 0, 0, 0, 0, 0]
 character(len=:), allocatable :: value
 integer :: i, length, status, number

 refused = .false.
 text = ''
 ! SLURM_NTASKS describes the allocation, not a launched step: a batch script may start one copy.
 do i = 1, size(variables)
  call get_environment_variable(trim(variables(i)), length=length, status=status)
  if (status /= 0 .or. length == 0) cycle
  if (allocated(value)) deallocate(value)
  allocate(character(len=length) :: value)
  call get_environment_variable(trim(variables(i)), value=value, status=status)
  if (status /= 0) cycle
  ! A list-directed null field (',' or '/') can succeed without assigning the integer.
  number = 0
  read(value, *, iostat=status) number
  if (status /= 0) cycle
  if (number <= allowed(i)) cycle
  refused = .true.
  text = 'lean_mpas: an MPI launcher started it with more than one task ('//trim(variables(i))//'='//value// &
   '). lean_mpas runs as one process: start it without mpirun, mpiexec or srun, or with one task. Nothing was written.'
  return
 end do
 end subroutine

 integer function lock_state_of(rc, err) result(state)
 integer(c_int), intent(in) :: rc, err

 state = LOCK_UNAVAILABLE
 if (rc == 0_c_int) then
  state = LOCK_TAKEN
 else if (err == errno_would_block) then
  state = LOCK_HELD
 end if
 end function

 integer(c_int) function current_errno() result(err)
 integer(c_int), pointer :: value

 call c_f_pointer(errno_location(), value)
 err = value
 end function

 function error_text(err) result(text)
 integer(c_int), intent(in) :: err
 character(len=:), allocatable :: text
 type(c_ptr) :: address
 character(kind=c_char), pointer :: chars(:)
 integer :: length, i

 ! Measure the C string as lean_nc does for nc_strerror.
 address = c_strerror(err)
 length = int(c_strlen(address))
 call c_f_pointer(address, chars, [length])
 allocate(character(len=length) :: text)
 do i = 1, length
  text(i:i) = chars(i)
 end do
 end function

 subroutine lock_run_directory(state, text)
 integer, intent(out) :: state
 character(len=:), allocatable, intent(out) :: text
 integer(c_int) :: fd, rc, err
 integer :: attempt

 text = ''
 state = LOCK_TAKEN
 if (directory_locked) return
 run_directory = c_opendir('.'//c_null_char)
 err = current_errno()
 if (.not. c_associated(run_directory)) then
  state = LOCK_UNAVAILABLE
  text = error_text(err)
  return
 end if
 fd = c_dirfd(run_directory)
 err = current_errno()
 if (fd < 0_c_int) then
  state = LOCK_UNAVAILABLE
  text = error_text(err)
  return
 end if
 ! LOCK_EX (2) + LOCK_NB (4) on Linux and Darwin. No file is created in the run directory.
 ! EINTR (4 on Linux and Darwin) does not establish whether another run holds the lock.
 do attempt = 1, 10
  rc = c_flock(fd, 6_c_int)
  err = current_errno()
  if (rc == 0_c_int .or. err /= 4_c_int) exit
 end do
 state = lock_state_of(rc, err)
 if (state == LOCK_TAKEN) then
  directory_locked = .true.
 else
  text = error_text(err)
 end if
 ! Keep the DIR and its descriptor: the kernel releases the lock even after SIGKILL.
 end subroutine

end module
