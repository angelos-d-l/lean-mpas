! Unit checks of src/run/lean_memory.f90, run by tests/memory/run.sh:
!   units need              the host and card sums of three cases, printed for tools/lean/memory.py's twin
!   units avail ROOT        what is available under a fixture root (Linux /proc and cgroup files), printed
!   units darwin            the macOS reading: status, positive and below hw.memsize (asserted here)
!   units verdict           the refusal (error) or warning above, at and below what is available, with and without
!                           enforce (asserted)
!   units card              the error and the warning "could not be read" from a failed card call (asserted)
!   units slabs INIT MESH   the glacier columns of an init file in slabs of 7 cells (printed); MESH a separate mesh file
!   units stages            phys_mode's parse of LEAN_CARD / LEAN_CHECK: unset means all, none means none (asserted)
program memory_units
 use iso_fortran_env, only: int64
 use lean_memory
 use lean_memory_terms
 use lean_messages, only: message_list, LOG_ERR, LOG_WARN
 use lean_nc
 use lean_input_fields, only: input_file
 implicit none
 character(len=512) :: mode, arg, arg2
 integer :: fails
 fails = 0
 call get_command_argument(1, mode)
 call get_command_argument(2, arg)
 call get_command_argument(3, arg2)
 select case (trim(mode))
 case ('need')
  call need()
 case ('avail')
  call avail(trim(arg))
 case ('darwin')
  call darwin()
 case ('verdict')
  call verdict()
 case ('card')
  call card()
 case ('slabs')
  call slabs(trim(arg), trim(arg2))
 case ('stages')
  call stages()
 case default
  error stop 'units: need | avail ROOT | darwin | verdict | card | slabs INIT [MESH] | stages'
 end select
 if (fails > 0) then
  print '(a,i0,a)', 'FAIL: ', fails, ' check(s)'
  error stop 1
 end if

contains

 ! An unset LEAN_CARD means every stage and radiation on the card; LEAN_CARD=none none of them;
 ! a list names its stages; LEAN_CHECK adds its stages.
 subroutine stages()
 use phys_mode, only: parse_stages, nstages
 logical :: on(nstages), checked(nstages), radiation, radiation_checked
 call parse_stages('', .false., '', on, checked, radiation, radiation_checked)
 call expect(all(on) .and. radiation .and. .not. any(checked), 'stages: LEAN_CARD unset: all on the card')
 call parse_stages('none', .true., '', on, checked, radiation, radiation_checked)
 call expect(.not. any(on) .and. .not. radiation, 'stages: LEAN_CARD=none: the physics on the host')
 call parse_stages('', .true., '', on, checked, radiation, radiation_checked)
 call expect(.not. any(on) .and. .not. radiation, 'stages: LEAN_CARD empty: the physics on the host')
 call parse_stages('all', .true., '', on, checked, radiation, radiation_checked)
 call expect(all(on) .and. radiation, 'stages: LEAN_CARD=all: all on the card')
 call parse_stages('thompson,radiation', .true., 'gf', on, checked, radiation, radiation_checked)
 call expect(on(1) .and. on(7) .and. checked(7) .and. count(on) == 2 .and. radiation .and. .not. radiation_checked, &
             'stages: LEAN_CARD=thompson,radiation LEAN_CHECK=gf')
 end subroutine

 subroutine expect(ok, what)
 logical, intent(in) :: ok
 character(*), intent(in) :: what
 if (ok) then
  print '(a)', 'ok    '//what
 else
  print '(a)', 'FAIL  '//what
  fails = fails + 1
 end if
 end subroutine

 subroutine need()
 type(memory_case) :: mc(3)
 type(host_need) :: h
 type(card_ledger) :: c
 character(len=8), parameter :: names(3) = [character(len=8) :: 'greece', 'x1', 'athens']
 integer :: i
 mc(1)%ncells = 2337; mc(1)%nedges = 7183; mc(1)%nvertices = 4847; mc(1)%nz = 45
 mc(2)%ncells = 2562; mc(2)%nedges = 7680; mc(2)%nvertices = 5120; mc(2)%nz = 45
 mc(2)%n_glacier = 80; mc(2)%glacier_capacity = 128
 mc(3)%ncells = 46251; mc(3)%nedges = 139361; mc(3)%nvertices = 93111; mc(3)%nz = 45
 do i = 1, 3
  mc(i)%largest_output = int(mc(i)%nedges, int64)*mc(i)%nz
  mc(i)%held = 28
  h = host_needed(mc(i))
  print '(a,1x,a,6(1x,i0))', 'host', trim(names(i)), h%resident, h%start, h%step, h%total, h%headroom, h%need
  mc(i)%card_build = .true.; mc(i)%any_card = .true.; mc(i)%staged_pools = .true.
  mc(i)%card_radiation = .true.; mc(i)%card_thompson = .true.; mc(i)%products = .true.; mc(i)%film = .true.
  mc(i)%mynn_ldc = 2368; mc(i)%mynn_plumes = 2368
  c = card_needed(mc(i), 9744_int64, 98304_int64, 900_int64*1048576_int64, .false., 123_int64*1048576_int64)
  print '(a,1x,a,7(1x,i0))', 'card', trim(names(i)), c%held_q, c%rows, c%stack, c%residual, c%full, c%headroom, more(c)
  c = card_needed(mc(i), 9744_int64, 98304_int64, 900_int64*1048576_int64, .true., 0_int64)
  print '(a,1x,a,7(1x,i0))', 'cardload', trim(names(i)), c%held_q, c%rows, c%stack, c%residual, c%full, c%headroom, more(c)
 end do
 end subroutine

 subroutine avail(root)
 character(*), intent(in) :: root
 type(host_need) :: h
 call host_available(h, root)
 print '(a,1x,i0,1x,a)', 'avail', h%available, trim(h%source)
 end subroutine

 subroutine darwin()
 type(host_need) :: h
 integer(int64) :: memsize
 integer :: u, ios
 call host_available(h, '/')
 print '(a,1x,i0,1x,a)', 'darwin', h%available, trim(h%source)
 call execute_command_line('sysctl -n hw.memsize > memsize.txt')
 open(newunit=u, file='memsize.txt', status='old', action='read', iostat=ios)
 read(u, *, iostat=ios) memsize
 close(u, status='delete')
 call expect(h%available > 0 .and. h%available < memsize, 'available_darwin: free + inactive > 0 and below hw.memsize')
 call expect(trim(h%source) == 'free and inactive pages', 'available_darwin: its source')
 end subroutine

 subroutine verdict()
 type(message_list) :: msgs
 integer(int64), parameter :: m = 1048576_int64
 integer :: side
 do side = SIDE_HOST, SIDE_CARD
  msgs%n = 0
  call memory_verdict(side, 1000*m, 512*m, 999*m, .true., 'REFUSAL-TEXT', msgs)
  call expect(msgs%n == 1 .and. msgs%line(1)%level == LOG_ERR .and. msgs%line(1)%text == 'REFUSAL-TEXT', 'verdict above, enforce: the refusal')
  msgs%n = 0
  call memory_verdict(side, 1000*m, 512*m, 1000*m, .true., 'REFUSAL-TEXT', msgs)
  call expect(msgs%n == 0, 'verdict equal: nothing')
  msgs%n = 0
  call memory_verdict(side, 1000*m, 512*m, 1001*m, .true., 'REFUSAL-TEXT', msgs)
  call expect(msgs%n == 0, 'verdict below: nothing')
  msgs%n = 0
  call memory_verdict(side, 1000*m, 512*m, -1_int64, .true., 'REFUSAL-TEXT', msgs)
  call expect(msgs%n == 0, 'verdict with available unknown: nothing (the report says so)')
  msgs%n = 0
  call memory_verdict(side, 1000*m, 512*m, 999*m, .false., 'REFUSAL-TEXT', msgs)
  call expect(msgs%n == 1 .and. msgs%line(1)%level == LOG_WARN, 'verdict above, no enforce: a warning')
  if (side == SIDE_HOST) then
   call expect(msgs%line(1)%text == 'config_lean_memory_check = .false.: lean_mpas goes on although the mesh does not '// &
               'fit in host memory (1000 MiB needed, 999 MiB available).', 'host warning text')
  else
   call expect(msgs%line(1)%text == 'config_lean_memory_check = .false.: lean_mpas goes on although the mesh does not '// &
               'fit on the card (1000 MiB needed, 999 MiB available).', 'card warning text')
  end if
 end do
 end subroutine

 subroutine card()
 type(message_list) :: msgs
 type(card_ledger) :: c, q
 c%failed_call = 'cudaMemGetInfo'
 c%status = 100
 q%rows = 700*1048576_int64
 q%headroom = 256*1048576_int64
 call card_verdict(c, q, .true., 'REFUSAL-TEXT', msgs)
 call expect(msgs%n == 1 .and. msgs%line(1)%level == LOG_ERR, 'card_failed: one ERR')
 call expect(msgs%line(1)%text == 'lean_mpas cannot read the card''s memory (cudaMemGetInfo returned 100): set '// &
             'config_lean_memory_check = .false. in &lean to run without the card part of the check.', 'card_failed: error text')
 msgs%n = 0
 call card_verdict(c, q, .false., 'REFUSAL-TEXT', msgs)
 call expect(msgs%n == 1 .and. msgs%line(1)%level == LOG_WARN, 'card_failed without enforce: one WARN')
 call expect(msgs%line(1)%text == 'config_lean_memory_check = .false.: lean_mpas goes on although the card''s memory '// &
             'could not be read (956 MiB needed, available unknown).', 'card_failed without enforce: warning text')
 end subroutine

 subroutine slabs(init_path, mesh_path)
 character(*), intent(in) :: init_path, mesh_path
 type(input_file) :: init, mesh
 type(message_list) :: msgs
 integer :: n, status, ncells
 status = nc_open(init%nc, init_path)
 if (status /= NC_NOERR) error stop 'slabs: init'
 init%path = init_path
 init%role = 'input'
 init%record = 0
 status = nc_inq_dim(init%nc, 'Time', n)
 if (status == NC_NOERR .and. n > 0) init%record = 1
 if (len(mesh_path) > 0) init%record = n            ! the last record: the two-record fixture starts at its second
 status = nc_inq_dim(init%nc, 'nCells', ncells)
 if (len(mesh_path) > 0) then
  status = nc_open(mesh%nc, mesh_path)
  mesh%path = mesh_path
  mesh%role = 'input'
  mesh%record = 0
  n = count_glacier(init, mesh, .false., ncells, 7, msgs)
 else
  n = count_glacier(init, mesh, .true., ncells, 7, msgs)
 end if
 print '(a,1x,i0)', 'glacier', n
 end subroutine
end program
