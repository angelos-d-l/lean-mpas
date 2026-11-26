! The lbc_in stream's records: which record each boundary read of the run takes, by the file's
! xtime with MPAS's seek rules, and the read itself. Every read is resolved at startup, so that a missing record stops
! the run before its first step; during the run each record is read when it is due.
module lean_lbc
 use iso_fortran_env, only: int64
 use ieee_arithmetic, only: ieee_is_finite
 use config, only: ncells, nedges, nvertices, nz, nc1, ne1
 use lean_nc
 use lean_messages, only: message_list, add_message, int_text, LOG_WARN, LOG_ERR, LOG_CRIT
 use lean_input_fields
 use ESMF, only: ESMF_TimeIntervalGet, ESMF_TimeIntervalSet
 use lean_clock, only: ESMF_Time, ESMF_TimeInterval, parse_time, time_string, operator(+), operator(-), operator(>=)
 use dyn_lbc_data, only: dyn_lbc_type
 implicit none
 private
 public :: lbc_request, lbc_reader, LBC_LATEST_BEFORE, LBC_EARLIEST_STRICTLY_AFTER
 public :: xtime_seconds, lbc_open, lbc_due, lbc_next

 ! MPAS_STREAM_LATEST_BEFORE and MPAS_STREAM_EARLIEST_STRICTLY_AFTER (framework/mpas_stream_manager.F)
 integer, parameter :: LBC_LATEST_BEFORE = 1, LBC_EARLIEST_STRICTLY_AFTER = 2

 type lbc_request                    ! one read the run will make
  integer(int64) :: at_s = 0         ! the step time of the read, seconds after the start
  integer :: step = 1
  integer :: whence = 0
  character(len=:), allocatable :: file     ! the file MPAS opens for at_s
  character(len=:), allocatable :: retest   ! the file one filename_interval away (earlier for LATEST_BEFORE); '' for 'none'
 end type

 type lbc_reader
  integer :: n = 0, next = 1
  integer(int64), allocatable :: at_s(:), valid_s(:)   ! when each read is due, and its record's time; seconds after the start
  character(len=512), allocatable :: file(:)
  integer, allocatable :: record(:), step(:)
 end type

 ! An lbc file that was opened and checked: its records' times, seconds after the start.
 type checked_file
  character(len=512) :: path = ''
  integer(int64), allocatable :: t(:)
 end type

contains

 ! Whole seconds from origin to text, both times as MPAS reads them (lean_clock's parse_time:
 ! YYYY-MM-DD[_hh:mm:ss[.fraction]], days and hours that roll over); -huge(0_int64) if either is not a time.
 ! parse_time's own messages are dropped: the caller names the record.
 integer(int64) function xtime_seconds(text, origin)
 character(*), intent(in) :: text, origin
 type(ESMF_Time) :: t, o
 type(ESMF_TimeInterval) :: gap
 type(message_list) :: dropped
 integer(int64) :: seconds
 integer :: days
 xtime_seconds = -huge(0_int64)
 if (len_trim(text) == 0 .or. len_trim(origin) == 0) return
 if (.not. parse_time(trim(text), t, dropped)) return
 if (.not. parse_time(trim(origin), o, dropped)) return
 if (t >= o) then
  gap = t - o
 else
  gap = o - t
 end if
 call ESMF_TimeIntervalGet(gap, D=days, S_i8=seconds)
 xtime_seconds = 86400_int64*int(days, int64) + seconds
 if (.not. (t >= o)) xtime_seconds = -xtime_seconds
 end function

 ! YYYY-MM-DD_hh:mm:ss of origin plus seconds (lean_clock's time_string).
 function time_after(origin, seconds) result(text)
 character(*), intent(in) :: origin
 integer(int64), intent(in) :: seconds
 character(len=19) :: text
 type(ESMF_Time) :: o, t
 type(ESMF_TimeInterval) :: gap
 type(message_list) :: dropped
 text = origin
 if (.not. parse_time(trim(origin), o, dropped)) return
 call ESMF_TimeIntervalSet(gap, D=int(abs(seconds)/86400_int64), S=int(mod(abs(seconds), 86400_int64)))
 if (seconds >= 0) then
  t = o + gap
 else
  t = o - gap
 end if
 text = time_string(t)
 end function

 ! Every read of the run, resolved as MPAS's read_stream resolves it when the read happens
 ! (framework/mpas_stream_manager.F:3700-3935): the file must exist; it is checked once, before any seek, against
 ! the mesh, for its fields and its xtimes; the record is MPAS_seekStream's (framework/mpas_io_streams.F:142-339);
 ! without one the same seek runs in the file one filename interval away (the request's retest), and without that
 ! there is no boundary data for the time; each record must be later than the one before (MPAS would divide by the
 ! zero gap, dynamics/mpas_atm_boundaries.F:266-272).
 ! The first problem ends the resolution: reader%n is then the number of reads resolved before it.
 subroutine lbc_open(reader, requests, start_time, msgs)
 type(lbc_reader), intent(out) :: reader
 type(lbc_request), intent(in) :: requests(:)
 character(*), intent(in) :: start_time
 type(message_list), intent(inout) :: msgs
 type(checked_file), allocatable :: known(:)
 character(len=:), allocatable :: primary, retest
 integer :: n, k, i, j, nknown, rec
 logical :: there, ok
 n = size(requests)
 allocate(reader%at_s(n), reader%valid_s(n), reader%file(n), reader%record(n), reader%step(n), known(2*n))
 reader%at_s = 0
 reader%valid_s = 0
 reader%file = ''
 reader%record = 0
 nknown = 0
 do k = 1, n
  primary = ''
  retest = ''
  if (allocated(requests(k)%file)) primary = trim(requests(k)%file)
  if (allocated(requests(k)%retest)) retest = trim(requests(k)%retest)
  inquire(file=primary, exist=there)
  if (.not. there) then
   call add_message(msgs, LOG_ERR, 'Stream ''lbc_in'' attempted to read non-existent file '''//primary//'''')
   call no_data(requests(k), primary, '', start_time, msgs)
   return
  end if
  call known_file(primary, i, ok)
  if (.not. ok) return
  rec = seek(known(i)%t, requests(k)%at_s, requests(k)%whence)
  j = i
  if (rec == 0 .and. len(retest) > 0) then
   inquire(file=retest, exist=there)
   if (there) then
    call known_file(retest, j, ok)
    if (.not. ok) return
    rec = seek(known(j)%t, requests(k)%at_s, requests(k)%whence)
   end if
  end if
  if (rec == 0) then
   call no_data(requests(k), primary, retest, start_time, msgs)
   return
  end if
  if (k > 1) then
   if (known(j)%t(rec) <= reader%valid_s(k-1)) then
    call add_message(msgs, LOG_ERR, 'lbc_in: the record read at '//time_after(start_time, requests(k)%at_s)//' ('// &
                     time_after(start_time, known(j)%t(rec))//') is not after the previous one ('// &
                     time_after(start_time, reader%valid_s(k-1))//')')
    return
   end if
  end if
  reader%step(k) = requests(k)%step
  reader%at_s(k) = requests(k)%at_s
  reader%valid_s(k) = known(j)%t(rec)
  reader%file(k) = known(j)%path
  reader%record(k) = rec
  reader%n = k
 end do
 contains
  ! The index of path among the files already checked; a new one is opened and checked first.
  subroutine known_file(path, i, ok)
  character(*), intent(in) :: path
  integer, intent(out) :: i
  logical, intent(out) :: ok
  ok = .true.
  do i = 1, nknown
   if (known(i)%path == path) return
  end do
  i = nknown + 1
  call check_file(path, start_time, known(i)%t, msgs, ok)
  if (.not. ok) return
  known(i)%path = path
  nknown = i
  end subroutine
 end subroutine

 ! No boundary data for a read: MPAS's boundary line, its core line, and where lean looked.
 subroutine no_data(request, primary, retest, start_time, msgs)
 type(lbc_request), intent(in) :: request
 character(*), intent(in) :: primary, retest, start_time
 type(message_list), intent(inout) :: msgs
 character(len=19) :: when
 when = time_after(start_time, request%at_s)
 if (request%whence == LBC_LATEST_BEFORE) then
  ! dynamics/mpas_atm_boundaries.F:149-150, mpas_atm_core.F:743
  call add_message(msgs, LOG_ERR, 'Could not read from ''lbc_in'' stream on or before the current date '// &
                   'to update lateral boundary tendencies')
  call add_message(msgs, LOG_ERR, 'Failed to process LBC data on or before '//when)
 else
  ! dynamics/mpas_atm_boundaries.F:158-159, mpas_atm_core.F:771
  call add_message(msgs, LOG_ERR, 'Could not read from ''lbc_in'' stream after the current date '// &
                   'to update lateral boundary tendencies')
  call add_message(msgs, LOG_ERR, 'Failed to process LBC data at next time after '//when)
 end if
 if (len(retest) > 0) then
  call add_message(msgs, LOG_ERR, '    (time '//when//'; looked in '//primary//' and '//retest//')')
 else
  call add_message(msgs, LOG_ERR, '    (time '//when//'; looked in '//primary//')')
 end if
 end subroutine

 ! One lbc file, opened once: its sizes against the mesh file, the fields lean needs, every xtime a time.
 ! t: the records' times, seconds after start_time.
 ! Only lbc_u, lbc_rho, lbc_theta, lbc_qv and xtime are required; the other lbc_* fields (w, qc, qr, qi, qs, qg, ni,
 ! nr) are optional, as MPAS reads a file without them (lbc_next). Only lbc_w has nVertLevelsP1,
 ! so a file without lbc_w need not have that dimension (MPAS asks a stream's file only for the dimensions of the
 ! fields it holds, framework/mpas_bootstrapping.F:562-640).
 subroutine check_file(path, start_time, t, msgs, ok)
 character(*), intent(in) :: path, start_time
 integer(int64), allocatable, intent(out) :: t(:)
 type(message_list), intent(inout) :: msgs
 logical, intent(out) :: ok
 character(len=9), parameter :: needed(5) = [character(len=9) :: 'lbc_u', 'lbc_rho', 'lbc_theta', 'lbc_qv', 'xtime']
 character(len=256), allocatable :: xtimes(:)
 type(input_file) :: f
 logical :: missing
 integer :: before, i, n
 ok = .false.
 call open_input(f, path, 'lbc_in', msgs)
 if (.not. f%opened) return
 before = count_problems(msgs)
 missing = .false.
 call one_size(f, 'nCells', ncells, mesh_why, .false., missing, msgs)
 call one_size(f, 'nEdges', nedges, mesh_why, .false., missing, msgs)
 if (dimension_length(f, 'nVertices') > 0) &
  call one_size(f, 'nVertices', nvertices, mesh_why, .false., missing, msgs)
 call one_size(f, 'nVertLevels', nz, mesh_why, .false., missing, msgs)
 if (has_variable(f, 'lbc_w')) call one_size(f, 'nVertLevelsP1', dimension_length(f, 'nVertLevels') + 1, &
                                             'MPAS 8.4.2 writes nVertLevels + 1', .false., missing, msgs)
 if (missing) call add_message(msgs, LOG_CRIT, 'Please check the input file(s) to be read by the ''lbc_in'' input stream.')
 if (.not. missing .and. count_problems(msgs) == before) then
  do i = 1, size(needed)
   if (.not. has_variable(f, trim(needed(i)))) &
    call add_message(msgs, LOG_ERR, trim(path)//': no variable '//trim(needed(i))//' (lbc_in stream)')
  end do
 end if
 if (.not. missing .and. count_problems(msgs) == before) then
  n = dimension_length(f, 'Time')
  allocate(xtimes(n), t(n))
  if (n > 0) then
   if (nc_get(f%nc, 'xtime', xtimes) /= NC_NOERR) &
    call add_message(msgs, LOG_ERR, trim(path)//': xtime: '//nc_reason('xtime'))
  end if
  do i = 1, n
   if (count_problems(msgs) /= before) exit
   if (xtime_seconds(xtimes(i), xtimes(i)) /= 0) then
    call add_message(msgs, LOG_ERR, trim(path)//': xtime of record '//int_text(i)//' is not a time: '''// &
                     trim(xtimes(i))//'''')
   else
    t(i) = xtime_seconds(xtimes(i), start_time)
   end if
  end do
 end if
 ok = .not. missing .and. count_problems(msgs) == before
 call close_input(f)
 end subroutine

 ! MPAS_seekStream (framework/mpas_io_streams.F:279-327) on the records' times t: the record at or before `at`
 ! (LATEST_BEFORE) or after it (EARLIEST_STRICTLY_AFTER), the closest one, the first on a tie, within MPAS's 10000 days;
 ! 0 when there is none.
 integer function seek(t, at, whence)
 integer(int64), intent(in) :: t(:), at
 integer, intent(in) :: whence
 integer(int64) :: gap, best
 integer :: i
 seek = 0
 best = 864000000_int64
 do i = 1, size(t)
  if (whence == LBC_LATEST_BEFORE) then
   if (t(i) > at) cycle
  else
   if (t(i) <= at) cycle
  end if
  gap = abs(t(i) - at)
  if (gap < best) then
   best = gap
   seek = i
  end if
 end do
 end function

 ! Whether the next read is due at step it.
 logical function lbc_due(reader, it)
 type(lbc_reader), intent(in) :: reader
 integer, intent(in) :: it
 lbc_due = .false.
 if (reader%next > reader%n) return
 lbc_due = reader%step(reader%next) <= it
 end function

 ! The next record of the run into record (ghost slots 0; its arrays are allocated on the first call only) and its
 ! time. lbc_w, lbc_qc, lbc_qr, lbc_qi, lbc_qs, lbc_qg, lbc_ni and lbc_nr are read when the file holds them; a missing
 ! one is 0 with MPAS's warning at every read (framework/mpas_io.F:875). MPAS does the same: build_stream ignores
 ! the error of MPAS_streamAddField (framework/mpas_stream_manager.F:4360-4399), the field keeps its initial zeros,
 ! and the tendency of zeros is zero (dynamics/mpas_atm_boundaries.F:266-300). lbc_u, lbc_rho, lbc_theta and lbc_qv
 ! stay required (checked at startup): MPAS would run on with a zero density at the boundary.
 ! A non-finite value is a CRIT line.
 ! Nothing is read when no read is left: valid_s is then -huge.
 subroutine lbc_next(reader, record, valid_s, msgs)
 type(lbc_reader), intent(inout) :: reader
 type(dyn_lbc_type), intent(inout) :: record
 integer(int64), intent(out) :: valid_s
 type(message_list), intent(inout) :: msgs
 type(input_file) :: f
 character(len=256) :: xtime
 integer :: k
 valid_s = -huge(0_int64)
 if (reader%next > reader%n) return
 k = reader%next
 reader%next = k + 1
 valid_s = reader%valid_s(k)
 if (.not. allocated(record%u)) allocate(record%u(nz,ne1), record%w(nz+1,nc1), record%rho(nz,nc1), record%theta(nz,nc1), &
                                         record%qv(nz,nc1), record%qc(nz,nc1), record%qr(nz,nc1))
 if (.not. allocated(record%qi)) allocate(record%qi(nz,nc1), record%qs(nz,nc1), record%qg(nz,nc1), record%ni(nz,nc1), &
                                          record%nr(nz,nc1))
 call open_input(f, trim(reader%file(k)), 'lbc_in', msgs)
 if (.not. f%opened) return
 f%record = reader%record(k)
 xtime = ''
 if (nc_get(f%nc, 'xtime', xtime, record=f%record) /= NC_NOERR) xtime = ''
 ! in the order of MPAS's lbc_in stream, which is the order of its warnings: lbc_scalars, lbc_u, lbc_w, lbc_rho,
 ! lbc_theta (tools/registry/gen_inc.c:2039-2205 adds a stream's var_arrays before its vars)
 call read_field(f, 'lbc_qv', record%qv, msgs)
 call optional_field('lbc_qc', record%qc)
 call optional_field('lbc_qr', record%qr)
 call optional_field('lbc_qi', record%qi)
 call optional_field('lbc_qs', record%qs)
 call optional_field('lbc_qg', record%qg)
 call optional_field('lbc_ni', record%ni)
 call optional_field('lbc_nr', record%nr)
 call read_field(f, 'lbc_u', record%u, msgs)
 call optional_field('lbc_w', record%w)
 call read_field(f, 'lbc_rho', record%rho, msgs)
 call read_field(f, 'lbc_theta', record%theta, msgs)
 call close_input(f)
 call finite('lbc_u', record%u)
 call finite('lbc_w', record%w)
 call finite('lbc_rho', record%rho)
 call finite('lbc_theta', record%theta)
 call finite('lbc_qv', record%qv)
 call finite('lbc_qc', record%qc)
 call finite('lbc_qr', record%qr)
 call finite('lbc_qi', record%qi)
 call finite('lbc_qs', record%qs)
 call finite('lbc_qg', record%qg)
 call finite('lbc_ni', record%ni)
 call finite('lbc_nr', record%nr)
 contains
  subroutine optional_field(name, a)
  character(*), intent(in) :: name
  real, intent(inout), contiguous, target :: a(:,:)
  if (has_variable(f, name)) then
   call read_field(f, name, a, msgs)
  else
   a = 0.
   call add_message(msgs, LOG_WARN, 'Variable '//name//' not in input file.')
  end if
  end subroutine

  subroutine finite(name, a)
  character(*), intent(in) :: name
  real, intent(in) :: a(:,:)
  integer :: i, j, n
  n = 0
  do j = 1, size(a, 2)
   do i = 1, size(a, 1)
    if (.not. ieee_is_finite(a(i,j))) n = n + 1
   end do
  end do
  if (n > 0) call add_message(msgs, LOG_CRIT, trim(reader%file(k))//': '//name//': '//int_text(n)// &
                              ' non-finite values in the record of '//trim(xtime))
  end subroutine
 end subroutine
end module
