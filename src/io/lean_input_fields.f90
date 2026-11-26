! One field of an MPAS NetCDF file into a model array, through lean_nc.
! Arrays are in Registry order (fastest first), as lean_nc gives them. Every cell, edge and vertex axis of a model array
! has one more slot than the file, the "missing neighbour" slot MPAS keeps in memory; it is left 0 here.
! A file may carry more neighbour slots than lean's maxedges (MPAS meshes: maxEdges 10); the extra slots must be empty
! and are dropped, and neighbour indices are remapped (remap_neighbours).
module lean_input_fields
 use lean_nc
 use lean_messages, only: message_list, add_message, int_text, LOG_OUT, LOG_ERR, LOG_CRIT
 implicit none
 private
 public :: input_file, open_input, close_input, count_problems, read_field, remap_neighbours
 public :: has_variable, dimension_length, nc_reason, one_size, missing_dimension, mesh_why

 ! Horizontal dimensions of every later file must match the mesh file.
 character(len=*), parameter :: mesh_why = 'the mesh file'

 type input_file
  type(nc_file) :: nc
  character(len=512) :: path = ''
  character(len=16) :: role = ''     ! the stream the file belongs to: input or lbc_in
  integer :: record = 0              ! the Time record to read; 0 when the file has no Time records
  logical :: opened = .false.
 end type

 interface read_field
  module procedure read_real_0, read_real_1, read_real_2, read_real_3, read_int_0, read_int_1, read_int_2
 end interface

contains

 ! A CRIT line when the file cannot be opened. record starts at 1 when the file has Time records.
 subroutine open_input(f, path, role, msgs)
 type(input_file), intent(out) :: f
 character(*), intent(in) :: path, role
 type(message_list), intent(inout) :: msgs
 integer :: n
 f%path = path
 f%role = role
 if (nc_open(f%nc, trim(path)) /= NC_NOERR) then
  call add_message(msgs, LOG_CRIT, trim(role)//' stream: cannot open '//trim(path)//': '//nc_message())
  return
 end if
 f%opened = .true.
 n = dimension_length(f, 'Time')
 if (n > 0) f%record = 1
 end subroutine

 subroutine close_input(f)
 type(input_file), intent(inout) :: f
 integer :: status
 if (f%opened) status = nc_close(f%nc)
 f%opened = .false.
 end subroutine

 ! The number of ERR lines.
 integer function count_problems(msgs)
 type(message_list), intent(in) :: msgs
 integer :: i
 count_problems = 0
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_ERR) count_problems = count_problems + 1
 end do
 end function

 logical function has_variable(f, name)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 has_variable = nc_inq_var(f%nc, name) == NC_NOERR
 end function

 ! A dimension's length; 0 when the file has none of that name.
 integer function dimension_length(f, name)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 integer :: n
 dimension_length = 0
 if (nc_inq_dim(f%nc, name, n) == NC_NOERR) dimension_length = n
 end function

 ! The reason part of lean_nc's last message ("operation: path: name: reason").
 function nc_reason(name) result(reason)
 character(*), intent(in) :: name
 character(len=:), allocatable :: reason, text, key
 integer :: i
 text = nc_message()
 key = ': '//trim(name)//': '
 i = index(text, key, back=.true.)
 if (i > 0) then
  reason = text(i+len(key):)
 else
  reason = text
 end if
 end function

 ! One dimension: an ERR line when it differs from the expected length (at_most: only when it is larger), and MPAS's
 ! missing-dimension lines when the file has none.
 subroutine one_size(f, name, built, why, at_most, missing, msgs)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name, why
 integer, intent(in) :: built
 logical, intent(in) :: at_most
 logical, intent(inout) :: missing
 type(message_list), intent(inout) :: msgs
 integer :: n
 logical :: fits
 if (nc_inq_dim(f%nc, name, n) /= NC_NOERR) then
  call missing_dimension(f, name, msgs)
  missing = .true.
  return
 end if
 fits = n == built
 if (at_most) fits = n <= built
 if (.not. fits) then
  if (why == mesh_why) then
   call add_message(msgs, LOG_ERR, trim(f%path)//': '//name//' = '//int_text(n)//'; the mesh file has '//int_text(built))
  else
   call add_message(msgs, LOG_ERR, trim(f%path)//': '//name//' = '//int_text(n)// &
                    '; this executable is built for '//int_text(built)//' ('//why//')')
  end if
 end if
 end subroutine

 subroutine missing_dimension(f, name, msgs)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 type(message_list), intent(inout) :: msgs
 call add_message(msgs, LOG_OUT, '       '//name//' *** not found in stream ***')
 call add_message(msgs, LOG_OUT, '')
 call add_message(msgs, LOG_ERR, 'At least one fields to be read from the '''//trim(f%role)//''' stream is dimensioned')
 call add_message(msgs, LOG_ERR, 'by '''//name//''', but the '''//name//''' dimension is not defined')
 call add_message(msgs, LOG_ERR, 'in the file '//trim(f%path))
 call add_message(msgs, LOG_ERR, trim(f%path)//': no dimension '//name//' ('//trim(f%role)// &
                  ' stream; an init file made by init_atmosphere has it)')
 end subroutine

 ! What a read of `name` into an array of shape `extent` needs. ok is .false. after an ERR line (variable missing,
 ! wrong type or wrong shape).
 ! flen: the file's lengths of the array's dimensions; start, count: the slice in the file (with the Time record);
 ! slot_dim: the neighbour-slot dimension that is longer in the file, 0 if none.
 subroutine plan_read(f, name, want_real, extent, msgs, ok, flen, start, count, slot_dim)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 logical, intent(in) :: want_real
 integer, intent(in) :: extent(:)
 type(message_list), intent(inout) :: msgs
 logical, intent(out) :: ok
 integer, intent(out) :: flen(size(extent))
 integer, allocatable, intent(out) :: start(:), count(:)
 integer, intent(out) :: slot_dim
 character(len=NC_MAX_NAME), allocatable :: dims(:)
 integer, allocatable :: lengths(:)
 integer :: xtype, rank, nd, i
 logical :: timed, fits
 ok = .false.
 slot_dim = 0
 flen = 0
 rank = size(extent)
 if (nc_inq_var(f%nc, name, xtype, dims, lengths) /= NC_NOERR) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': no variable '//trim(name)//' ('//trim(f%role)//' stream)')
  return
 end if
 fits = xtype == NC_INT
 if (want_real) fits = xtype == NC_FLOAT .or. xtype == NC_DOUBLE
 if (.not. fits) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': incompatible memory and NetCDF types')
  return
 end if
 nd = size(dims)
 timed = .false.
 if (nd > 0) timed = trim(dims(nd)) == 'Time'
 if (timed) nd = nd - 1
 if (nd /= rank) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': '//int_text(nd)// &
                   ' dimensions in the file, lean_mpas reads '//int_text(rank))
  return
 end if
 if (timed .and. f%record < 1) then
  call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': the file has no Time record')
  return
 end if
 do i = 1, rank
  flen(i) = lengths(i)
  fits = lengths(i) == extent(i)
  ! the ghost slot of the last dimension
  if (i == rank .and. lengths(i) == extent(i) - 1) &
   fits = trim(dims(i)) == 'nCells' .or. trim(dims(i)) == 'nEdges' .or. trim(dims(i)) == 'nVertices'
  ! more neighbour slots than lean keeps, in the two layouts lean reads: (slots, cells) and (R3, slots, cells)
  if (lengths(i) > extent(i) .and. (trim(dims(i)) == 'maxEdges' .or. trim(dims(i)) == 'maxEdges2') &
      .and. slot_dim == 0 .and. i == rank - 1) then
   fits = .true.
   slot_dim = i
  end if
  if (.not. fits) then
   call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': dimension '//trim(dims(i))//' = '// &
                    int_text(lengths(i))//', lean_mpas reads '//int_text(extent(i)))
   return
  end if
 end do
 if (timed) then
  allocate(start(rank+1), count(rank+1))
  start(rank+1) = f%record
  count(rank+1) = 1
 else
  allocate(start(rank), count(rank))
 end if
 start(1:rank) = 1
 count(1:rank) = flen
 ok = .true.
 end subroutine

 subroutine read_failed(f, name, msgs)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 type(message_list), intent(inout) :: msgs
 call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': '//nc_reason(name))
 end subroutine

 subroutine slots_used(f, name, kept, first, msgs)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 integer, intent(in) :: kept, first
 type(message_list), intent(inout) :: msgs
 call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)//': slots beyond '//int_text(kept)// &
                  ' are used (first at index '//int_text(first)//'): lean_mpas supports cells of at most 8 edges')
 end subroutine

 subroutine read_real_0(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out) :: a
 type(message_list), intent(inout) :: msgs
 integer :: flen(0), slot_dim, none(0)
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0.
 call plan_read(f, name, .true., none, msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (size(start) == 0) then
  if (nc_get(f%nc, name, a) /= NC_NOERR) call read_failed(f, name, msgs)
 else
  if (nc_get(f%nc, name, a, start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
 end if
 end subroutine

 subroutine read_real_1(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:)
 type(message_list), intent(inout) :: msgs
 integer :: flen(1), slot_dim
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0.
 call plan_read(f, name, .true., shape(a), msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (nc_get(f%nc, name, a(1:flen(1)), start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
 end subroutine

 subroutine read_real_2(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:,:)
 type(message_list), intent(inout) :: msgs
 real, allocatable :: t(:,:)
 integer :: flen(2), slot_dim, i, j, k
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0.
 call plan_read(f, name, .true., shape(a), msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (slot_dim == 0) then
  if (nc_get(f%nc, name, a(:,1:flen(2)), start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
  return
 end if
 allocate(t(flen(1),flen(2)))
 if (nc_get(f%nc, name, t, start=start, count=count) /= NC_NOERR) then
  call read_failed(f, name, msgs)
  return
 end if
 k = size(a, 1)
 do j = 1, flen(2)
  do i = k + 1, flen(1)
   if (t(i,j) /= 0.) then
    call slots_used(f, name, k, j, msgs)
    return
   end if
  end do
 end do
 a(:,1:flen(2)) = t(1:k,:)
 end subroutine

 subroutine read_real_3(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 real, intent(out), contiguous, target :: a(:,:,:)
 type(message_list), intent(inout) :: msgs
 real, allocatable :: t(:,:,:)
 integer :: flen(3), slot_dim, i, j, k
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0.
 call plan_read(f, name, .true., shape(a), msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (slot_dim == 0) then
  if (nc_get(f%nc, name, a(:,:,1:flen(3)), start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
  return
 end if
 allocate(t(flen(1),flen(2),flen(3)))
 if (nc_get(f%nc, name, t, start=start, count=count) /= NC_NOERR) then
  call read_failed(f, name, msgs)
  return
 end if
 k = size(a, 2)
 do j = 1, flen(3)
  do i = k + 1, flen(2)
   if (any(t(:,i,j) /= 0.)) then
    call slots_used(f, name, k, j, msgs)
    return
   end if
  end do
 end do
 a(:,:,1:flen(3)) = t(:,1:k,:)
 end subroutine

 subroutine read_int_0(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 integer, intent(out) :: a
 type(message_list), intent(inout) :: msgs
 integer :: flen(0), slot_dim, none(0)
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0
 call plan_read(f, name, .false., none, msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (size(start) == 0) then
  if (nc_get(f%nc, name, a) /= NC_NOERR) call read_failed(f, name, msgs)
 else
  if (nc_get(f%nc, name, a, start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
 end if
 end subroutine

 subroutine read_int_1(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 integer, intent(out), contiguous, target :: a(:)
 type(message_list), intent(inout) :: msgs
 integer :: flen(1), slot_dim
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0
 call plan_read(f, name, .false., shape(a), msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (nc_get(f%nc, name, a(1:flen(1)), start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
 end subroutine

 subroutine read_int_2(f, name, a, msgs)
 type(input_file), intent(inout) :: f
 character(*), intent(in) :: name
 integer, intent(out), contiguous, target :: a(:,:)
 type(message_list), intent(inout) :: msgs
 integer, allocatable :: t(:,:)
 integer :: flen(2), slot_dim, i, j, k
 integer, allocatable :: start(:), count(:)
 logical :: ok
 a = 0
 call plan_read(f, name, .false., shape(a), msgs, ok, flen, start, count, slot_dim)
 if (.not. ok) return
 if (slot_dim == 0) then
  if (nc_get(f%nc, name, a(:,1:flen(2)), start=start, count=count) /= NC_NOERR) call read_failed(f, name, msgs)
  return
 end if
 allocate(t(flen(1),flen(2)))
 if (nc_get(f%nc, name, t, start=start, count=count) /= NC_NOERR) then
  call read_failed(f, name, msgs)
  return
 end if
 k = size(a, 1)
 do j = 1, flen(2)
  do i = k + 1, flen(1)
   if (t(i,j) /= 0) then
    call slots_used(f, name, k, j, msgs)
    return
   end if
  end do
 end do
 a(:,1:flen(2)) = t(1:k,:)
 end subroutine

 ! Neighbour indices as MPAS keeps them in memory (mpas_block_creator): a missing
 ! neighbour, 0 in the file, becomes n+1. With degree, only the first degree(i) slots hold neighbours: the slots beyond
 ! stay 0, and a neighbour there is an error. The ghost column of a is not touched.
 subroutine remap_neighbours(f, name, a, n, msgs, degree)
 type(input_file), intent(in) :: f
 character(*), intent(in) :: name
 integer, intent(inout) :: a(:,:)
 integer, intent(in) :: n
 type(message_list), intent(inout) :: msgs
 integer, intent(in), optional :: degree(:)
 integer :: i, j, d
 do j = 1, size(a, 2) - 1
  d = size(a, 1)
  if (present(degree)) d = degree(j)
  do i = 1, size(a, 1)
   if (i <= d) then
    if (a(i,j) == 0) a(i,j) = n + 1
   else if (a(i,j) /= 0) then
    call add_message(msgs, LOG_ERR, trim(f%path)//': '//trim(name)// &
                     ': a neighbour in a slot beyond nEdgesOn* (first at index '//int_text(j)//')')
    return
   end if
  end do
 end do
 end subroutine
end module
