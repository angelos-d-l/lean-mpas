! netCDF-C through ISO_C_BINDING, with status returns and no netCDF-Fortran dependency.
! Dimensions and slices use Registry order (fastest first, Time last), reversed for C
! as in MPAS external/SMIOL/smiolf.F90:682-686,758,929. lean_nc never stops the program.
module lean_nc
  use iso_c_binding, only: c_int, c_float, c_double, c_char, c_size_t, c_int64_t, &
                          c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_loc
  implicit none
  private
  public :: nc_io_type_format, nc_create, nc_open, nc_close, nc_inq_format, nc_message
  public :: nc_def_dim, nc_inq_dim, nc_def_var, nc_inq_var, nc_put_att, nc_get_att
  public :: nc_put, nc_get, nc_copy_attributes, nc_copy_values, nc_end_define

  ! netcdf.h values; tests compare all 33 with the header of the linked library.
  integer, parameter, public :: NC_NOERR = 0
  integer, parameter, public :: NC_EBADID = -33
  integer, parameter, public :: NC_EEXIST = -35
  integer, parameter, public :: NC_EPERM = -37
  integer, parameter, public :: NC_EINVALCOORDS = -40
  integer, parameter, public :: NC_ENAMEINUSE = -42
  integer, parameter, public :: NC_ENOTATT = -43
  integer, parameter, public :: NC_EBADDIM = -46
  integer, parameter, public :: NC_ENOTVAR = -49
  integer, parameter, public :: NC_ENOTNC = -51
  integer, parameter, public :: NC_EEDGE = -57
  integer, parameter, public :: NC_ERANGE = -60
  integer, parameter, public :: NC_ENOTBUILT = -128
  integer, parameter, public :: NC_CHAR = 2
  integer, parameter, public :: NC_INT = 4
  integer, parameter, public :: NC_FLOAT = 5
  integer, parameter, public :: NC_DOUBLE = 6
  integer, parameter, public :: NC_FORMAT_CLASSIC = 1
  integer, parameter, public :: NC_FORMAT_64BIT_OFFSET = 2
  integer, parameter, public :: NC_FORMAT_NETCDF4 = 3
  integer, parameter, public :: NC_FORMAT_NETCDF4_CLASSIC = 4
  integer, parameter, public :: NC_FORMAT_64BIT_DATA = 5
  integer, parameter, public :: NC_UNLIMITED = 0
  integer, parameter, public :: NC_MAX_NAME = 256
  integer, parameter :: NC_NOWRITE = 0
  integer, parameter :: NC_WRITE = 1
  integer, parameter :: NC_CLOBBER = 0
  integer, parameter :: NC_NOCLOBBER = 4
  integer, parameter :: NC_64BIT_OFFSET = 512
  integer, parameter :: NC_64BIT_DATA = 32
  integer, parameter :: NC_NETCDF4 = 4096
  integer, parameter :: NC_NOFILL = 256
  integer, parameter :: NC_GLOBAL = -1

  integer, parameter, public :: LNC_ETYPE = -1001   ! incompatible memory and NetCDF types
  integer, parameter, public :: LNC_ESIZE = -1002   ! array size or string length does not match
  integer, parameter, public :: LNC_EARGS = -1003   ! conflicting/missing slice arguments, wrong counts of values,
                                                  ! record without a record dimension, record/start < 1, count < 0
  integer, parameter, public :: LNC_EFORMAT = -1004 ! a format lean_nc does not create

  type, public :: nc_file
    integer :: id = -1
    logical :: define_mode = .false.
    character(len=:), allocatable :: path
  end type

  character(len=:), allocatable, save :: last_message

  interface nc_put_att
    module procedure put_att_text, put_att_int, put_att_float, put_att_double
  end interface

  interface nc_get_att
    module procedure get_att_text, get_att_int, get_att_float, get_att_double
  end interface

  interface nc_put
    module procedure put_float_0, put_float_1, put_float_2, put_float_3
    module procedure put_double_0, put_double_1, put_double_2, put_double_3
    module procedure put_int_0, put_int_1, put_int_2, put_int_3, put_text_0
  end interface

  interface nc_get
    module procedure get_float_0, get_float_1, get_float_2, get_float_3
    module procedure get_double_0, get_double_1, get_double_2, get_double_3
    module procedure get_int_0, get_int_1, get_int_2, get_int_3, get_text_0, get_text_1
  end interface

  interface
    integer(c_int) function c_nc_create(path, mode, id) bind(C, name='nc_create')
      import c_int, c_char
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value :: mode
      integer(c_int), intent(out) :: id
    end function

    integer(c_int) function c_nc_open(path, mode, id) bind(C, name='nc_open')
      import c_int, c_char
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value :: mode
      integer(c_int), intent(out) :: id
    end function

    integer(c_int) function c_nc_close(id) bind(C, name='nc_close')
      import c_int
      integer(c_int), value :: id
    end function

    integer(c_int) function c_nc_inq_format(id, format) bind(C, name='nc_inq_format')
      import c_int
      integer(c_int), value :: id
      integer(c_int), intent(out) :: format
    end function

    integer(c_int) function c_nc_inq_format_extended(id, format, mode) bind(C, name='nc_inq_format_extended')
      import c_int
      integer(c_int), value :: id
      integer(c_int), intent(out) :: format, mode
    end function

    integer(c_int) function c_nc_set_fill(id, mode, old_mode) bind(C, name='nc_set_fill')
      import c_int
      integer(c_int), value :: id, mode
      integer(c_int), intent(out) :: old_mode
    end function

    integer(c_int) function c_nc_redef(id) bind(C, name='nc_redef')
      import c_int
      integer(c_int), value :: id
    end function

    integer(c_int) function c_nc_enddef(id) bind(C, name='nc_enddef')
      import c_int
      integer(c_int), value :: id
    end function

    ! Nullable size_t pointers let zero-dimensional variables pass NULL start/count.
    integer(c_int) function c_nc_inq_varnatts(id, varid, n) bind(C, name='nc_inq_varnatts')
      import c_int
      integer(c_int), value :: id, varid
      integer(c_int), intent(out) :: n
    end function

    integer(c_int) function c_nc_inq_attname(id, varid, n, name) bind(C, name='nc_inq_attname')
      import c_int, c_char
      integer(c_int), value :: id, varid, n
      character(kind=c_char), intent(out) :: name(*)
    end function

    integer(c_int) function c_nc_copy_att(src, svar, name, dest, dvar) bind(C, name='nc_copy_att')
      import c_int, c_char
      integer(c_int), value :: src, svar, dest, dvar
      character(kind=c_char), intent(in) :: name(*)
    end function

    integer(c_int) function c_nc_get_vara(id, varid, start, count, data) bind(C, name='nc_get_vara')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_put_vara(id, varid, start, count, data) bind(C, name='nc_put_vara')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_put_vara_float(id, varid, start, count, data) bind(C, name='nc_put_vara_float')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_put_vara_double(id, varid, start, count, data) bind(C, name='nc_put_vara_double')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_put_vara_int(id, varid, start, count, data) bind(C, name='nc_put_vara_int')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_put_vara_text(id, varid, start, count, data) bind(C, name='nc_put_vara_text')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_get_vara_float(id, varid, start, count, data) bind(C, name='nc_get_vara_float')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_get_vara_double(id, varid, start, count, data) bind(C, name='nc_get_vara_double')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_get_vara_int(id, varid, start, count, data) bind(C, name='nc_get_vara_int')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_get_vara_text(id, varid, start, count, data) bind(C, name='nc_get_vara_text')
      import c_int, c_ptr
      integer(c_int), value :: id, varid
      type(c_ptr), value :: start, count, data
    end function

    integer(c_int) function c_nc_def_dim(id, name, length, dimid) bind(C, name='nc_def_dim')
      import c_int, c_char, c_size_t
      integer(c_int), value :: id
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      integer(c_int), intent(out) :: dimid
    end function

    integer(c_int) function c_nc_inq_dimid(id, name, dimid) bind(C, name='nc_inq_dimid')
      import c_int, c_char
      integer(c_int), value :: id
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: dimid
    end function

    integer(c_int) function c_nc_inq_dim(id, dimid, name, length) bind(C, name='nc_inq_dim')
      import c_int, c_char, c_size_t
      integer(c_int), value :: id, dimid
      character(kind=c_char), intent(out) :: name(*)
      integer(c_size_t), intent(out) :: length
    end function

    integer(c_int) function c_nc_inq_dimlen(id, dimid, length) bind(C, name='nc_inq_dimlen')
      import c_int, c_size_t
      integer(c_int), value :: id, dimid
      integer(c_size_t), intent(out) :: length
    end function

    integer(c_int) function c_nc_inq_unlimdim(id, dimid) bind(C, name='nc_inq_unlimdim')
      import c_int
      integer(c_int), value :: id
      integer(c_int), intent(out) :: dimid
    end function

    integer(c_int) function c_nc_def_var(id, name, xtype, ndims, dimids, varid) bind(C, name='nc_def_var')
      import c_int, c_char, c_ptr
      integer(c_int), value :: id, xtype, ndims
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: dimids
      integer(c_int), intent(out) :: varid
    end function

    integer(c_int) function c_nc_inq_varid(id, name, varid) bind(C, name='nc_inq_varid')
      import c_int, c_char
      integer(c_int), value :: id
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: varid
    end function

    integer(c_int) function c_nc_inq_vartype(id, varid, xtype) bind(C, name='nc_inq_vartype')
      import c_int
      integer(c_int), value :: id, varid
      integer(c_int), intent(out) :: xtype
    end function

    integer(c_int) function c_nc_inq_varndims(id, varid, ndims) bind(C, name='nc_inq_varndims')
      import c_int
      integer(c_int), value :: id, varid
      integer(c_int), intent(out) :: ndims
    end function

    integer(c_int) function c_nc_inq_vardimid(id, varid, dimids) bind(C, name='nc_inq_vardimid')
      import c_int
      integer(c_int), value :: id, varid
      integer(c_int), intent(out) :: dimids(*)
    end function

    integer(c_int) function c_nc_inq_att(id, varid, name, xtype, length) bind(C, name='nc_inq_att')
      import c_int, c_char, c_size_t
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: xtype
      integer(c_size_t), intent(out) :: length
    end function

    integer(c_int) function c_nc_put_att_text(id, varid, name, length, value) bind(C, name='nc_put_att_text')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_put_att_int(id, varid, name, xtype, length, value) bind(C, name='nc_put_att_int')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid, xtype
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_put_att_float(id, varid, name, xtype, length, value) bind(C, name='nc_put_att_float')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid, xtype
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_put_att_double(id, varid, name, xtype, length, value) bind(C, name='nc_put_att_double')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid, xtype
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_get_att_text(id, varid, name, value) bind(C, name='nc_get_att_text')
      import c_int, c_char, c_ptr
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_get_att_int(id, varid, name, value) bind(C, name='nc_get_att_int')
      import c_int, c_char, c_ptr
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_get_att_float(id, varid, name, value) bind(C, name='nc_get_att_float')
      import c_int, c_char, c_ptr
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: value
    end function

    integer(c_int) function c_nc_get_att_double(id, varid, name, value) bind(C, name='nc_get_att_double')
      import c_int, c_char, c_ptr
      integer(c_int), value :: id, varid
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: value
    end function

    function c_nc_strerror(status) result(text) bind(C, name='nc_strerror')
      import c_int, c_ptr
      integer(c_int), value :: status
      type(c_ptr) :: text
    end function

    function c_strlen(text) result(length) bind(C, name='strlen')
      import c_ptr, c_size_t
      type(c_ptr), value :: text
      integer(c_size_t) :: length
    end function
  end interface

contains

  integer function nc_io_type_format(io_type, known) result(format)
    character(*), intent(in) :: io_type
    logical, intent(out), optional :: known
    if (present(known)) known = .true.
    ! Substring order and case follow MPAS framework/xml_stream_parser.c:1240-1266.
    if (index(io_type, 'pnetcdf,cdf5') > 0) then
      format = NC_FORMAT_64BIT_DATA
    else if (index(io_type, 'pnetcdf') > 0) then
      format = NC_FORMAT_64BIT_DATA
    else if (index(io_type, 'netcdf4') > 0) then
      format = NC_FORMAT_NETCDF4
    else if (index(io_type, 'netcdf') > 0) then
      format = NC_FORMAT_64BIT_OFFSET
    else
      format = NC_FORMAT_64BIT_DATA
      if (present(known)) known = .false.
    end if
  end function

  integer function nc_create(f, path, format, clobber) result(status)
    type(nc_file), intent(out) :: f
    character(*), intent(in) :: path
    integer, intent(in) :: format
    logical, intent(in) :: clobber
    integer :: mode
    integer(c_int) :: id, old_mode, close_status
    f%path = trim(path)
    select case (format)
    case (NC_FORMAT_64BIT_DATA)
      mode = NC_64BIT_DATA
    case (NC_FORMAT_64BIT_OFFSET)
      mode = NC_64BIT_OFFSET
    case (NC_FORMAT_NETCDF4)
      mode = NC_NETCDF4
    case default
      status = LNC_EFORMAT
      call save_error(status, 'nc_create', f)
      return
    end select
    ! MPAS framework/mpas_io.F:399-403 refuses an existing file unless clobber was requested.
    if (clobber) then
      mode = ior(mode, NC_CLOBBER)
    else
      mode = ior(mode, NC_NOCLOBBER)
    end if
    ! Keep NC_WRITE in the queried mode as on writable open, for the put permission check.
    status = c_nc_create(trim(path) // c_null_char, int(ior(mode, NC_WRITE), c_int), id)
    if (status == NC_NOERR) then
      ! Match PnetCDF's no-fill default; MPAS's SMIOL writer never changes it.
      status = c_nc_set_fill(id, int(NC_NOFILL, c_int), old_mode)
      if (status == NC_NOERR) then
        f%id = int(id)
        f%define_mode = .true.
      else
        close_status = c_nc_close(id)
      end if
    end if
    call save_error(status, 'nc_create', f)
  end function

  integer function nc_open(f, path, write) result(status)
    type(nc_file), intent(out) :: f
    character(*), intent(in) :: path
    logical, intent(in), optional :: write
    integer :: mode
    integer(c_int) :: id, old_mode, close_status
    f%path = trim(path)
    mode = NC_NOWRITE
    if (present(write)) then
      if (write) mode = NC_WRITE
    end if
    ! MPAS framework/mpas_io.F:406-414 opens an existing output for append/overwrite.
    status = c_nc_open(trim(path) // c_null_char, int(mode, c_int), id)
    if (status == NC_NOERR) then
      if (mode == NC_WRITE) status = c_nc_set_fill(id, int(NC_NOFILL, c_int), old_mode)
      if (status == NC_NOERR) then
        f%id = int(id)
      else
        close_status = c_nc_close(id)
      end if
    end if
    call save_error(status, 'nc_open', f)
  end function

  integer function nc_close(f) result(status)
    type(nc_file), intent(inout) :: f
    status = c_nc_close(int(f%id, c_int))
    f%id = -1
    f%define_mode = .false.
    call save_error(status, 'nc_close', f)
  end function

  integer function nc_inq_format(f, format) result(status)
    type(nc_file), intent(in) :: f
    integer, intent(out) :: format
    integer(c_int) :: c_format
    format = -1
    status = c_nc_inq_format(int(f%id, c_int), c_format)
    if (status == NC_NOERR) format = int(c_format)
    call save_error(status, 'nc_inq_format', f)
  end function

  integer function nc_def_dim(f, name, length) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer, intent(in) :: length
    integer(c_int) :: dimid
    status = enter_define(f)
    if (status == NC_NOERR) &
      status = c_nc_def_dim(int(f%id, c_int), trim(name) // c_null_char, int(length, c_size_t), dimid)
    call save_error(status, 'nc_def_dim', f, name)
  end function

  integer function nc_inq_dim(f, name, length, unlimited) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: name
    integer, intent(out) :: length
    logical, intent(out), optional :: unlimited
    integer(c_int) :: dimid, unlimid
    integer(c_size_t) :: c_length
    length = -1
    if (present(unlimited)) unlimited = .false.
    status = c_nc_inq_dimid(int(f%id, c_int), trim(name) // c_null_char, dimid)
    if (status == NC_NOERR) then
      status = c_nc_inq_dimlen(int(f%id, c_int), dimid, c_length)
      if (status == NC_NOERR) length = int(c_length)
    end if
    if (status == NC_NOERR .and. present(unlimited)) then
      status = c_nc_inq_unlimdim(int(f%id, c_int), unlimid)
      if (status == NC_NOERR) unlimited = dimid == unlimid
    end if
    call save_error(status, 'nc_inq_dim', f, name)
  end function

  integer function nc_def_var(f, name, xtype, dims) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name, dims(:)
    integer, intent(in) :: xtype
    integer(c_int), target :: dimids(size(dims))
    integer(c_int) :: varid
    type(c_ptr) :: dimids_ptr
    integer :: i
    ! MPAS external/SMIOL/smiol.c:818-827 supports just these four types.
    select case (xtype)
    case (NC_FLOAT, NC_DOUBLE, NC_INT, NC_CHAR)
      status = NC_NOERR
    case default
      status = LNC_ETYPE
    end select
    do i = 1, size(dims)
      if (status /= NC_NOERR) exit
      ! Reverse Registry order, as in external/SMIOL/smiolf.F90:758.
      status = c_nc_inq_dimid(int(f%id, c_int), trim(dims(i)) // c_null_char, dimids(size(dims)-i+1))
      if (status /= NC_NOERR) then
        call save_error(status, 'nc_def_var', f, trim(name) // ': ' // trim(dims(i)))
        return
      end if
    end do
    dimids_ptr = c_null_ptr
    if (size(dims) > 0) dimids_ptr = c_loc(dimids)
    if (status == NC_NOERR) status = enter_define(f)
    if (status == NC_NOERR) status = c_nc_def_var(int(f%id, c_int), trim(name) // c_null_char, &
                                               int(xtype, c_int), int(size(dims), c_int), dimids_ptr, varid)
    call save_error(status, 'nc_def_var', f, name)
  end function

  integer function nc_inq_var(f, name, xtype, dims, lengths) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: name
    integer, intent(out), optional :: xtype
    character(len=NC_MAX_NAME), allocatable, intent(out), optional :: dims(:)
    integer, allocatable, intent(out), optional :: lengths(:)
    integer(c_int) :: varid, c_xtype, ndims
    integer(c_int), allocatable :: dimids(:)
    integer(c_size_t) :: length
    character(kind=c_char) :: c_name(NC_MAX_NAME+1)
    integer :: i, j
    if (present(xtype)) xtype = -1
    status = c_nc_inq_varid(int(f%id, c_int), trim(name) // c_null_char, varid)
    if (status == NC_NOERR .and. present(xtype)) then
      status = c_nc_inq_vartype(int(f%id, c_int), varid, c_xtype)
      if (status == NC_NOERR) xtype = int(c_xtype)
    end if
    if (status == NC_NOERR .and. (present(dims) .or. present(lengths))) then
      status = c_nc_inq_varndims(int(f%id, c_int), varid, ndims)
      if (status == NC_NOERR) then
        allocate(dimids(ndims))
        if (ndims > 0) status = c_nc_inq_vardimid(int(f%id, c_int), varid, dimids)
        if (present(dims)) allocate(dims(ndims))
        if (present(lengths)) allocate(lengths(ndims))
        do i = 1, ndims
          if (status /= NC_NOERR) exit
          ! Return Registry order, as in external/SMIOL/smiolf.F90:929.
          status = c_nc_inq_dim(int(f%id, c_int), dimids(ndims-i+1), c_name, length)
          if (status /= NC_NOERR) exit
          if (present(lengths)) lengths(i) = int(length)
          if (present(dims)) then
            dims(i) = ''
            do j = 1, NC_MAX_NAME
              if (c_name(j) == c_null_char) exit
              dims(i)(j:j) = c_name(j)
            end do
          end if
        end do
      end if
    end if
    call save_error(status, 'nc_inq_var', f, name)
  end function

  integer function nc_end_define(f) result(status)
    type(nc_file), intent(inout) :: f
    status = leave_define(f)
  end function

  integer function nc_copy_attributes(source, dest, name) result(status)
    type(nc_file), intent(in) :: source
    type(nc_file), intent(inout) :: dest
    character(*), intent(in) :: name
    character(kind=c_char) :: att(NC_MAX_NAME+1)
    integer(c_int) :: svar, dvar, n, j

    status = attribute_id(source, name, svar)
    if (status == NC_NOERR) status = attribute_id(dest, name, dvar)
    if (status == NC_NOERR) status = c_nc_inq_varnatts(source%id, svar, n)
    if (status == NC_NOERR) status = enter_define(dest)
    if (status == NC_NOERR) then
      do j = 0, n-1
        status = c_nc_inq_attname(source%id, svar, j, att)
        if (status /= NC_NOERR) exit
        status = c_nc_copy_att(source%id, svar, att, dest%id, dvar)
        if (status /= NC_NOERR) exit
      end do
    end if
    call save_error(status, 'nc_copy_attributes', dest, name)
  end function

  ! Transfer the original type, including the raw bytes of character fields.
  integer function nc_copy_values(source, dest, name, xtype, lengths, src_record, dest_record) result(status)
    type(nc_file), intent(in) :: source
    type(nc_file), intent(inout) :: dest
    character(*), intent(in) :: name
    integer, intent(in) :: xtype, lengths(:), src_record, dest_record
    integer(c_size_t), target :: first(size(lengths)), count(size(lengths))
    integer(c_int64_t), allocatable, target :: words(:)
    type(c_ptr) :: s, c
    integer(c_int) :: svar, dvar
    integer :: bytes

    status = attribute_id(source, name, svar)
    if (status == NC_NOERR) status = attribute_id(dest, name, dvar)
    if (status /= NC_NOERR) return
    select case (xtype)
    case (NC_CHAR)
      bytes = 1
    case (NC_INT, NC_FLOAT)
      bytes = 4
    case (NC_DOUBLE)
      bytes = 8
    case default
      status = LNC_ETYPE
      return
    end select
    first = 0
    count = int(lengths(size(lengths):1:-1), c_size_t)
    s = c_null_ptr
    c = c_null_ptr
    if (size(count) > 0) then
      s = c_loc(first)
      c = c_loc(count)
    end if
    if (src_record > 0) then
      first(1) = src_record-1
      count(1) = 1
    end if
    if (product(count) == 0) return
    allocate(words((product(count)*bytes+7)/8))
    status = c_nc_get_vara(source%id, svar, s, c, c_loc(words))
    if (status == NC_NOERR) then
      if (dest_record > 0) first(1) = dest_record-1
      status = c_nc_put_vara(dest%id, dvar, s, c, c_loc(words))
    end if
    call save_error(status, 'nc_copy_values', dest, name)
  end function

  integer function put_att_text(f, var, name, value) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: var, name
    character(*), intent(in), target :: value
    integer(c_int) :: varid
    type(c_ptr) :: data
    status = attribute_id(f, var, varid)
    if (status == NC_NOERR) status = enter_define(f)
    if (status == NC_NOERR) then
      data = c_null_ptr
      if (len(value) > 0) data = c_loc(value)
      status = c_nc_put_att_text(int(f%id, c_int), varid, trim(name) // c_null_char, int(len(value), c_size_t), data)
    end if
    call save_error(status, 'nc_put_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function put_att_int(f, var, name, value) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: var, name
    integer(c_int), intent(in), target :: value
    integer(c_int) :: varid
    status = attribute_id(f, var, varid)
    if (status == NC_NOERR) status = enter_define(f)
    if (status == NC_NOERR) status = c_nc_put_att_int(int(f%id, c_int), varid, trim(name) // c_null_char, &
                                                   int(NC_INT, c_int), 1_c_size_t, c_loc(value))
    call save_error(status, 'nc_put_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function put_att_float(f, var, name, value) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: var, name
    real(c_float), intent(in), target :: value
    integer(c_int) :: varid
    status = attribute_id(f, var, varid)
    if (status == NC_NOERR) status = enter_define(f)
    if (status == NC_NOERR) status = c_nc_put_att_float(int(f%id, c_int), varid, trim(name) // c_null_char, &
                                                     int(NC_FLOAT, c_int), 1_c_size_t, c_loc(value))
    call save_error(status, 'nc_put_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function put_att_double(f, var, name, value) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: var, name
    real(c_double), intent(in), target :: value
    integer(c_int) :: varid
    status = attribute_id(f, var, varid)
    if (status == NC_NOERR) status = enter_define(f)
    if (status == NC_NOERR) status = c_nc_put_att_double(int(f%id, c_int), varid, trim(name) // c_null_char, &
                                                      int(NC_DOUBLE, c_int), 1_c_size_t, c_loc(value))
    call save_error(status, 'nc_put_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function get_att_text(f, var, name, value) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var, name
    character(len=:), allocatable, intent(out), target :: value
    integer(c_int) :: varid
    integer(c_size_t) :: length
    integer :: nul
    status = attribute_info(f, var, name, NC_CHAR, varid, length)
    if (status == NC_NOERR) then
      allocate(character(len=int(length)) :: value)
      if (length > 0) status = c_nc_get_att_text(int(f%id, c_int), varid, trim(name) // c_null_char, c_loc(value))
      if (status == NC_NOERR) then
        ! Lean follows MPAS's PIO NUL-to-blank choice (framework/mpas_io.F:2047-2055), not raw SMIOL bytes.
        nul = index(value, c_null_char)
        if (nul > 0) value(nul:) = ' '
      end if
    end if
    call save_error(status, 'nc_get_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function get_att_int(f, var, name, value) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var, name
    integer(c_int), intent(out), target :: value
    integer(c_int) :: varid
    integer(c_size_t) :: length
    status = attribute_info(f, var, name, NC_INT, varid, length)
    if (status == NC_NOERR) status = c_nc_get_att_int(int(f%id, c_int), varid, trim(name) // c_null_char, c_loc(value))
    call save_error(status, 'nc_get_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function get_att_float(f, var, name, value) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var, name
    real(c_float), intent(out), target :: value
    integer(c_int) :: varid
    integer(c_size_t) :: length
    status = attribute_info(f, var, name, NC_FLOAT, varid, length)
    ! Use the memory type's C call; MPAS narrows with real(doubleVal,RKIND), framework/mpas_io.F:1964.
    if (status == NC_NOERR) status = c_nc_get_att_float(int(f%id, c_int), varid, trim(name) // c_null_char, c_loc(value))
    call save_error(status, 'nc_get_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function get_att_double(f, var, name, value) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var, name
    real(c_double), intent(out), target :: value
    integer(c_int) :: varid
    integer(c_size_t) :: length
    status = attribute_info(f, var, name, NC_DOUBLE, varid, length)
    if (status == NC_NOERR) status = c_nc_get_att_double(int(f%id, c_int), varid, trim(name) // c_null_char, c_loc(value))
    call save_error(status, 'nc_get_att', f, trim(var) // ': ' // trim(name))
  end function

  integer function put_float_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(in), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = put_values(f, name, c_loc(data), 1_c_int64_t, NC_FLOAT, record, start, count)
  end function

  integer function put_float_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(in), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function put_float_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(in), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function put_float_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(in), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function put_double_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(in), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = put_values(f, name, c_loc(data), 1_c_int64_t, NC_DOUBLE, record, start, count)
  end function

  integer function put_double_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(in), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function put_double_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(in), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function put_double_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(in), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function put_int_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(in), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = put_values(f, name, c_loc(data), 1_c_int64_t, NC_INT, record, start, count)
  end function

  integer function put_int_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(in), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function put_int_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(in), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function put_int_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(in), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = put_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function get_float_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(inout), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = get_values(f, name, c_loc(data), 1_c_int64_t, NC_FLOAT, record, start, count)
  end function

  integer function get_float_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(inout), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function get_float_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(inout), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function get_float_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_float), intent(inout), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_FLOAT, record, start, count)
  end function

  integer function get_double_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(inout), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = get_values(f, name, c_loc(data), 1_c_int64_t, NC_DOUBLE, record, start, count)
  end function

  integer function get_double_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(inout), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function get_double_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(inout), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function get_double_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    real(c_double), intent(inout), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_DOUBLE, record, start, count)
  end function

  integer function get_int_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(inout), target :: data
    integer, intent(in), optional :: record, start(:), count(:)
    status = get_values(f, name, c_loc(data), 1_c_int64_t, NC_INT, record, start, count)
  end function

  integer function get_int_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(inout), target, contiguous :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function get_int_2(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(inout), target, contiguous :: data(:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function get_int_3(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    integer(c_int), intent(inout), target, contiguous :: data(:,:,:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: n
    type(c_ptr) :: pointer
    n = size(data, kind=c_int64_t)
    pointer = c_null_ptr
    if (n > 0) pointer = c_loc(data)
    status = get_values(f, name, pointer, n, NC_INT, record, start, count)
  end function

  integer function put_values(f, name, data, n, memory_type, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    type(c_ptr), intent(in) :: data
    integer(c_int64_t), intent(in) :: n
    integer, intent(in) :: memory_type
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: varid
    integer(c_int64_t) :: elements
    integer(c_size_t), allocatable, target :: c_start(:), c_count(:)
    type(c_ptr) :: start_ptr, count_ptr
    status = prepare_slice(f, name, memory_type, varid, c_start, c_count, elements, record, start, count)
    if (status == NC_NOERR) then
      if (n /= elements) status = LNC_ESIZE
    end if
    if (status == NC_NOERR .and. n > 0) then
      start_ptr = c_null_ptr
      count_ptr = c_null_ptr
      if (size(c_start) > 0) then
        start_ptr = c_loc(c_start)
        count_ptr = c_loc(c_count)
      end if
      status = check_write(f)
      if (status == NC_NOERR) status = leave_define(f)
      if (status == NC_NOERR) then
        select case (memory_type)
        case (NC_FLOAT)
          status = c_nc_put_vara_float(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        case (NC_DOUBLE)
          status = c_nc_put_vara_double(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        case (NC_INT)
          status = c_nc_put_vara_int(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        end select
      end if
    end if
    call save_error(status, 'nc_put', f, name)
  end function

  integer function get_values(f, name, data, n, memory_type, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    type(c_ptr), intent(in) :: data
    integer(c_int64_t), intent(in) :: n
    integer, intent(in) :: memory_type
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: varid
    integer(c_int64_t) :: elements
    integer(c_size_t), allocatable, target :: c_start(:), c_count(:)
    type(c_ptr) :: start_ptr, count_ptr
    status = prepare_slice(f, name, memory_type, varid, c_start, c_count, elements, record, start, count)
    if (status == NC_NOERR) then
      if (n /= elements) status = LNC_ESIZE
    end if
    if (status == NC_NOERR .and. n > 0) then
      start_ptr = c_null_ptr
      count_ptr = c_null_ptr
      if (size(c_start) > 0) then
        start_ptr = c_loc(c_start)
        count_ptr = c_loc(c_count)
      end if
      status = leave_define(f)
      if (status == NC_NOERR) then
        ! netCDF-C converts to the memory type; MPAS uses real(doubleVal,RKIND), framework/mpas_io.F:1964.
        select case (memory_type)
        case (NC_FLOAT)
          status = c_nc_get_vara_float(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        case (NC_DOUBLE)
          status = c_nc_get_vara_double(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        case (NC_INT)
          status = c_nc_get_vara_int(int(f%id, c_int), varid, start_ptr, count_ptr, data)
        end select
      end if
    end if
    call save_error(status, 'nc_get', f, name)
  end function

  integer function prepare_slice(f, name, memory_type, varid, c_start, c_count, elements, &
                                 record, start, count) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: name
    integer, intent(in) :: memory_type
    integer(c_int), intent(out) :: varid
    integer(c_size_t), allocatable, intent(out) :: c_start(:), c_count(:)
    integer(c_int64_t), intent(out) :: elements
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: xtype, ndims, unlimid
    integer(c_int), allocatable :: dimids(:)
    integer :: i, j
    status = LNC_EARGS
    if (present(record)) then
      if (present(start) .or. present(count)) return
      if (record < 1) return
    end if
    if (present(start) .neqv. present(count)) return
    status = c_nc_inq_varid(int(f%id, c_int), trim(name) // c_null_char, varid)
    if (status /= NC_NOERR) return
    status = c_nc_inq_vartype(int(f%id, c_int), varid, xtype)
    if (status /= NC_NOERR) return
    select case (memory_type)
    case (NC_FLOAT, NC_DOUBLE)
      if (xtype /= NC_FLOAT .and. xtype /= NC_DOUBLE) status = LNC_ETYPE
    case default
      if (xtype /= memory_type) status = LNC_ETYPE
    end select
    if (status /= NC_NOERR) return
    status = c_nc_inq_varndims(int(f%id, c_int), varid, ndims)
    if (status /= NC_NOERR) return
    if (present(start)) then
      status = LNC_EARGS
      if (size(start) /= ndims .or. size(count) /= ndims) return
      if (any(start < 1) .or. any(count < 0)) return
    end if
    allocate(dimids(ndims), c_start(ndims), c_count(ndims))
    if (ndims > 0) then
      status = c_nc_inq_vardimid(int(f%id, c_int), varid, dimids)
      if (status /= NC_NOERR) return
    end if
    if (present(record)) then
      status = LNC_EARGS
      if (ndims == 0) return
      status = c_nc_inq_unlimdim(int(f%id, c_int), unlimid)
      if (status /= NC_NOERR) return
      if (dimids(1) /= unlimid) then
        status = LNC_EARGS
        return
      end if
    end if
    c_start = 0_c_size_t
    do i = 1, ndims
      if (present(start)) then
        ! Reverse Registry slices and make start zero-based, external/SMIOL/smiolf.F90:682-686.
        j = ndims-i+1
        c_start(i) = int(start(j), c_size_t) - 1_c_size_t
        c_count(i) = int(count(j), c_size_t)
      else
        status = c_nc_inq_dimlen(int(f%id, c_int), dimids(i), c_count(i))
        if (status /= NC_NOERR) return
      end if
    end do
    if (present(record)) then
      c_start(1) = int(record, c_size_t) - 1_c_size_t
      c_count(1) = 1_c_size_t
    end if
    elements = 1_c_int64_t
    if (any(c_count == 0_c_size_t)) then
      elements = 0_c_int64_t
    else
      do i = 1, ndims
        ! Reject an unrepresentable product before multiplication can wrap to a plausible size.
        if (elements > huge(elements) / int(c_count(i), c_int64_t)) then
          status = LNC_ESIZE
          return
        end if
        elements = elements * int(c_count(i), c_int64_t)
      end do
    end if
    status = NC_NOERR
  end function

  integer function put_text_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name, data
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: varid
    integer :: length
    integer(c_size_t), allocatable, target :: c_start(:), c_count(:)
    character(kind=c_char,len=:), allocatable, target :: buffer
    status = text_shape(f, name, 1_c_int64_t, varid, c_start, c_count, length, record, start, count)
    if (status == NC_NOERR) then
      if (len(data) > length) status = LNC_ESIZE
    end if
    if (status == NC_NOERR) then
      allocate(character(kind=c_char,len=length) :: buffer)
      ! Blank-pad to the stream's StrLen (MPAS framework/mpas_io_streams.F:1517-1518).
      buffer(:) = data
      status = check_write(f)
      if (status == NC_NOERR) status = leave_define(f)
      if (status == NC_NOERR) status = c_nc_put_vara_text(int(f%id, c_int), varid, &
        c_loc(c_start), c_loc(c_count), c_loc(buffer(1:1)))
    end if
    call save_error(status, 'nc_put', f, name)
  end function

  integer function get_text_0(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    character(*), intent(inout) :: data
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: varid
    integer :: length, nul
    integer(c_size_t), allocatable, target :: c_start(:), c_count(:)
    character(kind=c_char,len=:), allocatable, target :: buffer
    status = text_shape(f, name, 1_c_int64_t, varid, c_start, c_count, length, record, start, count)
    if (status == NC_NOERR) then
      ! MPAS checks the declared length, not the trimmed content: framework/mpas_io.F:2006-2013.
      if (len(data) < length) status = LNC_ESIZE
    end if
    if (status == NC_NOERR) then
      allocate(character(kind=c_char,len=length) :: buffer)
      status = leave_define(f)
      if (status == NC_NOERR) status = c_nc_get_vara_text(int(f%id, c_int), varid, &
        c_loc(c_start), c_loc(c_count), c_loc(buffer(1:1)))
      if (status == NC_NOERR) then
        ! Lean follows MPAS's PIO NUL-to-blank choice, framework/mpas_io.F:2047-2055.
        nul = index(buffer, c_null_char)
        if (nul > 0) buffer(nul:) = ' '
        data = buffer
      end if
    end if
    call save_error(status, 'nc_get', f, name)
  end function

  integer function get_text_1(f, name, data, record, start, count) result(status)
    type(nc_file), intent(inout) :: f
    character(*), intent(in) :: name
    character(*), intent(inout) :: data(:)
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int) :: varid
    integer :: length, nul
    integer(c_int64_t) :: i, first, last
    integer(c_size_t), allocatable, target :: c_start(:), c_count(:)
    character(kind=c_char,len=:), allocatable, target :: buffer
    status = LNC_EARGS
    if (.not. present(record)) &
      status = text_shape(f, name, size(data, kind=c_int64_t), varid, c_start, c_count, length, &
                          start=start, count=count)
    if (status == NC_NOERR) then
      if (len(data) < length) status = LNC_ESIZE
    end if
    if (status == NC_NOERR .and. size(data, kind=c_int64_t) > 0) then
      allocate(character(kind=c_char,len=int(length, c_int64_t)*size(data, kind=c_int64_t)) :: buffer)
      status = leave_define(f)
      if (status == NC_NOERR) status = c_nc_get_vara_text(int(f%id, c_int), varid, &
        c_loc(c_start), c_loc(c_count), c_loc(buffer(1:1)))
      if (status == NC_NOERR) then
        do i = 1, size(data, kind=c_int64_t)
          first = (i-1)*length + 1
          last = i*length
          nul = index(buffer(first:last), c_null_char)
          if (nul > 0) buffer(first+nul-1:last) = ' '
          data(i) = buffer(first:last)
        end do
      end if
    end if
    call save_error(status, 'nc_get', f, name)
  end function

  integer function text_shape(f, name, strings, varid, c_start, c_count, length, record, start, count) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: name
    integer(c_int64_t), intent(in) :: strings
    integer(c_int), intent(out) :: varid
    integer(c_size_t), allocatable, intent(out) :: c_start(:), c_count(:)
    integer, intent(out) :: length
    integer, intent(in), optional :: record, start(:), count(:)
    integer(c_int64_t) :: elements
    status = LNC_EARGS
    if (present(start) .or. present(count)) return
    status = prepare_slice(f, name, NC_CHAR, varid, c_start, c_count, elements, record)
    if (status /= NC_NOERR) return
    if (size(c_count) == 0) then
      status = LNC_EARGS
      return
    end if
    ! StrLen is first in Registry order, last in the C count vector; never hard-code 64.
    length = int(c_count(size(c_count)))
    if (length < 1) then
      status = LNC_EARGS
      return
    end if
    if (elements / int(length, c_int64_t) /= strings) status = LNC_ESIZE
  end function

  integer function check_write(f) result(status)
    type(nc_file), intent(in) :: f
    integer(c_int) :: format, mode
    ! The HDF5 backend can return NC_EHDFERR on a read-only put; report the access error first.
    status = c_nc_inq_format_extended(int(f%id, c_int), format, mode)
    if (status == NC_NOERR) then
      if (iand(mode, int(NC_WRITE, c_int)) == 0) status = NC_EPERM
    end if
  end function

  integer function leave_define(f) result(status)
    type(nc_file), intent(inout) :: f
    status = NC_NOERR
    if (.not. f%define_mode) return
    ! SMIOL leaves define mode on data I/O, external/SMIOL/smiol.c:1221,1439.
    status = c_nc_enddef(int(f%id, c_int))
    if (status == NC_NOERR) f%define_mode = .false.
  end function

  integer function enter_define(f) result(status)
    type(nc_file), intent(inout) :: f
    status = NC_NOERR
    if (f%define_mode) return
    ! SMIOL switches modes internally too: external/SMIOL/smiol.c:587,839,1714.
    status = c_nc_redef(int(f%id, c_int))
    if (status == NC_NOERR) f%define_mode = .true.
  end function

  integer function attribute_id(f, var, varid) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var
    integer(c_int), intent(out) :: varid
    status = NC_NOERR
    varid = int(NC_GLOBAL, c_int)
    if (len_trim(var) > 0) status = c_nc_inq_varid(int(f%id, c_int), trim(var) // c_null_char, varid)
  end function

  integer function attribute_info(f, var, name, memory_type, varid, length) result(status)
    type(nc_file), intent(in) :: f
    character(*), intent(in) :: var, name
    integer, intent(in) :: memory_type
    integer(c_int), intent(out) :: varid
    integer(c_size_t), intent(out) :: length
    integer(c_int) :: xtype
    status = attribute_id(f, var, varid)
    if (status /= NC_NOERR) return
    status = c_nc_inq_att(int(f%id, c_int), varid, trim(name) // c_null_char, xtype, length)
    if (status /= NC_NOERR) return
    select case (memory_type)
    case (NC_FLOAT, NC_DOUBLE)
      if (xtype /= NC_FLOAT .and. xtype /= NC_DOUBLE) status = LNC_ETYPE
    case default
      if (xtype /= memory_type) status = LNC_ETYPE
    end select
    ! The C getter reads the entire attribute; reject arrays before giving it a scalar's address.
    if (status == NC_NOERR .and. memory_type /= NC_CHAR) then
      if (length /= 1_c_size_t) status = LNC_ESIZE
    end if
  end function

  function nc_message() result(text)
    character(len=:), allocatable :: text
    text = ''
    if (allocated(last_message)) text = last_message
  end function

  subroutine save_error(status, operation, f, name)
    integer, intent(in) :: status
    character(*), intent(in) :: operation
    type(nc_file), intent(in) :: f
    character(*), intent(in), optional :: name
    character(len=:), allocatable :: reason
    character(kind=c_char), pointer :: chars(:)
    type(c_ptr) :: text
    integer :: i, length
    if (status == NC_NOERR) return
    select case (status)
    case (LNC_ETYPE)
      reason = 'incompatible memory and NetCDF types'
    case (LNC_ESIZE)
      reason = 'array size or string length does not match'
    case (LNC_EARGS)
      reason = 'invalid slice arguments'
    case (LNC_EFORMAT)
      reason = 'unsupported output format'
    case default
      text = c_nc_strerror(int(status, c_int))
      length = int(c_strlen(text))
      call c_f_pointer(text, chars, [length])
      allocate(character(len=length) :: reason)
      do i = 1, length
        reason(i:i) = chars(i)
      end do
    end select
    last_message = trim(operation)
    if (allocated(f%path)) last_message = last_message // ': ' // f%path
    if (present(name)) last_message = last_message // ': ' // trim(name)
    last_message = last_message // ': ' // reason
  end subroutine

end module
