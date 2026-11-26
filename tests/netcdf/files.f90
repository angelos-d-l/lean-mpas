! lean_nc's file handles: create, open and close in each format, the fill and access modes (checked through independent
! netCDF-C calls) and the error messages. Run by tests/netcdf/run.sh.
program files
  use iso_c_binding, only: c_int, c_float, c_double, c_int64_t
  use lean_nc
  use nc_check
  implicit none
  integer, parameter :: formats(3) = [NC_FORMAT_64BIT_DATA, NC_FORMAT_64BIT_OFFSET, NC_FORMAT_NETCDF4]
  character(len=4), parameter :: magic(3) = ['CDF' // achar(5), 'CDF' // achar(2), achar(137) // 'HDF']
  type(nc_file) :: f
  integer :: i, next, status, format
  logical :: exists, known
  character(len=32) :: path, label
  character(len=:), allocatable :: message

  ! C queries, independent of lean_nc, check the handle's fill and access modes.
  interface
    integer(c_int) function c_nc_set_fill(id, mode, old_mode) bind(C, name='nc_set_fill')
      import c_int
      integer(c_int), value :: id, mode
      integer(c_int), intent(out) :: old_mode
    end function
    integer(c_int) function c_nc_redef(id) bind(C, name='nc_redef')
      import c_int
      integer(c_int), value :: id
    end function
  end interface

  call check('new_file_closed', f%id == -1 .and. .not. f%define_mode .and. .not. allocated(f%path))
  call check('initial_message_empty', nc_message() == '')
  call check('bits_float', bits(1.0_c_float) == 1065353216)
  call check('bits_double', bits(1.0_c_double) == 4607182418800017408_c_int64_t)
  call check('bitsum_signed_patterns', bitsum([1.0_c_float, -2.0_c_float]) == -8388608_c_int64_t)

  do i = 1, size(formats)
    write(path,'(a,i0,a)') 'fmt_', formats(i), '.nc'
    write(label,'(a,i0)') 'create_', formats(i)
    status = nc_create(f, trim(path) // '   ', formats(i), .true.)
    call check(trim(label), status == NC_NOERR, nc_message())
    call check('create_handle', f%id >= 0 .and. f%define_mode .and. f%path == trim(path))
    call nofill('create_nofill')
    status = nc_close(f)
    call check('create_close', status == NC_NOERR, nc_message())
    call check('closed_handle', f%id == -1 .and. .not. f%define_mode)
    call check('create_magic', first_bytes(trim(path), 4) == magic(i), trim(path))
    status = nc_open(f, trim(path) // '   ')
    call check('open_format', status == NC_NOERR, nc_message())
    call check('open_data_mode', f%id >= 0 .and. .not. f%define_mode .and. f%path == trim(path))
    status = nc_inq_format(f, format)
    call check('inq_format', status == NC_NOERR .and. format == formats(i), nc_message())
    status = c_nc_redef(int(f%id, c_int))
    call check('open_default_readonly', status == NC_EPERM)
    status = nc_close(f)
    call check('open_close', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path), write=.true.)
    call check('open_write', status == NC_NOERR .and. .not. f%define_mode, nc_message())
    call nofill('open_write_nofill')
    status = nc_close(f)
    call check('open_write_close', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path), write=.false.)
    call check('open_explicit_readonly', status == NC_NOERR, nc_message())
    status = c_nc_redef(int(f%id, c_int))
    call check('open_false_readonly', status == NC_EPERM)
    status = nc_close(f)
    call check('open_false_close', status == NC_NOERR, nc_message())
  end do

  status = nc_create(f, 'keep.nc', formats(1), .true.)
  call check('keep_setup', status == NC_NOERR, nc_message())
  status = nc_close(f)
  call check('keep_setup_close', status == NC_NOERR, nc_message())
  do i = 1, size(formats)
    next = mod(i, size(formats)) + 1
    status = nc_create(f, 'keep.nc', formats(next), .false.)
    call check('noclobber_keeps_file', status == NC_EEXIST .and. first_bytes('keep.nc', 4) == magic(i), nc_message())
    call check('noclobber_closed', f%id == -1 .and. .not. f%define_mode)
    message = nc_message()
    call check('noclobber_message', index(message, 'keep.nc') > 0 .and. index(message, 'nc_create') > 0, message)
    status = nc_create(f, 'keep.nc', formats(next), .true.)
    call check('clobber_replaces_file', status == NC_NOERR, nc_message())
    status = nc_close(f)
    call check('clobber_close', status == NC_NOERR, nc_message())
    call check('clobber_magic', first_bytes('keep.nc', 4) == magic(next))
    call check('success_keeps_last_error', nc_message() == message)
  end do

  status = nc_close(f)
  call check('close_twice', status == NC_EBADID .and. f%id == -1 .and. .not. f%define_mode, nc_message())
  message = nc_message()
  call check('close_message', index(message, 'nc_close') > 0 .and. index(message, 'keep.nc') > 0 .and. &
             index(message, 'Not a valid ID') > 0, message)
  status = nc_inq_format(f, format)
  call check('inq_closed', status == NC_EBADID, nc_message())
  message = nc_message()
  call check('inq_message', index(message, 'nc_inq_format') > 0 .and. index(message, 'keep.nc') > 0, message)

  status = nc_open(f, 'missing.nc')
  call check('open_missing', status /= NC_NOERR .and. f%id == -1 .and. .not. f%define_mode, nc_message())
  message = nc_message()
  call check('missing_message', index(message, 'missing.nc') > 0 .and. index(message, 'No such file') > 0 .and. &
             index(message, 'nc_open') > 0, message)

  status = nc_create(f, 'classic.nc', NC_FORMAT_CLASSIC, .true.)
  inquire(file='classic.nc', exist=exists)
  call check('classic_refused', status == LNC_EFORMAT .and. .not. exists .and. f%id == -1, nc_message())
  message = nc_message()
  call check('format_message', index(message, 'nc_create') > 0 .and. index(message, 'classic.nc') > 0 .and. &
             index(message, 'format') > 0, message)
  status = nc_create(f, 'classic.nc', NC_FORMAT_NETCDF4_CLASSIC, .true.)
  inquire(file='classic.nc', exist=exists)
  call check('netcdf4_classic_refused', status == LNC_EFORMAT .and. .not. exists, nc_message())
  status = nc_create(f, 'classic.nc', -999, .true.)
  inquire(file='classic.nc', exist=exists)
  call check('unknown_format_refused', status == LNC_EFORMAT .and. .not. exists, nc_message())

  call io_type('pnetcdf', NC_FORMAT_64BIT_DATA, .true.)
  call io_type('pnetcdf,cdf5', NC_FORMAT_64BIT_DATA, .true.)
  call io_type('netcdf', NC_FORMAT_64BIT_OFFSET, .true.)
  call io_type('netcdf4', NC_FORMAT_NETCDF4, .true.)
  call io_type('xnetcdf4y', NC_FORMAT_NETCDF4, .true.)
  call io_type('NETCDF4', NC_FORMAT_64BIT_DATA, .false.)
  call io_type('', NC_FORMAT_64BIT_DATA, .false.)
  call io_type('netcdf4,pnetcdf', NC_FORMAT_64BIT_DATA, .true.)
  call check('io_type_without_known', nc_io_type_format('netcdf4') == NC_FORMAT_NETCDF4)
  call finish('files')

contains

  subroutine nofill(name)
    character(*), intent(in) :: name
    integer(c_int) :: old_mode, code
    code = c_nc_set_fill(int(f%id, c_int), 256_c_int, old_mode) ! NC_NOFILL from netcdf.h
    call check(name, code == NC_NOERR .and. old_mode == 256_c_int)
  end subroutine

  subroutine io_type(text, expected_format, expected_known)
    character(*), intent(in) :: text
    integer, intent(in) :: expected_format
    logical, intent(in) :: expected_known
    format = nc_io_type_format(text, known)
    call check('io_type ' // text, format == expected_format .and. (known .eqv. expected_known))
  end subroutine

end program
