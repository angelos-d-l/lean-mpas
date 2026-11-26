! lean_nc's file definitions (dimensions, variables, attributes) in each file format, checked through independent
! netCDF-C calls; tests/netcdf/run.sh then checks the files with ncdump.
program define
  use iso_c_binding, only: c_int, c_float, c_double, c_char, c_size_t, c_ptr, c_loc, c_null_ptr, c_null_char
  use lean_nc
  use nc_check
  implicit none
  integer, parameter :: formats(3) = [NC_FORMAT_64BIT_DATA, NC_FORMAT_64BIT_OFFSET, NC_FORMAT_NETCDF4]
  character(len=*), parameter :: parent_id = 'aifb661d2y' // achar(10) // 'z8n1exeq3z' // achar(10)
  type(nc_file) :: f
  integer :: i, status, n, xtype
  logical :: unlimited
  character(len=32) :: path
  character(len=:), allocatable :: text
  character(len=NC_MAX_NAME), allocatable :: dims(:)
  integer, allocatable :: lengths(:)
  integer(c_int) :: int_value
  real(c_float) :: float_value
  real(c_double) :: double_value
  integer(c_int), target :: int_pair(2) = [2_c_int, 3_c_int]
  real(c_double), target :: double_pair(2) = [0.1_c_double, 0.2_c_double]

  ! Independent C writes make attributes that the scalar-only lean_nc writer cannot create.
  interface
    integer(c_int) function c_nc_put_att_int(id, varid, name, xtype, length, values) bind(C, name='nc_put_att_int')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid, xtype
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: values
    end function
    integer(c_int) function c_nc_put_att_double(id, varid, name, xtype, length, values) bind(C, name='nc_put_att_double')
      import c_int, c_char, c_size_t, c_ptr
      integer(c_int), value :: id, varid, xtype
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: length
      type(c_ptr), value :: values
    end function
  end interface

  do i = 1, size(formats)
    write(path,'(a,i0,a)') 'define_', formats(i), '.nc'
    status = nc_create(f, trim(path), formats(i), .true.)
    call check('create_' // trim(path), status == NC_NOERR, nc_message())
    call registry_definitions()
    call put_attributes()
    call define_errors()
    status = nc_close(f)
    call check('define_close', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path))
    call check('reopen_definitions', status == NC_NOERR, nc_message())
    call registry_order()
    call attributes()
    call attribute_errors()
    status = nc_close(f)
    call check('inquiry_close', status == NC_NOERR, nc_message())
    call define_after_open()
  end do
  call finish('define')

contains

  subroutine registry_definitions()
    status = nc_def_dim(f, 'nVertLevels', 2)
    call check('def_nVertLevels', status == NC_NOERR, nc_message())
    status = nc_def_dim(f, 'nCells   ', 3)
    call check('def_nCells_padded', status == NC_NOERR, nc_message())
    status = nc_def_dim(f, 'StrLen', 64)
    call check('def_StrLen', status == NC_NOERR, nc_message())
    status = nc_def_dim(f, 'Time', NC_UNLIMITED)
    call check('def_Time', status == NC_NOERR, nc_message())
    status = nc_def_var(f, 'theta   ', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nVertLevels', 'nCells', 'Time'])
    call check('def_theta_padded', status == NC_NOERR, nc_message())
    status = nc_def_var(f, 'xtime', NC_CHAR, [character(len=NC_MAX_NAME) :: 'StrLen', 'Time'])
    call check('def_xtime', status == NC_NOERR, nc_message())
    status = nc_def_var(f, 'ids', NC_INT, [character(len=NC_MAX_NAME) :: 'nCells'])
    call check('def_ids', status == NC_NOERR, nc_message())
    status = nc_def_var(f, 'radius', NC_DOUBLE, [character(len=NC_MAX_NAME) ::])
    call check('def_scalar', status == NC_NOERR, nc_message())
  end subroutine

  subroutine registry_order()
    call variable('theta', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nVertLevels', 'nCells', 'Time'], [2, 3, 0])
    call variable('xtime', NC_CHAR, [character(len=NC_MAX_NAME) :: 'StrLen', 'Time'], [64, 0])
    call variable('ids', NC_INT, [character(len=NC_MAX_NAME) :: 'nCells'], [3])
    call variable('radius', NC_DOUBLE, [character(len=NC_MAX_NAME) ::], [integer ::])
    status = nc_inq_dim(f, 'Time', n, unlimited)
    call check('Time_unlimited_empty', status == NC_NOERR .and. n == 0 .and. unlimited, nc_message())
    status = nc_inq_dim(f, 'nCells', n, unlimited)
    call check('nCells_fixed', status == NC_NOERR .and. n == 3 .and. .not. unlimited, nc_message())
    status = nc_inq_var(f, 'theta   ')
    call check('inq_var_padded_no_outputs', status == NC_NOERR, nc_message())
    status = nc_inq_dim(f, 'nCells   ', n)
    call check('inq_dim_padded_no_unlimited', status == NC_NOERR .and. n == 3, nc_message())
    status = nc_inq_var(f, 'theta', xtype=xtype)
    call check('inq_var_type_only', status == NC_NOERR .and. xtype == NC_FLOAT, nc_message())
    status = nc_inq_var(f, 'theta', dims=dims)
    call check('inq_var_dims_only', status == NC_NOERR, nc_message())
    if (status == NC_NOERR .and. allocated(dims)) then
      call check('inq_var_dims_only_count', size(dims) == 3)
      if (size(dims) == 3) call check('inq_var_dims_only_order', &
        all(dims == [character(len=NC_MAX_NAME) :: 'nVertLevels', 'nCells', 'Time']))
    end if
    status = nc_inq_var(f, 'theta', lengths=lengths)
    call check('inq_var_lengths_only', status == NC_NOERR, nc_message())
    if (status == NC_NOERR .and. allocated(lengths)) then
      call check('inq_var_lengths_only_count', size(lengths) == 3)
      if (size(lengths) == 3) call check('inq_var_lengths_only_order', all(lengths == [2, 3, 0]))
    end if
  end subroutine

  subroutine variable(name, expected_type, expected_dims, expected_lengths)
    character(*), intent(in) :: name, expected_dims(:)
    integer, intent(in) :: expected_type, expected_lengths(:)
    status = nc_inq_var(f, name, xtype, dims, lengths)
    call check('inq_' // name, status == NC_NOERR, nc_message())
    if (status /= NC_NOERR) return
    call check(name // '_type', xtype == expected_type)
    call check(name // '_dims_allocated', allocated(dims))
    call check(name // '_lengths_allocated', allocated(lengths))
    if (allocated(dims)) then
      call check(name // '_dimension_count', size(dims) == size(expected_dims))
      if (size(dims) == size(expected_dims)) call check(name // '_registry_dims', all(dims == expected_dims))
    end if
    if (allocated(lengths)) then
      call check(name // '_length_count', size(lengths) == size(expected_lengths))
      if (size(lengths) == size(expected_lengths)) call check(name // '_registry_lengths', all(lengths == expected_lengths))
    end if
  end subroutine

  subroutine put_attributes()
    status = nc_put_att(f, '', 'model_name', 'mpas')
    call check('put_model_name', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'parent_id', parent_id)
    call check('put_parent_id', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'config_time_integration_order', 2_c_int)
    call check('put_integer', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'sphere_radius', 6371229.0_c_float)
    call check('put_float', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'x_period', 0.1_c_double)
    call check('put_double', status == NC_NOERR, nc_message())
    status = nc_put_att(f, 'theta   ', 'units   ', 'K')
    call check('put_units_padded_names', status == NC_NOERR, nc_message())
    status = nc_put_att(f, 'theta', '_FillValue', -9999.0_c_float)
    call check('put_FillValue', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '   ', 'trailing   ', 'mpas   ')
    call check('put_trailing_blanks', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'empty', '')
    call check('put_empty_text', status == NC_NOERR, nc_message())
    status = nc_put_att(f, '', 'nul_text', 'mpas' // achar(0) // 'ignored')
    call check('put_nul_text', status == NC_NOERR, nc_message())
    status = c_nc_put_att_int(int(f%id, c_int), -1_c_int, 'int_pair' // c_null_char, &
                             int(NC_INT, c_int), 2_c_size_t, c_loc(int_pair))
    call check('C_put_int_pair', status == NC_NOERR)
    status = c_nc_put_att_double(int(f%id, c_int), -1_c_int, 'double_pair' // c_null_char, &
                                int(NC_DOUBLE, c_int), 2_c_size_t, c_loc(double_pair))
    call check('C_put_double_pair', status == NC_NOERR)
    status = c_nc_put_att_int(int(f%id, c_int), -1_c_int, 'empty_int' // c_null_char, &
                             int(NC_INT, c_int), 0_c_size_t, c_null_ptr)
    call check('C_put_empty_int', status == NC_NOERR)
  end subroutine

  subroutine attributes()
    call text_attribute('', 'model_name', 'mpas')
    call text_attribute('', 'parent_id', parent_id)
    call text_attribute('theta   ', 'units   ', 'K')
    call text_attribute('   ', 'trailing   ', 'mpas   ')
    call text_attribute('', 'empty', '')
    call text_attribute('', 'nul_text', 'mpas        ')
    status = nc_get_att(f, '', 'config_time_integration_order', int_value)
    call check('get_integer_bits', status == NC_NOERR .and. int_value == 2_c_int, nc_message())
    status = nc_get_att(f, '', 'sphere_radius', float_value)
    call check('get_float_bits', status == NC_NOERR .and. bits(float_value) == bits(6371229.0_c_float), nc_message())
    status = nc_get_att(f, '', 'x_period', double_value)
    call check('get_double_bits', status == NC_NOERR .and. bits(double_value) == bits(0.1_c_double), nc_message())
    status = nc_get_att(f, 'theta', '_FillValue', float_value)
    call check('get_FillValue_bits', status == NC_NOERR .and. bits(float_value) == bits(-9999.0_c_float), nc_message())
    status = nc_get_att(f, '', 'sphere_radius', double_value)
    call check('get_widen_bits', status == NC_NOERR .and. bits(double_value) == bits(6371229.0_c_double), nc_message())
    status = nc_get_att(f, '', 'x_period', float_value)
    call check('get_narrow_bits', status == NC_NOERR .and. bits(float_value) == bits(real(0.1_c_double, c_float)), nc_message())
  end subroutine

  subroutine text_attribute(var, name, expected)
    character(*), intent(in) :: var, name, expected
    status = nc_get_att(f, var, name, text)
    call check('get_text_' // trim(name), status == NC_NOERR, nc_message())
    call check('text_allocated_' // trim(name), allocated(text))
    if (allocated(text)) then
      call check('text_length_' // trim(name), len(text) == len(expected))
      call check('text_content_' // trim(name), text == expected)
    end if
  end subroutine

  subroutine attribute_errors()
    status = nc_get_att(f, 'theta', 'missing', text)
    call check('missing_attribute', status == NC_ENOTATT, nc_message())
    call error_message('nc_get_att', 'theta', 'missing')
    status = nc_get_att(f, '', 'model_name', int_value)
    call check('text_into_int', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'sphere_radius', text)
    call check('float_into_text', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'config_time_integration_order', float_value)
    call check('int_into_float', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'config_time_integration_order', double_value)
    call check('int_into_double', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'x_period', int_value)
    call check('double_into_int', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'model_name', float_value)
    call check('text_into_float', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'model_name', double_value)
    call check('text_into_double', status == LNC_ETYPE, nc_message())
    status = nc_get_att(f, '', 'int_pair', int_value)
    call check('int_pair_refused', status == LNC_ESIZE, nc_message())
    call error_message('nc_get_att', '', 'int_pair')
    status = nc_get_att(f, '', 'double_pair', double_value)
    call check('double_pair_refused', status == LNC_ESIZE, nc_message())
    status = nc_get_att(f, '', 'double_pair', float_value)
    call check('float_pair_refused', status == LNC_ESIZE, nc_message())
    status = nc_get_att(f, '', 'empty_int', int_value)
    call check('empty_numeric_refused', status == LNC_ESIZE, nc_message())
  end subroutine

  subroutine define_errors()
    status = nc_def_var(f, 'bad_dim', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nEdges'])
    call check('missing_dimension', status == NC_EBADDIM, nc_message())
    call error_message('nc_def_var', 'bad_dim', 'nEdges')
    status = nc_def_var(f, 'theta', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nVertLevels', 'nCells', 'Time'])
    call check('duplicate_variable', status == NC_ENAMEINUSE, nc_message())
    status = nc_def_dim(f, 'nCells', 3)
    call check('duplicate_dimension', status == NC_ENAMEINUSE, nc_message())
    status = nc_inq_var(f, 'nope')
    call check('missing_variable', status == NC_ENOTVAR, nc_message())
    call error_message('nc_inq_var', 'nope', '')
    status = nc_inq_dim(f, 'nEdges', n)
    call check('inq_missing_dimension', status == NC_EBADDIM, nc_message())
    call error_message('nc_inq_dim', 'nEdges', '')
    status = nc_def_var(f, 'bad_type', 3, [character(len=NC_MAX_NAME) :: 'nCells']) ! NC_SHORT is outside SMIOL's four.
    call check('unsupported_variable_type', status == LNC_ETYPE, nc_message())
  end subroutine

  subroutine error_message(operation, name, other)
    character(*), intent(in) :: operation, name, other
    character(len=:), allocatable :: message
    message = nc_message()
    call check('message_' // operation, index(message, operation) > 0 .and. index(message, trim(path)) > 0 .and. &
               index(message, name) > 0 .and. index(message, other) > 0, message)
  end subroutine

  subroutine define_after_open()
    status = nc_open(f, trim(path), write=.true.)
    call check('open_for_history', status == NC_NOERR .and. .not. f%define_mode, nc_message())
    status = nc_put_att(f, '', 'history', 'lean')
    call check('history_enters_define', status == NC_NOERR .and. f%define_mode, nc_message())
    status = nc_def_var(f, 'extra', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nCells'])
    call check('define_extra', status == NC_NOERR, nc_message())
    status = nc_close(f)
    call check('close_history', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path), write=.true.)
    call check('open_for_dimension', status == NC_NOERR .and. .not. f%define_mode, nc_message())
    status = nc_def_dim(f, 'extra_dim', 4)
    call check('dim_enters_define', status == NC_NOERR .and. f%define_mode, nc_message())
    status = nc_close(f)
    call check('close_extra_dimension', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path), write=.true.)
    call check('open_for_variable', status == NC_NOERR .and. .not. f%define_mode, nc_message())
    status = nc_def_var(f, 'extra_scalar', NC_INT, [character(len=NC_MAX_NAME) ::])
    call check('var_enters_define', status == NC_NOERR .and. f%define_mode, nc_message())
    status = nc_close(f)
    call check('close_extra_variable', status == NC_NOERR, nc_message())
    status = nc_open(f, trim(path))
    call check('reopen_extras', status == NC_NOERR, nc_message())
    call text_attribute('', 'history', 'lean')
    call variable('extra', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nCells'], [3])
    call variable('extra_scalar', NC_INT, [character(len=NC_MAX_NAME) ::], [integer ::])
    status = nc_inq_dim(f, 'extra_dim', n, unlimited)
    call check('extra_dimension_persisted', status == NC_NOERR .and. n == 4 .and. .not. unlimited, nc_message())
    status = nc_put_att(f, '', 'readonly', 'lean')
    call check('readonly_put_refused', status == NC_EPERM .and. .not. f%define_mode, nc_message())
    status = nc_def_dim(f, 'readonly', 1)
    call check('readonly_dim_refused', status == NC_EPERM .and. .not. f%define_mode, nc_message())
    status = nc_def_var(f, 'readonly', NC_FLOAT, [character(len=NC_MAX_NAME) ::])
    call check('readonly_var_refused', status == NC_EPERM .and. .not. f%define_mode, nc_message())
    status = nc_close(f)
    call check('close_extras', status == NC_NOERR, nc_message())
  end subroutine

end program
