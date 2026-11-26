! lean_nc's reads and writes: slices, records, appends, type conversions (bits, NaN), strings and their padding, and the
! refusals of out-of-bounds requests. Run by tests/netcdf/run.sh.
program slices
  use iso_c_binding, only: c_int, c_float, c_double
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_is_nan
  use lean_nc
  use nc_check
  implicit none
  integer, parameter :: formats(3) = [NC_FORMAT_64BIT_DATA, NC_FORMAT_64BIT_OFFSET, NC_FORMAT_NETCDF4]
  character(len=19), parameter :: stamps(3) = [ &
    '2026-10-11_06:00:00', '2026-10-11_07:00:00', '2026-10-11_08:00:00']
  type(nc_file) :: f
  integer :: fmt, status, n, t, i
  character(len=32) :: path
  real(c_double) :: v8(10)
  real(c_float) :: v4(10), plane(2,3)

  v8 = [1d0+2d0**(-24), 1d0+3d0*2d0**(-24), 0.1d0, -0d0, 1d-40, 1d-46, &
        real(huge(1._c_float), c_double), -2.5d0, 6371229d0, ieee_value(0d0, ieee_quiet_nan)]
  v4 = real(v8, c_float)
  do fmt = 1, size(formats)
    write(path,'(a,i0,a)') 'slices_', formats(fmt), '.nc'
    status = nc_create(f, trim(path), formats(fmt), .true.)
    call good('create_' // trim(path))
    call definitions()
    call write_records()
    call conversions_put()
    call nul_padding_put()
    status = nc_put(f, 'radius', 6371229.5d0)
    call good('put_double_scalar')
    status = nc_put(f, 'nsteps', 720_c_int)
    call good('put_int_scalar')
    status = nc_close(f)
    call good('close_written')
    status = nc_open(f, trim(path))
    call good('reopen_written')
    call write_read()
    call strings()
    call nul_padding()
    call conversions_get()
    call scalars()
    call slice_errors()
    call readonly_put()
    status = nc_close(f)
    call good('close_readonly')
    call append()
    call overwrite()
    call empty_record()
    call order_file()
  end do
  call finish('slices')

contains

  subroutine good(name)
    character(*), intent(in) :: name
    call check(name, status == NC_NOERR, nc_message())
  end subroutine

  subroutine definitions()
    status = nc_def_dim(f, 'nVertLevels', 2)
    call good('def_nVertLevels')
    status = nc_def_dim(f, 'nCells', 3)
    call good('def_nCells')
    status = nc_def_dim(f, 'StrLen', 64)
    call good('def_StrLen')
    status = nc_def_dim(f, 'nValues', 10)
    call good('def_nValues')
    status = nc_def_dim(f, 'TWO', 2)
    call good('def_TWO')
    status = nc_def_dim(f, 'Time', NC_UNLIMITED)
    call good('def_Time')
    call record_definitions()
    status = nc_def_var(f, 'ids', NC_INT, ['nCells'])
    call good('def_ids')
    status = nc_def_var(f, 'note', NC_CHAR, ['StrLen'])
    call good('def_note')
    status = nc_def_var(f, 'short_note', NC_CHAR, ['TWO'])
    call good('def_short_note')
    status = nc_def_var(f, 'd8', NC_DOUBLE, ['nValues'])
    call good('def_d8')
    status = nc_def_var(f, 'w8', NC_DOUBLE, ['nValues'])
    call good('def_w8')
    status = nc_def_var(f, 'f4', NC_FLOAT, ['nValues'])
    call good('def_f4')
    status = nc_def_var(f, 'big8', NC_DOUBLE, ['TWO'])
    call good('def_big8')
    status = nc_def_var(f, 'small4', NC_FLOAT, ['TWO'])
    call good('def_small4')
    status = nc_def_var(f, 'radius', NC_DOUBLE, [character(len=NC_MAX_NAME) ::])
    call good('def_radius')
    status = nc_def_var(f, 'nsteps', NC_INT, [character(len=NC_MAX_NAME) ::])
    call good('def_nsteps')
  end subroutine

  subroutine record_definitions()
    status = nc_def_var(f, 'theta', NC_FLOAT, [character(len=NC_MAX_NAME) :: 'nVertLevels', 'nCells', 'Time'])
    call good('def_theta')
    status = nc_def_var(f, 'xtime', NC_CHAR, [character(len=NC_MAX_NAME) :: 'StrLen', 'Time'])
    call good('def_xtime')
  end subroutine

  function formula(record) result(values)
    integer, intent(in) :: record
    real(c_float) :: values(2,3)
    integer :: k, c
    do c = 1, 3
      do k = 1, 2
        values(k,c) = real(1000*record + 100*c + k, c_float)
      end do
    end do
  end function

  subroutine write_records()
    do t = 1, 2
      plane = formula(t)
      status = nc_put(f, 'theta   ', plane, record=t)
      call check('put_theta_record_leaves_define', status == NC_NOERR .and. .not. f%define_mode, nc_message())
      status = nc_put(f, 'xtime', stamps(t), record=t)
      call good('put_xtime_record')
    end do
  end subroutine

  subroutine conversions_put()
    status = nc_put(f, 'ids', [11_c_int, 12_c_int, 13_c_int])
    call good('put_ids')
    status = nc_put(f, 'd8', v8)
    call good('put_double_into_double')
    status = nc_put(f, 'f4', v8)
    call good('put_double_into_float_var')
    status = nc_put(f, 'w8', v4)
    call good('put_single_into_double')
    status = nc_put(f, 'big8', [1d39, ieee_value(0d0, ieee_positive_inf)])
    call good('put_big8')
    status = nc_put(f, 'small4', [1d39, 0d0])
    call check('out_of_range_put', status == NC_ERANGE, nc_message())
    call message('nc_put', 'small4')
  end subroutine

  subroutine nul_padding_put()
    status = nc_put(f, 'note', 'abc' // achar(0) // 'xyz')
    call good('put_note_with_nul')
    status = nc_put(f, 'short_note', 'a')
    call good('put_string_length_from_dimension')
    status = nc_put_att(f, '', 'nul_text', 'lean' // achar(0))
    call check('attribute_reenters_define', status == NC_NOERR .and. f%define_mode, nc_message())
    status = nc_get(f, 'theta', plane, record=1)
    call check('get_leaves_define', status == NC_NOERR .and. .not. f%define_mode .and. &
               all(plane == formula(1)), nc_message())
  end subroutine

  subroutine write_read()
    real(c_float) :: whole(2,3,2), rec(2,3), level(3,2), scalar
    integer(c_int) :: ids(3)
    whole = -1
    status = nc_get(f, 'theta   ', whole)
    call good('get_whole_rank3')
    do t = 1, 2
      call check('whole_formula', all(whole(:,:,t) == formula(t)))
    end do
    rec = -1
    status = nc_get(f, 'theta', rec, record=2)
    call check('get_record_rank2', status == NC_NOERR .and. all(rec == formula(2)), nc_message())
    level = -1
    status = nc_get(f, 'theta', level, start=[2,1,1], count=[1,3,2])
    call good('get_level_rank2')
    do t = 1, 2
      do i = 1, 3
        call check('level_formula', level(i,t) == 1000*t+100*i+2)
      end do
    end do
    scalar = -1
    status = nc_get(f, 'theta', scalar, start=[1,2,2], count=[1,1,1])
    call check('get_element_rank0', status == NC_NOERR .and. scalar == 2201, nc_message())
    ids = -1
    status = nc_get(f, 'ids', ids)
    call check('get_ids', status == NC_NOERR .and. all(ids == [11,12,13]), nc_message())
  end subroutine

  subroutine strings()
    character(len=64) :: stamp
    character(len=80) :: times(2)
    stamp = '?'
    status = nc_get(f, 'xtime', stamp, record=1)
    call check('get_xtime_record', status == NC_NOERR .and. trim(stamp) == stamps(1) .and. &
               len_trim(stamp) == 19, nc_message())
    times = '?'
    status = nc_get(f, 'xtime', times)
    call good('get_xtime_whole')
    call check('get_two_strings', all(times == stamps(1:2)))
    call check('strings_blank_padded', times(1)(20:) == ' ' .and. times(2)(20:) == ' ')
  end subroutine

  subroutine nul_padding()
    character(len=64) :: note
    character(len=2) :: short_note
    character(len=80) :: notes(1)
    character(len=:), allocatable :: value
    note = '?'
    status = nc_get(f, 'note', note)
    call check('nul_cut_at_first', status == NC_NOERR .and. trim(note) == 'abc', nc_message())
    notes = '?'
    status = nc_get(f, 'note', notes)
    call check('array_nul_cut_and_padding', status == NC_NOERR .and. all(notes == 'abc'), nc_message())
    short_note = '?'
    status = nc_get(f, 'short_note', short_note)
    call check('get_string_length_from_dimension', status == NC_NOERR .and. short_note == 'a ', nc_message())
    status = nc_get_att(f, '', 'nul_text', value)
    call good('get_nul_attribute')
    if (allocated(value)) then
      call check('attribute_nul_length', len(value) == 5)
      if (len(value) == 5) call check('attribute_nul_padding', value(1:4) == 'lean' .and. value(5:5) == ' ')
    end if
  end subroutine

  subroutine conversions_get()
    real(c_float) :: values4(10), big4(2)
    real(c_double) :: values8(10)
    values4 = -1
    status = nc_get(f, 'd8', values4)
    call good('double_to_single')
    do i = 1, 9
      call check('double_to_single_bits', bits(values4(i)) == bits(real(v8(i), c_float)))
    end do
    call check('double_to_single_nan', ieee_is_nan(values4(10)))
    status = nc_get(f, 'f4', values4)
    call good('double_into_float_var')
    do i = 1, 9
      call check('double_into_float_bits', bits(values4(i)) == bits(real(v8(i), c_float)))
    end do
    call check('double_into_float_nan', ieee_is_nan(values4(10)))
    values8 = -1
    status = nc_get(f, 'w8', values8)
    call good('single_to_double')
    do i = 1, 9
      call check('single_to_double_bits', bits(values8(i)) == bits(real(v4(i), c_double)))
    end do
    call check('single_to_double_nan', ieee_is_nan(values8(10)))
    status = nc_get(f, 'big8', big4)
    call check('out_of_range_get', status == NC_ERANGE, nc_message())
    call message('nc_get', 'big8')
  end subroutine

  subroutine scalars()
    real(c_double) :: radius8
    real(c_float) :: radius4
    integer(c_int) :: nsteps
    radius8 = -1
    status = nc_get(f, 'radius', radius8)
    call check('get_double_scalar', status == NC_NOERR .and. bits(radius8) == bits(6371229.5d0), nc_message())
    radius4 = -1
    status = nc_get(f, 'radius', radius4)
    call check('get_scalar_into_float', status == NC_NOERR .and. &
               bits(radius4) == bits(real(6371229.5d0, c_float)), nc_message())
    nsteps = -1
    status = nc_get(f, 'nsteps', nsteps)
    call check('get_int_scalar', status == NC_NOERR .and. nsteps == 720, nc_message())
  end subroutine

  subroutine slice_errors()
    real(c_float) :: short(2,3,1), flat(6), ids4(3)
    integer(c_int) :: theta_int(2,3,2), ids(3)
    character(len=10) :: too_short
    character(len=64) :: text
    character(len=80) :: wrong_times(1)
    short = -1
    status = nc_get(f, 'theta', short)
    call check('whole_size_error_unchanged', status == LNC_ESIZE .and. all(short == -1), nc_message())
    call message('nc_get', 'theta')
    ids4 = -1
    ids = -1
    status = nc_get(f, 'ids', ids, record=1)
    call check('record_without_Time', status == LNC_EARGS, nc_message())
    status = nc_get(f, 'theta', short, record=1, start=[1,1,1])
    call check('record_with_start', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'theta', short, start=[1,1], count=[2,3,1])
    call check('wrong_start_size', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'theta', short, start=[1,1,1])
    call check('start_without_count', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'theta', short, count=[2,3,1])
    call check('count_without_start', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'theta', short, start=[1,1,1], count=[2,3])
    call check('wrong_count_size', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'ids', ids4)
    call check('int_into_float', status == LNC_ETYPE .and. all(ids4 == -1), nc_message())
    theta_int = -1
    status = nc_get(f, 'theta', theta_int)
    call check('float_into_int', status == LNC_ETYPE .and. all(theta_int == -1), nc_message())
    status = nc_get(f, 'xtime', ids4)
    call check('text_into_float', status == LNC_ETYPE .and. all(ids4 == -1), nc_message())
    status = nc_put(f, 'xtime', repeat('x', 65), record=1)
    call check('long_string_put', status == LNC_ESIZE, nc_message())
    too_short = '?'
    status = nc_get(f, 'xtime', too_short, record=1)
    call check('short_string_get_unchanged', status == LNC_ESIZE .and. too_short == '?', nc_message())
    status = nc_get(f, 'theta', short, record=0)
    call check('zero_record_unchanged', status == LNC_EARGS .and. all(short == -1), nc_message())
    status = nc_get(f, 'theta', short, start=[0,1,1], count=[1,1,1])
    call check('zero_start_unchanged', status == LNC_EARGS .and. all(short == -1), nc_message())
    flat = -1
    status = nc_get(f, 'theta', flat, start=[1,1,1], count=[-2,-3,1])
    call check('negative_counts_unchanged', status == LNC_EARGS .and. all(flat == -1), nc_message())
    status = nc_get(f, 'theta', flat, start=[1,1,1], count=[65536,65536,1])
    call check('int64_size_unchanged', status == LNC_ESIZE .and. all(flat == -1), nc_message())
    status = nc_get(f, 'theta', flat, start=[1,1,1], count=[huge(1),huge(1),huge(1)])
    call check('size_product_overflow_unchanged', status == LNC_ESIZE .and. all(flat == -1), nc_message())
    status = nc_get(f, 'theta', short, start=[1,1,5], count=[2,3,1])
    call check('C_out_of_bounds', status == NC_EINVALCOORDS .or. status == NC_EEDGE, nc_message())
    call message('nc_get', 'theta')
    text = '?'
    status = nc_get(f, 'xtime', text, start=[1,1], count=[64,1])
    call check('text_slice_refused_unchanged', status == LNC_EARGS .and. text == '?', nc_message())
    status = nc_put(f, 'xtime', stamps(1), start=[1,1], count=[64,1])
    call check('text_put_slice_refused', status == LNC_EARGS, nc_message())
    wrong_times = '?'
    status = nc_get(f, 'xtime', wrong_times)
    call check('text_array_size_unchanged', status == LNC_ESIZE .and. all(wrong_times == '?'), nc_message())
    status = nc_get(f, 'xtime', wrong_times, record=1)
    call check('text_array_record_refused', status == LNC_EARGS .and. all(wrong_times == '?'), nc_message())
    status = nc_get(f, 'missing', short)
    call check('get_missing_variable', status == NC_ENOTVAR, nc_message())
    call message('nc_get', 'missing')
  end subroutine

  subroutine readonly_put()
    plane = formula(1)
    status = nc_put(f, 'theta', plane, record=1)
    call check('readonly_put', status == NC_EPERM, nc_message())
    call message('nc_put', 'theta')
    status = nc_put(f, 'xtime', stamps(1), record=1)
    call check('readonly_put_text', status == NC_EPERM, nc_message())
    call message('nc_put', 'xtime')
  end subroutine

  subroutine message(operation, name)
    character(*), intent(in) :: operation, name
    character(len=:), allocatable :: text
    text = nc_message()
    call check('message_' // operation, index(text, operation) > 0 .and. index(text, trim(path)) > 0 .and. &
               index(text, name) > 0, text)
  end subroutine

  subroutine append()
    real(c_float) :: whole(2,3,3)
    character(len=80) :: times(3)
    status = nc_open(f, trim(path), write=.true.)
    call good('open_append')
    status = nc_inq_dim(f, 'Time', n)
    call check('append_starts_at_two', status == NC_NOERR .and. n == 2, nc_message())
    plane = formula(3)
    status = nc_put(f, 'theta', plane, record=3)
    call good('put_third_record')
    status = nc_put(f, 'xtime', stamps(3), record=3)
    call good('put_third_xtime')
    status = nc_close(f)
    call good('close_append')
    status = nc_open(f, trim(path))
    call good('reopen_append')
    status = nc_inq_dim(f, 'Time', n)
    call check('append_has_three', status == NC_NOERR .and. n == 3, nc_message())
    whole = -1
    status = nc_get(f, 'theta', whole)
    call good('get_appended')
    do t = 1, 3
      call check('append_formula_old_and_new', all(whole(:,:,t) == formula(t)))
    end do
    times = '?'
    status = nc_get(f, 'xtime', times)
    call check('append_strings_old_and_new', status == NC_NOERR .and. all(times == stamps), nc_message())
    status = nc_close(f)
    call good('close_append_check')
  end subroutine

  subroutine overwrite()
    real(c_float) :: whole(2,3,3)
    status = nc_open(f, trim(path), write=.true.)
    call good('open_overwrite')
    plane = formula(2) + 0.5_c_float
    status = nc_put(f, 'theta', plane, record=2)
    call good('overwrite_second_record')
    call put_errors()
    call numeric_ranks()
    status = nc_close(f)
    call good('close_overwrite')
    status = nc_open(f, trim(path))
    call good('reopen_overwrite')
    whole = -1
    status = nc_get(f, 'theta', whole)
    call good('get_overwritten')
    call check('overwrite_first_unchanged', all(whole(:,:,1) == formula(1)))
    call check('overwrite_second_new', all(whole(:,:,2) == formula(2)+0.5_c_float))
    call check('overwrite_third_unchanged', all(whole(:,:,3) == formula(3)))
    status = nc_close(f)
    call good('close_overwrite_check')
  end subroutine

  subroutine put_errors()
    status = nc_put(f, 'theta', [-1._c_float])
    call check('put_size_error', status == LNC_ESIZE, nc_message())
    status = nc_put(f, 'theta', plane, record=0)
    call check('put_record_error', status == LNC_EARGS, nc_message())
    status = nc_put(f, 'theta', plane, record=1, count=[2,3,1])
    call check('put_record_with_count_error', status == LNC_EARGS, nc_message())
    status = nc_put(f, 'theta', plane, start=[0,1,1], count=[2,3,1])
    call check('put_start_error', status == LNC_EARGS, nc_message())
    status = nc_put(f, 'theta', plane, start=[1,1,1], count=[-2,-3,1])
    call check('put_negative_counts_error', status == LNC_EARGS, nc_message())
    status = nc_put(f, 'theta', [1_c_int], start=[1,1,1], count=[1,1,1])
    call check('put_type_error', status == LNC_ETYPE, nc_message())
  end subroutine

  subroutine numeric_ranks()
    real(c_float) :: r4(1), r42(1,1), r43(1,1,1)
    real(c_double) :: r8(1), r82(1,1), r83(1,1,1)
    integer(c_int) :: ints(3), ints2(1,3), ints3(1,1,3)
    ! Different values expose a put wrapper that succeeds without writing.
    r4 = 1.25_c_float
    status = nc_put(f, 'radius', r4(1))
    call good('put_float_rank0')
    r8 = -1
    status = nc_get(f, 'radius', r8(1))
    call check('put_float_rank0_value', status == NC_NOERR .and. r8(1) == 1.25d0, nc_message())
    r43 = 2.25_c_float
    status = nc_put(f, 'radius', r43)
    call good('put_float_rank3')
    r8 = -1
    status = nc_get(f, 'radius', r8(1))
    call check('put_float_rank3_value', status == NC_NOERR .and. r8(1) == 2.25d0, nc_message())
    r42 = 3.25_c_float
    status = nc_put(f, 'radius', r42)
    call good('put_float_rank2_scalar_var')
    r8 = -1
    status = nc_get(f, 'radius', r8(1))
    call check('put_float_rank2_value', status == NC_NOERR .and. r8(1) == 3.25d0, nc_message())
    r4 = 4.25_c_float
    status = nc_put(f, 'radius', r4)
    call good('put_float_rank1_scalar_var')
    r4 = -1
    r42 = -1
    r43 = -1
    status = nc_get(f, 'radius', r4)
    call check('get_float_rank1_scalar_var', status == NC_NOERR .and. all(r4 == 4.25_c_float), nc_message())
    status = nc_get(f, 'radius', r42)
    call check('get_float_rank2_scalar_var', status == NC_NOERR .and. all(r42 == 4.25_c_float), nc_message())
    status = nc_get(f, 'radius', r43)
    call check('get_float_rank3_scalar_var', status == NC_NOERR .and. all(r43 == 4.25_c_float), nc_message())
    r83 = 0.1d0
    status = nc_put(f, 'radius', r83)
    call good('put_double_rank3')
    r83 = -1
    status = nc_get(f, 'radius', r83)
    call check('get_double_rank3', status == NC_NOERR .and. bits(r83(1,1,1)) == bits(0.1d0), nc_message())
    r82 = -2.5d0
    status = nc_put(f, 'radius', r82)
    call good('put_double_rank2')
    r82 = -1
    status = nc_get(f, 'radius', r82)
    call check('get_double_rank2', status == NC_NOERR .and. all(r82 == -2.5d0), nc_message())
    r8 = 6371229.5d0
    status = nc_put(f, 'radius', r8)
    call good('put_double_rank1_scalar_var')
    r8 = -1
    status = nc_get(f, 'radius', r8)
    call check('get_double_rank1_scalar_var', status == NC_NOERR .and. all(r8 == 6371229.5d0), nc_message())
    ints = [14,15,16]
    ints2 = reshape(ints, [1,3])
    status = nc_put(f, 'ids', ints2)
    call good('put_int_rank2')
    ints2 = -1
    status = nc_get(f, 'ids', ints2)
    call check('get_int_rank2', status == NC_NOERR .and. all(ints2 == reshape(ints,[1,3])), nc_message())
    ints = [21,22,23]
    ints3 = reshape(ints, [1,1,3])
    status = nc_put(f, 'ids', ints3)
    call good('put_int_rank3')
    ints3 = -1
    status = nc_get(f, 'ids', ints3)
    call check('get_int_rank3', status == NC_NOERR .and. all(ints3 == reshape(ints,[1,1,3])), nc_message())
    ! Numeric explicit put slices must not alter their neighbours.
    status = nc_put(f, 'theta', 2201.5_c_float, start=[1,2,2], count=[1,1,1])
    call good('put_element_rank0')
    status = nc_put(f, 'theta', reshape(formula(3), [6]), start=[1,1,3], count=[2,3,1])
    call good('put_slice_rank1')
  end subroutine

  subroutine empty_record()
    real(c_float) :: empty(2,3,0), one(2,3,1)
    character(len=80) :: times(0)
    write(path,'(a,i0,a)') 'empty_', formats(fmt), '.nc'
    status = nc_create(f, trim(path), formats(fmt), .true.)
    call good('create_empty')
    status = nc_def_dim(f, 'nVertLevels', 2)
    call good('empty_def_levels')
    status = nc_def_dim(f, 'nCells', 3)
    call good('empty_def_cells')
    status = nc_def_dim(f, 'Time', NC_UNLIMITED)
    call good('empty_def_Time')
    status = nc_def_dim(f, 'StrLen', 64)
    call good('empty_def_StrLen')
    call record_definitions()
    status = nc_get(f, 'theta', empty)
    call good('empty_record_get')
    status = nc_put(f, 'theta', empty)
    call good('empty_record_put')
    one = -1
    status = nc_get(f, 'theta', one)
    call check('empty_record_size_unchanged', status == LNC_ESIZE .and. all(one == -1), nc_message())
    status = nc_get(f, 'xtime', times)
    call good('empty_string_array')
    status = nc_close(f)
    call good('close_empty')
  end subroutine

  subroutine order_file()
    write(path,'(a,i0,a)') 'order_', formats(fmt), '.nc'
    status = nc_create(f, trim(path), formats(fmt), .true.)
    call good('create_order')
    call definitions()
    call write_records()
    status = nc_close(f)
    call good('close_order')
  end subroutine

end program
