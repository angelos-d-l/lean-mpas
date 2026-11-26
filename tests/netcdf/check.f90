! The pass/fail bookkeeping and bit-pattern helpers that the programs of tests/netcdf/run.sh share.
module nc_check
  use iso_c_binding, only: c_float, c_double, c_int32_t, c_int64_t
  implicit none
  private
  public :: check, finish, bits, bitsum, first_bytes
  integer :: checks = 0, failed = 0

  interface bits
    module procedure bits_float, bits_double
  end interface

contains

  subroutine check(name, ok, detail)
    character(*), intent(in) :: name
    logical, intent(in) :: ok
    character(*), intent(in), optional :: detail
    checks = checks + 1
    if (ok) then
      write(*,'(a)') 'ok   ' // name
    else
      failed = failed + 1
      if (present(detail)) then
        write(*,'(a)') 'FAIL ' // name // ': ' // detail
      else
        write(*,'(a)') 'FAIL ' // name // ': check failed'
      end if
    end if
  end subroutine

  subroutine finish(program)
    character(*), intent(in) :: program
    write(*,'(a,i0,a,i0,a)') program // ': ', checks, ' checks, ', failed, ' failed'
    if (failed > 0) error stop 1
  end subroutine

  function bits_float(x) result(pattern)
    real(c_float), intent(in) :: x
    integer(c_int32_t) :: pattern
    pattern = transfer(x, pattern)
  end function

  function bits_double(x) result(pattern)
    real(c_double), intent(in) :: x
    integer(c_int64_t) :: pattern
    pattern = transfer(x, pattern)
  end function

  function bitsum(a) result(total)
    real(c_float), intent(in) :: a(:)
    integer(c_int64_t) :: total
    integer :: i
    total = 0_c_int64_t
    do i = 1, size(a)
      total = total + int(bits(a(i)), c_int64_t)
    end do
  end function

  function first_bytes(path, n) result(bytes)
    character(*), intent(in) :: path
    integer, intent(in) :: n
    character(len=n) :: bytes
    integer :: unit
    open(newunit=unit, file=path, access='stream', form='unformatted', status='old', action='read')
    read(unit) bytes
    close(unit)
  end function

end module
