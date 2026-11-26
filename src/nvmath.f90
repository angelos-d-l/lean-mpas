! nvpow(a,b) = a**b in a separately compiled routine. Not used by the model at present.
module nvmath
 implicit none
 interface nvpow
  module procedure pw_r4r4, pw_r4i4, pw_r8r8, pw_r8i4, pw_r4r8, pw_r8r4, pw_i4i4, pw_i4r4, pw_i4r8
 end interface
contains
 real(4) function pw_r4r4(x, y)
 !$acc routine seq
 real(4), intent(in) :: x, y
 pw_r4r4 = x**y
 end function
 real(4) function pw_r4i4(x, n)
 !$acc routine seq
 real(4), intent(in) :: x
 integer, intent(in) :: n
 pw_r4i4 = x**n
 end function
 real(8) function pw_r8r8(x, y)
 !$acc routine seq
 real(8), intent(in) :: x, y
 pw_r8r8 = x**y
 end function
 real(8) function pw_r8i4(x, n)
 !$acc routine seq
 real(8), intent(in) :: x
 integer, intent(in) :: n
 pw_r8i4 = x**n
 end function
 real(8) function pw_r4r8(x, y)
 !$acc routine seq
 real(4), intent(in) :: x
 real(8), intent(in) :: y
 pw_r4r8 = x**y
 end function
 real(8) function pw_r8r4(x, y)
 !$acc routine seq
 real(8), intent(in) :: x
 real(4), intent(in) :: y
 pw_r8r4 = x**y
 end function
 integer function pw_i4i4(x, n)
 !$acc routine seq
 integer, intent(in) :: x, n
 pw_i4i4 = x**n
 end function

 real(4) function pw_i4r4(x, y)
 !$acc routine seq
 integer, intent(in) :: x
 real(4), intent(in) :: y
 pw_i4r4 = x**y
 end function
 real(8) function pw_i4r8(x, y)
 !$acc routine seq
 integer, intent(in) :: x
 real(8), intent(in) :: y
 pw_i4r8 = x**y
 end function
end module
