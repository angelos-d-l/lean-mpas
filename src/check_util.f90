! Bit-for-bit comparison of two arrays with a report of what differs (used to compare a step on the card with the same
! step on the host). A check is opened with
! check_open(name, step), the arrays are compared with compare_report (only differing arrays are printed), check_close prints the totals
! and adds them to the totals of the run, check_final prints the run totals per name.
module check_util
 implicit none
 integer, parameter :: max_names = 16
 character(24), save :: names(max_names) = ''
 integer(8), save :: run_values(max_names) = 0, run_differ(max_names) = 0
 integer, save :: run_checks(max_names) = 0, current = 0, cur_step = 0
 integer(8), save :: cur_values = 0, cur_differ = 0
 integer, save :: cur_fields = 0
 interface compare_report
  module procedure cmp1_real4
  module procedure cmp2_real4
  module procedure cmp3_real4
  module procedure cmp4_real4
  module procedure cmp5_real4
  module procedure cmp1_real8
  module procedure cmp2_real8
  module procedure cmp3_real8
  module procedure cmp4_real8
  module procedure cmp5_real8
  module procedure cmp1_int4
  module procedure cmp2_int4
  module procedure cmp3_int4
  module procedure cmp4_int4
  module procedure cmp5_int4
  module procedure cmp1_log
  module procedure cmp2_log
  module procedure cmp3_log
  module procedure cmp4_log
  module procedure cmp5_log
 end interface
contains
 subroutine check_open(name, step)
 character(*), intent(in) :: name
 integer, intent(in) :: step
 integer :: i
 current = 0
 do i = 1, max_names
  if (trim(names(i)) == name) current = i
 end do
 if (current == 0) then
  do i = 1, max_names
   if (len_trim(names(i)) == 0) then
    names(i) = name; current = i; exit
   end if
  end do
 end if
 cur_step = step; cur_values = 0; cur_differ = 0; cur_fields = 0
 end subroutine

 subroutine check_close()
 if (current == 0) return
 print '(a,a,a,i5,a,i14,a,i12,a,i4)', 'CHECK ', trim(names(current)), ' step', cur_step, ': values compared ', cur_values, '  differ ', &
       cur_differ, '  fields with differences ', cur_fields
 run_values(current) = run_values(current) + cur_values
 run_differ(current) = run_differ(current) + cur_differ
 run_checks(current) = run_checks(current) + 1
 flush(6)
 end subroutine

 subroutine check_final()
 integer :: i
 do i = 1, max_names
  if (len_trim(names(i)) > 0) print '(a,a,a,i5,a,i15,a,i12)', 'CHECK TOTAL ', trim(names(i)), ': checks ', run_checks(i), '  values ', &
                                                          run_values(i), '  differ ', run_differ(i)
 end do
 end subroutine

 subroutine report(label, n, ndiff, first, maxabs)
 character(*), intent(in) :: label
 integer, intent(in) :: n, ndiff, first
 real(8), intent(in) :: maxabs
 cur_values = cur_values + n
 cur_differ = cur_differ + ndiff
 if (ndiff > 0) then
  cur_fields = cur_fields + 1
  print '(a,a,a,i10,a,i10,a,es10.3,a,i10)', '   ', label, ': differ ', ndiff, ' of ', n, '  max abs ', maxabs, '  first index ', first
 end if
 end subroutine

 subroutine w_real4(label, a, b, n)
 character(*), intent(in) :: label
 integer, intent(in) :: n
 real(4), intent(in) :: a(n), b(n)
 integer :: i, ndiff, first
 real(8) :: maxabs
 ndiff = 0; first = 0; maxabs = 0.d0
 do i = 1, n
  if (transfer(a(i), 0) /= transfer(b(i), 0)) then
   ndiff = ndiff + 1
   if (first == 0) first = i
   if (a(i) == a(i) .and. b(i) == b(i)) maxabs = max(maxabs, abs(real(a(i),8)-real(b(i),8)))
  end if
 end do
 call report(label, n, ndiff, first, maxabs)
 end subroutine

 subroutine w_real8(label, a, b, n)
 character(*), intent(in) :: label
 integer, intent(in) :: n
 real(8), intent(in) :: a(n), b(n)
 integer :: i, ndiff, first
 real(8) :: maxabs
 ndiff = 0; first = 0; maxabs = 0.d0
 do i = 1, n
  if (transfer(a(i), 0_8) /= transfer(b(i), 0_8)) then
   ndiff = ndiff + 1
   if (first == 0) first = i
   if (a(i) == a(i) .and. b(i) == b(i)) maxabs = max(maxabs, abs(a(i)-b(i)))
  end if
 end do
 call report(label, n, ndiff, first, maxabs)
 end subroutine

 subroutine w_int4(label, a, b, n)
 character(*), intent(in) :: label
 integer, intent(in) :: n
 integer(4), intent(in) :: a(n), b(n)
 integer :: i, ndiff, first
 real(8) :: maxabs
 ndiff = 0; first = 0; maxabs = 0.d0
 do i = 1, n
  if (a(i) /= b(i)) then
   ndiff = ndiff + 1
   if (first == 0) first = i
   maxabs = max(maxabs, abs(real(a(i),8)-real(b(i),8)))
  end if
 end do
 call report(label, n, ndiff, first, maxabs)
 end subroutine

 subroutine w_log(label, a, b, n)
 character(*), intent(in) :: label
 integer, intent(in) :: n
 logical, intent(in) :: a(n), b(n)
 integer :: i, ndiff, first
 ndiff = 0; first = 0
 do i = 1, n
  if (a(i) .neqv. b(i)) then
   ndiff = ndiff + 1
   if (first == 0) first = i
  end if
 end do
 call report(label, n, ndiff, first, 0.d0)
 end subroutine
 subroutine cmp1_real4(label, a, b)
 character(*), intent(in) :: label
 real(4), intent(in) :: a(:), b(:)
 call w_real4(label, a, b, size(a))
 end subroutine

 subroutine cmp2_real4(label, a, b)
 character(*), intent(in) :: label
 real(4), intent(in) :: a(:,:), b(:,:)
 call w_real4(label, a, b, size(a))
 end subroutine

 subroutine cmp3_real4(label, a, b)
 character(*), intent(in) :: label
 real(4), intent(in) :: a(:,:,:), b(:,:,:)
 call w_real4(label, a, b, size(a))
 end subroutine

 subroutine cmp4_real4(label, a, b)
 character(*), intent(in) :: label
 real(4), intent(in) :: a(:,:,:,:), b(:,:,:,:)
 call w_real4(label, a, b, size(a))
 end subroutine

 subroutine cmp5_real4(label, a, b)
 character(*), intent(in) :: label
 real(4), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
 call w_real4(label, a, b, size(a))
 end subroutine

 subroutine cmp1_real8(label, a, b)
 character(*), intent(in) :: label
 real(8), intent(in) :: a(:), b(:)
 call w_real8(label, a, b, size(a))
 end subroutine

 subroutine cmp2_real8(label, a, b)
 character(*), intent(in) :: label
 real(8), intent(in) :: a(:,:), b(:,:)
 call w_real8(label, a, b, size(a))
 end subroutine

 subroutine cmp3_real8(label, a, b)
 character(*), intent(in) :: label
 real(8), intent(in) :: a(:,:,:), b(:,:,:)
 call w_real8(label, a, b, size(a))
 end subroutine

 subroutine cmp4_real8(label, a, b)
 character(*), intent(in) :: label
 real(8), intent(in) :: a(:,:,:,:), b(:,:,:,:)
 call w_real8(label, a, b, size(a))
 end subroutine

 subroutine cmp5_real8(label, a, b)
 character(*), intent(in) :: label
 real(8), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
 call w_real8(label, a, b, size(a))
 end subroutine

 subroutine cmp1_int4(label, a, b)
 character(*), intent(in) :: label
 integer(4), intent(in) :: a(:), b(:)
 call w_int4(label, a, b, size(a))
 end subroutine

 subroutine cmp2_int4(label, a, b)
 character(*), intent(in) :: label
 integer(4), intent(in) :: a(:,:), b(:,:)
 call w_int4(label, a, b, size(a))
 end subroutine

 subroutine cmp3_int4(label, a, b)
 character(*), intent(in) :: label
 integer(4), intent(in) :: a(:,:,:), b(:,:,:)
 call w_int4(label, a, b, size(a))
 end subroutine

 subroutine cmp4_int4(label, a, b)
 character(*), intent(in) :: label
 integer(4), intent(in) :: a(:,:,:,:), b(:,:,:,:)
 call w_int4(label, a, b, size(a))
 end subroutine

 subroutine cmp5_int4(label, a, b)
 character(*), intent(in) :: label
 integer(4), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
 call w_int4(label, a, b, size(a))
 end subroutine

 subroutine cmp1_log(label, a, b)
 character(*), intent(in) :: label
 logical, intent(in) :: a(:), b(:)
 call w_log(label, a, b, size(a))
 end subroutine

 subroutine cmp2_log(label, a, b)
 character(*), intent(in) :: label
 logical, intent(in) :: a(:,:), b(:,:)
 call w_log(label, a, b, size(a))
 end subroutine

 subroutine cmp3_log(label, a, b)
 character(*), intent(in) :: label
 logical, intent(in) :: a(:,:,:), b(:,:,:)
 call w_log(label, a, b, size(a))
 end subroutine

 subroutine cmp4_log(label, a, b)
 character(*), intent(in) :: label
 logical, intent(in) :: a(:,:,:,:), b(:,:,:,:)
 call w_log(label, a, b, size(a))
 end subroutine

 subroutine cmp5_log(label, a, b)
 character(*), intent(in) :: label
 logical, intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
 call w_log(label, a, b, size(a))
 end subroutine
end module
