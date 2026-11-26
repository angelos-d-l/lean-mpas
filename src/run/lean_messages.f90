! MPAS log line and value formats: framework/mpas_log.F:576,979-1014.
module lean_messages
 use mpas_kind_types, only: RKIND, StrKIND
 implicit none
 private
 public :: LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT, message_line, message_list
 public :: add_message, has_fatal, write_messages, int_text, real_text, logical_text, trim_message

 integer, parameter :: LOG_OUT = 1, LOG_WARN = 2, LOG_ERR = 3, LOG_CRIT = 4

 type message_line
  integer :: level
  character(len=8192) :: text
 end type

 type message_list
  type(message_line), allocatable :: line(:)
  integer :: n = 0
 end type

contains

 subroutine add_message(list, level, text)
 type(message_list), intent(inout) :: list
 integer, intent(in) :: level
 character(len=*), intent(in) :: text
 type(message_line), allocatable :: grown(:)

 if (.not. allocated(list%line)) allocate(list%line(4))
 if (list%n == size(list%line)) then
  allocate(grown(max(4, 2*list%n)))
  grown(1:list%n) = list%line(1:list%n)
  call move_alloc(grown, list%line)
 end if
 list%n = list%n + 1
 list%line(list%n)%level = level
 list%line(list%n)%text = trim_message(text)
 end subroutine

 function trim_message(text) result(clean)
 character(len=*), intent(in) :: text
 character(len=:), allocatable :: clean
 integer :: last

 ! nvfortran pads an iomsg with NUL bytes, not blanks: the text ends at the first NUL.
 last = index(text, achar(0)) - 1
 if (last < 0) last = len(text)
 clean = trim(text(1:last))
 end function

 logical function has_fatal(list)
 type(message_list), intent(in) :: list
 integer :: i

 has_fatal = .false.
 do i = 1, list%n
  if (list%line(i)%level == LOG_CRIT) then
   has_fatal = .true.
   return
  end if
 end do
 end function

 subroutine write_messages(unit, list)
 integer, intent(in) :: unit
 type(message_list), intent(in) :: list
 integer :: i
 character(len=16) :: prefix

 do i = 1, list%n
  prefix = ''
  select case (list%line(i)%level)
  case (LOG_WARN)
   prefix = 'WARNING:'
  case (LOG_ERR)
   prefix = 'ERROR:'
  case (LOG_CRIT)
   prefix = 'CRITICAL ERROR:'
  end select
  write(unit, '(a,a,a)') trim(prefix), ' ', trim(list%line(i)%text)
 end do
 end subroutine

 function int_text(i) result(text)
 integer, intent(in) :: i
 character(len=:), allocatable :: text
 character(len=StrKIND) :: buffer

 write(buffer, '(i17)') i
 text = trim(adjustl(buffer))
 end function

 function real_text(r) result(text)
 real(kind=RKIND), intent(in) :: r
 character(len=:), allocatable :: text
 character(len=StrKIND) :: buffer
 character(len=16) :: real_format
 integer :: p

 p = precision(r)
 write(real_format, '(a,i2.2,a,i2.2,a)') '(G', p+7, '.', p, ')'
 write(buffer, trim(real_format)) r
 text = trim(adjustl(buffer))
 end function

 function logical_text(l) result(text)
 logical, intent(in) :: l
 character(len=:), allocatable :: text

 if (l) then
  text = 'T'
 else
  text = 'F'
 end if
 end function

end module
