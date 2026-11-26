! lean_log in one mode per call (tests/run_shell/run.py):  log_main MODE  (lines, error, fatal, list_fatal, timers,
! running, rows, open, nul); run.py checks the log files it writes.
program log_main
 use iso_fortran_env, only: output_unit
 use mpas_kind_types, only: RKIND, R8KIND, StrKIND
 use lean_messages, only: message_list, add_message, write_messages, trim_message, LOG_OUT, LOG_WARN, LOG_CRIT, LOG_ERR
 use lean_log
 implicit none
 character(len=StrKIND) :: mode
 character(len=:), allocatable :: text
 type(message_list) :: messages
 integer :: status, i

 call get_command_argument(1, mode)
 if (trim(mode) == 'nul') then
  write(*, '(a)') trim_message('I/O diagnostic   '//repeat(achar(0), 16))
  write(*, '(a)') trim_message(achar(0)//'hidden')
  write(*, '(a)') trim_message('plain text   ')
  write(*, '(a)') trim_message('before'//achar(0)//'hidden')
  call add_message(messages, LOG_OUT, 'list text'//repeat(achar(0), 16))
  call write_messages(output_unit, messages)
  stop
 end if
 if (trim(mode) == 'rows') then
  write(*, '(a)') ' '//timer_header()
  write(*, '(a)') ' '//timer_row(1, 'total time', 132.85538_RKIND, 1, &
   132.85538_RKIND, 132.85538_RKIND, 132.85538_RKIND, 100.0_RKIND, 0.0_RKIND, 1.0_RKIND)
  write(*, '(a)') ' '//timer_row(2, 'time integration', 119.80722_RKIND, 360, &
   0.24797_RKIND, 1.61162_RKIND, 0.33280_RKIND, 90.18_RKIND, 90.18_RKIND, 1.0_RKIND)
  write(*, '(a)') ' '//timer_row(4, 'mp_thompson', 10.96807_RKIND, 360, &
   0.01758_RKIND, 0.03356_RKIND, 0.03047_RKIND, 8.26_RKIND, 79.57_RKIND, 1.0_RKIND)
  stop
 end if

 call log_open(status, text)
 if (trim(mode) == 'open') then
  write(*, '(l1,1x,l1)') status /= 0, len_trim(text) > 0
  stop
 end if
 if (status /= 0) error stop 99
 select case (trim(mode))
 case ('lines')
  call log_write('a')
  call log_write('w', LOG_WARN)
  call add_message(messages, LOG_OUT, 'b')
  call add_message(messages, LOG_WARN, 'x')
  call log_messages(messages)
  if (messages%n /= 0) error stop 99
  call log_step_begin('2026-11-08_00:00:00')
  call log_write(' split dynamics-transport integration 3')
  call log_step_time(0.553968_R8KIND)
 case ('error')
  call log_write('a')
  call log_write('e', LOG_ERR)
 case ('fatal')
  call log_write('a')
  call log_fatal('c')
  error stop 99
 case ('list_fatal')
  call add_message(messages, LOG_OUT, 'a')
  call add_message(messages, LOG_CRIT, 'c')
  call add_message(messages, LOG_OUT, 'never')
  call log_messages(messages)
  error stop 99
 case ('timers')
  call log_timer_start('total time')
  call log_timer_start('initialize')
  call log_timer_stop('initialize')
  do i = 1, 3
   call log_timer_start('time integration')
   call log_timer_start('physics')
   call log_timer_stop('physics')
   call log_timer_stop('time integration')
  end do
 case ('running')
  call log_timer_start('total time')
  call log_timer_start('initialize')
 case default
  error stop 99
 end select
 call log_finish()
end program
