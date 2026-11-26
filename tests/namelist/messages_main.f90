! lean's message list and its MPAS-style log lines (lean_messages), printed for tests/namelist/run.py to compare with
! the expected text.
program messages_main
 use mpas_kind_types, only: RKIND
 use lean_messages, only: message_list, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT, &
                          add_message, has_fatal, write_messages, int_text, real_text, logical_text
 implicit none
 type(message_list) :: msgs

 call add_message(msgs, LOG_OUT, 'Reading namelist from file x')
 call add_message(msgs, LOG_OUT, '')
 call add_message(msgs, LOG_WARN, 'w')
 call add_message(msgs, LOG_ERR, 'e')
 ! nvfortran pads iomsg with NUL bytes, not blanks: the text ends at the first NUL.
 call add_message(msgs, LOG_ERR, 'n' // achar(0) // achar(0) // 'junk')
 write(*, '(l1)') has_fatal(msgs)
 call add_message(msgs, LOG_CRIT, 'c')
 write(*, '(l1)') has_fatal(msgs)
 call write_messages(6, msgs)
 write(*, '(a)') int_text(-3)
 write(*, '(a)') int_text(2147483647)
 write(*, '(a)') real_text(0.5_RKIND)
 write(*, '(a)') real_text(720.0_RKIND)
 write(*, '(a)') real_text(30000.0_RKIND)
 write(*, '(a)') real_text(0.13e-3_RKIND)
 write(*, '(a)') real_text(1.0e9_RKIND)
 write(*, '(a)') real_text(1000.0_RKIND)
 write(*, '(a)') real_text(2.5_RKIND)
 write(*, '(a)') logical_text(.true.)
 write(*, '(a)') logical_text(.false.)
end program
