! The command line of lean_drop_in (tests/run_shell/run.py): prints NAMELIST|STREAMS, or the refusal and exit 2.
program cli_main
 use iso_fortran_env, only: error_unit
 use lean_drop_in, only: run_command, read_command_line
 implicit none
 type(run_command) :: cmd
 character(len=:), allocatable :: text
 integer :: status

 call read_command_line(cmd, status, text)
 if (status /= 0) then
  write(error_unit, '(a)') text
  error stop 2
 end if
 write(*, '(a)') trim(cmd%namelist_file)//'|'//trim(cmd%streams_file)
end program
