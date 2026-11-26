! lean's side of the namelist checks (tests/namelist/run.py):  lean_main read|suite|validate NAMELIST OUTPUT [FLAVOR]
! Reads the namelist, then resolves the physics suite (suite, validate) and validates it for FLAVOR (validate); writes
! the messages and the resulting options to OUTPUT. Exit 1 after a fatal message.
program lean_main
 use lean_namelist, only: run_config, read_namelist, resolve_suite, validate, dynamics_checks
 use lean_namelist_options, only: dump_options
 use lean_messages, only: message_list, has_fatal, write_messages
 implicit none
 type(run_config) :: cfg
 type(message_list) :: msgs
 character(len=1024) :: mode, path, output, flavor
 integer :: unit

 if (command_argument_count() < 3 .or. command_argument_count() > 4) error stop 2
 call get_command_argument(1, mode)
 call get_command_argument(2, path)
 call get_command_argument(3, output)
 flavor = 'lean'
 if (command_argument_count() == 4) call get_command_argument(4, flavor)
 if (trim(mode) /= 'read' .and. trim(mode) /= 'suite' .and. trim(mode) /= 'validate') error stop 2
 open(newunit=unit, file=trim(output), status='replace', action='write')
 call read_namelist(trim(path), cfg, msgs)
 if (trim(mode) /= 'read' .and. .not. has_fatal(msgs)) call resolve_suite(cfg, msgs)
 if (trim(mode) == 'validate' .and. .not. has_fatal(msgs)) call validate(cfg, trim(flavor), msgs)
 if (trim(mode) == 'validate' .and. .not. has_fatal(msgs)) call dynamics_checks(cfg, msgs)
 call write_messages(unit, msgs)
 if (has_fatal(msgs)) then
  close(unit)
  error stop 1
 end if
 call dump_options(unit, cfg)
 if (trim(mode) == 'suite') write(unit, '(a)') 'moist_physics = '//merge('T', 'F', cfg%moist_physics)
 close(unit)
end program
