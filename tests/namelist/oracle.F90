! MPAS's side of the namelist checks (tests/namelist/run.py):  oracle read|suite NAMELIST OUTPUT
! MPAS 8.4.2's own namelist reader (the include files that MPAS's Registry parser writes) and physics_namelist_check,
! with the framework stand-ins of oracle_stubs.F90; writes the resulting options to OUTPUT.
module oracle_reader
 use mpas_kind_types
 use mpas_derived_types
 use mpas_dmpar
 use mpas_pool_routines
 implicit none
contains
#include "namelist_defines.inc"
#include "namelist_call.inc"
end module

module oracle_physics
 use mpas_kind_types, only: StrKIND
 use mpas_derived_types, only: mpas_pool_type
 use mpas_pool_routines, only: mpas_pool_get_config
 use mpas_log, only: mpas_log_write
 implicit none
 character(len=StrKIND) :: mpas_err_message
 logical :: moist_physics
contains
#include "physics_pieces.inc"
end module

program oracle
 use oracle_reader, only: atm_setup_namelists
 use oracle_physics, only: physics_namelist_check, moist_physics
 use mpas_derived_types, only: mpas_pool_type, dm_info
 use mpas_log, only: log_unit
 use mpas_pool_routines, only: dump_pool
 implicit none
 type(mpas_pool_type) :: pool
 type(dm_info) :: dminfo
 character(len=1024) :: mode, path, output
 integer :: ierr

 if (command_argument_count() /= 3) error stop 2
 call get_command_argument(1, mode)
 call get_command_argument(2, path)
 call get_command_argument(3, output)
 if (trim(mode) /= 'read' .and. trim(mode) /= 'suite') error stop 2
 open(newunit=log_unit, file=trim(output), status='replace', action='write')
 ierr = atm_setup_namelists(pool, trim(path), dminfo)
 if (ierr /= 0) error stop 1
 if (trim(mode) == 'suite') call physics_namelist_check(pool)
 call dump_pool(log_unit, pool)
 ! The result of MPAS's check of whether any moist physics scheme runs
 if (trim(mode) == 'suite') write(log_unit, '(a)') 'moist_physics = '//merge('T', 'F', moist_physics)
 close(log_unit)
end program
