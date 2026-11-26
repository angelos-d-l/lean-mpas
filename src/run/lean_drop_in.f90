! atmosphere_model's command line, startup and finish, in MPAS's order: driver/mpas_subdriver.F:150-384, then the
! reads and checks of core_atmosphere/mpas_atm_core.F:118-251.
module lean_drop_in
 use iso_fortran_env, only: error_unit
 use mpas_kind_types, only: StrKIND
 !$ use omp_lib, only: omp_get_max_threads
 use lean_build_info, only: build_name, build_flavor, build_switches, build_git, build_profile, &
                           build_openmp, build_openacc
 use lean_guard, only: mpi_launch_check, lock_run_directory, LOCK_HELD, LOCK_UNAVAILABLE
 use lean_log, only: log_open, log_write, log_messages, log_timer_start, log_finish, LOG_FILE
 use physics_time, only: microphysics_messages
 use lean_messages, only: message_list, LOG_WARN, int_text
 use lean_namelist, only: run_config, read_namelist, resolve_suite, validate, dynamics_checks
 use lean_clock, only: run_clock, clock_from_config, step_count
 use lean_streams, only: stream_set, streams_inquire, packages_when, set_packages, streams_mesh, streams_parse
 use lean_run_inputs, only: run_inputs, read_run_mesh, open_run_inputs, bdy_checks, unserved_streams
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 implicit none
 private
 public :: run_command, read_command_line, drop_in_start, drop_in_finish

 type run_command
  ! MPAS tools/registry/gen_inc.c:107-108.
  character(len=StrKIND) :: namelist_file = 'namelist.atmosphere'
  character(len=StrKIND) :: streams_file = 'streams.atmosphere'
 end type

contains

 subroutine read_command_line(cmd, status, text)
 type(run_command), intent(out) :: cmd
 integer, intent(out) :: status
 character(len=:), allocatable, intent(out) :: text
 character(len=StrKIND) :: argument
 integer :: iArg, nArgs

 status = 0
 text = ''
 ! MPAS mpas_subdriver.F:150-180 never reads the final argument as an option.
 nArgs = command_argument_count()
 iArg = 1
 do while (iArg < nArgs)
  call get_command_argument(iArg, argument)
  if (len_trim(argument) == 0) exit
  if (trim(argument) == '-n') then
   iArg = iArg + 1
   call get_command_argument(iArg, cmd%namelist_file)
   if (len_trim(cmd%namelist_file) == 0 .or. trim(cmd%namelist_file) == '-s') then
    status = 2
    text = 'ERROR: The -n argument requires a namelist file argument.'
    return
   end if
  else if (trim(argument) == '-s') then
   iArg = iArg + 1
   call get_command_argument(iArg, cmd%streams_file)
   if (len_trim(cmd%streams_file) == 0 .or. trim(cmd%streams_file) == '-n') then
    status = 2
    text = 'ERROR: The -s argument requires a streams file argument.'
    return
   end if
  end if
  iArg = iArg + 1
 end do
 end subroutine

 ! The run's startup as atmosphere_model's: the refusals, the log, the namelist, the streams and the clock, and the
 ! inputs of the run directory. Each call's messages go to the log at once, where a CRITICAL ERROR ends the run.
 ! It returns what the model starts from (lean_model's run_model_from): the stream set and the clock, the mesh, the
 ! initial fields and the run's inputs.
 subroutine drop_in_start(cmd, cfg, set, clock, m, dyn, run)
 type(run_command), intent(in) :: cmd
 type(run_config), intent(out) :: cfg
 type(stream_set), intent(out) :: set
 type(run_clock), intent(out) :: clock
 type(dyn_mesh_type), intent(out) :: m
 type(cold_dyn_type), intent(out) :: dyn
 type(run_inputs), intent(out) :: run
 type(message_list) :: msgs
 character(len=:), allocatable :: text, lock_text
 logical :: refused
 integer :: state, status

 ! Every refusal comes before the first write to the log or an output, so a refused run leaves the directory as it was.
 call mpi_launch_check(refused, text)
 if (refused) then
  write(error_unit, '(a)') text
  error stop 2
 end if
 call lock_run_directory(state, lock_text)
 if (state == LOCK_HELD) then
  write(error_unit, '(a)') 'lean_mpas: another run holds this run directory (its lock is taken). '// &
   'One run per directory. Nothing was written.'
  error stop 2
 end if
 call log_open(status, text)
 if (status /= 0) then
  write(error_unit, '(a)') 'lean_mpas: cannot open '//LOG_FILE//': '//text
  error stop 1
 end if
 call log_identity()
 if (state == LOCK_UNAVAILABLE) then
  call log_write('cannot lock the run directory: '//lock_text// &
   '; lean_mpas cannot check that no other run uses it', LOG_WARN)
 end if
 call read_namelist(trim(cmd%namelist_file), cfg, msgs)
 call log_messages(msgs)
 ! MPAS mpas_framework.F:143-148; pio options are ignored for this one-process netCDF-C core.
 call log_write('')
 call log_write('----- I/O task configuration: -----')
 call log_write('')
 call log_write('    I/O task count  = 1')
 call log_write('    I/O task stride = 1')
 call log_write('')
 ! The stream inquiry and the two packages set before suite resolution (les, sfclayer).
 ! MPAS mpas_subdriver.F:262-281; mpas_atm_core_interface.F:137.
 call streams_inquire(trim(cmd%streams_file), set, msgs)
 call log_messages(msgs)
 call packages_when(cfg, set, msgs)
 call log_messages(msgs)
 call resolve_suite(cfg, msgs)
 call log_messages(msgs)
 ! The scheme packages and MPAS's package-variables block, after suite resolution.
 call set_packages(cfg, set, msgs)
 call log_messages(msgs)
 call validate(cfg, build_flavor, msgs)
 call log_messages(msgs)
 ! MPAS mpas_subdriver.F:300-301.
 call log_timer_start('total time')
 call log_timer_start('initialize')
 ! In MPAS order (mpas_subdriver.F:288-384; mpas_atm_core.F:134-247): the clock, the streams file and the
 ! mesh stream; the mesh read; the parse; the inputs of the run (every boundary read of step_count steps resolved
 ! now) and MPAS's limited-area check.
 call clock_from_config(cfg, clock, msgs)
 call log_messages(msgs)
 call streams_mesh(set, cfg, clock, msgs)
 call log_messages(msgs)
 call read_run_mesh(cfg, clock, step_count(clock), trim(cmd%namelist_file), set, m, run, msgs)
 call log_messages(msgs)
 call streams_parse(set, cfg, clock, msgs)
 call log_messages(msgs)
 call open_run_inputs(cfg, set, clock, step_count(clock), m, dyn, run, msgs)
 call log_messages(msgs)
 call bdy_checks(cfg, set, m, msgs)
 call log_messages(msgs)
 ! physics/mpas_atmphys_manager.F:696-698, before dynamics checks and the initial boundary reads.
 call microphysics_messages(msgs)
 call log_messages(msgs)
 ! what MPAS would read or write in this run and lean does not: said in the log, never lost silently
 call unserved_streams(cfg, set, clock, step_count(clock), msgs)
 call log_messages(msgs)
 call dynamics_checks(cfg, msgs)
 call log_messages(msgs)
 end subroutine

 subroutine log_identity()
 integer :: i, first, last, n

 ! MPAS mpas_atm_core_interface.F:342-346; mpas_framework.F:220-282.
 call log_write('')
 call log_write('MPAS-Atmosphere Version 8.4.2 (lean_mpas drop-in core)')
 call log_write('')
 call log_write('')
 call log_write('Output from ''git describe --dirty'': '//build_git)
 call log_write('')
 call log_write('Compile-time options:')
 call log_write('  Build target: '//build_name)
 call log_write('  OpenMP support: '//trim(merge('yes', 'no ', build_openmp)))
 call log_write('  OpenACC support: '//trim(merge('yes', 'no ', build_openacc)))
 call log_write('  Default real precision: single')
 call log_write('  Compiler flags: '//build_profile)
 call log_write('  I/O layer: netCDF-C')
 call log_write('  MUSICA support: no')
 call log_write('  SCOTCH support: no')
 call log_write('  lean_mpas flavor: '//build_flavor)
 if (len_trim(build_switches) == 0) then
  call log_write('  lean_mpas switches: none')
 else
  n = 1
  do i = 1, len_trim(build_switches)
   if (build_switches(i:i) == ' ') n = n + 1
  end do
  call log_write('  lean_mpas switches ('//int_text(n)//'):')
  first = 1
  do i = 1, n
   last = index(build_switches(first:), ' ')
   if (last == 0) last = len_trim(build_switches) - first + 2
   call log_write('    '//build_switches(first:first + last - 2))
   first = first + last
  end do
 end if
 call log_write('')
 call log_write('Run-time settings:')
 call log_write('  MPI task count: 1')
 !$ call log_write('  OpenMP max threads: '//int_text(omp_get_max_threads()))
 call log_write('')
 end subroutine

 subroutine drop_in_finish()
 ! MPAS mpas_atm_core.F:1116-1119; mpas_subdriver.F:447-469.
 call log_write('')
 call log_write(repeat('*', 56))
 call log_write('   Finished running the atmosphere core')
 call log_write(repeat('*', 56))
 call log_finish()
 end subroutine

end module
