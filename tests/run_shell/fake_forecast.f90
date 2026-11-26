! lean_drop_in's real startup in a run directory, then three steps of the summary's test data and the finish, so that
! tests/run_shell/run.py can check the whole log of a run without running the model.
#include "summary_data.inc"
program fake_forecast
 use iso_fortran_env, only: error_unit
 use mpas_kind_types, only: R8KIND
 use summary_data
 use lean_namelist, only: run_config
 use lean_streams, only: stream_set
 use lean_clock, only: run_clock
 use lean_run_inputs, only: run_inputs
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use lean_drop_in, only: run_command, read_command_line, drop_in_start, drop_in_finish
 use lean_log, only: log_timer_start, log_timer_stop, log_step_begin, log_step_time, log_write
 use lean_summary, only: summarize_timestep
 implicit none
 type(run_command) :: cmd
 type(run_config) :: cfg
 type(stream_set) :: set
 type(run_clock) :: clock
 type(dyn_mesh_type) :: m
 type(cold_dyn_type) :: dyn
 type(run_inputs) :: run
 character(len=:), allocatable :: text
 character(len=19), parameter :: stamps(3) = ['2026-10-12_06:00:00','2026-10-12_06:00:30','2026-10-12_06:01:00']
 real(kind=R8KIND) :: seconds
 integer :: status, step

 call read_command_line(cmd, status, text)
 if (status /= 0) then
  write(error_unit, '(a)') text
  error stop 2
 end if
 ! the real startup in the run directory, then three steps of the summary's test data
 call drop_in_start(cmd, cfg, set, clock, m, dyn, run)
 call log_timer_stop('initialize')
 call fill_summary_data('vel')
 do step = 1, 3
  call log_timer_start('time integration')
  call log_step_begin(stamps(step))
  call log_write(' split dynamics-transport integration 3')
  call summarize_timestep(cfg%config_print_global_minmax_vel, cfg%config_print_detailed_minmax_vel, &
                          cfg%config_print_global_minmax_sca, w, u, v, scalars, &
                          lat_cell, lon_cell, lat_edge, lon_edge, ncells, nedges, nz)
  call log_timer_stop('time integration', seconds)
  call log_step_time(seconds)
 end do
 call drop_in_finish()
end program
