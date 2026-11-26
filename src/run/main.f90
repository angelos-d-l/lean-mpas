! program lean_mpas: atmosphere_model's command line (-n NAMELIST, -s STREAMS), run in an MPAS run directory.
program lean_mpas
 use iso_fortran_env, only: error_unit
 use lean_drop_in, only: run_command, read_command_line, drop_in_start, drop_in_finish
 use lean_namelist, only: run_config
 use lean_streams, only: stream_set
 use lean_clock, only: run_clock
 use lean_run_inputs, only: run_inputs
 use dyn_mesh_data, only: dyn_mesh_type
 use cold_dyn_data, only: cold_dyn_type
 use lean_model, only: run_model_from
 implicit none
 type(run_command) :: cmd
 type(run_config) :: cfg
 type(stream_set) :: set
 type(run_clock) :: clock
 type(dyn_mesh_type) :: mesh
 type(cold_dyn_type) :: dyn
 type(run_inputs) :: run
 character(len=:), allocatable :: text
 integer :: status

 call read_command_line(cmd, status, text)
 if (status /= 0) then
  write(error_unit, '(a)') text
  error stop 2
 end if
 ! the run directory's namelist and streams, the forecast of the namelist's length, the log
 call drop_in_start(cmd, cfg, set, clock, mesh, dyn, run)
 call run_model_from(mesh, dyn, run, clock, cfg, set)
 call drop_in_finish()
end program
