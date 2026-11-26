! lean_summary's per-step extrema on one data set of summary_data.inc:  summary_main NAME
! tests/run_shell/run.py compares the log with summary_oracle's.
#include "summary_data.inc"
program summary_main
 use summary_data
 use lean_log, only: log_open, log_write, log_finish
 use lean_summary, only: summarize_timestep
 use mpas_kind_types, only: StrKIND
 implicit none
 character(len=StrKIND) :: name
 character(len=:), allocatable :: text
 integer :: status

 call get_command_argument(1, name)
 call fill_summary_data(trim(name))
 call log_open(status, text)
 if (status /= 0) error stop 2
 call log_write('summary begin')
 call summarize_timestep(print_vel, print_detailed, print_sca, w, u, v, scalars, &
                         lat_cell, lon_cell, lat_edge, lon_edge, ncells, nedges, nz)
 call log_write('summary end')
 call log_finish()
end program
