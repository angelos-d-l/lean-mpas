! MPAS's summarize_timestep (extracted from mpas_atm_time_integration.F by tests/run_shell/run.py) on one data set of
! summary_data.inc:  summary_oracle NAME OUTPUT
#include "summary_data.inc"
module oracle_summary
 use mpas_kind_types
 use mpas_derived_types
 use mpas_pool_routines
 use mpas_dmpar
 use mpas_log
 implicit none
 type(block_type), pointer :: block
contains
#include "summarize_timestep.inc"
end module

program summary_oracle
 use summary_data
 use oracle_summary, only: block, summarize_timestep
 use mpas_derived_types, only: domain_type, block_type, mpas_pool_type
 use mpas_log, only: log_unit, mpas_log_write
 implicit none
 type(domain_type) :: domain
 type(block_type), target :: test_block
 type(mpas_pool_type), target :: configs, structs, state, diag, mesh
 character(len=1024) :: name, output

 call get_command_argument(1, name)
 call get_command_argument(2, output)
 call fill_summary_data(trim(name))
 block => test_block
 block%configs => configs
 block%structs => structs
 configs%print_vel => print_vel
 configs%print_detailed => print_detailed
 configs%print_sca => print_sca
 structs%state => state
 structs%diag => diag
 structs%mesh => mesh
 state%w => w
 state%u => u
 state%scalars => scalars
 allocate(state%ncells, state%nedges, state%nz, state%nscalars)
 state%ncells = ncells
 state%nedges = nedges
 state%nz = nz
 state%nscalars = size(scalars,1)
 diag%v => v
 mesh%lat_cell => lat_cell
 mesh%lon_cell => lon_cell
 mesh%lat_edge => lat_edge
 mesh%lon_edge => lon_edge
 mesh%cell_ids => cell_ids
 open(newunit=log_unit, file=trim(output), status='replace', action='write')
 call mpas_log_write('summary begin')
 call summarize_timestep(domain)
 call mpas_log_write('summary end')
 close(log_unit)
end program
