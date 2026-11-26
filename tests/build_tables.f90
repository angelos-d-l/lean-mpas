! Not a test: runs MPAS's build_tables_thompson, which writes the four MP_THOMPSON_*.DBL tables (tools/build_tables.sh,
! ./lean tables).
program tables
 use atmphys_build_tables_thompson
 call build_tables_thompson
end program
