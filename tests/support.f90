! Not a test: minimal stand-ins for the MPAS framework modules (mpas_derived_types, mpas_log, mpas_io_units, the
! physics messages of mpas_atmphys_utilities) that MPAS's Thompson table builder calls. tools/build_reference.sh and
! ./lean tables compile it with the unchanged MPAS sources of reference/mpas_src to write the four Thompson tables
! (data/tables). Those builds compile only the part before module mp_radar (as services.f90): mp_radar needs
! module_mp_radar, which is compiled after it, so they write the same alias as radar_alias.f90 once module_mp_radar is
! built.
module mpas_derived_types
 integer, parameter :: MPAS_LOG_ERR=1
end module
module mpas_log
contains
 subroutine mpas_log_write(message, messageType, realArgs, intArgs, logicArgs)
 character(*), intent(in) :: message
 integer, optional :: messageType, intArgs(:)
 real, optional :: realArgs(:)
 logical, optional :: logicArgs(:)
 print *, trim(message)
 end subroutine
end module
module mpas_atmphys_utilities
contains
 subroutine physics_error_fatal(message)
 character(*) :: message
 print *, message
 error stop 1
 end subroutine
 subroutine physics_message(message)
 character(*) :: message
 print *, trim(message)
 end subroutine
end module
module mpas_io_units
contains
 subroutine mpas_new_unit(unit,unformatted)
 integer, intent(out) :: unit
 logical, optional :: unformatted
 unit=20
 end subroutine
 subroutine mpas_release_unit(unit)
 integer :: unit
 end subroutine
end module
! The MPAS build renames the WRF radar module; provide the same alias.
module mp_radar
 use module_mp_radar
end module
