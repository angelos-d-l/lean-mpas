! Stand-ins for the MPAS framework modules that the MPAS physics code compiled here uses (mpas_derived_types, mpas_log,
! mpas_atmphys_utilities, mpas_io_units): messages go to standard output, a fatal error stops the program.
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
 ! The only caller is the Thompson table read at startup (thompson.f90), which opens one file at a time on this unit and
 ! closes it before the next; lean opens every other file with newunit=, so a fixed unit is enough.
 subroutine mpas_new_unit(unit,unformatted)
 integer, intent(out) :: unit
 logical, optional :: unformatted
 unit=20
 end subroutine
 subroutine mpas_release_unit(unit)
 integer :: unit
 end subroutine
end module
