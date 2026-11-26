! thompson.f90 keeps the `use mp_radar` of MPAS's Thompson code. The radar routines compiled here are those of
! physics_wrf/module_mp_radar.F (third_party/), whose module is module_mp_radar; this alias gives them the name
! thompson.f90 uses.
module mp_radar
 use module_mp_radar
end module
