#!/bin/sh
# Compile src/phys_state.f90 into test_phys_state.o for the column test harnesses
# (tests/mynn_moisture, tests/suite_coupling), which call its routines to zero
# their fields after allocating them.  Usage: build_test_state.sh ROOT FLAGS...
# Its type dependencies are compiled for their module files only, into
# test-state-mod/, so these modules stay out of the harness's own search path;
# the harness links its own state objects.
set -eu
root=$1; shift
mkdir -p test-state-mod
for source in \
 "$root/reference/mpas_src/src/framework/mpas_kind_types.F" \
 "$root/reference/mpas_src/src/core_atmosphere/physics/physics_noahmp/utility/Machine.F90" \
 "$root/src/noahmp_state.f90" "$root/src/gf_state.f90" "$root/src/gwdo_state.f90"; do
 gfortran "$@" -Itest-state-mod -Jtest-state-mod -fsyntax-only "$source"
done
gfortran "$@" -Itest-state-mod -c "$root/src/phys_state.f90" -o test_phys_state.o
