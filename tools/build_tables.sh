#!/bin/sh
# Write the four Thompson tables (MP_THOMPSON_*.DBL) into build/tables/ with MPAS's
# own generator, atmphys_build_tables_thompson.F, compiled by tools/build_reference.sh
# O2. ./lean tables --build does the same in build/lean-tables/ and checks the tables
# against lean.lock.json; ./lean tables --from build/tables installs these.
# A build/tables/complete file keeps the tables of an earlier run.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
"$root/tools/build_reference.sh" O2
cd "$root/build/O2"
flags=$(cat flags.txt)
gfortran $flags -c "$root/reference/mpas_src/src/core_atmosphere/utils/atmphys_build_tables_thompson.F"
gfortran $flags -o build_tables mpas_kind_types.o services.o mpas_atmphys_functions.o module_mp_radar.o radar_alias.o module_mp_thompson.o atmphys_build_tables_thompson.o "$root/tests/build_tables.f90"
mkdir -p ../tables
cd ../tables
if [ ! -f complete ]; then
 rm -f ./*.DBL
 ../O2/build_tables > generation.log 2>&1
 touch complete
fi
