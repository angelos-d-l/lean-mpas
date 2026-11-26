#!/bin/sh
# Compile the original MPAS Thompson microphysics (reference/mpas_src, from
# ./lean fetch-upstream) and the modules it needs into build/MODE, in single
# precision. MODE is O<n>, the optimisation level (default O0); tools/build_tables.sh
# uses O2. The flags go to build/MODE/flags.txt.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
mode=${1:-O0}
mkdir -p "$root/build/$mode"
cd "$root/build/$mode"
opt="-O${mode#O}"
flags="-cpp -Dmpas -DSINGLE_PRECISION -ffree-form -ffree-line-length-none $opt"
ref="$root/reference/mpas_src/src"
# tests/support.f90 ends with module mp_radar, which uses MPAS's module_mp_radar: compile the file without it first,
# and the alias (radar_alias.f90) after module_mp_radar.
sed '/^module mp_radar/,$d' "$root/tests/support.f90" > services.f90
gfortran $flags -c "$ref/framework/mpas_kind_types.F" services.f90 "$ref/core_atmosphere/physics/mpas_atmphys_functions.F" "$ref/core_atmosphere/physics/physics_wrf/module_mp_radar.F"
printf 'module mp_radar\n use module_mp_radar\nend module\n' > radar_alias.f90
gfortran $flags -c radar_alias.f90 "$ref/core_atmosphere/physics/physics_wrf/module_mp_thompson.F"
printf '%s\n' "$flags" > flags.txt
