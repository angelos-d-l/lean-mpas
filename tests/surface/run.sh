#!/bin/sh
# Unit checks of the surface update (tests/surface/units.f90, oracle.F90) on the cpu build's objects:
#   sh tests/surface/run.sh [FC] [JOBS]
# Prints SURFACE TESTS OK <n>. Writes build/surface/units; a passing run removes it.
set -eu
cd "$(dirname "$0")/../.."
FC=${1:-gfortran}
JOBS=${2:-1}
py=${LEAN_PY:-${PYTHON:-python3}}
mkdir -p build/surface/units
trap 'code=$?; if [ "$code" -ne 0 ] && [ "$code" -ne 77 ]; then echo "SURFACE TESTS FAILED (build/surface/units kept)"; fi' 0
./lean build --target cpu --fc "$FC" --profile strict --jobs "$JOBS" --json > build/surface/units/build-result.json
b=$(python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(open(sys.argv[1]))["build_json"]).resolve().parent)' build/surface/units/build-result.json)
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
# MPAS's own physics_update_sst: the executable lines between the two markers, cut out once and compiled as they are
src=reference/mpas_src/src/core_atmosphere/physics/mpas_atmphys_update_surface.F
[ -f "$src" ] || { echo "SKIP surface units: no $src (the oracle needs MPAS's source: ./lean fetch-upstream)"; exit 77; }
[ "$(grep -c '^ subroutine physics_update_sst(' "$src")" -eq 1 ] || { echo "FAIL: physics_update_sst not found once"; exit 1; }
awk '/^ subroutine physics_update_sst\(/ {inside=1} inside && /^ if\(config_frac_seaice\) then/ && !on {on=1}
     /^!local and global max and min sea-surface temperatures/ {if (on) {on=0; inside=0}}
     on {print}' "$src" > build/surface/units/update_body.inc
[ "$(grep -c 'nb_to_land = nb_to_land + 1' build/surface/units/update_body.inc)" -eq 1 ] || { echo "FAIL: the cut-out body is not MPAS's update"; exit 1; }
[ "$(grep -c 'xicem(iCell)  = xice(iCell)' build/surface/units/update_body.inc)" -eq 1 ] || { echo "FAIL: the cut-out body stops early"; exit 1; }
# the fields the body assigns, for the map_complete unit
grep -oE '[A-Za-z_0-9]+\((iCell|iSoil,iCell)\) *= ' build/surface/units/update_body.inc | sed 's/(.*//' | sort -u > build/surface/units/assigned.txt
# the units run on greece_60km's sizes (units.f90 sets them: 2337 cells)
"$py" tests/surface/make_units_files.py build/surface/units 2337
# shellcheck disable=SC2086
(cd build/surface/units && "$fc" $flags $inc -I. -c ../../../tests/surface/oracle.F90 -o oracle.o)
# shellcheck disable=SC2086
"$fc" $flags $inc -Ibuild/surface/units -o build/surface/units/units tests/surface/units.f90 build/surface/units/oracle.o $objects $libs
n=0
for case in update_oracle map_complete file_ice read_record land_ice water_ice; do
  build/surface/units/units "$case"
  n=$((n + 1))
done
rm -rf build/surface/units
rmdir build/surface 2>/dev/null || :
echo "SURFACE TESTS OK $n"
