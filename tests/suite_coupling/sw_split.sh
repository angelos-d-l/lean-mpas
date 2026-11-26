#!/bin/sh
# The build switch LEAN_NOAHMP_SWSPLIT (Noah-MP takes RRTMG's direct/diffuse and visible/near-infrared split of the
# surface shortwave) on single columns (tests/suite_coupling/sw_split.f90), then two mutants that must be caught.
#   sh tests/suite_coupling/sw_split.sh
# About 2 minutes for the serial CPU build, then seconds for the columns and mutants. Uses ./lean's strict CPU build
# with the switch and its object/module manifests; leaves the hashed build directory and removes the column/mutant
# outputs even on failure.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
export OMP_NUM_THREADS=1
build_result=$(./lean build --flavor mpas --switch LEAN_NOAHMP_SWSPLIT=on --target cpu --fc gfortran --jobs 2 --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
out="$root/build/suite_coupling/sw_split"
rm -rf "$out"; mkdir -p "$out"
trap 'rm -rf "$out"' EXIT HUP INT TERM
cd "$out"
flags=$(cat "$b/flags.txt")
libs=$(cat "$b/libs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
gfortran $flags $inc -c "$root/tests/suite_coupling/sw_split.f90"
# Observe the raw RRTMG components at the surface, independently of the three ratios the switch adds.
sed '/beam(3)  = swdkdif(1,1)/a\
call sw_split_surface_probe(sibvisdir(1,1),sibnirdir(1,1),sibvisdif(1,1),sibnirdif(1,1),swdflx(1,1))
' "$root/src/radiation_sw.f90" > radiation_sw.f90
gfortran $flags $inc -c radiation_sw.f90
base_objects=$(echo "$objects" | grep -v '/radiation_sw\.f90\.o$')
gfortran $flags -o sw_split sw_split.o radiation_sw.o $base_objects $libs
echo '== LEAN_NOAHMP_SWSPLIT'
./sw_split "$root/reference/radiation"
rc=0
mutant() {
 name=$1; source=$2; expression=$3; object=$4
 mkdir "$name"; cd "$name"
 sed "$expression" "$source" > "$object.f90"
 cmp -s "$object.f90" "$source" && { echo "mutant $name not made"; exit 1; }
 gfortran $flags $inc -c "$object.f90"
 if [ "$object" = radiation_sw ]; then
  gfortran $flags -o sw_split ../sw_split.o radiation_sw.o $base_objects $libs
 else
  gfortran $flags -o sw_split ../sw_split.o ../radiation_sw.o $(echo "$base_objects" | grep -v '/AtmosForcingMod\.f90\.o$') AtmosForcingMod.o $libs
 fi
 echo "== mutant $name (must FAIL)"
 if ./sw_split "$root/reference/radiation"; then echo "mutant $name NOT caught"; rc=1; else echo "mutant $name caught"; fi
 cd "$out"
}
mutant toa "$out/radiation_sw.f90" 's/beam(4) = sibvisdir(1,1)/beam(4) = sibvisdir(1,kte+2)/' radiation_sw
mutant no_guard "$b/noahmp_lean/AtmosForcingMod.f90" 's/if ( noahmp%sw_frac_dir_vis + noahmp%sw_frac_dir_nir + noahmp%sw_frac_dif_vis > 0.0 ) then/if ( .true. ) then/' AtmosForcingMod
exit "$rc"
