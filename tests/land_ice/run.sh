#!/bin/sh
# Land ice, lean against MPAS 8.4.2: the unchanged MPAS Noah-MP driver
# driver_lsm_noahmp (glacier physics through NoahmpDriverMain) against lean's src/land_ice.f90 on the glacier columns of
# tests/land_ice/columns.f90, then a mutant that swaps the two interpolation weights of p8w in src/land_ice.f90 (it
# must be seen), then tests/land_ice/check_empty.sh (the card's glacier keep/restore statements with no glaciers).
#   sh tests/land_ice/run.sh
# Needs reference/mpas_src (./lean fetch-upstream; exit 77 without it); builds the plain mpas flavor with ./lean.
# Writes build/land_ice/ (about 20 MB); exit 1 if lean differs from MPAS or the mutant is not seen.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
phys="$root/reference/mpas_src/src/core_atmosphere/physics"
cd "$root"
[ -d "$phys" ] || { echo 'SKIP land ice: reference/mpas_src is absent (./lean fetch-upstream)'; exit 77; }
build_result=$(./lean build --flavor mpas --target cpu --fc gfortran --jobs 2 --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
lean=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
out="$root/build/land_ice"
[ -x "$lean/lean_mpas" ] || { echo "no build in $lean"; exit 2; }
flags="$(cat "$lean/flags.txt")"
libs=$(cat "$lean/libs.txt")
rm -rf "$out"; mkdir -p "$out/mpas" "$out/mutant"
cd "$out/mpas"
sed -n '/^module mpas_derived_types/,/^end module/p;/^module mpas_pool_routines/,/^end module/p;/^module mpas_timer/,/^end module/p' \
  "$root/tests/dyn_pool.f90" > pool.f90
sed -n '/^module mpas_log/,/^end module/p' "$root/tests/dyn_pool.f90" > log_interface.f90
inc="-I$out/mpas $(sed 's/^/-I/' "$lean/moddirs.txt")"
gfortran $flags $inc -c pool.f90
gfortran $flags $inc -c log_interface.f90 -o log_interface_unused.o
python3 "$root/tests/land_ice/pools.py" "$phys" "$out/mpas"
gfortran $flags $inc -c "$phys/mpas_atmphys_vars.F"
gfortran $flags $inc -c "$root/tests/land_ice/pools_module.f90"
gfortran $flags $inc -c "$phys/mpas_atmphys_driver_lsm_noahmp.F"
lean_objects=$(grep -v '/main\.f90\.o$' "$lean/objects.txt")
mpas_objects="$out/mpas/pool.o $out/mpas/mpas_atmphys_vars.o $out/mpas/pools_module.o $out/mpas/mpas_atmphys_driver_lsm_noahmp.o"
gfortran $flags $inc -c "$root/tests/land_ice/columns.f90"
gfortran $flags -o columns columns.o $mpas_objects $lean_objects $libs
ln -sf "$root/reference/config/NoahmpTable.TBL" NoahmpTable.TBL
ln -sf "$root/reference/config/LANDUSE.TBL" LANDUSE.TBL
./columns | tee columns.log
cd "$out/mutant"
ln -sf "$root/reference/config/NoahmpTable.TBL" NoahmpTable.TBL
ln -sf "$root/reference/config/LANDUSE.TBL" LANDUSE.TBL
sed 's/  io%p8w(1,k) = fzm\*totp + fzp\*totm/  io%p8w(1,k) = fzp*totp + fzm*totm/' "$root/src/land_ice.f90" > land_ice.f90
cmp -s land_ice.f90 "$root/src/land_ice.f90" && { echo "mutant: the edit did not apply"; exit 1; }
gfortran $flags -I. $inc -c land_ice.f90 -o land_ice.o
gfortran $flags -I. $inc -c "$root/tests/land_ice/columns.f90" -o columns.o
gfortran $flags -o columns_mutant columns.o land_ice.o $mpas_objects $(echo "$lean_objects" | grep -v '/land_ice\.f90\.o$') $libs
./columns_mutant > mutant.log 2>&1 || true
# seen = the comparison ran to its end and counted differences (a crash is not a detection)
if grep -q 'land ice lean/MPAS: .* [1-9][0-9]* differing values' mutant.log; then
  echo "mutant seen: $(grep 'land ice lean/MPAS' mutant.log)"
else
  echo "mutant NOT seen"; tail -3 mutant.log; exit 1
fi

sh "$root/tests/land_ice/check_empty.sh" "$lean"
