#!/bin/sh
# Sea ice, lean against MPAS 8.4.2: the unchanged MPAS drivers driver_sfclayer,
# driver_seaice and atmphys_sfc_diagnostics with the pristine MYNN surface layer and Noah sea-ice scheme, against lean's
# src/sea_ice.f90 + src/sea_ice_noah.f90, on the synthetic columns of tests/sea_ice/columns.f90. Then the mutation
# check: two one-character changes of src/sea_ice.f90 must each be seen.
#   sh tests/sea_ice/run.sh
# Needs reference/mpas_src (./lean fetch-upstream; exit 77 without it); builds the plain mpas flavor with ./lean.
# Writes build/sea_ice/ (about 20 MB); exit 1 if lean differs from MPAS or a mutant is not seen.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
phys="$root/reference/mpas_src/src/core_atmosphere/physics"
cd "$root"
[ -d "$phys" ] || { echo 'SKIP sea ice: reference/mpas_src is absent (./lean fetch-upstream)'; exit 77; }
build_result=$(./lean build --flavor mpas --target cpu --fc gfortran --jobs 2 --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
lean=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
out="$root/build/sea_ice"
[ -x "$lean/lean_mpas" ] || { echo "no build in $lean"; exit 2; }
flags="$(cat "$lean/flags.txt")"
libs=$(cat "$lean/libs.txt")
rm -rf "$out"; mkdir -p "$out/mpas" "$out/mutant"
cd "$out/mpas"
# the pool stand-in without its logger and clock: the program keeps lean's logger (src/serial_services.f90)
sed -n '/^module mpas_derived_types/,/^end module/p;/^module mpas_pool_routines/,/^end module/p;/^module mpas_timer/,/^end module/p' \
  "$root/tests/dyn_pool.f90" > pool.f90
sed -n '/^module mpas_log/,/^end module/p' "$root/tests/dyn_pool.f90" > log_interface.f90
inc="-I$out/mpas $(sed 's/^/-I/' "$lean/moddirs.txt")"
gfortran $flags $inc -c pool.f90
gfortran $flags $inc -c log_interface.f90 -o log_interface_unused.o
for f in physics_mmm/mynn_shared.F90 physics_mmm/sf_mynn.F90 physics_wrf/sf_mynn_pre.F physics_wrf/module_sf_mynn.F \
  physics_wrf/module_sf_sfclay.F physics_mmm/sf_sfclayrev.F90 physics_wrf/sf_sfclayrev_pre.F physics_wrf/module_sf_sfclayrev.F \
  mpas_atmphys_vars.F mpas_atmphys_lsm_shared.F physics_wrf/module_sf_noahlsm.F physics_wrf/module_sf_noah_seaice.F \
  physics_wrf/module_sf_noah_seaice_drv.F physics_wrf/module_sf_sfcdiags.F mpas_atmphys_driver_sfclayer.F \
  mpas_atmphys_driver_seaice.F mpas_atmphys_sfc_diagnostics.F; do
  gfortran $flags $inc -c "$phys/$f"
done
gfortran $flags $inc -c "$root/tests/sea_ice/original.f90"
# lean's objects: every unit of the build but its program and the logger interface of the stand-in
lean_objects=$(grep -v '/main\.f90\.o$' "$lean/objects.txt")
mpas_objects=$(ls ./*.o | grep -v log_interface_unused)
gfortran $flags $inc -c "$root/tests/sea_ice/columns.f90"
gfortran $flags -o columns columns.o $mpas_objects $lean_objects $libs
./columns | tee columns.log
# mutants: the same program with one change in src/sea_ice.f90 each; each must report differences
cd "$out/mutant"
status=0
for m in 1 2; do
  case $m in
    1) sed 's/ tsk_sea = max(tsk_sea, 271.4)/ tsk_sea = max(tsk_sea, 271.5)/' "$root/src/sea_ice.f90" > sea_ice.f90 ;;
    2) sed 's/ if (x >= 0.5) a%regime = regime_hold/ if (x > 0.5) a%regime = regime_hold/' "$root/src/sea_ice.f90" > sea_ice.f90 ;;
  esac
  cmp -s sea_ice.f90 "$root/src/sea_ice.f90" && { echo "mutant $m: the edit did not apply"; exit 1; }
  gfortran $flags -I. $inc -c sea_ice.f90 -o sea_ice.o
  gfortran $flags -I. $inc -c "$root/tests/sea_ice/columns.f90" -o columns.o
  objects=$(echo "$lean_objects" | grep -v '/sea_ice\.f90\.o$')
  gfortran $flags -o columns_mutant columns.o sea_ice.o $(cd "$out/mpas" && ls "$out"/mpas/*.o | grep -v 'log_interface_unused\|/columns\.o$') $objects $libs
  ./columns_mutant > mutant$m.log 2>&1 || true
  # seen = the comparison ran to its end and counted differences (a crash is not a detection)
  if grep -q 'sea ice lean/MPAS: .* [1-9][0-9]* differing values' mutant$m.log; then
    echo "mutant $m seen: $(grep 'sea ice lean/MPAS' mutant$m.log)"
  else
    echo "mutant $m NOT seen"; tail -3 mutant$m.log; status=1
  fi
done
exit $status
