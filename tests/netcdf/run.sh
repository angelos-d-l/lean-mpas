#!/bin/sh
# NetCDF checks through the same compiler, modules and netCDF-C as lean_mpas.
# Failure leaves build/netcdf/ for inspection; a passing run removes its scratch files.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
out="$root/build/netcdf"
rm -rf "$out"
mkdir -p "$out"
trap 'exit 1' HUP INT TERM
# Preserve the directory and normalize failures to 1; a missing ncdump is exit 2.
failure_status=1
trap 'code=$?; if [ "$code" -ne 0 ]; then exit "$failure_status"; fi' 0
build_result=$(./lean build --flavor mpas --target cpu --fc "${1:-gfortran}" --jobs "${2:-2}" --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
nc_config=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["netcdf"]["nc_config"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
libs=$(cat "$b/libs.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
object=$(grep '/src/io/lean_nc\.f90\.o$' "$b/objects.txt")
cd "$out"
"$fc" $flags $inc -c "$root/tests/netcdf/check.f90"
for source in "$root"/tests/netcdf/*.f90; do
  name=$(basename "$source" .f90)
  [ "$name" = check ] && continue
  "$fc" $flags -I. $inc -c "$source"
  "$fc" $flags -o "$name" "$name.o" check.o "$object" $libs
  "./$name"
done
ncdump=
if [ -n "$nc_config" ] && [ -x "$(dirname "$nc_config")/ncdump" ]; then
  ncdump="$(dirname "$nc_config")/ncdump"
else
  ncdump=$(command -v ncdump) || {
    echo 'ncdump missing: install it alongside netCDF-C or put it on PATH'
    failure_status=2; exit 2;
  }
fi
for format in 5 2 3; do
  case $format in
    5) expected=cdf5 ;;
    2) expected='64-bit offset' ;;
    3) expected=netCDF-4 ;;
  esac
  actual=$("$ncdump" -k "fmt_$format.nc") || exit 1
  [ "$actual" = "$expected" ] || { echo "fmt_$format.nc: ncdump -k: expected $expected, got $actual"; exit 1; }
  echo "ok   ncdump fmt_$format.nc: $actual"
  header=$("$ncdump" -h "define_$format.nc") || exit 1
  for line in 'float theta(Time, nCells, nVertLevels) ;' 'char xtime(Time, StrLen) ;' 'int ids(nCells) ;' \
              'double radius ;' 'theta:units = "K" ;' ':sphere_radius = 6371229.f ;'; do
    printf '%s\n' "$header" | grep -F "$line" >/dev/null || {
      echo "define_$format.nc: ncdump -h: missing $line"; exit 1;
    }
  done
  echo "ok   ncdump define_$format.nc: Registry dimensions and attributes"
  theta=$("$ncdump" -v theta "order_$format.nc") || exit 1
  printf '%s\n' "$theta" | tr -d '[:space:]' | grep -F \
    'theta=1101,1102,1201,1202,1301,1302,2101,2102,2201,2202,2301,2302;' >/dev/null || {
    echo "order_$format.nc: ncdump -v theta: wrong data order"; exit 1;
  }
  xtime=$("$ncdump" -v xtime "order_$format.nc") || exit 1
  padded=$(printf '"2026-10-11_06:00:00%45s"' '')
  printf '%s\n' "$xtime" | grep -F "$padded" >/dev/null || {
    echo "order_$format.nc: ncdump -v xtime: wrong blank padding"; exit 1;
  }
  echo "ok   ncdump order_$format.nc: theta data order and MPAS string padding"
done
cd "$root"
rm -rf "$out"
echo 'NETCDF TESTS OK'
