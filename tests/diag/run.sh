#!/bin/sh
# Diagnostics against an oracle of MPAS's own routines (tests/diag/mpas_oracle.f90): units, convective and isobaric
# columns, the SLP messages, the text audit of the card kernels (audit.py), the call order. Needs the release case
# inputs in data/cases (exit 77 without them); the output families, the OpenMP pass and boundary_vertex_vorticity.py
# compare with MPAS's reference outputs, which are not released,
# and SKIP without them.
#   sh tests/diag/run.sh [FC] [JOBS]
set -eu
cd "$(dirname "$0")/../.."
root=$PWD
fc=${1:-gfortran}
jobs=${2:-2}
cases=data/cases
if [ ! -f $cases/greece_60km/inputs/greece_60km.init.nc ] || [ ! -f $cases/x1_2562/inputs/x1.2562.init.nc ]; then
  echo 'SKIP diag: needs the release case inputs in data/cases (cases/README.md)'
  exit 77
fi
references=yes
for run in greece_60km/reference/strict_12h x1_2562/reference/strict_24h x1_2562/reference/strict_l55_diag1h; do
  [ -d "$cases/$run" ] || references=
done
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=1
mkdir -p build/diag
./lean build --target cpu --fc "$fc" --profile strict --jobs "$jobs" --json > build/diag/build-result.json
b=$(python3 -c 'import json;from pathlib import Path;print(Path(json.load(open("build/diag/build-result.json"))["build_json"]).resolve().parent)')
fc=$(python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
case "$fc" in
 *nvfortran) modflag=-module ;;
 *) modflag=-J ;;
esac
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
"$fc" $flags $inc -o build/diag/units tests/diag/units.f90 $objects $libs
build/diag/units
# Recompile only src/dyn_micro.f90, where precipitable water is computed, with LEAN_PRECIPW_MOIST; link the same
# objects otherwise.
mkdir -p build/diag/moist
"$fc" $flags $inc -DLEAN_PRECIPW_MOIST $modflag build/diag/moist -c src/dyn_micro.f90 -o build/diag/moist/dyn_micro.o
other_objects=$(printf '%s\n' "$objects" | grep -v '/dyn_micro\.f90\.o$')
"$fc" $flags $inc -DLEAN_PRECIPW_MOIST -o build/diag/units_moist tests/diag/units.f90 build/diag/moist/dyn_micro.o $other_objects $libs
build/diag/units_moist
# The oracle is compiled with the model's flags: under nvfortran that is how the MPAS reference build was compiled.
"$fc" $flags $inc $modflag build/diag -c tests/diag/mpas_oracle.f90 -o build/diag/mpas_oracle.o
"$fc" $flags $inc -Ibuild/diag -o build/diag/convective tests/diag/convective.f90 build/diag/mpas_oracle.o $objects $libs
build/diag/convective
"$fc" $flags $inc -Ibuild/diag -o build/diag/isobaric tests/diag/isobaric.f90 build/diag/mpas_oracle.o $objects $libs
(cd build/diag; ./isobaric)
python3 tests/diag/slp_messages.py
if [ -n "$references" ]; then
"${LEAN_PY:-${PYTHON:-python3}}" tests/diag/boundary_vertex_vorticity.py
"$fc" $flags $inc -Ibuild/diag -o build/diag/families tests/diag/families.f90 build/diag/mpas_oracle.o $objects $libs
(cd "$b"; "$root/build/diag/families" "$root" greece_60km)
(cd "$b"; "$root/build/diag/families" "$root" x1_2562)
"$fc" $flags $inc -Ibuild/diag -o build/diag/output_families tests/diag/output_families.f90 build/diag/mpas_oracle.o $objects $libs
(cd "$b"; "$root/build/diag/output_families" "$root" greece_60km)
(cd "$b"; "$root/build/diag/output_families" "$root" x1_2562)
(cd "$b"; "$root/build/diag/output_families" "$root" x1_2562 l55)
sh tests/diag/openmp.sh "$b" "$fc"
else
echo 'SKIP families, output_families, openmp, boundary_vertex_vorticity: the MPAS reference outputs of data/cases are not released'
fi
python3 tests/diag/audit.py
python3 tests/diag/order.py "$fc"
rm -rf build/diag
