#!/bin/sh
# The build switch LEAN_RAD_BLCLOUD (MYNN's subgrid cloud in the radiation) on single columns: builds the CPU program
# with the switch (./lean, strict CPU build, about 2 min), links tests/suite_coupling/rad_blcloud.f90 with
# its objects, runs it, then two mutants of bl_cloud_water, which the checks must catch.
#   sh tests/suite_coupling/rad_blcloud.sh      (the hashed ./lean build directory stays)
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
build_result=$(./lean build --flavor mpas --switch LEAN_RAD_BLCLOUD=on --target cpu --fc gfortran --jobs 2 --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
out="$root/build/suite_coupling/rad"; rm -rf "$out"; mkdir -p "$out"; cd "$out"
flags=$(cat "$b/flags.txt")
libs=$(cat "$b/libs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
gfortran $flags $inc -c "$root/tests/suite_coupling/rad_blcloud.f90"
gfortran $flags -o rad_blcloud rad_blcloud.o $objects $libs
echo '== LEAN_RAD_BLCLOUD'
./rad_blcloud "$root/reference/radiation"
rc=0
# mutants: subgrid water also on top of resolved cloud; the cloud-fraction gate dropped
mutant() {   # name sed-expression
  mkdir -p "$1"; cd "$1"
  sed "$2" "$root/src/radiation_blcloud.f90" > radiation_blcloud.f90
  cmp -s radiation_blcloud.f90 "$root/src/radiation_blcloud.f90" && { echo "mutant $1 not made"; exit 1; }
  gfortran $flags $inc -c radiation_blcloud.f90
  gfortran $flags -o rad_blcloud ../rad_blcloud.o $(echo "$objects" | grep -v '/radiation_blcloud\.f90\.o$') radiation_blcloud.o $libs
  echo "== mutant $1 (must FAIL)"
  if ./rad_blcloud "$root/reference/radiation"; then echo "mutant $1 NOT caught"; rc=1; else echo "mutant $1 caught"; fi
  cd "$out"
}
mutant on_resolved 's/if(qc(k)<1.e-6.and.cldfra_bl(k)>0.001)/if(cldfra_bl(k)>0.001)/'
mutant no_gate 's/if(qc(k)<1.e-6.and.cldfra_bl(k)>0.001)/if(qc(k)<1.e-6)/'
rm -rf "$out"
exit $rc
