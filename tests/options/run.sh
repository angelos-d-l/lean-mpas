#!/bin/sh
# Unit checks of the scheme 'off' sets and the namelist options (tests/options/units.f90, radii_columns.f90) on the
# cpu build's objects:
#   sh tests/options/run.sh [FC] [JOBS]
# Prints OPTIONS TESTS OK <n>. Writes build/options/units; a passing run removes it.
set -eu
cd "$(dirname "$0")/../.."
FC=${1:-gfortran}
JOBS=${2:-2}
mkdir -p build/options/units
trap 'code=$?; if [ "$code" -ne 0 ]; then echo "OPTIONS TESTS FAILED (build/options/units kept)"; fi' 0
./lean build --target cpu --fc "$FC" --profile strict --jobs "$JOBS" --json > build/options/units/build-result.json
b=$(python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(open(sys.argv[1]))["build_json"]).resolve().parent)' build/options/units/build-result.json)
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
# MPAS's own APVM lines (mpas_atm_time_integration.F, atm_compute_solve_diagnostics_work), cut out between their two
# markers for the unit apvm_oracle: the text must be found exactly once, and the oracle compiles it as written.
src=reference/mpas_src/src/core_atmosphere/dynamics/mpas_atm_time_integration.F
if [ -f "$src" ]; then
  [ "$(grep -c 'if (config_apvm_upwinding > 0.0) then' "$src")" -eq 1 ] || { echo "FAIL: APVM start marker not found once"; exit 1; }
  [ "$(grep -c 'end if  ! apvm upwinding' "$src")" -eq 1 ] || { echo "FAIL: APVM end marker not found once"; exit 1; }
  awk '/if \(config_apvm_upwinding > 0.0\) then/ {on=1} on {print} /end if  ! apvm upwinding/ {on=0}' "$src" \
    > build/options/units/apvm_oracle.inc
else
  : > build/options/units/apvm_oracle.inc
  echo "SKIP apvm_oracle: no $src"
fi
# MPAS's calc_cldfraction and calc_cldincidence as they are, in a small host module; the unit incidence_oracle compares
# lean's cld_incidence with them. Without reference/mpas_src the module holds a stub and incidence_oracle is skipped.
python3 - "$PWD/reference/mpas_src/src/core_atmosphere/physics/mpas_atmphys_driver_cloudiness.F" \
  build/options/units/cloud_reference.f90 <<'PY'
import sys
from pathlib import Path
source, out = Path(sys.argv[1]), Path(sys.argv[2])
routines = ''
if source.is_file():
    text = source.read_text()
    for name in ('calc_cldfraction', 'calc_cldincidence'):
        start = text.index(' subroutine %s(' % name)
        end = text.index(' end subroutine %s' % name, start) + len(' end subroutine %s' % name)
        routines += text[start:end] + '\n'
else:
    routines = (' subroutine calc_cldincidence(cldfrac,qc,qi,f_qc,f_qi,its,ite)\n'
                '  real,intent(out) :: cldfrac(:,:,:)\n  real,intent(in) :: qc(:,:,:),qi(:,:,:)\n'
                '  logical,intent(in) :: f_qc,f_qi\n  integer,intent(in) :: its,ite\n'
                "  error stop 'no MPAS source'\n end subroutine\n")
out.write_text('module cloud_reference\n use mpas_kind_types\n use config, only: kte => nz, kme => nz\n'
               ' use mpas_atmphys_constants, only: ep_2\n implicit none\n'
               ' integer,parameter :: ims=1,ime=1,kms=1,jms=1,jme=1\n integer,parameter :: jts=1,jte=1,kts=1\n'
               'contains\n' + routines + 'end module\n')
PY
cases_cloud=incidence_oracle
if grep -q 'no MPAS source' build/options/units/cloud_reference.f90; then
  echo "SKIP incidence_oracle: reference/mpas_src is absent (./lean fetch-upstream)"
  cases_cloud=
fi
# shellcheck disable=SC2086
(cd build/options/units && "$fc" $flags $inc -c cloud_reference.f90 -o cloud_reference.o)
# shellcheck disable=SC2086
"$fc" $flags $inc -Ibuild/options/units -o build/options/units/units tests/options/units.f90 build/options/units/cloud_reference.o $objects $libs
# The radiation columns with MPAS's own radii and with Thompson's, against MPAS's wrappers
# shellcheck disable=SC2086
"$fc" $flags $inc -o build/options/units/radii_columns tests/options/radii_columns.f90 $objects $libs
n=0
for case in schemes_default re_lw_off re_sw_off re_both re_false re_micro_off moist_none cloud_incidence $cases_cloud apvm_zero apvm_oracle_half apvm_oracle_one; do
  build/options/units/units "$case"
  n=$((n + 1))
done
# the second call with other values is an error stop (a nonzero code, and the reason on stderr)
set +e
build/options/units/units second_call > build/options/units/second.out 2> build/options/units/second.err
code=$?
set -e
[ "$code" -ne 0 ] && [ "$code" -ne 3 ] || { echo "FAIL second_call: exit $code"; exit 1; }
grep -q 'set once per run' build/options/units/second.err || { echo "FAIL second_call: no reason on stderr"; exit 1; }
echo "ok   second_call: error stop (exit $code)"
n=$((n + 1))
for mode in re0 re1; do
  build/options/units/radii_columns "$mode"
  n=$((n + 1))
done
set +e
build/options/units/units apvm_second_call > build/options/units/apvm.out 2> build/options/units/apvm.err
code=$?
set -e
[ "$code" -ne 0 ] && [ "$code" -ne 3 ] || { echo "FAIL apvm_second_call: exit $code"; exit 1; }
grep -q 'set once per run' build/options/units/apvm.err || { echo "FAIL apvm_second_call: no reason on stderr"; exit 1; }
echo "ok   apvm_second_call: error stop (exit $code)"
n=$((n + 1))
rm -rf build/options/units
rmdir build/options 2>/dev/null || :
echo "OPTIONS TESTS OK $n"
