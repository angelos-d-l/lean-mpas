#!/bin/sh
# Restart unit checks on the strict cpu build's objects, 1 thread: tests/restart/units.f90 in each of its modes,
# tests/restart/walk_modes.py, and a compile of the restart set with LEAN_BUDGET (which changes radiation_cache).
#   sh tests/restart/run.sh [FC] [JOBS]      prints RESTART TESTS OK 8; build/restart/units is removed at the end
set -eu
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
test -f src/io/lean_restart.f90 || { echo 'FAIL: no lean_restart'; exit 1; }
out="$PWD/build/restart/units"
mkdir -p "$out"
trap 'rm -rf "$out"' EXIT
build_result=$(./lean build --target cpu --fc "${1:-gfortran}" --profile strict --jobs "${2:-2}" --json)
b=$(printf '%s' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
libs=$(cat "$b/libs.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/src/run/main.f90.o$' "$b/objects.txt")
"$fc" $flags $inc -o "$out/units" tests/restart/units.f90 $objects $libs
ln -s "$PWD/reference/config/NoahmpTable.TBL" "$out/"
(cd "$out" && ./units)
(cd "$out" && "${LEAN_PY:-${PYTHON:-python3}}" "$OLDPWD/tests/restart/walk_modes.py")
(cd "$out" && ./units xtime_bits)
(cd "$out" && ./units switches_long)
(cd "$out" && ./units switches_only)
(cd "$out" && ./units missing_input)
(cd "$out" && ./units truncated_input)
(cd "$out" && LEAN_WRITER_DIE=-1 ./units failed_diagnostics_write)
# The budget changes radiation_cache's shape. Compile the walk against that shape.
mkdir -p "$out/budget"
(cd "$out/budget" && "$fc" $flags -DLEAN_BUDGET -I. $inc -c \
    "$OLDPWD/src/pre_dynamics.f90" "$OLDPWD/src/model_data.f90" "$OLDPWD/src/diag/diag_manager.f90" \
    "$OLDPWD/src/lean_fields.f90" "$OLDPWD/src/surface_bdy.f90" "$OLDPWD/src/io/lean_restart_set.f90")
echo 'RESTART BUDGET WALK COMPILE OK'
echo 'RESTART TESTS OK 8'
