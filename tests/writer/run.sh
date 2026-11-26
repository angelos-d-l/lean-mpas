#!/bin/sh
# Writer unit checks on the strict cpu build's objects, 1 thread: tests/writer/writer_main.f90 writes one small file per
# case and tests/writer/check_writer.py checks each with the ncdump of the netCDF this build links (or NCDUMP).
#   sh tests/writer/run.sh [FC] [JOBS]      prints WRITER TESTS OK <n>; build/writer is removed after a pass
set -eu
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
out="$PWD/build/writer"
mkdir -p "$out"
# Stop before a build when the writer source is missing.
test -f src/io/lean_writer.f90 || { echo 'FAIL: no lean_writer'; exit 1; }
build_result=$(./lean build --target cpu --fc "${1:-gfortran}" --profile strict --jobs "${2:-2}" --json)
b=$(printf '%s' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
libs=$(cat "$b/libs.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/src/run/main.f90.o$' "$b/objects.txt")
"$fc" $flags $inc -o "$out/writer" tests/writer/writer_main.f90 $objects $libs
# ncdump of the netCDF this build links (it reads CDF-5; another ncdump on PATH may not), unless NCDUMP names one
NCDUMP=${NCDUMP:-$(python3 -c 'import json,sys; from pathlib import Path; n=json.load(open(sys.argv[1]))["netcdf"]
print(next((str(c) for c in (Path(n["nc_config"]).parent / "ncdump", Path(n["prefix"]) / "bin/ncdump") if c.is_file()), ""))' "$b/build.json")}
export NCDUMP
python3 tests/writer/check_writer.py "$out"
rm -rf "$out"
