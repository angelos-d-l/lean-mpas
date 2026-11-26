#!/bin/sh
# Forecasts from MPAS run directories (run_dir.sh), in atmosphere_model's form:
#   sh tests/inputs/runs.sh [FC] [JOBS]            LEAN_PY overrides the Python with netCDF4 and numpy
# One plain build (flavor mpas), every run with 1 thread: greece_60km's log against MPAS's for a start at 06:30, dt 2.5 s
#   and LW 30 min / SW 15 min; LW without SW refused at the start; MPAS's big-endian Thompson tables give the outputs of
#   lean's own; the stops for a missing boundary file and a missing init file; x1_2562 at dt 600 s takes 7 microphysics
#   sub-steps. Prints RUNS OK <n>. Needs the release case inputs in data/cases (exit 77 without them); the comparisons
#   with MPAS's logs need its reference runs, which are not released, and SKIP without them.
# Writes build/inputs/run_<case> and build/inputs/runs/; a passing run removes them.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
FC=${1:-gfortran}
JOBS=${2:-2}
py=${LEAN_PY:-${PYTHON:-python3}}
work="$root/build/inputs/runs"
trap 'exit 1' HUP INT TERM
if [ ! -f data/cases/greece_60km/inputs/greece_60km.init.nc ] || [ ! -f data/cases/x1_2562/inputs/x1.2562.init.nc ]; then
  echo 'SKIP inputs runs: needs the release case inputs in data/cases (cases/README.md)'
  exit 77
fi
trap 'code=$?; if [ "$code" -ne 0 ]; then echo "INPUTS RUNS FAILED (build/inputs kept)"; exit 1; fi' 0
rm -rf "$work" "$root"/build/inputs/run_*
mkdir -p "$work"
. "$root/tests/inputs/run_dir.sh"

build_result=$(./lean build --flavor mpas --target cpu --fc "$FC" --jobs "$JOBS" --json) || {
  printf '%s\n' "$build_result"; exit 1;
}
b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')

# $1: the run directory, $2: the name of its log in $work (stdout, log.atmosphere.0000.out and .err); its exit status
mpas_run() {
  mpas_code=0
  (cd "$1" && OMP_NUM_THREADS=1 "$b/lean_mpas" > "$work/$2.stdout" 2>&1) || mpas_code=$?
  cat "$work/$2.stdout" "$1"/log.atmosphere.0000.out > "$work/$2.log" 2>/dev/null || :
  [ ! -f "$1/log.atmosphere.0000.err" ] || cat "$1/log.atmosphere.0000.err" >> "$work/$2.log"
  return "$mpas_code"
}
# $1: case, $2: log, $3: a text the log must hold
says() { grep -qF -- "$3" "$2" || { echo "FAIL $1: $2 lacks: $3"; tail -5 "$2"; exit 1; }; }
# $1: case, $2: a run directory that must hold no NetCDF output
no_output() {
  written=$(find "$2" -name '*.nc' -type f | head -1)
  [ -z "$written" ] || { echo "FAIL $1: wrote $written"; exit 1; }
}

passed=0
skipped=0
skip_names=""
greece="$root/data/cases/greece_60km/inputs"
x1="$root/data/cases/x1_2562/inputs"
reference="$root/data/cases/greece_60km/reference"
# $1: case, $2: the MPAS reference run it compares with; true (and a SKIP line) when that run's log is absent
no_reference() {
  [ -f "$reference/$2/log.atmosphere.0000.out" ] && return 1
  echo "SKIP $1: the MPAS reference run $2 is not released"
  skipped=$((skipped + 1)); skip_names="$skip_names $1"
  return 0
}

if no_reference start0630 strict_0630_3h; then :
else
# A start off the hour, against MPAS's log.
start_rd="$root/build/inputs/run_start0630"
make_run_dir "$start_rd" "$greece/v0630" cases/greece_60km/case.txt cases/greece_60km/mpas_runs/strict_0630_3h.txt
# MPAS's reference ran 3 h 2 min so that its diag file lies past the 09:30 boundary record
# (cases/greece_60km/mpas_runs/strict_0630_3h.txt); lean runs the first 3 h (90 steps of 120 s) and its log is
# compared over those steps.
python3 - "$start_rd/namelist.atmosphere" <<'PYEDIT'
from pathlib import Path
import sys
p=Path(sys.argv[1]); s=p.read_text().replace("0_03:02:00", "0_03:00:00"); p.write_text(s)
PYEDIT
mpas_run "$start_rd" start0630
python3 tests/settings/compare_log.py "$start_rd/log.atmosphere.0000.out" "$root/data/cases/greece_60km/reference/strict_0630_3h/log.atmosphere.0000.out" 90
rm -rf "$start_rd"
passed=$((passed + 1))
fi

# Other clock settings (dt 2.5 s; LW every 30 min with SW every 15 min), against MPAS's logs of the same runs.
full_run() {
 full_name=$1; full_steps=$2; full_reference=$3
 full_rd="$root/build/inputs/run_$full_name"
 make_run_dir "$full_rd" "$greece" cases/greece_60km/case.txt "$work/$full_name.spec"
 mpas_run "$full_rd" "$full_name" || { tail -20 "$work/$full_name.log"; exit 1; }
 python3 tests/settings/compare_log.py "$full_rd/log.atmosphere.0000.out" "$root/data/cases/greece_60km/reference/$full_reference/log.atmosphere.0000.out" "$full_steps"
 rm -rf "$full_rd"
}
if no_reference dt2p5 strict_dt2p5_30m; then :
else
cat > "$work/dt2p5.spec" <<'SPEC'
nml nhyd_model config_dt 2.5
nml nhyd_model config_run_duration '0_00:30:00'
SPEC
full_run dt2p5 720 strict_dt2p5_30m
passed=$((passed + 1))
fi
cat > "$work/radsplit.spec" <<'SPEC'
nml nhyd_model config_run_duration '0_03:00:00'
nml physics config_radtlw_interval '00:30:00'
nml physics config_radtsw_interval '00:15:00'
SPEC
if no_reference radsplit strict_radsw15_3h; then :
else
full_run radsplit 90 strict_radsw15_3h
passed=$((passed + 1))
fi
# LW every 15 min, SW every 30: MPAS cannot run longwave alone, lean refuses it at the start
cat > "$work/radlwalone.spec" <<'SPEC'
nml physics config_radtlw_interval '00:15:00'
nml physics config_radtsw_interval '00:30:00'
SPEC
alone_rd="$root/build/inputs/run_radlwalone"
make_run_dir "$alone_rd" "$greece" cases/greece_60km/case.txt "$work/radlwalone.spec"
if mpas_run "$alone_rd" radlwalone; then
 echo 'FAIL radlwalone: accepted'; exit 1
fi
says radlwalone "$work/radlwalone.log" 'config_radtlw_interval = 900.000'
says radlwalone "$work/radlwalone.log" 'config_radtsw_interval = 1800.00'
no_output radlwalone "$alone_rd"
rm -rf "$alone_rd"
passed=$((passed + 1))

# greece_be: MPAS's big-endian Thompson tables in place of lean's own; 100 steps (the boundary update at 03:00), the
# history, diag and restart files equal those of a run with lean's tables
cat > "$work/greece_100.spec" <<'SPEC'
nml nhyd_model config_run_duration '0_03:20:00'
stream restart output_interval 3:20:00
SPEC
own="$root/build/inputs/run_greece_own"
be="$root/build/inputs/run_greece_be"
make_run_dir "$own" "$greece" cases/greece_60km/case.txt "$work/greece_100.spec"
make_run_dir "$be" "$greece" cases/greece_60km/case.txt "$work/greece_100.spec"
for table in "$root"/data/tables/*.DBL; do
  rm "$be/$(basename "$table")"
  "$py" tools/dbl_big_endian.py "$table" "$be/$(basename "$table")"
done
mpas_run "$own" greece_own || { tail -20 "$work/greece_own.log"; exit 1; }
mpas_run "$be" greece_be || { tail -20 "$work/greece_be.log"; exit 1; }
"$py" - "$root" "$own" "$be" <<'PY'
import sys
from pathlib import Path
sys.path.insert(0, sys.argv[1])
from tests.restart.compare_runs import compare_netcdf
own, be = Path(sys.argv[2]), Path(sys.argv[3])
names = sorted(p.name for p in own.glob('*.nc') if not p.is_symlink())
assert names == sorted(p.name for p in be.glob('*.nc') if not p.is_symlink()), 'greece_be: the file sets differ'
assert 'restart.2026-11-08_03.20.00.nc' in names, names
for name in names:
    compare_netcdf(own / name, be / name)
print(f'ok   greece_be: {len(names)} files equal those of lean\'s own tables')
PY
rm -rf "$own" "$be"
passed=$((passed + 1))

# missing_lbc_run: no 06 UTC boundary file; the stop comes at the boundary update of 03:00, before the products file exists
no06="$root/build/inputs/run_greece_no06"
make_run_dir "$no06" "$greece" cases/greece_60km/case.txt "$work/greece_100.spec"
rm "$no06/lbc.2026-11-08_06.00.00.nc"
if (export LEAN_PRODUCTS="$work/p.prod"; mpas_run "$no06" no06); then
  echo 'FAIL missing_lbc_run: exit 0'; exit 1
fi
says missing_lbc_run "$work/no06.log" 'ERROR: Failed to process LBC data at next time after 2026-11-08_03:00:00'
says missing_lbc_run "$work/no06.log" 'CRITICAL ERROR: lean_mpas stops: 3 input problem(s) listed above'
[ ! -e "$work/p.prod" ] || { echo "FAIL missing_lbc_run: $work/p.prod exists"; exit 1; }
rm -rf "$no06"
echo 'ok   missing_lbc_run: the boundary read error and the stop, no products file'
passed=$((passed + 1))

# missing_init_run
noinit="$root/build/inputs/run_greece_noinit"
make_run_dir "$noinit" "$greece" cases/greece_60km/case.txt
rm "$noinit/greece_60km.init.nc"
if mpas_run "$noinit" noinit; then
  echo 'FAIL missing_init_run: exit 0'; exit 1
fi
says missing_init_run "$work/noinit.log" "CRITICAL ERROR: Could not open input file 'greece_60km.init.nc' to read mesh fields"
no_output missing_init_run "$noinit"
rm -rf "$noinit"
echo 'ok   missing_init_run: the stop at the mesh read, no output'
passed=$((passed + 1))

# micro7: x1 at dt 600 s, 6 steps: Thompson takes 7 sub-steps
cat > "$work/micro7.spec" <<'SPEC'
nml nhyd_model config_dt 600.0
nml nhyd_model config_run_duration '0_01:00:00'
SPEC
micro_rd="$root/build/inputs/run_micro7"
make_run_dir "$micro_rd" "$x1" cases/x1_2562/case.txt "$work/micro7.spec"
mpas_run "$micro_rd" micro7 || { tail -20 "$work/micro7.log"; exit 1; }
says micro7 "$work/micro7.log" '--- n_microp  = 7'
echo 'ok   micro7: 6 steps of 600 s with 7 microphysics sub-steps'
rm -rf "$micro_rd"
passed=$((passed + 1))

rm -rf "$work"
rmdir "$root/build/inputs" 2>/dev/null || :
if [ "$skipped" -gt 0 ]; then
  echo "RUNS OK $passed, $skipped skipped:$skip_names"
else
  echo "RUNS OK $passed"
fi
