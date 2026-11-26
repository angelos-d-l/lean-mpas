#!/bin/sh
# Every test suite of lean_mpas, in three tiers:
#
#   sh tests/run_all.sh quick|build|cases [FC]
#
# Prints one line per suite: ok (with the number of parts it skipped, if any), SKIP <why>, or FAIL <where the log is>;
# then a total line, TESTS OK or TESTS FAIL. Exit 0 when no suite failed, 1 otherwise. The log of each suite is in
# build/tests/<suite>.log. The builds use 2 jobs.
#
# FC is gfortran (default) or nvfortran. It applies to the suites that take it (namelist, streams, run_shell, netcdf,
# writer, restart, options, surface, settings, inputs, diag, memory); sea_ice, land_ice, mynn_moisture, gf_switches,
# rad_blcloud, sw_split, card_units and fractional_starts always use gfortran, so gfortran is needed in every case.
# OMP_NUM_THREADS defaults to 2; the suites that run forecasts set their own (1 for most, 2 for diag).
#
# The tiers add up: build runs the quick suites too, cases runs all three.
#
#   quick   About 2 minutes. Python 3.9 or newer (lean.lock.json tools_min); no model build. Set LEAN_PY (or PYTHON)
#           to a Python with netCDF4 and numpy: the converter and NetCDF-comparison suites SKIP without them. Some CLI
#           tests also configure with CMake and Ninja or compile a small program with gfortran, and skip those parts
#           without them.
#   build   About 30 minutes. What ./lean doctor asks for (gfortran, CMake, Ninja, netCDF-C with CDF-5 and its ncdump and
#           ncgen), the Thompson tables in data/tables (./lean tables) and MPAS 8.4.2's source in reference/mpas_src
#           (./lean fetch-upstream), from which the oracles are compiled. The suites build lean_mpas with ./lean build and
#           compile their programs against its objects; parts that need case data skip.
#   cases   About 2 hours. The release case inputs in data/cases (cases/README.md), and for the writer runs xarray in
#           LEAN_PY. The suites run real forecasts of greece_60km and x1_2562. Their comparisons with MPAS's own output
#           files and logs need the MPAS reference outputs (data/cases/*/reference), which are not released: those
#           parts SKIP and are counted.
#
# A suite that cannot run at all here exits 77 and prints why on its last SKIP line; run_all.sh reports it as SKIP.
# Not run here: tests/diag/mutations.py and convective_mutations.py (mutation checks of the diagnostics, after a strict
# CPU build), and the stranger round (tests/stranger/container.sh, mac.sh), which needs a tester; only its judge's
# self-test runs here.
set -u
tier=${1:-}
case $tier in
  quick|build|cases) ;;
  *) echo 'usage: sh tests/run_all.sh quick|build|cases [FC]' >&2; exit 2 ;;
esac
FC=${2:-gfortran}
JOBS=2
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"
PY=${LEAN_PY:-${PYTHON:-python3}}
export LEAN_PY="$PY" PYTHON="$PY" FC
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
logs="$root/build/tests"
mkdir -p "$logs"
passed=0
skipped=0
failed=0

# suite NAME COMMAND...: runs the command from the repository root and prints its line.
suite() {
  name=$1; shift
  log="$logs/$name.log"
  "$@" > "$log" 2>&1
  code=$?
  if [ "$code" -eq 0 ]; then
    parts=$(grep -cE '^(SKIP|skip) ' "$log")
    tests=$(sed -nE 's/.*OK \(skipped=([0-9]+)\).*/\1/p' "$log" | awk '{n += $1} END {print n + 0}')
    parts=$((parts + tests))
    if [ "$parts" -gt 0 ]; then echo "ok    $name ($parts parts skipped)"; else echo "ok    $name"; fi
    passed=$((passed + 1))
  elif [ "$code" -eq 77 ]; then
    why=$(grep -E '^SKIP' "$log" | tail -1 | sed -E 's/^SKIP [^:]*: //')
    echo "SKIP  $name: ${why:-see $log}"
    skipped=$((skipped + 1))
  else
    echo "FAIL  $name (exit $code; log build/tests/$name.log)"
    failed=$((failed + 1))
  fi
}

# skip NAME WHY: a suite that cannot run here.
skip() {
  echo "SKIP  $1: $2"
  skipped=$((skipped + 1))
}

if "$PY" -c 'import netCDF4, numpy' > /dev/null 2>&1; then netcdf4=yes; else netcdf4=; fi
no_netcdf4="needs netCDF4 and numpy in $PY (set LEAN_PY)"

# ---- quick: no model build ----
suite cli "$PY" -m unittest discover -s tests/lean_cli -t .
suite run_spec "$PY" -m unittest tests.test_run_spec
suite speed_table "$PY" -m unittest tests.speed_table.test_speed_table
suite writer_cdl "$PY" -m unittest tests.writer.test_compare_cdl tests.writer.test_source_guards
if [ -n "$netcdf4" ]; then
  suite converters "$PY" -m unittest tests.test_storm_output tests.test_surface_output tests.test_compare_runs
  suite netcdf_compare "$PY" -m unittest tests.restart.test_compare tests.writer.test_t0_values
else
  skip converters "$no_netcdf4"
  skip netcdf_compare "$no_netcdf4"
fi
suite sizes sh -c "cd tests/sizes && '$PY' -m unittest test_gen test_runtime test_storage"
suite sizes_audit "$PY" tests/sizes/audit.py --expect type=0,module=0,local=0,ice=0,routine=0
suite levels sh -c "cd tests/levels && '$PY' -m unittest test_audit test_capacity"
suite levels_audit "$PY" tests/levels/audit.py --expect param=0,fixed=0,cardlocal=0,dynprivate=0
suite stranger_judge "$PY" tests/stranger/judge.py --self-test
for test in tests/linux_cases/test_*.sh; do
  [ -f "$test" ] && suite "linux_cases_$(basename "$test" .sh | sed 's/^test_//')" bash "$test"
done

# ---- build: a strict CPU build, the oracles from reference/mpas_src ----
if [ "$tier" != quick ]; then
  missing=
  tools="$FC cmake ninja"; [ "$FC" = gfortran ] || tools="$FC gfortran cmake ninja"
  for tool in $tools; do command -v "$tool" > /dev/null 2>&1 || missing="$missing $tool"; done
  build_suites='namelist streams run_shell netcdf writer_units restart_units options_units surface_units settings
    card_units sea_ice land_ice mynn_moisture gf_switches rad_blcloud sw_split'
  if [ -n "$missing" ]; then
    for name in $build_suites; do skip "$name" "needs$missing (./lean doctor)"; done
  else
    suite namelist "$PY" tests/namelist/run.py --fc "$FC"
    suite streams "$PY" tests/streams/run.py --fc "$FC"
    suite run_shell "$PY" tests/run_shell/run.py --fc "$FC" --jobs "$JOBS"
    suite netcdf sh tests/netcdf/run.sh "$FC" "$JOBS"
    suite writer_units sh tests/writer/run.sh "$FC" "$JOBS"
    suite restart_units sh tests/restart/run.sh "$FC" "$JOBS"
    suite options_units sh tests/options/run.sh "$FC" "$JOBS"
    suite surface_units sh tests/surface/run.sh "$FC" "$JOBS"
    suite settings sh tests/settings/run.sh
    suite card_units sh tests/sizes/card_units.sh
    suite sea_ice sh tests/sea_ice/run.sh
    suite land_ice sh tests/land_ice/run.sh
    suite mynn_moisture sh tests/mynn_moisture/run.sh
    suite gf_switches sh tests/suite_coupling/gf_switches.sh
    suite rad_blcloud sh tests/suite_coupling/rad_blcloud.sh
    suite sw_split sh tests/suite_coupling/sw_split.sh
  fi
fi

# ---- cases: real forecasts of the release cases ----
if [ "$tier" = cases ]; then
  case_suites='inputs inputs_runs diag memory fractional_starts restart_runs writer_runs surface_runs options_runs'
  if [ -n "$missing" ]; then
    for name in $case_suites; do skip "$name" "needs$missing (./lean doctor)"; done
  elif [ -z "$netcdf4" ]; then
    for name in $case_suites; do skip "$name" "$no_netcdf4"; done
  else
    suite inputs sh tests/inputs/run.sh "$FC" "$JOBS"
    suite inputs_runs sh tests/inputs/runs.sh "$FC" "$JOBS"
    suite diag sh tests/diag/run.sh "$FC" "$JOBS"
    suite memory sh tests/memory/run.sh "$FC" "$JOBS"
    suite fractional_starts sh tests/settings/fractions.sh
    suite restart_runs sh tests/restart/runs.sh "$FC" "$JOBS"
    suite writer_runs sh tests/writer/runs.sh "$FC" "$JOBS"
    suite surface_runs sh tests/surface/runs.sh "$FC" "$JOBS"
    suite options_runs sh tests/options/runs.sh "$FC" "$JOBS"
  fi
fi

total=$((passed + skipped + failed))
if [ "$failed" -eq 0 ]; then
  echo "TESTS OK $tier: $passed of $total suites ok, $skipped skipped"
  exit 0
fi
echo "TESTS FAIL $tier: $failed of $total suites failed, $passed ok, $skipped skipped"
exit 1
