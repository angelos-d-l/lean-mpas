#!/bin/bash
# lean_run.sh CASE RUN LEAN_BUILD: one lean_mpas forecast of the same run as `mpas_run.sh CASE RUN`, in the new run
# directory LEAN_LAB/cases/CASE/lean_runs/RUN: the same namelist, streams, inputs and tables (those of the MPAS build
# the spec names), the same stack limit and the same cores (CASE_CORES), with lean_mpas linked as atmosphere_model.
# LEAN_BUILD is a lean build folder (lean_mpas and build.json). LEAN_THREADS (default 1) is OMP_NUM_THREADS of an
# --omp build. A card build runs on the card as LEAN_CARD says (unset: everything on the card), and the card's memory
# is sampled during the run. The spec's `ranks` and `card` are MPAS's and do not apply.
# The optional outputs (LEAN_FILM, LEAN_PRODUCTS, LEAN_STORM, LEAN_SURFACE) are off, so that lean writes what MPAS
# writes.
# Prints "LEAN OK|FAIL CASE/RUN: ..." with the wall time, the seconds per step, the resident memory and the card's
# peak, and appends the same to LEAN_LAB/cases/CASE/runs.log. Exit status 0 OK, 1 FAIL, 2 refused.
set -u
export LC_ALL=C OMP_NUM_THREADS=${LEAN_THREADS:-1} OMP_STACKSIZE=256M
unset LEAN_FILM LEAN_PRODUCTS LEAN_STORM LEAN_SURFACE
R=$(cd "$(dirname "$0")/../.." && pwd)
. "${CASE_ENV:-$R/linux/lean_env.sh}" || exit 1
[ "$#" -eq 3 ] || { echo 'usage: lean_run.sh CASE RUN LEAN_BUILD'; exit 2; }
CASE=$1; RUN=$2; KIND=LEAN; LABEL=$CASE/$RUN; OUT=lean.out
. "$R/linux/cases/verdict.sh"
. "$R/linux/cases/common.sh"
case_name "$CASE" && case_name "$RUN" || refuse 'invalid case or run name'
made=0; recorded=0; rc=
L=$(cd "$3" 2>/dev/null && pwd) || refuse "no lean build folder: $3"
[ -x "$L/lean_mpas" ] && [ -f "$L/build.json" ] || refuse "a lean build folder holds lean_mpas and build.json: $L"
target=$("$LEAN_PY" -c 'import json,sys; print(json.load(open(sys.argv[1]))["config"]["target"])' "$L/build.json") \
  || refuse "cannot read the target of $L/build.json"
case "$target" in cpu|gpu) ;; *) refuse "unknown target in $L/build.json: $target" ;; esac

# On every exit, a failure or an interrupt included: stop the card sampler and record the run once.
finish() {
  local status=$?
  card_watch_stop
  if [ "$made" -eq 1 ] && [ "$recorded" -eq 0 ]; then
    [ -n "$reason" ] || { reason="stopped, exit $status"; echo "LEAN FAIL $LABEL: $reason"; }
    record_run FAIL "${rc:-$status}"
  fi
}
trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

lab_dir
case_run_dir "$C/lean_runs/$RUN"
build_name=$(basename "$L"); tasks="threads=$OMP_NUM_THREADS"
[ "$target" = cpu ] || tasks="$tasks card=${LEAN_CARD:-all}"
ln -s "$L/lean_mpas" atmosphere_model || fail 'link lean_mpas as atmosphere_model'
md5=$(md5sum "$L/lean_mpas") || fail 'cannot hash the executable'
md5=${md5%% *}
raise_stack
peak=off
[ "$target" = cpu ] || card_watch_start
uptime_start=$(uptime)
timed_run ./atmosphere_model
rc=$?
card_watch_stop
[ "$target" = cpu ] || peak=$(card_peak)
verdict=FAIL
if mpas_verdict atmosphere "$rc" "$OUT" "${expected[@]}"; then verdict=OK; fi
reason=$VERDICT_REASON
record_run "$verdict" "$rc"
echo "LEAN $verdict $LABEL: $reason, rc=$rc, $report"
[ "$verdict" = OK ] || exit 1
