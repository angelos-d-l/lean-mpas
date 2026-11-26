#!/bin/bash
# mpas_run.sh CASE RUN: one forecast of MPAS 8.4.2's atmosphere_model for the run RUN of the case CASE, in the new run
# directory LEAN_LAB/cases/CASE/runs/RUN (layout and specs: linux/cases/common.sh and linux/README.md).
# The spec's `build` names the MPAS build under LEAN_LAB/mpas and `ranks` the number of MPI tasks; `card on` marks a
# build that runs on the NVIDIA card: its ranks then share the card through an MPS daemon that this script starts and
# stops, and the card's memory is sampled during the run.
# CASE_CORES (for example 0-7) pins the run with taskset, as lean_run.sh does with the same variable.
# Prints "MPAS OK|FAIL CASE/RUN: ..." with the wall time, the seconds per step, the largest resident memory of one rank
# and the card's peak, and appends the same to LEAN_LAB/cases/CASE/runs.log. Exit status 0 OK, 1 FAIL, 2 refused.
# CASE_MPS_PIDS and CASE_MPS_ALIVE replace the MPS process lookups in the tests.
set -u
export LC_ALL=C OMP_NUM_THREADS=1
R=$(cd "$(dirname "$0")/../.." && pwd)
. "${CASE_ENV:-$R/linux/lean_env.sh}" || exit 1
[ "$#" -eq 2 ] || { echo 'usage: mpas_run.sh CASE RUN'; exit 2; }
CASE=$1; RUN=$2; KIND=MPAS; LABEL=$CASE/$RUN; OUT=mpas.out
. "$R/linux/cases/verdict.sh"
. "$R/linux/cases/common.sh"
case_name "$CASE" && case_name "$RUN" || refuse 'invalid case or run name'
made=0; recorded=0; rc=; mps_starting=0; mps_owned=0; mps_pid=

# The PIDs of this user's MPS control daemons. The match is on the executable path: Linux's short process name
# truncates nvidia-cuda-mps-control.
mps_pids() {
  if [ -n "${CASE_MPS_PIDS:-}" ]; then
    "$CASE_MPS_PIDS"
  else
    pgrep -u "$(id -u)" -f '^([^[:space:]]*/)?nvidia-cuda-mps-control([[:space:]]|$)'
  fi
}

# The control daemon started by this run: the one that was not there before (mps_before). Other daemons of the same
# user are never claimed.
mps_new_pid() {
  local pid
  for pid in $(mps_pids); do
    case "$mps_before" in *" $pid "*) ;; *) echo "$pid"; return 0 ;; esac
  done
  return 1
}

start_mps() {
  local start_rc
  export CUDA_MPS_PIPE_DIRECTORY=$LEAN_LAB/mps/pipe CUDA_MPS_LOG_DIRECTORY=$LEAN_LAB/mps/log
  mkdir -p "$CUDA_MPS_PIPE_DIRECTORY" "$CUDA_MPS_LOG_DIRECTORY" || fail 'cannot make the MPS folders'
  # A daemon that already answers on this pipe belongs to another run, or was left by a killed one: never use or stop
  # it.
  if printf 'get_server_list\n' | nvidia-cuda-mps-control > /dev/null 2>&1; then
    fail "an MPS daemon already answers on $CUDA_MPS_PIPE_DIRECTORY"
  fi
  mps_before=" $(mps_pids | tr '\n' ' ') "
  mps_starting=1
  nvidia-cuda-mps-control -d > mps.out 2>&1
  start_rc=$?
  mps_pid=$(mps_new_pid) || mps_pid=
  # A failed start may still have left a daemon behind: it is ours when a new control process appeared.
  if [ "$start_rc" -eq 0 ] || [ -n "$mps_pid" ]; then mps_owned=1; fi
  mps_starting=0
  [ "$start_rc" -eq 0 ] || fail 'MPS start failed (mps.out)'
  [ -n "$mps_pid" ] || fail 'MPS control PID not found'
}

# Asks the daemon to quit and waits up to 120 s for its control process to exit.
stop_mps() {
  local waited=0
  printf 'quit\n' | nvidia-cuda-mps-control || echo 'MPS quit failed; waiting for the control process to exit' >&2
  [ -n "$mps_pid" ] || { echo 'MPS control PID unknown: the daemon may still run' >&2; return 1; }
  while "${CASE_MPS_ALIVE:-kill}" -0 "$mps_pid" 2>/dev/null; do
    if [ "$waited" -ge 120 ]; then echo "MPS control PID $mps_pid still alive after 120 s" >&2; return 1; fi
    sleep 1
    waited=$((waited + 1))
  done
  mps_owned=0
}

# On every exit, a failure or an interrupt included: stop the card sampler and the MPS daemon this run started (a signal
# during the daemon's start leaves it running unclaimed otherwise), then record the run once.
finish() {
  local status=$?
  card_watch_stop
  if [ "$mps_starting" -eq 1 ] && [ "$mps_owned" -eq 0 ]; then mps_pid=$(mps_new_pid) && mps_owned=1; fi
  [ "$mps_owned" -eq 0 ] || stop_mps || :
  if [ "$made" -eq 1 ] && [ "$recorded" -eq 0 ]; then
    [ -n "$reason" ] || { reason="stopped, exit $status"; echo "MPAS FAIL $LABEL: $reason"; }
    record_run FAIL "${rc:-$status}"
  fi
}
trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

lab_dir
case_run_dir "$C/runs/$RUN"
build_name=$BUILD; tasks="ranks=$RANKS"
[ -x "$B/atmosphere_model" ] || fail "no executable: $B/atmosphere_model"
ln -s "$B/atmosphere_model" . || fail 'link atmosphere_model'
md5=$(md5sum "$B/atmosphere_model") || fail 'cannot hash the executable'
md5=${md5%% *}
mpas_build_env
raise_stack
read -r -a mpirun_args <<< "${MPIRUN_ARGS:-}"
peak=off
if [ "$CARD" = on ]; then
  start_mps
  # Open MPI's way to hand the two MPS variables to every rank (a card build of MPAS runs with the NVIDIA HPC SDK's
  # Open MPI).
  mpirun_args+=(-x CUDA_MPS_PIPE_DIRECTORY -x CUDA_MPS_LOG_DIRECTORY)
  card_watch_start
fi
uptime_start=$(uptime)
timed_run "${MPIRUN:-mpirun}" -np "$RANKS" "${mpirun_args[@]}" ./atmosphere_model
rc=$?
card_watch_stop
[ "$CARD" = on ] && peak=$(card_peak)
if [ "$mps_owned" -eq 1 ] && ! stop_mps; then rc=1; reason='MPS shutdown failed'; fi
verdict=FAIL
if [ -z "$reason" ] && mpas_verdict atmosphere "$rc" "$OUT" "${expected[@]}"; then verdict=OK; fi
reason=${reason:-$VERDICT_REASON}
record_run "$verdict" "$rc"
echo "MPAS $verdict $LABEL: $reason, rc=$rc, $report"
[ "$verdict" = OK ] || exit 1
