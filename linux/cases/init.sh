#!/bin/bash
# init.sh CASE STAGE [MET_DIR]: one stage of MPAS 8.4.2's init_atmosphere_model for the case CASE (the specs
# cases/CASE/init/base.txt and STAGE.txt on MPAS's default namelist.init_atmosphere and streams.init_atmosphere), in the
# new work directory LEAN_LAB/cases/CASE/work/STAGE. The spec's `build` names the MPAS build under LEAN_LAB/mpas,
# `ranks` the MPI tasks, `inputs` the subfolder of inputs/ that gets the outputs and `expect` the files the stage must
# write. MET_DIR holds ungribbed first-guess files GFS:YYYY-MM-DD_HH (or GFS:YYYY-MM-DD_HH:00).
# Only after a clean verdict do the expected files move to LEAN_LAB/cases/CASE/inputs/[SUBFOLDER]; a file already there
# is never replaced. CASE_CORES pins the stage with taskset. Prints "INIT OK|FAIL CASE/STAGE: ...".
# Exit status 0 OK, 1 FAIL, 2 refused.
set -u
export LC_ALL=C OMP_NUM_THREADS=1
R=$(cd "$(dirname "$0")/../.." && pwd)
. "${CASE_ENV:-$R/linux/lean_env.sh}" || exit 1
[ "$#" -ge 2 ] && [ "$#" -le 3 ] || { echo 'usage: init.sh CASE STAGE [MET_DIR]'; exit 2; }
CASE=$1; STAGE=$2; KIND=INIT; LABEL=$CASE/$STAGE; OUT=init.out
. "$R/linux/cases/verdict.sh"
. "$R/linux/cases/common.sh"
case_name "$CASE" && case_name "$STAGE" || refuse 'invalid case or stage name'
lab_dir
W=$C/work/$STAGE
MET=
if [ "$#" -eq 3 ]; then MET=$(cd "$3" && pwd) || fail "no MET_DIR: $3"; fi
mkdir -p "$C/work" "$C/logs" || fail 'cannot make the case folders'
mkdir "$W" 2>/dev/null || refuse "exists or cannot create: $W"
summary=$("$LEAN_PY" "$R/tools/cases/run_spec.py" --out "$W" "$D/namelist.init_atmosphere" \
  "$D/streams.init_atmosphere" "$S/$CASE/init/base.txt" "$S/$CASE/init/$STAGE.txt") || fail 'run_spec failed'
eval "$summary"
case "$INPUTS" in /*|../*|*/../*|*/..|..) fail 'inputs must name a folder under inputs/' ;; esac
IFS=' ' read -r -a expected <<< "$EXPECT"
expect_basenames "${expected[@]}" || fail "$VERDICT_REASON"
DEST=$C/inputs/$INPUTS
mkdir -p "$DEST" || fail "cannot make $DEST"
destination_absent() {
  local f
  for f in "${expected[@]}"; do expect_absent "$DEST/$f" || return 1; done
}
destination_absent || refuse "$VERDICT_REASON"
cd "$W" || fail "cannot enter $W"
link_inputs "$C/inputs" "$DEST" "${expected[@]}" || fail "link inputs: $VERDICT_REASON"
if [ -n "$MET" ]; then
  # init_atmosphere opens GFS:YYYY-MM-DD_HH: a file named GFS:YYYY-MM-DD_HH:00 is linked under that name.
  for f in "$MET"/GFS:*; do
    [ -f "$f" ] || continue
    b=${f##*/}; b=${b%:00}
    ln -sfn "$f" "$b" || fail "link $b"
  done
fi
mpas_build
[ -x "$B/init_atmosphere_model" ] || fail "no executable: $B/init_atmosphere_model"
ln -s "$B/init_atmosphere_model" . || fail 'link init_atmosphere_model'
mpas_build_env
raise_stack
read -r -a mpirun_args <<< "${MPIRUN_ARGS:-}"
expect_absent "${expected[@]}" || refuse "$VERDICT_REASON"
timed_run "${MPIRUN:-mpirun}" -np "$RANKS" "${mpirun_args[@]}" ./init_atmosphere_model
rc=$?
if [ -f log.init_atmosphere.0000.out ]; then
  cp log.init_atmosphere.0000.out "$C/logs/init_$STAGE.log" || fail 'cannot save the rank-0 log'
fi
mpas_verdict init_atmosphere "$rc" "$OUT" "${expected[@]}" || fail "$VERDICT_REASON"
destination_absent || refuse "$VERDICT_REASON"
for f in "${expected[@]}"; do mv "$f" "$DEST/$f" || fail "cannot publish $f"; done
echo "INIT OK $LABEL: $EXPECT, wall ${wall}s, maxRSS $(max_rss_gb "$OUT") GB"
