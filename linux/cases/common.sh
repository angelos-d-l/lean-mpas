#!/bin/bash
# Sourced by init.sh, mpas_run.sh and lean_run.sh after verdict.sh; they set R (the repo root), KIND (INIT, MPAS or
# LEAN), LABEL (CASE/RUN) and CASE. One place for the folder layout, the run directory and the timed command, so that
# an MPAS run and a lean run of the same case get the same namelist, streams, inputs and tables on the same cores.
#
# LEAN_LAB (required) holds:
#   cases/CASE/inputs/         the case's init, static and boundary files; a spec's `inputs SUB` lays inputs/SUB/ over
#                              them
#   cases/CASE/work/STAGE/     init_atmosphere stages (init.sh); their outputs move to inputs/ after a clean verdict
#   cases/CASE/runs/RUN/       MPAS runs (mpas_run.sh)
#   cases/CASE/lean_runs/RUN/  lean runs (lean_run.sh)
#   cases/CASE/runs.log        one line per MPAS or lean run
#   cases/thompson_be/         MPAS's four MP_THOMPSON_*_DATA.DBL tables, big-endian, as cases/thompson_be.sha256 lists
#                              them
#   mpas/BUILD/                an MPAS 8.4.2 build tree, named by a spec's `build` line
# CASE_SPECS (default the repo's cases/) holds the specs, CASE_DEFAULTS (default reference/mpas_defaults/) MPAS's
# default namelists, streams and stream lists. CASE_TIME replaces /usr/bin/time (GNU time) and CASE_ENV replaces
# linux/lean_env.sh in the tests.
# The scripts also set OUT, the file that gets the model's terminal output and GNU time's report.
S=${CASE_SPECS:-$R/cases}
D=${CASE_DEFAULTS:-$R/reference/mpas_defaults}
reason=

# fail and refuse print "KIND FAIL LABEL: reason" and exit 1 (the run failed) or 2 (refused before anything ran).
fail() { reason="$*"; echo "$KIND FAIL $LABEL: $*"; exit 1; }
refuse() { reason="$*"; echo "$KIND FAIL $LABEL: $*"; exit 2; }

# A case, run or stage name is one folder name.
case_name() { [[ "$1" != . && "$1" != .. && "$1" =~ ^[A-Za-z0-9_.-]+$ ]]; }

# lab_dir: checks LEAN_LAB and sets C, the case's folder in it.
lab_dir() {
  [ -n "${LEAN_LAB:-}" ] && [ -d "$LEAN_LAB" ] || refuse 'set LEAN_LAB to the folder that holds cases/ and mpas/'
  C=$LEAN_LAB/cases/$CASE
}

# mpas_build: sets B, the MPAS build tree LEAN_LAB/mpas/BUILD that the spec names, and checks that it is there.
mpas_build() {
  case_name "$BUILD" || fail 'missing or invalid build in the spec'
  B=$LEAN_LAB/mpas/$BUILD
  [ -d "$B" ] || fail "no MPAS build: $B"
}

# mpas_build_env: sources B/env.sh, when there is one, before an MPAS program runs: the place for what that build needs
# at run time (MPIRUN, MPIRUN_ARGS, LD_LIBRARY_PATH; see linux/README.md). lean runs never read it.
mpas_build_env() {
  if [ -f "$B/env.sh" ]; then . "$B/env.sh" || fail "$B/env.sh failed"; fi
}

# The same stack for MPAS and lean: with the default 8 MB stack a gfortran build of MPAS 8.4.2 stops with SIGSEGV after
# a few steps of a 3 km case. Where unlimited is not allowed, the stack goes up to the hard limit.
raise_stack() { ulimit -s unlimited 2>/dev/null || ulimit -s hard 2>/dev/null || :; }

# case_run_dir DIR: makes DIR, a new run directory for the run RUN of CASE (the case's case.txt, then mpas_runs/RUN.txt,
# applied to MPAS's default namelist.atmosphere and streams.atmosphere), and enters it. It links the inputs, MPAS's
# physics tables from the MPAS build the spec names and the checked Thompson tables, and copies the stream lists.
# Sets the spec's BUILD, RANKS, CARD and EXPECT, B (the MPAS build), expected (EXPECT as an array), run_dir and made=1.
case_run_dir() {
  local dir=$1 specs=() summary f table tables
  mkdir -p "$(dirname "$dir")" || fail "cannot make $(dirname "$dir")"
  mkdir "$dir" 2>/dev/null || refuse "exists or cannot create: $dir"
  made=1; run_dir=$dir
  [ ! -f "$S/$CASE/case.txt" ] || specs+=("$S/$CASE/case.txt")
  specs+=("$S/$CASE/mpas_runs/$RUN.txt")
  summary=$("$LEAN_PY" "$R/tools/cases/run_spec.py" --out "$dir" "$D/namelist.atmosphere" "$D/streams.atmosphere" \
    "${specs[@]}") || fail 'run_spec failed'
  eval "$summary"
  case "$INPUTS" in /*|../*|*/../*|*/..|..) fail 'inputs must name a folder under inputs/' ;; esac
  IFS=' ' read -r -a expected <<< "$EXPECT"
  expect_basenames "${expected[@]}" || fail "$VERDICT_REASON"
  mpas_build
  cd "$dir" || fail "cannot enter $dir"
  link_inputs "$C/inputs" "$C/inputs/$INPUTS" "${expected[@]}" || fail "link inputs: $VERDICT_REASON"
  for f in CAM_ABS_DATA.DBL CAM_AEROPT_DATA.DBL CCN_ACTIVATE_DATA GENPARM.TBL LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL \
           OZONE_PLEV.TBL RRTMG_LW_DATA RRTMG_LW_DATA.DBL RRTMG_SW_DATA RRTMG_SW_DATA.DBL SOILPARM.TBL VEGPARM.TBL; do
    table=$B/src/core_atmosphere/physics/physics_wrf/files/$f
    [ -s "$table" ] && ln -sfn "$table" "$f" || fail "missing or unlinkable table: $table"
  done
  table=$B/src/core_atmosphere/physics/physics_noahmp/parameters/NoahmpTable.TBL
  [ -s "$table" ] && ln -sfn "$table" . || fail "missing or unlinkable table: $table"
  # The Thompson tables are checked against their sums, so that both models read the same four.
  (cd "$LEAN_LAB/cases/thompson_be" && sha256sum -c "$S/thompson_be.sha256") > tables.sha256.log 2>&1 \
    || fail 'Thompson table checksum failed (tables.sha256.log)'
  tables=("$LEAN_LAB"/cases/thompson_be/MP_THOMPSON_*.DBL)
  [ "${#tables[@]}" -eq 4 ] || fail 'expected four Thompson tables'
  for f in "${tables[@]}"; do ln -sfn "$f" . || fail "link $f"; done
  for f in output diagnostics surface diag_ugwp; do
    cp "$D/stream_list.atmosphere.$f" . || fail "copy stream list $f"
  done
  expect_absent "${expected[@]}" || fail "$VERDICT_REASON"
}

# timed_run CMD...: runs CMD under GNU time, pinned with taskset to CASE_CORES when it is set, its output and time's
# report in OUT. Sets wall (seconds) and returns CMD's status.
timed_run() {
  local start rc pin=()
  [ -z "${CASE_CORES:-}" ] || pin=(taskset -c "$CASE_CORES")
  start=$(date +%s)
  "${pin[@]}" "${CASE_TIME:-/usr/bin/time}" -v "$@" > "$OUT" 2>&1
  rc=$?
  wall=$(( $(date +%s) - start ))
  return "$rc"
}

# The card's used memory every 5 s while the model runs, into card_mib.log; card_peak is the largest reading. It is
# the whole card's reading, other processes on it included.
card_watch=
card_watch_start() {
  nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits --loop=5 > card_mib.log 2>&1 &
  card_watch=$!
}
card_watch_stop() {
  if [ -n "$card_watch" ]; then kill "$card_watch" 2>/dev/null || :; wait "$card_watch" 2>/dev/null || :; card_watch=; fi
}
card_peak() {
  [ -f card_mib.log ] || { echo unknown; return; }
  awk '/^[0-9]+$/ {if ($1 > m) m = $1; n++} END {if (n) print m + 0; else print "unknown"}' card_mib.log
}

# step_time LOG: "STEPS SECONDS_PER_STEP" from the 'time integration' row of the timer table that MPAS and lean_mpas
# both write at the end of their log: its calls are the steps, its total over the calls the seconds per step, start-up
# excluded.
step_time() {
  [ -f "$1" ] || { echo 'unknown unknown'; return; }
  awk '$2 == "time" && $3 == "integration" && $5 > 0 {printf "%d %.4f", $5, $4 / $5; found = 1}
       END {if (!found) printf "unknown unknown"}' "$1"
}

# record_run VERDICT RC: one line in cases/CASE/runs.log from the files of the run directory: what ran (build,
# executable md5, ranks or threads, cores) and what it took (wall time, steps, seconds per step, the largest resident
# memory of one process, the card's peak), the load at start and end, and the verdict's reason. Sets report, the
# same numbers for the terminal.
record_run() {
  local steps step_s rss card
  read -r steps step_s <<< "$(step_time "$run_dir/log.atmosphere.0000.out")"
  rss=$(max_rss_gb "$run_dir/$OUT")
  printf '%s %s %s %s rc=%s build=%s md5=%s %s cores=%s wall=%ss steps=%s s_per_step=%s maxRSS=%sGB card_peak=%sMiB | start: %s | end: %s | %s\n' \
    "$(date '+%F %T')" "$KIND" "$1" "$LABEL" "$2" "${build_name:-unknown}" "${md5:-unknown}" "${tasks:-unknown}" \
    "${CASE_CORES:-all}" "${wall:-unknown}" "$steps" "$step_s" "$rss" "${peak:-off}" \
    "${uptime_start:-not started}" "$(uptime)" "${reason:-}" >> "$C/runs.log" || echo "cannot append $C/runs.log" >&2
  recorded=1
  case "${peak:-off}" in
    off) card='card off' ;;
    *[!0-9]*) card="card peak $peak" ;;
    *) card="card peak $peak MiB" ;;
  esac
  report="wall ${wall:-unknown} s, $steps steps at $step_s s, maxRSS $rss GB, $card"
}
