#!/bin/bash
# Sourced by init.sh, mpas_run.sh and lean_run.sh: the checks that decide OK or FAIL. Paths are relative to the run's
# new work or run directory. Each check returns 1 with the reason in VERDICT_REASON.

# expect_absent FILE...: none of the files exists, not even as a dangling link (a run never overwrites an output).
expect_absent() {
  local f
  for f in "$@"; do
    if [ -e "$f" ] || [ -L "$f" ]; then VERDICT_REASON="exists: $f"; return 1; fi
  done
  return 0
}

# expect_regular FILE...: every file was written by the run: a regular, non-empty file, not a link.
expect_regular() {
  local f
  for f in "$@"; do
    if [ -L "$f" ] || [ ! -f "$f" ] || [ ! -s "$f" ]; then
      VERDICT_REASON="missing, empty or non-regular output: $f"; return 1
    fi
  done
  return 0
}

# expect_basenames FILE...: the expected outputs are plain file names in the run directory.
expect_basenames() {
  local f
  for f in "$@"; do
    case "$f" in ''|.|..|*/*) VERDICT_REASON="an expected output must be a file name: $f"; return 1 ;; esac
  done
}

# link_inputs TOP SELECTED EXPECTED...: links every file of TOP, then every file of SELECTED over them (a subfolder
# with a variant of some inputs), into the current directory; a file named like an expected output is never linked.
link_inputs() {
  local top=$1 selected=$2 layer f b expected skip
  shift 2
  for layer in "$top" "$selected"; do
    [ -d "$layer" ] || { VERDICT_REASON="no inputs directory: $layer"; return 1; }
    for f in "$layer"/*; do
      [ -d "$f" ] && continue
      [ -f "$f" ] || [ -L "$f" ] || continue
      b=${f##*/}; skip=0
      for expected in "$@"; do [ "$b" != "$expected" ] || skip=1; done
      [ "$skip" -eq 0 ] || continue
      ln -sfn "$f" "$b" || return 1
    done
  done
}

# nan_lines FILE...: the number of lines with NaN as a word (not inside words such as "nanoseconds").
nan_lines() {
  awk 'tolower($0) ~ /(^|[^[:alnum:]_])[-+]?nan([^[:alnum:]_]|$)/ {n++} END {print n+0}' "$@"
}

# max_rss_gb FILE: GNU time's "Maximum resident set size" in FILE, in GB; under mpirun that is the largest rank.
max_rss_gb() {
  [ -f "$1" ] || { printf unknown; return; }
  awk '/Maximum resident set size/ {rss=$NF; seen=1} END {if (seen) printf "%.2f",rss/1048576; else printf "unknown"}' "$1"
}

# mpas_verdict CORE RC OUTPUT EXPECTED...: OK when the command exited 0, the rank-0 log of CORE (atmosphere or
# init_atmosphere) ends with zero error and zero critical error messages, no rank left an .err or .abort log, no log and
# no OUTPUT line holds a NaN and every expected file was written. MPAS can exit 0 after an error, so the exit status
# alone is not enough. lean_mpas writes the same log and is judged the same way.
mpas_verdict() {
  local core=$1 rc=$2 output=$3 log f count
  local logs=()
  shift 3
  VERDICT_REASON=
  [ "$rc" -eq 0 ] || { VERDICT_REASON="command rc=$rc"; return 1; }
  case "$core" in atmosphere|init_atmosphere) ;; *) VERDICT_REASON="unknown core: $core"; return 1 ;; esac
  log=log.$core.0000.out
  [ -f "$log" ] || { VERDICT_REASON="missing $log"; return 1; }
  if ! awk '
    /^[[:space:]]*Error[[:space:]]+messages[[:space:]]*=/ {
      errors++
      if ($0 !~ /^[[:space:]]*Error[[:space:]]+messages[[:space:]]*=[[:space:]]*[0-9]+[[:space:]]*$/) bad=1
      split($0,a,"="); if (a[2]+0 != 0) bad=1
    }
    /^[[:space:]]*Critical[[:space:]]+error[[:space:]]+messages[[:space:]]*=/ {
      critical++
      if ($0 !~ /^[[:space:]]*Critical[[:space:]]+error[[:space:]]+messages[[:space:]]*=[[:space:]]*[0-9]+[[:space:]]*$/) bad=1
      split($0,a,"="); if (a[2]+0 != 0) bad=1
    }
    END {exit (bad || errors != 1 || critical != 1)}' "$log"; then
    VERDICT_REASON="missing, invalid or nonzero error summaries in $log"; return 1
  fi
  for f in log."$core".*.err log.*.abort; do
    if [ -e "$f" ] || [ -L "$f" ]; then VERDICT_REASON="error/abort log: $f"; return 1; fi
  done
  [ -f "$output" ] || { VERDICT_REASON="missing model output: $output"; return 1; }
  logs=("$output")
  for f in log.*; do [ ! -f "$f" ] || logs+=("$f"); done
  count=$(nan_lines "${logs[@]}") || { VERDICT_REASON='cannot read model logs'; return 1; }
  [ "$count" -eq 0 ] || { VERDICT_REASON="NaN lines=$count"; return 1; }
  expect_regular "$@" || return 1
  VERDICT_REASON=OK
}
