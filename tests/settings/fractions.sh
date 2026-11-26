#!/bin/sh
# A fractional-second start time, in the namelist or in the init file's initial_time, stops the run with the value
# named and nothing written. Needs the release case inputs in data/cases (exit 77 without them).
set -eu
cd "$(dirname "$0")/../.."
root=$PWD
if [ ! -f data/cases/greece_60km/inputs/greece_60km.init.nc ]; then
  echo 'SKIP fractional starts: needs the release case inputs in data/cases (cases/README.md)'
  exit 77
fi
py=${LEAN_PY:-${PYTHON:-python3}}
result=$(./lean build --target cpu --fc gfortran --profile strict --jobs 2 --json)
b=$(printf '%s\n' "$result" | python3 -c 'import json,sys;from pathlib import Path;print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
work="$root/build/settings_fractions"
rm -rf "$work"
mkdir -p "$work"
. tests/inputs/run_dir.sh
failed=0
for kind in start initial; do
 rd="$work/$kind"
 make_run_dir "$rd" "$root/data/cases/greece_60km/inputs" cases/greece_60km/case.txt
 if [ "$kind" = start ]; then
  python3 - "$rd/namelist.atmosphere" <<'PY'
from pathlib import Path
import sys
p=Path(sys.argv[1]);p.write_text(p.read_text().replace("2026-11-08_00:00:00", "2026-11-08_00:00:00.5"))
PY
  expected="config_start_time = '2026-11-08_00:00:00.5': fractional seconds are not supported"
 else
  rm "$rd/greece_60km.init.nc"
  cp "$root/data/cases/greece_60km/inputs/greece_60km.init.nc" "$rd/greece_60km.init.nc"
  "$py" - "$rd/greece_60km.init.nc" <<'PY'
import netCDF4,sys
with netCDF4.Dataset(sys.argv[1], 'r+') as f:
 f['initial_time'][...] = netCDF4.stringtoarr('2026-11-08_00:00:00.5'.ljust(64),64)
PY
  expected="initial_time = '2026-11-08_00:00:00.5': fractional seconds are not supported"
 fi
 if (cd "$rd" && OMP_NUM_THREADS=1 "$b/lean_mpas" > "$work/$kind.log" 2>&1); then
  echo "FAIL fractional $kind: accepted"; failed=1
 elif ! cat "$rd"/log.atmosphere.0000.* "$work/$kind.log" 2>/dev/null | grep -qF "$expected"; then
  echo "FAIL fractional $kind: missing value diagnostic"; tail -5 "$work/$kind.log"; failed=1
 else
  echo "ok   fractional $kind: rejected with value"
 fi
 ! ls "$rd"/history.*.nc > /dev/null 2>&1 || { echo "FAIL fractional $kind: wrote history"; failed=1; }
done
[ "$failed" = 0 ] || exit 1
rm -rf "$work"
echo 'FRACTIONAL STARTS OK'
