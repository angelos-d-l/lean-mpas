#!/bin/bash
# linux/cases/verdict.sh on hand-made logs and outputs: which runs are OK and which FAIL.
set -eu
R=$(cd "$(dirname "$0")/../.." && pwd)
. "$R/linux/cases/verdict.sh"
mkdir -p "$R/build"
T=$(mktemp -d "$R/build/linux-verdict.XXXXXX")
trap 'rm -rf "$T"' EXIT
cd "$T"
n=0
reset_files() {
  rm -rf log.* expected.nc model.out target.nc
  printf 'new output\n' > expected.nc
  printf 'finite velocity = 1.0; banana nanoseconds nan_count\n' > model.out
  printf '   Error \t messages =             0  \n  Critical error messages\t=\t  000 \n' > "log.$core.0000.out"
}
ok() {
  mpas_verdict "$core" "$1" model.out expected.nc || { echo "$2: unexpected FAIL: $VERDICT_REASON"; exit 1; }
  n=$((n + 1))
}
bad() {
  if mpas_verdict "$core" "$1" model.out expected.nc; then echo "$2: unexpected OK"; exit 1; fi
  [ -n "$VERDICT_REASON" ] || { echo "$2: no reason"; exit 1; }
  n=$((n + 1))
}
for core in atmosphere init_atmosphere; do
  reset_files; ok 0 "$core padded zeros"
  reset_files; bad 7 "$core nonzero status"
  for line in 'Error messages = 1' 'Critical error messages = 2' 'Error messages = 0.0' 'Error messages = 0 garbage'; do
    reset_files
    printf '%s\n' "$line" >> "log.$core.0000.out"
    bad 0 "$core invalid/nonzero/duplicate summary: $line"
  done
  reset_files; printf 'Critical error messages = 0\n' > "log.$core.0000.out"; bad 0 "$core missing errors"
  reset_files; printf 'Error messages = 0\n' > "log.$core.0000.out"; bad 0 "$core missing critical errors"
  reset_files; rm "log.$core.0000.out"; bad 0 "$core missing rank 0"
  reset_files; : > "log.$core.0003.err"; bad 0 "$core rank 3 error"
  reset_files; : > log.0003.abort; bad 0 "$core rank 3 abort"
  reset_files; printf 'velocity = -NaN\n' >> model.out; bad 0 "$core stdout NaN"
  reset_files; printf 'velocity = +nAn\n' > "log.$core.0003.out"; bad 0 "$core rank 3 NaN"
  reset_files; mv expected.nc target.nc; ln -s target.nc expected.nc; bad 0 "$core symlink output"
  reset_files; rm expected.nc; ln -s missing.nc expected.nc; bad 0 "$core dangling output"
  reset_files; : > expected.nc; bad 0 "$core empty output"
  reset_files; rm expected.nc; bad 0 "$core missing output"
  reset_files; rm expected.nc; mkdir expected.nc; bad 0 "$core directory output"
done
rmdir expected.nc
expect_absent expected.nc || { echo 'absent output rejected'; exit 1; }
n=$((n + 1))
ln -s missing.nc expected.nc
if expect_absent expected.nc; then echo 'dangling pre-launch output accepted'; exit 1; fi
n=$((n + 1))
echo "verdict tests: $n passed"
