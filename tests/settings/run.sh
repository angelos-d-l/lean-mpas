#!/bin/sh
# The run settings: tests/settings/test_settings.py, then the clock units (tests/settings/clock.f90) on the strict cpu
# build's objects. The fractional-start runs are tests/settings/fractions.sh (they need the release case inputs).
#   sh tests/settings/run.sh          FC=<compiler> to choose it (default gfortran)
set -eu
cd "$(dirname "$0")/../.."
root=$PWD
python3 -m unittest tests.settings.test_settings
mkdir -p build/settings
./lean build --target cpu --fc "${FC:-gfortran}" --profile strict --jobs 2 --json > build/settings/build-result.json
b=$(python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(open(sys.argv[1]))["build_json"]).resolve().parent)' build/settings/build-result.json)
fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
# shellcheck disable=SC2086
"$fc" $flags $inc -o build/settings/clock tests/settings/clock.f90 $objects $libs
build/settings/clock
rm -rf build/settings
