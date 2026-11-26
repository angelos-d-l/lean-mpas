#!/bin/sh
# The stranger round's MPAS run directory:
#   sh tests/stranger/make_run_dir.sh CASE STAGE
# STAGE (new) gets run/, a complete MPAS-Atmosphere 8.4.2 run directory of CASE (for example greece_60km) for a 3 h forecast,
# and expected.json beside it, read-only: what the judge compares (judge.py). run/ holds MPAS's default namelist and
# streams with the case's settings (cases/CASE/case.txt) except the physics suite and the land model, which stay at
# MPAS's defaults ('mesoscale_reference', 'sf_noah'): the README tells the stranger to change them. Every stream list,
# the inputs (copied, not linked), and MPAS's tables, with the Thompson tables in big-endian as MPAS writes them.
# Needs data/cases/CASE/inputs and data/tables (./lean tables). Python standard library only.
set -eu
[ $# -eq 2 ] || { echo 'usage: make_run_dir.sh CASE STAGE' >&2; exit 2; }
case_name=$1
stage=$2
root=$(cd "$(dirname "$0")/../.." && pwd -P)
inputs=$root/data/cases/$case_name/inputs
[ -f "$root/cases/$case_name/case.txt" ] || { echo "make_run_dir.sh: no cases/$case_name/case.txt" >&2; exit 1; }
[ -d "$inputs" ] || { echo "make_run_dir.sh: no $inputs" >&2; exit 1; }
[ ! -e "$stage" ] || { echo "make_run_dir.sh: $stage exists; give a new folder" >&2; exit 1; }
mkdir -p "$stage/run"
stage=$(cd "$stage" && pwd -P)
run=$stage/run
spec=$stage/spec.txt
# the case without its suite and land model, and a 3 h run
grep -v -e 'config_physics_suite' -e 'config_lsm_scheme' "$root/cases/$case_name/case.txt" > "$spec"
echo "nml nhyd_model config_run_duration '0_03:00:00'" >> "$spec"
python3 "$root/tools/cases/run_spec.py" --out "$run" "$root/reference/mpas_defaults/namelist.atmosphere" \
  "$root/reference/mpas_defaults/streams.atmosphere" "$spec" > /dev/null
rm "$spec"
cp "$root"/reference/mpas_defaults/stream_list.atmosphere.* "$run/"
for file in "$inputs"/*.init.nc "$inputs"/lbc.*.nc "$inputs"/*.sfc_update.nc "$inputs"/*.graph.info; do
  [ -e "$file" ] && cp "$file" "$run/"
done
cp "$root"/reference/radiation/OZONE_*.TBL "$root"/reference/radiation/RRTMG_*_DATA "$run/"
for table in LANDUSE.TBL GENPARM.TBL SOILPARM.TBL VEGPARM.TBL NoahmpTable.TBL; do
  cp "$root/reference/config/$table" "$run/"
done
python3 "$root/tests/stranger/stage.py" tables "$root/data/tables" "$run"
python3 "$root/tests/stranger/stage.py" expected "$run" lean "$stage/expected.json"
echo "STAGE OK $stage (run/ and expected.json)"
