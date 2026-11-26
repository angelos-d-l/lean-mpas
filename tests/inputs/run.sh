#!/bin/sh
# Input reading in Fortran: the stops and warnings of lean_inputs and lean_lbc on doctored and real
# files, the physics tables, the run directory's inputs and the longwave start check.
#   sh tests/inputs/run.sh [FC] [JOBS]          LEAN_PY overrides the Python with netCDF4 and numpy
# One plain build with ./lean (flavor mpas); the unit programs link its objects.
# Needs the release case inputs in data/cases (exit 77 without them). Writes build/inputs/; a passing run removes it,
# a failure exits 1 and keeps it.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
FC=${1:-gfortran}
JOBS=${2:-2}
export FC JOBS
py=${LEAN_PY:-${PYTHON:-python3}}
trap 'exit 1' HUP INT TERM
trap 'code=$?; if [ "$code" -eq 77 ]; then exit 77; elif [ "$code" -ne 0 ]; then echo "INPUTS TESTS FAILED (build/inputs kept)"; exit 1; fi' 0
greece="$root/data/cases/greece_60km/inputs"
x1="$root/data/cases/x1_2562/inputs"

# 1. the fixtures
python3 - "$root" <<'PY'
import hashlib
import sys
from pathlib import Path

root = Path(sys.argv[1])
wanted = []
for case in ('greece_60km', 'x1_2562'):
    for line in (root / 'cases' / case / 'SHA256SUMS').read_text().splitlines():
        digest, name = line.split()
        if name.startswith('inputs/'):
            wanted.append((digest, root / 'data' / 'cases' / case / name))
missing = [path for _, path in wanted if not path.is_file()]
if missing:
    print(f'SKIP inputs: needs the release case inputs in data/cases (cases/README.md); {missing[0]} is missing')
    sys.exit(77)
for digest, path in wanted:
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b''):
            h.update(chunk)
    if h.hexdigest() != digest:
        sys.exit(f'checksum mismatch for {path}')
print(f'ok   sha256 of {len(wanted)} fixtures')
PY

rm -rf "$root/build/inputs"
mkdir -p "$root/build/inputs/doctored"

# 2. the doctored copies
d="$root/build/inputs/doctored"
doctor() { "$py" tests/inputs/doctor.py "$1" "$2" "$d/${3:-$1}.nc"; }
doctor sphere_radius "$greece/greece_60km.init.nc" unit_sphere
for case in levels_over no_sphere_attributes planar slot9 empty_slot_neighbour index_order \
            invariant state double two_records no_xtime missing_field missing_diag ice_init bad_xtime initial_0630 initial_2400 initial_roll \
            nan_theta isice16 usgs mminlu_nul black_land3 density16 density_nan land_ice2 sea_ice1; do
  doctor "$case" "$greece/greece_60km.init.nc"
done
doctor wrong_vertices "$d/state.nc" state_vertices
# fixed_by_tools: the two mesh tools on the faulty copies; the init step's sfc_albbck again from the corrected albedo12m
cp "$d/black_land3.nc" "$d/black_land3_fixed.nc"
"$py" tools/mesh/fix_albedo.py "$d/black_land3_fixed.nc"
doctor init_albedo "$d/black_land3_fixed.nc" albedo_fixed
cp "$d/density16.nc" "$d/density_fixed.nc"
"$py" tools/mesh/set_mesh_density.py 60000 "$d/density_fixed.nc"
# the boundary cases: five records in one file, ice scalars, a NaN, and the set without its 06 UTC file
doctor multi "$greece/lbc.2026-11-08_00.00.00.nc" lbc_multi
doctor lbc_ice "$greece/lbc.2026-11-08_00.00.00.nc"
doctor wrong_vertices "$greece/lbc.2026-11-08_00.00.00.nc" lbc_vertices
doctor lbc_nan "$greece/lbc.2026-11-08_00.00.00.nc"
doctor lbc_no_w_qc_qr "$greece/lbc.2026-11-08_00.00.00.nc"
mkdir -p "$d/hole"
for hour in 00 03 09 12; do ln -s "$greece/lbc.2026-11-08_$hour.00.00.nc" "$d/hole/"; done
# the physics tables: MPAS's big-endian form of lean's four Thompson tables, a zeroed first marker, mixed byte orders,
# an empty directory, and directories with the eleven file names lean reads
tables="$root/data/tables"
thompson='MP_THOMPSON_QIautQS_DATA.DBL MP_THOMPSON_QRacrQG_DATA.DBL MP_THOMPSON_QRacrQS_DATA.DBL MP_THOMPSON_freezeH2O_DATA.DBL'
mkdir -p "$d/tables_be" "$d/tables_zero" "$d/tables_mixed" "$d/tables_empty" "$d/tables_all" "$d/tables_no_noahmp"
# the directories of the scheme sets (tables only for the schemes that run)
mkdir -p "$d/tables_no_lw" "$d/tables_no_sw" "$d/tables_no_rad" "$d/tables_no_thompson"
for name in $thompson; do
  "$py" tools/dbl_big_endian.py "$tables/$name" "$d/tables_be/$name"
  ln -s "$tables/$name" "$d/tables_zero/$name"
  ln -s "$tables/$name" "$d/tables_mixed/$name"
  : > "$d/tables_all/$name"
  : > "$d/tables_no_noahmp/$name"
  : > "$d/tables_no_lw/$name"
  : > "$d/tables_no_sw/$name"
  : > "$d/tables_no_rad/$name"
done
rm "$d/tables_zero/MP_THOMPSON_QIautQS_DATA.DBL" "$d/tables_mixed/MP_THOMPSON_QRacrQS_DATA.DBL"
cp "$tables/MP_THOMPSON_QIautQS_DATA.DBL" "$d/tables_zero/"
printf '\0\0\0\0' | dd of="$d/tables_zero/MP_THOMPSON_QIautQS_DATA.DBL" bs=1 count=4 conv=notrunc 2>/dev/null
ln -s "$d/tables_be/MP_THOMPSON_QRacrQS_DATA.DBL" "$d/tables_mixed/"
for name in LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL OZONE_PLEV.TBL RRTMG_LW_DATA RRTMG_SW_DATA; do
  : > "$d/tables_all/$name"
  : > "$d/tables_no_noahmp/$name"
done
: > "$d/tables_all/NoahmpTable.TBL"
for name in LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL OZONE_PLEV.TBL RRTMG_SW_DATA NoahmpTable.TBL; do : > "$d/tables_no_lw/$name"; done
for name in LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL OZONE_PLEV.TBL RRTMG_LW_DATA NoahmpTable.TBL; do : > "$d/tables_no_sw/$name"; done
for name in LANDUSE.TBL NoahmpTable.TBL; do : > "$d/tables_no_rad/$name"; done
for name in LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL OZONE_PLEV.TBL RRTMG_LW_DATA RRTMG_SW_DATA NoahmpTable.TBL; do
  : > "$d/tables_no_thompson/$name"
done

# the run directory of lean_run_inputs' cases (run_dir.sh), and each case's namelist and streams file beside
# the default pair
rd="$root/build/inputs/run_greece"
defaults="$root/reference/mpas_defaults"
. "$root/tests/inputs/run_dir.sh"
mkdir -p "$rd/spec"
variant() {   # $1: the case; standard input: its statements after cases/greece_60km/case.txt
  cat > "$rd/spec/$1.txt"
  python3 tools/cases/run_spec.py --out "$rd/spec" "$defaults/namelist.atmosphere" "$defaults/streams.atmosphere" \
    cases/greece_60km/case.txt "$rd/spec/$1.txt" > /dev/null
  mv "$rd/spec/namelist.atmosphere" "$rd/namelist.$1"
  mv "$rd/spec/streams.atmosphere" "$rd/streams.$1"
}
make_run_dir "$rd" "$greece" cases/greece_60km/case.txt
ln -s "$greece/v0630" "$rd/"
ln -s "$d/invariant.nc" "$rd/greece_60km.invariant.nc"
ln -s "$d/state.nc" "$rd/greece_60km.state.nc"
ln -s "$d/initial_0630.nc" "$d/initial_2400.nc" "$d/initial_roll.nc" "$rd/"
invariant_stream='<immutable_stream name="invariant" type="input" filename_template="greece_60km.invariant.nc" input_interval="initial_only"/>'
insert_invariant() {   # $1: the case whose streams file gets the invariant stream as its first element
  python3 - "$rd/streams.$1" "$invariant_stream" <<'PY'
import sys
from pathlib import Path
path = Path(sys.argv[1])
text = path.read_text()
assert text.startswith('<streams>\n')
path.write_text('<streams>\n' + sys.argv[2] + '\n' + text[len('<streams>\n'):])
PY
}
variant invariant <<'SPEC'
stream input filename_template greece_60km.state.nc
SPEC
insert_invariant invariant
variant late_reference <<'SPEC'
stream lbc_in reference_time 2026-11-08_03:00:00
SPEC
# final_only with the default filename_interval="input_interval" is stopped by MPAS's parser: the
# two cases that must reach the limited-area check give the stream no filename interval
variant final_only_lbc <<'SPEC'
stream lbc_in input_interval final_only
stream lbc_in filename_interval none
SPEC
variant final_only_record <<'SPEC'
stream lbc_in input_interval final_only
stream lbc_in filename_interval none
stream lbc_in record_interval 3:00:00
SPEC
variant half_hour_start <<'SPEC'
nml nhyd_model config_start_time '2026-11-08_06:30:00'
stream input filename_template v0630/greece_60km.init.nc
stream lbc_in filename_template v0630/lbc.$Y-$M-$D_$h.$m.$s.nc
SPEC
variant initial_0630 <<'SPEC'
nml nhyd_model config_start_time '2026-11-08_06:30:00'
stream input filename_template initial_0630.nc
stream lbc_in filename_template v0630/lbc.$Y-$M-$D_$h.$m.$s.nc
SPEC
variant initial_2400 <<'SPEC'
stream input filename_template initial_2400.nc
SPEC
variant initial_roll <<'SPEC'
stream input filename_template initial_roll.nc
SPEC
variant no_lbcs <<'SPEC'
nml limited_area config_apply_lbcs false
SPEC
variant radt_none <<'SPEC'
nml physics config_radtlw_interval 'none'
nml physics config_radtsw_interval 'none'
SPEC
variant restart_invariant <<'SPEC'
nml restart config_do_restart true
stream restart filename_template fake_restart.nc
SPEC
insert_invariant restart_invariant
# A lean file with the history identity but no restart marker, paired with
# the separate invariant mesh. Header refusal must precede any state read.
"${NCGEN:-ncgen}" -o "$rd/fake_restart.nc" <<'CDL'
netcdf fake_restart {
variables:
 :lean_flavor = "mpas";
}
CDL

# 3. the checks, the programs against the one build
programs() {   # $1: short name, $2: the programs; they land in build/inputs/$1
  build_result=$(./lean build --flavor mpas --target cpu --fc "$FC" --jobs "$JOBS" --json) || {
    printf '%s\n' "$build_result"; exit 1;
  }
  b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
  fc=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["compiler"]["path"])' "$b/build.json")
  flags=$(cat "$b/flags.txt")
  libs=$(cat "$b/libs.txt")
  inc=$(sed 's/^/-I/' "$b/moddirs.txt")
  objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
  mkdir -p "$root/build/inputs/$1"
  cd "$root/build/inputs/$1"
  # shellcheck disable=SC2086
  "$fc" $flags $inc -c "$root/tests/netcdf/check.f90"
  for name in $2; do
    # shellcheck disable=SC2086
    "$fc" $flags -I. -I"$root/tests/inputs" $inc -c "$root/tests/inputs/$name.f90"
    # shellcheck disable=SC2086
    "$fc" $flags -o "$name" "$name.o" check.o $objects $libs
  done
  cd "$root"
}
programs greece 'checks lbc tables run_inputs lw_capacity'
build/inputs/greece/checks greece "$root" "$d"
if build/inputs/greece/checks set_twice > "$d/set_twice.log" 2>&1; then
 echo 'FAIL set_twice: accepted'; exit 1
fi
grep -q 'the time step is set once per run' "$d/set_twice.log"
echo 'ok   set_twice: rejected another time step'
for setting_case in sfc_set_twice sfc_set_bad; do
 if build/inputs/greece/checks "$setting_case" > "$d/$setting_case.log" 2>&1; then
  echo "FAIL $setting_case: accepted"; exit 1
 fi
done
grep -q 'Noah-MP surface exchange is set once per run' "$d/sfc_set_twice.log"
grep -q 'Noah-MP surface exchange must be 1 or 2' "$d/sfc_set_bad.log"
echo 'ok   Noah-MP setter: another value and unsupported value rejected'

build/inputs/greece/lbc "$root" "$d"
build/inputs/greece/lw_capacity 3600 > "$d/lw_fit.log" 2>&1
grep -q 'LW CAPACITY START OK' "$d/lw_fit.log"
if build/inputs/greece/lw_capacity 4000 > "$d/lw_over.log" 2>&1; then
  echo 'FAIL longwave capacity: excessive layers accepted'; exit 1
fi
grep -q 'capacity.init.nc: the longwave radiation needs 65 layers above 55 levels; this executable holds 64' "$d/lw_over.log"
echo 'ok   longwave startup capacity: 64 accepted, 65 rejected'
if build/inputs/greece/lw_capacity 3600 1 > "$d/lw_one.log" 2>&1; then
  echo 'FAIL longwave one level: accepted'; exit 1
fi
grep -qF "capacity.init.nc: longwave radiation requires at least 2 mesh levels; found 1" "$d/lw_one.log"
if grep -q 'Index.*below lower bound' "$d/lw_one.log"; then exit 1; fi
echo 'ok   longwave one level: the diagnostic before interpolation'
build/inputs/greece/tables "$root" "$d"
for set in none lsm_off lw_off rad_off micro_off; do build/inputs/greece/tables "$root" "$d" "$set"; done
(cd "$rd" && "$root/build/inputs/greece/run_inputs" "$root")
(cd "$rd" && "$root/build/inputs/greece/run_inputs" "$root" radt_none)
(cd "$rd" && "$root/build/inputs/greece/run_inputs" "$root" no_lbcs)
(cd "$rd" && "$root/build/inputs/greece/run_inputs" "$root" sfc3)
# Inspect the actual ice flags after the model's cold start (a zero-length run), each in a fresh process.
. "$root/tests/inputs/run_dir.sh"
echo "nml nhyd_model config_run_duration '0_00:00:00'" > "$d/zero_length.txt"
for ice_case in ice_free_greece ice_free_x1 land_ice2 sea_ice1; do
 ice_rd="$root/build/inputs/run_$ice_case"
 case $ice_case in
  ice_free_x1) make_run_dir "$ice_rd" "$x1" cases/x1_2562/case.txt "$d/zero_length.txt" ;;
  *) make_run_dir "$ice_rd" "$greece" cases/greece_60km/case.txt "$d/zero_length.txt" ;;
 esac
 case $ice_case in
  land_ice2|sea_ice1)
   rm "$ice_rd/greece_60km.init.nc"
   ln -s "$d/$ice_case.nc" "$ice_rd/greece_60km.init.nc" ;;
 esac
 (cd "$ice_rd" && "$root/build/inputs/greece/checks" "$ice_case")
 rm -rf "$ice_rd"
done
build/inputs/greece/checks x1 "$root" "$d"

rm -rf "$root/build/inputs"
echo 'INPUTS TESTS OK'
