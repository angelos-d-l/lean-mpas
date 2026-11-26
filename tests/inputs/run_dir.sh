# An MPAS run directory for lean_mpas, sourced by the test scripts ($root is set):
#   make_run_dir DIR INPUTS SPEC...
# DIR gets namelist.atmosphere and streams.atmosphere (MPAS's defaults with the specs applied, tools/cases/run_spec.py),
# the four stream lists, and links to the init and boundary files of INPUTS and to the tables MPAS wants beside it.
make_run_dir() {
  run_dir=$1
  run_inputs=$2
  shift 2
  mkdir -p "$run_dir"
  python3 "$root/tools/cases/run_spec.py" --out "$run_dir" "$root/reference/mpas_defaults/namelist.atmosphere" \
    "$root/reference/mpas_defaults/streams.atmosphere" "$@" > /dev/null
  cp "$root"/reference/mpas_defaults/stream_list.atmosphere.* "$run_dir/"
  ln -s "$run_inputs"/*.init.nc "$run_dir/"
  for run_file in "$run_inputs"/lbc.*.nc; do
    [ -e "$run_file" ] && ln -s "$run_file" "$run_dir/"
  done
  ln -s "$root"/reference/radiation/OZONE_*.TBL "$root"/reference/radiation/RRTMG_*_DATA "$run_dir/"
  ln -s "$root/reference/config/LANDUSE.TBL" "$root/reference/config/NoahmpTable.TBL" "$root"/data/tables/*.DBL "$run_dir/"
}
