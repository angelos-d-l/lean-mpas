#!/bin/sh
# Surface-moisture switches: lean against MPAS 8.4.2 on single columns.
#   sh tests/mynn_moisture/run.sh          (needs reference/mpas_src, exit 77 without it; about 3 min;
#                                           KEEP=1 keeps the traces and objects, about 450 MB, in build/mynn_moisture)
# Builds the MPAS MYNN drivers once, then one harness per variant (lean host or card sources, build switches, dt), runs
# them, and summarises: switches off must be bit-identical to MPAS (exit 1 otherwise), the mutation must be seen.
# The variants named hyd_dew* have LEAN_MYNN_HYD_PS and LEAN_MYNN_DEW on, plus the switches their names add.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
ref="$root/reference/mpas_src/src"
phys="$ref/core_atmosphere/physics"
out="$root/build/mynn_moisture"
[ -d "$phys" ] || { echo 'SKIP mynn moisture: reference/mpas_src is absent (./lean fetch-upstream)'; exit 77; }
flags="-cpp -Dmpas -DSINGLE_PRECISION -ffree-form -ffree-line-length-none -O2 -ffp-contract=off"
rm -rf "$out"; mkdir -p "$out/mpas"

cd "$out/mpas"
cat > mpas_log_stub.f90 <<'EOF'
module mpas_log   ! empty: the MYNN wrappers use the module, but their mpas_log_write calls are commented out
end module
EOF
for f in mpas_log_stub.f90 "$ref/framework/mpas_kind_types.F" "$ref/framework/mpas_constants.F" \
  "$phys/mpas_atmphys_constants.F" "$phys/ccpp_kind_types.F" "$phys/physics_mmm/mynn_shared.F90" \
  "$phys/physics_mmm/bl_mynn_subroutines.F90" "$phys/physics_mmm/bl_mynn.F90" "$phys/physics_mmm/sf_mynn.F90" \
  "$phys/physics_wrf/sf_mynn_pre.F" "$phys/physics_wrf/bl_mynn_pre.F" "$phys/physics_wrf/bl_mynn_post.F" \
  "$phys/physics_wrf/module_sf_mynn.F" "$phys/physics_wrf/module_bl_mynn.F"; do
  gfortran $flags -c "$f"
done

# variant  side  switches  [DT]   (DT: the time step in seconds, passed to the column at run time; default 30)
build() {
  name=$1; side=$2; switches=$3; dtdef=${4:-}
  d="$out/$name"; mkdir -p "$d"; cd "$d"
  defs=""; for s in $switches; do defs="$defs -D$s"; done
  echo "${dtdef:+dt=$dtdef}" > dt.arg
  if [ "$side" = card ]; then s="$root/src/device"; else s="$root/src"; fi
  pre=""
  if [ "$side" = card ]; then
    printf 'module lean_warp\n contains\n subroutine warp_sync()\n end subroutine\nend module\n' > warp_stub.f90
    pre="warp_stub.f90 $root/src/phys_fault.f90"
  fi
  python3 "$root/tests/mynn_moisture/extract.py" "$d" "$side" "$phys" > /dev/null
  for f in $pre "$root/src/levels.f90" "$root/src/run_settings.f90" "$root/src/config.f90" "$root/src/winds.f90" "$s/state.f90" "$s/mynn_shared.f90" "$s/mynn_surface.f90" \
    "$s/mynn_pbl_helpers.f90" "$s/mynn_pbl.f90" "$s/mynn_conversions.f90" "$s/mynn.f90" sfc_diag_lean.f90 sfc_diag_mpas.f90 \
   ; do
    gfortran $flags $defs -I"$out/mpas" -c "$f"
  done
  sh "$root/tools/build_test_state.sh" "$root" $flags $defs -I"$out/mpas"
  gfortran $flags $defs -I"$out/mpas" -c "$root/tests/mynn_moisture/column.f90"
  gfortran -o column *.o "$out"/mpas/*.o
  echo "$name: $side sources, switches: ${switches:-none}${dtdef:+, dt $dtdef s}" > build.txt
}
run() {   # variant log arguments...
  name=$1; logfile=$2; shift 2
  cd "$out/$name"; ./column $(cat dt.arg) "$@" > "$logfile" 2>&1 || true
  echo "== $(cat build.txt) $*"; cat "$logfile"
}

build off_host host ""
build off_card card ""
run off_host run.log
run off_card run.log

run off_host run_mutate.log mutate trace_lean_mutate.bin trace_mpas_mutate.bin
for v in "hyd_dew:" "hyd_dew_rhosfc:LEAN_MYNN_RHOSFC" "hyd_dew_moist:LEAN_MYNN_MOIST" "hyd_dew_rhosfc_moist:LEAN_MYNN_RHOSFC LEAN_MYNN_MOIST" \
  "hyd_dew_q2mr:LEAN_SFC_Q2MR" "hyd_dew_all:LEAN_MYNN_RHOSFC LEAN_MYNN_MOIST LEAN_SFC_Q2MR"; do
  build "${v%%:*}" host "LEAN_MYNN_HYD_PS LEAN_MYNN_DEW ${v#*:}"; run "${v%%:*}" run.log
done
run hyd_dew run_perturb.log perturb trace_lean_perturb.bin trace_mpas_perturb.bin
build hyd_dew_all_card card "LEAN_MYNN_HYD_PS LEAN_MYNN_DEW LEAN_MYNN_RHOSFC LEAN_MYNN_MOIST LEAN_SFC_Q2MR"; run hyd_dew_all_card run.log
build hyd_dew_dt18 host "LEAN_MYNN_HYD_PS LEAN_MYNN_DEW" 18; run hyd_dew_dt18 run.log
build hyd_dew_moist_dt18 host "LEAN_MYNN_HYD_PS LEAN_MYNN_DEW LEAN_MYNN_MOIST" 18; run hyd_dew_moist_dt18 run.log
if python3 "$root/tests/mynn_moisture/summarize.py" "$out" > "$out/summary.txt"; then rc=0; else rc=1; fi
cat "$out/summary.txt"
# traces and objects are about 450 MB: kept only with KEEP=1; the logs and summary.txt stay
if [ "${KEEP:-0}" != 1 ]; then
  find "$out" \( -name '*.bin' -o -name '*.o' -o -name '*.mod' -o -name '*.s' -o -name '*.f90' -o -name column \) -delete
fi
exit $rc
