#!/bin/sh
# The build switches LEAN_GF_NOSHALLOW (no shallow plume) and LEAN_GF_DXMIN (no GF below a cell size) on 4096
# synthetic GF columns: one harness per build (host and card sources, switches off and on, two mutants), then
# tests/suite_coupling/gf_switches.py checks what each switch must do and that the mutants are caught. About 1 min.
#   sh tests/suite_coupling/gf_switches.sh      (outputs in build/suite_coupling/gf, deleted at the end unless KEEP=1)
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
out="$root/build/suite_coupling/gf"
flags="-cpp -Dmpas -DSINGLE_PRECISION -ffree-form -ffree-line-length-none -O2 -ffp-contract=off"
rm -rf "$out"; mkdir -p "$out"
# name  gf-source  defines
build() {
  d="$out/$1"; mkdir -p "$d"; cd "$d"
  for f in "$root/src/levels.f90" "$root/src/run_settings.f90" "$root/src/config.f90" "$root/src/winds.f90" "$root/src/state.f90" "$root/src/gf_state.f90" "$root/src/gf_core.f90" \
    "$2" "$root/tests/gf_fixture.f90" "$root/tests/gf_synthetic.f90"; do
    gfortran $flags $3 -c "$f"
  done
  sh "$root/tools/build_test_state.sh" "$root" $flags $3
  gfortran $flags $3 -c "$root/tests/suite_coupling/gf_switches.f90"
  gfortran -o gf_switches ./*.o
  ./gf_switches
}
build off "$root/src/gf.f90" ""
build off_card "$root/src/device/gf.f90" ""
build noshallow "$root/src/gf.f90" "-DLEAN_GF_NOSHALLOW"
build noshallow_card "$root/src/device/gf.f90" "-DLEAN_GF_NOSHALLOW"
build dxmin "$root/src/gf.f90" "-DLEAN_GF_DXMIN=5000"
build dxmin_card "$root/src/device/gf.f90" "-DLEAN_GF_DXMIN=5000"
build both "$root/src/gf.f90" "-DLEAN_GF_NOSHALLOW -DLEAN_GF_DXMIN=5000"
# mutants: the switch also drops the deep plume; the dx test the wrong way round
mkdir -p "$out/mut"
sed 's/^    call deep_column()$/#ifndef LEAN_GF_NOSHALLOW\n    call deep_column()\n#endif/' "$root/src/gf.f90" > "$out/mut/gf_noshallow_mutant.f90"
sed 's/if(a%dx >= real(LEAN_GF_DXMIN)) then/if(a%dx < real(LEAN_GF_DXMIN)) then/' "$root/src/gf.f90" > "$out/mut/gf_dxmin_mutant.f90"
cmp -s "$root/src/gf.f90" "$out/mut/gf_noshallow_mutant.f90" && { echo 'noshallow mutant not made'; exit 1; }
cmp -s "$root/src/gf.f90" "$out/mut/gf_dxmin_mutant.f90" && { echo 'dxmin mutant not made'; exit 1; }
build noshallow_mutant "$out/mut/gf_noshallow_mutant.f90" "-DLEAN_GF_NOSHALLOW"
build dxmin_mutant "$out/mut/gf_dxmin_mutant.f90" "-DLEAN_GF_DXMIN=5000"
if python3 "$root/tests/suite_coupling/gf_switches.py" "$out" 5000 > "$out/summary.txt"; then rc=0; else rc=1; fi
cat "$out/summary.txt"
[ "${KEEP:-0}" = 1 ] || find "$out" \( -name '*.bin' -o -name '*.o' -o -name '*.mod' -o -name gf_switches -o -name '*.f90' \) -delete
exit $rc
