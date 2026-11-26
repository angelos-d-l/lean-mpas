#!/bin/sh
# src/phys_dev.f90, src/phys_check.f90 and the other card-only units are compiled by the card build only, so a CPU
# build never reads them. Here gfortran parses them (-fsyntax-only, the OpenACC directives are comments) against the
# modules of the default CPU build, with the card-only units they use (phys_fault, radiation_device_tables, ...).
#   sh tests/sizes/card_units.sh
# Any compiler error fails. This is a parse and type check, not a card build: the directives are not read.
# Prints CARD UNITS SYNTAX OK; writes and removes build/sizes/card_units/.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
build_result=$(./lean build --target cpu --fc gfortran --profile strict --jobs 2 "$@" --json) || { printf '%s\n' "$build_result"; exit 1; }
b=$(printf '%s\n' "$build_result" | python3 -c 'import json,sys; from pathlib import Path; print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
out="$root/build/sizes/card_units"
rm -rf "$out"; mkdir -p "$out"; cd "$out"
flags=$(cat "$b/flags.txt")
inc="-I. $(sed 's/^/-I/' "$b/moddirs.txt" | tr '\n' ' ')"
for f in src/phys_fault.f90 src/phys_check.f90 src/device/radiation_device_tables.f90 src/device/radiation_fast_data.f90 src/device/radiation_fast_gas.f90 src/device/radiation_fast.f90 src/device/mynn_card.f90; do
  # shellcheck disable=SC2086
  gfortran $flags $inc -fsyntax-only "$root/$f" > "$(basename "$f").out" 2>&1 || { cat "$(basename "$f").out"; echo "CARD UNITS SYNTAX FAILED: $f"; exit 1; }
done
# lean_warp is CUDA Fortran (card build only): its host interface as a stub module for the parse
printf 'module lean_warp\n implicit none\ncontains\n integer function thread_slot()\n  thread_slot = 1\n end function\n subroutine warp_sync()\n end subroutine\nend module\n' > lean_warp_stub.f90
# shellcheck disable=SC2086
gfortran $flags $inc -fsyntax-only lean_warp_stub.f90 > lean_warp_stub.out 2>&1 || { cat lean_warp_stub.out; echo 'CARD UNITS SYNTAX FAILED: lean_warp stub'; exit 1; }
# shellcheck disable=SC2086
if ! gfortran $flags $inc -fsyntax-only "$root/src/phys_dev.f90" > phys_dev.out 2>&1; then
  cat phys_dev.out; echo 'CARD UNITS SYNTAX FAILED: src/phys_dev.f90'; exit 1
fi
# The output fetch lives in lean_model's LEAN_PHYS_DEV branch too.
# shellcheck disable=SC2086
if ! gfortran $flags $inc -cpp -DLEAN_PHYS_DEV -fsyntax-only "$root/src/lean_model.f90" > lean_model.out 2>&1; then
  cat lean_model.out; echo 'CARD UNITS SYNTAX FAILED: src/lean_model.f90'; exit 1
fi
cd "$root"; rm -rf "$out"
echo 'CARD UNITS SYNTAX OK (phys_dev and lean_model: no errors)'
