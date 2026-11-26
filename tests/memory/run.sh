#!/bin/sh
# The memory check: the unit checks of src/run/lean_memory.f90 against tools/lean/memory.py, then the model runs of
# tests/memory/model_runs.py. Needs the release case inputs in data/cases (exit 77 without them).
#   sh tests/memory/run.sh [FC] [JOBS]      LEAN_BASE_EXE: another build's lean_mpas, for the identity cases
# Builds the strict cpu build with ./lean, then in build/memory/: the units program and lean_mpas_test, the same
# objects with src/run/lean_memory.f90 compiled with -DLEAN_MEMORY_TEST (its LEAN_MEMORY_AVAILABLE_MIB hook).
# A passing run removes build/memory; a failure exits 1 and keeps it.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
fc=${1:-gfortran}
JOBS=${2:-2}
if [ ! -f data/cases/greece_60km/inputs/greece_60km.init.nc ] || [ ! -f data/cases/x1_2562/inputs/x1.2562.init.nc ]; then
  echo 'SKIP memory: needs the release case inputs in data/cases (cases/README.md)'
  exit 77
fi
trap 'code=$?; if [ "$code" -ne 0 ]; then echo "MEMORY TESTS FAILED (build/memory kept)"; exit 1; fi' 0
./lean build --target cpu --fc "$fc" --profile strict --jobs "$JOBS" > /dev/null
b="$root/build/cpu-$fc-strict"
w="$root/build/memory"
rm -rf "$w"
mkdir -p "$w/mod"
flags=$(cat "$b/flags.txt")
includes=$(sed 's/^/-I/' "$b/moddirs.txt" | tr '\n' ' ')
libs=$(cat "$b/libs.txt")
grep -v '/main.f90.o$' "$b/objects.txt" > "$w/objects.txt"
module_flag="-J"
if [ "$fc" = nvfortran ]; then module_flag="-module"; fi
# shellcheck disable=SC2086
"$fc" $flags $module_flag "$w/mod" -I"$w/mod" $includes -c tests/memory/units.f90 -o "$w/units.o"
# shellcheck disable=SC2086
"$fc" $flags "$w/units.o" $(cat "$w/objects.txt") -o "$w/units" $libs
# the test build: lean_memory with its hook, linked with everything else as it is
mkdir -p "$w/hook"
# shellcheck disable=SC2086
"$fc" $flags -DLEAN_MEMORY_TEST $module_flag "$w/hook" $includes -c src/run/lean_memory.f90 -o "$w/hook/lean_memory.o"
grep -v '/lean_memory.f90.o$' "$b/objects.txt" > "$w/hook/objects.txt"
echo "$w/hook/lean_memory.o" >> "$w/hook/objects.txt"
# shellcheck disable=SC2086
"$fc" $flags $(cat "$w/hook/objects.txt") -o "$w/lean_mpas_test" $libs
cd "$w"
./units verdict
./units card
./units stages
[ "$(uname)" = Darwin ] && ./units darwin
./units need > need.txt
python3 - "$root" "$w" <<'PY'
import sys
from pathlib import Path
root, w = Path(sys.argv[1]), Path(sys.argv[2])
sys.path[:0] = [str(root), str(root / 'tests/memory')]
from tools.lean import memory
import cgroup_fixtures
import subprocess
terms = memory.load_terms()
cases = {'greece': dict(ncells=2337, nedges=7183, nvertices=4847, nz=45),
         'x1': dict(ncells=2562, nedges=7680, nvertices=5120, nz=45, n_glacier=80, glacier_capacity=128),
         'athens': dict(ncells=46251, nedges=139361, nvertices=93111, nz=45)}
card = dict(card_build=True, any_card=True, staged_pools=True, card_radiation=True, card_thompson=True, products=True,
            film=True, mynn_ldc=2368, mynn_plumes=2368)
fails = 0
for line in (w / 'need.txt').read_text().splitlines():
    kind, name, *values = line.split()
    values = [int(v) for v in values]
    base = {**cases[name], 'largest_output': cases[name]['nedges'] * 45, 'held': 28}
    if kind == 'host':
        h = memory.host_needed(memory.case_dict(terms, **base), terms)
        want = [h['resident'], h['start'], h['step'], h['sum'], h['headroom'], h['need']]
    else:
        at_load = kind == 'cardload'
        c = memory.card_needed(memory.case_dict(terms, **base, **card), terms, 9744, 98304, held_q=900 * 2 ** 20,
                               stack_at_load=at_load, residual=0 if at_load else 123 * 2 ** 20)
        want = [c['held_q'], c['rows'], c['stack'], c['residual'], c['full'], c['headroom'], c['more']]
    ok = values == want
    fails += not ok
    print(('ok    ' if ok else 'FAIL  ') + f'need {kind} {name}: Fortran and Python ' + ('agree' if ok else f'{values} != {want}'))
for name, (size, source) in cgroup_fixtures.make(w / 'cgroup').items():
    out = subprocess.run(['./units', 'avail', str(w / 'cgroup' / name) + '/'], capture_output=True, text=True,
                         check=True).stdout.split(None, 2)
    python = memory.available_linux(str(w / 'cgroup' / name))
    got = (int(out[1]), out[2].strip())
    want = (-1 if size is None else size, source or python[1])
    ok = got == want and (python[0] if python[0] is not None else -1, python[1]) == got
    fails += not ok
    print(('ok    ' if ok else 'FAIL  ') + f'available_linux {name}: {got}' + ('' if ok else f' want {want}, Python {python}'))
for init, want in ((root / 'data/cases/x1_2562/inputs/x1.2562.init.nc', 80),
                   (root / 'data/cases/greece_60km/inputs/greece_60km.init.nc', 0)):
    out = subprocess.run(['./units', 'slabs', str(init)], capture_output=True, text=True, check=True).stdout.split()
    ok = out == ['glacier', str(want)]
    fails += not ok
    print(('ok    ' if ok else 'FAIL  ') + f'slabs of 7 cells: {init.name} {out[-1]} glacier columns (want {want})')
sys.exit(1 if fails else 0)
PY
cd "$root"
python3 tests/memory/model_runs.py --fc "$fc" --exe "$b/lean_mpas" --test-exe "$w/lean_mpas_test" \
  ${LEAN_BASE_EXE:+--base-exe "$LEAN_BASE_EXE"}
rm -rf "$w"
echo 'MEMORY TESTS OK'
