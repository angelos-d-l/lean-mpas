#!/bin/sh
# Exercise the host column buffers with two real OpenMP threads, using the same MPAS oracle.
set -eu
root=$PWD
b=$1
fc=$2
work=$root/build/diag/openmp
mkdir -p "$work"
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v -e '/main\.f90\.o$' -e '/diag_convective\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
case "$fc" in *nvfortran) omp=-mp; modflag=-module ;; *) omp=-fopenmp; modflag=-J ;; esac
python3 - "$work" <<'PY'
from pathlib import Path
import sys
w=Path(sys.argv[1])
s=Path('src/diag/diag_convective.f90').read_text()
s=s.replace('module diag_convective\n','module diag_convective\n use omp_lib,only:omp_get_num_threads\n',1)
s=s.replace(' do c=1,size(out%values,2)',' do c=1,size(out%values,2)\n  if(c==1.and.omp_get_num_threads()/=2)error stop "OPENMP expected two threads"',1)
(w/'convective.f90').write_text(s)
(w/'families.f90').write_text(Path('tests/diag/output_families.f90').read_text())
PY
"$fc" $flags $omp $inc $modflag "$work" -c "$work/convective.f90" -o "$work/convective.o"
"$fc" $flags $omp -I"$work" -Ibuild/diag $inc -o "$work/test" "$work/families.f90" build/diag/mpas_oracle.o "$work/convective.o" $objects $libs
# The probe works: the program as it is (its timing pass on one thread) must fail the two-thread requirement.
if (cd "$b"; OMP_NUM_THREADS=2 OMP_DYNAMIC=FALSE "$work/test" "$root" greece_60km) > "$work/one_thread.log" 2>&1; then
 echo 'FAIL OpenMP probe did not reject one thread'; exit 1
fi
grep 'OPENMP expected two threads' "$work/one_thread.log" >/dev/null
echo 'ok   OpenMP probe: one thread is rejected (OPENMP expected two threads)'
python3 - "$work/families.f90" <<'PY'
from pathlib import Path
import sys
p=Path(sys.argv[1]);s=p.read_text();assert s.count('omp_set_num_threads(1)')==1
p.write_text(s.replace('omp_set_num_threads(1)','omp_set_num_threads(2)'))
PY
"$fc" $flags $omp -I"$work" -Ibuild/diag $inc -o "$work/test" "$work/families.f90" build/diag/mpas_oracle.o "$work/convective.o" $objects $libs
if ! (cd "$b"; OMP_NUM_THREADS=2 OMP_DYNAMIC=FALSE "$work/test" "$root" greece_60km) > "$work/two_threads.log" 2>&1; then
 cat "$work/two_threads.log"; exit 1
fi
grep 'OUTPUT FAMILIES OK greece_60km' "$work/two_threads.log" >/dev/null
echo 'DIAG OPENMP OK 2 threads'
rm -rf "$work"
