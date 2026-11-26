#!/bin/sh
# Bounds-check the statements of src/phys_dev.f90 that keep and restore the glacier columns around a card step
# (keep_glacier/put_glacier), cut out as they are, with sea ice present and no glaciers. The directives are not exercised here; this checks the host arguments the card's check mode uses.
#   sh tests/land_ice/check_empty.sh [BUILD_DIR]
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$root"
if [ $# -gt 0 ]; then
 b=$1
else
 result=$(./lean build --flavor mpas --target cpu --fc gfortran --jobs 2 --json)
 b=$(printf '%s\n' "$result" | python3 -c 'import json,sys;from pathlib import Path;print(Path(json.load(sys.stdin)["build_json"]).resolve().parent)')
fi
out="$root/build/land_ice_empty"
mkdir -p "$out"
python3 - "$out/empty.f90" <<'PY'
from pathlib import Path
import sys
lines=Path('src/phys_dev.f90').read_text().splitlines()
first=[s for s in lines if 'call keep_glacier(glacier_before)' in s]
last=[s for s in lines if 'call put_glacier(glacier_host)' in s]
assert len(first)==len(last)==1
at=next(i for i,s in enumerate(lines) if 'call keep_glacier(glacier_host)' in s)-1
end=next(i for i in range(at+1,len(lines)) if lines[i].strip()=='end if')
block=lines[at:end+1]
assert block[0].strip().startswith('if (') and any('call put_glacier(glacier_before)' in s for s in block)
text='''program empty_glacier_check
 use land_ice, only: n_glacier, keep_glacier, put_glacier
 use NoahmpIOVarType, only: NoahmpIO_type
 use phys_mode, only: st_land
 implicit none
 type(NoahmpIO_type), allocatable :: glacier_before(:), glacier_host(:)
 logical :: ice_on
 integer :: ist, mask
 if(n_glacier/=0)error stop 'empty glacier test needs a fresh module'
 ist=st_land
 do mask=0,1
  ice_on=mask==1
'''+ '\n'.join(first+block+last)+'''
  if(allocated(glacier_before).or.allocated(glacier_host))error stop 'empty stash allocated columns'
 end do
 print '(a)', 'SEA-ICE-ONLY CHECK STASH OK (0 glaciers)'
end program
'''
Path(sys.argv[1]).write_text(text)
PY
flags=$(cat "$b/flags.txt")
inc=$(sed 's/^/-I/' "$b/moddirs.txt")
objects=$(grep -v '/main\.f90\.o$' "$b/objects.txt")
libs=$(cat "$b/libs.txt")
# shellcheck disable=SC2086
gfortran $flags -fcheck=all $inc -o "$out/empty" "$out/empty.f90" $objects $libs
"$out/empty"
python3 - <<'PY'
from pathlib import Path
import shutil
shutil.rmtree(Path('build/land_ice_empty'))
PY
