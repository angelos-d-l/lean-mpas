"""Text checks of the diagnostic card kernels (no goto, no vector level loop, no allocation or automatic array in a
routine seq); it reads the source and needs no card compiler."""
from pathlib import Path
import re
import sys

def faults(text):
    errors=[]
    for block in re.split(r'(?im)^\s*end subroutine[^\n]*',text):
        if '!$acc routine seq' not in block.lower() and '!$acc parallel' not in block.lower():
            continue
        code=re.sub(r'(?m)!(?!\$acc).*','',block).lower()
        if re.search(r'\bgo\s*to\b',code): errors.append('goto in card code')
        if re.search(r'!\$acc loop vector[^\n]*\n\s*do\s+\w+\s*=\s*1\s*,\s*(nz|nlev)',code):
            errors.append('vector level loop')
        if '!$acc routine seq' in code:
            if re.search(r'\b(allocatable|allocate)\b',code): errors.append('allocation in routine seq')
            if re.search(r'::[^\n]*\((?:nz|nlev|nk)\)',code): errors.append('automatic array in routine seq')
    return errors

def main():
    paths=[Path(p) for p in sys.argv[1:]] or sorted(Path('src/diag').glob('*.f90'))
    assert paths,'no diagnostic units'
    bad=[f'{p}: {e}' for p in paths for e in faults(p.read_text())]
    if bad: raise SystemExit('\n'.join(bad))
    print('DIAG AUDIT OK')
if __name__=='__main__':main()
