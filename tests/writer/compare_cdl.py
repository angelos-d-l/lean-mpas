#!/usr/bin/env python3
"""Compare ncdump headers in MPAS definition order."""
import argparse
import difflib
import os
import shutil
from pathlib import Path
import re
import subprocess

def normalized(text, globals_only=False):
    lines=text.splitlines()[1:]
    result=[]
    in_globals=False
    for line in lines:
        line=re.sub(r'\s*// \(\d+ currently\)', '', line).strip()
        if line=='// global attributes:': in_globals=True
        if globals_only and not in_globals: continue
        if re.match(r':lean_(flavor|switches|version)\s*=',line): continue
        if re.match(r':(file_id|git_version)\s*=',line):
            line=line.split('=')[0].strip()+' = <exempt> ;'
        if line: result.append(line)
    return result

def compare(actual, expected, globals_only=False):
    raw=[line.strip() for line in actual.splitlines() if line.strip()]
    lean=[i for i,line in enumerate(raw) if re.match(r':lean_(flavor|switches|version)\s*=',line)]
    fid=next((i for i,line in enumerate(raw) if re.match(r':file_id\s*=',line)), -1)
    wanted=['lean_flavor','lean_switches','lean_version']
    names=[raw[i].split('=')[0].strip()[1:] for i in lean]
    if lean!=list(range(fid-3,fid)) or names!=wanted:
        return ['DIFF lean attributes must precede file_id in flavor, switches, version order']
    # lean writes the exchange coefficients chvxy and chbxy in m s^-1; MPAS's Registry gives them the inverse unit.
    # Only the reference is adapted: a lean file with MPAS's unit must still fail.
    expected=re.sub(r'(ch[vb]xy:units\s*=\s*)"s m\^\{-1\}"',r'\1"m s^{-1}"',expected)
    a,b=normalized(actual,globals_only),normalized(expected,globals_only)
    changes=[repr(b[j:k])+' -> '+repr(a[i:l])
             for tag,j,k,i,l in difflib.SequenceMatcher(a=b,b=a,autojunk=False).get_opcodes() if tag!='equal']
    return ['DIFF '+'; '.join(changes)] if changes else []

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('lean'); p.add_argument('mpas'); p.add_argument('--globals',action='store_true')
    a=p.parse_args()
    actual=Path(a.lean).read_text() if a.lean.endswith('.cdl') else subprocess.check_output([os.environ.get('NCDUMP') or shutil.which('ncdump') or 'ncdump','-h',a.lean],text=True)
    diffs=compare(actual,Path(a.mpas).read_text(),a.globals)
    print('\n'.join(diffs) if diffs else 'STRUCTURE SAME '+a.lean)
    return bool(diffs)
if __name__=='__main__': raise SystemExit(main())
