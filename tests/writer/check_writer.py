"""Runs tests/writer/writer_main.f90 case by case in build/writer/ and checks each file it writes with ncdump."""
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
NCDUMP=os.environ.get('NCDUMP') or shutil.which('ncdump') or 'ncdump'  # it must read CDF-5 (NCDUMP=<path> to choose)
out=Path(sys.argv[1])
cases=['dims_order','types','attributes','globals','precision_double','formats','records','clobber_never','clobber_append','clobber_overwrite','clobber_truncate','partial_rename','fill','die','globals_greece','fill_double','copy_var']
def dump(folder,*args):
    return subprocess.check_output([NCDUMP,*args,str(folder/'test.nc')],text=True)
for case in cases:
    folder=out/case; folder.mkdir(exist_ok=True)
    subprocess.run([str(out/'writer'),case,str(Path.cwd())],cwd=folder,check=True)
    header=dump(folder,'-h') if case!='formats' else ''
    if case=='dims_order':
        dims=re.findall(r'^\s*(\w+) = (?:\d+|UNLIMITED)',header,re.M)
        assert dims==['nVertLevels','nCells','Time','StrLen','nEdges'],dims
    if case=='types':
        assert 'int isnowxy(Time, nCells)' in header
        assert 'char xtime(Time, StrLen)' in header and 'StrLen = 64' in header
    if case=='attributes':
        assert 'Time:units = "seconds since 2026-11-08 06:30:00"' in header
        assert 'isnowxy:_FillValue = -9999' in header
        assert 'qc_bl:descrition' in header
    if case=='globals':
        a=re.search(r':file_id = "([0-9a-z]{10})"',header).group(1)
        other=subprocess.check_output([NCDUMP,'-h',str(folder/'other.nc')],text=True)
        assert a!=re.search(r':file_id = "([0-9a-z]{10})"',other).group(1)
        subprocess.run([str(out/'writer'),case,str(Path.cwd())],cwd=folder,check=True)
        assert a!=re.search(r':file_id = "([0-9a-z]{10})"',dump(folder,'-h')).group(1), 'file_id repeated across processes'
        assert header.index(':first')<header.index(':second')<header.index(':third')<header.index(':file_id')
    if case=='precision_double':
        assert 'double qv(' in header and 'double Time(' in header
        assert ':second = 2.5 ;' in header
        assert 'int isnowxy(' in header
    if case=='formats':
        for i,magic in enumerate([b'CDF\x05',b'CDF\x05',b'CDF\x02',b'\x89HDF'],1):
            assert (folder/f'fmt{i}.nc').read_bytes()[:4]==magic
    if case in ('records','clobber_append','clobber_overwrite'):
        assert '// (3 currently)' in header
        times=re.findall(r'2026-11-08_\d\d:30:00',dump(folder,'-v','xtime').split('xtime =')[1])
        assert times==['2026-11-08_06:30:00','2026-11-08_07:30:00','2026-11-08_08:30:00'],times
        if case=='clobber_overwrite':
            values=[int(v) for v in re.findall(r'\d+',dump(folder,'-v','qv').split('qv =')[1])]
            assert values==[1]*6+[3]*6+[4]*6,values
    if case in ('clobber_never','clobber_truncate'):
        assert '// (1 currently)' in header
    if case in ('fill','fill_double'):
        data=dump(folder,'-p','17,17','-v','qv,isnowxy')
        if case=='fill_double': assert 'double qv(' in header
        assert 'qv =\n  _, _, _,' in data and 'isnowxy =\n  -2147483647, -2147483647' in data,data
    if case=='globals_greece':
        subprocess.run([sys.executable,'tests/writer/compare_cdl.py',str(folder/'test.nc'),
                        'cases/greece_60km/reference/strict_12h.history.cdl','--globals'],check=True)
    if case=='die':
        before=dump(folder,'-v','qv,latCell')
        env=dict(os.environ,LEAN_WRITER_DIE='2')
        result=subprocess.run([str(out/'writer'),'die_child'],cwd=folder,env=env)
        assert result.returncode==-9,result.returncode
        assert '// (1 currently)' in dump(folder,'-h')
        assert before==dump(folder,'-v','qv,latCell'), 'die changed record 1'
    if case=='copy_var':
        source=subprocess.check_output([NCDUMP,'-p','9,17',str(folder/'source.nc')],text=True)
        data=dump(folder,'-p','9,17')
        for variable in ('real_copy','char_copy'):
            assert next(line for line in source.splitlines() if f' {variable}(' in line) in data
            original=re.search(rf'^\s*{variable} =.*?;',source,re.M|re.S)[0]
            assert original in data
        assert 'real_copy:units = "meters"' in data
        assert 'real_copy:missing_value = -1.25' in data
        assert 'char_copy:description = "copied text"' in data
    print('ok  '+case)

print(f'WRITER TESTS OK {len(cases)}')
