"""tests/writer/t0_values.py's allowances on synthetic files. The check runs under LEAN_PY (or PYTHON), which needs
netCDF4 and numpy; unittest itself needs only Python."""
import os
import subprocess
import unittest


class T0ValuesTest(unittest.TestCase):
    def test_compiler_and_counts(self):
        script = '''
from pathlib import Path
from tempfile import TemporaryDirectory
import numpy as np
from netCDF4 import Dataset
from tests.writer.t0_values import compare
with TemporaryDirectory() as tmp:
    from tests.writer.t0_values import GFORTRAN_BITS,ANY_BITS
    cases=[(mesh,name,count,maximum) for mesh,fields in GFORTRAN_BITS.items() for name,(maximum,count) in fields.items()]
    assert len(cases)>200
    for mesh,name,count,maximum in cases:
        value=maximum/2
        root=Path(tmp)/mesh; root.mkdir(exist_ok=True,parents=True)
        a,b=root/'lean.nc',root/'mpas.nc'
        def files(n):
            base=np.float32(value)
            for path,v in [(a,np.nextafter(base,np.float32(np.inf))),(b,base)]:
                with Dataset(path,'w') as ds:
                    ds.createDimension('n',n)
                    ds.createVariable(name,'f4',('n',))[:]=v
        files(1)
        compare(a,b,'gfortran')
        any_allowed=next((v for key,v in ANY_BITS.items() if key in mesh),{})
        if name in any_allowed:
            compare(a,b,'nvfortran')
        else:
            try: compare(a,b,'nvfortran')
            except AssertionError: pass
            else: raise AssertionError('nvfortran accepted a one-bit '+name+' difference')
        files(count)
        compare(a,b,'gfortran')
        files(1)
        with Dataset(a,'a') as ds:ds[name][:]=np.float32(maximum*2)
        with Dataset(b,'a') as ds:ds[name][:]=np.float32(0)
        try: compare(a,b,'gfortran')
        except AssertionError as e: assert 'above the measured' in str(e),str(e)
        else: raise AssertionError('gfortran accepted excessive '+name+' maximum')
        files(count+1)
        try: compare(a,b,'gfortran')
        except AssertionError as e: assert 'count' in str(e), str(e)
        else: raise AssertionError('gfortran accepted too many differing '+name+' values')
    # Helicity: x1 keeps up to five cells on any compiler (ANY_BITS); Greece keeps none.
    def uh(mesh,n):
        root=Path(tmp)/(mesh+'_uh'); root.mkdir(exist_ok=True,parents=True)
        a,b=root/'lean.nc',root/'mpas.nc'
        base=np.float32(0.0001)
        for path,v in [(a,np.nextafter(base,np.float32(np.inf))),(b,base)]:
            with Dataset(path,'w') as ds:
                ds.createDimension('n',n)
                ds.createVariable('updraft_helicity_max','f4',('n',))[:]=v
        return a,b
    for fc in ('gfortran','nvfortran'):
        compare(*uh('x1_2562',5),fc)
        for mesh,n in [('x1_2562',6),('greece_60km',1)]:
            try: compare(*uh(mesh,n),fc)
            except AssertionError: pass
            else: raise AssertionError(fc+' accepted '+str(n)+' differing helicity values on '+mesh)
'''
        result = subprocess.run([os.environ.get('LEAN_PY', os.environ.get('PYTHON', 'python3')), '-c', script], capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
