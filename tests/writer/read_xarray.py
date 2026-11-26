#!/usr/bin/env python3
"""Check the default xarray reader against paired MPAS files."""
import sys
import numpy as np
import xarray as xr

def check(lean,mpas):
    with xr.open_dataset(lean) as a,xr.open_dataset(mpas) as b:
        assert set(a.data_vars)==set(b.data_vars),(lean,'data_vars')
        assert dict(a.sizes)==dict(b.sizes),(lean,'dimensions')
        extra={'lean_flavor','lean_switches','lean_version'}
        assert set(a.attrs)-extra==set(b.attrs)-extra,(lean,'global attribute keys')
        for n in a.variables:
            assert set(a[n].attrs)==set(b[n].attrs),(lean,n,'attribute keys')
        for path,ds in [(lean,a),(mpas,b)]:
            if 'Time' not in ds or 'xtime' not in ds:continue
            with xr.open_dataset(path,decode_times=False) as raw:
                initial=raw.initial_time.values.item()
                if isinstance(initial,bytes):initial=initial.decode()
                initial=str(initial).rstrip('\x00 ').replace('_','T')
                base=np.datetime64(initial,'ns')
                seconds=np.asarray(raw.Time.values,dtype='f8').ravel()
                for t,stamp,seconds_since in zip(ds.Time.values.ravel(),ds.xtime.values.ravel(),seconds):
                    if isinstance(stamp,bytes):stamp=stamp.decode()
                    expected=np.datetime64(str(stamp).rstrip('\x00 ').replace('_','T'))
                    computed=base+np.timedelta64(round(float(seconds_since)*1.e9),'ns')
                    assert np.datetime64(t)==expected==computed,(path,t,expected,computed)

def main(args):
    assert args and len(args)%2==0,'supply LEAN_NC MPAS_NC pairs'
    for i in range(0,len(args),2):check(*args[i:i+2])
    print('XARRAY OK '+str(len(args)//2))
if __name__=='__main__':main(sys.argv[1:])
