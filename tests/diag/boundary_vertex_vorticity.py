"""MPAS's vorticity_500hPa on the boundary vertices of the regional greece_60km mesh: its pressure at a vertex counts the
cells outside the mesh as zero. This reproduces MPAS's file that way (and shows that omitting them does not); it needs
the MPAS reference outputs of data/cases, which are not released."""
from pathlib import Path
import numpy as np
from netCDF4 import Dataset
root=Path(__file__).resolve().parents[2]
base=root/'data/cases/greece_60km'
with Dataset(base/'inputs/greece_60km.init.nc') as m, Dataset(base/'reference/strict_12h/history.2026-11-08_00.00.00.nc') as h, Dataset(base/'reference/strict_12h/diag.2026-11-08_00.00.00.nc') as d:
    cells=np.asarray(m['cellsOnVertex'][:]);ka=np.asarray(m['kiteAreasOnVertex'][:],dtype='f4');area=np.asarray(m['areaTriangle'][:],dtype='f4')
    p=np.asarray(h['pressure'][0],dtype='f4')/np.float32(100)
    vort=np.asarray(h['vorticity'][0],dtype='f4');want=np.asarray(d['vorticity_500hPa'][0],dtype='f4')
    boundary=np.flatnonzero(np.any(cells==0,axis=1));assert len(boundary)==344
    def compute(omit):
        out=[]
        for v in boundary:
            pv=np.zeros(p.shape[1],dtype='f4');den=np.float32(0)
            for j,c in enumerate(cells[v]):
                if c:
                    pv+=ka[v,j]*p[c-1];den+=ka[v,j]
            pv/=den if omit else area[v]
            pi=pv[::-1];fi=vort[v,::-1];kk=0;target=np.float32(500)
            for k in range(len(pi)-1):
                if target>pi[k] and target<=pi[k+1]:kk=k;break
            if target<pi[0]:val=fi[0]*target/pi[0]
            elif target>pi[-1]:val=fi[-1]
            else:
                dpu=target-pi[kk];dpl=pi[kk+1]-target
                val=(fi[kk]*dpl+fi[kk+1]*dpu)/(dpl+dpu)
            out.append(val)
        return np.asarray(out,dtype='f4')
    w=want[boundary];assert np.count_nonzero(w)>0
    for omit in (False,True):
        x=compute(omit);delta=np.abs(x.astype('f8')-w.astype('f8'));bits=np.abs(x.view('i4').astype('i8')-w.view('i4').astype('i8'))
        print(f'boundary_vertex_vorticity {"omit and renormalize" if omit else "outside pressure zero"}: vertices {len(w)}, differing {np.count_nonzero(bits)}, max_abs {delta.max():.17g}, max_ulps {bits.max()}')
        if not omit:assert bits.max()<=4,'zero outside pressure does not explain MPAS file'
