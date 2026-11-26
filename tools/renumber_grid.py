"""Renumber an MPAS grid file (grid.nc) along a space-filling curve, before its static and init files are made.

    python tools/renumber_grid.py IN.grid.nc OUT.grid.nc [--curve=hilbert|morton3]
    python tools/renumber_grid.py --check OLD.grid.nc NEW.grid.nc

Every file made from OUT.grid.nc afterwards (static, init, lbc) is then in the curve's order natively, and the model's
neighbour reads are local, which makes the time loop faster on a regional mesh. Cells go along a
Hilbert curve on (lon, lat) (or a 3-D Morton curve on x, y, z), edges by their first valid cell in the new order (then
the second), vertices by theirs. Every variable is permuted along each nCells, nEdges or nVertices axis; the connectivity variables (cellsOn*, edgesOn*, verticesOn*) get the new numbers, with values outside
1..n (MPAS's 0 for "none") kept; each cell keeps its own order of edges, vertices and neighbours, each edge its two cells and
vertices (the normal's direction) and its edgesOnEdge/weightsOnEdge in their order, each vertex its cells and edges. indexTo*ID
become 1..n in the new order. All other variables and the attributes are copied as they are. OUT.perm.npz beside the new file
holds the permutation (new -> old, 0-based).

--check verifies the new file against the old one: every connectivity round trip holds (edge of a cell has the cell, the
neighbour across an edge is the edge's other cell, vertex of a cell has the cell, ...), and every variable equals the old one
mapped through the permutation (values and connectivity). Exit 0 and CHECK OK, or 1 with the failures.
"""
import sys
from pathlib import Path

import numpy as np
import netCDF4 as nc


def hilbert_index(x, y, order=16):
    """index along a Hilbert curve of x, y in [0, 2**order) (integer arrays)"""
    n = 1 << order
    d = np.zeros(x.shape, dtype=np.int64)
    x = x.astype(np.int64).copy()
    y = y.astype(np.int64).copy()
    s = n >> 1
    while s > 0:
        rx = ((x & s) > 0).astype(np.int64)
        ry = ((y & s) > 0).astype(np.int64)
        d += s * s * ((3 * rx) ^ ry)
        # rotate
        flip = ry == 0
        mirror = flip & (rx == 1)
        fx = np.where(mirror, n - 1 - x, x)
        fy = np.where(mirror, n - 1 - y, y)
        x, y = np.where(flip, fy, fx), np.where(flip, fx, fy)
        s >>= 1
    return d


def morton3(x, y, z, bits=20):
    def spread(v):
        v = v.astype(np.int64) & ((1 << bits) - 1)
        out = np.zeros_like(v)
        for b in range(bits):
            out |= ((v >> b) & 1) << (3 * b)
        return out
    return spread(x) | (spread(y) << 1) | (spread(z) << 2)


def quantise(v, bits):
    lo, hi = float(v.min()), float(v.max())
    return np.clip(((v - lo) / max(hi - lo, 1e-30) * ((1 << bits) - 1)).round(), 0, (1 << bits) - 1).astype(np.int64)


ENT = {'nCells': 'cell', 'nEdges': 'edge', 'nVertices': 'vertex'}
CONN = {'cellsOnCell': 'cell', 'cellsOnEdge': 'cell', 'cellsOnVertex': 'cell',
        'edgesOnCell': 'edge', 'edgesOnEdge': 'edge', 'edgesOnVertex': 'edge',
        'verticesOnCell': 'vertex', 'verticesOnEdge': 'vertex'}
IDS = {'indexToCellID': 'cell', 'indexToEdgeID': 'edge', 'indexToVertexID': 'vertex'}


def permutations(f, curve):
    nc_, ne, nv = (len(f.dimensions[d]) for d in ('nCells', 'nEdges', 'nVertices'))
    if curve == 'morton3':
        key = morton3(*(quantise(f[v][:], 20) for v in ('xCell', 'yCell', 'zCell')))
    else:
        key = hilbert_index(quantise(f['lonCell'][:], 16), quantise(f['latCell'][:], 16))
    pc = np.argsort(key, kind='stable')
    ic = np.empty(nc_, np.int64); ic[pc] = np.arange(nc_)

    def first_cells(conn):
        c = np.asarray(conn, np.int64)
        valid = (c >= 1) & (c <= nc_)
        return np.sort(np.where(valid, ic[np.clip(c - 1, 0, nc_ - 1)], nc_ + 1), axis=1)
    ce = first_cells(f['cellsOnEdge'][:])
    pe = np.lexsort((np.arange(ne), ce[:, 1], ce[:, 0]))
    cv = first_cells(f['cellsOnVertex'][:])
    pv = np.lexsort((np.arange(nv), cv[:, 0]))
    return {'cell': pc, 'edge': pe, 'vertex': pv}


def inverse(p):
    i = np.empty(len(p), np.int64); i[p] = np.arange(len(p)); return i


def renumbered_values(a, inv):
    b = np.array(a, copy=True)
    m = (b >= 1) & (b <= len(inv))
    b[m] = inv[b[m] - 1] + 1
    return b


def raw_values(f):
    f.set_auto_maskandscale(False)
    f.set_auto_chartostring(False)


def permutation_path(path):
    return Path(str(path) + '.perm.npz')


def load_permutations(path):
    with np.load(path) as arrays:
        perms = {}
        for entity in ENT.values():
            if entity not in arrays:
                raise ValueError(f'{path}: missing {entity} permutation')
            p = arrays[entity]
            if (p.ndim != 1 or p.dtype.kind not in 'iu'
                    or not np.array_equal(np.sort(p), np.arange(p.size))):
                raise ValueError(f'{path}: invalid {entity} permutation')
            perms[entity] = p
    return perms


def check_mesh_sizes(f, perms):
    for dim, entity in ENT.items():
        if dim in f.dimensions and len(f.dimensions[dim]) != len(perms[entity]):
            raise ValueError(f'{f.filepath()}: {dim} has size {len(f.dimensions[dim])}, '
                             f'permutation needs {len(perms[entity])}')


def records(f, v):
    if v.dimensions:
        dim = v.dimensions[0]
        if dim not in ENT and (dim == 'Time' or f.dimensions[dim].isunlimited()):
            return range(v.shape[0]), v.dimensions[1:]
    return (Ellipsis,), v.dimensions


def permute(name, dims, a, perms, invs):
    if name in IDS:
        return np.arange(1, len(perms[IDS[name]]) + 1, dtype=a.dtype)
    for axis, dim in enumerate(dims):
        if dim in ENT:
            a = np.take(a, perms[ENT[dim]], axis=axis)
    if name in CONN:
        a = renumbered_values(a, invs[CONN[name]])
    return a


def copy_permuted(f, dst, perms, *, clobber):
    check_mesh_sizes(f, perms)
    invs = {k: inverse(p) for k, p in perms.items()}
    with nc.Dataset(dst, 'w', format=f.file_format, clobber=clobber) as g:
        g.setncatts({a: f.getncattr(a) for a in f.ncattrs()})
        for name, d in f.dimensions.items():
            g.createDimension(name, None if d.isunlimited() else len(d))
        for name, v in f.variables.items():
            fill = v.getncattr('_FillValue') if '_FillValue' in v.ncattrs() else None
            w = g.createVariable(name, v.datatype, v.dimensions, fill_value=fill)
            w.setncatts({a: v.getncattr(a) for a in v.ncattrs() if a != '_FillValue'})
            w.set_auto_maskandscale(False)
            w.set_auto_chartostring(False)
            keys, dims = records(f, v)
            for key in keys:
                w[key] = permute(name, dims, v[key], perms, invs)


def renumber(src, dst, curve='hilbert'):
    with nc.Dataset(src) as f:
        raw_values(f)
        perms = permutations(f, curve)
        copy_permuted(f, dst, perms, clobber=True)
    np.savez(permutation_path(dst), **perms)
    return perms


def apply_permutation(src, dst, perms):
    with nc.Dataset(src) as f:
        raw_values(f)
        copy_permuted(f, dst, perms, clobber=False)


def same_values(a, b):
    a, b = np.asarray(a), np.asarray(b)
    if a.shape != b.shape or a.dtype != b.dtype:
        return False
    if a.dtype.hasobject:
        return np.array_equal(a, b)
    return np.array_equal(a.ravel().view('u1'), b.ravel().view('u1'))


def check_variables(f, g, perms, bad):
    invs = {k: inverse(p) for k, p in perms.items()}
    if f.file_format != g.file_format:
        bad.append(f'file format differs: {f.file_format} vs {g.file_format}')
    if [(d, len(v), v.isunlimited()) for d, v in f.dimensions.items()] != [
            (d, len(v), v.isunlimited()) for d, v in g.dimensions.items()]:
        bad.append('dimensions differ')
    for name in g.variables.keys() - f.variables.keys():
        bad.append(f'unexpected variable {name}')
    for name, v in f.variables.items():
        w = g.variables.get(name)
        if w is None or v.dimensions != w.dimensions or v.shape != w.shape or v.dtype != w.dtype:
            bad.append(f'variable {name} is missing or its dimensions or dtype differ')
            continue
        keys, dims = records(f, v)
        for key in keys:
            expect = permute(name, dims, v[key], perms, invs)
            if not same_values(expect, w[key]):
                record = '' if key is Ellipsis else f' record {key}'
                bad.append(f'variable {name}{record} differs from the old one through the permutation')
                break


def check_connectivity(g, bad):
    required = ('nEdgesOnCell', 'edgesOnCell', 'cellsOnCell', 'verticesOnCell',
                'cellsOnEdge', 'verticesOnEdge', 'cellsOnVertex', 'edgesOnVertex')
    if not all(v in g.variables for v in required) or not all(d in g.dimensions for d in ENT):
        return
    n = {k: len(g.dimensions[d]) for d, k in ENT.items()}
    noc = np.asarray(g['nEdgesOnCell'][:])
    eoc, coc, voc = (np.asarray(g[v][:]) for v in ('edgesOnCell', 'cellsOnCell', 'verticesOnCell'))
    coe, voe = np.asarray(g['cellsOnEdge'][:]), np.asarray(g['verticesOnEdge'][:])
    cov, eov = np.asarray(g['cellsOnVertex'][:]), np.asarray(g['edgesOnVertex'][:])
    for c in range(n['cell']):
        for j in range(noc[c]):
            e = eoc[c, j]
            if not 1 <= e <= n['edge']:
                continue
            pair = coe[e - 1]
            if c + 1 not in pair:
                bad.append('cell %d: edge %d does not have the cell' % (c + 1, e)); continue
            other = pair[1] if pair[0] == c + 1 else pair[0]
            if coc[c, j] != other and not (other < 1 or other > n['cell']):
                bad.append('cell %d: neighbour %d across edge %d is not the edge\'s other cell %d' % (c + 1, coc[c, j], e, other))
            vtx = voc[c, j]
            if 1 <= vtx <= n['vertex'] and c + 1 not in cov[vtx - 1]:
                bad.append('cell %d: vertex %d does not have the cell' % (c + 1, vtx))
        if len(bad) > 20:
            break
    for v in range(n['vertex']):
        for j in range(eov.shape[1]):
            e = eov[v, j]
            if 1 <= e <= n['edge'] and v + 1 not in voe[e - 1]:
                bad.append('vertex %d: edge %d does not have the vertex' % (v + 1, e))
        if len(bad) > 20:
            break


def check(old, new, perm=None):
    bad = []
    with nc.Dataset(old) as f, nc.Dataset(new) as g:
        raw_values(f)
        raw_values(g)
        if perm is None:
            sidecar = permutation_path(new)
            perm = load_permutations(sidecar) if sidecar.is_file() else permutations(f, 'hilbert')
        try:
            check_mesh_sizes(f, perm)
            check_mesh_sizes(g, perm)
        except ValueError as err:
            bad.append(str(err))
        else:
            check_variables(f, g, perm, bad)
            check_connectivity(g, bad)
    if bad:
        print('\n'.join(bad[:20])); print('CHECK FAILED (%d)' % len(bad)); return 1
    print('CHECK OK'); return 0


def main():
    if '--help' in sys.argv or '-h' in sys.argv:
        print(__doc__)
        return
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    if '--check' in sys.argv:
        sys.exit(check(args[0], args[1]))
    curve = next((a.split('=', 1)[1] for a in sys.argv[1:] if a.startswith('--curve=')), 'hilbert')
    renumber(args[0], args[1], curve)
    sys.exit(check(args[0], args[1]))


if __name__ == '__main__':
    main()
