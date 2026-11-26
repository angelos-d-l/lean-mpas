"""Checks of the GF build switches on the records of tests/suite_coupling/gf_switches.f90 (one directory per build).

  python3 tests/suite_coupling/gf_switches.py OUT DXMIN

LEAN_GF_NOSHALLOW: no shallow plume anywhere (xmb_shallow, k22/kbcon/ktop_shallow 0); the deep plume (xmb_total, rain,
hbot, htop) bit-identical to the build without the switch; records without a shallow plume bit-identical except qc_cu/qi_cu
(gdc/gdc2: the shallow plume writes its in-plume cloud water even when its mass flux is 0; a diagnostic no scheme reads);
records with only a shallow plume all-zero tendencies and in-plume water. LEAN_GF_DXMIN: records with dx < DXMIN as GF never ran (zero tendencies,
no rain, hbot = nz, htop = 1), the others bit-identical. Every build finite; card sources = host sources; both mutants
must fail. Exit 1 on any failure.
"""
import sys
from pathlib import Path
import numpy as np

out, dxmin = Path(sys.argv[1]), float(sys.argv[2])
NZ = 45
rec = np.dtype([('dx', '<f4'), ('hbot', '<f4'), ('htop', '<f4'), ('xmb', '<f4'), ('xmbs', '<f4'), ('rain', '<f4'),
                ('prate', '<f4'), ('k22s', '<i4'), ('kbcons', '<i4'), ('ktops', '<i4'), ('th', '<f4', NZ), ('qv', '<f4', NZ),
                ('qc', '<f4', NZ), ('qi', '<f4', NZ), ('gdc', '<f4', NZ), ('gdc2', '<f4', NZ)])
PROFILES = ['th', 'qv', 'qc', 'qi', 'gdc', 'gdc2']


def load(name):
    return np.fromfile(out / name / 'gf-switches.bin', dtype=rec)


def same(a, b, fields):
    """records (bool array) where all fields are bit-identical"""
    ok = np.ones(len(a), bool)
    for f in fields:
        x, y = a[f].view(np.int32 if a[f].dtype.kind == 'f' else a[f].dtype), b[f].view(np.int32 if b[f].dtype.kind == 'f' else b[f].dtype)
        ok &= (x == y).reshape(len(a), -1).all(axis=1)
    return ok


ALL = list(rec.names)
DEEP = ['hbot', 'htop', 'xmb', 'rain', 'prate']


def noshallow_failures(off, on):
    bad = []
    if (on['xmbs'] != 0).any() or (on['k22s'] != 0).any() or (on['kbcons'] != 0).any() or (on['ktops'] != 0).any():
        bad.append('a shallow plume is left')
    if not same(off, on, DEEP).all():
        bad.append('the deep plume changed in %d records' % (~same(off, on, DEEP)).sum())
    quiet = (off['xmbs'] == 0) & (off['k22s'] == 0)
    if not same(off, on, [f for f in ALL if f not in ('gdc', 'gdc2')])[quiet].all():
        bad.append('records without a shallow plume changed')
    only = (off['xmbs'] > 0) & (off['prate'] == 0)
    if any((on[p][only] != 0).any() for p in PROFILES):
        bad.append('shallow-only records keep tendencies')
    return bad


def dxmin_failures(off, on):
    bad = []
    fine = off['dx'] < dxmin
    if not same(off, on, ALL)[~fine].all():
        bad.append('coarse records (dx >= %g) changed' % dxmin)
    f = on[fine]
    if any((f[p] != 0).any() for p in PROFILES) or (f['rain'] != 0).any() or (f['xmb'] != 0).any() or (f['xmbs'] != 0).any() \
       or (f['hbot'] != NZ).any() or (f['htop'] != 1).any():
        bad.append('fine records (dx < %g) still have GF output' % dxmin)
    return bad


fail = 0
runs = {n: load(n) for n in ['off', 'off_card', 'noshallow', 'noshallow_card', 'dxmin', 'dxmin_card', 'both',
                             'noshallow_mutant', 'dxmin_mutant']}
for n, r in runs.items():
    finite = all(np.isfinite(r[f]).all() for f in ALL if r[f].dtype.kind == 'f')
    print('%-17s records %d, finite %s' % (n, len(r), finite))
    fail += not finite
off = runs['off']
print('without the switches: deep plume in %d records, shallow plume in %d (%d shallow only), %d records with dx < %g' %
      ((off['xmb'] > 0).sum(), (off['xmbs'] > 0).sum(), ((off['xmbs'] > 0) & (off['prate'] == 0)).sum(), (off['dx'] < dxmin).sum(), dxmin))
for host, card in [('off', 'off_card'), ('noshallow', 'noshallow_card'), ('dxmin', 'dxmin_card')]:
    ok = same(runs[host], runs[card], ALL).all()
    print('card sources = host sources (%s): %s' % (host, ok)); fail += not ok
on = runs['noshallow']
changed = ~same(off, on, ALL)
# the effect: in each record, the largest change of the theta and qv tendencies at any level when the shallow plume is dropped
dth = np.abs(off['th'] - on['th']).max(axis=1)
dqv = np.abs(off['qv'] - on['qv']).max(axis=1)
print('LEAN_GF_NOSHALLOW: %d records change; max |d theta tendency| %.3e K/s, max |d qv tendency| %.3e kg/kg/s' %
      (changed.sum(), dth.max(), dqv.max()))
only = (off['xmbs'] > 0) & (off['prate'] == 0)
print('  shallow-only records (%d), what the switch removes, mean over the records, by model level:' % only.sum())
for lo, hi in [(0, 4), (4, 8), (8, 14), (14, 20)]:
    print('    levels %2d-%2d: theta tendency %+.3e K/s, qv tendency %+.3e kg/kg/s' %
          (lo + 1, hi, off['th'][only, lo:hi].mean(), off['qv'][only, lo:hi].mean()))
b = noshallow_failures(off, on); print('  checks:', 'pass' if not b else 'FAIL ' + '; '.join(b)); fail += bool(b)
b = noshallow_failures(off, runs['noshallow_mutant']); print('  mutant (deep plume dropped too) caught:', bool(b), '; '.join(b)); fail += not b
on = runs['dxmin']
print('LEAN_GF_DXMIN=%g: %d records change' % (dxmin, (~same(off, on, ALL)).sum()))
b = dxmin_failures(off, on); print('  checks:', 'pass' if not b else 'FAIL ' + '; '.join(b)); fail += bool(b)
b = dxmin_failures(off, runs['dxmin_mutant']); print('  mutant (dx test reversed) caught:', bool(b), '; '.join(b)); fail += not b
both = runs['both']
fine = off['dx'] < dxmin
ok = not dxmin_failures(runs['noshallow'], both) and same(runs['noshallow'], both, ALL)[~fine].all()
print('both switches = LEAN_GF_NOSHALLOW where dx >= %g, nothing elsewhere: %s' % (dxmin, ok)); fail += not ok
print('RESULT', 'PASS' if not fail else 'FAIL')
sys.exit(1 if fail else 0)
