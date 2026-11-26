"""Tables of the single-column runs of tests/mynn_moisture/run.sh: lean's surface-moisture switches against MPAS 8.4.2.

  python3 tests/mynn_moisture/summarize.py build/mynn_moisture

Checks (exit 1 if one fails): switches off, host and card sources equal MPAS in every word; the mutation is seen; the
card sources give the same trace as the host sources with all switches on; LEAN_SFC_Q2MR changes only water q2, by
exactly qsfc/(1-qsfc) - qsfc; with LEAN_MYNN_MOIST the snow tendency is exactly zero.
"""
import sys
from pathlib import Path
import numpy as np

out = Path(sys.argv[1])
REGIMES = 'day calm_night dew windy water_warm water_cool snow stratus'.split()
WATER = {'water_warm', 'water_cool', 'stratus'}
(T2, Q2, TH2, HFX, QFX, LH, UST, TSK, QSFC, QV1, TH1, QC1, PBLH, PSFC, F, W, C, ES, Q2L, VAP, COND, MAXMF, U10, V10,
 TQS, CS, RHOFAC, T2L, ULPS) = range(29)
bad = []


def trace(name, side='lean'):
    return np.fromfile(out / name / ('trace_%s.bin' % side), dtype='<f8').reshape(len(REGIMES), -1, 29)


def log(name):
    return (out / name / 'run.log').read_text()


def dewpoint(q, p):
    e = np.maximum(q, 1e-8) * p / (0.622 + np.maximum(q, 1e-8))   # tools/products_to_icon.py vapour_pressure, dewpoint
    x = np.log(np.maximum(e, 1e-3) / 611.2)
    return 273.15 + 243.5 * x / (17.67 - x)


print('## Switches off: lean against MPAS, word for word\n')
for name, label in (('off_host', 'host sources'), ('off_card', 'card sources')):
    last = [l for l in log(name).splitlines() if l.startswith('all regimes')]
    print('-', label + ':', last[0] if last else 'NO RESULT')
    if not last or not last[0].endswith(' 0 differ'):
        bad.append(name)
print()

print('## Mutation (lean theta(1) one ulp higher at the start of each regime, host sources)\n')
mlog = (out / 'off_host' / 'run_mutate.log').read_text()
print('\n'.join('    ' + l for l in mlog.splitlines() if 'differ' in l and l[:4] != '    '))
if mlog.strip().splitlines()[-1].endswith(' 0 differ'):
    bad.append('mutation not seen')
print()

print('## Card sources against host sources, all switches on (traces)\n')
same = np.array_equal(trace('hyd_dew_all'), trace('hyd_dew_all_card')) and np.array_equal(trace('hyd_dew_all', 'mpas'), trace('hyd_dew_all_card', 'mpas'))
print('- lean trace of the card sources equals the host trace:', same)
if not same:
    bad.append('card != host with switches on')
print()

print('## The conversions alone (10,000 random columns, a zero MYNN tendency in)\n')
for name in ('hyd_dew', 'hyd_dew_moist'):
    print('-', name + ':')
    print('\n'.join('    ' + l for l in log(name).splitlines() if l.startswith('conversions')))
print()


def pct(x, f):
    return 100. * x / f


print('## Water budget of the PBL step, sums over 3 days, in % of sum |dt*qfx|\n')
print('F = dt*qfx, W = vapour added at fixed dry density, C = condensate added, Es/Cs = MYNN\'s own change of the')
print('specific contents at its fixed moist density. "solver" = Es+Cs-F, "conversion" = (W+C)-(Es+Cs), "total" = W+C-F.\n')
print('The last column is the median per-step |total| in units of one float32 ulp of every layer\'s specific humidity')
print('(as mass, sum rho_m*dz*spacing(s)): about 1 or less means single-precision rounding of the step.\n')
print('| regime | sum dt*qfx kg/m2 | build | total | solver | conversion | vapour W-F | median per step |total|/ulps |')
print('|---|---|---|---|---|---|---|---|')
for i, reg in enumerate(REGIMES):
    for name, side, label in (('off_host', 'mpas', 'MPAS'), ('hyd_dew', 'lean', 'HYD_PS+DEW'),
                              ('hyd_dew_rhosfc', 'lean', '+RHOSFC'), ('hyd_dew_moist', 'lean', '+MOIST'),
                              ('hyd_dew_rhosfc_moist', 'lean', '+RHOSFC+MOIST')):
        x = trace(name, side)[i]
        fa = np.abs(x[:, F]).sum()
        tot = x[:, W] + x[:, C] - x[:, F]
        step_max = np.median(np.abs(tot) / x[:, ULPS])
        print('| %s | %.3f | %s | %+.3f | %+.3f | %+.3f | %+.3f | %.2f |' % (
            reg if label == 'MPAS' else '', x[:, F].sum(), label, pct(tot.sum(), fa),
            pct((x[:, ES] + x[:, CS] - x[:, F]).sum(), fa), pct((x[:, W] + x[:, C] - x[:, ES] - x[:, CS]).sum(), fa),
            pct((x[:, W] - x[:, F]).sum(), fa), step_max))
print()

print('## Condensate the conversion removes (C - Cs), kg/m2 per day, dt 30 s and 18 s\n')
print('| regime | dt 30 | dt 18 | ratio 18/30 | +MOIST dt 30 | +MOIST dt 18 |')
print('|---|---|---|---|---|---|')
for i, reg in enumerate(REGIMES):
    v = []
    for name in ('hyd_dew', 'hyd_dew_dt18', 'hyd_dew_moist', 'hyd_dew_moist_dt18'):
        x = trace(name)[i]
        v.append((x[:, C] - x[:, CS]).sum() / 3.)
    print('| %s | %.2e | %.2e | %s | %.1e | %.1e |' % (reg, v[0], v[1], '%.2f' % (v[1] / v[0]) if v[0] else '-', v[2], v[3]))
print()

tqs_moist = max(np.abs(trace(n)[:, :, TQS]).max() for n in ('hyd_dew_moist', 'hyd_dew_rhosfc_moist', 'hyd_dew_all'))
tqs_base = np.abs(trace('hyd_dew')[:, :, TQS]).max()
print('- largest |snow tendency| of the PBL: HYD_PS+DEW %.2e /s, with LEAN_MYNN_MOIST %.1e /s' % (tqs_base, tqs_moist))
if tqs_moist != 0:
    bad.append('snow tendency with MOIST')

print('\n## LEAN_SFC_Q2MR: what changes\n')
a, b = trace('hyd_dew'), trace('hyd_dew_q2mr')
other = [k for k in range(29) if k != Q2]
print('- every trace field but q2 identical to HYD_PS+DEW:', np.array_equal(a[:, :, other], b[:, :, other]))
if not np.array_equal(a[:, :, other], b[:, :, other]):
    bad.append('Q2MR changes more than q2')
for i, reg in enumerate(REGIMES):
    d = b[i, :, Q2] - a[i, :, Q2]
    q = a[i, :, QSFC].astype(np.float32)
    want = (q / (np.float32(1) - q)).astype(np.float64) - q
    if reg in WATER:
        print('- %s: q2 up by %.3f g/kg on average (%.2f %%); largest difference from qsfc/(1-qsfc) - qsfc: %.1e kg/kg' % (
            reg, 1e3 * d.mean(), 100 * (d / a[i, :, Q2]).mean(), np.abs(d - want).max()))
        # rounding of q2 = x - y in single precision is half an ulp of x; anything larger is not this formula
        if np.abs(d - want).max() > 3e-9:   # three float32 roundings of numbers near 0.01 (ulp 9.3e-10)
            bad.append('Q2MR difference in ' + reg)
    elif np.any(d != 0):
        bad.append('Q2MR changes land q2 in ' + reg)
print()

print('## Effect per regime against the HYD_PS and DEW switches alone, 3 days\n')
print('Mean difference over the 3 days, and in brackets the largest absolute difference of a step.\n')
print('"control" is the HYD_PS+DEW build with the skin 0.01 K warmer, not a switch: a change of the size the switches make,')
print('to show how far the column itself amplifies small differences.\n')
print('| regime | switches | T2 K | Q2 g/kg | Td2 K | HFX W/m2 | LH W/m2 | qv(1) g/kg | sum qfx mm |')
print('|---|---|---|---|---|---|---|---|---|')
base = trace('hyd_dew')
for i, reg in enumerate(REGIMES):
    for name, side, label in (('hyd_dew_rhosfc', 'lean', 'RHOSFC'), ('hyd_dew_moist', 'lean', 'MOIST'),
                              ('hyd_dew_rhosfc_moist', 'lean', 'RHOSFC+MOIST'), ('hyd_dew_q2mr', 'lean', 'Q2MR'),
                              ('hyd_dew_all', 'lean', 'all three'), ('hyd_dew', 'lean_perturb', 'control')):
        x, y = base[i], trace(name, side)[i]
        td = dewpoint(y[:, Q2], y[:, PSFC]) - dewpoint(x[:, Q2], x[:, PSFC])
        cols = []
        for d, s in ((y[:, T2] - x[:, T2], 1), (y[:, Q2] - x[:, Q2], 1e3), (td, 1), (y[:, HFX] - x[:, HFX], 1),
                     (y[:, LH] - x[:, LH], 1), (y[:, QV1] - x[:, QV1], 1e3)):
            cols.append('%+.3f (%.3f)' % (s * d.mean(), s * np.abs(d).max()))
        cols.append('%+.3f' % ((y[:, F] - x[:, F]).sum()))
        print('| %s | %s | %s |' % (reg if label == 'RHOSFC' else '', label, ' | '.join(cols)))
print()

print('## Water-cell q2 against MYNN\'s own 2 m humidity (the surface layer\'s q2, a mixing ratio)\n')
for i, reg in enumerate(REGIMES):
    if reg in WATER:
        for name in ('hyd_dew', 'hyd_dew_q2mr'):
            x = trace(name)[i]
            cap = np.mean(x[:, Q2L] == (np.float32(1.05) * x[:, QV1].astype(np.float32)).astype(np.float64))
            print('- %s, %s: diagnostic q2 / MYNN q2 - 1 = %+.2f %% on average (MYNN q2 at its 1.05*qv(1) cap in %.0f %% of steps)' % (
                reg, name, 100 * (x[:, Q2] / x[:, Q2L] - 1).mean(), 100 * cap))
print()
print('CHECKS:', 'all passed' if not bad else 'FAILED: ' + ', '.join(bad))
sys.exit(1 if bad else 0)
