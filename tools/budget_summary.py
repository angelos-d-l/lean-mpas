"""The water and energy ledger of a LEAN_BUDGET run.

    budget_summary.py RUN_OUT FIRST_STEP LAST_STEP DT_SECONDS

RUN_OUT is the run's standard output, with one `budget <step> <47 values>` line per step (src/lean_model.f90). Prints
global means per scheme over steps FIRST_STEP up to LAST_STEP: what each scheme hands the dynamics against what its
boundary terms say it should. FIRST_STEP '-' leaves out the first 2 h of model time as spin-up (step 1 + 7200 /
DT_SECONDS), LAST_STEP '-' is the last step. DT_SECONDS is the run's config_dt, for the rates between steps; it is
required, since every rate scales with it. Reads only; docs/water-energy-ledger.md explains the lines.
"""
import argparse
import sys

import numpy as np

COLS = ('step W Eint Epot Ekin Elat Eice pbl_qv pbl_qc pbl_qi pbl_h gf_qv gf_qc gf_qi gf_h mp_h lw_h sw_h wind_work '
        'qfx hfx lh gf_rain rainnc snownc graupelnc sw_top sw_sfc lw_top lw_sfc diss sw_mtop lw_mtop '
        'o_qfx o_hfx o_lh o_gfrain o_sw o_lw l_qfx l_hfx l_lh l_gfrain l_sw l_lw water lw_top_clr sw_top_clr').split()
LV, LF, DAY = 2.5e6, 3.34e5, 86400.


def read(path):
    rows = [[float(x) for x in l.split()[1:]] for l in open(path) if l.startswith('budget ')]
    return {n: np.array(v) for n, v in zip(COLS, np.array(rows).T)}


SPIN_UP_SECONDS = 7200.


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('run_out', metavar='RUN_OUT')
    ap.add_argument('first', metavar='FIRST_STEP', help="the first step to count, or '-' (after the first 2 h)")
    ap.add_argument('last', metavar='LAST_STEP', help="the step to stop at, or '-' (the last step)")
    ap.add_argument('dt', metavar='DT_SECONDS', type=float, help="the run's config_dt, s")
    a = ap.parse_args()
    if a.dt <= 0:
        sys.exit('budget_summary.py: DT_SECONDS must be positive')
    b = read(a.run_out)
    first = 1 + round(SPIN_UP_SECONDS / a.dt) if a.first == '-' else int(a.first)
    last = int(b['step'].max()) if a.last == '-' else int(a.last)
    pick = (b['step'] >= first) & (b['step'] < last)
    dstep = a.dt
    mean = lambda v: float(np.mean(v[pick]))
    rate = lambda n: np.append(np.diff(b[n]) / dstep, np.nan)          # per second, step n to n+1
    print(f'{int(pick.sum())} steps from {first} to {last}, dt {dstep:.0f} s')
    print(f"water in the air {b['W'][pick][0]:.3f} -> {b['W'][pick][-1]:.3f} kg/m2")

    pbl_w = b['pbl_qv'] + b['pbl_qc'] + b['pbl_qi']
    gf_w = b['gf_qv'] + b['gf_qc'] + b['gf_qi']
    mp_p = rate('rainnc')                                                # kg/m2/s, Thompson's surface precipitation
    dw = rate('W')
    print('\nWATER, mm/day')
    print(f"  evaporation qfx                {mean(b['qfx']) * DAY:9.4f}")
    print(f"  PBL water tendency             {mean(pbl_w) * DAY:9.4f}   residual against qfx {mean(pbl_w - b['qfx']) * DAY:+9.4f}")
    print(f"  GF rain                        {mean(b['gf_rain']) * DAY:9.4f}")
    print(f"  GF water tendency              {mean(gf_w) * DAY:9.4f}   residual, tendency + rain {mean(gf_w + b['gf_rain']) * DAY:+9.4f}")
    print(f"  Thompson surface precipitation {mean(mp_p) * DAY:9.4f}")
    print(f"  dW/dt                          {mean(dw) * DAY:9.4f}")
    print(f"  dynamics + Thompson residual   {mean(dw - pbl_w - gf_w + mp_p) * DAY:+9.4f}   dW/dt - tendencies + Thompson precipitation")

    print('\nENERGY, W/m2')
    print(f"  SW top net {mean(b['sw_top']):8.2f}   LW top net {mean(b['lw_top']):8.2f}   top balance {mean(b['sw_top'] + b['lw_top']):+8.2f}")
    print(f"  clear sky: OLR {-mean(b['lw_top_clr']):8.2f}   SW top net {mean(b['sw_top_clr']):8.2f}   "
          f"cloud effect LW {mean(b['lw_top'] - b['lw_top_clr']):+8.2f}   SW {mean(b['sw_top'] - b['sw_top_clr']):+8.2f}")
    print(f"  SW sfc net {mean(b['sw_sfc']):8.2f}   LW sfc net {mean(b['lw_sfc']):8.2f}   sfc net rad {mean(b['sw_sfc'] + b['lw_sfc']):+8.2f}")
    print(f"  SH {mean(b['hfx']):7.2f}   LH {mean(b['lh']):7.2f}   Lv*qfx {mean(LV * b['qfx']):7.2f}   surface balance {mean(b['sw_sfc'] + b['lw_sfc'] - b['hfx'] - b['lh']):+8.2f}")
    print(f"  radiation: SW heating {mean(b['sw_h']):8.3f} against model-top flux divergence {mean(b['sw_mtop'] - b['sw_sfc']):8.3f}, residual {mean(b['sw_h'] - b['sw_mtop'] + b['sw_sfc']):+7.3f}; absorbed above the model {mean(b['sw_top'] - b['sw_mtop']):7.3f}")
    print(f"             LW heating {mean(b['lw_h']):8.3f} against model-top flux divergence {mean(b['lw_mtop'] - b['lw_sfc']):8.3f}, residual {mean(b['lw_h'] - b['lw_mtop'] + b['lw_sfc']):+7.3f}; lost above the model {mean(b['lw_top'] - b['lw_mtop']):7.3f}")
    pbl_mse = b['pbl_h'] + LV * (b['pbl_qv']) - LF * b['pbl_qi']
    print(f"  PBL: heating {mean(b['pbl_h']):8.3f}, moist static energy {mean(pbl_mse):8.3f} against SH + LH {mean(b['hfx'] + b['lh']):8.3f}, residual {mean(pbl_mse - b['hfx'] - b['lh']):+7.3f}")
    gf_mse = b['gf_h'] + LV * b['gf_qv'] - LF * b['gf_qi']
    print(f"  GF: heating {mean(b['gf_h']):8.3f} (Lv*rain {mean(LV * b['gf_rain']):8.3f}), moist static energy residual {mean(gf_mse):+7.3f}")
    print(f"  Thompson heating {mean(b['mp_h']):8.3f} (Lv*precipitation {mean(LV * mp_p):8.3f})")
    print(f"  wind work of PBL + GWDO {mean(b['wind_work']):8.3f}, MYNN dissipative heating {mean(b['diss']):8.3f}")
    print(f"  over open water ({b['water'][0] * 100:.1f} % of the globe): E {mean(b['o_qfx']) * DAY:6.3f} mm/day, SH {mean(b['o_hfx']):6.2f}, LH {mean(b['o_lh']):6.2f}, SW {mean(b['o_sw']):7.2f}, LW {mean(b['o_lw']):7.2f}, balance {mean(b['o_sw'] + b['o_lw'] - b['o_hfx'] - b['o_lh']):+7.2f}, GF rain {mean(b['o_gfrain']) * DAY:6.3f}")
    print(f"  over the rest: E {mean(b['l_qfx']) * DAY:6.3f} mm/day, SH {mean(b['l_hfx']):6.2f}, LH {mean(b['l_lh']):6.2f}, SW {mean(b['l_sw']):7.2f}, LW {mean(b['l_lw']):7.2f}, balance {mean(b['l_sw'] + b['l_lw'] - b['l_hfx'] - b['l_lh']):+7.2f}, GF rain {mean(b['l_gfrain']) * DAY:6.3f}")
    etot = b['Eint'] + b['Epot'] + b['Ekin']
    emoist = etot + b['Elat'] - b['Eice']
    de = np.append(np.diff(etot) / dstep, np.nan)
    dem = np.append(np.diff(emoist) / dstep, np.nan)
    src = b['sw_h'] + b['lw_h'] + b['pbl_h'] + b['gf_h'] + b['mp_h'] + b['wind_work']
    print(f"  d(internal+potential+kinetic)/dt {mean(de):8.3f}, physics applied {mean(src):8.3f}, residual {mean(de - src):+7.3f}")
    print(f"  d(moist total)/dt {mean(dem):8.3f}, top net {mean(b['sw_top'] + b['lw_top']):8.3f} - sfc rad {mean(b['sw_sfc'] + b['lw_sfc']):8.3f} + SH + LH {mean(b['hfx'] + b['lh']):8.3f}: residual {mean(dem - b['sw_top'] - b['lw_top'] + b['sw_sfc'] + b['lw_sfc'] - b['hfx'] - b['lh']):+7.3f}")
    print(f"  (the energy terms: Eint {b['Eint'][pick][0]:.6e}, Epot {b['Epot'][pick][0]:.6e}, Ekin {b['Ekin'][pick][0]:.4e}, Elat {b['Elat'][pick][0]:.4e}, Eice {b['Eice'][pick][0]:.4e} J/m2)")


if __name__ == '__main__':
    main()
