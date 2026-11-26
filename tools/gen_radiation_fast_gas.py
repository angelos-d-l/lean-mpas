"""Writes src/device/radiation_fast_gas.f90: the RRTMG gas optics (taugb1..16 of the LW, taumol16..29 of the SW) for the
g-point path of the card (src/device/radiation_fast.f90), one thread per (column, band, layer), and the layer
coefficients of one column (setcoef, setcoef_sw).

Each routine is the device routine of src/device/radiation_lw_kernels.f90 / radiation_sw_kernels.f90 with
  - the arguments replaced by the group arrays of radiation_fast (slot index j first), so no array descriptors are passed;
  - its two layer loops restricted to the one layer lsel (each iteration of those loops is independent);
  - in the SW, the solar-source layer laysolfr: the serial loop writes sfluxzen at the iterations where lay equals the
    running laysolfr, the last write wins; each thread first replays that integer scan (lastw) and writes only if it is
    lastw. Same final values.
The arithmetic lines are copied unchanged.

    python3 tools/gen_radiation_fast_gas.py           write
    python3 tools/gen_radiation_fast_gas.py --check   write nothing; print DIFFERS <path> and exit 1 if the file differs
"""
import re
import sys
from pathlib import Path

root = Path(__file__).resolve().parents[1]
out = root / 'src/device/radiation_fast_gas.f90'

LW = {  # dummy -> (group array, trailing index) ; scalars -> expression
    'pavel': ('lw_lay', 1), 'coldry': ('lw_lay', 3), 'wx': ('lw_wx', None),
    'jp': ('lw_ji', 1), 'jt': ('lw_ji', 2), 'jt1': ('lw_ji', 3), 'indself': ('lw_ji', 4), 'indfor': ('lw_ji', 5),
    'indminor': ('lw_ji', 6), 'planklay': ('lw_planklay', None), 'planklev': ('lw_planklev', None),
    'plankbnd': ('lw_plankbnd', None), 'fracs': ('lw_fracs', None), 'taug': ('lw_taug', None)}
for k, n in enumerate('colh2o colco2 colo3 coln2o colco colch4 colo2 colbrd fac00 fac01 fac10 fac11 rat_h2oco2 rat_h2oco2_1 '
                      'rat_h2oo3 rat_h2oo3_1 rat_h2on2o rat_h2on2o_1 rat_h2och4 rat_h2och4_1 rat_n2oco2 rat_n2oco2_1 rat_o3co2 '
                      'rat_o3co2_1 selffac selffrac'.split(), 1):
    LW[n] = ('lw_jr', k)
LW.update({'forfac': ('lw_jr', 27), 'forfrac': ('lw_jr', 28), 'minorfrac': ('lw_jr', 29), 'scaleminor': ('lw_jr', 30),
           'scaleminorn2': ('lw_jr', 31)})
LW_SCALAR = {'nlayers': 'lw_nlay(j)', 'laytrop': 'lw_laytrop(j)'}
SW = {'jp': ('sw_ji', 1), 'jt': ('sw_ji', 2), 'jt1': ('sw_ji', 3), 'indself': ('sw_ji', 4), 'indfor': ('sw_ji', 5),
      'sfluxzen': ('sw_sflx', None), 'taug': ('lw_taug', None), 'taur': ('lw_fracs', None)}   # the SW stages reuse the LW arrays
for k, n in enumerate('colh2o colco2 colo3 coln2o colch4 colo2 colmol co2mult fac00 fac01 fac10 fac11 selffac selffrac '
                      'forfac forfrac'.split(), 1):
    SW[n] = ('sw_jr', k)
SW_SCALAR = {'nlayers': 'sw_nlay(j)', 'laytrop': 'sw_laytrop(j)'}


def split_args(text):
    args, cur, depth = [], '', 0
    for ch in text:
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        if ch == ',' and depth == 0:
            args.append(cur)
            cur = ''
        else:
            cur += ch
    args.append(cur)
    return args


def transform(line, arrays, scalars):
    pat = re.compile(r'\b(%s)\s*\(' % '|'.join(sorted(arrays, key=len, reverse=True)))
    res, i = '', 0
    while True:
        m = pat.search(line, i)
        if not m:
            res += line[i:]
            break
        res += line[i:m.start()]
        k, depth = m.end(), 1
        while depth:
            if line[k] == '(':
                depth += 1
            elif line[k] == ')':
                depth -= 1
            k += 1
        inner = transform(line[m.end():k - 1], arrays, scalars)
        name, extra = arrays[m.group(1)]
        args = ['j'] + [a.strip() for a in split_args(inner)] + ([str(extra)] if extra else [])
        res += '%s(%s)' % (name, ','.join(args))
        i = k
    for n, e in scalars.items():
        res = re.sub(r'\b%s\b' % n, e, res)
    return res


def routines(path, prefix):
    lines = path.read_text().split('\n')
    found = {}
    i = 0
    while i < len(lines):
        m = re.match(r'^subroutine (%s(?:\d+)?)\(' % prefix, lines[i])
        if m:
            j = i
            while not lines[j].startswith('end subroutine'):
                j += 1
            found[m.group(1)] = lines[i:j + 1]
            i = j
        i += 1
    return found


def convert(name, body, arrays, scalars):
    out = [' subroutine rf_%s(j, lsel)' % name]
    rest = body[1:-1]
    head = [s for s in rest if re.match(r'^\s*use\b', s, re.I)]
    out += [' ' + s.strip() for s in head]
    out += [' !$acc routine seq', ' integer, intent(in) :: j, lsel']
    decl, code, seen_code = [], [], False
    for s in rest:
        if re.match(r'^\s*use\b', s, re.I) or '!$acc routine' in s or not s.strip() or s.strip().startswith('!'):
            continue
        if 'intent(' in s.replace(' ', '').lower() or re.match(r'^\s*implicit\s+none', s, re.I):
            continue
        if not seen_code and re.match(r'^\s*(integer|real|logical|character|data)\b', s, re.I):
            decl.append(' ' + s.strip())
            continue
        seen_code = True
        code.append(s)
    solar = any('laysolfr' in s for s in code)
    if solar:
        decl.append(' integer :: lastw')
    out += decl
    # the laysolfr scan: init line, the crossing line and the loop it sits in
    if solar:
        init = next(s for s in code if re.match(r'^\s*laysolfr\s*=', s))
        cross = next((s for s in code if re.match(r'^\s*if\s*\(jp\(', s) and 'laysolfr' in s), None)
        wi = next(k for k, s in enumerate(code) if re.search(r'lay\s*\.eq\.\s*laysolfr', s))
        loop = next(code[k] for k in range(wi, -1, -1) if re.match(r'^\s*do lay\s*=', code[k]))
        out += [' ! the layer of the last sfluxzen write of the serial loop',
                ' lastw = -1', ' ' + transform(init.strip(), arrays, scalars),
                ' ' + transform(loop.strip(), arrays, scalars)]
        if cross is not None:
            out.append(' ' + transform(cross.strip(), arrays, scalars))
        out += [' if (lay .eq. laysolfr) lastw = lay', ' enddo']
    for s in code:
        st = s.strip()
        if solar and (s is init or s is cross):
            continue
        m = re.match(r'^do lay\s*=\s*(.+?)\s*,\s*(.+)$', st)
        if m:
            st = 'do lay = max(%s, lsel), min(%s, lsel)' % (m.group(1), m.group(2))
        if solar:
            st = re.sub(r'lay\s*\.eq\.\s*laysolfr', 'lay .eq. lastw', st)
        out.append(' ' + transform(st, arrays, scalars))
    out.append(' end subroutine')
    return out


def convert_inputs(name, body, arrays, scalars, local_scalars=()):
    """setcoef / setcoef_sw for one column slot j: the inputs stay arguments, the outputs are the group arrays (no local copies,
    no array sections passed); the per-column scalars in local_scalars become locals"""
    head = body[0]
    m = re.match(r'^subroutine\s+(\w+)\s*\((.*)\)\s*$', head)
    args = [a.strip() for a in m.group(2).split(',') if a.strip()]
    keep = [a for a in args if a not in arrays and a not in scalars and a not in local_scalars]
    out = [' subroutine rf_%s(j, %s)' % (name, ', '.join(keep))]
    rest = body[1:-1]
    out += [' ' + x.strip() for x in rest if re.match(r'^\s*use\b', x, re.I)]
    out += [' !$acc routine seq', ' integer, intent(in) :: j']
    seen_code = False
    for x in rest:
        st = x.strip()
        if re.match(r'^\s*use\b', x, re.I) or '!$acc routine' in x or not st or st.startswith('!') or re.match(r'implicit\s+none', st, re.I):
            continue
        if not seen_code and re.match(r'^(integer|real|logical|character|data)\b', st, re.I):
            if 'intent(' in st.replace(' ', '').lower():
                names = [n.split('(')[0].strip() for n in dg_split(st.split('::', 1)[1])]
                if all(n in keep for n in names):
                    out.append(' ' + st)
                elif any(n in local_scalars for n in names):
                    out.append(' ' + re.sub(r',\s*intent\s*\(\s*\w+\s*\)', '', st))
                continue
            out.append(' ' + st)
            continue
        seen_code = True
        out.append(' ' + transform(st, arrays, scalars))
    out.append(' end subroutine')
    return out


def dg_split(text):
    return [a for a in split_args(text) if a.strip()]


def main():
    lw = routines(root / 'src/device/radiation_lw_kernels.f90', 'taugb')
    sw = routines(root / 'src/device/radiation_sw_kernels.f90', 'taumol')
    text = ['! GENERATED by tools/gen_radiation_fast_gas.py from src/device/radiation_lw_kernels.f90 and radiation_sw_kernels.f90',
            '! - do not edit. RRTMG gas optics for the g-point path (radiation_fast.f90): one thread per column, band and layer.',
            "! Translated from RRTMG as MPAS 8.4.2 ships it. RRTMG's notice (NOTICE.md), with the years as its modules state",
            '! them: 2002-2009 in rrtmg_lw_taumol, 2002-2008 in rrtmg_sw_taumol:',
            '! Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).',
            '! Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).',
            '! This software may be used, copied, or redistributed as long as it is',
            '! not sold and this copyright notice is reproduced on each copy made.',
            '! This model is provided as is without any express or implied warranties.',
            '! (http://www.rtweb.aer.com/)',
            'module radiation_fast_gas',
            ' use parkind, only : im => kind_im, rb => kind_rb',
            ' use radiation_fast_data, only: lw_nlay, lw_laytrop, lw_lay, lw_wx, lw_ji, lw_jr, lw_planklay, lw_planklev, &',
            '  lw_plankbnd, lw_fracs, lw_taug, sw_nlay, sw_laytrop, sw_ji, sw_jr, sw_sflx',
            ' use lean_dev_rrlw_con, only: oneminus_lw => oneminus',
            ' use lean_dev_rrsw_con, only: oneminus_sw => oneminus',
            ' use lean_dev_rrlw_wvn, only: nspa_lw => nspa, nspb_lw => nspb',
            ' use lean_dev_rrsw_wvn, only: nspa_sw => nspa, nspb_sw => nspb',
            ' implicit none',
            ' private',
            ' public :: rf_lw_gas, rf_sw_gas, rf_setcoef, rf_setcoef_sw',
            'contains',
            ' ! band ib of the LW, layer l of column slot j',
            ' subroutine rf_lw_gas(j, ib, l)',
            ' !$acc routine seq',
            ' integer, intent(in) :: j, ib, l',
            ' if (l > lw_nlay(j)) return',
            ' select case (ib)']
    for k in range(1, 17):
        text += [' case (%d)' % k, '  call rf_taugb%d(j, l)' % k]
    text += [' end select', ' end subroutine', '',
             ' ! band ibm (1..14 = jb 16..29) of the SW, layer l of column slot j',
             ' subroutine rf_sw_gas(j, ibm, l)', ' !$acc routine seq', ' integer, intent(in) :: j, ibm, l',
             ' if (l > sw_nlay(j)) return', ' select case (ibm)']
    for k in range(1, 15):
        text += [' case (%d)' % k, '  call rf_taumol%d(j, l)' % (k + 15)]
    text += [' end select', ' end subroutine', '']
    for k in range(1, 17):
        body = convert('taugb%d' % k, lw['taugb%d' % k], LW, LW_SCALAR)
        body = [re.sub(r'\boneminus\b', 'oneminus_lw', re.sub(r'\bnsp([ab])\(', r'nsp\1_lw(', s)) for s in body]
        text += body + ['']
    for k in range(16, 30):
        body = convert('taumol%d' % k, sw['taumol%d' % k], SW, SW_SCALAR)
        body = [re.sub(r'\boneminus\b', 'oneminus_sw', re.sub(r'\bnsp([ab])\(', r'nsp\1_sw(', s)) for s in body]
        text += body + ['']
    lwc = routines(root / 'src/device/radiation_lw_kernels.f90', 'setcoef')
    swc = routines(root / 'src/device/radiation_sw_kernels.f90', 'setcoef_sw')
    LWO = dict(LW); LWO.pop('pavel'); LWO.pop('coldry'); LWO.pop('wx')
    body = convert_inputs('setcoef', lwc['setcoef'], LWO, {'laytrop': 'lw_laytrop(j)'})
    body[1:1] = [' use lean_dev_rrlw_wvn, only: totplnk, totplk16', ' use lean_dev_rrlw_ref, only: preflog, tref, chi_mls']
    text += body + ['']
    body = convert_inputs('setcoef_sw', swc['setcoef_sw'], SW, {'laytrop': 'sw_laytrop(j)'}, ('layswtch', 'laylow'))
    body[1:1] = [' use lean_dev_rrsw_ref, only: preflog, tref']
    text += body + ['']
    text += ['end module radiation_fast_gas', '']
    new = '\n'.join(text)
    if '--check' in sys.argv:
        if not out.exists() or out.read_text() != new:
            print('DIFFERS', out)
            sys.exit(1)
        return
    out.write_text(new)


if __name__ == '__main__':
    main()
