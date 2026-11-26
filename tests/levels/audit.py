#!/usr/bin/env python3
"""Inventory of vertical bounds in src/: the number of levels is read at run time, so src/ must not hold a literal 45
or 46 (the level count of the release cases, and that count plus one), a level parameter (except the count of a
--levels build), a fixed level bound or a card-local array sized by nz. Counts statements, except the literal and reduction lines.

    python3 tests/levels/audit.py [--expect literal=N,param=N,fixed=N,cardlocal=N,reduce=N,dynprivate=N] [--list]

The statement parser of tests/sizes/audit.py joins Fortran continuations. Dummies keep exact bounds; routine-seq
locals need a capacity (max_levels). The dynamics' loop-private arrays keep nz: they are gang-private on the card and
sized at launch, so they are counted apart (dynprivate).
"""
import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('sizes_audit', ROOT / 'tests/sizes/audit.py')
sizes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sizes)
VERTICAL = re.compile(r'\b(nz|nz1|kte|kme|kde|sw_layers|mp_top_level)\b', re.I)
CAPACITY = re.compile(r'\b(max_levels|max_sw_layers|max_lw_layers)\b', re.I)
LITERAL = re.compile(r'\b(45|46)\b')
# The files of the dynamics' gang-private declarations, counted as dynprivate; --expect guards their number.
DYNAMICS_PRIVATE = {'dyn_tend.f90', 'dyn_scalars.f90', 'dyn_boundary.f90',
                    'dyn_diagnostics.f90', 'winds.f90'}
# Exact file/text exceptions: a new 45 elsewhere in these files is still reported.
# These 22 file/text pairs cover the 25 non-level lines (three texts repeat).
NON_LEVEL = {
    ('dyn_device.f90', 'if (first <= 45 .and. 45 <= last) f%tend_w_euler = nan'): 'state field number',
    ('dyn_device.f90', 'if (first <= 46 .and. 46 <= last) f%tend_theta_euler = nan'): 'state field number',
    ('lean_model.f90', 'col(46:47)=real(rad(c)%flux(7:8),8)'): 'budget column',
    ('lean_model.f90', 'b(46:47)=b(46:47)+w*col(46:47)'): 'budget column',
    ('lean_model.f90', 'b(45)=water/area'): 'budget column',
    ('lean_model.f90', 'b(46:47)=b(46:47)/area'): 'budget column',
    ('noahmp_core.f90', 'CanopyIceMax = VegFrac * 6.6 * (0.27 + 46.0/SnowfallDensity) * (LeafAreaIndEff + StemAreaIndEff)'): 'snow physical constant',
    ('noahmp_core.f90', 'if ( SnowIceDens > SnowCompactAgingMax ) CompactionSnowAging(LoopInd) = CompactionSnowAging(LoopInd) * exp(-46.0e-3*(SnowIceDens-SnowCompactAgingMax))'): 'snow physical constant',
    ('radiation_climatology.f90', 'integer, parameter :: date_oz(12)=[16,45,75,105,136,166,197,228,258,289,319,350]'): 'day of year',
    ('radiation_lw.f90', 'real, parameter :: retab(95)=[                                                 5.92779, 6.26422, 6.61973, 6.99539, 7.39234,    7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,   10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,   15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,   20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,   27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,   31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,   34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,   38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,   42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,   50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,   65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,   93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,   124.954, 130.630, 136.457, 142.446, 148.608, 154.956,   161.503, 168.262, 175.248, 182.473, 189.952, 197.699,   205.728, 214.055, 222.694, 231.661, 240.971, 250.639]'): 'effective radius table',
    ('radiation_lw_kernels.f90', 'if (index .eq. 46) index = 45'): 'spectral table index',
    ('radiation_sw_kernels.f90', 'if (index .eq. 46) index = 45'): 'spectral table index',
    ('sea_ice_noah.f90', 'alb_ice = 0.45'): 'albedo constant',
    ('thompson.f90', 'REAL, PARAMETER, PRIVATE:: Kap1 = 17.46'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tpi_qcfz)) ALLOCATE(tpi_qcfz(ntb_c,nbc,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tni_qcfz)) ALLOCATE(tni_qcfz(ntb_c,nbc,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tpi_qrfz)) ALLOCATE(tpi_qrfz(ntb_r,ntb_r1,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tpg_qrfz)) ALLOCATE(tpg_qrfz(ntb_r,ntb_r1,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tni_qrfz)) ALLOCATE(tni_qrfz(ntb_r,ntb_r1,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'if (.NOT. ALLOCATED(tnr_qrfz)) ALLOCATE(tnr_qrfz(ntb_r,ntb_r1,45,ntb_IN))'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'do k = 1, 45'): 'freezing temperature table or physical constant',
    ('thompson.f90', 'idx_tc = MAX(1, MIN(NINT(-tempc), 45) )'): 'freezing temperature table or physical constant',
}


# The one level parameter allowed: a build with --levels N (LEAN_NZ) makes the count a constant on purpose.
FIXED_COUNT = {('levels.f90', 'integer, parameter :: nz = fixed_levels, nz1 = fixed_levels + 1')}


def level_literal(filename, line):
    text = sizes.code(line).strip()
    return bool(LITERAL.search(text)) and (filename, text) not in NON_LEVEL


def vertical_bound(text, filename):
    bounds = sizes.in_parentheses(text)
    return bool(VERTICAL.search(bounds) or
                # tridiag2 calls the level bound n.
                (filename == 'mynn_pbl_helpers.f90' and re.search(r'\bn\b', bounds, re.I)) or
                (filename in {'gf_core.f90', 'gf.f90', 'gwdo_core.f90', 'mynn_pbl.f90', 'mynn_pbl_helpers.f90'}
                 and LITERAL.search(bounds)))


def reduces_whole(statement, names):
    """Also catch expressions such as sum(abs(work)); bounded sections are safe."""
    for call in re.finditer(r'\b(sum|maxval|minval|any|all|count|maxloc|minloc|product|dot_product|norm2|matmul|findloc)\s*\(', statement, re.I):
        start = call.end()
        depth = 1
        end = start
        while end < len(statement) and depth:
            if statement[end] == '(':
                depth += 1
            elif statement[end] == ')':
                depth -= 1
            end += 1
        arguments = statement[start:end-1]
        for name in names:
            for ref in re.finditer(r'(?<![%\w])' + re.escape(name) + r'\b', arguments, re.I):
                tail = arguments[ref.end():].lstrip()
                if not tail.startswith('('):
                    return True
                # A full/open slice still includes the capacity tail. Track
                # capacity axes so a full non-level dimension remains legal.
                depth, end = 1, 1
                while end < len(tail) and depth:
                    depth += (tail[end] == '(') - (tail[end] == ')')
                    end += 1
                subs = sizes.split_top(tail[1:end-1])
                axes = names[name] if isinstance(names, dict) else range(len(subs))
                for axis in axes:
                    if axis < len(subs):
                        parts = subs[axis].split(':')
                        if len(parts) > 1 and (not parts[1].strip() or CAPACITY.search(parts[1])):
                            return True
    return False


def scan(src):
    found = {k: [] for k in ('literal', 'param', 'fixed', 'cardlocal', 'reduce', 'dynprivate')}
    for path in sorted(src.glob('*.f90')) + sorted((src / 'device').glob('*.f90')):
        lines = path.read_text().splitlines()
        device_file = path.parent.name == 'device'
        def add(kind, number):
            found[kind].append(f'{path.relative_to(src.parent)}:{number}: {lines[number-1].strip()}')
        if not device_file:
            for n, line in enumerate(lines, 1):
                if level_literal(path.name, line):
                    add('literal', n)
        in_type = False
        procedures = []
        for text, numbers in sizes.statements(lines):
            text = text.replace('&', '')   # free-form continuation markers
            low = text.lower()
            start = sizes.PROC_START.match(text)
            if start and not re.match(r'^\s*end\b', low):
                args = re.search(r'\(([^)]*)\)', text[start.end(2):])
                dummies = set(a.strip().lower() for a in args[1].split(',')) if args else set()
                result = re.search(r'result\s*\(\s*(\w+)\s*\)', low)
                dummies.add(start[2].lower())
                if result:
                    dummies.add(result[1])
                procedures.append(dict(dummies=dummies, declarations=[], private=set(), card=False, text=[]))
                continue
            if sizes.PROC_END.match(text):
                if not procedures:
                    continue
                proc = procedures.pop()
                capacity_locals = {}
                for decl, n, names in proc['declarations']:
                    local_names = names - proc['dummies']
                    if not local_names or re.search(r'\bintent\b', decl, re.I):
                        continue
                    private = bool(local_names & proc['private'])
                    card = proc['card'] or private
                    dynprivate = private and not proc['card'] and path.name in DYNAMICS_PRIVATE
                    if card and vertical_bound(decl, path.name):
                        add('dynprivate' if dynprivate else 'cardlocal', n)
                    if card:
                        attrs, entities = decl.split('::', 1)
                        dim = re.search(r'\bdimension\s*\(([^()]*)\)', attrs, re.I)
                        for entity in sizes.split_top(entities):
                            match = re.match(r'\s*(\w+)\s*(?:\(([^()]*)\))?', entity)
                            if match and match[1].lower() in local_names:
                                bounds = dim[1] if dim else match[2]
                                if bounds:
                                    axes = {i for i, b in enumerate(sizes.split_top(bounds)) if CAPACITY.search(b)}
                                    if axes:
                                        capacity_locals[match[1].lower()] = axes
                for statement, nums in proc['text']:
                    if reduces_whole(statement, capacity_locals):
                        add('reduce', nums[0])
                continue
            if re.match(r'^\s*end\s*type\b', low):
                in_type = False
                continue
            if sizes.TYPE_START.match(text) and not re.match(r'^\s*type\s*\(', low):
                in_type = True
                continue
            if procedures:
                proc = procedures[-1]
                for parent in procedures:
                    parent['text'].append((text, numbers))
                if re.match(r'^\s*!\$acc\s+routine\b', low):
                    proc['card'] = True
                if re.match(r'^\s*!\$acc', low):
                    for clause in re.findall(r'\bprivate\s*\(([^()]*)\)', low):
                        proc['private'].update(x.strip() for x in clause.split(','))
            if not sizes.DECL.match(text) or '::' not in text:
                continue
            attrs, entities = text.split('::', 1)
            names = {m[1].lower() for e in sizes.split_top(entities) if (m := re.match(r'\s*(\w+)', e))}
            if re.search(r'\bparameter\b', attrs, re.I):
                # Only level parameters, not numeric constants or capacity constants.
                if not device_file and (path.name, text.strip()) not in FIXED_COUNT and (VERTICAL.search(entities) or
                    (names & {'n'} and LITERAL.search(entities))):
                    add('param', numbers[0])
            elif in_type or not procedures:
                if not device_file and vertical_bound(text, path.name) and not re.search(r'\ballocatable\b', attrs, re.I):
                    add('fixed', numbers[0])
            else:
                procedures[-1]['declarations'].append((text, numbers[0], names))
    return found


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--expect', default='')
    parser.add_argument('--list', action='store_true')
    args = parser.parse_args()
    expected = dict((k, int(v)) for k, v in (item.split('=') for item in args.expect.split(',') if item))
    found = scan(ROOT / 'src')
    print('levels audit: ' + ', '.join(f'{k}={len(v)}' for k, v in found.items()))
    failed = False
    for kind, locations in found.items():
        mismatch = kind in expected and len(locations) != expected[kind]
        if mismatch:
            failed = True
            print(f'{kind}: {len(locations)}, expected {expected[kind]}')
        if args.list or mismatch:
            for location in locations:
                print(f' {kind} {location}')
    print('LEVELS AUDIT FAILED' if failed else 'LEVELS AUDIT OK')
    return int(failed)


if __name__ == '__main__':
    sys.exit(main())
