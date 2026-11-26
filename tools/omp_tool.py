"""OpenMP helper for the lean program: writes and audits the `!$omp` directives of src/*.f90.

  omp_tool.py gen FILE ROUTINE ORDINALS [SCHEDULE]
                                     put `!$omp parallel do` before the given top-level DO loops of a routine
                                     (ORDINALS: 1-based list like 1,2,5 or 'all'; in<N>:<ordinals> for the loops
                                     directly inside the N-th top-level loop); the private list is computed;
                                     SCHEDULE defaults to dynamic,64
  omp_tool.py check [FILE ...]       audit every `!$omp parallel do` in src/: every local variable or local
                                     array that the loop body assigns must be private, and no scalar dummy
                                     argument may be assigned (each a failure, exit 1); assignments to module
                                     or host names and the calls inside the loop are listed as notes
  omp_tool.py orphans [FILE ...]     in routines with orphaned `!$omp do` loops, the statements outside the
                                     worksharing loops and `!$omp single` that write shared data
  omp_tool.py modvars FILE ...       the routines that assign module variables, and saved locals
  omp_tool.py nonlocal FILE ...      the routines that assign host or module variables

What a directive means here: the loop is over cells, edges or vertices, and iteration c writes only its own column
(index c of the last dimension) of the arrays it writes. That property is a review item, not something this tool
can prove; a run with N threads must give the same bits as a run with one thread.
"""
import re, sys
from pathlib import Path

root = Path(__file__).resolve().parents[1]

def logical_lines(lines):
    """(first_index, text) with continuation lines joined; comments removed (except !$omp)."""
    out, i = [], 0
    while i < len(lines):
        raw = lines[i]; start = i
        text = raw.rstrip('\n')
        stripped = text.strip()
        if stripped.startswith('!$omp') or stripped.startswith('!$ '):
            out.append((start, stripped)); i += 1; continue
        text = re.sub(r'!.*$', '', text) if "'" not in text and '"' not in text else text
        while text.rstrip().endswith('&') and i + 1 < len(lines):
            text = text.rstrip()[:-1] + ' ' + re.sub(r'!.*$', '', lines[i+1].strip().lstrip('&'))
            i += 1
        if ';' in text and "'" not in text and '"' not in text:
            for part in text.split(';'):
                if part.strip(): out.append((start, part))
        else:
            out.append((start, text))
        i += 1
    return out

def routines(lines):
    ll = logical_lines(lines)
    res, cur = [], None
    for idx, (ln, t) in enumerate(ll):
        m = re.match(r'\s*(?:pure\s+|elemental\s+|recursive\s+)*(?:(?:real|integer|logical)\s*(?:\([^)]*\))?\s+)?(subroutine|function)\s+(\w+)', t, re.I)
        if m and cur is None and 'end' not in t.lower().split()[:1]:
            cur = {'name': m.group(2).lower(), 'start': ln, 'll': []}
        if cur is not None:
            cur['ll'].append((ln, t))
            if re.match(r'\s*end\s+(subroutine|function)\b', t, re.I):
                cur['end'] = ln; res.append(cur); cur = None
    return res

def split_top(s):
    parts, depth, cur = [], 0, ''
    for ch in s:
        if ch == '(': depth += 1
        if ch == ')': depth -= 1
        if ch == ',' and depth == 0: parts.append(cur); cur = ''
        else: cur += ch
    if cur.strip(): parts.append(cur)
    return parts

def declarations(r):
    """local names (no intent, not parameter), dummy names with intent, and the dummy-scalar set."""
    local, dummy_scalar, dummy_array = set(), set(), set()
    for ln, t in r['ll']:
        if '::' not in t or t.strip().lower().startswith(('use ', 'type ', 'interface')): continue
        left, right = t.split('::', 1)
        ll = left.lower()
        if 'parameter' in ll: continue
        for item in split_top(right):
            nm = re.match(r'\s*(\w+)', item)
            if not nm: continue
            name = nm.group(1).lower()
            is_array = '(' in item.split('=')[0] or 'dimension' in ll or 'allocatable' in ll or 'pointer' in ll
            if 'intent' in ll:
                (dummy_array if is_array else dummy_scalar).add(name)
            elif 'save' in ll:
                pass  # explicit static: shared by design, reviewed by hand
            else:
                local.add(name)
    return local, dummy_scalar, dummy_array

def loop_end(ll, k):
    """index in ll of the END DO matching the DO at ll[k]."""
    depth = 0
    for j in range(k, len(ll)):
        t = ll[j][1]
        if re.match(r'\s*do\b(?!\s+while)', t, re.I) and not t.strip().lower().startswith('double'): depth += 1
        elif re.match(r'\s*end\s*do\b', t, re.I): depth -= 1
        if depth == 0: return j
    raise SystemExit('unbalanced do')

ASSIGN = re.compile(r'^\s*(?:\d+\s+)?(\w+)\s*(?:\([^=]*\))?\s*(?:%\s*\w+\s*(?:\([^=]*\))?\s*)*=(?!=)')
def assigned(body):
    names, do_vars, calls, comp = set(), set(), [], set()
    for ln, t in body:
        m = re.match(r'\s*do\s+(\w+)\s*=', t, re.I)
        if m: do_vars.add(m.group(1).lower()); continue
        if re.match(r'\s*(if\s*\(.*\)\s*)?call\s', t, re.I): calls.append(t.strip())
        tt = re.sub(r'^\s*if\s*\(.*?\)\s*(?=\w+\s*(\(|=|%))', '', t, flags=re.I) if re.match(r'\s*if\s*\(', t, re.I) and 'then' not in t.lower() else t
        m = ASSIGN.match(tt)
        if m and m.group(1).lower() not in ('do', 'if', 'else', 'where', 'select', 'case', 'end'):
            names.add(m.group(1).lower())
            if re.match(r'\s*\w+\s*(\([^=]*\))?\s*%', tt): comp.add(m.group(1).lower())
    assigned.component_writes = comp
    return names, do_vars, calls

def top_level_loops(r, lo=0, hi=None):
    """list of (index_in_ll_of_do) for DO loops at routine level (not nested in another DO); lo/hi restrict the range."""
    ll, res, k = r['ll'], [], lo
    hi = len(ll) if hi is None else hi
    while k < hi:
        t = ll[k][1]
        if re.match(r'\s*do\b(?!\s+while)', t, re.I):
            res.append(k); k = loop_end(ll, k) + 1
        else: k += 1
    return res

def privates(r, k):
    local, ds, da = declarations(r)
    ll = r['ll']; e = loop_end(ll, k)
    names, do_vars, calls = assigned(ll[k+1:e])
    outer = re.match(r'\s*do\s+(\w+)', ll[k][1], re.I).group(1).lower()
    priv = sorted(((names | do_vars) & local) - {outer})
    return priv, names, calls

def gen(file, routine, ordinals, schedule='dynamic,64'):
    path = root/file
    lines = path.read_text().split('\n')
    r = next(x for x in routines([l+'\n' for l in lines]) if x['name'] == routine)
    if ordinals.startswith('in'):
        # in<N>:<ordinals>: loops directly inside the N-th top-level loop (for example the loop over scalars)
        outer, ordinals = ordinals[2:].split(':')
        k0 = top_level_loops(r)[int(outer)-1]
        loops = top_level_loops(r, k0+1, loop_end(r['ll'], k0))
    else:
        loops = top_level_loops(r)
    pick = range(1, len(loops)+1) if ordinals == 'all' else [int(x) for x in ordinals.split(',')]
    inserts = []
    for o in pick:
        k = loops[o-1]
        priv, names, calls = privates(r, k)
        ln = r['ll'][k][0]
        indent = re.match(r'\s*', lines[ln]).group(0)
        d = f'{indent}!$omp parallel do schedule({schedule})' + (f' private({",".join(priv)})' if priv else '')
        inserts.append((ln, d))
    for ln, d in sorted(inserts, reverse=True):
        if lines[ln-1].strip().startswith('!$omp'): continue
        lines.insert(ln, d)
    path.write_text('\n'.join(lines))
    print(f'{file}:{routine}: {len(inserts)} loops')

def check(files):
    bad = 0
    for f in files:
        lines = Path(f).read_text().split('\n')
        for r in routines([l+'\n' for l in lines]):
            local, ds, da = declarations(r)
            ll = r['ll']
            for k, (ln, t) in enumerate(ll):
                if not re.match(r'\s*!\$omp\s+parallel\s+do', t, re.I): continue
                # the directive text (possibly several !$omp continuation lines)
                dtext = t
                j = k + 1
                while dtext.rstrip().endswith('&'):
                    dtext = dtext.rstrip()[:-1] + ll[j][1].replace('!$omp&', '').replace('!$omp', ''); j += 1
                pm = re.search(r'private\(([^)]*)\)', dtext, re.I)
                priv = {x.strip().lower() for x in pm.group(1).split(',')} if pm else set()
                sm = re.search(r'shared\(([^)]*)\)', dtext, re.I)
                shared = {x.strip().lower() for x in sm.group(1).split(',')} if sm else set()
                rm = re.search(r'reduction\([^:]*:([^)]*)\)', dtext, re.I)
                red = {x.strip().lower() for x in rm.group(1).split(',')} if rm else set()
                while not re.match(r'\s*do\b', ll[j][1], re.I): j += 1
                e = loop_end(ll, j)
                outer = re.match(r'\s*do\s+(\w+)', ll[j][1], re.I).group(1).lower()
                names, do_vars, calls = assigned(ll[j+1:e])
                need = ((names | do_vars) & local) - {outer}
                missing = need - priv - red - shared
                scalar_bad = (names & ds) - red - assigned.component_writes
                extra = priv - (need | {outer})
                tag = f'{f}:{ll[j][0]+1} ({r["name"]})'
                if missing: print(f'MISSING PRIVATE {tag}: {sorted(missing)}'); bad += 1
                if scalar_bad: print(f'SHARED DUMMY SCALAR ASSIGNED {tag}: {sorted(scalar_bad)}'); bad += 1
                if extra: print(f'note {tag}: private but not assigned in loop: {sorted(extra)}')
                for c in calls: print(f'call in parallel loop {tag}: {c[:110]}')
                dyn = (names - local) - ds - da
                if dyn: print(f'note {tag}: assigned non-local, non-dummy names (module/host/derived): {sorted(dyn)}')
    return bad

def orphans(files):
    """Routines with orphaned `!$omp do` loops run on every thread of the team: list the statements outside the worksharing
    loops and outside `!$omp single` that write something other than a routine local (each thread writes the same shared
    data, or races). Locals are per thread (the program is compiled with -fopenmp, which implies -frecursive)."""
    for f in files:
        lines = Path(f).read_text().split('\n')
        for r in routines([l+'\n' for l in lines]):
            ll = r['ll']
            if not any(re.match(r'\s*!\$omp\s+do\b', t, re.I) for _, t in ll): continue
            local, ds, da = declarations(r)
            inside = [False]*len(ll)
            k = 0
            while k < len(ll):
                t = ll[k][1]
                if re.match(r'\s*!\$omp\s+do\b', t, re.I):
                    j = k + 1
                    while not re.match(r'\s*do\b', ll[j][1], re.I): j += 1
                    e = loop_end(ll, j)
                    for x in range(k, e+1): inside[x] = True
                    k = e + 1
                elif re.match(r'\s*!\$omp\s+single', t, re.I):
                    j = k
                    while not re.match(r'\s*!\$omp\s+end\s+single', ll[j][1], re.I): j += 1
                    for x in range(k, j+1): inside[x] = True
                    k = j + 1
                else: k += 1
            outside = [(ll[i][0], ll[i][1]) for i in range(len(ll)) if not inside[i]]
            names, do_vars, calls = assigned(outside)
            hit = sorted(names - local - {r['name']})
            for ln, t in outside:
                m = ASSIGN.match(t)
                if m and m.group(1).lower() in hit: print(f'{f}:{ln+1} ({r["name"]}): {t.strip()[:120]}')

def main():
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        print(__doc__); sys.exit(0 if len(sys.argv) > 1 else 2)
    if sys.argv[1] == 'gen': gen(sys.argv[2], sys.argv[3], sys.argv[4], *(sys.argv[5:6]))
    elif sys.argv[1] == 'check':
        files = sys.argv[2:] or sorted(str(p) for p in (root/'src').glob('*.f90'))
        raise SystemExit(1 if check(files) else 0)
    elif sys.argv[1] == 'orphans': orphans(sys.argv[2:] or sorted(str(p) for p in (root/'src').glob('*.f90')))
    elif sys.argv[1] == 'modvars': modvars(sys.argv[2:])
    elif sys.argv[1] == 'nonlocal': nonlocal_assignments(sys.argv[2:])
    else:
        print(__doc__, file=sys.stderr); sys.exit(2)


def module_variables(lines):
    """names declared at module level (before the first CONTAINS) that are not parameters."""
    names, ll, seen_module = set(), logical_lines([l+'\n' for l in lines]), False
    for ln, t in ll:
        s = t.strip().lower()
        if re.match(r'module\s+(?!procedure)\w+', s): seen_module = True; continue
        if s.startswith('contains'): break
        if not seen_module or '::' not in t: continue
        left, right = t.split('::', 1)
        if 'parameter' in left.lower(): continue
        for item in split_top(right):
            nm = re.match(r'\s*(\w+)', item)
            if nm: names.add(nm.group(1).lower())
    return names

def modvars(files):
    """For every module-level variable and every implicitly/explicitly saved local: the routines that assign it."""
    for f in files:
        lines = Path(f).read_text().split('\n')
        mv = module_variables(lines)
        for r in routines([l+'\n' for l in lines]):
            local, ds, da = declarations(r)
            names, do_vars, calls = assigned(r['ll'])
            hit = sorted((names & mv) - local - ds - da)
            if hit: print(f'{f}: {r["name"]} assigns module variables {hit}')
            for ln, t in r['ll']:
                if '::' in t and '=' in t.split('::', 1)[1] and 'parameter' not in t.split('::', 1)[0].lower() and not t.strip().lower().startswith('type'):
                    print(f'{f}:{ln+1}: {r["name"]} implicit SAVE (initialised local): {t.strip()[:100]}')
                if re.match(r'\s*save\b', t, re.I) or ('save' in t.split('::', 1)[0].lower() and '::' in t):
                    print(f'{f}:{ln+1}: {r["name"]} SAVE: {t.strip()[:100]}')
                if re.match(r'\s*data\b', t, re.I): print(f'{f}:{ln+1}: {r["name"]} DATA statement: {t.strip()[:100]}')


def nonlocal_assignments(files):
    """routines that assign a name that is neither a local, a dummy argument, a derived-type view component nor its own function result: host/module variables."""
    for f in files:
        lines = Path(f).read_text().split('\n')
        for r in routines([l+'\n' for l in lines]):
            local, ds, da = declarations(r)
            names, do_vars, calls = assigned(r['ll'])
            hdr = r['ll'][0][1]
            res = re.search(r'result\s*\(\s*(\w+)\s*\)', hdr, re.I)
            own = {r['name']} | ({res.group(1).lower()} if res else set())
            hit = sorted(names - local - ds - da - own - do_vars)
            if hit: print(f'{f}: {r["name"]}: {hit}')


if __name__ == '__main__':
    main()
