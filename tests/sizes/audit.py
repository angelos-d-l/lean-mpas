#!/usr/bin/env python3
"""What in src/*.f90 has a compiled-in horizontal size, and where a physics state object is used as a whole: the mesh
size is read at run time, so src/ must hold neither.

    python3 tests/sizes/audit.py [--expect type=N,module=N,local=N,whole=N,ice=N,routine=N] [--list]

Prints the six counts, then SIZES AUDIT OK, or the lines of each count that is over its expected number (exit 1).
  type, module, local: declaration statements that name ncells, nedges, nvertices, nc1, ne1 or nv1 inside a bound, in a
    type body, at module level, and local to a procedure (a statement counts once, at its first line). Parameters and
    dummy arguments are not counted.
  whole: lines that use an object of state_type, noahmp_state_type, gf_state_type or gwdo_state_type as a whole: in an
    OpenACC clause that copies or updates (copyin, copy, copyout, create, device, self, host), in c_loc(), in the list of a write, or on either side of an assignment. An allocatable member
    of such an object must reach the card and the copies by itself. src/phys_state.f90 (generated, the
    member-wise routines themselves) is not read for this count; `rad` is one array of a flat type and is not counted.
  ice: the same whole-object uses for ice_cells, including the imported sea_ice_cells name in phys_dev.
  routine: horizontal-bound declaration statements inside !$acc routine procedures, including src/device/.
"""
import re
import sys
from pathlib import Path

root = Path(__file__).resolve().parents[2]
SIZE = re.compile(r'\b(ncells|nedges|nvertices|nc1|ne1|nv1)\b', re.I)
STATE_TYPES = 'state_type|noahmp_state_type|gf_state_type|gwdo_state_type'
DECL = re.compile(r'^\s*(real|integer|logical|character|double\s+precision|complex|type\s*\()', re.I)
TYPE_START = re.compile(r'^\s*type\s*(,[^:]*)?(::)?\s*(\w+)\s*$', re.I)
PROC_START = re.compile(r'^\s*(?:(?:recursive|pure|elemental|impure)\s+)*(?:(?:real|integer|logical|double\s+precision|character|complex|type)\s*'
                        r'(?:\([^)]*\)|\*\s*\d+)?\s+)?(?:(?:recursive|pure|elemental|impure)\s+)*(subroutine|function)\s+(\w+)\s*(\(.*)?', re.I)
PROC_END = re.compile(r'^\s*end\s*(subroutine|function)\b', re.I)


def code(line):
    """the statement text of a line, without its comment (an OpenACC directive is kept)"""
    if re.match(r'^\s*!\$acc', line, re.I):
        return line
    return line.split('!', 1)[0]


def in_parentheses(text):
    """the parts of text that are inside parentheses"""
    out, depth = '', 0
    for ch in text:
        if ch == ')':
            depth -= 1
        out += ch if depth > 0 else ' '
        if ch == '(':
            depth += 1
    return out


def split_top(text):
    parts, depth, cur = [], 0, ''
    for ch in text:
        if ch in '([':
            depth += 1
        if ch in ')]':
            depth -= 1
        if ch == ',' and depth == 0:
            parts.append(cur)
            cur = ''
        else:
            cur += ch
    return parts + [cur]


def statements(lines):
    """[(text of the statement, [numbers of its lines])]: continuation lines joined"""
    out, text, numbers = [], '', []
    for number, line in enumerate(lines, 1):
        c = code(line).rstrip()
        if line.startswith('#'):
            continue
        numbers.append(number)
        if c.endswith('&'):
            text += c[:-1] + ' '
            continue
        out.append((text + c, numbers))
        text, numbers = '', []
    return out


def size_lines(path):
    """{'type': [...], 'module': [...], 'local': [...]}: the line numbers of declarations with a fixed horizontal size"""
    lines = path.read_text().split('\n')
    found = {'type': [], 'module': [], 'local': []}
    in_type, depth, dummies = False, 0, []
    for text, numbers in statements(lines):
        if re.match(r'^\s*end\s*type\b', text, re.I):
            in_type = False
            continue
        if PROC_END.match(text):
            if dummies:     # (an interface body with a prefix this file does not know, src/lean_warp.f90, has no start here)
                depth -= 1
                dummies.pop()
            continue
        start = PROC_START.match(text)
        if start and not re.match(r'^\s*end\b', text, re.I):
            depth += 1
            args = re.search(r'\(([^)]*)\)', text[start.end(2):])
            names = [a.strip().lower() for a in args.group(1).split(',')] if args else []
            result = re.search(r'result\s*\(\s*(\w+)\s*\)', text, re.I)
            dummies.append(names + [start.group(2).lower()] + ([result.group(1).lower()] if result else []))
            continue
        if '::' not in text and TYPE_START.match(text) and not re.match(r'^\s*type\s*\(', text, re.I):
            in_type = True
            continue
        if '::' not in text or not DECL.match(text):
            continue
        attributes, entities = text.split('::', 1)
        if re.search(r'\bparameter\b', attributes, re.I):
            continue
        if not in_type and depth > 0:
            if re.search(r'\bintent\b', attributes, re.I):
                continue
            names = [m.group(1).lower() for m in (re.match(r'\s*(\w+)', e) for e in split_top(entities)) if m]
            if all(n in dummies[-1] for n in names):
                continue
        kind = 'type' if in_type else 'local' if depth > 0 else 'module'
        if SIZE.search(in_parentheses(text)):     # a statement counts once, at its first line
            found[kind].append(numbers[0])
    return found


def whole_lines(path, types=STATE_TYPES):
    """the line numbers that use a physics state object as a whole"""
    lines = path.read_text().split('\n')
    names = set()
    for text, _ in statements(lines):
        m = re.match(r'^\s*type\s*\(\s*(?:%s)\s*\)[^:]*::(.*)$' % types, text, re.I)
        if m:
            names.update(re.match(r'\s*(\w+)', e).group(1).lower() for e in split_top(m.group(1)))
    if types == "ice_cells" and path.name == "phys_dev.f90":
        names.add("sea_ice_cells")
    if not names:
        return []
    name = r'\b(?:%s)\b(?!\s*%%)' % '|'.join(sorted(names))
    found = []
    for number, line in enumerate(lines, 1):
        c = code(line).lower()
        if re.match(r'^\s*!\$acc', c):
            # the variable lists of the clauses: copyin(a, b), update device(a) ...
            lists = re.findall(r'\b(?:copyin|copy|copyout|create|device|self|host)\s*\(([^()]*)\)', c)
            hit = any(re.fullmatch(name, item.strip()) for l in lists for item in l.split(','))
        elif re.search(r'\bc_loc\s*\(\s*' + name + r'\s*\)', c):
            hit = True
        elif re.match(r'^\s*write\s*\(', c):
            items = c[c.index(')') + 1:]
            hit = any(re.fullmatch(name, item.strip()) for item in split_top(items))
        else:
            hit = False
            for part in c.split(';'):
                if re.match(r'^\s*(type\s*\(|call\b|if\b.*\bcall\b)', part) or '=>' in part:
                    continue
                if re.match(r'^\s*' + name + r'\s*=[^=]', part) or re.search(r'[^=<>/]=\s*' + name + r'\s*$', part):
                    hit = True
        if hit:
            found.append(number)
    return found


def routine_lines(path):
    """Declaration lines with a horizontal bound inside an OpenACC routine."""
    found, declarations, device = [], [], False
    for text, numbers in statements(path.read_text().splitlines()):
        if PROC_START.match(text) and not re.match(r'^\s*end\b', text, re.I):
            declarations, device = [], False
        if re.match(r'^\s*!\$acc\s+routine\b', text, re.I):
            device = True
        if DECL.match(text) and '::' in text and SIZE.search(in_parentheses(text)):
            declarations.append(numbers[0])
        if PROC_END.match(text) and device:
            found.extend(declarations)
    return found


def main():
    expect = {}
    for i, arg in enumerate(sys.argv):
        if arg == '--expect':
            expect = {k: int(v) for k, v in (item.split('=') for item in sys.argv[i + 1].split(','))}
    found = {'type': [], 'module': [], 'local': [], 'whole': [], 'ice': [], 'routine': []}
    for path in sorted((root / 'src').glob('*.f90')):
        sizes = size_lines(path)
        for kind in ('type', 'module', 'local'):
            found[kind] += ['src/%s:%d' % (path.name, n) for n in sizes[kind]]
        if path.name != 'phys_state.f90':
            found['whole'] += ['src/%s:%d' % (path.name, n) for n in whole_lines(path)]
        found['ice'] += ['src/%s:%d' % (path.name, n) for n in whole_lines(path, 'ice_cells')]
    for path in sorted((root / 'src').glob('*.f90')) + sorted((root / 'src/device').glob('*.f90')):
        found['routine'] += [str(path.relative_to(root)) + ':' + str(n) for n in routine_lines(path)]
    print('sizes audit: ' + ', '.join('%s=%d' % (k, len(found[k])) for k in ('type', 'module', 'local', 'whole', 'ice', 'routine')))
    if '--list' in sys.argv:
        for kind in ('module', 'local', 'whole', 'ice', 'routine'):
            for where in found[kind]:
                print(' %s %s' % (kind, where))
        files = {}
        for where in found['type']:
            files[where.split(':')[0]] = files.get(where.split(':')[0], 0) + 1
        for name, n in files.items():
            print(' type %s: %d lines' % (name, n))
    over = [k for k in expect if len(found[k]) > expect[k]]
    under = [k for k in expect if len(found[k]) < expect[k]]
    for kind in over:
        print('OVER %s: %d, expected %d' % (kind, len(found[kind]), expect[kind]))
        for where in found[kind] if kind != 'type' else found[kind][:20]:
            print(' ' + where)
    for kind in under:
        print('UNDER %s: %d, expected %d: lower the expected number' % (kind, len(found[kind]), expect[kind]))
    if over or under:
        print('SIZES AUDIT FAILED')
        return 1
    print('SIZES AUDIT OK')
    return 0


if __name__ == '__main__':
    sys.exit(main())
