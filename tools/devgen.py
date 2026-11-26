"""Source-to-source steps that make a lean physics routine device code (OpenACC routine seq). Nothing else is changed.

The lean schemes are written with internal procedures that share the locals of their parent (call prepare_column, call fall_speeds ...).
nvfortran refuses `acc routine` for a parent whose internal procedures use host data (NVFORTRAN-S-0155), and has no BLOCK construct in
device code. Every such internal procedure here has no arguments and no locals, so it is inlined: the call statement is replaced by the
body of the procedure (a `return` of the procedure becomes a jump to a label after the body). The executed statements, their order and their
text are unchanged.

  inline_internal(lines, parent)         inline all contained procedures of the routine `parent`
  module_procedures(lines)               names of the module-level procedures (not the internal ones)
  add_routine_seq(lines, names)          `!$acc routine seq` after the header of each named procedure
  module_variables(lines, module=None)   names of the non-parameter variables of a module's specification part
  drop_lines(lines, patterns)            delete debug-only statements (print, write to unit 0, error stop)
  insert_before(lines, pattern, new)     new lines before the first line matching pattern
  acc_list(directive, names)             `!$acc <directive>(names)` lines
All functions take and return lists of source lines (free form, as in src/).
"""
import re

SUB = re.compile(r'^\s*(?:(?:recursive|pure|elemental|impure)\s+)*subroutine\s+(\w+)\s*(\(.*)?$', re.I)
FUNC = re.compile(r'^\s*(?:(?:recursive|pure|elemental|impure)\s+)*(?:(?:real|integer|logical|double\s+precision|character|complex|type)\s*'
                  r'(?:\([^)]*\)|\*\s*\d+)?\s+)?(?:(?:recursive|pure|elemental|impure)\s+)*function\s+(\w+)', re.I)
END_SUB = re.compile(r'^\s*end\s*subroutine(?:\s+(\w+))?\s*$', re.I)
END_FUNC = re.compile(r'^\s*end\s*function(?:\s+(\w+))?\s*$', re.I)
MODULE = re.compile(r'^\s*module\s+(\w+)\s*$', re.I)
END_MODULE = re.compile(r'^\s*end\s*module\b', re.I)
CONTAINS = re.compile(r'^\s*contains\s*$', re.I)
CALL = re.compile(r'^(\s*)call\s+(\w+)\s*(\(\s*\))?\s*(!.*)?$', re.I)


def code_of(line):
    """the statement text of a line without trailing comment (strings holding '!' are not expected in the routines handled here)"""
    return line.split('!', 1)[0] if '!' in line else line


def header_end(lines, i):
    """index of the last line of the statement starting at line i (continuation lines ending in '&')"""
    while code_of(lines[i]).rstrip().endswith('&'):
        i += 1
    return i


def proc_start(line):
    """(kind, name) if the line starts a procedure"""
    m = SUB.match(line)
    if m:
        return 'subroutine', m.group(1)
    m = FUNC.match(line)
    if m and not re.match(r'^\s*end\b', line, re.I):
        return 'function', m.group(1)
    return None


def proc_end(line):
    return END_SUB.match(line) or END_FUNC.match(line)


def module_procedures(lines):
    """[(name, header line index)] of the procedures defined directly in a module (after its `contains`), in file order"""
    out = []
    in_contains = False
    depth = 0
    for i, s in enumerate(lines):
        if MODULE.match(s):
            in_contains = False
            depth = 0
        elif CONTAINS.match(s) and depth == 0:
            in_contains = True
        ps = proc_start(s)
        if ps and in_contains:
            if depth == 0:
                out.append((ps[1], i))
            depth += 1
        elif proc_end(s) and in_contains:
            depth -= 1
        elif END_MODULE.match(s):
            in_contains = False
    return out


def routine_span(lines, name):
    """(header line, contains line or None, end line) of a module-level procedure `name` that may have internal procedures"""
    for pname, i in module_procedures(lines):
        if pname.lower() == name.lower():
            break
    else:
        raise KeyError(name)
    depth = 0
    contains = None
    j = header_end(lines, i) + 1
    while j < len(lines):
        s = lines[j]
        if CONTAINS.match(s) and contains is None:
            contains = j
        if proc_start(s) and contains is not None:
            depth += 1
        if proc_end(s):
            if depth == 0:
                return i, contains, j
            depth -= 1
        j += 1
    raise ValueError('no end for ' + name)


def parse_internal(lines, contains, end):
    """{name: body lines} of the internal subroutines between `contains` and the parent's end (no arguments, checked)"""
    procs = {}
    order = []
    j = contains + 1
    while j < end:
        m = SUB.match(lines[j])
        if m:
            name = m.group(1).lower()
            h = header_end(lines, j)
            args = (m.group(2) or '').replace(' ', '')
            assert args in ('', '()'), 'internal procedure %s has arguments' % name
            k = h + 1
            while not END_SUB.match(lines[k]):
                k += 1
            procs[name] = lines[h + 1:k]
            order.append(name)
            j = k + 1
        else:
            assert not FUNC.match(lines[j]) or not code_of(lines[j]).strip(), 'internal function: ' + lines[j]
            j += 1
    return procs, order


DECL_START = re.compile(r'^\s*(real|integer|logical|double\s+precision|character|type|implicit)\b', re.I)


def inline_internal(lines, parent):
    i, contains, end = routine_span(lines, parent)
    if contains is None:
        return lines
    procs, order = parse_internal(lines, contains, end)
    labels = [90000]

    def expand(body, stack):
        out = []
        for s in body:
            m = CALL.match(s)
            if m and m.group(2).lower() in procs:
                name = m.group(2).lower()
                assert name not in stack, 'recursive internal call ' + name
                labels[0] += 1
                label = labels[0]
                inner = expand(procs[name], stack + [name])
                used = False
                for t in inner:
                    c = code_of(t)
                    assert not DECL_START.match(c), 'internal procedure %s declares %s' % (name, c.strip())
                    if re.search(r'\breturn\b', c, re.I):
                        t = re.sub(r'\breturn\b', 'goto %d' % label, t, count=1, flags=re.I)
                        used = True
                    out.append(t)
                if used:
                    out.append('%d continue' % label)
            else:
                out.append(s)
        return out

    body = expand(lines[header_end(lines, i) + 1:contains], [])
    return lines[:header_end(lines, i) + 1] + body + lines[end:]


def add_routine_seq(lines, names):
    """`!$acc routine seq` right after the header statement of each named module-level procedure"""
    wanted = {n.lower() for n in names}
    found = set()
    inserts = []
    for name, i in module_procedures(lines):
        if name.lower() in wanted:
            inserts.append(header_end(lines, i) + 1)
            found.add(name.lower())
    missing = wanted - found
    assert not missing, 'routines not found: %s' % sorted(missing)
    out = list(lines)
    for pos in sorted(inserts, reverse=True):
        out.insert(pos, ' !$acc routine seq')
    return out


DECL = re.compile(r'^\s*(real|integer|logical|double\s+precision|character|complex|type)\b[^!]*?::\s*(.*)$', re.I)


def split_top(text, sep=','):
    parts, depth, cur = [], 0, ''
    for ch in text:
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        if ch == sep and depth == 0:
            parts.append(cur)
            cur = ''
        else:
            cur += ch
    parts.append(cur)
    return parts


def module_variables(lines, module=None):
    """names declared without PARAMETER in the specification part of a module (up to its `contains` / `end module`), derived type bodies excluded"""
    start = None
    for i, s in enumerate(lines):
        m = MODULE.match(s)
        if m and (module is None or m.group(1).lower() == module.lower()):
            start = i
            break
    assert start is not None, 'module not found'
    names = []
    j = start + 1
    in_type = False
    while j < len(lines) and not CONTAINS.match(lines[j]) and not END_MODULE.match(lines[j]):
        e = header_end(lines, j)
        stmt = ' '.join(code_of(x).rstrip().rstrip('&') for x in lines[j:e + 1])
        j = e + 1
        if re.match(r'^\s*end\s*type\b', stmt, re.I):
            in_type = False
            continue
        if re.match(r'^\s*type\s*(,|::|\w)', stmt, re.I) and not re.match(r'^\s*type\s*\(', stmt, re.I):
            in_type = True
            continue
        if in_type or re.search(r'\bparameter\b', stmt, re.I):
            continue
        m = DECL.match(stmt)
        if not m:
            continue
        for part in split_top(m.group(2)):
            nm = re.match(r'\s*(\w+)', part)
            if nm:
                names.append(nm.group(1).lower())
    return names


def drop_lines(lines, patterns):
    """delete whole statements (with their continuation lines) whose code matches one of the patterns"""
    pats = [re.compile(p, re.I) for p in patterns]
    out = []
    i = 0
    while i < len(lines):
        e = header_end(lines, i)
        if any(p.search(code_of(lines[i])) for p in pats):
            i = e + 1
            continue
        out.extend(lines[i:e + 1])
        i = e + 1
    return out


def acc_list(directive, names, per_line=8):
    """`!$acc <directive>(names)` lines of at most per_line names"""
    out = []
    for i in range(0, len(names), per_line):
        out.append(' !$acc %s(%s)' % (directive, ', '.join(names[i:i + per_line])))
    return out


def insert_before(lines, pattern, new_lines):
    """insert new_lines before the first line matching pattern"""
    pat = re.compile(pattern, re.I)
    for i, s in enumerate(lines):
        if pat.search(code_of(s)):
            return lines[:i] + new_lines + lines[i:]
    raise KeyError(pattern)


def _match_fwd(s, i):
    """index of the ')' matching the '(' at s[i]"""
    d = 0
    while i < len(s):
        if s[i] == '(':
            d += 1
        elif s[i] == ')':
            d -= 1
            if d == 0:
                return i
        i += 1
    raise ValueError('unbalanced')
