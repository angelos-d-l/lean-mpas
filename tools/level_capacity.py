"""Capacity bounds for locals of generated column routines; dummies keep their shape.

A local array of an `!$acc routine seq` routine gets a fixed vertical size, max_levels (max_levels+1 for nz1, kme
and kde), so the card needs no allocation inside the routine. Both device generators use this after adding routine
seq. Only a vertical bound is changed. References without an index use the original active section, so that unused
levels cannot enter array arithmetic or a reduction.
"""
import re
import devgen as dg

LEVEL = re.compile(r'\b(nz1|nz|kte|kme|kde)\b', re.I)


def capacity_locals(lines):
    out = list(lines)
    for name, _ in reversed(dg.module_procedures(out)):
        start, contains, end = dg.routine_span(out, name)
        h = dg.header_end(out, start)
        if not any(re.match(r'\s*!\$acc\s+routine', s, re.I) for s in out[h+1:end]):
            continue
        header = ' '.join(out[start:h+1]).replace('&', '')
        args = set(x.strip().lower() for x in header[header.index('(')+1:header.rindex(')')].split(',')) if '(' in header else set()
        work = {}
        edits = {}
        i = h+1
        while i < (contains or end):
            j = dg.header_end(out, i)
            stmt = ' '.join(dg.code_of(x).strip().strip('&') for x in out[i:j+1])
            if dg.DECL.match(stmt) and '::' in stmt:
                attrs, ents = stmt.split('::', 1)
                if not re.search(r'\b(intent|parameter|allocatable|pointer)\b', attrs, re.I):
                    dim = re.search(r'\bdimension\s*\(([^()]*)\)', attrs, re.I)
                    changed = []
                    touched = False
                    for ent in dg.split_top(ents):
                        m = re.match(r'\s*(\w+)\s*(?:\(([^()]*)\))?', ent)
                        if not m:
                            raise ValueError(stmt)
                        bounds = dim[1] if dim else m[2]
                        # MYNN's tridiagonal solve aliases the level count as n.
                        vertical = LEVEL if name.lower() != 'tridiag2' else re.compile(r'\bn\b', re.I)
                        if bounds and vertical.search(bounds) and m[1].lower() not in args:
                            dims = dg.split_top(bounds)
                            active = [d.strip() if ':' in d else '1:'+d.strip() for d in dims]
                            work[m[1].lower()] = (dims, active)
                            cap = vertical.sub(lambda b: 'max_levels+1' if b[0].lower() in ('nz1','kme','kde') else 'max_levels', bounds)
                            if dim:
                                pass
                            else:
                                ent = ent[:m.start(2)] + cap + ent[m.end(2):]
                            touched = True
                        changed.append(ent.strip())
                    if touched:
                        dummy_line = None
                        if dim:
                            # GF shares a declaration between a dummy and locals.
                            dummies = [e for e in changed if re.match(r'\s*(\w+)', e)[1].lower() in args]
                            if dummies:
                                dummy_line = attrs.strip()+' :: '+', '.join(dummies)
                                changed = [e for e in changed if e not in dummies]
                            attrs = attrs[:dim.start(1)] + vertical.sub(lambda b: 'max_levels+1' if b[0].lower() in ('nz1','kme','kde') else 'max_levels', dim[1]) + attrs[dim.end(1):]
                        indent = re.match(r'\s*', out[i])[0]
                        new = indent+attrs.strip()+' :: '+', '.join(changed)
                        if dummy_line:
                            new = indent+dummy_line+'\n'+new
                        edits[i] = (j, new)
            i = j+1
        if not work:
            continue
        body = []
        call_end = -1
        i = h+1
        while i < end:
            if i in edits:
                j, line = edits[i]
                body.extend(line.splitlines())
                i = j+1
                continue
            line = out[i]
            code, sep, comment = line.partition('!')
            if re.match(r'\s*call\b', code, re.I):
                call_end = dg.header_end(out, i)
            if not re.match(r'\s*(use\b|#|implicit\b)', code, re.I) and not dg.DECL.match(code):
                for var, (dims, active) in work.items():
                    pat = re.compile(r'(?<![\w%])'+var+r'\b', re.I)
                    def section(m):
                        tail = code[m.end():]
                        # A keyword keeps the callee's dummy name.
                        prefix = code[:m.start()]
                        keyword = prefix.rstrip().endswith(('(', ',')) or (i <= call_end and not prefix.replace('&', '').strip())
                        if re.match(r'\s*=(?!=)', tail) and keyword:
                            return m[0]
                        indexed = re.match(r'\s*\(', tail)
                        if indexed:
                            return m[0]
                        return m[0]+'('+','.join(active)+')'
                    code = pat.sub(section, code)
                    # Existing full slices must also stop at the active level bound.
                    pat = re.compile(r'(?<![\w%])('+var+r')\s*\(([^()]*)\)', re.I)
                    def slices(m):
                        subs = dg.split_top(m[2])
                        if len(subs) != len(dims):
                            return m[0]
                        for d in range(len(subs)):
                            if LEVEL.search(dims[d]) or (name.lower() == 'tridiag2' and re.search(r'\bn\b', dims[d])):
                                parts = subs[d].split(':')
                                if len(parts) > 1 and not parts[1].strip():
                                    if not parts[0].strip():
                                        parts[0] = active[d].split(':')[0]
                                    parts[1] = active[d].split(':')[-1]
                                    subs[d] = ':'.join(parts)
                        return m[1]+'('+','.join(subs)+')'
                    code = pat.sub(slices, code)
            body.append(code+sep+comment)
            i += 1
        out[h+1:end] = [' use config, only: max_levels'] + body
    return out
