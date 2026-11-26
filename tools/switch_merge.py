"""Build-switch blocks in Fortran text.

resolve() gives the lines one build compiles from text with #ifdef/#ifndef blocks of a switch; tools/gen_memory.py
and tools/report_state_memory.py use it to read the state types as a build with or without a switch sees them.
"""
import re


def resolve(lines, switch, on):
    """the lines one build compiles: the #ifdef/#ifndef/#else/#endif blocks of switch resolved; blocks of other switches stay,
    also inside them"""
    out, keep, depth = [], None, 0
    for s in lines:
        m = re.match(r'#(if|ifdef|ifndef|elif|else|endif)\b\s*(\w*)', s)
        d = m.group(1) if m else ''
        if keep is None and d in ('ifdef', 'ifndef') and m.group(2) == switch:
            keep = on if d == 'ifdef' else not on
        elif keep is not None and d in ('if', 'ifdef', 'ifndef'):
            depth += 1
            if keep:
                out.append(s)
        elif keep is not None and depth and d == 'endif':
            depth -= 1
            if keep:
                out.append(s)
        elif keep is not None and depth == 0 and d == 'else':
            keep = not keep
        elif keep is not None and depth == 0 and d == 'endif':
            keep = None
        elif keep is None or keep:
            out.append(s)
    assert keep is None, 'unterminated #if'
    return out
