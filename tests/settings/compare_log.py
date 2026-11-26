"""A lean log against an MPAS log of the same run: the timestep stamps, the boundary intervals, the radiation decisions
of each step and the first step's extrema; the fields themselves are compared elsewhere.

  compare_log.py LEAN_LOG MPAS_LOG STEPS
"""
import math
import re
import sys
from pathlib import Path

actual, reference, count = Path(sys.argv[1]), Path(sys.argv[2]), int(sys.argv[3])
if not reference.exists():
    print(f'SKIP MPAS log comparison: no {reference}')
    sys.exit(0)
text, ref = actual.read_text(), reference.read_text()
stamps = lambda s: re.findall(r'Begin timestep (\S+)', s)
a, b = stamps(text), stamps(ref)[:count]
assert len(a) == count, (len(a), count)
assert a == b, 'timestep stamps differ'
intervals = lambda s: re.findall(r'Updated lateral boundary conditions\. LBCs are now valid\s+from ([^\n]+)', s)
# Compare only intervals opened during the requested steps (the reference may run one more step).
blocks = ref.split('Begin timestep ')
ref_prefix = blocks[0] + 'Begin timestep '.join(blocks[1:count+1])
assert intervals(text) == intervals(ref_prefix), (intervals(text), intervals(ref_prefix))
# Each block must contain both manager decisions, including steps with no radiation.
def radiation_steps(log):
    result = []
    for block in log.split('Begin timestep ')[1:count+1]:
        flags = re.findall(r'L_RAD(LW|SW) = ([TF])', block)
        assert [half for half, flag in flags] == ['LW', 'SW'], ('radiation log lines', flags)
        result.append(flags)
    return result
assert radiation_steps(text) == radiation_steps(ref), 'per-step radiation decisions differ'

extrema = lambda s: re.findall(r'global min, max ([wu])\s+([^\n]+)', s)
a, b = extrema(text), extrema(ref_prefix)
assert len(a) == len(b) == 2*count, (len(a), len(b))
for _, line in a:
    assert all(math.isfinite(v) for v in map(float,line.split())), 'non-finite extrema'
def values(pair):
    return pair[0], list(map(float,pair[1].split()))
differences = sum(values(x) != values(y) for x,y in zip(a[2:], b[2:]))
print(f'MPAS LOG TIMING OK: {count} stamps, boundary intervals and radiation decisions; later extrema lines differing {differences}/{2*(count-1)}')
# The first time step's extrema: exact printed digits with LOG_EXACT=1 (a lean nvfortran strict build against the
# nvfortran-built MPAS reference). Otherwise (another compiler, such as gfortran) each value within 2 units of the
# reference's last printed digit.
import os
def close(x, y):
    if x[0] != y[0] or len(x[1].split()) != len(y[1].split()): return False
    if os.environ.get('LOG_EXACT') == '1': return values(x) == values(y)
    return all(abs(float(u) - float(v)) <= 2.01 * 10.0 ** -(len(v.split('.')[1]) if '.' in v else 0)
               for u, v in zip(x[1].split(), y[1].split()))
mismatches = [(x,y) for x,y in zip(a[:2],b[:2]) if not close(x,y)]
if mismatches:
    for x,y in mismatches:
        print(f'FAIL step-1 extrema: lean {x}, MPAS {y}')
    sys.exit(2)
print('MPAS LOG OK: step-1 extrema ' + ('equal MPAS printed digits' if os.environ.get('LOG_EXACT') == '1'
      else 'within 2 units of MPAS last printed digit (LOG_EXACT=1 for the exact check)'))
