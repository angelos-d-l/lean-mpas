"""MPAS's messages of a failed sea-level pressure (with the first failing column) and of a layer mean whose pressure
does not increase, as the isobaric check writes them to its log."""
from pathlib import Path
import re
import sys
text=Path(sys.argv[1] if len(sys.argv)>1 else 'build/diag/log.atmosphere.0000.out').read_text()
lines=[line.strip() for line in text.splitlines() if line.strip()]
start=next(i for i,line in enumerate(lines) if line.startswith('Troubles finding level '))
block=lines[start:start+4]
assert re.fullmatch(r'Troubles finding level .* above ground\.',block[0]),block
assert block[1]=='Problems first occur at (1)',block
assert re.fullmatch(r'Surface pressure = .* hPa\.',block[2]),block
assert block[3]=='*** MSLP field will not be computed',block
assert 'ERROR: Error in compute_layer_mean: pressure should increase with index' in lines
print('ISOBARIC MESSAGES OK')
