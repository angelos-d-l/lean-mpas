"""Write a little-endian Fortran unformatted file (lean's Thompson .DBL tables) in big-endian, the form MPAS built
with -byteswapio reads. Same values, record by record; the copy is read back and checked.

  dbl_big_endian.py IN OUT
"""
import struct, sys
import numpy as np


def records(data, e):
    out, i = [], 0
    while i < len(data):
        n = struct.unpack(e + 'i', data[i:i + 4])[0]
        if struct.unpack(e + 'i', data[i + 4 + n:i + 8 + n])[0] != n:
            sys.exit(f'bad record marker at byte {i}')
        out.append(data[i + 4:i + 4 + n]); i += 8 + n
    return out


if len(sys.argv) != 3 or sys.argv[1] in ('-h', '--help'):
    print(__doc__.strip())
    sys.exit(0 if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help') else 2)
src, dst = sys.argv[1:3]
little = records(open(src, 'rb').read(), '<')
with open(dst, 'wb') as f:
    for r in little:
        if len(r) % 8: sys.exit('record is not real(8)')
        f.write(struct.pack('>i', len(r))); f.write(np.frombuffer(r, '<f8').astype('>f8').tobytes()); f.write(struct.pack('>i', len(r)))
big = records(open(dst, 'rb').read(), '>')
same = len(big) == len(little) and all(np.array_equal(np.frombuffer(a, '<u8'), np.frombuffer(b, '>u8'))   # bits, not values
                                       for a, b in zip(little, big))
if not same: sys.exit('read-back differs')
print(f'{dst}: {len(big)} records, values equal to {src}')
