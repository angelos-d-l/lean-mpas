#!/usr/bin/env python3
"""Helpers of make_run_dir.sh (the stranger's run directory). Standard library only.

    stage.py tables SRC_DIR DEST_DIR      the four Thompson tables of SRC_DIR (lean's little-endian *.DBL) written to
                                          DEST_DIR in big-endian, the form MPAS's build_tables writes on MPAS's build
                                          (gfortran -fconvert=big-endian); every value is read back and compared
    stage.py expected RUN_DIR FLAVOR OUT  the expected manifest of the forecast RUN_DIR describes: flavor, start, end,
                                          time step, step count, and the exact record times of the 'output' (history)
                                          and 'diagnostics' streams; OUT is written read-only
"""
import array
import json
import os
import re
import struct
import sys
import xml.etree.ElementTree as ET
from datetime import datetime, timedelta
from pathlib import Path

THOMPSON = ('MP_THOMPSON_QRacrQG_DATA.DBL', 'MP_THOMPSON_QRacrQS_DATA.DBL', 'MP_THOMPSON_freezeH2O_DATA.DBL',
            'MP_THOMPSON_QIautQS_DATA.DBL')
STAMP = '%Y-%m-%d_%H:%M:%S'


def records(data, order):
    """The records of a Fortran unformatted sequential file with 4-byte markers of byte order order ('<' or '>')."""
    out, i = [], 0
    while i < len(data):
        n = struct.unpack(order + 'i', data[i:i + 4])[0]
        if n < 0 or i + 8 + n > len(data) or struct.unpack(order + 'i', data[i + 4 + n:i + 8 + n])[0] != n:
            raise ValueError(f'bad record marker at byte {i}')
        out.append(data[i + 4:i + 4 + n])
        i += 8 + n
    return out


def big_endian(src, dst):
    little = records(src.read_bytes(), '<')
    with open(dst, 'wb') as out:
        for record in little:
            if len(record) % 8:
                raise ValueError(f'{src}: a record is not real(8)')
            values = array.array('d')
            values.frombytes(record)
            if sys.byteorder == 'little':
                values.byteswap()
            out.write(struct.pack('>i', len(record)) + values.tobytes() + struct.pack('>i', len(record)))
    back = records(dst.read_bytes(), '>')
    for a, b in zip(little, back):
        swapped = array.array('d')
        swapped.frombytes(b)
        swapped.byteswap()
        if swapped.tobytes() != a:
            raise ValueError(f'{dst}: read-back differs')
    if len(back) != len(little):
        raise ValueError(f'{dst}: read-back has {len(back)} records, not {len(little)}')


def namelist_value(text, name):
    found = re.search(r'^\s*' + name + r'\s*=\s*(.+?)\s*$', text, re.MULTILINE | re.IGNORECASE)
    if not found:
        raise ValueError(f'namelist.atmosphere has no {name}')
    return found.group(1).strip().strip('\'"')


def duration(text):
    """MPAS's [D_]HH:MM:SS or a number of seconds, as a timedelta."""
    days, _, clock = text.rpartition('_')
    parts = [int(part) for part in clock.split(':')]
    while len(parts) < 3:
        parts.insert(0, 0)
    return timedelta(days=int(days or 0), hours=parts[0], minutes=parts[1], seconds=parts[2])


def stream_times(streams, name, start, end):
    """The record times of output stream name: the start and every output_interval up to and with the end."""
    for element in ET.parse(streams).getroot():
        if element.get('name') == name:
            interval = element.get('output_interval', 'none')
            if interval == 'none':
                return []
            step = duration(interval)
            times, t = [], start
            while t <= end:
                times.append(t.strftime(STAMP))
                t += step
            return times
    raise ValueError(f'{streams}: no stream {name}')


def expected(run_dir, flavor, out):
    run_dir = Path(run_dir)
    text = (run_dir / 'namelist.atmosphere').read_text()
    start = datetime.strptime(namelist_value(text, 'config_start_time'), STAMP)
    length = duration(namelist_value(text, 'config_run_duration'))
    dt = float(namelist_value(text, 'config_dt'))
    end = start + length
    steps = length.total_seconds() / dt
    if steps != int(steps):
        raise ValueError(f'the run length is not a whole number of {dt} s steps')
    streams = run_dir / 'streams.atmosphere'
    manifest = {
        'about': 'the forecast the run directory describes (tests/stranger/stage.py expected); judge.py compares',
        'flavor': flavor, 'start': start.strftime(STAMP), 'end': end.strftime(STAMP), 'dt_seconds': dt,
        'steps': int(steps),
        'history': stream_times(streams, 'output', start, end),
        'diag': stream_times(streams, 'diagnostics', start, end),
    }
    out = Path(out)
    if out.exists():
        os.chmod(out, 0o644)
    out.write_text(json.dumps(manifest, indent=1) + '\n')
    os.chmod(out, 0o444)


def main(argv):
    if len(argv) == 3 and argv[0] == 'tables':
        src, dst = Path(argv[1]), Path(argv[2])
        for name in THOMPSON:
            big_endian(src / name, dst / name)
        return 0
    if len(argv) == 4 and argv[0] == 'expected':
        expected(*argv[1:])
        return 0
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == '__main__':
    try:
        sys.exit(main(sys.argv[1:]))
    except (OSError, ValueError) as error:
        sys.exit(f'stage.py: {error}')
