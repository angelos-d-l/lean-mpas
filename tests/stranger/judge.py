#!/usr/bin/env python3
"""The judge of a stranger round (tests/stranger/container.sh, mac.sh): did the tester, with the README only, build
lean_mpas and run the forecast? Standard library only.

    judge.py WORK EXPECTED [--commit SHA]   JUDGE OK (<n> minor items) | JUDGE FAIL: <why> (one line each), exit 0 | 1
    judge.py --self-test                    a finished fixture passes, each negative fixture fails for its reason

WORK is the round's work root on this machine (for the container, the folder mounted at /work); EXPECTED the
expected.json that make_run_dir.sh wrote beside the run directory, outside WORK. The commit to find is --commit, else the
HEAD of WORK/lean_mpas.bundle (git bundle list-heads). The judge passes only when:
- WORK/.record (tests/stranger/record) shows a finished command with exit status 0;
- the recorded executable is a lean_mpas with build.json beside it naming that commit, not dirty, and the build's
  flavor (generated/lean_build_info.f90) is the expected one;
- the recorded run directory has a complete log.atmosphere.0000.out (the flavor line, the timer block, the closing line,
  no error) with exactly the expected steps from the expected start to the expected end, and no log.atmosphere.0000.err;
- the history.*.nc and diag.*.nc files there hold exactly the expected record times (none missing, none extra, none
  twice), each file a classic netCDF file (CDF-1, CDF-2 or CDF-5) that opens, written after the recorded start, with
  the expected global attribute lean_flavor;
- WORK/report.md says `Finished: yes` and marks no item [blocked] or [guessed].
Paths in .record are taken relative to its work line, so a record made inside the container judges on the host.
"""
import argparse
import json
import os
import re
import struct
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timedelta
from pathlib import Path

STAMP = '%Y-%m-%d_%H:%M:%S'
LOG = 'log.atmosphere.0000.out'
ERR = 'log.atmosphere.0000.err'
SIZES = {1: 1, 2: 1, 3: 2, 4: 4, 5: 4, 6: 8, 7: 1, 8: 2, 9: 4, 10: 8, 11: 8}


# ---- classic netCDF (CDF-1, CDF-2, CDF-5) headers and the xtime records -------------------------------------------

class NetCDFError(Exception):
    pass


class Header:
    def __init__(self, data):
        if data[:3] != b'CDF' or data[3] not in (1, 2, 5):
            raise NetCDFError('not a classic netCDF file (CDF-1, CDF-2 or CDF-5)')
        self.data, self.pos, self.version = data, 4, data[3]
        self.long = self.version == 5
        self.numrecs = self.count()
        self.dims = [(self.name(), self.count()) for _ in self.items(10)]
        self.attrs = self.attributes()
        self.vars = {}
        for _ in self.items(11):
            name = self.name()
            dimids = [self.count() for _ in range(self.count())]
            attrs = self.attributes()
            kind = self.int32()
            vsize = self.count()
            begin = self.uint64() if self.version != 1 else self.int32()
            self.vars[name] = {'dims': dimids, 'type': kind, 'vsize': vsize, 'begin': begin, 'attrs': attrs}

    def take(self, n):
        if self.pos + n > len(self.data):
            raise NetCDFError('header ends early')
        piece = self.data[self.pos:self.pos + n]
        self.pos += n
        return piece

    def int32(self):
        return struct.unpack('>i', self.take(4))[0]

    def uint64(self):
        return struct.unpack('>Q', self.take(8))[0]

    def count(self):
        return self.uint64() if self.long else struct.unpack('>I', self.take(4))[0]

    def name(self):
        n = self.count()
        text = self.take(n).decode('utf-8', 'replace')
        self.take((4 - n % 4) % 4)
        return text

    def items(self, tag):
        found = self.int32()
        n = self.count()
        if found not in (0, tag) or (found == 0 and n):
            raise NetCDFError('bad header list')
        return range(n)

    def attributes(self):
        out = {}
        for _ in self.items(12):
            name = self.name()
            kind = self.int32()
            n = self.count()
            if kind not in SIZES:
                raise NetCDFError(f'attribute {name}: unknown type {kind}')
            raw = self.take(n * SIZES[kind])
            self.take((4 - len(raw) % 4) % 4)
            out[name] = raw.decode('utf-8', 'replace').rstrip('\0') if kind == 2 else raw
        return out


def read_times(path):
    """(the global lean_flavor or None, the xtime strings of every record) of a classic netCDF file."""
    with open(path, 'rb') as stream:
        head = stream.read(1 << 20)
        try:
            header = Header(head)
        except NetCDFError as error:
            if 'ends early' not in str(error):
                raise
            stream.seek(0)
            header = Header(stream.read())
        unlimited = [i for i, (_, length) in enumerate(header.dims) if length == 0]
        xtime = header.vars.get('xtime')
        if xtime is None or xtime['type'] != 2 or not xtime['dims'] or xtime['dims'][0] not in unlimited:
            raise NetCDFError('no record variable xtime of characters')
        record_vars = [v for v in header.vars.values() if v['dims'] and v['dims'][0] in unlimited]
        width = 1
        for dim in xtime['dims'][1:]:
            width *= header.dims[dim][1]
        recsize = width if len(record_vars) == 1 else sum(v['vsize'] for v in record_vars)
        numrecs = header.numrecs
        if numrecs in (0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF):
            raise NetCDFError('streaming record count: the file was not closed')
        times = []
        for record in range(numrecs):
            stream.seek(xtime['begin'] + record * recsize)
            text = stream.read(width)
            if len(text) != width:
                raise NetCDFError(f'record {record + 1} of {numrecs} is cut short')
            times.append(text.decode('utf-8', 'replace').rstrip('\0 '))
        return header.attrs.get('lean_flavor'), times


# ---- the checks ----------------------------------------------------------------------------------------------------

def read_record(work):
    path = work / '.record'
    if not path.is_file():
        return None, ['no .record: the forecast was not started with record']
    record = {'arg': []}
    for line in path.read_text(errors='replace').splitlines():
        key, _, value = line.partition('=')
        if key == 'arg':
            record['arg'].append(value)
        elif key:
            record[key] = value
    return record, []


def mapped(record, path, work):
    """path of the machine that recorded it, on this machine: relative to the recorded work root."""
    root = record.get('work', '')
    if root and (path == root or path.startswith(root.rstrip('/') + '/')):
        return work / os.path.relpath(path, root)
    return Path(path)


def bundle_commit(work):
    bundle = work / 'lean_mpas.bundle'
    try:
        done = subprocess.run(['git', 'bundle', 'list-heads', str(bundle)], capture_output=True, text=True)
    except OSError:
        return None
    for line in done.stdout.splitlines():
        sha, _, ref = line.partition(' ')
        if ref == 'HEAD':
            return sha
    return None


def check_build(record, work, expected, commit):
    fails = []
    exe = mapped(record, record.get('executable', ''), work)
    if exe.name != 'lean_mpas' or not exe.is_file():
        return [f'the recorded executable {record.get("executable")} is not a built lean_mpas']
    build_json = exe.parent / 'build.json'
    if not build_json.is_file():
        return [f'no build.json beside {exe}']
    try:
        build = json.loads(build_json.read_text())
    except ValueError:
        return [f'{build_json} is not JSON']
    if build.get('commit') != commit:
        fails.append(f'the executable was built from commit {build.get("commit")}, not the bundle\'s {commit}')
    if build.get('dirty') is not False:
        fails.append(f'the executable was built from a changed tree (build.json dirty: {build.get("dirty")})')
    info = exe.parent / 'generated' / 'lean_build_info.f90'
    found = re.search(r"build_flavor = '([^']*)'", info.read_text()) if info.is_file() else None
    if found is None:
        fails.append(f'no build_flavor in {info}')
    elif found.group(1) != expected['flavor']:
        fails.append(f'the executable is the {found.group(1)!r} flavor, not {expected["flavor"]!r}')
    return fails


def check_log(run_dir, expected):
    fails = []
    if (run_dir / ERR).exists():
        fails.append(f'{ERR} exists: the run reported an error')
    log = run_dir / LOG
    if not log.is_file():
        return fails + [f'no {LOG} in {run_dir}']
    lines = log.read_text(errors='replace').splitlines()
    flavor = [m.group(1) for m in (re.search(r'lean_mpas flavor: (\S+)', line) for line in lines) if m]
    if flavor != [expected['flavor']]:
        fails.append(f'the log names the flavor {flavor}, not [{expected["flavor"]!r}]')
    if not any(line.strip() == 'Timer information:' for line in lines):
        fails.append('the log has no timer block: incomplete log')
    tail = [line for line in lines if line.strip()]
    if not tail or not tail[-1].strip().startswith('Logging complete.  Closing file at'):
        fails.append('the log does not end with its closing line: incomplete log')
    for kind in ('Error messages', 'Critical error messages'):
        counts = [line for line in lines if line.strip().startswith(kind + ' =')]
        if counts and counts[-1].split('=')[1].strip() != '0':
            fails.append(f'the log counts {counts[-1].strip()}')
    steps = [line.strip()[len('Begin timestep '):] for line in lines if line.strip().startswith('Begin timestep ')]
    if len(steps) != expected['steps']:
        fails.append(f'the log has {len(steps)} steps, not {expected["steps"]}')
    if steps and steps[0] != expected['start']:
        fails.append(f'the forecast started at {steps[0]}, not {expected["start"]}')
    if steps:
        try:
            end = datetime.strptime(steps[-1], STAMP) + timedelta(seconds=expected['dt_seconds'])
            if end.strftime(STAMP) != expected['end']:
                fails.append(f'the forecast ended at {end.strftime(STAMP)}, not {expected["end"]}')
        except ValueError:
            fails.append(f'unreadable step time {steps[-1]!r}')
    return fails


def check_outputs(run_dir, expected, start):
    fails = []
    for prefix, key in (('history', 'history'), ('diag', 'diag')):
        seen = []
        for path in sorted(run_dir.glob(prefix + '.*.nc')):
            if path.stat().st_mtime < start:
                fails.append(f'{path.name} was written before the forecast started (a stale output)')
            try:
                flavor, times = read_times(path)
            except (OSError, NetCDFError) as error:
                fails.append(f'{path.name} does not open: {error}')
                continue
            if flavor != expected['flavor']:
                fails.append(f'{path.name} has lean_flavor {flavor!r}, not {expected["flavor"]!r}')
            seen += times
        want = expected[key]
        missing = sorted(set(want) - set(seen))
        extra = sorted(set(seen) - set(want))
        twice = sorted({t for t in seen if seen.count(t) > 1})
        if missing:
            fails.append(f'{prefix} records missing: {", ".join(missing)}')
        if extra:
            fails.append(f'{prefix} records not expected (extra records): {", ".join(extra)}')
        if twice:
            fails.append(f'{prefix} records written twice: {", ".join(twice)}')
    return fails


def check_report(work):
    report = work / 'report.md'
    if not report.is_file():
        return ['no report.md'], 0
    text = report.read_text(errors='replace')
    fails = []
    if not re.search(r'^\s*\**finished:?\**:?\s*\**yes\b', text, re.IGNORECASE | re.MULTILINE):
        fails.append('report.md does not say Finished: yes')
    for mark in ('blocked', 'guessed'):
        n = len(re.findall(r'\[' + mark + r'\]', text, re.IGNORECASE))
        if n:
            fails.append(f'report.md marks {n} item(s) [{mark}]')
    return fails, len(re.findall(r'\[minor\]', text, re.IGNORECASE))


def judge(work, expected, commit):
    """(fails, minor): the reasons the round fails (empty when it passes) and the count of [minor] items."""
    work = Path(work)
    record, fails = read_record(work)
    if record is None:
        return fails, 0
    if 'status' not in record:
        fails.append('the recorded command has no exit status: it did not finish')
    elif record['status'] != '0':
        fails.append(f'the recorded exit status is {record["status"]}, not 0')
    if commit is None:
        fails.append('no commit to compare: no --commit and no HEAD in lean_mpas.bundle')
    else:
        fails += check_build(record, work, expected, commit)
    run_dir = mapped(record, record.get('cwd', ''), work)
    fails += check_log(run_dir, expected)
    fails += check_outputs(run_dir, expected, int(record.get('start', '0') or 0))
    report_fails, minor = check_report(work)
    return fails + report_fails, minor


def main(argv=None):
    parser = argparse.ArgumentParser(prog='judge.py', description=__doc__.split('\n')[0])
    parser.add_argument('work', nargs='?', type=Path)
    parser.add_argument('expected', nargs='?', type=Path)
    parser.add_argument('--commit')
    parser.add_argument('--self-test', action='store_true')
    args = parser.parse_args(argv)
    if args.self_test:
        return self_test()
    if args.work is None or args.expected is None:
        parser.error('WORK and EXPECTED are needed')
    expected = json.loads(args.expected.read_text())
    fails, minor = judge(args.work, expected, args.commit or bundle_commit(args.work))
    for why in fails:
        print(f'JUDGE FAIL: {why}')
    if not fails:
        print(f'JUDGE OK ({minor} minor items)')
    return 1 if fails else 0


# ---- self-test -----------------------------------------------------------------------------------------------------

EXPECTED = {'flavor': 'lean', 'start': '2026-11-08_00:00:00', 'end': '2026-11-08_03:00:00', 'dt_seconds': 120.0,
            'steps': 90, 'history': ['2026-11-08_00:00:00', '2026-11-08_03:00:00'],
            'diag': ['2026-11-08_00:00:00', '2026-11-08_01:00:00', '2026-11-08_02:00:00', '2026-11-08_03:00:00']}
COMMIT = '0123456789abcdef0123456789abcdef01234567'


def write_cdf(path, times, flavor, version=5):
    """A classic netCDF file of version 1 or 5: dims Time (record) and StrLen 64, the global lean_flavor, a record
    variable xtime and, so that the record size is a sum, a record variable t of one float."""
    def count(n):
        return struct.pack('>Q' if version == 5 else '>I', n)

    def name(text):
        raw = text.encode()
        return count(len(raw)) + raw + b'\0' * ((4 - len(raw) % 4) % 4)

    head = b'CDF' + bytes([version]) + count(len(times))
    head += struct.pack('>i', 10) + count(2) + name('Time') + count(0) + name('StrLen') + count(64)
    raw = flavor.encode()
    head += (struct.pack('>i', 12) + count(1) + name('lean_flavor') + struct.pack('>i', 2) + count(len(raw)) + raw
             + b'\0' * ((4 - len(raw) % 4) % 4))
    offset = 8 if version == 5 else 4

    def var(text, dims, kind, vsize, begin):
        return (name(text) + count(len(dims)) + b''.join(count(d) for d in dims) + struct.pack('>i', 0) + count(0)
                + struct.pack('>i', kind) + count(vsize) + struct.pack('>Q' if version != 1 else '>i', begin))
    size = len(head) + 4 + len(count(0)) + len(var('xtime', [0, 1], 2, 64, 0)) + len(var('t', [0], 5, 4, 0))
    body = struct.pack('>i', 11) + count(2) + var('xtime', [0, 1], 2, 64, size) + var('t', [0], 5, 4, size + 64)
    data = head + body
    assert len(data) == size, (len(data), size, offset)
    for stamp in times:
        data += stamp.encode().ljust(64, b' ') + struct.pack('>f', 1.0)
    Path(path).write_bytes(data)


def good_fixture(work):
    """A finished round: the record, the build, the run directory with its log and outputs, the report."""
    exe_dir = work / 'lean_mpas' / 'build' / 'cpu-gfortran-strict-lean'
    (exe_dir / 'generated').mkdir(parents=True)
    (exe_dir / 'lean_mpas').write_text('')
    (exe_dir / 'build.json').write_text(json.dumps({'commit': COMMIT, 'dirty': False}))
    (exe_dir / 'generated' / 'lean_build_info.f90').write_text(" character(len=*), parameter :: build_flavor = 'lean'\n")
    run = work / 'run'
    run.mkdir()
    (run / 'atmosphere_model').symlink_to(exe_dir / 'lean_mpas')
    start = int(time.time()) - 100
    write_record(work, run, 0, start)
    write_log(run, 'lean', EXPECTED['start'], EXPECTED['steps'])
    for stamp in EXPECTED['history']:
        write_cdf(run / f'history.{stamp.replace(":", ".")}.nc', [stamp], 'lean', 5)
    for stamp in EXPECTED['diag']:
        write_cdf(run / f'diag.{stamp.replace(":", ".")}.nc', [stamp], 'lean', 1)
    (work / 'report.md').write_text('Finished: yes\n\n- README line 12: the clone URL is a placeholder [minor]\n')
    return run


def write_record(work, run, status, start):
    lines = [f'work=/work', f'cwd=/work/{run.relative_to(work)}', 'arg=./atmosphere_model',
             f'executable=/work/lean_mpas/build/cpu-gfortran-strict-lean/lean_mpas', f'start={start}']
    if status is not None:
        lines += [f'status={status}', f'end={start + 60}']
    (work / '.record').write_text('\n'.join(lines) + '\n')


def write_log(run, flavor, start, steps, complete=True):
    t0 = datetime.strptime(start, STAMP)
    lines = ['-' * 70, 'Beginning MPAS-atmosphere Output Log File for task       0 of       1', '',
             ' Compile-time options:', f'   lean_mpas flavor: {flavor}', '']
    lines += [f' Begin timestep {(t0 + timedelta(seconds=120 * i)).strftime(STAMP)}' for i in range(steps)]
    if complete:
        lines += ['', '', ' Timer information: ', ' -----------------------------------------',
                  ' Total log messages printed:', '    Output messages =                 1516',
                  '    Warning messages =                  15', '    Error messages =                     0',
                  '    Critical error messages =            0', ' -----------------------------------------',
                  ' Logging complete.  Closing file at 2026/11/22 18:36:56']
    (run / LOG).write_text('\n'.join(lines) + '\n')


def self_test():
    def extra(work, run):
        write_cdf(run / 'diag.2026-11-08_04.00.00.nc', ['2026-11-08_04:00:00'], 'lean')

    def two_records(work, run):
        write_cdf(run / 'diag.2026-11-08_03.00.00.nc', ['2026-11-08_03:00:00', '2026-11-08_03:00:00'], 'lean')

    def longer(work, run):
        write_log(run, 'lean', EXPECTED['start'], 120)
        write_cdf(run / 'history.2026-11-08_06.00.00.nc', ['2026-11-08_06:00:00'], 'lean')
        for hour in (4, 5, 6):
            write_cdf(run / f'diag.2026-11-08_0{hour}.00.00.nc', [f'2026-11-08_0{hour}:00:00'], 'lean')

    def shorter(work, run):
        write_log(run, 'lean', EXPECTED['start'], 60)
        (run / 'diag.2026-11-08_03.00.00.nc').unlink()
        (run / 'history.2026-11-08_03.00.00.nc').unlink()

    def changed_start(work, run):
        write_log(run, 'lean', '2026-11-08_01:00:00', 90)
        for path in run.glob('*.nc'):
            path.unlink()
        for hour in (1, 4):
            write_cdf(run / f'history.2026-11-08_0{hour}.00.00.nc', [f'2026-11-08_0{hour}:00:00'], 'lean')

    def wrong_flavor(work, run):
        write_log(run, 'mpas', EXPECTED['start'], 90)
        info = work / 'lean_mpas/build/cpu-gfortran-strict-lean/generated/lean_build_info.f90'
        info.write_text(" character(len=*), parameter :: build_flavor = 'mpas'\n")
        write_cdf(run / 'diag.2026-11-08_01.00.00.nc', ['2026-11-08_01:00:00'], 'mpas')

    def stale(work, run):
        old = time.time() - 10000
        os.utime(run / 'diag.2026-11-08_02.00.00.nc', (old, old))

    def nonzero(work, run):
        write_record(work, run, 1, int(time.time()) - 100)

    def unfinished(work, run):
        write_record(work, run, None, int(time.time()) - 100)

    def incomplete_log(work, run):
        write_log(run, 'lean', EXPECTED['start'], 90, complete=False)

    def err_log(work, run):
        (run / ERR).write_text('CRITICAL ERROR: x\n')

    def other_commit(work, run):
        path = work / 'lean_mpas/build/cpu-gfortran-strict-lean/build.json'
        path.write_text(json.dumps({'commit': 'f' * 40, 'dirty': False}))

    def dirty(work, run):
        path = work / 'lean_mpas/build/cpu-gfortran-strict-lean/build.json'
        path.write_text(json.dumps({'commit': COMMIT, 'dirty': True}))

    def not_netcdf(work, run):
        (run / 'diag.2026-11-08_01.00.00.nc').write_bytes(b'\x89HDF\r\n\x1a\n' + b'\0' * 64)

    def blocked(work, run):
        (work / 'report.md').write_text('Finished: yes\n\n- the tables [blocked]\n- the suite [guessed]\n')

    def not_finished(work, run):
        (work / 'report.md').write_text('Finished: no\n')

    cases = [
        ('extra records', extra, 'extra records'), ('a record twice', two_records, 'written twice'),
        ('a longer run', longer, 'steps, not 90'), ('a shortened run', shorter, 'records missing'),
        ('a changed start', changed_start, 'started at 2026-11-08_01:00:00'),
        ('a wrong flavor', wrong_flavor, "flavor, not 'lean'"), ('a stale output', stale, 'stale output'),
        ('a non-zero recorded exit', nonzero, 'exit status is 1'), ('an unfinished command', unfinished, 'no exit status'),
        ('an incomplete log', incomplete_log, 'incomplete log'), ('an error log', err_log, ERR),
        ('another commit', other_commit, 'not the bundle'), ('a dirty build', dirty, 'changed tree'),
        ('a NetCDF-4 output', not_netcdf, 'does not open'), ('a blocked item', blocked, '[blocked]'),
        ('a report that did not finish', not_finished, 'Finished: yes'),
    ]
    bad = 0
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp) / 'good'
        work.mkdir()
        good_fixture(work)
        fails, minor = judge(work, EXPECTED, COMMIT)
        if fails or minor != 1:
            print(f'SELF-TEST FAIL: a finished run: {fails}, {minor} minor')
            bad += 1
        else:
            print('ok    a finished run: JUDGE OK (1 minor items)')
        for label, change, reason in cases:
            work = Path(tmp) / label.replace(' ', '_')
            work.mkdir()
            run = good_fixture(work)
            change(work, run)
            fails, _ = judge(work, EXPECTED, COMMIT)
            if any(reason in why for why in fails):
                print(f'ok    {label}: JUDGE FAIL: {next(why for why in fails if reason in why)}')
            else:
                print(f'SELF-TEST FAIL: {label}: expected a failure with {reason!r}, got {fails}')
                bad += 1
    print('JUDGE SELF-TEST OK' if not bad else f'JUDGE SELF-TEST FAIL: {bad}')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
