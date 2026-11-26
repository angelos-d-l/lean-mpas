"""Serial real-model restart checks: one plain build (flavor mpas), every run in atmosphere_model's form in an MPAS run
directory, a restarted run judged by its NetCDF outputs against the continuous run's. They need the release case inputs
in data/cases. Scratch run directories are always removed. RESTART_CASE=<name> runs one case; RESTART_MEASURE=1 also
reports the differences from MPAS's own restart runs where data/cases holds them."""
from pathlib import Path
from contextlib import ExitStack
import hashlib
import json
import os
import re
import shlex
import shutil
import sys
import subprocess
import time

import numpy as np

from netCDF4 import Dataset

ROOT = Path(__file__).resolve().parents[2]
os.chdir(ROOT)
sys.path.insert(0, str(ROOT))
from tests.restart.compare_runs import compare_netcdf, compare_variable
sys.path.insert(0, str(ROOT / "tools"))
from gen_restart_set import copies, inventory
sys.path.insert(0, str(ROOT / 'tools/cases'))
import make_sfc_update
from tools.field_classes import load as field_classes
from tools.cases.run_spec import edit_namelist

FC = sys.argv[1] if len(sys.argv) > 1 else 'gfortran'
JOBS = sys.argv[2] if len(sys.argv) > 2 else '2'
assert 1 <= int(JOBS) <= 2
WORK = ROOT / 'build/restart/runs'
GREECE = 'greece_60km'
X1 = 'x1_2562'
INIT = {GREECE: 'greece_60km.init.nc', X1: 'x1.2562.init.nc'}
SURFACE_NAME = {GREECE: 'greece_60km.sfc_update.nc', X1: 'x1.2562.sfc_update.nc'}
GREECE_SURFACE = ROOT / 'data/cases/greece_60km/inputs/greece_60km.sfc_update.nc'
STAMP1 = '2026-11-08_01:00:00'
FILE1 = 'restart.2026-11-08_01.00.00.nc'
BUILD = []
LOGS = ('log.atmosphere.0000.out', 'log.atmosphere.0000.err')


def command(args, log, cwd=ROOT, env=None, expected=0):
    with log.open('w') as out:
        result = subprocess.run(list(map(str, args)), cwd=cwd, env=env, stdout=out, stderr=subprocess.STDOUT)
    if expected is not None and result.returncode != expected:
        print(log.read_text()[-10000:])
        raise AssertionError(f'{args[0]}: exit {result.returncode}, expected {expected}')
    return result.returncode


def build():
    """The one build of every case."""
    if not BUILD:
        log = WORK / 'build.log'
        command(['./lean', 'build', '--flavor', 'mpas', '--target', 'cpu', '--fc', FC, '--jobs', JOBS, '--json'], log)
        BUILD.append(Path(json.loads(log.read_text()[log.read_text().index('{'):])['build_json']).resolve().parent)
    return BUILD[0]


def directory(name, mesh, spec, inputs=None, surface=None):
    rd = WORK / name
    inp = inputs or ROOT / 'data/cases' / mesh / 'inputs'
    base = 'strict_12h.txt' if mesh == GREECE else 'strict_24h.txt'
    sources = [f'cases/{mesh}/case.txt', f'cases/{mesh}/mpas_runs/{base}']
    extra = WORK / (name + '.spec')
    extra.write_text(spec)
    cmd = 'root=' + shlex.quote(str(ROOT)) + '; . tests/inputs/run_dir.sh; make_run_dir '
    cmd += ' '.join(shlex.quote(str(path)) for path in [rd, inp, *sources, extra])
    command(['sh', '-c', cmd], WORK / (name + '.setup.log'))
    if surface is not None:
        # the surface file of the surface update: make_run_dir links only the init and boundary files
        (rd / SURFACE_NAME[mesh]).symlink_to(Path(surface).resolve())
    return rd


def set_steps(rd, steps):
    """config_run_duration of the run directory's namelist: `steps` steps of 120 s."""
    s = steps * 120
    path = rd / 'namelist.atmosphere'
    path.write_text(edit_namelist(path.read_text(), 'nhyd_model', 'config_run_duration',
                                  f"'{s // 86400}_{s // 3600 % 24:02d}:{s // 60 % 60:02d}:{s % 60:02d}'"))


def run(rd, mesh, steps, env=None, expected=0, name=None):
    """atmosphere_model's form in the run directory, `steps` steps from its start. Returns the exit code and the text of
    log.atmosphere.0000.out (of stdout and the .err file if it stopped before opening it)."""
    label = name or rd.name
    set_steps(rd, steps)
    launcher = rd / 'atmosphere_model'
    if not launcher.is_symlink():
        launcher.symlink_to(build() / 'lean_mpas')
    for log_name in LOGS:
        (rd / log_name).unlink(missing_ok=True)
    start = time.monotonic()
    out = WORK / (label + '.stdout')
    code = command([launcher], out, rd, dict(os.environ, **(env or {})), None)
    log = rd / LOGS[0]
    text = log.read_text() if log.is_file() else out.read_text() + (
        (rd / LOGS[1]).read_text() if (rd / LOGS[1]).is_file() else '')
    (WORK / (label + '.log')).write_text(text)
    if expected is not None and code != expected:
        print(text[-10000:])
        raise AssertionError(f'{label}: exit {code}, expected {expected}')
    print(f'{label}: wall {time.monotonic()-start:.3f} s', flush=True)
    return code, text


def same(left, right):
    assert left.is_file() and right.is_file(), (left, right)
    with left.open('rb') as a, right.open('rb') as b:
        while block := a.read(1024 * 1024):
            assert block == b.read(1024 * 1024), f'{left.name}: bytes differ'
        assert not b.read(1), f'{left.name}: lengths differ'


def file_greece():
    rd = directory('file_greece', GREECE, 'stream restart output_interval 1:00:00\n')
    run(rd, GREECE, 30)
    path = rd / FILE1
    assert path.is_file(), f'FAIL: no restart file {FILE1}'
    print(f'greece_60km restart: {path.stat().st_size} bytes', flush=True)
    with Dataset(path) as rst, Dataset(rd / INIT[GREECE]) as init:
        assert re.fullmatch(r'[0-9a-f]{16}', rst.lean_restart)
        assert rst.lean_restart_steps == 30
        assert rst.lean_flavor == 'mpas'
        assert 'lean_switches' in rst.ncattrs()
        rows = inventory(json.loads((ROOT / 'config/restart.json').read_text()))
        required = {row['name'] for row in rows
                    if not (row.get('guard') or row.get('condition'))
                    and row['name'] not in ('lean_glacier_real', 'lean_glacier_int', 'lean_glacier_exchange')}
        assert len(required) == 800
        classes = field_classes(ROOT / 'src/lean_fields.f90')
        held = {name for name, cls in classes.items() if cls == 'SOURCE_HELD'}
        sources = (set(copies()) | held) & set(init.variables)
        assert len(sources) > 35
        expected = required | sources
        actual = set(rst.variables)
        assert expected == actual, (sorted(expected-actual), sorted(actual-expected))
        for name in sources:
            compare_variable(init[name], rst[name])
    assert (rd / 'restart_timestamp').read_bytes() == (' ' + STAMP1 + '\n').encode()
    return rd


def compare_restart_tool(rd):
    """tests/restart/compare_restart.py on a lean restart: equal to itself; one qke value changed is a DIFF; a change of
    dss, a known departure from MPAS's file, is reported as KNOWN and passes."""
    original = rd / FILE1
    command = [sys.executable, 'tests/restart/compare_restart.py', str(original)]
    exact = subprocess.run(command + [str(original), '--fc', 'gfortran'], capture_output=True, text=True)
    assert exact.returncode == 0 and 'RESTART VALUES OK ' in exact.stdout, exact.stdout + exact.stderr
    changed = WORK / 'changed.nc'
    shutil.copyfile(original, changed)
    with Dataset(changed, 'r+') as ds:
        ds['qke'][0, 0, 0] = ds['qke'][0, 0, 0] + 1
    different = subprocess.run(command + [str(changed), '--fc', 'gfortran'], capture_output=True, text=True)
    lines = different.stdout.splitlines()
    assert different.returncode == 1 and len(lines) == 2, different.stdout + different.stderr
    assert lines[0].startswith('DIFF qke 1 ') and lines[1] == 'RESTART VALUES FAIL', lines
    shutil.copyfile(original, changed)
    with Dataset(changed, 'r+') as ds:
        ds['dss'][..., 0] = ds['dss'][..., 0] + 1
    known = subprocess.run(command + [str(changed), '--fc', 'gfortran'], capture_output=True, text=True)
    lines = known.stdout.splitlines()
    assert known.returncode == 0 and len(lines) == 2, known.stdout + known.stderr
    assert lines[0].startswith('KNOWN dss ') and lines[1].startswith('RESTART VALUES OK '), lines
    changed.unlink()


def host_fetch_noop():
    spec = ('stream output output_interval 0:30:00\n'
            'stream diagnostics output_interval 0:30:00\n')
    a = directory('host_fetch_noop_a', GREECE, spec + 'stream restart output_interval none\n')
    b = directory('host_fetch_noop_b', GREECE, spec + 'stream restart output_interval 0:30:00\n')
    extras = {f'LEAN_{name.upper()}': f'run.{name}' for name in ('film', 'products', 'storm', 'surface')}
    for rd in (a, b):
        run(rd, GREECE, 30, env=extras)
    stamps = ('2026-11-08_00.00.00', '2026-11-08_00.30.00', '2026-11-08_01.00.00')
    expected = {f'{stream}.{stamp}.nc' for stream in ('history', 'diag') for stamp in stamps}
    for rd in (a, b):
        assert {p.name for stream in ('history', 'diag') for p in rd.glob(f'{stream}.*.nc')} == expected
    assert not list(a.glob('restart.*.nc'))
    assert {p.name for p in b.glob('restart.*.nc')} == {f'restart.{stamp}.nc' for stamp in stamps[1:]}
    for file in sorted(expected):
        compare_netcdf(a / file, b / file)
    for file in extras.values():
        same(a / file, b / file)
    for rd in (a, b):
        shutil.rmtree(rd)


def restart_clobber(rd):
    path = rd / FILE1
    saved = rd / 'saved.nc'
    shutil.copyfile(path, saved)
    stamp = rd / 'restart_timestamp'
    sentinel = b' 2026-11-08_00:00:00\n'
    stamp.write_bytes(sentinel)
    _, log = run(rd, GREECE, 30, name='restart_clobber')
    assert f"Writing to stream 'restart' would clobber file '{FILE1}'," in log
    assert "but clobber_mode is set to 'never_modify'." in log
    same(path, saved)
    assert stamp.read_bytes() == sentinel
    saved.unlink()


def killed_restart():
    spec = ('stream restart output_interval 1:00:00\n'
            'stream output output_interval none\nstream diagnostics output_interval none\n')
    rd = directory('killed_restart', GREECE, spec)
    run(rd, GREECE, 30, env={'LEAN_WRITER_DIE': '1'}, expected=-9)
    assert not list(rd.glob('restart.*.nc'))
    assert (rd / (FILE1 + '.lean_partial')).is_file()
    assert not (rd / 'restart_timestamp').exists()
    shutil.rmtree(rd)
    rd = directory('failed_restart_write', GREECE, spec)
    code, log = run(rd, GREECE, 30, env={'LEAN_WRITER_DIE': '-1'}, expected=None)
    assert code != 0, 'injected restart write failure succeeded'
    messages_in_order(log, 'ERROR: Error writing one or more output streams')
    assert not (rd / (FILE1 + '.lean_partial')).exists(), 'failed restart left a partial file'
    assert not (rd / FILE1).exists()
    assert not (rd / 'restart_timestamp').exists()
    print('ok   failed_restart_write', flush=True)
    shutil.rmtree(rd)


def measure_mpas(mesh, offcycle, a, b, checkpoint):
    from tests.restart.compare_restart import bit_difference, compare, paired_values

    refs = ROOT / 'data/cases' / mesh / 'reference'
    ref_a = refs / ('strict_rst_a0610' if offcycle else 'strict_rst_a12h')
    ref_b = ref_a if offcycle else refs / 'strict_rst_b6h'
    final = '2026-11-08_12.00.00.nc'
    pairs = [(a / checkpoint, ref_a / checkpoint)]
    pairs.extend((b / (stream + final), ref_b / (stream + final))
                 for stream in ('history.', 'diag.', 'restart.')
                 if (b / (stream + final)).is_file())
    for lean_file, mpas_file in pairs:
        if not mpas_file.is_file():
            print(f'MPAS {mesh} {mpas_file.parent.name} {mpas_file.name}: missing reference')
            continue
        with Dataset(lean_file) as left, Dataset(mpas_file) as right:
            shared = [name for name in left.variables if name in right.variables and not name.startswith('lean_')]
            diffs = []
            for name in shared:
                x, y, same_dims = paired_values(left[name], right[name])
                count, delta = bit_difference(x, y)
                matching = same_dims and x.shape == y.shape and x.dtype == y.dtype
                if not matching:
                    count, delta = max(x.size, y.size), float('inf')
                if count:
                    diffs.append((name, count, delta, matching))
            print(f'MPAS {mesh} {mpas_file.parent.name} {mpas_file.name}: '
                  f'{len(shared)-len(diffs)}/{len(shared)} fields byte-identical')
            for name, count, delta, same_shape_type in diffs:
                print(f'MPAS DIFF {name} {count} {delta:.17g}' +
                      ('' if same_shape_type else ' (shape or type differs)'))
        if lean_file.name.startswith('restart.'):
            compare(lean_file, mpas_file, FC)


def split(mesh, offcycle, extra='', tag='', surface=None, start='6:00:00', step_a=None):
    """A 12 h run A with a restart every `start`, and a run B from it (an off-cycle `start` keeps every other stream's phase).
    extra: more run_spec lines (an option set); tag: appended to the case name; surface: the surface file of both runs."""
    suffix = 'greece' if mesh == GREECE else 'x1'
    if offcycle and start == '6:00:00':
        start = '6:10:00'
    hours, minutes = (int(x) for x in start.split(':')[:2])
    name = 'split_' + tag + suffix + (f'_{hours:02d}{minutes:02d}' if offcycle else '')
    interval = start
    step0 = step_a or (hours*30 + minutes//2)
    stamp = f'2026-11-08_{hours:02d}.{minutes:02d}.00'
    path = f'restart.{stamp}.nc'
    spec = ('stream output output_interval 3:00:00\nstream diagnostics output_interval 1:00:00\n'
            f'stream restart output_interval {interval}\n') + extra
    if offcycle:
        fixed_phase = ['output', 'diagnostics'] + (['lbc_in'] if mesh == GREECE else [])
        spec += ''.join(f'stream {stream} reference_time 2026-11-08_00:00:00\n' for stream in fixed_phase)
    a = directory(name + '_a', mesh, spec, surface=surface)
    _, log_a = run(a, mesh, 360)
    assert (a / path).is_file()
    expected_timestamp = (' ' + stamp.replace('.', ':') + '\n').encode()
    if offcycle:
        # restart_timestamp names the last restart A wrote: a split of 4:10 writes a second one, at 8:20
        every = hours*60 + minutes
        last = (720 // every) * every
        last_stamp = f' 2026-11-08_{last//60:02d}:{last%60:02d}:00\n'.encode()
        assert (a / 'restart_timestamp').read_bytes() == last_stamp
    b = directory(name + '_b', mesh, spec + "nml restart config_do_restart true\n"
                  "nml nhyd_model config_start_time 'file'\n", surface=surface)
    (b / path).symlink_to(a / path)
    (b / 'restart_timestamp').write_bytes(expected_timestamp)
    _, log_b = run(b, mesh, 360-step0)
    assert "Reading initial state from 'restart' stream" in log_b
    assert f'lean_mpas resumes from {path}:' in log_b
    assert re.search(r"Timing for read of 'restart' stream: .* s", log_b)
    assert re.search(r"Timing for write of 'restart' stream: .* s", log_a)
    print(f'{name}: {(a / path).stat().st_size} bytes', flush=True)
    for log in (log_a, log_b):
        print('\n'.join(line.strip() for line in log.splitlines()
                        if any(f"Timing for {verb} of 'restart'" in line for verb in ('read', 'write', 'restore'))), flush=True)
    first_hour = hours + 1 if minutes == 0 else hours + 1
    expected = {f'history.2026-11-08_{h:02}.00.00.nc' for h in (3, 6, 9, 12) if h * 60 > hours * 60 + minutes}
    expected |= {f'diag.2026-11-08_{h:02}.00.00.nc' for h in range(first_hour, 13)}
    actual = {p.name for p in b.glob('history.*.nc')} | {p.name for p in b.glob('diag.*.nc')}
    assert actual == expected, (actual, expected)
    counts = [(f, compare_netcdf(a / f, b / f)) for f in sorted(expected)]
    if not offcycle and start == '6:00:00':
        final = 'restart.2026-11-08_12.00.00.nc'
        count = compare_netcdf(a / final, b / final)
        assert count > 800
        print(f'{name}: {count} restart variables at 12:00', flush=True)
    if surface is not None and mesh == X1:
        # the cells that changed type between the 3-hourly histories of run A: the updates before the restart and the ones
        # B applies after it (every history of B equals A's, compared above)
        with ExitStack() as stack:
            land = {h: np.array(stack.enter_context(Dataset(a / f'history.2026-11-08_{h:02}.00.00.nc'))['xland'][0])
                    for h in (3, 6, 9, 12)}
        for before, after in ((3, 6), (6, 9), (9, 12)):
            to_land = int(((land[before] > 1.5) & (land[after] < 1.5)).sum())
            to_ocean = int(((land[before] < 1.5) & (land[after] > 1.5)).sum())
            print(f'{name}: cells that changed type {before}:00 to {after}:00: {to_land} to sea ice, {to_ocean} to water'
                  f' ({"before" if after*60 <= hours*60 + minutes else "after"} the restart at {start})', flush=True)
    if mesh == X1 and not extra:
        with Dataset(a / path) as ds:
            ds.set_auto_maskandscale(False)
            sea = np.count_nonzero(ds['lean_seaice_xice'][:] >= np.float32(0.02))
            land = np.count_nonzero(ds['lean_glacier_of'][:])
            assert (sea, land) == (146, 80), (sea, land)
            print(f'{name}: {sea} sea-ice and {land} land-ice cells', flush=True)
    print(f'{name}: {counts}', flush=True)
    if os.environ.get('RESTART_MEASURE') == '1' and not extra and not (mesh == X1 and offcycle):
        measure_mpas(mesh, offcycle, a, b, path)
    for rd in (a, b):
        shutil.rmtree(rd)


START6 = '2026-11-08_06:00:00'
START610 = '2026-11-08_06:10:00'
LBC_PHASE = 'stream lbc_in reference_time 2026-11-08_00:00:00\n'
HOURLY = 'stream output output_interval 3:00:00\nstream diagnostics output_interval 1:00:00\n'


def restart_path(rd, stamp):
    return rd / ('restart.' + stamp.replace(':', '.') + '.nc')


def resume(name, source, start, spec='', *, link_as=None, duration='0_00:02:00'):
    rd = directory(name, GREECE, spec + 'nml restart config_do_restart true\n'
                   f"nml nhyd_model config_start_time '{start}'\n"
                   f"nml nhyd_model config_run_duration '{duration}'\n")
    target = restart_path(rd, link_as) if link_as else rd / source.name
    assert source.is_file(), source
    target.symlink_to(source)
    return rd


def messages_in_order(log, *messages):
    cursor = 0
    lines = [line.strip() for line in log.splitlines()]
    for message in messages:
        matches = (message.fullmatch(line) if hasattr(message, 'fullmatch') else message == line
                   for line in lines[cursor:])
        offset = next((i for i, found in enumerate(matches) if found), None)
        assert offset is not None, f'missing line: {message}\n{log[-3500:]}'
        cursor += offset + 1
        print(lines[cursor-1], flush=True)


def refused_start(rd, *messages):
    before = {p.name for p in rd.glob('*.nc')}
    code, _ = run(rd, GREECE, 1, expected=None)
    assert code != 0, f'{rd.name}: run succeeded'
    messages_in_order((rd / 'log.atmosphere.0000.out').read_text(), *messages)
    assert {p.name for p in rd.glob('*.nc')} == before, f'{rd.name}: wrote NetCDF output'
    assert not list(rd.glob('*.lean_partial')), f'{rd.name}: left a partial file'
    shutil.rmtree(rd)


def lbc_offcycle_default():
    a = directory('lbc_offcycle_a', GREECE, LBC_PHASE + 'stream restart output_interval 6:10:00\n')
    run(a, GREECE, 185)
    b = resume('lbc_offcycle_default', restart_path(a, START610), START610)
    refused_start(b, "ERROR: Stream 'lbc_in' attempted to read non-existent file 'lbc.2026-11-08_06.10.00.nc'")
    shutil.rmtree(a)


def diag_offcycle_default():
    a = directory('diag_phase_a', GREECE, HOURLY + 'stream restart output_interval 6:10:00\n')
    c = directory('diag_phase_c', GREECE, HOURLY +
                  'stream diagnostics reference_time 2026-11-08_00:10:00\n')
    for rd in (a, c):
        # A and C reset these per-step maxima at different times. The split
        # cases check their continuation with matching windows.
        drop_fields(rd, 'diagnostics', ('updraft_helicity_max', 'w_velocity_max', 'wind_speed_level1_max'))
        fields = rd / 'stream_list.atmosphere.diagnostics'
        with fields.open('a') as stream:
            stream.write('relhum\n')
    run(a, GREECE, 215)
    b = resume('diag_phase_b', restart_path(a, START610), START610, HOURLY + LBC_PHASE)
    shutil.copyfile(a / 'stream_list.atmosphere.diagnostics', b / 'stream_list.atmosphere.diagnostics')
    run(b, GREECE, 30)
    diag = 'diag.2026-11-08_07.10.00.nc'
    with Dataset(b / diag) as resumed, Dataset(a / 'diag.2026-11-08_07.00.00.nc') as previous:
        assert resumed['relhum'][:].tobytes() != previous['relhum'][:].tobytes(), \
            'relhum: still holds the 07:00 values at 07:10'
    run(c, GREECE, 215)
    # Check the fields made on the physics diagnostics alarm before the whole record.
    with Dataset(b / diag) as resumed, Dataset(c / diag) as cold:
        for name in ('relhum', 'refl10cm_max'):
            compare_variable(resumed[name], cold[name])
    count = compare_netcdf(b / diag, c / diag)
    print(f'diag_offcycle_default: {count} variables at 07:10', flush=True)
    for rd in (a, b, c):
        shutil.rmtree(rd)


def precipw_at_resume():
    spec = ('stream output output_interval none\nstream diagnostics output_interval none\n'
            'stream restart output_interval 6:00:00\n')
    user_stream = (
        '<stream name="water" type="output" filename_template="water.$Y-$M-$D_$h.$m.$s.nc" '
        'output_interval="2:00:00">\n<var name="precipw"/>\n</stream>\n')

    def add_stream(rd):
        streams = rd / 'streams.atmosphere'
        streams.write_text(streams.read_text().replace('</streams>', user_stream + '</streams>'))

    a = directory('precipw_at_resume_a', GREECE, spec)
    add_stream(a)
    run(a, GREECE, 180)
    b = resume('precipw_at_resume_b', restart_path(a, START6), START6, spec)
    add_stream(b)
    run(b, GREECE, 1)
    record = 'water.2026-11-08_06.00.00.nc'
    with Dataset(a / record) as cold, Dataset(b / record) as resumed:
        compare_variable(cold['precipw'], resumed['precipw'])
        print(f'precipw_at_resume: {cold["precipw"].size} values identical at 06:00', flush=True)
    for rd in (a, b):
        shutil.rmtree(rd)


def file_digest(path):
    digest = hashlib.sha256()
    with path.open('rb') as stream:
        while data := stream.read(1024 * 1024):
            digest.update(data)
    return digest.digest()


def rerun_same_dir(source):
    b = resume('rerun_same_dir', source, START6, HOURLY + 'stream restart output_interval 6:00:00\n')
    # Keep the explicit start independent of the timestamp so a rewrite cannot hide.
    run(b, GREECE, 180, name='rerun_same_dir_first')
    stamp = b / 'restart_timestamp'
    assert stamp.read_text() == ' 2026-11-08_12:00:00\n'
    stamp.write_text(' 2026-11-08_06:00:00\n')
    expected = {f'diag.2026-11-08_{h:02}.00.00.nc': 'diagnostics' for h in range(7, 13)}
    expected.update({f'history.2026-11-08_{h:02}.00.00.nc': 'output' for h in (9, 12)})
    expected['restart.2026-11-08_12.00.00.nc'] = 'restart'
    def outputs():
        return {p.name for p in b.glob('*.nc') if not p.is_symlink()}
    assert outputs() == expected.keys(), outputs()
    before = {name: file_digest(b / name) for name in (*expected, stamp.name)}
    _, log = run(b, GREECE, 180, name='rerun_same_dir_second')
    lines = log.splitlines()
    clobber = "ERROR:     but clobber_mode is set to 'never_modify'."
    for name, stream in expected.items():
        first = f"ERROR: Writing to stream '{stream}' would clobber file '{name}',"
        assert first in lines and lines[lines.index(first)+1] == clobber, name
        print(first + '\n' + clobber, flush=True)
    assert lines.count(clobber) == len(expected)
    assert outputs() == expected.keys(), outputs()
    for name, digest in before.items():
        assert file_digest(b / name) == digest, f'{name}: bytes changed on rerun'
    shutil.rmtree(b)


def restart_log(source):
    b = resume('restart_log', source, 'file', HOURLY + 'stream restart output_interval 6:00:00\n',
               duration='0_06:00:00')
    timestamp = b / 'restart_timestamp'
    timestamp.write_text(' ' + START6 + '\n')
    run(b, GREECE, 180)
    lines = [line.strip() for line in (b / 'log.atmosphere.0000.out').read_text().splitlines()]
    first = lines.index("Reading initial state from 'restart' stream")
    assert re.fullmatch(r"Timing for read of 'restart' stream: \S+ s", lines[first+1]), lines[first:first+3]
    assert lines[first+2] == '----- done reading initial state -----', lines[first:first+3]
    resume_line = next(line for line in lines[first+3:] if line.startswith('lean_mpas resumes from '))
    assert re.fullmatch(
        rf"lean_mpas resumes from {re.escape(source.name)}: written by lean_mpas .+ "
        r"\(layout [0-9a-f]{16}\), 180 steps after 2026-11-08_00:00:00\.", resume_line), resume_line
    assert lines.index(resume_line) < lines.index('Begin timestep ' + START6)
    for stream in ('history', 'diag'):
        assert not list(b.glob(f'{stream}.2026-11-08_06.00.00.nc'))
        assert (b / f'{stream}.2026-11-08_12.00.00.nc').is_file()
    assert restart_path(b, '2026-11-08_12:00:00').is_file()
    assert timestamp.read_bytes() == b' 2026-11-08_12:00:00\n'
    print('\n'.join((*lines[first:first+3], resume_line)), flush=True)
    shutil.rmtree(b)


def drop_fields(rd, stream, names):
    path = rd / f'stream_list.atmosphere.{stream}'
    fields = path.read_text().splitlines()
    assert set(names) <= set(fields)
    path.write_text(''.join(field + '\n' for field in fields if field not in names))


def streams_changed():
    maxima = ('updraft_helicity_max', 'w_velocity_max', 'wind_speed_level1_max')
    refl = ('refl10cm', 'refl10cm_max', 'refl10cm_1km', 'refl10cm_1km_max')
    spec = HOURLY + 'stream restart output_interval 1:00:00\n'
    a = directory('streams_changed_a', GREECE, spec)
    drop_fields(a, 'output', ['relhum'])
    # Even without a relhum output, isobaric diagnostics allocate its memory.
    (a / 'stream_list.atmosphere.diagnostics').write_text('rainc\nacsnow\n')
    run(a, GREECE, 90)
    phase = LBC_PHASE + 'stream output reference_time 2026-11-08_00:00:00\n'
    b = resume('streams_changed', a / FILE1, STAMP1, spec + phase)
    for stream in ('output', 'diagnostics'):
        drop_fields(b, stream, ['rainc'])
    # A user stream at the restart time exposes the restored values before a step can update them.
    streams = b / 'streams.atmosphere'
    xml = '<stream name="restored" type="output" filename_template="restored.nc" output_interval="initial_only">\n'
    xml += ''.join(f'<var name="{name}"/>\n' for name in (*maxima, 'relhum'))
    streams.write_text(streams.read_text().replace('</streams>', xml + '</stream>\n</streams>'))
    with Dataset(a / FILE1) as ds:
        assert 'rainc' in ds.variables
        present = set((*maxima, 'relhum', *refl)) & ds.variables.keys()
        assert not present, f'A carried unrequested diagnostics: {sorted(present)}'
    _, log = run(b, GREECE, 60)
    warnings = [line.strip() for line in log.splitlines() if f'{FILE1} holds no' in line]
    expected = [f"{FILE1} holds no '{name}': it starts from 0 at {STAMP1} "
                '(the run that wrote the file did not compute it).' for name in (*maxima, 'relhum', *refl)]
    assert warnings == ['WARNING: ' + line for line in expected], warnings
    print('\n'.join(warnings), flush=True)
    with Dataset(b / 'restored.nc') as ds:
        for name in (*maxima, 'relhum'):
            ds[name].set_auto_maskandscale(False)
            assert ds[name].size and np.all(ds[name][:] == 0), f'{name}: nonzero at restart'
    history = 'history.2026-11-08_03.00.00.nc'
    with Dataset(a / history) as old, Dataset(b / history) as new:
        assert set(old.variables) - set(new.variables) == {'rainc'}
        assert set(new.variables) - set(old.variables) == {'relhum'}
        assert len(old.variables) > 80
        for name in set(old.variables) & set(new.variables):
            compare_variable(old[name], new[name])
    for rd in (a, b):
        shutil.rmtree(rd)


def mesh_mismatch():
    inputs = ROOT / 'data/cases' / GREECE / 'inputs/l55'
    spec = 'stream restart output_interval 1:00:00\n'
    for mesh, levels in ((X1, 45), (GREECE, 55)):
        a = directory(f'mesh_mismatch_{levels}_a', mesh, spec, inputs if levels == 55 else None)
        run(a, mesh, 30)
        b = resume(f'mesh_mismatch_{levels}', a / FILE1, STAMP1, LBC_PHASE)
        if levels == 45:
            refused_start(b, 'Checking consistency of limited-area settings...', '- config_apply_lbcs = T',
                          '- Maximum value in bdyMaskCell = 0',
                          'ERROR: config_apply_lbcs = true, but no boundary cells found in the bdyMaskCell field.',
                          'ERROR: Please ensure that config_apply_lbcs = false for global simulations.',
                          'ERROR: Please correct issues with the model input fields and/or namelist.',
                          'CRITICAL ERROR: Core init failed for core atmosphere')
        else:
            refused_start(b, 'ERROR: lbc.2026-11-08_00.00.00.nc: nVertLevels = 45; the mesh file has 55')
        shutil.rmtree(a)


def timestamp_forms(source):
    for name, contents in (('leading_blank', ' ' + START6), ('without_blank', START6)):
        b = resume('timestamp_' + name, source, 'file')
        (b / 'restart_timestamp').write_text(contents + '\n')
        _, log = run(b, GREECE, 1)
        assert f'lean_mpas resumes from {source.name}:' in log, name
        shutil.rmtree(b)
    b = resume('timestamp_missing', source, 'file')
    refused_start(b, re.compile(r'ERROR: Cannot read the start time from file restart_timestamp: .+'))
    start7 = '2026-11-08_07:00:00'
    b = resume('timestamp_wrong_time', source, start7, link_as=start7)
    refused_start(b, f'CRITICAL ERROR: {restart_path(b, start7).name} holds the state of {START6}, '
                  f'not of the start time {start7}.')


def extras_warn(source):
    b = resume('extras_warn', source, START6)
    _, log = run(b, GREECE, 5, env={'LEAN_STORM': 'run.storm'})
    expected = f"LEAN_STORM starts its sums at the restart time {START6}: lean's own outputs are not carried by a restart."
    actual = [line.strip() for line in log.splitlines() if 'starts its sums at the restart time' in line]
    assert actual == ['WARNING: ' + expected], actual
    print(actual[0], flush=True)
    assert (b / 'run.storm').stat().st_size > 0
    shutil.rmtree(b)


def refusal(case, source, stamp, message, attribute=None):
    rd = directory(case, GREECE, "nml restart config_do_restart true\n"
                   f"nml nhyd_model config_start_time '{stamp}'\n"
                   "nml nhyd_model config_run_duration '0_00:02:00'\n")
    target = rd / ('restart.' + stamp.replace(':', '.') + '.nc')
    if attribute:
        shutil.copyfile(source, target)
        with Dataset(target, 'r+') as file:
            file.setncattr(*attribute)
    else:
        target.symlink_to(source)
    before = {p.name for p in rd.glob('*.nc')}
    code, fallback_log = run(rd, GREECE, 1, expected=None)
    assert code != 0, f'{case}: run succeeded'
    for file in ('log.atmosphere.0000.out', 'log.atmosphere.0000.err'):
        log = rd / file
        assert log.is_file() and message in log.read_text(), (case, message,
                log.read_text()[-4500:] if log.is_file() else fallback_log)
    assert {p.name for p in rd.glob('*.nc')} == before
    assert not list(rd.glob('*.lean_partial'))
    shutil.rmtree(rd)

# The namelist options and the surface update across a restart.
OPTIONS_SPEC = ("nml nhyd_model config_apvm_upwinding 0.5\nnml physics config_microp_re false\n"
                "nml physics config_radt_cld_scheme 'cld_incidence'\nnml physics config_sst_update true\n"
                "stream surface input_interval 3:00:00\n")
OFF_PBL_SPEC = ("nml physics config_pbl_scheme 'off'\nnml physics config_convection_scheme 'off'\n"
                "nml physics config_gwdo_scheme 'off'\n")
SFC_SPEC = "nml physics config_sst_update true\nstream surface input_interval 3:00:00\n"


def x1_surface_file():
    out = WORK / 'sfc_ice'
    out.mkdir(parents=True, exist_ok=True)
    if not (out / 'x1.2562.sfc_update.nc').exists():
        make_sfc_update.main([str(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc'), str(out)])
    return out / 'x1.2562.sfc_update.nc'


def split_sfc_x1_turned_on():
    """A (x1) has no surface update, B continues from A's restart at 04:10 with the update on and
    the made surface file. The record at or before 04:10 is the one of 03:00: it freezes water cells that A never had as sea
    ice. B must run, and those cells must take the sea-ice path (finite values, the sea-ice rows of its restart filled)."""
    surface = x1_surface_file()
    spec = ('stream output output_interval 3:00:00\nstream diagnostics output_interval 1:00:00\n'
            'stream restart output_interval 4:10:00\n'
            + ''.join(f'stream {s} reference_time 2026-11-08_00:00:00\n' for s in ('output', 'diagnostics')))
    a = directory('turned_on_a', X1, spec)
    run(a, X1, 125)
    path = 'restart.2026-11-08_04.10.00.nc'
    assert (a / path).is_file()
    b_spec = spec.replace('restart output_interval 4:10:00', 'restart output_interval 2:40:00') + SFC_SPEC
    b = directory('turned_on_b', X1, b_spec + "nml restart config_do_restart true\nnml nhyd_model config_start_time 'file'\n",
                  surface=surface)
    # x1's static snoalb is 0 on water: a copy of A's restart with 0.3 on every cell tells where B's new sea-ice cells get theirs
    shutil.copyfile(a / path, b / path)
    with Dataset(b / path, 'r+') as d:
        d['snoalb'][:] = np.float32(0.3)
    (b / 'restart_timestamp').write_bytes(b' 2026-11-08_04:10:00\n')
    _, log = run(b, X1, 80)     # ends at 06:50, before B's second update (07:10)
    assert "Read 'surface' input stream valid at 2026-11-08_03:00:00" in log, log[-1500:]
    with Dataset(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc') as init:
        xice0, xland0 = np.array(init['xice'][0]), np.array(init['xland'][0])
    with Dataset(surface) as sfc:
        records = [np.array(sfc['xice'][k]) for k in range(3)]
    want = make_sfc_update.counts(records, xice0, xland0)[1][0]
    with Dataset(b / 'history.2026-11-08_06.00.00.nc') as h, Dataset(b / 'restart.2026-11-08_06.50.00.nc') as r:
        for name in ('skintemp', 't2m', 'hfx'):
            assert np.isfinite(np.array(h[name][0])).all(), f'{name} of the 06:00 history is not finite'
        frozen = (xland0 > 1.5) & (np.array(h['xland'][0]) < 1.5)
        assert int(frozen.sum()) == want and want > 0, (int(frozen.sum()), want)
        skin = np.array(h['skintemp'][0])[frozen]
        assert ((skin > 200) & (skin < 300)).all(), f'skintemp of the new sea-ice cells: {skin.min()} to {skin.max()}'
        ice = np.array(r['lean_seaice_xice'][0])
        assert (ice[frozen] >= 0.02).all(), 'the new sea-ice cells are not sea ice in the restart'
        snoalb = np.array(r['lean_seaice_snoalb'][0])
        assert (snoalb[frozen] == np.float32(0.3)).all(), 'the new sea-ice cells do not take the restart\'s static snoalb'
    print(f'split_sfc_x1_turned_on: {want} cells froze at the first update of B and ran as sea ice', flush=True)
    for rd in (a, b):
        shutil.rmtree(rd)


def restart_scheme_change():
    """A with the PBL, convection and GWDO on writes a restart at 06:00; B with all three off continues
    from it. B equals B', started from a copy of that restart whose PBL and convection tendencies are zeroed (they are
    not read), in every variable except the zeroed ones, which B passes through as it was given them."""
    a = directory('scheme_change_a', GREECE, HOURLY + 'stream restart output_interval 6:00:00\n')
    run(a, GREECE, 180)
    source = restart_path(a, START6)
    with Dataset(source) as d:
        zeroed = [n for n in ('rublten', 'rvblten', 'rthblten', 'qc_bl', 'qi_bl', 'cldfrac_bl') if n in d.variables]
        zeroed += sorted(n for n in d.variables if 'cuten' in n)
        assert len(zeroed) >= 6, zeroed
    copy = WORK / 'scheme_change_zeroed.nc'
    shutil.copyfile(source, copy)
    with Dataset(copy, 'r+') as d:
        for name in zeroed:
            d[name][:] = 0
    tail = 'stream restart output_interval 0:02:00\n'
    b = resume('scheme_change_b', source, START6, OFF_PBL_SPEC + tail)
    c = resume('scheme_change_c', copy, START6, OFF_PBL_SPEC + tail, link_as=START6)
    for rd in (b, c):
        run(rd, GREECE, 1)
    name = 'restart.2026-11-08_06.02.00.nc'
    with Dataset(b / name) as x, Dataset(c / name) as y, Dataset(source) as given:
        assert set(x.variables) == set(y.variables)
        for var in x.variables:
            if var in zeroed:
                assert np.array_equal(np.array(x[var][:]), np.array(given[var][:])), f'{var}: B did not pass the value through'
            else:
                compare_variable(x[var], y[var])
    print(f'restart_scheme_change: {len(x.variables) - len(zeroed)} variables equal after one step; {len(zeroed)} unread ones passed through', flush=True)
    for rd in (a, b, c):
        shutil.rmtree(rd)
    copy.unlink()


def main():
    if not all((ROOT / 'data/cases' / mesh / 'inputs' / INIT[mesh]).is_file() for mesh in (GREECE, X1)):
        print('SKIP restart runs: needs the release case inputs in data/cases (cases/README.md)')
        return 77
    WORK.mkdir(parents=True, exist_ok=True)
    only = os.environ.get('RESTART_CASE')
    count = 0
    skipped = False

    def chosen(*names):
        return not only or only in names

    def done(name):
        nonlocal count
        count += 1
        print(f'ok   {name}', flush=True)

    try:
        if chosen('file_greece', 'compare_restart_tool', 'restart_clobber', 'refuse_layout', 'refuse_flavor',
                  'refuse_other'):
            rd = file_greece()
            if chosen('file_greece'):
                done('file_greece')
            if chosen('compare_restart_tool'):
                compare_restart_tool(rd)
                done('compare_restart_tool')
            if chosen('restart_clobber'):
                restart_clobber(rd)
                done('restart_clobber')
            if chosen('refuse_layout'):
                with Dataset(rd / FILE1) as original:
                    layout = original.lean_restart
                refusal('refuse_layout', rd / FILE1, STAMP1,
                        f'{FILE1} was written with restart layout 0000000000000000; this lean_mpas has layout {layout}. '
                        "Resume with the lean_mpas that wrote it, or start from the 'input' stream.",
                        ('lean_restart', '0000000000000000'))
                done('refuse_layout')
            if chosen('refuse_flavor'):
                with Dataset(rd / FILE1) as original:
                    switches = original.lean_switches
                refusal('refuse_flavor', rd / FILE1, STAMP1,
                        f"{FILE1} was written by lean_mpas flavor 'other' with switches '{switches}'; "
                        f"this one is flavor 'mpas' with switches '{switches}'. "
                        'A restart continues the same physics: rebuild with the flavor and switches that wrote it.',
                        ('lean_flavor', 'other'))
                done('refuse_flavor')
            if chosen('refuse_other'):
                refusal('refuse_other', rd / 'history.2026-11-08_00.00.00.nc', STAMP1,
                        f'{FILE1} is not a lean_mpas restart file (it has no global attribute lean_restart).')
                done('refuse_other')
            shutil.rmtree(rd)
        if chosen('killed_restart'):
            killed_restart()
            done('killed_restart')
        if chosen('host_fetch_noop'):
            host_fetch_noop()
            done('host_fetch_noop')
        for mesh, suffix in ((GREECE, 'greece'), (X1, 'x1')):
            for offcycle in (False, True):
                name = 'split_' + suffix + ('_0610' if offcycle else '')
                if chosen(name):
                    split(mesh, offcycle)
                    done(name)
        for name, mesh, offcycle, extra, tag, surface_file, start in (
                ('split_options_greece', GREECE, False, OPTIONS_SPEC, 'options_', GREECE_SURFACE, '6:00:00'),
                ('split_options_greece_0610', GREECE, True, OPTIONS_SPEC, 'options_', GREECE_SURFACE, '6:10:00'),
                ('split_off_greece', GREECE, False, OFF_PBL_SPEC, 'off_', None, '6:00:00'),
                ('split_sfc_x1', X1, False, SFC_SPEC, 'sfc_', 'x1', '6:00:00'),
                ('split_sfc_x1_0410', X1, True, SFC_SPEC, 'sfc_', 'x1', '4:10:00')):
            if chosen(name):
                split(mesh, offcycle, extra, tag, x1_surface_file() if surface_file == 'x1' else surface_file, start)
                done(name)
        if chosen('split_sfc_x1_turned_on'):
            split_sfc_x1_turned_on()
            done('split_sfc_x1_turned_on')
        if chosen('restart_scheme_change'):
            restart_scheme_change()
            done('restart_scheme_change')
        if chosen('refuse_mpas'):
            ref = ROOT / f'data/cases/{GREECE}/reference/strict_rst_a12h/restart.2026-11-08_06.00.00.nc'
            if ref.is_file():
                refusal('refuse_mpas', ref, '2026-11-08_06:00:00',
                        f'{ref.name} was written by MPAS, not by lean_mpas: lean_mpas resumes only from its own restart files. '
                        "Start this run from the 'input' stream (config_do_restart = .false.).")
                done('refuse_mpas')
            else:
                print('SKIP refuse_mpas: the MPAS restart files of data/cases are not released', flush=True)
                skipped = True
        for case in (lbc_offcycle_default, diag_offcycle_default, precipw_at_resume, streams_changed, mesh_mismatch):
            if chosen(case.__name__):
                case()
                done(case.__name__)
        with_aligned = (restart_log,) if only == 'restart_log' else (rerun_same_dir, timestamp_forms, extras_warn)
        if chosen(*(case.__name__ for case in with_aligned)):
            a = directory('user_cases_a', GREECE, HOURLY + 'stream restart output_interval 6:00:00\n')
            run(a, GREECE, 180)
            for case in with_aligned:
                if chosen(case.__name__):
                    case(restart_path(a, START6))
                    done(case.__name__)
            shutil.rmtree(a)
        assert count == (1 if only else 28) - int(skipped), (only, count)
        print(f'RESTART RUNS OK {count}')
    finally:
        shutil.rmtree(WORK)


if __name__ == '__main__':
    sys.exit(main())
