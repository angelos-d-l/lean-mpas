"""Serial real-model runs of the surface update (SST and sea ice from the 'surface' stream).

  python3 tests/surface/model_runs.py --fc gfortran [--jobs 1] [case ...]     the named cases, or every case

sfc_greece, sfc_x1       the runs of cases/greece_60km/mpas_runs/sfc_upd_7h.txt and cases/x1_2562/mpas_runs/sfc_ice_7h.txt (x1: a surface file made by
                         tools/cases/make_sfc_update.py), 7 h of 120 s steps with a record every 3 h: the log, the
                         histories, the restart; against MPAS's own history when its reference is in
                         data/cases/<case>/reference/<run>/ (not released: SKIP, counted)
sst_update_default_streams, sfc_read_no_update, sfc_ice_on_land, sfc_records_offset   the stops and the record rules
sfc_time_files, sfc_late_ice   a filename template with time tokens: a file per read
off_lsm_sfc              the update with no land-surface scheme
update_off_greece, update_off_x1   3 h without the update: the restart's xicem is its xice, the sfcbdy phase is blank
Evolved fields are compared with MPAS only by an nvfortran build; a gfortran build is judged on structure, on the
fields the update sets from the file (xland, xice, sst, skintemp of water, vegfra, the soil and snow of a melted cell)
and on lean-to-lean identity. One plain build (flavor mpas); every run is atmosphere_model's form in an MPAS run
directory. The runs need the release case inputs in data/cases. Scratch run directories are removed after a pass.
"""
import argparse
from pathlib import Path
import json
import os
import re
import shlex
import shutil
import subprocess
import sys

import numpy as np
from netCDF4 import Dataset, chartostring

ROOT = Path(__file__).resolve().parents[2]
os.chdir(ROOT)
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(ROOT / 'tools/cases'))
import make_sfc_update
from tools.cases.run_spec import edit_namelist

parser = argparse.ArgumentParser()
parser.add_argument('--fc', default='gfortran')
parser.add_argument('--jobs', default='1')
parser.add_argument('cases', nargs='*')
ARGS = parser.parse_args()
FC, JOBS, SELECTED = ARGS.fc, ARGS.jobs, ARGS.cases
assert 1 <= int(JOBS) <= 2
WORK = ROOT / 'build/surface/runs'
GREECE, X1 = 'greece_60km', 'x1_2562'
SKIPPED = []
BUILD = []
LOGS = ('log.atmosphere.0000.out', 'log.atmosphere.0000.err')
# The settings of cases/greece_60km/mpas_runs/sfc_upd_7h.txt and cases/x1_2562/mpas_runs/sfc_ice_7h.txt, without the
# lines that only describe the MPAS reference run (build, ranks, minutes, expect).
SFC_SPEC = ("nml nhyd_model config_run_duration '0_07:00:00'\n"
            'nml physics config_sst_update true\n'
            'stream surface input_interval 3:00:00\n'
            'stream output output_interval 1:00:00\n'
            'stream diagnostics output_interval 1:00:00\n'
            'stream restart output_interval 7:00:00\n')
OFF_STREAMS = ("nml nhyd_model config_run_duration '0_03:00:00'\n"
                 'stream output output_interval 1:00:00\n'
                 'stream diagnostics output_interval 1:00:00\n'
                 'stream restart output_interval 3:00:00\n')
STEPS_7H = 210
HOURS = [f'2026-11-08_{h:02d}.00.00' for h in range(8)]


def command(cmd, log, cwd=ROOT, env=None, expected=0):
    with log.open('w') as out:
        result = subprocess.run(list(map(str, cmd)), cwd=cwd, env=env, stdout=out, stderr=subprocess.STDOUT)
    if expected is not None and result.returncode != expected:
        print(log.read_text()[-10000:])
        raise AssertionError(f'{cmd[0]}: exit {result.returncode}, expected {expected}')
    return result.returncode


def build():
    """The one build of every case."""
    if not BUILD:
        WORK.mkdir(parents=True, exist_ok=True)
        log = WORK / 'build.log'
        command(['./lean', 'build', '--flavor', 'mpas', '--target', 'cpu', '--fc', FC, '--jobs', JOBS, '--json'], log)
        BUILD.append(Path(json.loads(log.read_text()[log.read_text().index('{'):])['build_json']).resolve().parent)
    return BUILD[0]


def directory(name, mesh, spec, surface=None):
    """A run directory of the case with `spec` applied after the case's own; `surface` replaces the surface file."""
    rd = WORK / name
    shutil.rmtree(rd, ignore_errors=True)
    base = 'strict_12h.txt' if mesh == GREECE else 'strict_24h.txt'
    extra = WORK / (name + '.spec')
    extra.write_text(spec)
    cmd = 'root=' + shlex.quote(str(ROOT)) + '; . tests/inputs/run_dir.sh; make_run_dir '
    cmd += ' '.join(shlex.quote(str(p)) for p in [rd, ROOT / 'data/cases' / mesh / 'inputs',
                                                   f'cases/{mesh}/case.txt', f'cases/{mesh}/mpas_runs/{base}', extra])
    command(['sh', '-c', cmd], WORK / (name + '.setup.log'))
    template = 'greece_60km.sfc_update.nc' if mesh == GREECE else 'x1.2562.sfc_update.nc'
    if surface is None and mesh == GREECE:
        surface = ROOT / 'data/cases/greece_60km/inputs/greece_60km.sfc_update.nc'
    if surface is not None:
        (rd / template).symlink_to(Path(surface).resolve())
    return rd


def set_steps(rd, steps):
    """config_run_duration of the run directory's namelist: `steps` steps of 120 s."""
    s = steps * 120
    path = rd / 'namelist.atmosphere'
    path.write_text(edit_namelist(path.read_text(), 'nhyd_model', 'config_run_duration',
                                  f"'{s // 86400}_{s // 3600 % 24:02d}:{s // 60 % 60:02d}:{s % 60:02d}'"))


def run(rd, mesh, steps, expected=0):
    """atmosphere_model's form in the run directory, `steps` steps long. Returns the exit code and the text of
    log.atmosphere.0000.out (of stdout and the .err file if it stopped before opening it), also in WORK/<run directory>.log."""
    set_steps(rd, steps)
    launcher = rd / 'atmosphere_model'
    if not launcher.is_symlink():
        launcher.symlink_to(build() / 'lean_mpas')
    for name in LOGS:
        (rd / name).unlink(missing_ok=True)
    out = WORK / (rd.name + '.stdout')
    code = command([launcher], out, rd, dict(os.environ), None)
    log = rd / LOGS[0]
    text = log.read_text() if log.is_file() else out.read_text() + (
        (rd / LOGS[1]).read_text() if (rd / LOGS[1]).is_file() else '')
    (WORK / (rd.name + '.log')).write_text(text)
    if expected is not None and code != expected:
        print(text[-10000:])
        raise AssertionError(f'{rd.name}: exit {code}, expected {expected}')
    return code, text


def x1_file(name='x1.2562.sfc_update.nc'):
    """The made surface file of x1 (tools/cases/make_sfc_update.py), in the scratch folder."""
    out = WORK / 'sfc_ice'
    out.mkdir(parents=True, exist_ok=True)
    if not (out / name).exists():
        make_sfc_update.main([str(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc'), str(out)])
        if name != 'x1.2562.sfc_update.nc':
            (out / 'x1.2562.sfc_update.nc').rename(out / name)
    return out / name


def read(path, name):
    with Dataset(path) as d:
        d.set_auto_maskandscale(False)
        return np.array(d[name][:])


def surface_records(path):
    with Dataset(path) as d:
        return np.array(d['sst'][:]), np.array(d['xice'][:])


def reads_in_log(text):
    return re.findall(r"Read 'surface' input stream valid at (\S+)", text)


def hist(rd, hour):
    return rd / f'history.{HOURS[hour]}.nc'


def thresholded(x):
    return np.where(x < 0.02, np.float32(0), x).astype(np.float32)


def reference(mesh, run_name):
    """MPAS's reference directory of the run, or None with a counted SKIP."""
    path = ROOT / 'data/cases' / mesh / 'reference' / run_name
    if (path / f'history.{HOURS[4]}.nc').is_file():
        return path
    SKIPPED.append(f'sfc_{run_name}')
    print(f'SKIP sfc_{run_name}: the MPAS reference {path.relative_to(ROOT)} is not released', flush=True)
    return None


def check_updates(rd, mesh, records_file, hours_applied, label):
    """Every update's values in the histories: sst, xice, xland, skintemp on water, vegfra, melted cells' soil and snow."""
    sst, xice = surface_records(records_file)
    start_xland = read(rd / (('greece_60km' if mesh == GREECE else 'x1.2562') + '.init.nc'), 'xland')[0]
    for k, hour in enumerate(hours_applied):
        record = min(k, len(sst) - 1)
        h = hist(rd, hour)          # the history after the update of record k (the update comes at the start of the step)
        hs = read(h, 'sst')[0]
        assert np.array_equal(hs, sst[record]), f'FAIL {label}: sst of {h.name} is not record {record}'
        water = read(h, 'xland')[0] > 1.5
        assert np.array_equal(read(h, 'skintemp')[0][water], sst[record][water]), \
            f'FAIL {label}: skintemp on water of {h.name} is not the sst of record {record}'
        assert np.array_equal(read(h, 'xice')[0], thresholded(xice[record])), \
            f'FAIL {label}: xice of {h.name} is not record {record} under the threshold'
    return start_xland


def type_changes(rd, before_hour, after_hour):
    """Cells that went water to sea ice and back between two histories (xland 2 to 1, 1 to 2 where xice crossed 0.02)."""
    a, b = hist(rd, before_hour), hist(rd, after_hour)
    xa, xb = read(a, 'xland')[0], read(b, 'xland')[0]
    return int(((xa > 1.5) & (xb < 1.5)).sum()), int(((xa < 1.5) & (xb > 1.5)).sum())


def sfc_greece():
    rd = directory('sfc_greece', GREECE, SFC_SPEC)
    code, text = run(rd, GREECE, STEPS_7H)
    assert reads_in_log(text) == ['2026-11-08_00:00:00', '2026-11-08_03:00:00', '2026-11-08_06:00:00'], \
        f'FAIL sfc_greece: the reads in the log: {reads_in_log(text)}'
    records = ROOT / 'data/cases/greece_60km/inputs/greece_60km.sfc_update.nc'
    # the records are read at the start of the steps at 00:00, 03:00 and 06:00: the histories at 01:00, 04:00, 07:00 follow them
    check_updates(rd, GREECE, records, [1, 4, 7], 'sfc_greece')
    for hour in (2, 3, 5, 6):       # between two updates the sst of the history is the last record
        k = 0 if hour <= 3 else (1 if hour <= 6 else 2)    # a history at an update's time is written before the update
        sst, _ = surface_records(records)
        assert np.array_equal(read(hist(rd, hour), 'sst')[0], sst[k]), f'FAIL sfc_greece: sst at hour {hour}'
    restart = rd / 'restart.2026-11-08_07.00.00.nc'
    assert restart.is_file(), 'FAIL sfc_greece: no restart at 07:00'
    with Dataset(restart) as d:
        assert 'xicem' in d.variables and 'lean_alarm_sfcbdy' in d.variables, 'FAIL sfc_greece: the restart lacks xicem or the sfcbdy phase'
        previous = ''.join(np.asarray(chartostring(d['lean_alarm_sfcbdy'][:])).ravel().tolist()).strip()
        assert previous.startswith('2026-11-08_06:00:00'), f'FAIL sfc_greece: sfcbdy previous ring {previous!r}'
    ref = reference(GREECE, 'sfc_upd_7h')
    if ref is not None:
        for hour in (4, 7):
            for name in ('sst', 'xice', 'xland', 'vegfra'):
                assert np.array_equal(read(hist(rd, hour), name), read(ref / f'history.{HOURS[hour]}.nc', name)), \
                    f'FAIL sfc_greece: {name} of hour {hour} differs from MPAS'
    print('ok   sfc_greece: reads at 00:00, 03:00, 06:00; sst, skintemp of water and xice follow the records; xicem and the sfcbdy phase in the restart', flush=True)
    shutil.rmtree(rd)


def sfc_x1():
    path = x1_file()
    rd = directory('sfc_x1', X1, SFC_SPEC, path)
    code, text = run(rd, X1, STEPS_7H)
    assert reads_in_log(text) == ['2026-11-08_00:00:00', '2026-11-08_03:00:00', '2026-11-08_06:00:00'], \
        f'FAIL sfc_x1: the reads in the log: {reads_in_log(text)}'
    start_xland = check_updates(rd, X1, path, [1, 4, 7], 'sfc_x1')
    xice0, xland0 = None, None
    with Dataset(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc') as init:
        xice0, xland0 = np.array(init['xice'][0]), np.array(init['xland'][0])
    sst, xice = surface_records(path)
    expected = make_sfc_update.counts(list(xice), xice0, xland0)
    # the counts of cells that changed type at 03:00 and at 06:00: histories 03:00 to 04:00 and 06:00 to 07:00
    for (hour_before, hour_after), want in (((3, 4), expected[1]), ((6, 7), expected[2])):
        to_land, to_ocean = type_changes(rd, hour_before, hour_after)
        assert (to_land, to_ocean) == (want[0], want[1]), \
            f'FAIL sfc_x1: cells that changed type {hour_before}:00 to {hour_after}:00: lean ({to_land}, {to_ocean}), the script ({want[0]}, {want[1]})'
    # a melted cell: water, snow-free, vegfra 0, soil at the water values; a new sea-ice cell: land, vegfra 0
    h = hist(rd, 4)
    xl = read(h, 'xland')[0]
    melted = (xland0 < 1.5) & (xl > 1.5)
    frozen = (xland0 > 1.5) & (xl < 1.5)
    assert melted.sum() == expected[1][1] and frozen.sum() == expected[1][0]
    for name in ('vegfra', 'snow', 'snowh'):
        assert not read(h, name)[0][melted].any(), f'FAIL sfc_x1: {name} on the melted cells is not 0'
    assert not read(h, 'vegfra')[0][frozen].any(), 'FAIL sfc_x1: vegfra on the new sea-ice cells is not 0'
    for name in ('sh2o', 'smois'):
        assert (read(h, name)[0][melted] == 1).all(), f'FAIL sfc_x1: {name} on the melted cells is not 1'
    assert (read(h, 'tslb')[0][melted] == np.float32(273.16)).all(), 'FAIL sfc_x1: tslb on the melted cells is not 273.16'
    with Dataset(rd / 'restart.2026-11-08_07.00.00.nc') as d:
        assert 'xicem' in d.variables
        assert np.array_equal(np.array(d['xicem'][0]), thresholded(xice[2])), 'FAIL sfc_x1: xicem of the restart is not the last record'
    ref = reference(X1, 'sfc_ice_7h')
    if ref is not None:
        for hour in (4, 7):
            r = ref / f'history.{HOURS[hour]}.nc'
            ours = hist(rd, hour)
            xl_now = read(ours, 'xland')[0]
            changed = (xl_now != start_xland)
            for name in ('xland', 'xice', 'vegfra', 'sst'):
                a, b = read(ours, name)[0], read(r, name)[0]
                assert np.array_equal(a[changed], b[changed]), f'FAIL sfc_x1: {name} of hour {hour} differs from MPAS on the cells that changed type'
            water = xl_now > 1.5
            assert np.array_equal(read(ours, 'skintemp')[0][water], read(r, 'skintemp')[0][water]), \
                f'FAIL sfc_x1: skintemp on water of hour {hour} differs from MPAS'
            # the soil of sea-ice and water cells: MPAS's driver and update write exactly 1.0 (smois, sh2o), 273.16 (tslb) or leave
            # the input's value, and nothing rewrites it when the fraction changes. Judged where MPAS holds one of these values.
            with Dataset(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc') as init:
                given = {n: np.array(init[n][0]) for n in ('smois', 'sh2o', 'tslb')}
            shared = {'smois': (np.float32(1.0),), 'sh2o': (np.float32(1.0),), 'tslb': (np.float32(273.16),)}
            for name, specials in shared.items():
                mine, theirs = read(ours, name)[0], read(r, name)[0]
                # water and sea-ice cells only: on a land cell an unchanged value is Noah-MP's, which the compiler decides
                column = ((read(r, 'xland')[0] > 1.5) | (read(r, 'xice')[0] >= 0.02))[:, None]
                known = column & (np.isin(theirs, specials) | (theirs == given[name]))
                wrong = known & (mine != theirs)
                assert not wrong.any(), (f'FAIL sfc_x1: {name} of hour {hour} differs from MPAS on {int(wrong.any(axis=1).sum())} cells '
                                         f'(first cell {int(np.flatnonzero(wrong.any(axis=1))[0]) + 1}: MPAS {theirs[wrong][0]}, lean {mine[wrong][0]})')
    print(f'ok   sfc_x1: reads at 00:00, 03:00, 06:00; the cells that changed type equal the script (03:00: {expected[1][0]} to ice, {expected[1][1]} to water; '
          f'06:00: {expected[2][0]} and {expected[2][1]})', flush=True)
    shutil.rmtree(rd)


def stops(name, mesh, spec, steps, want, surface=None):
    """A run that must stop with `want` in its log, rc /= 0, and no output file."""
    rd = directory(name, mesh, spec, surface)
    code, text = run(rd, mesh, steps, expected=None)
    assert code != 0, f'FAIL {name}: the run did not stop'
    assert want in text, f'FAIL {name}: no "{want}" in the log:\n{text[-1500:]}'
    written = [p.name for p in rd.glob('*.nc') if not p.is_symlink()]
    return rd, text, written


def sst_update_default_streams():
    # MPAS's default streams (input_interval none on 'surface') with config_sst_update true: lean stops
    rd, text, written = stops('sst_default', GREECE, 'nml physics config_sst_update true\n', 10,
                              "config_sst_update = .true. needs an input_interval on the 'surface' stream (it is 'none')")
    assert not written, f'FAIL sst_update_default_streams: wrote {written}'
    print('ok   sst_update_default_streams: the stop at the start, nothing written', flush=True)
    shutil.rmtree(rd)


def sfc_read_no_update():
    rd, text, written = stops('sfc_read_only', GREECE, 'stream surface input_interval 3:00:00\n', 10,
                              "The 'surface' stream is read every 3:00:00, but config_sst_update = .false.")
    assert not written, f'FAIL sfc_read_no_update: wrote {written}'
    print('ok   sfc_read_no_update: the stop at the start, nothing written', flush=True)
    shutil.rmtree(rd)


def sfc_ice_on_land():
    src = x1_file()
    bad = WORK / 'sfc_ice' / 'x1_land_ice.nc'
    shutil.copyfile(src, bad)
    with Dataset(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc') as init:
        land = int(np.flatnonzero(np.array(init['xland'][0]) < 1.5)[0])
    with Dataset(bad, 'a') as d:
        d['xice'][1, land] = 0.5
    rd, text, _ = stops('sfc_ice_land', X1, SFC_SPEC, 95,
                        f'land cell(s) receive sea ice (the first: cell {land + 1}, xice 0.5', surface=bad)
    assert '2026-11-08_03:00:00: ' in text, 'FAIL sfc_ice_on_land: the stop does not name the time'
    assert reads_in_log(text) == ['2026-11-08_00:00:00', '2026-11-08_03:00:00'], 'FAIL sfc_ice_on_land: the reads before the stop'
    print(f'ok   sfc_ice_on_land: the stop at 03:00 names cell {land + 1}', flush=True)
    shutil.rmtree(rd)


def make_file(path, records, source=None):
    """A copy of greece's surface file with the given records only: [(xtime text, source record index), ...]."""
    source = source or ROOT / 'data/cases/greece_60km/inputs/greece_60km.sfc_update.nc'
    with Dataset(source) as src, Dataset(path, 'w', format='NETCDF3_64BIT_DATA') as out:
        for name, dim in src.dimensions.items():
            out.createDimension(name, None if dim.isunlimited() else len(dim))
        for name, var in src.variables.items():
            v = out.createVariable(name, var.dtype, var.dimensions)
            v.setncatts({a: var.getncattr(a) for a in var.ncattrs()})
        out.setncatts({a: src.getncattr(a) for a in src.ncattrs()})
        for k, (stamp, index) in enumerate(records):
            out['xtime'][k, :] = np.frombuffer(stamp.ljust(64).encode('ascii'), dtype='S1')
            out['Time'][k] = src['Time'][index]
            out['sst'][k, :] = src['sst'][index, :]
            out['xice'][k, :] = src['xice'][index, :]


def sfc_records_offset():
    # (a) the first record is after the start: a stop at the first step, naming the start time
    late = WORK / 'late.sfc_update.nc'
    WORK.mkdir(parents=True, exist_ok=True)
    make_file(late, [('2026-11-08_03:00:00', 1), ('2026-11-08_06:00:00', 2)])
    rd, text, written = stops('sfc_late', GREECE, SFC_SPEC, 5,
                              'does not contain a record at or before 2026-11-08_00:00:00', surface=late)
    assert 'Error reading input stream surface' in text, 'FAIL sfc_records_offset: no MPAS line of the failed read'
    assert not reads_in_log(text), 'FAIL sfc_records_offset: a read succeeded'
    shutil.rmtree(rd)
    # (b): a daily file read every 6 h: each update applies the record at or before it, and the log names that record's time
    daily = WORK / 'daily.sfc_update.nc'
    make_file(daily, [('2026-11-08_00:00:00', 0), ('2026-11-09_00:00:00', 4)])
    spec = SFC_SPEC.replace('input_interval 3:00:00', 'input_interval 6:00:00')
    rd = directory('sfc_daily', GREECE, spec, daily)
    run(rd, GREECE, STEPS_7H)
    text = (WORK / 'sfc_daily.log').read_text()
    assert reads_in_log(text) == ['2026-11-08_00:00:00', '2026-11-08_00:00:00'], \
        f'FAIL sfc_records_offset: the daily file reads {reads_in_log(text)}'
    sst, _ = surface_records(daily)
    for hour in (1, 7):
        assert np.array_equal(read(hist(rd, hour), 'sst')[0], sst[0]), f'FAIL sfc_records_offset: sst at hour {hour}'
    print('ok   sfc_records_offset: the stop names the start time; a daily file read every 6 h applies the record at or before each update', flush=True)
    shutil.rmtree(rd)


TIME_TEMPLATE = 'stream surface filename_template sfc.$Y-$M-$D_$h.nc\n'


def file_set(mesh, source, records, ice_cell=None):
    """A set of one-record files sfc.<date>_<hh>.nc in the scratch folder, one per hour in `records` ({hour: record index of
    `source`}); ice_cell: the cell that gets xice 0.5 in the last file. Returns the paths."""
    out = WORK / ('files_' + mesh)
    shutil.rmtree(out, ignore_errors=True)
    out.mkdir(parents=True)
    paths = []
    for hour, index in records.items():
        path = out / f'sfc.2026-11-08_{hour:02d}.nc'
        make_file(path, [(f'2026-11-08_{hour:02d}:00:00', index)], source=source)
        paths.append(path)
    if ice_cell is not None:
        with Dataset(paths[-1], 'a') as d:
            d['xice'][0, ice_cell] = 0.5
    return paths


def link_files(rd, paths):
    for path in paths:
        (rd / path.name).symlink_to(path)


def sfc_time_files():
    # A filename template with time tokens names a file per ring (MPAS: read_stream opens the file of the read time,
    # mpas_stream_manager.F:3686-3698)
    source = x1_file()
    paths = file_set('x1', source, {0: 0, 3: 1, 6: 2})
    rd = directory('sfc_files', X1, SFC_SPEC + TIME_TEMPLATE)
    link_files(rd, paths)
    code, text = run(rd, X1, STEPS_7H)
    assert reads_in_log(text) == ['2026-11-08_00:00:00', '2026-11-08_03:00:00', '2026-11-08_06:00:00'], \
        f'FAIL sfc_time_files: the reads in the log: {reads_in_log(text)}'
    check_updates(rd, X1, source, [1, 4, 7], 'sfc_time_files')
    print('ok   sfc_time_files: a file per ring: the records of three files are read at 00:00, 03:00, 06:00', flush=True)
    shutil.rmtree(rd)


def sfc_late_ice():
    # Sea ice that appears only in the last file of the set. The file test at the start looks at every file of the run, so
    # the sea-ice code is there when the cell freezes at 06:00
    source = ROOT / 'data/cases/greece_60km/inputs/greece_60km.sfc_update.nc'
    with Dataset(ROOT / 'data/cases/greece_60km/inputs/greece_60km.init.nc') as init:
        water = np.flatnonzero(np.array(init['xland'][0]) > 1.5)
    cell = int(water[len(water) // 2])
    paths = file_set('greece', source, {0: 0, 3: 1, 6: 2}, ice_cell=cell)
    rd = directory('sfc_late_ice', GREECE, SFC_SPEC + TIME_TEMPLATE)
    link_files(rd, paths)
    run(rd, GREECE, STEPS_7H)
    assert read(hist(rd, 4), 'xland')[0][cell] > 1.5, 'FAIL sfc_late_ice: the cell froze before its file'
    assert read(hist(rd, 7), 'xland')[0][cell] == 1, 'FAIL sfc_late_ice: the cell did not freeze at 06:00'
    with Dataset(rd / 'restart.2026-11-08_07.00.00.nc') as d:
        assert d['lean_seaice_xice'][0][cell] >= 0.02, 'FAIL sfc_late_ice: the cell is not sea ice in the restart'
    skin = float(read(hist(rd, 7), 'skintemp')[0][cell])
    assert 200 < skin < 300, f'FAIL sfc_late_ice: skintemp of the new sea-ice cell {skin}'
    print(f'ok   sfc_late_ice: cell {cell + 1} freezes at 06:00 from the last file and runs as sea ice', flush=True)
    shutil.rmtree(rd)


def off_lsm_sfc():
    # The surface update with config_lsm_scheme 'off' (x1, the made file). MPAS's update writes tslb
    # itself, without any land-surface scheme: 273.16 on water after the first update (mpas_atmphys_update_surface.F:271-275),
    # the skin temperature the cell had on a new sea-ice cell (:226). There is no MPAS reference of this combination: the
    # test judges lean's rule on the cells that changed type.
    path = x1_file()
    rd = directory('off_lsm_sfc', X1, SFC_SPEC + "nml physics config_lsm_scheme 'off'\n", path)
    run(rd, X1, STEPS_7H)
    with Dataset(ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc') as init:
        xland0 = np.array(init['xland'][0])
    h3, h4 = hist(rd, 3), hist(rd, 4)
    tslb, xland = read(h4, 'tslb')[0], read(h4, 'xland')[0]
    water = xland > 1.5
    assert (tslb[water] == np.float32(273.16)).all(), 'FAIL off_lsm_sfc: tslb on water after the update is not 273.16'
    frozen = (xland0 > 1.5) & (xland < 1.5)
    assert frozen.any()
    old_skin = read(h3, 'skintemp')[0][frozen]       # the history at 03:00 is written before the update of 03:00
    for level in range(4):
        assert (tslb[frozen][:, level] == old_skin).all(), f'FAIL off_lsm_sfc: tslb level {level + 1} of a new sea-ice cell is not its skin temperature'
    print(f'ok   off_lsm_sfc: LSM off: tslb is 273.16 on water and the old skin temperature on {int(frozen.sum())} new sea-ice cells', flush=True)
    shutil.rmtree(rd)


def update_off(mesh, label):
    """With the update off xicem is the start's raw xice (mpas_atmphys_init.F:328-333) and no
    sfcbdy alarm exists, so its phase in the restart is blank."""
    rd = directory('update_off_' + label, mesh, OFF_STREAMS)
    run(rd, mesh, 90)
    with Dataset(rd / 'restart.2026-11-08_03.00.00.nc') as d:
        assert np.array_equal(np.array(d['xicem'][:]), np.array(d['xice'][:])), f'FAIL update_off_{label}: xicem is not the restart\'s xice'
        phase = ''.join(np.asarray(chartostring(d['lean_alarm_sfcbdy'][:])).ravel().tolist()).replace('\x00', '').strip()
        assert phase == '', f'FAIL update_off_{label}: the sfcbdy phase is {phase!r}, not blank'
    print(f'ok   update_off_{label}: xicem is the restart\'s xice, the sfcbdy phase is blank', flush=True)
    shutil.rmtree(rd)


CASES = {
    'sst_update_default_streams': sst_update_default_streams,
    'sfc_read_no_update': sfc_read_no_update,
    'sfc_ice_on_land': sfc_ice_on_land,
    'sfc_records_offset': sfc_records_offset,
    'sfc_time_files': sfc_time_files,
    'sfc_late_ice': sfc_late_ice,
    'off_lsm_sfc': off_lsm_sfc,
    'sfc_greece': sfc_greece,
    'sfc_x1': sfc_x1,
    'update_off_greece': lambda: update_off(GREECE, 'greece'),
    'update_off_x1': lambda: update_off(X1, 'x1'),
}

if __name__ == '__main__':
    if not all((ROOT / 'data/cases' / mesh / 'inputs').is_dir() for mesh in (GREECE, X1)):
        print('SKIP surface runs: needs the release case inputs in data/cases (cases/README.md)')
        sys.exit(77)
    WORK.mkdir(parents=True, exist_ok=True)
    names = SELECTED or list(CASES)
    for name in names:
        CASES[name]()
    shutil.rmtree(WORK, ignore_errors=True)
    try:
        WORK.parent.rmdir()
    except OSError:
        pass
    print(f'SURFACE RUNS OK {len(names)}' + (f', {len(SKIPPED)} skipped: {", ".join(SKIPPED)}' if SKIPPED else ''))
