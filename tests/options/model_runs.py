"""Serial real-model runs of the namelist options and the scheme 'off' sets.

  python3 tests/options/model_runs.py [FC] [JOBS] [case ...]     the named cases, or every case

One plain build (flavor mpas); every run is atmosphere_model's form in an MPAS run directory. The base run of each mesh
(greece_60km and x1_2562, 3 h of 120 s steps, history and diag 1 h, restart 3 h, every option at its case value) is what
an option run must differ from. The runs need the release case inputs in data/cases; the comparisons with MPAS's own
'off' runs need its reference outputs, which are not released, and SKIP without them. Scratch run directories are
removed after a pass.
"""
from pathlib import Path
import json
import os
import shlex
import shutil
import subprocess
import sys

import numpy as np
from netCDF4 import Dataset

ROOT = Path(__file__).resolve().parents[2]
os.chdir(ROOT)
sys.path.insert(0, str(ROOT))
from tests.writer.t0_values import compare as compare_t0
from tools.cases.run_spec import edit_namelist

args = sys.argv[1:]
FC = args[0] if len(args) > 0 else 'gfortran'
JOBS = args[1] if len(args) > 1 else '2'
SELECTED = args[2:]
assert 1 <= int(JOBS) <= 2
WORK = ROOT / 'build/options/runs'
GREECE, X1 = 'greece_60km', 'x1_2562'
STEPS = 90   # 3 h of 120 s
STREAMS = ('nml nhyd_model config_run_duration \'0_03:00:00\'\n'
           'stream output output_interval 1:00:00\n'
           'stream diagnostics output_interval 1:00:00\n'
           'stream restart output_interval 3:00:00\n')
BUILD = []
BASES = {}
STAMPS = tuple(f'2026-11-08_0{h}.00.00' for h in range(4))
LOGS = ('log.atmosphere.0000.out', 'log.atmosphere.0000.err')


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
        log = WORK / 'build.log'
        command(['./lean', 'build', '--flavor', 'mpas', '--target', 'cpu', '--fc', FC, '--jobs', JOBS, '--json'], log)
        BUILD.append(Path(json.loads(log.read_text()[log.read_text().index('{'):])['build_json']).resolve().parent)
    return BUILD[0]


def directory(name, mesh, spec):
    """A run directory of the case with `spec` (run_spec.py lines) applied after the case's own."""
    rd = WORK / name
    shutil.rmtree(rd, ignore_errors=True)
    base = 'strict_12h.txt' if mesh == GREECE else 'strict_24h.txt'
    extra = WORK / (name + '.spec')
    extra.write_text(spec)
    cmd = 'root=' + shlex.quote(str(ROOT)) + '; . tests/inputs/run_dir.sh; make_run_dir '
    cmd += ' '.join(shlex.quote(str(p)) for p in [rd, ROOT / 'data/cases' / mesh / 'inputs',
                                                   f'cases/{mesh}/case.txt', f'cases/{mesh}/mpas_runs/{base}', extra])
    command(['sh', '-c', cmd], WORK / (name + '.setup.log'))
    return rd


def set_steps(rd, steps):
    """config_run_duration of the run directory's namelist: `steps` steps of 120 s."""
    s = steps * 120
    path = rd / 'namelist.atmosphere'
    path.write_text(edit_namelist(path.read_text(), 'nhyd_model', 'config_run_duration',
                                  f"'{s // 86400}_{s // 3600 % 24:02d}:{s // 60 % 60:02d}:{s % 60:02d}'"))


def run(rd, steps=STEPS, env=None, expected=0):
    """atmosphere_model's form in the run directory, `steps` steps long (None: the namelist's own length). Returns the
    exit code and the text of log.atmosphere.0000.out (of stdout and the .err file if it stopped before opening it)."""
    if steps is not None:
        set_steps(rd, steps)
    launcher = rd / 'atmosphere_model'
    if not launcher.is_symlink():
        launcher.symlink_to(build() / 'lean_mpas')
    for name in LOGS:
        (rd / name).unlink(missing_ok=True)
    out = WORK / (rd.name + '.stdout')
    code = command([launcher], out, rd, dict(os.environ, **(env or {})), None)
    log = rd / LOGS[0]
    text = log.read_text() if log.is_file() else out.read_text() + (
        (rd / LOGS[1]).read_text() if (rd / LOGS[1]).is_file() else '')
    (WORK / (rd.name + '.log')).write_text(text)
    if expected is not None and code != expected:
        print(text[-10000:])
        raise AssertionError(f'{rd.name}: exit {code}, expected {expected}')
    return code, text


def base(mesh, label):
    """The run of the mesh with every option at its case value (STREAMS), kept until the end of the pass."""
    if mesh not in BASES:
        rd = directory('base_' + label, mesh, STREAMS)
        run(rd)
        for name in [f'{kind}.{stamp}.nc' for kind in ('history', 'diag') for stamp in STAMPS]:
            assert (rd / name).is_file(), f'FAIL base_{label}: no {name}'
        BASES[mesh] = rd
    return BASES[mesh]


def finite_and_changed(rd, label, fields=('u', 'theta', 'qv')):
    """The 03:00 history of a run with an option set, against the base run's: every field finite, and the option acts, so
    some of the fields differ."""
    old = base(GREECE if label == 'greece' else X1, label)
    changed = []
    with Dataset(rd / 'history.2026-11-08_03.00.00.nc') as new, Dataset(old / 'history.2026-11-08_03.00.00.nc') as before:
        for name in fields:
            a = np.asarray(new[name][:])
            assert np.isfinite(a).all(), f'FAIL {label}: {name} is not finite'
            if a.tobytes() != np.asarray(before[name][:]).tobytes():
                changed.append(name)
    assert changed, f'FAIL {label}: the option changed none of {fields}'
    return changed


def apvm_runs():
    """config_apvm_upwinding 0.5 on both meshes, 3 h: the run finishes, is finite, and differs from the base run."""
    for mesh, label in ((GREECE, 'greece'), (X1, 'x1')):
        rd = directory('apvm_' + label, mesh, STREAMS + 'nml nhyd_model config_apvm_upwinding 0.5\n')
        run(rd)
        changed = finite_and_changed(rd, label)
        print(f'ok   apvm_runs {label}: 3 h with apvm 0.5, finite, differs from the base run in {", ".join(changed)}', flush=True)
        shutil.rmtree(rd)


def restart_radii(rd):
    """re_cloud, re_ice and re_snow of the 03:00 restart file: Thompson's radii of the last step (zero at a cold start)."""
    with Dataset(rd / 'restart.2026-11-08_03.00.00.nc') as restart:
        return [np.asarray(restart[name][:]) for name in ('re_cloud', 're_ice', 're_snow')]


def options_runs():
    """cld_incidence on greece; microp_re false on both meshes; LW 'off' with microp_re true on greece. With MPAS's own
    radii (has_re = 0) Thompson computes none, so the restart's re_* stay at their cold-start zeros; with Thompson's they are
    positive. Every run finishes and is finite."""
    runs = (
        ('cld_incidence', GREECE, 'greece', 'nml physics config_radt_cld_scheme \'cld_incidence\'\n', True),
        ('re_false', GREECE, 'greece', 'nml physics config_microp_re false\n', False),
        ('re_false', X1, 'x1', 'nml physics config_microp_re false\n', False),
        ('lw_off', GREECE, 'greece', 'nml physics config_radt_lw_scheme \'off\'\n', False),
    )
    for name, mesh, label, line, thompson in runs:
        rd = directory(f'{name}_{label}', mesh, STREAMS + line)
        run(rd)
        changed = finite_and_changed(rd, label)
        radii = restart_radii(rd)
        for values in radii:
            assert np.isfinite(values).all(), f'FAIL {name} {label}: radii not finite'
        positive = [bool((values > 0).any()) for values in radii]
        if thompson:
            assert all(positive), f'FAIL {name} {label}: Thompson gave no radii {positive}'
        else:
            assert not any(positive), f'FAIL {name} {label}: radii changed from their start values {positive}'
        print(f'ok   options_runs {name} {label}: 3 h, finite, differs from the base run in {", ".join(changed)}; '
              f'Thompson radii {"computed" if thompson else "not computed (start values kept)"}', flush=True)
        shutil.rmtree(rd)


# The MPAS run specs of the 'off' sets MPAS 8.4.2 runs (cases/<mesh>/mpas_runs/<run>.txt), each with the 3 h streams
OFF_RUNS = {
    GREECE: ('off_conv_3h', 'off_gwdo_3h', 'off_lw_3h', 'off_rad_3h', 'off_pbl_3h', 'off_lsm_3h', 'off_sfc_3h',
             'off_all_3h'),
    X1: ('off_rad_3h', 'off_lsm_3h', 'off_sfc_3h', 'off_all_3h'),
}
SKIPPED = []
# Fields that must equal MPAS's in every later record, by construction (arrays no scheme that runs writes; evolved fields are
# compared only by an nvfortran build, whose state is MPAS's): with no land-surface scheme MPAS's soil arrays stay the input's and Noah-MP's accumulated snow
# is never written (mpas_atmphys_driver_lsm_noahmp.F, called from mpas_atmphys_driver.F:283-317 only); with no physics at all the
# physics driver never sets surface_pressure (mpas_atmphys_interface.F:436,959), relhum (the physics diagnostics) or the ozone
# arrays (mpas_atmphys_init.F), so the file's t=0 values stay.
LATER_EXACT_LSM = ('sh2o', 'smois', 'tslb', 'snowh', 'acsnow')
LATER_EXACT_NONE = ('surface_pressure', 'relhum', 'pin', 'ozmixm')


def run_spec_length(rd):
    """A run of the length its spec sets; the text of log.atmosphere.0000.out."""
    run(rd, None)
    return (rd / 'log.atmosphere.0000.out').read_text()


def variables_of(path):
    with Dataset(path) as ds:
        return set(ds.variables)


def off_structure():
    """Every 'off' set runs on its meshes and ends. Without a scheme its outputs are gone (rainc and cuprec with
    convection off), the radiation log lines follow the schemes, and with no physics at all the time tracker's lines are
    not written. Against MPAS's own files of the same runs the variables of each history and diag file must be exactly
    MPAS's; a missing reference prints SKIP."""
    for mesh, label in ((GREECE, 'greece'), (X1, 'x1')):
        base_diag = variables_of(base(mesh, label) / 'diag.2026-11-08_03.00.00.nc')
        base_history = variables_of(base(mesh, label) / 'history.2026-11-08_03.00.00.nc')
        assert {'rainc', 'cuprec', 'olrtoa'} <= base_diag | base_history, 'the base run lacks the fields the test looks for'
        for name in OFF_RUNS[mesh]:
            rd = directory(f'{name}_{label}', mesh, (ROOT / f'cases/{mesh}/mpas_runs/{name}.txt').read_text())
            log = run_spec_length(rd)
            for stamp in STAMPS:
                for kind in ('history', 'diag'):
                    assert (rd / f'{kind}.{stamp}.nc').is_file(), f'FAIL {name} {label}: no {kind}.{stamp}.nc'
            assert (rd / 'restart.2026-11-08_03.00.00.nc').is_file(), f'FAIL {name} {label}: no restart'
            assert 'ERROR' not in log and 'CRITICAL' not in log, f'FAIL {name} {label}: the log holds an error'
            with Dataset(rd / 'history.2026-11-08_03.00.00.nc') as history:
                for field in ('u', 'theta'):
                    assert np.isfinite(np.asarray(history[field][:])).all(), f'FAIL {name} {label}: {field} not finite'
            diag = variables_of(rd / 'diag.2026-11-08_03.00.00.nc')
            history = variables_of(rd / 'history.2026-11-08_03.00.00.nc')
            conv_off = name in ('off_conv_3h', 'off_pbl_3h', 'off_sfc_3h', 'off_all_3h')
            assert conv_off == ('rainc' not in diag | history and 'cuprec' not in diag | history), \
                f'FAIL {name} {label}: rainc and cuprec must be gone exactly with convection off'
            radiation = {'off_lw_3h': (False, True), 'off_rad_3h': (False, False),
                         'off_all_3h': (False, False)}.get(name, (True, True))
            lines = [line for line in log.splitlines() if '--- time to' in line]
            assert any('L_RADLW' in line for line in lines) == (radiation[0] and name != 'off_all_3h'), \
                f'FAIL {name} {label}: the LW line follows the LW scheme'
            assert any('L_RADSW' in line for line in lines) == (radiation[1] and name != 'off_all_3h'), \
                f'FAIL {name} {label}: the SW line follows the SW scheme'
            assert any('L_CONV' in line for line in lines) == (not conv_off and name != 'off_all_3h'), \
                f'FAIL {name} {label}: the convection line follows the convection scheme'
            if name == 'off_all_3h':
                assert not lines, f'FAIL {name} {label}: the time tracker wrote {len(lines)} lines with no physics'
            reference = ROOT / f'data/cases/{mesh}/reference/{name}'
            if not (reference / 'history.2026-11-08_03.00.00.nc').is_file():
                SKIPPED.append(f'off_structure {name} {label}')
                print(f'SKIP off_structure {name} {label}: the MPAS reference {reference.relative_to(ROOT)} is not released', flush=True)
            else:
                for kind, mine in (('history', history), ('diag', diag)):
                    theirs = variables_of(reference / f'{kind}.2026-11-08_03.00.00.nc') - {'file_id'}
                    assert mine - {'file_id'} == theirs, \
                        f'FAIL {name} {label} {kind}: only lean {sorted(mine - theirs)[:6]}, only MPAS {sorted(theirs - mine)[:6]}'
                # off_t0: the 00:00 records of history and diag against MPAS's own, every field (t=0 holds for any compiler)
                for kind in ('history', 'diag'):
                    compare_t0(rd / f'{kind}.2026-11-08_00.00.00.nc', reference / f'{kind}.2026-11-08_00.00.00.nc', FC)
                later = (LATER_EXACT_LSM if name in ('off_lsm_3h', 'off_sfc_3h', 'off_all_3h') else ()) + \
                        (LATER_EXACT_NONE if name == 'off_all_3h' else ())
                for stamp in STAMPS[1:]:
                    with Dataset(rd / f'history.{stamp}.nc') as mine_ds, Dataset(reference / f'history.{stamp}.nc') as mpas_ds:
                        for field in later:
                            if field in mpas_ds.variables:
                                a = np.asarray(mine_ds[field][:]); b = np.asarray(mpas_ds[field][:])
                                assert a.shape == b.shape and a.tobytes() == b.tobytes(), \
                                    f'FAIL {name} {label} {stamp} {field}: differs from MPAS in {int((a != b).sum())} values'
                print(f'ok   off_t0 {name} {label}: the 00:00 history and diag records equal MPAS\'s', flush=True)
            print(f'ok   off_structure {name} {label}: runs, finite, outputs and log lines follow the schemes', flush=True)
            shutil.rmtree(rd)


def off_species():
    """With no microphysics (suite 'none') the cloud and number constituents are zero from the first step on and qv
    evolves; MPAS has no such constituents then."""
    for mesh, label in ((GREECE, 'greece'), (X1, 'x1')):
        rd = directory(f'species_{label}', mesh, (ROOT / f'cases/{mesh}/mpas_runs/off_all_3h.txt').read_text())
        run_spec_length(rd)
        with Dataset(rd / 'restart.2026-11-08_03.00.00.nc') as restart:
            for name in ('qc', 'qr', 'qi', 'qs', 'qg', 'ni', 'nr'):
                assert not np.asarray(restart[name][:]).any(), f'FAIL species {label}: {name} is not zero'
            qv = np.asarray(restart['qv'][:])
        with Dataset(rd / 'history.2026-11-08_00.00.00.nc') as start:
            qv0 = np.asarray(start['qv'][:])
        assert np.isfinite(qv).all() and qv.tobytes() != qv0.tobytes(), f'FAIL species {label}: qv did not evolve'
        print(f'ok   off_species {label}: qc..nr are 0 after 3 h, qv evolves', flush=True)
        shutil.rmtree(rd)


# Restart-only fields of the runs with convection or all physics 'off', lean's 03:00 restart against MPAS's:
# rthdynten is the raw dynamics tendency when no convection scheme reads it (mpas_atm_time_integration.F:2760-2796 convert it
# only for Grell-Freitas and ntiedtke; :6936 is the raw value); with no physics the physics initialisation never sets znt,
# sfc_emiss and sfc_albedo (0 as allocated, mpas_atm_core.F:580-596) and the physics driver never sets surface_pressure
# (mpas_atmphys_interface.F:436,959; the input's value stays). The gfortran limit for rthdynten is 1e-3 of the field's
# largest value (measured: 1.2e-4 and 4e-4 of it).
# x1 only with no physics: with physics running, gfortran's evolved rthdynten on x1 differs from MPAS's by 1e-4 to 3e-4 even with
# convection on (off_rad, off_lsm; the field's largest value is 1.8e-3), more than the term this checks (1.3e-4), so with
# gfortran only the run without physics (noise 7e-7) can tell; an nvfortran build checks the others.
RESTART_RUNS = {GREECE: ('off_conv_3h', 'off_pbl_3h', 'off_sfc_3h', 'off_all_3h'), X1: ('off_all_3h',)}
EXACT_NO_PHYSICS = ('znt', 'sfc_emiss', 'sfc_albedo', 'surface_pressure')


def off_restart():
    for mesh, label in ((GREECE, 'greece'), (X1, 'x1')):
        for name in RESTART_RUNS[mesh]:
            reference = ROOT / f'data/cases/{mesh}/reference/{name}/restart.2026-11-08_03.00.00.nc'
            if not reference.is_file():
                SKIPPED.append(f'off_restart {name} {label}')
                print(f'SKIP off_restart {name} {label}: the MPAS restart {reference.relative_to(ROOT)} is not released', flush=True)
                continue
            rd = directory(f'restart_{name}_{label}', mesh, (ROOT / f'cases/{mesh}/mpas_runs/{name}.txt').read_text())
            run_spec_length(rd)
            with Dataset(rd / 'restart.2026-11-08_03.00.00.nc') as mine, Dataset(reference) as mpas:
                a = np.asarray(mine['rthdynten'][:]).ravel(); b = np.asarray(mpas['rthdynten'][:]).ravel()
                worst = float(np.abs(a.astype('f8') - b.astype('f8')).max()); limit = 1e-3 * float(np.abs(b).max())
                assert worst <= limit, f'FAIL off_restart {name} {label}: rthdynten differs by {worst:.3g}, limit {limit:.3g}'
                checked = ['rthdynten']
                if name == 'off_all_3h':
                    for field in EXACT_NO_PHYSICS:
                        x = np.asarray(mine[field][:]).ravel(); y = np.asarray(mpas[field][:]).ravel()
                        assert x.tobytes() == y.tobytes(), \
                            f'FAIL off_restart {name} {label}: {field} differs from MPAS in {int((x != y).sum())} cells, max {float(np.abs(x - y).max()):.3g}'
                        checked.append(field)
            print(f'ok   off_restart {name} {label}: {", ".join(checked)} agree with MPAS\'s restart', flush=True)
            shutil.rmtree(rd)


CASES = {
    'apvm_runs': apvm_runs,
    'options_runs': options_runs,
    'off_structure': off_structure,
    'off_t0': off_structure,   # the 00:00 comparison runs inside the same 13 runs
    'off_species': off_species,
    'off_restart': off_restart,
}

if __name__ == '__main__':
    if not all((ROOT / 'data/cases' / mesh / 'inputs').is_dir() for mesh in (GREECE, X1)):
        print('SKIP options runs: needs the release case inputs in data/cases (cases/README.md)')
        sys.exit(77)
    WORK.mkdir(parents=True, exist_ok=True)
    names = SELECTED or [name for name in CASES if name != 'off_t0']
    for name in names:
        CASES[name]()
    shutil.rmtree(WORK)
    try:
        WORK.parent.rmdir()
    except OSError:
        pass
    if SKIPPED:
        print(f'{len(SKIPPED)} skipped (MPAS references not released): ' + ', '.join(SKIPPED), flush=True)
    print('OPTIONS RUNS OK')
