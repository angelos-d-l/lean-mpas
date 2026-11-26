#!/bin/bash
# linux/cases/init.sh, mpas_run.sh and lean_run.sh with stand-ins for the models, mpirun, GNU time, taskset, MPS and
# nvidia-smi: what each publishes, records and refuses, and that MPAS and lean get the same run directory and cores.
# No model and no card.
set -eu
R=$(cd "$(dirname "$0")/../.." && pwd)
cd "$R"
exec "${LEAN_PY:-${PYTHON:-python3}}" - "$R" <<'PY'
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(sys.argv.pop())

# One stand-in for every outside program; it acts by the name it was started under (or STUB_KIND) and logs each call
# to events.jsonl. state.json, written by the test, sets the failure to play.
STUB = r'''
import hashlib, json, os, signal, subprocess, sys, time
from pathlib import Path
kind = os.environ.pop('STUB_KIND', None) or Path(sys.argv[0]).name
args = sys.argv[1:]
root = Path(os.environ['STUB_ROOT'])
statefile = root / 'state.json'
def state(): return json.loads(statefile.read_text())
def save(s):
    staged = root / ('state.' + str(os.getpid()))
    staged.write_text(json.dumps(s)); staged.replace(statefile)
def event(name, **kw):
    with (root / 'events.jsonl').open('a') as f:
        f.write(json.dumps(dict(event=name, args=args, **kw)) + '\n')
s = state()
if kind == 'date':
    if args == ['+%s']:
        s['seconds'] = s.get('seconds', 1794124800) + 12; save(s); print(s['seconds'])
    elif args == ['+%F %T']:
        event('record-run'); print('2026-01-01 08:00:00')
    else: raise AssertionError(args)
elif kind == 'sleep':
    assert args == ['1'], args
    event('wait')
elif kind == 'uptime': print('08:00 up 2 days, load average: 0.10, 0.20, 0.30')
elif kind == 'taskset':
    assert args[0] == '-c', args
    event('taskset', cores=args[1]); os.execvp(args[2], args[2:])
elif kind == 'time':
    assert args[0] == '-v', args
    rc = subprocess.call(args[1:])
    print('\tMaximum resident set size (kbytes): 1048576', file=sys.stderr)
    sys.exit(rc)
elif kind == 'nvidia-smi':
    event('card-sample')
    print(s.get('card', 100), flush=True)
    if any(a.startswith('--loop') for a in args):
        while True: time.sleep(.1)
elif kind == 'mps-pids':
    assert not args, args
    pids = [313131, 313132]  # daemons of other runs of the same user
    if (root / 'mps-owned').exists(): pids.append(424242)
    event('mps-pids', pids=pids)
    print('\n'.join(map(str, pids)))
elif kind == 'mps-alive':
    assert args == ['-0', '424242'], args
    alive = (root / 'mps-owned').exists()
    event('mps-alive', alive=alive)
    sys.exit(0 if alive else 1)
elif kind == 'nvidia-cuda-mps-control':
    owned = root / 'mps-owned'
    if args == ['-d']:
        event('mps-start')
        if s.get('mps_fail'): sys.exit(1)
        owned.write_text('yes')
        if s.get('mps_start_signal'):
            os.kill(os.getppid(), s['mps_start_signal'])
            time.sleep(.1)
            event('mps-start-return')
    else:
        cmd = sys.stdin.read().strip()
        if cmd == 'get_server_list':
            event('mps-probe')
            sys.exit(0 if (owned.exists() or s.get('mps_existing')) and not s.get('mps_probe_fail') else 1)
        assert cmd == 'quit', cmd
        event('mps-quit'); assert owned.exists(), 'quit without ownership'
        if s.pop('quit_fail_once', False): save(s); sys.exit(1)
        owned.unlink()
elif kind == 'sha256sum':
    assert args[:1] == ['-c'], args
    event('table-check')
    if s.get('bad_tables'): sys.exit(1)
    for line in Path(args[1]).read_text().splitlines():
        digest, filename = line.split()
        assert hashlib.sha256(Path(filename).read_bytes()).hexdigest() == digest
elif kind == 'md5sum':
    print(hashlib.md5(Path(args[0]).read_bytes()).hexdigest(), args[0])
elif kind in ('init_atmosphere_model', 'atmosphere_model', 'lean_mpas'):
    event('model', kind=kind, cwd=os.getcwd(), env={k: os.environ.get(k) for k in
          ('OMP_NUM_THREADS', 'OMP_STACKSIZE', 'LEAN_CARD', 'LEAN_PRODUCTS', 'LEAN_FILM', 'BUILD_ENV',
           'CUDA_MPS_PIPE_DIRECTORY', 'CUDA_MPS_LOG_DIRECTORY')})
    assert not args, args
    mode = s.get('mode', 'ok')
    core = 'init_atmosphere' if kind == 'init_atmosphere_model' else 'atmosphere'
    Path('log.' + core + '.0000.out').write_text(
        '    timer_name   total   calls\n'
        ' 1 total time                26.00000         1\n'
        ' 2  time integration          25.60500        90\n'
        '   Error messages =              ' + ('1' if mode == 'errors' else '0') + '\n'
        '   Critical error messages =     0\n')
    if s.get('check_links'):
        assert Path('mesh.nc').read_text() == 'variant mesh\n'
        assert Path('top_only.nc').read_text() == 'top only\n'
        assert not Path('result.nc').exists() and not Path('result.nc').is_symlink()
        if kind == 'init_atmosphere_model':
            assert Path('GFS:2026-01-01_00').is_symlink()
            assert not Path('GFS:2026-01-01_00:00').exists()
    if mode not in ('missing',):
        if mode == 'symlink': Path('result.nc').symlink_to(root / 'base-result')
        else: Path('result.nc').write_text('' if mode == 'empty' else 'new result\n')
    print('velocity = NaN' if mode == 'nan' else 'finite banana nanoseconds')
    event('model-stop')
    sys.exit(7 if mode == 'rc7' else 0)
else: raise AssertionError((kind, args))
'''

# mpirun checks its arguments, logs them and replaces itself with the model.
MPIRUN = r'''
import json, os, sys
from pathlib import Path
args = sys.argv[1:]
with (Path(os.environ['STUB_ROOT']) / 'mpi.jsonl').open('a') as f: f.write(json.dumps(args) + '\n')
assert args[:2] == ['-np', '2'], args
args = args[2:]
while args[0] in ('-x', '--bind-to'):
    args = args[2:]
os.execvp(args[0], args)
'''

TABLES = ('CAM_ABS_DATA.DBL CAM_AEROPT_DATA.DBL CCN_ACTIVATE_DATA GENPARM.TBL LANDUSE.TBL OZONE_DAT.TBL OZONE_LAT.TBL '
          'OZONE_PLEV.TBL RRTMG_LW_DATA RRTMG_LW_DATA.DBL RRTMG_SW_DATA RRTMG_SW_DATA.DBL SOILPARM.TBL VEGPARM.TBL').split()


class RunnerTests(unittest.TestCase):
    def setUp(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        self.tmp = tempfile.TemporaryDirectory(prefix='linux-runners-', dir=ROOT / 'build')
        self.addCleanup(self.tmp.cleanup)
        self.t = Path(self.tmp.name)
        self.lab = self.t / 'lab'
        self.data = self.lab / 'cases' / 'demo'
        (self.data / 'inputs/v1').mkdir(parents=True)
        self.bin = self.t / 'bin'
        self.bin.mkdir()
        stub = self.bin / 'stub.py'
        stub.write_text('#!' + sys.executable + '\n' + STUB)
        stub.chmod(0o755)
        for name in ('date', 'sleep', 'uptime', 'taskset', 'time', 'nvidia-smi', 'nvidia-cuda-mps-control',
                     'mps-pids', 'mps-alive', 'sha256sum', 'md5sum'):
            (self.bin / name).symlink_to(stub)
        (self.bin / 'mpirun').write_text('#!' + sys.executable + '\n' + MPIRUN)
        (self.bin / 'mpirun').chmod(0o755)
        self.specs = self.t / 'specs'
        (self.specs / 'demo/init').mkdir(parents=True)
        (self.specs / 'demo/mpas_runs').mkdir()
        (self.specs / 'demo/init/base.txt').write_text('build MPAS-gnu\nranks 2\n')
        (self.specs / 'demo/init/variant.txt').write_text('inputs v1\nexpect result.nc\n')
        (self.specs / 'demo/case.txt').write_text('ranks 2\ninputs v1\nnml nhyd_model config_dt 120.0\n')
        self.run_spec = self.specs / 'demo/mpas_runs/test.txt'
        self.run_spec.write_text('build MPAS-nv\ncard on\nexpect result.nc\n')
        (self.data / 'inputs/result.nc').write_text('base untouched\n')
        (self.data / 'inputs/mesh.nc').write_text('base mesh\n')
        (self.data / 'inputs/top_only.nc').write_text('top only\n')
        (self.data / 'inputs/v1/mesh.nc').write_text('variant mesh\n')
        self.met = self.t / 'met'
        self.met.mkdir()
        (self.met / 'GFS:2026-01-01_00:00').write_text('met\n')
        self.build = self.t / 'lean-build'
        self.build.mkdir()
        lean = self.build / 'lean_mpas'
        lean.write_text(f'#!/bin/sh\nSTUB_KIND=lean_mpas exec {stub} "$@"\n')
        lean.chmod(0o755)
        (self.build / 'build.json').write_text('{"config":{"target":"cpu"}}')
        (self.t / 'base-result').write_text('do not follow\n')
        for name, exe in (('MPAS-gnu', 'init_atmosphere_model'), ('MPAS-nv', 'atmosphere_model')):
            b = self.lab / 'mpas' / name
            b.mkdir(parents=True)
            (b / exe).symlink_to(stub)
            (b / 'env.sh').write_text(f'export BUILD_ENV={name}\n')
            wrf = b / 'src/core_atmosphere/physics/physics_wrf/files'
            wrf.mkdir(parents=True)
            for table in TABLES:
                (wrf / table).write_text('table\n')
            noah = b / 'src/core_atmosphere/physics/physics_noahmp/parameters'
            noah.mkdir(parents=True)
            (noah / 'NoahmpTable.TBL').write_text('noah\n')
        tables = self.lab / 'cases/thompson_be'
        tables.mkdir()
        sums = []
        for n in range(4):
            name = f'MP_THOMPSON_{n}_DATA.DBL'
            (tables / name).write_bytes(b'big endian fixture')
            sums.append(hashlib.sha256((tables / name).read_bytes()).hexdigest() + '  ' + name)
        (self.specs / 'thompson_be.sha256').write_text('\n'.join(sums) + '\n')
        self.state = {'check_links': True}
        envfile = self.t / 'env.sh'
        envfile.write_text('export LEAN_PY=' + sys.executable + '\n')
        self.env = dict(os.environ, PATH=str(self.bin) + ':' + os.environ['PATH'], LEAN_LAB=str(self.lab),
                        STUB_ROOT=str(self.t), CASE_ENV=str(envfile), CASE_SPECS=str(self.specs),
                        CASE_TIME=str(self.bin / 'time'), CASE_CORES='2-3',
                        CASE_MPS_PIDS=str(self.bin / 'mps-pids'), CASE_MPS_ALIVE=str(self.bin / 'mps-alive'),
                        OPENBLAS_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='2')
        for name in ('LEAN_THREADS', 'LEAN_CARD', 'MPIRUN', 'MPIRUN_ARGS'):
            self.env.pop(name, None)

    def events(self, name=None):
        p = self.t / 'events.jsonl'
        found = [json.loads(line) for line in p.read_text().splitlines()] if p.exists() else []
        return [e for e in found if e['event'] == name] if name else found

    def names(self):
        return [e['event'] for e in self.events()]

    def run_script(self, script, *args, rc=0, check_verdict=True):
        (self.t / 'state.json').write_text(json.dumps(self.state))
        p = subprocess.Popen(['bash', str(ROOT / 'linux/cases' / script), *map(str, args)],
                             env=self.env, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True, start_new_session=True)
        try: out, err = p.communicate(timeout=90)   # the slowest case polls its stand-in 120 times
        except subprocess.TimeoutExpired:
            os.killpg(p.pid, signal.SIGKILL)
            out, err = p.communicate()
            self.fail('script hung: ' + out + err)
        self.assertEqual(p.returncode, rc, out + err)
        self.stderr = err
        if check_verdict: self.assertIn(('FAIL' if rc else 'OK'), out, out + err)
        return out

    def init(self, **kw): return self.run_script('init.sh', 'demo', 'variant', self.met, **kw)
    def mpas(self, **kw): return self.run_script('mpas_run.sh', 'demo', 'test', **kw)
    def lean(self, **kw): return self.run_script('lean_run.sh', 'demo', 'test', self.build, **kw)

    def runs_log(self):
        return (self.data / 'runs.log').read_text()

    def assert_no_model_or_quit(self):
        self.assertFalse({'model', 'mps-quit'} & set(self.names()))

    def run_dir_view(self, directory):
        """The inputs of a run directory: the content of a copied file, the target of a link; outputs left out."""
        view = {}
        for p in sorted(directory.iterdir()):
            if p.name in ('atmosphere_model', 'mpas.out', 'lean.out', 'result.nc') or p.name.startswith('log.'):
                continue
            view[p.name] = ('link', os.readlink(p)) if p.is_symlink() else ('file', p.read_bytes())
        return view

    # init.sh

    def test_init_publishes_after_clean_verdict(self):
        base = self.data / 'inputs/result.nc'
        before = hashlib.sha256(base.read_bytes()).hexdigest()
        out = self.init()
        dest = self.data / 'inputs/v1/result.nc'
        self.assertTrue(dest.is_file() and not dest.is_symlink())
        self.assertEqual(dest.read_text(), 'new result\n')
        self.assertEqual(hashlib.sha256(base.read_bytes()).hexdigest(), before)
        self.assertTrue((self.data / 'logs/init_variant.log').is_file())
        model = self.events('model')[0]
        self.assertEqual(model['env']['BUILD_ENV'], 'MPAS-gnu')
        self.assertEqual(self.events('taskset')[0]['cores'], '2-3')
        self.assertFalse(any(n.startswith('mps-') for n in self.names()))
        self.assertIn('INIT OK demo/variant: result.nc, wall 12s, maxRSS 1.00 GB', out)

    def test_init_existing_destination_refused(self):
        (self.data / 'inputs/v1/result.nc').write_text('old')
        self.init(rc=2); self.assert_no_model_or_quit()

    def test_init_dangling_destination_refused(self):
        (self.data / 'inputs/v1/result.nc').symlink_to('missing')
        self.init(rc=2); self.assert_no_model_or_quit()

    def test_init_failed_verdict_keeps_outputs_in_work(self):
        self.state['mode'] = 'errors'
        self.init(rc=1)
        self.assertFalse((self.data / 'inputs/v1/result.nc').exists())
        self.assertTrue((self.data / 'work/variant/result.nc').is_file())

    def test_init_symlink_output_never_published(self):
        self.state['mode'] = 'symlink'
        self.init(rc=1)
        self.assertFalse((self.data / 'inputs/v1/result.nc').exists())
        self.assertEqual((self.t / 'base-result').read_text(), 'do not follow\n')

    def test_init_existing_work_refused(self):
        (self.data / 'work/variant').mkdir(parents=True)
        self.init(rc=2); self.assert_no_model_or_quit()

    # mpas_run.sh

    def test_mpas_card_run_mps_order_and_record(self):
        out = self.mpas()
        names = self.names()
        self.assertLess(names.index('mps-probe'), names.index('mps-start'))
        self.assertLess(names.index('mps-start'), names.index('model'))
        self.assertLess(names.index('model-stop'), names.index('mps-quit'))
        mpi = json.loads((self.t / 'mpi.jsonl').read_text().splitlines()[0])
        self.assertEqual(mpi, ['-np', '2', '-x', 'CUDA_MPS_PIPE_DIRECTORY', '-x', 'CUDA_MPS_LOG_DIRECTORY', './atmosphere_model'])
        model = self.events('model')[0]
        self.assertEqual(model['env']['CUDA_MPS_PIPE_DIRECTORY'], str(self.lab / 'mps/pipe'))
        self.assertEqual(model['env']['BUILD_ENV'], 'MPAS-nv')
        self.assertEqual(model['env']['OMP_NUM_THREADS'], '1')
        run = self.data / 'runs/test'
        self.assertEqual(len(list(run.glob('stream_list.atmosphere.*'))), 4)
        self.assertEqual(len(list(run.glob('MP_THOMPSON_*.DBL'))), 4)
        self.assertTrue((run / 'NoahmpTable.TBL').is_symlink())
        log = self.runs_log()
        for word in ('MPAS OK demo/test rc=0', 'build=MPAS-nv', 'md5=', 'ranks=2', 'cores=2-3', 'wall=12s',
                     'steps=90', 's_per_step=0.2845', 'maxRSS=1.00GB', 'card_peak=100MiB', 'start: 08:00 up'):
            self.assertIn(word, log)
        self.assertIn('MPAS OK demo/test: OK, rc=0, wall 12 s, 90 steps at 0.2845 s, maxRSS 1.00 GB, card peak 100 MiB', out)

    def test_mpas_skips_expected_file_in_variant_inputs(self):
        variant = self.data / 'inputs/v1/result.nc'
        variant.write_text('variant preserved\n')
        self.mpas()
        self.assertEqual(variant.read_text(), 'variant preserved\n')
        output = self.data / 'runs/test/result.nc'
        self.assertFalse(output.is_symlink())
        self.assertEqual(output.read_text(), 'new result\n')

    def test_mpas_exit_zero_with_errors_is_fail_and_stops_mps(self):
        self.state['mode'] = 'errors'
        self.assertIn('error summaries', self.mpas(rc=1))
        self.assertIn('model', self.names())
        self.assertIn('mps-quit', self.names())
        self.assertIn('MPAS FAIL demo/test rc=0', self.runs_log())

    def test_mpas_second_use_refused(self):
        self.mpas(); self.mpas(rc=2)
        self.assertEqual(len(self.events('model')), 1)
        self.assertEqual(self.runs_log().count('\n'), 1)

    def test_mps_start_failure_never_launches_or_quits(self):
        self.state['mps_fail'] = True
        self.assertIn('MPS start failed', self.mpas(rc=1)); self.assert_no_model_or_quit()
        self.assertEqual(len(self.events('mps-start')), 1)
        self.assertIn('MPAS FAIL demo/test rc=1', self.runs_log())

    def test_existing_mps_never_claimed(self):
        self.state['mps_existing'] = True
        self.assertIn('already answers', self.mpas(rc=1)); self.assert_no_model_or_quit()
        self.assertNotIn('mps-start', self.names())

    def test_mps_term_during_start_cleans_up_and_records_fail(self):
        self.state['mps_start_signal'] = int(signal.SIGTERM)
        out = self.mpas(rc=143, check_verdict=False)
        self.assertFalse((self.t / 'mps-owned').exists(), 'daemon left alive after TERM during start')
        names = self.names()
        self.assertNotIn('model', names)
        self.assertEqual(names.count('mps-quit'), 1)
        self.assertLess(names.index('mps-start-return'), names.index('mps-quit'))
        self.assertIn('MPAS FAIL demo/test: stopped, exit 143', out)
        self.assertIn('MPAS FAIL demo/test rc=143', self.runs_log())

    def test_mps_failed_quit_is_retried_on_exit(self):
        self.state.update(quit_fail_once=True, mps_probe_fail=True)
        self.assertIn('MPS shutdown failed', self.mpas(rc=1))
        events = self.events(); names = self.names()
        quits = [i for i, n in enumerate(names) if n == 'mps-quit']
        self.assertEqual(len(quits), 2, 'the exit handler must retry the failed shutdown')
        self.assertIn('MPS quit failed', self.stderr)
        self.assertLess(quits[0], names.index('record-run'))
        self.assertLess(names.index('record-run'), quits[1])
        polls = [e for e in events[quits[0]:quits[1]] if e['event'] == 'mps-alive']
        self.assertTrue(polls and all(e['alive'] for e in polls), 'a failed control probe is not a daemon exit')
        self.assertEqual(names[quits[0]:quits[1]].count('wait'), 120)
        self.assertFalse((self.t / 'mps-owned').exists())
        self.assertIn('MPS shutdown failed', self.runs_log())

    def test_mps_successful_quit_confirms_pid_exit(self):
        self.mpas()
        events = self.events(); names = self.names()
        polls = [i for i, e in enumerate(events) if e['event'] == 'mps-alive']
        self.assertTrue(polls, 'a successful quit still needs the PID to exit')
        self.assertFalse(events[polls[-1]]['alive'])
        self.assertLess(names.index('mps-quit'), polls[-1])
        self.assertLess(polls[-1], names.index('record-run'))
        self.assertEqual(names.count('mps-quit'), 1)
        self.assertFalse((self.t / 'mps-owned').exists())

    def test_bad_tables_prevent_launch(self):
        self.state['bad_tables'] = True
        self.assertIn('Thompson table checksum failed', self.mpas(rc=1)); self.assert_no_model_or_quit()
        self.assertIn('MPAS FAIL demo/test', self.runs_log())

    def test_cpu_mpas_never_starts_mps_or_samples_the_card(self):
        self.run_spec.write_text(self.run_spec.read_text().replace('card on', 'card off'))
        self.mpas()
        self.assertFalse(any(n.startswith('mps-') or n == 'card-sample' for n in self.names()))
        self.assertIn('card_peak=offMiB', self.runs_log())

    def test_mpirun_and_its_arguments_from_the_build_env(self):
        self.run_spec.write_text('build MPAS-nv\nexpect result.nc\n')
        env = self.lab / 'mpas/MPAS-nv/env.sh'
        env.write_text(env.read_text() + f"MPIRUN={self.bin / 'mpirun'}\nMPIRUN_ARGS='--bind-to none'\n")
        self.mpas()
        mpi = json.loads((self.t / 'mpi.jsonl').read_text().splitlines()[0])
        self.assertEqual(mpi, ['-np', '2', '--bind-to', 'none', './atmosphere_model'])

    def test_no_pinning_without_case_cores(self):
        del self.env['CASE_CORES']
        self.mpas()
        self.assertNotIn('taskset', self.names())
        self.assertIn('cores=all', self.runs_log())

    def test_missing_lab_refused(self):
        self.env['LEAN_LAB'] = str(self.t / 'nowhere')
        self.assertIn('set LEAN_LAB', self.mpas(rc=2)); self.assert_no_model_or_quit()

    # lean_run.sh

    def test_lean_gets_the_same_run_directory_and_cores_as_mpas(self):
        self.run_spec.write_text('build MPAS-nv\nexpect result.nc\n')
        self.mpas()
        self.env.update(LEAN_PRODUCTS='inherited.prod', LEAN_FILM='inherited.film')
        out = self.lean()
        mpas_dir, lean_dir = self.data / 'runs/test', self.data / 'lean_runs/test'
        self.assertEqual(self.run_dir_view(mpas_dir), self.run_dir_view(lean_dir))
        self.assertEqual(os.readlink(lean_dir / 'atmosphere_model'), str(self.build / 'lean_mpas'))
        self.assertEqual([e['cores'] for e in self.events('taskset')], ['2-3', '2-3'])
        model = self.events('model')[1]
        self.assertEqual(model['kind'], 'lean_mpas')
        self.assertEqual(Path(model['cwd']).resolve(), lean_dir.resolve())
        self.assertEqual(model['env']['OMP_NUM_THREADS'], '1')
        self.assertEqual(model['env']['OMP_STACKSIZE'], '256M')
        self.assertIsNone(model['env']['LEAN_PRODUCTS'])
        self.assertIsNone(model['env']['LEAN_FILM'])
        self.assertIsNone(model['env']['BUILD_ENV'], "lean must not read the MPAS build's env.sh")
        self.assertEqual(len((self.t / 'mpi.jsonl').read_text().splitlines()), 1, 'lean runs without mpirun')
        lines = self.runs_log().splitlines()
        self.assertIn('MPAS OK demo/test rc=0', lines[0])
        for word in ('LEAN OK demo/test rc=0', 'build=lean-build', 'threads=1', 'cores=2-3', 'steps=90',
                     's_per_step=0.2845', 'maxRSS=1.00GB', 'card_peak=offMiB'):
            self.assertIn(word, lines[1])
        self.assertIn('LEAN OK demo/test: OK, rc=0, wall 12 s, 90 steps at 0.2845 s', out)

    def test_lean_card_build_threads_and_card_record(self):
        (self.build / 'build.json').write_text('{"config":{"target":"gpu"}}')
        self.env.update(LEAN_THREADS='2', LEAN_CARD='none')
        self.lean()
        model = self.events('model')[0]
        self.assertEqual(model['env']['OMP_NUM_THREADS'], '2')
        self.assertEqual(model['env']['LEAN_CARD'], 'none')
        self.assertIn('card-sample', self.names())
        self.assertFalse(any(n.startswith('mps-') for n in self.names()))
        for word in ('threads=2 card=none', 'card_peak=100MiB'):
            self.assertIn(word, self.runs_log())

    def test_lean_existing_run_directory_refused(self):
        (self.data / 'lean_runs/test').mkdir(parents=True)
        self.lean(rc=2); self.assert_no_model_or_quit()

    def test_lean_bad_build_folder_refused(self):
        (self.build / 'build.json').unlink()
        self.assertIn('lean_mpas and build.json', self.lean(rc=2)); self.assert_no_model_or_quit()

    def test_lean_failures_are_recorded(self):
        for mode in ('nan', 'symlink', 'empty', 'missing', 'rc7', 'errors'):
            with self.subTest(mode=mode):
                self.state['mode'] = mode
                self.lean(rc=1)
                self.assertIn('LEAN FAIL demo/test', self.runs_log().splitlines()[-1])
                for p in sorted((self.data / 'lean_runs/test').iterdir()): p.unlink()
                (self.data / 'lean_runs/test').rmdir()


unittest.main(verbosity=2)
PY
