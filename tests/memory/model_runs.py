#!/usr/bin/env python3
"""The memory check in the program: runs of lean_mpas in fresh greece_60km run directories.

    python3 tests/memory/model_runs.py --exe EXE --test-exe TEST_EXE [--base-exe BASE] [--fc FC]

EXE: the strict build; TEST_EXE: the same objects with lean_memory's LEAN_MEMORY_AVAILABLE_MIB hook (tests/memory/run.sh);
BASE: another build of lean_mpas (for example of the commit before a change): its history and diag files must equal
EXE's and its log must differ only by the memory report (left out, those two cases are SKIPPED and counted).
LEAN_PY: a Python with netCDF4 (default $PYTHON, else python3). Each case prints ok/FAIL/SKIP; exit 1 on a FAIL.
"""
import argparse
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PY = os.environ.get('LEAN_PY', os.environ.get('PYTHON', 'python3'))
GREECE = ROOT / 'data/cases/greece_60km/inputs'
REPORT = [r' ?Memory check: nCells 2337, nEdges 7183, nVertices 4847, nVertLevels 45, 0 glacier columns',
      r' *host: \d+ MiB needed \(\d+ MiB and \d+ MiB headroom\), (\d+ MiB available \(.+\)|available unknown)']
TOO_BIG = (r"The mesh of greece_60km.init.nc does not fit in host memory: lean_mpas needs \d+ MiB \(with \d+ MiB headroom\) "
      r"and 64 MiB are available \(LEAN_MEMORY_AVAILABLE_MIB\)\. It stops before allocating the mesh; ./lean doctor "
      r"--init greece_60km.init.nc --namelist \S+ --streams \S+ --build build/\S+ lists what the sum is "
      r"made of\.")
STOP = 'lean_mpas stops: 1 memory problem(s) listed above'
BOOT = "Bootstrapping framework with mesh fields from input file"
SHORT = ["nml nhyd_model config_run_duration '0_00:12:00'", 'stream output output_interval 0:06:00',
         'stream diagnostics output_interval 0:06:00']


class Runs:
    def __init__(self, args, work: Path):
        self.args, self.work, self.fails, self.skips = args, work, 0, 0

    def report(self, ok, name, detail=''):
        self.fails += not ok
        print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok or not detail else f': {detail}'))

    def skip(self, name, why):
        self.skips += 1
        print(f'SKIP  {name}: {why}')

    def run_dir(self, name, specs=(), init=None):
        d = self.work / name
        spec = self.work / f'{name}.spec'
        spec.write_text('\n'.join(specs) + '\n')
        subprocess.run(['sh', '-c', f'root="{ROOT}"; . tests/inputs/run_dir.sh; '
                        f'make_run_dir "{d}" "{GREECE}" cases/greece_60km/case.txt "{spec}"'], cwd=ROOT, check=True)
        if init is not None:
            (d / 'greece_60km.init.nc').unlink()
            shutil.copy(init, d / 'greece_60km.init.nc')
        return d

    def launch(self, exe, d, steps=0, env=None, timed=False):
        """atmosphere_model's form in d, `steps` steps of 120 s long. The result's stdout is the text of
        log.atmosphere.0000.out and .err (moved to the work folder, so d keeps its file set), then the program's own."""
        nml = d / 'namelist.atmosphere'
        s = steps * 120
        nml.write_text(re.sub(r"(?m)^(\s*config_run_duration\s*=\s*).*$",
                              lambda m: m.group(1) + f"'{s // 86400}_{s // 3600 % 24:02d}:{s // 60 % 60:02d}:{s % 60:02d}'",
                              nml.read_text()))
        command = [str(exe)]
        if timed:
            command = ['/usr/bin/time', '-l' if platform.system() == 'Darwin' else '-v'] + command
        done = subprocess.run(command, cwd=d, capture_output=True, text=True, env={**os.environ, **(env or {})})
        logs = ''
        for name in ('log.atmosphere.0000.out', 'log.atmosphere.0000.err'):
            if (d / name).is_file():
                logs += (d / name).read_text()
                (d / name).replace(self.work / f'{d.name}.{name}')
        done.stdout = logs + done.stdout
        return done

    def lines(self, text):
        return [line.rstrip('\n') for line in text.splitlines()]

    # ---- cases ----
    def launcher(self):
        d = self.run_dir('launcher')
        done = self.launch(self.args.exe, d)
        out = self.lines(done.stdout)
        at = next((i for i, line in enumerate(out) if line.strip().startswith('Memory check:')), None)
        ok = (done.returncode == 0 and at is not None and re.fullmatch(REPORT[0], out[at]) is not None
              and re.fullmatch(REPORT[1], out[at + 1]) is not None and out[at + 2].strip().startswith(BOOT))
        self.report(ok, 'launcher: the memory report on stdout right before the bootstrapping line',
                    f'rc {done.returncode}, {out[at:at + 3] if at is not None else done.stdout[-800:]}')
        d = self.run_dir('launcher_refused')
        before = set(os.listdir(d))
        done = self.launch(self.args.test_exe, d, env={'LEAN_MEMORY_AVAILABLE_MIB': '64,'})
        out = self.lines(done.stdout)
        ok = (done.returncode == 1 and any(re.fullmatch(REPORT[0], l) for l in out) and any(re.fullmatch('ERROR: ' + TOO_BIG, l) for l in out)
              and 'CRITICAL ERROR: ' + STOP in out and not any(BOOT in l for l in out) and set(os.listdir(d)) == before)
        self.report(ok, 'launcher refused (64 MiB): the report, the refusal with both numbers, the stop, exit 1, no bootstrapping, no new file',
                    f'rc {done.returncode}, new {set(os.listdir(d)) - before}, {done.stdout[-1500:]}')
        d = self.run_dir('launcher_fits')
        done = self.launch(self.args.test_exe, d, steps=2, env={'LEAN_MEMORY_AVAILABLE_MIB': '100000,'})
        self.report(done.returncode == 0 and 'MiB available (LEAN_MEMORY_AVAILABLE_MIB)' in done.stdout,
                    'launcher with 100000 MiB: runs its 2 steps', f'rc {done.returncode} {done.stderr[-500:]}')

    def drop_in_refused(self):
        d = self.run_dir('dropin_refused', SHORT)
        before = set(os.listdir(d))
        done = subprocess.run([str(self.args.test_exe)], cwd=d, capture_output=True, text=True,
                              env={**os.environ, 'LEAN_MEMORY_AVAILABLE_MIB': '64,'})
        log = (d / 'log.atmosphere.0000.out').read_text() if (d / 'log.atmosphere.0000.out').is_file() else ''
        err = (d / 'log.atmosphere.0000.err').read_text() if (d / 'log.atmosphere.0000.err').is_file() else ''
        new = set(os.listdir(d)) - before - {'log.atmosphere.0000.out', 'log.atmosphere.0000.err'}
        ok = (done.returncode != 0 and re.search(REPORT[0], log) is not None and re.search(TOO_BIG.replace('^', ''), log + err)
              is not None and STOP in log + err and BOOT not in log and not new)
        self.report(ok, 'drop-in form refused (64 MiB): the report, the refusal and the stop in the log, rc != 0, no bootstrapping, no output',
                    f'rc {done.returncode}, new {new}, log tail {log[-1200:]} err {err[-400:]}')

    def preview_dirs(self):
        nested = ['stream output filename_template nested/deeper/history.$Y-$M-$D_$h.$m.$s.nc']
        d = self.run_dir('dirs_fit', nested)
        done = self.launch(self.args.test_exe, d, steps=0, env={'LEAN_MEMORY_AVAILABLE_MIB': '100000,'})
        created = [l for l in self.lines(done.stdout) if '*** created directory' in l]
        self.report(len(created) == 1 and 'nested/deeper' in created[0] and (d / 'nested/deeper').is_dir(),
                    'preview_dirs fitting: one "*** created directory" line (the real parse), the directory exists',
                    f'{created} rc {done.returncode}')
        d = self.run_dir('dirs_refused', nested)
        done = self.launch(self.args.test_exe, d, steps=0, env={'LEAN_MEMORY_AVAILABLE_MIB': '64,'})
        self.report('*** created directory' not in done.stdout and not (d / 'nested').exists() and done.returncode == 1,
                    'preview_dirs refused: no "*** created directory" line, no directory', f'rc {done.returncode}')

    def dims_only(self):
        out = self.work / 'dims_only.nc'
        made = subprocess.run([PY, str(ROOT / 'tools/cases/make_dims_only.py'), 'greece', str(out), '--ncells', '2000000'],
                              capture_output=True, text=True)
        if made.returncode:
            return self.skip('dims_only', f'make_dims_only.py: {made.stderr.strip()[-200:]}')
        d = self.run_dir('dims_only', init=out)
        done = self.launch(self.args.exe, d, timed=True)
        rss = re.search(r'(\d+)\s+maximum resident set size', done.stderr) or re.search(
            r'Maximum resident set size \(kbytes\): (\d+)', done.stderr)
        mib = int(rss.group(1)) / 2 ** 20 if rss and platform.system() == 'Darwin' else int(rss.group(1)) / 1024 if rss else 1e9
        refused = re.search(r'The mesh of greece_60km.init.nc does not fit in host memory: lean_mpas needs (\d+) MiB',
                            done.stdout)
        self.report(done.returncode == 1 and refused is not None and STOP in done.stdout and mib < 200,
                    f'dims_only 2,000,000 cells: refused ({refused.group(1) if refused else "?"} MiB needed), the stop, maxRSS '
                    f'{mib:.0f} MiB < 200', done.stdout[-800:])

    def missing_field(self):
        out = self.work / 'no_ivgtyp.nc'
        script = ('import netCDF4,sys\nsrc=netCDF4.Dataset(sys.argv[1]);dst=netCDF4.Dataset(sys.argv[2],"w",format=src.data_model)\n'
                  'dst.setncatts({k:src.getncattr(k) for k in src.ncattrs()})\n'
                  'for n,d in src.dimensions.items(): dst.createDimension(n,None if d.isunlimited() else len(d))\n'
                  'for n,v in src.variables.items():\n'
                  ' if n=="ivgtyp": continue\n'
                  ' w=dst.createVariable(n,v.datatype,v.dimensions);w.setncatts({k:v.getncattr(k) for k in v.ncattrs()});w[...]=v[...]\n'
                  'dst.close()\n')
        made = subprocess.run([PY, '-c', script, str(GREECE / 'greece_60km.init.nc'), str(out)], capture_output=True, text=True)
        if made.returncode:
            return self.skip('missing_field', made.stderr.strip()[-200:])
        d = self.run_dir('missing_field', init=out)
        done = self.launch(self.args.exe, d)
        out_lines = self.lines(done.stdout)
        self.report(done.returncode == 1 and 'ERROR: greece_60km.init.nc: no variable ivgtyp (input stream)' in out_lines
                    and 'CRITICAL ERROR: lean_mpas stops: 1 input problem(s) listed above' in out_lines
                    and not any(BOOT in l for l in out_lines) and not any(l.startswith('Memory check') for l in out_lines),
                    'missing_field: lean\'s input message and its stop before the bootstrapping line', done.stdout[-800:])

    def identity(self):
        if not self.args.base_exe:
            self.skip('identity', 'no --base-exe')
            return self.skip('log_bytes', 'no --base-exe')
        dirs = {}
        for name, exe in (('base', self.args.base_exe), ('head', self.args.exe)):
            d = self.run_dir(f'identity_{name}', SHORT)
            done = subprocess.run([str(exe)], cwd=d, capture_output=True, text=True)
            if done.returncode:
                return self.report(False, f'identity: the {name} run', done.stderr[-500:])
            dirs[name] = d
        script = ('import netCDF4,sys,numpy as np,pathlib\n'
                  'a,b=map(pathlib.Path,sys.argv[1:3]);bad=[]\n'
                  'names=sorted(p.name for p in a.glob("*.nc") if not p.is_symlink())\n'
                  'if names!=sorted(p.name for p in b.glob("*.nc") if not p.is_symlink()): bad.append("file sets differ")\n'
                  'for n in names:\n'
                  ' x=netCDF4.Dataset(a/n);y=netCDF4.Dataset(b/n);x.set_auto_mask(False);y.set_auto_mask(False)\n'
                  ' for v in x.variables:\n'
                  '  if x[v][...].tobytes()!=y[v][...].tobytes(): bad.append(n+":"+v)\n'
                  ' ga={k:x.getncattr(k) for k in x.ncattrs() if k not in ("lean_version","git_version","file_id")}\n'
                  ' gb={k:y.getncattr(k) for k in y.ncattrs() if k not in ("lean_version","git_version","file_id")}\n'
                  ' if str(ga)!=str(gb): bad.append(n+": attributes")\n'
                  'print(len(names),";".join(bad))\n')
        done = subprocess.run([PY, '-c', script, str(dirs['base']), str(dirs['head'])], capture_output=True, text=True)
        words = done.stdout.split(None, 1)
        ok = done.returncode == 0 and len(words) == 1 and int(words[0]) >= 2
        self.report(ok, f'identity: history and diag files of base and head equal ({words[0] if words else "?"} files)',
                    done.stdout + done.stderr[-400:])
        # the build identity block, timestamps and every decimal number (the timer block's times and shares)
        mask = re.compile(r"(Output from 'git describe --dirty': .*|Build target: .*|Compiler flags: .*|"
                          r"\d{4}/\d\d/\d\d \d\d:\d\d:\d\d|\d+\.\d+(E[+-]\d+)?)")
        base = [mask.sub('#', l.strip()) for l in (dirs['base'] / 'log.atmosphere.0000.out').read_text().splitlines()]
        head = [mask.sub('#', l.strip()) for l in (dirs['head'] / 'log.atmosphere.0000.out').read_text().splitlines()]
        at = next((i for i, l in enumerate(head) if l.startswith('Memory check:')), None)
        ok = at is not None and re.fullmatch(REPORT[0], head[at]) and re.fullmatch(REPORT[1], head[at + 1])
        if ok:
            head = head[:at] + head[at + 2:]
            differ = [(a, b) for a, b in zip(base, head) if a != b]
            counts = [(a, b) for a, b in differ if re.fullmatch(r'Output messages =\s+\d+', a)]
            ok = len(base) == len(head) and len(differ) == len(counts) == 1 and all(
                int(b.split('=')[1]) - int(a.split('=')[1]) == 2 for a, b in counts)
            detail = f'{len(differ)} differing lines: {differ[:4]}'
        else:
            detail = 'no memory report in the head log'
        self.report(bool(ok), 'log_bytes: the logs equal after masking, but the memory report (2 lines) and the closing count (+2)', detail)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    parser.add_argument('--exe', type=Path, required=True)
    parser.add_argument('--test-exe', type=Path, required=True)
    parser.add_argument('--base-exe', type=Path)
    parser.add_argument('--fc', default='gfortran')
    parser.add_argument('--keep', action='store_true')
    args = parser.parse_args(argv)
    work = Path(tempfile.mkdtemp(prefix='memory-runs-', dir=ROOT / 'build'))
    runs = Runs(args, work)
    for case in (runs.launcher, runs.drop_in_refused, runs.preview_dirs, runs.dims_only, runs.missing_field,
                 runs.identity):
        case()
    print(f'model runs: {runs.fails} failed, {runs.skips} skipped')
    if not runs.fails and not args.keep:
        shutil.rmtree(work)
    return 1 if runs.fails else 0


if __name__ == '__main__':
    sys.exit(main())
