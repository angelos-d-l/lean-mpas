#!/usr/bin/env python3
"""Checks of the program's startup and log code in src/run/: the command line and startup order (lean_drop_in,
cli_main, fake_forecast), the log and message accounting (lean_log, log_main), the two refusals before a run writes
(lean_guard, guard_main) and the per-step extrema (lean_summary) against MPAS's own summarize_timestep and mpas_log
text, compiled with the model's host flags."""
import argparse
import fcntl
import hashlib
import json
from itertools import zip_longest
import os
from pathlib import Path
import re
import selectors
import shutil
import signal
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from tools.lean.build_info import write_build_info
from tools.lean.config import BuildConfig, unit_flags
from tests.namelist.run import validation_input, dynamics_warnings
from tools import gen_namelist

OUT = 'log.atmosphere.0000.out'
ERR = 'log.atmosphere.0000.err'
MPAS = ROOT / 'reference/mpas_src/src'
DATE = r'\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}'
LAUNCHER_VARIABLES = (
    'OMPI_COMM_WORLD_SIZE', 'PMI_SIZE', 'MV2_COMM_WORLD_SIZE', 'SLURM_STEP_NUM_TASKS',
    'OMPI_COMM_WORLD_RANK', 'PMI_RANK', 'MV2_COMM_WORLD_RANK', 'SLURM_PROCID', 'PMIX_RANK',
    'SLURM_NTASKS', 'SLURM_JOB_ID',
)
# framework/mpas_timer.F:329-348, verbatim messages, with mpas_log_write's blank prefix.
TIMER_HEADER = (
    '', '', '  Timer information:', '     Globals are computed across all threads and processors', '',
    '  Columns:', '     total time: Global max of accumulated time spent in timer',
    '     calls: Total number of times this timer was started / stopped.',
    '     min: Global min of time spent in a single start / stop',
    '     max: Global max of time spent in a single start / stop',
    '     avg: Global max of average time spent in a single start / stop',
    '     pct_tot: Percent of the timer at level 1',
    '     pct_par: Percent of the parent timer (one level up)',
    '     par_eff: Parallel efficiency, global average total time / global max total time', '', '',
    '    timer_name                                            total       calls        min            max'
    '            avg      pct_tot   pct_par     par_eff',
)
# MPAS 8.4.2 greece_60km strict_12h/log.atmosphere.0000.out:5174,5175,5182,5206.
TIMER_ROWS = (
    TIMER_HEADER[-1],
    '  1 total time                                         132.85538         1      132.85538'
    '      132.85538      132.85538   100.00       0.00       1.00',
    '  2  time integration                                  119.80722       360        0.24797'
    '        1.61162        0.33280    90.18      90.18       1.00',
    '  4    mp_thompson                                      10.96807       360        0.01758'
    '        0.03356        0.03047     8.26      79.57       1.00',
)
LINE_MESSAGES = (
    ' a', 'WARNING: w', ' b', 'WARNING: x', '', ' Begin timestep 2026-11-08_00:00:00',
    '  split dynamics-transport integration 3', '  Timing for integration step: 0.553968 s',
)
CASES = [
    {'name': 'log_lines', 'mode': 'lines'},
    {'name': 'log_error', 'mode': 'error'},
    {'name': 'log_fatal', 'mode': 'fatal', 'status': 1},
    {'name': 'log_list_fatal', 'mode': 'list_fatal', 'status': 1},
    {'name': 'stale_err_removed', 'mode': 'lines'},
    {'name': 'timer_rows', 'mode': 'rows'},
    {'name': 'timer_tree', 'mode': 'timers'},
    {'name': 'timer_running', 'mode': 'running'},
    {'name': 'open_fail', 'mode': 'open'},
    {'name': 'nul_message', 'mode': 'nul'},
]
MPI_REFUSED = (
    ('OMPI_COMM_WORLD_SIZE', '2'), ('PMI_SIZE', '4'), ('MV2_COMM_WORLD_SIZE', '2'),
    ('SLURM_STEP_NUM_TASKS', '8'), ('OMPI_COMM_WORLD_RANK', '1'), ('PMI_RANK', '3'),
    ('MV2_COMM_WORLD_RANK', '1'), ('SLURM_PROCID', '2'), ('PMIX_RANK', '1'),
)
MPI_ALLOWED = (
    ('openmpi_one', {'OMPI_COMM_WORLD_SIZE': '1', 'OMPI_COMM_WORLD_RANK': '0'}),
    ('pmi_one', {'PMI_SIZE': '1', 'PMI_RANK': '0'}),
    ('slurm_batch', {'SLURM_NTASKS': '8', 'SLURM_JOB_ID': '42'}),
    ('slurm_step_one', {'SLURM_STEP_NUM_TASKS': '1', 'SLURM_PROCID': '0'}),
    ('empty', {'PMI_SIZE': ''}),
    ('invalid', {'OMPI_COMM_WORLD_SIZE': 'abc'}),
)
CASES += [dict(case, program='guard_main') for case in [
    {'name': 'guard_clean', 'mode': 'guard'},
    *[{'name': 'mpi_refused_' + variable, 'mode': 'guard', 'status': 2, 'env': {variable: value}}
      for variable, value in MPI_REFUSED],
    *[{'name': 'mpi_allowed_' + label, 'mode': 'guard', 'env': env} for label, env in MPI_ALLOWED],
    {'name': 'lock_held', 'mode': 'guard', 'status': 2},
    {'name': 'lock_after_kill', 'mode': 'guard'},
    {'name': 'lock_classify', 'mode': 'classify'},
    *[{'name': 'mpi_refused_boundary_' + variable, 'mode': 'guard', 'status': 2, 'env': {variable: value}}
      for variable, value in (('PMI_SIZE', '2'), ('SLURM_STEP_NUM_TASKS', '2'),
                              ('PMI_RANK', '1'), ('SLURM_PROCID', '1'))],
    {'name': 'mpi_refused_first', 'mode': 'guard', 'status': 2,
     'env': {'PMI_RANK': '1', 'OMPI_COMM_WORLD_SIZE': '2'}, 'first': 'OMPI_COMM_WORLD_SIZE'},
]]
CASES += [{'name': 'lock_eintr', 'program': 'guard_eintr', 'mode': 'guard'}]
SUMMARY_CASES = ('vel', 'vel_positive', 'vel_log', 'detailed', 'detailed_ties', 'sca_only', 'sca_vel',
                 'sca_detailed', 'none',
                 'nan_w', 'nan_u', 'nan_default', 'nan_all_w')
CASES += [{'name': 'summary_' + name, 'program': 'summary_main', 'mode': name,
           'status': 1 if name in ('nan_w', 'nan_u', 'nan_all_w') else 0} for name in SUMMARY_CASES]
MUTANTS = {'start_from_large': 'vel_positive', 'u_stride_nz': 'detailed_ties'}
CLI_N_ERROR = 'ERROR: The -n argument requires a namelist file argument.'
CLI_S_ERROR = 'ERROR: The -s argument requires a streams file argument.'
CASES += [dict(case, program='cli_main') for case in [
    {'name': 'cli_defaults', 'args': [], 'expected': 'namelist.atmosphere|streams.atmosphere'},
    {'name': 'cli_n', 'args': ['-n', 'nl.custom'], 'expected': 'nl.custom|streams.atmosphere'},
    {'name': 'cli_s_then_n', 'args': ['-s', 'st.x', '-n', 'nl.x'], 'expected': 'nl.x|st.x'},
    {'name': 'cli_repeat', 'args': ['-n', 'a', '-n', 'b'], 'expected': 'b|streams.atmosphere'},
    {'name': 'cli_trailing', 'args': ['-n'], 'expected': 'namelist.atmosphere|streams.atmosphere'},
    {'name': 'cli_missing_n', 'args': ['-n', '-s', 'x'], 'status': 2, 'error': CLI_N_ERROR},
    {'name': 'cli_missing_s', 'args': ['-s', '-n', 'x'], 'status': 2, 'error': CLI_S_ERROR},
    {'name': 'cli_empty', 'args': ['-n', '', '-s', 'x'], 'status': 2, 'error': CLI_N_ERROR},
]]
CASES += [dict(case, program='fake_forecast', args=[]) for case in [
    {'name': 'finish_ok'},
    {'name': 'finish_positive_definite', 'settings': {'config_positive_definite': '.true.'}},
    {'name': 'namelist_missing', 'status': 1},
    {'name': 'validate_stop', 'settings': {'config_horiz_mixing': "'2d_fixed'"}, 'status': 1},
    {'name': 'flavor_mismatch', 'settings': {'config_lean_flavor': "'lean'"}, 'status': 1},
    {'name': 'suite_fatal', 'settings': {'config_physics_suite': "'convection_allowing'"}, 'status': 1},
    {'name': 'lock_unavailable'},
]]
CASES += [dict(case, program='executable') for case in [
    {'name': 'exe_forecast', 'args': []},
    {'name': 'exe_mpi_nothing_written', 'args': [], 'env': {'OMPI_COMM_WORLD_SIZE': '2'}, 'status': 2},
    {'name': 'exe_lock_nothing_written', 'args': [], 'status': 2},
    {'name': 'exe_usage_nothing_written', 'args': ['-n', '-s', 'x'], 'status': 2},
    {'name': 'exe_readonly', 'args': [], 'status': 1},
    # also: no history file in the hour (output_interval none), so the diagnostics notice names the output stream
    {'name': 'exe_names', 'args': ['-n', 'nl.custom', '-s', 'st.custom'],
     'spec': 'stream output output_interval none\n'},
    # MPAS's own defaults for apvm (0.5) and microp_re (false) run, not stop; two steps
    {'name': 'stock_namelist', 'args': [], 'settings': {
        'config_apvm_upwinding': '0.5', 'config_microp_re': '.false.', 'config_run_duration': "'0_00:04:00'"}},
    # with a scheme 'off' the per-step '--- time to' lines follow the schemes, as in MPAS's own log of the same run
    # (data/cases/greece_60km/reference/<run>, not released; absent: SKIP)
    *[{'name': 'time_lines_' + run, 'args': [], 'reference': run + '_3h', 'settings': {
        **settings, 'config_run_duration': "'0_00:04:00'"}} for run, settings in (
        ('off_conv', {'config_convection_scheme': "'off'"}),
        ('off_lw', {'config_radt_lw_scheme': "'off'"}),
        ('off_rad', {'config_radt_lw_scheme': "'off'", 'config_radt_sw_scheme': "'off'"}),
        ('off_all', {'config_physics_suite': "'none'", 'config_lsm_scheme': None}),
    )],
    {'name': 'restart_log'},
]]

DEBUG_CASES = [
    {'name': 'log_fatal_debug', 'mode': 'fatal', 'status': 1},
    {'name': 'log_list_fatal_debug', 'mode': 'list_fatal', 'status': 1},
    {'name': 'exe_forecast_debug', 'program': 'executable', 'args': [], 'settings': {'config_run_duration': "'0_00:04:00'"}},
]
DEBUG_CASES = [dict(case, profile='debug') for case in DEBUG_CASES]


def snapshot(directory):
    result = {'.': directory.stat().st_mtime_ns}
    for path in sorted(directory.rglob('*')):
        if path.is_symlink():
            # the run directory's links to the inputs and tables: the link, not the gigabyte behind it
            result[str(path.relative_to(directory))] = ('link', os.readlink(path))
            continue
        info = path.stat()
        digest = hashlib.sha256(path.read_bytes()).hexdigest() if path.is_file() else None
        result[str(path.relative_to(directory))] = (info.st_size, info.st_mtime_ns, digest)
    return result


def lines(text):
    return [re.sub(DATE, '<date>', line.rstrip()) for line in text.splitlines()]


def equal_lines(actual, expected, label):
    for number, (got, want) in enumerate(zip_longest(actual, expected), 1):
        if got != want:
            raise AssertionError(f'{label}:{number}: expected {want!r}, got {got!r}')


def header(kind='Output'):
    return ['-' * 70, f'Beginning MPAS-atmosphere {kind} Log File for task       0 of       1',
            '    Opened at <date>', '-' * 70, '']


def closing(output, warning=0, error=0, critical=0):
    return ['', ' ' + '-' * 41, ' Total log messages printed:',
            f'    Output messages =         {output:12d}',
            f'    Warning messages =        {warning:12d}',
            f'    Error messages =          {error:12d}',
            f'    Critical error messages = {critical:12d}',
            ' ' + '-' * 41, ' Logging complete.  Closing file at <date>']


def timer_rows(text):
    rows = []
    for line in text.splitlines():
        if not re.match(r'^ \s*\d+ ', line):
            continue
        payload = line[1:]
        # Unfinished timers may have adjacent overflowing min/max fields (MPAS prints asterisks).
        fields = ((0, 2), (3, 48), (48, 63), (63, 73), (73, 88), (88, 103), (103, 118),
                  (119, 127), (130, 138), (141, 149))
        rows.append([payload[start:stop].strip() for start, stop in fields])
    return rows


def check_log(case, directory, done, before):
    name = case['name']
    if name == 'nul_message':
        if '\x00' in done.stdout or '\x00' in done.stderr:
            raise AssertionError('NUL-padded diagnostic emitted NUL bytes')
        equal_lines(lines(done.stdout), ['I/O diagnostic', '', 'plain text', 'before', ' list text'], 'NUL padding')
        if snapshot(directory) != before:
            raise AssertionError('message-only mode wrote files')
        return
    if name == 'open_fail':
        equal_lines(lines(done.stdout), ['T T'], 'stdout')
        if snapshot(directory) != before:
            raise AssertionError('failed log_open changed the run directory')
        return
    if name == 'timer_rows':
        equal_lines(lines(done.stdout), TIMER_ROWS, 'stdout')
        # TIMER_ROWS are lines of an MPAS log; where that log is present, check them against it too.
        if MPAS_GREECE_LOG.is_file():
            oracle = MPAS_GREECE_LOG.read_text().splitlines()
            equal_lines([oracle[i - 1].rstrip() for i in (5174, 5175, 5182, 5206)], TIMER_ROWS, 'MPAS log')
        if snapshot(directory) != before:
            raise AssertionError('format-only mode wrote files')
        return
    output = lines((directory / OUT).read_text())
    if name in ('log_lines', 'stale_err_removed'):
        equal_lines(output, header() + list(LINE_MESSAGES) + list(TIMER_HEADER) + closing(23, 2), OUT)
        if (directory / ERR).exists():
            raise AssertionError('unexpected error log')
    elif name == 'log_error':
        equal_lines(output, header() + [' a', 'ERROR: e'] + list(TIMER_HEADER) + closing(18, error=1), OUT)
        equal_lines(lines((directory / ERR).read_text()), header('Error') + ['ERROR: e'], ERR)
    elif case.get('mode') in ('fatal', 'list_fatal'):
        equal_lines(output, header() + [' a', 'CRITICAL ERROR: c'] + closing(1, critical=1), OUT)
        equal_lines(lines((directory / ERR).read_text()), header('Error') +
                    ['CRITICAL ERROR: c', 'Logging complete.  Closing file at <date>'], ERR)
        if 'lean_mpas stopped: c (see log.atmosphere.0000.err)' not in done.stderr.splitlines():
            raise AssertionError(f'missing fatal diagnostic in stderr: {done.stderr!r}')
    elif name == 'timer_tree':
        rows = timer_rows((directory / OUT).read_text())
        actual = [(int(row[0]), ' '.join(row[1:-8]), int(row[-7])) for row in rows]
        expected = [(1, 'total time', 1), (2, 'initialize', 1), (2, 'time integration', 3), (3, 'physics', 3)]
        if actual != expected:
            raise AssertionError(f'timer tree expected {expected!r}, got {actual!r}')
        if rows[0][-3:-1] != ['100.00', '0.00']:
            raise AssertionError(f'root percentages: {rows[0][-3:-1]}')
        for row in rows:
            low, high, avg = map(float, row[-6:-3])
            if not low <= avg <= high or row[-1] != '1.00':
                raise AssertionError(f'timer range/efficiency: {row}')
    elif name == 'timer_running':
        error = 'ERROR: Timer initialize is still running.'
        if error not in output:
            raise AssertionError('missing running-timer error in output log')
        equal_lines(lines((directory / ERR).read_text()), header('Error') + [error], ERR)
        rows = timer_rows((directory / OUT).read_text())
        row = next((row for row in rows if row[0] == '2' and row[1] == 'initialize'), None)
        if row is None or row[-7] != '1' or row[-4] != '0.00000':
            raise AssertionError(f'running timer row: {row}')


def killed_lock(directory, env):
    code = ('import fcntl, os, sys, time\n'
            'fd = os.open(sys.argv[1], os.O_RDONLY)\n'
            'fcntl.flock(fd, fcntl.LOCK_EX)\n'
            'print("locked", flush=True)\n'
            'time.sleep(60)\n')
    child = subprocess.Popen([sys.executable, '-u', '-c', code, str(directory)],
                             env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    try:
        with selectors.DefaultSelector() as ready:
            ready.register(child.stdout, selectors.EVENT_READ)
            if not ready.select(10) or child.stdout.readline() != 'locked\n':
                raise AssertionError('child did not announce its directory lock within 10 seconds')
        if child.poll() is not None:
            raise AssertionError('lock child ended before SIGKILL')
        child.kill()
        if child.wait(timeout=10) != -signal.SIGKILL:
            raise AssertionError('lock child was not killed by SIGKILL')
    finally:
        if child.poll() is None:
            child.kill()
            child.wait(timeout=10)
        child.stdout.close()
        child.stderr.close()


def check_guard(case, directory, done, before):
    name = case['name']
    if name == 'lock_classify':
        equal_lines(lines(done.stdout), ['0 1 2 2 2 2 2 2 2'], 'stdout')
        if snapshot(directory) != before:
            raise AssertionError('classifying errno wrote files')
    elif name.startswith('mpi_refused_') or name == 'lock_held':
        if name == 'lock_held':
            text = ('lean_mpas: another run holds this run directory (its lock is taken). '
                    'One run per directory. Nothing was written.')
        else:
            variable = case.get('first', next(iter(case['env'])))
            value = case['env'][variable]
            text = (f'lean_mpas: an MPI launcher started it with more than one task ({variable}={value}). '
                    'lean_mpas runs as one process: start it without mpirun, mpiexec or srun, or with one task. '
                    'Nothing was written.')
        if text not in done.stderr.splitlines():
            raise AssertionError(f'stderr missing {text!r}: {done.stderr!r}')
        if snapshot(directory) != before:
            raise AssertionError('refused guard changed the run directory')
    else:
        if not (directory / 'proof.txt').is_file():
            raise AssertionError('allowed guard did not create proof.txt')
        if name == 'lock_eintr':
            equal_lines(lines(done.stdout), [], 'EINTR must be retried before classifying the lock')


def extract(text, first, last):
    source = text.splitlines(keepends=True)
    if sum(line.rstrip('\n') == first for line in source) != 1:
        raise RuntimeError(f'oracle first line does not match exactly once: {first}')
    if sum(line.rstrip('\n') == last for line in source) != 1:
        raise RuntimeError(f'oracle last line does not match exactly once: {last}')
    start = next(i for i, line in enumerate(source) if line.rstrip('\n') == first)
    end = next(i for i, line in enumerate(source) if line.rstrip('\n') == last)
    return ''.join(source[start:end + 1])


def oracle_includes(work):
    inc = work / 'inc'
    inc.mkdir()
    source = (MPAS / 'core_atmosphere/dynamics/mpas_atm_time_integration.F').read_text()
    summary = extract(source, '   subroutine summarize_timestep(domain)', '   end subroutine summarize_timestep')
    # MPAS's summarize_timestep reads latCell/lonCell at the location of each maximum; when every value is NaN the
    # location stays huge(1) and the lookup is out of bounds. Guard the five lookups (two of them identical u blocks),
    # as reference_fixes/summarize_timestep.inc does; no value changes.
    lookup = re.compile(r'^         kMax = mod\(offset_1d, size\((w|u|spd),1\)\) \+ 1\n'
                        r'         indexMax = \(offset_1d / size\(\1,1\)\) \+ 1(?: ! Integer divide)?\n'
                        r'         latMax = lat(Cell|Edge)\(indexMax\)\n'
                        r'         lonMax = lon\2\(indexMax\)\n', re.MULTILINE)
    patched, count = lookup.subn(lambda m: '         if (offset_1d /= huge(1)) then\n' +
                                ''.join('   ' + line for line in m[0].splitlines(keepends=True)) +
                                '         end if\n', summary)
    if count != 5:
        raise RuntimeError(f'location guard matched {count} times, expected five')
    (inc / 'summarize_timestep.inc').write_text(patched)
    retained = ROOT / 'reference_fixes/summarize_timestep.inc'
    if not retained.is_file():
        raise RuntimeError('missing patched reference: reference_fixes/summarize_timestep.inc')
    if (inc / 'summarize_timestep.inc').read_bytes() != retained.read_bytes():
        raise RuntimeError('generated summary oracle differs from reference_fixes/summarize_timestep.inc')
    pristine = work / 'pristine_inc'
    pristine.mkdir()
    (pristine / 'summarize_timestep.inc').write_text(summary)
    log = (MPAS / 'framework/mpas_log.F').read_text()
    pieces = extract(log, '   function mpas_log_escape_dollars(str) result(escape_str)',
                     '   end function mpas_log_escape_dollars')
    pieces += extract(log, '   subroutine log_expand_string(inString, outString, intArgs, int8Args, logicArgs, realArgs)',
                      '   end subroutine log_expand_string')
    (inc / 'log_pieces.inc').write_text(pieces)
    return inc


def summary_lines(output):
    result = lines(output)
    start = result.index(' summary begin') + 1
    end = result.index(' summary end') if ' summary end' in result else len(result)
    return result[start:end]


def critical_lines(output):
    return [line for line in lines(output) if line.startswith('CRITICAL ERROR:')]


def identity(build, profile='strict'):
    # build: (build_git, build_name) of the executable, read from its generated lean_build_info
    return ['', ' MPAS-Atmosphere Version 8.4.2 (lean_mpas drop-in core)', '', '',
            " Output from 'git describe --dirty': " + build[0], '', ' Compile-time options:',
            f'   Build target: {build[1]}', '   OpenMP support: no', '   OpenACC support: no',
            '   Default real precision: single', f'   Compiler flags: {profile}', '   I/O layer: netCDF-C',
            '   MUSICA support: no', '   SCOTCH support: no', '   lean_mpas flavor: mpas',
            '   lean_mpas switches: none', '', ' Run-time settings:', '   MPI task count: 1', '']


def block_at(output, expected, label, start=0):
    for i in range(start, len(output)):
        if output[i:i + len(expected)] == list(expected):
            return i + len(expected)
    raise AssertionError(f'{label}: missing contiguous block {expected!r}')


def multi_line_extra(output):
    """Lines beyond one per message. The streams setup writes some messages of several lines, as MPAS's
    C parser does; a message counts once (framework/mpas_log.F:576-640). Measured on a log that holds the whole
    setup: its lines minus its own counts."""
    start = output.index(' Total log messages printed:') - 2
    counts = [int(line.split('=')[1]) for line in output[start + 3:start + 7]]
    return len(output[5:start]) - sum(counts)


def check_counts(output):
    start = output.index(' Total log messages printed:') - 2
    messages = output[5:start]
    warning = sum(line.startswith('WARNING:') for line in messages)
    error = sum(line.startswith('ERROR:') for line in messages)
    critical = sum(line.startswith('CRITICAL ERROR:') for line in messages)
    # MPAS's own log of the same streams setup gives the extra lines of its multi-line messages (19); a run that
    # stops before the end of that setup has at most as many
    if not any(line.startswith(' Reading streams configuration from file ') for line in messages):
        extra = 0     # a stop before the streams setup: every message so far is one line
    else:
        extra = multi_line_extra(mpas_greece_log())
        if '  ** Validating streams' not in messages:
            extra = min(extra, max(0, multi_line_extra(output)))
    count = len(messages) - warning - error - critical - extra
    equal_lines(output[start:], closing(count, warning, error, critical), 'counted log closing')


def check_cli(case, directory, done, before, programs, env):
    if 'error' in case:
        if case['error'] not in done.stderr.splitlines():
            raise AssertionError(f'CLI stderr missing {case["error"]!r}: {done.stderr!r}')
    else:
        equal_lines(lines(done.stdout), [case['expected']], 'CLI stdout')
    if case['name'] == 'cli_trailing':
        second = subprocess.run([str(programs['cli_main']), '-n', 'a', '-s'], cwd=directory, env=env,
                                capture_output=True, text=True, timeout=20)
        if second.returncode != 0:
            raise AssertionError(f'trailing -s exit {second.returncode}')
        equal_lines(lines(second.stdout), ['a|streams.atmosphere'], 'trailing -s stdout')
    if snapshot(directory) != before:
        raise AssertionError('CLI parser wrote files')


GREECE = ROOT / 'data/cases/greece_60km/inputs'
STEPS = 30          # one hour of 120 s steps
ICE_WARNINGS = ['WARNING: Variable %s not in input file.' % name for name in ('qi', 'qs', 'qg', 'ni', 'nr')]
STREAM_WARNINGS = []
MPAS_GREECE_LOG = ROOT / 'data/cases/greece_60km/reference/strict_12h' / OUT
# Cases that run the forecast past its input reading (they need the release inputs of greece_60km), and cases that
# compare with MPAS's own log of the greece strict_12h run, which is not released.
NEEDS_INPUTS = {'finish_ok', 'finish_positive_definite', 'lock_unavailable', 'exe_forecast', 'exe_names',
                'stock_namelist', 'exe_forecast_debug', 'restart_log',
                *(case['name'] for case in CASES if case['name'].startswith('time_lines_'))}
NEEDS_MPAS_LOG = {'finish_ok', 'finish_positive_definite', 'lock_unavailable', 'exe_forecast', 'exe_names',
                  'exe_forecast_debug', 'summary_vel_log'}


class Skip(Exception):
    """A case that needs data this checkout does not hold."""


def mpas_greece_log():
    if not MPAS_GREECE_LOG.is_file():
        raise Skip(f'the MPAS log {MPAS_GREECE_LOG.relative_to(ROOT)} is not released')
    return lines(MPAS_GREECE_LOG.read_text())


def missing_data(name):
    """Why a case cannot run here, or None."""
    if name in NEEDS_INPUTS and not (GREECE / 'greece_60km.init.nc').is_file():
        return 'needs the greece_60km inputs in data/cases (cases/README.md)'
    if name in NEEDS_MPAS_LOG and not MPAS_GREECE_LOG.is_file():
        return f'the MPAS log {MPAS_GREECE_LOG.relative_to(ROOT)} is not released'
    return None
LBC_WARNINGS = ['WARNING: Variable lbc_%s not in input file.' % name for name in ('qi', 'qs', 'qg', 'ni', 'nr')]


def run_directory(directory, settings, namelist='namelist.atmosphere', streams='streams.atmosphere', more=''):
    """An MPAS run directory of the greece_60km case for one hour (tests/inputs/run_dir.sh's recipe): MPAS's default
    namelist and streams file with the case's settings, the stream lists, links to the inputs and the tables."""
    spec = directory / 'hour.txt'
    spec.write_text("nml nhyd_model config_run_duration '0_01:00:00'\n" + more)
    defaults = ROOT / 'reference/mpas_defaults'
    done = subprocess.run([sys.executable, str(ROOT / 'tools/cases/run_spec.py'), '--out', str(directory),
                           str(defaults / 'namelist.atmosphere'), str(defaults / 'streams.atmosphere'),
                           str(ROOT / 'cases/greece_60km/case.txt'), str(spec)], capture_output=True, text=True)
    if done.returncode:
        raise AssertionError(f'run_spec: {done.stderr.strip()}')
    spec.unlink()
    records = {option.name: record.name for record in [*gen_namelist.read_registry(), gen_namelist.LEAN_RECORD]
               for option in record.options}
    path = directory / 'namelist.atmosphere'
    path.write_text(validation_input(path.read_text(), settings, records))
    if namelist != 'namelist.atmosphere':
        path.rename(directory / namelist)
    if streams != 'streams.atmosphere':
        (directory / 'streams.atmosphere').rename(directory / streams)
    for source in defaults.glob('stream_list.atmosphere.*'):
        shutil.copy(source, directory / source.name)
    sources = [GREECE / 'greece_60km.init.nc', *sorted(GREECE.glob('lbc.*.nc')),
               *sorted((ROOT / 'reference/radiation').glob('OZONE_*.TBL')),
               *sorted((ROOT / 'reference/radiation').glob('RRTMG_*_DATA')),
               ROOT / 'reference/config/LANDUSE.TBL', ROOT / 'reference/config/NoahmpTable.TBL',
               *sorted((ROOT / 'data/tables').glob('*.DBL'))]
    for source in sources:
        (directory / source.name).symlink_to(source)


def check_forecast(case, directory, done, build):
    name = case['name']
    output = lines((directory / OUT).read_text())
    prefix = header() + identity(build)
    equal_lines(output[:len(prefix)], prefix, 'startup identity')
    check_counts(output)
    read = output.index(' Reading namelist from file namelist.atmosphere')
    if case.get('status', 0):
        error = lines((directory / ERR).read_text())
        if name == 'namelist_missing':
            critical = 'CRITICAL ERROR: Namelist file namelist.atmosphere does not exist.'
            equal_lines(output[read:read + 2], [' Reading namelist from file namelist.atmosphere', critical],
                        'missing namelist first fatal')
            expected = [critical]
        elif name in ('validate_stop', 'flavor_mismatch'):
            problem = ("config_horiz_mixing = '2d_fixed': lean_mpas supports only '2d_smagorinsky'"
                       if name == 'validate_stop' else
                       "config_lean_flavor = 'lean', but this executable is the 'mpas' flavor: rebuild with --flavor lean")
            expected = ['ERROR: ' + problem, 'CRITICAL ERROR: lean_mpas stops: 1 namelist problem(s) listed above; '
                        'docs/namelist.md lists what lean_mpas does with each option']
        else:
            critical = 'CRITICAL ERROR: MPAS core_physics abort'
            start = output.index(' Total log messages printed:') - 2
            equal_lines(output[start - 1:start], [critical], 'suite stops before counts')
            expected = [line for line in output[5:start] if line.startswith(('ERROR:', 'CRITICAL ERROR:'))]
        equal_lines(error, header('Error') + expected + ['Logging complete.  Closing file at <date>'], ERR)
        return
    if (directory / ERR).exists():
        raise AssertionError('successful forecast wrote error log')
    io = block_at(output, ['', ' ----- I/O task configuration: -----', '', '     I/O task count  = 1',
                           '     I/O task stride = 1', ''], 'I/O block', read + 1)
    # These six lines are pinned to the MPAS log as well.
    equal_lines(output[io - 6:io], mpas_greece_log()[36:42], 'I/O block vs MPAS')
    suite = block_at(output, [" ----- Setting up physics suite 'convection_permitting' -----"], 'suite', io)
    frame = ['', ' Checking consistency of dynamics settings...']
    if name == 'finish_positive_definite':
        frame += list(dynamics_warnings('config_positive_definite'))
    frame += ['  ----- done checking dynamics settings -----', '']
    cursor = block_at(output, frame, 'dynamics frame', suite)
    for stamp in ('2026-10-12_06:00:00', '2026-10-12_06:00:30', '2026-10-12_06:01:00'):
        block = ['', ' Begin timestep ' + stamp, '  split dynamics-transport integration 3', '',
                 ' global min, max w -0.500000 1.25000', ' global min, max u -5.00000 27.0000']
        equal_lines(output[cursor:cursor + len(block)], block, 'step block')
        cursor += len(block)
        if not re.fullmatch(r'  Timing for integration step: \S+ s', output[cursor]):
            raise AssertionError(f'bad integration timing: {output[cursor]!r}')
        cursor += 1
    finish = ['', ' ' + '*' * 56, '    Finished running the atmosphere core', ' ' + '*' * 56]
    equal_lines(output[cursor:cursor + 4], finish, 'finish banner')
    equal_lines(output[cursor + 4:cursor + 4 + len(TIMER_HEADER)], TIMER_HEADER, 'timer header')
    rows = timer_rows((directory / OUT).read_text())
    equal_lines([(int(row[0]), row[1], int(row[-7])) for row in rows],
                [(1, 'total time', 1), (2, 'initialize', 1), (2, 'time integration', 3)], 'forecast timers')
    # the real startup reads the greece init file, which holds no ice scalars (MPAS's five lines)
    warnings = [line for line in output if line.startswith('WARNING:')]
    if name == 'lock_unavailable':
        equal_lines(warnings, ['WARNING: cannot lock the run directory: Permission denied; '
                               'lean_mpas cannot check that no other run uses it'] + ICE_WARNINGS + STREAM_WARNINGS,
                    'lock unavailable warning')
    elif name == 'finish_positive_definite':
        equal_lines(warnings, ICE_WARNINGS + STREAM_WARNINGS + list(dynamics_warnings('config_positive_definite')[:-1]),
                    'positive definite warnings')
    else:
        equal_lines(warnings, ICE_WARNINGS + STREAM_WARNINGS, 'successful forecast warnings')


def check_executable(case, directory, done, before, programs, env):
    name = case['name']
    if name in ('exe_mpi_nothing_written', 'exe_lock_nothing_written', 'exe_usage_nothing_written',
                'exe_readonly'):
        expected = {
            'exe_mpi_nothing_written': 'lean_mpas: an MPI launcher started it with more than one task '
                                      '(OMPI_COMM_WORLD_SIZE=2). lean_mpas runs as one process: start it without '
                                      'mpirun, mpiexec or srun, or with one task. Nothing was written.',
            'exe_lock_nothing_written': 'lean_mpas: another run holds this run directory (its lock is taken). '
                                       'One run per directory. Nothing was written.',
            'exe_usage_nothing_written': CLI_N_ERROR,
            'exe_readonly': 'lean_mpas: cannot open log.atmosphere.0000.out:',
        }[name]
        if expected not in done.stderr:
            raise AssertionError(f'executable stderr missing {expected!r}: {done.stderr!r}')
        if snapshot(directory) != before:
            raise AssertionError('refused executable changed the run directory')
        return
    if name.startswith('time_lines_'):
        reference = ROOT / 'data/cases/greece_60km/reference' / case['reference'] / OUT
        if not reference.is_file():
            raise Skip(f'the MPAS log {reference.relative_to(ROOT)} is not released')
        def per_step(path):
            blocks, current = {}, None
            for line in lines(path.read_text()):
                if line.startswith(' Begin timestep '):
                    current = blocks.setdefault(line, [])
                elif current is not None and line.startswith(' --- time to '):
                    current.append(line)
            return blocks
        mine, theirs = per_step(directory / OUT), per_step(reference)
        assert mine and set(mine) <= set(theirs), f'steps of the run are not in the reference: {sorted(mine)[:3]}'
        for step, block in mine.items():
            equal_lines(block, theirs[step], f'{name} {step}')
        return
    if name == 'stock_namelist':
        # MPAS echoes a namelist value only for a record it could not read to its end (gen_inc.c:865-899), and its
        # nhyd_model and physics records are complete here: no line for either option, as in MPAS's own logs.
        text = (directory / OUT).read_text()
        problems = [line for line in lines(text) if line.startswith(('ERROR:', 'CRITICAL ERROR:'))]
        if problems:
            raise AssertionError(f'the stock namelist stopped the run: {problems[:3]!r}')
        stamps = [line for line in lines(text) if line.startswith(' Begin timestep ')]
        equal_lines(stamps, [' Begin timestep 2026-11-08_00:00:00', ' Begin timestep 2026-11-08_00:02:00'], 'steps')
        if 'config_apvm_upwinding' in text or 'config_microp_re' in text:
            raise AssertionError('the log echoes an option MPAS does not echo for a complete record')
        if 'Finished running the atmosphere core' not in text:
            raise AssertionError('the run did not finish')
        written = (directory / 'namelist.atmosphere').read_text()
        if not re.search(r'config_apvm_upwinding\s*=\s*0\.5', written) or not re.search(r'config_microp_re\s*=\s*\.false\.', written):
            raise AssertionError('the namelist of the run does not hold the two options')
        return
    # a forecast from the run directory, as atmosphere_model runs it
    all_output = lines((directory / OUT).read_text())
    stream_times=[line for line in all_output if line.startswith(('  Timing for diagnostic computation:',
                                                                '  Timing for stream output:'))]
    expected_writes=1 if case.get('profile','strict')=='debug' else 2
    if len(stream_times)!=2*expected_writes:
        raise AssertionError(f'expected {expected_writes} timed stream writes: {stream_times!r}')
    for pair in range(expected_writes):
        for offset,label in enumerate(('diagnostic computation','stream output')):
            if not re.fullmatch(rf'  Timing for {label}: \S+ s',stream_times[2*pair+offset]):
                raise AssertionError('bad stream timing')
    output=[line for line in all_output if line not in stream_times]

    diag_notices=[line for line in output if line.startswith(" stream '") and 'additional physics diagnostics follow' in line]
    expected_notices=[]
    if name=='exe_names':
        expected_notices=[f" stream '{s}' has no time interval (none); additional physics diagnostics follow the other stream" for s in ('output',)]
    equal_lines(diag_notices,expected_notices,'diagnostic interval notices')
    output=[line for line in output if line not in diag_notices]
    profile = case.get('profile', 'strict')
    debug = profile == 'debug'
    prefix = header() + identity(programs['build_debug' if debug else 'build'], profile)
    equal_lines(output[:len(prefix)], prefix, 'executable build identity')
    if (directory / ERR).exists():
        raise AssertionError('the forecast wrote an error log: ' + (directory / ERR).read_text()[-300:])
    namelist, streams = ('nl.custom', 'st.custom') if name == 'exe_names' else ('namelist.atmosphere', 'streams.atmosphere')
    block_at(output, [' Reading namelist from file ' + namelist], 'executable namelist name')
    block_at(output, [' Initializing MPAS_streamInfo from file ' + streams], 'executable streams name')
    frame = ['', ' Checking consistency of dynamics settings...', '  ----- done checking dynamics settings -----', '']
    cursor = block_at(output, frame, 'executable dynamics frame')
    # no stream warnings: lean serves every stream that rings in these runs
    unserved = [line for line in output if line.startswith("WARNING: Stream '")]
    equal_lines(unserved, STREAM_WARNINGS, 'streams not served')
    # the startup's first boundary read, then the steps: MPAS's lines 372-387 of the greece log, the first step's
    # boundary update included (the stream-output timing lines are left out of output above)
    mpas_log = mpas_greece_log()
    micro_prefixes = (' --- specifics on cloud microphysics option', ' --- dt_microp =', ' --- n_microp  =')
    micro_lines = lambda log: [line for line in log if line.startswith(micro_prefixes)]
    equal_lines(micro_lines(output), micro_lines(mpas_log), 'microphysics specifics vs MPAS')
    equal_lines(output[cursor:cursor + 16], mpas_log[371:387], 'first boundary reads and step vs MPAS log:372-387')
    steps = 2 if debug else STEPS
    stamps = [line for line in output if line.startswith(' Begin timestep ')]
    expected = [' Begin timestep 2026-11-08_%02d:%02d:00' % divmod(2 * step, 60) for step in range(steps)]
    equal_lines(stamps, expected, 'Begin timestep lines')
    cursor += 5
    # the longwave and shortwave decisions of each step are MPAS's lines
    begins = [i for i, line in enumerate(mpas_log) if line.startswith(' Begin timestep ')] + [len(mpas_log)]
    radiation = (' --- time to ',)
    for step in range(steps):
        block = ['', expected[step]]
        if step == 0:
            block += mpas_log[378:387]
        decisions=[line for line in mpas_log[begins[step]:begins[step + 1]] if line.startswith(radiation)]
        block += decisions
        block += ['  split dynamics-transport integration 3', '']
        equal_lines(output[cursor:cursor + len(block)], block, f'step {step + 1} block')
        cursor += len(block)
        if step == 0 and not debug and 'gfortran' in programs['build'][1]:
            # the first step's extrema are MPAS's own (its log 410-411): summarize_timestep sees the model's arrays
            equal_lines(output[cursor:cursor + 2], mpas_log[409:411], 'first step extrema vs MPAS log:410-411')
        for field in ('w', 'u'):
            if not re.fullmatch(rf' global min, max {field} \S+ \S+', output[cursor]):
                raise AssertionError(f'step {step + 1}: bad extrema line {output[cursor]!r}')
            cursor += 1
        if not re.fullmatch(r'  Timing for integration step: \S+ s', output[cursor]):
            raise AssertionError(f'step {step + 1}: bad integration timing {output[cursor]!r}')
        cursor += 1
    finish = ['', ' ' + '*' * 56, '    Finished running the atmosphere core', ' ' + '*' * 56]
    equal_lines(output[cursor:cursor + 4], finish, 'finish banner')
    equal_lines(output[cursor + 4:cursor + 4 + len(TIMER_HEADER)], TIMER_HEADER, 'timer header')
    rows = timer_rows((directory / OUT).read_text())
    equal_lines([(int(row[0]), row[1], int(row[-7])) for row in rows],
                [(1, 'total time', 1), (2, 'initialize', 1),
                 (2, 'diagnostic_fields', expected_writes), (2, 'stream_output', expected_writes),
                 (2, 'time integration', steps)], 'forecast timers')
    check_counts(all_output)
    if any(line.startswith(('ERROR:', 'CRITICAL ERROR:')) for line in output):
        raise AssertionError('the forecast logged an error')
    if done.stdout.strip():
        raise AssertionError(f'the drop-in form wrote on standard output: {done.stdout[:300]!r}')
    if name != 'exe_forecast':
        return
    products = directory / 'drop_in.prod'
    if not products.is_file():
        raise AssertionError('no products file')
    if (directory / 'forecast.bin').exists():
        raise AssertionError('the drop-in form wrote a forecast file')


def check_summary(case, directory, done, programs, env):
    oracle = subprocess.run([str(programs['summary_oracle']), case['mode'], str(directory / 'oracle.txt')],
                            cwd=directory, env=env, capture_output=True, text=True, timeout=20)
    expected = (directory / 'oracle.txt').read_text()
    actual = (directory / OUT).read_text()
    if done.returncode != oracle.returncode:
        raise AssertionError(f'oracle exit {oracle.returncode}, lean exit {done.returncode}')
    if case['mode'].startswith('nan_'):
        equal_lines(critical_lines(actual), critical_lines(expected), 'NaN CRIT vs MPAS')
        if case['mode'] in ('nan_w', 'nan_all_w'):
            equal_lines(critical_lines(actual), ["CRITICAL ERROR: NaN detected in 'w' field."], 'pinned NaN')
        if case['mode'] == 'nan_all_w':
            pristine = subprocess.run([str(programs['summary_pristine']), case['mode'], str(directory / 'pristine.txt')],
                                      cwd=directory, env=env, capture_output=True, text=True, timeout=20)
            if pristine.returncode == 0 or not re.search(r'above upper bound|[Ss]ubscript out of range', pristine.stderr):
                raise AssertionError(f'pristine must expose the unset-location bounds defect: {pristine.stderr!r}')
        return
    got = summary_lines(actual)
    equal_lines(got, summary_lines(expected), 'summary vs MPAS (zero text tolerance)')
    pristine = subprocess.run([str(programs['summary_pristine']), case['mode'], str(directory / 'pristine.txt')],
                              cwd=directory, env=env, capture_output=True, text=True, timeout=20)
    if pristine.returncode != 0:
        raise AssertionError(f'pristine finite case exit {pristine.returncode}: {pristine.stderr!r}')
    equal_lines(summary_lines(expected), summary_lines((directory / 'pristine.txt').read_text()),
                'location fix has zero effect on finite cases')
    if case['mode'] == 'vel':
        equal_lines(got, ['', ' global min, max w -0.500000 1.25000',
                          ' global min, max u -5.00000 27.0000'], 'pinned vel')
    elif case['mode'] == 'vel_positive':
        equal_lines(got[:2], ['', ' global min, max w 0.00000 1.75000'], 'pinned positive')
    elif case['mode'] == 'vel_log':
        # Replay four printed extrema as formatter inputs, not as a forecast-value comparison.
        equal_lines(got, [''] + mpas_greece_log()[409:411], 'extrema line forms vs MPAS 8.4.2 log:410-411')
    elif case['mode'] == 'detailed_ties':
        equal_lines(got, ['', '  global min w: -3.00000 k=2, 5.72958 lat, 114.592 lon',
                          '  global max w: 4.00000 k=3, 11.4592 lat, -73.5211 lon',
                          '  global min u: -40.0000 k=2, -5.72958 lat, 143.239 lon',
                          '  global max u: 40.0000 k=3, -8.59437 lat, -116.493 lon',
                          '  global max wsp: 50.0000 k=3, -8.59437 lat, -116.493 lon'], 'pinned tie locations')
    elif case['mode'] == 'sca_only':
        equal_lines(got[:2], ['', '  global min, max scalar 1 -0.100000E-04 0.120000E-01'], 'pinned scalar')
        if len(got) != 9:
            raise AssertionError('expected eight scalar lines')
    elif case['mode'] == 'sca_detailed':
        if len(got) != 14 or got[6] != '  global min, max scalar 1 -0.100000E-04 0.120000E-01':
            raise AssertionError('expected detailed extrema followed directly by eight scalar lines')
    elif case['mode'] == 'none':
        equal_lines(got, [], 'pinned none')


def compile_harnesses(work, fc):
    required = ['lean_log', 'lean_summary', 'lean_drop_in']
    if any(case.get('program') == 'guard_main' for case in CASES):
        required.append('lean_guard')
    for name in required:
        if not (ROOT / f'src/run/{name}.f90').is_file():
            raise RuntimeError(f'missing src/run/{name}.f90')
    cfg = BuildConfig('cpu', fc, 'strict')
    options, defines = unit_flags(cfg, 'serial_services', 'base')
    flags = options + ['-D' + define for define in defines] + ['-I' + str(work),
                                                            '-I' + str(ROOT / 'tests/run_shell')]
    sources = [ROOT / 'third_party/mpas-8.4.2/src/framework/mpas_kind_types.F']
    sources += [ROOT / f'src/run/{name}.f90' for name in
                ('lean_messages', 'lean_namelist_options', 'lean_namelist')]
    sources.append(write_build_info(cfg, work, describe='run-shell-test'))
    # lean_drop_in starts the model's inputs: its two programs are linked with the executable's objects
    sources += [ROOT / f'src/run/{name}.f90' for name in ('lean_log', 'lean_guard', 'lean_summary')
                if (ROOT / f'src/run/{name}.f90').is_file()]
    objects = []
    # --jobs is accepted for the common test interface; every compile is deliberately serial.
    for source in sources:
        target = work / (source.stem + '.o')
        command = [fc, *flags, '-c', str(source), '-o', str(target)]
        done = subprocess.run(command, cwd=work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'compile {source.name}: {done.stderr.strip() or done.stdout.strip()}')
        objects.append(str(target))
    programs = {}
    for name in sorted({case.get('program', 'log_main') for case in CASES} - {'executable', *MODEL_PROGRAMS}):
        source = ROOT / f'tests/run_shell/{"guard_main" if name == "guard_eintr" else name}.f90'
        target = work / name
        extra = []
        if name == 'guard_eintr':
            # Deterministic libc interposition: two interruptions, then a successful nonblocking lock.
            shim = work / 'flock_eintr.f90'
            shim.write_text('''integer(c_int) function flock_eintr(fd, operation) bind(C, name='flock') result(rc)
 use iso_c_binding, only: c_int, c_f_pointer
 use lean_build_info, only: errno_location
 implicit none
 integer(c_int), value :: fd, operation
 integer(c_int), pointer :: err
 integer, save :: calls = 0
 calls = calls + 1
 call c_f_pointer(errno_location(), err)
 rc = 0_c_int
 if (calls <= 2) then
  err = 4_c_int
  rc = -1_c_int
 end if
end function
''')
            extra = [str(shim)]
        done = subprocess.run([fc, *flags, str(source), *extra, *objects, '-o', str(target)],
                              cwd=work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'link {name}: {done.stderr.strip() or done.stdout.strip()}')
        programs[name] = target
    inc = oracle_includes(work)
    oracle_work = work / 'oracle'
    oracle_work.mkdir()
    oracle_objects = [str(work / 'mpas_kind_types.o')]
    # Bounds checks prove the patched all-NaN lookups are safe, rather than just not crashing by chance.
    oracle_flags = flags + ['-I' + str(inc)] + (['-fcheck=bounds'] if fc == 'gfortran' else ['-Mbounds'])
    for source in ('summary_stubs.F90', 'summary_oracle.F90'):
        target = oracle_work / (Path(source).stem + '.o')
        done = subprocess.run([fc, *oracle_flags, '-c', str(ROOT / 'tests/run_shell' / source), '-o', str(target)],
                              cwd=oracle_work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'oracle compile {source}: {done.stderr.strip()}')
        oracle_objects.append(str(target))
    programs['summary_oracle'] = oracle_work / 'summary_oracle'
    done = subprocess.run([fc, *oracle_flags, *oracle_objects, '-o', str(programs['summary_oracle'])],
                          cwd=oracle_work, capture_output=True, text=True)
    if done.returncode:
        raise RuntimeError(f'oracle link: {done.stderr.strip()}')
    pristine_work = work / 'pristine'
    pristine_work.mkdir()
    pristine_flags = flags + ['-I' + str(work / 'pristine_inc'), '-I' + str(oracle_work)]
    pristine_flags += ['-fcheck=bounds'] if fc == 'gfortran' else ['-Mbounds']
    pristine_object = pristine_work / 'summary_oracle.o'
    done = subprocess.run([fc, *pristine_flags, '-c', str(ROOT / 'tests/run_shell/summary_oracle.F90'),
                           '-o', str(pristine_object)], cwd=pristine_work, capture_output=True, text=True)
    if done.returncode:
        raise RuntimeError(f'pristine compile: {done.stderr.strip()}')
    programs['summary_pristine'] = pristine_work / 'summary_oracle'
    done = subprocess.run([fc, *pristine_flags, *oracle_objects[:-1], str(pristine_object),
                           '-o', str(programs['summary_pristine'])], cwd=pristine_work, capture_output=True, text=True)
    if done.returncode:
        raise RuntimeError(f'pristine link: {done.stderr.strip()}')
    # This mutant must be seen by a comparison, not merely by a compiler or runtime error.
    text = (ROOT / 'src/run/lean_summary.f90').read_text()
    for name in MUTANTS:
        mutant_work = work / name
        mutant_work.mkdir()
        if name == 'start_from_large':
            start = text.index('else if (print_vel) then')
            mutated = text[:start] + text[start:].replace('scalar_min = 0.0', 'scalar_min = 1.0e20', 1)
        else:
            offset = '(k-1) + size(u,1)*(iEdge-1)'
            if text.count(offset) != 2:
                raise RuntimeError('u_stride_nz requires exactly two u offset encodings')
            mutated = text.replace(offset, '(k-1) + nz*(iEdge-1)')
        if text == mutated:
            raise RuntimeError(f'{name} mutation did not match')
        source = mutant_work / 'lean_summary.f90'
        source.write_text(mutated)
        target = mutant_work / 'lean_summary.o'
        done = subprocess.run([fc, *flags, '-c', str(source), '-o', str(target)],
                              cwd=mutant_work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'{name} compile: {done.stderr.strip()}')
        programs[name] = mutant_work / 'summary_main'
        other = [obj for obj in objects if Path(obj).name != 'lean_summary.o']
        done = subprocess.run([fc, *flags, str(ROOT / 'tests/run_shell/summary_main.f90'), str(target), *other,
                               '-o', str(programs[name])], cwd=mutant_work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'{name} link: {done.stderr.strip()}')
    return programs


MODEL_PROGRAMS = ('cli_main', 'fake_forecast')


def compile_model_programs(work, build_dir, programs):
    """cli_main and fake_forecast against the executable's own objects, with its flags (as tests/inputs/run.sh)."""
    fc = json.loads((build_dir / 'build.json').read_text())['compiler']['path']
    flags = (build_dir / 'flags.txt').read_text().split()
    flags += ['-I' + line for line in (build_dir / 'moddirs.txt').read_text().split()]
    flags += ['-I' + str(ROOT / 'tests/run_shell')]
    objects = [line for line in (build_dir / 'objects.txt').read_text().split() if not line.endswith('/main.f90.o')]
    libs = (build_dir / 'libs.txt').read_text().split()
    target_dir = work / 'model_programs'
    target_dir.mkdir()
    for name in MODEL_PROGRAMS:
        target = target_dir / name
        done = subprocess.run([fc, *flags, str(ROOT / f'tests/run_shell/{name}.f90'), *objects, *libs, '-o', str(target)],
                              cwd=target_dir, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'link {name}: {done.stderr.strip() or done.stdout.strip()}')
        programs[name] = target


def compile_debug_log(work):
    work.mkdir()
    cfg = BuildConfig('cpu', 'gfortran', 'debug')
    options, defines = unit_flags(cfg, 'serial_services', 'base')
    if '-fcheck=all' not in options or any(flag in options for flag in ('-fopenmp', '-frecursive')):
        raise RuntimeError('debug fatal checks require the non-OpenMP recursion-checked profile')
    flags = options + ['-D' + define for define in defines]
    sources = [ROOT / 'third_party/mpas-8.4.2/src/framework/mpas_kind_types.F',
               ROOT / 'src/run/lean_messages.f90', ROOT / 'src/run/lean_log.f90']
    objects = []
    for source in sources:
        target = work / (source.stem + '.o')
        done = subprocess.run(['gfortran', *flags, '-c', str(source), '-o', str(target)],
                              cwd=work, capture_output=True, text=True)
        if done.returncode:
            raise RuntimeError(f'debug compile {source.name}: {done.stderr.strip()}')
        objects.append(str(target))
    target = work / 'log_main'
    done = subprocess.run(['gfortran', *flags, str(ROOT / 'tests/run_shell/log_main.f90'), *objects,
                           '-o', str(target)], cwd=work, capture_output=True, text=True)
    if done.returncode:
        raise RuntimeError(f'debug log link: {done.stderr.strip()}')
    return target


def build_executable(fc, jobs, profile='strict'):
    # the plain mpas flavor: one executable for every run directory
    done = subprocess.run([str(ROOT / 'lean'), 'build', '--flavor', 'mpas', '--target', 'cpu', '--fc', fc,
                           '--profile', profile, '--jobs', str(jobs), '--json'],
                          cwd=ROOT, capture_output=True, text=True, timeout=1800)
    if done.returncode:
        raise RuntimeError(f'executable build: {done.stdout.strip()} {done.stderr.strip()}')
    result = json.loads(done.stdout)
    executable = ROOT / result['exe']
    generated = executable.parent / 'generated/lean_build_info.f90'
    # The literal is generated from git; check the executable against what was embedded, not a later git state.
    build = []
    for key in ('build_git', 'build_name'):
        match = re.search(key + r" = '([^']*)'", generated.read_text())
        if not match:
            raise RuntimeError(f'generated {key} literal absent')
        build.append(match[1])
    return executable, tuple(build)


def positive_int(text):
    value = int(text)
    if value < 1:
        raise argparse.ArgumentTypeError('must be 1 or more')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc', choices=('gfortran', 'nvfortran'), default='gfortran')
    parser.add_argument('--jobs', type=positive_int, default=2)
    parser.add_argument('--keep', action='store_true')
    args = parser.parse_args()
    cases = CASES + (DEBUG_CASES if args.fc == 'gfortran' else [])
    if not shutil.which(args.fc):
        print(f'RUN SHELL: missing compiler {args.fc}', file=sys.stderr)
        return 2
    work = ROOT / 'build/run_shell' / args.fc
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    try:
        programs = compile_harnesses(work, args.fc)
        programs['executable'], programs['build'] = build_executable(args.fc, args.jobs)
        compile_model_programs(work, programs['executable'].parent, programs)
        if args.fc == 'gfortran':
            programs['log_main_debug'] = compile_debug_log(work / 'debug')
            programs['executable_debug'], programs['build_debug'] = build_executable(args.fc, args.jobs, 'debug')
    except RuntimeError as error:
        for case in cases:
            print(f'FAIL {case["name"]}: {error}')
        print(f'RUN SHELL FAIL: {len(cases)} of {len(cases)} cases')
        return 1
    failures = skipped = 0
    env = {key: value for key, value in os.environ.items() if key not in LAUNCHER_VARIABLES}
    for case in cases:
        name = case['name']
        reason = missing_data(name)
        if name in ('open_fail', 'lock_unavailable', 'exe_readonly') and os.geteuid() == 0:
            reason = 'root can write in a read-only directory'
        if reason:
            print(f'SKIP {name}: {reason}')
            skipped += 1
            continue
        directory = work / name
        directory.mkdir()
        lock_fd = None
        try:
            if name == 'restart_log':
                done = subprocess.run(
                    ['sh', 'tests/restart/runs.sh', args.fc, str(args.jobs)], cwd=ROOT,
                    env={**env, 'RESTART_CASE': name}, capture_output=True, text=True, timeout=900)
                assert done.returncode == 0 and f'ok   {name}' in done.stdout, done.stdout + done.stderr
                print(f'ok {name}')
                continue
            if case.get('program') in ('fake_forecast', 'executable') and name != 'namelist_missing':
                if name == 'exe_names':
                    run_directory(directory, case.get('settings', {}), 'nl.custom', 'st.custom', case['spec'])
                else:
                    run_directory(directory, case.get('settings', {}))
            if name == 'stale_err_removed':
                for file in (OUT, ERR):
                    (directory / file).write_text('old\n')
            if name in ('open_fail', 'exe_readonly'):
                directory.chmod(0o555)
            if name in ('lock_held', 'exe_lock_nothing_written'):
                lock_fd = os.open(directory, os.O_RDONLY)
                fcntl.flock(lock_fd, fcntl.LOCK_EX)
            if name == 'lock_after_kill':
                killed_lock(directory, env)
            before = snapshot(directory)
            if name == 'lock_unavailable':
                directory.chmod(0o333)
            arguments = case['args'] if 'args' in case else [case['mode']]
            program = case.get('program', 'log_main')
            if case.get('profile') == 'debug':
                program += '_debug'
            case_env = {**env, **case.get('env', {})}
            if name == 'exe_forecast':
                case_env['LEAN_PRODUCTS'] = str(directory / 'drop_in.prod')
            done = subprocess.run([str(programs[program]), *arguments], cwd=directory, env=case_env,
                                  capture_output=True, text=True, timeout=600 if program.startswith('executable') else 60)
            if name == 'lock_unavailable':
                directory.chmod(0o755)
            status = case.get('status', 0)
            if (status == 'nonzero' and done.returncode == 0) or (status != 'nonzero' and done.returncode != status):
                raise AssertionError(f'exit {done.returncode}, expected {case.get("status", 0)}; {done.stderr.strip()}')
            if case.get('program') in ('guard_main', 'guard_eintr'):
                check_guard(case, directory, done, before)
            elif case.get('program') == 'summary_main':
                check_summary(case, directory, done, programs, env)
            elif case.get('program') == 'cli_main':
                check_cli(case, directory, done, before, programs, env)
            elif case.get('program') == 'fake_forecast':
                check_forecast(case, directory, done, programs['build'])
            elif case.get('program') == 'executable':
                check_executable(case, directory, done, before, programs, env)
            else:
                check_log(case, directory, done, before)
            print(f'ok {name}')
        except Skip as reason:
            print(f'SKIP {name}: {reason}')
            skipped += 1
        except (AssertionError, OSError, subprocess.TimeoutExpired) as error:
            print(f'FAIL {name}: {error}')
            failures += 1
        finally:
            if lock_fd is not None:
                os.close(lock_fd)
            if name in ('open_fail', 'lock_unavailable', 'exe_readonly'):
                directory.chmod(0o755)
    for name, mode in MUTANTS.items():
        directory = work / ('mutant_' + name)
        directory.mkdir()
        try:
            done = subprocess.run([str(programs[name]), mode], cwd=directory, env=env,
                                  capture_output=True, text=True, timeout=20)
            if done.returncode != 0:
                raise AssertionError(f'mutant must run successfully, exit {done.returncode}')
            actual = summary_lines((directory / OUT).read_text())
            expected = summary_lines((work / f'summary_{mode}/oracle.txt').read_text())
            if actual == expected:
                raise AssertionError('mutant survived MPAS comparison')
            print(f'seen {name}: summary_{mode} differs from MPAS')
        except (AssertionError, OSError, subprocess.TimeoutExpired) as error:
            print(f'FAIL mutant {name}: {error}')
            failures += 1
    if failures:
        print(f'RUN SHELL FAIL: {failures} of {len(cases)} cases')
        return 1
    print(f'RUN SHELL OK: {len(cases)} cases, {len(MUTANTS)} mutants seen, {skipped} skipped ({args.fc})')
    if not args.keep:
        shutil.rmtree(work)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
