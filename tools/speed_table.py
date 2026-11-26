"""Compare component wall times from lean stdout and an MPAS timer table.

    python3 tools/speed_table.py --lean LEAN_STDOUT [--lean ...] [--label L ...] [--mpas log.atmosphere.0000.out] [--total]

Row (as printed)                        MPAS timers                          lean timers
Physics without microphysics and        physics driver minus both RRTMG      physics
  radiation
Radiation                               rrtmg_lwrad + rrtmg_swrad            radiation
Microphysics                            microphysics                         Thompson
dyn_tend                                atm_compute_dyn_tend                 tend
Acoustic                                atm_advance_acoustic_step +          acoustic + pert
                                        atm_divergence_damping_3d +
                                        small_step_prep
Recover                                 atm_recover_large_step_variables     recover
Solve diagnostics                       atm_compute_solve_diagnostics        diag
Vertical implicit coefficients and      atm_compute_vert_imp_coefs +         vert_imp + moist
  moist coefficients                    atm_compute_moist_coefficients
Scalar transport, plain                 atm_advance_scalars                  transport12
Scalar transport, monotonic             atm_advance_scalars_mono             mono
Boundaries                              all atm_bdy_* timers                 boundary
Output and diagnostics for output       diagnostic_fields + stream_output    outputs
Whole loop                              time integration + diagnostic_fields wall
                                        + stream_output
Not in the rows above                   Whole loop less the rows above       Whole loop less the rows above

On the MPAS side "Not in the rows above" includes atm_rk_integration_setup, physics_get_tend, and
atm_rk_dynamics_substep_finish. The nested calc_cldfraction timer stays in physics driver and mp_thompson stays in
microphysics. Timers outside the chosen Whole loop, such as initialize and stream_input, are excluded.

On the lean side it includes copies (RK setup and finish), after (after transport), step_start (which contains
shift), transfer, rest, and differences between the broad timers and the summed detail timers. lean's dynamics
timer (not a row here) spans the copies through moist and the after timer. The boundary timer covers LBC record
updates and boundary work in dynamics, transport and after microphysics. Radiation and Thompson have their own
timers outside physics. The outputs timer covers auxiliary per-step outputs. Streams and their diagnostics are
outside that timer.
"""

import argparse
from pathlib import Path
import sys


RADIATION = ('rrtmg_lwrad', 'rrtmg_swrad')
RELAXZONE = 'atm_bdy_adjust_dynamics_relaxzone_tend'
COMPONENTS = (
    ('Physics without microphysics and radiation', ('physics driver',), 'timing', ('physics',)),
    ('Radiation', RADIATION, 'timing', ('radiation',)),
    ('Microphysics', ('microphysics',), 'timing', ('Thompson',)),
    ('dyn_tend', ('atm_compute_dyn_tend',), 'detail_wall', ('tend',)),
    ('Acoustic', ('atm_advance_acoustic_step', 'atm_divergence_damping_3d', 'small_step_prep'),
     'detail_wall', ('acoustic', 'pert')),
    ('Recover', ('atm_recover_large_step_variables',), 'detail_wall', ('recover',)),
    ('Solve diagnostics', ('atm_compute_solve_diagnostics',), 'detail_wall', ('diag',)),
    ('Vertical implicit coefficients and moist coefficients',
     ('atm_compute_vert_imp_coefs', 'atm_compute_moist_coefficients'), 'detail_wall', ('vert_imp', 'moist')),
    ('Scalar transport, plain', ('atm_advance_scalars',), 'detail_wall', ('transport12',)),
    ('Scalar transport, monotonic', ('atm_advance_scalars_mono',), 'detail_wall', ('mono',)),
    ('Boundaries', (RELAXZONE,), 'timing', ('boundary',)),
    ('Output and diagnostics for output', ('diagnostic_fields', 'stream_output'), 'timing_extra', ('outputs',)),
)


def parse_lean(lines):
    timers = {}
    for line in lines:
        fields = line.split()
        if not fields or fields[0] not in ('timing', 'timing_extra', 'detail_wall', 'timing_cpu'):
            continue
        kind, label, *values = fields
        names = label.split('/')
        if names[0] != 'steps' or len(names) != len(values):
            raise ValueError(f'invalid lean {kind} line')
        timers[kind] = {'steps': int(values[0])}
        timers[kind].update((name, float(value)) for name, value in zip(names[1:], values[1:]))
    if 'timing' not in timers:
        raise ValueError('no lean timing line')
    return timers['timing']['steps'], timers


def parse_mpas(lines):
    timers = {}
    steps = None
    in_table = False
    for line in lines:
        fields = line.split()
        if not in_table:
            in_table = 'timer_name' in fields
            continue
        if len(fields) < 10:
            break
        try:
            int(fields[0])
            seconds = float(fields[-8])
            calls = int(fields[-7])
            for field in fields[-6:]:
                float(field)
        except ValueError:
            break
        name = ' '.join(fields[1:-8])
        timers[name] = seconds
        if name == 'time integration':
            steps = calls
    if not in_table:
        raise ValueError('no MPAS timer_name table')
    if steps is None:
        raise ValueError('no time integration row in the MPAS timer table')
    return steps, timers


def read_timers(path, parse):
    try:
        with path.open() as log:
            steps, timers = parse(log)
        if steps <= 0:
            raise ValueError('step count must be positive')
    except (OSError, ValueError) as error:
        raise ValueError(f'{path}: {error}') from error
    return steps, timers


def timer_sum(timers, names, missing):
    missing.update(name for name in names if name not in timers)
    return sum(timers.get(name, 0.0) for name in names)


def lean_components(timers, missing):
    rows = [timer_sum(timers.get(kind, {}), names, missing) for _, _, kind, names in COMPONENTS]
    wall = timer_sum(timers['timing'], ('wall',), missing)
    return rows + [wall, wall - sum(rows)]


def mpas_components(timers, missing):
    rows = []
    for _, names, _, _ in COMPONENTS:
        if names == (RELAXZONE,):
            names += tuple(name for name in timers if name.startswith('atm_bdy_') and name != RELAXZONE)
        seconds = timer_sum(timers, names, missing)
        if names == ('physics driver',):
            seconds -= timer_sum(timers, RADIATION, missing)
        rows.append(seconds)
    wall = timer_sum(timers, ('time integration', 'diagnostic_fields', 'stream_output'), missing)
    return rows + [wall, wall - sum(rows)]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--lean', type=Path, action='append', required=True, help='lean stdout (repeatable)')
    parser.add_argument('--label', action='append', default=[], help='lean column label in file order (default: file stem)')
    parser.add_argument('--mpas', type=Path, help='MPAS log.atmosphere.0000.out')
    parser.add_argument('--total', action='store_true', help='show seconds total instead of seconds per step')
    args = parser.parse_args(argv)
    if len(args.label) > len(args.lean):
        print(f'{parser.prog}: error: more --label values than --lean files', file=sys.stderr)
        return 2
    labels = [label.replace('|', r'\|') for label in args.label + [p.stem for p in args.lean[len(args.label):]]]

    unit = 's total' if args.total else 's/step'
    headings = ['Component']
    columns = []
    notes = []
    try:
        if args.mpas is not None:
            steps, timers = read_timers(args.mpas, parse_mpas)
            missing = set()
            rows = mpas_components(timers, missing)
            headings.append('MPAS s total' if args.total else 'MPAS s per step')
            columns.append([value / (1 if args.total else steps) for value in rows])
            if missing:
                notes.append('MPAS: ' + ', '.join(sorted(missing)))
        for path, label in zip(args.lean, labels):
            steps, timers = read_timers(path, parse_lean)
            missing = set()
            rows = lean_components(timers, missing)
            headings.append(f'lean {label} {unit}')
            columns.append([value / (1 if args.total else steps) for value in rows])
            if missing:
                notes.append(f'lean ({label}): ' + ', '.join(sorted(missing)))
    except ValueError as error:
        print(f'{parser.prog}: error: {error}', file=sys.stderr)
        return 2

    if args.mpas is not None:
        headings.extend(f'{label} / MPAS' for label in labels)
    print('| ' + ' | '.join(headings) + ' |')
    print('| --- |' + ' ---: |' * (len(headings) - 1))
    names = [name for name, *_ in COMPONENTS] + ['Whole loop', 'Not in the rows above']
    for name, values in zip(names, zip(*columns)):
        cells = [f'{value:.3g}' for value in values]
        if args.mpas is not None:
            mpas = values[0]
            cells.extend(f'{value / mpas:.3g}' if mpas else '-' for value in values[1:])
        print('| ' + ' | '.join([name, *cells]) + ' |')
    if notes:
        print('\nMissing timers: ' + '; '.join(notes))
    return 0


if __name__ == '__main__':
    sys.exit(main())
