"""tools/speed_table.py on two excerpts of the same greece_60km 3 h run: lean's timing lines
(data/lean_greece3h_excerpt.out) and MPAS's timer table (data/mpas_greece3h_excerpt.log)."""

import contextlib
import io
from pathlib import Path
import unittest
from unittest.mock import patch

from tools import speed_table


DATA = Path(__file__).parent / 'data'
LEAN = str(DATA / 'lean_greece3h_excerpt.out')
MPAS = str(DATA / 'mpas_greece3h_excerpt.log')


def run_main(*argv):
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        status = speed_table.main(list(argv))
    return status, out.getvalue(), err.getvalue()


class SpeedTableTest(unittest.TestCase):
    def test_lean_labels_and_wall_clock(self):
        steps, timers = speed_table.parse_lean([
            'timing steps/radiation/physics/unexpected/wall 3 4 7 2 90',
            'detail_wall steps/mono/tend 3 10 12',
            'timing_cpu steps/physics 3 21',
        ])
        self.assertEqual(steps, 3)
        self.assertEqual(timers['timing']['unexpected'], 2)
        self.assertEqual(timers['timing_cpu']['physics'], 21)
        rows = speed_table.lean_components(timers, set())
        self.assertEqual(rows[:4], [7, 4, 0, 12])
        self.assertEqual(rows[-2], 90)

    def test_mpas_timer_table(self):
        with Path(MPAS).open() as log:
            steps, timers = speed_table.parse_mpas(log)
        self.assertEqual(steps, 90)
        self.assertEqual(timers['atm_compute_dyn_tend'], 4.30928)
        self.assertEqual(timers['time integration'], 25.60218)
        self.assertEqual(timers['Grell-Freitas'], 2.35571)
        self.assertAlmostEqual(speed_table.mpas_components(timers, set())[-2], 29.77677)

    def test_component_sums_and_remainders(self):
        mpas = speed_table.mpas_components({
            'time integration': 60, 'diagnostic_fields': 4, 'stream_output': 1,
            'physics driver': 15, 'rrtmg_lwrad': 2, 'rrtmg_swrad': 1, 'microphysics': 3,
            'atm_advance_acoustic_step': 6, 'atm_divergence_damping_3d': 2, 'small_step_prep': 2,
            'atm_bdy_adjust_dynamics_relaxzone_tend': 4, 'atm_bdy_other': 2,
        }, set())
        lean = speed_table.lean_components({
            'timing': {'wall': 48, 'physics': 9, 'radiation': 2, 'Thompson': 3, 'boundary': 4},
            'timing_extra': {'outputs': 1, 'rest': 15},
            'detail_wall': {'acoustic': 4, 'pert': 3, 'copies': 7},
        }, set())
        self.assertEqual(mpas, [12, 3, 3, 0, 10, 0, 0, 0, 0, 0, 6, 5, 65, 26])
        self.assertEqual(lean, [9, 2, 3, 0, 7, 0, 0, 0, 0, 0, 4, 1, 48, 22])

    def test_missing_timers_are_zero_and_named(self):
        with patch('tools.speed_table.parse_lean', return_value=(90, {'timing': {'wall': 68}})):
            status, out, err = run_main('--mpas', MPAS, '--lean', LEAN, '--label', 'A')
        self.assertEqual((status, err), (0, ''))
        self.assertIn('| dyn_tend | 0.0479 | 0 | 0 |', out)
        self.assertIn('Missing timers: lean (A): ', out)
        self.assertIn(', tend,', out.splitlines()[-1])

    def test_two_lean_columns_and_totals(self):
        args = ('--lean', LEAN, '--label', 'A', '--lean', LEAN, '--label', 'B')
        status, out, err = run_main('--mpas', MPAS, *args)
        self.assertEqual((status, err), (0, ''))
        self.assertEqual(out.splitlines()[0], '| Component | MPAS s per step | lean A s/step | lean B s/step | A / MPAS | B / MPAS |')
        self.assertIn('| dyn_tend | 0.0479 | 0.143 | 0.143 | 2.99 | 2.99 |', out)
        self.assertIn('| Whole loop | 0.331 | 0.763 | 0.763 | 2.3 | 2.3 |', out)
        status, out, err = run_main(*args, '--total')
        self.assertEqual((status, err), (0, ''))
        self.assertEqual(out.splitlines()[0], '| Component | lean A s total | lean B s total |')
        self.assertIn('| Whole loop | 68.6 | 68.6 |', out)

    def test_missing_timing_or_unreadable_file(self):
        status, out, err = run_main('--lean', MPAS)
        self.assertEqual((status, out), (2, ''))
        self.assertIn('no lean timing line', err)
        status, out, err = run_main('--mpas', LEAN, '--lean', LEAN)
        self.assertEqual((status, out), (2, ''))
        self.assertIn('no MPAS timer_name table', err)
        status, out, err = run_main('--lean', str(DATA / 'missing'))
        self.assertEqual((status, out), (2, ''))
        self.assertIn(str(DATA / 'missing'), err)


if __name__ == '__main__':
    unittest.main()
