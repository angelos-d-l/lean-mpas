"""tools/gen_memory.py: the rows of the startup memory sum, their formulas and the card ledger."""
import contextlib
import io
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from tools.lean import config, memory

ROOT = config.ROOT
sys.path.insert(0, str(ROOT / 'tools'))
import gen_memory  # noqa: E402

ATHENS = gen_memory.CASES['athens']
CARD = dict(card_build=True, any_card=True, staged_pools=True, card_radiation=True, card_thompson=True)


class GenMemoryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.terms = memory.load_terms()
        cls.rows = {row['name']: row for row in cls.terms['rows']}

    def mib(self, row, side, case, array=None):
        values = memory.array_bytes(self.rows[row], memory.case_dict(self.terms, **case), self.terms, side)
        return (values[array] if array else sum(values.values())) / 2 ** 20

    def test_fresh(self):
        self.assertEqual(gen_memory.main(['--check']), 0)

    def test_stale_names_the_member(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp = Path(tmp)
            for rel in ['config/memory.json', gen_memory.OUT_JSON, gen_memory.OUT_F90, gen_memory.OUT_DOC] + sorted(
                    {spec['file'] for spec in gen_memory.load_manifest()['generated']} | {'src/dyn_mesh_data.f90', 'src/lean_fields.f90'}):
                (tmp / rel).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy(ROOT / rel, tmp / rel)
            state = tmp / 'src/state.f90'
            text = state.read_text()
            self.assertIn('allocate(s%qke(nz,ncells))', text)
            state.write_text(text.replace('allocate(s%qke(nz,ncells))', 'allocate(s%qke(nz+1,ncells))'))
            err = io.StringIO()
            with contextlib.redirect_stderr(err):
                self.assertEqual(gen_memory.main(['--check', '--root', str(tmp)]), 1)
            self.assertIn('row state member qke', err.getvalue())

    def test_types_equal_report_state_memory(self):
        subprocess.run([sys.executable, 'report_state_memory.py'], cwd=ROOT / 'tools', check=True,
                       capture_output=True)
        measured = json.loads((ROOT / 'build/state-memory.json').read_text())['compiler_storage_bytes']
        case = memory.case_dict(self.terms, **ATHENS)
        for row, group in (('state', 'atmosphere'), ('land', 'Noah-MP'), ('gf', 'GF'), ('gwdo', 'GWDO'),
                           ('physics_mesh', 'mesh')):
            with self.subTest(row=row):
                self.assertEqual(memory.row_bytes(self.rows[row], case, self.terms, 'host'), measured[group])

    def test_athens_sizes_of_the_starting_list(self):
        card = {**ATHENS, **CARD, **memory.slots({}, ATHENS['ncells'], ATHENS['nz'])}
        expected = [  # (row, side, array or None, expected MiB on the athens case)
            ('radiation_groups', 'card', None, 491.3), ('micro_pools', 'card', None, 406.03),
            ('micro_pools', 'card', 'pool_pa', 190.6), ('mynn_stage', 'card', 'ms', 79.3), ('mynn_stage', 'card', 'pp', 20.0),
            ('thompson_first_use', 'card', 'mc_nm', 1.88), ('thompson_first_use', 'card', 'pt_list', 1.41),
            ('thompson_first_use', 'card', 'th_n0', 2.88), ('thompson_first_use', 'card', 'th_vt', 8.63),
            ('radiation_cache', 'host', 'rad_heating', 15.88),
            ('dyn_grid', 'host', 'zb_cell', 64.93), ('dyn_grid', 'host', 'zb3_cell', 64.93),
            ('dyn_fields', 'host', 'horiz_flux', 191.38), ('mslp_work', 'host', 'pressure', 7.94),
            ('start_winds', 'host', 'start_winds', 15.88), ('read_scratch', 'host', 'slots20', 10.63),
            ('maxima', 'host', 'vertex_order', 1.41)]
        for row, side, array, mib in expected:
            with self.subTest(row=row, array=array):
                case = {**card, 'products': True, 'maxima': True, 'film': True}
                self.assertAlmostEqual(self.mib(row, side, case, array), mib, delta=0.05)
        self.assertAlmostEqual(self.mib('film_in', 'card', {**card, 'film': True}), 63.52, delta=0.01)
        # horiz_flux is host only: the card's dyn_fields is the host's less it
        self.assertAlmostEqual(self.mib('dyn_fields', 'host', card) - self.mib('dyn_fields', 'card', card), 191.38,
                               delta=0.01)

    def test_slot_rules(self):
        # src/device/mynn_card.f90:92-100 (46,251 cells: 12,288 -> 4 x 11,584); micro_pool.f90:71-80 rounding to 128
        self.assertEqual(memory.slots({}, 46251, 45)['mynn_ldc'], 11584)
        self.assertEqual(memory.slots({'LEAN_MICRO_SLOTS': '1000', 'LEAN_GF_SLOTS': '100'}, 46251, 45)['micro_slots'], 896)
        self.assertEqual(memory.slots({'LEAN_GF_SLOTS': '100'}, 46251, 45)['gf_slots'], 128)
        self.assertEqual(memory.slots({}, 2337, 45)['mynn_ldc'], 2368)
        self.assertEqual([memory.glacier_capacity(n, True) for n in (0, 1, 64, 65, 80, 129)], [0, 64, 64, 128, 128, 256])
        self.assertEqual(memory.glacier_capacity(80, False), 80)

    def test_glacier_rows(self):
        x1 = gen_memory.CASES['x1_2562']
        self.assertEqual(self.mib('glacier_list', 'host', x1) * 2 ** 20, 128 * 65536)
        self.assertEqual(self.mib('glacier_grow', 'host', x1) * 2 ** 20, 64 * 65536)
        self.assertEqual(self.mib('glacier_list', 'host', {**x1, 'cold': False, 'glacier_capacity': 80}) * 2 ** 20,
                         80 * 65536)

    def test_ledger(self):
        case = memory.case_dict(self.terms, **{**ATHENS, **CARD, **memory.slots({}, 46251, 45)})
        for at_load in (True, False):
            with self.subTest(stack_at_load=at_load):
                held_q = 900 * 2 ** 20 + (9744 * 98304 if at_load else 0)
                ledger = memory.card_needed(case, self.terms, 9744, 98304, held_q=held_q, stack_at_load=at_load,
                                            residual=123 * 2 ** 20)
                # the stack above the default 1 KiB: charged at the first kernel launch, so held_q already holds it at load
                self.assertEqual(ledger['stack'], 0 if at_load else (9744 - 1024) * 98304)
                self.assertEqual(ledger['full'], held_q + ledger['rows'] + ledger['stack'] + ledger['residual'])
                # the program asks for rows + stack + residual + headroom more, the doctor for full + headroom:
                # they differ by held_q exactly
                self.assertEqual(ledger['need'] - ledger['more'], held_q)
                self.assertEqual(ledger['headroom'], max(int(ledger['full'] * 0.05), 256 * 2 ** 20))
        static = dict(self.rows['radiation_cache'], at_load=True, name='static_table')
        terms = dict(self.terms, rows=self.terms['rows'] + [static])
        self.assertEqual(memory.card_needed(case, terms, 0, 0)['rows'], memory.card_needed(case, self.terms, 0, 0)['rows'])

    def test_measured_ledger(self):
        """The measured terms (the test card): held_q 206 MiB, the stack above the default 1 KiB charged at the first launch,
        residual 32 MiB."""
        measured = self.terms['measured']
        self.assertEqual(measured['held_q_bytes'], 206 * 2 ** 20)
        self.assertFalse(measured['stack_at_load'])
        self.assertEqual(measured['stack_default_bytes'], 1024)
        self.assertEqual(measured['residual_bytes'], 32 * 2 ** 20)
        case = memory.case_dict(self.terms, **{**ATHENS, **CARD, **memory.slots({}, 46251, 45)})
        ledger = memory.card_needed(case, self.terms, 9744, 73728)
        self.assertAlmostEqual(ledger['stack'] / 2 ** 20, 613.1, delta=0.05)   # the ledger's stack on the test card (73,728 threads)
        self.assertEqual(memory.card_needed(case, self.terms, 512, 73728)['stack'], 0)

    def test_card_rounding(self):
        """On the card each array of 2 MiB or more takes whole 2 MiB pages; a smaller one its own bytes; the host
        side and array_bytes stay exact."""
        two = 2 ** 21
        case = memory.case_dict(self.terms, **{**ATHENS, **CARD, **memory.slots({}, 46251, 45)})
        for row in self.terms['rows']:
            with self.subTest(row=row['name']):
                exact = memory.array_bytes(row, case, self.terms, 'card')
                want = sum(-(-b // two) * two if b >= two else b for b in exact.values())
                self.assertEqual(memory.row_bytes(row, case, self.terms, 'card'), want)
                self.assertEqual(memory.row_bytes(row, case, self.terms, 'host'),
                                 sum(memory.array_bytes(row, case, self.terms, 'host').values()))
        # pool_pa of 190.6 MiB (199,884,800 B on the athens case) rounds to 192 MiB
        pools = memory.array_bytes(self.rows['micro_pools'], case, self.terms, 'card')
        self.assertEqual(pools['pool_pa'], 199884800)

    def test_host_need(self):
        case = memory.case_dict(self.terms, **gen_memory.CASES['greece_60km'])
        need = memory.host_needed(case, self.terms)
        self.assertEqual(need['sum'], need['resident'] + max(need['start'], need['step']))
        self.assertEqual(need['headroom'], 512 * 2 ** 20)
        self.assertEqual(need['need'], need['sum'] + need['headroom'])

    def test_fortran_formula(self):
        self.assertEqual(gen_memory.fortran('4*nz*(ncells+1) if cold else 0', {}),
                         'merge(((4_int64*int(mc%nz,int64))*(int(mc%ncells,int64)+1_int64)),0_int64,mc%cold)')
        self.assertEqual(gen_memory.fortran('card_build and not any_card', {}), '(mc%card_build.and.(.not.mc%any_card))')
        with self.assertRaises(gen_memory.GenError):
            gen_memory.fortran('ncells**2', {})

    def test_docs_list_every_row(self):
        doc = (ROOT / gen_memory.OUT_DOC).read_text()
        for row in self.terms['rows']:
            with self.subTest(row=row['name']):
                line = next(l for l in doc.splitlines() if l.startswith(f"| `{row['name']}` |"))
                self.assertIn(f"| {row['side']} | {row['life']} |", line)
                self.assertEqual(line.count('|'), 7 + 2 * len(gen_memory.CASES) )


if __name__ == '__main__':
    unittest.main()
