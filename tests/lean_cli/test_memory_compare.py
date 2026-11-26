"""tools/memory_compare.py on synthetic measured lists (real lists come from a heap profiler and the card runtime)."""
import contextlib
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path

from tools.lean import config, memory

ROOT = config.ROOT
sys.path.insert(0, str(ROOT / 'tools'))
import gen_memory  # noqa: E402
import memory_compare  # noqa: E402

MIB = 2 ** 20
CARD = dict(card_build=True, any_card=True, staged_pools=True, card_radiation=True, card_thompson=True)


def heap_lines(terms, case):
    """A heap report as the rows predict it: each host array of 1 MiB or more under its row's first routine."""
    lines = ['LIVE AT PEAK (>=1MiB blocks) total B 0']
    for row in terms['rows']:
        for array, size in memory.array_bytes(row, case, terms, 'host').items():
            if size >= MIB and row['life'] != 'step':
                lines.append(f'{size} B     1  __mod_MOD_{row["routines"][0]} < MAIN__')
    return '\n'.join(lines) + '\n'


def device_lines(terms, case):
    prefix = lambda row: next((p for p in row['card_names'] if p.endswith('%')), '')
    return ''.join(f'{size} {prefix(row)}{array}\n' for row in terms['rows'] if not row['at_load']
                   for array, size in memory.array_bytes(row, case, terms, 'card').items() if size >= MIB)


class CompareTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.terms = memory.load_terms()

    def run_tool(self, case, heap=None, present=None, extra=()):
        with tempfile.TemporaryDirectory() as tmp:
            tmp = Path(tmp)
            (tmp / 'case.json').write_text(json.dumps(case))
            args = ['--case', str(tmp / 'case.json'), '--rows', str(ROOT / 'config/memory_terms.json')]
            if heap is not None:
                (tmp / 'heap.txt').write_text(heap)
                args += ['--heap', str(tmp / 'heap.txt')]
            if present is not None:
                (tmp / 'present.txt').write_text(present)
                args += ['--present', str(tmp / 'present.txt')]
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                status = memory_compare.main(args + list(extra))
        return status, out.getvalue()

    def cases(self):
        greece = dict(gen_memory.CASES['greece_60km'])
        athens = dict(gen_memory.CASES['athens'])
        athens_card = {**athens, **CARD, **memory.slots({}, athens['ncells'], athens['nz'])}
        return greece, athens, athens_card

    def test_measured_synthetic_lists_pass(self):
        greece, athens, athens_card = self.cases()
        for name, case in (('greece', greece), ('athens', athens)):
            with self.subTest(case=name):
                status, out = self.run_tool(case, heap=heap_lines(self.terms, memory.case_dict(self.terms, **case)))
                self.assertEqual(status, 0, out)
                self.assertNotIn('NO ROW', out)
        status, out = self.run_tool(athens_card, present=device_lines(self.terms, memory.case_dict(self.terms, **athens_card)))
        self.assertEqual(status, 0, out)

    def test_unknown_routine_and_two_owners(self):
        greece, _, _ = self.cases()
        heap = heap_lines(self.terms, memory.case_dict(self.terms, **greece))
        status, out = self.run_tool(greece, heap=heap + f'{5 * MIB} B 1  __x_MOD_mystery < MAIN__\n'
                                    f'{2 * MIB} B 1  __x_MOD_rf_allocate < __y_MOD_allocate_fields\n')
        self.assertEqual(status, 1)
        self.assertIn('NO ROW mystery < main 5.0', out)
        status, out = self.run_tool(greece, heap=heap + f'{2 * MIB} B 1  __x_MOD_open_outputs < __y_MOD_allocate_fields\n')
        self.assertIn('TWICE open_outputs < allocate_fields', out)

    def test_projection_leaves_small_arrays_out(self):
        greece, _, _ = self.cases()
        case = memory.case_dict(self.terms, **greece)
        gwdo = next(row for row in self.terms['rows'] if row['name'] == 'gwdo')
        sizes = memory.array_bytes(gwdo, case, self.terms, 'host')
        self.assertTrue(sizes and max(sizes.values()) < MIB)
        check = memory_compare.Comparison(case, self.terms)
        without = dict(self.terms, rows=[row for row in self.terms['rows'] if row['name'] != 'gwdo'])
        self.assertEqual(check.projected_host(), memory_compare.Comparison(case, without).projected_host())
        self.assertGreater(memory.host_needed(case, self.terms)['sum'], memory.host_needed(case, without)['sum'])

    def test_mutations_fail_and_name_the_row(self):
        greece, athens, athens_card = self.cases()
        g_heap = heap_lines(self.terms, memory.case_dict(self.terms, **greece))
        a_heap = heap_lines(self.terms, memory.case_dict(self.terms, **athens))
        device = device_lines(self.terms, memory.case_dict(self.terms, **athens_card))
        for case, kwargs, flag in ((greece, {'heap': g_heap}, ['--drop', 'auto']),
                                   (athens, {'heap': a_heap}, ['--scale', 'auto=0.5']),
                                   (athens_card, {'present': device}, ['--drop', 'auto']),
                                   (athens_card, {'present': device}, ['--scale', 'auto=0.5'])):
            with self.subTest(flag=flag, present='present' in kwargs):
                status, out = self.run_tool(case, extra=flag, **kwargs)
                self.assertEqual(status, 1, out)
                self.assertRegex(out, r'MUTATION (drop|scale=0.5) \w+')
                self.assertIn('MEMORY FAIL', out)
        # without microphysics greece's largest manifest row less 10 % stays inside the -2 % tolerance: the tool refuses
        dry = {**greece, 'micro_on': False}
        status, out = self.run_tool(dry, heap=heap_lines(self.terms, memory.case_dict(self.terms, **dry)),
                                    extra=['--scale', 'auto=0.9'])
        self.assertEqual(status, 2, out)
        self.assertIn('MUTATION TOO SMALL', out)

    def test_notify_and_smi_readers(self):
        notify = ('alloc  CUDA data  devaddr=0x1 bytes=100 variable=a\n'
                  'alloc  CUDA data  devaddr=0x2 bytes=300 variable=b\n'
                  'free   CUDA data  devaddr=0x1 bytes=100\n'
                  'alloc  CUDA data  devaddr=0x3 bytes=50 variable=c\n')
        self.assertEqual(sorted(memory_compare.read_notify(notify)), [(100, 'a'), (300, 'b')])
        self.assertEqual(memory_compare.read_smi('12345, 4000 MiB\n12345, 5312 MiB\n'), 5312 * MIB)
        self.assertEqual(memory_compare.read_heap('LIVE AT PEAK x\n    191.4 MiB     1  __a_MOD_b < c\n'),
                         [(int(191.4 * MIB), ['b', 'c'])])

    def test_present_reader_of_the_ledger(self):
        """The card runtime's present lists: '<bytes> <MiB> MiB line:<n> name:<name>(:,:) ...', the header line skipped."""
        dump = ('present_entries 2 blocks 2 static_MiB 3.6 allocated_MiB(D) 3081.3 raw_sum_MiB 3084.9\n'
                '   199884800     190.62 MiB line:71 name:pool_pa presentcount:1+0 aliases_inside:0\n'
                '    68082944      64.93 MiB line:20 name:g%zb3_cell(:,:,:) presentcount:0+1 aliases_inside:0\n')
        self.assertEqual(memory_compare.read_present(dump), [(199884800, 'pool_pa'), (68082944, 'g%zb3_cell')])
        self.assertEqual(memory_compare.read_present('100 a\n'), [(100, 'a')])
        # a member reached through a pointer component: g%m$p%zxu is the mesh's m%zxu
        self.assertEqual(memory_compare.read_present('25085160 23.92 MiB line:20 name:g%m$p%zxu(:,:)\n'),
                         [(25085160, 'm%zxu')])


if __name__ == '__main__':
    unittest.main()
