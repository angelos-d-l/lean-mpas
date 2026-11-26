"""./lean doctor's memory part and the Python twins of the program's memory check (tools/lean/memory.py, ncread.py),
and the gpu checks of tools/lean/doctor.py."""
import contextlib
import io
import json
import os
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests.lean_cli.test_doctor import FakeEnv, by_code
from tools.lean import card, cli, config, doctor, memory, ncread

ROOT = config.ROOT
X1 = ROOT / 'data/cases/x1_2562/inputs/x1.2562.init.nc'
GREECE = ROOT / 'data/cases/greece_60km/inputs/greece_60km.init.nc'
BUILD = ROOT / 'build/cpu-gfortran-strict'
needs_inits = unittest.skipUnless(X1.is_file() and GREECE.is_file(),
                                  'needs the release case inputs in data/cases (cases/README.md)')
sys.path.insert(0, str(ROOT / 'tests/memory'))
import cgroup_fixtures  # noqa: E402


def run_dir(tmp: Path, init: Path, case: str) -> Path:
    d = tmp / 'run'
    subprocess.run(['sh', '-c', f'root="{ROOT}"; . tests/inputs/run_dir.sh; make_run_dir "{d}" "{init.parent}" '
                    f'cases/{case}/case.txt'], cwd=ROOT, check=True, capture_output=True)
    return d


class MemoryTwinTest(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.tmp)

    def test_card_parity(self):
        """The doctor's full minus the program's rows + stack + residual is held_q to the byte; both use one
        headroom."""
        terms = memory.load_terms()
        case = memory.case_dict(terms, ncells=46251, nedges=139361, nvertices=93111, nz=45, card_build=True,
                                any_card=True, staged_pools=True, card_radiation=True, card_thompson=True,
                                **memory.slots({}, 46251, 45))
        held_q = 1_234_567_891
        ledger = memory.card_needed(case, terms, 9744, 48 * 1536, held_q=held_q, stack_at_load=False, residual=77_000_001)
        self.assertEqual(ledger['full'] - (ledger['rows'] + ledger['stack'] + ledger['residual']), held_q)
        self.assertEqual(ledger['need'] - ledger['more'], held_q)

    def test_stages_default(self):
        """phys_mode's parse (tests/memory/units.f90 stages): an unset LEAN_CARD means all, none means the host."""
        every = {'any_card': True, 'card_radiation': True, 'card_thompson': True}
        none = {'any_card': False, 'card_radiation': False, 'card_thompson': False}
        self.assertEqual(memory.stages({}, True), every)
        self.assertEqual(memory.stages({'LEAN_CARD': 'none'}, True), none)
        self.assertEqual(memory.stages({'LEAN_CARD': 'all'}, True), every)
        self.assertEqual(memory.stages({'LEAN_CARD': 'thompson'}, True),
                         {'any_card': False, 'card_radiation': False, 'card_thompson': True})
        self.assertEqual(memory.stages({}, False), none)

    def test_available_fixtures(self):
        for name, (size, source) in cgroup_fixtures.make(self.tmp / 'cg').items():
            with self.subTest(name=name):
                got = memory.available_linux(str(self.tmp / 'cg' / name))
                self.assertEqual(got[0], size)
                if source:
                    self.assertEqual(got[1], source)

    def test_available_darwin(self):
        if sys.platform != 'darwin':
            self.skipTest('macOS only')
        size, source = memory.available_darwin()
        memsize = int(subprocess.run(['sysctl', '-n', 'hw.memsize'], capture_output=True, text=True).stdout)
        self.assertTrue(0 < size < memsize, (size, memsize))
        self.assertEqual(source, 'free and inactive pages')

    @needs_inits
    @unittest.skipUnless((BUILD / 'lean_mpas').is_file(), 'needs the strict cpu build')
    def test_same_as_program(self):
        """The doctor with --build gives the host need the program prints in its memory report (it runs the
        program's own check)."""
        for init, case in ((GREECE, 'greece_60km'), (X1, 'x1_2562')):
            with self.subTest(case=case):
                d = run_dir(self.tmp / case, init, case)
                launched = subprocess.run([str(BUILD / 'lean_mpas')], cwd=d, capture_output=True,
                                          text=True, env={**os.environ, 'LEAN_MEMORY_REPORT': '1'})
                report = next(line for line in launched.stdout.splitlines() if 'host:' in line)
                checks, extra = memory.memory_checks('cpu', run_dir=d, build=BUILD)
                host = next(c for c in checks if c.name == 'host memory')
                self.assertTrue(report.strip().startswith(f"host: {extra['json']['memory']['host']['need']} MiB needed"),
                                report)
                self.assertTrue(host.found.startswith(report.split('host: ')[1].split(' (')[0]))

    @needs_inits
    def test_estimate_and_card_info(self):
        out = io.StringIO()
        zero = card.Card('GPU-a', 'Test Card A', 120, 48, 1536, total_mib=12227, free_mib=10)
        with contextlib.redirect_stdout(out), mock.patch('tools.lean.card.device_zero', return_value=zero), \
                mock.patch('tools.lean.doctor.checks', return_value=[]):
            status = cli.main(['doctor', '--target', 'gpu', '--init', str(X1)])
        text = out.getvalue()
        self.assertIn('info  card memory: about ', text)
        self.assertIn('10 MiB free', text)
        self.assertNotIn('E_MEMORY_CARD', text, 'an estimate has no card verdict')
        self.assertEqual(status, 0, text)

    @needs_inits
    def test_host_verdict_from_the_estimate(self):
        with mock.patch.object(memory, 'host_available', return_value=(64 * 2 ** 20, 'a test')):
            checks, _ = memory.memory_checks('cpu', init=X1)
        host = next(c for c in checks if c.name == 'host memory')
        self.assertEqual(host.code, 'E_MEMORY_HOST')
        text, status = doctor.render(checks)
        self.assertEqual(status, 1)
        self.assertRegex(text, r'E_MEMORY_HOST: host memory \d+ MiB needed \(with 512 MiB headroom\), 64 MiB available '
                               r'\(a test\) \(a smaller mesh or fewer levels, or a machine with more memory\)')

    @needs_inits
    def test_tables(self):
        d = run_dir(self.tmp, GREECE, 'greece_60km')
        nml = memory.read_namelist(d / 'namelist.atmosphere')
        self.assertEqual(by_code(memory.table_checks(d, nml)), {})
        (d / 'RRTMG_LW_DATA').unlink()
        self.assertEqual(set(by_code(memory.table_checks(d, {**nml, 'config_radt_lw_scheme': 'off'}))), {'E_NO_TABLE'},
                         'RRTMG is opened whatever the scheme')
        (d / 'MP_THOMPSON_QIautQS_DATA.DBL').unlink()
        self.assertEqual(set(by_code(memory.table_checks(d, {**nml, 'config_microp_scheme': 'off'}))), {'E_NO_TABLE'})
        self.assertEqual(len(by_code(memory.table_checks(d, nml))), 1)
        self.assertEqual(sum(1 for c in memory.table_checks(d, nml) if c.code == 'E_NO_TABLE'), 2)
        shutil.copy(GREECE.parent.parent.parent.parent / 'tables/MP_THOMPSON_QIautQS_DATA.DBL',
                    d / 'MP_THOMPSON_QIautQS_DATA.DBL')
        (d / 'MP_THOMPSON_QRacrQS_DATA.DBL').unlink()
        (d / 'MP_THOMPSON_QRacrQS_DATA.DBL').write_bytes(struct.pack('>i', 2759904) + b'\0' * 8)   # big-endian
        checks = memory.table_checks(d, nml)
        order = next(c for c in checks if c.name == 'Thompson table order')
        self.assertEqual((order.code, order.hint), ('E_TABLE_ORDER', f'{d}: the Thompson tables are in mixed byte orders'))
        (d / 'NoahmpTable.TBL').unlink()
        self.assertIn('NoahmpTable.TBL', [c.name for c in memory.table_checks(d, nml) if c.code == 'E_NO_TABLE'])
        (d / 'LANDUSE.TBL').unlink()
        restart = [c.name for c in memory.table_checks(d, {**nml, 'config_do_restart': True}) if c.code == 'E_NO_TABLE']
        self.assertNotIn('LANDUSE.TBL', restart)


class NcreadTest(unittest.TestCase):
    @needs_inits
    def test_case_inits(self):
        self.assertEqual(ncread.dimensions(X1)['nCells'], 2562)
        self.assertEqual(ncread.glacier_count(X1, slab=7), 80)
        self.assertEqual(ncread.glacier_count(GREECE), 0)

    def test_header_reader_cdf5_and_classic(self):
        if GREECE.is_file():
            self.assertEqual(ncread.header_dimensions(GREECE)['nCells'], 2337)   # CDF-5
        with tempfile.TemporaryDirectory() as tmp:
            for version in (1, 2):
                path = Path(tmp) / f'cdf{version}.nc'
                count = '>i' if version < 5 else '>q'
                header = b'CDF' + bytes([version]) + struct.pack('>i', 3)          # 3 records
                header += struct.pack('>i', 0x0A) + struct.pack(count, 2)
                for name, size in ((b'Time', 0), (b'nCells', 17)):
                    header += struct.pack(count, len(name)) + name + b'\0' * ((4 - len(name) % 4) % 4)
                    header += struct.pack(count, size)
                path.write_bytes(header + b'\0' * 16)
                self.assertEqual(ncread.header_dimensions(path), {'Time': 3, 'nCells': 17})
            nc4 = Path(tmp) / 'nc4.nc'
            nc4.write_bytes(b'\x89HDF\r\n\x1a\n')
            with self.assertRaises(ncread.NcError):
                ncread.header_dimensions(nc4)


class GpuChecksTest(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.tmp)
        self.env = FakeEnv(self.tmp)
        nvhpc = self.tmp / 'nvhpc' / 'Linux_x86_64' / '2025'
        (nvhpc / 'compilers' / 'bin').mkdir(parents=True)
        self.cuda = nvhpc / 'cuda' / '12.8' / 'bin'
        self.cuda.mkdir(parents=True)
        self.nvfortran = nvhpc / 'compilers' / 'bin' / 'nvfortran'
        self.answer('25.3')
        (self.env.bin / 'nvfortran').symlink_to(self.nvfortran)

    def answer(self, version, dryrun=''):
        self.nvfortran.write_text('#!/bin/sh\nfor a in "$@"; do [ "$a" = -dryrun ] && { echo "' + dryrun + '"; exit 0; }; '
                                  f'done\necho; echo "nvfortran {version}-0 64-bit target on x86-64 Linux"\n')
        self.nvfortran.chmod(0o755)

    def smi(self, answer, header=''):
        smi = self.env.bin / 'nvidia-smi'
        smi.write_text('#!/bin/sh\nif [ $# -eq 0 ]; then echo "' + header + '"; exit 0; fi\n'
                       'case "$*" in *uuid*) printf "' + answer[1] + '";; *) echo "' + answer[0] + '";; esac\n')
        smi.chmod(0o755)

    def test_gpu_new(self):
        self.smi(('12.0, 595.99.02', ''), header='| NVIDIA-SMI 595.99.02   Driver Version: 595.99.02   CUDA Version: 12.2 |')
        self.answer('25.3', dryrun='/opt/nvidia/hpc_sdk/Linux_x86_64/25.3/cuda/12.8/bin/ptxas -arch=sm_120')
        with mock.patch('tools.lean.card.device_zero', return_value=None):
            found = self.env.checks('gpu', 'nvfortran')
        codes = by_code(found)
        self.assertIn('E_CUDA_DRIVER', codes)
        self.assertEqual(codes['E_CUDA_DRIVER'].hint, 'nvfortran would build for CUDA 12.8; the driver supports CUDA 12.2 '
                                                      '(a newer driver, or an NVHPC with a CUDA at or below 12.2)')
        self.assertIn('E_NO_CUOBJDUMP', codes)
        two = ('GPU-a, A, 1, 1, 0, 595.99.02\\nGPU-b, B, 1, 1, 0, 595.99.02\\n')
        self.smi(('12.0, 595.99.02', two))
        (self.cuda / 'cuobjdump').write_text('#!/bin/sh\n')
        (self.cuda / 'cuobjdump').chmod(0o755)
        with mock.patch('tools.lean.card.device_zero', return_value=None):
            codes = by_code(self.env.checks('gpu', 'nvfortran'))
        self.assertEqual(set(codes), {'W_CARD_WHICH'})
        text, status = doctor.render(self.env.checks('gpu', 'nvfortran'))
        self.assertIn('W_CARD_WHICH: 2 cards and no driver library to tell which one CUDA uses first: the card estimate '
                      'is not made', text)
        self.assertEqual(status, 0)

    def test_locks(self):
        (self.cuda / 'cuobjdump').write_text('#!/bin/sh\n')
        (self.cuda / 'cuobjdump').chmod(0o755)
        self.smi(('8.9, 595.99.02', 'GPU-b, NVIDIA Test Card B, 1, 1, 0, 595.99.02\\n'))
        self.answer('25.5')
        strict = json.loads((ROOT / 'lean.lock.json').read_text())   # without the minimums, untested is an error
        strict['gpu'].pop('cc_min')
        strict['nvhpc'].pop('min')
        with mock.patch.object(config, 'load_lock', return_value=strict):
            self.assertTrue({'E_GPU_CC', 'E_NVHPC_VERSION'} <= set(by_code(self.env.checks('gpu', 'nvfortran'))))
        codes = by_code(self.env.checks('gpu', 'nvfortran'))   # with them, above the minimum is a warning
        self.assertTrue({'W_GPU_UNTESTED', 'W_NVHPC_UNTESTED'} <= set(codes), codes)
        self.assertFalse(any(code.startswith('E_') for code in codes), codes)
        text, status = doctor.render(self.env.checks('gpu', 'nvfortran'))
        self.assertIn('W_NVHPC_UNTESTED: nvfortran 25.5: lean_mpas is tested with NVHPC 25.3', text)
        self.assertIn('W_GPU_UNTESTED: card cc89 (NVIDIA Test Card B): lean_mpas is tested on cc120; ./lean build '
                      'compiles for cc89', text)
        self.assertEqual(status, 0)


if __name__ == '__main__':
    unittest.main()
