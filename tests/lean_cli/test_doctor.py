"""Tests for ./lean doctor (tools/lean/doctor.py), with fake tools on a temporary PATH and a temporary tables folder."""
import contextlib
import hashlib
import io
import json
import os
import re
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests.lean_cli.test_netcdf import fake_install, fake_nc_config
from tools.lean import cli, config, doctor, tables

FAKES = {
    'gfortran': '#!/bin/sh\ncase $1 in -dumpversion) echo {gfortran};; '
                '--version) echo "GNU Fortran (fake) {gfortran}";; esac\n',
    'cmake': '#!/bin/sh\necho "cmake version 3.31.2"\n',
    'ninja': '#!/bin/sh\necho 1.12.1\n',
    'git': '#!/bin/sh\necho "git version 2.50.1"\n',
}
TABLE = b'a Thompson table'


class FakeEnv:
    """A bin directory of fake tools and a tables folder holding one table that matches a one-table lock."""

    def __init__(self, tmp: Path, gfortran='15.2.0', drop=(), cdf5=True):
        self.bin = tmp / 'bin'
        self.bin.mkdir()
        for name, text in FAKES.items():
            if name in drop:
                continue
            path = self.bin / name
            path.write_text(text.replace('{gfortran}', gfortran), encoding='utf-8')
            path.chmod(0o755)
        self.nc_config = fake_nc_config(tmp, '', '4.9.3', cdf5)
        if 'nc-config' in drop:
            self.nc_config.unlink()
        self.root = tmp / 'root'
        (self.root / 'data' / 'tables').mkdir(parents=True)
        (self.root / 'data' / 'tables' / 'T.DBL').write_bytes(TABLE)
        self.lock = {'T.DBL': {'sha256': hashlib.sha256(TABLE).hexdigest(), 'size': len(TABLE)}}

    @contextlib.contextmanager
    def active(self):
        with mock.patch.dict(os.environ, {'PATH': str(self.bin)}), mock.patch.object(tables, 'ROOT', self.root), \
                mock.patch.object(tables, 'expected', return_value=self.lock):
            os.environ.pop('NETCDF', None)
            os.environ.pop('NETCDF_LIBS', None)
            yield

    def checks(self, target='cpu', fc='gfortran'):
        with self.active():
            return doctor.checks(target, fc)


def by_code(checks):
    return {check.code: check for check in checks if not check.ok}


class DoctorTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name).resolve()

    def test_doctor_all_ok(self):
        env = FakeEnv(self.tmp)
        found = env.checks()
        self.assertTrue(all(check.ok for check in found), [check for check in found if not check.ok])
        names = [check.name for check in found]
        for name in ('gfortran', 'cmake', 'ninja', 'python', 'git', 'netcdf', 'T.DBL'):
            self.assertIn(name, names)
        self.assertEqual(names.index('netcdf'), names.index('git') + 1)
        netcdf = found[names.index('netcdf')]
        self.assertEqual(netcdf.required_for, 'build')
        self.assertEqual(netcdf.found, f'netCDF-C 4.9.3 in {self.tmp / "lib"} '
                                      f'(CDF-5 yes, netCDF-4 yes; {env.nc_config} via PATH)')
        self.assertEqual(netcdf.need, 'netCDF-C with CDF-5 (NETCDF=<prefix>, or nc-config on PATH)')
        text, status = doctor.render(found)
        self.assertEqual(status, 0)
        lines = text.splitlines()
        self.assertEqual(lines[-1], 'DOCTOR OK')
        self.assertEqual(len(lines), len(found) + 1, 'one line per requirement, then the verdict')

    def test_doctor_netcdf_without_cdf5(self):
        env = FakeEnv(self.tmp, cdf5=False)
        found = env.checks()
        failed = by_code(found)
        self.assertEqual(set(failed), {'E_NETCDF_CDF5'})
        self.assertEqual(failed['E_NETCDF_CDF5'].hint,
                         'brew install netcdf | apt install libnetcdf-dev | '
                         'conda install -c conda-forge libnetcdf; or NETCDF=<prefix> '
                         'of a netCDF-C built with CDF-5, as for building MPAS')
        text, status = doctor.render(found)
        self.assertEqual(status, 1)
        self.assertIn('E_NETCDF_CDF5:', text)
        self.assertEqual(text.splitlines()[-1], 'DOCTOR FAIL: 1 missing')

    def test_doctor_netcdf_missing(self):
        env = FakeEnv(self.tmp, drop=('nc-config',))
        found = env.checks()
        self.assertEqual(set(by_code(found)), {'E_NO_NETCDF'})
        self.assertEqual(doctor.render(found)[1], 1)

    def test_doctor_netcdf4_optional(self):
        env = FakeEnv(self.tmp)
        script = env.nc_config.read_text(encoding='utf-8').replace('--has-nc4) echo yes', '--has-nc4) echo no')
        env.nc_config.write_text(script, encoding='utf-8')
        found = env.checks()
        netcdf = next((check for check in found if check.name == 'netcdf'), None)
        self.assertIsNotNone(netcdf)
        self.assertTrue(netcdf.ok)
        self.assertIn('CDF-5 yes, netCDF-4 no;', netcdf.found)
        self.assertEqual(doctor.render(found)[1], 0)

    def test_doctor_netcdf_explicit_prefix(self):
        env = FakeEnv(self.tmp, cdf5=False)
        nc_config = fake_nc_config(self.tmp, 'explicit', '4.9.3', True)
        with env.active(), mock.patch.dict(os.environ, {'NETCDF': str(nc_config.parent.parent)}):
            found = doctor.checks('cpu', 'gfortran')
        netcdf = next((check for check in found if check.name == 'netcdf'), None)
        self.assertIsNotNone(netcdf)
        self.assertTrue(netcdf.ok)
        self.assertIn(f'{nc_config} via NETCDF)', netcdf.found)

    def test_doctor_netcdf_reports_skipped(self):
        env = FakeEnv(self.tmp, cdf5=False)
        nc_config = fake_nc_config(self.tmp, 'homebrew', '4.9.3', True)
        with env.active(), mock.patch.dict(os.environ, {'PATH': f'{env.bin}{os.pathsep}{nc_config.parent}'}):
            found = doctor.checks('cpu', 'gfortran')
        netcdf = next((check for check in found if check.name == 'netcdf'), None)
        self.assertIsNotNone(netcdf)
        self.assertTrue(netcdf.ok)
        self.assertEqual(netcdf.found, f'netCDF-C 4.9.3 in {nc_config.parent.parent / "lib"} '
                                      f'(CDF-5 yes, netCDF-4 yes; {nc_config} via PATH; '
                                      f'skipped {env.nc_config} (netCDF 4.9.3, no CDF-5))')

    def test_doctor_static_link_check(self):
        env = FakeEnv(self.tmp)
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lhdf5_hl -lhdf5 -lz')
        for error in ('ld: library not found for -lhdf5_hl', None):
            with self.subTest(error=error), env.active(), \
                    mock.patch.dict(os.environ, {'NETCDF': str(prefix)}), \
                    mock.patch('tools.lean.netcdf.link_check', return_value=error) as link_check:
                found = doctor.checks('cpu', 'gfortran')
                nc = next(check for check in found if check.name == 'netcdf')
                self.assertEqual(nc.ok, error is None)
                link_check.assert_called_once()
                self.assertEqual(link_check.call_args.args[1], str(env.bin / 'gfortran'))
                self.assertEqual(link_check.call_args.args[0].kind, 'static')
                if error:
                    self.assertEqual(nc.code, 'E_NETCDF_STATIC')
                    self.assertIn(error, nc.found)
                    self.assertEqual(nc.hint, 'NETCDF_LIBS=<the dependency libraries with their -L>')
                    self.assertEqual(doctor.render(found)[1], 1)
                else:
                    self.assertEqual(nc.found, f'netCDF-C 4.9.3, static, in {prefix / "lib"} '
                                              '(CDF-5 yes from netcdf_meta.h, link check OK; '
                                              'deps -lhdf5_hl -lhdf5 -lz)')
                    self.assertEqual(doctor.render(found)[1], 0)

    def test_doctor_shared_skips_link_check(self):
        env = FakeEnv(self.tmp)
        with env.active(), mock.patch('tools.lean.netcdf.link_check') as link_check:
            self.assertTrue(next(check for check in doctor.checks('cpu', 'gfortran')
                                 if check.name == 'netcdf').ok)
        link_check.assert_not_called()

    def test_doctor_static_uses_explicit_build_compiler(self):
        env = FakeEnv(self.tmp)
        prefix = fake_install(self.tmp, 'static', 'static', True, True, static_libs='-lnetcdf -lz')
        compiler = self.tmp / 'chosen compiler'
        compiler.write_text('#!/bin/sh\necho 15.2.0\n', encoding='utf-8')
        compiler.chmod(0o755)
        with env.active(), mock.patch.dict(os.environ, {'NETCDF': str(prefix)}), \
                mock.patch('tools.lean.netcdf.link_check', return_value=None) as link_check:
            found = doctor.checks('cpu', 'gfortran', compiler=str(compiler))
        self.assertTrue(all(check.ok for check in found))
        self.assertEqual(link_check.call_args.args[1], str(compiler))
        self.assertIn('CDF-5 yes from nc-config, link check OK',
                      next(check.found for check in found if check.name == 'netcdf'))

    def test_doctor_static_missing_dependencies_has_static_hint(self):
        env = FakeEnv(self.tmp)
        prefix = fake_install(self.tmp, 'static', 'static', False, True)
        with env.active(), mock.patch.dict(os.environ, {'NETCDF': str(prefix)}):
            failed = by_code(doctor.checks('cpu', 'gfortran'))
        self.assertEqual(set(failed), {'E_NETCDF_STATIC'})
        self.assertEqual(failed['E_NETCDF_STATIC'].hint, 'NETCDF_LIBS=<the dependency libraries with their -L>')

    def test_doctor_shared_meta_reports_header(self):
        env = FakeEnv(self.tmp)
        prefix = fake_install(self.tmp, 'shared', 'shared', False, True)
        with env.active(), mock.patch.dict(os.environ, {'NETCDF': str(prefix)}):
            found = doctor.checks('cpu', 'gfortran')
        nc = next(check for check in found if check.name == 'netcdf')
        self.assertTrue(nc.ok)
        self.assertIn('netcdf_meta.h', nc.found)
        self.assertNotIn(';  via ', nc.found)

    def test_fake_env_ignores_inherited_netcdf(self):
        env = FakeEnv(self.tmp)
        with mock.patch.dict(os.environ, {'NETCDF': str(self.tmp / 'missing')}):
            with env.active():
                self.assertIsNone(os.environ.get('NETCDF'))
            self.assertEqual(os.environ['NETCDF'], str(self.tmp / 'missing'))

    def test_cli_doctor_help_names_netcdf(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            status = cli.main(['doctor', '--help'])
        self.assertEqual(status, 0)
        self.assertIn('netCDF-C', out.getvalue())

    def test_doctor_old_gfortran(self):
        env = FakeEnv(self.tmp, gfortran='12.2.0')
        failed = by_code(env.checks())
        self.assertEqual(set(failed), {'E_FC_OLD'})
        self.assertEqual(failed['E_FC_OLD'].found, '12.2.0')
        text, status = doctor.render(env.checks())
        self.assertEqual(status, 1)
        self.assertEqual(text.splitlines()[-1], 'DOCTOR FAIL: 1 missing')
        self.assertIn('E_FC_OLD:', text)

    def test_doctor_missing_ninja(self):
        env = FakeEnv(self.tmp, drop=('ninja',))
        failed = by_code(env.checks())
        self.assertEqual(set(failed), {'E_NO_NINJA'})
        self.assertIn('brew install ninja', failed['E_NO_NINJA'].hint)
        text, _ = doctor.render(env.checks())
        line = next(line for line in text.splitlines() if line.startswith('E_NO_NINJA:'))
        self.assertIn('(brew install ninja | port install ninja | apt install ninja-build)', line)

    def test_doctor_cmake_3_27_4_too_old(self):
        """CMake 3.27.4 (Ubuntu 23.10) gives a group no module of the groups it links, so Ninja may compile a file
        before the module it uses exists (seen: radiation_scratch compiled before RRTMG's parrrsw.mod)."""
        env = FakeEnv(self.tmp)
        (env.bin / 'cmake').write_text('#!/bin/sh\necho "cmake version 3.27.4"\n', encoding='utf-8')
        failed = by_code(env.checks())
        self.assertEqual(set(failed), {'E_CMAKE_OLD'})
        self.assertEqual(failed['E_CMAKE_OLD'].found, '3.27.4')
        self.assertIn('pip install cmake', failed['E_CMAKE_OLD'].hint)

    def test_cmakelists_minimum_is_the_lock_pin(self):
        """CMakeLists.txt and ./lean doctor stop at the same CMake: lean.lock.json tools_min.cmake."""
        text = (config.ROOT / 'CMakeLists.txt').read_text(encoding='utf-8')
        found = re.search(r'cmake_minimum_required\(VERSION ([0-9.]+)\)', text)
        self.assertIsNotNone(found)
        self.assertEqual(found.group(1), config.load_lock()['tools_min']['cmake'])

    def test_doctor_json(self):
        env = FakeEnv(self.tmp, drop=('git',))
        text, status = doctor.render(env.checks(), as_json=True)
        self.assertEqual(status, 1)
        data = json.loads(text)
        self.assertEqual(set(data), {'checks', 'missing', 'verdict'})
        self.assertEqual(data['verdict'], 'DOCTOR FAIL: 1 missing')
        self.assertEqual(data['missing'], 1)
        for entry in data['checks']:
            self.assertEqual(set(entry), {'name', 'ok', 'found', 'need', 'code', 'hint', 'required_for'})
        git = next(entry for entry in data['checks'] if entry['name'] == 'git')
        self.assertEqual((git['ok'], git['code']), (False, 'E_NO_GIT'))

    def test_missing_tables_warn_and_bad_tables_fail(self):
        env = FakeEnv(self.tmp)
        env.lock['U.DBL'] = {'sha256': '0' * 64, 'size': 3}
        failed = by_code(env.checks())
        self.assertEqual(set(failed), {'W_NO_TABLES'})
        text, status = doctor.render(env.checks())
        self.assertEqual((status, text.splitlines()[-1]), (0, 'DOCTOR OK'), 'a missing table is a warning only')
        (env.root / 'data' / 'tables' / 'U.DBL').write_bytes(b'bad')
        failed = by_code(env.checks())
        self.assertEqual(set(failed), {'E_TABLES_SHA'})
        self.assertEqual(doctor.render(env.checks())[1], 1)

    def test_gpu_checks(self):
        env = FakeEnv(self.tmp)
        nvhpc = self.tmp / 'nvhpc' / 'Linux_x86_64' / '2025'
        (nvhpc / 'compilers' / 'bin').mkdir(parents=True)
        (nvhpc / 'cuda' / '12.8' / 'bin').mkdir(parents=True)
        (nvhpc / 'cuda' / '12.8' / 'bin' / 'cuobjdump').write_text('#!/bin/sh\n')   # without it: E_NO_CUOBJDUMP
        (nvhpc / 'cuda' / '12.8' / 'bin' / 'cuobjdump').chmod(0o755)
        nvfortran = nvhpc / 'compilers' / 'bin' / 'nvfortran'
        nvfortran.write_text('#!/bin/sh\necho\necho "nvfortran 25.3-0 64-bit target on x86-64 Linux -tp alderlake "\n',
                             encoding='utf-8')
        nvfortran.chmod(0o755)
        (env.bin / 'nvfortran').symlink_to(nvfortran)
        smi = env.bin / 'nvidia-smi'
        # cc 8.9 is above lean.lock.json gpu.cc_min (70) but untested: a warning; cc 6.1 is below it: an error
        for answer, codes in (('12.0, 570.133.07', set()), ('8.9, 570.133.07', {'W_GPU_UNTESTED'}),
                              ('6.1, 570.133.07', {'E_GPU_CC'})):
            with self.subTest(answer=answer):
                smi.write_text(f'#!/bin/sh\necho "{answer}"\n', encoding='utf-8')
                smi.chmod(0o755)
                found = env.checks('gpu', 'nvfortran')
                self.assertEqual(set(by_code(found)), codes)
                gpu = next(check for check in found if check.name == 'gpu')
                self.assertIn('570.133.07', gpu.found)
        smi.unlink()
        self.assertEqual(set(by_code(env.checks('gpu', 'nvfortran'))), {'E_NO_GPU'})
        nvfortran.write_text('#!/bin/sh\necho "nvfortran 24.11-0 64-bit target on x86-64 Linux"\n', encoding='utf-8')
        self.assertIn('E_NVHPC_VERSION', by_code(env.checks('cpu', 'nvfortran')))

    def test_cli_doctor(self):
        env = FakeEnv(self.tmp)
        out = io.StringIO()
        with env.active(), contextlib.redirect_stdout(out):
            status = cli.main(['doctor', '--target', 'cpu', '--fc', 'gfortran'])
        self.assertEqual(status, 0)
        self.assertEqual(out.getvalue().splitlines()[-1], 'DOCTOR OK')


if __name__ == '__main__':
    unittest.main()
