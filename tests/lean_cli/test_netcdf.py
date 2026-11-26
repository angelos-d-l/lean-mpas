"""netCDF-C discovery with small fake installs, one check of the netCDF-C installed here, and tests/netcdf/run.sh."""
import contextlib
import io
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from dataclasses import FrozenInstanceError, replace
from pathlib import Path
from unittest import mock

from tools.lean import config, netcdf
from tools.lean.netcdf import LIBRARY, NetCDF, NetCDFError, default_libdir, find, link_args, probe

ROOT = Path(__file__).resolve().parents[2]
CPU = config.BuildConfig('cpu', 'gfortran', 'strict')
FAKE = NetCDF('/x/bin/nc-config', 'NETCDF', '4.9.3', '/x', '/x/include', '/x/lib',
              '/x/lib/' + LIBRARY, True, True)


def fake_nc_config(root, name, version, cdf5, library=True) -> Path:
    prefix = root / name
    path = prefix / 'bin' / 'nc-config'
    path.parent.mkdir(parents=True, exist_ok=True)
    answers = {'--version': f'netCDF {version}', '--prefix': str(prefix),
               '--includedir': str(prefix / 'include'), '--libdir': str(prefix / 'lib'),
               '--has-cdf5': 'yes' if cdf5 else 'no', '--has-nc4': 'yes'}
    cases = ''.join(f'  {option}) echo {shlex.quote(answer)};;\n' for option, answer in answers.items())
    path.write_text('#!/bin/sh\ncase $1 in\n' + cases + 'esac\n', encoding='utf-8')
    path.chmod(0o755)
    (prefix / 'lib').mkdir(exist_ok=True)
    if library:
        (prefix / 'lib' / LIBRARY).touch()
    return path


def fake_install(root, name, kind, nc_config, cdf5, static_libs=None, settings_extra=None, lib='lib') -> Path:
    prefix = root / name
    includedir = prefix / 'include'
    libdir = prefix / lib
    includedir.mkdir(parents=True, exist_ok=True)
    libdir.mkdir(parents=True, exist_ok=True)
    (includedir / 'netcdf.h').touch()
    (includedir / 'netcdf_meta.h').write_text(
        '#define NC_VERSION_MAJOR 4\n#define NC_VERSION_MINOR 9\n#define NC_VERSION_PATCH 3\n'
        f'#define NC_HAS_CDF5 {int(cdf5)}\n#define NC_HAS_HDF5 1\n', encoding='utf-8')
    if kind in ('shared', 'both'):
        (libdir / LIBRARY).touch()
    if kind in ('static', 'both'):
        (libdir / 'libnetcdf.a').touch()
    if settings_extra is not None:
        (libdir / 'libnetcdf.settings').write_text(
            f'netCDF configuration\nExtra libraries: {settings_extra}\n', encoding='utf-8')
    if nc_config:
        path = fake_nc_config(root, name, '4.9.3', cdf5, library=False)
        script = path.read_text(encoding='utf-8').replace(str(prefix / 'lib'), str(libdir))
        if static_libs is not None:
            script = ('#!/bin/sh\nif [ "$*" = "--libs --static" ]; then\n'
                      f'  echo {shlex.quote(static_libs)}\n  exit 0\nfi\n' + script.removeprefix('#!/bin/sh\n'))
        path.write_text(script, encoding='utf-8')
    return prefix


class NetCDFTest(unittest.TestCase):

    def setUp(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        tmp = tempfile.TemporaryDirectory(prefix='netcdf-test-', dir=ROOT / 'build')
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name).resolve()

    def test_constants_match_netcdf_h(self):
        names = set('''NC_NOERR NC_EBADID NC_EEXIST NC_EPERM NC_EINVALCOORDS NC_ENAMEINUSE
            NC_ENOTATT NC_EBADDIM NC_ENOTVAR NC_ENOTNC NC_EEDGE NC_ERANGE NC_ENOTBUILT
            NC_CHAR NC_INT NC_FLOAT NC_DOUBLE NC_FORMAT_CLASSIC NC_FORMAT_64BIT_OFFSET
            NC_FORMAT_NETCDF4 NC_FORMAT_NETCDF4_CLASSIC NC_FORMAT_64BIT_DATA NC_UNLIMITED NC_MAX_NAME
            NC_NOWRITE NC_WRITE NC_CLOBBER NC_NOCLOBBER NC_64BIT_OFFSET NC_64BIT_DATA NC_NETCDF4
            NC_NOFILL NC_GLOBAL'''.split())
        source = (ROOT / 'src/io/lean_nc.f90').read_text()
        constants = re.findall(r'^\s*integer,\s*parameter(?:,\s*(?:public|private))?\s*::\s*'
                               r'(NC_\w+)\s*=\s*(-?\d+)\s*(?:!.*)?$', source, re.MULTILINE)
        self.assertEqual(len(constants), 33)
        self.assertEqual({name for name, _ in constants}, names)
        header = (Path(find().includedir) / 'netcdf.h').read_text()
        for name, value in constants:
            with self.subTest(name=name):
                match = re.search(r'^\s*#\s*define\s+' + name + r'\s+([^\r\n]+)', header, re.MULTILINE)
                self.assertIsNotNone(match, name)
                literal = match[1].split('/*', 1)[0].strip().strip('() \t').rstrip('Ll')
                self.assertEqual(int(value), int(literal, 0))

    def test_lean_nc_in_base(self):
        for cfg in (CPU, config.BuildConfig('gpu', 'nvfortran', 'strict')):
            with self.subTest(target=cfg.target):
                self.assertIn(('src/io/lean_nc.f90', 'base'), config.sources(cfg))
                self.assertEqual(config.unit_flags(cfg, 'lean_nc', 'base'),
                                 config.unit_flags(cfg, 'serial_services', 'base'))

    def test_netcdf_variable_wins(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        nc = find({'NETCDF': str(a.parent.parent), 'PATH': str(b.parent)})
        self.assertEqual(nc, NetCDF(str(a), 'NETCDF', '4.9.3', str(a.parent.parent),
                                    str(a.parent.parent / 'include'), str(a.parent.parent / 'lib'),
                                    str(a.parent.parent / 'lib' / LIBRARY), True, True))
        with self.assertRaises(FrozenInstanceError):
            nc.source = 'PATH'

    def test_netcdf_variable_without_cdf5_fails(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.2', False)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(a.parent.parent), 'PATH': str(b.parent)})
        self.assertIsInstance(raised.exception, ValueError)
        self.assertEqual(raised.exception.code, 'E_NETCDF_CDF5')
        self.assertIn('NETCDF', raised.exception.detail)
        self.assertIn('4.9.2', raised.exception.detail)

    def test_empty_netcdf_prefix_fails(self):
        prefix = self.tmp / 'empty'
        prefix.mkdir()
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(prefix), 'PATH': str(b.parent)})
        self.assertEqual(raised.exception.code, 'E_NO_NETCDF')
        self.assertIn('bin/nc-config', raised.exception.detail)
        self.assertIn('netcdf.h', raised.exception.detail)

    def test_netcdf_variable_without_library_fails(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True, library=False)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(a.parent.parent), 'PATH': str(b.parent)})
        self.assertEqual(raised.exception.code, 'E_NO_NETCDF')
        self.assertIn(LIBRARY, raised.exception.detail)

    def test_netcdf_variable_silent_fails(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        a.write_text('#!/bin/sh\nexit 0\n', encoding='utf-8')
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(a.parent.parent), 'PATH': str(b.parent)})
        self.assertEqual(raised.exception.code, 'E_NO_NETCDF')
        self.assertIn(str(a), raised.exception.detail)

    def test_path_skips_nc_config_without_cdf5(self):
        macports = fake_nc_config(self.tmp, 'macports', '4.9.2', False)
        homebrew = fake_nc_config(self.tmp, 'homebrew', '4.9.3', True)
        nc = find({'PATH': os.pathsep.join(map(str, (macports.parent, homebrew.parent)))})
        self.assertEqual((nc.nc_config, nc.source), (str(homebrew), 'PATH'))
        self.assertEqual(nc.skipped, (f'{macports} (netCDF 4.9.2, no CDF-5)',))

    def test_path_skips_nc_config_without_library(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True, library=False)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        nc = find({'PATH': os.pathsep.join(map(str, (a.parent, b.parent)))})
        self.assertEqual(nc.nc_config, str(b))
        self.assertEqual(nc.skipped, (f'{a} (no {LIBRARY} or libnetcdf.a in {a.parent.parent / "lib"})',))

    def test_path_duplicates_probed_once(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.2', False)
        alias = self.tmp / 'alias'
        alias.symlink_to(a.parent, target_is_directory=True)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        with mock.patch('tools.lean.netcdf.probe', wraps=probe) as probing:
            nc = find({'PATH': os.pathsep.join(map(str, (a.parent, alias, b.parent)))})
        self.assertEqual(nc.nc_config, str(b))
        self.assertEqual(nc.skipped, (f'{a} (netCDF 4.9.2, no CDF-5)',))
        self.assertEqual(probing.call_args_list, [mock.call(str(a), 'PATH'), mock.call(str(b), 'PATH')])

    def test_path_without_any_cdf5_fails(self):
        a = fake_nc_config(self.tmp, 'macports', '4.9.2', False)
        b = fake_nc_config(self.tmp, 'other', '4.9.1', False)
        with self.assertRaises(NetCDFError) as raised:
            find({'PATH': os.pathsep.join(map(str, (a.parent, b.parent)))})
        self.assertEqual(raised.exception.code, 'E_NETCDF_CDF5')
        for path in (a, b):
            self.assertIn(str(path), raised.exception.detail)

    def test_no_nc_config_fails(self):
        with self.assertRaises(NetCDFError) as raised:
            find({'PATH': str(self.tmp)})
        self.assertEqual(raised.exception.code, 'E_NO_NETCDF')

    def test_path_skips_silent_nc_config(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        a.write_text('#!/bin/sh\nexit 0\n', encoding='utf-8')
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        nc = find({'PATH': os.pathsep.join(map(str, (a.parent, b.parent)))})
        self.assertEqual(nc.nc_config, str(b))
        self.assertEqual(len(nc.skipped), 1)
        self.assertIn(str(a), nc.skipped[0])

    def test_path_ignores_nonexecutable_nc_config(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        a.chmod(0o644)
        b = fake_nc_config(self.tmp, 'b', '4.9.3', True)
        nc = find({'PATH': os.pathsep.join(map(str, (a.parent, b.parent)))})
        self.assertEqual(nc.nc_config, str(b))
        self.assertEqual(nc.skipped, ())

    def test_empty_netcdf_uses_path(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        nc = find({'NETCDF': '', 'PATH': str(a.parent)})
        self.assertEqual((nc.nc_config, nc.source), (str(a), 'PATH'))

    def test_find_uses_process_environment_by_default(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        with mock.patch.dict(os.environ, {'PATH': str(a.parent), 'NETCDF': ''}):
            nc = find()
        self.assertEqual((nc.nc_config, nc.source), (str(a), 'PATH'))

    def test_link_args(self):
        nc = NetCDF('/x/bin/nc-config', 'NETCDF', '4.9.3', '/x', '/x/include', '/x/lib',
                    '/x/lib/' + LIBRARY, True, True)
        self.assertEqual(link_args(nc), ['-L/x/lib', '-lnetcdf', '-Wl,-rpath,/x/lib'])

    def test_cmake_file_links_netcdf(self):
        static = replace(FAKE, kind='static', library='/x/lib/libnetcdf.a', deps=('-lhdf5_hl', '-lhdf5', '-lz'))
        for nc in (FAKE, static, replace(FAKE, libdir='/usr/lib')):
            with self.subTest(nc=nc):
                text = config.cmake_file(CPU, self.tmp, nc=nc)
                line = 'target_link_libraries(lean_mpas PRIVATE ' + ' '.join('"' + arg + '"' for arg in link_args(nc)) + ')'
                self.assertEqual(text.splitlines().count(line), 1)
                self.assertGreater(text.index(line), text.index('add_executable(lean_mpas'))
                options = re.search(r'LINK_OPTIONS "([^"]*)"', text).group(1)
                self.assertEqual(options.split(';'), config.link_flags(CPU))
                self.assertIn('"' + str(ROOT / 'tools/lean/netcdf.py') + '"', text)

    def test_config_main_same_link_from_elsewhere(self):
        prefix = fake_nc_config(self.tmp, 'relative', '4.9.3', True).parent.parent
        elsewhere = self.tmp / 'elsewhere'
        elsewhere.mkdir()
        with context_chdir(self.tmp):
            env = {**os.environ, 'NETCDF': os.path.abspath('relative')}
        lines = []
        for index, cwd in enumerate((ROOT, elsewhere)):
            out = self.tmp / f'config-{index}' / 'lean_sources.cmake'
            with context_chdir(cwd), mock.patch.dict(os.environ, env, clear=True):
                status = config.main(['cmake', '--target', 'cpu', '--fc', 'gfortran', '--profile', 'strict',
                                      '--out', str(out)])
            self.assertEqual(status, 0)
            links = [line for line in out.read_text().splitlines() if line.startswith('target_link_libraries(lean_mpas ')]
            self.assertEqual(len(links), 1)
            self.assertIn(f'"-L{prefix}/lib"', links[0])
            lines.append(links[0])
        self.assertEqual(lines[0], lines[1])

    def test_config_main_netcdf_error(self):
        out = self.tmp / 'lean_sources.cmake'
        err = io.StringIO()
        with mock.patch.object(netcdf, 'find', side_effect=NetCDFError('E_NETCDF_CDF5', 'no CDF-5')), \
                contextlib.redirect_stderr(err):
            status = config.main(['cmake', '--target', 'cpu', '--fc', 'gfortran', '--profile', 'strict',
                                  '--out', str(out)])
        self.assertEqual(status, 2)
        self.assertEqual(err.getvalue(), 'config: E_NETCDF_CDF5 no CDF-5\n')
        self.assertFalse(out.exists())

    def test_objects_txt_consumers_link_libs(self):
        scripts = [path for path in sorted((ROOT / 'tests').rglob('*.sh')) if 'objects.txt' in path.read_text()]
        self.assertTrue(scripts)
        for path in scripts:
            with self.subTest(script=str(path.relative_to(ROOT))):
                text = path.read_text()
                links = []
                for line in text.splitlines():
                    if not re.match(r'^\s*(gfortran|nvfortran|\$fc|"\$fc")\s', line):
                        continue
                    words = shlex.split(line)
                    if '-o' in words and '-c' not in words:
                        links.append(line)
                        self.assertTrue(line.rstrip().endswith('$libs'), line)
                self.assertTrue(links, 'objects.txt consumer has no link line')
                self.assertRegex(text, r'libs=\$\(cat "[^"]+/libs\.txt"\)')

    def test_link_args_default_dir(self):
        nc = NetCDF('/x/bin/nc-config', 'PATH', '4.9.3', '/usr', '/usr/include', '/usr/lib',
                    '/usr/lib/' + LIBRARY, True, True)
        for libdir in ('/usr/lib/x86_64-linux-gnu', '/usr/lib64', '/usr/lib', '/lib', '/lib64',
                       '/usr/lib/aarch64-linux-gnu'):
            with self.subTest(libdir=libdir):
                self.assertTrue(default_libdir(libdir))
                self.assertEqual(link_args(replace(nc, libdir=libdir)), ['-lnetcdf'])
        for libdir in ('/x/lib', '/usr/local/lib', '/usr/lib/not-multiarch',
                       '/usr/lib/x86_64-linux-gnu/extra', '/x/usr/lib/x86_64-linux-gnu'):
            with self.subTest(libdir=libdir):
                self.assertFalse(default_libdir(libdir))

    def test_relative_netcdf_made_absolute(self):
        a = fake_nc_config(self.tmp, 'rel/a', '4.9.3', True)
        with context_chdir(self.tmp):
            nc = find({'NETCDF': 'rel/a', 'PATH': str(self.tmp)})
        self.assertEqual(nc.nc_config, str(a))

    def test_relative_path_made_absolute(self):
        a = fake_nc_config(self.tmp, 'rel/a', '4.9.3', True)
        with context_chdir(self.tmp):
            nc = find({'PATH': 'rel/a/bin'})
        self.assertEqual(nc.nc_config, str(a))

    def test_empty_path_entry_means_working_directory(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        with context_chdir(a.parent):
            nc = find({'PATH': ''})
        self.assertEqual(nc.nc_config, str(a))

    def test_probe_options(self):
        a = fake_nc_config(self.tmp, 'a with spaces', '4.9.3', True)
        log = self.tmp / 'probe.log'
        script = a.read_text(encoding='utf-8').replace(
            'case $1 in', f'printf "%s\\n" "$*" >> {shlex.quote(str(log))}\ncase $1 in')
        a.write_text(script, encoding='utf-8')
        nc = probe(str(a), 'NETCDF')
        self.assertIsNotNone(nc)
        self.assertEqual(log.read_text(encoding='utf-8').splitlines(),
                         ['--version', '--prefix', '--includedir', '--libdir', '--has-cdf5', '--has-nc4'])
        self.assertEqual(nc.version, '4.9.3')
        self.assertTrue(nc.cdf5)
        self.assertTrue(nc.nc4)

    def test_probe_needs_first_four_answers(self):
        for option in ('--version', '--prefix', '--includedir', '--libdir'):
            with self.subTest(option=option):
                a = fake_nc_config(self.tmp, option, '4.9.3', True)
                script = a.read_text(encoding='utf-8').replace(f'{option}) echo ', f'{option}) exit 0;;\n  unused) echo ')
                a.write_text(script, encoding='utf-8')
                self.assertIsNone(probe(str(a), 'NETCDF'))

    def test_probe_failure_is_unusable(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        a.write_text('#!/bin/sh\necho "netCDF 4.9.3"\nexit 1\n', encoding='utf-8')
        self.assertIsNone(probe(str(a), 'PATH'))
        self.assertIsNone(probe(str(self.tmp / 'missing'), 'PATH'))

    def test_netcdf4_is_optional(self):
        a = fake_nc_config(self.tmp, 'a', '4.9.3', True)
        a.write_text(a.read_text(encoding='utf-8').replace('--has-nc4) echo yes', '--has-nc4) echo no'),
                     encoding='utf-8')
        nc = find({'PATH': str(a.parent)})
        self.assertTrue(nc.cdf5)
        self.assertFalse(nc.nc4)

    def test_static_with_nc_config(self):
        prefix = self.tmp / 'static'
        fake_install(self.tmp, 'static', 'static', True, True,
                     static_libs=f'-L{prefix}/lib -lnetcdf -lhdf5_hl -lhdf5 -lm -lz -lcurl')
        for env in ({'NETCDF': str(prefix), 'PATH': ''}, {'PATH': str(prefix / 'bin')}):
            with self.subTest(env=env):
                nc = find(env)
                self.assertEqual(nc.kind, 'static')
                self.assertEqual(nc.capabilities_from, 'nc-config')
                self.assertEqual(nc.library, str(prefix / 'lib' / 'libnetcdf.a'))
                self.assertEqual(link_args(nc), [str(prefix / 'lib' / 'libnetcdf.a'), f'-L{prefix}/lib',
                                                '-lhdf5_hl', '-lhdf5', '-lm', '-lz', '-lcurl'])
                self.assertFalse(any('-Wl,-rpath' in word for word in link_args(nc)))

    def test_static_without_nc_config(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, lib='lib64',
                              settings_extra='-lHDF5::HDF5 -lhdf5::hdf5_hl -lm -lz -lCURL::libcurl')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(nc.kind, 'static')
        self.assertEqual(nc.deps, ('-lhdf5_hl', '-lhdf5', '-lm', '-lz', '-lcurl'))
        self.assertEqual(nc.version, '4.9.3')
        self.assertEqual(nc.capabilities_from, 'netcdf_meta.h')
        self.assertEqual(nc.includedir, str(prefix / 'include'))
        self.assertEqual(nc.libdir, str(prefix / 'lib64'))
        self.assertEqual(nc.nc_config, '')
        self.assertTrue(nc.nc4)

    def test_static_meta_without_cdf5_fails(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, False)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(raised.exception.code, 'E_NETCDF_CDF5')
        self.assertIn('netcdf_meta.h', raised.exception.detail)

    def test_static_nc_config_without_cdf5_fails(self):
        prefix = fake_install(self.tmp, 'static', 'static', True, False)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(raised.exception.code, 'E_NETCDF_CDF5')

    def test_static_without_dependency_list_fails(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True)
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(raised.exception.code, 'E_NETCDF_STATIC')
        self.assertIn('NETCDF_LIBS', raised.exception.detail)

    def test_netcdf_libs_overrides(self):
        prefix = fake_install(self.tmp, 'static', 'static', True, True,
                              static_libs='-lnetcdf -lwrong', settings_extra='-lalso_wrong')
        nc = find({'NETCDF': str(prefix), 'PATH': '', 'NETCDF_LIBS': '-L/x -lhdf5_hl -lhdf5 -lz'})
        self.assertEqual(nc.deps, ('-L/x', '-lhdf5_hl', '-lhdf5', '-lz'))

    def test_shared_preferred_when_both(self):
        prefix = fake_install(self.tmp, 'both', 'both', True, True)
        nc = find({'PATH': str(prefix / 'bin')})
        self.assertEqual(nc.kind, 'shared')
        self.assertEqual(nc.library, str(prefix / 'lib' / LIBRARY))
        self.assertEqual(nc.deps, ())
        self.assertEqual(link_args(nc), [f'-L{prefix}/lib', '-lnetcdf', f'-Wl,-rpath,{prefix}/lib'])

    def test_shared_without_nc_config(self):
        prefix = fake_install(self.tmp, 'shared', 'shared', False, True)
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(nc.kind, 'shared')
        self.assertEqual(nc.capabilities_from, 'netcdf_meta.h')
        self.assertEqual(link_args(nc), [f'-L{prefix}/lib', '-lnetcdf', f'-Wl,-rpath,{prefix}/lib'])

    def test_meta_missing_header_or_version_fails(self):
        for index, missing in enumerate(('netcdf.h', 'netcdf_meta.h', 'NC_VERSION_PATCH', 'NC_HAS_CDF5')):
            with self.subTest(missing=missing):
                prefix = fake_install(self.tmp, f'incomplete-{index}', 'shared', False, True)
                header = prefix / 'include' / 'netcdf_meta.h'
                if missing.endswith('.h'):
                    (prefix / 'include' / missing).unlink()
                else:
                    header.write_text('\n'.join(line for line in header.read_text().splitlines()
                                                if missing not in line), encoding='utf-8')
                with self.assertRaises(NetCDFError) as raised:
                    find({'NETCDF': str(prefix), 'PATH': ''})
                self.assertEqual(raised.exception.code, 'E_NO_NETCDF')
                self.assertIn(missing, raised.exception.detail)

    def test_meta_nc4_overrides_hdf5(self):
        prefix = fake_install(self.tmp, 'shared', 'shared', False, True)
        header = prefix / 'include' / 'netcdf_meta.h'
        with header.open('a', encoding='utf-8') as stream:
            stream.write('#define NC_HAS_NC4 0\n')
        self.assertFalse(find({'NETCDF': str(prefix), 'PATH': ''}).nc4)

    def test_meta_lib64_preferred_only_when_it_has_a_library(self):
        prefix = fake_install(self.tmp, 'shared', 'shared', False, True)
        (prefix / 'lib64').mkdir()
        self.assertEqual(find({'NETCDF': str(prefix), 'PATH': ''}).libdir, str(prefix / 'lib'))
        (prefix / 'lib64' / LIBRARY).touch()
        self.assertEqual(find({'NETCDF': str(prefix), 'PATH': ''}).libdir, str(prefix / 'lib64'))
        (prefix / 'lib' / LIBRARY).unlink()
        (prefix / 'lib64' / LIBRARY).unlink()
        with self.assertRaises(NetCDFError) as raised:
            find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(raised.exception.code, 'E_NO_NETCDF')
        self.assertIn('libnetcdf', raised.exception.detail)

    def test_config_static_deps_normalized_before_settings(self):
        prefix = fake_install(self.tmp, 'static', 'static', True, True,
                              static_libs='-L/other -lnetcdf -lHDF5::HDF5 -lz -lhdf5::hdf5_hl '
                                          '-lCURL::libcurl -lhdf5 -lz -L/other',
                              settings_extra='-lwrong')
        nc = find({'PATH': str(prefix / 'bin')})
        self.assertEqual(nc.deps, ('-L/other', '-lhdf5_hl', '-lhdf5', '-lz', '-lcurl'))

    def test_static_settings_fallback_with_nc_config(self):
        prefix = fake_install(self.tmp, 'static', 'static', True, True, settings_extra='-lz')
        self.assertEqual(find({'PATH': str(prefix / 'bin')}).deps, ('-lz',))

    def test_static_empty_dependency_answer_is_usable(self):
        for source in ('NETCDF_LIBS', 'nc-config', 'settings'):
            with self.subTest(source=source):
                prefix = fake_install(self.tmp, source, 'static', source == 'nc-config', True,
                                      static_libs='-lnetcdf' if source == 'nc-config' else None,
                                      settings_extra='' if source == 'settings' else None)
                env = {'NETCDF': str(prefix), 'PATH': ''}
                if source == 'NETCDF_LIBS':
                    env['NETCDF_LIBS'] = ''
                self.assertEqual(find(env).deps, ())

    def test_static_link_args_default_dir(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lm')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        nc = replace(nc, libdir='/usr/lib', library='/usr/lib/libnetcdf.a')
        self.assertEqual(link_args(nc), ['/usr/lib/libnetcdf.a', '-lm'])

    def test_path_skips_static_without_dependencies(self):
        a = fake_install(self.tmp, 'static', 'static', True, True)
        with self.assertRaises(NetCDFError) as raised:
            find({'PATH': str(a / 'bin')})
        self.assertEqual(raised.exception.code, 'E_NETCDF_STATIC')
        b = fake_install(self.tmp, 'shared', 'shared', True, True)
        nc = find({'PATH': os.pathsep.join(str(p / 'bin') for p in (a, b))})
        self.assertEqual(nc.prefix, str(b))
        self.assertEqual(len(nc.skipped), 1)
        self.assertIn('NETCDF_LIBS', nc.skipped[0])

    def test_static_dependencies_with_spaces(self):
        prefix = self.tmp / 'with spaces'
        fake_install(self.tmp, 'with spaces', 'static', True, True,
                     static_libs=f'{shlex.quote("-L" + str(prefix / "lib"))} -lnetcdf -lz')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertEqual(nc.deps, ('-lz',))
        nc = find({'NETCDF': str(prefix), 'PATH': '', 'NETCDF_LIBS': "'-L/other path' -lz"})
        self.assertEqual(nc.deps, ('-L/other path', '-lz'))

    def test_link_check_compiles_runs_and_cleans(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lm')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        compiler = self.tmp / 'fake compiler'
        log = self.tmp / 'compile.json'
        program = '#!/bin/sh\nprintf \'CDF\\005\' > check.nc\n'
        compiler.write_text(
            f'#!{sys.executable}\nimport json, os, pathlib, sys\n'
            f'pathlib.Path({str(log)!r}).write_text(json.dumps({{"args": sys.argv[1:], "cwd": os.getcwd(), '
            '"source": pathlib.Path(sys.argv[1]).read_text()}))\n'
            'out = pathlib.Path(sys.argv[sys.argv.index("-o") + 1])\n'
            f'out.write_text({program!r})\n'
            'out.chmod(0o755)\n', encoding='utf-8')
        compiler.chmod(0o755)
        self.assertIsNone(netcdf.link_check(nc, str(compiler)))
        record = json.loads(log.read_text())
        self.assertEqual(record['args'][1:1 + len(link_args(nc))], link_args(nc))
        self.assertIn('iso_c_binding', record['source'].lower())
        self.assertIn('32_c_int', record['source'])
        self.assertFalse(Path(record['cwd']).exists())

    def test_link_check_reports_compiler_last_line(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lmissing')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        compiler = self.tmp / 'compiler'
        compiler.write_text('#!/bin/sh\necho first >&2\necho "ld: library not found for -lmissing" >&2\nexit 1\n')
        compiler.chmod(0o755)
        self.assertEqual(netcdf.link_check(nc, str(compiler)), 'ld: library not found for -lmissing')

    def test_link_check_reports_program_failure_or_bad_magic(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lm')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        for body, expected in (('echo first; echo "nc_create failed" >&2; exit 1', 'nc_create failed'),
                               ("printf 'CDF\\002' > check.nc", 'CDF-5'),
                               ('exit 0', 'check.nc')):
            with self.subTest(body=body):
                compiler = self.tmp / 'compiler'
                compiler.write_text('#!/bin/sh\nwhile [ "$1" != "-o" ]; do shift; done\nshift\n'
                                    f'printf "%s\\n" {shlex.quote("#!/bin/sh" + chr(10) + body)} > "$1"\n'
                                    'chmod +x "$1"\n')
                compiler.chmod(0o755)
                self.assertIn(expected, netcdf.link_check(nc, str(compiler)))

    def test_link_check_missing_compiler(self):
        prefix = fake_install(self.tmp, 'static', 'static', False, True, settings_extra='-lm')
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        self.assertIn('missing-compiler', netcdf.link_check(nc, str(self.tmp / 'missing-compiler')))

    def test_link_check_shared_does_nothing(self):
        prefix = fake_install(self.tmp, 'shared', 'shared', False, True)
        nc = find({'NETCDF': str(prefix), 'PATH': ''})
        with mock.patch('tools.lean.netcdf.subprocess.run') as run:
            self.assertIsNone(netcdf.link_check(nc, 'unused'))
        run.assert_not_called()

    @unittest.skipUnless(shutil.which('nc-config') or os.environ.get('NETCDF'), 'no netCDF-C install to check')
    def test_installed_netcdf_has_cdf5(self):
        nc = find()
        self.assertTrue(nc.cdf5)
        self.assertTrue(Path(nc.library).is_file(), nc.library)


class NetCDFRunnerTest(unittest.TestCase):
    """Exercise the shell runner's build-record plumbing and failure/cleanup contract without a compiler."""

    def setUp(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        tmp = tempfile.TemporaryDirectory(prefix='netcdf-runner-', dir=ROOT / 'build')
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        for name in ('dirname', 'basename', 'rm', 'mkdir', 'cat', 'sed', 'grep', 'tr'):
            (self.bin / name).symlink_to(shutil.which(name))
        (self.bin / 'python3').symlink_to(sys.executable)
        source = self.root / 'tests/netcdf'
        source.mkdir(parents=True)
        shutil.copyfile(ROOT / 'tests/netcdf/run.sh', source / 'run.sh')
        for name in ('check.f90', 'define.f90', 'files.f90', 'slices.f90'):
            (source / name).touch()
        self.build = self.root / 'build/model'
        self.build.mkdir(parents=True)
        self.out = self.root / 'build/netcdf'
        self.out.mkdir()
        (self.out / 'stale').touch()
        self.compiler = self.bin / 'recorded-compiler'
        self.executable(self.compiler, f'printf "%s\\n" "$*" >> {shlex.quote(str(self.root / "compiler.log"))}\n'
                        '[ -z "${COMPILER_FAIL:-}" ] || exit "$COMPILER_FAIL"\n'
                        'while [ "$#" -gt 0 ]; do\n'
                        '  case $1 in -c) exit 0;; -o) shift; program=$1;; esac\n'
                        '  shift\ndone\n'
                        'cat > "$program" <<\'PROGRAM\'\n'
                        '#!/bin/sh\n'
                        f'printf "%s\\n" "$0 $*" >> {shlex.quote(str(self.root / "program.log"))}\n'
                        'exit "${PROGRAM_FAIL:-0}"\n'
                        'PROGRAM\n'
                        '/bin/chmod +x "$program"\n')
        self.ncdump = self.root / 'netcdf/bin/ncdump'
        self.ncdump.parent.mkdir(parents=True)
        self.header_lines = ('float theta(Time, nCells, nVertLevels) ;', 'char xtime(Time, StrLen) ;',
                             'int ids(nCells) ;', 'double radius ;', 'theta:units = "K" ;',
                             ':sphere_radius = 6371229.f ;')
        header = ' '.join(shlex.quote(line) for line in self.header_lines)
        self.theta_dump = 'theta = 1101, 1102, 1201, 1202, 1301, 1302,\n  2101, 2102, 2201, 2202, 2301, 2302;'
        self.xtime_dump = 'xtime = "2026-10-11_06:00:00' + ' ' * 45 + '";'
        self.executable(self.ncdump, f'if [ "$1" = -h ]; then printf "%s\\n" {header}; exit 0; fi\n'
                        'if [ "$1" = -v ]; then\n'
                        f'  case $2 in theta) printf "%s\\n" {shlex.quote(self.theta_dump)};;\n'
                        f'    xtime) printf "%s\\n" {shlex.quote(self.xtime_dump)};; *) exit 1;; esac\n'
                        '  exit 0\nfi\n'
                        'case $2 in fmt_5.nc) echo cdf5;; fmt_2.nc) echo "64-bit offset";; '
                        'fmt_3.nc) echo netCDF-4;; *) exit 1;; esac\n')
        self.record = {'compiler': {'path': str(self.compiler)},
                       'netcdf': {'nc_config': str(self.ncdump.with_name('nc-config'))}}
        (self.build / 'build.json').write_text(json.dumps(self.record))
        (self.build / 'flags.txt').write_text('-O1 -DNETCDF_RUNNER\n')
        (self.build / 'libs.txt').write_text('-lnetcdf_runner\n')
        (self.build / 'moddirs.txt').write_text(str(self.build / 'mod/base') + '\n')
        self.object = str(self.build / 'CMakeFiles/base.dir/src/io/lean_nc.f90.o')
        (self.build / 'objects.txt').write_text(self.object + '\n/unused/main.f90.o\n')
        answer = json.dumps({'build_json': str(self.build / 'build.json')})
        self.executable(self.root / 'lean',
                        f'printf "%s\\n" "$*" > {shlex.quote(str(self.root / "lean.log"))}\n'
                        '[ -z "${BUILD_FAIL:-}" ] || exit "$BUILD_FAIL"\n'
                        f'printf "%s\\n" {shlex.quote(answer)}\n')

    def executable(self, path, text):
        path.write_text('#!/bin/sh\n' + text)
        path.chmod(0o755)

    def run_script(self, *args, **env):
        return subprocess.run(['/bin/sh', str(self.root / 'tests/netcdf/run.sh'), *args],
                              env={**os.environ, 'PATH': str(self.bin), **env}, capture_output=True, text=True)

    def test_runner_uses_build_record_and_adjacent_ncdump(self):
        self.executable(self.bin / 'ncdump', 'exit 9\n')
        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn('NETCDF TESTS OK', done.stdout)
        self.assertFalse(self.out.exists())
        self.assertEqual((self.root / 'lean.log').read_text().strip(),
                         'build --flavor mpas --target cpu --fc gfortran --jobs 2 --json')
        calls = (self.root / 'compiler.log').read_text().splitlines()
        self.assertEqual(len(calls), 7)
        self.assertTrue(all('-O1 -DNETCDF_RUNNER' in line for line in calls))
        self.assertIn('-I' + str(self.build / 'mod/base'), calls[0])
        self.assertTrue(calls[-1].endswith(self.object + ' -lnetcdf_runner'), calls[-1])
        self.assertNotIn('/unused/main.f90.o', calls[-1])
        self.assertEqual((self.root / 'program.log').read_text().split(), ['./define', './files', './slices'])

    def test_runner_path_ncdump_fallback(self):
        self.ncdump.rename(self.bin / 'ncdump')
        self.record['netcdf']['nc_config'] = ''
        (self.build / 'build.json').write_text(json.dumps(self.record))
        done = self.run_script('nvfortran', '1')
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn('--fc nvfortran --jobs 1 --json', (self.root / 'lean.log').read_text())
        self.assertFalse(self.out.exists())

    def test_runner_missing_ncdump_keeps_scratch(self):
        self.ncdump.unlink()
        done = self.run_script()
        self.assertEqual(done.returncode, 2, done.stdout + done.stderr)
        self.assertIn('ncdump missing', done.stdout)
        self.assertTrue((self.out / 'files').is_file())
        self.assertFalse((self.out / 'stale').exists())

    def test_runner_failures_exit_one_and_keep_scratch(self):
        for name in ('BUILD_FAIL', 'COMPILER_FAIL', 'PROGRAM_FAIL'):
            with self.subTest(stage=name):
                done = self.run_script(**{name: '2'})
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertTrue(self.out.is_dir())
                self.assertFalse((self.out / 'stale').exists())
                self.assertNotIn('NETCDF TESTS OK', done.stdout)

    def test_runner_ncdump_errors_and_mismatch_keep_scratch(self):
        for text in ('exit 2\n', 'echo wrong-format\n'):
            with self.subTest(ncdump=text):
                self.executable(self.ncdump, text)
                done = self.run_script()
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertTrue((self.out / 'files').is_file())
                self.assertNotIn('NETCDF TESTS OK', done.stdout)

    def test_runner_checks_each_header_line(self):
        original = self.ncdump.read_text()
        for line in self.header_lines:
            with self.subTest(missing=line):
                self.ncdump.write_text(original.replace(shlex.quote(line), "'wrong header'"))
                done = self.run_script()
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertIn('define_5.nc: ncdump -h: missing ' + line, done.stdout)
                self.assertTrue((self.out / 'define').is_file())
                self.assertNotIn('NETCDF TESTS OK', done.stdout)

    def test_runner_header_error_keeps_scratch(self):
        self.ncdump.write_text(self.ncdump.read_text().replace('if [ "$1" = -h ]; then',
                                                            'if [ "$1" = -h ]; then exit 2;'))
        done = self.run_script()
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertTrue((self.out / 'define').is_file())
        self.assertNotIn('NETCDF TESTS OK', done.stdout)

    def test_runner_checks_data_order_and_padding(self):
        original = self.ncdump.read_text()
        for text, bad, message in ((self.theta_dump, self.theta_dump.replace('1101, 1102', '1102, 1101'),
                                    'theta: wrong data order'),
                                   (self.xtime_dump, self.xtime_dump.replace(' ' * 45, ' ' * 44),
                                    'xtime: wrong blank padding')):
            with self.subTest(oracle=message):
                self.ncdump.write_text(original.replace(shlex.quote(text), shlex.quote(bad)))
                done = self.run_script()
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertIn('order_5.nc: ncdump -v ' + message, done.stdout)
                self.assertTrue((self.out / 'slices').is_file())
                self.assertNotIn('NETCDF TESTS OK', done.stdout)

    def test_runner_data_dump_error_keeps_scratch(self):
        original = self.ncdump.read_text()
        for variable in ('theta', 'xtime'):
            with self.subTest(variable=variable):
                self.ncdump.write_text(original.replace('if [ "$1" = -v ]; then',
                    f'if [ "$1" = -v ] && [ "$2" = {variable} ]; then exit 2; fi\n'
                    'if [ "$1" = -v ]; then'))
                done = self.run_script()
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertTrue((self.out / 'slices').is_file())
                self.assertNotIn('NETCDF TESTS OK', done.stdout)


class context_chdir:
    """Python 3.10 has no contextlib.chdir."""

    def __init__(self, path):
        self.path = path

    def __enter__(self):
        self.previous = Path.cwd()
        os.chdir(self.path)

    def __exit__(self, *exc):
        os.chdir(self.previous)


if __name__ == '__main__':
    unittest.main()
