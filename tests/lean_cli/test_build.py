"""Tests for ./lean build (tools/lean/build.py), with fake compiler, CMake and Ninja on PATH: nothing is compiled.

The fakes record each call (its arguments) in calls.log next to them. The build
directory goes under a temporary root (build.ROOT is patched); the sources are the repo's own.
"""
import contextlib
import hashlib
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path
from unittest import mock

from tests.lean_cli.test_netcdf import FAKE, context_chdir, fake_install, fake_nc_config
from tools.lean import build, cli, config, generate, netcdf, tables
from tools.lean.config import BuildConfig, build_dir_name, link_flags

ROOT = Path(__file__).resolve().parents[2]
CPU = BuildConfig('cpu', 'gfortran', 'strict')
NAME = 'cpu-gfortran-strict'

RECORD = 'printf "%s ARGS=%s\\n" "${0##*/}" "$*" >> "${0%/*}/calls.log"\n'
FAKE_GFORTRAN = ('#!/bin/sh\n' + RECORD +
                 'case $1 in -dumpversion) echo {version};; --version) echo "GNU Fortran (fake) {version}";; esac\n')
FAKE_CMAKE = ('#!/bin/sh\n' + RECORD +
              'if [ "$1" = --version ]; then echo "cmake version 3.31.2"; exit 0; fi\n'
              'while [ $# -gt 0 ]; do if [ "$1" = -B ]; then mkdir -p "$2"; fi; shift; done\n')
FAKE_NINJA = ('#!/bin/sh\n' + RECORD +
              'd=.; mode=build\n'
              'while [ $# -gt 0 ]; do\n'
              '  case $1 in --version) echo 1.12.1; exit 0;; -C) d=$2; shift;; -t) mode=$2; shift;; esac\n'
              '  shift\n'
              'done\n'
              'case $mode in\n'
              '  query) printf "lean_mpas:\\n  input: Fortran_EXECUTABLE_LINKER__lean_mpas_\\n'
              '    CMakeFiles/base.dir/src/a.f90.o\\n    CMakeFiles/model.dir/src/b.f90.o\\n    | implicit.dep\\n'
              '  outputs:\\n";;\n'
              '  commands) printf "gfortran -O2 -c a.f90\\ngfortran -O2 -c b.f90\\n";;\n'
              '  build) : > "$d/lean_mpas";;\n'
              'esac\n')


class FakeTools:
    """A temporary root with data/tables and NoahmpTable.TBL, and a bin directory of fake tools."""

    def __init__(self, tmp: Path, gfortran_version='15.2.0', cdf5=True):
        self.root = tmp / 'root'
        self.bin = tmp / 'bin'
        self.bin.mkdir()
        for name, text in (('gfortran', FAKE_GFORTRAN.replace('{version}', gfortran_version)),
                           ('cmake', FAKE_CMAKE), ('ninja', FAKE_NINJA)):
            path = self.bin / name
            path.write_text(text, encoding='utf-8')
            path.chmod(0o755)
        self.nc_config = fake_nc_config(tmp, '', '4.9.3', cdf5)
        (self.root / 'data' / 'tables').mkdir(parents=True)
        (self.root / 'data' / 'tables' / 'A.DBL').write_bytes(b'table a')
        (self.root / 'data' / 'tables' / 'B.DBL').write_bytes(b'table b, cut')   # fails its sha256: never linked
        self.table_lock = {'A.DBL': {'sha256': hashlib.sha256(b'table a').hexdigest(), 'size': 7},
                           'B.DBL': {'sha256': hashlib.sha256(b'table b').hexdigest(), 'size': 7}}
        (self.root / 'reference' / 'config').mkdir(parents=True)
        (self.root / 'reference' / 'config' / 'NoahmpTable.TBL').write_bytes(b'noahmp')
        self.build_dir = self.root / 'build' / NAME

    def calls(self, tool: str) -> list[str]:
        log = self.bin / 'calls.log'
        lines = log.read_text(encoding='utf-8').splitlines() if log.is_file() else []
        return [line for line in lines if line.startswith(tool + ' ')]

    def run(self, cfg=CPU, path_only=False, env=None, **kwargs) -> tuple[int, str]:
        path = str(self.bin) if path_only else f'{self.bin}{os.pathsep}{os.environ["PATH"]}'
        out = io.StringIO()
        new_env = {key: value for key, value in os.environ.items() if key not in ('NETCDF', 'NETCDF_LIBS')}
        new_env.update({'PATH': path, **(env or {})})
        with mock.patch.dict(os.environ, new_env, clear=True), mock.patch.object(build, 'ROOT', self.root), \
                mock.patch.object(tables, 'ROOT', self.root), mock.patch.object(generate, 'ROOT', self.root), \
                mock.patch.object(tables, 'expected', return_value=self.table_lock), \
                contextlib.redirect_stdout(out), contextlib.redirect_stderr(io.StringIO()) as err:
            status = build.build(cfg, **kwargs)
        self.stderr = err.getvalue()
        return status, out.getvalue()


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def resolve_config(*args: str) -> BuildConfig:
    with mock.patch.object(build, 'build', return_value=0) as compile_build, \
            contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()) as err:
        status = cli.main(['build', *args, '--jobs', '2'])
    if status != 0:
        raise AssertionError(err.getvalue())
    compile_build.assert_called_once()
    return compile_build.call_args.args[0]


class BuildDirectoryTest(unittest.TestCase):

    def test_build_dir_differs_per_switch_set(self):
        lean = resolve_config('--flavor', 'lean')
        dew_off = resolve_config('--flavor', 'lean', '--switch', 'LEAN_MYNN_DEW=off')
        self.assertNotEqual(build_dir_name(lean), build_dir_name(dew_off))
        self.assertEqual(build_dir_name(CPU), NAME)

    def test_build_kind_overrides_have_separate_directories(self):
        lean = resolve_config('--flavor', 'lean')
        gpu = resolve_config('--flavor', 'lean', '--target', 'gpu')
        debug = resolve_config('--flavor', 'lean', '--profile', 'debug')
        names = {build_dir_name(cfg) for cfg in (lean, gpu, debug)}
        self.assertEqual(len(names), 3)
        self.assertEqual(build_dir_name(lean), build_dir_name(resolve_config('--flavor', 'lean')))

    def test_hash_includes_each_build_kind_field(self):
        cfg = resolve_config('--flavor', 'lean', '--switch', 'LEAN_MYNN_DEW=off', '--target', 'cpu', '--fc', 'nvfortran')
        envelope = replace(cfg, fc='gfortran', profile='envelope', variant='o3')
        pairs = (
            ('target', cfg, replace(cfg, target='gpu')),
            ('fc', cfg, replace(cfg, fc='gfortran')),
            ('profile', cfg, replace(cfg, profile='debug')),
            ('variant', envelope, replace(envelope, variant='o3native')),
            ('omp', cfg, replace(cfg, omp=True)),
        )
        for field, first, second in pairs:
            with self.subTest(field=field):
                self.assertEqual(sorted(config.global_defines(first)), sorted(config.global_defines(second)))
                self.assertNotEqual(build_dir_name(first).rsplit('-', 1)[-1],
                                    build_dir_name(second).rsplit('-', 1)[-1])

    def test_build_dir_hashes_build_kind_and_sorted_global_defines(self):
        cfg = resolve_config('--flavor', 'lean', '--switch', 'LEAN_MYNN_DEW=off')
        identity = [cfg.target, cfg.fc, cfg.profile, cfg.variant, cfg.omp, sorted(config.global_defines(cfg))]
        digest = hashlib.sha256(json.dumps(identity, separators=(',', ':')).encode('utf-8')).hexdigest()[:8]
        self.assertEqual(build_dir_name(cfg), f'cpu-gfortran-strict-{digest}')

    def test_hash_ignores_define_order_and_unit_rules(self):
        cfg = resolve_config('--flavor', 'lean', '--switch', 'LEAN_MYNN_DEW=off')
        original = build_dir_name(cfg)
        flags = config.load_flags()
        flags[cfg.fc]['defines'].reverse()
        flags['unit_rules'].append({'when': {}, 'units': ['config'], 'defines': ['TEST_UNIT_ONLY']})
        with mock.patch.object(config, 'load_flags', return_value=flags):
            self.assertEqual(build_dir_name(cfg), original)
        flags[cfg.fc]['defines'].append('TEST_GLOBAL')
        with mock.patch.object(config, 'load_flags', return_value=flags):
            self.assertNotEqual(build_dir_name(cfg), original)

    def test_switches_have_separate_directories(self):
        self.assertEqual(build_dir_name(replace(CPU, switches=(('LEAN_MYNN_DEW', 'off'),))), NAME)
        cfg = replace(CPU, switches=(('LEAN_MYNN_DEW', 'on'),))
        self.assertNotEqual(build_dir_name(cfg), NAME)
        self.assertRegex(build_dir_name(cfg), r'^cpu-gfortran-strict-[0-9a-f]{8}$')
        first = replace(CPU, switches=(('LEAN_GF_DXMIN', '4000'),))
        second = replace(first, switches=(('LEAN_GF_DXMIN', '2500'),))
        self.assertNotEqual(build_dir_name(first), build_dir_name(second))

    def test_flavor_names(self):
        mpas = resolve_config('--flavor', 'mpas')
        self.assertEqual(mpas, CPU)
        self.assertEqual(build_dir_name(mpas), NAME)
        for omp in (False, True):
            with self.subTest(omp=omp):
                lean = resolve_config('--flavor', 'lean', *(['--omp'] if omp else []))
                self.assertEqual(build_dir_name(lean), f'cpu-gfortran-strict{"-omp" if omp else ""}-lean')
                budget = replace(lean, switches=lean.switches + (('LEAN_BUDGET', 'on'),))
                self.assertRegex(build_dir_name(budget),
                                 rf'^cpu-gfortran-strict{"-omp" if omp else ""}-[0-9a-f]{{8}}$')

    def test_cli_passes_link(self):
        with mock.patch.object(build, 'build', return_value=0) as compile_build, \
                contextlib.redirect_stderr(io.StringIO()) as err:
            status = cli.main(['build', '--link', 'lean_gpu', '--flavor', 'lean', '--target', 'gpu', '--jobs', '2'])
        self.assertEqual(status, 0, err.getvalue())
        self.assertEqual(compile_build.call_args.kwargs['link'], 'lean_gpu')
        self.assertEqual(compile_build.call_args.args[0].target, 'gpu')


class CmakeArgsTest(unittest.TestCase):

    def test_cmake_args(self):
        cfg = BuildConfig('gpu', 'nvfortran', 'strict')
        self.assertEqual(build.cmake_args(cfg, Path('/repo'), '/opt/nv/bin/nvfortran'), [
            'cmake', '-G', 'Ninja', '-S', '/repo', '-B', '/repo/build/gpu-nvfortran-strict',
            '-DCMAKE_Fortran_COMPILER=/opt/nv/bin/nvfortran', f'-DPython3_EXECUTABLE={sys.executable}',
            '-DLEAN_TARGET=gpu', '-DLEAN_FC=nvfortran', '-DLEAN_PROFILE=strict', '-DLEAN_OMP=OFF', '-DLEAN_VARIANT='])

    def test_cmake_args_omp_and_variant(self):
        cfg = BuildConfig('cpu', 'gfortran', 'fast', omp=True, variant='v1')
        args = build.cmake_args(cfg, Path('/repo'), 'gfortran')
        self.assertIn('-B', args)
        self.assertEqual(args[args.index('-B') + 1], '/repo/build/cpu-gfortran-fast-v1-omp')
        self.assertEqual(args[-1:], ['-DLEAN_VARIANT=v1'])
        self.assertIn('-DLEAN_OMP=ON', args)

    def test_cmake_args_levels_pass_the_config_file(self):
        args = build.cmake_args(BuildConfig('cpu', 'gfortran', 'strict', levels=45), Path('/repo'), 'gfortran')
        self.assertEqual(args[-1], '-DLEAN_CONFIG_FILE=/repo/build/cpu-gfortran-strict-levels45/lean-config.json')


class BuildTest(unittest.TestCase):

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.tools = FakeTools(Path(self.tmp.name).resolve())

    def test_build_runs_doctor_first(self):
        old = Path(self.tmp.name).resolve() / 'old'
        old.mkdir()
        tools = FakeTools(old, gfortran_version='12.2.0')
        status, out = tools.run(path_only=True)
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_FC_OLD', out)
        configure_calls = [call for call in tools.calls('cmake') if not call.endswith('ARGS=--version')]
        self.assertEqual(configure_calls, [])
        self.assertEqual([call for call in tools.calls('ninja') if not call.endswith('ARGS=--version')], [])
        self.assertFalse(tools.build_dir.exists())

    def test_build_stops_without_cdf5_netcdf(self):
        fake_nc_config(Path(self.tmp.name).resolve(), '', '4.9.2', False)
        status, out = self.tools.run(path_only=True)
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_NETCDF_CDF5', out)
        configure_calls = [call for call in self.tools.calls('cmake') if not call.endswith('ARGS=--version')]
        self.assertEqual(configure_calls, [])
        self.assertEqual([call for call in self.tools.calls('ninja') if not call.endswith('ARGS=--version')], [])
        self.assertFalse(self.tools.build_dir.exists())

    def test_fake_tools_ignores_inherited_netcdf(self):
        inherited = str(Path(self.tmp.name) / 'missing')

        def check_environment(*args, **kwargs):
            self.assertIsNone(os.environ.get('NETCDF'))
            return []

        with mock.patch.dict(os.environ, {'NETCDF': inherited}), \
                mock.patch.object(build, 'doctor_failures', side_effect=check_environment):
            status, out = self.tools.run()
            self.assertEqual(os.environ['NETCDF'], inherited)
        self.assertEqual(status, 0, out)

    def test_fake_tools_keeps_explicit_netcdf(self):
        nc_config = fake_nc_config(Path(self.tmp.name).resolve(), 'explicit', '4.9.2', False)
        status, out = self.tools.run(path_only=True, env={'NETCDF': str(nc_config.parent.parent)})
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_NETCDF_CDF5', out)
        self.assertIn('NETCDF=', out)

    def test_build_fails_without_ninja(self):
        (self.tools.bin / 'ninja').unlink()
        status, out = self.tools.run(path_only=True)
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_NO_NINJA', out)
        self.assertIn('brew install ninja', out)

    def test_build_json_fields(self):
        stale = self.tools.build_dir / 'OLD.DBL'
        self.tools.build_dir.mkdir(parents=True)
        stale.symlink_to(self.tools.root / 'gone.DBL')
        compiler = str(self.tools.bin / 'gfortran')
        status, out = self.tools.run(jobs=1)
        self.assertEqual(status, 0, out)
        self.assertEqual(out.strip().splitlines()[-1], f'BUILD OK build/{NAME}/lean_mpas')

        data = json.loads((self.tools.build_dir / 'build.json').read_text(encoding='utf-8'))
        self.assertEqual(data['config'], {'target': 'cpu', 'fc': 'gfortran', 'profile': 'strict', 'omp': False,
                                          'variant': None, 'switches': [], 'gpu_cc': None, 'levels': None})
        self.assertNotIn('preset', data)
        self.assertNotIn('mesh', data)
        self.assertEqual(data['switches'], {})
        self.assertEqual(data['defines'], ['mpas', 'SINGLE_PRECISION'])
        self.assertEqual(data['build_dir'], f'build/{NAME}')
        self.assertEqual(data['compiler'], {'path': compiler, 'version': 'GNU Fortran (fake) 15.2.0'})
        self.assertEqual(data['cmake'], '3.31.2')
        self.assertEqual(data['ninja'], '1.12.1')
        self.assertIn('commit', data)
        self.assertIn('dirty', data)
        self.assertEqual(data['link'], link_flags(CPU))
        self.assertEqual(data['cmake_added']['module_dir'], '-J<dir>')
        self.assertTrue(data['cmake_added']['why'])

        by_path = {entry['path']: entry for entry in data['sources']}
        self.assertEqual(len(by_path), len(data['sources']))
        self.assertEqual(len(data['sources']), len(config.compile_units(CPU, self.tools.build_dir)))
        entry = by_path['src/config.f90']
        options, defines = config.unit_flags(CPU, 'config', 'base')
        self.assertEqual(entry, {'path': 'src/config.f90', 'sha256': sha256(ROOT / 'src/config.f90'),
                                 'group': 'base', 'options': options, 'defines': defines})
        for entry in data['sources']:
            self.assertEqual(set(entry), {'path', 'sha256', 'group', 'options', 'defines'})
        split = [path for path in by_path if path.endswith('/noahmp_lean/NoahmpMainMod.f90')]
        self.assertEqual(len(split), 1, 'the split Noah-MP units are listed by their path in the build directory')

        self.assertEqual(data['tables'], [{'name': 'A.DBL', 'sha256': sha256(self.tools.root / 'data/tables/A.DBL')},
                                          {'name': 'NoahmpTable.TBL',
                                           'sha256': sha256(self.tools.root / 'reference/config/NoahmpTable.TBL')}])
        self.assertEqual((self.tools.build_dir / 'A.DBL').resolve(), self.tools.root / 'data/tables/A.DBL')
        self.assertEqual((self.tools.build_dir / 'NoahmpTable.TBL').resolve(),
                         self.tools.root / 'reference/config/NoahmpTable.TBL')
        self.assertFalse(stale.is_symlink(), 'a table link that is not verified any more is removed')
        self.assertFalse((self.tools.build_dir / 'B.DBL').exists(), 'a table that fails its sha256 is not linked')

        objects = (self.tools.build_dir / 'objects.txt').read_text(encoding='utf-8').splitlines()
        self.assertEqual(objects, [str(self.tools.build_dir / 'CMakeFiles/base.dir/src/a.f90.o'),
                                   str(self.tools.build_dir / 'CMakeFiles/model.dir/src/b.f90.o')])
        moddirs = (self.tools.build_dir / 'moddirs.txt').read_text(encoding='utf-8').splitlines()
        groups = config.load_units()['groups']
        self.assertEqual(moddirs, [str(self.tools.build_dir / spec['module_dir']) for spec in groups.values()])
        options, defines = config.unit_flags(CPU, 'config', 'model')
        self.assertEqual((self.tools.build_dir / 'flags.txt').read_text(encoding='utf-8'),
                         ' '.join(options + ['-D' + name for name in defines]) + '\n')

        configure = [call for call in self.tools.calls('cmake') if not call.endswith('ARGS=--version')]
        self.assertEqual(configure,
                         ['cmake ARGS=' + ' '.join(build.cmake_args(CPU, self.tools.root, compiler)[1:])])
        self.assertIn(f'ninja ARGS=-C {self.tools.build_dir} -j 1 lean_mpas', self.tools.calls('ninja'))

    def test_build_json_records_netcdf(self):
        nc = netcdf.find({'PATH': str(self.tools.bin)})
        status, out = self.tools.run(jobs=2)
        self.assertEqual(status, 0, out)
        data = json.loads((self.tools.build_dir / 'build.json').read_text())
        self.assertEqual(data['netcdf']['version'], '4.9.3')
        self.assertTrue(data['netcdf']['library'].endswith(netcdf.LIBRARY))
        self.assertEqual(data['netcdf']['library'], nc.library)
        self.assertEqual(data['netcdf']['source'], 'PATH')
        self.assertEqual(data['netcdf']['skipped'], [])
        self.assertEqual(data['netcdf']['deps'], [])
        self.assertIs(data['netcdf']['default_libdir'], False)
        self.assertEqual(data['link_libraries'], netcdf.link_args(nc))
        self.assertEqual((self.tools.build_dir / 'libs.txt').read_text(), ' '.join(netcdf.link_args(nc)) + '\n')
        logline = f'lean build: netCDF-C 4.9.3 in {nc.libdir}\n'
        self.assertEqual(self.tools.stderr.count(logline), 1)
        self.assertLess(self.tools.stderr.index(logline), self.tools.stderr.index('lean build: configure '))
        fake_nc_config(Path(self.tmp.name).resolve(), '', '4.9.3', False)
        status, out = self.tools.run(path_only=True, jobs=2)
        self.assertEqual(status, 1, out)
        self.assertIn('BUILD FAIL: E_NETCDF_CDF5', out)
        for name in ('build.json', 'objects.txt', 'moddirs.txt', 'flags.txt', 'libs.txt'):
            self.assertFalse((self.tools.build_dir / name).exists(), name)

    def test_build_json_records_static(self):
        prefix = fake_install(Path(self.tmp.name).resolve(), 'static', 'static', True, True,
                              static_libs='-lnetcdf -lhdf5_hl -lhdf5 -lz')
        env = {'NETCDF': str(prefix), 'NETCDF_LIBS': '-lhdf5_hl -lhdf5 -lz'}
        nc = netcdf.find(env)
        with mock.patch.object(netcdf, 'link_check', return_value=None):
            status, out = self.tools.run(jobs=2, env=env)
        self.assertEqual(status, 0, out)
        data = json.loads((self.tools.build_dir / 'build.json').read_text())
        self.assertEqual(data['netcdf']['kind'], 'static')
        self.assertEqual(data['netcdf']['deps'], ['-lhdf5_hl', '-lhdf5', '-lz'])
        self.assertEqual(data['link_libraries'], netcdf.link_args(nc))
        libs = (self.tools.build_dir / 'libs.txt').read_text()
        self.assertEqual(libs, ' '.join(netcdf.link_args(nc)) + '\n')
        self.assertTrue(libs.startswith(str(prefix / 'lib/libnetcdf.a')))
        self.assertNotIn('-Wl,-rpath', libs)

    def test_build_pins_netcdf_for_configure_and_ninja(self):
        prefix = fake_nc_config(Path(self.tmp.name).resolve(), 'chosen', '4.9.3', True).parent.parent
        run_logged = build._run_logged
        for env, expected in (({}, self.tools.bin.parent), ({'NETCDF': 'chosen'}, prefix)):
            with self.subTest(env=env), context_chdir(self.tmp.name), \
                    mock.patch.object(build, '_run_logged', wraps=run_logged) as run:
                status, out = self.tools.run(jobs=2, env=env)
            self.assertEqual(status, 0, out)
            self.assertEqual(len(run.call_args_list), 2)
            for call in run.call_args_list:
                self.assertEqual(call.args[2]['NETCDF'], str(expected))
                self.assertTrue(Path(call.args[2]['NETCDF']).is_absolute())
            data = json.loads((self.tools.build_dir / 'build.json').read_text())
            self.assertEqual(data['netcdf']['prefix'], str(expected))

    def test_dry_run_json_prints_commands(self):
        status, out = self.tools.run(dry_run=True, as_json=True)
        self.assertEqual(status, 0, out)
        self.assertEqual(json.loads(out), ['gfortran -O2 -c a.f90', 'gfortran -O2 -c b.f90'])
        self.assertEqual(len([call for call in self.tools.calls('cmake') if '-G Ninja' in call]), 1)
        self.assertFalse([call for call in self.tools.calls('ninja') if ' -j ' in call], 'a dry run builds nothing')
        self.assertFalse((self.tools.build_dir / 'build.json').exists())

    def test_flags_txt_has_defines(self):
        cfg = resolve_config('--flavor', 'lean', '--switch', 'LEAN_GF_NOSHALLOW=off')
        status, out = self.tools.run(cfg=cfg)
        self.assertEqual(status, 0, out)
        folder = self.tools.root / 'build' / build_dir_name(cfg)
        flags = (folder / 'flags.txt').read_text(encoding='utf-8')
        self.assertIn('-DLEAN_MYNN_DEW', flags.split())
        self.assertNotIn('-DLEAN_GF_NOSHALLOW', flags.split())
        options, defines = config.unit_flags(cfg, 'config', 'model')
        self.assertEqual(flags, ' '.join(options + ['-D' + name for name in defines]) + '\n')
        serialized = json.loads((folder / 'lean-config.json').read_text(encoding='utf-8'))
        self.assertEqual(serialized['switches'], [list(item) for item in cfg.switches])

    def test_published_build_refused_before_records_or_checks(self):
        cfg = resolve_config('--flavor', 'lean', '--target', 'gpu')
        folder = self.tools.root / 'build' / build_dir_name(cfg)
        folder.mkdir(parents=True)
        records = ('build.json', 'objects.txt', 'moddirs.txt', 'flags.txt', 'libs.txt', 'lean_mpas')
        for name in records:
            (folder / name).write_text('keep ' + name)
        alias = folder.parent / 'lean_gpu'
        alias.symlink_to(folder.name, target_is_directory=True)
        with mock.patch.object(build, 'device_drift', side_effect=build.BuildError('E_DEVICE_DRIFT', 'tripwire')) as drift:
            status, out = self.tools.run(cfg=cfg, compiler='/another/compiler')
        self.assertEqual(status, 1, out)
        self.assertIn('BUILD FAIL: E_PUBLISHED', out)
        self.assertIn(f'build/{alias.name} is published', out)
        drift.assert_not_called()
        for name in records:
            self.assertEqual((folder / name).read_text(), 'keep ' + name)
        self.assertEqual(alias.resolve(), folder)
        self.assertEqual(self.tools.calls('cmake'), [])
        self.assertEqual(self.tools.calls('ninja'), [])

    def test_published_directory_requires_its_own_link_name(self):
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        self.tools.build_dir.mkdir(parents=True)
        alias = self.tools.build_dir.parent / 'program-live'
        alias.symlink_to(self.tools.build_dir, target_is_directory=True)
        (self.tools.build_dir / 'flags.txt').write_text('keep flags')
        status, out = self.tools.run(link='program-other')
        self.assertEqual(status, 1, out)
        self.assertIn('BUILD FAIL: E_PUBLISHED', out)
        self.assertEqual((self.tools.build_dir / 'flags.txt').read_text(), 'keep flags')
        self.assertFalse((alias.parent / 'program-other').exists())
        self.assertEqual(self.tools.calls('cmake'), [])

    def test_second_published_alias_also_needs_authorization(self):
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        self.tools.build_dir.mkdir(parents=True)
        aliases = [self.tools.build_dir.parent / name for name in ('program-live', 'program-second')]
        for alias in aliases:
            alias.symlink_to(self.tools.build_dir.name, target_is_directory=True)
        status, out = self.tools.run(link=aliases[0].name)
        self.assertEqual(status, 1, out)
        self.assertIn(f'BUILD FAIL: E_PUBLISHED build/{aliases[1].name}', out)
        self.assertEqual(self.tools.calls('cmake'), [])

    def test_published_directory_allows_matching_link_and_dry_run(self):
        self.tools.build_dir.mkdir(parents=True)
        alias = self.tools.build_dir.parent / 'program-live'
        alias.symlink_to(self.tools.build_dir.name, target_is_directory=True)
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        for options in ({'dry_run': True}, {'dry_run': True, 'link': alias.name}, {'link': alias.name}):
            with self.subTest(options=options):
                status, out = self.tools.run(**options)
                self.assertEqual(status, 0, out)
                self.assertEqual(alias.resolve(), self.tools.build_dir)

    def test_link_published_only_after_success(self):
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        name = 'lean_gpu'
        original = build.link_build

        def publish(folder, alias):
            self.assertTrue((folder / 'lean_mpas').is_file())
            self.assertTrue((folder / 'build.json').is_file())
            self.assertTrue((folder / 'flags.txt').is_file())
            original(folder, alias)

        with mock.patch.object(build, 'link_build', side_effect=publish) as publish_link:
            status, out = self.tools.run(link=name)
        self.assertEqual(status, 0, out)
        publish_link.assert_called_once_with(self.tools.build_dir, name)
        self.assertEqual((self.tools.root / 'build' / name).resolve(), self.tools.build_dir)

    def test_dry_run_does_not_publish_link_or_flags(self):
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        alias = self.tools.root / 'build' / 'program-old'
        alias.parent.mkdir()
        alias.symlink_to('earlier')
        status, out = self.tools.run(link=alias.name, dry_run=True)
        self.assertEqual(status, 0, out)
        self.assertEqual(alias.readlink(), Path('earlier'))
        self.assertFalse((self.tools.build_dir / 'flags.txt').exists())
        self.assertFalse(alias.with_name(alias.name + '.prev').exists())

    def test_failed_build_does_not_publish_link_or_keep_flags(self):
        (self.tools.root / 'data/tables/B.DBL').write_bytes(b'table b')
        alias = self.tools.root / 'build' / 'program-old'
        alias.parent.mkdir()
        alias.symlink_to('earlier')
        self.tools.build_dir.mkdir()
        (self.tools.build_dir / 'flags.txt').write_text('old flags\n', encoding='utf-8')
        (self.tools.bin / 'ninja').write_text('#!/bin/sh\n[ "$1" = --version ] && echo 1.12.1 && exit 0\n'
                                           'exit 1\n', encoding='utf-8')
        status, out = self.tools.run(link=alias.name)
        self.assertEqual(status, 1, out)
        self.assertIn('BUILD FAIL: E_COMPILE', out)
        self.assertEqual(alias.readlink(), Path('earlier'))
        self.assertFalse((self.tools.build_dir / 'flags.txt').exists())

    def test_link_requires_complete_tables_before_mutating_build(self):
        self.tools.build_dir.mkdir(parents=True)
        alias = self.tools.build_dir.parent / 'program-live'
        records = ('build.json', 'objects.txt', 'moddirs.txt', 'flags.txt', 'libs.txt', 'lean_mpas')
        table = self.tools.root / 'data/tables/B.DBL'
        for state in ('missing', 'bad'):
            with self.subTest(state=state):
                for name in records:
                    (self.tools.build_dir / name).write_text('keep ' + name)
                alias.unlink(missing_ok=True)
                alias.symlink_to('earlier', target_is_directory=True)
                (self.tools.bin / 'calls.log').unlink(missing_ok=True)
                if state == 'missing':
                    table.unlink(missing_ok=True)
                else:
                    table.write_bytes(b'wrong b')  # right size, wrong sha256
                status, out = self.tools.run(link=alias.name)
                self.assertEqual(status, 1, out)
                self.assertIn('BUILD FAIL: E_NO_TABLES', out)
                self.assertIn('B.DBL', out)
                self.assertIn('./lean tables --from PATH|URL', out)
                self.assertEqual(alias.readlink(), Path('earlier'))
                for name in records:
                    self.assertEqual((self.tools.build_dir / name).read_text(), 'keep ' + name)
                self.assertEqual(self.tools.calls('cmake'), [])
                self.assertEqual(self.tools.calls('ninja'), [])

    def test_link_tables_checked_before_published_guard_and_device(self):
        cfg = resolve_config('--flavor', 'lean', '--target', 'gpu')
        folder = self.tools.root / 'build' / build_dir_name(cfg)
        folder.mkdir(parents=True)
        (folder / 'build.json').write_text('keep record')
        (folder.parent / 'program-live').symlink_to(folder.name, target_is_directory=True)
        with mock.patch.object(build, 'device_drift') as drift, \
                mock.patch.object(build, 'doctor_failures') as doctor_checks:
            status, out = self.tools.run(cfg=cfg, link='program-other')
        self.assertEqual(status, 1, out)
        self.assertIn('BUILD FAIL: E_NO_TABLES', out)
        drift.assert_not_called()
        doctor_checks.assert_not_called()
        self.assertEqual((folder / 'build.json').read_text(), 'keep record')

    def test_dry_run_with_link_requires_complete_tables(self):
        (self.tools.root / 'data/tables/B.DBL').unlink()
        status, out = self.tools.run(link='program-live', dry_run=True, as_json=True)
        self.assertEqual(status, 1, out)
        self.assertEqual(json.loads(out)['code'], 'E_NO_TABLES')
        self.assertIn('./lean tables --from PATH|URL', json.loads(out)['detail'])
        self.assertEqual(self.tools.calls('cmake'), [])
        self.assertEqual(self.tools.calls('ninja'), [])
        self.assertFalse(self.tools.build_dir.exists())

    def test_compiler_option_overrides_the_compiler(self):
        other = self.tools.bin / 'my-gfortran'
        other.symlink_to(self.tools.bin / 'gfortran')
        status, out = self.tools.run(compiler=str(other))
        self.assertEqual(status, 0, out)
        data = json.loads((self.tools.build_dir / 'build.json').read_text(encoding='utf-8'))
        self.assertEqual(data['compiler']['path'], str(other))
        configure = [call for call in self.tools.calls('cmake') if '-G Ninja' in call]
        self.assertIn(f'-DCMAKE_Fortran_COMPILER={other} ', configure[0])

    def test_another_compiler_configures_fresh(self):
        """CMake drops every -D value when the cached compiler changes ('you may have to reset some variables'),
        so a build with another compiler in the same directory must start from an empty cache (--fresh)."""
        compiler = self.tools.bin / 'gfortran'
        self.tools.build_dir.mkdir(parents=True)
        cache = self.tools.build_dir / 'CMakeCache.txt'
        for cached, fresh in (('/somewhere/else/gfortran', True), (str(compiler), False)):
            with self.subTest(cached=cached):
                cache.write_text(f'# comment\nCMAKE_Fortran_COMPILER:STRING={cached}\n', encoding='utf-8')
                (self.tools.bin / 'calls.log').unlink(missing_ok=True)
                status, out = self.tools.run()
                self.assertEqual(status, 0, out)
                configure = [call for call in self.tools.calls('cmake') if '-G Ninja' in call]
                self.assertEqual(len(configure), 1)
                self.assertEqual('ARGS=--fresh -G Ninja ' in configure[0], fresh, configure[0])

    def test_failed_configure_leaves_no_build_json(self):
        (self.tools.bin / 'cmake').write_text('#!/bin/sh\n[ "$1" = --version ] && echo "cmake version 3.31.2" && '
                                              'exit 0\nexit 1\n', encoding='utf-8')
        self.tools.build_dir.mkdir(parents=True)
        (self.tools.build_dir / 'build.json').write_text('{}', encoding='utf-8')
        (self.tools.build_dir / 'libs.txt').write_text('old libraries\n', encoding='utf-8')
        status, out = self.tools.run()
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_CONFIGURE', out)
        self.assertFalse((self.tools.build_dir / 'build.json').exists())
        self.assertFalse((self.tools.build_dir / 'libs.txt').exists())

    def test_failed_ninja_is_build_fail_and_leaves_no_build_json(self):
        (self.tools.bin / 'ninja').write_text('#!/bin/sh\n[ "$1" = --version ] && echo 1.12.1 && exit 0\n'
                                              'case "$*" in *-j*) exit 1;; esac\n', encoding='utf-8')
        self.tools.build_dir.mkdir(parents=True)
        (self.tools.build_dir / 'build.json').write_text('{}', encoding='utf-8')
        (self.tools.build_dir / 'libs.txt').write_text('old libraries\n', encoding='utf-8')
        status, out = self.tools.run()
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_COMPILE', out)
        self.assertFalse((self.tools.build_dir / 'build.json').exists())
        self.assertFalse((self.tools.build_dir / 'libs.txt').exists())

    def test_gpu_build_checks_device_drift(self):
        from tests.lean_cli.test_generate import flip_one_byte, scratch_copy
        scratch_copy(self.tools.root)
        flip_one_byte(self.tools.root / 'src/device/gf_core.f90')
        nvfortran = self.tools.bin / 'nvfortran'
        nvfortran.write_text('#!/bin/sh\n' + RECORD + 'echo; echo "nvfortran 25.3-0 64-bit target on x86-64 Linux"\n',
                             encoding='utf-8')
        nvfortran.chmod(0o755)
        cfg = BuildConfig('gpu', 'nvfortran', 'strict')
        earlier = self.tools.root / 'build' / build_dir_name(cfg)
        earlier.mkdir(parents=True)
        for name in ('build.json', 'objects.txt', 'moddirs.txt'):
            (earlier / name).write_text('the record of an earlier build\n', encoding='utf-8')
        status, out = self.tools.run(cfg=cfg)
        self.assertEqual(status, 1)
        self.assertIn('BUILD FAIL: E_DEVICE_DRIFT', out)
        self.assertEqual(sorted(path.name for path in earlier.iterdir()), [], 'the earlier record is gone')
        self.assertIn('src/device/gf_core.f90', out)
        self.assertEqual(self.tools.calls('cmake'), [], 'cmake is never called')
        self.assertEqual(self.tools.calls('ninja'), [])

    def test_json_verdict(self):
        status, out = self.tools.run(as_json=True)
        self.assertEqual(status, 0)
        self.assertEqual(json.loads(out), {'verdict': 'BUILD OK', 'exe': f'build/{NAME}/lean_mpas',
                                           'build_json': f'build/{NAME}/build.json'})


class BuildRecordTest(unittest.TestCase):
    """build_record() itself, with the tool and git probes mocked."""

    def record(self, cfg: BuildConfig, nc=FAKE) -> dict:
        with tempfile.TemporaryDirectory() as tmp, \
                mock.patch.object(build, '_first_line', return_value='fake 1.2.3'), \
                mock.patch.object(build, 'git_state', return_value=('a' * 40, False)):
            return build.build_record(cfg, Path(tmp).resolve() / build_dir_name(cfg), '/opt/fc', [], nc=nc)

    def test_build_record_netcdf_lists_and_default_libdir(self):
        nc = replace(FAKE, libdir='/usr/lib', library='/usr/lib/' + netcdf.LIBRARY, skipped=('a: no CDF-5',))
        with mock.patch.object(netcdf, 'find', side_effect=AssertionError('must use the supplied NetCDF')):
            record = self.record(CPU, nc=nc)
        self.assertEqual(record['netcdf']['skipped'], ['a: no CDF-5'])
        self.assertEqual(record['netcdf']['deps'], [])
        self.assertIs(record['netcdf']['default_libdir'], True)
        self.assertEqual(record['link_libraries'], ['-lnetcdf'])
        with mock.patch.object(netcdf, 'find', return_value=nc) as find_nc:
            self.assertEqual(self.record(CPU, nc=None)['netcdf'], record['netcdf'])
        find_nc.assert_called_once_with()

    def test_build_record_has_switches(self):
        cfg = resolve_config('--flavor', 'lean', '--target', 'gpu', '--switch', 'LEAN_GF_NOSHALLOW=off')
        record = self.record(cfg)
        self.assertEqual(record['switches']['LEAN_GF_NOSHALLOW'], 'off')
        self.assertEqual(record['switches'], dict(cfg.switches))
        expected = set(config.global_defines(cfg))
        self.assertEqual(set(record['defines']), expected)
        self.assertEqual(len(record['defines']), len(expected))
        self.assertRegex(Path(record['build_dir']).name, r'^gpu-nvfortran-strict-[0-9a-f]{8}$')
        for source in record['sources']:
            self.assertTrue(expected.issubset(source['defines']), source['path'])

    def test_gpu_record_lists_cmake_preprocess(self):
        cfg = BuildConfig('gpu', 'nvfortran', 'strict')
        units = config.load_units()
        added = self.record(cfg)['cmake_added']
        self.assertEqual(added['module_dir'], '-module <dir>')
        self.assertEqual(added['preprocess'], {
            'sources': ['src/dyn_micro.f90', 'src/lean_model.f90'],
            'e_line': '<fc> -Mpreprocess <defines> <includes> <flags> -E <source> > <source>-pp.<ext>',
            'e_line_flags': ['-Mpreprocess', '-E'], 'compile': '<source>-pp.<ext>', 'compile_flags': [],
            'why': units['cmake_preprocess_why']})
        self.assertEqual(added['preprocess']['sources'], sorted(units['cmake_preprocess']['gpu']))

    def test_switched_record_lists_cmake_preprocess(self):
        for cfg in (resolve_config('--flavor', 'lean', '--target', 'gpu'), resolve_config('--flavor', 'lean')):
            with self.subTest(cfg=cfg):
                added = self.record(cfg)['cmake_added']
                self.assertIn('preprocess', added)
                self.assertEqual(added['preprocess']['sources'], sorted(config.preprocess_set(cfg)))
                self.assertNotIn('src/land_ice.f90', added['preprocess']['sources'])
                self.assertNotIn('noahmp_lean/ResistanceAboveCanopyMostMod.f90', added['preprocess']['sources'])

    def test_gfortran_preprocess_adds_fpreprocessed(self):
        with mock.patch.object(config, 'load_units',
                               return_value={'cmake_preprocess': {'cpu': ['src/x.f90']}, 'cmake_preprocess_why': 'w'}):
            added = build.cmake_added(CPU)
        self.assertEqual(added['preprocess']['e_line_flags'], ['-cpp', '-E'])
        self.assertEqual(added['preprocess']['compile_flags'], ['-fpreprocessed'])
        self.assertEqual(added['preprocess']['sources'], ['src/x.f90'])

    def test_cpu_record_has_no_preprocess(self):
        added = self.record(CPU)['cmake_added']
        self.assertNotIn('preprocess', added)
        self.assertEqual(set(added), {'module_dir', 'flags', 'why'})


class BuildLinkTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve() / 'build'
        self.root.mkdir()
        self.first, self.second = self.root / 'A', self.root / 'B'
        self.first.mkdir()
        self.second.mkdir()
        self.link = self.root / 'lean_gpu'

    def test_link_replaces_real_directory(self):
        self.link.mkdir()
        (self.link / 'old-binary').write_bytes(b'original')
        previous = self.link.with_name(self.link.name + '.prev')
        previous.mkdir()
        (previous / 'obsolete').write_bytes(b'older')
        build.link_build(self.second, self.link.name)
        self.assertTrue(self.link.is_symlink())
        self.assertEqual(self.link.resolve(), self.second)
        self.assertFalse(previous.is_symlink())
        self.assertEqual([path.name for path in previous.iterdir()], ['old-binary'])
        self.assertEqual((previous / 'old-binary').read_bytes(), b'original')

    def test_link_replaces_symlink(self):
        self.link.symlink_to(self.first.name, target_is_directory=True)
        previous = self.link.with_name(self.link.name + '.prev')
        previous.mkdir()
        (previous / 'rollback').write_bytes(b'keep')
        replace_link = os.replace

        def replace_atomically(source, destination):
            self.assertEqual(Path(destination), self.link)
            self.assertEqual(self.link.resolve(), self.first, 'old link survives until replacement')
            self.assertTrue(Path(source).is_symlink())
            replace_link(source, destination)

        with mock.patch.object(os, 'replace', side_effect=replace_atomically) as replacement:
            build.link_build(self.second, self.link.name)
        replacement.assert_called_once()
        self.assertEqual(self.link.resolve(), self.second)
        self.assertEqual(list(self.first.iterdir()), [], 'nothing nested inside the old target')
        self.assertEqual((previous / 'rollback').read_bytes(), b'keep')
        self.assertEqual(sorted(path.name for path in self.root.iterdir()),
                         ['A', 'B', self.link.name, previous.name])

    def test_previous_symlink_is_removed_without_touching_its_target(self):
        self.link.mkdir()
        (self.link / 'old-binary').write_bytes(b'original')
        previous = self.link.with_name(self.link.name + '.prev')
        previous.symlink_to(self.first.name, target_is_directory=True)
        (self.first / 'unrelated').write_bytes(b'keep')
        build.link_build(self.second, self.link.name)
        self.assertFalse(previous.is_symlink())
        self.assertEqual((previous / 'old-binary').read_bytes(), b'original')
        self.assertEqual((self.first / 'unrelated').read_bytes(), b'keep')

    def test_new_and_dangling_links(self):
        for dangling in (False, True):
            with self.subTest(dangling=dangling):
                if dangling:
                    self.link.symlink_to('gone')
                build.link_build(self.second, self.link.name)
                self.assertEqual(self.link.resolve(), self.second)
                self.link.unlink()

    def test_link_rejects_paths_and_the_build_directory_itself(self):
        for name in ('', '.', '..', '../escape', '/absolute', 'nested/link', self.second.name):
            with self.subTest(name=name), self.assertRaises(ValueError):
                build.link_build(self.second, name)
        self.assertTrue(self.second.is_dir())
        self.assertEqual(sorted(path.name for path in self.root.iterdir()), ['A', 'B'])


class GitStateTest(unittest.TestCase):
    """git_state() on a temporary git repository: only the build inputs count for dirty."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        for rel in ('src/a.f90', 'tools/lean/x.py', 'CMakeLists.txt'):
            (self.root / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.root / rel).write_text('x\n', encoding='utf-8')
        (self.root / '.gitignore').write_text('__pycache__/\n', encoding='utf-8')
        env = {**os.environ, 'GIT_CONFIG_GLOBAL': os.devnull, 'GIT_CONFIG_NOSYSTEM': '1'}
        env = {key: value for key, value in env.items() if key not in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_INDEX_FILE')}
        for command in (['init', '-q'], ['add', '-A'],
                        ['-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', 'init']):
            subprocess.run(['git', '-C', str(self.root), *command], check=True, env=env, capture_output=True)

    def state(self):
        with mock.patch.object(build, 'ROOT', self.root):
            return build.git_state()

    def test_clean_tree(self):
        commit, dirty = self.state()
        self.assertRegex(commit, r'^[0-9a-f]{40}$')
        self.assertIs(dirty, False)

    def test_untracked_build_input_is_dirty(self):
        (self.root / 'tools/lean/new.py').write_text('y\n', encoding='utf-8')
        self.assertIs(self.state()[1], True)

    def test_tracked_change_is_dirty(self):
        (self.root / 'src/a.f90').write_text('changed\n', encoding='utf-8')
        self.assertIs(self.state()[1], True)

    def test_untracked_outside_inputs_or_ignored_is_clean(self):
        (self.root / 'scratch').mkdir(parents=True)
        (self.root / 'scratch/r.json').write_text('{}', encoding='utf-8')
        (self.root / 'tools/lean/__pycache__').mkdir()
        (self.root / 'tools/lean/__pycache__/x.pyc').write_bytes(b'c')
        self.assertIs(self.state()[1], False)

    def test_outside_a_git_tree(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(build, 'ROOT', Path(tmp)), \
                mock.patch.dict(os.environ, {'GIT_CEILING_DIRECTORIES': str(Path(tmp).resolve().parent)}):
            self.assertEqual(build.git_state(), (None, None))


if __name__ == '__main__':
    unittest.main()
