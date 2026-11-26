"""The executable's flavor and generated, compiler-readable build identity."""
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import cli, config
from tools.lean.build_info import build_info_text, git_describe, write_build_info
from tests.lean_cli.test_netcdf import FAKE


class BuildInfoTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        (config.ROOT / 'build').mkdir(exist_ok=True)

    def resolve(self, *args):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            self.assertEqual(cli.main(['build', *args, '--jobs', '2']), 0)
        build.assert_called_once()
        return build.call_args.args[0]

    def value(self, text, name):
        match = re.search(r'character\(len=\*\), parameter :: ' + name + r' = (.*(?:\n\s+.*)*)', text)
        self.assertIsNotNone(match, name)
        pieces = []
        for line in match.group(1).splitlines():
            if line.strip() == '&':
                continue
            literal = re.match(r"\s*'((?:[^']|'')*)'", line)
            self.assertIsNotNone(literal, line)
            pieces.append(literal.group(1).replace("''", "'"))
            if not line.rstrip().endswith('&'):
                break
        return ''.join(pieces)

    def test_presets_name_their_flavor(self):
        expected = {'mpas': 'mpas', 'lean': 'lean'}
        presets = config.load_presets()
        self.assertEqual(set(presets), set(expected))
        for name, flavor in expected.items():
            with self.subTest(preset=name):
                self.assertIn(presets[name].get('flavor'), config.FLAVORS)
                self.assertEqual(presets[name]['flavor'], flavor)

    def test_flavor_of_builds(self):
        cases = [
            ([], 'mpas'),
            (['--flavor', 'mpas'], 'mpas'),
            (['--flavor', 'mpas', '--switch', 'LEAN_MYNN_DEW=off'], 'mpas'),
            (['--flavor', 'mpas', '--switch', 'LEAN_BUDGET=on'], 'mpas'),
            (['--flavor', 'lean'], 'lean'),
            (['--flavor', 'lean', '--switch', 'LEAN_BUDGET=on'], 'lean'),
            (['--flavor', 'lean', '--switch', 'LEAN_MYNN_DEW=off'], 'custom'),
            (['--flavor', 'mpas', '--switch', 'LEAN_REFL_CARD=on'], 'custom'),
        ]
        self.assertEqual(config.CUSTOM_FLAVOR, 'custom')
        for args, expected in cases:
            with self.subTest(args=args):
                self.assertEqual(config.build_flavor(self.resolve(*args)), expected)

    def test_build_info_text(self):
        cfg = self.resolve('--flavor', 'lean', '--target', 'gpu')
        text = build_info_text(cfg, describe='abc1234-dirty', system='Linux')
        for line in (
            "character(len=*), parameter :: build_flavor = 'lean'",
            "character(len=*), parameter :: build_git = 'abc1234-dirty'",
            "character(len=*), parameter :: build_profile = 'strict'",
            'logical, parameter :: build_openmp = .false.',
            'logical, parameter :: build_openacc = .true.',
            'integer(c_int), parameter :: errno_would_block = 11',
            "function errno_location() bind(C, name='__errno_location') result(p)",
        ):
            self.assertIn(line, [line.strip() for line in text.splitlines()])
        self.assertEqual(self.value(text, 'build_name'), config.build_dir_name(cfg))
        self.assertEqual(self.value(text, 'build_switches'), ' '.join(sorted(config.switch_defines(cfg))))
        darwin = build_info_text(cfg, describe='test', system='Darwin')
        self.assertIn("bind(C, name='__error')", darwin)
        self.assertIn('integer(c_int), parameter :: errno_would_block = 35', darwin)
        with self.assertRaisesRegex(ValueError, 'Windows'):
            build_info_text(cfg, system='Windows')

        plain = build_info_text(self.resolve(), describe='test', system='Linux')
        self.assertEqual(self.value(plain, 'build_switches'), '')
        self.assertEqual(self.value(plain, 'build_flavor'), 'mpas')
        switched = build_info_text(self.resolve('--flavor', 'lean', '--switch', 'LEAN_GF_NOSHALLOW=off'),
                                   describe='test', system='Linux')
        switches = self.value(switched, 'build_switches').split()
        for switch in ('LEAN_MYNN_DEW', 'LEAN_GF_DXMIN=4000'):
            self.assertIn(switch, switches)
        self.assertNotIn('LEAN_GF_NOSHALLOW', switches)
        self.assertEqual(switches, sorted(set(switches)))
        for name in config.load_presets():
            args = ['--flavor', name]
            generated = build_info_text(self.resolve(*args), describe='test', system='Linux')
            self.assertLessEqual(max(map(len, generated.splitlines())), 132, name)
        describe = "x" * 99 + "'" + 'y' * 160
        quoted = build_info_text(cfg, describe=describe, system='Linux')
        self.assertEqual(self.value(quoted, 'build_git'), describe)
        self.assertLessEqual(max(map(len, quoted.splitlines())), 132)
        variant = build_info_text(self.resolve('--profile', 'envelope', '--variant', 'o3native'),
                                  describe='test', system='Linux')
        self.assertEqual(self.value(variant, 'build_profile'), 'envelope-o3native')
        omp = build_info_text(self.resolve('--omp'), describe='test', system='Linux')
        self.assertIn('logical, parameter :: build_openmp = .true.', omp)

    @unittest.skipUnless(shutil.which('gfortran'), 'needs gfortran')
    def test_build_info_compiles(self):
        for args in (['--flavor', 'lean'], []):
            with self.subTest(args=args), tempfile.TemporaryDirectory(dir=config.ROOT / 'build') as folder:
                source = Path(folder) / 'lean_build_info.f90'
                source.write_text(build_info_text(self.resolve(*args), describe="test's " + 'x' * 220),
                                  encoding='utf-8')
                done = subprocess.run(['gfortran', '-c', source.name], cwd=folder, capture_output=True, text=True)
                self.assertEqual(done.returncode, 0, done.stderr)

    def test_write_only_when_changed(self):
        with tempfile.TemporaryDirectory(dir=config.ROOT / 'build') as folder:
            cfg, build_dir = self.resolve(), Path(folder)
            path = write_build_info(cfg, build_dir, describe='first', system='Darwin')
            self.assertEqual(path, build_dir / 'generated/lean_build_info.f90')
            before = path.stat().st_mtime_ns
            self.assertEqual(write_build_info(cfg, build_dir, describe='first', system='Darwin'), path)
            self.assertEqual(path.stat().st_mtime_ns, before)
            write_build_info(cfg, build_dir, describe='second', system='Darwin')
            self.assertIn("build_git = 'second'", path.read_text())
            self.assertNotEqual(path.stat().st_mtime_ns, before)

    def test_git_describe_unknown(self):
        with mock.patch('tools.lean.build_info.subprocess.run', side_effect=OSError):
            self.assertEqual(git_describe(), 'unknown')
        with mock.patch('tools.lean.build_info.subprocess.run',
                        return_value=subprocess.CompletedProcess([], 128, '', 'no git repository')):
            self.assertEqual(git_describe(), 'unknown')
        with mock.patch('tools.lean.build_info.subprocess.run',
                        return_value=subprocess.CompletedProcess([], 0, ' abc1234-dirty\n', '')) as run:
            self.assertEqual(git_describe(), 'abc1234-dirty')
            self.assertEqual(run.call_args.args[0], ['git', '-C', str(config.ROOT),
                                                    'describe', '--dirty', '--always'])

    def test_generated_unit_in_base(self):
        for cfg in (config.BuildConfig('cpu', 'gfortran', 'strict'),
                    config.BuildConfig('gpu', 'nvfortran', 'strict')):
            with self.subTest(target=cfg.target), tempfile.TemporaryDirectory(dir=config.ROOT / 'build') as folder:
                build_dir = Path(folder)
                units = config.compile_units(cfg, build_dir)
                base = [path for path, _, group in units if group == 'base']
                after = base.index(config.ROOT / 'src/run/lean_namelist.f90')
                self.assertEqual(base[after + 1:after + 5], [build_dir / 'generated/lean_build_info.f90',
                                 *[config.ROOT / f'src/run/{name}.f90'
                                   for name in ('lean_guard', 'lean_log', 'lean_summary')]])
                self.assertNotIn(('generated/lean_build_info.f90', 'base'), config.sources(cfg))
                host = config.unit_flags(cfg, 'serial_services', 'base')
                self.assertEqual(config.unit_flags(cfg, 'lean_build_info', 'base'), host)
                card = config.load_flags()['nvfortran']['card']['compile'] if cfg.target == 'gpu' else []
                self.assertEqual(config.unit_flags(cfg, 'lean_summary', 'base'), (host[0] + card, host[1]))

    def test_compile_units_writes_build_info(self):
        cfg = config.BuildConfig('cpu', 'gfortran', 'strict')
        with tempfile.TemporaryDirectory(dir=config.ROOT / 'build') as folder, \
                mock.patch('tools.lean.build_info.git_describe', return_value='x'):
            for name in ('compile', 'cmake'):
                build_dir = Path(folder) / name
                if name == 'compile':
                    config.compile_units(cfg, build_dir)
                else:
                    config.cmake_file(cfg, build_dir, nc=FAKE)
                path = build_dir / 'generated/lean_build_info.f90'
                self.assertTrue(path.is_file(), path)
                self.assertEqual(path.read_text(), build_info_text(cfg, describe='x'))

    def test_driver_ends_with_drop_in_and_main(self):
        for cfg in (config.BuildConfig('cpu', 'gfortran', 'strict'),
                    config.BuildConfig('gpu', 'nvfortran', 'strict')):
            driver = [path for path, group in config.sources(cfg) if group == 'driver']
            self.assertEqual(driver[-3:], ['src/lean_model.f90', 'src/run/lean_drop_in.f90', 'src/run/main.f90'])


if __name__ == '__main__':
    unittest.main()
