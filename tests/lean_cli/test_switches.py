"""Explicit build switches, including the real CMake -> Ninja command path (no model compilation)."""
import contextlib
import io
import json
import os
import shlex
import shutil
import subprocess
import tempfile
import unittest
from dataclasses import asdict
from pathlib import Path
from unittest import mock

from tests.lean_cli.test_netcdf import FAKE
from tools.lean import cli, config, netcdf
from tools.lean.config import BuildConfig

ROOT = Path(__file__).resolve().parents[2]
GOLDEN = ROOT / 'tests/lean_cli/golden/cpu-gfortran-strict.commands'
CPU = BuildConfig('cpu', 'gfortran', 'strict')
SWITCHES = {
    'LEAN_PRECIPW_MOIST', 'LEAN_RAD_2025', 'LEAN_MYNN_HYD_PS', 'LEAN_MYNN_DEW', 'LEAN_MYNN_RHOSFC', 'LEAN_MYNN_MOIST',
    'LEAN_SFC_Q2MR', 'LEAN_MYNN_FILL_THL', 'LEAN_MONO_RELAX', 'LEAN_NOAHMP_SWSPLIT', 'LEAN_GF_NOSHALLOW',
    'LEAN_RAD_BLCLOUD', 'LEAN_REFL_CARD', 'LEAN_MICRO_ONEKERNEL', 'LEAN_BUDGET', 'LEAN_NOAHMP_T2Z', 'LEAN_NOAHMP_RSF3',
    'LEAN_NOAHMP_THETA', 'LEAN_MYNN_SALT', 'LEAN_GF_DXMIN',
}
REQUIRES = {}


def run_cli(*args):
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        status = cli.main(list(args))
    return status, out.getvalue(), err.getvalue()


def tool_paths():
    """(path, placeholder) for this machine's gfortran, CMake and Ninja, on PATH and resolved, longest first."""
    pairs = set()
    for tool, placeholder in (('gfortran', '<FC>'), ('cmake', '<CMAKE>'), ('ninja', '<NINJA>')):
        found = shutil.which(tool)
        if found:
            pairs.add((found, placeholder))
            pairs.add((str(Path(found).resolve()), placeholder))
    return sorted(pairs, key=lambda pair: -len(pair[0]))


def normalize_commands(text):
    """The commands with this machine's paths replaced, so that the golden file holds on any machine."""
    build_dir = ROOT / 'build' / config.build_dir_name(CPU)
    for path, placeholder in tool_paths():
        text = text.replace(path, placeholder)
    return (text.replace(netcdf.find().libdir, '<NETCDF_LIBDIR>')
            .replace(str(build_dir), '<BUILD_DIR>').replace(str(ROOT), '<ROOT>'))


class SwitchesTest(unittest.TestCase):
    def test_normalize_commands_netcdf_libdir(self):
        with mock.patch.object(netcdf, 'find', return_value=FAKE):
            self.assertEqual(normalize_commands('-L/x/lib -lnetcdf -Wl,-rpath,/x/lib\n'),
                             '-L<NETCDF_LIBDIR> -lnetcdf -Wl,-rpath,<NETCDF_LIBDIR>\n')

    def test_table_lists_the_switches(self):
        table = json.loads((ROOT / 'config/switches.json').read_text(encoding='utf-8'))['switches']
        self.assertEqual(set(table), SWITCHES)
        integers = {'LEAN_GF_DXMIN'}
        for name, entry in table.items():
            with self.subTest(name=name):
                self.assertEqual(set(entry), {'kind', 'requires', 'why'})
                self.assertEqual(entry['kind'], 'int' if name in integers else 'bool')
                self.assertEqual(entry['requires'], REQUIRES.get(name, []))
                self.assertIsInstance(entry['why'], str)
                self.assertTrue(entry['why'].strip())

    def test_requires(self):
        for name, (requirement,) in REQUIRES.items():
            with self.subTest(name=name):
                for value in (None, 'off'):
                    switches = {name: 'on'}
                    if value is not None:
                        switches[requirement] = value
                    with self.assertRaisesRegex(ValueError, requirement):
                        config.check_switches(switches)
                config.check_switches({name: 'off'})
        config.check_switches({name: 'on' for pair in REQUIRES.items() for name in (pair[0], *pair[1])})

    def test_bad_switch_value_rejected(self):
        for switch in ('LEAN_MYNN_DEW=1', 'LEAN_MYNN_DEW=true', 'LEAN_GF_DXMIN=on'):
            with self.subTest(switch=switch), mock.patch('tools.lean.build.build') as build:
                status, _, err = run_cli('build', '--dry-run', '--switch', switch)
                self.assertEqual(status, 2)
                self.assertIn(switch.split('=')[0], err)
                self.assertNotIn('unrecognized arguments', err)
                build.assert_not_called()

    def test_unknown_or_incomplete_switch_rejected(self):
        for switch, message in (('LEAN_UNKNOWN=on', 'LEAN_UNKNOWN'), ('LEAN_MYNN_DEW', 'NAME='),
                                ('LEAN_GF_DXMIN=x', 'LEAN_GF_DXMIN')):
            with self.subTest(switch=switch), mock.patch('tools.lean.build.build') as build:
                status, _, err = run_cli('build', '--dry-run', '--switch', switch)
                self.assertEqual(status, 2)
                self.assertIn(message, err)
                self.assertNotIn('unrecognized arguments', err)
                build.assert_not_called()

    def test_cli_switches_are_sorted_and_unique(self):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            status, _, err = run_cli('build', '--dry-run', '--jobs', '1', '--switch', 'LEAN_MYNN_DEW=on',
                                    '--switch', 'LEAN_BUDGET=on', '--switch', 'LEAN_MYNN_DEW=off')
        self.assertEqual(status, 0, err)
        self.assertEqual(build.call_args.args[0].switches, (('LEAN_BUDGET', 'on'), ('LEAN_MYNN_DEW', 'off')))

    def test_switch_defines(self):
        cfg = BuildConfig('gpu', 'nvfortran', 'strict', switches=(('LEAN_MYNN_DEW', 'on'),
                          ('LEAN_REFL_CARD', 'on'), ('LEAN_BUDGET', 'off'), ('LEAN_GF_DXMIN', '3000')))
        self.assertEqual(cfg.switches, tuple(sorted(cfg.switches)))
        self.assertEqual(config.switch_defines(cfg), ['LEAN_GF_DXMIN=3000', 'LEAN_MYNN_DEW', 'LEAN_REFL_CARD'])
        for path, group in config.sources(cfg):
            defines = config.unit_flags(cfg, Path(path).stem, group)[1]
            for define in config.switch_defines(cfg):
                self.assertEqual(defines.count(define), 1, path)
            self.assertNotIn('LEAN_BUDGET', defines, path)

    def test_environment_switches_ignored(self):
        with mock.patch.dict(os.environ, {'LEAN_BUDGET': '1', 'LEAN_MYNN_DEW': '1'}):
            for target, fc in (('cpu', 'gfortran'), ('gpu', 'nvfortran')):
                cfg = BuildConfig(target, fc, 'strict')
                for path, group in config.sources(cfg):
                    defines = config.unit_flags(cfg, Path(path).stem, group)[1]
                    self.assertNotIn('LEAN_BUDGET', defines, path)
                    self.assertNotIn('LEAN_MYNN_DEW', defines, path)

    def test_cmake_config_file_rebuilds_whole_config(self):
        cfg = BuildConfig('cpu', 'gfortran', 'debug', omp=True, switches=(('LEAN_MYNN_DEW', 'on'),))
        (ROOT / 'build').mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='config-') as tmp:
            folder = Path(tmp).resolve()
            source, out = folder / 'lean-config.json', folder / 'lean_sources.cmake'
            source.write_text(json.dumps(asdict(cfg)), encoding='utf-8')
            with mock.patch.object(config, 'cmake_file', wraps=config.cmake_file) as emit, \
                    mock.patch.object(netcdf, 'find', return_value=FAKE):
                status = config.main(['cmake', '--config-file', str(source), '--out', str(out)])
            self.assertEqual(status, 0)
            emit.assert_called_once_with(cfg, folder)
            text = out.read_text(encoding='utf-8')
            for flag in ('-fcheck=all', '-fopenmp', '-DLEAN_MYNN_DEW'):
                self.assertIn(flag, text)


@unittest.skipUnless(all(shutil.which(tool) for tool in ('gfortran', 'cmake', 'ninja')),
                     'needs gfortran, cmake and ninja; configures only')
class SwitchCommandsTest(unittest.TestCase):
    def dry_run(self, *args):
        done = subprocess.run(['./lean', 'build', '--target', 'cpu', '--fc', 'gfortran', *args,
                               '--dry-run', '--jobs', '1'], cwd=ROOT, capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertTrue(done.stdout.strip(), 'no ninja -t commands output')
        return done.stdout

    def test_switch_reaches_compile_commands(self):
        for switches in ([], ['--switch', 'LEAN_MYNN_DEW=on'], []):
            with self.subTest(switches=switches):
                text = self.dry_run(*switches)
                compiles = [line for line in text.splitlines() if '-c' in shlex.split(line)]
                self.assertTrue(compiles, 'no compile commands')
                for group in ('esmf_time', 'noahmp_reference', 'noahmp_lean'):
                    self.assertTrue(any(f'CMakeFiles/{group}.dir/' in line for line in compiles), group)
                for line in compiles:
                    self.assertEqual('-DLEAN_MYNN_DEW' in shlex.split(line), bool(switches), line)
                if switches:
                    cfg = BuildConfig('cpu', 'gfortran', 'strict', switches=(('LEAN_MYNN_DEW', 'on'),))
                    build_dir = ROOT / 'build' / config.build_dir_name(cfg)
                    data = json.loads((build_dir / 'lean-config.json').read_text(encoding='utf-8'))
                    self.assertEqual(data, json.loads(json.dumps(asdict(cfg))))
                    self.assertIn(f'LEAN_CONFIG_FILE:FILEPATH={build_dir}/lean-config.json',
                                  (build_dir / 'CMakeCache.txt').read_text(encoding='utf-8'))

    def test_plain_build_commands_unchanged(self):
        commands = normalize_commands(self.dry_run())
        self.assertEqual(commands.splitlines(), GOLDEN.read_text(encoding='utf-8').splitlines())

    def test_link_line_ends_with_netcdf(self):
        words = shlex.split(self.dry_run().splitlines()[-1])
        nc = netcdf.find()
        last_object = max(index for index, word in enumerate(words) if word.endswith('.o'))
        self.assertGreater(words.index('-lnetcdf'), last_object)
        for word in netcdf.link_args(nc):
            self.assertIn(word, words)


if __name__ == '__main__':
    unittest.main()
