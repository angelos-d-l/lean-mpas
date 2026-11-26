"""The two flavors of config/presets.json, and real CMake/Ninja commands (no model compile)."""
import contextlib
import io
import json
import os
import shutil
import subprocess
import tempfile
import unittest
from dataclasses import asdict, replace
from pathlib import Path
from unittest import mock

from tools.lean import build, cli, config
from tools.lean.config import BuildConfig
from tests.lean_cli.test_switches import GOLDEN, normalize_commands

ROOT = Path(__file__).resolve().parents[2]
UNIT_DEFINES = {'HIDE_MPI', 'EXNER_EXACT', 'LEAN_PHYS_DEV'}


def run_cli(*args):
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        status = cli.main(['build', *args, '--dry-run', '--jobs', '1'])
    return status, out.getvalue(), err.getvalue()


class PresetsTest(unittest.TestCase):
    def resolve(self, *args):
        with mock.patch('tools.lean.build.build', return_value=0) as compile_build:
            status, _, err = run_cli(*args)
        self.assertEqual(status, 0, err)
        compile_build.assert_called_once()
        return compile_build.call_args.args[0]

    def test_preset_table_has_the_two_flavors(self):
        table = json.loads((ROOT / 'config/presets.json').read_text())
        self.assertEqual(set(table), {'mpas', 'lean'})
        kinds = json.loads((ROOT / 'config/switches.json').read_text())['switches']
        for name, entry in table.items():
            with self.subTest(preset=name):
                self.assertEqual(set(entry), {'target', 'fc', 'profile', 'switches', 'why', 'flavor'})
                self.assertEqual(entry['flavor'], name)
                self.assertTrue(entry['why'].strip())
                self.assertLessEqual(set(entry['switches']), set(kinds))

    def test_precipw_moist_on_flavor_lean(self):
        self.assertEqual(dict(self.resolve('--flavor', 'lean').switches)['LEAN_PRECIPW_MOIST'], 'on')

    def test_flavor_mpas_has_no_switches(self):
        for target, fc in (('cpu', 'gfortran'), ('gpu', 'nvfortran')):
            cfg = self.resolve('--flavor', 'mpas', '--target', target, '--fc', fc)
            self.assertEqual(cfg.switches, ())
            self.assertEqual(cfg, BuildConfig(target, fc, 'strict'))
            for path, group in config.sources(cfg):
                defines = config.unit_flags(cfg, Path(path).stem, group)[1]
                self.assertEqual(set(defines) - UNIT_DEFINES, {'mpas', 'SINGLE_PRECISION'}, path)

    def test_flavor_lean_switches(self):
        """The lean flavor switches on the 16 departures of config/presets.json; LEAN_REFL_CARD (card only) stays off."""
        cfg = self.resolve('--flavor', 'lean')
        self.assertEqual(len(cfg.switches), 16)
        self.assertNotIn('LEAN_REFL_CARD', dict(cfg.switches))

    def test_cli_overrides_flavor(self):
        cfg = self.resolve('--flavor', 'lean', '--target', 'cpu', '--fc', 'gfortran', '--profile', 'debug',
                           '--switch', 'LEAN_MYNN_DEW=off', '--switch', 'LEAN_GF_DXMIN=2500', '--omp')
        self.assertEqual((cfg.target, cfg.fc, cfg.profile, cfg.omp), ('cpu', 'gfortran', 'debug', True))
        defines = config.unit_flags(cfg, 'lean_model', 'driver')[1]
        self.assertIn('LEAN_GF_DXMIN=2500', defines)
        self.assertNotIn('LEAN_MYNN_DEW', defines)

    def test_flavor_environment_ignored(self):
        expected = [self.resolve('--flavor', name) for name in config.FLAVORS]
        with mock.patch.dict(os.environ, {'LEAN_BUDGET': '1', 'LEAN_GF_NOSHALLOW': '1'}):
            self.assertEqual([self.resolve('--flavor', name) for name in config.FLAVORS], expected)

    def test_bad_selections_rejected(self):
        for args in (('--flavor', 'missing'), ('--switch', 'LEAN_UNKNOWN=on')):
            with self.subTest(args=args), mock.patch('tools.lean.build.build') as compile_build:
                status, _, _ = run_cli(*args)
                self.assertEqual(status, 2)
                compile_build.assert_not_called()

    def test_cmake_config_path_for_switches(self):
        plain = BuildConfig('cpu', 'gfortran', 'strict')
        self.assertFalse(any(arg.startswith('-DLEAN_CONFIG_FILE=')
                             for arg in build.cmake_args(plain, ROOT, 'gfortran')))
        cfg = replace(plain, switches=(('LEAN_BUDGET', 'on'),))
        self.assertTrue(any(arg.startswith('-DLEAN_CONFIG_FILE=') for arg in build.cmake_args(cfg, ROOT, 'gfortran')))

    def test_switches_round_trip_through_cmake_config(self):
        cfg = self.resolve('--flavor', 'lean')
        (ROOT / 'build').mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='config-') as tmp:
            folder = Path(tmp).resolve()
            source, out = folder / 'lean-config.json', folder / 'lean_sources.cmake'
            source.write_text(json.dumps(asdict(cfg)))
            with mock.patch.object(config, 'cmake_file', wraps=config.cmake_file) as emit:
                self.assertEqual(config.main(['cmake', '--config-file', str(source), '--out', str(out)]), 0)
            emit.assert_called_once_with(cfg, folder)
            text = out.read_text()
            for define in ('-DLEAN_MYNN_DEW', '-DLEAN_GF_DXMIN=4000'):
                self.assertIn(define, text)


@unittest.skipUnless(all(shutil.which(tool) for tool in ('gfortran', 'cmake', 'ninja')),
                     'needs gfortran, cmake and ninja; configures only')
class PresetCommandsTest(unittest.TestCase):
    def dry_run(self, *args):
        done = subprocess.run(['./lean', 'build', '--target', 'cpu', '--fc', 'gfortran', *args,
                               '--dry-run', '--jobs', '1'], cwd=ROOT, capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done.stdout

    def test_flavor_mpas_matches_golden_commands(self):
        commands = normalize_commands(self.dry_run('--flavor', 'mpas'))
        self.assertEqual(commands.splitlines(), GOLDEN.read_text().splitlines())


if __name__ == '__main__':
    unittest.main()
