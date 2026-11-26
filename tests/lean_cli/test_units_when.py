"""Conditional source selection and card flags; no model compile."""
import contextlib
import io
import json
import re
import tempfile
import unittest
from dataclasses import asdict
from pathlib import Path
from unittest import mock

from tools.lean import cli, config
from tools.lean.config import BuildConfig

ROOT = Path(__file__).resolve().parents[2]
ICE = ('sea_ice_noah', 'sea_ice', 'land_ice_exchange', 'land_ice')
CONDITIONAL = (*ICE, 'radiation_blcloud')
NV = ['-Mpreprocess', '-Mfree', '-O2', '-Kieee', '-Mnofma']
GF = ['-cpp', '-ffree-form', '-ffree-line-length-none', '-O2', '-ffp-contract=off']
CARD = ['-acc', '-gpu=cc120,nofma', '-Minfo=accel']


class UnitsWhenTest(unittest.TestCase):
    def resolve(self, *args):
        with mock.patch('tools.lean.build.build', return_value=0) as build, \
                contextlib.redirect_stderr(io.StringIO()) as err:
            status = cli.main(['build', *args, '--dry-run', '--jobs', '2'])
        self.assertEqual(status, 0, err.getvalue())
        build.assert_called_once()
        return build.call_args.args[0]

    def ice_paths(self, cfg):
        return [path for path, _ in config.sources(cfg) if Path(path).stem in ICE]

    def test_gpu_ice_units_from_device(self):
        cfg = self.resolve('--flavor', 'lean', '--target', 'gpu')
        expected = [f'src/device/{unit}.f90' for unit in ICE[:3]] + ['src/land_ice.f90']
        self.assertEqual(self.ice_paths(cfg), expected)
        sources = config.sources(cfg)
        at = sources.index(('src/noahmp_cold_start.f90', 'model'))
        self.assertEqual(sources[at + 1:at + 6], [(path, 'model') for path in expected] +
                         [('src/phys_mode.f90', 'model')])
        for unit in ICE:
            with self.subTest(unit=unit):
                options, defines = config.unit_flags(cfg, unit, 'model')
                self.assertEqual(options, NV + (CARD if unit != 'land_ice' else []))

    def test_ice_units_in_every_build(self):
        for cfg in (self.resolve('--flavor', 'lean', '--target', 'gpu'), BuildConfig('gpu', 'nvfortran', 'strict'),
                    BuildConfig('cpu', 'gfortran', 'strict')):
            with self.subTest(cfg=cfg):
                self.assertEqual(self.ice_paths(cfg), [f'src/device/{u}.f90' if cfg.target == 'gpu' and u != 'land_ice'
                                                       else f'src/{u}.f90' for u in ICE])

    def test_blcloud_unit_follows_switch(self):
        for value in ('on', 'off'):
            with self.subTest(value=value):
                cfg = self.resolve('--target', 'gpu', '--switch', f'LEAN_RAD_BLCLOUD={value}')
                sources = config.sources(cfg)
                clouds = [(path, group) for path, group in sources if Path(path).stem == 'radiation_blcloud']
                self.assertEqual(clouds, [('src/radiation_blcloud.f90', 'driver')] if value == 'on' else [])
                if value == 'on':
                    at = sources.index(('src/device/radiation_column.f90', 'driver'))
                    self.assertEqual(sources[at + 1:at + 3], clouds + [('src/phys_dev.f90', 'driver')])
                    options, defines = config.unit_flags(cfg, 'radiation_blcloud', 'driver')
                    self.assertEqual(options, NV + CARD)
                    self.assertIn('LEAN_RAD_BLCLOUD', defines)

    def test_when_uses_global_defines(self):
        cfg = BuildConfig('gpu', 'nvfortran', 'strict')
        for defines, expected in (([], list(ICE)), (['LEAN_RAD_BLCLOUD'], [*ICE, 'radiation_blcloud']),
                                  (['LEAN_RAD_BLCLOUD_OTHER'], list(ICE))):
            with self.subTest(defines=defines), mock.patch('tools.lean.config.global_defines', return_value=defines):
                selected = [Path(path).stem for path, _ in config.sources(cfg) if Path(path).stem in CONDITIONAL]
                self.assertEqual(selected, expected)

    def test_cpu_conditional_units_use_host_sources_and_flags(self):
        for fc, expected_flags in (('gfortran', GF), ('nvfortran', NV)):
            with self.subTest(fc=fc):
                cfg = self.resolve('--fc', fc, '--switch', 'LEAN_RAD_BLCLOUD=on')
                selected = [(path, group) for path, group in config.sources(cfg) if Path(path).stem in CONDITIONAL]
                self.assertEqual(selected, [(f'src/{unit}.f90', 'model') for unit in ICE] +
                                 [('src/radiation_blcloud.f90', 'driver')])
                for path, group in selected:
                    self.assertTrue((ROOT / path).is_file(), path)
                    options, defines = config.unit_flags(cfg, Path(path).stem, group)
                    self.assertEqual(options, expected_flags)
                    self.assertIn('LEAN_RAD_BLCLOUD', defines)

    def test_generated_cmake_conditional_sources_and_flags(self):
        configs = (
            (self.resolve('--flavor', 'lean', '--target', 'gpu'), True, True),
            (self.resolve('--flavor', 'lean', '--target', 'gpu', '--switch', 'LEAN_RAD_BLCLOUD=off'), False, False),
            (self.resolve('--switch', 'LEAN_RAD_BLCLOUD=on'), True, True),
            (BuildConfig('cpu', 'gfortran', 'strict'), False, False),
        )
        (ROOT / 'build').mkdir(exist_ok=True)
        for cfg, ice, cloud in configs:
            with self.subTest(cfg=cfg), tempfile.TemporaryDirectory(prefix='lean-units-when-', dir=ROOT / 'build') as tmp:
                folder = Path(tmp)
                source, out = folder / 'lean-config.json', folder / 'lean_sources.cmake'
                source.write_text(json.dumps(asdict(cfg)))
                self.assertEqual(config.main(['cmake', '--config-file', str(source), '--out', str(out)]), 0)
                text = out.read_text()
                props = dict(re.findall(r'set_source_files_properties\("([^"]+)" PROPERTIES COMPILE_OPTIONS "([^"]*)"\)',
                                        text))
                expected_paths = []
                for unit in CONDITIONAL:
                    present = True if unit in ICE else cloud
                    device = cfg.target == 'gpu' and unit in ICE[:3]
                    path = ROOT / ('src/device' if device else 'src') / f'{unit}.f90'
                    self.assertEqual(str(path) in props, present, unit)
                    other = ROOT / ('src' if device else 'src/device') / f'{unit}.f90'
                    self.assertNotIn(f'"{other}"', text)
                    if not present:
                        self.assertNotIn(f'"{path}"', text)
                        continue
                    expected_paths.append(path)
                    self.assertTrue(path.is_file(), path)
                    group = 'model' if unit in ICE else 'driver'
                    options, defines = config.unit_flags(cfg, unit, group)
                    base = NV if cfg.fc == 'nvfortran' else GF
                    self.assertEqual(options, base + (CARD if cfg.target == 'gpu' and unit != 'land_ice' else []))
                    self.assertEqual(props[str(path)].split(';'), options + ['-D' + item for item in defines])
                positions = [text.index(f'"{path}"') for path in expected_paths]
                self.assertEqual(positions, sorted(positions))


if __name__ == '__main__':
    unittest.main()
