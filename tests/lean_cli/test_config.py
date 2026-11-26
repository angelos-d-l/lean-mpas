"""Tests for the flag resolver (tools/lean/config.py) and the Noah-MP splitter (tools/lean/split_noahmp.py)."""
import contextlib
import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from collections import Counter
from dataclasses import replace
from pathlib import Path
from unittest import mock

from tests.lean_cli.test_netcdf import FAKE
from tools.lean import cli, config
from tools.lean.config import (COMPILERS, BuildConfig, build_dir_name, link_flags, load_flags, load_lock, load_units,
                               sources, unit_flags)
from tools.lean.split_noahmp import split_noahmp

ROOT = Path(__file__).resolve().parents[2]

GF = ['-cpp', '-ffree-form', '-ffree-line-length-none']
NV = ['-Mpreprocess', '-Mfree']
GF_STRICT = GF + ['-O2', '-ffp-contract=off']
NV_STRICT = NV + ['-O2', '-Kieee', '-Mnofma']
CARD = ['-acc', '-gpu=cc120,nofma', '-Minfo=accel']
BASE_DEFINES = {'mpas', 'SINGLE_PRECISION'}

CPU_GF = BuildConfig('cpu', 'gfortran', 'strict')
CPU_NV = BuildConfig('cpu', 'nvfortran', 'strict')
GPU = BuildConfig('gpu', 'nvfortran', 'strict')

def emit_cmake(cfg: BuildConfig, build_dir: Path) -> str:
    """The text `python3 -m tools.lean.config cmake` writes to <build_dir>/lean_sources.cmake for cfg."""
    out = build_dir / 'lean_sources.cmake'
    args = ['cmake', '--target', cfg.target, '--fc', cfg.fc, '--profile', cfg.profile,
            '--out', str(out)] + (['--omp'] if cfg.omp else [])
    with contextlib.redirect_stderr(io.StringIO()) as err, mock.patch('tools.lean.netcdf.find', return_value=FAKE):
        status = config.main(args)
    assert status == 0, err.getvalue()
    return out.read_text(encoding='utf-8')


def cmake_list(value: str) -> list[str]:
    return value.split(';') if value else []


class CMakeTest(unittest.TestCase):
    """The CMake file of a build: what CMakeLists.txt include()s."""

    # The defines are -D options at the end: CMake's Ninja rule for a Fortran file it does not preprocess itself
    # (Fortran_PREPROCESS OFF) has no $DEFINES, so COMPILE_DEFINITIONS would never reach the compiler.
    PROPS = re.compile(r'set_source_files_properties\("([^"]+)" PROPERTIES COMPILE_OPTIONS "([^"]*)"\)')

    def expected_units(self, cfg: BuildConfig, build_dir: Path) -> dict:
        """abs path -> (stem, group) of every file cfg compiles, once each, in its first group."""
        return {str(path): (unit, group) for path, unit, group in config.compile_units(cfg, build_dir)}

    def test_generated_cmake_declares_configure_depends(self):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp).resolve()
            text = emit_cmake(CPU_GF, build_dir)
            depends = re.search(r'set_property\(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS([^)]*)\)', text)
            self.assertIsNotNone(depends, 'no CMAKE_CONFIGURE_DEPENDS')
            for rel in ('config/flags.json', 'config/units.json', 'lean.lock.json', 'src/noahmp_core.f90',
                        'src/device/noahmp_core.f90', 'tools/lean/netcdf.py', 'tools/lean/build_info.py'):
                self.assertIn(f'"{ROOT / rel}"', depends.group(1))
            counts = Counter(m.group(1) for m in self.PROPS.finditer(text))
            self.assertEqual(text.count('set_source_files_properties('), sum(counts.values()))
            expected = self.expected_units(CPU_GF, build_dir)
            self.assertEqual(sorted(counts), sorted(expected))
            self.assertEqual({path: n for path, n in counts.items() if n != 1}, {})

    def test_generated_cmake_flags_are_unit_flags(self):
        for cfg in (CPU_GF, BuildConfig('cpu', 'gfortran', 'debug', omp=True), GPU):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(cfg=cfg):
                build_dir = Path(tmp).resolve()
                text = emit_cmake(cfg, build_dir)
                expected = self.expected_units(cfg, build_dir)
                for match in self.PROPS.finditer(text):
                    stem, group = expected[match.group(1)]
                    options, defines = unit_flags(cfg, stem, group)
                    self.assertEqual(cmake_list(match.group(2)), options + ['-D' + name for name in defines],
                                     match.group(1))
                link = re.search(r'set_target_properties\(lean_mpas PROPERTIES [^)]*LINK_OPTIONS "([^"]*)"',
                                 text)
                self.assertEqual(cmake_list(link.group(1)), link_flags(cfg))

    def test_generated_cmake_has_one_object_library_per_group(self):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp).resolve()
            text = emit_cmake(GPU, build_dir)
            groups = list(load_units()['groups'])
            self.assertEqual(re.findall(r'add_library\((\w+) OBJECT', text), groups)
            for group in groups:
                self.assertIn(f'set_target_properties({group} PROPERTIES Fortran_MODULE_DIRECTORY '
                              f'"{build_dir}/mod/{group}"', text)
                includes = re.search(rf'target_include_directories\({group} PRIVATE([^)]*)\)', text).group(1)
                for other in groups:
                    self.assertIn(f'"{build_dir}/mod/{other}"', includes)
            esmf = re.findall(r'target_include_directories\(esmf_time PRIVATE([^)]*)\)', text)
            self.assertTrue(any(f'"{ROOT}/third_party/mpas-8.4.2/src/external/esmf_time_f90"' in i for i in esmf))
            self.assertIn('add_executable(lean_mpas', text)
            for group in groups:
                self.assertIn(f'$<TARGET_OBJECTS:{group}>', text)

    def test_generated_cmake_sets_no_compile_definitions(self):
        """Defines go in as -D options only: CMake's Ninja rule for a Fortran file with Fortran_PREPROCESS OFF has no
        $DEFINES, so a COMPILE_DEFINITIONS property would be dropped silently (-DSINGLE_PRECISION would never reach
        the compiler)."""
        setters = re.compile(r'COMPILE_DEFINITIONS\s+"|compile_definitions\s*\(|add_definitions\s*\(', re.IGNORECASE)
        for cfg in (CPU_GF, GPU):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(cfg=cfg):
                text = emit_cmake(cfg, Path(tmp).resolve())
                self.assertIsNone(setters.search(text))
                self.assertIn('-DSINGLE_PRECISION', text)
        cmakelists = (ROOT / 'CMakeLists.txt').read_text(encoding='utf-8')
        self.assertIsNone(setters.search(cmakelists))

    PREPROCESSED = re.compile(r'set_property\(SOURCE "([^"]+)" PROPERTY Fortran_PREPROCESS ON\)')

    def flavor(self, name):
        with mock.patch('tools.lean.build.build', return_value=0) as build:
            self.assertEqual(cli.main(['build', '--flavor', name, '--dry-run', '--jobs', '2']), 0)
        return build.call_args.args[0]

    def preprocess_paths(self, cfg, build_dir):
        return {str((build_dir if rel.startswith('noahmp_lean/') else ROOT) / rel)
                for rel in config.preprocess_set(cfg)}

    def test_ifdef_module_files_preprocessed(self):
        cfg = self.flavor('lean')
        preprocessed = config.preprocess_set(cfg)
        self.assertLessEqual({'src/lean_model.f90'}, preprocessed)
        self.assertNotIn('src/noahmp_core.f90', preprocessed)
        self.assertNotIn('src/device/noahmp_core.f90', preprocessed)
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp).resolve()
            text = config.cmake_file(cfg, build_dir, nc=FAKE)
            self.assertEqual(set(self.PREPROCESSED.findall(text)), self.preprocess_paths(cfg, build_dir))

    def test_plain_preprocess_unchanged(self):
        for cfg, expected in ((CPU_GF, set()), (CPU_NV, set()), (GPU, {'src/lean_model.f90', 'src/dyn_micro.f90'})):
            with self.subTest(cfg=cfg):
                self.assertEqual(config.preprocess_set(cfg), expected)

    def test_noahmp_runtime_modules_need_no_hidden_use_preprocessing(self):
        pieces = {'noahmp_lean/' + name + '.f90' for name in (
            'ResistanceAboveCanopyMostMod', 'ResistanceBareGroundMostMod',
            'SurfaceEnergyFluxVegetatedMod', 'SurfaceEnergyFluxBareGroundMod')}
        for target, fc in (('cpu', 'gfortran'), ('gpu', 'nvfortran')):
            cfg = BuildConfig(target, fc, 'strict', switches=(('LEAN_NOAHMP_THETA', 'on'),))
            with tempfile.TemporaryDirectory() as tmp, self.subTest(target=target):
                build_dir = Path(tmp).resolve()
                preprocessed = config.preprocess_set(cfg)
                self.assertFalse(pieces & preprocessed)
                self.assertFalse(any(path.endswith('noahmp_core.f90') for path in preprocessed))
                emitted = set(self.PREPROCESSED.findall(config.cmake_file(cfg, build_dir, nc=FAKE)))
                self.assertEqual(emitted, self.preprocess_paths(cfg, build_dir))
                self.assertFalse({str(build_dir / path) for path in pieces} & emitted)

    def test_generated_cmake_preprocesses_lean_model_on_the_card_only(self):
        """On the card, `use phys_dev` in src/lean_model.f90 sits under #ifdef LEAN_PHYS_DEV, which CMake's scan of
        the raw source skips (it does not know the -D options): CMake must preprocess that file itself
        (Fortran_PREPROCESS ON) so that the scan sees the use and orders phys_dev first."""
        for cfg in (GPU, CPU_GF, CPU_NV, BuildConfig('cpu', 'gfortran', 'strict', omp=True)):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(cfg=cfg):
                build_dir = Path(tmp).resolve()
                text = emit_cmake(cfg, build_dir)
                expected = self.preprocess_paths(cfg, build_dir)
                self.assertEqual(set(self.PREPROCESSED.findall(text)), expected)
                self.assertEqual(text.count('PROPERTY Fortran_PREPROCESS'), len(expected))

    def test_every_hidden_use_is_preprocessed(self):
        """Tripwire: switched builds preprocess every guarded use/module, including untaken branches.
        Plain builds preprocess only the files whose dependencies the build's own defines hide from the raw scan."""
        directive = re.compile(r'\s*#\s*(ifdef|ifndef|if|elif|else|endif)\b\s*(\w*)')
        dependency = re.compile(r'\s*(?:use\b|module\s+(?!procedure\b|subroutine\b|function\b)\w)', re.IGNORECASE)
        for cfg in (GPU, CPU_GF, CPU_NV, self.flavor('lean'),
                    BuildConfig('gpu', 'nvfortran', 'strict', switches=self.flavor('lean').switches),
                    BuildConfig('cpu', 'gfortran', 'strict', switches=(('LEAN_BUDGET', 'off'),)),
                    BuildConfig('cpu', 'gfortran', 'strict', switches=(('LEAN_NOAHMP_THETA', 'on'),)),
                    BuildConfig('gpu', 'nvfortran', 'strict', switches=(('LEAN_RAD_BLCLOUD', 'on'),))):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(cfg=cfg):
                build_dir = Path(tmp).resolve()
                preprocessed = set(self.PREPROCESSED.findall(config.cmake_file(cfg, build_dir, nc=FAKE)))
                switched = bool(cfg.switches)
                hidden = set()
                for path, unit, group in config.compile_units(cfg, build_dir):
                    options, defines = unit_flags(cfg, unit, group)
                    defined = {name.split('=')[0] for name in defines} | ({'_OPENACC'} if '-acc' in options else set())
                    stack = []                      # one entry per open #if: True when the scan skips a taken branch
                    for line in path.read_text(encoding='utf-8', errors='replace').splitlines():
                        match = directive.match(line)
                        if match and match.group(1) in ('ifdef', 'ifndef', 'if'):
                            stack.append(switched or (match.group(1) == 'ifdef' and match.group(2) in defined))
                        elif match and match.group(1) in ('elif', 'else') and stack:
                            stack[-1] = switched
                        elif match and match.group(1) == 'endif' and stack:
                            stack.pop()
                        elif any(stack) and dependency.match(line):
                            hidden.add(str(path))
                self.assertLessEqual(hidden, preprocessed)
                if switched:
                    self.assertEqual(hidden, preprocessed)
                elif cfg.target == 'gpu':
                    self.assertEqual(hidden, {str(ROOT / 'src/lean_model.f90'), str(ROOT / 'src/dyn_micro.f90')})
                self.assertEqual(preprocessed, self.preprocess_paths(cfg, build_dir))

    def test_preprocess_list_names_compiled_files(self):
        for cfg in (CPU_GF, GPU, self.flavor('lean'),
                    BuildConfig('gpu', 'nvfortran', 'strict', switches=(('LEAN_BUDGET', 'off'),))):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(cfg=cfg):
                build_dir = Path(tmp).resolve()
                compiled = {str(path) for path, _, _ in config.compile_units(cfg, build_dir)}
                preprocessed = self.preprocess_paths(cfg, build_dir)
                self.assertLessEqual(preprocessed, compiled)
                ice_dir = 'src/device' if cfg.target == 'gpu' else 'src'
                ice = {str(ROOT / ice_dir / f'{name}.f90') for name in
                       ('sea_ice', 'sea_ice_noah', 'land_ice_exchange')} | {str(ROOT / 'src/land_ice.f90')}
                self.assertFalse(preprocessed & ice)

    def test_preprocess_of_a_file_not_compiled_is_refused(self):
        units = load_units()
        units['cmake_preprocess'] = {'gpu': ['src/no_such_file.f90']}
        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(config, 'load_units', return_value=units):
            with self.assertRaisesRegex(ValueError, 'no_such_file.f90'):
                config.cmake_file(GPU, Path(tmp).resolve(), nc=FAKE)


def configure_strict_cpu(build_dir: Path, *extra: str, env_extra=None):
    """A configure of CMakeLists.txt for a strict gfortran CPU build: it only probes the compiler (one process at a
    time); nothing is built."""
    env = {**os.environ, **(env_extra or {})}
    return subprocess.run(['cmake', '-G', 'Ninja', '-S', str(ROOT), '-B', str(build_dir),
                           f'-DCMAKE_Fortran_COMPILER={shutil.which("gfortran")}', '-DLEAN_TARGET=cpu',
                           '-DLEAN_FC=gfortran', '-DLEAN_PROFILE=strict', *extra],
                          env=env, capture_output=True, text=True)


@unittest.skipUnless(sys.platform == 'darwin' and shutil.which('cmake') and shutil.which('ninja')
                     and shutil.which('gfortran'), 'needs macOS with cmake, ninja and gfortran')
class CMakeListsMacTest(unittest.TestCase):
    """CMakeLists.txt itself keeps the macOS SDK flags out: a configure with no
    -DCMAKE_OSX_SYSROOT= must not add -isysroot, and an exported MACOSX_DEPLOYMENT_TARGET must not add
    -mmacosx-version-min."""

    def configure(self, build_dir: Path, *extra: str, env_extra=None):
        return configure_strict_cpu(build_dir, *extra, env_extra=env_extra)

    def test_bare_configure_adds_no_sdk_flags(self):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp).resolve()
            done = self.configure(build_dir, env_extra={'MACOSX_DEPLOYMENT_TARGET': '14.0'})
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            for name in ('build.ninja', 'CMakeFiles/rules.ninja'):
                text = (build_dir / name).read_text(encoding='utf-8')
                for flag in ('-isysroot', '-mmacosx-version-min'):
                    self.assertFalse(flag in text, f'{flag} in {name}')

    def test_explicit_sdk_flags_are_refused(self):
        for define in ('-DCMAKE_OSX_SYSROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk',
                       '-DCMAKE_OSX_DEPLOYMENT_TARGET=14.0'):
            with tempfile.TemporaryDirectory() as tmp, self.subTest(define=define):
                done = self.configure(Path(tmp).resolve(), define)
                self.assertNotEqual(done.returncode, 0)
                self.assertIn('leave it empty', ' '.join((done.stdout + done.stderr).split()))

    def test_empty_sysroot_is_still_accepted(self):
        with tempfile.TemporaryDirectory() as tmp:
            done = self.configure(Path(tmp).resolve(), '-DCMAKE_OSX_SYSROOT=')
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)


@unittest.skipUnless(shutil.which('cmake') and shutil.which('ninja') and shutil.which('gfortran'),
                     'needs cmake, ninja and gfortran')
class CMakeListsLinkTest(unittest.TestCase):
    """No -Wl,--dependency-file on the link line. CMake 3.27 and later ask a linker that can (GNU ld on Linux) for
    its own dependency file; lean's link line has none. On macOS the linker cannot, so this test bites on Linux."""

    def test_link_rule_has_no_linker_depfile(self):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp).resolve()
            done = configure_strict_cpu(build_dir)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            for name in ('build.ninja', 'CMakeFiles/rules.ninja'):
                self.assertFalse('--dependency-file' in (build_dir / name).read_text(encoding='utf-8'),
                                 f'--dependency-file in {name}')


class FlagsTest(unittest.TestCase):

    def check(self, cfg, unit, group, options, defines=BASE_DEFINES):
        got_options, got_defines = unit_flags(cfg, unit, group)
        self.assertEqual(got_options, options)
        self.assertEqual(set(got_defines), set(defines))

    def test_gfortran_strict(self):
        self.check(CPU_GF, 'dyn_tend', 'model', GF_STRICT)

    def test_gfortran_strict_omp(self):
        self.check(BuildConfig('cpu', 'gfortran', 'strict', omp=True), 'dyn_tend', 'model', GF_STRICT + ['-fopenmp'])

    def test_gfortran_debug(self):
        poison = ['-finit-real=snan', '-fcheck=all', '-ffpe-trap=invalid,zero,overflow', '-fbacktrace']
        self.check(BuildConfig('cpu', 'gfortran', 'debug'), 'dyn_tend', 'model', GF + ['-O0'] + poison)
        # with OpenMP, the debug profile puts -fopenmp right after -O0, before the checking flags
        self.check(BuildConfig('cpu', 'gfortran', 'debug', omp=True), 'dyn_tend', 'model',
                   GF + ['-O0', '-fopenmp'] + poison)

    def test_nv_debug(self):
        self.check(BuildConfig('cpu', 'nvfortran', 'debug'), 'radiation_lw', 'driver',
                   ['-Mpreprocess', '-Mfree', '-O0', '-Kieee', '-Mnofma', '-Mbounds', '-Ktrap=fp'])

    def test_nv_radiation_lw(self):
        self.check(CPU_NV, 'radiation_lw', 'driver', ['-Mpreprocess', '-Mfree', '-O2', '-Kieee', '-Mnofma', '-Mnoinline'])

    def test_nv_omp_order(self):
        self.check(BuildConfig('cpu', 'nvfortran', 'strict', omp=True), 'gf_core', 'model',
                   NV_STRICT + ['-mp', '-Mnovect'])

    def test_gpu_gf_core(self):
        self.check(GPU, 'gf_core', 'model', NV_STRICT + ['-Mnovect'] + CARD)

    def test_gpu_lean_warp(self):
        options, _ = unit_flags(GPU, 'lean_warp', 'base')
        self.assertEqual(options[-2:], ['-Minfo=accel', '-cuda'])
        self.assertEqual(options, NV_STRICT + CARD + ['-cuda'])

    def test_gpu_defines(self):
        self.check(GPU, 'dyn_acoustic', 'model', NV_STRICT + CARD, BASE_DEFINES | {'EXNER_EXACT'})
        self.check(GPU, 'lean_model', 'driver', NV_STRICT + CARD, BASE_DEFINES | {'LEAN_PHYS_DEV'})
        self.check(CPU_NV, 'dyn_acoustic', 'model', NV_STRICT)
        self.check(CPU_NV, 'lean_model', 'driver', NV_STRICT)

    def test_noahmp_lean(self):
        self.assertEqual(unit_flags(CPU_GF, 'NoahmpMainMod', 'noahmp_lean')[0], GF_STRICT + ['-std=f2018'])
        self.assertEqual(unit_flags(GPU, 'NoahmpMainMod', 'noahmp_lean')[0], NV_STRICT + ['-acc', '-gpu=cc120,nofma'])
        # the closures get no per-unit rule: nvmath in the gpu noahmp_lean group has no -Minfo=accel
        self.assertEqual(unit_flags(GPU, 'nvmath', 'noahmp_lean')[0], NV_STRICT + ['-acc', '-gpu=cc120,nofma'])
        self.assertEqual(unit_flags(GPU, 'nvmath', 'model')[0], NV_STRICT + CARD)

    def test_esmf_hide_mpi(self):
        self.check(CPU_GF, 'ESMF_ClockMod', 'esmf_time', GF_STRICT, BASE_DEFINES | {'HIDE_MPI'})
        self.check(CPU_GF, 'dyn_tend', 'model', GF_STRICT)

    def test_gpu_link(self):
        self.assertEqual(link_flags(GPU), ['-Mpreprocess', '-Mfree', '-O2', '-Kieee', '-Mnofma', '-acc',
                                           '-gpu=cc120,nofma', '-cuda'])
        self.assertEqual(link_flags(CPU_GF), GF_STRICT)
        self.assertEqual(link_flags(BuildConfig('cpu', 'gfortran', 'strict', omp=True)), GF_STRICT + ['-fopenmp'])

    def test_unknown_group_is_refused(self):
        with self.assertRaises(ValueError):
            unit_flags(CPU_GF, 'dyn_tend', 'nonsense')

    def test_variants(self):
        """The gfortran envelope variants, -mcpu=native on arm64 and -march=native on x86-64.

        A variant's flags take the place of the profile flags; common flags, -fopenmp and the unit rules stay."""
        expected = {('envelope', 'o2fma'): ['-O2'], ('envelope', 'o3'): ['-O3'], ('envelope', 'o3native'): ['-O3', 'N']}
        for machine, native in (('arm64', '-mcpu=native'), ('x86_64', '-march=native')):
            for (profile, variant), flags in expected.items():
                flags = [native if flag == 'N' else flag for flag in flags]
                cfg = BuildConfig('cpu', 'gfortran', profile, variant=variant)
                omp = BuildConfig('cpu', 'gfortran', profile, omp=True, variant=variant)
                with self.subTest(machine=machine, profile=profile, variant=variant), \
                        mock.patch.object(config, 'machine', return_value=machine):
                    self.check(cfg, 'dyn_tend', 'model', GF + flags)
                    self.check(omp, 'dyn_tend', 'model', GF + flags + ['-fopenmp'])
                    self.check(cfg, 'NoahmpVarInitMod', 'noahmp_lean', GF + flags + ['-std=f2018'])
                    self.assertEqual(link_flags(cfg), GF + flags)
        variants = load_flags()['gfortran']['variants']
        self.assertEqual({profile: sorted(table) for profile, table in variants.items()},
                         {'envelope': ['o2fma', 'o3', 'o3native'],
                          'fast': ['fast']})
        self.assertEqual({entry['status'] for entry in variants['envelope'].values()}, {'envelope'})
        kept = {name: entry['status'] for name, entry in variants['fast'].items()}
        self.assertEqual(kept, {'fast': 'supported'})
        self.assertTrue(variants['fast']['fast']['platforms'])
        self.assertTrue(all(entry['why'] for table in variants.values() for entry in table.values()))
        with self.assertRaisesRegex(ValueError, 'fast needs --variant'):
            BuildConfig('cpu', 'gfortran', 'fast')
        with self.assertRaisesRegex(ValueError, "no envelope variant 'fast' for gfortran"):
            link_flags(BuildConfig('cpu', 'gfortran', 'envelope', variant='fast'))
        with mock.patch.object(config, 'machine', return_value='ppc64le'):
            self.assertEqual(link_flags(BuildConfig('cpu', 'gfortran', 'envelope', variant='o3')), GF + ['-O3'])
            with self.assertRaisesRegex(ValueError, "envelope variant 'o3native' of gfortran has no flags for ppc64le"):
                link_flags(BuildConfig('cpu', 'gfortran', 'envelope', variant='o3native'))

    def test_compile_classes(self):
        """A variant with classes: each unit takes the flags of its class (compile_classes), the link the kernel's;
        unit rules stay on top; builds without classes ignore the table."""
        fast = BuildConfig('cpu', 'gfortran', 'fast', omp=True, variant='fast')
        kernel = GF + ['-O3', '-march=native', '-fno-math-errno', '-funroll-loops', '-fopenmp']
        with mock.patch.object(config, 'machine', return_value='x86_64'):
            self.assertEqual(config.unit_class(fast, 'dyn_tend', 'model'), 'kernel')
            self.assertEqual(config.unit_class(fast, 'lean_writer', 'model'), 'infrastructure')
            self.assertEqual(config.unit_class(fast, 'ESMF_TimeMod', 'esmf_time'), 'infrastructure')
            self.check(fast, 'dyn_tend', 'model', kernel)
            self.check(fast, 'mpas_atmphys_functions', 'physics_support', kernel)
            self.check(fast, 'lean_writer', 'model', GF + ['-O2', '-fopenmp'])
            self.check(fast, 'NoahmpVarInitMod', 'noahmp_lean', kernel + ['-std=f2018'])
            self.assertEqual(link_flags(fast), kernel)
            # no classes: the infrastructure units keep the profile flags
            self.check(CPU_GF, 'lean_writer', 'model', GF_STRICT)
            self.check(BuildConfig('cpu', 'gfortran', 'envelope', variant='o3'), 'main', 'driver', GF + ['-O3'])
        table = load_flags()['compile_classes']
        used = {table['default']} | {entry['class'] for entry in table['members']}
        self.assertTrue(all(entry['why'] for entry in table['members']))
        for fc in COMPILERS:
            for profile, variants in load_flags()[fc]['variants'].items():
                for name, variant in variants.items():
                    if 'classes' in variant:
                        with self.subTest(fc=fc, variant=name):
                            self.assertLessEqual(used, set(variant['classes']))
        compiled = {Path(rel).stem for rel, _ in sources(BuildConfig('cpu', 'gfortran', 'strict'))}
        named = {unit for entry in table['members'] for unit in entry.get('units', [])}
        self.assertLessEqual(named - {'lean_build_info'}, compiled)

    def test_ifx(self):
        """ifx: free form with its preprocessor, -qopenmp last, the fast variant by classes."""
        IFX = ['-fpp', '-free']
        strict = BuildConfig('cpu', 'ifx', 'strict', omp=True)
        self.check(strict, 'dyn_tend', 'model', IFX + ['-O2', '-fp-model=strict', '-no-fma', '-qopenmp'])
        fast = BuildConfig('cpu', 'ifx', 'fast', omp=True, variant='fast')
        self.check(fast, 'dyn_tend', 'model', IFX + ['-O3', '-xHost', '-qopenmp'])
        self.check(fast, 'main', 'driver', IFX + ['-O2', '-qopenmp'])
        self.assertEqual(load_flags()['ifx']['variants']['fast']['fast']['status'], 'supported')
        with self.assertRaises(ValueError):
            BuildConfig('gpu', 'ifx', 'strict')

    def test_machine_names(self):
        for raw, name in (('arm64', 'arm64'), ('aarch64', 'arm64'), ('x86_64', 'x86_64'), ('AMD64', 'x86_64')):
            with self.subTest(raw=raw), mock.patch('platform.machine', return_value=raw):
                self.assertEqual(config.machine(), name)


class ConfigTest(unittest.TestCase):

    def test_build_config_checks(self):
        with self.assertRaisesRegex(ValueError, 'fast needs --variant'):
            BuildConfig('cpu', 'gfortran', 'fast')
        for bad in (dict(target='tpu'), dict(fc='ifort'), dict(profile='quick'), dict(target='gpu'),
                    dict(target='gpu', fc='nvfortran', omp=True), dict(variant='o3')):
            args = {'target': 'cpu', 'fc': 'gfortran', 'profile': 'strict', **bad}
            with self.subTest(**bad), self.assertRaises(ValueError):
                BuildConfig(**args)

    def test_build_dir_name(self):
        self.assertEqual(build_dir_name(CPU_GF), 'cpu-gfortran-strict')
        self.assertEqual(build_dir_name(GPU), 'gpu-nvfortran-strict')
        self.assertEqual(build_dir_name(BuildConfig('cpu', 'nvfortran', 'strict', omp=True)), 'cpu-nvfortran-strict-omp')
        self.assertEqual(build_dir_name(BuildConfig('cpu', 'gfortran', 'envelope', omp=True, variant='o3native')),
                         'cpu-gfortran-envelope-o3native-omp')
        self.assertRegex(build_dir_name(BuildConfig('cpu', 'gfortran', 'strict', switches=(('LEAN_MYNN_DEW', 'on'),))),
                         r'^cpu-gfortran-strict-[0-9a-f]{8}$')
        self.assertEqual(build_dir_name(BuildConfig('gpu', 'nvfortran', 'strict', levels=45)),
                         'gpu-nvfortran-strict-levels45')

    def test_fixed_levels_define_only_the_levels_unit(self):
        for cfg in (replace(CPU_GF, levels=45), replace(GPU, levels=45)):
            with self.subTest(target=cfg.target):
                self.assertIn('LEAN_NZ=45', unit_flags(cfg, 'levels', 'base')[1])
                self.assertNotIn('LEAN_NZ=45', unit_flags(cfg, 'config', 'base')[1])
                self.assertNotIn('LEAN_NZ=45', config.global_defines(cfg))
        for cfg in (CPU_GF, GPU):
            self.assertFalse([name for name in unit_flags(cfg, 'levels', 'base')[1] if name.startswith('LEAN_NZ')])
        for bad in (0, config.max_levels() + 1):
            with self.subTest(levels=bad), self.assertRaisesRegex(ValueError, 'levels'):
                BuildConfig('cpu', 'gfortran', 'strict', levels=bad)

    def test_tables_load(self):
        flags, units, lock = load_flags(), load_units(), load_lock()
        self.assertEqual(flags['nvfortran']['card'], {'compile': CARD, 'link': ['-acc', '-gpu=cc120,nofma', '-cuda']})
        self.assertEqual(len(flags['unit_rules']), 10)
        self.assertTrue(all(rule['why'] for rule in flags['unit_rules']))
        self.assertEqual(len(units['device_overrides']), 36)   # including the five hand-kept card dynamics files
        self.assertEqual(lock['mpas_model']['commit'], 'f34984b2c89353cf31c235b08d3b4acd96b37bb3')
        self.assertEqual(sorted(lock['tables']), ['MP_THOMPSON_QIautQS_DATA.DBL', 'MP_THOMPSON_QRacrQG_DATA.DBL',
                                                  'MP_THOMPSON_QRacrQS_DATA.DBL', 'MP_THOMPSON_freezeH2O_DATA.DBL'])

    def test_lock_table_sizes(self):
        for name, table in load_lock()['tables'].items():
            path = ROOT / 'build' / 'tables' / name
            if path.exists():
                self.assertEqual(path.stat().st_size, table['size'], name)


class SourcesTest(unittest.TestCase):

    def test_namelist_units_in_base(self):
        paths = ['src/run/lean_messages.f90', 'src/run/lean_namelist_options.f90',
                 'src/run/lean_namelist.f90', 'src/run/lean_guard.f90',
                 'src/run/lean_log.f90', 'src/run/lean_summary.f90']
        for cfg in (CPU_GF, CPU_NV, GPU):
            with self.subTest(cfg=cfg):
                listed = sources(cfg)
                base = [path for path, group in listed if group == 'base']
                self.assertEqual(base[-6:], paths)
                for path in paths:
                    self.assertEqual([group for p, group in listed if p == path], ['base'])
                    flags = unit_flags(cfg, 'serial_services', 'base')
                    if path.endswith('lean_summary.f90') and cfg.target == 'gpu':
                        flags = (flags[0] + CARD, flags[1])
                    self.assertEqual(unit_flags(cfg, Path(path).stem, 'base'), flags)

    def test_stream_units_in_physics_support(self):
        paths = ['src/run/lean_clock.f90', 'src/run/lean_xml.f90', 'src/run/lean_stream_registry.f90',
                 'src/run/lean_streams.f90', 'src/run/lean_field_catalog.f90']
        for cfg in (CPU_GF, CPU_NV, GPU):
            with self.subTest(cfg=cfg):
                listed = sources(cfg)
                support = [path for path, group in listed if group == 'physics_support']
                self.assertEqual(support[-len(paths):], paths)
                for path in paths:
                    self.assertEqual([group for p, group in listed if p == path], ['physics_support'])
                    self.assertEqual(unit_flags(cfg, Path(path).stem, 'physics_support'),
                                     unit_flags(cfg, 'radar_alias', 'physics_support'))

    def test_input_units_in_model(self):
        # Input reading fills the model's own types, so it is compiled in the model group, last.
        paths = ['src/io/lean_input_fields.f90', 'src/io/lean_lbc.f90', 'src/io/lean_inputs.f90',
                 'src/io/lean_restart_layout.f90', 'src/io/lean_restart_file.f90',
                 'src/io/lean_run_inputs.f90', 'src/io/lean_writer.f90']
        for cfg in (CPU_GF, CPU_NV, GPU):
            with self.subTest(cfg=cfg):
                listed = sources(cfg)
                model = [path for path, group in listed if group == 'model' and path.startswith('src/io/')]
                self.assertEqual(model, paths)
                for path in paths:
                    self.assertEqual([group for p, group in listed if p == path], ['model'])
                    self.assertEqual(unit_flags(cfg, Path(path).stem, 'model'),
                                     unit_flags(cfg, 'model_start', 'model'))

    def test_gpu_sources(self):
        gpu = [path for path, _ in sources(GPU)]
        cpu = [path for path, _ in sources(CPU_NV)]
        self.assertIn('src/device/thompson.f90', gpu)
        self.assertNotIn('src/thompson.f90', gpu)
        for path in ('src/phys_check.f90', 'src/phys_dev.f90', 'src/device/radiation_device_tables.f90'):
            self.assertIn(path, gpu)
            self.assertNotIn(path, cpu)
        self.assertIn('src/thompson.f90', cpu)
        self.assertIn(('src/device/noahmp_core.f90', 'noahmp_lean'), sources(GPU))
        self.assertIn(('src/nvmath.f90', 'noahmp_lean'), sources(GPU))
        self.assertNotIn(('src/nvmath.f90', 'noahmp_lean'), sources(CPU_NV))

    def test_sources_exist(self):
        for cfg in (CPU_GF, GPU):
            for path, group in sources(cfg):
                with self.subTest(path=path, group=group):
                    self.assertTrue((ROOT / path).is_file())

    def test_closure_groups_come_from_closure_json(self):
        closure = json.loads((ROOT / 'third_party' / 'mpas-8.4.2' / 'closure.json').read_text())
        by_group = {}
        for path, group in sources(CPU_GF):
            by_group.setdefault(group, []).append(path)
        prefix = 'third_party/mpas-8.4.2/src/'
        self.assertEqual(by_group['noahmp_reference'], [prefix + p for p in closure['noahmp']])
        self.assertEqual(by_group['esmf_time'], [prefix + p for p in closure['esmf_time'] if p.endswith('.F90')])

class SplitTest(unittest.TestCase):

    def test_split_keeps_order_and_leaves_unchanged_files_alone(self):
        with tempfile.TemporaryDirectory() as tmp:
            core = Path(tmp) / 'core.f90'
            core.write_text('! header\n! Source: a/B.F90\nmodule b\nend\n! Source: c/A.F90\nmodule a\nend\n')
            out = Path(tmp) / 'out'
            pieces = split_noahmp(core, out)
            self.assertEqual([p.name for p in pieces], ['B.f90', 'A.f90'])
            self.assertEqual(pieces[0].read_text(), 'module b\nend\n')
            before = pieces[1].stat().st_mtime_ns
            (out / 'B.f90').write_text('stale')
            split_noahmp(core, out)
            self.assertEqual(pieces[0].read_text(), 'module b\nend\n')
            self.assertEqual(pieces[1].stat().st_mtime_ns, before)


class CardUnitsTest(unittest.TestCase):

    def test_run_settings_card_unit(self):
        self.assertIn(('src/run_settings.f90', 'base'), sources(GPU))
        self.assertTrue(all(f in unit_flags(GPU, 'run_settings', 'base')[0] for f in CARD))
        self.assertFalse(any(f in unit_flags(GPU, 'config', 'base')[0] for f in CARD))


if __name__ == '__main__':
    unittest.main()
