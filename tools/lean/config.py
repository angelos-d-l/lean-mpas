"""The flag resolver: which files a lean_mpas build compiles, and with which flags.

The data lives in files at the repo root, this module only reads them:
- config/flags.json   compiler flags: common, defines, profiles, omp, the card flags, and the ordered unit_rules;
- config/switches.json  explicit build switches, their kinds and requirements;
- config/presets.json  the two flavors (mpas, lean) and their switch sets;
- config/units.json   the source files by group, in compile order, with the card-only files and overrides;
- lean.lock.json      pins: upstream commits, toolchains, the Thompson tables.

A build is a BuildConfig. sources(cfg) lists its files, unit_flags(cfg, unit, group) gives one file's options and
defines, link_flags(cfg) the link options, build_dir_name(cfg) its folder under build/.

    python3 -m tools.lean.config cmake --target cpu|gpu --fc gfortran|nvfortran|ifx --profile P [--omp] [--variant V]
                                       --out FILE
    python3 -m tools.lean.config cmake --config-file BUILD_CONFIG_JSON --out FILE

writes the CMake file of a build, which CMakeLists.txt include()s: cmake_file(cfg, build_dir, nc=None) gives its text. The
Noah-MP core is split into its compilation units under the folder of FILE (the build directory).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import platform
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from tools.lean import netcdf
from tools.lean.netcdf import NetCDF
from tools.lean.split_noahmp import MARKER, split_noahmp

ROOT = Path(__file__).resolve().parents[2]
TARGETS = ('cpu', 'gpu')
COMPILERS = ('gfortran', 'nvfortran', 'ifx')
PROFILES = ('strict', 'debug', 'fast', 'envelope')
NEEDS_VARIANT = ('fast', 'envelope')
COMPILED = ('.f90', '.F90', '.F', '.f')    # closure.json also lists *.inc files, which are included, not compiled
FLAVORS = ('mpas', 'lean')
CUSTOM_FLAVOR = 'custom'
# What the generated CMake file is made from: CMake reruns the emitter when one of these changes. closure.json gives
# the closure sources, and the emitter's own modules can change what it writes.
CONFIGURE_DEPENDS = ('config/flags.json', 'config/units.json', 'lean.lock.json', 'src/noahmp_core.f90',
                     'src/device/noahmp_core.f90', 'third_party/mpas-8.4.2/closure.json', 'tools/lean/config.py',
                     'tools/lean/split_noahmp.py', 'tools/lean/netcdf.py', 'config/switches.json',
                     'config/presets.json', 'tools/lean/build_info.py')


@dataclass(frozen=True)
class BuildConfig:
    target: str
    fc: str
    profile: str
    omp: bool = False
    variant: Optional[str] = None
    switches: tuple[tuple[str, str], ...] = ()
    gpu_cc: Optional[int] = None      # the card's compute capability (89 for 8.9); None: lean.lock.json gpu.cc
    levels: Optional[int] = None      # the vertical level count fixed at compile time; None: from the mesh file

    def __post_init__(self):
        for value, allowed, name in ((self.target, TARGETS, 'target'), (self.fc, COMPILERS, 'fc'),
                                     (self.profile, PROFILES, 'profile')):
            if value not in allowed:
                raise ValueError(f'{name} must be one of {", ".join(allowed)}, not {value!r}')
        if self.profile in NEEDS_VARIANT and not self.variant:
            raise ValueError(f'{self.profile} needs --variant')
        if self.profile not in NEEDS_VARIANT and self.variant:
            raise ValueError(f'{self.profile} takes no --variant')
        if self.target == 'gpu' and self.fc != 'nvfortran':
            raise ValueError('the gpu target needs nvfortran')
        if self.target == 'gpu' and self.omp:
            raise ValueError('the gpu build runs one host thread: no --omp')
        if self.gpu_cc is not None and (self.target != 'gpu' or self.gpu_cc < 10):
            raise ValueError('--gpu-cc needs the gpu target and a compute capability such as 89 or 120')
        if self.levels is not None and not 1 <= self.levels <= max_levels():
            raise ValueError(f'--levels must be between 1 and {max_levels()} (max_levels in src/config.f90), '
                             f'not {self.levels}')
        switches = dict(self.switches)
        check_switches(switches)
        object.__setattr__(self, 'switches', tuple(sorted(switches.items())))


def _load(name: str) -> dict:
    return json.loads((ROOT / name).read_text(encoding='utf-8'))


def load_lock() -> dict:
    return _load('lean.lock.json')


def load_flags() -> dict:
    return _load('config/flags.json')


def load_units() -> dict:
    return _load('config/units.json')


def load_presets() -> dict:
    return _load('config/presets.json')


def max_levels() -> int:
    """The most vertical levels a column holds: max_levels of src/config.f90."""
    found = re.search(r'^\s*integer, parameter :: max_levels\s*=\s*(\d+)', (ROOT / 'src/config.f90').read_text(
        encoding='utf-8'), re.MULTILINE)
    if found is None:
        raise ValueError('src/config.f90 has no max_levels')
    return int(found.group(1))


def check_switches(switches: dict) -> None:
    """Check explicit values and dependencies. An off switch emits no define, including an integer switch."""
    table = _load('config/switches.json')['switches']
    for name, value in switches.items():
        if name not in table:
            raise ValueError(f'unknown switch {name!r} (config/switches.json)')
        kind = table[name]['kind']
        if kind == 'bool' and value not in ('on', 'off'):
            raise ValueError(f'{name} must be on or off, not {value!r}')
        if kind == 'int' and value != 'off' and (not isinstance(value, str) or
                                                not re.fullmatch(r'[+-]?[0-9]+', value)):
            raise ValueError(f'{name} must be an integer or off, not {value!r}')
    for name, value in switches.items():
        if value == 'off':
            continue
        for requirement in table[name]['requires']:
            if switches.get(requirement) != 'on':
                raise ValueError(f'{name} needs {requirement}=on')


def switch_defines(cfg: BuildConfig) -> list[str]:
    """Global defines of the explicit switches, in name order."""
    return [name if value == 'on' else f'{name}={value}' for name, value in cfg.switches if value != 'off']


def build_flavor(cfg: BuildConfig) -> str:
    """Match the selected physics set, excluding the developer switch LEAN_BUDGET."""
    active = set(switch_defines(cfg)) - {'LEAN_BUDGET'}
    presets = load_presets()
    for name in FLAVORS:
        candidate = presets[name]
        switches = {key if value == 'on' else f'{key}={value}'
                    for key, value in candidate['switches'].items() if value != 'off'}
        if active == switches:
            return candidate['flavor']
    return CUSTOM_FLAVOR


def global_defines(cfg: BuildConfig) -> list[str]:
    """Defines shared by every unit; unit_rules add only to their matching files."""
    defines = list(load_flags()[cfg.fc]['defines'])
    defines += [item for item in switch_defines(cfg) if item not in defines]
    return defines


def build_dir_name(cfg: BuildConfig) -> str:
    """target-fc-profile[-variant][-omp][-levelsN], then the flavor when it is not mpas (-lean) or, for a custom switch
    set (LEAN_BUDGET included), the first 8 hex digits of a hash of the build kind and the sorted global defines."""
    parts = [cfg.target, cfg.fc, cfg.profile]
    if cfg.variant:
        parts.append(cfg.variant)
    if cfg.omp:
        parts.append('omp')
    if cfg.levels is not None:
        parts.append(f'levels{cfg.levels}')
    name = '-'.join(parts)
    if not cfg.switches:
        return name
    flavor = build_flavor(cfg)
    if flavor != CUSTOM_FLAVOR and 'LEAN_BUDGET' not in switch_defines(cfg):
        return name if flavor == 'mpas' else f'{name}-{flavor}'
    identity = [cfg.target, cfg.fc, cfg.profile, cfg.variant, cfg.omp, sorted(global_defines(cfg))]
    digest = hashlib.sha256(json.dumps(identity, separators=(',', ':')).encode('utf-8')).hexdigest()[:8]
    return f'{name}-{digest}'


def sources(cfg: BuildConfig) -> list[tuple[str, str]]:
    """(path, group) of every file cfg compiles, in compile order; paths relative to the repo root.

    The closure groups come from third_party/mpas-8.4.2/closure.json. For the gpu target the card-only
    files are included and src/device replaces src for the device overrides (and for the noahmp_lean group). The
    noahmp_lean core file is listed once; split_noahmp() turns it into its compilation units. A {path, when} entry
    is included only when its define is in the global list.
    """
    units = load_units()
    gpu = cfg.target == 'gpu'
    defines = {name.partition('=')[0] for name in global_defines(cfg)}
    closure = None
    out = []
    for group, spec in units['groups'].items():
        if 'closure' in spec:
            if closure is None:
                closure = _load(units['closure_file'])
            root = units['closure_root']
            out += [(f'{root}/{path}', group) for path in closure[spec['closure']] if path.endswith(COMPILED)]
            continue
        card_only = units['gpu_only'].get(group, [])
        for entry in spec['sources']:
            if isinstance(entry, dict) and entry.get('when') and entry['when'] not in defines:
                continue
            path = entry['path'] if isinstance(entry, dict) else entry
            if path in card_only and not gpu:
                continue
            if gpu and path not in card_only:
                path = _device_path(path, spec, units['device_overrides'])
            out.append((path, group))
    return out


def _device_path(path: str, group: dict, overrides: list[str]) -> str:
    """The src/device copy of a src/ file that the card build compiles instead, else the path itself."""
    name = Path(path).name
    if path == f'src/{name}' and (Path(path).stem in overrides or 'gpu_dir' in group):
        return f'{group.get("gpu_dir", "src/device")}/{name}'
    return path


def machine() -> str:
    """The build machine as a key of a variant's per-machine flags: arm64 or x86_64 (others as platform names them)."""
    name = platform.machine().lower()
    return {'aarch64': 'arm64', 'amd64': 'x86_64'}.get(name, name)


def _machine_flags(cfg: BuildConfig, flags, what: str) -> list[str]:
    """flags as a list, or the list of this machine when flags holds one list per machine."""
    if isinstance(flags, dict):
        if machine() not in flags:
            raise ValueError(f'the {cfg.profile} variant {cfg.variant!r} of {cfg.fc} has no {what}flags for '
                             f'{machine()}')
        flags = flags[machine()]
    return list(flags)


def _variant(cfg: BuildConfig, table: dict) -> dict:
    variant = table['variants'].get(cfg.profile, {}).get(cfg.variant)
    if variant is None:
        raise ValueError(f'no {cfg.profile} variant {cfg.variant!r} for {cfg.fc} in config/flags.json')
    return variant


def _variant_flags(cfg: BuildConfig, table: dict, klass: str = 'kernel') -> list[str]:
    """The flags of cfg's variant in config/flags.json variants.<profile>; per-machine flags for this machine. A
    variant with classes gives the flags of class klass (the link takes the kernel class)."""
    variant = _variant(cfg, table)
    if 'classes' not in variant:
        return _machine_flags(cfg, variant['flags'], '')
    if klass not in variant['classes']:
        raise ValueError(f'the {cfg.profile} variant {cfg.variant!r} of {cfg.fc} has no {klass} class')
    return _machine_flags(cfg, variant['classes'][klass], f'{klass} ')


def has_classes(cfg: BuildConfig) -> bool:
    """True when cfg's variant gives its flags by compile class (config/flags.json compile_classes)."""
    return cfg.profile in NEEDS_VARIANT and 'classes' in _variant(cfg, load_flags()[cfg.fc])


def unit_class(cfg: BuildConfig, unit: str, group: str) -> str:
    """The compile class of a unit: the first compile_classes member entry that names it or its group (an entry
    with fc only for that compiler), else the default class."""
    table = load_flags()['compile_classes']
    for entry in table['members']:
        if entry.get('fc', cfg.fc) != cfg.fc:
            continue
        if group in entry.get('groups', []) or unit in entry.get('units', []):
            return entry['class']
    return table['default']


def _profile_flags(cfg: BuildConfig, table: dict, klass: str = 'kernel') -> list[str]:
    """The profile's (or the variant's, for klass when it has classes) options with the omp flags in place."""
    if cfg.profile in NEEDS_VARIANT:
        flags = _variant_flags(cfg, table, klass)
    else:
        flags = list(table['profiles'][cfg.profile])
    if cfg.omp:
        omp = table['omp']
        after = omp['after'].get(cfg.profile)
        at = flags.index(after) + 1 if after in flags else len(flags)
        flags[at:at] = omp['flags']
    return flags


def _matches(rule: dict, cfg: BuildConfig, profile: list[str], unit: str, group: str, unit_groups: list[str]) -> bool:
    when = rule['when']
    for key, value in (('fc', cfg.fc), ('target', cfg.target), ('profile', cfg.profile), ('variant', cfg.variant)):
        wanted = when.get(key, value)
        # a list names every value the rule applies to
        if wanted != value and not (isinstance(wanted, list) and value in wanted):
            return False
    if 'has' in when and when['has'] not in profile:
        return False
    if 'groups' in rule:
        return group in rule['groups']
    return group in unit_groups and unit in rule['units']


def unit_flags(cfg: BuildConfig, unit: str, group: str) -> tuple[list[str], list[str]]:
    """(options in command-line order, defines) for the file of stem `unit` in `group`.

    options = common + profile (with omp; for a variant with classes, the flags of the unit's class) + the options of
    each matching unit rule, in rule order; defines = the
    compiler's defines + switches + matching rule defines.
    """
    flags = load_flags()
    if group not in load_units()['groups']:
        raise ValueError(f'unknown group {group!r}')
    table = flags[cfg.fc]
    profile = _profile_flags(cfg, table, unit_class(cfg, unit, group) if has_classes(cfg) else 'kernel')
    options = list(table['common']) + profile
    defines = global_defines(cfg)
    for rule in flags['unit_rules']:
        if not _matches(rule, cfg, profile, unit, group, flags['unit_rule_groups']):
            continue
        extra = rule.get('options', [])
        if extra == 'card.compile':
            extra = table['card']['compile']
        options += extra
        defines += rule.get('defines', [])
    # Only the levels unit gets the fixed count: the other units see it through the levels module.
    if cfg.levels is not None and unit == 'levels':
        defines.append(f'LEAN_NZ={cfg.levels}')
    return gpu_cc_flags(cfg, options), defines


def gpu_cc_flags(cfg: BuildConfig, options: list[str]) -> list[str]:
    """-gpu=ccNNN of the flags table for the card's own compute capability (cfg.gpu_cc; unchanged when None)."""
    if cfg.gpu_cc is None:
        return list(options)
    return [re.sub(r'\bcc\d+', f'cc{cfg.gpu_cc}', option) if option.startswith('-gpu=') else option
            for option in options]


def link_flags(cfg: BuildConfig) -> list[str]:
    """common + profile (with omp; the kernel class of a variant with classes), + the card link flags for gpu. No
    defines: -D means nothing when linking."""
    table = load_flags()[cfg.fc]
    flags = list(table['common']) + _profile_flags(cfg, table)
    if cfg.target == 'gpu':
        flags += table['card']['link']
    return gpu_cc_flags(cfg, flags)


def compile_units(cfg: BuildConfig, build_dir: Path) -> list[tuple[Path, str, str]]:
    """(absolute path, unit, group) of every file cfg compiles, each file once, in compile order.

    The Noah-MP core is split into <build_dir>/<group>/<Name>.f90 (split_noahmp writes them). A file listed in two
    groups (config.f90 in base and model; nvmath.f90 in noahmp_lean and model on the card) is kept in its first group:
    a later group needs its module, so it must be built first. On the card the kept nvmath compile is the noahmp_lean
    one, without -Minfo=accel, which only prints messages.
    """
    # Imported here because build_info uses this module's build name and flavor resolver.
    from tools.lean.build_info import write_build_info
    write_build_info(cfg, build_dir)
    generated = load_units()['generated']
    units, seen = [], set()
    for rel, group in sources(cfg):
        if rel.endswith('noahmp_core.f90'):
            files = split_noahmp(ROOT / rel, build_dir / group)
        else:
            files = [ROOT / rel]
        for path in files:
            if path not in seen:
                seen.add(path)
                units.append((path, path.stem, group))
        for unit, entry in generated.items():
            if rel == entry['after']:
                path = build_dir / entry['path']
                units.append((path, unit, entry['group']))
    return units


def preprocess_set(cfg: BuildConfig) -> set[str]:
    """Compiled sources CMake must preprocess: the target's list of config/units.json cmake_preprocess, and with
    switches on, also those of its switched list that this build compiles.

    Paths are relative to ROOT, except noahmp_lean/<Name>.f90, relative to the build directory. These are the
    actual split compilation units; noahmp_core.f90 is never compiled. The switched list names the files of both
    branches of every guarded use/module; only those cfg compiles (sources(cfg): the enabled units, the host or the
    device copy) are kept.
    """
    table = load_units().get('cmake_preprocess', {})
    preprocess = set(table.get(cfg.target, []))
    if not cfg.switches:
        return preprocess
    compiled = set()
    for rel, group in sources(cfg):
        if rel.endswith('noahmp_core.f90'):
            for part in (ROOT / rel).read_text(encoding='utf-8').split(MARKER)[1:]:
                name = Path(part.split('\n', 1)[0]).stem
                compiled.add(f'{group}/{name}.f90')
        else:
            compiled.add(rel)
    return preprocess | (set(table.get('switched', [])) & compiled)


def _quote(value) -> str:
    """A CMake quoted argument: backslash, double quote and $ escaped."""
    text = str(value)
    return '"' + text.replace('\\', '\\\\').replace('"', '\\"').replace('$', '\\$') + '"'


def _options(options: list[str], what: str) -> str:
    """Options as one quoted CMake ;-list. CMake de-duplicates COMPILE_OPTIONS and LINK_OPTIONS, so a repeated
    option would silently disappear from the command: it is refused here instead, as is a ';' inside an option."""
    for option in options:
        if ';' in option:
            raise ValueError(f'{what}: an option holds a ";", which CMake reads as a list separator: {option!r}')
    repeated = sorted({option for option in options if options.count(option) > 1})
    if repeated:
        raise ValueError(f'{what}: CMake would drop the repeat of {" ".join(repeated)}')
    return _quote(';'.join(options))


def cmake_file(cfg: BuildConfig, build_dir: Path, nc: Optional[NetCDF] = None) -> str:
    """The CMake file of a build, which CMakeLists.txt include()s: one OBJECT library per group, the options and
    defines of every source (as COMPILE_OPTIONS, the defines as -D at the end), the module and include directories,
    and the lean_mpas executable.

    Each group links the groups before it, so CMake builds their modules first. Every group sees every group's
    module directory (nvfortran needs the sibling .mod directories).
    """
    nc = netcdf.find() if nc is None else nc
    build_dir = Path(build_dir)
    units = load_units()
    groups = list(units['groups'])
    module_dirs = [build_dir / units['groups'][group]['module_dir'] for group in groups]
    compiled = compile_units(cfg, build_dir)
    lines = ['# Generated by python3 -m tools.lean.config cmake from the flags table. Do not edit: CMake writes',
             f'# it again when one of its inputs changes. Build {build_dir_name(cfg)}.', '',
             'set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS']
    lines += [f'  {_quote(ROOT / rel)}' for rel in CONFIGURE_DEPENDS]
    lines.append(')')
    for index, group in enumerate(groups):
        spec = units['groups'][group]
        lines += ['', f'add_library({group} OBJECT']
        lines += [f'  {_quote(path)}' for path, _, in_group in compiled if in_group == group]
        lines.append(')')
        lines.append(f'set_target_properties({group} PROPERTIES Fortran_MODULE_DIRECTORY {_quote(module_dirs[index])})')
        includes = module_dirs + ([ROOT / spec['include_dir']] if 'include_dir' in spec else [])
        lines.append(f'target_include_directories({group} PRIVATE')
        lines += [f'  {_quote(path)}' for path in includes]
        lines.append(')')
        if index:
            lines.append(f'target_link_libraries({group} PRIVATE {" ".join(groups[:index])})')
    lines += ['', '# The defines are compile options (-D): CMake leaves COMPILE_DEFINITIONS out of the Ninja compile',
              '# of a Fortran file it does not preprocess itself (Fortran_PREPROCESS OFF, set in CMakeLists.txt).']
    for path, unit, group in compiled:
        options, defines = unit_flags(cfg, unit, group)
        options = options + ['-D' + name for name in defines]
        lines.append(f'set_source_files_properties({_quote(path)} PROPERTIES COMPILE_OPTIONS '
                     f'{_options(options, str(path))})')
    compiled_paths = {str(path.relative_to(build_dir) if path.is_relative_to(build_dir) else path.relative_to(ROOT)):
                      path for path, _, _ in compiled}
    preprocess = sorted(preprocess_set(cfg))
    for rel in preprocess:
        if rel not in compiled_paths:
            raise ValueError(f'config/units.json cmake_preprocess: {rel} is not compiled by '
                             f'{build_dir_name(cfg)}')
    if preprocess:
        lines += ['', '# CMake preprocesses these itself, so its module scan sees guarded use/module lines with the',
                  '# -D options (config/units.json cmake_preprocess).']
        lines += [f'set_property(SOURCE {_quote(compiled_paths[rel])} PROPERTY Fortran_PREPROCESS ON)'
                  for rel in preprocess]
    lines += ['', 'add_executable(lean_mpas']
    lines += [f'  $<TARGET_OBJECTS:{group}>' for group in groups]
    lines.append(')')
    lines += ["# After the objects, GNU ld with --as-needed (Ubuntu's default) keeps the library.",
              'target_link_libraries(lean_mpas PRIVATE ' + ' '.join(_quote(arg) for arg in netcdf.link_args(nc)) + ')']
    lines.append(f'set_target_properties(lean_mpas PROPERTIES LINKER_LANGUAGE Fortran LINK_OPTIONS '
                 f'{_options(link_flags(cfg), "link")})')
    return '\n'.join(lines) + '\n'


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog='python3 -m tools.lean.config', description=__doc__.split('\n')[0])
    commands = parser.add_subparsers(dest='command', required=True)
    cmake = commands.add_parser('cmake', help='write the CMake file of a build')
    cmake.add_argument('--config-file', type=Path, help='the whole BuildConfig as JSON')
    cmake.add_argument('--target', choices=TARGETS)
    cmake.add_argument('--fc', choices=COMPILERS)
    cmake.add_argument('--profile', choices=PROFILES)
    cmake.add_argument('--omp', action='store_true')
    cmake.add_argument('--variant')
    cmake.add_argument('--out', type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        if args.config_file:
            cfg = BuildConfig(**json.loads(args.config_file.read_text(encoding='utf-8')))
        else:
            cfg = BuildConfig(args.target, args.fc, args.profile, omp=args.omp, variant=args.variant)
        out = args.out.resolve()
        out.parent.mkdir(parents=True, exist_ok=True)
        text = cmake_file(cfg, out.parent)
        if not out.is_file() or out.read_text(encoding='utf-8') != text:
            out.write_text(text, encoding='utf-8')
    except (OSError, TypeError, ValueError) as error:
        print(f'config: {error}', file=sys.stderr)
        return 2
    return 0


if __name__ == '__main__':
    sys.exit(main())
