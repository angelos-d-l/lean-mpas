"""The pristine MPAS 8.4.2 files that the lean build compiles, vendored in third_party/mpas-8.4.2/.

    python3 -m tools.lean.vendor --write    copy the closure from reference/mpas_src/src (./lean fetch-upstream)
    python3 -m tools.lean.vendor --check    prints VENDOR OK or VENDOR FAIL: <problems>

The closure is three lists, stored in closure.json in this order:
- fixed: the 9 framework and physics files that the build compiles by name (config/units.json);
- esmf_time: every *.F90 and *.inc of src/external/esmf_time_f90; the *.F90 in a dependency order, then the *.inc.
  The order comes from a walk of the files in directory order, which differs between file systems, so it was
  computed once (APFS) and stored. A recomputation elsewhere is compared as a set, and the stored order is checked
  to be a valid dependency order;
- noahmp: the Noah-MP files of the original closure, in compile order: the modules reachable from its four driver
  modules, skipping mpas_kind_types and mpas_log.
Paths are relative to src/. The files sit at dest/src/<path>, unchanged, with the MPAS LICENSE, the terms of Noah-MP and
of the WRF physics beside their files, SHA256SUMS (`sha256sum -c` format, over LICENSE, closure.json, README.md and
src/**) and a README.md that says what they are.

--check checks the vendored tree against SHA256SUMS and closure.json, README.md against closure.json, the stored
orders against the vendored files, and, when reference/mpas_src is present, the
lists and the bytes against upstream as well. Exit status: 0 OK, 1 FAIL, 2 when --write cannot run.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / 'reference' / 'mpas_src' / 'src'
DEST = ROOT / 'third_party' / 'mpas-8.4.2'
MPAS_TAG = 'v8.4.2'
MPAS_COMMIT = 'f34984b2c89353cf31c235b08d3b4acd96b37bb3'

FIXED = [
    'framework/mpas_kind_types.F',
    'framework/mpas_constants.F',
    'core_atmosphere/physics/mpas_atmphys_constants.F',
    'core_atmosphere/physics/ccpp_kind_types.F',
    'core_atmosphere/physics/mpas_atmphys_functions.F',
    'core_atmosphere/physics/physics_wrf/module_mp_radar.F',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_vinterp.F',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_lw.F',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_sw.F',
]
FIXED_WHAT = {
    'framework/mpas_kind_types.F': 'MPAS real and string kinds',
    'framework/mpas_constants.F': 'MPAS physical constants',
    'core_atmosphere/physics/mpas_atmphys_constants.F': 'constants of the MPAS physics',
    'core_atmosphere/physics/ccpp_kind_types.F': 'CCPP real kinds (kind_phys)',
    'core_atmosphere/physics/mpas_atmphys_functions.F': 'gamma functions and saturation mixing ratios',
    'core_atmosphere/physics/physics_wrf/module_mp_radar.F': 'radar reflectivity support of Thompson microphysics',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_vinterp.F': 'ozone interpolation to model levels for RRTMG',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_lw.F': 'RRTMG longwave radiation',
    'core_atmosphere/physics/physics_wrf/module_ra_rrtmg_sw.F': 'RRTMG shortwave radiation',
}
GROUPS = ('fixed', 'esmf_time', 'noahmp')
ESMF_DIR = 'external/esmf_time_f90'
NOAHMP_DIR = 'core_atmosphere/physics/physics_noahmp'
NOAHMP_ROOTS = ('noahmpdrivermainmod', 'noahmpreadtablemod', 'noahmpinitmainmod', 'noahmpiovarinitmod')
NOAHMP_SKIP = ('mpas_kind_types', 'mpas_log')
OWN_FILES = ('LICENSE', 'README.md', 'SHA256SUMS', 'closure.json')
# The terms of the packages whose files are copied, kept beside those files as their own terms ask.
PACKAGE_TERMS = ('core_atmosphere/physics/physics_noahmp/LICENSE.txt', 'core_atmosphere/physics/physics_wrf/LICENSE')
HASHED_OWN_FILES = ('LICENSE', 'README.md', 'closure.json')  # SHA256SUMS covers these and src/**

MODULE = re.compile(r'^\s*module\s+(\w+)\s*$', re.M | re.I)
USE = re.compile(r'^\s*use\s+(\w+)', re.M | re.I)


class VendorError(Exception):
    pass


def _rel(path: Path, src_root: Path) -> str:
    return path.relative_to(src_root).as_posix()


def esmf_closure(src_root: Path) -> list[str]:
    """Every file of src/external/esmf_time_f90: the *.F90 in dependency order, then the *.inc.

    The files are visited in directory order (an unsorted glob), so the *.F90 order depends on the file system. On
    APFS it gives the order that closure.json stores.
    """
    folder = src_root / ESMF_DIR
    files = list(folder.glob('*.F90'))
    if not files:
        raise VendorError(f'no *.F90 in {folder}')
    modules = {}
    for path in files:
        for name in MODULE.findall(path.read_text()):
            modules[name.lower()] = path
    order: list[Path] = []
    active: set[Path] = set()

    def visit(path: Path) -> None:
        if path in order:
            return
        if path in active:
            raise VendorError(f'circular use through {path}')
        active.add(path)
        for name in USE.findall(path.read_text()):
            if name.lower() in modules:
                visit(modules[name.lower()])
        active.remove(path)
        order.append(path)

    for path in files:
        visit(path)
    return [_rel(p, src_root) for p in order + sorted(folder.glob('*.inc'))]


def noahmp_closure(src_root: Path) -> list[str]:
    """The Noah-MP files of the original closure, in compile order."""
    modules: dict[str, Path] = {}
    uses: dict[str, list[str]] = {}
    for path in sorted((src_root / NOAHMP_DIR).rglob('*.F90')):
        text = path.read_text()
        for name in MODULE.findall(text):
            key = name.lower()
            if key in modules:
                raise VendorError(f'module {key} is defined twice: {modules[key]} and {path}')
            modules[key] = path
            uses[key] = [n.lower() for n in USE.findall(text)]
    order: list[Path] = []
    visiting: set[str] = set()

    def visit(name: str) -> None:
        if name in NOAHMP_SKIP:
            return
        if name not in modules:
            raise VendorError(f'module {name} is not in {src_root / NOAHMP_DIR}')
        path = modules[name]
        if path in order:
            return
        if name in visiting:
            raise VendorError(f'circular use through {name}')
        visiting.add(name)
        for dependency in uses[name]:
            visit(dependency)
        visiting.remove(name)
        order.append(path)

    for name in NOAHMP_ROOTS:
        visit(name)
    return [_rel(p, src_root) for p in order]


def order_problems(group: str, paths: list[str], src_root: Path) -> list[str]:
    """The places where a file of paths uses a module that a later file of paths defines (a bad compile order)."""
    defined: dict[str, int] = {}
    texts = []
    for i, rel in enumerate(paths):
        path = src_root / rel
        text = path.read_text() if path.suffix.lower() in ('.f90', '.f') and path.is_file() else ''
        texts.append(text)
        for name in MODULE.findall(text):
            defined.setdefault(name.lower(), i)
    problems = []
    for i, rel in enumerate(paths):
        late = sorted({n.lower() for n in USE.findall(texts[i]) if defined.get(n.lower(), -1) > i})
        problems += [f'closure.json {group}: {rel} uses {name} before it is compiled' for name in late]
    return problems


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _readme(closure: dict) -> str:
    lines = [
        f'# MPAS-Model {MPAS_TAG[1:]}: the original files the lean build compiles',
        '',
        f'Every file under `src/` is copied unchanged from MPAS-Model {MPAS_TAG}',
        f'(https://github.com/MPAS-Dev/MPAS-Model, commit `{MPAS_COMMIT}`), at its original path under `src/`.',
        'The lean build compiles these files as-is. `LICENSE` is the MPAS license. The packages keep their own terms:',
        'Noah-MP (`src/core_atmosphere/physics/physics_noahmp/LICENSE.txt`), the WRF physics',
        '(`src/core_atmosphere/physics/physics_wrf/LICENSE`), RRTMG and the Mersenne Twister (notices inside',
        '`module_ra_rrtmg_lw.F` and `module_ra_rrtmg_sw.F`) and the ESMF time manager (file headers). `NOTICE.md` at the',
        'root of the repository has the whole list.',
        '',
        '- `closure.json`: the three lists below, in compile order.',
        '- `SHA256SUMS`: the sha256 of `LICENSE`, `closure.json`, this `README.md` and every file under `src/`',
        '  (`shasum -a 256 -c SHA256SUMS`).',
        '- `python3 -m tools.lean.vendor --check` checks this folder; `--write` makes it again from',
        '  `reference/mpas_src` (`./lean fetch-upstream`).',
        '',
        'None of these files is one of the patched copies in `reference_fixes/`',
        "(used only by the tests that compile MPAS's own code; docs/fixes.md).",
        '',
        f'## fixed ({len(closure["fixed"])} files): compiled by name by the lean build',
        '',
    ]
    lines += [f'- `src/{p}`: {FIXED_WHAT.get(p, "not one of the fixed files")}' for p in closure['fixed']]
    lines += [
        '',
        f'## esmf_time ({len(closure["esmf_time"])} files): the ESMF time manager of MPAS, the model clock',
        '',
        'Every `*.F90` and `*.inc` of `src/external/esmf_time_f90`, compiled with `-DHIDE_MPI`.',
        '',
    ]
    lines += [f'- `src/{p}`' for p in closure['esmf_time']]
    lines += [
        '',
        f'## noahmp ({len(closure["noahmp"])} files): the Noah-MP land surface model',
        '',
        'The modules that the column driver, the table reader and the cold-start initializer use',
        f'(`{"`, `".join(NOAHMP_ROOTS)}`), without `ErrorHandleMod` and its netCDF I/O.',
        '',
    ]
    lines += [f'- `src/{p}`' for p in closure['noahmp']]
    return '\n'.join(lines) + '\n'


def vendor(src_root: Path, dest: Path) -> dict:
    """Copy the closure of src_root to dest/src and write LICENSE, closure.json, SHA256SUMS and README.md."""
    closure = {'fixed': list(FIXED), 'esmf_time': esmf_closure(src_root), 'noahmp': noahmp_closure(src_root)}
    paths = [p for group in GROUPS for p in closure[group]]
    if len(set(paths)) != len(paths):
        raise VendorError('a file is in two groups of the closure')
    licence = src_root.parent / 'LICENSE'
    if not licence.is_file():
        raise VendorError(f'{licence} is missing')
    dest.mkdir(parents=True, exist_ok=True)
    if (dest / 'src').exists():
        shutil.rmtree(dest / 'src')
    for rel in paths + list(PACKAGE_TERMS):
        (dest / 'src' / rel).parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src_root / rel, dest / 'src' / rel)
    shutil.copyfile(licence, dest / 'LICENSE')
    (dest / 'closure.json').write_text(json.dumps(closure, indent=2) + '\n')
    (dest / 'README.md').write_text(_readme(closure))
    names = sorted(HASHED_OWN_FILES + tuple('src/' + p for p in paths + list(PACKAGE_TERMS)))
    (dest / 'SHA256SUMS').write_text(''.join(f'{_sha256(dest / n)}  {n}\n' for n in names))
    return closure


def check(dest: Path, src_root: Path | None = None) -> list[str]:
    """The problems of the vendored tree at dest; with src_root, also against the upstream sources there."""
    try:
        closure = json.loads((dest / 'closure.json').read_text())
    except (OSError, ValueError) as error:
        return [f'closure.json cannot be read: {error}']
    if not isinstance(closure, dict) or list(closure) != list(GROUPS) or not all(
            isinstance(closure[g], list) and all(isinstance(p, str) for p in closure[g]) for g in GROUPS):
        return [f'closure.json must hold the lists {", ".join(GROUPS)}, in that order']
    problems = []
    if closure['fixed'] != FIXED:
        problems.append('closure.json fixed is not the 9 fixed files')
    paths = [p for g in GROUPS for p in closure[g]]
    if len(set(paths)) != len(paths):
        problems.append('closure.json lists a file twice')
    expected = set(HASHED_OWN_FILES) | {'src/' + p for p in paths + list(PACKAGE_TERMS)}

    sums = {}
    try:
        for number, line in enumerate((dest / 'SHA256SUMS').read_text().splitlines(), 1):
            match = re.fullmatch(r'([0-9a-f]{64})  (\S.*)', line)
            if not match:
                problems.append(f'SHA256SUMS line {number} is not "<sha256>  <path>"')
                continue
            sums[match.group(2)] = match.group(1)
    except OSError as error:
        problems.append(f'SHA256SUMS cannot be read: {error}')
    for name in sorted(expected):
        path = dest / name
        if not path.is_file():
            problems.append(f'{name} is missing')
        elif name not in sums:
            problems.append(f'{name} is not in SHA256SUMS')
        elif _sha256(path) != sums[name]:
            problems.append(f'{name} is changed (its sha256 is not the one in SHA256SUMS)')
    problems += [f'{name} is in SHA256SUMS but not in closure.json' for name in sorted(set(sums) - expected)]
    present = {_rel(p, dest) for p in (dest / 'src').rglob('*') if p.is_file()} if (dest / 'src').is_dir() else set()
    problems += [f'{name} is not in closure.json' for name in sorted(present - expected)]
    problems += [f'{p.name} does not belong in {dest}' for p in sorted(dest.iterdir())
                 if p.name not in OWN_FILES + ('src',)]
    if (dest / 'README.md').is_file() and (dest / 'README.md').read_text() != _readme(closure):
        problems.append('README.md is not the README of closure.json')
    for group in ('esmf_time', 'noahmp'):
        problems += order_problems(group, closure[group], dest / 'src')

    if src_root is not None:
        try:
            upstream = {'fixed': list(FIXED), 'esmf_time': esmf_closure(src_root), 'noahmp': noahmp_closure(src_root)}
        except (OSError, VendorError) as error:
            return problems + [f'the closure cannot be computed from {src_root}: {error}']
        # esmf_time as a set: its stored order was computed on APFS, and the walk's order depends on the file system
        problems += [f'closure.json {g} is not the closure computed from {src_root}'
                     for g in GROUPS if (sorted(closure[g]) != sorted(upstream[g]) if g == 'esmf_time'
                                         else closure[g] != upstream[g])]
        originals = ([('LICENSE', src_root.parent / 'LICENSE')]
                     + [('src/' + p, src_root / p) for p in paths + list(PACKAGE_TERMS)])
        for name, original in originals:
            if not original.is_file():
                problems.append(f'{name}: {original} is missing upstream')
            elif (dest / name).is_file() and (dest / name).read_bytes() != original.read_bytes():
                problems.append(f'{name} differs from {original}')
    return problems


def _upstream_commit(src_root: Path) -> str:
    result = subprocess.run(['git', '-C', str(src_root.parent), 'rev-parse', 'HEAD'],
                            capture_output=True, text=True)
    return result.stdout.strip() if result.returncode == 0 else ''


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog='python3 -m tools.lean.vendor',
                                     description='Vendor or check the pristine MPAS files of the lean build.')
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--write', action='store_true', help=f'copy the closure from {SRC.relative_to(ROOT)}')
    mode.add_argument('--check', action='store_true', help=f'check {DEST.relative_to(ROOT)}')
    args = parser.parse_args(argv)
    if args.write:
        commit = _upstream_commit(SRC) if SRC.is_dir() else ''
        if commit != MPAS_COMMIT:
            print(f'vendor: {SRC.relative_to(ROOT)} must be MPAS-Model {MPAS_TAG} {MPAS_COMMIT} '
                  f'(found {commit or "nothing"}); run ./lean fetch-upstream', file=sys.stderr)
            return 2
        try:
            closure = vendor(SRC, DEST)
        except (OSError, VendorError) as error:
            print(f'vendor: {error}', file=sys.stderr)
            return 2
        print(f'wrote {DEST.relative_to(ROOT)}: ' + ', '.join(f'{g} {len(closure[g])}' for g in GROUPS))
    src_root = SRC if SRC.is_dir() else None
    if src_root is None:
        print(f'vendor: {SRC.relative_to(ROOT)} is absent; checked the hashes only', file=sys.stderr)
    problems = check(DEST, src_root)
    print('VENDOR OK' if not problems else 'VENDOR FAIL: ' + '; '.join(problems))
    return 0 if not problems else 1


if __name__ == '__main__':
    sys.exit(main())
