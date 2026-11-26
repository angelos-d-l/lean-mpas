"""./lean tables: place the four Thompson tables (*.DBL) in data/tables/ and check them against lean.lock.json.

    install(source) -> 0 (TABLES OK) | 1 (TABLES FAIL)   source: a folder, or a URL of a folder (file://, https://)
    rebuild()       -> 0 | 1                             regenerate in build/lean-tables/, check, then install
    verified()      -> the tables of data/tables/ whose size and sha256 match the lock
    status()        -> {name: 'ok' | 'missing' | 'bad'} for every table of the lock

Each table is copied to <name>.part next to its place, checked, and renamed only when its sha256 matches: an
interrupted or corrupt copy never takes the name of a table, and is deleted (E_TABLES_SHA). A table already in
place with the right sha256 is not copied again.

rebuild() regenerates the tables with the original utility, MPAS's atmphys_build_tables_thompson.F from
reference/mpas_src, compiled with gfortran and the flags of tools/build_reference.sh O2, as tools/build_tables.sh
does. It works only in build/lean-tables/, which it empties first, so every call really regenerates. It never
writes or deletes build/O2/ or build/tables/, which belong to tools/build_tables.sh. Every generated table must match
lean.lock.json (size and sha256: E_TABLES_SHA, or E_TABLES_SOURCE when it is missing) before any of them is
installed, whatever data/tables/ already holds.
"""
from __future__ import annotations

import hashlib
import shutil
import subprocess
import sys
import urllib.parse
import urllib.request
from pathlib import Path

from tools.lean import config

ROOT = config.ROOT
TABLES_DIR = 'data/tables'
WORK_DIR = 'build/lean-tables'     # rebuild()'s own directory, emptied on every call
REF_SRC = 'reference/mpas_src/src'
FC = 'gfortran'
# tools/build_reference.sh O2: the flags tools/build_tables.sh compiles the generator with.
FLAGS = ['-cpp', '-Dmpas', '-DSINGLE_PRECISION', '-ffree-form', '-ffree-line-length-none', '-O2']
GENERATOR = 'build_tables'


def expected() -> dict:
    """{name: {'sha256': ..., 'size': ...}} of the Thompson tables in lean.lock.json."""
    return config.load_lock()['tables']


def tables_dir() -> Path:
    return ROOT / TABLES_DIR


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, 'rb') as data:
        for block in iter(lambda: data.read(1 << 20), b''):
            digest.update(block)
    return digest.hexdigest()


def _good(path: Path, pin: dict) -> bool:
    return path.is_file() and path.stat().st_size == pin['size'] and sha256(path) == pin['sha256']


def status() -> dict:
    """{name: 'ok' | 'missing' | 'bad'} for each table of the lock, as it is in data/tables/."""
    found = {}
    for name, pin in sorted(expected().items()):
        path = tables_dir() / name
        found[name] = 'missing' if not path.exists() else 'ok' if _good(path, pin) else 'bad'
    return found


def verified() -> list[Path]:
    """The tables of data/tables/ that match the lock, sorted by name; a missing or bad one is left out."""
    return [tables_dir() / name for name, state in status().items() if state == 'ok']


def _is_url(source: str) -> bool:
    return urllib.parse.urlparse(source).scheme in ('file', 'http', 'https', 'ftp')


def _copy(source: str, name: str, target: Path) -> None:
    """Copy table name from the folder or URL source to target (OSError on any failure)."""
    if _is_url(source):
        url = source.rstrip('/') + '/' + urllib.parse.quote(name)
        try:
            with urllib.request.urlopen(url, timeout=60) as answer, open(target, 'wb') as out:
                shutil.copyfileobj(answer, out, 1 << 20)
        except ValueError as error:   # a malformed URL
            raise OSError(str(error)) from error
    else:
        shutil.copyfile(Path(source) / name, target)


def install(source: str) -> int:
    """Copy every table of the lock from source into data/tables/ and check it (see the module docstring)."""
    dest = tables_dir()
    dest.mkdir(parents=True, exist_ok=True)
    failures = []
    for name, pin in sorted(expected().items()):
        target = dest / name
        if _good(target, pin):
            print(f'ok    {name} (already in {TABLES_DIR}/)')
            continue
        part = dest / (name + '.part')
        try:
            _copy(source, name, part)
        except OSError as error:
            part.unlink(missing_ok=True)
            failures.append(f'E_TABLES_SOURCE {name}: {error}')
            print(f'E_TABLES_SOURCE: {name} cannot be copied from {source}: {error}', file=sys.stderr)
            continue
        if _good(part, pin):
            part.replace(target)
            print(f'ok    {name} sha256 {pin["sha256"]}')
        else:
            part.unlink()
            target.unlink(missing_ok=True)   # a bad table already in place must not stay either
            failures.append(f'E_TABLES_SHA {name}')
            print(f'E_TABLES_SHA: {name} from {source} does not match lean.lock.json; the copy is removed',
                  file=sys.stderr)
    print(f'TABLES FAIL: {failures[0]}' if failures else 'TABLES OK')
    return 1 if failures else 0


def _prepare(work: Path) -> None:
    """Write the two small sources tools/build_reference.sh writes: services.f90 (tests/support.f90 up to its
    `module mp_radar`, which needs module_mp_radar first) and radar_alias.f90."""
    lines = (ROOT / 'tests' / 'support.f90').read_text(encoding='utf-8').splitlines(keepends=True)
    cut = next((i for i, line in enumerate(lines) if line.startswith('module mp_radar')), len(lines))
    (work / 'services.f90').write_text(''.join(lines[:cut]), encoding='utf-8')
    (work / 'radar_alias.f90').write_text('module mp_radar\n use module_mp_radar\nend module\n', encoding='utf-8')


def _steps(work: Path) -> list[list[str]]:
    """The commands, run in work, of tools/build_reference.sh O2 and tools/build_tables.sh; the last one runs the
    generator, which writes the four tables into work."""
    ref = ROOT / REF_SRC
    physics = ref / 'core_atmosphere' / 'physics'
    objects = ['mpas_kind_types.o', 'services.o', 'mpas_atmphys_functions.o', 'module_mp_radar.o', 'radar_alias.o',
               'module_mp_thompson.o', 'atmphys_build_tables_thompson.o']
    return [
        [FC, *FLAGS, '-c', str(ref / 'framework' / 'mpas_kind_types.F'), 'services.f90',
         str(physics / 'mpas_atmphys_functions.F'), str(physics / 'physics_wrf' / 'module_mp_radar.F')],
        [FC, *FLAGS, '-c', 'radar_alias.f90', str(physics / 'physics_wrf' / 'module_mp_thompson.F')],
        [FC, *FLAGS, '-c', str(ref / 'core_atmosphere' / 'utils' / 'atmphys_build_tables_thompson.F')],
        [FC, *FLAGS, '-o', GENERATOR, *objects, str(ROOT / 'tests' / 'build_tables.f90')],
        ['./' + GENERATOR],
    ]


def _empty(work: Path) -> None:
    if work.is_symlink() or work.is_file():
        work.unlink()
    elif work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)


def check_generated(work: Path) -> list[str]:
    """The failures of the tables in work against the lock (E_TABLES_SOURCE: missing; E_TABLES_SHA: other bytes)."""
    failures = []
    for name, pin in sorted(expected().items()):
        path = work / name
        if not path.is_file():
            failures.append(f'E_TABLES_SOURCE {name}')
            print(f'E_TABLES_SOURCE: {name} was not generated in {WORK_DIR}/', file=sys.stderr)
        elif not _good(path, pin):
            failures.append(f'E_TABLES_SHA {name}')
            print(f'E_TABLES_SHA: the generated {name} does not match lean.lock.json', file=sys.stderr)
        else:
            print(f'ok    {name} generated, sha256 {pin["sha256"]}')
    return failures


def rebuild() -> int:
    """Regenerate the tables in build/lean-tables/, check each one against the lock, then install them."""
    if not (ROOT / REF_SRC).is_dir():
        print(f'TABLES FAIL: E_TABLES_BUILD {REF_SRC} is missing (run ./lean fetch-upstream first)')
        return 1
    work = ROOT / WORK_DIR
    _empty(work)
    _prepare(work)
    steps = _steps(work)
    for number, argv in enumerate(steps, 1):
        last = number == len(steps)
        try:
            if last:   # the generator's output goes to its log, as in tools/build_tables.sh
                print(f'lean tables: generating in {WORK_DIR}/ (output in {WORK_DIR}/generation.log)',
                      file=sys.stderr)
                with open(work / 'generation.log', 'wb') as log:
                    done = subprocess.run(argv, cwd=work, stdout=log, stderr=subprocess.STDOUT)
            else:
                done = subprocess.run(argv, cwd=work)
        except OSError as error:
            print(f'TABLES FAIL: E_TABLES_BUILD {argv[0]}: {error}')
            return 1
        if done.returncode:
            print(f'TABLES FAIL: E_TABLES_BUILD step {number} of {len(steps)} ({Path(argv[0]).name}) '
                  f'exited {done.returncode}')
            return 1
    failures = check_generated(work)
    if failures:
        print(f'TABLES FAIL: {failures[0]}')
        return 1
    return install(str(work))
