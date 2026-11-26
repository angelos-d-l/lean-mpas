"""./lean build: check the tools and netCDF-C, configure with CMake, build lean_mpas with Ninja, record the build.

    build(cfg, jobs=2, dry_run=False, as_json=False, compiler=None, link=None) -> 0 (BUILD OK), 1 (BUILD FAIL)

The build directory is build/<build_dir_name(cfg)>. CMake is run on every call, so a changed config/flags.json or
config/units.json, or another compiler, always reaches the commands; the resolver writes the generated file only
when it changes, so an unchanged build is not recompiled. Next to lean_mpas the build writes:
    build.json     what was built: config, tools, commit, every source with its sha256 and flags, link, netcdf, tables
    flags.txt      the model's base compile flags and global defines, space-joined
    objects.txt    the absolute path of every object file, one per line (test programs link these)
    libs.txt       netCDF-C link_libraries, space-joined; check programs add these after objects.txt
    moddirs.txt    the module directories, one per line
    configure.out  the output of CMake;  ninja.out  the output of Ninja
    ninja2.out     the second Ninja run of a gpu build (after the card stack frame is known)
and links the Thompson tables and NoahmpTable.TBL, which lean_mpas reads from the directory it runs in.

CMake and Ninja receive NETCDF as the selected absolute prefix, so a configure from the source root finds the same
library.

A build directory that a build/NAME symlink points to is rebuilt only with --link NAME (E_PUBLISHED), so a link
that people run is never left pointing at a half-built executable; --link also requires all four Thompson tables
(tables.status(), E_NO_TABLES). Both are checked before anything changes. Then a gpu build checks that src/device/ is what the generators produce (generate.drift();
E_DEVICE_DRIFT), then every build runs the doctor's `build` requirements (doctor.checks: the compiler, CMake,
Ninja, Python; E_NO_FC, E_FC_OLD, ...). Only then does CMake run. The Thompson tables linked are those of
data/tables/ that match lean.lock.json (tables.verified()); a plain build leaves a missing or bad one out with W_NO_TABLES.
"""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import asdict
from pathlib import Path
from typing import Optional

from tools.lean import build_info, card, config, doctor, generate, netcdf, tables
from tools.lean.config import BuildConfig, build_dir_name
from tools.lean.doctor import first_line as _first_line
from tools.lean.doctor import version as _version
from tools.lean.netcdf import NetCDF

ROOT = config.ROOT
TARGET = 'lean_mpas'
NOAHMP_TABLE = 'reference/config/NoahmpTable.TBL'
# What CMake itself adds to the lines of config/flags.json: listed in build.json with the reason.
CMAKE_ADDED = {
    'module_dir': {'gfortran': '-J<dir>', 'nvfortran': '-module <dir>', 'ifx': '-module <dir>'},
    'flags': ['-I<dir>', '-c <source>', '-o <object>'],
    'why': 'the module directory of the unit\'s group (-J / -module), every group\'s module directory and the '
           'include directory of its group (-I), and the source and object file names. They place files and '
           'change no code.',
}
# What CMake adds to a source of config/units.json cmake_preprocess (Fortran_PREPROCESS ON): an -E line that writes
# <source>-pp.<ext>, with CMake's own preprocess flag in front, then the compile of that file. The flags are CMake's
# (Modules/Compiler/GNU-Fortran.cmake, PGI-Fortran.cmake, which NVHPC includes, IntelLLVM-Fortran.cmake); NVHPC has no postprocess flag.
CMAKE_PREPROCESS = {
    'gfortran': {'e_line': '<fc> -cpp <defines> <includes> <flags> -E <source> -o <source>-pp.<ext>',
                 'e_line_flags': ['-cpp', '-E'], 'compile': '<source>-pp.<ext>', 'compile_flags': ['-fpreprocessed']},
    'nvfortran': {'e_line': '<fc> -Mpreprocess <defines> <includes> <flags> -E <source> > <source>-pp.<ext>',
                  'e_line_flags': ['-Mpreprocess', '-E'], 'compile': '<source>-pp.<ext>', 'compile_flags': []},
    'ifx': {'e_line': '<fc> -fpp <defines> <includes> <flags> -P <source> -o <source>-pp.<ext>',
            'e_line_flags': ['-fpp', '-P'], 'compile': '<source>-pp.<ext>', 'compile_flags': []},
}
# What a build is made from: a tracked change or an untracked (not ignored) file here makes build.json say dirty.
# Untracked files elsewhere (scratch output) do not.
BUILD_INPUTS = ('CMakeLists.txt', 'config', 'src', 'third_party', 'tools/lean', 'reference/config', 'lean',
                'lean.lock.json')


class BuildError(Exception):
    def __init__(self, code: str, detail: str):
        super().__init__(f'{code} {detail}')
        self.code, self.detail = code, detail


def _env() -> dict:
    """The environment of a child: the tool probes' (doctor.probe_env)."""
    return doctor.probe_env()


def doctor_failures(cfg: BuildConfig, compiler: Optional[str] = None) -> list[doctor.Check]:
    """The doctor's failed `build` requirements of cfg (the card and the tables are not needed to build)."""
    return [check for check in doctor.checks(cfg.target, cfg.fc, compiler=compiler, with_tables=False)
            if not check.ok and check.required_for == 'build']


def device_drift() -> None:
    """BuildError E_DEVICE_DRIFT if src/device/ is not what the generators produce (E_GENERATE if one fails)."""
    try:
        differs = generate.drift()
    except generate.GenerateError as error:
        raise BuildError('E_GENERATE', str(error)) from error
    if differs:
        raise BuildError('E_DEVICE_DRIFT', f'{" ".join(differs)} differ from what tools/gen_device_*.py produce '
                                           f'(./lean generate --check; ./lean generate rewrites them)')


def cmake_args(cfg: BuildConfig, root: Path, compiler: str) -> list[str]:
    """The CMake configure command of cfg, from source root into root/build/<build_dir_name(cfg)>. Every cache
    value the build depends on is given, so none is left over from an earlier configure."""
    build_dir = Path(root) / 'build' / build_dir_name(cfg)
    args = ['cmake', '-G', 'Ninja', '-S', str(root), '-B', str(build_dir),
            f'-DCMAKE_Fortran_COMPILER={compiler}', f'-DPython3_EXECUTABLE={sys.executable}',
            f'-DLEAN_TARGET={cfg.target}', f'-DLEAN_FC={cfg.fc}', f'-DLEAN_PROFILE={cfg.profile}',
            f'-DLEAN_OMP={"ON" if cfg.omp else "OFF"}', f'-DLEAN_VARIANT={cfg.variant or ""}']
    # The -D values above carry target, fc, profile, omp and variant. A config with anything more (switches, the
    # card's compute capability) is passed whole as lean-config.json; a plain build's command stays as it is.
    plain = BuildConfig(cfg.target, cfg.fc, cfg.profile, omp=cfg.omp, variant=cfg.variant)
    if cfg != plain:
        args.append(f'-DLEAN_CONFIG_FILE={build_dir}/lean-config.json')
    return args


def _cached_compiler(build_dir: Path) -> Optional[str]:
    """The compiler in the build directory's CMake cache, or None if there is no cache."""
    cache = build_dir / 'CMakeCache.txt'
    if not cache.is_file():
        return None
    for line in cache.read_text(encoding='utf-8', errors='replace').splitlines():
        if line.startswith('CMAKE_Fortran_COMPILER:') and '=' in line:
            return line.split('=', 1)[1]
    return None


def _configure_command(cfg: BuildConfig, build_dir: Path, compiler: str) -> list[str]:
    """cmake_args, with --fresh when the cache holds another compiler: CMake would then delete its cache itself
    and configure again without the -D values ("you may have to reset some variables"), and fail."""
    command = cmake_args(cfg, ROOT, compiler)
    cached = _cached_compiler(build_dir)
    if cached is not None and cached != compiler:
        command.insert(1, '--fresh')
    return command


def _run_logged(command: list[str], log: Path, env: dict, code: str) -> None:
    """Run command with its output in log; a failure is BuildError(code) with the end of the log on stderr."""
    try:
        with open(log, 'w', encoding='utf-8') as out:
            status = subprocess.run(command, stdout=out, stderr=subprocess.STDOUT, env=env).returncode
    except OSError as error:
        raise BuildError(code, f'{command[0]}: {error}') from error
    if status:
        tail = log.read_text(encoding='utf-8', errors='replace').splitlines()[-20:]
        print('\n'.join(tail), file=sys.stderr)
        raise BuildError(code, f'{command[0]} exited {status} (see {_rel(log)})')


def _ninja_lines(build_dir: Path, *args: str) -> list[str]:
    done = subprocess.run(['ninja', '-C', str(build_dir), *args], capture_output=True, text=True, env=_env())
    if done.returncode:
        raise BuildError('E_NINJA_QUERY', f'ninja {" ".join(args)} exited {done.returncode}: {done.stderr.strip()}')
    return done.stdout.splitlines()


def objects(build_dir: Path) -> list[Path]:
    """The object files lean_mpas links, in link order: the explicit inputs that `ninja -t query` lists."""
    lines = _ninja_lines(build_dir, '-t', 'query', TARGET)
    found, inputs = [], False
    for line in lines:
        text = line.strip()
        if text.startswith('input:') or text == 'outputs:':
            inputs = text.startswith('input:')
        elif inputs and text.endswith('.o') and not text.startswith('|'):
            found.append(build_dir / text)
    if not found:
        raise BuildError('E_NINJA_QUERY', f'ninja -t query {TARGET} lists no object files')
    return found


def _tables() -> list[Path]:
    """The tables to link: the Thompson tables of data/tables/ that match lean.lock.json, and NoahmpTable.TBL."""
    noahmp = ROOT / NOAHMP_TABLE
    if not noahmp.is_file():
        raise BuildError('E_NOAHMP_TABLE', f'{NOAHMP_TABLE} is missing')
    thompson = tables.verified()
    left_out = sorted(set(tables.expected()) - {table.name for table in thompson})
    if left_out:
        print(f'W_NO_TABLES: {", ".join(left_out)} missing or bad in {tables.TABLES_DIR}/: not linked into the '
              f'build directory (an MPAS run directory brings its own; {doctor.HINTS["tables"]})', file=sys.stderr)
    return thompson + [noahmp]


def _link_tables(build_dir: Path, tables: list[Path]) -> None:
    """Symlink each table into the build directory; a *.DBL link that is not in tables is removed."""
    names = {table.name for table in tables}
    for old in build_dir.glob('*.DBL'):
        if old.is_symlink() and old.name not in names:
            old.unlink()
    for table in tables:
        link = build_dir / table.name
        if link.is_symlink() or link.exists():
            link.unlink()
        link.symlink_to(table)


def link_build(build_dir: Path, name: str) -> None:
    """Point build/NAME at build_dir (an atomic symlink swap). A real directory already named NAME is kept as
    NAME.prev, so it can be restored."""
    if not name or name in ('.', '..') or Path(name).name != name or name == build_dir.name:
        raise ValueError('--link needs a single name different from the build directory')
    link = build_dir.parent / name
    with tempfile.TemporaryDirectory(dir=build_dir.parent, prefix=f'.{name}-') as tmp:
        staged = Path(tmp) / 'link'
        staged.symlink_to(build_dir.name, target_is_directory=True)
        if link.is_dir() and not link.is_symlink():
            previous = link.with_name(name + '.prev')
            if previous.is_symlink() or previous.is_file():
                previous.unlink()
            elif previous.exists():
                shutil.rmtree(previous)
            link.rename(previous)
        staged.replace(link)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, 'rb') as data:
        for block in iter(lambda: data.read(1 << 20), b''):
            digest.update(block)
    return digest.hexdigest()


def _rel(path: Path) -> str:
    """path relative to the source root when it is inside it, else absolute."""
    try:
        return str(Path(path).relative_to(ROOT))
    except ValueError:
        return str(path)


def _git(*args: str) -> Optional[str]:
    try:
        done = subprocess.run(['git', '-C', str(ROOT), *args], capture_output=True, text=True)
    except OSError:
        return None
    return done.stdout if done.returncode == 0 else None


def git_state() -> tuple[Optional[str], Optional[bool]]:
    """(commit, dirty) of the source root. Dirty means a build input (BUILD_INPUTS) has a tracked change or an
    untracked file that is not ignored. (None, None) outside a git work tree."""
    commit = _git('rev-parse', 'HEAD')
    if not commit:
        return None, None
    status = _git('status', '--porcelain', '--untracked-files=normal', '--', *BUILD_INPUTS)
    return commit.strip(), (bool(status.strip()) if status is not None else None)


def cmake_added(cfg: BuildConfig) -> dict:
    """The flags CMake adds to those of config/flags.json, with the reason: the file-placing flags of
    every compile and, for the sources in config.preprocess_set(cfg), its two-step compile."""
    added = {'module_dir': CMAKE_ADDED['module_dir'][cfg.fc], 'flags': CMAKE_ADDED['flags'],
             'why': CMAKE_ADDED['why']}
    units = config.load_units()
    sources = sorted(config.preprocess_set(cfg))
    if sources:
        added['preprocess'] = {'sources': list(sources), **CMAKE_PREPROCESS[cfg.fc],
                               'why': units['cmake_preprocess_why']}
    return added


def build_record(cfg: BuildConfig, build_dir: Path, compiler: str, tables: list[Path],
                 nc: Optional[NetCDF] = None) -> dict:
    """The content of build.json."""
    nc = netcdf.find() if nc is None else nc
    nc_record = asdict(nc)
    nc_record.update(skipped=list(nc.skipped), deps=list(nc.deps), default_libdir=netcdf.default_libdir(nc.libdir))
    commit, dirty = git_state()
    units = []
    for path, unit, group in config.compile_units(cfg, build_dir):
        options, defines = config.unit_flags(cfg, unit, group)
        rel = path.relative_to(config.ROOT) if path.is_relative_to(config.ROOT) else path
        units.append({'path': str(rel), 'sha256': _sha256(path), 'group': group, 'options': options,
                      'defines': defines})
    return {
        'config': asdict(cfg),
        'switches': dict(cfg.switches),
        'defines': config.global_defines(cfg),
        'build_dir': _rel(build_dir),
        'compiler': {'path': compiler, 'version': _first_line([compiler, '--version'])},
        'cmake': '.'.join(map(str, _version(_first_line(['cmake', '--version'])))),
        'ninja': '.'.join(map(str, _version(_first_line(['ninja', '--version'])))),
        'commit': commit,
        'dirty': dirty,
        'sources': units,
        'link': config.link_flags(cfg),
        'netcdf': nc_record,
        'link_libraries': netcdf.link_args(nc),
        'cmake_added': cmake_added(cfg),
        'tables': [{'name': table.name, 'sha256': _sha256(table)} for table in tables],
    }


def read_frame(build_dir: Path, compiler: str) -> int:
    """The largest kernel stack frame of the linked lean_mpas (cuobjdump -res-usage, next to the CUDA nvfortran uses)."""
    tool = card.find_cuobjdump(compiler)
    if tool is None:
        raise BuildError('E_NO_CUOBJDUMP', 'cuobjdump not found next to the CUDA nvfortran uses (the card build reads '
                                           "its kernels' stack frames with it)")
    done = subprocess.run([tool, '-res-usage', str(build_dir / TARGET)], capture_output=True, text=True, env=_env())
    if done.returncode:
        raise BuildError('E_CARD_FRAME', f'cuobjdump -res-usage exited {done.returncode}: {done.stderr.strip()[-200:]}')
    return card.largest_frame(done.stdout)


def link_frame(cfg: BuildConfig, build_dir: Path, relink, frame_of) -> int:
    """The card's largest kernel stack frame is known only after a link, and the program needs it (its card memory
    check reserves that stack). The frame of the first link goes into lean_build_info; when it changed, Ninja runs
    again (only the units that use lean_build_info recompile) and the frame is read again: E_CARD_FRAME if it moved.
    relink() runs Ninja; frame_of() reads the frame."""
    frame = frame_of()
    if frame == build_info.recorded_frame(build_dir / build_info.GENERATED):
        return frame
    build_info.write_build_info(cfg, build_dir, frame=frame)
    relink()
    again = frame_of()
    if again != frame:
        raise BuildError('E_CARD_FRAME', f'the largest kernel stack frame moved from {frame} to {again} bytes between '
                                         'the two links')
    return frame


def _verdict(as_json: bool, ok: bool, build_dir: Path, error: Optional[BuildError] = None) -> int:
    rel = _rel(build_dir)
    if as_json:
        out = ({'verdict': 'BUILD OK', 'exe': f'{rel}/{TARGET}', 'build_json': f'{rel}/build.json'} if ok else
               {'verdict': 'BUILD FAIL', 'code': error.code, 'detail': error.detail})
        print(json.dumps(out))
    else:
        print(f'BUILD OK {rel}/{TARGET}' if ok else f'BUILD FAIL: {error.code} {error.detail}')
    return 0 if ok else 1


def build(cfg: BuildConfig, jobs: int = 2, dry_run: bool = False, as_json: bool = False,
          compiler: Optional[str] = None, link: Optional[str] = None) -> int:
    """Check, configure and build cfg (see the module docstring). A dry run configures, then prints the commands
    Ninja would run (`ninja -t commands lean_mpas`; a JSON list with as_json) and builds nothing."""
    build_dir = ROOT / 'build' / build_dir_name(cfg)
    try:
        if link is not None:
            missing = [name for name, state in tables.status().items() if state != 'ok']
            if missing:
                raise BuildError('E_NO_TABLES', f'{", ".join(missing)} missing or bad in {tables.TABLES_DIR}/; '
                                               'run ./lean tables --from PATH|URL, or ./lean tables --build')
        if not dry_run:
            for alias in sorted(build_dir.parent.glob('*')):
                if alias.is_symlink() and alias.resolve() == build_dir.resolve() and alias.name != link:
                    raise BuildError('E_PUBLISHED', f'build/{alias.name} is published: it links to this build '
                                                   f'directory; rebuild it with --link {alias.name}, or remove '
                                                   'the link first')
        if not dry_run and build_dir.is_dir():   # a failed build must not leave the record of an earlier one
            for name in ('build.json', 'objects.txt', 'moddirs.txt', 'flags.txt', 'libs.txt'):
                (build_dir / name).unlink(missing_ok=True)
        if cfg.target == 'gpu':
            device_drift()
        fails = doctor_failures(cfg, compiler)
        for check in fails[1:]:
            print(f'{check.code}: {doctor.detail(check)}', file=sys.stderr)
        if fails:
            raise BuildError(fails[0].code, doctor.detail(fails[0]))
        fc = doctor.find_compiler(cfg.fc, compiler)
        nc = netcdf.find()
        build_dir.mkdir(parents=True, exist_ok=True)
        # Write plain configs too: a CMake cache may still name this file after a switched build.
        config_file = build_dir / 'lean-config.json'
        text = json.dumps(asdict(cfg), indent=2) + '\n'
        if not config_file.is_file() or config_file.read_text(encoding='utf-8') != text:
            config_file.write_text(text, encoding='utf-8')
        print(f'lean build: netCDF-C {nc.version} in {nc.libdir}', file=sys.stderr)
        print(f'lean build: configure {_rel(build_dir)}', file=sys.stderr)
        env = {**_env(), 'NETCDF': os.path.abspath(nc.prefix)}
        _run_logged(_configure_command(cfg, build_dir, fc), build_dir / 'configure.out', env, 'E_CONFIGURE')
        if dry_run:
            commands = _ninja_lines(build_dir, '-t', 'commands', TARGET)
            print(json.dumps(commands, indent=1) if as_json else '\n'.join(commands))
            return 0
        print(f'lean build: ninja -j {jobs} {TARGET} (output in {_rel(build_dir)}/ninja.out)', file=sys.stderr)
        ninja = ['ninja', '-C', str(build_dir), '-j', str(jobs), TARGET]
        ninja_env = {**_env(), 'NETCDF': env['NETCDF']}
        _run_logged(ninja, build_dir / 'ninja.out', ninja_env, 'E_COMPILE')
        frame = 0
        if cfg.target == 'gpu':
            frame = link_frame(cfg, build_dir, lambda: _run_logged(ninja, build_dir / 'ninja2.out', ninja_env, 'E_COMPILE'),
                               lambda: read_frame(build_dir, fc))
        object_files = objects(build_dir)
        table_files = _tables()
        _link_tables(build_dir, table_files)
        moddirs = [build_dir / spec['module_dir'] for spec in config.load_units()['groups'].values()]
        (build_dir / 'objects.txt').write_text(''.join(f'{path}\n' for path in object_files), encoding='utf-8')
        (build_dir / 'moddirs.txt').write_text(''.join(f'{path}\n' for path in moddirs), encoding='utf-8')
        record = build_record(cfg, build_dir, fc, table_files, nc=nc)
        record['card_frame_bytes'] = frame
        options, defines = config.unit_flags(cfg, 'config', 'model')
        (build_dir / 'flags.txt').write_text(' '.join(options + ['-D' + name for name in defines]) + '\n',
                                           encoding='utf-8')
        (build_dir / 'libs.txt').write_text(' '.join(netcdf.link_args(nc)) + '\n', encoding='utf-8')
        (build_dir / 'build.json').write_text(json.dumps(record, indent=2) + '\n', encoding='utf-8')
        if link is not None:
            link_build(build_dir, link)
    except BuildError as error:
        return _verdict(as_json, False, build_dir, error)
    except (OSError, ValueError) as error:
        return _verdict(as_json, False, build_dir, BuildError('E_BUILD', str(error)))
    return _verdict(as_json, True, build_dir)
