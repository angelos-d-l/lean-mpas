"""Find a shared or static netCDF-C with CDF-5, from NETCDF or nc-config on PATH."""
from __future__ import annotations

import os
import re
import shlex
import subprocess
import sys
import tempfile
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Mapping, Optional

LIBRARY = 'libnetcdf.dylib' if sys.platform == 'darwin' else 'libnetcdf.so'


@dataclass(frozen=True)
class NetCDF:
    nc_config: str
    source: str
    version: str
    prefix: str
    includedir: str
    libdir: str
    library: str
    cdf5: bool
    nc4: bool
    skipped: tuple[str, ...] = ()
    kind: str = 'shared'
    capabilities_from: str = 'nc-config'
    deps: tuple[str, ...] = ()


class NetCDFError(ValueError):
    def __init__(self, code: str, detail: str):
        super().__init__(f'{code} {detail}')
        self.code, self.detail = code, detail


def _answer(nc_config: str, *options: str) -> Optional[str]:
    env = dict(os.environ)
    try:
        done = subprocess.run([nc_config, *options], capture_output=True, text=True, env=env, timeout=60)
    except (OSError, subprocess.SubprocessError):
        return None
    lines = done.stdout.strip().splitlines()
    return lines[0].strip() if done.returncode == 0 and lines else None


def _library(libdir: Path) -> Optional[tuple[str, str]]:
    for kind, name in (('shared', LIBRARY), ('static', 'libnetcdf.a')):
        path = libdir / name
        if path.is_file():
            return kind, str(path)
    return None


def probe(nc_config: str, source: str) -> Optional[NetCDF]:
    """The six nc-config answers, or None when its version or directories are missing."""
    version, prefix, includedir, libdir, cdf5, nc4 = (
        _answer(nc_config, option) for option in
        ('--version', '--prefix', '--includedir', '--libdir', '--has-cdf5', '--has-nc4'))
    if not all((version, prefix, includedir, libdir)):
        return None
    kind, library = _library(Path(libdir)) or ('shared', str(Path(libdir) / LIBRARY))
    return NetCDF(nc_config, source, version.removeprefix('netCDF ').strip(), prefix, includedir, libdir,
                  library, cdf5 == 'yes', nc4 == 'yes', kind=kind)


def _prefix(prefix: str) -> NetCDF:
    includedir = Path(prefix) / 'include'
    header = includedir / 'netcdf_meta.h'
    missing = [str(includedir / name) for name in ('netcdf.h', 'netcdf_meta.h')
               if not (includedir / name).is_file()]
    if missing:
        raise NetCDFError('E_NO_NETCDF', f'NETCDF={prefix}: {prefix}/bin/nc-config missing or not executable; '
                          'missing ' + ', '.join(missing))
    try:
        text = header.read_text(encoding='utf-8')
    except (OSError, UnicodeError) as error:
        raise NetCDFError('E_NO_NETCDF', f'{header}: {error}') from None
    values = {name: int(value) for name, value in re.findall(r'^\s*#\s*define\s+(NC_\w+)\s+(\d+)\b', text,
                                                           re.MULTILINE)}
    required = ('NC_VERSION_MAJOR', 'NC_VERSION_MINOR', 'NC_VERSION_PATCH', 'NC_HAS_CDF5')
    missing = [name for name in required if name not in values]
    if missing:
        raise NetCDFError('E_NO_NETCDF', f'{header}: missing ' + ', '.join(missing))
    # MPAS Makefile:758-763 prefers a lib64 containing netCDF over lib.
    libdir = Path(prefix) / ('lib64' if _library(Path(prefix) / 'lib64') else 'lib')
    library = _library(libdir)
    if library is None:
        raise NetCDFError('E_NO_NETCDF', f'NETCDF={prefix}: no {LIBRARY} or libnetcdf.a in {prefix}/lib64 or {libdir}')
    kind, path = library
    version = '.'.join(str(values[name]) for name in required[:3])
    return NetCDF('', 'NETCDF', version, prefix, str(includedir), str(libdir), path,
                  values['NC_HAS_CDF5'] == 1, values.get('NC_HAS_NC4', values.get('NC_HAS_HDF5', 0)) == 1,
                  kind=kind, capabilities_from='netcdf_meta.h')


def _dependencies(nc: NetCDF, env: Mapping[str, str]) -> tuple[str, ...]:
    source = 'NETCDF_LIBS'
    answer = env.get(source)
    if answer is None:
        source = 'nc-config --libs --static'
        answer = _answer(nc.nc_config, '--libs', '--static') if nc.nc_config else None
    if answer is None:
        source = str(Path(nc.libdir) / 'libnetcdf.settings')
        try:
            match = re.search(r'^\s*Extra libraries:[ \t]*(.*)$', Path(source).read_text(encoding='utf-8'),
                              re.MULTILINE)
            answer = match.group(1) if match else None
        except (OSError, UnicodeError):
            pass
    if answer is None:
        raise NetCDFError('E_NETCDF_STATIC', f'no static dependency list: set NETCDF_LIBS, or supply '
                          f'nc-config --libs --static or Extra libraries: in {source}')
    try:
        words = shlex.split(answer)
    except ValueError as error:
        raise NetCDFError('E_NETCDF_STATIC', f'{source}: {error}') from None
    if source == 'nc-config --libs --static':
        words = [word for word in words if word not in ('-L' + nc.libdir, '-lnetcdf')]
    words = ['-l' + word.rsplit('::', 1)[1].lower().removeprefix('lib')
             if word.startswith('-l') and '::' in word else word for word in words]
    words = list(dict.fromkeys(words))
    if '-lhdf5_hl' in words and '-lhdf5' in words and words.index('-lhdf5_hl') > words.index('-lhdf5'):
        # A static library's user must precede the library that provides its symbols.
        words.remove('-lhdf5_hl')
        words.insert(words.index('-lhdf5'), '-lhdf5_hl')
    return tuple(words)


def _usable(nc: NetCDF, env: Mapping[str, str]) -> NetCDF:
    if not Path(nc.library).is_file():
        raise NetCDFError('E_NO_NETCDF', f'no {LIBRARY} or libnetcdf.a in {nc.libdir}')
    if not nc.cdf5:
        raise NetCDFError('E_NETCDF_CDF5', f'netCDF {nc.version}, no CDF-5')
    return replace(nc, deps=_dependencies(nc, env)) if nc.kind == 'static' else nc


def find(environ: Optional[Mapping[str, str]] = None) -> NetCDF:
    """An explicit NETCDF prefix wins; otherwise take the first usable nc-config on PATH."""
    env = os.environ if environ is None else environ
    prefix = os.path.abspath(env['NETCDF']) if env.get('NETCDF') else None
    folders = [os.path.abspath(folder) for folder in env.get('PATH', os.defpath).split(os.pathsep)]
    if prefix:
        # MPAS Makefile:758-763 takes an explicit NETCDF prefix; do not pass over the user's choice.
        path = str(Path(prefix) / 'bin' / 'nc-config')
        if os.path.isfile(path) and os.access(path, os.X_OK):
            nc = probe(path, 'NETCDF')
            if nc is None:
                raise NetCDFError('E_NO_NETCDF', f'NETCDF={prefix}: {path} gave no version or directories')
        else:
            nc = _prefix(prefix)
        try:
            return _usable(nc, env)
        except NetCDFError as error:
            where = nc.nc_config or str(Path(nc.includedir) / 'netcdf_meta.h')
            raise NetCDFError(error.code, f'NETCDF={prefix}: {where} ({error.detail})') from None

    skipped, seen, codes = [], set(), []
    for folder in folders:
        path = str(Path(folder) / 'nc-config')
        real = os.path.realpath(path)
        if real in seen or not os.path.isfile(path) or not os.access(path, os.X_OK):
            continue
        seen.add(real)
        nc = probe(path, 'PATH')
        if nc is None:
            skipped.append(f'{path} (nc-config gave no version or directories)')
            codes.append('E_NO_NETCDF')
        else:
            try:
                return replace(_usable(nc, env), skipped=tuple(skipped))
            except NetCDFError as error:
                skipped.append(f'{path} ({error.detail})')
                codes.append(error.code)
    code = 'E_NETCDF_CDF5' if codes and all(code == 'E_NETCDF_CDF5' for code in codes) else 'E_NO_NETCDF'
    if 'E_NETCDF_STATIC' in codes:
        code = 'E_NETCDF_STATIC'
    raise NetCDFError(code, '; '.join(skipped) if skipped else 'no executable nc-config on PATH')


def default_libdir(path: str) -> bool:
    """Default linker directories need neither -L nor an rpath."""
    path = os.path.normpath(path)
    return path in ('/usr/lib', '/usr/lib64', '/lib', '/lib64') or bool(
        re.fullmatch(r'/usr/lib/[^/]+-linux-gnu', path))


def link_args(nc: NetCDF) -> list[str]:
    # A default linker directory gets no -L or rpath: a -L or rpath to a system directory would put it ahead of
    # NVHPC's own CUDA libraries.
    if nc.kind == 'static':
        return [nc.library] + ([] if default_libdir(nc.libdir) else ['-L' + nc.libdir]) + list(nc.deps)
    if default_libdir(nc.libdir):
        return ['-lnetcdf']
    return ['-L' + nc.libdir, '-lnetcdf', '-Wl,-rpath,' + nc.libdir]


_LINK_PROGRAM = """program netcdf_link_check
  use iso_c_binding, only: c_char, c_int, c_null_char
  implicit none
  interface
    integer(c_int) function nc_create(path, mode, id) bind(C, name='nc_create')
      import c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value :: mode
      integer(c_int), intent(out) :: id
    end function
    integer(c_int) function nc_close(id) bind(C, name='nc_close')
      import c_int
      integer(c_int), value :: id
    end function
  end interface
  integer(c_int) :: status, id
  status = nc_create('check.nc' // c_null_char, 32_c_int, id)
  if (status /= 0_c_int) then
    write(*, '(a,i0)') 'nc_create returned ', status
    stop 1
  end if
  status = nc_close(id)
  if (status /= 0_c_int) then
    write(*, '(a,i0)') 'nc_close returned ', status
    stop 1
  end if
end program
"""


def link_check(nc: NetCDF, compiler: str) -> Optional[str]:
    """A static link must resolve its dependencies and actually create CDF-5."""
    if nc.kind != 'static':
        return None
    env = dict(os.environ)
    try:
        build = Path(__file__).resolve().parents[2] / 'build'
        build.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(prefix='netcdf-link-', dir=build) as tmp:
            folder = Path(tmp)
            source, program = folder / 'check.f90', folder / 'check'
            source.write_text(_LINK_PROGRAM, encoding='utf-8')
            for command in ([compiler, str(source), *link_args(nc), '-o', str(program)], [str(program)]):
                done = subprocess.run(command, cwd=folder, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                      text=True, env=env, timeout=60)
                if done.returncode:
                    lines = done.stdout.strip().splitlines()
                    return lines[-1] if lines else f'{command[0]} exited with status {done.returncode}'
            with (folder / 'check.nc').open('rb') as stream:
                if stream.read(4) != b'CDF\x05':
                    return 'check.nc: expected CDF-5 magic CDF\\x05'
    except (OSError, subprocess.SubprocessError) as error:
        return str(error).splitlines()[-1]
    return None
