"""./lean doctor: are the tools, netCDF-C with CDF-5 and the Thompson tables in place?

    checks(target, fc) -> list[Check]          one Check per requirement
    render(checks, as_json=False) -> (text, status)

The text has one line per requirement, then `DOCTOR OK` or `DOCTOR FAIL: <n> missing` (n counts the E_ codes; a
W_ code is a warning and does not fail the doctor). Every failure has a stable code and an install hint:

    E_NO_FC E_FC_OLD           the compiler (gfortran -dumpversion; nvfortran: the first line of --version)
    E_NVHPC_VERSION            nvfortran is not NVHPC 25.3, or the NVHPC bundle has no CUDA 12.8 (gpu)
    E_NO_CMAKE E_CMAKE_OLD     cmake --version          E_NO_NINJA E_NINJA_OLD   ninja --version
    E_PYTHON_OLD               the Python running ./lean            E_NO_GIT  git (fetch-upstream)
    E_NO_NETCDF E_NETCDF_CDF5  netCDF-C with CDF-5: NETCDF=<prefix>, else nc-config on PATH
    E_NETCDF_STATIC           static netCDF-C dependency libraries and a CDF-5 compile/link/run check
    E_NO_GPU E_GPU_CC          nvidia-smi --query-gpu=compute_cap,driver_version (gpu; 12.0 is cc120); W_GPU_UNTESTED
                               a card from lean.lock.json gpu.cc_min up, W_NVHPC_UNTESTED a newer NVHPC (nvhpc.min)
    E_CUDA_DRIVER E_NO_CUOBJDUMP W_CARD_WHICH   (gpu) the build's CUDA against the driver's, cuobjdump, device 0
    E_MEMORY_HOST E_MEMORY_CARD E_NO_TABLE E_TABLE_ORDER E_INIT   with --build/--init/--run-dir (tools/lean/memory.py)
    E_TABLES_SHA W_NO_TABLES   a table of data/tables/ that does not match lean.lock.json; a missing one

The pins are lean.lock.json tools_min, nvhpc and gpu. Check.required_for says what needs the requirement:
`build` (./lean build stops on these), `run` (the model run: the card and the tables), `fetch-upstream`.
"""
from __future__ import annotations

import json
import os
import platform
import re
import shutil
import subprocess
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Optional

from tools.lean import config, netcdf, tables

HINTS = {
    'gfortran': 'brew install gcc | port install gcc14 | apt install gfortran',
    'ifx': 'install the Intel oneAPI HPC toolkit and source its setvars.sh (ifx 2025 or newer)',
    'nvfortran': 'install NVHPC 25.3 from developer.nvidia.com/hpc-sdk and put its compilers/bin on PATH',
    'cuda': 'NVHPC 25.3 brings CUDA 12.8 in <nvhpc>/Linux_x86_64/2025/cuda/12.8: install the bundle that has it',
    'cmake': 'brew install cmake | port install cmake | pip install cmake | apt install cmake (Ubuntu 24.04 or newer)',
    'ninja': 'brew install ninja | port install ninja | apt install ninja-build',
    'python': 'brew install python | port install python312 | apt install python3',
    'git': 'brew install git | port install git | apt install git',
    'netcdf': 'brew install netcdf | apt install libnetcdf-dev | conda install -c conda-forge libnetcdf; '
              'or NETCDF=<prefix> of a netCDF-C built with CDF-5, as for building MPAS',
    'netcdf_static': 'NETCDF_LIBS=<the dependency libraries with their -L>',
    'gpu': 'an NVIDIA card and its driver, with nvidia-smi on PATH: apt install nvidia-driver-570',
    'tables': './lean tables --from PATH|URL, or ./lean tables --build',
}


@dataclass(frozen=True)
class Check:
    name: str
    ok: bool
    found: str
    need: str
    code: Optional[str]
    hint: Optional[str]
    required_for: str


def probe_env() -> dict:
    """The environment of a tool probe: this one's."""
    return dict(os.environ)


def first_line(command: list[str]) -> Optional[str]:
    """The first non-empty line a tool prints, or None if it cannot be run or fails."""
    try:
        done = subprocess.run(command, capture_output=True, text=True, env=probe_env(), timeout=60)
    except (OSError, subprocess.SubprocessError):
        return None
    lines = done.stdout.strip().splitlines()
    return lines[0].strip() if done.returncode == 0 and lines else None


def version(text: Optional[str]) -> tuple[int, ...]:
    """(3, 31, 2) from 'cmake version 3.31.2'; () if text holds no version."""
    found = re.search(r'\d+(?:\.\d+)*', text or '')
    return tuple(int(part) for part in found.group(0).split('.')) if found else ()


def find_compiler(fc: str, compiler: Optional[str] = None) -> Optional[str]:
    """The compiler's absolute path: compiler as given (a symlink or wrapper script is used by its own path), else the
    first fc on PATH; None if it is not an executable file."""
    if compiler:
        path = os.path.abspath(compiler)
        return path if os.path.isfile(path) and os.access(path, os.X_OK) else None
    return shutil.which(fc)


def _check(name: str, ok: bool, found: str, need: str, code: str, required_for: str = 'build',
           hint: Optional[str] = None) -> Check:
    return Check(name, ok, found, need, None if ok else code, None if ok else hint or HINTS[name], required_for)


def _compiler(fc: str, compiler: Optional[str], lock: dict) -> Check:
    path = find_compiler(fc, compiler)
    if fc == 'gfortran':
        need = f'{lock["tools_min"]["gfortran"]} or newer'
        if path is None:
            return _check(fc, False, f'{compiler} not found' if compiler else 'not found', need, 'E_NO_FC')
        found = first_line([path, '-dumpversion']) or '(no version)'
        return _check(fc, version(found) >= version(lock['tools_min']['gfortran']), found, need, 'E_FC_OLD')
    if fc == 'ifx':
        need = f'{lock["tools_min"]["ifx"]} or newer'
        if path is None:
            return _check(fc, False, f'{compiler} not found' if compiler else 'not found', need, 'E_NO_FC')
        line = first_line([path, '--version']) or ''
        match = re.search(r'ifx \(IFX\)\s+(\d+(?:\.\d+)*)', line)
        found = match.group(1) if match else (line or '(no version)')
        return _check(fc, bool(match) and version(found) >= version(lock['tools_min']['ifx']), found, need, 'E_FC_OLD')
    need = f'NVHPC {lock["nvhpc"]["version"]}'
    if path is None:
        return _check(fc, False, f'{compiler} not found' if compiler else 'not found', need, 'E_NO_FC')
    line = first_line([path, '--version']) or ''
    match = re.search(r'nvfortran\s+(\d+\.\d+)', line)
    found = match.group(1) if match else (line or '(no version)')
    pinned, low = lock['nvhpc']['version'], lock['nvhpc'].get('min')
    if match and found != pinned and low is not None and version(found) >= version(low):
        # a newer NVHPC than the pinned one is untested, not refused
        return Check(fc, False, found, need, 'W_NVHPC_UNTESTED', f'nvfortran {found}: lean_mpas is tested with NVHPC '
                     f'{pinned}', 'build')
    return _check(fc, bool(match) and found == pinned, found, need, 'E_NVHPC_VERSION')


def _cuda(fc: str, compiler: Optional[str], lock: dict, pinned: bool = True) -> Check:
    """CUDA in the NVHPC bundle of nvfortran (<nvhpc>/<arch>/<year>/compilers/bin -> <year>/cuda/<version>). A
    compiler wrapper resolves outside the bundle, so every nvfortran on PATH is tried as well."""
    need = f'CUDA {lock["nvhpc"]["cuda"]} in the NVHPC bundle'
    on_path = [str(Path(folder) / fc) for folder in os.environ.get('PATH', '').split(os.pathsep) if folder]
    candidates = [find_compiler(fc, compiler)] + on_path
    for candidate in candidates:
        if not candidate or not os.path.isfile(candidate):
            continue
        parents = Path(candidate).resolve().parents
        cuda = parents[2] / 'cuda' if len(parents) > 2 else None
        if cuda is not None and cuda.is_dir():
            found = sorted(p.name for p in cuda.iterdir() if re.fullmatch(r'\d+\.\d+', p.name))
            ok = lock['nvhpc']['cuda'] in found if pinned else bool(found)   # another NVHPC: E_CUDA_DRIVER judges it
            return _check('cuda', ok, f'{cuda}: {", ".join(found) or "none"}', need, 'E_NVHPC_VERSION')
    return _check('cuda', False, 'no NVHPC cuda folder next to nvfortran', need, 'E_NVHPC_VERSION')


def _gpu(lock: dict) -> Check:
    want = lock['gpu']['cc']
    low = lock['gpu'].get('cc_min')
    need = f'cc{low} or newer (lean.lock.json gpu.cc_min)' if low is not None else f'cc{want} (lean.lock.json gpu.cc)'
    smi = shutil.which('nvidia-smi')
    line = first_line([smi, '--query-gpu=compute_cap,driver_version', '--format=csv,noheader']) if smi else None
    parts = [part.strip() for part in (line or '').split(',')]
    if len(parts) != 2 or not re.fullmatch(r'\d+\.\d+', parts[0]):
        return _check('gpu', False, 'nvidia-smi not found' if smi is None else f'nvidia-smi gave {line!r}', need,
                      'E_NO_GPU', 'run')
    major, minor = (int(part) for part in parts[0].split('.'))
    cc = major * 10 + minor
    if cc != want and low is not None and cc >= low:
        # a card other than the tested one (from gpu.cc_min up) is a warning; the build compiles for its cc
        from tools.lean import card
        name = next((entry['name'] for entry in card.smi_lines()), 'unknown')
        return Check('gpu', False, f'cc{cc}, driver {parts[1]}', need, 'W_GPU_UNTESTED',
                     f'card cc{cc} ({name}): lean_mpas is tested on cc{want}; ./lean build compiles for cc{cc}', 'run')
    return _check('gpu', cc == want, f'cc{cc}, driver {parts[1]}', need, 'E_GPU_CC', 'run',
                  None if cc == want else _cc_hint(cc, want, low))


def _cc_hint(cc: int, want: int, low: Optional[int]) -> str:
    if low is None:
        return f'this card is cc{cc}; lean_mpas card builds are tested on cc{want} only'
    return f'this card is cc{cc}; lean_mpas card builds need compute capability {low // 10}.{low % 10} (cc{low}) or newer'


def _build_cuda(nvfortran: str, cc: int) -> Optional[str]:
    """The CUDA version nvfortran would build for: the cuda/<version> its -dryrun of an empty OpenACC source names."""
    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / 'empty.f90'
        source.write_text('end\n', encoding='utf-8')
        try:
            done = subprocess.run([nvfortran, '-acc', f'-gpu=cc{cc}', '-dryrun', str(source)], capture_output=True,
                                  text=True, env=probe_env(), timeout=60, cwd=tmp)
        except (OSError, subprocess.SubprocessError):
            return None
    found = re.findall(r'cuda/(\d+\.\d+)', done.stdout + done.stderr)
    return found[0] if found else None


def _driver_cuda() -> Optional[str]:
    """The CUDA version the driver supports: nvidia-smi's header (CUDA Version: 13.2)."""
    smi = shutil.which('nvidia-smi')
    if smi is None:
        return None
    try:
        done = subprocess.run([smi], capture_output=True, text=True, env=probe_env(), timeout=60)
    except (OSError, subprocess.SubprocessError):
        return None
    found = re.search(r'CUDA Version:\s*(\d+\.\d+)', done.stdout)
    return found.group(1) if found else None


def _gpu_build(fc: str, compiler: Optional[str], lock: dict) -> list[Check]:
    """The gpu build checks: the CUDA the build uses against the driver's (E_CUDA_DRIVER), cuobjdump beside it
    (E_NO_CUOBJDUMP), device 0 known when there are two cards (W_CARD_WHICH)."""
    from tools.lean import card
    path = find_compiler(fc, compiler)
    out = []
    zero = card.device_zero()
    cc = zero.cc if zero else lock['gpu']['cc']
    build, driver = (_build_cuda(path, cc) if path else None), _driver_cuda()
    if build and driver and version(build) > version(driver):
        out.append(Check('cuda driver', False, f'CUDA {build}', f'CUDA {driver} or older', 'E_CUDA_DRIVER',
                         f'nvfortran would build for CUDA {build}; the driver supports CUDA {driver} (a newer driver, or '
                         f'an NVHPC with a CUDA at or below {driver})', 'build'))
    else:
        out.append(Check('cuda driver', True, f'build CUDA {build or "not determined"}, driver CUDA '
                         f'{driver or "not determined"}', 'the build\'s CUDA at or below the driver\'s', None, None, 'build'))
    tool = card.find_cuobjdump(path)
    out.append(Check('cuobjdump', tool is not None, tool or 'not found', 'cuobjdump next to the CUDA nvfortran uses',
                     None if tool else 'E_NO_CUOBJDUMP',
                     None if tool else "cuobjdump not found next to the CUDA nvfortran uses (the card build reads its "
                     "kernels' stack frames with it)", 'build'))
    cards = card.smi_lines()
    if len(cards) > 1 and zero is None:
        out.append(Check('card', False, f'{len(cards)} cards', 'the driver library (libcuda)', 'W_CARD_WHICH',
                         f'{len(cards)} cards and no driver library to tell which one CUDA uses first: the card '
                         'estimate is not made', 'run'))
    return out


def _tool(name: str, lock: dict) -> Check:
    need = f'{lock["tools_min"][name]} or newer'
    path = shutil.which(name)
    if path is None:
        return _check(name, False, 'not found', need, f'E_NO_{name.upper()}')
    found = first_line([path, '--version']) or '(no version)'
    ok = version(found) >= version(lock['tools_min'][name])
    return _check(name, ok, '.'.join(map(str, version(found))) or found, need, f'E_{name.upper()}_OLD')


def _python(lock: dict) -> Check:
    need = lock['tools_min']['python']
    found = platform.python_version()
    return _check('python', version(found) >= version(need), found, f'{need} or newer', 'E_PYTHON_OLD')


def _git() -> Check:
    path = shutil.which('git')
    found = (first_line([path, '--version']) if path else None) or 'not found'
    return _check('git', path is not None and found != 'not found', found, 'git on PATH', 'E_NO_GIT',
                  'fetch-upstream')


def _netcdf(compiler: str) -> Check:
    need = 'netCDF-C with CDF-5 (NETCDF=<prefix>, or nc-config on PATH)'
    try:
        nc = netcdf.find()
    except netcdf.NetCDFError as error:
        return _check('netcdf', False, error.detail, need, error.code,
                      hint=HINTS['netcdf_static'] if error.code == 'E_NETCDF_STATIC' else None)
    if nc.kind == 'static':
        error = netcdf.link_check(nc, compiler)
        if error is not None:
            return _check('netcdf', False, error, need, 'E_NETCDF_STATIC', hint=HINTS['netcdf_static'])
        found = (f'netCDF-C {nc.version}, static, in {nc.libdir} '
                 f'(CDF-5 yes from {nc.capabilities_from}, link check OK; deps {" ".join(nc.deps)}')
    else:
        source = nc.nc_config or str(Path(nc.includedir) / 'netcdf_meta.h')
        found = (f'netCDF-C {nc.version} in {nc.libdir} '
                 f'(CDF-5 yes, netCDF-4 {"yes" if nc.nc4 else "no"}; {source} via {nc.source}')
    if nc.skipped:
        found += '; skipped ' + '; '.join(nc.skipped)
    return _check('netcdf', True, found + ')', need, 'E_NO_NETCDF')


def _tables() -> list[Check]:
    pins = tables.expected()
    out = []
    for name, state in tables.status().items():
        need = f'sha256 {pins[name]["sha256"][:16]}... ({pins[name]["size"]} bytes)'
        found = {'ok': 'sha256 matches', 'missing': f'not in {tables.TABLES_DIR}/',
                 'bad': f'{tables.TABLES_DIR}/{name} does not match lean.lock.json'}[state]
        out.append(_check(name, state == 'ok', found, need, 'W_NO_TABLES' if state == 'missing' else 'E_TABLES_SHA',
                          'run', HINTS['tables']))
    return out


def checks(target: str, fc: str, compiler: Optional[str] = None, with_tables: bool = True) -> list[Check]:
    """One Check per requirement of a target/fc build and its run. compiler replaces the first fc on PATH;
    with_tables=False leaves out the tables (./lean build checks those itself)."""
    lock = config.load_lock()
    found = [_compiler(fc, compiler, lock)]
    if target == 'gpu':
        found.append(_cuda(fc, compiler, lock, pinned=found[0].code != 'W_NVHPC_UNTESTED'))
    found += [_tool('cmake', lock), _tool('ninja', lock), _python(lock), _git(),
              _netcdf(find_compiler(fc, compiler) or compiler or fc)]
    if target == 'gpu':
        found.append(_gpu(lock))
        found += _gpu_build(fc, compiler, lock)
    if with_tables:
        found += _tables()
    return found


def detail(check: Check) -> str:
    """'<name> <found>, need <need> (<hint>)': what a failure line says after its code."""
    return f'{check.name} {check.found}, need {check.need} ({check.hint})'


def render(found: list[Check], as_json: bool = False, extra: Optional[dict] = None) -> tuple[str, int]:
    """The doctor's output and exit status (0 when no E_ code failed). extra (the memory part, tools/lean/memory.py):
    {'lines': info and note lines printed before the verdict, 'json': added to the JSON}; None prints the checks
    only."""
    missing = sum(1 for check in found if not check.ok and check.code.startswith('E_'))
    verdict = f'DOCTOR FAIL: {missing} missing' if missing else 'DOCTOR OK'
    if as_json:
        data = {'checks': [asdict(check) for check in found], 'missing': missing, 'verdict': verdict}
        if extra is not None:
            data.update(extra.get('json', {}))
        return json.dumps(data, indent=1), 1 if missing else 0
    lines = []
    for check in found:
        if check.need == '':     # a memory or run-directory table line: its text is complete in found and hint
            lines.append(f'ok    {check.name}: {check.found}' if check.ok else
                         f'{check.code}: {check.hint}' if check.code in ('E_NO_TABLE', 'E_TABLE_ORDER', 'E_INIT') else
                         f'{check.code}: {check.name} {check.found} ({check.hint})')
        elif check.ok:
            lines.append(f'ok    {check.name}: {check.found} (need {check.need})')
        elif check.code in ('W_NVHPC_UNTESTED', 'W_GPU_UNTESTED', 'E_CUDA_DRIVER', 'E_NO_CUOBJDUMP', 'W_CARD_WHICH'):
            lines.append(f'{check.code}: {check.hint}')
        else:
            lines.append(f'{check.code}: {detail(check)}')
    lines += (extra or {}).get('lines', [])
    return '\n'.join(lines + [verdict]), 1 if missing else 0
