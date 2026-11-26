"""The memory sum in Python: the rows of config/memory_terms.json evaluated for a case, the host need and the card
ledger, with the same arithmetic as src/run/lean_memory.f90 (docs/memory.md lists the rows).

    load_terms() -> dict                      config/memory_terms.json
    case_dict(terms, **fields) -> dict        memory_case's fields with the program's defaults
    row_bytes(row, case, terms, side) -> int  one row on 'host' or 'card'
    host_needed(case, terms) -> dict          resident, start, step, sum, headroom, need
    card_needed(case, terms, frame_bytes, threads, held_q=None, stack_at_load=None, residual=None) -> dict
    host_available(root='/') -> (bytes or None, source or why)
    slots(env, ncells, nz) -> dict            the slot and chunk variables as the program rounds them
    glacier_capacity(n, cold) -> int
"""
from __future__ import annotations

import ast
import json
import os
import platform
import re
from pathlib import Path
from typing import Optional

ROOT = Path(__file__).resolve().parents[2]
TERMS = ROOT / 'config/memory_terms.json'
MIB = 2 ** 20


def load_terms(path: Path = TERMS) -> dict:
    return json.loads(Path(path).read_text(encoding='utf-8'))


def case_dict(terms: dict, **fields) -> dict:
    """memory_case with the program's defaults; unknown fields are an error (as a Fortran component would be)."""
    case = {**{k: 0 for k in terms['case_int']}, **{k: False for k in terms['case_logical']}, **terms['case_defaults']}
    for key, value in fields.items():
        if key not in case:
            raise KeyError(f'memory_case has no field {key!r}')
        case[key] = value
    return case


def evaluate(expr: str, case: dict, constants: dict):
    return eval(compile(ast.parse(expr, mode='eval'), '<formula>', 'eval'), {'__builtins__': {}, 'max': max, 'min': min},
                {**constants, **case})


def array_bytes(row: dict, case: dict, terms: dict, side: str) -> dict:
    """{array: bytes} of a row on one side; empty when its condition there is false."""
    cond = row[side]
    if cond is None or not evaluate(cond, case, terms['constants']):
        return {}
    skip = row.get('card_skip', []) if side == 'card' else []
    return {a['name']: int(evaluate(a['bytes'], case, terms['constants'])) for a in row['arrays'] if a['name'] not in skip}


def card_round(size: int, terms: dict) -> int:
    """One card allocation as the driver charges it: whole pages from card_round_from_bytes up (the measured block)."""
    page, start = terms['measured'].get('card_round_bytes', 0), terms['measured'].get('card_round_from_bytes', 0)
    return -(-size // page) * page if page and size >= start else size


def row_bytes(row: dict, case: dict, terms: dict, side: str) -> int:
    """A row's bytes on one side; on the card each array rounded as the driver allocates it (card_round)."""
    sizes = array_bytes(row, case, terms, side).values()
    return sum(card_round(size, terms) for size in sizes) if side == 'card' else sum(sizes)


def headroom(total: int, fraction: float, min_mib: int) -> int:
    """max(fraction x total, min_mib MiB), in whole bytes as the program computes it (int64, truncated)."""
    return max(int(total * fraction), min_mib * MIB)


def host_needed(case: dict, terms: dict) -> dict:
    """Host = resident + max(start, step); need = that + max(5 %, 512 MiB)."""
    lives = {'resident': 0, 'start': 0, 'step': 0}
    for row in terms['rows']:
        lives[row['life']] += row_bytes(row, case, terms, 'host')
    total = lives['resident'] + max(lives['start'], lives['step'])
    room = headroom(total, terms['headroom']['host_fraction'], terms['headroom']['host_min_mib'])
    return {**lives, 'sum': total, 'headroom': room, 'need': total + room}


def card_needed(case: dict, terms: dict, frame_bytes: int, threads: int, held_q: Optional[int] = None,
                stack_at_load: Optional[bool] = None, residual: Optional[int] = None) -> dict:
    """The card ledger. held_q: what the card holds when the program first reads its free memory (CUDA context,
    runtime and module image); rows: the card rows not allocated at load, each array rounded to the driver's pages;
    stack: 0 when the stack is charged at load, else threads x (the largest kernel frame - the default stack already
    in held_q); residual: the measured remainder. full = held_q + rows + stack + residual, headroom =
    max(5 % of full, 256 MiB). The program, which reads the free memory after held_q is taken, needs `more` =
    rows + stack + residual + headroom; the doctor, before any run, needs full + headroom."""
    measured = terms['measured']
    held_q = measured['held_q_bytes'] if held_q is None else held_q
    stack_at_load = measured['stack_at_load'] if stack_at_load is None else stack_at_load
    residual = measured['residual_bytes'] if residual is None else residual
    rows = sum(row_bytes(row, case, terms, 'card') for row in terms['rows'] if not row['at_load'])
    stack = 0 if stack_at_load else threads * max(frame_bytes - measured.get('stack_default_bytes', 0), 0)
    full = held_q + rows + stack + residual
    room = headroom(full, terms['headroom']['card_fraction'], terms['headroom']['card_min_mib'])
    return {'held_q': held_q, 'rows': rows, 'stack': stack, 'residual': residual, 'full': full, 'headroom': room,
            'more': rows + stack + residual + room, 'need': full + room, 'threads': threads}


def glacier_capacity(n: int, cold: bool) -> int:
    """The list's size: a cold start doubles from 64 (src/land_ice.f90:38-45); a restart allocates exactly n."""
    if n <= 0:
        return 0
    if not cold:
        return n
    capacity = 64
    while capacity < n:
        capacity *= 2
    return capacity


def slots(env: dict, ncells: int, nz: int) -> dict:
    """The card's slot and chunk variables with the program's defaults and rounding (src/phys_dev.f90
    radiation_device_start, src/device/micro_pool.f90 micro_pool_start, src/device/mynn_card.f90 mynn_card_start)."""
    def number(name, default):
        try:
            return int(str(env.get(name, default)).strip())
        except ValueError:
            return default
    rad = max(1, min(number('LEAN_RAD_SLOTS', 2048), ncells))
    micro = max(128, (number('LEAN_MICRO_SLOTS', 8192) // 128) * 128)
    gf = max(128, (number('LEAN_GF_SLOTS', 8192) // 128) * 128)
    nch = max(128, min(number('LEAN_MYNN_CHUNK', 12288), ncells))
    n = (ncells + nch - 1) // nch
    nch = (((ncells + n - 1) // n + 31) // 32) * 32
    ldc = ((nch + 31) // 32) * 32
    plumes = max(128, min(number('LEAN_MYNN_PLUMES', 8192), ldc))
    plumelev = max(2, min(number('LEAN_MYNN_PLUMELEV', 16), nz - 1))
    return {'rad_slots': rad, 'micro_slots': micro, 'gf_slots': gf, 'mynn_ldc': ldc, 'mynn_plumes': plumes,
            'mynn_plumelev': plumelev}


def stages(env: dict, card_build: bool) -> dict:
    """LEAN_CARD / LEAN_CHECK as phys_mode parses them (src/phys_mode.f90 parse_stages: an unset LEAN_CARD means all,
    LEAN_CARD=none keeps the physics on the host); a cpu build has none."""
    if not card_build:
        return {'any_card': False, 'card_radiation': False, 'card_thompson': False}
    card, check = env.get('LEAN_CARD'), env.get('LEAN_CHECK', '')
    all_on = card is None or 'all' in card
    card = card or ''
    names = ['thompson', 'inputs', 'surface', 'land', 'pbl', 'gwdo', 'gf', 'finish']
    on = {name: all_on or name in card or name in check for name in names}
    radiation = all_on or 'radiation' in card or 'radiation' in check
    return {'any_card': any(on[name] for name in names[1:]) or radiation, 'card_radiation': radiation,
            'card_thompson': on['thompson']}


# ---- what is available on the host -----------------------------------------------------------------------------------
def _read(path: Path) -> Optional[str]:
    try:
        return path.read_text(encoding='utf-8')
    except OSError:
        return None


def _limit(text: Optional[str]) -> Optional[int]:
    """A cgroup limit: None for 'max', a missing file or v1's 2^62-or-more sentinel."""
    if text is None or text.strip() in ('', 'max'):
        return None
    value = int(text.split()[0])
    return None if value >= 2 ** 62 else value


def available_linux(root: str = '/') -> tuple[Optional[int], str]:
    """MemAvailable and every finite cgroup limit up the process's path (v2 memory.max, v1 memory.limit_in_bytes):
    the smallest of MemAvailable and max(0, limit - usage) at each level, and its source."""
    base = Path(root)
    meminfo = _read(base / 'proc/meminfo')
    found = re.search(r'^MemAvailable:\s+(\d+)\s+kB', meminfo or '', re.M)
    if not found:
        return None, 'no MemAvailable in /proc/meminfo'
    best, source = int(found.group(1)) * 1024, 'MemAvailable'
    mounts = _read(base / 'proc/self/mountinfo') or ''
    groups = _read(base / 'proc/self/cgroup') or ''
    for line in mounts.splitlines():
        left, _, right = line.partition(' - ')
        fields, kinds = left.split(), right.split()
        if len(fields) < 5 or not kinds:
            continue
        mount_root, point = fields[3], fields[4]
        version = 2 if kinds[0] == 'cgroup2' else 1 if kinds[0] == 'cgroup' and 'memory' in right.split()[-1].split(',') else 0
        if not version:
            continue
        path = None
        for entry in groups.splitlines():
            parts = entry.split(':', 2)
            if len(parts) == 3 and ((version == 2 and parts[0] == '0' and parts[1] == '') or
                                    (version == 1 and 'memory' in parts[1].split(','))):
                path = parts[2]
        if path is None:
            continue
        relative = os.path.relpath(path, mount_root) if path.startswith(mount_root) else path.lstrip('/')
        level = base / point.lstrip('/') / ('' if relative == '.' else relative)
        top = base / point.lstrip('/')
        while True:
            limit_file, usage_file = (('memory.max', 'memory.current') if version == 2 else
                                      ('memory.limit_in_bytes', 'memory.usage_in_bytes'))
            limit = _limit(_read(level / limit_file))
            if limit is not None:
                usage = int((_read(level / usage_file) or '0').split()[0])
                free = max(0, limit - usage)
                if free < best:
                    cgroup = '/' + str(level.relative_to(top)) if level != top else '/'
                    best, source = free, f'cgroup {cgroup}'
            if level == top:
                break
            level = level.parent
    return best, source


def available_darwin() -> tuple[Optional[int], str]:
    """(free + inactive) pages x page size from host_statistics64, as the program reads it (an estimate: the
    compressor is not counted)."""
    import ctypes
    import ctypes.util
    try:
        lib = ctypes.CDLL(ctypes.util.find_library('System'))
        lib.mach_host_self.restype = ctypes.c_uint
        HOST_VM_INFO64 = 4           # the flavor (mach/host_info.h)
        count = ctypes.c_uint(104)   # HOST_VM_INFO64_COUNT of the macOS SDK (mach/host_info.h)
        buffer = (ctypes.c_uint * 104)()
        status = lib.host_statistics64(lib.mach_host_self(), HOST_VM_INFO64, buffer, ctypes.byref(count))
        page = ctypes.c_size_t()
        size = ctypes.c_size_t(ctypes.sizeof(page))
        lib.sysctlbyname(b'hw.pagesize', ctypes.byref(page), ctypes.byref(size), None, 0)
    except (OSError, AttributeError) as error:
        return None, f'host_statistics64: {error}'
    if status != 0:
        return None, f'host_statistics64 returned {status}'
    # vm_statistics64: free_count, active_count, inactive_count are the first three natural_t words
    return (buffer[0] + buffer[2]) * page.value, 'free and inactive pages'


def host_available(root: str = '/') -> tuple[Optional[int], str]:
    system = platform.system()
    if system == 'Linux' or root != '/':
        return available_linux(root)
    if system == 'Darwin':
        return available_darwin()
    return None, f'{system} is not Linux or macOS'


# ---- ./lean doctor's memory part ------------------------------------------------------------------------------------
# the first Fortran record marker of each table (the byte length of its one record): read as little- and big-endian,
# it tells the byte order of the file
THOMPSON = {'MP_THOMPSON_QIautQS_DATA.DBL': 28160, 'MP_THOMPSON_QRacrQG_DATA.DBL': 8586368,
            'MP_THOMPSON_QRacrQS_DATA.DBL': 2759904, 'MP_THOMPSON_freezeH2O_DATA.DBL': 27106200}
ALWAYS = ('RRTMG_LW_DATA', 'RRTMG_SW_DATA', 'OZONE_DAT.TBL', 'OZONE_LAT.TBL', 'OZONE_PLEV.TBL')
SCHEMES = ('config_microp_scheme', 'config_convection_scheme', 'config_lsm_scheme', 'config_pbl_scheme',
           'config_radt_lw_scheme', 'config_radt_sw_scheme', 'config_sfclayer_scheme')
HOST_HINT = 'a smaller mesh or fewer levels, or a machine with more memory'
CARD_HINT = ('a smaller mesh or fewer levels, a card with more memory; LEAN_RAD_SLOTS and LEAN_MICRO_SLOTS lower the fixed '
             'part')


def read_namelist(path) -> dict:
    """{option: value} of a Fortran namelist file, option names lower case; strings unquoted, logicals bool."""
    out = {}
    for line in Path(path).read_text(encoding='utf-8', errors='replace').splitlines():
        line = line.split('!', 1)[0].strip()
        found = re.match(r'(\w+)\s*=\s*(.+?)\s*,?\s*$', line)
        if not found:
            continue
        value = found.group(2).strip()
        if value[:1] in '\'"':
            value = value[1:-1] if len(value) > 1 else ''
        elif value.lower().strip('.') in ('true', 't', 'false', 'f'):
            value = value.lower().strip('.') in ('true', 't')
        out[found.group(1).lower()] = value
    return out


def stream_template(path, name: str) -> Optional[str]:
    import xml.etree.ElementTree as ET
    try:
        tree = ET.parse(path)
    except (ET.ParseError, OSError):
        return None
    for element in tree.getroot():
        if element.get('name') == name:
            return element.get('filename_template')
    return None


def _check(name, ok, found, code, hint, required_for='run'):
    from tools.lean.doctor import Check
    return Check(name, ok, found, '', None if ok else code, None if ok else hint, required_for)


def table_checks(run_dir: Path, nml: dict) -> list:
    """The tables startup opens: RRTMG and ozone always; LANDUSE.TBL on a cold start with physics; NoahmpTable.TBL
    with a land surface; the Thompson tables with microphysics, in one byte order."""
    cold = not nml.get('config_do_restart', False)
    on = {key: nml.get(key, 'suite') != 'off' for key in SCHEMES}
    wanted = list(ALWAYS)
    if cold and any(on.values()):
        wanted.append('LANDUSE.TBL')
    if on['config_lsm_scheme']:
        wanted.append('NoahmpTable.TBL')
    out = []
    for name in wanted:
        there = (run_dir / name).exists()
        out.append(_check(name, there, 'present' if there else 'missing', 'E_NO_TABLE',
                          f'{run_dir}/{name} is missing: lean_mpas opens it at startup'))
    if not on['config_microp_scheme']:
        return out
    orders = []
    for name, record in THOMPSON.items():
        if not (run_dir / name).exists():
            out.append(_check(name, False, 'missing', 'E_NO_TABLE', f'{run_dir}/{name} is missing: lean_mpas opens it at '
                                                                    'startup'))
            orders.append('missing')
            continue
        with open(run_dir / name, 'rb') as stream:
            head = stream.read(4)
        orders.append('little' if len(head) == 4 and int.from_bytes(head, 'little') == record else
                      'big' if len(head) == 4 and int.from_bytes(head, 'big') == record else 'unreadable')
    if 'missing' in orders:
        return out
    state = 'unreadable' if 'unreadable' in orders else 'in mixed byte orders' if len(set(orders)) > 1 else None
    out.append(_check('Thompson table order', state is None, f'{orders[0]}-endian' if state is None else state,
                      'E_TABLE_ORDER', f'{run_dir}: the Thompson tables are {state}'))
    return out


def program_report(exe: Path, run_dir: Path) -> tuple[Optional[dict], list]:
    """The build's own sum: lean_mpas started in run_dir with LEAN_MEMORY_REPORT=1 (it prints its check and stops before
    the mesh). (numbers or None, the program's ERR lines)."""
    import subprocess
    done = subprocess.run([str(exe)], cwd=run_dir, capture_output=True, text=True,
                          env={**os.environ, 'LEAN_MEMORY_REPORT': '1'}, timeout=600)
    errors = [line.strip() for line in done.stdout.splitlines() if line.strip().startswith(('ERROR', 'CRITICAL'))]
    text = done.stdout
    head = re.search(r'Memory check: nCells (\d+), nEdges (\d+), nVertices (\d+), nVertLevels (\d+), (\d+) glacier', text)
    if not head:
        return None, errors or [f'lean_mpas exited {done.returncode} without its memory check']
    out = dict(zip(('ncells', 'nedges', 'nvertices', 'nz', 'n_glacier'), map(int, head.groups())))
    out['case'] = {found.group(1): (found.group(2) == 'T' if found.group(2) in 'TF' else int(found.group(2)))
                   for found in re.finditer(r'^case (\w+) (-?\d+|[TF])\s*$', text, re.M)}
    host = re.search(r'host: (\d+) MiB needed \((\d+) MiB and (\d+) MiB headroom\), (?:(\d+) MiB available \((.*)\)|'
                     r'available unknown)', text)
    if host:
        out['host'] = {'need': int(host.group(1)), 'sum': int(host.group(2)), 'headroom': int(host.group(3)),
                       'available': int(host.group(4)) if host.group(4) else None, 'source': host.group(5)}
    card_line = re.search(r'card (\S+): (\d+) MiB more needed \(rows (\d+), stack (\d+) for (\d+) threads, residual (\d+), '
                          r'headroom (\d+)\), (\d+) MiB free of (\d+) MiB now', text)
    if card_line:
        keys = ('uuid', 'more', 'rows', 'stack', 'threads', 'residual', 'headroom', 'free_q', 'total')
        out['card'] = {k: (v if k == 'uuid' else int(v)) for k, v in zip(keys, card_line.groups())}
    return out, errors


def memory_checks(target: str, init=None, namelist=None, streams=None, run_dir=None, build=None,
                  env=None) -> tuple[list, dict]:
    """(checks, extra) of ./lean doctor with --init/--namelist/--streams/--run-dir/--build: with --build the build's
    own sum (program_report), otherwise an estimate from the init, the namelist and the environment."""
    import shutil
    import tempfile
    from tools.lean import card, ncread
    env = dict(os.environ) if env is None else env
    terms = load_terms()
    checks, lines, report = [], [], {}
    run_dir = Path(run_dir).resolve() if run_dir else None
    namelist = Path(namelist) if namelist else (run_dir / 'namelist.atmosphere' if run_dir else None)
    streams = Path(streams) if streams else (run_dir / 'streams.atmosphere' if run_dir else None)
    nml = read_namelist(namelist) if namelist and namelist.is_file() else {}
    if init is None and streams is not None and streams.is_file():
        template = stream_template(streams, 'input')
        if template:
            init = (run_dir or streams.parent) / template
    if run_dir is not None:
        checks += table_checks(run_dir, nml)
    zero = card.device_zero() if target == 'gpu' else None
    if build is not None:
        build = Path(build).resolve()
        record = json.loads((build / 'build.json').read_text(encoding='utf-8'))
        # The program runs in a directory of its own (it writes its log and lock where it runs): links to the run
        # directory's files, or to the init, namelist, streams and stream lists given.
        if run_dir is None and (init is None or namelist is None or streams is None):
            checks.append(_check('init', False, 'none', 'E_INIT', 'no init: give --run-dir, or --init, --namelist '
                                 'and --streams'))
            return checks, {'lines': lines, 'json': {'memory': report}}
        made = work = Path(tempfile.mkdtemp(prefix='lean-doctor-'))
        if run_dir is not None:
            for entry in run_dir.iterdir():
                if not entry.name.startswith('log.atmosphere.'):
                    (work / entry.name).symlink_to(entry)
        else:
            for sibling in Path(streams).resolve().parent.iterdir():   # the stream lists beside the streams file
                if sibling.name.startswith('stream_list.'):
                    (work / sibling.name).symlink_to(sibling)
            (work / 'namelist.atmosphere').symlink_to(Path(namelist).resolve())
            (work / 'streams.atmosphere').symlink_to(Path(streams).resolve())
            template = stream_template(streams, 'input') or Path(init).name
            (work / template).parent.mkdir(parents=True, exist_ok=True)
            (work / template).symlink_to(Path(init).resolve())
        try:
            numbers, errors = program_report(build / 'lean_mpas', work)
        finally:
            shutil.rmtree(made)
        if numbers is None:
            checks.append(_check('init', False, str(init), 'E_INIT', f'{init}: ' + '; '.join(errors)))
            return checks, {'lines': lines, 'json': {'memory': report}}
        report.update(numbers)
        host = numbers.get('host', {})
        avail = host.get('available')
        found = (f"{host.get('need')} MiB needed (with {host.get('headroom')} MiB headroom), " +
                 (f"{avail} MiB available ({host.get('source')})" if avail is not None else 'available unknown'))
        checks.append(_check('host memory', avail is None or host['need'] <= avail, found, 'E_MEMORY_HOST', HOST_HINT))
        if 'card' in numbers:
            c = numbers['card']
            held_q = -(-terms['measured']['held_q_bytes'] // MIB)
            full = held_q + c['rows'] + c['stack'] + c['residual']
            room = max(int(full * terms['headroom']['card_fraction']), terms['headroom']['card_min_mib'])
            free0 = zero.free_mib if zero and zero.free_mib is not None else None
            total = zero.total_mib if zero else c['total']
            found = (f'{full + room} MiB needed (with {room} MiB headroom), ' +
                     (f'{free0} MiB free of {total} MiB on {c["uuid"]}' if free0 is not None else 'free unknown'))
            report['card_doctor'] = {'full': full, 'headroom': room, 'need': full + room, 'free_0': free0}
            checks.append(_check('card memory', free0 is None or full + room <= free0, found, 'E_MEMORY_CARD', CARD_HINT))
        elif record['config']['target'] == 'gpu':
            lines.append('info  card memory: unknown (the program could not read the card)')
        return checks, {'lines': lines, 'json': {'memory': report}}
    # without --build: an estimate from the init, the namelist and the environment
    if init is None:
        checks.append(_check('init', False, 'none', 'E_INIT', 'no init: give --init FILE or --run-dir DIR'))
        return checks, {'lines': lines, 'json': {'memory': report}}
    try:
        dims = ncread.dimensions(init)
    except (ncread.NcError, OSError) as error:
        checks.append(_check('init', False, str(init), 'E_INIT', f'{init}: {error}'))
        return checks, {'lines': lines, 'json': {'memory': report}}
    cold = not nml.get('config_do_restart', False)
    fields = dict(ncells=dims.get('nCells', 0), nedges=dims.get('nEdges', 0), nvertices=dims.get('nVertices', 0),
                  nz=dims.get('nVertLevels', 0), cold=cold, apply_lbcs=bool(nml.get('config_apply_lbcs', False)),
                  micro_on=nml.get('config_microp_scheme', 'suite') != 'off')
    why = ['the largest output taken as nEdges x nVertLevels', 'every held field counted']
    try:
        fields['n_glacier'] = ncread.glacier_count(init) if cold else 0
    except (ncread.NoLibrary, ncread.NcError) as error:
        why.append(f'the glacier columns left out ({error})')
    fields['glacier_capacity'] = glacier_capacity(fields.get('n_glacier', 0), cold)
    fields['largest_output'] = max(fields['ncells'], fields['nedges'] * fields['nz'])
    fields['held'] = len(terms['held_names']) + (len(terms['start_names']) if cold else 0)
    card_build = target == 'gpu'
    fields.update(card_build=card_build, staged_pools=True, **stages(env, card_build),
                  **slots(env, fields['ncells'], fields['nz']))
    case = case_dict(terms, **fields)
    report['case'] = case
    need = host_needed(case, terms)
    avail, source = host_available()
    found = (f"{-(-need['need'] // MIB)} MiB needed (with {-(-need['headroom'] // MIB)} MiB headroom), " +
             (f'{avail // MIB} MiB available ({source})' if avail is not None else 'available unknown'))
    checks.append(_check('host memory', avail is None or need['need'] <= avail, found, 'E_MEMORY_HOST', HOST_HINT))
    lines.append('info  the sum is an estimate without --build: ' + '; '.join(why))
    if card_build:
        threads = zero.threads if zero else 0
        ledger = card_needed(case, terms, 0, threads)
        free0 = f'{zero.free_mib} MiB free' if zero and zero.free_mib is not None else 'free unknown'
        lines.append(f"info  card memory: about {-(-ledger['need'] // MIB)} MiB needed (estimate: no build, its stack "
                     f"frame not known{'' if zero else ', no card found'}), {free0}")
    return checks, {'lines': lines, 'json': {'memory': report}}
