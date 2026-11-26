#!/usr/bin/env python3
"""The memory sum's rows: the state types and the other allocation routines, parsed
from their allocate statements, and the hand rows of config/memory.json. Writes

    config/memory_terms.json         every row: side conditions, life, at_load, its arrays with their byte formulas
    src/run/lean_memory_terms.f90    the same rows for the program: memory_case, row_bytes(i, mc, side)
    docs/memory.md                   the rows with their bytes for greece_60km, x1_2562 and athens (cases/athens_3km)

    python3 tools/gen_memory.py [--check]

A formula is a Python expression over memory_case's fields (CASE_INT, CASE_LOGICAL) and config/memory.json's constants:
+ - * // max min, comparisons, and or not, `a if c else b`. Python evaluates it (tools/lean/memory.py); this generator
translates it to Fortran in int64. --check writes nothing and exits 1 naming the first row or member that differs.
"""
from __future__ import annotations

import ast
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'tools'))
from switch_merge import resolve  # noqa: E402

MANIFEST = 'config/memory.json'
OUT_JSON = 'config/memory_terms.json'
OUT_F90 = 'src/run/lean_memory_terms.f90'
OUT_DOC = 'docs/memory.md'
SIDES = {'host': 1, 'card': 2, 'both': 3}
LIVES = {'resident': 1, 'start': 2, 'step': 3}
# the fields of memory_case (src/run/lean_memory_terms.f90; tools/lean/memory.py case_dict builds the same)
CASE_INT = ['ncells', 'nedges', 'nvertices', 'nz', 'nsteps', 'n_glacier', 'glacier_capacity', 'lbc_requests',
            'lbc_files', 'rad_slots', 'micro_slots', 'gf_slots', 'mynn_ldc', 'mynn_plumes', 'mynn_plumelev', 'held',
            'largest_output', 'largest_copy']
CASE_INT64 = {'largest_output', 'largest_copy'}
CASE_LOGICAL = ['card_build', 'any_card', 'staged_pools', 'card_radiation', 'card_thompson', 'cold', 'apply_lbcs',
                'moist_physics', 'micro_on', 'apvm', 'surface_update', 'products', 'storm', 'film', 'surface', 'budget',
                'maxima', 'swsplit']
CASE_DEFAULTS = {'rad_slots': 2048, 'micro_slots': 8192, 'gf_slots': 8192, 'mynn_plumes': 8192, 'mynn_plumelev': 16,
                 'cold': True, 'moist_physics': True, 'micro_on': True}
# The three meshes of docs/memory.md and the tests (greece_60km, x1_2562 from their init files; athens, the mesh of
# cases/athens_3km, from tools/report_state_memory.py). largest_output: elements of nEdges x nVertLevels, the largest history field.
CASES = {
    'greece_60km': dict(ncells=2337, nedges=7183, nvertices=4847, nz=45),
    'x1_2562': dict(ncells=2562, nedges=7680, nvertices=5120, nz=45, n_glacier=80, glacier_capacity=128),
    'athens': dict(ncells=46251, nedges=139361, nvertices=93111, nz=45),
}
for _case in CASES.values():
    _case.setdefault('largest_output', _case['nedges'] * _case['nz'])
    _case.setdefault('held', 60)


class GenError(Exception):
    pass


def load_manifest(root: Path = ROOT) -> dict:
    return json.loads((root / MANIFEST).read_text(encoding='utf-8'))


# ---- the parse of allocation routines ---------------------------------------------------------------------------------
def _joined(text: str) -> str:
    """Free-form continuation lines joined, comments dropped."""
    out = []
    for line in text.splitlines():
        code = line.split('!', 1)[0].rstrip() if not line.lstrip().startswith('!$') else ''
        out.append(code)
    return re.sub(r'&\s*\n\s*&?', ' ', '\n'.join(out))


def _shape(dims: str, aliases: dict) -> str:
    """'nz+1,maxedges,nc1' -> '(nz+1)*maxedges*(ncells+1)'; '0:ll' -> '(ll-(0)+1)'."""
    if not dims.strip():
        return '1'
    parts = []
    for dim in dims.split(','):
        dim = dim.strip()
        for alias, value in aliases.items():
            dim = re.sub(r'\b' + alias + r'\b', '(' + value + ')', dim)
        if ':' in dim:
            low, high = dim.split(':')
            dim = f'(({high})-({low})+1)'
        parts.append(dim if re.fullmatch(r'\w+', dim) else f'({dim})')
    return '*'.join(parts)


def _member_kinds(text: str) -> dict:
    """name -> bytes of one element, from the declarations (`::`) of every derived type in text."""
    kinds = {}
    for block in re.findall(r'\n\s*type\s*(?:,[^\n:]*)?(?:::)?\s*\w+\s*\n(.*?)\n\s*end\s+type', text, re.S | re.I):
        for line in block.splitlines():
            if '::' not in line:
                continue
            spec, names = line.split('!', 1)[0].split('::', 1)
            spec = spec.lower().replace(' ', '')
            if spec.startswith(('type(', 'class(', 'character', 'procedure')):
                continue
            eight = ('double' in spec or '(8)' in spec or 'kind=8' in spec or 'int64' in spec or 'real64' in spec
                     or '(rb)' in spec or 'kind=rb' in spec or 'complex' in spec)
            for name in re.findall(r'(\w+)\s*(?:\([^)]*\))?\s*(?:=[^,]*)?(?:,|$)', names.strip()):
                kinds.setdefault(name, 8 if eight else 4)
    return kinds


def parse_routine(spec: dict, root: Path = ROOT) -> list[dict]:
    """[{name, bytes}] of every `allocate(<var>%name(shape))` of spec's routine, in source order."""
    text = (root / spec['file']).read_text(encoding='utf-8')
    found = re.search(r'\n\s*subroutine\s+' + spec['routine'] + r'\s*\(', text, re.I)
    if not found:
        raise GenError(f"{spec['file']}: no subroutine {spec['routine']}")
    end = re.search(r'\n\s*end\s+subroutine', text[found.start():], re.I)
    body = _joined(text[found.start():found.start() + end.start()])
    kinds = {}
    for path in [spec['file']] + spec.get('types_in', []):
        for name, size in _member_kinds((root / path).read_text(encoding='utf-8')).items():
            kinds.setdefault(name, size)
    aliases = spec.get('aliases', {})
    aliases = {'nc1': 'ncells+1', 'ne1': 'nedges+1', 'nv1': 'nvertices+1', **aliases}
    arrays, seen = [], set()
    for stmt in re.findall(r'\ballocate\s*\((.*)\)', body, re.I):
        for name, dims in re.findall(r'\b' + spec['var'] + r'%(\w+)\(([^()]*(?:\([^()]*\)[^()]*)*)\)', stmt):
            if name in seen or name in spec.get('skip', []):
                continue
            seen.add(name)
            size = spec.get('element_bytes', {}).get(name, kinds.get(name, 4))
            arrays.append({'name': name, 'bytes': f'{size}*{_shape(dims, aliases)}'})
    if not arrays:
        raise GenError(f"{spec['file']}: {spec['routine']} allocates nothing through {spec['var']}%")
    return arrays


def state_type_arrays(spec: dict, root: Path = ROOT) -> list[dict]:
    """The members of a state type as tools/report_state_memory.py counts them: the allocate_* shape of an allocatable
    member, the declared shape of a fixed one, 4 bytes an element; the Noah-MP switch LEAN_NOAHMP_SWSPLIT resolved off
    unless the row names it as its 'switch'."""
    whole = (root / spec['file']).read_text(encoding='utf-8')
    text = whole.split(' type ' + spec['type'] + '\n', 1)[1].split(' end type', 1)[0]
    switch = 'LEAN_NOAHMP_SWSPLIT'
    text = '\n'.join(resolve(text.split('\n'), switch, spec.get('switch') == switch))
    routine = re.search(r' subroutine ' + spec['routine'] + r'\((\w+)\)', whole)
    shapes = {}
    if routine:
        start = routine.start()
        shapes = dict(re.findall(r'allocate\(' + routine[1] + r'%(\w+)\(([^()]*)\)\)',
                                 whole[start:whole.index(' end subroutine', start)]))
    arrays = []
    for line in text.splitlines():
        if '::' not in line:
            continue
        decl = line.split('::')[1].strip()
        for match in re.finditer(r'(\w+)\s*(?:\(([^)]*)\))?\s*(?:,|$)', decl):
            name, dims = match.group(1), match.group(2) or ''
            arrays.append({'name': name, 'bytes': '4*' + _shape(shapes.get(name, dims), {})})
    return arrays


def switch_delta(spec: dict, root: Path = ROOT) -> list[dict]:
    """The members a Noah-MP switch adds to the land type (its row's 'delta_of')."""
    base = {a['name']: a['bytes'] for a in state_type_arrays({**spec, 'switch': None}, root)}
    with_switch = state_type_arrays(spec, root)
    return [a for a in with_switch if base.get(a['name']) != a['bytes']]


def writer_names(root: Path = ROOT) -> tuple[list[str], list[str]]:
    """(the SOURCE_HELD names of field_class, the names of input_at_start) in src/lean_fields.f90: output_start holds
    each of them with at most nCells elements (output_start in src/lean_fields.f90)."""
    text = _joined((root / 'src/lean_fields.f90').read_text(encoding='utf-8'))
    body = text[text.index('function field_class'):text.index('end function', text.index('function field_class'))]
    held = []
    for names, source in re.findall(r"case\s*\(([^)]*)\)\s*\n\s*source\s*=\s*(\w+)", body):
        if source == 'SOURCE_HELD':
            held += re.findall(r"'(\w+)'", names)
    start = text[text.index('function input_at_start'):]
    start = re.findall(r"name\s*==\s*'(\w+)'", start[:start.index('end function')])
    if not held or not start:
        raise GenError('src/lean_fields.f90: no SOURCE_HELD names or input_at_start names')
    return held, start


# ---- formulas ---------------------------------------------------------------------------------------------------------
def evaluate(expr: str, case: dict, constants: dict):
    env = {**{k: 0 for k in CASE_INT}, **{k: False for k in CASE_LOGICAL}, **CASE_DEFAULTS, **constants, **case}
    return eval(compile(ast.parse(expr, mode='eval'), '<formula>', 'eval'), {'__builtins__': {}, 'max': max, 'min': min},
                env)


def fortran(expr: str, constants: dict) -> str:
    """The formula in Fortran, int64 throughout (mc is the memory_case)."""
    ops = {ast.Add: '+', ast.Sub: '-', ast.Mult: '*', ast.FloorDiv: '/'}
    cmp = {ast.Eq: '==', ast.NotEq: '/=', ast.Lt: '<', ast.LtE: '<=', ast.Gt: '>', ast.GtE: '>='}

    def walk(node) -> str:
        if isinstance(node, ast.Expression):
            return walk(node.body)
        if isinstance(node, ast.Constant):
            if isinstance(node.value, bool):
                return '.true.' if node.value else '.false.'
            if isinstance(node.value, int):
                return f'{node.value}_int64'
        if isinstance(node, ast.Name):
            if node.id in constants:
                return f'{constants[node.id]}_int64'
            if node.id in CASE_LOGICAL:
                return f'mc%{node.id}'
            if node.id in CASE_INT:
                return f'int(mc%{node.id},int64)'
            raise GenError(f'unknown name {node.id!r} in {expr!r}')
        if isinstance(node, ast.BinOp) and type(node.op) in ops:
            return f'({walk(node.left)}{ops[type(node.op)]}{walk(node.right)})'
        if isinstance(node, ast.BoolOp):
            joint = '.and.' if isinstance(node.op, ast.And) else '.or.'
            return '(' + joint.join(walk(v) for v in node.values) + ')'
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.Not):
            return f'(.not.{walk(node.operand)})'
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
            return f'(-{walk(node.operand)})'
        if isinstance(node, ast.Compare) and len(node.ops) == 1 and type(node.ops[0]) in cmp:
            return f'({walk(node.left)}{cmp[type(node.ops[0])]}{walk(node.comparators[0])})'
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id in ('max', 'min'):
            return f'{node.func.id}(' + ','.join(walk(a) for a in node.args) + ')'
        if isinstance(node, ast.IfExp):
            return f'merge({walk(node.body)},{walk(node.orelse)},{walk(node.test)})'
        raise GenError(f'unsupported formula part {ast.dump(node)[:60]} in {expr!r}')

    return walk(ast.parse(expr, mode='eval'))


# ---- rows -------------------------------------------------------------------------------------------------------------
def rows(manifest: dict, root: Path = ROOT) -> list[dict]:
    """Every row: name, side, life, at_load, host/card conditions (None: not on that side), arrays, routines, why."""
    out = []
    for spec in manifest['generated']:
        if spec.get('kind') == 'state_type':
            arrays = switch_delta(spec, root) if spec.get('switch') else state_type_arrays(spec, root)
        else:
            arrays = parse_routine(spec, root)
        out.append(_row(spec, arrays, f"{spec['file']} {spec['routine']}"))
    for spec in manifest['rows']:
        out.append(_row(spec, spec['arrays'], None))
    names = [row['name'] for row in out]
    if len(set(names)) != len(names):
        raise GenError('two rows with one name: ' + ', '.join(sorted({n for n in names if names.count(n) > 1})))
    constants = manifest['constants']
    for row in out:
        for expr in [row['host'], row['card']] + [a['bytes'] for a in row['arrays']]:
            if expr is not None:
                fortran(expr, constants)   # unknown names fail here
    return out


def _row(spec: dict, arrays: list[dict], source) -> dict:
    host, card = spec.get('host'), spec.get('card')
    if host is None and card is None:
        raise GenError(f"row {spec['name']}: neither host nor card")
    side = 'both' if host is not None and card is not None else ('host' if host is not None else 'card')
    return {'name': spec['name'], 'side': side, 'life': spec['life'], 'at_load': spec.get('at_load', False),
            'host': host, 'card': card, 'card_skip': spec.get('card_skip', []), 'arrays': arrays,
            'routines': spec.get('routines', [spec['routine']] if 'routine' in spec else []),
            'card_names': spec.get('card_names', [spec['var'] + '%'] if 'var' in spec else []), 'why': spec['why'],
            'source': source}


def row_bytes(row: dict, case: dict, constants: dict, side: str, measured: dict | None = None) -> int:
    """Bytes of a row on one side ('host' or 'card') for a case: 0 when its condition there is false. With measured,
    each card array is rounded to the driver's pages (measured card_round_*)."""
    cond = row[side]
    if cond is None or not evaluate(cond, case, constants):
        return 0
    skip = row['card_skip'] if side == 'card' else []
    sizes = [int(evaluate(a['bytes'], case, constants)) for a in row['arrays'] if a['name'] not in skip]
    if side == 'card' and measured:
        page, start = measured['card_round_bytes'], measured['card_round_from_bytes']
        sizes = [-(-b // page) * page if b >= start else b for b in sizes]
    return sum(sizes)


# ---- outputs ----------------------------------------------------------------------------------------------------------
def terms_json(manifest: dict, table: list[dict], root: Path = ROOT) -> str:
    held, start = writer_names(root)
    return json.dumps({'about': 'GENERATED by tools/gen_memory.py from the allocation routines and state types in src/ and config/memory.json - do not edit',
                       'held_names': held, 'start_names': start,
                       'constants': manifest['constants'], 'case_int': CASE_INT, 'case_logical': CASE_LOGICAL,
                       'case_defaults': CASE_DEFAULTS, 'headroom': manifest['headroom'], 'measured': manifest['measured'],
                       'rows': table}, indent=1) + '\n'


def _collapse(arrays: list[dict], skip: list) -> list[str]:
    """The arrays' formulas with equal shapes summed: 'k*shape' terms, so a state type is a few statements."""
    terms = {}
    for array in arrays:
        if array['name'] in skip:
            continue
        size, _, shape = array['bytes'].partition('*')
        if size.isdigit() and shape:
            terms[shape] = terms.get(shape, 0) + int(size)
        else:
            terms[array['bytes']] = terms.get(array['bytes'], 0) + 1
    return [f'{k}*({shape})' if k != 1 else shape for shape, k in terms.items()]


def _collapse_card(arrays: list[dict], skip: list) -> list[tuple[int, str]]:
    """(count, formula) of the card arrays with equal formulas counted once: each stays one allocation."""
    terms = {}
    for array in arrays:
        if array['name'] not in skip:
            terms[array['bytes']] = terms.get(array['bytes'], 0) + 1
    return [(k, formula) for formula, k in terms.items()]


def _wrap(text: str, indent: str) -> str:
    """A long statement over continuation lines of at most 120 characters, cut at '+' or ','."""
    lines, line = [], indent
    for piece in re.split(r'(?<=[+,])', text):
        if len(line) + len(piece) > 118 and line.strip():
            lines.append(line + '&')
            line = indent + '  '
        line += piece
    lines.append(line)
    return '\n'.join(lines)


def fortran_text(manifest: dict, table: list[dict], root: Path = ROOT) -> str:
    c = manifest['constants']
    m = manifest['measured']
    h = manifest['headroom']
    held, start = writer_names(root)
    names = ', '.join(f"'{row['name']}'" for row in table)
    sides = ', '.join(str(SIDES[row['side']]) for row in table)
    lives = ', '.join(str(LIVES[row['life']]) for row in table)
    loads = ', '.join('.true.' if row['at_load'] else '.false.' for row in table)
    out = ['! GENERATED by tools/gen_memory.py from the allocation routines and state types in src/ and config/memory.json - do not edit.',
           '! The rows of the memory sum; docs/memory.md lists them.',
           'module lean_memory_terms',
           ' use iso_fortran_env, only: int64',
           ' implicit none',
           ' private',
           ' public :: SIDE_HOST, SIDE_CARD, SIDE_BOTH, LIFE_RESIDENT, LIFE_START, LIFE_STEP, memory_case',
           ' public :: n_rows, row_name, row_side, row_life, row_at_load, row_bytes, write_case',
           ' public :: card_stack_at_load, card_residual_bytes, card_held_q_bytes, glacier_column_bytes',
           ' public :: card_stack_default_bytes, card_round_bytes, card_round_from_bytes, card_round',
           ' public :: host_headroom_fraction, host_headroom_min, card_headroom_fraction, card_headroom_min',
           ' public :: held_names, start_names',
           ' integer, parameter :: SIDE_HOST = 1, SIDE_CARD = 2, SIDE_BOTH = 3, LIFE_RESIDENT = 1, LIFE_START = 2, LIFE_STEP = 3',
           ' type memory_case']
    for name in CASE_INT:
        kind = 'integer(int64)' if name in CASE_INT64 else 'integer'
        out.append(f' {kind} :: {name} = {CASE_DEFAULTS.get(name, 0)}')
    for name in CASE_LOGICAL:
        out.append(f" logical :: {name} = {'.true.' if CASE_DEFAULTS.get(name) else '.false.'}")
    out += [' end type',
            f' integer, parameter :: n_rows = {len(table)}',
            _wrap(f' character(len=48), parameter :: row_name(n_rows) = [character(len=48) :: {names}]', ' '),
            _wrap(f' integer, parameter :: row_side(n_rows) = [{sides}]', ' '),
            _wrap(f' integer, parameter :: row_life(n_rows) = [{lives}]', ' '),
            _wrap(f' logical, parameter :: row_at_load(n_rows) = [{loads}]', ' '),
            f" ! the card ledger's measured terms, {m['status']}",
            f" logical, parameter :: card_stack_at_load = {'.true.' if m['stack_at_load'] else '.false.'}",
            f" integer(int64), parameter :: card_residual_bytes = {m['residual_bytes']}_int64",
            f" integer(int64), parameter :: card_held_q_bytes = {m['held_q_bytes']}_int64",
            f" integer(int64), parameter :: card_stack_default_bytes = {m['stack_default_bytes']}_int64",
            f" integer(int64), parameter :: card_round_bytes = {m['card_round_bytes']}_int64, "
            f"card_round_from_bytes = {m['card_round_from_bytes']}_int64",
            f" integer(int64), parameter :: glacier_column_bytes = {c['glacier_column_bytes']}_int64",
            ' ! the headrooms: max(fraction x sum, min), host and card',
            f" real(8), parameter :: host_headroom_fraction = {h['host_fraction']}d0, card_headroom_fraction = {h['card_fraction']}d0",
            f" integer(int64), parameter :: host_headroom_min = {h['host_min_mib']}_int64*1048576_int64",
            f" integer(int64), parameter :: card_headroom_min = {h['card_min_mib']}_int64*1048576_int64",
            ' ! the writer\'s held fields (src/lean_fields.f90 field_class SOURCE_HELD; input_at_start on a cold start)',
            _wrap(f' character(len=32), parameter :: held_names({len(held)}) = [character(len=32) :: ' +
                  ', '.join(f"'{n}'" for n in held) + ']', ' '),
            _wrap(f' character(len=32), parameter :: start_names({len(start)}) = [character(len=32) :: ' +
                  ', '.join(f"'{n}'" for n in start) + ']', ' '),
            'contains',
            ' ! one card allocation as the driver charges it: whole card_round_bytes pages from card_round_from_bytes up',
            ' integer(int64) function card_round(size) result(b)',
            ' integer(int64), intent(in) :: size',
            ' b = size',
            ' if (size >= card_round_from_bytes) b = ((size + card_round_bytes - 1_int64)/card_round_bytes)*card_round_bytes',
            ' end function',
            '',
            ' ! the bytes of row i on one side (SIDE_HOST or SIDE_CARD) for the case mc: 0 when its condition is false there',
            ' integer(int64) function row_bytes(i, mc, side) result(b)',
            ' integer, intent(in) :: i, side',
            ' type(memory_case), intent(in) :: mc',
            ' b = 0_int64',
            ' select case (i)']
    for index, row in enumerate(table, 1):
        out.append(f" case ({index})   ! {row['name']}")
        for side, code in (('host', 'SIDE_HOST'), ('card', 'SIDE_CARD')):
            if row[side] is None:
                continue
            out.append(_wrap(f'  if (side == {code}) then; if ({fortran(row[side], c)}) then', '  '))
            if side == 'card':   # each array its own allocation, rounded to the driver's pages
                for count, term in _collapse_card(row['arrays'], row['card_skip']):
                    factor = f'{count}_int64*' if count != 1 else ''
                    out.append(_wrap(f'   b = b + {factor}card_round({fortran(term, c)})', '   '))
            else:
                for term in _collapse(row['arrays'], []):
                    out.append(_wrap(f'   b = b + {fortran(term, c)}', '   '))
            out.append('  end if; end if')
    out += [' end select', ' end function', '',
            " ! the case as 'case <field> <value>' lines (LEAN_MEMORY_REPORT: tools/memory_compare.py --case reads them)",
            ' subroutine write_case(mc, unit)',
            ' type(memory_case), intent(in) :: mc',
            ' integer, intent(in) :: unit']
    out += [f"  write(unit, '(a,i0)') 'case {name} ', mc%{name}" for name in CASE_INT]
    out += [f"  write(unit, '(a,l1)') 'case {name} ', mc%{name}" for name in CASE_LOGICAL]
    out += [' end subroutine', 'end module', '']
    return '\n'.join(out)


def mib(value: int) -> str:
    return f'{value / 2**20:.2f}'


def doc_text(manifest: dict, table: list[dict]) -> str:
    c = manifest['constants']
    lines = ['# The memory sum', '',
             'GENERATED by `tools/gen_memory.py` from the allocation routines and state types in `src/` and `config/memory.json`: do not edit. '
             'The host needs resident + max(start, step), plus a headroom of '
             f"max({manifest['headroom']['host_fraction'] * 100:g} %, {manifest['headroom']['host_min_mib']} MiB); "
             "the card needs its rows plus the memory the CUDA runtime holds, the kernels' stack and a measured residual, "
             f"plus a headroom of max({manifest['headroom']['card_fraction'] * 100:g} %, "
             f"{manifest['headroom']['card_min_mib']} MiB) (`tools/lean/memory.py` card_needed, the same arithmetic as "
             '`src/run/lean_memory.f90`).', '',
             'MiB on each side for three meshes at 45 levels, as a cold cpu run (host) and a card build with every '
             "stage on the card (card). athens is the mesh of `cases/athens_3km`; its largest output and held count are the table's assumptions "
             '(nEdges x nVertLevels, 60 held fields), not a run.', '',
             '| Row | Side | Life | At load | Host when | Card when | ' +
             ' | '.join(f'{n} host | {n} card' for n in CASES) + ' |',
             '|' + '---|' * (6 + 2 * len(CASES))]
    card_case = dict(card_build=True, any_card=True, staged_pools=True, card_radiation=True, card_thompson=True)
    for row in table:
        cells = []
        for case in CASES.values():
            cells.append(mib(row_bytes(row, case, c, 'host')))
            cells.append(mib(row_bytes(row, {**case, **card_case}, c, 'card', manifest['measured'])))
        lines.append(f"| `{row['name']}` | {row['side']} | {row['life']} | {'yes' if row['at_load'] else 'no'} | "
                     f"`{row['host']}` | `{row['card']}` | " + ' | '.join(cells) + ' |')
    lines += ['', '## Where each row comes from', '']
    for row in table:
        lines.append(f"- `{row['name']}`: {row['why']}" + (f" (parsed from {row['source']})" if row['source'] else ''))
    lines.append('')
    return '\n'.join(lines)


def outputs(root: Path = ROOT) -> dict:
    manifest = load_manifest(root)
    table = rows(manifest, root)
    return {OUT_JSON: terms_json(manifest, table, root), OUT_F90: fortran_text(manifest, table, root),
            OUT_DOC: doc_text(manifest, table)}


def main(argv=None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    check = '--check' in argv
    root = ROOT
    if '--root' in argv:
        root = Path(argv[argv.index('--root') + 1])
    try:
        made = outputs(root)
    except (GenError, OSError, KeyError, ValueError) as error:
        print(f'gen_memory: {error}', file=sys.stderr)
        return 1
    stale = []
    for rel, text in made.items():
        path = root / rel
        old = path.read_text(encoding='utf-8') if path.is_file() else None
        if old == text:
            continue
        if check:
            stale.append(rel + _first_difference(old, text))
        else:
            path.write_text(text, encoding='utf-8')
    if stale:
        print('gen_memory: stale: ' + '; '.join(stale) + ' (python3 tools/gen_memory.py rewrites them)', file=sys.stderr)
        return 1
    return 0


def _first_difference(old, new) -> str:
    """' (row X member Y)' for a stale terms file, so a changed bound is named."""
    if old is None:
        return ' (missing)'
    try:
        before, after = json.loads(old)['rows'], json.loads(new)['rows']
    except (ValueError, KeyError):
        return ''
    old_rows = {r['name']: {a['name']: a['bytes'] for a in r['arrays']} for r in before}
    for row in after:
        was = old_rows.get(row['name'])
        if was is None:
            return f" (new row {row['name']})"
        for array in row['arrays']:
            if was.get(array['name']) != array['bytes']:
                return f" (row {row['name']} member {array['name']})"
    return ''


if __name__ == '__main__':
    sys.exit(main())
