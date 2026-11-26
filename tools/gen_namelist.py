#!/usr/bin/env python3
"""Writes src/run/lean_namelist_options.f90 (the namelist readers, defaults and option checks) and docs/namelist.md from
MPAS 8.4.2's Registry.xml (reference/mpas_src, ./lean fetch-upstream) and config/namelist.json.

    python3 tools/gen_namelist.py [--check]

--check writes nothing and prints DIFFERS <path> for each output that differs (exit 1); exit 2 without the Registry or
with a table that does not match it."""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
import re
import sys
from typing import NamedTuple
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
REGISTRY = ROOT / 'reference/mpas_src/src/core_atmosphere/Registry.xml'
OUT_FORTRAN = ROOT / 'src/run/lean_namelist_options.f90'
TABLE = ROOT / 'config/namelist.json'
OUT_DOCS = ROOT / 'docs/namelist.md'
DEFINED = frozenset({'DO_PHYSICS', 'DO_CHEMISTRY', 'CORE_ATMOSPHERE'})
SCHEMES = (
    'config_microp_scheme', 'config_convection_scheme', 'config_lsm_scheme',
    'config_pbl_scheme', 'config_gwdo_scheme', 'config_radt_cld_scheme',
    'config_radt_lw_scheme', 'config_radt_sw_scheme', 'config_sfclayer_scheme',
)


class Option(NamedTuple):
    name: str
    type: str
    default: str
    record: str


class Record(NamedTuple):
    name: str
    options: tuple[Option, ...]


LEAN_RECORD = Record('lean', (
    Option('config_lean_flavor', 'character', '', 'lean'),
    Option('config_lean_strict_inputs', 'logical', 'false', 'lean'),
    Option('config_lean_memory_check', 'logical', 'true', 'lean'),
))


def preprocess(path: Path, defined: frozenset = DEFINED) -> str:
    """The Registry's text with its #ifdef/#ifndef conditionals resolved and its relative quoted includes inserted; any other
    directive is an error."""
    lines = []
    stack = []
    active = True
    for number, line in enumerate(path.read_text(encoding='utf-8').splitlines(keepends=True), 1):
        directive = line.lstrip()
        if not directive.startswith('#'):
            if active:
                lines.append(line)
            continue
        location = f'{path}:{number}'
        conditional = re.fullmatch(r'#\s*(ifdef|ifndef)\s+([A-Za-z_]\w*)\s*', directive)
        include = re.fullmatch(r'#\s*include\s+"([^"]+)"\s*', directive)
        if conditional:
            kind, name = conditional.groups()
            condition = name in defined
            if kind == 'ifndef':
                condition = not condition
            stack.append((active, condition, False, number))
            active = active and condition
        elif re.fullmatch(r'#\s*else\s*', directive):
            if not stack or stack[-1][2]:
                raise ValueError(f'{location}: unbalanced #else')
            parent, condition, _, start = stack[-1]
            stack[-1] = (parent, condition, True, start)
            active = parent and not condition
        elif re.fullmatch(r'#\s*endif\s*', directive):
            if not stack:
                raise ValueError(f'{location}: unbalanced #endif')
            active = stack.pop()[0]
        elif include:
            if active:
                lines.append(preprocess(path.parent / include.group(1), defined))
        else:
            raise ValueError(f'{location}: unsupported directive {directive.strip()}')
    if stack:
        raise ValueError(f'{path}:{stack[-1][3]}: missing #endif')
    return ''.join(lines)


def read_registry(path: Path = REGISTRY) -> list[Record]:
    records = []
    for record in ET.fromstring(preprocess(path)).findall('nml_record'):
        name = record.attrib['name']
        if 'in_subpool' in record.attrib:
            raise ValueError(f'{path}: record {name}: in_subpool is not supported')
        options = tuple(Option(option.attrib['name'], option.attrib['type'],
                               option.attrib['default_value'], name)
                        for option in record.findall('nml_option'))
        records.append(Record(name, options))
    return records


def fortran_default(option: Option) -> str:
    """The default as MPAS's generated reader has it (MPAS src/tools/registry/gen_inc.c:810-825: atof written with %lf, atoi,
    case-sensitive logicals)."""
    value = option.default
    if option.type == 'real':
        match = re.match(r'\s*([+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?)', value)
        return f'{float(match.group(1)) if match else 0.0:.6f}'
    if option.type == 'integer':
        match = re.match(r'\s*([+-]?[0-9]+)', value)
        return str(int(match.group(1))) if match else '0'
    if option.type == 'logical':
        return '.true.' if value in ('true', '.true.') else '.false.'
    if option.type == 'character':
        if "'" in value or '"' in value:
            raise ValueError(f'{option.name}: quote in character default')
        return f"'{value}'"
    raise ValueError(f'{option.name}: unsupported type {option.type}')


def fortran_type(option: Option) -> str:
    return {'real': 'real(kind=RKIND)', 'character': 'character(len=StrKIND)'}.get(option.type, option.type)


def load_table(path: Path = TABLE) -> dict:
    return json.loads(path.read_text(encoding='utf-8'))


def check_table(records: list[Record], table: dict) -> None:
    if not isinstance(table, dict) or set(table) != {'about', 'options'}:
        raise ValueError('namelist table needs only about and options')
    if not isinstance(table['about'], str) or not table['about'].strip():
        raise ValueError('namelist table needs a non-empty about string')
    rows = table['options']
    if not isinstance(rows, dict):
        raise ValueError('namelist table options must be an object')
    options = {o.name: o for record in records for o in record.options}
    for name in options:
        if name not in rows:
            raise ValueError(f'{name}: missing table row')
    keys = {
        'use': {'range'}, 'ignore': set(), 'fixed': {'values'},
        'warn': set(), 'scheme': {'values'}, 'mpas': set(),
    }
    for name, entry in rows.items():
        if name not in options:
            raise ValueError(f'{name}: not a Registry option')
        if not isinstance(entry, dict):
            raise ValueError(f'{name}: row must be an object')
        status = entry.get('status')
        if not isinstance(status, str) or status not in keys:
            raise ValueError(f'{name}: unknown status {status!r}')
        unknown = entry.keys() - {'status', 'why'} - keys[status]
        if unknown:
            raise ValueError(f'{name}: unknown key {sorted(unknown)[0]} for {status}')
        if not isinstance(entry.get('why'), str) or not entry['why'].strip():
            raise ValueError(f'{name}: missing why sentence')
        if 'range' in entry:
            # a `use` row of a real option may carry MPAS's Registry range, checked at run start
            bounds = entry['range']
            if (options[name].type != 'real' or not isinstance(bounds, list) or len(bounds) != 2
                    or any(type(b) not in (int, float) or not math.isfinite(b) for b in bounds)
                    or bounds[0] > bounds[1]):
                raise ValueError(f'{name}: range needs [low, high] numbers, low <= high, on a real option')
        if status == 'scheme' and name not in SCHEMES:
            raise ValueError(f'{name}: not one of the nine scheme options')
        if status == 'mpas' and name not in ('config_time_integration', 'config_positive_definite', 'config_epssm'):
            raise ValueError(f'{name}: not one of the three MPAS checks')
        option = options[name]
        if option.type == 'real' and status in ('fixed', 'warn') and 'values' not in entry:
            # The run's default is MPAS's %lf text; the check compares against float(that text). With
            # SINGLE_PRECISION (every lean build), a user who writes the Registry text gets the same value
            # only when the %lf text is the same decimal number.
            if float(fortran_default(option)) != float(option.default.strip()):
                raise ValueError(f'{name}: real default {option.default!r} is not exact in MPAS\'s %lf form; give values')
        if 'values' not in entry:
            continue
        values = entry['values']
        if not isinstance(values, list) or not values:
            raise ValueError(f'{name}: values must be a non-empty list')
        types = {'integer': (int,), 'real': (int, float), 'logical': (bool,), 'character': (str,)}
        for value in values:
            if type(value) not in types[options[name].type]:
                raise ValueError(f'{name}: value {value!r} is not {options[name].type}')
            if isinstance(value, float) and not math.isfinite(value):
                raise ValueError(f'{name}: value must be finite')


def allowed(option: Option, entry: dict) -> list:
    if 'values' in entry:
        return entry['values']
    default = fortran_default(option)
    if option.type == 'character':
        return [option.default]
    if option.type == 'logical':
        return [default == '.true.']
    return [{'integer': int, 'real': float}[option.type](default)]


def fortran_string(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def literal(option: Option, value) -> str:
    if option.type == 'character':
        return fortran_string(value)
    if option.type == 'logical':
        return '.true.' if value else '.false.'
    return repr(value)


def allowed_text(option: Option, entry: dict) -> str:
    if option.type == 'real' and 'values' not in entry:
        return option.default
    return ' or '.join(literal(option, value) for value in allowed(option, entry))


def markdown(records: list[Record], table: dict) -> str:
    check_table(records, table)
    lines = ['''# namelist.atmosphere

Generated by `tools/gen_namelist.py` from MPAS 8.4.2's `Registry.xml` and
`config/namelist.json`; edit the table and regenerate both outputs.

## Reading rules

The line numbers below are those of MPAS 8.4.2's `src/tools/registry/gen_inc.c`.

- **Order and rewind.** Records are read in Registry order, each after a rewind,
  starting from the Registry defaults (`gen_inc.c:855`).
- **Early end or absent record (`ierr < 0`).** Keep the values read so far and log
  every value of that record, as MPAS does (`gen_inc.c:865-899`).
- **Unknown name or bad value (`ierr > 0`).** Stop with MPAS's fatal record message;
  lean first writes the runtime's cause (`gen_inc.c:910-911`). Exception measured with
  gfortran 15.2: a bad value last before end of file, with its `/` on a later line,
  gives `ierr = -1`; the early-end rule applies, keeping the values read so far and the
  defaults for what was not read.
- **Unknown records.** The Fortran runtime skips records it does not know,
  including `&foo` and (for MPAS) `&lean`.
- **The `&lean` record.** Read last with the same rules; a missing `&lean` is silent.

Defaults follow MPAS's generated reader (`gen_inc.c:810-825`): reals are written
with C `%lf` (six decimal places), integers with `atoi`, logicals are true only for
`true` or `.true.`, and character values are verbatim. The table shows real defaults
as Registry text; checks use the same six-decimal literal as the generated default.

## Departures from MPAS

- A missing file is a CRITICAL ERROR in lean; MPAS writes an ERROR to `log.abort`.
- Lean writes `namelist read: <iomsg>` before MPAS's fatal record message.
- A file that exists but cannot be opened gets a CRITICAL ERROR with the runtime's cause.
- Validation lists every problem in one run before its final fatal count.
- Unknown values that MPAS lets through stop in lean, for example `config_horiz_mixing`.
- A missing `&lean` is silent; MPAS skips this unknown record entirely.
- MPAS expands `$` sequences in the namelist path and in the suite lines
  (`framework/mpas_log.F:973-1031`); lean writes them as given.

`fixed` and `scheme` rows reject values outside their lists; scheme checks run after
suite resolution. A `warn` row is accepted and ignored: it warns only when changed from the
Registry default and does not add a validation problem. `mpas` rows retain MPAS's own checks or warnings.

## Options

| Record | Option | MPAS default | lean_mpas |
|---|---|---|---|''']
    for record in records:
        for option in record.options:
            entry = table['options'][option.name]
            status, why = entry['status'], entry['why']
            if status == 'use':
                text = 'used'
                if 'range' in entry:
                    text += f" ({entry['range'][0]!r} to {entry['range'][1]!r})"
            elif status == 'ignore':
                text = f'accepted, no effect: {why}'
            elif status == 'fixed':
                text = f'only {allowed_text(option, entry)}: {why}'
            elif status == 'warn':
                text = f'accepted and ignored, with a warning: {why}'
            elif status == 'scheme':
                text = f'after suite resolution only {allowed_text(option, entry)}: {why}'
            else:
                text = why
            lines.append(f'| `{record.name}` | `{option.name}` | `{allowed_text(option, {})}` | {text} |')
    lines.extend(['', '## &lean', '',
                  "Optional lean-only options, read last; the defaults below are lean's, and MPAS skips this record.", '',
                  '| Record | Option | MPAS default | lean_mpas |', '|---|---|---|---|'])
    for option, why in zip(LEAN_RECORD.options, (
        "Optional executable flavor ('mpas' or 'lean'): the run stops at start when it differs from the flavor "
        'the executable was built with.',
        'Strict input checking: the albedo and meshDensity checks stop instead of warning.',
        'The memory check before the mesh is allocated: .false. turns its stops into warnings and '
        'the run goes on.',
    )):
        lines.append(f'| `lean` | `{option.name}` | `{allowed_text(option, {})}` | {why} |')
    lines.append('')
    return '\n'.join(lines)


def value_text(option: Option, exact: bool = False) -> str:
    formatter = {'real': 'exact_real_text' if exact else 'real_text', 'integer': 'int_text',
                 'logical': 'logical_text', 'character': 'trim'}[option.type]
    return f'{formatter}(cfg%{option.name})'


def checked_value_text(option: Option) -> str:
    if option.type == 'character':
        return f'''"'"//trim(cfg%{option.name})//"'"'''
    if option.type == 'logical':
        return f"trim(merge('.true. ', '.false.', cfg%{option.name}))"
    return value_text(option)


def range_check(option: Option, entry: dict) -> list[str]:
    """A `use` row with MPAS's Registry range: a value outside, or NaN, is an error (counted with the fixed ones)."""
    low, high = (literal(option, bound) for bound in entry['range'])
    name = f'cfg%{option.name}'
    tail = fortran_string(f': lean_mpas supports only {low} to {high} (the range of MPAS\'s Registry)')
    return [
        f'  if (.not. ({name} >= {low} .and. {name} <= {high})) then',
        '   call add_message(msgs, LOG_ERR, &',
        f"    '{option.name} = '// &",
        f'    {checked_value_text(option)}// &',
        f'    {tail})',
        '   n_fixed = n_fixed + 1',
        '  end if',
    ]


def option_checks(records: list[Record], table: dict) -> list[str]:
    lines = [
        ' subroutine check_options(cfg, msgs, n_fixed, n_scheme)',
        '  type(run_config), intent(in) :: cfg',
        '  type(message_list), intent(inout) :: msgs',
        '  integer, intent(inout) :: n_fixed, n_scheme',
        '',
    ]
    for record in records:
        for option in record.options:
            entry = table['options'][option.name]
            status = entry['status']
            if 'range' in entry:
                lines.extend(range_check(option, entry))
                continue
            if status not in ('fixed', 'scheme', 'warn'):
                continue
            values = ([literal(option, value) for value in entry['values']]
                      if 'values' in entry else [fortran_default(option)])
            name = f'cfg%{option.name}'
            if option.type == 'character':
                name = f'trim({name})'
            operator = '.eqv.' if option.type == 'logical' else '=='
            comparisons = [f'{name} {operator} {value}' for value in values]
            if len(comparisons) == 1:
                lines.append(f'  if (.not. ({comparisons[0]})) then')
            else:
                lines.append('  if (.not. ( &')
                for i, comparison in enumerate(comparisons):
                    suffix = ' .or. &' if i < len(comparisons) - 1 else ')) then'
                    lines.append(f'    ({comparison}){suffix}')
            level = 'LOG_WARN' if status == 'warn' else 'LOG_ERR'
            lines.extend([
                f'   call add_message(msgs, {level}, &',
                f"    '{option.name} = '// &",
                f'    {checked_value_text(option)}// &',
            ])
            if status == 'scheme':
                lines.extend([
                    "    ' (config_physics_suite = '// &",
                    '''    "'"//trim(cfg%config_physics_suite)//"'"// &''',
                ])
            if status == 'warn':
                tail = ': lean_mpas does not write this printout; the run goes on without it'
            else:
                prefix = '):' if status == 'scheme' else ':'
                tail = f'{prefix} lean_mpas supports only {allowed_text(option, entry)}'
            lines.append(f'    {fortran_string(tail)})')
            if status != 'warn':
                counter = 'n_scheme' if status == 'scheme' else 'n_fixed'
                lines.append(f'   {counter} = {counter} + 1')
            lines.append('  end if')
    lines.extend([' end subroutine', ''])
    return lines


def fortran_module(records: list[Record], table: dict | None = None) -> str:
    if table is None:
        table = load_table()
    check_table(records, table)
    checks = option_checks(records, table)
    registry_records = records
    records = [*records, LEAN_RECORD]
    lines = [
        '! Generated by tools/gen_namelist.py from MPAS 8.4.2 Registry.xml and config/namelist.json; do not edit.',
        '! Defines: ' + ', '.join(sorted(DEFINED)),
        '! Defaults and reading rules: MPAS 8.4.2 src/tools/registry/gen_inc.c:810-911.',
        'module lean_namelist_options',
        ' use mpas_kind_types, only: RKIND, StrKIND',
        ' use lean_messages, only: message_list, add_message, has_fatal, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT, &',
        '                          int_text, real_text, logical_text',
        ' implicit none',
        ' private',
        ' public :: run_config, read_records, dump_options, check_options, config_attribute, namelist_attributes',
        ' type config_attribute',
        "  character(len=64) :: name = ''",
        '  integer :: kind = 0',
        '  real(8) :: r = 0',
        '  integer :: i = 0',
        "  character(len=StrKIND) :: text = ''",
        ' end type',
        '',
        ' type run_config',
    ]
    for record in records:
        for option in record.options:
            lines.append(f'  {fortran_type(option)} :: {option.name} = {fortran_default(option)}')
    lines.extend([
        '  ! not read from the namelist: set by resolve_suite',
        '  logical :: moist_physics = .true.          ! mpas_atmphys_control.F:310-318',
        '  logical :: sfclayer_off_written = .false. ! config_sfclayer_scheme was written \'off\' (before resolve_suite sets sf_mynn for MYNN)',
        ' end type',
        '',
        'contains',
        '',
        ' subroutine read_records(nml_unit, cfg, msgs)',
        '  integer, intent(in) :: nml_unit',
        '  type(run_config), intent(inout) :: cfg',
        '  type(message_list), intent(inout) :: msgs',
        '',
        '  if (has_fatal(msgs)) return',
    ])
    for record in records:
        lines.extend([
            f'  call read_{record.name}(nml_unit, cfg, msgs)',
            '  if (has_fatal(msgs)) return',
        ])
    lines.extend([' end subroutine', ''])
    # MPAS driver/mpas_subdriver.F:532-556: Registry iteration order and YES/NO logical attributes.
    options = [o for r in registry_records for o in r.options]
    lines.extend([' subroutine namelist_attributes(cfg, attrs)',
                  '  type(run_config), intent(in) :: cfg',
                  '  type(config_attribute), allocatable, intent(out) :: attrs(:)',
                  f'  allocate(attrs({len(options)}))'])
    for i, o in enumerate(options, 1):
        kind = {'real':1, 'integer':2, 'logical':3, 'character':3}[o.type]
        member = {'real':'r', 'integer':'i', 'logical':'text', 'character':'text'}[o.type]
        value = f"merge('YES', 'NO ', cfg%{o.name})" if o.type == 'logical' else 'cfg%' + o.name
        lines.extend([f"  attrs({i})%name = '{o.name}'", f'  attrs({i})%kind = {kind}',
                      f'  attrs({i})%{member} = {value}'])
    lines.extend([' end subroutine', ''])
    for record in records:
        lines.extend([
            f' subroutine read_{record.name}(nml_unit, cfg, msgs)',
            '  integer, intent(in) :: nml_unit',
            '  type(run_config), intent(inout) :: cfg',
            '  type(message_list), intent(inout) :: msgs',
        ])
        for option in record.options:
            lines.append(f'  {fortran_type(option)} :: {option.name}')
        lines.extend([
            '  integer :: ierr',
            '  character(len=StrKIND) :: iomsg_text',
            f'  namelist /{record.name}/ &',
        ])
        for i, option in enumerate(record.options):
            lines.append('     ' + option.name + (', &' if i < len(record.options) - 1 else ''))
        for option in record.options:
            lines.append(f'  {option.name} = cfg%{option.name}')
        lines.extend([
            '  rewind(nml_unit)',
            f'  read(nml_unit, nml={record.name}, iostat=ierr, iomsg=iomsg_text)',
            '  if (ierr > 0) then',
            "   call add_message(msgs, LOG_ERR, 'namelist read: '//trim(iomsg_text))",
            f"   call add_message(msgs, LOG_CRIT, 'Error while reading namelist record {record.name}.')",
            '   return',
            '  end if',
        ])
        for option in record.options:
            lines.append(f'  cfg%{option.name} = {option.name}')
        if record != LEAN_RECORD:
            lines.extend([
                '  if (ierr < 0) then',
                '   call add_message(msgs, LOG_OUT, &',
                f"    '*** Encountered an issue while attempting to read namelist record {record.name}')",
                "   call add_message(msgs, LOG_OUT, '    The following values will be used for variables in this record:')",
                "   call add_message(msgs, LOG_OUT, ' ')",
            ])
            for option in record.options:
                lines.extend([
                    f"   call add_message(msgs, LOG_OUT, '        {option.name} = '// &",
                    f'    {value_text(option)})',
                ])
            lines.extend(["   call add_message(msgs, LOG_OUT, ' ')", '  end if'])
        lines.extend([' end subroutine', ''])
    lines.extend(checks)
    lines.extend([
        ' subroutine dump_options(unit, cfg)',
        '  integer, intent(in) :: unit',
        '  type(run_config), intent(in) :: cfg',
        '',
    ])
    for record in records:
        for option in record.options:
            lines.extend([
                f"  write(unit, '(a)') '{option.name} = '// &",
                f'   {value_text(option, exact=True)}',
            ])
    lines.extend([
        ' end subroutine',
        '',
        ' function exact_real_text(r) result(text)',
        '  real(kind=RKIND), intent(in) :: r',
        '  character(len=:), allocatable :: text',
        '  character(len=32) :: buffer',
        '',
        "  write(buffer, '(ES15.8E2)') r",
        '  text = trim(adjustl(buffer))',
        ' end function',
        '',
        'end module',
        '',
    ])
    return '\n'.join(lines)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--registry', type=Path)
    parser.add_argument('--check', action='store_true', help='report drift without writing files')
    args = parser.parse_args(argv)
    registry = args.registry if args.registry is not None else args.root / REGISTRY.relative_to(ROOT)
    if not registry.is_file():
        print(f'E_NO_REGISTRY {registry} (./lean fetch-upstream)', file=sys.stderr)
        return 2
    try:
        records = read_registry(registry)
        table = load_table(args.root / TABLE.relative_to(ROOT))
        generated = ((OUT_FORTRAN, fortran_module(records, table)), (OUT_DOCS, markdown(records, table)))
    except (OSError, ValueError, ET.ParseError) as exc:
        print(str(exc), file=sys.stderr)
        return 2
    differs = False
    for target, text in generated:
        output = args.root / target.relative_to(ROOT)
        data = text.encode('utf-8')
        if args.check:
            if not output.is_file() or output.read_bytes() != data:
                print(f'DIFFERS {output.relative_to(args.root).as_posix()}')
                differs = True
        else:
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_bytes(data)
    return 1 if differs else 0


if __name__ == '__main__':
    sys.exit(main())
