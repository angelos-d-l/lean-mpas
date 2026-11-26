#!/usr/bin/env python3
"""Writes src/run/lean_stream_registry.f90: MPAS 8.4.2's packages, fields, pools and immutable streams as Fortran
tables, so that the run needs no Registry. Read from MPAS 8.4.2's Registry.xml (reference/mpas_src, ./lean
fetch-upstream).

    python3 tools/gen_stream_registry.py [--check]

--check writes nothing and prints DIFFERS src/run/lean_stream_registry.f90 (exit 1); exit 2 without the Registry."""
from __future__ import annotations
import argparse
from pathlib import Path
import sys
from typing import NamedTuple
import xml.etree.ElementTree as ET

if __package__ in (None, ''):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from tools.gen_namelist import preprocess, REGISTRY, ROOT, fortran_string

OUT = Path('src/run/lean_stream_registry.f90')


class Field(NamedTuple):
    name: str
    packages: tuple[str, ...]
    scratch: bool
    pool: str
    dims: tuple[str, ...] = ()   # the Registry's dimensions, Time dropped (a var_array's: without the constituent index)


class Immutable(NamedTuple):
    name: str
    direction: int
    template: str
    entries: tuple[tuple[str, tuple[str, ...]], ...]


class Tables(NamedTuple):
    packages: tuple[tuple[str, str], ...]
    fields: tuple[Field, ...]
    pools: tuple[tuple[str, tuple[str, ...]], ...]
    immutable: tuple[Immutable, ...]


def packages(el):
    return tuple(el.get('packages', '').split(';')) if el.get('packages') else ()


def merge(elements):
    """MPAS src/tools/registry/gen_inc.c:2296-2509: first attributes survive; later children append."""
    merged = {}
    for el in elements:
        name = el.attrib['name']
        if name in merged:
            merged[name].extend(el)
        else:
            merged[name] = el
    return list(merged.values())


def load(registry=REGISTRY) -> Tables:
    root = ET.fromstring(preprocess(Path(registry)))
    pkgs = tuple((p.attrib['name'], p.get('active_when', '')) for p in root.findall('packages/package'))
    # push_attributes precedes the merge (gen_inc.c:2215-2293).
    for struct in root.findall('var_struct'):
        if struct.find('var_struct') is not None:
            raise ValueError('nested var_struct is not supported')
        for el in struct:
            if 'packages' not in el.attrib and 'packages' in struct.attrib:
                el.set('packages', struct.attrib['packages'])
            if el.tag == 'var_array':
                for var in el.findall('var'):
                    if 'packages' not in var.attrib and 'packages' in el.attrib:
                        var.set('packages', el.attrib['packages'])
    fields, pools = [], []
    for struct in merge(root.findall('var_struct')):
        members = merge(struct.findall('var_array')) + struct.findall('var')
        pool = struct.attrib['name']
        pools.append((pool, tuple(el.attrib['name'] for el in members)))
        for el in members:
            name = el.attrib['name']
            if len(name) > 32:
                raise ValueError(f'{name}: field name exceeds 32 characters')
            active = packages(el)
            if el.tag == 'var_array':
                # build_struct_package_lists, gen_inc.c:352-480: union in constituent order;
                # any constituent without a package makes the whole array unconditional.
                constituents = el.findall('var')
                active = tuple(dict.fromkeys(p for v in constituents for p in packages(v)))
                if not constituents or any(not packages(v) for v in constituents):
                    active = ()
            dims = tuple(d for d in el.get('dimensions', '').split() if d != 'Time')
            fields.append(Field(name, active, el.get('persistence') == 'scratch', pool, dims))
    by_field = {f.name: f for f in fields}
    by_pool = dict(pools)
    streams = merge(root.findall('streams/stream'))
    by_stream = {s.attrib['name']: s for s in streams}

    def entries_for(stream, attached=None):
        entries = []
        for tag in ('var_struct', 'var_array', 'var'):
            for el in stream.findall(tag):
                names = by_pool[el.attrib['name']] if tag == 'var_struct' else (el.attrib['name'],)
                entries.extend((n, packages(el) if attached is None else attached) for n in names)
        return entries

    immutable = []
    # gen_inc.c:2039-2205: nested streams, pools, arrays, independent variables.
    for stream in streams:
        if stream.get('immutable') != 'true':
            continue
        entries = []
        for nested in stream.findall('stream'):
            child = by_stream[nested.attrib['name']]
            if child.get('immutable') != 'true':
                raise ValueError('immutable stream contains a mutable stream')
            for tag in ('var', 'var_array', 'var_struct'):
                for el in child.findall(tag):
                    names = by_pool[el.attrib['name']] if tag == 'var_struct' else (el.attrib['name'],)
                    entries.extend((n, packages(nested)) for n in names)
        entries.extend(entries_for(stream))
        for name, _ in entries:
            if by_field[name].scratch:
                raise ValueError(f'{name}: scratch field in immutable stream')
        direction = (1 if 'input' in stream.get('type', '') else 0) + (2 if 'output' in stream.get('type', '') else 0)
        immutable.append(Immutable(stream.attrib['name'], direction or 4,
                                   stream.get('filename_template', ''), tuple(entries)))
    return Tables(pkgs, tuple(fields), tuple(pools), tuple(immutable))


def fortran_module(tables) -> str:
    t = tables
    lines = ["! Generated by tools/gen_stream_registry.py from MPAS 8.4.2 Registry.xml; do not edit.",
             '! Package inheritance and merge: MPAS src/tools/registry/gen_inc.c:352-480,2215-2509.',
             '! Pool and immutable entry order: gen_inc.c:1941-1951,2039-2205.',
             'module lean_stream_registry', ' use mpas_kind_types, only: StrKIND', ' implicit none']

    def array(name, values, kind):
        for part, first in enumerate(range(0, len(values), 100), 1):
            chunk = values[first:first+100]
            lines.append(f' {kind}, parameter :: {name}_{part}({len(chunk)}) = [ &')
            if kind.startswith('character'):
                lines.append(f'  {kind} :: &')
            for i, value in enumerate(chunk):
                lines.append('  ' + value + (', &' if i < len(chunk)-1 else ' ]'))
        chunks = ', '.join(f'{name}_{p}' for p in range(1, (len(values)+99)//100+1))
        lines.append(f' {kind}, parameter :: {name}({len(values)}) = [ &')
        parts = chunks.split(', ')
        lines.extend('  ' + part + (', &' if i < len(parts)-1 else ' ]')
                     for i, part in enumerate(parts))

    def strings(name, values, length=32):
        array(name, [fortran_string(v) for v in values], f'character(len={length})')

    def integers(name, values):
        array(name, [str(v) for v in values], 'integer')

    def mask(names):
        return sum(1 << [p[0] for p in t.packages].index(n) for n in set(names))

    for name, count in (('PACKAGES', len(t.packages)), ('FIELDS', len(t.fields)),
                        ('POOLS', len(t.pools)), ('IMMUTABLE', len(t.immutable)),
                        ('ENTRIES', sum(len(s.entries) for s in t.immutable))):
        lines.append(f' integer, parameter :: N_{name} = {count}')
    strings('PACKAGE_NAME', [p[0] for p in t.packages])
    strings('FIELD_NAME', [f.name for f in t.fields])
    integers('FIELD_PACKAGES', [mask(f.packages) for f in t.fields])
    array('FIELD_SCRATCH', ['.true.' if f.scratch else '.false.' for f in t.fields], 'logical')
    # framework/mpas_bootstrapping.F:933-1071 (get_dimlist_for_field): what "Reading dimensions from input streams" lists.
    strings('FIELD_DIMS', [' '.join(f.dims) for f in t.fields], 48)
    integers('FIELD_POOL', [[p[0] for p in t.pools].index(f.pool)+1 for f in t.fields])
    strings('POOL_NAME', [p[0] for p in t.pools])
    strings('IMMUTABLE_NAME', [s.name for s in t.immutable])
    integers('IMMUTABLE_DIRECTION', [s.direction for s in t.immutable])
    strings('IMMUTABLE_TEMPLATE', [s.template for s in t.immutable], 'StrKIND')
    first, starts = 1, []
    for s in t.immutable:
        starts.append(first)
        first += len(s.entries)
    integers('IMMUTABLE_FIRST', starts)
    integers('IMMUTABLE_COUNT', [len(s.entries) for s in t.immutable])
    integers('ENTRY_FIELD', [[f.name for f in t.fields].index(n)+1 for s in t.immutable for n, _ in s.entries])
    integers('ENTRY_PACKAGES', [mask(p) for s in t.immutable for _, p in s.entries])
    for name in ('les', 'sfclayer'):
        text = dict(t.packages)[name]
        lines.append(f' character(len=*), parameter :: {name.upper()}_WHEN = &')
        chunks = [text[i:i+70] for i in range(0, len(text), 70)]
        lines.extend('  ' + fortran_string(c) + ('// &' if i < len(chunks)-1 else '')
                     for i, c in enumerate(chunks))
    lines.extend(['contains'])
    for name, table in (('field', 'FIELD'), ('pool', 'POOL'), ('package', 'PACKAGE')):
        count = {'field': 'FIELDS', 'pool': 'POOLS', 'package': 'PACKAGES'}[name]
        lines.extend([f' integer function {name}_index(name) result(idx)',
                      '  character(len=*), intent(in) :: name', '  integer :: i', '  idx = 0',
                      f'  do i = 1, N_{count}', f'   if ({table}_NAME(i) /= name) cycle',
                      '   idx = i', '   return', '  end do', ' end function'])
    lines.extend(['end module', ''])
    return '\n'.join(lines)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--registry', type=Path)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args(argv)
    registry = args.registry if args.registry is not None else args.root / REGISTRY.relative_to(ROOT)
    if not registry.is_file():
        print(f'E_NO_REGISTRY {registry} (./lean fetch-upstream)', file=sys.stderr)
        return 2
    try:
        data = fortran_module(load(registry)).encode('utf-8')
    except (OSError, ValueError, KeyError, ET.ParseError) as exc:
        print(str(exc), file=sys.stderr)
        return 2
    output = args.root / OUT
    if args.check:
        if not output.is_file() or output.read_bytes() != data:
            print(f'DIFFERS {OUT.as_posix()}')
            return 1
    else:
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_bytes(data)
    return 0


if __name__ == '__main__':
    sys.exit(main())
