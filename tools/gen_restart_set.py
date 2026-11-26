"""Writes the restart layout (src/io/lean_restart_layout.f90), the host walk of every restart member
(src/io/lean_restart_set.f90) and the restart inventory (docs/restart.md) from config/restart.json, the state
declarations in src/, the field catalog (src/run/lean_field_catalog.f90), MPAS 8.4.2's NoahmpIOVarType.F90
(third_party/) and the invariant stream of MPAS 8.4.2's Registry.xml (reference/mpas_src, ./lean fetch-upstream).

    python3 tools/gen_restart_set.py [--check]

--check writes nothing and prints DIFFERS <path> for each file that differs (exit 1); exit 2 without the Registry."""
from hashlib import sha256
import json
from pathlib import Path
import re
import sys

from gen_phys_state import TYPES, fields, parser

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / 'src'
NOAH = ROOT / 'third_party/mpas-8.4.2/src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpIOVarType.F90'
OBJECTS = [
    ('state', 'phys', 'state', 'state_type', 'allocate_state', 's'),
    ('lands', 'land', 'noahmp_state', 'noahmp_state_type', 'allocate_land', 's'),
    ('convection', 'gf', 'gf_state', 'gf_state_type', 'allocate_gf', 's'),
    ('drag', 'gwdo', 'gwdo_state', 'gwdo_state_type', 'allocate_gwdo', 's'),
    ('f', 'dyn', 'dyn_step', 'dyn_fields', 'allocate_fields', 'f'),
    ('mp', 'micro', 'dyn_micro', 'micro_fields', 'allocate_micro', 'mp'),
]
DIMS = dict(ncells='nCells', nedges='nEdges', nvertices='nVertices', nz='nVertLevels',
            nz1='nVertLevelsP1', num_soils='nSoilLevels', maxedges='maxEdges',
            nc1='lean_nCellsP1', ne1='lean_nEdgesP1', nv1='lean_nVerticesP1', num_scalars='lean_nScalars')
DIMS['nz+1'] = DIMS['nz1']
GHOST = {'lean_nCellsP1': ('nCells', 'ncells', 'nc1'),
         'lean_nEdgesP1': ('nEdges', 'nedges', 'ne1'),
         'lean_nVerticesP1': ('nVertices', 'nvertices', 'nv1')}


def catalog_names():
    source = (SRC / 'run/lean_field_catalog.f90').read_text()
    blocks = re.findall(r':: VAR_NAME_\d+\(\d+\) = \[ &(.*?)\]', source, re.S)
    return set(re.findall(r"'([^']+)'", ''.join(blocks)))


def dimension(extent):
    extent = extent.strip()
    if extent in DIMS:
        return DIMS[extent]
    if ':' in extent:
        lo, hi = map(int, extent.split(':'))
        extent = str(hi - lo + 1)
    return 'lean_n' + str(int(extent))


def allocations(path, subroutine, obj):
    body = (SRC / path).read_text().split(' subroutine ' + subroutine + '(')[1].split(' end subroutine')[0]
    return dict(re.findall(r'\b' + obj + r'%(\w+)\(([^()]*)\)', body))


def object_rows(config):
    for obj, prefix, mod, type_name, routine, local in OBJECTS:
        if obj in ('f', 'mp'):
            members = parser.fields(SRC / (mod + '.f90'), type_name)
        else:
            short = next(short for short, file, _, _ in TYPES if file == mod)
            members = fields(short)
        shapes = allocations(mod + '.f90', routine, local)
        guard = ''
        for field in members:
            if isinstance(field, str):
                if field.startswith('#ifdef '):
                    assert not guard, field
                    guard = field.split()[1]
                else:
                    assert field == '#endif' and guard, field
                    guard = ''
                continue
            name = field['name']
            if obj == 'mp' and (name == 'sum_pw' or name.startswith('film_')):
                continue
            kind = field['type'].split('(')[0].strip()
            dims = ['nSoilLevels' if obj == 'lands' and d.strip() == '1:4' else dimension(d)
                    for d in shapes[name].split(',')] if field['rank'] else []
            assert len(dims) == field['rank'], (name, dims, field)
            member = obj + '%' + name
            expr = {'f': 'fc%', 'mp': 'cmp%'}.get(obj, obj + '%') + name
            mpas = config['mpas'].get(member, [])
            for j, ncname in enumerate(mpas or [f'lean_{prefix}_{name}']):
                section = expr if len(mpas) < 2 else f'{expr}({j+1},:,:)'
                yield dict(name=ncname, member=member, expr=section, dims=dims if len(mpas) < 2 else dims[1:],
                           kind=kind, mpas_name=bool(mpas), guard=guard, primary=j == 0,
                           condition='noahmp_opt_sfc == 2' if name in parser.SFC2_FIELDS else '')


def glacier_members():
    body = NOAH.read_text().split('type, public :: NoahmpIO_type')[1].split('end type')[0]
    declaration = re.compile(r'^\s*(real\(kind=\w+\)|integer)\s*,\s*allocatable\s*,\s*dimension\(([: ,]+)\)\s*::\s*(\w+)', re.I)
    rows, guard = [], ''
    for line in body.splitlines():
        if line.startswith('#ifdef '):
            guard = line.split()[1]
        elif line.startswith('#endif'):
            guard = ''
        m = declaration.match(line)
        if m:
            rows.append(dict(name=m[3].lower(), kind=m[1].split('(')[0].lower(), rank=m[2].count(':'), guard=guard))
    return rows


def exchange_members():
    body = (SRC / 'land_ice_exchange.f90').read_text().split(' type glacier_exchange\n')[1].split(' end type')[0]
    for line in body.splitlines():
        kind, names = line.strip().split(' :: ')
        for decl in parser.dg.split_top(names):
            m = re.fullmatch(r'\s*(\w+)(?:\(([^)]+)\))?\s*', decl)
            assert m, decl
            yield dict(name=m[1], kind=kind, extent=m[2] or '')


def inventory(config):
    rows = list(object_rows(config))
    rows += [{'mpas_name': False, **row} for row in config['extras']]
    shapes = allocations('sea_ice.f90', 'allocate_ice', 'ice')
    for field in parser.fields(SRC / 'sea_ice.f90', 'ice_cells'):
        name = field['name']
        rows.append(dict(name='lean_seaice_' + name, member='seaice%' + name, kind='real',
                         dims=[dimension(d) for d in shapes[name].split(',')], mpas_name=False))
    result = []
    for row in rows:
        row = dict(row)
        row.setdefault('expr', row['member'])
        dims = row['dims']
        axes = [j for j, d in enumerate(dims) if d in GHOST]
        if row['mpas_name'] and axes:
            assert len(axes) == 1 and axes[0] == len(dims)-1, row
            j = axes[0]
            owned_dim, owned_n, ghost_n = GHOST[dims[j]]
            target = row['expr']
            # Split the final dimension, including the already sliced moisture species.
            if target.endswith(')'):
                owned = target[:-2] + ': ' + owned_n + ')'
                ghost = target[:-2] + ghost_n + ')'
            else:
                args = ':,' * j
                owned = f'{target}({args}:{owned_n})'
                ghost = f'{target}({args}{ghost_n})'
            result.append(dict(row, dims=dims[:j] + [owned_dim], expr=owned, ghost=j+1))
            result.append(dict(row, dims=dims[:j], expr=ghost, name='lean_ghost_' + row['name'],
                               mpas_name=False, primary=False))
        else:
            result.append(row)
    names = [row['name'] for row in result]
    assert len(names) == len(set(names)), 'duplicate restart variable'
    assert {r['name'] for r in result if r['mpas_name']} <= catalog_names()
    return result


def layout_hash(rows):
    variables = [(r['name'], r['kind'], r['dims'], r.get('guard'), r.get('condition'), r.get('static', False)) for r in rows]
    data = json.dumps([variables, glacier_members(), list(exchange_members())], separators=(',', ':'), sort_keys=True)
    return sha256(data.encode()).hexdigest()[:16]


def copies():
    registry = (ROOT / 'reference/mpas_src/src/core_atmosphere/Registry.xml').read_text()
    invariant = re.search(r'<stream\s+name="invariant"(.*?)</stream>', registry, re.S)[1]
    # ivgtyp and isltyp are restart rows (the surface update changes them), not copies
    held = ('ivgtyp', 'isltyp')
    return [n for n in re.findall(r'<var\s+name="(\w+)"', invariant) if n not in held] + ['u_init', 'v_init']


def render_calls(rows):
    lines = []
    for row in rows:
        guard, condition = row.get('guard'), row.get('condition')
        if row.get('optional'):
            condition = 'allocated(' + row['expr'] + ')'
        if guard:
            lines.append('#ifdef ' + guard)
        expr = row['expr']
        if row['member'].startswith('rf%'):
            expr = 'saved_' + row['member'][3:]
        diagnostic = row.get('optional_diag', False)
        if diagnostic:
            lines.append(' rf%optional_diag=.true.')
        if condition:
            lines.append(' if (' + condition + ') &')
        call = 'restart_static' if row.get('static') else 'restart_var'
        lines.append(f" {' ' if condition else ''}call {call}(rf, '{row['name']}', &")
        lines.append(f"  {expr}, '{' '.join(row['dims'])}')")
        if diagnostic:
            lines.append(' rf%optional_diag=.false.')
        if guard:
            lines.append('#endif')
    return lines


def glacier_routines():
    out = [' subroutine glacier_words(io, nr, ni)',
           ' type(NoahmpIO_type), intent(in) :: io',
           ' integer, intent(out) :: nr, ni', ' nr=0', ' ni=0']
    for f in glacier_members():
        count = 'nr' if f['kind'] == 'real' else 'ni'
        name = 'io%' + f['name']
        if f['guard']:
            out.append('#ifdef ' + f['guard'])
        out.append(f' if (allocated({name})) {count}={count}+size({name})')
        if f['guard']:
            out.append('#endif')
    out += [' end subroutine', '', ' subroutine glacier_column(io, r, i, unpack)',
            ' type(NoahmpIO_type), intent(inout) :: io',
            ' real, target, intent(inout) :: r(:)',
            ' integer, target, intent(inout) :: i(:)',
            ' logical, intent(in) :: unpack',
            ' integer :: nr, ni, n', ' nr=0', ' ni=0']
    for f in glacier_members():
        buffer, count = ('r', 'nr') if f['kind'] == 'real' else ('i', 'ni')
        name = 'io%' + f['name']
        section = f'{buffer}({count}+1:{count}+n)'
        packing, unpacking = name, section
        if f['rank'] > 1:
            packing = f'reshape({name},[n])'
            unpacking = f'reshape({section},shape({name}))'
        if f['guard']:
            out.append('#ifdef ' + f['guard'])
        out += [f' if (allocated({name})) then', f'  n=size({name})', '  if (unpack) then',
                f'   {name}={unpacking}', '  else', f'   {section}={packing}', '  end if',
                f'  {count}={count}+n', ' end if']
        if f['guard']:
            out.append('#endif')
    out += [' end subroutine', '', ' subroutine exchange_column(x, values, unpack)',
            ' type(glacier_exchange), intent(inout) :: x', ' real, intent(inout) :: values(:)',
            ' logical, intent(in) :: unpack', ' integer :: n', ' n=0']
    for f in exchange_members():
        name = 'x%' + f['name']
        extent = f['extent'] or '1'
        slot = f'values(n+1:n+{extent})' if f['extent'] else 'values(n+1)'
        to, back = (f'transfer({name},0.)', f'transfer({slot},0)') if f['kind'] == 'integer' else (name, slot)
        out += [' if (unpack) then', f'  {name}={back}', ' else', f'  {slot}={to}', ' end if', f' n=n+{extent}']
    out.append(' end subroutine')
    return out


def walk(rows):
    axes = [f['extent'] for f in exchange_members()]
    exchange_words = '+'.join([str(axes.count(''))] + [a for a in axes if a])
    extras = next(j for j,r in enumerate(rows) if r['name']=='lean_glacier_real')
    end = extras + 3
    lines = ['! Generated by tools/gen_restart_set.py from config/restart.json, the state declarations in src/, MPAS 8.4.2 Registry.xml and NoahmpIOVarType.F90. Do not edit.',
             'module lean_restart_set',
             ' use config, only: ncells, nedges, nvertices, nc1, ne1, nv1, max_levels',
             ' use model_data, only: fc, cmp, rad, rad_heating, emiss, ice, snow, albedo, s',
             ' use model_state, only: state_type', ' use noahmp_state, only: noahmp_state_type, noahmp_opt_sfc',
             ' use gf_state, only: gf_state_type', ' use gwdo_state, only: gwdo_state_type',
             ' use sea_ice, only: seaice => ice, any_sea_ice',
             ' use surface_bdy, only: xicem, ivgtyp_now, isltyp_now',
             ' use land_ice, only: glacier, glacier_of, n_glacier, exchange, glacier_exchange',
             ' use NoahmpIOVarType, only: NoahmpIO_type',
             ' use lean_fields, only: sst, vegetation, cf_time, smois_first',
             ' use diag_manager, only: rainc, acsnow, uh_max, w_max, wsp_max, relhum, refl10cm, &',
             '  refl10cm_max, refl10cm_1km, refl10cm_1km_max',
             ' use lean_writer, only: restart_file, restart_var, restart_static, RESTART_GET, RESTART_PUT',
              ' implicit none', ' private', ' public :: restart_walk, glacier_words, glacier_column, restart_mesh_vars',
             f' character(len=32), parameter :: restart_mesh_vars({len(copies())}) = [character(len=32) :: &',
             *[f"  '{name}', &" for name in copies()[:-1]], f"  '{copies()[-1]}' ]",
             'contains',
             ' subroutine restart_walk(rf, state, lands, convection, drag)',
             ' type(restart_file), intent(inout) :: rf', ' type(state_type), intent(inout) :: state',
             ' type(noahmp_state_type), intent(inout) :: lands', ' type(gf_state_type), intent(inout) :: convection',
             ' type(gwdo_state_type), intent(inout) :: drag',
             ' real, allocatable :: glacier_real(:,:), exchange_real(:,:)',
             ' integer, allocatable :: glacier_int(:,:)',
             ' integer :: nr, ni, j',
             ' character(len=64) :: saved_xtime, saved_initial_time, saved_cold_start, saved_alarms(5)',
             '#ifdef LEAN_BUDGET',
             ' real, allocatable :: flux(:,:)',
             ' allocate(flux(8,ncells))',
             ' if (rf%mode == RESTART_PUT) then',
             '  do j=1,ncells', '   flux(:,j)=rad(j)%flux', '  end do', ' end if',
             '#endif',
             ' saved_xtime=rf%xtime',
             ' saved_initial_time=rf%initial_time',
             ' saved_cold_start=rf%cold_start',
             ' saved_alarms=rf%alarms']
    lines += render_calls(rows[:extras])
    lines += [' if (n_glacier > 0 .and. .not.rf%failed) then',
              '  call glacier_words(glacier(1),nr,ni)',
              '  allocate(glacier_real(nr,n_glacier),glacier_int(ni,n_glacier))',
              '  allocate(exchange_real(' + exchange_words + ',n_glacier))',
              '  if (rf%mode == RESTART_PUT) then', '   do j=1,n_glacier',
              '    call glacier_column(glacier(j),glacier_real(:,j),glacier_int(:,j),.false.)',
              '    call exchange_column(exchange(j),exchange_real(:,j),.false.)', '   end do', '  end if']
    lines += render_calls(rows[extras:end])
    lines += ['  if (rf%mode == RESTART_GET .and. .not.rf%failed) then', '   do j=1,n_glacier',
              '    call glacier_column(glacier(j),glacier_real(:,j),glacier_int(:,j),.true.)',
              '    call exchange_column(exchange(j),exchange_real(:,j),.true.)', '   end do', '  end if', ' end if']
    lines += render_calls(rows[end:])
    lines += [' if (rf%mode == RESTART_GET .and. .not.rf%failed) then',
              '  rf%xtime=saved_xtime', '  rf%initial_time=saved_initial_time',
              '  rf%cold_start=saved_cold_start', '  rf%alarms=saved_alarms', ' end if',
              '#ifdef LEAN_BUDGET', ' if (rf%mode == RESTART_GET .and. .not.rf%failed) then',
              '  do j=1,ncells', '   rad(j)%flux=flux(:,j)', '  end do', ' end if', '#endif',
              ' end subroutine', '']
    return '\n'.join(lines + glacier_routines() + ['end module', ''])


def markdown(rows):
    lines = ['# Restart a forecast', '',
             'For a limited-area restart between LBC records, set `reference_time` on `lbc_in`',
             "to the original run's LBC schedule anchor. For example, a forecast that started at",
             '2026-11-08_00:00:00 with 3-hourly LBC files and restarts at 06:10 needs',
             '`reference_time="2026-11-08_00:00:00"`. Without it the model looks for an LBC file at 06:10 and stops.', '',
             '1. In the cold run, give the `restart` stream an `output_interval` that reaches the split.',
             '2. In a fresh run directory with the same inputs and tables, link or copy the desired',
             '   restart file. Set `config_do_restart = .true.` in `&restart`.',
             '3. Set `config_start_time` to the time saved in that file, or to `file` and copy',
             '   `restart_timestamp` into the run directory. Set the remaining run duration.',
             '4. Use a lean_mpas built with the same flavor and switches: a restart file written with a different',
             '   restart inventory (RESTART_LAYOUT) is refused. Start it from inside that run directory.', '',
             'lean_mpas resumes only from its own NetCDF restart files and refuses MPAS restart files.',
             'Keep the original `reference_time` on `output` and `diagnostics` if you want their',
             'original schedule at an off-cycle split. Standard history and diagnostics start writing',
             'after the split. Restart files and the timestamp are published after a successful write.', '',
             '## Restart inventory', '', 'Generated by `tools/gen_restart_set.py`.', '',
             'Dimensions are listed fastest first. Every carried variable has a Time record.',
             'The copied inputs retain the dimensions and types of the source file.', '',
             '| File name | Source | Dimensions | Named by |',
             '|---|---|---|---|']
    for row in rows:
        source = row['member']
        guard = row.get('guard') or row.get('condition')
        if guard:
            source += ' (' + guard + ')'
        label = 'MPAS' if row['mpas_name'] else 'lean'
        dims = ' '.join(row['dims'])
        lines.append(f"| `{row['name']}` | `{source}` | {dims} | {label} |")
    lines += ['', '## Copied input fields', '',
              'The restart copies these fields when present in the mesh source, from MPAS',
              '`core_atmosphere/Registry.xml:499-600`, plus the initial wind profiles.', '']
    lines += ['- `' + name + '`' for name in copies()]
    lines += ['', 'It also copies the held fields listed in `src/lean_fields.f90`.', '',
              '## Packed glacier members', '',
              '| Member | Kind | Rank |', '|---|---|---|']
    lines += [f"| `{f['name']}` | {f['kind']} | {f['rank']} |" for f in glacier_members()]
    return '\n'.join(lines) + '\n'


def main():
    registry = ROOT / 'reference/mpas_src/src/core_atmosphere/Registry.xml'
    if not registry.is_file():
        print(f'E_NO_REGISTRY {registry} (./lean fetch-upstream)', file=sys.stderr)
        return 2
    rows = inventory(json.loads((ROOT / 'config/restart.json').read_text()))
    layout = ('! Generated by tools/gen_restart_set.py from config/restart.json, the state declarations in src/ and MPAS 8.4.2 NoahmpIOVarType.F90. Do not edit.\n'
              '! RESTART_LAYOUT names the restart inventory; a restart file with another layout is refused.\n'
              'module lean_restart_layout\n'
              f" character(len=16), parameter :: RESTART_LAYOUT = '{layout_hash(rows)}'\nend module\n")
    failed = False
    for name, content in [('src/io/lean_restart_layout.f90', layout),
                          ('src/io/lean_restart_set.f90', walk(rows)), ('docs/restart.md', markdown(rows))]:
        path = ROOT / name
        if '--check' in sys.argv:
            if not path.exists() or path.read_text() != content:
                print('DIFFERS ' + name)
                failed = True
        else:
            path.write_text(content)
    return int(failed)


if __name__ == '__main__':
    sys.exit(main())
