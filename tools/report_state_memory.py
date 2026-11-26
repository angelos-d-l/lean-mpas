"""Compute fixed array payload by field group; not RSS or a timing result.

The target stores default real/integer/logical and kind_noahmp in four bytes, the SINGLE_PRECISION configuration of
every lean build. Each field is listed so future additions cannot silently disappear in totals. The count is checked
against what gfortran allocates for the same types (a small program compiled in build/state-memory-check/), and
written to build/state-memory.json.

    python3 tools/report_state_memory.py   -> build/state-memory.json, for the cases/athens_3km mesh (46251 cells)

LEAN_LEVELS=N counts N levels instead of 45; LEAN_NOAHMP_SWSPLIT=1 counts the fields of that switch. gfortran must
be on PATH.
"""
from pathlib import Path
import argparse
import re
import json
import math
import subprocess
import os
from switch_merge import resolve

argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter).parse_args()
root = Path(__file__).resolve().parents[1]
# the mesh of cases/athens_3km, with 45 levels unless LEAN_LEVELS gives another count
sizes = dict(ncells=46251, nz=int(os.environ.get('LEAN_LEVELS', '45')), nedges=139361, nvertices=93111, maxedges=8)
groups = {}


def add(group, name, dims):
    shape = []
    for dim in dims.split(',') if dims else []:
        bounds = dim.split(':')
        values = [eval(x, {'__builtins__': {}}, sizes) for x in bounds]
        shape.append(values[0] if len(values) == 1 else values[1] - values[0] + 1)
    groups.setdefault(group, {})[name] = 4 * math.prod(shape)


fixed_members = {}
allocatable = {}   # type name -> its members, for a type with allocatable members


def fields(path, typename):
    whole = (root / path).read_text()
    text = whole.split(' type ' + typename + '\n', 1)[1].split(' end type', 1)[0]
    # a type with allocatable members: the shapes are those of its allocate_* routine, allocate(s%name(shape))
    shapes = {}
    if 'allocatable' in text:
        allocator = {'state_type': 'state', 'mesh_type': 'mesh', 'noahmp_state_type': 'land',
                     'gf_state_type': 'gf', 'gwdo_state_type': 'gwdo'}[typename]
        routine = re.search(r' subroutine allocate_' + allocator + r'\((\w+)\)', whole)
        a = routine.start()
        shapes = dict(re.findall(r'allocate\(' + routine[1] + r'%(\w+)\(([^()]*)\)\)',
                                 whole[a:whole.index(' end subroutine', a)]))
        allocatable[typename] = []
        fixed_members[typename] = []
    # the optional Noah-MP fields (src/noahmp_state.f90) count in a build made with LEAN_NOAHMP_SWSPLIT only
    text = '\n'.join(resolve(text.split('\n'), 'LEAN_NOAHMP_SWSPLIT', os.environ.get('LEAN_NOAHMP_SWSPLIT') == '1'))
    for line in text.splitlines():
        if '::' not in line:
            continue
        assert re.match(r'\s*(real(?:\(kind=kind_noahmp\))?|integer|logical)\s*(,\s*allocatable\s*)?::', line), line
        decl = line.split('::')[1].strip()
        for match in re.finditer(r'(\w+)\s*(?:\(([^)]*)\))?\s*(?:,|$)', decl):
            name, dims = match.group(1), match.group(2) or ''
            if name in shapes:
                allocatable[typename].append(name)
            elif typename in fixed_members:
                fixed_members[typename].append((name, bool(dims)))
            yield name, shapes.get(name, dims)


pbl = set('qke tsq qsq cov el_pbl sh sm qc_bl qi_bl cldfra_bl exch_h exch_m maxwidth maxmf ztop_plume kpbl ktop_plume pblh regime'.split())
atmos = set('u v rho_moist p_hyd psfc qv qc qi qr qs qg ni nr theta p exner dz w'.split())
micro = set('rainprod evapprod re_cloud re_ice re_snow nt mu rainnc rainncv snownc snowncv graupelnc graupelncv sr'.split())
for name, dims in fields('src/state.f90', 'state_type'):
    group = ('tendencies' if name.startswith('t_') else 'PBL moments and diagnostics' if name in pbl
             else 'atmospheric state and forcing' if name in atmos else 'Thompson diagnostics' if name in micro
             else 'step counter' if name == 'timestep' else 'surface layer state and forcing')
    add('atmosphere/' + group, name, dims)
for name, dims in fields('src/noahmp_state.f90', 'noahmp_state_type'):
    prefix = '_'.join(name.split('_')[:2]) if '_' in name else name
    if name.startswith('forcing_'):
        prefix = 'forcing'
    if name in {'net_shortwave', 'rain_convective', 'surface_albedo', 'sw_frac_dir_vis', 'sw_frac_dir_nir', 'sw_frac_dif_vis'}:
        prefix = 'coupling boundary'
    add('Noah-MP/' + prefix, name, dims)
for name,dims in fields('src/gf_state.f90','gf_state_type'):
    group=('external forcing' if name in ['area','terrain','surface_pressure','shortwave','dynamics_theta','dynamics_qv','radiation_theta']
           else 'tendencies' if name in ['rthcuten','rqvcuten','rqccuten','rqicuten']
           else 'in-cloud diagnostics' if name in ['qc_cu','qi_cu'] else 'mass flux, levels and precipitation')
    add('GF/'+group,name,dims)
for name,dims in fields('src/gwdo_state.f90','gwdo_state_type'):
    add('GWDO/geometry and diagnostics',name,dims)
for name,dims in fields('src/state.f90','mesh_type'):
    add('mesh/metadata outside physics state',name,dims)
report = {'basis': 'fixed domain array payload, 4 bytes per real/integer/logical; excludes views, stack, compiler padding, tables and IO fixtures',
          'groups': {g: {'bytes': sum(f.values()), 'MiB': sum(f.values()) / 2**20, 'fields': f} for g, f in groups.items()}}
report['total_bytes'] = sum(g['bytes'] for g in report['groups'].values())
report['physics_state_bytes']=sum(g['bytes'] for k,g in report['groups'].items() if not k.startswith('mesh/'))
# The expected Noah-MP payload in bytes of this mesh: 123027660 (2660 bytes a cell; Noah-MP has no atmospheric
# levels, so LEAN_LEVELS does not change it) plus 2 more per-cell fields, 5 with LEAN_NOAHMP_SWSPLIT. A change to
# the Noah-MP members must update these numbers.
assert sum(g['bytes'] for k, g in report['groups'].items() if k.startswith('Noah-MP/')) == 123027660 + 4*sizes['ncells']*(2 + 3*(os.environ.get('LEAN_NOAHMP_SWSPLIT') == '1'))


def measure(var, typename, short):
    """Fortran that prints the bytes of an object: its storage size, or for a type with allocatable members the
    sizes of the members after its allocate_* routine"""
    if typename not in allocatable:
        return 'print *,storage_size(%s,kind=8)/8' % var
    terms = ['size(%s%%%s,kind=8)' % (var, name) for name in allocatable[typename]]
    lines = ['call allocate_%s(%s)' % (short, var), 'bytes=0']
    for name, array in fixed_members[typename]:
        field = var + '%' + name
        count = '*size(%s,kind=8)' % field if array else ''
        lines.append('bytes=bytes+storage_size(%s,kind=8)/8%s' % (field, count))
    lines += ['bytes=bytes+4*(' + '+'.join(terms[i:i + 4]) + ')' for i in range(0, len(terms), 4)]
    return '\n'.join(lines + ['print *,bytes'])


work = root / 'build/state-memory-check'
work.mkdir(parents=True, exist_ok=True)
source = work / 'measure.f90'
exe = work / 'measure'
source.write_text('''program measure
use config,only: set_mesh_sizes
use model_state,only: state_type,mesh_type,allocate_state,allocate_mesh
use noahmp_state
use gf_state
use gwdo_state
implicit none
integer(kind=8) :: bytes
type(state_type),save :: s
type(noahmp_state_type),save :: land
type(gf_state_type),save :: gf
type(gwdo_state_type),save :: drag
type(mesh_type),save :: mesh
MEASURE
end program
'''.replace('MEASURE', 'call set_mesh_sizes(%d,%d,%d,%d)\n' % (sizes['ncells'],sizes['nedges'],sizes['nvertices'],sizes['nz']) + '\n'.join(measure(*v) for v in [('s', 'state_type', 'state'), ('land', 'noahmp_state_type', 'land'),
                                                         ('gf', 'gf_state_type', 'gf'), ('drag', 'gwdo_state_type', 'gwdo'),
                                                         ('mesh', 'mesh_type', 'mesh')])))
try:
    sources = ['reference/mpas_src/src/framework/mpas_kind_types.F',
               'reference/mpas_src/src/core_atmosphere/physics/physics_noahmp/utility/Machine.F90',
               'src/levels.f90', 'src/run_settings.f90', 'src/config.f90', 'src/winds.f90', 'src/state.f90', 'src/noahmp_state.f90','src/gf_state.f90','src/gwdo_state.f90']
    subprocess.run(['gfortran', '-cpp', '-DSINGLE_PRECISION', '-ffree-form',
                    '-ffree-line-length-none', *['-D'+s for s in ('LEAN_NOAHMP_SWSPLIT',) if os.environ.get(s) == '1'], '-c', *[str(root / p) for p in sources]], cwd=work, check=True)
    subprocess.run(['gfortran', *map(str, work.glob('*.o')), '-I' + str(work),
                    str(source), '-o', str(exe)], check=True)
    measured = list(map(int, subprocess.check_output([str(exe)], text=True).split()))
    expected = [sum(g['bytes'] for k, g in report['groups'].items() if k.startswith(prefix))
                for prefix in ['atmosphere/', 'Noah-MP/','GF/','GWDO/','mesh/']]
    assert measured == expected, (measured, expected)
    report['compiler_storage_bytes'] = dict(zip(['atmosphere', 'Noah-MP','GF','GWDO','mesh'], measured))
finally:
    for path in work.iterdir():
        path.unlink()
    work.rmdir()
out = root / 'build/state-memory.json'
out.parent.mkdir(exist_ok=True)
out.write_text(json.dumps(report, indent=2) + '\n')
for group, item in report['groups'].items():
    print(f"{group}: {item['bytes']} bytes ({item['MiB']:.6f} MiB)")
print('physics state payload:',report['physics_state_bytes'],'bytes; with separate mesh:',report['total_bytes'],'bytes')
