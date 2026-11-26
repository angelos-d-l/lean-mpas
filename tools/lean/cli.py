"""The ./lean command line: one subcommand per job, a verdict line, and the exit status.

    ./lean doctor [--target cpu|gpu] [--fc gfortran|nvfortran|ifx] [--json]
                  [--init FILE] [--namelist FILE] [--streams FILE] [--run-dir DIR] [--build DIR]
    ./lean build [--target cpu|gpu] [--fc gfortran|nvfortran|ifx] [--profile P] [--variant V] [--omp]
                 [--flavor mpas|lean] [--switch NAME=on|off|N] [--levels N]
                 [--jobs N] [--dry-run] [--json] [--compiler PATH] [--link NAME] [--gpu-cc N]
    ./lean tables --from PATH|URL | --build
    ./lean fetch-upstream
    ./lean generate [--check]

Exit status: 0 for OK, 1 for FAIL, 2 for bad usage.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

from tools.lean.config import COMPILERS, FLAVORS, PROFILES, TARGETS, BuildConfig, load_lock, load_presets

DEFAULT_FC = {'cpu': 'gfortran', 'gpu': 'nvfortran'}


def _positive(text: str) -> int:
    value = int(text)
    if value < 1:
        raise argparse.ArgumentTypeError('must be 1 or more')
    return value


def parser() -> argparse.ArgumentParser:
    top = argparse.ArgumentParser(
        prog='./lean', description='Build and check lean_mpas.')
    commands = top.add_subparsers(dest='command', required=True, metavar='command')
    doctor = commands.add_parser(
        'doctor', help='check the tools, their versions and the Thompson tables',
        description='Check the compiler, CMake, Ninja, Python, git, netCDF-C (and for gpu NVHPC, CUDA and the card) and the '
                    'Thompson tables: one line per requirement, then DOCTOR OK or DOCTOR FAIL: <n> missing.')
    doctor.add_argument('--target', choices=TARGETS, default='cpu')
    doctor.add_argument('--fc', choices=COMPILERS, help='default: gfortran for cpu, nvfortran for gpu')
    doctor.add_argument('--json', action='store_true', help='print the checks as JSON')
    memory = doctor.add_argument_group('the memory sum')
    memory.add_argument('--init', type=Path, metavar='FILE', help='the init (mesh) file of the run')
    memory.add_argument('--namelist', type=Path, metavar='FILE', help='its namelist.atmosphere')
    memory.add_argument('--streams', type=Path, metavar='FILE', help='its streams.atmosphere')
    memory.add_argument('--run-dir', type=Path, metavar='DIR', help='an MPAS run directory: its init, namelist, streams '
                                                                     'and the tables startup opens')
    memory.add_argument('--build', type=Path, metavar='DIR', help="a build directory: the sum of its own executable "
                                                                   "(otherwise an estimate)")
    build = commands.add_parser(
        'build', help='configure with CMake and build lean_mpas with Ninja into build/<name>/',
        description='Configure with CMake and build lean_mpas with Ninja into build/<name>/; writes build.json.')
    build.add_argument('--flavor', choices=FLAVORS, help='physics switch set (default mpas; config/presets.json)')
    build.add_argument('--target', choices=TARGETS, help='default: cpu')
    build.add_argument('--fc', choices=COMPILERS, help='default: gfortran for cpu, nvfortran for gpu')
    build.add_argument('--profile', choices=PROFILES,
                       help='strict (value-safe, the default), debug (run-time checks), fast (the CPU speed build) '
                            'or envelope (plain -O2/-O3 builds whose forecast spread bounds what a faster build may '
                            'change); fast and envelope need --variant (config/flags.json)')
    build.add_argument('--variant', help='a variant of the fast or envelope profile, as named in config/flags.json '
                                         '<fc>.variants')
    build.add_argument('--omp', action='store_true', help='OpenMP host threads')
    build.add_argument('--switch', action='append', default=[], metavar='NAME=on|off|N',
                       help='explicit build switch (repeatable; config/switches.json)')
    build.add_argument('--levels', type=_positive, metavar='N',
                       help='fix the number of vertical levels at compile time: faster loops over levels, and the '
                            'executable runs only meshes with nVertLevels = N (default: any count, from the mesh)')
    build.add_argument('--jobs', type=_positive, default=2, help='parallel compiles (default 2)')
    build.add_argument('--dry-run', action='store_true', help='configure, print the compile commands, build nothing')
    build.add_argument('--json', action='store_true', help='print the result as JSON')
    build.add_argument('--compiler', metavar='PATH', help='the compiler to use instead of the first --fc on PATH')
    build.add_argument('--link', metavar='NAME', help='after a successful build, link build/NAME to it')
    build.add_argument('--gpu-cc', type=int, metavar='N',
                       help="the card's compute capability for -gpu=ccN (default: device 0's when the driver answers, "
                            'else lean.lock.json gpu.cc)')
    tables = commands.add_parser(
        'tables', help='place the Thompson tables in data/tables/ and check their sha256',
        description='Copy the four Thompson tables into data/tables/ and check each sha256 against lean.lock.json; '
                    'a bad copy is removed.')
    source = tables.add_mutually_exclusive_group(required=True)
    source.add_argument('--from', dest='source', metavar='PATH|URL',
                        help='a folder, or the URL of one, that holds the tables')
    source.add_argument('--build', action='store_true',
                        help='regenerate them from reference/mpas_src with the original utility, in a fresh '
                             'build/lean-tables/, check every one against lean.lock.json, then place them')
    commands.add_parser(
        'fetch-upstream', help='clone the pinned MPAS-Model and its externals into reference/mpas_src/',
        description='Clone MPAS-Model v8.4.2 at the commit of lean.lock.json, with MMM-physics and GSL_UGWP at '
                    'their tags, into reference/mpas_src/. A checkout already there is never overwritten.')
    generate = commands.add_parser(
        'generate', help='regenerate src/device/ with the device-code generators',
        description='Run the device-code generators tools/gen_device_physics.py, tools/gen_device_radiation.py and '
                    'tools/gen_radiation_fast_gas.py, which write src/device/.')
    generate.add_argument('--check', action='store_true', help='write nothing; fail if src/device/ differs')
    return top


def _doctor(args: argparse.Namespace) -> int:
    from tools.lean import doctor
    fc = args.fc or DEFAULT_FC[args.target]
    if args.target == 'gpu' and fc != 'nvfortran':
        print('lean doctor: the gpu target needs nvfortran', file=sys.stderr)
        return 2
    found, extra = doctor.checks(args.target, fc), None
    if any(getattr(args, name) is not None for name in ('init', 'namelist', 'streams', 'run_dir', 'build')):
        from tools.lean import memory
        more, extra = memory.memory_checks(args.target, init=args.init, namelist=args.namelist, streams=args.streams,
                                           run_dir=args.run_dir, build=args.build)
        found += more
    if extra is None:   # no memory option given
        text, status = doctor.render(found, as_json=args.json)
    else:
        text, status = doctor.render(found, as_json=args.json, extra=extra)
    print(text)
    return status


def _build(args: argparse.Namespace) -> int:
    from tools.lean import build
    try:
        settings = dict(load_presets()[args.flavor or 'mpas'])
        settings.pop('why')
        settings.pop('flavor')
        switches = dict(settings.pop('switches'))
        for name in ('target', 'fc', 'profile'):
            value = getattr(args, name)
            if value is not None:
                settings[name] = value
        if args.fc is None:
            settings['fc'] = DEFAULT_FC[settings['target']]
        for text in args.switch:
            name, equal, value = text.partition('=')
            if not equal:
                raise ValueError(f'--switch needs NAME=on|off|N, not {text!r}')
            switches[name] = value
        cfg = BuildConfig(**settings, omp=args.omp, variant=args.variant, switches=tuple(switches.items()),
                          gpu_cc=_gpu_cc(args, settings['target']), levels=args.levels)
    except ValueError as error:
        print(f'lean build: {error}', file=sys.stderr)
        return 2
    return build.build(cfg, jobs=args.jobs, dry_run=args.dry_run, as_json=args.json, compiler=args.compiler,
                       link=args.link)


def _gpu_cc(args: argparse.Namespace, target: str):
    """--gpu-cc; else device 0's compute capability when the driver answers; None (lean.lock.json gpu.cc) when that
    is the lock's own value or there is no card."""
    if target != 'gpu':
        return args.gpu_cc
    from tools.lean import card
    cc = args.gpu_cc
    if cc is None:
        found = card.device_zero()
        cc = found.cc if found else None
    lock = load_lock()['gpu']['cc']
    return None if cc in (None, lock) else cc


def _tables(args: argparse.Namespace) -> int:
    from tools.lean import tables
    return tables.rebuild() if args.build else tables.install(args.source)


def _fetch_upstream(args: argparse.Namespace) -> int:
    from tools.lean import upstream
    return upstream.fetch()


def _generate(args: argparse.Namespace) -> int:
    from tools.lean import generate
    return generate.generate(check=args.check)


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else list(argv)
    try:
        args = parser().parse_args(argv)
    except SystemExit as stop:   # --help (0) and usage errors (2)
        return stop.code if isinstance(stop.code, int) else 2
    return {'doctor': _doctor, 'build': _build, 'tables': _tables, 'fetch-upstream': _fetch_upstream,
            'generate': _generate}[args.command](args)
