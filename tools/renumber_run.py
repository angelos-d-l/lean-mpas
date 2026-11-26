"""Renumber existing MPAS run inputs with one mesh permutation.

    python tools/renumber_run.py IN_DIR OUT_DIR --init NAME [--also 'GLOB' ...] [--curve=hilbert|morton3]
    python tools/renumber_run.py --apply FILE.perm.npz IN.nc OUT.nc
    python tools/renumber_run.py --check OLD.nc NEW.nc [--perm FILE.perm.npz]
    python tools/renumber_run.py --check IN_DIR OUT_DIR --init NAME [--also 'GLOB' ...] [--perm FILE.perm.npz]

OUT_DIR must be new or empty. The init file defines the permutation for itself, lbc.*.nc, and files selected by
--also (relative to IN_DIR). Other entries are symlinked. If a glob selects files inside a directory, its other
entries are symlinked inside the corresponding output directory. Graph partition links warn because their order
belongs to the original mesh. The permutation is saved as NAME.perm.npz alongside the new init file.

Apply mode takes that permutation (new -> old, 0-based) and requires a new output file and matching mesh sizes.
Check mode compares NetCDF data through the same permutation and checks connectivity where available. It looks
for NEW.nc.perm.npz first, then a unique *.init.nc.perm.npz or *.perm.npz beside NEW.nc, unless --perm is
given. Directory checks use the init sidecar by default and check the init, all LBC inputs and all other copied
files. Repeat --also to check that every additional file selected by those globs is present. Checks exit 0 when they
pass and 1 on failure.
"""
import argparse
import os
import sys
from pathlib import Path
from time import perf_counter

from renumber_grid import apply_permutation, check, load_permutations, permutation_path, renumber


def selected_files(src, init, extra):
    if init.is_absolute() or '..' in init.parts or not (src / init).is_file():
        raise ValueError(f'expected an init file relative to {src}: {init}')
    selected = {init}
    for pattern in ['lbc.*.nc', *extra]:
        if Path(pattern).is_absolute() or '..' in Path(pattern).parts:
            raise ValueError(f'expected a glob relative to {src}: {pattern}')
        selected.update(p.relative_to(src) for p in src.glob(pattern) if p.is_file())
    return selected


def report(src, dst, elapsed):
    print(f'{dst.name}: {src.stat().st_size:,} -> {dst.stat().st_size:,} bytes in {elapsed:.3f} s')


def link_other_entries(src, dst, selected, parents, directory=Path('.')):
    for entry in sorted((src / directory).iterdir()):
        relative = entry.relative_to(src)
        if relative in selected:
            continue
        if relative in parents:
            link_other_entries(src, dst, selected, parents, relative)
        else:
            (dst / relative).symlink_to(entry.resolve())
            if entry.match('*.graph.info*') or entry.match('graph.info*'):
                print(f'WARNING: {relative} uses the original mesh order', file=sys.stderr)


def renumber_directory(src, dst, init, extra, curve):
    src, dst = src.resolve(), dst.resolve()
    if dst == src or src in dst.parents:
        raise ValueError('output directory must be outside the input directory')
    selected = selected_files(src, init, extra)
    if os.path.lexists(src / permutation_path(init)):
        raise ValueError(f'{permutation_path(init)} already exists in the input and would collide with the new permutation')
    dst.mkdir(parents=True, exist_ok=True)
    if any(dst.iterdir()):
        raise ValueError(f'output directory must be empty: {dst}')

    parents = {parent for path in selected for parent in path.parents}
    for name in [init, *sorted(selected - {init})]:
        old, new = src / name, dst / name
        new.parent.mkdir(parents=True, exist_ok=True)
        start = perf_counter()
        if name == init:
            perms = renumber(old, new, curve)
        else:
            apply_permutation(old, new, perms)
        report(old, new, perf_counter() - start)
    link_other_entries(src, dst, selected, parents)


def find_permutation(new):
    sidecar = permutation_path(new)
    if sidecar.is_file():
        return sidecar
    for pattern in ('*.init.nc.perm.npz', '*.perm.npz'):
        candidates = [p for p in new.parent.glob(pattern) if p.is_file()]
        if len(candidates) == 1:
            return candidates[0]
    raise ValueError(f'could not identify a permutation for {new}; specify --perm')


def check_directory(src, dst, init, extra, perm):
    selected = selected_files(src, init, extra)
    sidecar = permutation_path(init)
    perms = load_permutations(perm or dst / sidecar)
    # walk skips symlinked subdirectories, just as the file filter skips symlinked inputs.
    copied = set()
    for directory, _, files in os.walk(dst):
        for name in files:
            path = Path(directory) / name
            relative = path.relative_to(dst)
            if not path.is_symlink() and path.is_file() and (src / relative).is_file():
                copied.add(relative)
    files = (copied | selected) - {init, sidecar}
    failures = 0
    for name in [init, *sorted(files)]:
        print(name)
        if name not in copied:
            print('CHECK FAILED: missing source or renumbered file')
            failures += 1
        else:
            failures += check(src / name, dst / name, perms)
    return int(failures != 0)


def main():
    parser = argparse.ArgumentParser(usage=argparse.SUPPRESS, description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--apply', metavar='PERM', type=Path)
    mode.add_argument('--check', action='store_true')
    parser.add_argument('src', type=Path)
    parser.add_argument('dst', type=Path)
    parser.add_argument('--init', type=Path)
    parser.add_argument('--also', action='append', default=[])
    parser.add_argument('--curve', choices=('hilbert', 'morton3'), default='hilbert')
    parser.add_argument('--perm', type=Path)
    args = parser.parse_args()

    if args.perm and not args.check:
        parser.error('--perm is only for --check')
    if args.also and not args.init:
        parser.error('--also requires --init')
    if args.apply and args.init:
        parser.error('--apply takes two files')
    if not args.init and (args.src.is_dir() or not (args.apply or args.check)):
        parser.error('directory mode requires --init')

    try:
        if args.apply:
            start = perf_counter()
            apply_permutation(args.src, args.dst, load_permutations(args.apply))
            report(args.src, args.dst, perf_counter() - start)
        elif args.check:
            if args.init:
                return check_directory(args.src, args.dst, args.init, args.also, args.perm)
            return check(args.src, args.dst, load_permutations(args.perm or find_permutation(args.dst)))
        else:
            renumber_directory(args.src, args.dst, args.init, args.also, args.curve)
    except (OSError, ValueError, RuntimeError) as err:
        print(f'ERROR: {err}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
