"""./lean fetch-upstream: the pinned MPAS-Model checkout in reference/mpas_src/, with its two externals.

    fetch(dest=Path('reference/mpas_src')) -> 0 (UPSTREAM OK) | 1 (UPSTREAM FAIL)

- dest is a git checkout at the pinned commit: `UPSTREAM OK (already present)`, nothing is touched.
- dest holds anything else (another commit, or files that are not a checkout of their own): E_UPSTREAM_MISMATCH,
  nothing is touched.
- dest is absent or empty: `git clone --branch v8.4.2 --depth 1`, a check of its commit, then each external of
  src/core_atmosphere/Externals.cfg (MMM-physics, GSL_UGWP) is cloned at its tag into src/core_atmosphere/physics/
  <local_path> (MPAS runs checkout_externals from that folder) and its commit is checked against lean.lock.json.
  The clone is made in <dest>.part and renamed only when every check passes; on a failure it is deleted.

A relative dest is taken from the repo root. The pins come from lean.lock.json (lock= replaces it in tests).
"""
from __future__ import annotations

import configparser
import shutil
import subprocess
from pathlib import Path
from typing import Optional

from tools.lean import config

ROOT = config.ROOT
EXTERNALS_CFG = 'src/core_atmosphere/Externals.cfg'
EXTERNALS_ROOT = 'src/core_atmosphere/physics'
LOCK_KEYS = {'MMM-physics': 'mmm_physics', 'GSL_UGWP': 'gsl_ugwp'}


class UpstreamError(Exception):
    pass


def _git(*args: str, cwd: Optional[Path] = None) -> subprocess.CompletedProcess:
    return subprocess.run(['git', *args], cwd=cwd, capture_output=True, text=True)


def checkout_commit(path: Path) -> Optional[str]:
    """The HEAD commit of the git checkout whose top folder is path; None if path is not the top of one (a plain
    folder inside another repository is not a checkout of its own)."""
    top = _git('rev-parse', '--show-toplevel', cwd=path)
    if top.returncode or Path(top.stdout.strip()).resolve() != path.resolve():
        return None
    head = _git('rev-parse', 'HEAD', cwd=path)
    return head.stdout.strip() if head.returncode == 0 else None


def _clone(repo: str, tag: str, commit: str, target: Path, what: str) -> None:
    done = _git('clone', '--quiet', '--branch', tag, '--depth', '1', repo, str(target))
    if done.returncode:
        raise UpstreamError(f'E_UPSTREAM_CLONE {what}: git clone --branch {tag} {repo} failed: '
                            f'{done.stderr.strip()}')
    found = checkout_commit(target)
    if found != commit:
        raise UpstreamError(f'E_UPSTREAM_MISMATCH {what} {tag} is {found}, lean.lock.json pins {commit}')


def _externals(checkout: Path, lock: dict) -> None:
    cfg = configparser.ConfigParser()
    if not cfg.read(checkout / EXTERNALS_CFG):
        raise UpstreamError(f'E_UPSTREAM_MISMATCH {EXTERNALS_CFG} is missing from the checkout')
    names = [name for name in cfg.sections() if cfg.has_option(name, 'repo_url')]
    if sorted(names) != sorted(LOCK_KEYS):
        raise UpstreamError(f'E_UPSTREAM_MISMATCH {EXTERNALS_CFG} lists {", ".join(names)}, lean.lock.json pins '
                            f'{", ".join(LOCK_KEYS)}')
    for name in names:
        section, pin = cfg[name], lock[LOCK_KEYS[name]]
        if section['tag'] != pin['tag']:
            raise UpstreamError(f'E_UPSTREAM_MISMATCH {name}: {EXTERNALS_CFG} has tag {section["tag"]}, '
                                f'lean.lock.json pins {pin["tag"]}')
        _clone(section['repo_url'], pin['tag'], pin['commit'], checkout / EXTERNALS_ROOT / section['local_path'],
               name)


def fetch(dest: Path = Path('reference/mpas_src'), lock: Optional[dict] = None) -> int:
    """Fill dest with the pinned upstream sources (see the module docstring)."""
    lock = lock or config.load_lock()
    pin = lock['mpas_model']
    dest = Path(dest) if Path(dest).is_absolute() else ROOT / dest
    if dest.is_dir() and any(dest.iterdir()):
        found = checkout_commit(dest)
        if found == pin['commit']:
            print('UPSTREAM OK (already present)')
            return 0
        print(f'UPSTREAM FAIL: E_UPSTREAM_MISMATCH {dest} holds {found or "files that are not a git checkout"}, '
              f'lean.lock.json pins {pin["commit"]}; nothing was changed')
        return 1
    if dest.exists() and not dest.is_dir():
        print(f'UPSTREAM FAIL: E_UPSTREAM_MISMATCH {dest} is a file; nothing was changed')
        return 1
    part = dest.with_name(dest.name + '.part')
    if part.exists():
        shutil.rmtree(part)
    dest.parent.mkdir(parents=True, exist_ok=True)
    try:
        _clone(pin['repo'], pin['tag'], pin['commit'], part, 'MPAS-Model')
        _externals(part, lock)
    except UpstreamError as error:
        shutil.rmtree(part, ignore_errors=True)
        print(f'UPSTREAM FAIL: {error}')
        return 1
    if dest.is_dir():
        dest.rmdir()   # empty: checked above
    part.rename(dest)
    print('UPSTREAM OK')
    return 0
