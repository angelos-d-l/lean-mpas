"""./lean generate: run the device-code generators that write src/device/.

    generate(check) -> 0 (GENERATE OK) | 1 (GENERATE DRIFT: <files> | GENERATE FAIL: E_GENERATOR ...)
    drift()         -> the src/device/ files that differ from what the generators produce

tools/gen_device_physics.py, tools/gen_device_radiation.py and tools/gen_radiation_fast_gas.py read src/*.f90 and
the original files of third_party/mpas-8.4.2/; with --check they write nothing and print `DIFFERS <path>` for each
file that differs.
They run as they are, from the root they live in, with the Python that runs ./lean.
The hand-kept files of src/device/ (mynn_card, radiation_fast, radiation_fast_data, the dyn_* dynamics kernels: config/units.json
device_overrides_why) are written by no generator, so the check does not see them.
"""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

from tools.lean import config

ROOT = config.ROOT
GENERATORS = ('tools/gen_device_physics.py', 'tools/gen_device_radiation.py', 'tools/gen_radiation_fast_gas.py')


class GenerateError(Exception):
    pass


def _rel(path: str) -> str:
    try:
        return str(Path(path).resolve().relative_to(Path(ROOT).resolve()))
    except ValueError:
        return path


def run_generators(check: bool) -> list[str]:
    """Run every generator (with --check if check); the files they report as differing, relative to the root.
    GenerateError if one fails for another reason."""
    differs = []
    for script in GENERATORS:
        command = [sys.executable, str(ROOT / script)] + (['--check'] if check else [])
        try:
            done = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
        except OSError as error:
            raise GenerateError(f'{Path(script).name}: {error}') from error
        found = [_rel(line.split(None, 1)[1]) for line in done.stdout.splitlines() if line.startswith('DIFFERS ')]
        if done.returncode and not (check and found):
            tail = (done.stderr.strip().splitlines() or ['(no output)'])[-1]
            raise GenerateError(f'{Path(script).name} exited {done.returncode}: {tail}')
        differs += found
    return differs


def drift() -> list[str]:
    """The src/device/ files that differ from what the generators produce (none: the committed code is current)."""
    return run_generators(check=True)


def generate(check: bool) -> int:
    """Regenerate src/device/, or with check only compare it (see the module docstring)."""
    try:
        differs = run_generators(check)
    except GenerateError as error:
        print(f'GENERATE FAIL: E_GENERATOR {error}')
        return 1
    if differs:
        print(f'GENERATE DRIFT: {" ".join(differs)}')
        return 1
    print('GENERATE OK')
    return 0
