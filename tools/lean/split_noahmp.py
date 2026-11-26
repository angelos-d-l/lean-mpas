"""Split the lean Noah-MP code into its original compilation units.

src/noahmp_core.f90 (and src/device/noahmp_core.f90) hold the Noah-MP modules one after another, each after a line
'! Source: physics_noahmp/<dir>/<Name>.F90'. They are compiled one file per module, <Name>.f90: combining them would
allow cross-module optimizations that the original build does not have. The MOST and Chen97 surface-resistance
alternatives share each MOST unit (they never call each other), so the units and their flags are those of the
original files. The text before the first marker is a header and is dropped.
"""
from __future__ import annotations

from pathlib import Path

MARKER = '! Source: '


def split_noahmp(core: Path, out_dir: Path) -> list[Path]:
    """Write each module of core to out_dir/<Name>.f90 and return the paths in source order.

    A file that already has the bytes is not rewritten, so its time stamp stays and a build does not recompile it.
    """
    out_dir.mkdir(parents=True, exist_ok=True)
    pieces = []
    for part in Path(core).read_text(encoding='utf-8').split(MARKER)[1:]:
        header, body = part.split('\n', 1)
        path = out_dir / (Path(header).stem + '.f90')
        data = body.encode('utf-8')
        if not path.is_file() or path.read_bytes() != data:
            path.write_bytes(data)
        pieces.append(path)
    return pieces
