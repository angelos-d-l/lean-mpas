#!/usr/bin/env python3
"""lean_mpas: build and check the model. ./lean --help lists the commands (tools/lean/cli.py)."""
import sys
from pathlib import Path

if sys.version_info < (3, 9):
    sys.exit('E_PYTHON_OLD: ./lean needs Python 3.9 or newer, this is ' + sys.version.split()[0])
sys.path.insert(0, str(Path(__file__).resolve().parent))

from tools.lean.cli import main  # noqa: E402  (after the path is set)

sys.exit(main())
