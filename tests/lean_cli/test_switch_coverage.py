"""Every physics switch used by the host and device sources has a table entry."""
import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
# Defines the build sets itself (for the card build, and LEAN_NZ for --levels), not physics switches.
NON_SWITCH_DEFINES = {'LEAN_PHYS_DEV', 'LEAN_NVMATH', 'LEAN_NZ'}


class SwitchCoverageTest(unittest.TestCase):
    def test_every_source_switch_in_table(self):
        table = json.loads((ROOT / 'config/switches.json').read_text(encoding='utf-8'))['switches']
        allowed = set(table) | NON_SWITCH_DEFINES
        missing = []
        for folder in (ROOT / 'src', ROOT / 'src/device'):
            for path in sorted(folder.glob('*.f90')):
                for number, line in enumerate(path.read_text(encoding='utf-8').splitlines(), 1):
                    if re.match(r'^\s*#\s*(?:if|ifdef|ifndef)\b', line):
                        for name in sorted(set(re.findall(r'\bLEAN_[A-Z0-9_]+\b', line)) - allowed):
                            missing.append(f'{path.relative_to(ROOT)}:{number}: {name}')
        self.assertFalse(missing, 'Source switches missing from config/switches.json:\n' + '\n'.join(missing))
