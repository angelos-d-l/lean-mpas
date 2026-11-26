"""Every diagnostic field lean_mpas lists as its own (SOURCE_DIAG) is fetched by diag_fetch, and docs/fields.md is current.
Run from the repository root."""
from pathlib import Path
import re
import subprocess
import sys
import unittest
from tools.field_classes import load

class DiagnosticCoverage(unittest.TestCase):
    def test_fetch_names(self):
        classes=load('src/lean_fields.f90')
        produced={n for n,c in classes.items() if c=='SOURCE_DIAG'}
        text=Path('src/diag/diag_manager.f90').read_text().split('subroutine diag_fetch',1)[1].split('end subroutine',1)[0]
        fetched={n for case in re.findall(r'case\((.*?)\)',text,re.S) for n in re.findall(r"'([^']+)'",case)}
        self.assertEqual(len(produced),102)
        self.assertEqual(produced,fetched)
    def test_document(self):
        p=subprocess.run([sys.executable,'tools/gen_fields_doc.py','--check'],capture_output=True,text=True)
        self.assertEqual(p.returncode,0,p.stdout+p.stderr)
        self.assertIn('FIELDS DOC OK',p.stdout)
