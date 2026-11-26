"""The source class of each output field (src/lean_fields.f90) against the field catalog and the MPAS history and diag
files of the greece_60km case (their CDL in cases/). Run from the repository root."""
from pathlib import Path
import re
import unittest
from tools.field_classes import load, gather_names
from tools.gen_field_catalog import load as catalog

class FieldClassesTest(unittest.TestCase):
    def test_map(self):
        path=Path('src/lean_fields.f90')
        classes=load(path)
        self.assertEqual(set(classes), gather_names(path))
        self.assertLessEqual(set(classes), {v.name for v in catalog().variables})
        names=set()
        for kind in ('history','diag'):
            text=Path(f'cases/greece_60km/reference/strict_12h.{kind}.cdl').read_text()
            names.update(re.findall(r'^\s+(?:float|int|char)\s+(\w+)\s*[(;]',text,re.M))
        self.assertEqual(len(names),199)
        counts={c:sum(classes.get(n)==c for n in names) for c in set(classes.values())}
        for cls,count in [('TIME',3),('MESH',40),('STATE',10),('OUTPUT_DIAG',3),('LEAN',31),('HELD',11),('DIAG',101)]:
            self.assertEqual(counts.get('SOURCE_'+cls,0),count,cls)

    def test_dimension_local_lengths(self):
        longest=max(len(v.dims) for v in catalog().variables)
        for path in ('src/lean_fields.f90','src/io/lean_writer.f90'):
            text=Path(path).read_text()
            for name in re.findall(r'^\s*(\w+)\s*=\s*VAR_DIMS\(',text,re.M|re.I):
                declarations=re.findall(r'character\(len=(\d+)\)\s*::\s*([^\n]+)',text,re.I)
                lengths=[int(n) for n,names in declarations if name.lower() in [x.strip().lower() for x in names.split(',')]]
                self.assertTrue(lengths,(path,name))
                self.assertGreaterEqual(min(lengths),longest,(path,name,longest))
