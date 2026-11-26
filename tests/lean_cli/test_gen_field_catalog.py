"""tools/gen_field_catalog.py: the field catalog generated from MPAS 8.4.2's Registry, and its --check."""
import contextlib
import hashlib
import io
from pathlib import Path
import tempfile
import unittest
from tools import gen_field_catalog as gen

@unittest.skipUnless(gen.REGISTRY.is_file(), 'MPAS Registry absent')
class CatalogTest(unittest.TestCase):
    def test_catalog(self):
        t = gen.load()
        self.assertEqual((len(t.variables), sum(v.constituent > 0 for v in t.variables), len(t.attributes), len(t.dimensions)), (874,54,1855,29))
        v = {v.name:v for v in t.variables}
        def attrs(name):
            a = v[name]
            return t.attributes[a.first-1:a.first-1+a.count]
        self.assertEqual([a[0] for a in attrs('Time')], ['units','long_name','standard_name'])
        self.assertIn(('_FillValue','-9999',True), attrs('isnowxy'))
        self.assertIn('descrition', [a[0] for a in attrs('qc_bl')])
        self.assertIn(('units','m s^{-1}',False), attrs('chvxy'))
        self.assertEqual([a.name for a in t.variables if a.field == v['qv'].field], ['qv','qc','qr','qi','qs','qg','ni','nr','nc','nifa','nwfa','tke'])
        from tools.gen_stream_registry import load
        self.assertEqual(v['qv'].field, [f.name for f in load().fields].index('scalars')+1)
        self.assertEqual(v['qv'].constituent, 1)
        self.assertEqual((v['u'].dims,v['u'].time), ('nVertLevels nEdges',True))
        self.assertEqual([v[n].default for n in ('isice_lu','isnowxy','t_oml')], ['24','-9999','0.0'])
        self.assertEqual(dict(t.dimensions)['nMonths'], 'namelist:months')
        self.assertEqual(dict(t.dimensions)['nVertLevelsP1'], 'nVertLevels+1')
        module=gen.fortran_module(t)
        self.assertIn(hashlib.sha256(gen.preprocess(gen.REGISTRY).encode()).hexdigest(), module)
        self.assertTrue(all(len(s)<=132 for s in module.splitlines()))
        self.assertEqual(gen.main(['--check']),0)
        with tempfile.TemporaryDirectory(dir='build') as tmp:
            path=Path(tmp)/gen.OUT
            path.parent.mkdir(parents=True)
            path.write_text(module+'! drift\n')
            out=io.StringIO()
            with contextlib.redirect_stdout(out):
                self.assertEqual(gen.main(['--root',tmp,'--registry',str(gen.REGISTRY),'--check']),1)
            self.assertEqual(out.getvalue(),'DIFFERS src/run/lean_field_catalog.f90\n')

class SavedCatalogTest(unittest.TestCase):
    def test_saved_registry_fingerprint(self):
        text=(Path(__file__).resolve().parents[2]/gen.OUT).read_text()
        self.assertIn("REGISTRY_SHA256 = 'b2bdb24639c4b70b7b63b9a1879b1192fe676c47cf1765ddeede3eac6922492c'",text)
