"""tests/writer/compare_cdl.py on small CDL headers."""
import unittest
from tests.writer.compare_cdl import compare

CDL='''netcdf a {
dimensions:
 nCells = 2 ;
 Time = UNLIMITED ; // (1 currently)
variables:
 float a(Time, nCells) ;
  a:units = "K" ;
  a:long_name = "A" ;
// global attributes:
 :model_name = "mpas" ;
 :git_version = "abc" ;
 :file_id = "123" ;
}
'''
LEAN=CDL.replace(' :file_id', ' :lean_flavor = "mpas" ;\n :lean_switches = "" ;\n :lean_version = "abc" ;\n :file_id')
class CompareTest(unittest.TestCase):
    def test_same(self):
        self.assertEqual(compare(LEAN,CDL),[])
    def test_differences(self):
        for a,b in [('a(Time, nCells)','a(nCells, Time)'),('float a','double a'),('  a:units = "K" ;\n',''),('  a:units = "K" ;\n  a:long_name = "A" ;','  a:long_name = "A" ;\n  a:units = "K" ;'),(':file_id',':extra = "x" ;\n :file_id')]:
            with self.subTest(a=a):
                self.assertEqual(len(compare(LEAN.replace(a,b),CDL)),1)
    def test_exemptions(self):
        other=CDL.replace('"123"','"456"').replace('"abc"','"def"').replace(' :file_id',' :lean_flavor = "mpas" ;\n :lean_switches = "" ;\n :lean_version = "abc" ;\n :file_id')
        self.assertEqual(compare(other,CDL),[])

    def test_lean_attributes_must_precede_file_id(self):
        wrong=CDL.replace(' :file_id', ' :lean_version = "abc" ;\n :file_id')
        self.assertEqual(len(compare(wrong,CDL)),1)

    def test_specified_exchange_coefficient_units(self):
        mpas=CDL.replace('a:units = "K"','chvxy:units = "s m^{-1}"')
        lean=LEAN.replace('a:units = "K"','chvxy:units = "s m^{-1}"').replace('"s m^{-1}"','"m s^{-1}"')
        self.assertEqual(compare(lean,mpas),[])
        self.assertTrue(compare(lean.replace('"m s^{-1}"','"s m^{-1}"'),mpas))

    def test_missing_lean_attributes(self):
        for missing in ('lean_flavor','lean_switches','lean_version', 'all'):
            attrs=''.join(f' :{name} = "x" ;\n' for name in ('lean_flavor','lean_switches','lean_version') if missing not in (name,'all'))
            actual=CDL.replace(' :file_id',attrs+' :file_id')
            with self.subTest(missing=missing):
                self.assertTrue(compare(actual,CDL), 'DIFF required for missing lean provenance')
