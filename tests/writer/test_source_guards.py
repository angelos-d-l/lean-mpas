"""Source checks of error paths and card guards that a CPU run cannot reach. Run from the repository root."""
from pathlib import Path
import re
import unittest


class SourceGuardsTest(unittest.TestCase):
    def test_ice_guard(self):
        text=Path('src/lean_model.f90').read_text()
        line=next(s for s in text.splitlines() if 'update self(sea_ice_cells%tslb)' in s)
        self.assertIn('if(loc==1.and.ice_on)',line)

    def test_checked_init_opens(self):
        text=Path('src/lean_fields.f90').read_text()
        opens=list(re.finditer(r'status=nc_open\(f,run%cold%init_path\)',text))
        self.assertEqual(len(opens),2)
        for match in opens:
            following=text[match.end():].lstrip().splitlines()[:4]
            self.assertIn('if(status/=NC_NOERR)then',following[0])
            self.assertIn('run%cold%init_path','\n'.join(following))
            self.assertIn('error stop','\n'.join(following))

    @unittest.skipUnless(Path('reference/mpas_src/src').is_dir(), 'reference/mpas_src is absent (./lean fetch-upstream)')
    def test_isobaric_reference_fix(self):
        original=Path('reference/mpas_src/src/core_atmosphere/diagnostics/mpas_isobaric_diagnostics.F').read_text()
        patched=Path('reference_fixes/mpas_isobaric_diagnostics.F').read_text()
        old='if (NEED_TEMP .or. NEED_RELHUM .or. NEED_DEWPOINT .or. need_mslp) then'
        new='if (NEED_TEMP .or. NEED_RELHUM .or. NEED_DEWPOINT .or. need_mslp .or. need_t_isobaric .or. need_meanT_500_300) then'
        self.assertEqual(original.count(old),1)
        self.assertEqual(patched,original.replace(old,new))
