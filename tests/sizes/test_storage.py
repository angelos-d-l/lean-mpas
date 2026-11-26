"""Storage allocated once at the start (sea ice, glacier, acoustic work) and the card forms of the sea-ice members,
read from the source text."""
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class StorageTest(unittest.TestCase):
    def test_start_allocations(self):
        model = (ROOT / 'src/lean_model.f90').read_text()
        for name in ('allocate_ice', 'allocate_glacier_of', 'allocate_acoustic_work'):
            self.assertIn('call ' + name + '()', model)
            self.assertLess(model.index('call ' + name + '()'), model.index('call phys_device_start('))

    def test_ice_members(self):
        text = (ROOT / 'src/sea_ice.f90').read_text()
        names = 'xice sst tslb snow snowc snoalb z0 albedo emiss grdflx acsnow acsnom sfcrunoff'.split()
        dev = (ROOT / 'src/phys_dev.f90').read_text()
        for name in names:
            self.assertRegex(text, r'allocate\(ice%' + name + r'\(')
            self.assertRegex(text, r'to%' + name + r'\([:,]+\) = from%' + name)
            for direction in ('device', 'self'):
                self.assertRegex(dev, r'!\$acc update ' + direction + r'\([^\n]*sea_ice_cells%' + name + r'\b')

    def test_acoustic_and_boundary(self):
        text = (ROOT / 'src/device/dyn_acoustic.f90').read_text()   # the card's work arrays; the host kernel has none
        for name in ('ts_work', 'rs_work', 'flag_index', 'flag_arg', 'flag_val'):
            self.assertIn('allocate(' + name + '(', text)
        text = (ROOT / 'src/dyn_boundary.f90').read_text()
        allocate = text.split(' subroutine lbc_allocate(')[1].split(' end subroutine')[0]
        self.assertIn('allocate(rho_edge(nz,ne1))', allocate)
        self.assertNotIn('allocate(rho_edge', text.split(' subroutine derive_record(')[1])


if __name__ == '__main__':
    unittest.main()
