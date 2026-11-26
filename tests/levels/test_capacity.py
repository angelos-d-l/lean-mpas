"""tools/level_capacity.py, which the device generators use: a card-local level array gets the capacity max_levels,
while dummy shapes and unrelated locals are kept."""
import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
from level_capacity import capacity_locals


class CapacityTests(unittest.TestCase):
    def test_local_extent_and_active_operations(self):
        lines = '''module m
contains
subroutine column(a)
!$acc routine seq
use config, only: nz
real, intent(inout) :: a(nz)
real :: work(nz), bins(9), scalar
work = a
work(2:) = a(2:)
scalar = sum(work)
a = work
call use_work(work=work)
call use_work( &
 & work=work)
end subroutine
end module'''.splitlines()
        text = '\n'.join(capacity_locals(lines))
        self.assertIn('a(nz)', text)
        self.assertIn('work(max_levels)', text)
        self.assertIn('bins(9)', text)
        self.assertIn('sum(work(1:nz))', text)
        self.assertIn('work(1:nz) = a', text)
        self.assertIn('work(2:nz) = a(2:)', text)
        self.assertIn('call use_work(work=work(1:nz))', text)
        self.assertIn('& work=work(1:nz))', text)

    def test_dimension_and_interface_level(self):
        lines = '''module m
contains
subroutine column(a)
!$acc routine seq
use config, only: kte => nz, kme => nz1
real, intent(inout) :: a(kme)
real, dimension(1:kme) :: work
work(:) = a
end subroutine
end module'''.splitlines()
        text = '\n'.join(capacity_locals(lines))
        self.assertIn('dimension(1:max_levels+1)', text)
        self.assertIn('work(1:kme) = a', text)

    def test_host_routine_is_untouched(self):
        lines = '''module m
contains
subroutine host
real :: a(nz)
end subroutine
end module'''.splitlines()
        self.assertEqual(capacity_locals(lines), lines)

    def test_mixed_dummy_and_local_dimension(self):
        lines = """module m
contains
subroutine column(a)
!$acc routine seq
use config, only: nz
real, dimension(1:nz) :: a, work
work = a
end subroutine
end module""".splitlines()
        text = '\n'.join(capacity_locals(lines))
        self.assertIn('dimension(1:nz) :: a', text)
        self.assertIn('dimension(1:max_levels) :: work', text)
