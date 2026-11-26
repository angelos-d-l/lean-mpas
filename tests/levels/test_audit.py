"""tests/levels/audit.py on small source fixtures."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

(Path(__file__).resolve().parents[2] / 'build').mkdir(exist_ok=True)


class AuditTests(unittest.TestCase):
    def test_inventory_kinds(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        fixtures = {
            'config.f90': 'subroutine literal\nreal :: a(45)\n! 45 is a comment\nend subroutine\n',
            'param.f90': 'module sizes\nuse config\ninteger, parameter :: layers=nz+1\nend module\n',
            'fixed.f90': 'module fields\ntype record\nreal :: field(nz)\nend type\nend module\n',
            'local.f90': 'subroutine column(a)\n!$acc routine seq\nreal :: a(nz)\nreal :: work(nz)\nend subroutine\n',
            'reduce.f90': 'subroutine capacity\n!$acc routine seq\nreal :: work(max_levels)\nprint *, sum(work)\nend subroutine\n',
        }
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            src = Path(temp) / 'src'
            src.mkdir()
            for name, text in fixtures.items():
                (src / name).write_text(text)
            found = audit.scan(src)
            self.assertEqual({k: len(v) for k, v in found.items()},
                             dict(literal=1, param=1, fixed=1, cardlocal=1, reduce=1, dynprivate=0))

    def test_table_exception_is_exact(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        self.assertFalse(audit.level_literal('thompson.f90', '         do k = 1, 45'))
        self.assertTrue(audit.level_literal('thompson.f90', 'real :: unrelated(45)'))

    def test_private_and_contained_reduction(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            src = Path(temp) / 'src'
            src.mkdir()
            (src / 'loop.f90').write_text("""subroutine driver(a)
real :: a(nz)
real :: private_work(nz), unrelated(max_lw_layers)
!$acc parallel loop private(private_work)
do i=1,4
enddo
end subroutine
subroutine column
!$acc routine seq
real :: work(max_levels)
contains
subroutine inner
print *, maxval(work)
end subroutine
end subroutine
""")
            found = audit.scan(src)
            self.assertEqual(len(found['cardlocal']), 1)
            self.assertEqual(len(found['reduce']), 1)

    def test_mynn_tridiagonal_local(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            src = Path(temp) / 'src'
            (src / 'device').mkdir(parents=True)
            (src / 'device/mynn_pbl_helpers.f90').write_text("""subroutine tridiag2(a)
!$acc routine seq
use config, only: n => nz
real :: a(n)
real :: cp(n),dp(n)
end subroutine
""")
            self.assertEqual(len(audit.scan(src)['cardlocal']), 1)

    def test_reduction_expressions(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        for text in ['sum(work)', 'maxval(abs(work))', 'sum(work*other)', 'any(mask=work)']:
            self.assertTrue(audit.reduces_whole(text, {'work'}), text)
        for text in ['sum(work(1:nz))', 'sum(other_work)', 'sum(work(:nz)*other(:nz))']:
            self.assertFalse(audit.reduces_whole(text, {'work'}), text)

    def test_unbounded_reduction_sections(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        for text in ['sum(work(:))', 'maxval(abs(work(1:)))', 'any(work(:,2))',
                     'sum(work(1:max_levels))', 'sum(work(1:max_levels+1))']:
            self.assertTrue(audit.reduces_whole(text, {'work'}), text)
        self.assertFalse(audit.reduces_whole('sum(work(1:nz,:))', {'work': {0}}))

    def test_dynamics_private_keeps_runtime_bound(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            src = Path(temp) / 'src'
            src.mkdir()
            (src / 'dyn_tend.f90').write_text('''subroutine loop
real :: work(nz)
!$acc parallel loop private(work)
do i=1,4
enddo
end subroutine
subroutine column
!$acc routine seq
real :: work(nz)
end subroutine
''')
            found = audit.scan(src)
            self.assertEqual(len(found['cardlocal']), 1)
            self.assertEqual(len(found['dynprivate']), 1)

    def test_continued_dummy_without_intent(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            src = Path(temp) / 'src'
            src.mkdir()
            (src / 'continued.f90').write_text('''subroutine column(first, &
 & second)
!$acc routine seq
real :: first(nz), second(nz)
end subroutine
''')
            self.assertEqual(len(audit.scan(src)['cardlocal']), 0)

    def test_additional_reductions(self):
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        for function, extra in [('product', ''), ('dot_product', ', other'),
                                ('norm2', ''), ('matmul', ', other'), ('findloc', ', 0.')]:
            with self.subTest(function=function):
                for ref in ['work', 'abs(work)', 'work(:)', 'work(1:max_levels)']:
                    self.assertTrue(audit.reduces_whole(f'{function}({ref}{extra})', {'work': {0}}))
                self.assertFalse(audit.reduces_whole(f'{function}(work(1:nz){extra})', {'work': {0}}))
                if extra:
                    self.assertTrue(audit.reduces_whole(f'{function}(other, work)', {'work': {0}}))

    def test_dynamics_private_count_is_asserted(self):
        import contextlib
        import io
        from unittest.mock import patch
        spec = importlib.util.spec_from_file_location('levels_audit', Path(__file__).with_name('audit.py'))
        audit = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(audit)
        with tempfile.TemporaryDirectory(dir=Path(__file__).resolve().parents[2] / 'build') as temp:
            root = Path(temp)
            src = root / 'src'
            src.mkdir()
            declarations = '\n'.join(f'real :: work{i}(nz)' for i in range(13))
            private = ','.join(f'work{i}' for i in range(13))
            path = src / 'dyn_tend.f90'
            for count, expected in [(12, 0), (13, 1)]:
                path.write_text('subroutine driver\n' + '\n'.join(declarations.splitlines()[:count]) +
                                f'\n!$acc parallel loop private({private})\nend subroutine\n')
                output = io.StringIO()
                with patch.object(audit, 'ROOT', root), patch('sys.argv', ['audit.py', '--expect', 'dynprivate=12']), contextlib.redirect_stdout(output):
                    self.assertEqual(audit.main(), expected)
                self.assertIn(f'dynprivate={count}', output.getvalue())
                self.assertEqual(len(audit.scan(src)['cardlocal']), 0)
