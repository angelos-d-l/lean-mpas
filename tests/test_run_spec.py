"""Apply case specs to MPAS's generated defaults without rewriting other text."""
import contextlib
import difflib
import io
import shlex
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
DEFAULTS = ROOT / 'reference' / 'mpas_defaults'
TOOL = ROOT / 'tools' / 'cases' / 'run_spec.py'
KEYS = ('BUILD', 'RANKS', 'INPUTS', 'CARD', 'MINUTES', 'EXPECT')


class RunSpecTest(unittest.TestCase):
    def setUp(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        self.scratch = tempfile.TemporaryDirectory(dir=ROOT / 'build')
        self.addCleanup(self.scratch.cleanup)
        self.tmp = Path(self.scratch.name)
        self.calls = 0

    def run_specs(self, *specs, core='atmosphere', paths=None, bases=None, out=None):
        self.calls += 1
        work = self.tmp / str(self.calls)
        work.mkdir()
        if paths is None:
            paths = []
            for i, contents in enumerate(specs):
                path = work / f'spec-{i}.txt'
                path.write_text(contents, encoding='utf-8')
                paths.append(path)
        if bases is None:
            bases = (DEFAULTS / f'namelist.{core}', DEFAULTS / f'streams.{core}')
        if out is None:
            out = work / 'out'
            out.mkdir()
        done = subprocess.run(
            [sys.executable, str(TOOL), '--out', str(out),
             *(str(path) for path in bases), *(str(path) for path in paths)],
            cwd=work, capture_output=True, text=True, timeout=10,
        )
        return done, out

    def assert_error(self, done, out, line=1):
        self.assertEqual(done.returncode, 2, done.stderr)
        self.assertEqual(done.stdout, '')
        self.assertIn(f':{line}:', done.stderr)
        self.assertNotIn('Traceback', done.stderr)
        self.assertEqual(list(out.iterdir()), [])

    def test_empty_spec_is_identity(self):
        for core in ('atmosphere', 'init_atmosphere'):
            with self.subTest(core=core):
                done, out = self.run_specs('', core=core)
                self.assertEqual(done.returncode, 0, done.stderr)
                for kind in ('namelist', 'streams'):
                    name = f'{kind}.{core}'
                    self.assertEqual((out / name).read_bytes(), (DEFAULTS / name).read_bytes())
                self.assertEqual(done.stdout, "BUILD=''\nRANKS=1\nINPUTS=.\nCARD=off\nMINUTES=10\nEXPECT=''\n")

    def test_nml_replace_in_place(self):
        done, out = self.run_specs('nml nhyd_model config_dt 120.0\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        before = (DEFAULTS / 'namelist.atmosphere').read_text().splitlines(keepends=True)
        after = (out / 'namelist.atmosphere').read_text().splitlines(keepends=True)
        changes = [line for line in difflib.ndiff(before, after) if line.startswith(('- ', '+ '))]
        self.assertEqual(changes, ['-     config_dt = 720.0\n', '+     config_dt = 120.0\n'])
        self.assertEqual((out / 'streams.atmosphere').read_bytes(),
                         (DEFAULTS / 'streams.atmosphere').read_bytes())

    def test_nml_add_missing_option(self):
        done, out = self.run_specs("nml physics config_lsm_scheme 'sf_noahmp'\n")
        self.assertEqual(done.returncode, 0, done.stderr)
        before = (DEFAULTS / 'namelist.atmosphere').read_text()
        close = before.index('\n/', before.index('&physics\n'))
        expected = before[:close] + "\n    config_lsm_scheme = 'sf_noahmp'" + before[close:]
        self.assertEqual((out / 'namelist.atmosphere').read_text(), expected)

    def test_nml_errors(self):
        for record in ('damping', 'nosuch'):
            with self.subTest(record=record):
                done, out = self.run_specs(f'nml {record} config_dt 1.0\n')
                self.assert_error(done, out)
                if record == 'damping':
                    self.assertIn('nhyd_model', done.stderr)

    def test_stream_replace_only_that_stream(self):
        done, out = self.run_specs('stream output output_interval 1:00:00\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        tree = ET.parse(out / 'streams.atmosphere')
        self.assertEqual(tree.find("./stream[@name='output']").get('output_interval'), '1:00:00')
        self.assertEqual(tree.find("./stream[@name='diagnostics']").get('output_interval'), '3:00:00')
        before = (DEFAULTS / 'streams.atmosphere').read_text()
        self.assertEqual((out / 'streams.atmosphere').read_text(),
                         before.replace('output_interval="6:00:00"', 'output_interval="1:00:00"', 1))

    def test_stream_add_attribute(self):
        done, out = self.run_specs('stream lbc_in reference_time 2026-11-06_06:30:00\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        before = ET.parse(DEFAULTS / 'streams.atmosphere').getroot()
        after = ET.parse(out / 'streams.atmosphere').getroot()
        self.assertEqual(len(before), len(after))
        for old, new in zip(before, after):
            attrs = dict(old.attrib)
            if old.get('name') == 'lbc_in':
                attrs['reference_time'] = '2026-11-06_06:30:00'
            self.assertEqual(new.attrib, attrs)
        expected = (DEFAULTS / 'streams.atmosphere').read_text().replace(
            '                  input_interval="3:00:00" />',
            '                  input_interval="3:00:00"\n'
            '                  reference_time="2026-11-06_06:30:00" />',
        )
        self.assertEqual((out / 'streams.atmosphere').read_text(), expected)

    def test_stream_unknown(self):
        done, out = self.run_specs('stream nosuch output_interval 1:00:00\n')
        self.assert_error(done, out)

    def test_summary_eval(self):
        done, _ = self.run_specs(
            'build MPAS-nv-strict\nranks 8\ncard on\nminutes 45\ninputs v0630\n'
            'expect a.nc\nexpect b.nc\n'
        )
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual([line.split('=', 1)[0] for line in done.stdout.splitlines()], list(KEYS))
        shell = subprocess.run(
            ['bash', '-c', 'eval "$1"; printf \'%s|\' "$BUILD" "$RANKS" "$INPUTS" '
             '"$CARD" "$MINUTES" "$EXPECT"', 'run-spec-test', done.stdout],
            capture_output=True, text=True, timeout=10,
        )
        self.assertEqual(shell.returncode, 0, shell.stderr)
        self.assertEqual(shell.stdout, 'MPAS-nv-strict|8|v0630|on|45|a.nc b.nc|')

    def test_layering(self):
        done, out = self.run_specs('ranks 1\nexpect a.nc\n', 'ranks 8\nexpect b.nc\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn('RANKS=8\n', done.stdout)
        self.assertIn("EXPECT='a.nc b.nc'\n", done.stdout)
        done, out = self.run_specs('ranks 1\nranks 8\n')
        self.assert_error(done, out, line=2)

    def test_repo_specs_apply(self):
        cases = ROOT / 'cases'
        for spec in sorted(cases.glob('*/mpas_runs/*.txt')):
            with self.subTest(spec=spec.relative_to(ROOT)):
                base = spec.parent.parent / 'case.txt'
                paths = [base, spec] if base.exists() else [spec]
                done, _ = self.run_specs(paths=paths)
                self.assertEqual(done.returncode, 0, done.stderr)
        for spec in sorted(cases.glob('*/init/*.txt')):
            if spec.name == 'base.txt':
                continue
            with self.subTest(spec=spec.relative_to(ROOT)):
                base = spec.parent / 'base.txt'
                self.assertTrue(base.is_file(), f'missing init base: {base}')
                done, _ = self.run_specs(paths=[base, spec], core='init_atmosphere')
                self.assertEqual(done.returncode, 0, done.stderr)

    def test_bad_statements_do_not_write(self):
        for statement in (
            'unknown value', 'ranks', 'ranks 1 extra', 'ranks 0', 'ranks -1', 'ranks 1.5',
            'minutes 0', 'minutes nope', 'card yes', 'card on extra', 'build',
            'inputs', 'expect', 'expect a.nc b.nc', 'nml physics config_lsm_scheme',
            'stream output output_interval', 'stream output output_interval 1:00:00 extra',
            'stream output invalid=attribute value',
        ):
            with self.subTest(statement=statement):
                done, out = self.run_specs('# heading\n\n' + statement + '\n')
                self.assert_error(done, out, line=3)

    def test_all_scalars_override_and_reject_duplicates(self):
        first = 'build first\nranks 1\ninputs .\ncard off\nminutes 10\n'
        second = 'build second\nranks 8\ninputs v0630\ncard on\nminutes 45\n'
        done, _ = self.run_specs(first, second)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(done.stdout, "BUILD=second\nRANKS=8\nINPUTS=v0630\nCARD=on\nMINUTES=45\nEXPECT=''\n")
        for statement in first.splitlines():
            with self.subTest(statement=statement):
                done, out = self.run_specs(statement + '\n' + statement + '\n')
                self.assert_error(done, out, line=2)

    def test_comments_and_verbatim_namelist_value(self):
        value = "'a path/with spaces',  2.0, 'a=b'"
        done, out = self.run_specs(
            '# comment\n  \n ranks 8 # ranks\n'
            f'  nml nhyd_model config_start_time {value}\n'
            'stream output output_interval 1:00:00 # cadence\n'
        )
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn(f'    config_start_time = {value}\n',
                      (out / 'namelist.atmosphere').read_text())
        self.assertIn('RANKS=8\n', done.stdout)
        tree = ET.parse(out / 'streams.atmosphere')
        self.assertEqual(tree.find("./stream[@name='output']").get('output_interval'), '1:00:00')

    def test_repeated_edits_and_case_insensitive_namelist_names(self):
        done, out = self.run_specs(
            "nml PHYSICS CONFIG_LSM_SCHEME 'first'\n"
            'stream lbc_in reference_time first\n',
            "nml physics config_lsm_scheme 'sf_noahmp'\n"
            'stream lbc_in reference_time 2026-11-06_06:30:00\n',
        )
        self.assertEqual(done.returncode, 0, done.stderr)
        text = (out / 'namelist.atmosphere').read_text()
        self.assertEqual(text.lower().count('config_lsm_scheme ='), 1)
        self.assertIn("= 'sf_noahmp'\n", text)
        tree = ET.parse(out / 'streams.atmosphere')
        self.assertEqual(tree.find("./immutable_stream[@name='lbc_in']").get('reference_time'),
                         '2026-11-06_06:30:00')

    def test_later_error_preserves_existing_outputs(self):
        out = self.tmp / 'existing'
        out.mkdir()
        saved = {'namelist.atmosphere': b'previous namelist\n',
                 'streams.atmosphere': b'previous streams\n', 'keep.txt': b'keep\n'}
        for name, contents in saved.items():
            (out / name).write_bytes(contents)
        done, _ = self.run_specs('nml nhyd_model config_dt 120.0\n',
                                 'stream nosuch output_interval 1:00:00\n', out=out)
        self.assertEqual(done.returncode, 2, done.stderr)
        self.assertIn('spec-1.txt:1:', done.stderr)
        self.assertEqual(done.stdout, '')
        self.assertEqual({path.name: path.read_bytes() for path in out.iterdir()}, saved)

    def test_output_directory_must_exist(self):
        out = self.tmp / 'missing'
        done, _ = self.run_specs('', out=out)
        self.assertEqual(done.returncode, 2, done.stderr)
        self.assertEqual(done.stdout, '')
        self.assertFalse(out.exists())
        self.assertNotIn('Traceback', done.stderr)

    def test_command_line_errors_have_a_location(self):
        done = subprocess.run([sys.executable, str(TOOL)], cwd=self.tmp,
                              capture_output=True, text=True, timeout=10)
        self.assertEqual(done.returncode, 2, done.stderr)
        self.assertEqual(done.stdout, '')
        self.assertIn('run_spec.py:0:', done.stderr)

    def test_write_failure_restores_both_outputs(self):
        from tools.cases import run_spec

        original_replace = Path.replace
        for existing in (False, True):
            with self.subTest(existing=existing):
                out = self.tmp / f'io-error-{existing}'
                out.mkdir()
                saved = {}
                if existing:
                    saved = {'namelist.atmosphere': b'previous namelist\n',
                             'streams.atmosphere': b'previous streams\n'}
                    for name, contents in saved.items():
                        (out / name).write_bytes(contents)
                spec = self.tmp / 'write-error.txt'
                spec.write_text('nml nhyd_model config_dt 120.0\n', encoding='utf-8')
                failed = []

                def fail_second_write(source, target):
                    target = Path(target)
                    if target == out / 'streams.atmosphere' and not failed:
                        failed.append(True)
                        raise OSError('injected second output write failure')
                    return original_replace(source, target)

                stdout, stderr = io.StringIO(), io.StringIO()
                with mock.patch.object(Path, 'replace', fail_second_write), \
                        contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
                    status = run_spec.main([
                        '--out', str(out), str(DEFAULTS / 'namelist.atmosphere'),
                        str(DEFAULTS / 'streams.atmosphere'), str(spec),
                    ])
                self.assertEqual(status, 2, stderr.getvalue())
                self.assertTrue(failed, 'the second output write was attempted')
                self.assertIn('write-error.txt:0:', stderr.getvalue())
                self.assertIn('injected second output write failure', stderr.getvalue())
                self.assertEqual(stdout.getvalue(), '')
                self.assertEqual({path.name: path.read_bytes() for path in out.iterdir()}, saved)

    def test_missing_spec_does_not_write(self):
        done, out = self.run_specs(paths=[self.tmp / 'missing.txt'])
        self.assert_error(done, out, line=0)
        self.assertIn('missing.txt:', done.stderr)

    def test_shell_metacharacters_remain_literal(self):
        build = "MPAS-$(false);'quoted'"
        inputs = 'v$HOME;false'
        expected = '$(false).nc'
        done, _ = self.run_specs(f'build {build}\ninputs {inputs}\nexpect {expected}\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        for key, value in (('BUILD', build), ('INPUTS', inputs), ('EXPECT', expected)):
            self.assertIn(f'{key}={shlex.quote(value)}\n', done.stdout)
        shell = subprocess.run(
            ['bash', '-c', 'eval "$1"; printf \'%s|\' "$BUILD" "$INPUTS" "$EXPECT"',
             'run-spec-test', done.stdout], capture_output=True, text=True, timeout=10,
        )
        self.assertEqual(shell.returncode, 0, shell.stderr)
        self.assertEqual(shell.stdout, f'{build}|{inputs}|{expected}|')

    def test_stream_values_are_xml_escaped(self):
        value = 'history.&<"\'$Y.nc'
        done, out = self.run_specs(f'stream output filename_template {value}\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        tree = ET.parse(out / 'streams.atmosphere')
        self.assertEqual(tree.find("./stream[@name='output']").get('filename_template'), value)

    def test_stream_edits_ignore_comments_and_other_elements(self):
        streams = self.tmp / 'streams.atmosphere'
        text = (DEFAULTS / streams.name).read_text()
        text = text.replace('<streams>', '<streams>\n'
                            '<!-- <stream name="output" output_interval="comment" /> -->\n'
                            '<stream-info name="output" output_interval="untouched" />', 1)
        streams.write_text(text, encoding='utf-8')
        done, out = self.run_specs('stream output output_interval 1:00:00\n',
                                   bases=(DEFAULTS / 'namelist.atmosphere', streams))
        self.assertEqual(done.returncode, 0, done.stderr)
        expected = text.replace('output_interval="6:00:00"', 'output_interval="1:00:00"', 1)
        self.assertEqual((out / streams.name).read_text(), expected)

    def test_identity_preserves_crlf_and_missing_final_newline(self):
        bases = []
        for name in ('namelist.atmosphere', 'streams.atmosphere'):
            path = self.tmp / name
            path.write_bytes((DEFAULTS / name).read_bytes().replace(b'\n', b'\r\n').rstrip(b'\r\n'))
            bases.append(path)
        done, out = self.run_specs('', bases=bases)
        self.assertEqual(done.returncode, 0, done.stderr)
        for path in bases:
            self.assertEqual((out / path.name).read_bytes(), path.read_bytes())


if __name__ == '__main__':
    unittest.main()
