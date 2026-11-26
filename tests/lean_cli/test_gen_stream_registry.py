"""Generated stream tables against MPAS 8.4.2's Registry and saved logs."""
import contextlib
import io
from pathlib import Path
import re
import tempfile
import unittest

from tools import gen_stream_registry as gen

ROOT = Path(__file__).resolve().parents[2]
REFERENCE = ROOT / 'data/cases'
GREECE_LOG = REFERENCE / 'greece_60km/reference/strict_12h/log.atmosphere.0000.out'
X1_LOG = REFERENCE / 'x1_2562/reference/strict_24h/log.atmosphere.0000.out'
NO_LOGS = 'the MPAS reference logs of data/cases/*/reference are not released'
POOL_SIZES = (('mesh', 102), ('state', 9), ('diag', 180), ('tend', 12), ('lbc', 9),
              ('halo_scratch', 1), ('diag_physics', 248), ('tend_physics', 43),
              ('atm_input', 2), ('ngw_input', 7), ('sfc_input', 58), ('tend_iau', 4),
              ('diag_physics_noahmp', 60), ('output_noahmp', 90))
IMMUTABLE_SIZES = (('invariant', 87), ('input', 123), ('restart', 406), ('iau', 5),
                   ('lbc_in', 5), ('ugwp_oro_data_in', 20), ('ugwp_ngw_in', 3), ('da_state', 58))


@unittest.skipUnless(gen.REGISTRY.is_file(), 'MPAS Registry absent')
class StreamRegistryTest(unittest.TestCase):
    def setUp(self):
        self.tables = gen.load()

    def test_counts(self):
        t = self.tables
        self.assertEqual(len(t.packages), 19)
        self.assertEqual(t.packages[0][0], 'mp_kessler_in')
        self.assertEqual(t.packages[-1][0], 'sfclayer')
        self.assertEqual(len(t.fields), 825)
        self.assertEqual(tuple((n, len(f)) for n, f in t.pools), POOL_SIZES)
        self.assertEqual(dict(t.pools)['state'], ('scalars', 'aerosols', 'initial_time', 'xtime',
                                                'Time', 'u', 'w', 'rho_zz', 'theta_m'))
        self.assertEqual(tuple((s.name, len(s.entries)) for s in t.immutable), IMMUTABLE_SIZES)
        self.assertEqual({f.name for f in t.fields if f.scratch},
                         {'scale', 'tend_ru_physics', 'tend_rtheta_physics', 'tend_rho_physics',
                          'tend_uzonal', 'tend_umerid'})

    def test_inheritance(self):
        fields = {f.name: f for f in self.tables.fields}
        self.assertEqual(fields['u_amb'].packages, ('iau',))
        self.assertEqual(fields['lbc_u'].packages, ('limited_area',))
        self.assertEqual(fields['scalars'].packages, ())
        self.assertEqual(fields['scalars_amb'].packages[:4],
                         ('iau', 'bl_mynn_in', 'bl_ysu_in', 'cu_ntiedtke_in'))
        self.assertIn('mp_thompson_in', fields['scalars_amb'].packages)
        self.assertIn('limited_area', fields['lbc_scalars'].packages)
        self.assertIn('mp_thompson_in', fields['lbc_scalars'].packages)

    def test_immutable_order(self):
        streams = {s.name: s for s in self.tables.immutable}
        entries = streams['input'].entries
        self.assertEqual(entries[0], ('latCell', ('no_invariant_stream',)))
        self.assertEqual(entries[87], ('scalars', ()))
        self.assertEqual(entries[-1], ('h_oml_initial', ()))
        self.assertEqual(tuple(n for n, _ in streams['lbc_in'].entries),
                         ('lbc_scalars', 'lbc_u', 'lbc_w', 'lbc_rho', 'lbc_theta'))

    def test_field_dims(self):
        # FIELD_DIMS: each field's Registry dimensions without Time, as get_dimlist_for_field
        # gives them (framework/mpas_bootstrapping.F:933-1071): a var_array's are those of one constituent.
        fields = {f.name: f for f in self.tables.fields}
        self.assertEqual(fields['latCell'].dims, ('nCells',))
        self.assertEqual(fields['cellsOnEdge'].dims, ('TWO', 'nEdges'))
        self.assertEqual(fields['lbc_w'].dims, ('nVertLevelsP1', 'nCells'))
        self.assertEqual(fields['scalars'].dims, ('nVertLevels', 'nCells'))
        self.assertEqual(fields['lbc_scalars'].dims, ('nVertLevels', 'nCells'))
        self.assertEqual(fields['xtime'].dims, ())
        self.assertEqual(fields['mminlu'].dims, ())
        self.assertTrue(all(len(' '.join(f.dims)) <= 48 for f in self.tables.fields))
        module = gen.fortran_module(self.tables)
        self.assertIn(" character(len=48), parameter :: FIELD_DIMS(825) = [ &", module)
        self.assertIn("  'nVertLevelsP1 nCells', &", module)

    @unittest.skipUnless(GREECE_LOG.is_file(), NO_LOGS)
    def test_dimension_blocks_match_mpas_log(self):
        # The dimensions MPAS lists for a stream are those of its active fields, in order of first appearance
        # (framework/mpas_bootstrapping.F:562-640): the greece strict_12h log, lines 251-284.
        fields = {f.name: f for f in self.tables.fields}
        active = {'mp_thompson_in', 'cu_grell_freitas_in', 'bl_mynn_in', 'sf_noahmp_in', 'sfclayer',
                  'no_invariant_stream', 'limited_area'}
        log = GREECE_LOG.read_text().splitlines()
        for stream in self.tables.immutable:
            if stream.name not in ('input', 'lbc_in'):
                continue
            with self.subTest(stream=stream.name):
                dims = []
                for name, packages in stream.entries:
                    field = fields[name]
                    if ((field.packages and not active.intersection(field.packages)) or
                            (packages and not active.intersection(packages)) or field.scratch):
                        continue
                    dims.extend(d for d in field.dims if d not in dims)
                first = next(i for i, line in enumerate(log)
                             if line.startswith(f" ----- reading dimensions from stream '{stream.name}'"))
                logged = []
                for line in log[first + 1:]:
                    if not line.startswith('        '):
                        break
                    logged.append(line.split()[0])
                self.assertEqual(dims, logged)

    @unittest.skipUnless(GREECE_LOG.is_file() and X1_LOG.is_file(), NO_LOGS)
    def test_deactivated_lists_match_mpas_logs(self):
        fields = {f.name: f for f in self.tables.fields}
        for case, log_path, count in (('greece_60km', GREECE_LOG, 37), ('x1_2562', X1_LOG, 41)):
            with self.subTest(case=case):
                active = {'mp_thompson_in', 'cu_grell_freitas_in', 'bl_mynn_in', 'sf_noahmp_in',
                          'sfclayer', 'no_invariant_stream'}
                if case == 'greece_60km':
                    active.add('limited_area')
                inactive = []
                for stream in self.tables.immutable:
                    for name, packages in stream.entries:
                        field = fields[name]
                        if ((field.packages and not active.intersection(field.packages)) or
                                (packages and not active.intersection(packages)) or field.scratch):
                            inactive.append(name)
                log = log_path.read_text()
                before_parse = log.split('Parsing run-time I/O configuration')[0]
                expected = re.findall(r' \* Requested field (\S+) is deactivated', before_parse)
                self.assertEqual(len(expected), count)
                self.assertEqual(inactive, expected)

    def test_lines_fit(self):
        self.assertTrue(all(len(line) <= 132 for line in gen.fortran_module(self.tables).splitlines()))

    def test_committed_module_is_current(self):
        self.assertEqual(gen.main(['--check']), 0)

    def test_check_reports_drift(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            root = Path(tmp)
            output = root / 'src/run/lean_stream_registry.f90'
            output.parent.mkdir(parents=True)
            output.write_text(gen.fortran_module(self.tables) + '! drift\n')
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                status = gen.main(['--root', str(root), '--registry', str(gen.REGISTRY), '--check'])
            self.assertEqual(status, 1)
            self.assertEqual(out.getvalue(), 'DIFFERS src/run/lean_stream_registry.f90\n')

    def test_active_when_in_lean_streams(self):
        text = (ROOT / 'src/run/lean_streams.f90').read_text()
        normalized = re.sub(r'[\s&]', '', text).lower()
        for name in ('les', 'sfclayer'):
            self.assertIn(name.upper() + '_WHEN', text)
            expression = dict(self.tables.packages)[name]
            expression = re.sub(r'\bconfig_\w+', lambda m: 'cfg%' + m.group(), expression)
            self.assertIn(re.sub(r'\s', '', expression).lower(), normalized)

    def test_invalid_registry_rejected(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            path = Path(tmp) / 'Registry.xml'
            for text, message in (
                ('<registry><var_struct name="a"><var_struct name="b"/></var_struct></registry>', 'nested'),
                ('<registry><var_struct name="a"><var name="' + 'x'*33 + '"/></var_struct></registry>', '32'),
                ('<registry><var_struct name="a"><var name="x" persistence="scratch"/></var_struct>'
                 '<streams><stream name="s" type="input" immutable="true" filename_template="x">'
                 '<var name="x"/></stream></streams></registry>', 'scratch'),
            ):
                path.write_text(text)
                with self.subTest(message=message), self.assertRaisesRegex(ValueError, message):
                    gen.load(path)


if __name__ == '__main__':
    unittest.main()
