"""MPAS 8.4.2 Registry options, defaults and generated namelist readers."""
import contextlib
from collections import Counter
import copy
import importlib.util
import io
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock
import xml.etree.ElementTree as ET

from tools import gen_namelist as gen

ROOT = Path(__file__).resolve().parents[2]
RECORD_COUNTS = (
    ('nhyd_model', 45), ('damping', 11), ('limited_area', 1), ('io', 3),
    ('decomposition', 4), ('restart', 2), ('printout', 3), ('IAU', 2),
    ('assimilation', 1), ('development', 2), ('physics', 67), ('soundings', 1),
    ('physics_lsm_noahmp', 23),
)
# 'off' is allowed for each scheme (MPAS's own value), and cld_incidence for the cloud scheme.
SCHEME_VALUES = {
    'config_microp_scheme': ['mp_thompson', 'off'],
    'config_convection_scheme': ['cu_grell_freitas', 'off'],
    'config_lsm_scheme': ['sf_noahmp', 'off'],
    'config_pbl_scheme': ['bl_mynn', 'off'],
    'config_gwdo_scheme': ['bl_ysu_gwdo', 'off'],
    'config_radt_cld_scheme': ['cld_fraction', 'cld_incidence', 'off'],
    'config_radt_lw_scheme': ['rrtmg_lw', 'off'],
    'config_radt_sw_scheme': ['rrtmg_sw', 'off'],
    'config_sfclayer_scheme': ['sf_mynn', 'off'],
}


def setUpModule():
    (ROOT / 'build').mkdir(exist_ok=True)


def registry_options(text):
    return [(record.attrib['name'], [
        (option.attrib['name'], option.attrib['type'], option.attrib['default_value'])
        for option in record.findall('nml_option')
    ]) for record in ET.fromstring(text).findall('nml_record')]


def subroutine(text, name):
    match = re.search(rf'^ subroutine {name}\(.*?^ end subroutine(?: {name})?$', text, re.M | re.S)
    if match is None:
        raise AssertionError(f'missing subroutine {name}')
    return match.group()


def statements(text):
    text = re.sub(r'&\s*\n\s*', '', text)
    text = re.sub(r'\s*//\s*', '//', text)
    return [line.lstrip() for line in text.splitlines()]


class GeneratorTest(unittest.TestCase):
    def test_namelist_attributes(self):
        records=gen.read_registry()
        text=gen.fortran_module(records)
        body=subroutine(text,'namelist_attributes')
        names=re.findall(r"attrs\(\d+\)%name = '([^']+)'",body)
        self.assertEqual(names,[o.name for r in records for o in r.options])
        self.assertEqual(len(names),165)
        for name,kind,value in [('config_dt',1,'r'),('config_apply_lbcs',3,'text'),('num_soil_layers',2,'i')]:
            i=names.index(name)+1
            self.assertIn(f'attrs({i})%kind = {kind}',body)
            self.assertIn(f'attrs({i})%{value}',body)
        self.assertIn("merge('YES', 'NO ', cfg%config_apply_lbcs)",body)

    def test_preprocess_rules(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            root = Path(tmp)
            (root / 'sub').mkdir()
            (root / 'sub/x.xml').write_text('#ifndef DISABLED\nincluded\n#include "y.xml"\n#endif\n')
            (root / 'sub/y.xml').write_text('relative to x\n')
            path = root / 'Registry.xml'
            path.write_text('begin\n#ifdef DO_PHYSICS\nphysics\n#ifndef CORE_ATMOSPHERE\n'
                            'wrong\n#else\ncore\n#endif\n#include "sub/x.xml"\n#else\n'
                            'wrong\n#endif\n#ifndef DO_CHEMISTRY\nwrong\n#else\nchemistry\n#endif\nend\n')
            self.assertEqual(gen.preprocess(path), 'begin\nphysics\ncore\nincluded\nrelative to x\nchemistry\nend\n')
            self.assertEqual(gen.preprocess(path, frozenset()), 'begin\nwrong\nwrong\nend\n')
            for text, line in (
                ('ok\n#define X\n', 2), ('ok\n#endif\n', 2),
                ('ok\n#ifdef X\n', 2), ('#else\n', 1),
                ('#ifdef X\n#else\n#else\n#endif\n', 3),
                ('#ifdef X\n#if Y\n#endif\n', 2),
                ('#ifdef\n', 1), ('#include <x.xml>\n', 1),
                ('#ifdef X\n#endif extra\n', 2),
            ):
                with self.subTest(text=text):
                    path.write_text(text)
                    with self.assertRaisesRegex(ValueError, re.escape(f'{path}:{line}')):
                        gen.preprocess(path)
            path.write_text('#include "sub/x.xml"\n')
            (root / 'sub/x.xml').write_text('ok\n#unknown\n')
            with self.assertRaisesRegex(ValueError, re.escape(f'{root / "sub/x.xml"}:2')):
                gen.preprocess(path)

    @unittest.skipUnless(shutil.which('cpp') and gen.REGISTRY.exists(), 'cpp or MPAS registry absent')
    def test_matches_cpp(self):
        result = subprocess.run([
            'cpp', '-P', '-traditional-cpp', '-DDO_PHYSICS', '-DDO_CHEMISTRY',
            '-DCORE_ATMOSPHERE', str(gen.REGISTRY),
        ], capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        expected = registry_options(result.stdout)
        self.assertEqual(registry_options(gen.preprocess(gen.REGISTRY)), expected)
        records = gen.read_registry()
        self.assertEqual([(record.name, [(o.name, o.type, o.default) for o in record.options])
                          for record in records], expected)

    def test_noahmp_surface_exchange_is_used(self):
        row = json.loads(gen.TABLE.read_text())['options']['config_noahmp_iopt_sfc']
        self.assertEqual(row['status'], 'use')
        self.assertEqual(set(row), {'status', 'why'})

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_records_of_mpas_842(self):
        records = gen.read_registry()
        self.assertEqual([(r.name, len(r.options)) for r in records], list(RECORD_COUNTS))
        options = [o for r in records for o in r.options]
        self.assertEqual(len(options), 165)
        names = [o.name.lower() for o in options]
        self.assertEqual(len(names), len(set(names)))
        self.assertFalse(set(names) & {'nml_unit', 'cfg', 'msgs', 'ierr', 'iomsg_text'})
        for record in records:
            self.assertTrue(all(o.record == record.name for o in record.options))
        self.assertEqual(gen.DEFINED, frozenset({'DO_PHYSICS', 'DO_CHEMISTRY', 'CORE_ATMOSPHERE'}))
        self.assertEqual(gen.LEAN_RECORD, gen.Record('lean', (
            gen.Option('config_lean_flavor', 'character', '', 'lean'),
            gen.Option('config_lean_strict_inputs', 'logical', 'false', 'lean'),
            gen.Option('config_lean_memory_check', 'logical', 'true', 'lean'),
        )))

    def test_rejects_subpool(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            path = Path(tmp) / 'Registry.xml'
            path.write_text('<registry><nml_record name="sub" in_subpool="true" /></registry>')
            with self.assertRaisesRegex(ValueError, 'in_subpool.*sub|sub.*in_subpool'):
                gen.read_registry(path)

    def test_defaults_follow_gen_inc(self):
        cases = (
            ('real', '720.0', '720.000000'), ('real', '30000.', '30000.000000'),
            ('real', '0.13e-3', '0.000130'), ('real', '1.0e9', '1000000000.000000'),
            ('real', ' -1.25e2tail', '-125.000000'), ('real', 'invalid', '0.000000'),
            ('integer', '2', '2'), ('integer', ' -3x', '-3'), ('integer', 'x3', '0'),
            ('logical', 'true', '.true.'), ('logical', '.true.', '.true.'),
            ('logical', 'True', '.false.'), ('logical', 'false', '.false.'),
            ('character', 'SRK3', "'SRK3'"), ('character', '', "''"),
        )
        for kind, default, expected in cases:
            with self.subTest(kind=kind, default=default):
                self.assertEqual(gen.fortran_default(gen.Option('x', kind, default, 'rec')), expected)
        for default in ("a'b", 'a"b'):
            with self.subTest(default=default), self.assertRaises(ValueError):
                gen.fortran_default(gen.Option('x', 'character', default, 'rec'))

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_reader_shape(self):
        records = gen.read_registry() + [gen.LEAN_RECORD]
        text = gen.fortran_module(records[:-1])
        driver = statements(subroutine(text, 'read_records'))
        calls = [line for line in driver if line.startswith('call read_')]
        self.assertEqual(calls, [f'call read_{r.name}(nml_unit, cfg, msgs)' for r in records])
        for call in calls:
            self.assertEqual(driver[driver.index(call) + 1], 'if (has_fatal(msgs)) return')
        for record in records:
            with self.subTest(record=record.name):
                block = subroutine(text, f'read_{record.name}')
                raw = [line.lstrip() for line in block.splitlines()]
                group = raw.index(f'namelist /{record.name}/ &')
                self.assertEqual(raw[group + 1:group + 1 + len(record.options)], [
                    o.name + (', &' if i < len(record.options) - 1 else '')
                    for i, o in enumerate(record.options)
                ])
                lines = statements(block)
                rewind = lines.index('rewind(nml_unit)')
                for option in record.options:
                    self.assertLess(lines.index(f'{option.name} = cfg%{option.name}'), rewind)
                read = lines.index(f'read(nml_unit, nml={record.name}, iostat=ierr, iomsg=iomsg_text)')
                self.assertEqual(read, rewind + 1)
                error = [
                    'if (ierr > 0) then',
                    "call add_message(msgs, LOG_ERR, 'namelist read: '//trim(iomsg_text))",
                    f"call add_message(msgs, LOG_CRIT, 'Error while reading namelist record {record.name}.')",
                    'return', 'end if',
                ]
                self.assertEqual(lines[read + 1:read + 1 + len(error)], error)
                copy_start = read + 1 + len(error)
                copies = [f'cfg%{o.name} = {o.name}' for o in record.options]
                self.assertEqual(lines[copy_start:copy_start + len(copies)], copies)
                if record.name == 'lean':
                    self.assertNotIn('if (ierr < 0) then', lines)
                    self.assertNotIn('call add_message(msgs, LOG_OUT', block)
                    continue
                missing = lines.index('if (ierr < 0) then')
                self.assertEqual(missing, copy_start + len(copies))
                values = []
                for option in record.options:
                    formatter = {'integer': 'int_text', 'real': 'real_text',
                                 'logical': 'logical_text', 'character': 'trim'}[option.type]
                    values.append(f"call add_message(msgs, LOG_OUT, '        {option.name} = '//"
                                  f'{formatter}(cfg%{option.name}))')
                expected = [
                    f"call add_message(msgs, LOG_OUT, '*** Encountered an issue while attempting to read namelist record {record.name}')",
                    "call add_message(msgs, LOG_OUT, '    The following values will be used for variables in this record:')",
                    "call add_message(msgs, LOG_OUT, ' ')", *values,
                    "call add_message(msgs, LOG_OUT, ' ')", 'end if',
                ]
                self.assertEqual(lines[missing + 1:missing + 1 + len(expected)], expected)

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_declarations_and_dump(self):
        records = gen.read_registry() + [gen.LEAN_RECORD]
        text = gen.fortran_module(records[:-1])
        self.assertIn('tools/gen_namelist.py', text.split('module lean_namelist_options')[0])
        self.assertIn('8.4.2', text.split('module lean_namelist_options')[0])
        for define in gen.DEFINED:
            self.assertIn(define, text.split('module lean_namelist_options')[0])
        declarations = re.search(r' type run_config\n(.*?) end type', text, re.S).group(1)
        dump = [line for line in statements(subroutine(text, 'dump_options')) if line.startswith('write(')]
        expected_dump = []
        for record in records:
            for option in record.options:
                kind = {'real': 'real(kind=RKIND)', 'character': 'character(len=StrKIND)'}.get(option.type, option.type)
                self.assertIn(f'{kind} :: {option.name} = {gen.fortran_default(option)}', declarations)
                formatter = {'real': 'exact_real_text', 'integer': 'int_text',
                             'character': 'trim', 'logical': 'logical_text'}[option.type]
                expected_dump.append(f"write(unit, '(a)') '{option.name} = '//{formatter}(cfg%{option.name})")
        self.assertEqual(dump, expected_dump)
        self.assertIn("'(ES15.8E2)'", text)
        self.assertIn('private', text)
        self.assertNotRegex(text, r'public[^\n]*exact_real_text')

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_lines_fit(self):
        text = gen.fortran_module(gen.read_registry(), gen.load_table())
        for number, line in enumerate(text.splitlines(), 1):
            self.assertLessEqual(len(line), 132, f'line {number}: {line}')

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_table_covers_every_option(self):
        records = gen.read_registry()
        table = gen.load_table()
        self.assertIsNone(gen.check_table(records, table))
        self.assertTrue(table['about'])
        rows = table['options']
        self.assertEqual(list(rows), [o.name for r in records for o in r.options])
        self.assertEqual(Counter(row['status'] for row in rows.values()),
                         Counter({'use': 32, 'ignore': 40, 'fixed': 81, 'scheme': 9, 'mpas': 3}))

    def test_real_default_must_survive_mpas_lf_form(self):
        # MPAS writes a real default with %lf (6 decimals); a fixed or warn check that compares against
        # the Registry text would then misfire on a run that leaves the option out.
        tiny = gen.Option('tiny', 'real', '1.0e-7', 'rec')
        records = [gen.Record('rec', (tiny,))]
        for status, extra in (('fixed', {}), ('warn', {})):
            table = {'about': 'Test.', 'options': {'tiny': {'status': status, 'why': 'Test.', **extra}}}
            with self.assertRaisesRegex(ValueError, 'tiny: real default'):
                gen.check_table(records, table)
        table = {'about': 'Test.', 'options': {'tiny': {'status': 'fixed', 'why': 'Test.', 'values': [0.0]}}}
        self.assertIsNone(gen.check_table(records, table))

    def test_table_rules(self):
        options = (
            gen.Option('count', 'integer', '2', 'rec'),
            gen.Option('rate', 'real', '0.13e-3', 'rec'),
            gen.Option('enabled', 'logical', 'true', 'rec'),
            gen.Option('label', 'character', 'value', 'rec'),
            gen.Option('config_microp_scheme', 'character', 'suite', 'rec'),
            gen.Option('config_time_integration', 'character', 'SRK3', 'rec'),
        )
        records = [gen.Record('rec', options)]
        table = {'about': 'Test table.', 'options': {
            o.name: {'status': 'fixed', 'why': 'Test option.'} for o in options
        }}
        self.assertIsNone(gen.check_table(records, table))
        for name, row in (
            ('count', {'status': 'use', 'why': 'Used.'}),
            ('rate', {'status': 'use', 'why': 'Used within a range.', 'range': [0.0, 1.0]}),
            ('enabled', {'status': 'warn', 'why': 'Pending.'}),
            ('label', {'status': 'fixed', 'why': 'Fixed.'}),
            ('config_microp_scheme', {'status': 'scheme', 'why': 'Scheme.', 'values': ['mp_thompson']}),
            ('config_time_integration', {'status': 'mpas', 'why': 'MPAS checks this.'}),
        ):
            table['options'][name] = row
        self.assertIsNone(gen.check_table(records, table))
        faults = (
            ('count', None),
            ('absent', {'status': 'ignore', 'why': 'No such option.'}),
            ('count', {'status': 'unknown', 'why': 'Unknown status.'}),
            ('count', {'status': 'fixed', 'why': 'Typo.', 'value': [2]}),
            ('count', {'status': 'fixed'}),
            ('count', {'status': 'fixed', 'why': ''}),
            ('count', {'status': 'fixed', 'why': 7}),
            ('count', {'status': 'use', 'why': 'An unknown key.', 'extra': [6]}),
            ('rate', {'status': 'use', 'why': 'Backwards range.', 'range': [1.0, 0.0]}),
            ('rate', {'status': 'use', 'why': 'One bound.', 'range': [0.0]}),
            ('rate', {'status': 'use', 'why': 'Text bounds.', 'range': ['0', '1']}),
            ('count', {'status': 'use', 'why': 'Range of an integer.', 'range': [0.0, 1.0]}),
            ('rate', {'status': 'fixed', 'why': 'Range on a fixed row.', 'values': [0.0], 'range': [0.0, 1.0]}),
            ('count', {'status': 'fixed', 'why': 'Wrong values.', 'values': 2}),
            ('count', {'status': 'fixed', 'why': 'Empty values.', 'values': []}),
            ('count', {'status': 'fixed', 'why': 'Wrong type.', 'values': [2.0]}),
            ('count', {'status': 'fixed', 'why': 'Wrong type.', 'values': [True]}),
            ('rate', {'status': 'fixed', 'why': 'Wrong type.', 'values': ['0.0']}),
            ('rate', {'status': 'fixed', 'why': 'Wrong type.', 'values': [False]}),
            ('enabled', {'status': 'fixed', 'why': 'Wrong type.', 'values': [1]}),
            ('label', {'status': 'fixed', 'why': 'Wrong type.', 'values': [None]}),
            ('label', {'status': 'scheme', 'why': 'Not a scheme.', 'values': ['value']}),
            ('label', {'status': 'mpas', 'why': 'Not an MPAS check.'}),
            ('label', {'status': 'ignore', 'why': 'Unused values.', 'values': ['value']}),
        )
        for name, row in faults:
            with self.subTest(option=name, row=row):
                bad = copy.deepcopy(table)
                if row is None:
                    del bad['options'][name]
                else:
                    bad['options'][name] = row
                with self.assertRaisesRegex(ValueError, re.escape(name)):
                    gen.check_table(records, bad)

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_fixed_scheme_and_ignored_rows(self):
        options = {o.name: o for r in gen.read_registry() for o in r.options}
        rows = gen.load_table()['options']
        fixed = (
            'config_time_integration_order', 'config_split_dynamics_transport',
            'config_dynamics_split_steps', 'config_number_of_sub_steps',
            'config_h_mom_eddy_visc2', 'config_h_mom_eddy_visc4', 'config_v_mom_eddy_visc2',
            'config_h_theta_eddy_visc2', 'config_h_theta_eddy_visc4', 'config_v_theta_eddy_visc2',
            'config_horiz_mixing', 'config_les_model', 'config_mix_full', 'config_mix_scalars',
            'config_scalar_advection', 'config_monotonic', 'config_mpas_cam_coef', 'config_rayleigh_damp_u',
            'config_IAU_option', 'config_jedi_da', 'config_do_DAcycling', 'config_sounding_interval',
            'num_soil_layers', 'config_urban_physics', 'config_oml1d', 'config_ngw_scheme',
            'config_o3climatology', 'config_frac_seaice', 'config_sfc_albedo',
            'config_sstdiurn_update', 'config_deepsoiltemp_update', 'config_conv_interval',
            'config_pbl_interval', 'config_radt_cld_overlap',
            *[n for n in options if n.startswith('config_mynn_')],
            *[n for n in options if n.startswith('config_noahmp_iopt_') and n != 'config_noahmp_iopt_sfc'],
        )
        self.assertEqual(len(fixed), 73)
        extra_fixed = ('config_calendar_type', 'config_h_ScaleWithMesh', 'months', 'noznlev',
                       'config_bucket_update', 'config_ugwp_diags',
                       'config_gfconv_closure_deep', 'config_gfconv_closure_shallow')
        for name in (*fixed, *extra_fixed):
            with self.subTest(option=name):
                option = options[name]
                default = {'integer': int, 'real': float, 'logical': lambda x: x == '.true.',
                           'character': lambda x: x[1:-1]}[option.type](gen.fortran_default(option))
                self.assertEqual(rows[name]['status'], 'fixed')
                self.assertEqual(gen.allowed(option, rows[name]), [default])
        self.assertEqual(set(gen.SCHEMES), set(SCHEME_VALUES))
        for name, values in SCHEME_VALUES.items():
            self.assertEqual(rows[name]['status'], 'scheme')
            self.assertEqual(rows[name]['values'], values)
        ignored = (
            'config_w_adv_order', 'config_theta_adv_order', 'config_scalar_adv_order',
            'config_u_vadv_order', 'config_w_vadv_order', 'config_theta_vadv_order',
            'config_scalar_vadv_order', 'config_n_microp', 'config_pio_num_iotasks', 'config_pio_stride',
            'config_block_decomp_file_prefix', 'config_number_of_blocks', 'config_explicit_proc_decomp',
            'config_proc_decomp_file_prefix', 'config_halo_exch_method', 'config_gpu_aware_mpi',
        )
        self.assertEqual(len(ignored), 16)
        for name in ignored:
            self.assertEqual(rows[name]['status'], 'ignore', name)
        # apvm keeps MPAS's Registry range
        for name in ('config_apvm_upwinding', 'config_microp_re', 'config_sst_update', 'config_do_restart'):
            self.assertEqual(rows[name]['status'], 'use')
        self.assertEqual(rows['config_apvm_upwinding']['range'], [0.0, 1.0])

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_allowed_text(self):
        options = {o.name: o for r in gen.read_registry() for o in r.options}
        rows = gen.load_table()['options']
        for name, expected in (
            ('config_mynn_closure', '2.5'), ('config_horiz_mixing', "'2d_smagorinsky'"),
            ('config_mix_full', '.true.'), ('config_noahmp_iopt_dveg', '4'),
            ('config_apvm_upwinding', '0.5'),
        ):
            with self.subTest(option=name):
                self.assertEqual(gen.allowed_text(options[name], rows[name]), expected)
        option = gen.Option('rate', 'real', '0.13e-3', 'rec')
        self.assertEqual(gen.allowed(option, {}), [0.00013])
        self.assertEqual(gen.allowed_text(option, {}), '0.13e-3')
        self.assertEqual(gen.allowed_text(option, {'values': [0.0, 0.25]}), '0.0 or 0.25')
        self.assertEqual(gen.allowed_text(options['config_mix_full'], {'values': [True, False]}),
                         '.true. or .false.')

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_generated_checks(self):
        records, table = gen.read_registry(), gen.load_table()
        text = gen.fortran_module(records, table)
        self.assertRegex(text, r'public[^\n]*check_options')
        body = subroutine(text, 'check_options')
        self.assertIn('type(run_config), intent(in) :: cfg', body)
        self.assertRegex(body, r'integer, intent\(inout\) :: n_fixed, n_scheme')
        blocks = re.findall(r'^  if \((.*?)\) then\n(.*?)^  end if$', body, re.M | re.S)
        expected = [o for r in records for o in r.options
                    if table['options'][o.name]['status'] in ('fixed', 'scheme', 'warn')
                    or 'range' in table['options'][o.name]]
        self.assertEqual(len(blocks), 91)
        self.assertEqual([re.search(r'cfg%(\w+)', condition).group(1) for condition, _ in blocks],
                         [o.name for o in expected])
        for option, (condition, messages) in zip(expected, blocks):
            with self.subTest(option=option.name):
                row = table['options'][option.name]
                self.assertIn('.not.', condition)
                if 'range' in row:
                    low, high = row['range']
                    self.assertIn(f'cfg%{option.name} >= {low!r}', condition)
                    self.assertIn(f'cfg%{option.name} <= {high!r}', condition)
                    self.assertIn('lean_mpas supports only 0.0 to 1.0', messages)
                    self.assertIn('n_fixed = n_fixed + 1', messages)
                    continue
                self.assertIn('.eqv.' if option.type == 'logical' else '==', condition)
                if option.type == 'character':
                    self.assertIn(f'trim(cfg%{option.name})', condition)
                if option.type == 'real':
                    literal = repr(row['values'][0]) if 'values' in row else gen.fortran_default(option)
                    self.assertIn(literal, condition)
                self.assertEqual(messages.count('call add_message'), 1)
                if row['status'] == 'warn':
                    self.assertIn('LOG_WARN', messages)
                    self.assertIn('does not write this printout; the run goes on without it', messages)
                    self.assertNotIn('n_fixed =', messages)
                    self.assertNotIn('n_scheme =', messages)
                else:
                    self.assertIn('LOG_ERR', messages)
                    self.assertIn('lean_mpas supports only', messages)
                    counter = 'n_scheme' if row['status'] == 'scheme' else 'n_fixed'
                    self.assertIn(f'{counter} = {counter} + 1', messages)
                    self.assertEqual('cfg%config_physics_suite' in messages, row['status'] == 'scheme')
        records = [gen.Record('rec', (gen.Option('rate', 'real', '0.13e-3', 'rec'),))]
        table = {'about': 'Multiple allowed values.', 'options': {
            'rate': {'status': 'fixed', 'why': 'Test.', 'values': [0.0, 0.25]},
        }}
        body = subroutine(gen.fortran_module(records, table), 'check_options')
        self.assertRegex(' '.join(statements(body)), r'cfg%rate == 0\.0.*\.or\..*cfg%rate == 0\.25')

    @unittest.skipUnless(shutil.which('gfortran') and gen.REGISTRY.exists(), 'gfortran or MPAS registry absent')
    def test_check_options_messages(self):
        from tools.lean.config import BuildConfig, unit_flags
        spec = importlib.util.spec_from_file_location('namelist_runner', ROOT / 'tests/namelist/run.py')
        runner = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(runner)
        options, defines = unit_flags(BuildConfig('cpu', 'gfortran', 'strict'), 'serial_services', 'base')
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            root = Path(tmp)
            generated = root / 'lean_namelist_options.f90'
            generated.write_text(gen.fortran_module(gen.read_registry(), gen.load_table()))
            main = root / 'checks_main.f90'
            main.write_text('''program checks_main
 use iso_fortran_env, only: output_unit
 use lean_namelist_options, only: run_config, check_options
 use lean_messages, only: message_list, write_messages, has_fatal
 implicit none
 type(run_config) :: cfg
 type(message_list) :: msgs
 integer :: n_fixed = 7, n_scheme = 11
 cfg%config_apvm_upwinding = 0.0
 cfg%config_microp_re = .true.
 cfg%config_physics_suite = 'convection_permitting'
 cfg%config_horiz_mixing = '2d_smagorinsky '
'''.lstrip() + ''.join(f" cfg%{name} = '{values[0]}'\n" for name, values in SCHEME_VALUES.items()) + '''\
 call check_options(cfg, msgs, n_fixed, n_scheme)
 write(output_unit, '(3(i0,1x))') n_fixed, n_scheme, msgs%n
 cfg%config_horiz_mixing = '2D_SMAGORINSKY'
 cfg%config_mix_full = .false.
 cfg%config_print_detailed_minmax_vel = .true.
 cfg%config_print_global_minmax_sca = .true.
 cfg%config_microp_scheme = 'mp_wsm6'
 cfg%config_mynn_closure = 1000.0
 cfg%config_noahmp_iopt_dveg = 5
 call check_options(cfg, msgs, n_fixed, n_scheme)
 write(output_unit, '(3(i0,1x))') n_fixed, n_scheme, msgs%n
 call write_messages(output_unit, msgs)
 if (has_fatal(msgs)) error stop 1
end program
''')
            exe = runner.build_program(root, 'gfortran', options, defines, 'checks', [
                ROOT / 'src/run/lean_messages.f90', generated, main,
            ])
            result = subprocess.run([str(exe)], cwd=root, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual([line.rstrip() for line in result.stdout.splitlines()], [
                '7 11 0', '11 12 5',
                "ERROR: config_horiz_mixing = '2D_SMAGORINSKY': lean_mpas supports only '2d_smagorinsky'",
                'ERROR: config_mix_full = .false.: lean_mpas supports only .true.',
                "ERROR: config_microp_scheme = 'mp_wsm6' (config_physics_suite = 'convection_permitting'): "
                "lean_mpas supports only 'mp_thompson' or 'off'",
                'ERROR: config_mynn_closure = 1000.00: lean_mpas supports only 2.5',
                'ERROR: config_noahmp_iopt_dveg = 5: lean_mpas supports only 4',
            ])

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_docs_lists_every_option(self):
        records, table = gen.read_registry(), gen.load_table()
        docs = gen.markdown(records, table)
        rows = re.findall(r'^\| `([^`]+)` \| `([^`]+)` \| (.*?) \| (.*?) \|$', docs, re.M)
        options = [o for r in [*records, gen.LEAN_RECORD] for o in r.options]
        self.assertEqual([(r, n) for r, n, _, _ in rows], [(o.record, o.name) for o in options])
        self.assertEqual(len(rows), 168)   # MPAS's 165 and the 3 of the lean record
        for option, (_, _, default, description) in zip(options, rows):
            self.assertEqual(default, f'`{gen.allowed_text(option, {})}`')
            if option.record == 'lean':
                continue
            row = table['options'][option.name]
            status = row['status']
            if status == 'use':
                span = f" ({row['range'][0]!r} to {row['range'][1]!r})" if 'range' in row else ''
                self.assertEqual(description, 'used' + span)
            else:
                self.assertIn(row['why'], description)
            if status in ('fixed', 'scheme'):
                prefix = 'after suite resolution only' if status == 'scheme' else 'only'
                self.assertTrue(description.startswith(f'{prefix} {gen.allowed_text(option, row)}:'))
            if status == 'ignore':
                self.assertTrue(description.startswith('accepted, no effect:'))
            if status == 'warn':
                self.assertTrue(description.startswith('accepted and ignored, with a warning:'))
        for rule in ('Order and rewind', 'Early end or absent record', 'Unknown name or bad value', 'Unknown records',
                     'The `&lean` record', 'ierr = -1', 'A missing file is a CRITICAL ERROR', 'namelist read: <iomsg>',
                     'cannot be opened', 'every problem in one run',
                     'config_horiz_mixing', 'gfortran 15.2', 'later line', 'suite lines'):
            self.assertIn(rule, docs)
        self.assertIn('MPAS expands `$`', docs)
        self.assertIn('lean writes them as given', docs)

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_committed_module_is_current(self):
        records, table = gen.read_registry(), gen.load_table()
        self.assertEqual(gen.OUT_FORTRAN.read_text(), gen.fortran_module(records, table))
        self.assertEqual(gen.OUT_DOCS.read_text(), gen.markdown(records, table))
        self.assertEqual(gen.main(['--check']), 0)

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_check_reports_drift(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            root = Path(tmp)
            table_path = root / 'config/namelist.json'
            table_path.parent.mkdir(parents=True)
            table_path.write_bytes(gen.TABLE.read_bytes())
            paths = ('src/run/lean_namelist_options.f90', 'docs/namelist.md')
            for name, source in zip(paths, (gen.OUT_FORTRAN, gen.OUT_DOCS)):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(source.read_bytes())
            for name in paths:
                with self.subTest(output=name):
                    path = root / name
                    original = path.read_bytes()
                    path.write_bytes(b'X' + original[1:])
                    out = io.StringIO()
                    with contextlib.redirect_stdout(out):
                        status = gen.main(['--check', '--root', str(root), '--registry', str(gen.REGISTRY)])
                    self.assertEqual((status, out.getvalue()), (1, f'DIFFERS {name}\n'))
                    self.assertEqual(path.read_bytes(), b'X' + original[1:])
                    path.unlink()
                    with contextlib.redirect_stdout(io.StringIO()):
                        self.assertEqual(gen.main(['--check', '--root', str(root), '--registry', str(gen.REGISTRY)]), 1)
                    self.assertFalse(path.exists())
                    path.write_bytes(original)
            for name in paths:
                (root / name).unlink()
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                self.assertEqual(gen.main(['--check', '--root', str(root), '--registry', str(gen.REGISTRY)]), 1)
            self.assertEqual(out.getvalue(), ''.join(f'DIFFERS {name}\n' for name in paths))

    def test_root_and_missing_registry(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp:
            root = Path(tmp)
            registry = root / 'reference/mpas_src/src/core_atmosphere/Registry.xml'
            out = io.StringIO()
            with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
                self.assertEqual(gen.main(['--root', str(root)]), 2)
            self.assertEqual(out.getvalue(), f'E_NO_REGISTRY {registry} (./lean fetch-upstream)\n')
            self.assertFalse((root / 'src').exists())
            registry.parent.mkdir(parents=True)
            registry.write_text('<registry><nml_record name="one"><nml_option name="x" '
                                'type="integer" default_value="2" /></nml_record></registry>')
            table = root / 'config/namelist.json'
            table.parent.mkdir()
            table.write_text(json.dumps({'about': 'Scratch table.', 'options': {
                'x': {'status': 'fixed', 'why': 'Scratch option.'},
            }}))
            self.assertEqual(gen.main(['--root', str(root)]), 0)
            output = root / 'src/run/lean_namelist_options.f90'
            self.assertIn('integer :: x = 2', output.read_text())
            self.assertIn('only 2: Scratch option.', (root / 'docs/namelist.md').read_text())
            self.assertEqual(gen.main(['--check', '--root', str(root)]), 0)


class NamelistRunnerTest(unittest.TestCase):
    def test_flavors_match_build_tool(self):
        from tools.lean.config import FLAVORS
        text = (ROOT / 'src/run/lean_namelist.f90').read_text()
        declaration = re.search(r'character\(len=4\),\s*parameter,\s*public\s*::\s*lean_flavors\(2\)\s*=\s*\[(.*?)\]',
                                text, re.IGNORECASE | re.DOTALL)
        self.assertIsNotNone(declaration, 'public lean_flavors declaration is absent')
        self.assertEqual(tuple(re.findall(r"'([^']*)'", declaration.group(1))), FLAVORS)

    @unittest.skipUnless(gen.REGISTRY.exists(), 'MPAS registry absent')
    def test_validate_covers_fixed_and_warn_rows(self):
        spec = importlib.util.spec_from_file_location('namelist_runner', ROOT / 'tests/namelist/run.py')
        runner = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(runner)
        rows = gen.load_table()['options']
        for status, count in (('fixed', 81), ('warn', 0)):
            cases = [case for case in runner.VALIDATE_CASES if case['name'].startswith(f'validate_{status}_')]
            names = [case['name'].removeprefix(f'validate_{status}_') for case in cases]
            self.assertEqual(len(names), count)
            self.assertEqual(set(names), {name for name, row in rows.items() if row['status'] == status})
        self.assertTrue(all(case['mode'] == 'validate' and not case.get('oracle')
                            for case in runner.VALIDATE_CASES))

    def test_main_runs_every_case_table(self):
        spec = importlib.util.spec_from_file_location('namelist_runner', ROOT / 'tests/namelist/run.py')
        runner = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(runner)
        sources = [ROOT / 'tests/namelist/messages_main.f90']
        with tempfile.TemporaryDirectory(dir=ROOT / 'build') as tmp, contextlib.ExitStack() as stack:
            root = Path(tmp)
            stack.enter_context(mock.patch.object(runner, 'ROOT', root))
            stack.enter_context(mock.patch.object(runner.shutil, 'which', return_value='/fc'))
            for table in ('READ_CASES', 'SUITE_CASES', 'LEAN_CASES', 'VALIDATE_CASES'):
                stack.enter_context(mock.patch.object(runner, table, [
                    {'name': table.lower() + str(i), 'sources': sources, 'expected': ('pass',)}
                    for i in range(2)
                ]))
            build = stack.enter_context(mock.patch.object(runner, 'build_program', side_effect=
                lambda work, fc, options, defines, name, sources: work / name / name))
            def execute(command, work, label):
                lines = runner.MESSAGES_EXPECTED if command[0].endswith('/messages') else ('pass',)
                return subprocess.CompletedProcess(command, 0, '\n'.join(lines) + '\n', '')
            run = stack.enter_context(mock.patch.object(runner, 'run_command', side_effect=execute))
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                self.assertEqual(runner.main([]), 0)
            self.assertEqual(build.call_count, 9)
            self.assertEqual(run.call_count, 9)
            self.assertIn('NAMELIST OK: 9 cases, 0 mutants seen', out.getvalue())
            for call in build.call_args_list:
                self.assertEqual([source.name for source in call.args[-1]][:2],
                                 ['lean_messages.f90', 'lean_namelist_options.f90'])


if __name__ == '__main__':
    unittest.main()
