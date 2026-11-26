#!/usr/bin/env python3
"""lean's namelist reader and checks (src/run/lean_namelist.f90, driven by lean_main.f90) against MPAS 8.4.2's own
generated reader and checks (oracle.F90 with oracle_stubs.F90), and lean's message formatting (messages_main.f90), on
the cases in tests/namelist/cases and the generated option cases; every program is compiled with the model's host
flags and the stdout lines must match."""
import argparse
from itertools import zip_longest
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from tools.lean.config import BuildConfig, unit_flags
from tools import gen_namelist

KINDS = ROOT / 'third_party/mpas-8.4.2/src/framework/mpas_kind_types.F'
MPAS = ROOT / 'reference/mpas_src/src'
MESSAGES_EXPECTED = (
    'F',
    'T',
    ' Reading namelist from file x',
    '',
    'WARNING: w',
    'ERROR: e',
    'ERROR: n',
    'CRITICAL ERROR: c',
    '-3',
    '2147483647',
    '0.500000',
    '720.000',
    '30000.0',
    '0.130000E-03',
    '0.100000E+10',
    '1000.00',
    '2.50000',
    'T',
    'F',
)

# Reader cases use a named input and compare the output file with MPAS's oracle.
# Small standalone cases still have sources, expected stdout, and optional args/status.
READ_CASES = [
    {'name': 'mpas_default', 'input': 'mpas_default', 'oracle': True},
    {'name': 'ours', 'input': 'ours', 'oracle': True},
    {'name': 'empty', 'input': 'empty', 'oracle': True, 'blocks': 13},
    {'name': 'odd_syntax', 'input': 'odd_syntax', 'oracle': True, 'dump': (
        'config_dt = 3.00000000E+01',
        'config_len_disp = 3.00000000E+03',
        'config_start_time = 2026-10-12_06:00:00',
        'config_microp_re = T', 'config_sst_update = F', 'config_apply_lbcs = T',
    )},
    {'name': 'crlf', 'input': 'crlf', 'oracle': True, 'same_dump': 'ours'},
    {'name': 'reversed', 'input': 'reversed', 'oracle': True, 'same_dump': 'ours'},
    {'name': 'with_lean_record', 'input': 'with_lean_record', 'oracle': True, 'same_dump': 'ours',
     'lean_dump': ('config_lean_flavor = lean', 'config_lean_strict_inputs = T')},
    {'name': 'unterminated', 'input': 'unterminated', 'oracle': True,
     'used_values': ('        config_dt = 30.0000',)},
    {'name': 'unknown_name', 'input': 'unknown_name', 'oracle': True, 'status': 1,
     'read_error': True, 'gfortran_cause': 'config_bogus'},
    {'name': 'bad_value', 'input': 'bad_value', 'oracle': True, 'status': 1, 'read_error': True},
    {'name': 'bad_value_at_end', 'input': 'bad_value_at_end', 'oracle': True,
     'used_values': ('        config_dt = 30.0000', '        config_len_disp = 0.00000'),
     # nvfortran 25.3 reads the repeat count '3x0' into a scalar as an error (iostat > 0), gfortran as an end
     # of record (iostat < 0). MPAS's own reader built with nvfortran stops too, so lean follows the compiler.
     'nvfortran': {'status': 1, 'used_values': (), 'read_error': True}},
]
CP_NOAHMP = {'config_physics_suite': 'convection_permitting', 'config_lsm_scheme': 'sf_noahmp'}
LEAN_SCHEMES = {
    'config_microp_scheme': 'mp_thompson',
    'config_convection_scheme': 'cu_grell_freitas',
    'config_pbl_scheme': 'bl_mynn',
    'config_gwdo_scheme': 'bl_ysu_gwdo',
    'config_radt_cld_scheme': 'cld_fraction',
    'config_radt_lw_scheme': 'rrtmg_lw',
    'config_radt_sw_scheme': 'rrtmg_sw',
    'config_sfclayer_scheme': 'sf_mynn',
    'config_lsm_scheme': 'sf_noahmp',
}
SUITE_CASES = [
    {'name': 'mesoscale_default', 'physics': {'config_physics_suite': 'mesoscale_reference'}},
    {'name': 'cp_default', 'physics': {'config_physics_suite': 'convection_permitting'}},
    {'name': 'cp_noahmp', 'physics': CP_NOAHMP},
    {'name': 'none_default', 'physics': {'config_physics_suite': 'none'}},
    {'name': 'none_explicit', 'physics': {'config_physics_suite': 'none', **LEAN_SCHEMES}},
    {'name': 'unknown_suite', 'physics': {'config_physics_suite': 'convection_allowing'}, 'status': 1},
    *[{'name': f'illegal_{option}', 'physics': {**CP_NOAHMP, option: 'x_illegal'}, 'status': 1}
      for option in LEAN_SCHEMES],
    {'name': 'illegal_long_name', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_microp_scheme': 'mp_thompson_with_a_long_name'}},
    *[{'name': f'legal_{value}', 'physics': {**CP_NOAHMP, option: value, **other}}
      for option, value, other in (
          ('config_microp_scheme', 'mp_kessler', {}),
          ('config_microp_scheme', 'mp_thompson_aerosols', {}),
          ('config_microp_scheme', 'mp_wsm6', {}),
          ('config_convection_scheme', 'cu_kain_fritsch', {}),
          ('config_convection_scheme', 'cu_tiedtke', {}),
          ('config_convection_scheme', 'cu_ntiedtke', {}),
          ('config_pbl_scheme', 'bl_ysu', {'config_sfclayer_scheme': 'sf_monin_obukhov'}),
          ('config_gwdo_scheme', 'bl_ugwp_gwdo', {}),
          ('config_radt_lw_scheme', 'cam_lw', {}),
          ('config_radt_sw_scheme', 'cam_sw', {}),
          ('config_radt_cld_scheme', 'cld_incidence', {}),
          ('config_radt_cld_scheme', 'cld_fraction_thompson', {}),
          ('config_sfclayer_scheme', 'sf_monin_obukhov_rev', {'config_pbl_scheme': 'bl_ysu'}),
          ('config_lsm_scheme', 'sf_noah', {}),
      )],
    {'name': 'mynn_forces_sf_mynn',
     'physics': {**CP_NOAHMP, 'config_sfclayer_scheme': 'sf_monin_obukhov'}},
    {'name': 'ysu_with_sf_mynn', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_pbl_scheme': 'bl_ysu', 'config_sfclayer_scheme': 'sf_mynn'}},
    {'name': 'ysu_with_monin', 'physics': {**CP_NOAHMP, 'config_pbl_scheme': 'bl_ysu',
                                        'config_sfclayer_scheme': 'sf_monin_obukhov_rev'}},
    {'name': 'cld_off_rad_on', 'physics': {**CP_NOAHMP, 'config_radt_cld_scheme': 'off'}},
    {'name': 'cld_off_lw_only', 'physics': {**CP_NOAHMP, 'config_radt_cld_scheme': 'off',
                                         'config_radt_sw_scheme': 'off'}},
    {'name': 'cld_off_sw_only', 'physics': {**CP_NOAHMP, 'config_radt_cld_scheme': 'off',
                                         'config_radt_lw_scheme': 'off'}},
    {'name': 'ysu_with_sf_off', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_pbl_scheme': 'bl_ysu', 'config_sfclayer_scheme': 'off'}},
    {'name': 'mynn_forces_sf_mynn_from_off',
     'physics': {**CP_NOAHMP, 'config_sfclayer_scheme': 'off'}},
    {'name': 'cld_off_rad_off', 'physics': {**CP_NOAHMP, 'config_radt_cld_scheme': 'off',
                                         'config_radt_lw_scheme': 'off', 'config_radt_sw_scheme': 'off'}},
    {'name': 'lsm_without_sfclayer', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_pbl_scheme': 'off', 'config_sfclayer_scheme': 'off'}},
    {'name': 'all_off', 'physics': {'config_physics_suite': 'convection_permitting',
                                   **dict.fromkeys(LEAN_SCHEMES, 'off')}},
    {'name': 'upper_case_scheme', 'physics': {**CP_NOAHMP, 'config_microp_scheme': 'MP_THOMPSON'}, 'status': 1},
    {'name': 'trailing_blank_scheme', 'physics': {**CP_NOAHMP, 'config_microp_scheme': 'mp_thompson '}},
    {'name': 'leading_blank_scheme', 'physics': {**CP_NOAHMP, 'config_microp_scheme': ' mp_thompson'}, 'status': 1},
    {'name': 'suite_trailing_blank', 'physics': {**CP_NOAHMP, 'config_physics_suite': 'convection_permitting '}},
    # Multiple triggers make MPAS's check order observable, including the first fatal.
    {'name': 'illegal_microp_and_convection', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_microp_scheme': 'x_illegal', 'config_convection_scheme': 'x_illegal'}},
    {'name': 'cld_switch_before_sfclayer_error', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_radt_cld_scheme': 'off', 'config_sfclayer_scheme': 'x_illegal'}},
    {'name': 'lsm_dependency_before_illegal_value', 'status': 1,
     'physics': {**CP_NOAHMP, 'config_pbl_scheme': 'off', 'config_sfclayer_scheme': 'off',
                 'config_lsm_scheme': 'x_illegal'}},
]
# `moist_physics`: all 128 on/off combinations of the seven schemes MPAS's own check lists
# (mpas_atmphys_control.F:310-318), each against the oracle's value. pbl 'bl_mynn' forces the surface layer on, so a
# written 'off' there leaves moist_physics true; lsm on with the surface layer off is MPAS's fatal call.
MOIST_SCHEMES = (
    ('config_microp_scheme', 'mp_thompson'), ('config_convection_scheme', 'cu_grell_freitas'),
    ('config_lsm_scheme', 'sf_noahmp'), ('config_pbl_scheme', 'bl_mynn'),
    ('config_radt_lw_scheme', 'rrtmg_lw'), ('config_radt_sw_scheme', 'rrtmg_sw'),
    ('config_sfclayer_scheme', 'sf_mynn'),
)
MOIST_CASES = []
for bits in range(128):
    on = [bool(bits >> i & 1) for i in range(7)]
    physics = {'config_physics_suite': 'convection_permitting'}
    physics.update({name: value if flag else 'off' for (name, value), flag in zip(MOIST_SCHEMES, on)})
    sfclayer_on = on[6] or on[3]
    MOIST_CASES.append({'name': 'moist_' + ''.join('1' if flag else '0' for flag in on), 'physics': physics,
                        'status': int(on[2] and not sfclayer_on)})
SUITE_CASES += MOIST_CASES
SUITE_CASES = [dict(case, input='suite_' + case['name'], mode='suite', oracle=True) for case in SUITE_CASES]
LEAN_CASES = [
    {'name': 'missing_file', 'input': 'missing_file', 'status': 1, 'check': 'missing'},
    {'name': 'lean_unknown_name', 'input': 'lean_unknown_name', 'status': 1, 'check': 'lean_unknown'},
    {'name': 'cannot_open', 'input': 'cannot_open', 'status': 1, 'check': 'cannot_open'},
]
NOAH_INSTRUCTION = tuple('ERROR: ' + line for line in (
    'To run lean_mpas, set in &physics:',
    "    config_physics_suite = 'convection_permitting'",
    "    config_lsm_scheme = 'sf_noahmp'",
    'and put NoahmpTable.TBL in the run directory. Noah-MP gives a different forecast from Noah.',
    'A scheme may also be \'off\' where MPAS 8.4.2 defines that (docs/fixes.md, "Scheme sets MPAS cannot run", '
    'lists the combinations that stop).',
))
SCHEME_ORDER = (
    'config_microp_scheme', 'config_convection_scheme', 'config_lsm_scheme', 'config_pbl_scheme',
    'config_gwdo_scheme', 'config_radt_cld_scheme', 'config_radt_lw_scheme', 'config_radt_sw_scheme',
    'config_sfclayer_scheme',
)


def allowed_schemes(name):
    # 'off' beside the one scheme lean has; the cloud scheme also has cld_incidence
    extra = ["'cld_incidence'"] if name == 'config_radt_cld_scheme' else []
    return ' or '.join([f"'{LEAN_SCHEMES[name]}'", *extra, "'off'"])


def scheme_errors(values, suite='convection_permitting'):
    return tuple(f"ERROR: {name} = '{values[name]}' (config_physics_suite = '{suite}'): "
                 f"lean_mpas supports only {allowed_schemes(name)}"
                 for name in SCHEME_ORDER if name in values)


def option_validation_cases():
    if not gen_namelist.REGISTRY.exists():
        return []
    rows = gen_namelist.load_table()['options']
    cases = []
    for record in gen_namelist.read_registry():
        for option in record.options:
            row = rows[option.name]
            if row['status'] == 'warn':
                cases.append({'name': 'warn_' + option.name, 'settings': {option.name: '.true.'},
                              'problems': 0, 'expected': (
                                  f'WARNING: {option.name} = .true.: lean_mpas does not write this '
                                  'printout; the run goes on without it',)})
            if row['status'] != 'fixed':
                continue
            allowed = gen_namelist.allowed(option, row)
            if option.type == 'logical':
                literal = '.false.' if allowed[0] else '.true.'
                rendered = literal
            elif option.type == 'integer':
                literal = rendered = str(max(allowed) + 1)
            elif option.type == 'real':
                literal, rendered = '1000.0', '1000.00'
            else:
                literal = rendered = "'x_unsupported'"
            cases.append({'name': 'fixed_' + option.name, 'settings': {option.name: literal},
                          'expected': (f'ERROR: {option.name} = {rendered}: lean_mpas supports only '
                                       + gen_namelist.allowed_text(option, row),)})
    return cases


OFF = "'off'"
ALL_BUT_MICROP = 'convection, pbl, sfclayer, lsm, radt_lw, radt_sw'
MICROP_OFF_TAIL = (" on: MPAS 8.4.2 may finish such a run, but its microphysics-to-physics copy then reads cloud species it has "
           "never set (mpas_atmphys_interface.F:316-335), so its numbers come from memory outside its arrays. "
           "Set every scheme 'off' (config_physics_suite = 'none'), or keep the microphysics.")


def microp_off(listed):
    return "ERROR: config_microp_scheme = 'off' with " + listed + MICROP_OFF_TAIL


CONV_WITHOUT_PBL = ("ERROR: config_convection_scheme = 'cu_grell_freitas' with config_pbl_scheme = 'off': MPAS 8.4.2 stops there with a "
      "segmentation fault in its first step: its convection reads PBL tendencies it has not allocated "
      "(mpas_atmphys_driver_convection.F:695-745). Set config_convection_scheme = 'off' too, or keep the PBL.")
GWDO_WITHOUT_PBL = ("ERROR: config_gwdo_scheme = 'bl_ysu_gwdo' with config_pbl_scheme = 'off': MPAS 8.4.2 stops there with a "
      "segmentation fault in its first step: its gravity-wave drag writes PBL tendencies it has not allocated "
      "(mpas_atmphys_driver_gwdo.F:525-546). Set config_gwdo_scheme = 'off' too, or keep the PBL.")
SFCLAYER_OFF_WITH_MYNN = ("ERROR: config_sfclayer_scheme = 'off' with config_pbl_scheme = 'bl_mynn': MPAS 8.4.2 turns the surface layer "
      "back to sf_mynn (mpas_atmphys_control.F:280-281) after leaving its fields unallocated (Registry.xml:488-490), "
      "and stops with a segmentation fault in its first step. "
      "Leave config_sfclayer_scheme at 'suite' or 'sf_mynn'.")
SW_OFF_LW_ON = ("ERROR: config_radt_sw_scheme = 'off' with config_radt_lw_scheme = 'rrtmg_lw': MPAS 8.4.2 stops there with a "
      "segmentation fault in its first physics step: its longwave driver writes xlat_p, which only the shortwave driver "
      "allocates for RRTMG (mpas_atmphys_driver_radiation_lw.F:162,519; mpas_atmphys_driver_radiation_sw.F:125). "
      "Set both radiation schemes 'off', or both on.")
RANGE = "ERROR: config_apvm_upwinding = {}: lean_mpas supports only 0.0 to 1.0 (the range of MPAS's Registry)"
# The 'off' sets MPAS 8.4.2 runs, and option values it accepts: no problem at all.
OPTION_CASES = [
    {'name': name, 'settings': settings, 'problems': 0, 'expected': ()}
    for name, settings in (
        ('off_conv', {'config_convection_scheme': OFF}),
        ('off_gwdo', {'config_gwdo_scheme': OFF}),
        ('off_lw', {'config_radt_lw_scheme': OFF}),
        ('off_rad', {'config_radt_lw_scheme': OFF, 'config_radt_sw_scheme': OFF}),
        ('off_pbl', {'config_pbl_scheme': OFF, 'config_convection_scheme': OFF, 'config_gwdo_scheme': OFF}),
        ('off_lsm', {'config_lsm_scheme': OFF}),
        ('off_sfc', {'config_sfclayer_scheme': OFF, 'config_lsm_scheme': OFF, 'config_pbl_scheme': OFF,
                     'config_convection_scheme': OFF, 'config_gwdo_scheme': OFF}),
        ('apvm_half', {'config_apvm_upwinding': '0.5'}),
        ('apvm_one', {'config_apvm_upwinding': '1.0'}),
        ('microp_re_false', {'config_microp_re': '.false.'}),
    )
] + [
    # The combinations MPAS 8.4.2 does not define (it crashes or reads unset memory) are refused, in this order:
    # microphysics off with other physics on, surface layer off with MYNN, convection or GWDO without the PBL,
    # shortwave off with longwave on.
    {'name': 'microp_off', 'settings': {'config_microp_scheme': OFF}, 'problems': 1, 'expected': (microp_off(ALL_BUT_MICROP),)},
    {'name': 'microp_off_lists_the_rest', 'settings': {'config_microp_scheme': OFF, 'config_convection_scheme': OFF,
                                           'config_lsm_scheme': OFF},
     'problems': 1, 'expected': (microp_off('pbl, sfclayer, radt_lw, radt_sw'),)},
    {'name': 'microp_off_radiation_only', 'settings': {
        'config_microp_scheme': OFF, 'config_convection_scheme': OFF, 'config_pbl_scheme': OFF,
        'config_sfclayer_scheme': OFF, 'config_lsm_scheme': OFF, 'config_gwdo_scheme': OFF},
     'problems': 1, 'expected': (microp_off('radt_lw, radt_sw'),)},
    {'name': 'sfclayer_off_with_mynn', 'settings': {'config_sfclayer_scheme': OFF}, 'problems': 1, 'expected': (SFCLAYER_OFF_WITH_MYNN,)},
    {'name': 'microp_off_then_sfclayer_off', 'settings': {'config_microp_scheme': OFF, 'config_sfclayer_scheme': OFF},
     'problems': 2, 'expected': (microp_off(ALL_BUT_MICROP), SFCLAYER_OFF_WITH_MYNN)},
    {'name': 'sfclayer_rule_not_for_suite_none', 'settings': {
        'config_physics_suite': "'none'", 'config_lsm_scheme': None, 'config_pbl_scheme': "'bl_mynn'"},
     'problems': 1, 'expected': (microp_off('pbl, sfclayer'),)},
    {'name': 'pbl_off', 'settings': {'config_pbl_scheme': OFF}, 'problems': 2, 'expected': (CONV_WITHOUT_PBL, GWDO_WITHOUT_PBL)},
    {'name': 'convection_without_pbl', 'settings': {'config_pbl_scheme': OFF, 'config_gwdo_scheme': OFF},
     'problems': 1, 'expected': (CONV_WITHOUT_PBL,)},
    {'name': 'gwdo_without_pbl', 'settings': {'config_pbl_scheme': OFF, 'config_convection_scheme': OFF},
     'problems': 1, 'expected': (GWDO_WITHOUT_PBL,)},
    {'name': 'sw_off_lw_on', 'settings': {'config_radt_sw_scheme': OFF}, 'problems': 1, 'expected': (SW_OFF_LW_ON,)},
    {'name': 'apvm_range_high', 'settings': {'config_apvm_upwinding': '1.5'}, 'problems': 1,
     'expected': (RANGE.format('1.50000'),)},
    {'name': 'apvm_range_negative', 'settings': {'config_apvm_upwinding': '-0.5'}, 'problems': 1,
     'expected': (RANGE.format('-0.500000'),)},
]
VALIDATE_CASES = [
    {'name': 'ours', 'problems': 0, 'expected': ()},
    {'name': 'dynamics_frame', 'problems': 0, 'expected': ()},
    # MPAS's own apvm 0.5 and microp_re false are accepted; its five default schemes stop
    {'name': 'mpas_default', 'input': 'mpas_default', 'problems': 5, 'expected': (
        *scheme_errors({
            'config_microp_scheme': 'mp_wsm6', 'config_convection_scheme': 'cu_ntiedtke',
            'config_lsm_scheme': 'sf_noah', 'config_pbl_scheme': 'bl_ysu',
            'config_sfclayer_scheme': 'sf_monin_obukhov_rev',
        }, 'mesoscale_reference'), *NOAH_INSTRUCTION,
    )},
    *option_validation_cases(),
    {'name': 'noah', 'settings': {'config_lsm_scheme': None},
     'expected': (*scheme_errors({'config_lsm_scheme': 'sf_noah'}), *NOAH_INSTRUCTION)},
    {'name': 'suite_none_noahmp', 'settings': {'config_physics_suite': "'none'",
                                               **{name: f"'{LEAN_SCHEMES[name]}'" for name in SCHEME_ORDER},
                                               'config_microp_scheme': "'mp_kessler'"},
     'problems': 1, 'expected': (*scheme_errors({'config_microp_scheme': 'mp_kessler'}, 'none'), *NOAH_INSTRUCTION)},
    {'name': 'suite_none', 'settings': {'config_physics_suite': "'none'", 'config_lsm_scheme': None},
     'problems': 0, 'expected': ()},
    *[{'name': name, 'settings': {option: f"'{value}'" for option, value in values.items()},
       'problems': len(values), 'expected': scheme_errors(values)}
      for name, values in (
          ('kessler', {'config_microp_scheme': 'mp_kessler'}),
          ('kain_fritsch', {'config_convection_scheme': 'cu_kain_fritsch'}),
          ('ysu_monin', {'config_pbl_scheme': 'bl_ysu', 'config_sfclayer_scheme': 'sf_monin_obukhov'}),
          ('ugwp', {'config_gwdo_scheme': 'bl_ugwp_gwdo'}),
          ('cld_thompson', {'config_radt_cld_scheme': 'cld_fraction_thompson'}),
          ('cam', {'config_radt_lw_scheme': 'cam_lw', 'config_radt_sw_scheme': 'cam_sw'}),
      )],
    # MPAS switches a cloud scheme 'off' to cld_incidence, which lean runs
    {'name': 'cld_off', 'settings': {'config_radt_cld_scheme': "'off'"}, 'problems': 0,
     'before_errors': ('', '     config_radt_cld_scheme is not set for radiation calculation',
                       '     switch calculation of cloud fraction to config_radt_cld_scheme = cld_incidence'),
     'expected': ()},
    {'name': 'cld_incidence', 'settings': {'config_radt_cld_scheme': "'cld_incidence'"}, 'problems': 0, 'expected': ()},
    *OPTION_CASES,
    {'name': 'rk4', 'settings': {'config_time_integration': "'RK4'"}, 'expected': (
        'ERROR: Unknown time integration option RK4', "ERROR: Currently, only 'SRK3' is supported.",
    )},
    {'name': 'positive_definite', 'settings': {'config_positive_definite': '.true.'},
     'problems': 0, 'mpas_warnings': ('config_positive_definite',), 'expected': ()},
    {'name': 'epssm', 'settings': {'config_epssm': '0.1'},
     'problems': 0, 'mpas_warnings': ('config_epssm',), 'expected': ()},
    {'name': 'flavor_mismatch', 'settings': {'config_lean_flavor': "'mpas'"}, 'expected': (
        "ERROR: config_lean_flavor = 'mpas', but this executable is the 'lean' flavor: rebuild with --flavor mpas",
    )},
    {'name': 'flavor_unknown', 'settings': {'config_lean_flavor': "'Lean'"}, 'expected': (
        "ERROR: config_lean_flavor = 'Lean': the flavors are 'mpas' and 'lean'",
    )},
    *[{'name': 'flavor_match_' + flavor, 'settings': {'config_lean_flavor': f"'{flavor}'"},
       'args': (flavor,), 'problems': 0, 'expected': ()} for flavor in ('lean', 'mpas')],
    {'name': 'several', 'settings': {'config_horiz_mixing': "'2d_fixed'", 'config_microp_scheme': "'mp_wsm6'",
                                     'config_lean_flavor': "'mpas'"}, 'problems': 3, 'expected': (
        "ERROR: config_horiz_mixing = '2d_fixed': lean_mpas supports only '2d_smagorinsky'",
        *scheme_errors({'config_microp_scheme': 'mp_wsm6'}),
        "ERROR: config_lean_flavor = 'mpas', but this executable is the 'lean' flavor: rebuild with --flavor mpas",
    )},
    # Exercise all validation stages together, including warnings between errors.
    {'name': 'warning_order', 'settings': {
        'config_time_integration': "'RK4'", 'config_positive_definite': '.true.', 'config_epssm': '0.1',
        'config_horiz_mixing': "'2d_fixed'", 'config_print_detailed_minmax_vel': '.true.',
        'config_microp_scheme': "'mp_wsm6'", 'config_lean_flavor': "'mpas'",
    }, 'problems': 4, 'expected': (
        "ERROR: config_horiz_mixing = '2d_fixed': lean_mpas supports only '2d_smagorinsky'",
        *scheme_errors({'config_microp_scheme': 'mp_wsm6'}),
        "ERROR: config_lean_flavor = 'mpas', but this executable is the 'lean' flavor: rebuild with --flavor mpas",
    ), 'time_errors': ('ERROR: Unknown time integration option RK4',
                      "ERROR: Currently, only 'SRK3' is supported.")},
    {'name': 'trimmed', 'settings': {'config_physics_suite': "'convection_permitting '",
                                    'config_lsm_scheme': "'sf_noahmp '",
                                    'config_time_integration': "'SRK3 '", 'config_lean_flavor': "'lean '"},
     'problems': 0, 'expected': ()},
    {'name': 'blank_flavor', 'settings': {'config_lean_flavor': "'   '"}, 'problems': 0, 'expected': ()},
    {'name': 'read_fatal', 'input': 'lean_unknown_name', 'status': 1, 'check': 'lean_unknown'},
    {'name': 'suite_fatal', 'settings': {'config_physics_suite': "'convection_allowing'",
                                        'config_time_integration': "'RK4'", 'config_lean_flavor': "'mpas'"},
     'problems': 0, 'status': 1, 'expected': (
        'ERROR:', 'ERROR: ------------------------------ FATAL CALLED ------------------------------',
        "ERROR: Unrecognized choice of physics suite: config_physics_suite = 'convection_allowing'",
        'CRITICAL ERROR: MPAS core_physics abort',
    )},
]
VALIDATE_CASES = [dict(case, name='validate_' + case['name'], mode='validate',
                       input=case.get('input', 'validate_' + case['name']),
                       status=case.get('status', int(case.get('problems', 1) > 0)))
                  for case in VALIDATE_CASES]
MUTANTS = [
    {'name': 'dt_default', 'case': 'empty',
     'before': 'config_dt = 720.000000', 'after': 'config_dt = 721.000000'},
    {'name': 'no_rewind', 'case': 'reversed', 'routine': 'read_damping',
     'before': '  rewind(nml_unit)\n', 'after': ''},
    {'name': 'ntiedtke', 'case': 'mesoscale_default', 'source': 'src/run/lean_namelist.f90',
     'routine': 'resolve_suite',
     'before': "cfg%config_convection_scheme = 'cu_ntiedtke'",
     'after': "cfg%config_convection_scheme = 'cu_tiedtke'"},
    {'name': 'no_mynn_override', 'case': 'mynn_forces_sf_mynn', 'source': 'src/run/lean_namelist.f90',
     'routine': 'resolve_suite',
     'before': "   cfg%config_sfclayer_scheme = 'sf_mynn'\n", 'after': ''},
    {'name': 'mix_full', 'case': 'validate_fixed_config_mix_full', 'routine': 'check_options',
     'before': 'cfg%config_mix_full .eqv. .true.', 'after': 'cfg%config_mix_full .eqv. .false.'},
    # each scheme-set check made to fail by a one-line change
    {'name': 'moist_ignores_sfclayer', 'case': 'moist_0000001', 'source': 'src/run/lean_namelist.f90',
     'routine': 'resolve_suite',
     'before': "cfg%config_radt_sw_scheme == 'off' .and. &\n                             cfg%config_sfclayer_scheme == 'off')",
     'after': "cfg%config_radt_sw_scheme == 'off' .and. &\n                             .true.)"},
    {'name': 'convection_check_inverted', 'case': 'validate_convection_without_pbl', 'source': 'src/run/lean_namelist.f90',
     'routine': 'check_scheme_set', 'before': "if (conv_on .and. pbl_off)", 'after': "if (.not. conv_on .and. pbl_off)"},
    {'name': 'sw_lw_check_dropped', 'case': 'validate_sw_off_lw_on', 'source': 'src/run/lean_namelist.f90',
     'routine': 'check_scheme_set', 'before': 'if (lw_on .and. .not. sw_on) then', 'after': 'if (.false.) then'},
    {'name': 'apvm_range_open', 'case': 'validate_apvm_range_high', 'routine': 'check_options',
     'before': 'cfg%config_apvm_upwinding <= 1.0', 'after': 'cfg%config_apvm_upwinding <= 2.0'},
    {'name': 'suite_clause_dropped', 'case': 'validate_suite_none_noahmp',
     'source': 'src/run/lean_namelist.f90', 'routine': 'validate',
     'before': "(trim(cfg%config_physics_suite) /= 'convection_permitting' .or. &", 'after': '(.false. .or. &'},
]


def run_command(command, work, label):
    result = subprocess.run(command, cwd=work, capture_output=True, text=True)
    (work / f'{label}.stdout').write_text(result.stdout, encoding='utf-8')
    (work / f'{label}.stderr').write_text(result.stderr, encoding='utf-8')
    return result


def require_success(result, label):
    if result.returncode != 0:
        lines = (result.stderr or result.stdout).splitlines()
        detail = f': {lines[0]}' if lines else ''
        raise RuntimeError(f'{label} exited {result.returncode}{detail}')


def build_program(work, fc, options, defines, name, sources):
    folder = work / name
    folder.mkdir()
    objects = []
    for source in [KINDS, *sources]:
        obj = folder / f'{source.stem}.o'
        command = [fc, *options, *[f'-D{value}' for value in defines], '-c', str(source), '-o', str(obj)]
        result = run_command(command, folder, f'{source.stem}.compile')
        require_success(result, f'compile {source.relative_to(ROOT)}')
        objects.append(str(obj))
    program = folder / name
    result = run_command([fc, *options, *objects, '-o', str(program)], folder, 'link')
    require_success(result, f'link {name}')
    return program


def output_difference(result, expected, status=0):
    if result.returncode != status:
        return f'exit status {result.returncode}, expected {status}'
    actual = [line.rstrip() for line in result.stdout.splitlines()]
    expected = [line.rstrip() for line in expected]
    for number, (got, want) in enumerate(zip_longest(actual, expected), 1):
        if got != want:
            return f'line {number}: expected {want!r}, got {got!r}'
    return None


def extract_piece(path, first, last):
    lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    starts = [i for i, line in enumerate(lines) if line.rstrip('\r\n') == first]
    ends = [i for i, line in enumerate(lines) if line.rstrip('\r\n') == last]
    if len(starts) != 1 or len(ends) != 1 or starts[0] >= ends[0]:
        raise RuntimeError(f'{path.name}: extraction boundaries must each match exactly once: {first!r}, {last!r}')
    return ''.join(lines[starts[0]:ends[0] + 1])


def build_oracle(work, fc, options, defines):
    inc = work / 'inc'
    inc.mkdir()
    result = run_command([
        'cpp', '-P', '-traditional-cpp', '-DDO_PHYSICS', '-DDO_CHEMISTRY', '-DCORE_ATMOSPHERE',
        '-UMPAS_USE_MUSICA', '-UMPAS_CAM_DYCORE', str(MPAS / 'core_atmosphere/Registry.xml'),
    ], work, 'registry.cpp')
    require_success(result, 'cpp Registry.xml')
    registry = work / 'Registry_processed.xml'
    registry.write_text(result.stdout, encoding='utf-8')

    cwork = work / 'c_tools'
    cwork.mkdir()
    ezxml = MPAS / 'external/ezxml'
    sources = [ezxml / 'ezxml.c']
    sources += [MPAS / 'tools/registry' / f'{name}.c'
                for name in ('parse', 'dictionary', 'gen_inc', 'fortprintf', 'utility')]
    sources += [MPAS / 'tools/input_gen' / f'{name}.c' for name in ('namelist_gen', 'test_functions')]
    for source in sources:
        result = run_command(['cc', '-w', f'-I{ezxml}', '-c', str(source), '-o', str(cwork / f'{source.stem}.o')],
                             cwork, f'{source.stem}.compile')
        require_success(result, f'compile {source.name}')
    for name, objects in (
        ('parse', ('parse', 'dictionary', 'gen_inc', 'fortprintf', 'utility', 'ezxml')),
        ('namelist_gen', ('namelist_gen', 'test_functions', 'ezxml')),
    ):
        result = run_command(['cc', '-w', f'-I{ezxml}', *[str(cwork / f'{obj}.o') for obj in objects],
                              '-o', str(cwork / name)], cwork, f'{name}.link')
        require_success(result, f'link {name}')
    result = run_command([str(cwork / 'parse'), str(registry), '-DDO_PHYSICS'], inc, 'parse')
    require_success(result, 'parse Registry_processed.xml')
    result = run_command([str(cwork / 'namelist_gen'), str(registry), str(work / 'mpas_default.nml'),
                          'in_defaults=true'], work, 'namelist_gen')
    require_success(result, 'namelist_gen Registry_processed.xml')

    log_source = MPAS / 'framework/mpas_log.F'
    pieces = [extract_piece(log_source, first, last) for first, last in (
        ('   function mpas_log_escape_dollars(str) result(escape_str)',
         '   end function mpas_log_escape_dollars'),
        ('   subroutine log_expand_string(inString, outString, intArgs, int8Args, logicArgs, realArgs)',
         '   end subroutine log_expand_string'),
    )]
    (inc / 'log_pieces.inc').write_text('\n'.join(pieces), encoding='utf-8')
    physics = MPAS / 'core_atmosphere/physics'
    pieces = [extract_piece(physics / 'mpas_atmphys_utilities.F', first, last) for first, last in (
        (' subroutine physics_message(str)', ' end subroutine physics_message'),
        (' subroutine physics_error_fatal(str)', ' end subroutine physics_error_fatal'),
    )]
    pieces.append(extract_piece(physics / 'mpas_atmphys_control.F',
                                ' subroutine physics_namelist_check(configs)',
                                ' end subroutine physics_namelist_check'))
    (inc / 'physics_pieces.inc').write_text('\n'.join(pieces), encoding='utf-8')
    return build_program(work, fc, [*options, f'-I{inc}'], defines, 'oracle', [
        ROOT / 'tests/namelist/oracle_stubs.F90', ROOT / 'tests/namelist/oracle.F90',
    ])


def prepare_inputs(work):
    folder = work / 'inputs'
    folder.mkdir()
    ours = ROOT / 'reference/config/namelist.atmosphere'
    text = ours.read_text(encoding='utf-8')
    records = re.findall(r'^&.*?^/[ \t]*(?:\n|$)', text, re.MULTILINE | re.DOTALL)
    if ''.join(records).strip() != text.strip():
        raise RuntimeError('ours: expected complete namelist records for reversed fixture')
    variants = {
        'crlf': text.replace('\n', '\r\n'),
        'reversed': ''.join(reversed(records)),
        'with_lean_record': text + "&lean config_lean_flavor = 'lean', config_lean_strict_inputs = true /\n",
        'lean_unknown_name': text + "&lean config_lean_flavour = 'lean' /\n",
        'cannot_open': text,
    }
    for case in SUITE_CASES:
        variants[case['input']] = ('&physics\n' + ''.join(
            f" {option} = '{value}'\n" for option, value in case['physics'].items()) + '/\n')
    if VALIDATE_CASES and gen_namelist.REGISTRY.exists():
        option_records = {option.name: record.name
                          for record in [*gen_namelist.read_registry(), gen_namelist.LEAN_RECORD]
                          for option in record.options}
        for case in VALIDATE_CASES:
            if case['input'].startswith('validate_'):
                variants[case['input']] = validation_input(text, case.get('settings', {}), option_records)
    inputs = {'ours': ours, 'mpas_default': work / 'mpas_default.nml', 'missing_file': work / 'absent.nml'}
    for name in ('empty', 'odd_syntax', 'unterminated', 'unknown_name', 'bad_value', 'bad_value_at_end'):
        inputs[name] = ROOT / 'tests/namelist/cases' / f'{name}.nml'
    for name, content in variants.items():
        path = work / 'noperm.nml' if name == 'cannot_open' else folder / f'{name}.nml'
        path.write_bytes(content.encode('utf-8'))
        inputs[name] = path
    return inputs


def validation_input(text, settings, option_records):
    for option, literal in settings.items():
        assignment = re.compile(r'^[ \t]*' + re.escape(option) + r'\s*=.*\n', re.MULTILINE | re.IGNORECASE)
        replacement = '' if literal is None else f'    {option} = {literal}\n'
        text, count = assignment.subn(replacement, text)
        if count > 1:
            raise RuntimeError(f'ours: duplicate assignment to {option}')
        if count:
            continue
        if literal is None:
            raise RuntimeError(f'ours: cannot remove absent option {option}')
        record = option_records[option]
        header = re.search(r'^&' + re.escape(record) + r'[ \t]*\n', text, re.MULTILINE | re.IGNORECASE)
        if header:
            text = text[:header.end()] + replacement + text[header.end():]
        else:
            text += f'&{record}\n{replacement}/\n'
    return text


def run_reader(program, case, path, work, label):
    output = work / f'{label}.out'
    result = run_command([str(program), case.get('mode', 'read'), str(path), str(output),
                          *case.get('args', ())], program.parent, label)
    if not output.is_file():
        raise RuntimeError(f'{label}: exit {result.returncode}, no output file')
    return subprocess.CompletedProcess(result.args, result.returncode, output.read_text(encoding='utf-8'), result.stderr)


def mpas_lines(result):
    return [line for line in result.stdout.splitlines()
            if not line.startswith(('ERROR: namelist read: ', 'config_lean_'))]


def dump_lines(result):
    return [line.rstrip() for line in result.stdout.splitlines() if re.match(r'^\w+ = ', line)]


def compare_readers(result, reference):
    filtered = subprocess.CompletedProcess(result.args, result.returncode, '\n'.join(mpas_lines(result)), result.stderr)
    return output_difference(filtered, reference.stdout.splitlines(), reference.returncode)


def check_read(case, result, reference, oracle_results, compiler):
    case = {**case, **case.get(compiler, {})}
    status = case.get('status', 0)
    if reference.returncode != status:
        return f'oracle exit status {reference.returncode}, expected {status}'
    difference = compare_readers(result, reference)
    if difference:
        return difference
    lines = result.stdout.splitlines()
    oracle_lines = reference.stdout.splitlines()
    if 'blocks' in case:
        blocks = sum(line.startswith(' *** Encountered an issue while attempting to read namelist record ')
                     for line in oracle_lines)
        if blocks != case['blocks']:
            return f'oracle "Encountered an issue" blocks {blocks}, expected {case["blocks"]}'
    for line in case.get('used_values', ()):
        if ' ' + line not in oracle_lines:
            return f'oracle missing used-value line {line!r}'
    for line in case.get('dump', ()):
        if line not in dump_lines(reference):
            return f'oracle missing option {line!r}'
    for line in case.get('lean_dump', ()):
        if line not in dump_lines(result):
            return f'lean missing option {line!r}'
    if 'same_dump' in case:
        if dump_lines(reference) != dump_lines(oracle_results[case['same_dump']]):
            return f'oracle dump differs from {case["same_dump"]}'
    if '\0' in result.stdout:
        return 'lean output holds NUL bytes'
    if case.get('read_error'):
        errors = [line for line in lines if line.startswith('ERROR: namelist read: ')]
        if len(errors) != 1:
            return f"expected exactly one 'namelist read:' line, got {len(errors)}"
        if compiler == 'gfortran' and case.get('gfortran_cause', '') not in errors[0]:
            return f"the 'namelist read:' line does not name {case['gfortran_cause']}"
    return None


def check_lean(case, result, path):
    if result.returncode != case.get('status', 0):
        return f'exit status {result.returncode}, expected {case.get("status", 0)}'
    first = f' Reading namelist from file {path}'
    if case['check'] == 'missing':
        return output_difference(result, [first, f'CRITICAL ERROR: Namelist file {path} does not exist.'], 1)
    lines = result.stdout.splitlines()
    if case['check'] == 'cannot_open':
        prefix = f'CRITICAL ERROR: Cannot open namelist file {path}: '
        if len(lines) != 2 or lines[0] != first or not lines[1].startswith(prefix) or not lines[1][len(prefix):].strip():
            return "expected 'Reading namelist from file', then 'Cannot open' with a non-empty reason"
    elif case['check'] == 'lean_unknown':
        if (len(lines) < 2 or not lines[-2].startswith('ERROR: namelist read: ')
                or lines[-1] != 'CRITICAL ERROR: Error while reading namelist record lean.'):
            return "expected 'namelist read:', then the record lean error as the last two lines"
    return None


def dynamics_warnings(option):
    source = (MPAS / 'core_atmosphere/dynamics/mpas_atm_time_integration.F').read_text(encoding='utf-8')
    condition, count = {
        'config_positive_definite': ('if (config_positive_definite) then', 4),
        'config_epssm': ('if (config_epssm /= 0.0_RKIND) then', 9),
    }[option]
    start = source.index(condition)
    block = source[start:source.index('end if', start)]
    calls = re.findall(r"call mpas_log_write\('((?:''|[^'])*)'(.*?)\)", block, re.DOTALL)
    if len(calls) != count + 1 or calls[-1] != ('', ''):
        raise RuntimeError(f'{option}: expected {count} MPAS warnings followed by an empty OUT line')
    if any('messageType=MPAS_LOG_WARN' not in args.replace(' ', '') for _, args in calls[:-1]):
        raise RuntimeError(f'{option}: unexpected MPAS warning level')
    return tuple('WARNING: ' + text.replace("''", "'") for text, _ in calls[:-1]) + ('',)


def check_validate(case, result, path):
    if case.get('check'):
        difference = check_lean(case, result, path)
        if difference:
            return difference
        if 'Setting up physics suite' in result.stdout or 'lean_mpas stops:' in result.stdout:
            return 'validation continued after a fatal read'
        return None
    expected = [*case.get('time_errors', ())]
    for option in case.get('mpas_warnings', ()):
        expected.extend(dynamics_warnings(option))
    expected.extend(case['expected'])
    problems = case.get('problems', 1)
    if problems:
        expected.append(f'CRITICAL ERROR: lean_mpas stops: {problems} namelist problem(s) listed above; '
                        'docs/namelist.md lists what lean_mpas does with each option')
    lines = [line.rstrip() for line in result.stdout.splitlines()]
    if case['name'] == 'validate_dynamics_frame':
        dump = next(i for i, line in enumerate(lines) if re.match(r'^\w+ = ', line))
        frame = ['', ' Checking consistency of dynamics settings...',
                 '  ----- done checking dynamics settings -----', '']
        if lines[dump - 4:dump] != frame:
            return f'expected dynamics frame before option dump: {lines[dump - 4:dump]!r}'
    diagnostics = [line for i, line in enumerate(lines)
                   if line.startswith(('WARNING:', 'ERROR:', 'CRITICAL ERROR:'))
                   or (not line and i > 0 and lines[i - 1].startswith('WARNING:'))]
    output = '\n'.join(diagnostics) + ('\n' if diagnostics else '')
    filtered = subprocess.CompletedProcess(result.args, result.returncode, output, result.stderr)
    difference = output_difference(filtered, expected, case['status'])
    if difference:
        return difference
    if 'before_errors' in case:
        block = '\n'.join(case['before_errors']) + '\n'
        output = '\n'.join(lines)
        if block not in output or (case['expected'] and output.index(block) >= output.index(case['expected'][0])):
            return 'expected the three cloud-scheme lines before the first validation message'
    return None


def build_mutant(work, fc, options, defines, mutant):
    original = ROOT / mutant.get('source', 'src/run/lean_namelist_options.f90')
    text = original.read_text(encoding='utf-8')
    start, end = 0, len(text)
    if 'routine' in mutant:
        start = text.index(f' subroutine {mutant["routine"]}(')
        end = text.index(' end subroutine', start)
    piece = text[start:end]
    if piece.count(mutant['before']) != 1:
        raise RuntimeError(f'{mutant["name"]}: mutation must match exactly once')
    text = text[:start] + piece.replace(mutant['before'], mutant['after']) + text[end:]
    folder = work / f'mutation_{mutant["name"]}'
    folder.mkdir()
    source = folder / original.name
    source.write_text(text, encoding='utf-8')
    sources = [ROOT / 'src/run/lean_messages.f90', ROOT / 'src/run/lean_namelist_options.f90',
               ROOT / 'src/run/lean_namelist.f90', ROOT / 'tests/namelist/lean_main.f90']
    return build_program(work, fc, options, defines, f'mutant_{mutant["name"]}', [
        source if path == original else path for path in sources
    ])


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc', choices=('gfortran', 'nvfortran'), default='gfortran')
    parser.add_argument('--profile', choices=('strict', 'debug'), default='strict')
    parser.add_argument('--keep', action='store_true', help='keep build files after a passing run')
    args = parser.parse_args(argv)
    fc = shutil.which(args.fc)
    if fc is None:
        print(f'missing tool: {args.fc}', file=sys.stderr)
        return 2

    options, defines = unit_flags(BuildConfig('cpu', args.fc, args.profile), 'serial_services', 'base')
    work = ROOT / 'build/namelist' / f'{args.fc}-{args.profile}'
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)

    cases = [
        {'name': 'messages', 'sources': [ROOT / 'tests/namelist/messages_main.f90'],
         'expected': MESSAGES_EXPECTED},
        *READ_CASES, *SUITE_CASES, *LEAN_CASES, *VALIDATE_CASES,
    ]
    missing_reference = not (MPAS / 'core_atmosphere/Registry.xml').is_file()
    skipped = [case for case in cases if (case.get('oracle') or case.get('mode') == 'validate') and missing_reference]
    for case in skipped:
        print(f'SKIP {case["name"]}: MPAS reference sources absent')
    cases = [case for case in cases if case not in skipped]
    read_cases = {case['name']: case for case in cases if case.get('oracle')}
    input_cases = {case['name']: case for case in cases if 'input' in case}
    mutants = [mutant for mutant in MUTANTS if mutant['case'] in input_cases]
    oracle_results = {}
    setup_error = None
    if any('input' in case for case in cases):
        try:
            inputs = prepare_inputs(work)
            lean = build_program(work, fc, options, defines, 'lean_main', [
                ROOT / 'src/run/lean_messages.f90', ROOT / 'src/run/lean_namelist_options.f90',
                ROOT / 'src/run/lean_namelist.f90', ROOT / 'tests/namelist/lean_main.f90',
            ])
            if read_cases:
                oracle = build_oracle(work, fc, options, defines)
        except FileNotFoundError as exc:
            print(f'missing tool: {exc.filename}', file=sys.stderr)
            return 2
        except RuntimeError as exc:
            setup_error = str(exc)
    failures = 0
    for case in cases:
        name = case['name']
        try:
            if 'input' in case:
                if setup_error:
                    raise RuntimeError(setup_error)
                path = inputs[case['input']]
                if case.get('check') == 'cannot_open':
                    mode = path.stat().st_mode & 0o777
                    try:
                        os.chmod(path, 0)
                        result = run_reader(lean, case, path, work, f'{name}.lean')
                    finally:
                        os.chmod(path, mode)
                else:
                    result = run_reader(lean, case, path, work, f'{name}.lean')
                if case.get('oracle'):
                    reference = run_reader(oracle, case, path, work, f'{name}.oracle')
                    oracle_results[name] = reference
                    difference = check_read(case, result, reference, oracle_results, args.fc)
                elif case.get('mode') == 'validate':
                    difference = check_validate(case, result, path)
                else:
                    difference = check_lean(case, result, path)
            else:
                program = build_program(work, fc, options, defines, name, [
                    ROOT / 'src/run/lean_messages.f90',
                    ROOT / 'src/run/lean_namelist_options.f90',
                    *case['sources'],
                ])
                result = run_command([str(program), *case.get('args', ())], program.parent, 'run')
                difference = output_difference(result, case['expected'], case.get('status', 0))
        except FileNotFoundError as exc:
            print(f'missing tool: {exc.filename}', file=sys.stderr)
            return 2
        except RuntimeError as exc:
            difference = str(exc)
        if difference is None:
            print(f'ok {name}')
        else:
            failures += 1
            print(f'FAIL {name}: {difference}')

    seen = 0
    for mutant in mutants:
        name = mutant['name']
        try:
            if setup_error:
                raise RuntimeError(setup_error)
            case = input_cases[mutant['case']]
            if case.get('oracle'):
                reference = oracle_results[case['name']]
                require_success(reference, f'oracle {case["name"]}')
            program = build_mutant(work, fc, options, defines, mutant)
            result = run_reader(program, case, inputs[case['input']], work, f'mutant_{name}')
            if case.get('mode') != 'validate' or result.returncode != 1:
                # A validate mutant may still stop (ERROR STOP 1) and differ only in its messages.
                require_success(result, f'mutant {name}')
            if case.get('mode') == 'validate':
                difference = check_validate(case, result, inputs[case['input']])
            else:
                difference = compare_readers(result, reference)
            if difference is None:
                raise RuntimeError('mutation was not detected')
            seen += 1
            print(f'ok mutant {name} ({case["name"]}): {difference}')
        except FileNotFoundError as exc:
            print(f'missing tool: {exc.filename}', file=sys.stderr)
            return 2
        except RuntimeError as exc:
            failures += 1
            print(f'FAIL mutant {name}: {exc}')
    if failures:
        print(f'NAMELIST FAIL: {failures} failed checks ({len(cases)} cases, {len(mutants)} mutants)')
        return 1
    print(f'NAMELIST OK: {len(cases)} cases, {seen} mutants seen ({args.fc} {args.profile})')
    if not args.keep:
        shutil.rmtree(work)
        if not any(work.parent.iterdir()):
            work.parent.rmdir()
    return 0


if __name__ == '__main__':
    sys.exit(main())
