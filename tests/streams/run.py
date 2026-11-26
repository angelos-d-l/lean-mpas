#!/usr/bin/env python3
"""Serial streams checks: lean's stream parser, clock and I/O plan, compiled with the model's host flags, against
oracles built from MPAS 8.4.2's own unchanged sources and against the logs of MPAS reference runs."""
import argparse
import re
from pathlib import Path
import shutil
import sys
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from tools.lean.config import BuildConfig, sources, unit_flags
from tests.namelist.run import run_command, require_success, extract_piece, output_difference

MPAS = ROOT / 'reference/mpas_src/src'
HERE = ROOT / 'tests/streams'
KINDS = ROOT / 'third_party/mpas-8.4.2/src/framework/mpas_kind_types.F'
ESMF = ROOT / 'third_party/mpas-8.4.2/src/external/esmf_time_f90'
PINS = {
    'strings': (
        'interval 30:00 -> 0000000000_000:030:000 0 1800 0 1',
        'time 2026-02-31_00:00:00 -> 2026-03-03_00:00:00',
    ),
    'ref_before_start': tuple('2026-11-08_' + t + ' lbc_output' for t in ('07:00:00', '09:00:00', '12:00:00')),
    'ref_after_start': tuple('2026-11-08_' + t + ' output_output' for t in ('00:00:00', '01:00:00', '04:00:00')),
    'odd_dt': tuple('2026-11-08_' + t + ' hourly_output' for t in ('06:30:00', '07:00:07', '08:00:07')),
    'names': (
        'expand -> h.2026-11-08_06.30.15.312.23415.all.nc',
        'expand -> a.23415.23415.46815',
        'expand -> x',
        'expand -> 2026',
        'filename -> lbc.2026-11-08_06.00.00.nc',
        'filename -> lbc.2026-11-08_03.00.00.nc',
        'filename -> lbc.2026-11-08_00.00.00.nc',
        'filename -> mesh.2026-11-08_06.30.15.nc',
        'CRITICAL ERROR: ERROR: mpas_expand_string option $G is not a valid expansion character.',
    ),
}
PREFIXES = {'strings': ('dt 7.2 -> 0 7 19999980 100000000 ',
                        'dt 2.5 -> 0 2 50000000 100000000 ')}


def compile_one(cfg, folder, source, group='physics_support', unit='radar_alias', includes=()):
    options, defines = unit_flags(cfg, unit, group)
    obj = folder / (source.stem + '.o')
    result = run_command([cfg.fc, *options, *['-D' + d for d in defines],
                          *['-I' + str(p) for p in includes], '-c', str(source), '-o', str(obj)],
                         folder, source.stem + '.compile')
    require_success(result, 'compile ' + source.name)
    return obj


def build_clock(cfg, work):
    source = ROOT / 'src/run/lean_clock.f90'
    if not source.is_file():
        raise RuntimeError('missing src/run/lean_clock.f90')
    common = work / 'common'
    common.mkdir()
    objects = [compile_one(cfg, common, KINDS)]
    for path, group in sources(cfg):
        if group == 'esmf_time':
            objects.append(compile_one(cfg, common, ROOT / path, group, Path(path).stem, [ESMF]))
    if MPAS.is_dir():
        log = MPAS / 'framework/mpas_log.F'
        (common / 'log_pieces.inc').write_text(extract_piece(
            log, '   subroutine log_expand_string(inString, outString, intArgs, int8Args, logicArgs, realArgs)',
            '   end subroutine log_expand_string'), encoding='utf-8')
        manager = MPAS / 'framework/mpas_stream_manager.F'
        defines = [line for line in manager.read_text().splitlines() if line.startswith('#define')]
        if len(defines) != 4:
            raise RuntimeError('mpas_stream_manager.F: expected exactly four #define lines')
        filename = extract_piece(manager,
            '    subroutine mpas_build_stream_filename(ref_time, when, filename_interval, filename_template, blockID, filename, ierr) !{{{',
            '    end subroutine mpas_build_stream_filename !}}}')
        (common / 'filename_oracle.F90').write_text(
            '\n'.join(defines) + '\nmodule filename_oracle\n use mpas_timekeeping\n implicit none\ncontains\n'
            + filename + '\nend module\n', encoding='utf-8')
    programs = {}
    for side, files in (
        ('oracle', [HERE / 'clock_stubs.F90', MPAS / 'framework/mpas_string_utils.F',
                    MPAS / 'framework/mpas_timekeeping.F', common / 'filename_oracle.F90', HERE / 'clock_oracle.F90']),
        ('lean', [ROOT / 'src/run' / (stem + '.f90') for stem in
                  ('lean_messages', 'lean_namelist_options', 'lean_namelist', 'lean_clock')]
                 + [HERE / 'clock_main.f90']),
    ):
        if side == 'oracle' and not MPAS.is_dir():
            continue
        folder = work / side
        folder.mkdir()
        includes = [common, MPAS / 'framework']
        side_objects = [compile_one(cfg, folder, path, includes=includes) for path in files]
        options, _ = unit_flags(cfg, 'radar_alias', 'physics_support')
        program = folder / side
        result = run_command([cfg.fc, *options, *map(str, objects + side_objects), '-o', str(program)],
                             folder, 'link')
        require_success(result, 'link ' + side)
        programs[side] = program
    return programs


def lean_expected(case, result):
    expected = [line[2:] for line in case.read_text().splitlines() if line.startswith('= ')]
    actual = result.stdout.splitlines()
    # The runtime supplies iomsg; require its exact stable prefix and a nonempty compiler-specific cause.
    for i, line in enumerate(expected):
        if line.endswith('<iomsg>') and i < len(actual):
            prefix = line[:-len('<iomsg>')]
            if actual[i].startswith(prefix) and actual[i][len(prefix):].strip():
                expected[i] = actual[i]
    return expected


def check_pins(case, lines):
    for pin in PINS.get(case, ()):
        if pin not in lines:
            return 'missing pinned line ' + repr(pin)
    for prefix in PREFIXES.get(case, ()):
        if not any(line.startswith(prefix) for line in lines):
            return 'missing pinned prefix ' + repr(prefix)
    return None


def clock_group(args, work):
    cases = sorted((HERE / 'clock_cases').glob('*.txt'))
    cases += sorted((HERE / 'clock_cases/lean').glob('*.txt'))
    failures = skipped = 0
    programs = None
    try:
        programs = build_clock(BuildConfig('cpu', args.fc, args.profile), work)
    except (RuntimeError, OSError) as exc:
        build_error = str(exc)
    for case in cases:
        name = 'clock/' + case.relative_to(HERE / 'clock_cases').with_suffix('').as_posix()
        difference = None
        if programs is None:
            difference = build_error
        elif case.parent.name != 'lean' and not MPAS.is_dir():
            print('SKIP ' + name + ': reference/mpas_src is absent')
            skipped += 1
            continue
        else:
            if case.parent.name == 'lean':
                for timestamp in case.parent.glob('*.timestamp'):
                    shutil.copyfile(timestamp, programs['lean'].parent / timestamp.name)
            result = run_command([str(programs['lean']), str(case)], programs['lean'].parent,
                                 case.parent.name + '-' + case.stem)
            if case.parent.name == 'lean':
                expected = lean_expected(case, result)
                difference = output_difference(result, expected)
            else:
                reference = run_command([str(programs['oracle']), str(case)], programs['oracle'].parent,
                                        case.stem)
                status = 1 if case.stem == 'names' else 0
                difference = output_difference(reference, reference.stdout.splitlines(), status)
                if difference:
                    difference = 'oracle ' + difference
                else:
                    difference = output_difference(result, reference.stdout.splitlines(), reference.returncode)
                difference = difference or check_pins(case.stem, reference.stdout.splitlines())
        if difference:
            print('FAIL ' + name + ': ' + difference)
            failures += 1
        else:
            print('ok ' + name)
    if failures:
        print(f'STREAMS FAIL: {failures} of {len(cases)} cases')
        return 1
    print(f'STREAMS OK: {len(cases)} cases, {skipped} skipped ({args.fc} {args.profile})')
    return 0


XML_EXPECTED = {
    'syntax_close_only': ['ERROR: In file streams.atmosphere, Found unexpected closing tag "streams" at line 1.',
                          '@ syntax failed'],
    'tree_two_roots': ['@ syntax ok', '@ tree failed: markup outside of root element'],
    # A comment holding '--': MPAS's syntax check passes it, ezxml stops there (external/ezxml/ezxml.c:569-572) and
    # MPAS runs on without the streams after it. lean stops instead of running without them.
    'tree_comment_dashes': ['@ syntax ok', '@ tree failed: unclosed <!--'],
    'tree_unquoted': ['@ syntax ok', '@ tree failed: In file streams.atmosphere, line 2: '
                      'the value of attribute "type" is not in quotes.'],
    'tree_values': ['@ syntax ok', '@ 0 streams',
                    '@ 1 stream name="a&b" filename_template="x$" test="<$>\'\"" '
                    'spaces="x y z" name="second" empty="" unicode="éα"'],
}


def build_parser(cfg, work, streams=False):
    source = ROOT / 'src/run/lean_xml.f90'
    if not source.is_file():
        raise RuntimeError('missing src/run/lean_xml.f90')
    common = work / 'common'
    common.mkdir()
    objects = [compile_one(cfg, common, KINDS)]
    objects += [compile_one(cfg, common, ROOT / 'src/run' / (stem + '.f90'))
                for stem in ('lean_messages', 'lean_xml')]
    objects.append(compile_one(cfg, common, HERE / 'xml_main.f90'))
    options, _ = unit_flags(cfg, 'radar_alias', 'physics_support')
    lean = common / 'xml_main'
    require_success(run_command([cfg.fc, *options, *map(str, objects), '-o', str(lean)], common, 'link'), 'link xml')
    programs = {'lean': lean}
    if MPAS.is_dir():
        oracle = work / 'oracle'
        oracle.mkdir()
        exe = oracle / 'xml_oracle'
        require_success(run_command(['cc', '-w', '-I' + str(MPAS / 'external/ezxml'),
                                     str(MPAS / 'framework/xml_stream_parser.c'),
                                     str(MPAS / 'external/ezxml/ezxml.c'),
                                     str(HERE / 'oracle_stubs.c'), str(HERE / 'oracle_main.c'),
                                     '-o', str(exe)], oracle, 'compile'), 'build xml oracle')
        programs['oracle'] = exe
    return programs


def xml_group(args, work):
    cases = sorted(p for p in (HERE / 'cases').iterdir()
                   if p.name.startswith(('syntax_', 'tree_')))
    programs = None
    try:
        programs = build_parser(BuildConfig('cpu', args.fc, args.profile), work)
    except (RuntimeError, OSError) as exc:
        build_error = str(exc)
    failures = skipped = 0
    for case in cases:
        difference = None
        if programs is None:
            difference = build_error
        elif case.name not in XML_EXPECTED and 'oracle' not in programs:
            print('SKIP xml/' + case.name + ': reference/mpas_src is absent')
            skipped += 1
            continue
        else:
            folders = {}
            for side in programs:
                folder = work / (case.name + '-' + side)
                shutil.copytree(case, folder)
                folders[side] = folder
            result = run_command([str(programs['lean']), 'streams.atmosphere'], folders['lean'], 'run')
            if case.name in XML_EXPECTED:
                difference = output_difference(result, XML_EXPECTED[case.name])
            else:
                oracle = run_command([str(programs['oracle']), 'streams.atmosphere', 'input'], folders['oracle'], 'run')
                lines = oracle.stdout.splitlines()
                before_mesh = lines[1:next((i for i, line in enumerate(lines) if line.startswith('@ mesh')), len(lines))]
                expected = before_mesh + ['@ syntax failed']
                actual = result.stdout.splitlines()
                syntax_end = next((i for i, line in enumerate(actual) if line.startswith('@ syntax')), len(actual)-1)
                difference = output_difference(oracle, lines, 1)
                if not difference:
                    difference = output_difference(result, actual[:syntax_end+1])
                    if actual[:syntax_end+1] != expected:
                        difference = f'expected {expected!r}, got {actual[:syntax_end+1]!r}'
        if difference:
            failures += 1
            print('FAIL xml/' + case.name + ': ' + difference)
        else:
            print('ok xml/' + case.name)
    print(f'STREAMS {"FAIL" if failures else "OK"}: {len(cases)} xml cases, {skipped} skipped ({args.fc} {args.profile})')
    return int(failures != 0)


PACKAGE_ORDER = (
    'mp_kessler_in', 'mp_thompson_in', 'mp_thompson_aers_in', 'mp_wsm6_in',
    'cu_grell_freitas_in', 'cu_kain_fritsch_in', 'cu_ntiedtke_in', 'bl_ysu_in', 'bl_mynn_in',
    'sf_noahmp_in', 'iau', 'limited_area', 'jedi_da', 'no_invariant_stream', 'ugwp_orog_stream',
    'ugwp_ngw_stream', 'ugwp_diags_stream', 'les', 'sfclayer',
)
CP_PACKAGES = {'mp_thompson_in', 'cu_grell_freitas_in', 'bl_mynn_in', 'sf_noahmp_in'}


def build_streams(cfg, work):
    if not (ROOT / 'src/run/lean_streams.f90').is_file():
        raise RuntimeError('missing src/run/lean_streams.f90')
    common = work / 'common'
    common.mkdir()
    objects = [compile_one(cfg, common, KINDS)]
    for path, group in sources(cfg):
        if group == 'esmf_time':
            objects.append(compile_one(cfg, common, ROOT / path, group, Path(path).stem, [ESMF]))
    objects += [compile_one(cfg, common, ROOT / 'src/run' / (stem + '.f90')) for stem in
                ('lean_messages', 'lean_namelist_options', 'lean_namelist', 'lean_clock', 'lean_xml',
                 'lean_stream_registry', 'lean_streams')]
    objects.append(compile_one(cfg, common, HERE / 'streams_main.f90'))
    options, _ = unit_flags(cfg, 'radar_alias', 'physics_support')
    exe = common / 'streams_main'
    require_success(run_command([cfg.fc, *options, *map(str, objects), '-o', str(exe)], common, 'link'), 'link streams')
    programs = {'lean': exe}
    if MPAS.is_dir():
        oracle = work / 'oracle'
        oracle.mkdir()
        program = oracle / 'xml_oracle'
        require_success(run_command(['cc', '-w', '-I' + str(MPAS / 'external/ezxml'),
                                     str(MPAS / 'framework/xml_stream_parser.c'),
                                     str(MPAS / 'external/ezxml/ezxml.c'), str(HERE / 'oracle_stubs.c'),
                                     str(HERE / 'oracle_main.c'), '-o', str(program)], oracle, 'compile'), 'build parser oracle')
        programs['oracle'] = program
    return programs


def streams_group(args, work):
    cases = sorted(p for p in (HERE / 'cases').iterdir() if p.name.startswith('packages_'))
    greece = ROOT / 'data/cases/greece_60km/reference/strict_12h'
    cases.append(greece)
    programs = None
    try:
        programs = build_streams(BuildConfig('cpu', args.fc, args.profile), work)
    except (RuntimeError, OSError) as exc:
        build_error = str(exc)
    failures = skipped = 0
    names = PACKAGE_ORDER
    for case in cases:
        label = case.name if case != greece else 'packages_greece'
        if not case.is_dir():
            print('SKIP streams/' + label + ': reference case absent')
            skipped += 1
            continue
        difference = None
        if programs is None:
            difference = build_error
        else:
            folder = work / label
            folder.mkdir()
            for filename in ('streams.atmosphere', 'namelist.atmosphere'):
                shutil.copyfile(case / filename, folder / filename)
            path = 'nope.xml' if label == 'packages_inquiry_missing' else 'streams.atmosphere'
            result = run_command([str(programs['lean']), 'packages', 'namelist.atmosphere', path], folder, 'run')
            lines = result.stdout.splitlines()
            active = CP_PACKAGES | {'no_invariant_stream', 'sfclayer'}
            if label == 'packages_inquiry_missing':
                expected = [' Initializing MPAS_streamInfo from file nope.xml',
                            'ERROR: Could not open file nope.xml in read_and_broadcast',
                            'CRITICAL ERROR: Initialization of streamInfo object failed for core atmosphere']
                difference = output_difference(result, expected, 1)
            else:
                if label == 'packages_greece':
                    active |= {'limited_area'}
                    log = (greece / 'log.atmosphere.0000.out').read_text().splitlines()
                    first = log.index(' Initializing MPAS_streamInfo from file streams.atmosphere')
                    last = log.index('     sf_noahmp_in            = T', first)
                    expected_log = log[first:last+2]
                    if [line for line in lines if not line.startswith('@')] != expected_log:
                        difference = 'package log window differs from saved MPAS run'
                if label == 'packages_les':
                    active |= {'les'}
                if label == 'packages_sfclayer_none':
                    active = {'no_invariant_stream'}
                if label == 'packages_sfclayer_mynn':
                    active = {'no_invariant_stream', 'sfclayer'}
                if label == 'packages_sfclayer_off':
                    active -= {'sfclayer'}
                if label == 'packages_invariant':
                    active -= {'no_invariant_stream'}
                if label == 'packages_ugwp':
                    active |= {'ugwp_orog_stream', 'ugwp_ngw_stream', 'ugwp_diags_stream'}
                expected = [f'@ package {name} {"T" if name in active else "F"}' for name in names]
                if [line for line in lines if line.startswith('@')] != expected:
                    difference = 'expected packages ' + repr(expected) + ', got ' + repr(lines)
                for name in ('les', 'sfclayer'):
                    pin = f'   {name} : {"T" if name in active else "F"}'
                    if pin not in lines:
                        difference = 'missing ' + repr(pin)
                if result.returncode != 0:
                    difference = f'exit {result.returncode}: {result.stderr}'
        if difference:
            failures += 1
            print('FAIL streams/' + label + ': ' + difference)
        else:
            print('ok streams/' + label)
    count = len(cases)
    for group in (parse_cases, real_log_cases, loop_cases):
        group_failures, group_skipped, group_count = group(programs, work)
        failures += group_failures
        skipped += group_skipped
        count += group_count
    print(f'STREAMS {"FAIL" if failures else "OK"}: {count} streams cases, {skipped} skipped ({args.fc} {args.profile})')
    return int(failures != 0)


PARSE_FAILURES = {
    'ref_self', 'ref_bad_attribute', 'ref_undefined', 'ref_missing_attribute', 'ref_unexpandable',
    'fi_input_absent', 'fi_output_absent', 'fi_input_initial_only', 'fi_output_initial_only',
    'fi_input_final_only', 'fi_output_final_only', 'missing_name', 'missing_type', 'missing_template',
    'input_without_interval', 'output_without_interval', 'duplicate_name', 'shared_template_output',
    'immutable_with_var', 'missing_list_file', 'unreadable_list_file', 'unknown_immutable',
    'bad_direction', 'template_bad_var', 'template_dollar_dollar', 'dirs_readonly', 'mesh_missing',
    'trailing_blanks', 'trailing_blanks_create', 'ref_trailing_blank',
}
MESH = {'mesh_invariant': 'invariant', 'missing_name': 'invariant', 'mesh_restart': 'restart',
        'nested_input': 'invariant'}
# The mesh file of both branches of driver/mpas_subdriver.F:325-354 (start 00:00:00, or 06:30:15 with its own namelist).
MESH_FILE = {'mesh_filename_interval': 'init.2026-11-08_06.00.00.nc', 'user_stream': 'init.2026-11-08_00.00.00.nc',
             'mesh_restart': 'restart.2026-11-08_00.00.00.nc', 'mesh_io_unknown': 'init.2026-11-08_00.00.00.nc'}
LEAN_PARSE = {
    'mutable_named_input': ['ERROR: In file streams.atmosphere, stream "input" is a Registry immutable stream; '
                            'define it with <immutable_stream>.',
                            'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'stream_in_immutable': ['ERROR: In file streams.atmosphere, the set of variables in stream "input" cannot be modified.',
                            'CRITICAL ERROR: stream xml get attribute failed: streams.atmosphere'],
    'bad_reference_time': ['ERROR: Invalid DateTime string (invalid date substring) tomorrow',
                           'ERROR: In file streams.atmosphere, stream "hourly": reference_time "tomorrow" is not a valid time',
                           'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'bad_record_interval': ['ERROR: Invalid TimeInterval string (unreadable number) abc',
                            'ERROR: In file streams.atmosphere, stream "hourly": record_interval "abc" is not a valid time interval',
                            'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'bad_output_interval': ['ERROR: Invalid TimeInterval string (unreadable number) soon',
                            'ERROR: In file streams.atmosphere, stream "hourly": output_interval "soon" is not a valid time interval',
                            'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    # MPAS's real order: the default filename interval is the output interval, parsed at the create
    # (xml_stream_parser.c:1446-1504, mpas_stream_manager.F:6046-6047), before the output alarm.
    'bad_output_interval_default': ['ERROR: Invalid TimeInterval string (unreadable number) soon',
                                    'ERROR: In file streams.atmosphere, stream "hourly": filename_interval "soon" is not a valid time interval',
                                    'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'mesh_bad_reference': ['ERROR: Invalid DateTime string (invalid date substring) tomorrow',
                           'ERROR: In file streams.atmosphere, stream "input": reference_time "tomorrow" is not a valid time',
                           'CRITICAL ERROR: stream xml get attribute failed: streams.atmosphere'],
    'mesh_bad_filename_interval': ['ERROR: Invalid TimeInterval string (unreadable number) abc',
                                   'ERROR: In file streams.atmosphere, stream "input": filename_interval "abc" is not a valid time interval',
                                   'CRITICAL ERROR: stream xml get attribute failed: streams.atmosphere'],
    'bad_filename_interval': ['ERROR: Invalid TimeInterval string (unreadable number) abc',
                              'ERROR: In file streams.atmosphere, stream "hourly": filename_interval "abc" is not a valid time interval',
                              'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'two_roots': ['ERROR: Problems encountered while parsing run-time I/O config file streams.atmosphere',
                   'ERROR: markup outside of root element',
                   'CRITICAL ERROR: stream xml get attribute failed: streams.atmosphere'],
    'unknown_field': ['ERROR: Requested field bogus_field not available',
                      'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    'unknown_pool': ['ERROR: Requested pool bogus_pool does not exist.',
                     'CRITICAL ERROR: xml stream parser failed: streams.atmosphere'],
    **{name: ['ERROR: In file streams.atmosphere, stream "input" is not read at the start time: '
              'lean_mpas cannot read the initial state.',
              'CRITICAL ERROR: xml stream parser failed: streams.atmosphere']
       for name in ('input_without_alarm', 'input_final_only', 'input_inactive_package', 'input_type_none')},
    'template_g': ['CRITICAL ERROR: ERROR: mpas_expand_string option $G is not a valid expansion character.'],
}


# The stream manager's field lines (mpas_stream_manager.F:886-926, 1006-1011), which the C oracle does not print.
FIELD_PREFIXES = ('* Requested field', 'ERROR: Requested field', 'ERROR: Requested pool', 'WARNING: Requested field',
                  'WARNING: One or more packages')
DEACTIVATED = '* Requested field %s is deactivated due to packages, or is a scratch variable.'
UNDEFINED = "WARNING: One or more packages in '%s' attached to field '%s' is undefined"
# Exact field lines of the parse (after the Registry's immutable streams are set up), in order. Every oracle
# case is compared: one without an entry must print none.
ALREADY = 'WARNING: Requested field %s already in stream hourly'
FIELD_LINES = {
    # In the C parser's order: the list file adds rho, u, w; <var> theta; <var_array> scalars; then the
    # diag struct repeats rho and theta and has six jedi_da members; then the nested 'input' repeats nine.
    'user_stream': [ALREADY % 'rho', ALREADY % 'theta']
                   + [DEACTIVATED % name for name in ('temperature', 'spechum', 'stream_function',
                                                      'velocity_potential', 'cldmask', 'brtemp')]
                   + [ALREADY % name for name in ('scalars', 'u', 'w', 'rho', 'theta', 'relhum', 'rho_base',
                                                  'theta_base', 'surface_pressure')],
    'duplicate_field': ['WARNING: Requested field theta already in stream hourly'],
    'undefined_field_package': [UNDEFINED % ('bogus;limited_area', 'theta'), UNDEFINED % ('bogus', 'theta'),
                                UNDEFINED % ('iau;xy', 'theta')],
    'inactive_user_field': [DEACTIVATED % 'dtaux3d_ls'],
    'nested_input': ["WARNING: One or more packages in 'bogus' attached to fields from stream 'input' is undefined",
                     'WARNING: Requested field theta already in stream copy2'],
    'nested_later': ['WARNING: Requested field theta already in stream a'],
    'nested_earlier': [],
    'nested_unknown': [],
    # A scratch field stays in the stream's field pool (a repeat is MPAS's WARN) and is left out of the file
    # without a line (framework/mpas_io_streams.F:504, 926).
    'trailing_blanks_merged': ['WARNING: Requested field theta already in stream a'],
    'scratch_field': ['WARNING: Requested field tend_uzonal already in stream hourly'],
    # tend_physics: 43 members, 5 scratch, 5 of packages the test namelist leaves inactive.
    'scratch_pool': [DEACTIVATED % name for name in ('rncmpten', 'rnifampten', 'rnwfampten', 'rqrcuten', 'rqscuten')],
}
# Lines the nested-stream cases must hold, as MPAS 8.4.2 logs them for a nested mutable stream defined later or
# nowhere (an MPAS reference run with a hand-written streams file, docs/streams.md).
REQUIRED_LINES = {
    'nested_later': ['ERROR: Stream later does not exist in call to MPAS_stream_mgr_get_property().'],
    'nested_unknown': ['ERROR: Stream nosuch does not exist in call to MPAS_stream_mgr_get_property().'],
}
# '@ fields <stream> <stream_active> <size of stream_fields>' and '@ has <stream> <field>' (stream_has_field).
GREECE_NAMELIST = ROOT / 'data/cases/greece_60km/reference/strict_12h/namelist.atmosphere'
FIELD_CHECKS = {
    'mpas_default': (['@ fields output T 94', '@ fields diagnostics T 109', '@ fields surface T 2', '@ fields input T 123',
                      '@ fields diag_ugwp F 0', '@ fields iau F 0', '@ fields da_state F 0',
                      '@ fields ugwp_oro_data_in F 0'], []),
    'undefined_field_package': (['@ fields a T 1', '@ has a theta', '@ fields b T 1', '@ has b theta',
                                 '@ fields c T 1', '@ has c rho', '@ fields d T 0', '@ fields e T 0',
                                 '@ fields f T 1', '@ has f theta'],
                                ['@ has c theta', '@ has d theta', '@ has e theta', '@ has f rho']),
    'inactive_user_field': (['@ fields hourly T 1', '@ has hourly theta'], ['@ has hourly dtaux3d_ls']),
    'duplicate_field': (['@ fields hourly T 2'], []),
    'nested_input': (['@ fields input T 36', '@ fields copy T 123', '@ has copy latCell', '@ fields copy2 T 1',
                      '@ has copy2 theta'], ['@ has input latCell', '@ has copy2 latCell']),
    'nested_later': (['@ fields a T 12', '@ has a q2', '@ has a scalars', '@ has a lbc_u', '@ fields later T 12'], []),
    'nested_earlier': (['@ fields a T 1', '@ fields early T 1'], ['@ has a q2']),
    'nested_unknown': (['@ fields a T 1'], []),
    # 'a' and 'a ' are one stream in MPAS's manager: the second element's fields, packages, alarm, reference time,
    # record interval and filename interval go to the first; its template does not.
    'trailing_blanks_merged': (['@ fields a T 2', '@ has a theta', '@ has a rho'], []),
    'scratch_field': (['@ fields hourly T 1', '@ has hourly theta'], ['@ has hourly tend_uzonal']),
    'scratch_pool': (['@ fields hourly T 33', '@ has hourly rthdynten', '@ has hourly rthratenlw'],
                     ['@ has hourly ' + name for name in ('tend_ru_physics', 'tend_rtheta_physics', 'tend_rho_physics',
                                                          'tend_uzonal', 'tend_umerid', 'rqrcuten')]),
}


# Cases whose field lines are MPAS's own: these two parse the default stream lists with the test namelist, which
# sets the packages of the greece strict_12h reference run, so the parse's field lines are that run's log lines.
MPAS_LOG_FIELD_LINES = ('greece_strict_12h', 'mpas_default')


def expected_field_lines(case):
    if case.name in MPAS_LOG_FIELD_LINES:
        log = GREECE_NAMELIST.parent / 'log.atmosphere.0000.out'
        if not log.is_file():
            raise FileNotFoundError(str(log))
        return field_lines([line.strip() for line in log.read_text().splitlines()])
    return FIELD_LINES.get(case.name, [])


def field_lines(lines):
    start = next((i for i, line in enumerate(lines) if line.startswith('Parsing run-time I/O')), len(lines))
    return [line.strip() for line in lines[start:] if line.strip().startswith(FIELD_PREFIXES)]


def real_runs():
    wanted = ('log.atmosphere.0000.out', 'namelist.atmosphere', 'streams.atmosphere')
    return sorted(p for p in (ROOT / 'data/cases').glob('*/reference/*') if all((p / f).is_file() for f in wanted))


def mpas_finished(run):
    """True when MPAS ran the reference run to its end; a run where MPAS itself stopped has no full plan to replay."""
    return 'Finished running the atmosphere core' in (run / 'log.atmosphere.0000.out').read_text()


def real_folder(run, work, tag):
    folder = work / (tag + '-' + run.parents[1].name + '-' + run.name)
    folder.mkdir()
    for name in ('namelist.atmosphere', 'streams.atmosphere'):
        shutil.copyfile(run / name, folder / name)
    for item in (ROOT / 'reference/mpas_defaults').glob('stream_list.atmosphere.*'):
        shutil.copyfile(item, folder / item.name)
    # A reference run can name user lists as well as the defaults. Replay its own inputs.
    for item in run.glob('stream_list.*'):
        shutil.copyfile(item, folder / item.name)
    return folder


def real_log_cases(programs, work):
    """The stream setup window of every MPAS 8.4.2 reference run, line for line."""
    runs = real_runs()
    if not runs:
        print('SKIP streams/real_logs: data/cases holds no reference run')
        return 0, 1, 1
    failures = skipped = 0
    for run in runs:
        label = 'real_logs/' + run.parents[1].name + '/' + run.name
        log = [line.rstrip() for line in (run / 'log.atmosphere.0000.out').read_text().splitlines()]
        first = next((i for i, line in enumerate(log)
                      if line == ' Initializing MPAS_streamInfo from file streams.atmosphere'), None)
        # The window ends where MPAS validates the streams, or at the CRITICAL ERROR it stopped the setup with.
        last, status = next(((i, int(log[i].startswith('CRITICAL ERROR:'))) for i in range(first or 0, len(log))
                             if log[i] == '  ** Validating streams' or log[i].startswith('CRITICAL ERROR:')),
                            (None, 0))
        if first is None or last is None:
            print('SKIP streams/' + label + ': MPAS stopped before the end of its stream setup')
            skipped += 1
            continue
        difference = None
        if programs is None:
            difference = 'streams driver could not be built'
        else:
            folder = real_folder(run, work, 'real')
            result = run_command([str(programs['lean']), 'startup', 'namelist.atmosphere', 'streams.atmosphere'], folder, 'startup')
            actual = SimpleNamespace(stdout='\n'.join(line.rstrip() for line in result.stdout.splitlines()),
                                     returncode=result.returncode, stderr=result.stderr)
            difference = output_difference(actual, log[first:last+1], status)
        if difference:
            failures += 1
            print('FAIL streams/' + label + ': ' + difference)
        else:
            print('ok streams/' + label)
    return failures, skipped, len(runs)


def loop_run(programs, folder, mode='loop'):
    """lean's I/O plan step by step: the events MPAS's log shows, the writes by time, and the '@' notes."""
    result = run_command([str(programs['lean']), mode, 'namelist.atmosphere', 'streams.atmosphere'], folder, mode)
    events, writes, notes, now = [], [], [], None
    for line in result.stdout.splitlines():
        if line.startswith('start '):
            now = line.split()[1]
        if line.startswith('@ now '):
            now = line.split()[2]
        elif line.startswith('@') or line == 'lbc first':
            notes.append(line)
        elif line.startswith('write '):
            writes.append((now, *line.split()[1:]))
        else:
            events.append(line)
    return SimpleNamespace(result=result, events=events, writes=writes, notes=notes)


def start_notes(loop):
    """plan_start sets neither diagnostics_rang nor restart_rang: MPAS consults them after a step only."""
    wrong = [note for note in loop.notes if note.startswith('@ start ')]
    return ('set by plan_start: ' + repr(wrong)) if wrong else None


def log_events(log):
    """core_atmosphere/mpas_atm_core.F:666-909 as its log lines show it."""
    events = []
    for line in log:
        if line.startswith(' Begin timestep '):
            if not any(event.startswith('step ') for event in events):
                events.insert(0, 'start ' + line.split()[2])
            events.append('step ' + line.split()[2])
        elif 'Updated lateral boundary conditions. LBCs are now valid' in line:
            events.append('lbc')
        elif re.match(r" +Read '(\S+)' input stream valid at", line):
            events.append('read ' + line.split("'")[1])
        elif line.startswith(' Timing for stream output'):
            events.append('output')
    return events


def times(prefix, hours):
    return [prefix + f'{hour:02d}:00:00' for hour in hours]


WHOLE_RUNS = ('strict_12h', 'strict_24h', 'strict_1h', 'atm8_6h')


def real_loop_checks(run, loop):
    name = run.parents[1].name + '/' + run.name
    log = (run / 'log.atmosphere.0000.out').read_text().splitlines()
    expected = log_events(log)
    if loop.events != expected:
        where = next((i for i, pair in enumerate(zip(loop.events, expected)) if pair[0] != pair[1]),
                     min(len(loop.events), len(expected)))
        return (f'loop_logs: event {where}: expected {expected[where:where+3]!r}, got {loop.events[where:where+3]!r} '
                f'({len(expected)} and {len(loop.events)} events)')
    # MPAS's own record interval of lbc_in (the limited-area check's log line).
    for line in log:
        if "Input interval for 'lbc_in' stream = " in line:
            note = '@ record_interval lbc_in ' + line.split("'")[3]
            if note not in loop.notes:
                return 'record_interval: missing ' + repr(note)
    written = {}
    for _, stream, filename in loop.writes:
        written.setdefault(stream, set()).add(filename)
    # loop_files: the files of the run's directory.
    for stream, prefix in (('output', 'history.'), ('diagnostics', 'diag.')):
        listed = {p.name for p in run.glob(prefix + '*.nc')}
        got = written.get(stream, set())
        if listed and (got != listed if run.name in WHOLE_RUNS else not listed <= got):
            return f'loop_files: {stream}: the run kept {sorted(listed)!r}, lean writes {sorted(got)!r}'
    if (run / 'output_files.txt').is_file():
        # Saved manifests use filename-size or size-filename columns.
        rows = [line.split() for line in (run / 'output_files.txt').read_text().splitlines() if line.strip()]
        listed = {row[1] if row[0].isdigit() else row[0] for row in rows}
        # A restart run's manifest also lists the linked input restart under
        # its absolute inputs path. The start time identifies that one record.
        if expected and expected[0].startswith('start '):
            stamp = expected[0].split()[1].replace(':', '.')
            initial = f'restart.{stamp}.nc'
            linked_input = {filename for filename in listed
                            if Path(filename).is_absolute() and 'inputs' in Path(filename).parts
                            and Path(filename).name == initial}
            listed -= linked_input
        got = set().union(*written.values()) if written else set()
        if got != listed:
            return f'loop_files: the run wrote {sorted(listed)!r}, lean writes {sorted(got)!r}'
    steps = [event for event in loop.events if event.startswith('step ')]
    lbc_steps = [loop.events[i-1].split()[1] for i, event in enumerate(loop.events) if event == 'lbc']
    restart = [write for write in loop.writes if write[1] == 'restart']
    if name == 'greece_60km/strict_12h':
        reads = [(i, event) for i, event in enumerate(loop.events) if event.startswith('read ')]
        first_step = loop.events.index('step 2026-11-08_00:00:00')
        if len(steps) != 360:
            return f'{len(steps)} steps, expected 360'
        if lbc_steps != times('2026-11-08_', (0, 3, 6, 9)):
            return 'lbc updates at ' + repr(lbc_steps)
        if reads != [(first_step + 2 + k, 'read ' + stream)
                     for k, stream in enumerate(('ugwp_oro_data_in', 'ugwp_ngw_in', 'da_state'))]:
            return 'reads: ' + repr(reads)
        if restart or '@ restart_rang' in loop.notes:
            return 'a restart write or ring in a 12 h run'
        if '@ record_interval surface none' not in loop.notes:
            return 'surface_interval: the default file has a record interval'
        # io_plan%read holds only streams that read: the three logged at the first step are inactive.
        if any(note.startswith('@ reads ') for note in loop.notes):
            return 'io_plan%read lists a stream that reads nothing: ' + repr([n for n in loop.notes if n.startswith('@ reads ')])
    if name == 'x1_2562/strict_24h':
        if restart != [('2026-11-09_00:00:00', 'restart', 'restart.2026-11-09_00.00.00.nc')]:
            return 'restart writes: ' + repr(restart)
        if loop.notes.count('@ restart_rang') != 1 or loop.notes[-1] != '@ restart_rang':
            return 'restart_rang is not at the last step only'
    if name == 'greece_60km/strict_sfc_3h' and '@ record_interval surface 0000000000_003:000:000' not in loop.notes:
        return 'surface_interval: ' + repr([note for note in loop.notes if note.startswith('@ record_interval surface')])
    if name == 'greece_60km/strict_sfc_3h':
        logged = loop.events.count('read surface')
        if logged == 0 or loop.notes.count('@ reads surface') != logged:
            return f"io_plan%read: surface logged {logged} times, read {loop.notes.count('@ reads surface')} times"
    return None


def lean_loop_checks(case, loop):
    by_stream = {}
    for now, stream, filename in loop.writes:
        by_stream.setdefault(stream, []).append((now, filename))
    day = '2026-11-08_'
    if case == 'multi_record_names':
        hours = range(13)
        expected = {
            'h_none': [(day + f'{h:02d}:00:00', 'h_none.2026-11-08_00.00.00.nc') for h in hours],
            'h_six': [(day + f'{h:02d}:00:00', f'h_six.2026-11-08_{6*(h//6):02d}.00.00.nc') for h in hours],
            'h_default': [(day + f'{h:02d}:00:00', f'h_default.2026-11-08_{h:02d}.00.00.nc') for h in hours],
        }
        if by_stream != expected:
            return 'writes: ' + repr(by_stream)
    if case == 'final_output' and by_stream != {'fin': [(day + '12:00:00', 'fin.2026-11-08_12.00.00.nc')]}:
        return 'writes: ' + repr(by_stream)
    if case == 'lbc_offset':
        lbc_steps = [loop.events[i-1].split()[1] for i, event in enumerate(loop.events) if event == 'lbc']
        if loop.notes[-2:] != ['lbc first', '@ lbc_file lbc.2026-11-08_06.00.00.nc'] and \
           ['lbc first', '@ lbc_file lbc.2026-11-08_06.00.00.nc'] != [n for n in loop.notes if 'lbc' in n][:2]:
            return 'first LBC read: ' + repr([n for n in loop.notes if 'lbc' in n])
        if lbc_steps != times(day, (7, 9, 12, 15, 18)):
            return 'lbc updates at ' + repr(lbc_steps)
    return None


def loop_cases(programs, work):
    runs = [run for run in real_runs() if any(line.startswith(' Begin timestep ')
            for line in (run / 'log.atmosphere.0000.out').read_text().splitlines())]
    lean_cases = ('multi_record_names', 'final_output', 'lbc_offset')
    count = len(runs) + len(lean_cases) + 1
    failures = skipped = 0
    for run in runs:
        label = 'loop/' + run.parents[1].name + '/' + run.name
        if not mpas_finished(run):
            print('SKIP streams/' + label + ': MPAS stopped during this run')
            skipped += 1
            continue
        if programs is None:
            difference = 'streams driver could not be built'
        else:
            loop = loop_run(programs, real_folder(run, work, 'loop'))
            difference = output_difference(loop.result, loop.result.stdout.splitlines()) or start_notes(loop) or real_loop_checks(run, loop)
        print(('FAIL streams/' + label + ': ' + difference) if difference else 'ok streams/' + label)
        failures += bool(difference)
    for case in lean_cases:
        if programs is None:
            difference = 'streams driver could not be built'
        else:
            folder = work / ('loop-' + case)
            shutil.copytree(HERE / 'cases' / case, folder)
            if not (folder / 'namelist.atmosphere').exists():
                shutil.copyfile(HERE / 'cases/namelist.atmosphere', folder / 'namelist.atmosphere')
            loop = loop_run(programs, folder)
            difference = output_difference(loop.result, loop.result.stdout.splitlines()) or start_notes(loop) or lean_loop_checks(case, loop)
        print(('FAIL streams/loop/' + case + ': ' + difference) if difference else 'ok streams/loop/' + case)
        failures += bool(difference)
    # lean_only_restart_run. NOT an oracle comparison: no MPAS restart log exists among the references, so a
    # restart run of lean is compared with lean's own cold run of the same files. Independent of lean are only
    # the two pins from MPAS's source (mpas_atm_core.F:667-672): the initial stream is 'restart', and 'output'
    # and 'diagnostics' are not written at the start. That the 'output' event stays at the start (other
    # streams' output alarms still ring, :711-713) and that the later writes are the cold run's is lean's
    # reading of the source; a short MPAS restart reference run would make this an oracle comparison.
    greece = ROOT / 'data/cases/greece_60km/reference/strict_12h'
    if not greece.is_dir():
        print('SKIP streams/loop/lean_only_restart_run: reference case absent')
        skipped += 1
    else:
        difference = None
        if programs is None:
            difference = 'streams driver could not be built'
        else:
            cold = loop_run(programs, real_folder(greece, work, 'cold'))
            warm = loop_run(programs, real_folder(greece, work, 'warm'), 'loop-restart')
            start = '2026-11-08_00:00:00'
            at_start = [[write[1] for write in loop.writes if write[0] == start] for loop in (cold, warm)]
            difference = output_difference(warm.result, warm.result.stdout.splitlines())
            if not difference and ('@ initial input' not in cold.notes or '@ initial restart' not in warm.notes):
                difference = 'initial stream: ' + repr([n for n in cold.notes + warm.notes if n.startswith('@ initial')])
            if not difference and at_start != [['output', 'diagnostics'], []]:
                difference = 'writes at the start: ' + repr(at_start)
            if not difference and [w for w in cold.writes if w[0] != start] != [w for w in warm.writes if w[0] != start]:
                difference = 'the later writes of a restart run differ'
            # lean against lean: the other streams' output alarms still ring at the start (da_state, diag_ugwp),
            # so MPAS's 'Timing for stream output' line should stay and the events be those of the cold run.
            if not difference and warm.events != cold.events:
                difference = 'the events of a restart run differ'
        print(('FAIL streams/loop/lean_only_restart_run: ' + difference) if difference else 'ok streams/loop/lean_only_restart_run')
        failures += bool(difference)
    return failures, skipped, count


def parse_cases(programs, work):
    cases = sorted(p for p in (HERE / 'cases').iterdir()
                   if p.is_dir() and not p.name.startswith(('syntax_', 'tree_', 'packages_')))
    failures = skipped = 0
    for case in cases:
        difference = None
        if programs is None:
            difference = 'parse driver could not be built'
        elif case.name not in LEAN_PARSE and 'oracle' not in programs:
            print('SKIP streams/' + case.name + ': reference/mpas_src is absent')
            skipped += 1
            continue
        else:
            folders = {}
            permissions = []
            try:
                for side in programs:
                    folder = work / (case.name + '-' + side)
                    shutil.copytree(case, folder)
                    marker = case / 'source.txt'
                    if marker.exists():
                        source = ROOT / marker.read_text().strip()
                        if not source.is_dir():
                            raise FileNotFoundError(str(source))
                        shutil.copyfile(source / 'streams.atmosphere', folder / 'streams.atmosphere')
                        for item in (ROOT / 'reference/mpas_defaults').glob('stream_list.*'):
                            shutil.copyfile(item, folder / item.name)
                    if not (folder / 'namelist.atmosphere').exists():
                        shutil.copyfile(HERE / 'cases/namelist.atmosphere', folder / 'namelist.atmosphere')
                    if case.name in ('dirs_readonly', 'unreadable_list_file'):
                        item = folder / ('ro' if case.name == 'dirs_readonly' else 'list.hourly')
                        permissions.append((item, item.stat().st_mode))
                        item.chmod(0o555 if case.name == 'dirs_readonly' else 0)
                    folders[side] = folder
                repetitions = 2 if case.name == 'dirs' else 1
                for repetition in range(repetitions):
                    result = run_command([str(programs['lean']), 'parse', 'namelist.atmosphere', 'streams.atmosphere'], folders['lean'], f'run{repetition}')
                    lines = result.stdout.splitlines()
                    if case.name in LEAN_PARSE:
                        errors = [line for line in lines if line.startswith(('ERROR:', 'CRITICAL ERROR:'))]
                        expected = LEAN_PARSE[case.name]
                        difference = output_difference(SimpleNamespace(stdout='\n'.join(errors), returncode=result.returncode), expected, 1)
                        if not difference and '@ parser status=1' not in lines:
                            difference = 'missing @ parser status=1'
                    else:
                        mesh = MESH.get(case.name, 'input')
                        oracle = run_command([str(programs['oracle']), 'streams.atmosphere', mesh], folders['oracle'], f'run{repetition}')
                        status = int(case.name in PARSE_FAILURES)
                        difference = difference or output_difference(oracle, oracle.stdout.splitlines(), status)
                        reference = [line for line in oracle.stdout.splitlines() if not line.startswith('@')]
                        actual = [line for line in lines if not line.startswith('@') and not line.strip().startswith(FIELD_PREFIXES)]
                        if case.name == 'mesh_missing':
                            detail = 'ERROR: In file streams.atmosphere, no <immutable_stream> named "input"; the mesh is read from it.'
                            if detail not in actual:
                                difference = 'missing the diagnostic for a missing input stream'
                            actual = [line for line in actual if line != detail]
                        if not difference:
                            difference = output_difference(SimpleNamespace(stdout='\n'.join(actual), returncode=result.returncode), reference, status)
                        if not difference:
                            # The mesh stream's attributes as xml_stream_get_attributes returns them.
                            mesh_lines = [[line.rstrip() for line in text if line.startswith('@ mesh status=0')]
                                          for text in (oracle.stdout.splitlines(), lines)]
                            if mesh_lines[0] != mesh_lines[1]:
                                difference = f'mesh stream: expected {mesh_lines[0]!r}, got {mesh_lines[1]!r}'
                        if not difference and status == 0:
                            actual_trace = [line for line in lines if line.startswith('@') and not line.startswith(('@ parser', '@ mesh'))]
                            expected_trace = [line for line in oracle.stdout.splitlines() if line.startswith('@') and not line.startswith(('@ parser', '@ mesh'))]
                            difference = output_difference(SimpleNamespace(stdout='\n'.join(actual_trace), returncode=0), expected_trace)
                        if not difference and f'@ parser status={status}' not in lines:
                            difference = f'missing @ parser status={status}'
                        if not difference and field_lines(lines) != expected_field_lines(case):
                            difference = f'field lines: expected {expected_field_lines(case)!r}, got {field_lines(lines)!r}'
                        for required in REQUIRED_LINES.get(case.name, ()):
                            if not difference and required not in lines:
                                difference = 'missing ' + repr(required)
                        if not difference and case.name in FIELD_CHECKS:
                            namelist = 'namelist.atmosphere'
                            if case.name == 'mpas_default':
                                if not GREECE_NAMELIST.is_file():
                                    raise FileNotFoundError(str(GREECE_NAMELIST))
                                namelist = str(GREECE_NAMELIST)
                            listed = run_command([str(programs['lean']), 'fields', namelist, 'streams.atmosphere'], folders['lean'], 'fields')
                            present, absent = FIELD_CHECKS[case.name]
                            got = listed.stdout.splitlines()
                            wrong = [line for line in present if line not in got] + ['not ' + line for line in absent if line in got]
                            if wrong or listed.returncode != 0:
                                difference = 'stream fields: ' + repr(wrong) + ' in ' + repr([line for line in got if line.startswith('@ fields')])
                        if not difference and case.name in MESH_FILE and repetition == 0:
                            inspected = run_command([str(programs['lean']), 'inspect', 'namelist.atmosphere', 'streams.atmosphere'], folders['lean'], 'inspect')
                            if '@ mesh_file ' + MESH_FILE[case.name] not in inspected.stdout.splitlines():
                                difference = 'wrong mesh file name, expected ' + MESH_FILE[case.name]
                        if not difference and case.name == 'dirs':
                            created = '         *** created directory out/sub for stream "hourly"'
                            if (created in lines) != (repetition == 0):
                                difference = 'directory creation log does not match first/second parse'
                    # A failure in any repetition fails the case.
                    if difference:
                        difference = f'run {repetition}: {difference}'
                        break
            except FileNotFoundError as exc:
                print('SKIP streams/' + case.name + ': ' + str(exc))
                skipped += 1
                continue
            finally:
                for item, mode in permissions:
                    item.chmod(mode)
        if difference:
            failures += 1
            print('FAIL streams/' + case.name + ': ' + difference)
        else:
            print('ok streams/' + case.name)
    return failures, skipped, len(cases)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc', choices=('gfortran', 'nvfortran'), default='gfortran')
    parser.add_argument('--profile', choices=('strict', 'debug'), default='strict')
    parser.add_argument('--keep', action='store_true')
    parser.add_argument('--only', choices=('clock', 'xml', 'streams'))
    args = parser.parse_args()
    if not shutil.which(args.fc):
        print('missing tool: ' + args.fc, file=sys.stderr)
        return 2
    work = ROOT / 'build/streams' / (args.fc + '-' + args.profile)
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    status = 0
    for group, runner in (('clock', clock_group), ('xml', xml_group), ('streams', streams_group)):
        if args.only not in (None, group):
            continue
        folder = work / group
        folder.mkdir()
        status = max(status, runner(args, folder))
    if status == 0 and not args.keep:
        shutil.rmtree(work)
    return status


if __name__ == '__main__':
    sys.exit(main())
