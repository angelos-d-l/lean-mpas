#!/usr/bin/env python3
"""Apply case specs to MPAS default inputs; print shell-quoted run settings.

    run_spec.py --out DIR BASE_NAMELIST BASE_STREAMS SPEC [SPEC ...]

A spec (cases/<case>/case.txt, mpas_runs/<run>.txt, or init/base.txt and init/<stage>.txt for init_atmosphere)
has one statement a line; # starts a comment:
  nml RECORD OPTION VALUE...      set OPTION in &RECORD of the namelist (added before the record's / if absent)
  stream NAME ATTRIBUTE VALUE     set an attribute of the stream NAME (added if absent)
  build | ranks | inputs | card | minutes VALUE
                                  a run setting, at most once each (card on|off; ranks and minutes positive integers)
  expect FILE                     a file the run must write
Specs apply in order. The edited namelist and streams go to DIR under their base names; the settings are printed as
KEY=value lines for a shell: BUILD, RANKS (default 1), INPUTS (.), CARD (off), MINUTES (10), EXPECT. An error names
the spec file and line and exits 2.
"""
import argparse
import re
import shlex
import sys
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path
from xml.sax.saxutils import escape


NML_NAME = re.compile(r'[A-Za-z][A-Za-z0-9_]*\Z')
XML_NAME = re.compile(r'[A-Za-z_][A-Za-z0-9_.:-]*\Z')
STREAM_TAG = re.compile(
    r'<!--.*?-->|<\?.*?\?>|<(?:immutable_stream|stream)(?=\s|/?>)'
    r'''(?:[^<>"']|"[^"]*"|'[^']*')*>''', re.DOTALL,
)
ATTRIBUTE = re.compile(r'''([A-Za-z_][A-Za-z0-9_.:-]*)\s*=\s*(["'])(.*?)\2''', re.DOTALL)


class SpecParser(argparse.ArgumentParser):
    def error(self, message):
        self.exit(2, f'{self.prog}:0: {message}\n')


def edit_namelist(text, record, option, value):
    if not NML_NAME.fullmatch(record) or not NML_NAME.fullmatch(option):
        raise ValueError('nml needs a record name and an option name')
    lines = text.splitlines(keepends=True)
    current = None
    close = None
    matches = []
    for i, line in enumerate(lines):
        header = re.fullmatch(r'\s*&([A-Za-z][A-Za-z0-9_]*)\s*(?:!.*)?', line.rstrip('\r\n'))
        if header:
            current = header[1]
        elif re.fullmatch(r'\s*/\s*(?:!.*)?', line.rstrip('\r\n')):
            if current is not None and current.lower() == record.lower():
                close = i
            current = None
        else:
            assignment = re.match(r'([ \t]*[A-Za-z][A-Za-z0-9_]*[ \t]*=[ \t]*)', line)
            if assignment is None:
                continue
            name = assignment[1].split('=', 1)[0].strip()
            if name.lower() != option.lower():
                continue
            if current is None or current.lower() != record.lower():
                raise ValueError(f'{option} belongs to &{current}, not &{record}')
            matches.append((i, assignment.end()))
    if close is None:
        raise ValueError(f'missing or unclosed namelist record &{record}')
    if len(matches) > 1:
        raise ValueError(f'{option} occurs more than once in &{record}')
    if matches:
        i, start = matches[0]
        ending = lines[i][len(lines[i].rstrip('\r\n')):]
        lines[i] = lines[i][:start] + value + ending
    else:
        ending = '\r\n' if '\r\n' in text else '\n'
        lines.insert(close, f'    {option} = {value}{ending}')
    return ''.join(lines)


def edit_stream(text, name, attribute, value):
    if not XML_NAME.fullmatch(attribute):
        raise ValueError(f'invalid stream attribute name: {attribute}')
    ET.fromstring(text)
    for match in STREAM_TAG.finditer(text):
        tag = match[0]
        if tag.startswith(('<!--', '<?')):
            continue
        element = ET.fromstring(tag if tag.endswith('/>') else tag[:-1] + '/>')
        if element.get('name') != name:
            continue
        attributes = list(ATTRIBUTE.finditer(tag))
        escaped = escape(value, {'"': '&quot;', "'": '&apos;'})
        for attr in attributes:
            if attr[1] == attribute:
                tag = tag[:attr.start(3)] + escaped + tag[attr.end(3):]
                break
        else:
            last = attributes[-1]
            prefix = tag[tag.rfind('\n', 0, last.start()) + 1:last.start()]
            indent = re.match(r'[ \t]*', prefix)[0]
            ending = '\r\n' if '\r\n' in tag else '\n'
            tag = (tag[:last.end()] + f'{ending}{indent}{attribute}="{escaped}"'
                   + tag[last.end():])
        result = text[:match.start()] + tag + text[match.end():]
        ET.fromstring(result)
        return result
    raise ValueError(f'no stream named {name}')


def apply_spec(path, namelist, streams, settings, expected):
    line_number = 0
    try:
        lines = path.read_bytes().decode('utf-8').splitlines()
        seen = set()
        for line_number, line in enumerate(lines, 1):
            line = line.split('#', 1)[0]
            words = line.split()
            if not words:
                continue
            command = words[0]
            if command in ('build', 'ranks', 'inputs', 'card', 'minutes'):
                if len(words) != 2:
                    raise ValueError(f'{command} needs exactly one value')
                if command in seen:
                    raise ValueError(f'{command} occurs more than once in this spec')
                value = words[1]
                if command in ('ranks', 'minutes'):
                    if not re.fullmatch(r'[0-9]+', value) or int(value) < 1:
                        raise ValueError(f'{command} needs a positive integer')
                if command == 'card' and value not in ('on', 'off'):
                    raise ValueError('card must be on or off')
                settings[command.upper()] = value
                seen.add(command)
            elif command == 'expect':
                if len(words) != 2:
                    raise ValueError('expect needs exactly one file')
                expected.append(words[1])
            elif command == 'nml':
                parts = line.split(None, 3)
                if len(parts) != 4:
                    raise ValueError('nml needs RECORD OPTION VALUE...')
                namelist = edit_namelist(namelist, *parts[1:])
            elif command == 'stream':
                if len(words) != 4:
                    raise ValueError('stream needs NAME ATTRIBUTE VALUE')
                streams = edit_stream(streams, *words[1:])
            else:
                raise ValueError(f'unknown statement: {command}')
    except (OSError, ValueError, ET.ParseError) as error:
        raise ValueError(f'{path}:{line_number}: {error}') from error
    return namelist, streams


def write_pair(out, names, contents):
    if not out.is_dir():
        raise ValueError(f'output directory must exist: {out}')
    if names[0] == names[1]:
        raise ValueError('base namelist and streams must have different basenames')
    targets = [out / name for name in names]
    for target in targets:
        if target.is_dir():
            raise ValueError(f'output path is a directory: {target}')
    # Stage both files before replacing either; retain old files for I/O-error rollback.
    with tempfile.TemporaryDirectory(prefix='.run_spec-', dir=out) as scratch:
        scratch = Path(scratch)
        staged = [scratch / str(i) for i in range(2)]
        for path, text in zip(staged, contents):
            path.write_bytes(text.encode('utf-8'))
        replaced = []
        try:
            for i, target in enumerate(targets):
                backup = None
                if target.exists() or target.is_symlink():
                    backup = scratch / f'old-{i}'
                    target.replace(backup)
                replaced.append((target, backup))
                staged[i].replace(target)
        except OSError:
            for target, backup in reversed(replaced):
                if backup is not None:
                    backup.replace(target)
                else:
                    target.unlink(missing_ok=True)
            raise


def main(argv=None):
    parser = SpecParser(description=__doc__)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('base_namelist', type=Path)
    parser.add_argument('base_streams', type=Path)
    parser.add_argument('spec', type=Path, nargs='+')
    args = parser.parse_args(argv)
    settings = {'BUILD': '', 'RANKS': '1', 'INPUTS': '.', 'CARD': 'off', 'MINUTES': '10'}
    expected = []
    contents = []
    for path in (args.base_namelist, args.base_streams):
        try:
            contents.append(path.read_bytes().decode('utf-8'))
        except (OSError, ValueError) as error:
            print(f'{path}:0: {error}', file=sys.stderr)
            return 2
    try:
        for path in args.spec:
            contents = apply_spec(path, *contents, settings, expected)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 2
    try:
        write_pair(args.out, (args.base_namelist.name, args.base_streams.name), contents)
    except (OSError, ValueError) as error:
        print(f'{args.spec[-1]}:0: {error}', file=sys.stderr)
        return 2
    settings['EXPECT'] = ' '.join(expected)
    for key, value in settings.items():
        print(f'{key}={shlex.quote(value)}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
