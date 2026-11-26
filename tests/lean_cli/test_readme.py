"""Keep README.md true: what can be checked mechanically.

    commands         every ./lean line of a code block parses; a cpu build line also configures (--dry-run)
    paths            every repository path the README names exists; build/<name> is the name its build lines give
    codes            every E_/W_ code, ./lean option and quoted message start is in the code
    options          every config_* is an MPAS option (config/namelist.json) or a &lean one, with the status the
                     README gives it
    minima           the versions it states equal lean.lock.json
    self_sufficient  the steps from clone to forecast (the intro and sections 1-4) are all there and link no file

LEAN_README=<path> checks another README (for example an older one, to see these tests fail on it).

card_default: the card section says an unset LEAN_CARD means all and LEAN_CARD=none keeps the physics on the host;
src/phys_mode.f90's parse_stages must say the same.
"""
import json
import os
import re
import shlex
import subprocess
import unittest
from pathlib import Path

from tools import gen_namelist
from tools.lean import cli
from tools.lean.config import BuildConfig, build_dir_name, load_lock, load_presets

ROOT = Path(__file__).resolve().parents[2]
README = Path(os.environ.get('LEAN_README', ROOT / 'README.md'))
STEP_SECTIONS = ('', '1. What you need', '2. Build', '3. Your run directory', '4. Run')
# Paths of MPAS's own source tree that the README names (MPAS's build puts the tables there): not in this repository.
MPAS_TREE = ('src/core_atmosphere/',)
CARD_DEFAULT = re.compile(r"all_on\s*=\s*\.not\.\s*card_set")   # src/phys_mode.f90 parse_stages: unset means all


def text():
    return README.read_text(encoding='utf-8')


def sections():
    """{heading: body}: '' is the text before the first '## ' heading."""
    out, name, body = {}, '', []
    for line in text().splitlines():
        if line.startswith('## '):
            out[name] = '\n'.join(body)
            name, body = line[3:].strip(), []
        else:
            body.append(line)
    out[name] = '\n'.join(body)
    return out


def code_lines():
    lines, inside = [], False
    for line in text().splitlines():
        if line.startswith('```'):
            inside = not inside
        elif inside:
            lines.append(line)
    return lines


def lean_commands():
    return [shlex.split(line.split(' #')[0])[1:] for line in code_lines() if line.startswith('./lean ')]


def backticked():
    return re.findall(r'`([^`\n]+)`', text())


def corpus():
    """The code's text, with Fortran's doubled quote as one, for the codes and the messages."""
    parts = [(ROOT / 'lean').read_text(encoding='utf-8')]
    for pattern in ('src/**/*.f90', 'src/**/*.F90', 'tools/**/*.py'):
        parts += [path.read_text(encoding='utf-8', errors='replace') for path in sorted(ROOT.glob(pattern))]
    return '\n'.join(parts).replace("''", "'")


def option_strings(command=None):
    """The options of one ./lean command, or of all of them."""
    found = set()
    for name, action in cli.parser()._subparsers._group_actions[0].choices.items():
        if command in (None, name):
            for option in action._actions:
                found.update(option.option_strings)
    return found


def build_name(argv):
    """build/<name> of a ./lean build line, as tools/lean/cli.py _build makes the config."""
    args, _ = cli.parser().parse_known_args(['build', *argv])
    settings = dict(load_presets()[args.flavor or 'mpas'])
    settings.pop('why')
    settings.pop('flavor')
    switches = dict(settings.pop('switches'))
    for name in ('target', 'fc', 'profile'):
        if getattr(args, name) is not None:
            settings[name] = getattr(args, name)
    if args.fc is None:
        settings['fc'] = cli.DEFAULT_FC[settings['target']]
    cfg = BuildConfig(**settings, omp=args.omp, variant=args.variant, switches=tuple(switches.items()))
    return build_dir_name(cfg), cfg.target


class ReadmeTest(unittest.TestCase):

    def test_commands(self):
        commands = lean_commands()
        self.assertTrue(any(c[:1] == ['build'] for c in commands), 'no ./lean build line')
        self.assertTrue(any(c[:1] == ['doctor'] for c in commands), 'no ./lean doctor line')
        for argv in commands:
            with self.subTest(command=' '.join(argv)):
                try:
                    _, unknown = cli.parser().parse_known_args(argv)
                except SystemExit:
                    self.fail(f'./lean {" ".join(argv)} does not parse')
                unknown = {word for word in unknown if word.startswith('-')}
                self.assertEqual(unknown, set(), 'unknown options')
                if argv[0] == 'build' and not unknown and build_name(argv)[1] == 'cpu':
                    self.dry_run(argv)

    def dry_run(self, argv):
        name = build_name(argv)[0]
        made = not (ROOT / 'build' / name).exists()
        try:
            done = subprocess.run([str(ROOT / 'lean'), *argv, '--dry-run'], cwd=ROOT, capture_output=True,
                                  text=True, timeout=600)
        finally:
            if made and (ROOT / 'build' / name).exists():
                subprocess.run(['rm', '-rf', str(ROOT / 'build' / name)])
        if 'BUILD FAIL: E_NO' in done.stdout or 'BUILD FAIL: E_NETCDF' in done.stdout:
            self.skipTest(f'the tools for a dry run are missing: {done.stdout.strip()}')
        self.assertEqual(done.returncode, 0, f'./lean {" ".join(argv)} --dry-run: {done.stdout}{done.stderr}')

    def test_paths(self):
        names = {build_name(argv)[0] for argv in lean_commands() if argv[:1] == ['build']}
        words = set(backticked())
        for line in code_lines():
            words.update(line.split())
        words.update(re.findall(r'\]\(([^)#\s]+)\)', text()))
        checked = 0
        for word in sorted(words):
            word = word.strip('`')
            built = re.search(r'(?:^|/)build/([^/\s`]+)', word)
            if built:
                with self.subTest(path=word):
                    self.assertIn(built.group(1), names, f'{word}: no build line of the README makes this folder')
                checked += 1
                continue
            if word.startswith(MPAS_TREE) or '<' in word or '://' in word or word.startswith(('/', '-', '$', '.')):
                continue
            if re.fullmatch(r'(?:[\w.-]+/)+[\w.*-]*|[\w-]+\.md', word) and not word.startswith(('lean_mpas/',)):
                if word.split('/')[0] not in ('tools', 'config', 'docs', 'src', 'tests', 'reference', 'cases',
                                              'linux') and not word.endswith('.md'):
                    continue
                with self.subTest(path=word):
                    self.assertTrue(list(ROOT.glob(word)), f'{word} does not exist')
                checked += 1
        self.assertGreater(checked, 3)

    def test_codes(self):
        source = corpus()
        codes = set(re.findall(r'\b[EW]_[A-Z0-9_]+\b', text()))
        self.assertTrue(codes, 'no E_/W_ code')
        for code in sorted(codes):
            with self.subTest(code=code):
                self.assertTrue(code in source, f'{code} is not in the code')
        known = option_strings()
        for word in backticked():
            if word.startswith('--'):
                option = word.split()[0].split('=')[0]
                with self.subTest(option=option):
                    self.assertTrue(option in known, f'{option} is no option of ./lean')
        for message in self.messages():
            pieces = [piece.strip(' ,') for piece in re.split(r'<[^>]*>|\.\.\.', message)]
            pieces = [piece for piece in pieces if len(piece) >= 6]
            with self.subTest(message=message):
                self.assertEqual([piece for piece in pieces if piece not in source], [], 'not in the code')

    def messages(self):
        """The quoted message starts: the first column of 'When it stops', the lines of the Memory section's
        code block, and the stop line under the table."""
        found = []
        body = sections().get('9. When it stops', '')
        for line in body.splitlines():
            if line.startswith('| `'):
                found += re.findall(r'`([^`]+)`', line.split('|')[1])
        found += re.findall(r'`(CRITICAL ERROR: [^`]+)`', body)
        memory = sections().get('6. Memory', '')
        found += re.findall(r'^Memory check:.*$', memory, re.MULTILINE)
        self.assertGreater(len(found), 8, 'the message starts are missing')
        return [message.replace('CRITICAL ERROR: ', '') for message in found]

    def test_options(self):
        options = json.loads((ROOT / 'config/namelist.json').read_text())['options']
        lean = {option.name: 'use' for option in gen_namelist.LEAN_RECORD.options}
        status = {name: entry['status'] for name, entry in options.items()} | lean
        body = text()
        for name in sorted(set(re.findall(r'\bconfig_[a-z0-9_]+\b', body))):
            with self.subTest(option=name):
                self.assertTrue(name in status, f'{name} is no MPAS or &lean option')
        reads = body.split('Options lean_mpas reads as MPAS does:')[1].split('`. ')[0]
        schemes = body.split('Each\nscheme (')[1].split(')')[0] if 'Each\nscheme (' in body else \
            body.split('Each scheme (')[1].split(')')[0]
        fixed = body.split('in one form only must keep that value')[1].split(';')[0]
        for part, allowed in ((reads, {'use', 'scheme'}), (schemes, {'scheme'}), (fixed, {'fixed'})):
            names = re.findall(r'\bconfig_[a-z0-9_]+\b', part)
            self.assertTrue(names)
            for name in names:
                with self.subTest(option=name, said=sorted(allowed)):
                    self.assertIn(status.get(name), allowed, f'{name} is {status.get(name)!r} in config/namelist.json')

    def test_minima(self):
        lock = load_lock()
        need = sections().get('1. What you need', '')
        for tool, key in (('gfortran', 'gfortran'), ('CMake', 'cmake'), ('Ninja', 'ninja'), ('Python', 'python')):
            with self.subTest(tool=tool):
                found = re.search(r'^\| ' + tool + r' \| (\d+(?:\.\d+)*) or newer', need, re.MULTILINE)
                self.assertIsNotNone(found, f'no minimum for {tool}')
                self.assertEqual(found.group(1), lock['tools_min'][key])
        body = text()
        for name, value in (('NVHPC', lock['nvhpc']['version']), ('CUDA', lock['nvhpc']['cuda'])):
            stated = re.findall(name + r' (\d+\.\d+)', body)
            with self.subTest(tool=name):
                self.assertTrue(stated, f'no {name} version')
                self.assertEqual(set(stated), {value})

    def test_self_sufficient(self):
        found = sections()
        for name in STEP_SECTIONS:
            with self.subTest(section=name or 'intro'):
                self.assertTrue(name in found, f'no section {name!r}')
                links = [target for target in re.findall(r'\]\(([^)]+)\)', found.get(name, ''))
                         if '://' not in target]
                self.assertEqual(links, [], 'a step needs a linked file')
        steps = '\n'.join(found.get(name, '') for name in STEP_SECTIONS)
        for needed in ('git clone', './lean doctor', './lean build', 'config_physics_suite', 'NoahmpTable.TBL',
                       'ln -sf', './atmosphere_model'):
            with self.subTest(step=needed):
                self.assertTrue(needed in steps, f'the steps do not say {needed!r}')

    def test_card_default(self):
        card = sections().get('5. On an NVIDIA card', '')
        self.assertTrue(re.search(r'unset `?LEAN_CARD`? means all', card), 'the card section does not say an unset '
                        'LEAN_CARD means all')
        self.assertIn('`LEAN_CARD=none` keeps the physics on the host', card)
        self.assertTrue(CARD_DEFAULT.search((ROOT / 'src/phys_mode.f90').read_text()),
                        'src/phys_mode.f90 parse_stages does not make an unset LEAN_CARD mean all')

if __name__ == '__main__':
    unittest.main()
