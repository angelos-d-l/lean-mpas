#!/usr/bin/env python3
"""Coverage of the memory sum by measurement: the rows of a case against what a run held at its peak.

    tools/memory_compare.py --case CASE.json --rows ROWS.json [--heap REPORT] [--present DUMP | --notify LIST]
                            [--smi SAMPLES] [--drop auto | --scale auto=0.5]

CASE.json   memory_case's fields as JSON (the doctor's `case`, or written by hand); ROWS.json config/memory_terms.json.
REPORT      a heap tracer's report of the host blocks live at the run's peak (any malloc tracer that records the
            allocating routine and its caller): the lines after 'LIVE AT PEAK', "<bytes> B <count>  <routine> < <caller>"
            (the MiB form is read too, to 0.1 MiB). Only blocks of 1 MiB or more are compared.
DUMP        a list of live device entries that you make yourself (for example from the table acc_present_dump()
            prints, called at the moment to be measured), one entry a line: "<bytes> <name>", or the long form
            "<bytes> <MiB> MiB line:<n> name:<name>(:,:) ..."
LIST        NVCOMPILER_ACC_NOTIFY=16 output: alloc/free lines with key=value fields (bytes=, devaddr=, variable=);
            the entries live at the largest total are taken
SAMPLES     nvidia-smi used_memory of the run's pid, one MiB value a line (or 'pid, <n> MiB'): the peak is P

Checks (each failing line names what failed): every traced routine of 1 MiB or more maps to rows (NO ROW
<routine> <MiB>; TWICE <routine> when its two frames belong to different rows); the host rows projected onto the
tracer's scope (each array of 1 MiB or more, life resident + max(start, step)) within -2 % and +5 % (or 64 MiB) of the
traced peak; device entries matched to card rows by array name (generated rows to the byte, manifest rows within 2 %);
with --smi the card's F within -2 % and +5 % (or 64 MiB) of P. Verdict MEMORY OK or MEMORY FAIL: <n> problem(s).
--drop auto / --scale auto=0.5 change the largest manifest row of the case (on the side measured) before the
checks: a guard must then fail; MUTATION TOO SMALL (exit 2) when that change could stay inside the tolerance.
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from tools.lean import memory  # noqa: E402

MIB = 2 ** 20
SCOPE = MIB          # blocks below 1 MiB are not compared (a heap tracer's usual threshold)


def tolerance(reference: int) -> tuple[int, int]:
    """(low, high) bounds around a measured total: -2 %, and +5 % or 64 MiB, whichever is larger."""
    return int(reference * 0.98), reference + max(int(reference * 0.05), 64 * MIB)


def routine_name(frame: str) -> str:
    """'__lean_model_MOD_run_model_from' -> 'run_model_from'; 'foo_' -> 'foo'."""
    frame = frame.strip()
    if '_MOD_' in frame:
        frame = frame.split('_MOD_', 1)[1]
    return frame.rstrip('_').lower()


def read_heap(text: str) -> list[tuple[int, list[str]]]:
    """[(bytes, [routine, caller])] of the 'LIVE AT PEAK' block."""
    out, active = [], False
    for line in text.splitlines():
        if line.startswith('LIVE AT PEAK'):
            active = True
            continue
        if not active or not line.strip():
            continue
        found = re.match(r'\s*(\d+) B\s+\d+\s+(.*)', line) or re.match(r'\s*([\d.]+) MiB\s+\d+\s+(.*)', line)
        if not found:
            continue
        size = int(found.group(1)) if ' B ' in line else int(float(found.group(1)) * MIB)
        out.append((size, [routine_name(part) for part in found.group(2).split(' < ')]))
    return out


def read_present(text: str) -> list[tuple[int, str]]:
    """'<bytes> <name>' lines, or the long form '<bytes> <MiB> MiB line:<n> name:<name>(:,:) ...' (the name: token)."""
    out = []
    for line in text.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0].isdigit():
            named = [part[5:] for part in parts if part.startswith('name:')]
            name = re.sub(r'\(.*$', '', named[0]) if named else parts[1]
            if '$p%' in name:   # a member reached through a pointer component: g%m$p%zxu is m%zxu
                head, _, member = name.rpartition('$p%')
                name = head.rsplit('%', 1)[-1] + '%' + member
            out.append((int(parts[0]), name))
    return out


def read_notify(text: str) -> list[tuple[int, str]]:
    """The device entries live when the running total was largest."""
    live, total, peak, at_peak = {}, 0, -1, {}
    for line in text.splitlines():
        fields = dict(re.findall(r'(\w+)=(\S+)', line))
        if 'bytes' not in fields:
            continue
        key = fields.get('devaddr') or fields.get('variable') or str(len(live))
        name = fields.get('variable') or fields.get('function') or key
        if line.lstrip().lower().startswith('alloc'):
            live[key] = (int(fields['bytes']), name)
            total += int(fields['bytes'])
            if total > peak:
                peak, at_peak = total, dict(live)
        elif line.lstrip().lower().startswith(('free', 'delete', 'dealloc')) and key in live:
            total -= live.pop(key)[0]
    return list(at_peak.values())


def read_smi(text: str) -> int:
    values = [int(found.group(1)) for found in (re.search(r'(\d+)\s*(?:MiB)?\s*$', line) for line in text.splitlines())
              if found]
    return max(values) * MIB if values else 0


def leaf(name: str) -> str:
    return name.rsplit('%', 1)[-1].lower()


class Comparison:
    def __init__(self, case: dict, terms: dict):
        self.case, self.terms, self.problems, self.notes = case, terms, [], []

    def fail(self, text: str) -> None:
        self.problems.append(text)

    def host_rows(self) -> list[dict]:
        return [row for row in self.terms['rows'] if memory.row_bytes(row, self.case, self.terms, 'host') > 0]

    def projected_host(self) -> int:
        lives = {'resident': 0, 'start': 0, 'step': 0}
        for row in self.terms['rows']:
            lives[row['life']] += sum(b for b in memory.array_bytes(row, self.case, self.terms, 'host').values()
                                      if b >= SCOPE)
        return lives['resident'] + max(lives['start'], lives['step'])

    def heap(self, lines: list[tuple[int, list[str]]]) -> None:
        owners = {}
        for row in self.host_rows():
            for routine in row['routines']:
                owners.setdefault(routine.lower(), set()).add(row['name'])
        traced = 0
        for size, frames in lines:
            if size < SCOPE:
                continue
            traced += size
            claims = [owners.get(frame) for frame in frames[:2]]
            first = claims[0] or (claims[1] if len(claims) > 1 else None)
            if not first:
                self.fail(f'NO ROW {" < ".join(frames)} {size / MIB:.1f}')
            elif len(claims) > 1 and claims[0] and claims[1] and not (claims[0] & claims[1]):
                self.fail(f'TWICE {" < ".join(frames)} ({", ".join(sorted(claims[0] | claims[1]))})')
        projected = self.projected_host()
        low, high = tolerance(traced)
        line = f'host: rows {projected / MIB:.1f} MiB on the tracer\'s scope, traced peak {traced / MIB:.1f} MiB'
        if low <= projected <= high:
            self.notes.append(line)
        else:
            self.fail(line + f' (tolerance {low / MIB:.1f} to {high / MIB:.1f})')

    def device(self, entries: list[tuple[int, str]]) -> None:
        """Each device entry to one card row: the rows whose card_names prefix ('f%') starts its name, else every
        card row, then by the array name (the part after the last %)."""
        sizes = {row['name']: memory.array_bytes(row, self.case, self.terms, 'card') for row in self.terms['rows']}
        found = {}
        for size, name in entries:
            if size < SCOPE:
                continue
            rows = [row for row in self.terms['rows'] if sizes[row['name']] and
                    any(p.endswith('%') and name.lower().startswith(p.lower()) for p in row['card_names'])]
            rows = [row for row in rows or self.terms['rows'] if leaf(name) in {a.lower() for a in sizes[row['name']]}]
            if not rows:
                self.fail(f'NO ROW {name} {size / MIB:.1f}')
            elif len(rows) > 1:
                self.fail(f'TWICE {name} ({", ".join(sorted(row["name"] for row in rows))})')
            else:
                row = rows[0]
                expected = {a.lower(): b for a, b in sizes[row['name']].items()}[leaf(name)]
                key = (row['name'], leaf(name))
                found[key] = (row, expected, found.get(key, (0, 0, 0))[2] + size)
        for (row_name, array), (row, expected, measured) in sorted(found.items()):
            exact = row.get('source') is not None
            ok = measured == expected if exact else abs(measured - expected) <= 0.02 * expected
            if not ok:
                self.fail(f'card {row_name}%{array}: row {expected} B, device {measured} B' + ('' if exact else ' (2 %)'))

    def card_total(self, peak: int, frame_bytes: int, threads: int) -> None:
        ledger = memory.card_needed(self.case, self.terms, frame_bytes, threads)
        low, high = tolerance(peak)
        line = f'card: F {ledger["full"] / MIB:.1f} MiB, process peak {peak / MIB:.1f} MiB'
        if low <= ledger['full'] <= high:
            self.notes.append(line)
        else:
            self.fail(line + f' (tolerance {low / MIB:.1f} to {high / MIB:.1f})')


def mutate(terms: dict, case: dict, how: str, side: str) -> tuple[dict, str, int]:
    """terms with the largest manifest row of the case on `side` dropped or scaled; (terms, row name, bytes removed)."""
    manifest = [row for row in terms['rows'] if row['source'] is None]
    row = max(manifest, key=lambda r: memory.row_bytes(r, case, terms, side))
    before = memory.row_bytes(row, case, terms, side)
    changed = dict(terms)
    if how == 'drop':
        changed['rows'] = [r for r in terms['rows'] if r is not row]
        return changed, row['name'], before
    factor = float(how.split('=', 1)[1])
    scaled = dict(row, arrays=[dict(a, bytes=f'({a["bytes"]})*{int(factor * 1000)}//1000') for a in row['arrays']])
    changed['rows'] = [scaled if r is row else r for r in terms['rows']]
    return changed, row['name'], before - memory.row_bytes(scaled, case, changed, side)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(prog='tools/memory_compare.py', description=__doc__.split('\n')[0])
    parser.add_argument('--case', type=Path, required=True)
    parser.add_argument('--rows', type=Path, required=True)
    parser.add_argument('--heap', type=Path)
    device = parser.add_mutually_exclusive_group()
    device.add_argument('--present', type=Path)
    device.add_argument('--notify', type=Path)
    parser.add_argument('--smi', type=Path)
    parser.add_argument('--frame', type=int, default=0, help='the build\'s card_frame_bytes (with --smi)')
    parser.add_argument('--threads', type=int, default=0, help='SMs x threads per SM of the card (with --smi)')
    mutation = parser.add_mutually_exclusive_group()
    mutation.add_argument('--drop', choices=['auto'])
    mutation.add_argument('--scale', metavar='auto=F')
    args = parser.parse_args(argv)
    terms = memory.load_terms(args.rows)
    case = memory.case_dict(terms, **json.loads(args.case.read_text(encoding='utf-8')))
    heap = read_heap(args.heap.read_text(encoding='utf-8')) if args.heap else None
    entries = (read_present(args.present.read_text(encoding='utf-8')) if args.present else
               read_notify(args.notify.read_text(encoding='utf-8')) if args.notify else None)
    if args.drop or args.scale:
        side = 'host' if heap is not None else 'card'
        how = 'drop' if args.drop else 'scale=' + args.scale.split('=', 1)[1]
        original = terms
        terms, name, removed = mutate(terms, case, how, side)
        row = next(r for r in original['rows'] if r['name'] == name)
        named = any(size >= SCOPE for size in memory.array_bytes(row, case, original, side).values())
        if how == 'drop' or side == 'card':
            small = not named    # a dropped row's traced arrays give NO ROW; a scaled card row misses its 2 %
        else:
            measured = sum(size for size, _ in heap if size >= SCOPE)
            low, high = tolerance(measured)
            small = removed <= measured - low   # the projection falls by `removed`: only the -2 % side
        if small:
            print(f'MUTATION TOO SMALL {name}: {removed / MIB:.1f} MiB stays inside the tolerance')
            return 2
        print(f'MUTATION {how} {name} ({removed / MIB:.1f} MiB)')
    check = Comparison(case, terms)
    if heap is not None:
        check.heap(heap)
    if entries is not None:
        check.device(entries)
    if args.smi:
        check.card_total(read_smi(args.smi.read_text(encoding='utf-8')), args.frame, args.threads)
    for line in check.notes:
        print('ok    ' + line)
    for line in check.problems:
        print(line)
    print(f'MEMORY FAIL: {len(check.problems)} problem(s)' if check.problems else 'MEMORY OK')
    return 1 if check.problems else 0


if __name__ == '__main__':
    sys.exit(main())
