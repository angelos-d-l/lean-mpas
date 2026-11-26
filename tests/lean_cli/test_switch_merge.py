"""tools/switch_merge.py: the lines one build compiles from text with #ifdef blocks of a build switch."""
import unittest
from tools.switch_merge import resolve

SWITCH = 'LEAN_EXAMPLE'


class ResolveTest(unittest.TestCase):
    def test_ifdef_and_else(self):
        text = [' a', '#ifdef LEAN_EXAMPLE', ' on', '#else', ' off', '#endif', ' b']
        self.assertEqual(resolve(text, SWITCH, True), [' a', ' on', ' b'])
        self.assertEqual(resolve(text, SWITCH, False), [' a', ' off', ' b'])

    def test_ifndef(self):
        text = ['#ifndef LEAN_EXAMPLE', ' off', '#endif']
        self.assertEqual(resolve(text, SWITCH, True), [])
        self.assertEqual(resolve(text, SWITCH, False), [' off'])

    def test_other_switches_stay(self):
        text = ['#ifdef LEAN_OTHER', ' x', '#endif', '#ifdef LEAN_EXAMPLE', '#ifdef LEAN_OTHER', ' y', '#endif', '#endif']
        self.assertEqual(resolve(text, SWITCH, True), ['#ifdef LEAN_OTHER', ' x', '#endif', '#ifdef LEAN_OTHER', ' y', '#endif'])
        self.assertEqual(resolve(text, SWITCH, False), ['#ifdef LEAN_OTHER', ' x', '#endif'])


if __name__ == '__main__':
    unittest.main()
