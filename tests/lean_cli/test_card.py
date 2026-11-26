"""Device 0, the card's compute capability for the build, and the card build's stack frame."""
import contextlib
import io
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools.lean import build, build_info, card, cli, config
from tools.lean.config import BuildConfig

SMI = ('GPU-aaaaaaaa-0000-0000-0000-000000000000, NVIDIA Test Card A, 12227, 11000, 200, 595.99.02\n'
       'GPU-bbbbbbbb-0000-0000-0000-000000000000, NVIDIA Test Card B, 20475, 20000, 10, 595.99.02\n')


class FakeDriver:
    """Two cards; CUDA_VISIBLE_DEVICES reorders them as libcuda would."""
    cards = [dict(uuid='GPU-aaaaaaaa-0000-0000-0000-000000000000', name='NVIDIA Test Card A', cc=(12, 0), sms=48),
             dict(uuid='GPU-bbbbbbbb-0000-0000-0000-000000000000', name='NVIDIA Test Card B', cc=(8, 9), sms=48)]

    def __init__(self, cards=None):
        self.cards = cards if cards is not None else self.cards
        visible = os.environ.get('CUDA_VISIBLE_DEVICES')
        self.visible = [self.cards[int(i)] for i in visible.split(',')] if visible else list(self.cards)

    def count(self):
        return len(self.visible)

    def uuid(self, i):
        return self.visible[i]['uuid']

    def name(self, i):
        return self.visible[i]['name']

    def attribute(self, i, a):
        c = self.visible[i]
        return {75: c['cc'][0], 76: c['cc'][1], 16: c['sms'], 39: 1536}[a]

    def cuda_version(self):
        return '13.2'


class CardTest(unittest.TestCase):
    def test_device_zero_follows_cuda_visible_devices(self):
        smi = card.smi_lines(lambda command: SMI)
        with mock.patch.dict(os.environ, {'CUDA_VISIBLE_DEVICES': '1'}):
            found = card.device_zero(FakeDriver(), smi)
        self.assertEqual(found.uuid, 'GPU-bbbbbbbb-0000-0000-0000-000000000000')
        self.assertEqual((found.cc, found.threads, found.total_mib, found.free_mib), (89, 48 * 1536, 20475, 20000))
        with mock.patch.dict(os.environ, {'CUDA_VISIBLE_DEVICES': ''}):
            found = card.device_zero(FakeDriver(), smi)
        self.assertEqual((found.uuid[:12], found.cc, found.free_mib), ('GPU-aaaaaaaa', 120, 11000))

    def test_no_driver_no_card(self):
        class Missing:
            def count(self):
                raise OSError('no libcuda')
        self.assertIsNone(card.device_zero(Missing(), []))
        self.assertIsNone(card.device_zero(FakeDriver(cards=[]), []))

    def test_largest_frame(self):
        text = ('Function _Z1av:\n REG:40 STACK:9744 SHARED:0 LOCAL:0\nFunction _Z1bv:\n REG:20 STACK:120 SHARED:0\n')
        self.assertEqual(card.largest_frame(text), 9744)
        self.assertEqual(card.largest_frame('nothing'), 0)


class FrameTest(unittest.TestCase):
    cfg = BuildConfig('gpu', 'nvfortran', 'strict')

    def two_links(self, frames):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp)
            build_info.write_build_info(self.cfg, build_dir, describe='abc', system='Linux')
            links, reads = [], iter(frames)
            try:
                frame = build.link_frame(self.cfg, build_dir, lambda: links.append(
                    (build_dir / build_info.GENERATED).read_text()), lambda: next(reads))
            except build.BuildError as error:
                return error, links
            return frame, links

    def test_two_links_write_the_frame(self):
        frame, links = self.two_links([9744, 9744])
        self.assertEqual(frame, 9744)
        self.assertEqual(len(links), 1, 'Ninja runs a second time')
        self.assertIn('integer, parameter :: card_frame_bytes = 9744', links[0])

    def test_frame_moved_between_links_fails(self):
        error, links = self.two_links([9744, 9760])
        self.assertEqual(error.code, 'E_CARD_FRAME')
        self.assertEqual(error.detail, 'the largest kernel stack frame moved from 9744 to 9760 bytes between the two links')

    def test_unchanged_frame_links_once(self):
        with tempfile.TemporaryDirectory() as tmp:
            build_dir = Path(tmp)
            build_info.write_build_info(self.cfg, build_dir, describe='abc', system='Linux', frame=9744)
            build_info.write_build_info(self.cfg, build_dir, describe='abc', system='Linux')   # a configure keeps it
            self.assertIn('card_frame_bytes = 9744', (build_dir / build_info.GENERATED).read_text())
            links = []
            self.assertEqual(build.link_frame(self.cfg, build_dir, lambda: links.append(1), lambda: 9744), 9744)
            self.assertEqual(links, [])

    def test_cpu_frame_is_zero(self):
        text = build_info.build_info_text(BuildConfig('cpu', 'gfortran', 'strict'), describe='abc', system='Darwin',
                                          frame=9744)
        self.assertIn(' integer, parameter :: card_frame_bytes = 0', text.splitlines())


class GpuCcTest(unittest.TestCase):
    def resolve(self, *args, driver=None):
        with mock.patch('tools.lean.build.build', return_value=0) as run, \
                mock.patch('tools.lean.card.device_zero', return_value=driver):
            self.assertEqual(cli.main(['build', '--target', 'gpu', *args]), 0)
        return run.call_args.args[0]

    def test_cc_flags(self):
        cfg = self.resolve('--gpu-cc', '89')
        options, _ = config.unit_flags(cfg, 'dyn_tend', 'model')
        self.assertIn('-gpu=cc89,nofma', options)
        self.assertIn('-gpu=cc89,nofma', config.link_flags(cfg))
        self.assertNotIn('-gpu=cc120,nofma', options + config.link_flags(cfg))
        ada = card.Card('GPU-b', 'B', 89, 48, 1536)
        self.assertEqual(self.resolve(driver=ada).gpu_cc, 89, "device 0's cc by default")
        for driver in (card.Card('GPU-a', 'A', 120, 48, 1536), None):
            with self.subTest(driver=driver):
                cfg = self.resolve(driver=driver)
                self.assertIsNone(cfg.gpu_cc)
                self.assertIn('-gpu=cc120,nofma', config.unit_flags(cfg, 'dyn_tend', 'model')[0])
                self.assertEqual(config.unit_flags(cfg, 'dyn_tend', 'model'),
                                 config.unit_flags(BuildConfig('gpu', 'nvfortran', 'strict'), 'dyn_tend', 'model'))

    def test_gpu_cc_needs_the_gpu_target(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            self.assertEqual(cli.main(['build', '--gpu-cc', '89']), 2)
        self.assertIn('--gpu-cc needs the gpu target', err.getvalue())


if __name__ == '__main__':
    unittest.main()
