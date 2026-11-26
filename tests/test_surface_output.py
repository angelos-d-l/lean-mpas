"""tools/surface_to_nc.py on synthetic surface streams; needs netCDF4 and numpy (LEAN_PY), no model run."""
from pathlib import Path
import struct
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'tools'))
import netCDF4
import numpy as np
from surface_to_nc import FIELDS, STATIC, convert, read_stream


class SurfaceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        (ROOT / 'build').mkdir(exist_ok=True)

    def stream(self, path, steps=(120, 240, 250)):
        names = tuple(FIELDS)
        nc = 3
        static = np.arange(6*nc, dtype='<f4').reshape(6, nc)
        fields = np.arange(len(names)*nc, dtype='<f4').reshape(len(names), nc)
        with path.open('wb') as f:
            f.write(struct.pack('<iif', nc, len(names), 30.))
            for name in names:
                f.write(name.ljust(32).encode('ascii'))
            f.write(static.tobytes())
            for it in steps:
                f.write(struct.pack('<i', it))
                f.write((fields + np.float32(it)).tobytes())
        return names, static, fields

    def test_all_fields_multiple_records_and_time(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='surface-test-') as tmp:
            src, dst = Path(tmp)/'test.sfc', Path(tmp)/'test.nc'
            names, static, fields = self.stream(src)
            convert(src, dst)
            dt, found, raw_static, records = read_stream(src)
            self.assertEqual(names, found)
            np.testing.assert_array_equal(raw_static, static)
            with netCDF4.Dataset(dst) as ds:
                ds.set_auto_mask(False)
                np.testing.assert_array_equal(ds['time'][:], [3600., 7200., 7500.])
                np.testing.assert_array_equal(ds['it'][:], [120, 240, 250])
                for i, name in enumerate(STATIC):
                    np.testing.assert_array_equal(ds[name][:].view('u4'), static[i].view('u4'))
                for i, name in enumerate(names):
                    self.assertEqual(ds[name].dtype, np.dtype('f4'))
                    self.assertEqual(ds[name].lean_source, FIELDS[name][0])
                    np.testing.assert_array_equal(ds[name][:].view('u4'), records['fields'][:, i].view('u4'))

    def test_empty_stream(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='surface-test-') as tmp:
            src, dst = Path(tmp)/'test.sfc', Path(tmp)/'test.nc'
            self.stream(src, ())
            convert(src, dst)
            with netCDF4.Dataset(dst) as ds:
                self.assertEqual(len(ds.dimensions['time']), 0)

    def test_truncation_and_duplicate_names(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='surface-test-') as tmp:
            src = Path(tmp)/'test.sfc'
            self.stream(src)
            data = src.read_bytes()
            for length in (1, 11, 20, 12+32*len(FIELDS), len(data)-1):
                src.write_bytes(data[:length])
                with self.assertRaises(ValueError):
                    read_stream(src)
            src.write_bytes(data[:44] + data[12:44] + data[76:])
            with self.assertRaisesRegex(ValueError, 'duplicate'):
                read_stream(src)

    def test_reject_output_over_input(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='surface-test-') as tmp:
            src = Path(tmp)/'test.sfc'
            self.stream(src)
            before = src.read_bytes()
            with self.assertRaises(ValueError):
                convert(src, src)
            self.assertEqual(src.read_bytes(), before)


if __name__ == '__main__':
    unittest.main()
