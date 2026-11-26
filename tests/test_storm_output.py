"""tools/storm_to_nc.py and tools/products_to_icon.py on a synthetic storm stream and mesh; needs netCDF4 and numpy
(LEAN_PY), no model run."""
from pathlib import Path
import struct
import os
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'tools'))
import netCDF4
import numpy as np
from storm_to_nc import FIELDS, INSTANTANEOUS, convert, read_stream
import products_to_icon as icon


class StormTest(unittest.TestCase):
    def setUp(self):
        (ROOT / 'build').mkdir(exist_ok=True)
        self.tmp = tempfile.TemporaryDirectory(dir=ROOT / 'build', prefix='storm-test-')
        self.addCleanup(self.tmp.cleanup)
        self.src = Path(self.tmp.name)/'test.storm'
        self.mesh = Path(self.tmp.name)/'mesh.nc'
        self.dst = Path(self.tmp.name)/'test.nc'
        with netCDF4.Dataset(self.mesh, 'w') as ds:
            ds.createDimension('nCells', 3)
            for name in ('latCell', 'lonCell'):
                ds.createVariable(name, 'f8', ('nCells',))[:] = [.1, .2, .3]

    def stream(self, steps=(120, 240, 250), names=tuple(FIELDS), nc=3, dt=30.):
        fields = np.arange(len(names)*nc, dtype='=f4').reshape(len(names), nc)
        with self.src.open('wb') as f:
            f.write(struct.pack('=iif', nc, len(names), dt))
            for name in names:
                f.write(name.ljust(32).encode('ascii'))
            for it in steps:
                f.write(struct.pack('=i', it))
                f.write((fields + np.float32(it)).tobytes())

    def test_round_trip(self):
        self.stream()
        dt, names, records = read_stream(self.src)
        self.assertEqual(dt, 30.)
        self.assertEqual(names, tuple(FIELDS))
        self.assertIsInstance(records.base, np.memmap)
        convert(self.src, self.mesh, self.dst)
        with netCDF4.Dataset(self.dst) as ds:
            ds.set_auto_mask(False)
            self.assertTrue(ds.dimensions['time'].isunlimited())
            np.testing.assert_array_equal(ds['time'][:], [3600, 7200, 7500])
            np.testing.assert_array_equal(ds['it'][:], [120, 240, 250])
            np.testing.assert_array_equal(ds['lat'][:], np.degrees([.1, .2, .3]))
            np.testing.assert_array_equal(ds['lon'][:], np.degrees([.1, .2, .3]))
            for i, name in enumerate(names):
                self.assertEqual(ds[name].dtype, np.dtype('f4'))
                mpas = ds[name].mpas_method if name in INSTANTANEOUS else ds[name].mpas_name
                self.assertEqual((ds[name].units, ds[name].long_name, mpas), FIELDS[name])
                np.testing.assert_array_equal(ds[name][:].view('u4'), records['fields'][:, i].view('u4'))

    def test_empty(self):
        self.stream(())
        self.assertEqual(len(read_stream(self.src)[2]), 0)
        convert(self.src, self.mesh, self.dst)
        with netCDF4.Dataset(self.dst) as ds:
            self.assertEqual(len(ds.dimensions['time']), 0)

    def test_truncated(self):
        self.stream()
        data = self.src.read_bytes()
        for length in (0, 1, 11, 20, 139, 141, len(data)-1):
            with self.subTest(length=length):
                self.src.write_bytes(data[:length])
                with self.assertRaisesRegex(ValueError, 'truncated'):
                    read_stream(self.src)

    def test_bad_header(self):
        for names in (('uh', 'uh'), ('unknown',)):
            self.stream(names=names)
            with self.assertRaisesRegex(ValueError, 'unknown or duplicate'):
                read_stream(self.src)
        for nc, dt in ((0, 30), (3, 0), (3, float('nan')), (3, float('inf'))):
            self.stream(nc=nc, dt=dt)
            with self.assertRaisesRegex(ValueError, 'dimensions or dt'):
                read_stream(self.src)
        self.src.write_bytes(struct.pack('=iif', 3, 0, 30))
        with self.assertRaises(ValueError):
            read_stream(self.src)

    def test_steps(self):
        for steps in ((0,), (-1,), (120, 120), (240, 120)):
            self.stream(steps)
            with self.assertRaisesRegex(ValueError, 'positive and increasing'):
                read_stream(self.src)

    def test_output_over_input(self):
        self.stream()
        for path in (self.src, self.mesh):
            before = path.read_bytes()
            with self.assertRaisesRegex(ValueError, 'output must differ'):
                convert(self.src, self.mesh, path)
            self.assertEqual(path.read_bytes(), before)

    def test_mesh_size(self):
        self.stream(nc=2)
        with self.assertRaisesRegex(ValueError, 'mesh nCells'):
            convert(self.src, self.mesh, self.dst)

    def test_box_maximum(self):
        old_lon, old_lat = icon.LON, icon.LAT
        self.addCleanup(setattr, icon, 'LON', old_lon)
        self.addCleanup(setattr, icon, 'LAT', old_lat)
        icon.LON = np.array([0., 1., 2.])
        icon.LAT = np.array([0., 1., 2.])
        # A narrow peak is away from the coarse grid nodes; excluded peaks must not enter.
        lon = np.array([0, 2, 0, 2, 1.1, 2.1, 1.2])
        lat = np.array([0, 0, 2, 2, 1.1, 2.1, 1.2])
        keep = np.array([True]*6 + [False])
        values = np.array([0, 0, 0, 0, 100, 200, 300], dtype='f4')
        R = icon.Regrid(lon, lat, keep)
        linear = R(values)
        linear[2, 2] = np.nan
        mapped = icon.box_maximum(linear, values, lon, lat, keep)
        self.assertLess(linear[1, 1], 100)
        self.assertEqual(mapped[1, 1], 100)
        self.assertTrue(np.isnan(mapped[2, 2]))
        self.assertEqual(np.nanmax(mapped), 100)
        self.assertTrue(np.all(mapped[np.isfinite(linear)] >= linear[np.isfinite(linear)]))

    def test_kernel_reset_top_face_and_threads(self):
        tmp = Path(self.tmp.name)
        (tmp/'config.f90').write_text("""module config
 integer, parameter :: ncells=66, nc1=67, nv1=4, nz=2, maxedges=3, vertex_degree=3, dt_s=1800
 real, parameter :: dt=1800.
end module
""")
        (tmp/'driver.f90').write_text("""program test_storm
 use config
 use storm_output
 implicit none
 real :: w(nz+1,nc1), vort(nz,nv1), ka(3,nv1), area(nc1), z(nz+1,nc1)
 integer :: vertices(maxedges,nc1), degree(nc1), kite(maxedges,nc1), c
 character(512) :: path
 call get_command_argument(1,path)
 vort=0.001953125; ka=0.5; area=1.; degree=2; kite=1
 do c=1,nc1
  vertices(:,c)=[1,2,3]
  z(:,c)=[0.,2500.,6000.]
 enddo
 call storm_start(trim(path),z,area)
 w=0.; w(:,1)=[-3.,-2.,-1.]; w(:,2)=[1.,2.,100.]
 call storm_step(1,3,w,vort,vertices,degree,kite,ka,area,z,mod(1,2)==0)
 w=0.
 call storm_step(2,3,w,vort,vertices,degree,kite,ka,area,z,mod(2,2)==0)
 w=0.; w(:,1)=[-4.,-3.,-2.]; w(:,2)=0.5
 call storm_step(3,3,w,vort,vertices,degree,kite,ka,area,z,mod(3,2)==0)
end program
""")
        subprocess.run(['gfortran', '-O2', '-ffp-contract=off', '-ffree-line-length-none', '-fopenmp',
                        'config.f90', str(ROOT/'src/storm_output.f90'), 'driver.f90', '-o', 'kernel'],
                       cwd=tmp, check=True, capture_output=True)
        outputs = []
        for threads in (1, 2):
            path = tmp/f't{threads}.storm'
            subprocess.run([str(tmp/'kernel'), str(path)], cwd=tmp, check=True, capture_output=True,
                           env=dict(os.environ, OMP_NUM_THREADS=str(threads)))
            outputs.append(path.read_bytes())
        self.assertEqual(outputs[0], outputs[1])
        _, names, records = read_stream(tmp/'t2.storm')
        np.testing.assert_array_equal(records['it'], [2, 3])
        data = {n: records['fields'][:, i] for i, n in enumerate(names)}
        # A record is due every second step and at the last step, so the first record (step 2) still holds step 1's
        # maxima; the top face's 100 enters UH but not the w maximum.
        np.testing.assert_array_equal(data['uh_max'][:, :2], [[0., 250.48828125], [0., 2.9296875]])
        np.testing.assert_array_equal(data['w_max'][:, :2], [[0., 2.], [0., .5]])
        np.testing.assert_array_equal(data['uh'][:, :2], [[0., 0.], [0., 2.9296875]])
        np.testing.assert_array_equal(data['w_col'][:, :2], [[0., 0.], [-3., .5]])
        for values in data.values():
            self.assertTrue(np.all(values[:, 2:] == 0))

    def test_missing_storm_record_and_dimensions(self):
        self.stream((120, 250))
        records = icon.storm_records(self.src, 3, 30.)
        np.testing.assert_array_equal(icon.storm_at_step(records, 120), read_stream(self.src)[2]['fields'][0, 0])
        with self.assertRaisesRegex(ValueError, 'product step 240'):
            icon.storm_at_step(records, 240)
        for n, dt in ((2, 30), (3, 18)):
            with self.assertRaisesRegex(ValueError, 'ncells or dt'):
                icon.storm_records(self.src, n, dt)


if __name__ == '__main__':
    unittest.main()
