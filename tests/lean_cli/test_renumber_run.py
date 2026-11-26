"""Run-directory renumbering on a two-cell mesh with two Time records."""
import contextlib
import io
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

try:
    import netCDF4 as nc
    import numpy as np
except ImportError:
    nc = None

if nc is not None:
    sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
    import renumber_run
    from renumber_grid import load_permutations, raw_values


INIT = 'tiny.init.nc'
LBC = 'lbc.2026-11-08_00.00.00.nc'
SFC = 'surface/sfc_update.nc'
TIMES = np.array([list('2026-11-08_00:00:00'), list('2026-11-08_03:00:00')], dtype='S1') if nc else None


def field(f, name, dtype, dims, values, **kwargs):
    v = f.createVariable(name, dtype, dims, **kwargs)
    v[:] = values
    return v


def time_fields(f):
    f.createDimension('StrLen', 19)
    f.createDimension('nVertLevels', 2)
    xtime = field(f, 'xtime', 'S1', ('Time', 'StrLen'), TIMES)
    xtime._Encoding = 'ascii'


def make_init(path):
    with nc.Dataset(path, 'w', format='NETCDF3_64BIT_OFFSET') as f:
        f.title = 'Two adjacent quadrilaterals'
        for name, size in {'Time': None, 'nCells': 2, 'nEdges': 7, 'nVertices': 6,
                           'maxEdges': 4, 'TWO': 2, 'vertexDegree': 3}.items():
            f.createDimension(name, size)
        time_fields(f)
        field(f, 'lonCell', 'f8', ('nCells',), [1., 0.])
        field(f, 'latCell', 'f8', ('nCells',), [0., 0.])
        field(f, 'cellsOnEdge', 'i4', ('nEdges', 'TWO'),
              [[1, 0], [1, 2], [1, -1], [1, 3], [2, 0], [2, 0], [2, 0]])
        field(f, 'verticesOnEdge', 'i4', ('nEdges', 'TWO'),
              [[1, 2], [2, 3], [3, 4], [4, 1], [2, 5], [5, 6], [6, 3]])
        field(f, 'nEdgesOnCell', 'i4', ('nCells',), [4, 4])
        field(f, 'edgesOnCell', 'i4', ('nCells', 'maxEdges'), [[1, 2, 3, 4], [5, 6, 7, 2]])
        field(f, 'cellsOnCell', 'i4', ('nCells', 'maxEdges'), [[0, 2, -1, 3], [0, 0, 0, 1]])
        field(f, 'verticesOnCell', 'i4', ('nCells', 'maxEdges'), [[1, 2, 3, 4], [2, 5, 6, 3]])
        field(f, 'cellsOnVertex', 'i4', ('nVertices', 'vertexDegree'),
              [[1, 0, 0], [1, 2, 0], [1, 2, 0], [1, 0, 0], [2, 0, 0], [2, 0, 0]])
        field(f, 'edgesOnVertex', 'i4', ('nVertices', 'vertexDegree'),
              [[4, 1, 0], [1, 2, 5], [2, 3, 7], [3, 4, 0], [5, 6, 0], [6, 7, 0]])
        for entity, dim in [('Cell', 'nCells'), ('Edge', 'nEdges'), ('Vertex', 'nVertices')]:
            n = len(f.dimensions[dim])
            field(f, f'indexTo{entity}ID', 'i4', (dim,), np.arange(11, 11 + n))
        field(f, 'theta', 'f4', ('Time', 'nCells', 'nVertLevels'), np.arange(8).reshape(2, 2, 2))
        field(f, 'u', 'f4', ('Time', 'nEdges', 'nVertLevels'), np.arange(28).reshape(2, 7, 2))
        field(f, 'cellPairs', 'i4', ('Time', 'nCells', 'nCells'), np.arange(8).reshape(2, 2, 2))


def make_lbc(path, n_cells=2, format='NETCDF3_64BIT_DATA'):
    with nc.Dataset(path, 'w', format=format) as f:
        f.createDimension('Time', None if format == 'NETCDF3_64BIT_DATA' else 2)
        f.createDimension('nCells', n_cells)
        f.createDimension('nEdges', 7)
        time_fields(f)
        theta = np.arange(4 * n_cells, dtype='i2').reshape(2, n_cells, 2)
        theta[1, 0, 0] = -999
        v = field(f, 'lbc_theta', 'i2', ('Time', 'nCells', 'nVertLevels'), theta, fill_value=-999)
        v.scale_factor = 0.5
        v.add_offset = 250.
        v.units = 'K'
        u = np.arange(28, dtype='f4').reshape(2, 7, 2)
        u[1, 0] = [np.nan, -0.]
        field(f, 'lbc_u', 'f4', ('Time', 'nEdges', 'nVertLevels'), u)


@unittest.skipIf(nc is None, 'needs netCDF4 and numpy')
class RenumberRunTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.src, self.dst = Path(tmp.name) / 'old', Path(tmp.name) / 'new'
        self.src.mkdir()
        make_init(self.src / INIT)
        make_lbc(self.src / LBC)
        (self.src / SFC).parent.mkdir()
        make_lbc(self.src / SFC, format='NETCDF3_CLASSIC')
        for name in ['surface/streams', 'tiny.graph.info.part.2', 'namelist.atmosphere']:
            (self.src / name).touch()
        (self.src / 'tables').mkdir()
        self.sidecar = self.dst / (INIT + '.perm.npz')

    def cli(self, *args):
        out, err = io.StringIO(), io.StringIO()
        with patch.object(sys, 'argv', ['renumber_run.py', *map(str, args)]):
            with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
                status = renumber_run.main()
        return status, out.getvalue(), err.getvalue()

    def renumber_directory(self):
        status, out, err = self.cli(self.src, self.dst, '--init', INIT, '--also', 'surface/*.nc')
        self.assertEqual(status, 0, out + err)
        return err

    def test_directory_preserves_formats_metadata_and_axes(self):
        self.assertIn('tiny.graph.info.part.2 uses the original mesh order', self.renumber_directory())
        status, out, err = self.cli('--check', self.src, self.dst, '--init', INIT)
        self.assertEqual(status, 0, out + err)
        self.assertEqual(out.count('CHECK OK'), 3)
        for name in ['surface/streams', 'tiny.graph.info.part.2', 'namelist.atmosphere', 'tables']:
            self.assertEqual((self.dst / name).readlink(), (self.src / name).resolve())
        for name, format in [(INIT, 'NETCDF3_64BIT_OFFSET'), (LBC, 'NETCDF3_64BIT_DATA'), (SFC, 'NETCDF3_CLASSIC')]:
            with nc.Dataset(self.dst / name) as f:
                raw_values(f)
                self.assertEqual(f.file_format, format)
                self.assertEqual(len(f.dimensions['Time']), 2)
                self.assertEqual(f.dimensions['Time'].isunlimited(), name != SFC)
                self.assertEqual(f['xtime']._Encoding, 'ascii')
                np.testing.assert_array_equal(f['xtime'][:], TIMES)
        with nc.Dataset(self.dst / INIT) as f:
            self.assertEqual(f.title, 'Two adjacent quadrilaterals')
            np.testing.assert_array_equal(f['cellPairs'][1], [[7, 6], [5, 4]])
            np.testing.assert_array_equal(f['theta'][1], [[6, 7], [4, 5]])
            np.testing.assert_array_equal(f['indexToCellID'][:], [1, 2])
            self.assertEqual(set(f['cellsOnEdge'][:].ravel()), {-1, 0, 1, 2, 3})
        with nc.Dataset(self.dst / LBC) as f:
            raw_values(f)
            v = f['lbc_theta']
            self.assertEqual(v.dimensions, ('Time', 'nCells', 'nVertLevels'))
            self.assertEqual(v.dtype, np.dtype('i2'))
            self.assertEqual((v._FillValue, v.scale_factor, v.add_offset, v.units), (-999, 0.5, 250., 'K'))
            np.testing.assert_array_equal(v[1], [[6, 7], [-999, 5]])
            edges = load_permutations(self.sidecar)['edge']
            np.testing.assert_array_equal(edges, [1, 4, 5, 6, 0, 2, 3])
            np.testing.assert_array_equal(f['lbc_u'][0], np.arange(14).reshape(7, 2)[edges])

    def test_wrong_lbc_record_fails_file_and_directory_check(self):
        self.renumber_directory()
        with nc.Dataset(self.dst / LBC, 'a') as f:
            raw_values(f)
            f['lbc_theta'][1] = f['lbc_theta'][1][::-1]
        (self.dst / 'other.grid.nc.perm.npz').touch()
        status, out, err = self.cli('--check', self.src / LBC, self.dst / LBC)
        self.assertEqual(status, 1, out + err)
        self.assertIn('lbc_theta record 1 differs', out)
        status, out, err = self.cli('--check', self.src, self.dst, '--init', INIT)
        self.assertEqual(status, 1, out + err)
        self.assertIn('lbc_theta record 1 differs', out)

    def test_apply_rejects_wrong_mesh_size(self):
        self.renumber_directory()
        wrong, output = self.src / 'wrong.nc', self.dst / 'wrong.nc'
        make_lbc(wrong, n_cells=3)
        status, out, err = self.cli('--apply', self.sidecar, wrong, output)
        self.assertEqual(status, 1, out + err)
        self.assertIn('nCells has size 3, permutation needs 2', err)
        self.assertFalse(output.exists())

    def test_apply_then_check_with_explicit_permutation(self):
        self.renumber_directory()
        output = self.src.parent / 'applied.nc'
        status, out, err = self.cli('--apply', self.sidecar, self.src / LBC, output)
        self.assertEqual(status, 0, out + err)
        status, out, err = self.cli('--check', self.src / LBC, output, '--perm', self.sidecar)
        self.assertEqual(status, 0, out + err)


if __name__ == '__main__':
    unittest.main()
