"""Reads of an init file for ./lean doctor's memory estimate, with the standard library only:
netCDF-C through ctypes when a shared library is found (any format), else a header reader of CDF-1, CDF-2 and CDF-5
(dimensions only; read_slab then raises NoLibrary and the glacier count is left out).

    dimensions(path) -> {name: length}            the record dimension's length is its record count
    read_slab(path, name, record, first, count)   values of a (Time,) nCells variable: record (1-based, 0 for a
                                                  variable without Time), cells first..first+count-1 (1-based)
    glacier_count(init, mesh=None, record=1, mesh_record=1, slab=65536)   the cells of the program's glacier rule
                                                  (mesh: a separate static file for ivgtyp)
"""
from __future__ import annotations

import ctypes
import ctypes.util
import os
import struct
from pathlib import Path

NC_NOWRITE = 0


class NoLibrary(Exception):
    pass


class NcError(Exception):
    pass


_lib = None


def library():
    """netCDF-C's shared library (NETCDF=<prefix>/lib first, then the linker's search), or None."""
    global _lib
    if _lib is not None:
        return _lib or None
    candidates = []
    prefix = os.environ.get('NETCDF')
    if prefix:
        candidates += [str(Path(prefix) / 'lib' / name) for name in ('libnetcdf.dylib', 'libnetcdf.so')]
    found = ctypes.util.find_library('netcdf')
    if found:
        candidates.append(found)
    candidates += ['/opt/homebrew/lib/libnetcdf.dylib', '/usr/local/lib/libnetcdf.dylib',
                   '/usr/lib/x86_64-linux-gnu/libnetcdf.so', '/usr/lib/aarch64-linux-gnu/libnetcdf.so']
    for name in candidates:
        try:
            _lib = ctypes.CDLL(name)
            return _lib
        except OSError:
            continue
    _lib = False
    return None


def _check(status: int, what: str) -> None:
    if status != 0:
        message = library().nc_strerror
        message.restype = ctypes.c_char_p
        raise NcError(f'{what}: {message(status).decode()}')


def _open(path) -> int:
    lib = library()
    ncid = ctypes.c_int()
    _check(lib.nc_open(str(path).encode(), NC_NOWRITE, ctypes.byref(ncid)), f'{path}')
    return ncid.value


def dimensions(path) -> dict:
    lib = library()
    if lib is None:
        return header_dimensions(path)
    ncid = _open(path)
    try:
        n = ctypes.c_int()
        _check(lib.nc_inq_ndims(ncid, ctypes.byref(n)), 'nc_inq_ndims')
        out = {}
        for dimid in range(n.value):
            name = ctypes.create_string_buffer(257)
            length = ctypes.c_size_t()
            _check(lib.nc_inq_dim(ncid, dimid, name, ctypes.byref(length)), 'nc_inq_dim')
            out[name.value.decode()] = length.value
        return out
    finally:
        lib.nc_close(ncid)


def read_slab(path, name: str, record: int, first: int, count: int, kind: str = 'f') -> list:
    """kind 'f' (float) or 'i' (int); netCDF-C converts the file's type."""
    lib = library()
    if lib is None:
        raise NoLibrary('no netCDF-C shared library')
    ncid = _open(path)
    try:
        varid, ndims = ctypes.c_int(), ctypes.c_int()
        _check(lib.nc_inq_varid(ncid, name.encode(), ctypes.byref(varid)), f'{path}: no variable {name}')
        _check(lib.nc_inq_varndims(ncid, varid, ctypes.byref(ndims)), 'nc_inq_varndims')
        if ndims.value == 2:
            start, size = (ctypes.c_size_t * 2)(max(record, 1) - 1, first - 1), (ctypes.c_size_t * 2)(1, count)
        else:
            start, size = (ctypes.c_size_t * 1)(first - 1), (ctypes.c_size_t * 1)(count)
        if kind == 'i':
            data = (ctypes.c_int * count)()
            _check(lib.nc_get_vara_int(ncid, varid, start, size, data), f'{name}')
        else:
            data = (ctypes.c_float * count)()
            _check(lib.nc_get_vara_float(ncid, varid, start, size, data), f'{name}')
        return list(data)
    finally:
        lib.nc_close(ncid)


def glacier_count(init, mesh=None, record: int = 1, mesh_record: int = 1, slab: int = 65536) -> int:
    """The cells with xice < 0.02 (src/sea_ice.f90 xice_threshold), xland < 1.5 and ivgtyp 15 (src/land_ice.f90
    glacier_category): the cells src/lean_model.f90 starts as glaciers (the sea-ice/glacier branch of its cold start)."""
    mesh = mesh or init
    ncells = dimensions(init)['nCells']
    n = 0
    for first in range(1, ncells + 1, slab):
        count = min(slab, ncells - first + 1)
        xice = read_slab(init, 'xice', record, first, count)
        xland = read_slab(init, 'xland', record, first, count)
        ivgtyp = read_slab(mesh, 'ivgtyp', mesh_record if mesh != init else record, first, count, 'i')
        n += sum(1 for a, b, c in zip(xice, xland, ivgtyp) if a < 0.02 and b < 1.5 and c == 15)
    return n


def header_dimensions(path) -> dict:
    """The dimensions of a CDF-1, CDF-2 or CDF-5 header (NetCDF classic format spec); NcError for another format."""
    with open(path, 'rb') as stream:
        head = stream.read(8)
        if head[:3] != b'CDF' or head[3] not in (1, 2, 5):
            raise NcError(f'{path}: not a CDF-1, CDF-2 or CDF-5 file (a NetCDF-4 file needs netCDF-C)')
        big = head[3] == 5
        count_format = '>q' if big else '>i'
        width = 8 if big else 4
        numrecs = struct.unpack(count_format, head[4:4 + width] if not big else head[4:8] + stream.read(4))[0]
        tag = struct.unpack('>i', stream.read(4))[0]
        n = struct.unpack(count_format, stream.read(width))[0]
        out = {}
        if tag == 0x0A:
            for _ in range(n):
                length = struct.unpack(count_format, stream.read(width))[0]
                name = stream.read(length).decode()
                stream.read((4 - length % 4) % 4)
                size = struct.unpack(count_format, stream.read(width))[0]
                out[name] = numrecs if size == 0 else size
        return out
