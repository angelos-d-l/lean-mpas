# The storm output (`LEAN_STORM`)

`LEAN_STORM=/absolute/path/run.storm` writes, on the native mesh, the maxima of updraft helicity and vertical velocity
over every step, as MPAS 8.4.2's convective diagnostics `updraft_helicity_max` and `w_velocity_max` define them. It is
output only. Unset or empty, nothing is allocated, computed or written. [output-formats.md](output-formats.md) has what
the four optional outputs share.

## When

Every step updates the running maxima. A record is written at the step that reaches each whole hour after the run's start
and at the last step (once when both coincide); after writing, both maxima are set back to zero. There is no
record at step 0, so the first record covers steps 1 to its own step, and each later one the steps after the record
before it.

## Definition

As MPAS 8.4.2, `diagnostics/mpas_convective_diagnostics.F` (`convective_diagnostics_update`: lines 154-158 for w, 169-189
for updraft helicity; `integral_zstaggered`: 548-565):

- On every level, the relative vorticity at the cell centre is the kite-area weighted sum of the vertex vorticity divided
  by `areaCell`; it is clipped at zero, as is the layer-mean vertical velocity `0.5*(w(k) + w(k+1))`, and the two are
  multiplied.
- That product is integrated in increasing level order between 2000 and 5000 m above ground, with the layer thickness
  `max(0, min(z_agl(k+1), 5000) - max(z_agl(k), 2000))` and `z_agl(k) = zgrid(k) - zgrid(1)`, all in single precision.
- `w_max` is the largest `w` over the interfaces 1 to `nz` (the top interface `nz+1` is left out).

MPAS adds the kite terms by scattering from the vertices; lean gathers them per cell in `verticesOnCell` order. The sum
of signed terms then rounds differently, so `uh` agrees with MPAS's to rounding, not bit for bit.

## Fields

| Name | Units | Meaning | MPAS counterpart |
|---|---|---|---|
| `uh_max` | m2 s-2 | largest 2-5 km updraft helicity over the steps since the previous record, at least 0 | `updraft_helicity_max` |
| `w_max` | m s-1 | largest vertical velocity over those steps and the interfaces 1..nz, at least 0 | `w_velocity_max` |
| `uh` | m2 s-2 | updraft helicity at the record's step | the value that enters `updraft_helicity_max` |
| `w_col` | m s-1 | largest vertical velocity of the column at the record's step (negative in a descending column) | the value that enters `w_velocity_max` |

## Layout

Fortran unformatted stream, int32 and float32 in the machine's byte order (little-endian on the supported machines), no
record markers or padding. Byte offsets from 0:

| Offset | Content |
|---:|---|
| 0 | `ncells` int32 |
| 4 | `nf` int32, 4 |
| 8 | `dt` float32, seconds |
| 12 | `nf` names, 32 ASCII bytes each, blank padded |
| 140 | the first record |

Record `r` (from 0) starts at `140 + r*(4 + 16*ncells)`. Within it: `it` int32 at 0, then `uh_max`, `w_max`, `uh` and
`w_col`, each `ncells` float32, at 4, `4 + 4*ncells`, `4 + 8*ncells` and `4 + 12*ncells`.

## Conversion

```sh
python3 -I tools/storm_to_nc.py STORM INIT_NC OUT_NC      # needs NumPy and netCDF4
```

`INIT_NC` is the init file of the run (for `latCell`, `lonCell`; its cell count must match). The reader checks the
header, the names, complete records and increasing steps; an empty file converts too. The NetCDF file has an
unlimited `time` and `nCells`, the four fields as float32 with `units`, `long_name` and `mpas_name`, `it` as int32,
`time` as float64 seconds from the run's start, and `lat`, `lon` in degrees.

`tools/products_to_icon.py ... --storm FILE` maps `uh_max` to the regular grid of the products: each grid point takes
the larger of its linearly interpolated value and the values of the mesh cells (outside the specified boundary zone)
whose nearest grid point it is, so that a peak on one cell is not smoothed away. The step of every product record must
be in the storm output, and the cell count and `dt` must agree.
