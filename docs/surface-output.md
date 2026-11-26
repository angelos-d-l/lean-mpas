# The surface output (`LEAN_SURFACE`)

`LEAN_SURFACE=/absolute/path/run.surface` writes the instantaneous surface-layer, radiation and Noah-MP fields of every
cell once an hour, for studying the land surface without full history files. It is output only. Unset or empty,
nothing is allocated or written. [output-formats.md](output-formats.md) has what the four optional outputs share.

```sh
LEAN_SURFACE="$PWD/run.surface" ./atmosphere_model
python3 -I tools/surface_to_nc.py run.surface run.surface.nc     # needs NumPy and netCDF4
```

## When

A record is written at the step that reaches each whole hour after the run's start and at the last step (once when
both coincide). There is no record at step 0. The values are those of the record's step: no averaging, no
interpolation, no humidity conversion.

## Fields, in file order

`land` means `xland < 1.5`. Noah-MP runs only there; over water its fields hold zeros, which are not water values. Every
field is written for every cell.

| Name | Units | Cells | Meaning |
|---|---|---|---|
| `tsk` | K | all | surface skin temperature; over water the start's SST, or the last surface record's with `config_sst_update` |
| `xice` | 1 | all | sea-ice fraction: the start's, or the last surface record's with `config_sst_update` |
| `snow` | kg m-2 | all | snow water equivalent as the radiation sees it; land refreshed every step, water the start's |
| `snowh` | m | all | snow depth |
| `gsw` | W m-2 | all | net downward surface shortwave of the last radiation call (held until the next) |
| `glw` | W m-2 | all | downward surface longwave of the last radiation call (held until the next) |
| `albedo` | 1 | all | surface albedo as the radiation sees it; land refreshed when Noah-MP gives a valid one, water the start's |
| `emiss` | 1 | all | surface emissivity as the radiation sees it; land refreshed every step, water the start's |
| `hfx` | W m-2 | all | upward sensible heat flux |
| `qfx` | kg m-2 s-1 | all | upward moisture flux |
| `lh` | W m-2 | all | upward latent heat flux |
| `ust` | m s-1 | all | friction velocity |
| `znt` | m | all | roughness length |
| `hpbl` | m | all | PBL height |
| `chs` | m s-1 | all | surface exchange coefficient |
| `chs2` | m s-1 | all | 2 m heat exchange coefficient |
| `cqs2` | m s-1 | all | 2 m moisture exchange coefficient |
| `flhc` | W m-2 K-1 | all | heat exchange coefficient |
| `flqc` | kg m-2 s-1 | all | moisture exchange coefficient |
| `qsfc` | kg kg-1 | all | surface specific humidity |
| `br` | 1 | all | bulk Richardson number |
| `zol` | 1 | all | height over Obukhov length |
| `grdflx` | W m-2 | land | ground heat flux, positive into the soil or snow |
| `tslb1` | K | land | top soil layer temperature |
| `smois1` | m3 m-3 | land | top soil layer total moisture |
| `sh2o1` | m3 m-3 | land | top soil layer liquid water |
| `tmn` | K | land | deep soil boundary temperature |
| `tv` | K | land | vegetation temperature |
| `tg` | K | land | ground temperature |
| `tah` | K | land | canopy air temperature |
| `eah` | Pa | land | canopy air vapour pressure |
| `fveg` | 1 | land | vegetation fraction |
| `lai` | m2 m-2 | land | leaf area index |
| `canwat` | mm | land | canopy liquid water plus ice (MPAS's `canwat`) |
| `t2mv` | K | land | 2 m temperature of the vegetated tile |
| `t2mb` | K | land | 2 m temperature of the bare tile |
| `q2mv` | kg kg-1 | land | 2 m specific humidity of the vegetated tile (MPAS's `q2mvxy` output is the mixing ratio) |
| `q2mb` | kg kg-1 | land | 2 m specific humidity of the bare tile (MPAS's `q2mbxy` output is the mixing ratio) |
| `fsa` | W m-2 | land | absorbed surface shortwave |
| `fira` | W m-2 | land | net longwave, positive to the atmosphere |
| `fsh` | W m-2 | land | sensible heat, positive to the atmosphere |
| `fgev` | W m-2 | land | ground evaporation latent heat, positive to the atmosphere |
| `fcev` | W m-2 | land | canopy evaporation latent heat, positive to the atmosphere |
| `fctr` | W m-2 | land | transpiration latent heat, positive to the atmosphere |
| `chv` | m s-1 | land | exchange coefficient of the vegetated tile above the displacement height |
| `chb` | m s-1 | land | exchange coefficient of the bare tile above the displacement height |
| `chv2` | m s-1 | land | 2 m heat exchange coefficient of the vegetated tile |
| `chb2` | m s-1 | land | 2 m heat exchange coefficient of the bare tile |
| `t2` | K | all | 2 m temperature |
| `q2` | kg kg-1 | all | 2 m humidity: a mixing ratio over land; over water see `LEAN_SFC_Q2MR` in [fixes.md](fixes.md) |
| `th2` | K | all | 2 m potential temperature |
| `u10` | m s-1 | all | 10 m zonal wind |
| `v10` | m s-1 | all | 10 m meridional wind |

Before the records, a static block of six fields:

| Field | Units | Meaning |
|---|---|---|
| `xland` | 1 | 1 land, 2 water, at the start |
| `ivgtyp` | category, stored as float32 | vegetation (land-use) category at the start |
| `isltyp` | category, stored as float32 | soil category at the start |
| `lat` | degrees north | cell latitude |
| `lon` | degrees east | cell longitude |
| `terrain` | m | terrain height |

A surface update (`config_sst_update`) can change `xland` and the categories during the run; the static block keeps
those of the start.

## Layout

Fortran unformatted stream, int32 and float32 in the machine's byte order (little-endian on the supported machines), no
record markers or padding. Byte offsets from 0:

| Offset | Content |
|---:|---|
| 0 | `ncells` int32 |
| 4 | `nf` int32, 53 |
| 8 | `dt` float32, seconds |
| 12 | `nf` names, 32 ASCII bytes each, blank padded (1696 bytes) |
| 1708 | the static block: six fields of `ncells` float32 |
| 1708 + 24*ncells | the first record |

A record is `it` int32 followed by the `nf` fields in header order, each `ncells` float32: `4 + 4*nf*ncells` bytes.

The converter writes `time` (unlimited) and `nCells`, the static fields as float32 `(nCells)`, the fields as float32
`(time, nCells)` with `units`, `long_name` and their source in the model, `it` as int32 and `time` as float64 seconds
from the run's start. It refuses a truncated file, unknown or repeated names and steps that do not increase.
