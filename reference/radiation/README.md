# RRTMG and ozone tables

Copied unchanged from MPAS 8.4.2's `src/core_atmosphere/physics/physics_wrf/files/`. Sums in `SHA256SUMS`.

- `RRTMG_LW_DATA`, `RRTMG_SW_DATA`: the single-precision files (not the `.DBL` ones), Fortran sequential unformatted,
  **big-endian with 4-byte record markers** (the first marker of `RRTMG_LW_DATA`, `00 00 58 80`, is 22656 bytes read
  big-endian). MPAS is built with big-endian unformatted I/O (gfortran `-fconvert=big-endian`, nvfortran
  `-byteswapio`); on a little-endian machine open them with `convert='big_endian'`, as lean's table reader does.
- `OZONE_PLEV.TBL`, `OZONE_LAT.TBL`, `OZONE_DAT.TBL`: the ozone climatology RRTMG uses (`config_o3climatology = true`).
