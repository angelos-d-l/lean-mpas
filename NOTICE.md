# Notices

lean_mpas is a rewrite of the model core of MPAS-Atmosphere 8.4.2. Much of it is translated from MPAS and from the
physics packages MPAS ships, and some of their files are included unchanged. Each part keeps the terms of its source.
This file says which part comes from where. The texts are in `LICENSE`, `LICENSE-MPAS` and `licenses/`, and inside
the files that carry a notice of their own.

lean_mpas is a modified, derivative work. It is not the version of MPAS available from Los Alamos National Laboratory
and UCAR.

## What this means for the package as a whole

lean_mpas contains Noah-MP, and the Noah-MP terms say: "You shall not sell, license or transfer for a fee the
Software, or any work that in any manner contains the Software." So lean_mpas as a whole may be used, changed and
passed on without charge, and may not be sold, licensed or transferred for a fee. RRTMG's code, too, may not be sold.

## Which part comes from where

| Part of this repository | Comes from | Terms |
|---|---|---|
| Everything not listed in the rows below | lean_mpas's own code, and code translated or copied from MPAS-Atmosphere 8.4.2: the dynamics, the drivers of the physics schemes, the start-up, streams, clock and diagnostics. `docs/code-map.md` says which file came from which MPAS file. Copied from MPAS: `third_party/mpas-8.4.2/src/framework/` and the `.F` files directly under its `physics/`, `reference/mpas_defaults/`, the `mpas_*.F` files and `summarize_timestep.inc` of `reference_fixes/`, `tests/diag/mpas_oracle.f90` | BSD 3-clause: `LICENSE` for lean_mpas's own code, `LICENSE-MPAS` (Los Alamos National Security and UCAR) for what comes from MPAS |
| `src/noahmp*.f90`, `src/land_ice*.f90`, `src/model_startup_io.f90` and their copies in `src/device/`; `tests/land_ice/pools_module.f90`; `third_party/mpas-8.4.2/src/core_atmosphere/physics/physics_noahmp/`; `reference/config/NoahmpTable.TBL` | Noah-MP (UCAR), with MPAS's land driver in `src/noahmp_coupling.f90` | the Noah-MP terms, `licenses/Noah-MP-LICENSE.txt`; MPAS's driver code as in the first row |
| `src/thompson.f90`, `src/gf*.f90`, `src/sea_ice*.f90` and their copies in `src/device/`; `src/radiation_lw.f90`, `src/radiation_sw.f90` and the effective radii in `src/radiation_cloud.f90`; `third_party/.../physics_wrf/module_mp_radar.F` and `module_ra_rrtmg_vinterp.F`; `reference_fixes/module_mp_thompson.F` and `module_cu_gf.mpas.F`; `LANDUSE.TBL`, `VEGPARM.TBL`, `SOILPARM.TBL`, `GENPARM.TBL` of `reference/config/` and the `OZONE_*.TBL` files of `reference/radiation/` | the WRF physics that MPAS ships, with MPAS's driver code in `src/gf.f90` and `src/sea_ice.f90` | public domain, `licenses/WRF-PUBLIC-DOMAIN-NOTICE.txt`; MPAS's driver code as in the first row |
| `src/radiation_lw_kernels.f90`, `src/radiation_sw_kernels.f90` and their copies in `src/device/`; `src/device/radiation_fast.f90`, `radiation_fast_gas.f90`, `radiation_device_tables.f90`; `reference/radiation/RRTMG_LW_DATA` and `RRTMG_SW_DATA` | RRTMG, Atmospheric and Environmental Research (AER), as WRF and MPAS ship it | RRTMG's notice: in each source file, and below for the two data files |
| `third_party/.../physics_wrf/module_ra_rrtmg_lw.F` and `module_ra_rrtmg_sw.F`, unchanged | RRTMG (AER), with WRF's interface routines, the public-domain random generator `kissvec` and, in the longwave file, a Mersenne Twister module | RRTMG's notice for RRTMG's modules; public domain for WRF's routines and `kissvec`; the notice of Makoto Matsumoto and Takuji Nishimura inside the file for the Mersenne Twister |
| `src/mynn*.f90`, `src/gwdo*.f90` and their copies in `src/device/` | MMM-physics (NCAR), with WRF's interface routines and MPAS's driver code | BSD 3-clause, `licenses/MMM-physics-LICENSE.txt`; WRF's routines public domain; MPAS's driver code as in the first row |
| `src/run/lean_xml.f90` | ezxml (Aaron Voisine) and MPAS's `xml_stream_parser.c` | MIT, `licenses/ezxml-LICENSE.txt`, and MPAS's licence |
| `third_party/mpas-8.4.2/src/external/esmf_time_f90/`, unchanged | the ESMF time manager as MPAS and WRF ship it | see "The ESMF time manager" below |

The tables of `reference/config/` and `reference/radiation/` are the files MPAS's build downloads into its
`physics_wrf/files/` folder (from the MPAS-Data repository, which states no terms of its own). They are WRF's tables,
Noah-MP's table and RRTMG's data.

## Noah-MP

The Noah-MP terms (`licenses/Noah-MP-LICENSE.txt`, also beside the Noah-MP files in `third_party/`) grant a
royalty-free licence "to use, create derivative works, publish, distribute, disseminate, transfer, modify, revise and
copy" Noah-MP, and forbid selling it or a work that contains it (quoted above).

The notice they require with each copy:

"The source of this material is the Research Applications Laboratory at the National Center for Atmospheric
Research, a program of the University Corporation for Atmospheric Research (UCAR) pursuant to a Cooperative
Agreement with the National Science Foundation; ©2007 University Corporation for Atmospheric Research. All
Rights Reserved."

The notice they require on scholarly works related to or derived from Noah-MP:

"The Noah-MP modeling system was developed at the National Center for Atmospheric Research (NCAR) with collaborations
from university partners. NCAR is sponsored by the United States National Science Foundation."

## RRTMG

RRTMG's notice, as its modules in MPAS 8.4.2 state it. The years differ by module: 2006-2008 in `mcica_subcol_gen_lw`
and `mcica_subcol_gen_sw`, 2002-2009 in `rrtmg_lw_cldprmc` and `rrtmg_lw_taumol`, 2002-2008 in the others.

"Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER). This software may be used, copied, or
redistributed as long as it is not sold and this copyright notice is reproduced on each copy made. This model is
provided as is without any express or implied warranties. (http://www.rtweb.aer.com/)"

The notice says nothing about changed versions. lean_mpas's RRTMG files are translations with changes (fixed-size
work arrays, a card version); each carries the notice.

## The ESMF time manager

lean_mpas's clock (`src/run/lean_clock.f90`) calls the ESMF time manager, 17 files that MPAS and WRF ship and that are
included here unchanged. Eleven of them carry this header:

"Earth System Modeling Framework. Copyright 2002-2003, University Corporation for Atmospheric Research, Massachusetts
Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for
Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight
Center. Licensed under the GPL."

The header names no version of the GPL, and neither MPAS nor WRF includes its text. The other six files
(`ESMF.F90`, `ESMF_ShrTimeMod.F90`, `ESMF_Stubs.F90`, `MeatMod.F90`, `wrf_error_fatal.F90`, `wrf_message.F90`) state
no terms. lean_mpas passes these files on exactly as MPAS does; a program built from lean_mpas links them, as MPAS's
own `atmosphere_model` does.

## Data in the release assets

The case inputs attached to the releases (`cases/README.md`) are files made with MPAS's `init_atmosphere` from:

- the GFS forecast of NOAA/NCEP of 2026-11-08 00 UTC (public domain);
- meshes that UCAR distributes for MPAS (`x1.2562`, and a regional cut of `x1.163842`);
- the geographical datasets that UCAR distributes for MPAS: GMTED2010 terrain (USGS and NGA), MODIS land use, green
  fraction and albedo (NASA), STATSGO soil categories (USDA), and SoilGrids (ISRIC - World Soil Information, CC BY 4.0).

## No endorsement

lean_mpas is not a product of UCAR, NCAR, Los Alamos National Laboratory or AER, and none of them endorses or
supports it.
