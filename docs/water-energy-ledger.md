# The water and energy ledger (`LEAN_BUDGET`)

A build switch for finding physical leaks. A card build with `LEAN_BUDGET` prints one line per step on standard output,
`budget <step> <47 values>`: global means of the water and energy each scheme hands the dynamics, sampled after the
physics and before the dynamics. `tools/budget_summary.py` turns the lines into a closure per scheme. Without the
switch nothing of it is compiled.

```sh
./lean build --flavor lean --target gpu --switch LEAN_BUDGET=on
cd /path/to/run_directory && /path/to/lean_mpas/build/<build>/lean_mpas > run.out
python3 tools/budget_summary.py run.out - - 600   # '-': skip a 2 h spin-up / to the last step; dt in s; needs NumPy
```

The ledger is computed on the card path: keep every physics stage on the card (`LEAN_CARD` unset, the default of a card
build). On a regional mesh the column closures hold (PBL, Grell-Freitas, radiation), but the whole-atmosphere terms
also carry what crosses the lateral boundary, so the ledger is most useful on a global mesh. It adds global sums
to every step: a build for checks, not for forecasts.

## What it closes

| Scheme | Closure |
|---|---|
| MYNN water | column tendency against the surface moisture flux |
| Grell-Freitas water | column tendency plus its rain |
| dynamics and Thompson water | change of total water against the tendencies and Thompson's surface precipitation |
| radiation | heating against the flux divergence at the model top (RRTMG also keeps layers above the model) |
| MYNN energy | moist static energy of the tendencies against the sensible and latent heat fluxes |
| Grell-Freitas energy | moist static energy of the tendencies, zero when conserved |
| whole model | change of moist total energy against the fluxes |

The mass is the dry mass of the tendency coupling (`src/dyn_handoff.f90`); the latent heats are 2.5e6 and 3.34e5 J/kg.

## An example

The 120 km global mesh x1.40962, a GFS start, 48 h, with the MYNN and Grell-Freitas switches of flavor `lean` except
`LEAN_MYNN_FILL_THL`, steps 13 to 288:

| | |
|---|---|
| MYNN water residual | 0.0000 mm/day (+0.074 without `LEAN_MYNN_RHOSFC`, `LEAN_MYNN_MOIST`, `LEAN_SFC_Q2MR`, `LEAN_MYNN_DEW`, `LEAN_MYNN_HYD_PS`) |
| Grell-Freitas water residual | -0.070 mm/day |
| dynamics and Thompson water residual | -0.012 mm/day |
| shortwave and longwave heating against flux divergence | -0.26 and +0.63 W/m2 |
| MYNN energy residual | +7.4 W/m2 |
| Grell-Freitas energy residual | +4.1 W/m2 |
| balance at the top | -6.0 W/m2 (OLR 247.5 against GFS's 241.4) |

Almost all of the MYNN energy residual is `moisture_check` heating twice: `mynn_tendencies` hands it `thl`, which does
not change when vapour condenses, yet filling a negative `qc` adds `L*dqc` to it, and the `Dth` that follows adds
`L*sqc2` again. A fill is how the condensate of the EDMF plumes reaches the grid, so it happens all the time under
cumulus. `LEAN_MYNN_FILL_THL` makes the check work on a copy of `thl` ([fixes.md](fixes.md)). The same run with it,
W/m2:

| | without | with |
|---|---|---|
| MYNN moist static energy residual | +7.39 | +0.71 |
| whole model, moist total energy residual | +5.54 | -0.68 |
| dynamics residual | -3.64 | -2.70 |
| surface balance | +13.5 | +1.2 |
| ocean latent heat flux (GFS 106.6) | 100.8 | 107.0 |
| net shortwave at the top | 241.5 | 231.9 |
| net shortwave at the surface | 165.6 | 154.9 |
| net longwave at the top | -247.5 | -246.2 |
| balance at the top | -6.0 | -14.4 |

Water stays at 0.0000. The cooler cloud layer holds more cloud and the planet is brighter by about 10 W/m2: a response
to check on a global mesh (see `LEAN_RAD_BLCLOUD` in [fixes.md](fixes.md)).
