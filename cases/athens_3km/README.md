# athens_3km: a 3 km day around Athens

The case of the speed tables in `docs/acceptance.md`. Its mesh and data are not public; the specs and the sums
(`SHA256SUMS`) are here so that the runs can be read and repeated on another mesh of this size.

- **Day:** GFS 2026-11-08 00 UTC on a 3 km regional mesh around Athens: the init file `athens.init.nc`, nine boundary
  files (24 h) and the partition file `athens.graph.info.part.8`, made with `init_atmosphere`.
- **Runs:** `case.txt` (dt 18 s, radiation every 15 min, boundaries on) and `mpas_runs/*.txt`:

| Run | Build | Ranks, card | maxRSS | Card peak | Files |
|---|---|---|---|---|---|
| `strict_1h` | `MPAS-nv-strict` | 1, no card | 5.47 GB | - | 2 history, 5 diag, a restart file at 01:00 |
| `atm8_6h` | `MPAS-nv-atm` | 8, the card through MPS | 1.41 GB per rank | 5541 MiB | 3 history, 7 diag |

  These are the memory peaks of these reference runs; the card speed comparison in `docs/acceptance.md` is a separate
  timing run, pinned on an otherwise idle machine (card peak 5,669 MiB).
  `ref_gnu_p8_1h` and `ref_nv_p8_1h` are MPAS on 8 ranks of plain cores for the speed table; `strict_diag1h` and
  `strict_bit10` are the frequent-diagnostics and step-by-step comparisons.
- **Step by step against `MPAS-nv-strict`** (lean with nvfortran on the CPU, a cold start): 0 of 143 fields differ after 1
  and after 10 steps.
