# AGENTS.md

For an AI coding agent (or a person) who is asked to change lean_mpas. `README.md` says what lean_mpas is and how to
build and run it; this file says how to change it without breaking it.

## What you are working on

lean_mpas is a single-precision Fortran rewrite of the `atmosphere_model` of MPAS-Atmosphere 8.4.2. It runs in an
MPAS run directory, as one process, on the CPU or on one NVIDIA card. The numerics and the physics are MPAS's, in
MPAS's order of operations; the framework around them (registry, pools, MPI, PIO) is not there. The reference for
every formula is MPAS's own source, which `./lean fetch-upstream` puts in `reference/mpas_src/`.

Read these first, in this order:

1. `README.md`: build, run directory, the card, memory, restart.
2. `docs/code-map.md`: the folders, the start-up path, one model step with the file of each part, which lean file
   came from which MPAS file.
3. `docs/fixes.md`: every place where lean departs from MPAS on purpose, and why.

## Set up and check

```sh
./lean doctor                      # the tools; ends with DOCTOR OK
./lean build --flavor lean         # CPU, gfortran; prints BUILD OK and the build folder
sh tests/run_all.sh quick          # about 2 minutes, no model build
```

The case inputs are release assets (`cases/README.md` says where they go). With them, "Running a case with lean" in
`cases/README.md` gives a forecast of `greece_60km` in about a minute on a laptop. Use that case for every experiment:
it has lateral boundaries, land, sea and all the physics, and it is small.

## The rule: no change without a comparison

A change is finished when you can say what it did to the forecast.

1. Before the change, run a case and keep its run directory.
2. Make the change, rebuild, run the same case into a second run directory.
3. Compare them:

   ```sh
   $LEAN_PY tools/compare_runs.py runs/before runs/after
   ```

   It prints, per output file, `identical` or the variables that differ, how many values and by how much.

- A change that should not alter results (a clean-up, a speed-up, a new output field, new error handling) must give
  `identical` on every file. If it does not, find out why before going on. Do not accept a small difference as noise:
  on one machine with one build, lean's results are reproducible to the last bit.
- A change that should alter results (a physics change, a bug fix) must alter only what you expect. Say in your report
  which fields changed and by how much, and whether that is what the change was meant to do.
- Then run the tests: `sh tests/run_all.sh quick`, and `sh tests/run_all.sh build` when you touched Fortran.

## Where a change goes

- **A physics change** goes behind a build switch, so that `--flavor mpas` stays MPAS 8.4.2's physics unchanged:
  an `#ifdef LEAN_<NAME>` in the source, an entry in `config/switches.json`, and an entry in `docs/fixes.md` (what, the
  MPAS file and line, why). Try it with `./lean build --flavor lean --switch LEAN_<NAME>=on`. It joins the `lean`
  flavor (`config/presets.json`) only when its effect is known.
- **A bug in MPAS's code** that lean inherited is fixed in lean and recorded in `docs/fixes.md` section 1.
- **The card.** Most files of `src/device/` are generated from the `src/` file of the same name: edit `src/`, then
  run `./lean generate` (`./lean generate --check` tells whether `src/device/` is current). The code map lists the few
  card files written by hand. A card build needs nvfortran and a card; say so in your report when you could not
  build or run it.
- **Generated files** carry a GENERATED header that names their generator. Change the generator or its input in
  `config/`, never the generated file.
- **A new source file** is added to `config/units.json`, which holds the compile order.
- **Code from MPAS or its physics packages** keeps the terms of its source. `NOTICE.md` lists which part comes from
  where; when you translate or copy more of it, add it there. Noah-MP's terms forbid selling it or a work that contains
  it, and RRTMG's notice stays in every file that holds RRTMG's code.

## Style of the code

- Flat, explicit loops and short routines. No new abstraction layer, registry or option framework: lean_mpas is small
  because it does one thing.
- The model state lives once in memory. Physics reads and writes it directly; do not add copies of fields.
- No allocation inside the time loop.
- A comment says what the code does or why, in a sentence. No history of the change, no names, no dates.
- Messages that a user sees follow MPAS's wording where MPAS has one.

## What you cannot check here

- `sh tests/run_all.sh cases` compares with MPAS's own outputs where they exist. They are not in the release, so
  those parts print SKIP. To compare with MPAS yourself, build MPAS 8.4.2 and use the scripts in `linux/`.
- `docs/running.md` lists what each test tier needs and what it skips.

When something in this file or in the docs does not match the code, the code is right: fix the text in the same
change.
