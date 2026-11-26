#!/bin/sh
# Writer runs of the real model (tests/writer/model_runs.py), 1 thread; needs the release case inputs in data/cases.
#   sh tests/writer/runs.sh [FC] [JOBS]
set -eu
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
mkdir -p build/writer/cache
export MPLCONFIGDIR="$PWD/build/writer/cache" XDG_CACHE_HOME="$PWD/build/writer/cache"
export LEAN_PY=${LEAN_PY:-${PYTHON:-python3}}
exec "$LEAN_PY" tests/writer/model_runs.py "${1:-gfortran}" "${2:-2}"
