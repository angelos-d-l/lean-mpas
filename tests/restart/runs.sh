#!/bin/sh
# Restart runs of the real model (tests/restart/model_runs.py), 1 thread; needs the release case inputs in data/cases.
#   sh tests/restart/runs.sh [FC] [JOBS]
set -eu
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
export LEAN_PY=${LEAN_PY:-${PYTHON:-python3}}
exec "$LEAN_PY" tests/restart/model_runs.py "${1:-gfortran}" "${2:-2}"
