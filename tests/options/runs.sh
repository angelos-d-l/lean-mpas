#!/bin/sh
# Real-model runs of the namelist options and the scheme 'off' sets (tests/options/model_runs.py):
#   sh tests/options/runs.sh [FC] [JOBS] [case ...]     LEAN_PY overrides the Python with netCDF4 and numpy
# Scratch is removed after a pass.
set -eu
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
export LEAN_PY=${LEAN_PY:-${PYTHON:-python3}}
exec "$LEAN_PY" tests/options/model_runs.py "$@"
