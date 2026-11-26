#!/bin/sh
# Real-model runs of the surface update (tests/surface/model_runs.py):
#   sh tests/surface/runs.sh [FC] [JOBS] [case ...]     LEAN_PY overrides the Python with netCDF4 and numpy
# Scratch is removed after a pass.
set -eu
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
export LEAN_PY=${LEAN_PY:-${PYTHON:-python3}}
fc=${1:-gfortran}
jobs=${2:-1}
[ "$#" -ge 2 ] && shift 2 || shift "$#"
exec "$LEAN_PY" tests/surface/model_runs.py --fc "$fc" --jobs "$jobs" "$@"
