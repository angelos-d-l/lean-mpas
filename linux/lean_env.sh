# linux/lean_env.sh: the environment of the scripts in linux/cases. Source it, never run it.
#   LEAN_PY        a Python 3 (default $PYTHON, else python3; the standard library is enough): the scripts run
#                  tools/cases/run_spec.py and read build.json with it.
#   LEAN_NVHPC_BIN optional: the bin folder of the NVIDIA HPC SDK compilers (the one that holds nvfortran). When set,
#                  its libraries and the SDK's MPI go on PATH and LD_LIBRARY_PATH, which a card build of lean_mpas and
#                  an nvfortran build of MPAS need at run time.
# Nothing else: each script sets its own OMP_* and the MPAS build's own env.sh adds what that build needs.
export LEAN_PY=${LEAN_PY:-${PYTHON:-python3}}
if [ -n "${LEAN_NVHPC_BIN:-}" ]; then
  export PATH=$LEAN_NVHPC_BIN/../../comm_libs/mpi/bin:$LEAN_NVHPC_BIN:$PATH
  export LD_LIBRARY_PATH=$LEAN_NVHPC_BIN/../../comm_libs/mpi/lib:$LEAN_NVHPC_BIN/../lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
fi
