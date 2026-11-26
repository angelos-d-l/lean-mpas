# The lean_mpas build: CMake + Ninja, with every flag from the flags table (config/flags.json, read by
# tools/lean/config.py). CMake only orders the compiles by their Fortran modules; it adds no flag of its own
# beyond module directories, include paths and outputs, so that a build's flags are exactly the table's.
#
# Normally run through ./lean build (docs/running.md), which writes the whole build configuration to LEAN_CONFIG_FILE
# and calls the two commands below; called directly, the build has the given target, compiler and profile and no
# physics switch (the mpas flavor).
#
#   cmake -G Ninja -S . -B build/<dir> -DCMAKE_Fortran_COMPILER=<fc> -DLEAN_TARGET=cpu|gpu -DLEAN_FC=gfortran|nvfortran|ifx
#         -DLEAN_PROFILE=strict|debug|fast|envelope [-DLEAN_OMP=ON] [-DLEAN_VARIANT=V]
#   ninja -C build/<dir> lean_mpas
# CMake 3.28 or newer (lean.lock.json tools_min; ./lean doctor checks it). CMake before 3.27.5 gives an OBJECT library
# no Fortran module of the libraries it links (tested: 3.26.4, 3.27.0, 3.27.4, the last is Ubuntu 23.10's), so Ninja could compile a
# file before the module it uses existed (radiation_scratch before RRTMG's parrrsw.mod). 3.27.5, 3.27.9, 3.28.1,
# 3.29.0 and 3.31.2 pass them on; the pin is the next minor version.
cmake_minimum_required(VERSION 3.28)

# macOS: no -isysroot and no -mmacosx-version-min, since the flags table has neither (enforced here rather than left
# to the caller). project() would fill CMAKE_OSX_SYSROOT with the SDK and CMAKE_OSX_DEPLOYMENT_TARGET from
# $MACOSX_DEPLOYMENT_TARGET, and both would reach every compile and link line. Both are set empty before project(), and
# a non-empty value given with -D is refused, not silently replaced. Passing them empty (-DCMAKE_OSX_SYSROOT=) is
# fine. Elsewhere they mean nothing.
foreach(lean_name CMAKE_OSX_SYSROOT CMAKE_OSX_DEPLOYMENT_TARGET)
  if(DEFINED CACHE{${lean_name}} AND NOT "$CACHE{${lean_name}}" STREQUAL "")
    message(FATAL_ERROR "lean_mpas: ${lean_name}=$CACHE{${lean_name}} would add a flag that config/flags.json does "
                        "not have; leave it empty (strict builds carry only the flags of the table)")
  endif()
endforeach()
set(CMAKE_OSX_SYSROOT "" CACHE PATH "Left empty: no -isysroot (strict flags only)" FORCE)
set(CMAKE_OSX_DEPLOYMENT_TARGET "" CACHE STRING "Left empty: no -mmacosx-version-min (strict flags only)" FORCE)

project(lean_mpas LANGUAGES Fortran)

# Strict builds carry only the flags we set: no build type, no default or environment (FFLAGS, LDFLAGS) flags, no
# IPO/LTO, and none of the platform's default link flags (Darwin adds -Wl,-search_paths_first and
# -Wl,-headerpad_max_install_names; the flags table has neither).
set(CMAKE_BUILD_TYPE "" CACHE STRING "Left empty: every flag comes from config/flags.json" FORCE)
set(CMAKE_Fortran_FLAGS "" CACHE STRING "Left empty: every flag comes from config/flags.json" FORCE)
set(CMAKE_EXE_LINKER_FLAGS "" CACHE STRING "Left empty: every flag comes from config/flags.json" FORCE)
set(CMAKE_Fortran_LINK_FLAGS "")
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION OFF)
# No -Wl,--dependency-file: CMake 3.27 and later ask a linker that can (GNU ld) for its own dependency file, a flag
# the flags table does not have. Ninja still relinks when an object changes.
set(CMAKE_LINK_DEPENDS_USE_LINKER FALSE)

# The compiler preprocesses each file itself (-cpp / -Mpreprocess are in the flags table), and CMake adds no -nocpp,
# which it would otherwise add to gfortran compiles with Fortran_PREPROCESS OFF. Only the files of config/units.json
# cmake_preprocess get CMake's own -E step, so that its module scan sees the use statements behind #ifdef.
set(CMAKE_Fortran_PREPROCESS OFF)
set(CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF "")

set(LEAN_TARGET "" CACHE STRING "cpu or gpu")
set(LEAN_FC "" CACHE STRING "gfortran, nvfortran or ifx")
set(LEAN_PROFILE "" CACHE STRING "strict, debug, fast or envelope")
option(LEAN_OMP "OpenMP host threads" OFF)
set(LEAN_VARIANT "" CACHE STRING "the variant of a fast or envelope build")
set(LEAN_CONFIG_FILE "" CACHE FILEPATH "the whole BuildConfig as JSON (./lean writes it before configure)")
foreach(name LEAN_TARGET LEAN_FC LEAN_PROFILE)
  if(NOT ${name})
    message(FATAL_ERROR "lean_mpas: give -D${name}=...")
  endif()
endforeach()

# The flags table is per compiler: the compiler CMake found must be the one named.
if(LEAN_FC STREQUAL "gfortran")
  set(lean_compiler_ids GNU)
elseif(LEAN_FC STREQUAL "ifx")
  set(lean_compiler_ids IntelLLVM)
else()
  set(lean_compiler_ids NVHPC PGI)
endif()
if(NOT CMAKE_Fortran_COMPILER_ID IN_LIST lean_compiler_ids)
  message(FATAL_ERROR "lean_mpas: LEAN_FC=${LEAN_FC}, but ${CMAKE_Fortran_COMPILER} is ${CMAKE_Fortran_COMPILER_ID}")
endif()

find_package(Python3 3.9 REQUIRED COMPONENTS Interpreter)

if(LEAN_CONFIG_FILE)
  set(lean_args cmake --config-file ${LEAN_CONFIG_FILE} --out ${CMAKE_BINARY_DIR}/lean_sources.cmake)
  set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS "${LEAN_CONFIG_FILE}")
else()
  set(lean_args cmake --target ${LEAN_TARGET} --fc ${LEAN_FC} --profile ${LEAN_PROFILE}
                --out ${CMAKE_BINARY_DIR}/lean_sources.cmake)
  if(LEAN_OMP)
    list(APPEND lean_args --omp)
  endif()
  if(LEAN_VARIANT)
    list(APPEND lean_args --variant ${LEAN_VARIANT})
  endif()
endif()
execute_process(COMMAND ${Python3_EXECUTABLE} -m tools.lean.config ${lean_args}
                WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                RESULT_VARIABLE lean_status)
if(NOT lean_status EQUAL 0)
  message(FATAL_ERROR "lean_mpas: python3 -m tools.lean.config ${lean_args} failed (${lean_status})")
endif()
include(${CMAKE_BINARY_DIR}/lean_sources.cmake)
