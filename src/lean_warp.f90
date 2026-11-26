! The thread's index in its card launch (thread_slot), for the column schemes whose work arrays live in a pool
! (src/device/micro_pool.f90). CUDA Fortran on the card; on the host it returns 1.
module lean_warp
 implicit none
 private
 public :: thread_slot
contains
 ! thread_slot: the thread's index in its launch, 1-based, for work arrays that live in a pool sized for the threads of
 ! a launch instead of in the per-thread stack frame (CUDA reserves the largest frame for every thread the card can hold). A kernel
 ! that uses it caps its grid (num_gangs x vector_length) at the pool's size; each thread then takes several columns in turn.
 ! On the host: 1 (the card build's host path runs one thread at a time for the pooled schemes).
 attributes(device) integer function thread_slot_dev()
  use cudadevice
  thread_slot_dev = (blockIdx%x - 1) * blockDim%x + threadIdx%x
 end function
 integer function thread_slot()
 !$acc routine seq bind(thread_slot_dev)
  thread_slot = 1
 end function
end module
