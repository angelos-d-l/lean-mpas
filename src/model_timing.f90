! Seconds of the model loop. elapsed is WALL time (with several threads the CPU time of the process is the sum
! over the threads and says nothing about the speed), elapsed_cpu the process CPU time of the same intervals. The timers
! may be called by all threads of a team (the dynamics runs inside one parallel region): they synchronise the team, the
! master thread keeps the clock. Timers 1-6 (physics, dynamics, scalars, thompson, radiation, boundary) do not nest. 7 is
! the host-card transfers of the OpenACC build. Most of them run between the other timers, but the physics state copies of a
! card stage (phys_dev.f90 bundle_to_device/bundle_to_host) run inside timer 1 or 5. That timer's wall time then includes
! them, and its CPU time counts only from the last nested start (the timers share one CPU mark). 8 is the start of
! each step (vegetation and surface updates, shift of the time levels) and 9 the per-step outputs (optional outputs,
! state check). The model startup and the final output are not timed here.
module model_timing
 implicit none
 real(kind=8), save :: elapsed(9)=0.d0, elapsed_cpu(9)=0.d0, cpu_mark=0.d0
 ! Finer wall-clock timers inside the dynamics (they may be nested in the six above; no CPU time).
 integer, parameter :: ndetail=12
 real(kind=8), save :: detail(ndetail)=0.d0, detail_t0(ndetail)=0.d0
 character(len=22), parameter :: detail_name(ndetail)=[character(len=22) :: 'copies rk_setup/finish','vert_imp_coefs','compute_dyn_tend','pert+boundary tend', &
  'acoustic steps','recover','solve_diagnostics','moist_coefficients','transport stage 1,2','transport stage 3 mono','after_transport','shift time levels']
 integer, parameter :: d_copies=1,d_coefs=2,d_tend=3,d_pert=4,d_acoustic=5,d_recover=6,d_diag=7,d_moist=8,d_transport=9,d_mono=10,d_after=11,d_shift=12
 ! The dynamics kernels run on the card's queue 1 without host waits (dyn_*.f90, async(1)). The stage timers drain the queue when they
 ! stop (drain_queue, set by the card build: this unit is compiled without OpenACC), so a stage's seconds are the card's; the finer
 ! detail timers do not (they stop about 70 times per step, and each drain costs a launch gap): they measure the host's enqueue
 ! time.
 abstract interface
  subroutine drain_interface()
  end subroutine
 end interface
 procedure(drain_interface), pointer, save :: drain_queue => null()
 integer, parameter :: physics_timer=1,dynamics_timer=2,scalars_timer=3,thompson_timer=4,radiation_timer=5,boundary_timer=6,transfer_timer=7
 integer, parameter :: start_timer=8, output_timer=9
contains
 subroutine detail_start(i)
 integer, intent(in) :: i
 integer(kind=8) :: tick, rate
 !$omp barrier
 !$omp master
 call system_clock(tick, rate)
 detail_t0(i) = real(tick,8)/real(rate,8)
 !$omp end master
 end subroutine
 subroutine detail_stop(i)
 integer, intent(in) :: i
 integer(kind=8) :: tick, rate
 !$omp barrier
 !$omp master
 call system_clock(tick, rate)
 detail(i) = detail(i) + real(tick,8)/real(rate,8) - detail_t0(i)
 !$omp end master
 end subroutine
 subroutine timer_start(t)
 real(kind=8), intent(out) :: t
 integer(kind=8) :: tick, rate
 t = 0.d0
 !$omp barrier
 !$omp master
 call system_clock(tick, rate)
 t = real(tick,8)/real(rate,8)
 call cpu_time(cpu_mark)
 !$omp end master
 end subroutine
 subroutine timer_stop(index,t)
 integer, intent(in) :: index
 real(kind=8), intent(in) :: t
 integer(kind=8) :: tick, rate
 real(kind=8) :: now, cpu_now
 !$omp barrier
 !$omp master
 if (associated(drain_queue)) call drain_queue()
 call system_clock(tick, rate)
 call cpu_time(cpu_now)
 now = real(tick,8)/real(rate,8)
 elapsed(index)=elapsed(index)+now-t
 elapsed_cpu(index)=elapsed_cpu(index)+cpu_now-cpu_mark
 !$omp end master
 end subroutine
end module
