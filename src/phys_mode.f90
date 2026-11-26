! Which physics stages run on the card. Chosen at run time by environment variables (OpenACC build only; a plain build always
! runs everything on the host):
!   LEAN_CARD=thompson,inputs,surface,land,pbl,gwdo,gf,finish|all   these stages run as kernels on the card; unset means all,
!                                   LEAN_CARD=none keeps the physics on the host
!   LEAN_CHECK=thompson,gf,...      the stage runs on the card AND on the host from the same inputs, the two results are compared bit for bit
!                                   and the run continues with the host result (so the run itself is the host run)
! Stages: thompson (microphysics after the dynamics), inputs (dynamics state -> physics columns), surface (MYNN surface layer), land (Noah-MP),
! pbl (MYNN PBL), gwdo, gf (Grell-Freitas and the accumulation of the convection and radiation tendencies), finish (tendencies to the
! dynamics arrays and the land diagnostics for the radiation).
! radiation (RRTMG at its intervals) is on the card with `radiation` or `all` in LEAN_CARD; `radiation` in LEAN_CHECK runs it on both and
! prints the largest differences (the card has its own exp and log, so the comparison is a size, not bits); the run goes on with the host.
module phys_mode
 use dyn_config, only: host_inputs
 implicit none
 integer, parameter :: st_thompson = 1, st_inputs = 2, st_surface = 3, st_land = 4, st_pbl = 5, st_gwdo = 6, st_gf = 7, st_finish = 8
 integer, parameter :: nstages = 8
 character(8), parameter :: stage_name(nstages) = [character(8) :: 'thompson', 'inputs', 'surface', 'land', 'pbl', 'gwdo', 'gf', 'finish']
 logical, save :: card(nstages) = .false., check(nstages) = .false.
 logical, save :: card_thompson = .false., check_thompson = .false.
 logical, save :: card_radiation = .false., check_radiation = .false.

 integer, save :: state_check_every = 10   ! LEAN_ACT=n: check the state for non-finite values every n steps
 logical, save :: any_card = .false.        ! some stage of the pre-dynamics chain is on the card: the chain runs staged
contains
 subroutine phys_mode_init()
 character(256) :: sc, sk
 integer :: n, i
 logical :: card_set
 sc = ''; sk = ''
 call get_environment_variable('LEAN_ACT', sc, status=n)
 if (n == 0) read(sc, *, iostat=n) state_check_every
 if (state_check_every < 1) state_check_every = 10
 sc = ''
 call get_environment_variable('LEAN_CARD', sc, status=n)
 card_set = n /= 1                       ! status 1: not in the environment
 if (n /= 0) sc = ''
 call get_environment_variable('LEAN_CHECK', sk, status=n)
 if (n /= 0) sk = ''
#ifdef _OPENACC
 call parse_stages(trim(sc), card_set, trim(sk), card, check, card_radiation, check_radiation)
 card_thompson = card(st_thompson)
 check_thompson = check(st_thompson)
 any_card = any(card(2:nstages)) .or. card_radiation
 host_inputs = .not. card(st_inputs) .or. check(st_inputs)
 print *, 'physics stages on the card:'
 do i = 1, nstages
  print '(3x,a8,a,l1,a,l1)', stage_name(i), ' card=', card(i), ' check=', check(i)
 end do
 print '(3x,a8,a,l1,a,l1)', 'radiation', ' card=', card_radiation, ' check=', check_radiation
#endif
 end subroutine

 ! LEAN_CARD and LEAN_CHECK as a card build reads them (phys_mode_init, and the memory estimate of lean_memory.f90). An unset
 ! LEAN_CARD (card_set false) means all; LEAN_CARD=none names no stage, so the physics stays on the host. A checked
 ! stage also runs on the card.
 subroutine parse_stages(card_text, card_set, check_text, on, checked, radiation, radiation_checked)
 character(*), intent(in) :: card_text, check_text
 logical, intent(in) :: card_set
 logical, intent(out) :: on(nstages), checked(nstages), radiation, radiation_checked
 logical :: all_on
 integer :: i
 all_on = .not. card_set .or. index(card_text, 'all') > 0
 do i = 1, nstages
  checked(i) = index(check_text, trim(stage_name(i))) > 0
  on(i) = all_on .or. index(card_text, trim(stage_name(i))) > 0 .or. checked(i)
 end do
 radiation_checked = index(check_text, 'radiation') > 0
 radiation = all_on .or. index(card_text, 'radiation') > 0 .or. radiation_checked
 end subroutine
end module
