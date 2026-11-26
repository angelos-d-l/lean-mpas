! Land ice (compiled in every build). MPAS 8.4.2
! runs Noah-MP's glacier physics on a land cell of the ice category without sea ice (NoahmpDriverMainMod.F90:161-209:
! xice below the threshold, xland 1, ivgtyp 15 -> NoahmpMainGlacier). Lean's own Noah-MP (noahmp_core.f90) is the soil
! physics only, so a glacier cell keeps the NoahmpIO column MPAS keeps (one column per cell, made at the start by the
! cold start of noahmp_cold_start.f90) and each step runs the original driver on it, NoahmpDriverMain, with the inputs
! MPAS's driver_lsm_noahmp gives it (mpas_atmphys_driver_lsm_noahmp.F lsm_noahmp_fromMPAS 32-558, lsm_noahmp_sounding_fromMPAS
! 561-645, lsm_noahmp_toMPAS 648-1046). The column carries every Noah-MP field from step to step, as MPAS's pools do; only the fields
! another scheme changes between two land steps (skin temperature, fluxes, surface humidity, roughness, snow depth) and
! the forcing are set each step. One departure, shared with lean's soil cells: the forcing temperature and humidity
! use the physics qv, which is limited to >= 0.
! The original driver allocates its column type at every call, as in MPAS; there are few glacier cells.
! What a step exchanges with the model column goes through one record per glacier cell (land_ice_exchange.f90):
! glacier_pack fills it from the column, glacier_step runs the driver on it, glacier_unpack writes the results back. The
! CPU build does the three in the same cell (pre_dynamics.f90). The card build packs and unpacks on the card and runs
! glacier_step on the host, between two copies of the records (phys_dev.f90): the driver's derived type and its
! allocations cannot run on the card. This module is host code in every build; land_ice_exchange has a card version.
module land_ice
 use config, only: nz, ncells
 use NoahmpIOVarType, only: NoahmpIO_type
 use NoahmpDriverMainMod, only: NoahmpDriverMain
 use mpas_atmphys_constants, only: P0, rcp
 use sea_ice, only: xice_threshold
 use land_ice_exchange, only: glacier_of, n_glacier, glacier_exchange, exchange, allocate_exchange, glacier_pack, &
                              glacier_unpack
 implicit none
 private
 public :: glacier, glacier_of, n_glacier, glacier_category, start_glacier, finish_glacier_start, exchange, glacier_exchange, &
           glacier_pack, glacier_step, glacier_unpack, keep_glacier, put_glacier
 type(NoahmpIO_type), allocatable, save :: glacier(:)
 integer, parameter :: glacier_category = 15    ! isice_table of the MODIS categories (NoahmpTable.TBL)
contains
 ! Cold start of glacier cell c: io is the cell's NoahmpIO column after the cold start (noahmp_cold_start.f90, which
 ! ran NoahmpInitMain on it). The list grows at the start only.
 subroutine start_glacier(c, io)
 integer, intent(in) :: c
 type(NoahmpIO_type), intent(in) :: io
 type(NoahmpIO_type), allocatable :: grown(:)
 integer :: g
 if (.not. allocated(glacier)) allocate(glacier(64))
 if (n_glacier == size(glacier)) then
  allocate(grown(2*size(glacier)))
  do g = 1, n_glacier
   grown(g) = glacier(g)
  end do
  call move_alloc(grown, glacier)
 end if
 n_glacier = n_glacier + 1
 glacier(n_glacier) = io
 ! mpas_atmphys_manager.F:740-744 with config_frac_seaice = true
 glacier(n_glacier)%xice_threshold = xice_threshold
 glacier_of(c) = n_glacier
 end subroutine

 ! After the cold start of every cell: one exchange record per glacier cell, with the radiation surface inputs of the start.
 subroutine finish_glacier_start()
 integer :: c, g
 call allocate_exchange()
 do c = 1, ncells
  g = glacier_of(c)
  if (g > 0) then
   exchange(g)%c = c
   exchange(g)%emiss = glacier(g)%emiss(1); exchange(g)%snow = glacier(g)%snow(1); exchange(g)%albedo = glacier(g)%albedo(1)
  end if
 end do
 end subroutine

 ! One land step of glacier column g, in place of update_land, on its exchange record x (glacier_pack before it);
 ! itimestep: the physics step (1 at a cold start). Host code.
 subroutine glacier_step(g, itimestep)
 integer, intent(in) :: g, itimestep
 real :: z0, z1, z2, w1, w2, totm, totp, mult, fzm, fzp
 integer :: k
 associate(io => glacier(g), x => exchange(g), z => exchange(g)%z, gsw => exchange(g)%gsw, &
           glw => exchange(g)%glw, coszen => exchange(g)%coszen, raincv => exchange(g)%raincv)
 ! lsm_noahmp_fromMPAS (mpas_atmphys_driver_lsm_noahmp.F:121-262); yr, month, day, julian and the vegetation fractions
 ! are set by the model loop (lean_model.f90)
 io%restart_flag = .false.
 io%soiltstep = 0
 io%itimestep = itimestep
 io%coszen(1) = coszen
 io%swdown(1) = gsw / (1.-io%albedo(1))
 io%swddir(1) = 0.   ! read only by the urban solar panels
 io%swddif(1) = 0.
 io%glw(1) = glw
 io%rainbl(1) = 0.; io%snowbl(1) = 0.; io%rainshv(1) = 0.; io%hailncv(1) = 0.; io%mp_hail(1) = 0.; io%mp_shcv(1) = 0.
 io%seaice(1) = 0.
 io%sr(1) = x%sr
 io%rainncv(1) = x%rainncv
 io%snowncv(1) = x%snowncv
 io%graupelncv(1) = x%graupelncv
 io%rainbl(1) = io%rainbl(1) + io%rainncv(1)
 io%snowbl(1) = io%snowbl(1) + io%snowncv(1)
 io%mp_rainnc(1) = x%rainncv
 io%mp_snow(1) = x%snowncv
 io%mp_graup(1) = x%graupelncv
 io%raincv(1) = raincv
 io%rainbl(1) = io%rainbl(1) + io%raincv(1)
 io%raincv(1) = raincv
 io%mp_rainc(1) = raincv
 if (io%xice(1) >= xice_threshold) io%seaice(1) = 1.
 ! lsm_noahmp_sounding_fromMPAS (:604-641)
 do k = 1, nz
  io%dz8w(1,k) = z(k+1) - z(k)
  io%qv_curr(1,k) = x%qv(k)
  io%t_phy(1,k) = x%theta(k)*x%exner(k)
  io%u_phy(1,k) = x%u(k)
  io%v_phy(1,k) = x%v(k)
 end do
 k = 1
 z0 = z(k)
 z1 = 0.5*(z(k)+z(k+1))
 z2 = 0.5*(z(k+1)+z(k+2))
 w1 = (z0-z2)/(z1-z2)
 w2 = 1.-w1
 totm = x%p(k)
 totp = x%p(k+1)
 io%p8w(1,k) = w1*totm + w2*totp
 do k = 2, nz
  totm = x%p(k-1)
  totp = x%p(k)
  mult = 1./(z(k+1)-z(k-1))
  fzm = mult*(z(k)-z(k-1))
  fzp = mult*(z(k+1)-z(k))
  io%p8w(1,k) = fzm*totp + fzp*totm
 end do
 ! the pool fields other schemes write between two land steps (the surface layer: fluxes, surface humidity)
 io%tsk(1) = x%tsk; io%hfx(1) = x%hfx; io%qfx(1) = x%qfx; io%lh(1) = x%lh; io%qsfc(1) = x%qsfc
 io%znt(1) = x%znt; io%snowh(1) = x%snowh
 ! one thread at a time: MPAS runs its Noah-MP driver outside OpenMP (mpas_atmphys_driver.F:294-300, no PARALLEL DO)
 !$omp critical (noahmp_glacier)
 call NoahmpDriverMain(io)
 !$omp end critical (noahmp_glacier)
 ! lsm_noahmp_toMPAS (:731-753)
 x%tsk = io%tsk(1); x%hfx = io%hfx(1); x%qfx = io%qfx(1); x%lh = io%lh(1); x%qsfc = io%qsfc(1); x%znt = io%znt(1)
 x%snowh = io%snowh(1)
 ! the 2 m diagnostics of a glacier cell, Noah-MP's (mpas_atmphys_sfc_diagnostics.F:111-115)
 x%q2 = io%q2mxy(1)
 x%t2 = io%t2mxy(1)
 x%th2 = x%t2*(P0/x%psfc)**rcp
 ! the radiation surface inputs (sfc_emiss, snow, sfc_albedo of the pools)
 x%acsnow = io%acsnow(1)
 x%emiss = io%emiss(1); x%snow = io%snow(1); x%albedo = io%albedo(1)
 end associate
 end subroutine

 ! The card build's check mode runs a land stage twice from the same inputs (phys_dev.f90): the glacier columns are kept
 ! before the first run and put back before the second. Allocates; check mode only.
 subroutine keep_glacier(copy)
 type(NoahmpIO_type), allocatable, intent(inout) :: copy(:)
 if(n_glacier==0)return
 copy = glacier(1:n_glacier)
 end subroutine

 subroutine put_glacier(copy)
 type(NoahmpIO_type), intent(in) :: copy(:)
 if(n_glacier==0)return
 glacier(1:n_glacier) = copy
 end subroutine
end module
