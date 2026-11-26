! Unit checks of the surface update (SST and sea ice) on the real modules:  units CASE
! One case per process. Prints "ok   CASE ..." and exits 0, or a FAIL line and a nonzero code.
!   update_oracle  MPAS's physics_update_sst (tests/surface/oracle.F90, its lines cut out by run.sh) and lean's update_sst on
!                  2337 synthetic cells, 30 of them special: every field of the map table equal bit for bit to its lean target
!   map_complete   every field the oracle assigns has a "! map" row in src/surface_bdy.f90 and the other way round
!   file_ice       surface_file_has_ice on a file with ice on water at 03:00 and on one without
!   read_record    read_surface_record: the record at or before the time, and the stop when there is none
!   land_ice       update_sst with sea ice on a land cell: a stop, and nothing is changed
!   water_ice      update_sst with sea ice on a water cell and no sea-ice cells set up at the start: a stop, nothing is changed
program surface_units
 use config, only: ncells, nc1, set_mesh_sizes
 use model_state, only: state_type, allocate_state
 use noahmp_state, only: noahmp_state_type, allocate_land
 use sea_ice, only: sea_ice_cells => ice, allocate_ice, any_sea_ice
 use model_data, only: md_emiss => emiss, md_ice => ice, md_snow => snow, md_albedo => albedo
 use lean_fields, only: lf_sst => sst, lf_xice => xice, lf_vegetation => vegetation
 use surface_bdy, only: xicem, ivgtyp_now, isltyp_now, surface_bdy_start, read_surface_record, update_sst, surface_file_has_ice, update_counts
 use lean_messages, only: message_list, LOG_CRIT, LOG_ERR
 use mpas_kind_types, only: RKIND
 use mpas_update_oracle, only: oracle_update_sst
 implicit none
 character(len=64) :: name
 character(len=*), parameter :: dir = 'build/surface/units/'
 integer, parameter :: nsoil = 4, nspecial = 30
 type(state_type), save :: state
 type(noahmp_state_type), save :: lands
 ! the oracle's pools and the record, at program level so that make_ice sees them
 real(kind=RKIND), allocatable, save :: o_sst(:), o_xice(:), o_xicem(:), o_xland(:), o_tsk(:), o_tmn(:), o_vegfra(:), &
  o_albbck(:), o_albedo(:), o_emiss(:), o_emibck(:), o_snow(:), o_snowc(:), o_snowh(:), o_seaice(:), &
  o_tslb(:,:), o_sh2o(:,:), o_smois(:,:)
 integer, allocatable, save :: o_ivgtyp(:), o_isltyp(:)
 real, allocatable, save :: sst_in(:), xice_in(:)

 call get_command_argument(1, name)
 select case (trim(name))
 case ('update_oracle')
  call update_oracle()
 case ('map_complete')
  call map_complete()
 case ('file_ice')
  call file_ice()
 case ('read_record')
  call read_record()
 case ('land_ice')
  call land_ice()
 case ('water_ice')
  call water_ice()
 case default
  print '(a)', 'FAIL: unknown case '//trim(name)
  stop 2
 end select

contains

 subroutine setup()
 call set_mesh_sizes(2337, 7183, 4847, 45)
 allocate(md_emiss(ncells), md_ice(ncells), md_snow(ncells), md_albedo(ncells))
 allocate(lf_sst(ncells), lf_xice(ncells), lf_vegetation(ncells))
 call allocate_state(state)
 call allocate_land(lands)
 call allocate_ice()
 call surface_bdy_start(md_ice)
 end subroutine

 ! 30 special cells that cover MPAS's branches; the others are plain water. Both sides start from the same arrays.
 subroutine update_oracle()
 integer :: c, k, nb_to_land, nb_to_ocean, nb_removed
 type(message_list) :: msgs
 logical :: ice_after
 call setup()
 allocate(o_sst(ncells), o_xice(ncells), o_xicem(ncells), o_xland(ncells), o_tsk(ncells), o_tmn(ncells), o_vegfra(ncells), &
  o_albbck(ncells), o_albedo(ncells), o_emiss(ncells), o_emibck(ncells), o_snow(ncells), o_snowc(ncells), o_snowh(ncells), &
  o_seaice(ncells), o_tslb(nsoil,ncells), o_sh2o(nsoil,ncells), o_smois(nsoil,ncells), o_ivgtyp(ncells), o_isltyp(ncells))
 allocate(sst_in(nc1), xice_in(nc1))
 any_sea_ice = .true.
 ! plain water everywhere
 do c = 1, ncells
  o_xland(c) = 2.; o_xicem(c) = 0.; o_xice(c) = 0.; xice_in(c) = 0.
  sst_in(c) = 280. + 0.01*c; o_sst(c) = sst_in(c)
  o_tsk(c) = sst_in(c) - 0.5; o_tmn(c) = 285.; o_vegfra(c) = 40.; o_albbck(c) = 0.2
  o_albedo(c) = 0.3; o_emiss(c) = 0.9; o_emibck(c) = 0.95
  o_snow(c) = 0.; o_snowc(c) = 0.; o_snowh(c) = 0.; o_seaice(c) = 0.
  o_ivgtyp(c) = 17; o_isltyp(c) = 14
  do k = 1, nsoil
   o_tslb(k,c) = 275. + k; o_sh2o(k,c) = 0.2 + 0.01*k; o_smois(k,c) = 0.3 + 0.01*k
  end do
 end do
 ! the 30 special cells: kind, xice at the last update (xicem), and xice read now
 do c = 1, nspecial
  select case (c)
  case (1:8)                  ! water, nothing happens
   continue
  case (9);  xice_in(c) = 0.01      ! below the threshold: removed
  case (10); xice_in(c) = 0.02      ! exactly the threshold: water to sea ice
  case (11); xice_in(c) = 0.6
  case (12); xice_in(c) = 1.0
  case (13); xice_in(c) = 0.021
  case (14); call make_ice(c, 0.5, 0.0)       ! sea ice to water, by 0
  case (15); call make_ice(c, 0.5, 0.015)     ! sea ice to water, by a fraction below the threshold
  case (16); call make_ice(c, 0.8, 0.4)       ! rescale down
  case (17); call make_ice(c, 0.5, 0.5)       ! no change
  case (18); call make_ice(c, 1.0, 0.5)       ! a full cell
  case (19); call make_ice(c, 0.3, 0.3)
  case (20); call make_ice(c, 0.02, 0.019)    ! the threshold itself, to water
  case (21); call make_ice(c, 0.3, 0.9)       ! rescale up
  case (22:24)                ! land: xland 1, no ice, nothing happens
   o_xland(c) = 1.; o_ivgtyp(c) = 10; o_isltyp(c) = 6; o_snow(c) = 5.; o_snowc(c) = 1.; o_snowh(c) = 0.05
  case (25); call make_ice(c, 0.5, 0.001)     ! sea ice to water, by a tiny value
  case (26); xice_in(c) = 0.02                ! a second new sea-ice cell at exactly the threshold (as cell 10)
  case default
   continue
  end select
 end do
 do c = 1, ncells
  lf_sst(c) = 1.; lf_xice(c) = md_ice(c); lf_vegetation(c) = real(o_vegfra(c))
  state%xland(c) = real(o_xland(c)); state%tsk(c) = real(o_tsk(c)); state%snowh(c) = real(o_snowh(c)); state%znt(c) = 0.0123
  md_albedo(c) = real(o_albedo(c)); md_emiss(c) = real(o_emiss(c)); md_snow(c) = real(o_snow(c)); md_ice(c) = real(o_xicem(c))
  xicem(c) = real(o_xicem(c)); ivgtyp_now(c) = o_ivgtyp(c); isltyp_now(c) = o_isltyp(c)
  lf_xice(c) = real(o_xicem(c))
  sea_ice_cells%xice(c) = real(o_xicem(c)); sea_ice_cells%sst(c) = 0.; sea_ice_cells%tslb(:,c) = real(o_tslb(:,c))
  sea_ice_cells%snow(c) = real(o_snow(c)); sea_ice_cells%snowc(c) = real(o_snowc(c)); sea_ice_cells%snoalb(c) = 0.3
  sea_ice_cells%albedo(c) = real(o_albedo(c)); sea_ice_cells%emiss(c) = real(o_emiss(c)); sea_ice_cells%z0(c) = 0.01
  lands%water_state_soilmoisture(1:nsoil,c) = real(o_smois(:,c))
  lands%water_state_soilliqwater(1:nsoil,c) = real(o_sh2o(:,c))
  lands%energy_state_temperaturesoilsnow(1:nsoil,c) = real(o_tslb(:,c))
 end do
 ! MPAS's routine, then lean's: the stream read has put the new xice into MPAS's pool, xicem keeps the one of the last update
 o_xice = xice_in(1:ncells)
 call oracle_update_sst(ncells, nsoil, .true., real(0.02, RKIND), 15, 17, o_ivgtyp, o_isltyp, o_vegfra, o_albbck, &
  o_sst, o_tmn, o_tsk, o_tslb, o_sh2o, o_smois, o_snow, o_snowc, o_snowh, o_seaice, o_xice, o_xland, &
  o_albedo, o_emiss, o_emibck, o_xicem, nb_to_land, nb_to_ocean, nb_removed)
 call update_sst(sst_in, xice_in, state, lands, msgs)
 call require(msgs%n == 0, 'update_sst wrote a message')
 ! the branch counts
 call require(update_counts(1) == nb_to_land .and. update_counts(2) == nb_to_ocean .and. update_counts(3) == nb_removed, &
  'the branch counts: lean '//itext(update_counts(1))//' '//itext(update_counts(2))//' '//itext(update_counts(3))// &
  ', MPAS '//itext(nb_to_land)//' '//itext(nb_to_ocean)//' '//itext(nb_removed))
 call require(nb_to_land == 5 .and. nb_to_ocean == 4 .and. nb_removed > 0, 'the synthetic cells use the branches: '// &
  itext(nb_to_land)//' '//itext(nb_to_ocean)//' '//itext(nb_removed))
 do c = 1, ncells
  ! sst: the read value, every cell (the pool is replaced by the stream read)
  call same(real(sst_in(c)), lf_sst(c), 'sst', c)
  ! xice, in the three places lean keeps it
  call same(real(o_xice(c)), lf_xice(c), 'xice (lean_fields)', c)
  call same(real(o_xice(c)), md_ice(c), 'xice (model_data)', c)
  call same(real(o_xice(c)), sea_ice_cells%xice(c), 'xice (sea ice)', c)
  call same(real(o_xicem(c)), xicem(c), 'xicem', c)
  call require(ivgtyp_now(c) == o_ivgtyp(c), 'ivgtyp of cell '//itext(c)//': MPAS '//itext(o_ivgtyp(c))//', lean '//itext(ivgtyp_now(c)))
  call require(isltyp_now(c) == o_isltyp(c), 'isltyp of cell '//itext(c)//': MPAS '//itext(o_isltyp(c))//', lean '//itext(isltyp_now(c)))
  call same(real(o_xland(c)), state%xland(c), 'xland', c)
  call same(real(o_tsk(c)), state%tsk(c), 'tsk', c)
  call same(real(o_vegfra(c)), lf_vegetation(c), 'vegfra', c)
  call same(real(o_snow(c)), md_snow(c), 'snow', c)
  call same(real(o_snowh(c)), state%snowh(c), 'snowh', c)
  call same(real(o_albedo(c)), md_albedo(c), 'sfc_albedo', c)
  call same(real(o_emiss(c)), md_emiss(c), 'sfc_emiss', c)
  ice_after = o_xland(c) < 1.5 .and. o_xice(c) >= 0.02
  ! the sea-ice memories of the cells that are sea ice or water (a land cell has none)
  if (o_xland(c) >= 1.5 .or. ice_after) then
   call same(real(o_albedo(c)), sea_ice_cells%albedo(c), 'sfc_albedo (sea ice)', c)
   call same(real(o_emiss(c)), sea_ice_cells%emiss(c), 'sfc_emiss (sea ice)', c)
   call same(real(o_snowc(c)), sea_ice_cells%snowc(c), 'snowc', c)
   call same(real(o_snow(c)), sea_ice_cells%snow(c), 'snow (sea ice)', c)
   do k = 1, nsoil
    call same(real(o_tslb(k,c)), sea_ice_cells%tslb(k,c), 'tslb (sea ice or water)', c)
   end do
  else
   do k = 1, nsoil
    call same(real(o_tslb(k,c)), lands%energy_state_temperaturesoilsnow(k,c), 'tslb (land)', c)
   end do
  end if
  do k = 1, nsoil
   call same(real(o_smois(k,c)), lands%water_state_soilmoisture(k,c), 'smois', c)
   call same(real(o_sh2o(k,c)), lands%water_state_soilliqwater(k,c), 'sh2o', c)
  end do
 end do
 ! the cells that changed type keep the memory the sea-ice code starts from
 do c = 10, 13
  call require(sea_ice_cells%z0(c) == state%znt(c), 'new sea ice: z0 is the cell''s roughness, cell '//itext(c))
  call require(sea_ice_cells%sst(c) == sst_in(c), 'new sea ice: the open-water temperature is sst, cell '//itext(c))
 end do
 call require(sea_ice_cells%snoalb(10) == 0.3, 'snoalb of a cell that stays')
 print '(a)', 'ok   update_oracle: 40 cells, every target equals MPAS ('//itext(nb_to_land)//' to land, '// &
  itext(nb_to_ocean)//' to ocean, '//itext(nb_removed)//' removed)'
 end subroutine

 subroutine make_ice(cell, before, now)
 integer, intent(in) :: cell
 real, intent(in) :: before, now
 integer :: k
 o_xland(cell) = 1.; o_xicem(cell) = before; xice_in(cell) = now
 o_ivgtyp(cell) = 15; o_isltyp(cell) = 16; o_vegfra(cell) = 0.; o_snow(cell) = 3.; o_snowc(cell) = 1.; o_snowh(cell) = 0.04
 o_albedo(cell) = 0.5; o_emiss(cell) = 0.95; o_tsk(cell) = 262.
 o_tslb(:,cell) = 255. + 0.5*[(k, k = 1, nsoil)]
 o_smois(:,cell) = 1.; o_sh2o(:,cell) = 1.
  end subroutine

 subroutine map_complete()
 character(len=256) :: line
 character(len=64), allocatable :: assigned(:), mapped(:)
 integer :: unit, ios, n, nm, i, j
 logical :: found
 allocate(assigned(64), mapped(64))
 n = 0
 open(newunit=unit, file=dir//'assigned.txt', status='old', action='read')
 do
  read(unit, '(a)', iostat=ios) line
  if (ios /= 0) exit
  if (len_trim(line) == 0) cycle
  n = n + 1
  assigned(n) = trim(line)
 end do
 close(unit)
 nm = 0
 open(newunit=unit, file='src/surface_bdy.f90', status='old', action='read')
 do
  read(unit, '(a)', iostat=ios) line
  if (ios /= 0) exit
  i = index(line, '! map ')
  if (i == 0) cycle
  nm = nm + 1
  line = adjustl(line(i+6:))
  mapped(nm) = line(1:index(line, ' ')-1)
 end do
 close(unit)
 call require(n == 19, 'the oracle assigns 19 fields, found '//itext(n))
 do i = 1, n
  found = .false.
  do j = 1, nm
   if (trim(mapped(j)) == trim(assigned(i))) found = .true.
  end do
  call require(found, 'MPAS assigns '//trim(assigned(i))//' but update_sst has no "! map '//trim(assigned(i))//'" row')
 end do
 do j = 1, nm
  found = .false.
  do i = 1, n
   if (trim(mapped(j)) == trim(assigned(i))) found = .true.
  end do
  call require(found, 'update_sst has a map row for '//trim(mapped(j))//', which MPAS does not assign')
 end do
 print '(a)', 'ok   map_complete: '//itext(n)//' fields, each with a row in update_sst'
 end subroutine

 subroutine file_ice()
 real, allocatable :: xland(:)
 type(message_list) :: msgs
 call setup()
 allocate(xland(nc1))
 xland = 2.
 xland(1:20) = 1.        ! the first 20 cells are land: the ice of the file is on cell 30
 call require(surface_file_has_ice(dir//'ice.sfc_update.nc', xland, msgs), 'a file with ice on water at 03:00: true')
 call require(.not. surface_file_has_ice(dir//'noice.sfc_update.nc', xland, msgs), 'a file without ice: false')
 xland(30) = 1.          ! the same ice on land: no sea ice appears
 call require(.not. surface_file_has_ice(dir//'ice.sfc_update.nc', xland, msgs), 'ice on land only: false')
 call require(msgs%n == 0, 'surface_file_has_ice wrote a message')
 print '(a)', 'ok   file_ice: ice on water in a later record is found, ice on land and no ice are not'
 end subroutine

 subroutine read_record()
 real, allocatable :: s(:), x(:)
 character(len=19) :: valid
 type(message_list) :: msgs
 integer :: i
 logical :: failed
 call setup()
 allocate(s(nc1), x(nc1))
 call read_surface_record(dir//'ice.sfc_update.nc', '2026-11-08_04:00:00', s, x, valid, msgs)
 call require(msgs%n == 0, 'a read at 04:00 wrote a message: '//first_text(msgs))
 call require(valid == '2026-11-08_03:00:00', 'the record at or before 04:00 is 03:00, got '//valid)
 call require(abs(s(7) - 251.007) < 1.e-3 .and. abs(x(30) - 0.6) < 1.e-6, 'the values of record 2: '//rtext(s(7))//' '//rtext(x(30)))
 call read_surface_record(dir//'ice.sfc_update.nc', '2026-11-08_06:00:00', s, x, valid, msgs)
 call require(valid == '2026-11-08_06:00:00' .and. abs(s(7) - 252.007) < 1.e-3, 'an exact time reads its own record')
 call read_surface_record(dir//'ice.sfc_update.nc', '2026-11-08_23:00:00', s, x, valid, msgs)
 call require(valid == '2026-11-08_06:00:00', 'after the last record: the last one')
 msgs = message_list()
 call read_surface_record(dir//'ice.sfc_update.nc', '2026-11-07_23:00:00', s, x, valid, msgs)
 failed = .false.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_ERR .and. index(msgs%line(i)%text, 'does not contain a record at or before 2026-11-07_23:00:00') > 0) failed = .true.
 end do
 call require(failed, 'no record at or before the time: the stop gives the reason')
 print '(a)', 'ok   read_record: the latest record at or before the time, and the reason when there is none'
 end subroutine

 subroutine land_ice()
 real, allocatable :: sst_in(:), xice_in(:)
 type(message_list) :: msgs
 integer :: c, i
 logical :: found
 call setup()
 allocate(sst_in(nc1), xice_in(nc1))
 any_sea_ice = .true.
 do c = 1, ncells
  state%xland(c) = 2.; state%tsk(c) = 280.; xice_in(c) = 0.; sst_in(c) = 281.; sea_ice_cells%xice(c) = 0.; xicem(c) = 0.
  md_ice(c) = 0.; lf_xice(c) = 0.; lf_sst(c) = 1.
 end do
 state%xland(77) = 1.        ! a land cell
 xice_in(77) = 0.5
 call update_sst(sst_in, xice_in, state, lands, msgs, 'x.nc at 2026-11-08_03:00:00')
 found = .false.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_CRIT .and. index(msgs%line(i)%text, 'x.nc at 2026-11-08_03:00:00: 1 land cell(s) receive sea ice (the first: cell 77, xice') > 0) found = .true.
 end do
 call require(found, 'the stop for sea ice on land names the cell')
 call require(state%xland(77) == 1. .and. lf_xice(77) == 0. .and. xicem(77) == 0., 'nothing is changed')
 print '(a)', 'ok   land_ice: sea ice on a land cell stops before any change'
 end subroutine

 subroutine water_ice()
 real, allocatable :: sst_in(:), xice_in(:)
 type(message_list) :: msgs
 integer :: c, i
 logical :: found
 call setup()
 allocate(sst_in(nc1), xice_in(nc1))
 any_sea_ice = .false.
 do c = 1, ncells
  state%xland(c) = 2.; state%tsk(c) = 280.; xice_in(c) = 0.; sst_in(c) = 281.; sea_ice_cells%xice(c) = 0.; xicem(c) = 0.
  md_ice(c) = 0.; lf_xice(c) = 0.; lf_sst(c) = 1.
 end do
 xice_in(55) = 0.5
 xice_in(56) = 0.7
 call update_sst(sst_in, xice_in, state, lands, msgs, 'x.nc at 2026-11-08_03:00:00')
 found = .false.
 do i = 1, msgs%n
  if (msgs%line(i)%level == LOG_CRIT .and. index(msgs%line(i)%text, 'x.nc at 2026-11-08_03:00:00: 2 water cell(s) receive sea ice (the first: cell 55, xice') > 0 .and. &
      index(msgs%line(i)%text, 'set up no sea-ice cells at its start') > 0) found = .true.
 end do
 call require(found, 'the stop for a new sea-ice cell names the cell')
 call require(state%xland(55) == 2. .and. lf_xice(55) == 0. .and. xicem(55) == 0. .and. lf_sst(55) == 1., 'nothing is changed')
 any_sea_ice = .true.
 msgs%n = 0
 call update_sst(sst_in, xice_in, state, lands, msgs, 'x.nc at 2026-11-08_03:00:00')
 call require(msgs%n == 0 .and. state%xland(55) == 1., 'with the sea-ice cells set up the same record is applied')
 print '(a)', 'ok   water_ice: a water cell that freezes without sea-ice cells at the start stops before any change'
 end subroutine

 subroutine same(a, b, what, c)
 real, intent(in) :: a, b
 character(*), intent(in) :: what
 integer, intent(in) :: c
 if (transfer(a, 0) /= transfer(b, 0)) then
  print '(a)', 'FAIL '//trim(name)//': '//what//' of cell '//itext(c)//': MPAS '//rtext(a)//', lean '//rtext(b)
  stop 1
 end if
 end subroutine

 function first_text(msgs) result(t)
 type(message_list), intent(in) :: msgs
 character(len=:), allocatable :: t
 t = ''
 if (msgs%n > 0) t = trim(msgs%line(1)%text)
 end function

 function itext(i) result(t)
 integer, intent(in) :: i
 character(len=:), allocatable :: t
 character(len=16) :: buffer
 write(buffer, '(i0)') i
 t = trim(adjustl(buffer))
 end function

 function rtext(x) result(t)
 real, intent(in) :: x
 character(len=24) :: t
 write(t, '(g0.8)') x
 end function

 subroutine require(condition, text)
 logical, intent(in) :: condition
 character(*), intent(in) :: text
 if (.not. condition) then
  print '(a)', 'FAIL '//trim(name)//': '//text
  stop 1
 end if
 end subroutine
end program
