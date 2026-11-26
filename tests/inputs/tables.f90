! The run directory's physics tables (lean_inputs):  tables ROOT DOCTORED [SET]
! the byte order of the four Thompson tables from their first record marker, and the files lean reads.
! SET (one per process: the scheme set is fixed once per run): which schemes run; a table is asked for
! only by the schemes that run. Without SET every scheme runs.
program tables
 use nc_check, only: check, finish
 use lean_messages
 use lean_inputs
 use schemes, only: set_schemes
 implicit none
 character(len=*), parameter :: missing = ' is missing: lean_mpas reads MPAS''s physics tables from the run directory'
 character(len=30), parameter :: thompson(4) = [character(len=30) :: 'MP_THOMPSON_QIautQS_DATA.DBL', &
  'MP_THOMPSON_QRacrQG_DATA.DBL', 'MP_THOMPSON_QRacrQS_DATA.DBL', 'MP_THOMPSON_freezeH2O_DATA.DBL']
 character(len=16), parameter :: others(7) = [character(len=16) :: 'LANDUSE.TBL', 'OZONE_DAT.TBL', 'OZONE_LAT.TBL', &
  'OZONE_PLEV.TBL', 'RRTMG_LW_DATA', 'RRTMG_SW_DATA', 'NoahmpTable.TBL']
 character(len=400) :: root, doctored, set
 character(len=:), allocatable :: dir
 character(len=600) :: want_text(20)
 integer :: want_level(20), nwant = 0
 type(message_list) :: msgs
 logical :: big
 integer :: i

 call get_command_argument(1, root)
 call get_command_argument(2, doctored)
 call get_command_argument(3, set)
 if (len_trim(set) > 0) then
  call one_set()
  call finish('tables '//trim(set))
  stop
 end if

 ! lean's own tables: little-endian
 big = .true.
 call thompson_table_order(trim(root)//'/data/tables', big, msgs)
 call verify('little_endian')
 call check('little_endian: big_endian is .false.', .not. big)

 ! MPAS's: the same tables written big-endian (tools/dbl_big_endian.py)
 big = .false.
 call thompson_table_order(trim(doctored)//'/tables_be', big, msgs)
 call verify('big_endian')
 call check('big_endian: big_endian is .true.', big)

 ! QIautQS with its first 4 bytes zeroed
 dir = trim(doctored)//'/tables_zero'
 big = .true.
 call want(LOG_ERR, dir//'/'//trim(thompson(1))//': not a Thompson table of MPAS 8.4.2 (first record of 28160 bytes expected)')
 call thompson_table_order(dir, big, msgs)
 call verify('zeroed_marker')
 call check('zeroed_marker: big_endian is .false.', .not. big)

 ! three little-endian and one big-endian
 dir = trim(doctored)//'/tables_mixed'
 big = .true.
 call want(LOG_ERR, 'the Thompson tables in '//dir//' mix byte orders')
 call thompson_table_order(dir, big, msgs)
 call verify('mixed')
 call check('mixed: big_endian is .false.', .not. big)

 ! an empty directory: the four Thompson tables, then all eleven files
 dir = trim(doctored)//'/tables_empty'
 big = .true.
 do i = 1, 4
  call want(LOG_ERR, dir//'/'//trim(thompson(i))//missing)
 end do
 call thompson_table_order(dir, big, msgs)
 call verify('empty, the byte order')
 call check('empty: big_endian is .false.', .not. big)
 do i = 1, 7
  call want(LOG_ERR, dir//'/'//trim(others(i))//missing)
 end do
 do i = 1, 4
  call want(LOG_ERR, dir//'/'//trim(thompson(i))//missing)
 end do
 call check_run_tables(dir, msgs)
 call check('empty: 11 lines', msgs%n == 11)
 call verify('empty, the files')

 ! a directory with the eleven files; then without NoahmpTable.TBL
 call check_run_tables(trim(doctored)//'/tables_all', msgs)
 call check('all: no line at all', msgs%n == 0)
 call verify('all')
 dir = trim(doctored)//'/tables_no_noahmp'
 call want(LOG_ERR, dir//'/NoahmpTable.TBL'//missing)
 call check_run_tables(dir, msgs)
 call verify('no_noahmp')
 ! with every scheme on, the message for each table that a scheme that is off would not ask for
 dir = trim(doctored)//'/tables_no_lw'
 call want(LOG_ERR, dir//'/RRTMG_LW_DATA'//missing)
 call check_run_tables(dir, msgs)
 call verify('no_lw, longwave on')
 dir = trim(doctored)//'/tables_no_thompson'
 do i = 1, 4
  call want(LOG_ERR, dir//'/'//trim(thompson(i))//missing)
 end do
 call check_run_tables(dir, msgs)
 call verify('no_thompson, microphysics on')

 call finish('tables')
contains
 ! The scheme sets: a run directory without the tables of the schemes that are off runs.
 subroutine one_set()
 select case (trim(set))
 case ('none')
  ! suite 'none': no table at all, not even LANDUSE.TBL (MPAS reads it only with moist_physics, mpas_atm_core.F:580-596)
  call set_schemes('off', 'off', 'off', 'off', 'off', 'off', 'off', 'off', 'off', .true.)
  call check_run_tables(trim(doctored)//'/tables_empty', msgs)
  call verify('none: no table asked for')
 case ('lsm_off')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'off', 'rrtmg_lw', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call check_run_tables(trim(doctored)//'/tables_no_noahmp', msgs)
  call verify('lsm_off: no NoahmpTable.TBL')
 case ('lw_off')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'off', 'rrtmg_sw', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call check_run_tables(trim(doctored)//'/tables_no_lw', msgs)
  call verify('lw_off: no RRTMG_LW_DATA')
  ! the shortwave scheme and the ozone tables still count
  dir = trim(doctored)//'/tables_no_sw'
  call want(LOG_ERR, dir//'/RRTMG_SW_DATA'//missing)
  call check_run_tables(dir, msgs)
  call verify('lw_off: RRTMG_SW_DATA still asked for')
 case ('rad_off')
  call set_schemes('mp_thompson', 'cu_grell_freitas', 'bl_mynn', 'sf_mynn', 'sf_noahmp', 'off', 'off', &
   'cld_fraction', 'bl_ysu_gwdo', .true.)
  call check_run_tables(trim(doctored)//'/tables_no_rad', msgs)
  call verify('rad_off: no RRTMG and no ozone tables')
 case ('micro_off')
  call set_schemes('off', 'off', 'off', 'off', 'off', 'rrtmg_lw', 'rrtmg_sw', 'cld_fraction', 'off', .true.)
  call check_run_tables(trim(doctored)//'/tables_no_thompson', msgs)
  call verify('micro_off: no Thompson tables')
 case default
  call check('known set '//trim(set), .false.)
 end select
 end subroutine

 subroutine want(level, text)
 integer, intent(in) :: level
 character(*), intent(in) :: text
 nwant = nwant + 1
 want_level(nwant) = level
 want_text(nwant) = text
 end subroutine

 ! The lines of msgs are exactly the wanted ones, in order. Both lists are then emptied.
 subroutine verify(name)
 character(*), intent(in) :: name
 integer :: k
 logical :: ok
 ok = msgs%n == nwant
 if (ok) then
  do k = 1, nwant
   if (msgs%line(k)%level /= want_level(k) .or. trim(msgs%line(k)%text) /= trim(want_text(k))) ok = .false.
  end do
 end if
 call check(name//': the lines', ok)
 if (.not. ok) then
  print '(a)', '   got:'
  do k = 1, msgs%n
   print '(a,i0,a)', '    ', msgs%line(k)%level, ' '//trim(msgs%line(k)%text)
  end do
  print '(a)', '   wanted:'
  do k = 1, nwant
   print '(a,i0,a)', '    ', want_level(k), ' '//trim(want_text(k))
  end do
 end if
 msgs%n = 0
 nwant = 0
 end subroutine
end program
