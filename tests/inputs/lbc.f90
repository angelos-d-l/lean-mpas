! The lbc_in reader (lean_lbc) on the greece_60km boundary files:  lbc ROOT DOCTORED
! MPAS's seek rules, the retest in the neighbouring file, the stops at startup, the ice scalars.
program lbc
 use iso_fortran_env, only: int32, int64
 use nc_check, only: check, finish
 use config
 use dyn_config, only: num_scalars, i_qv, i_qc, i_qr, i_qi, i_qs, i_qg, i_ni, i_nr
 use lean_messages
 use lean_inputs
 use dyn_mesh_data, only: dyn_mesh_type
 use dyn_lbc_data, only: dyn_lbc_type
 use dyn_boundary, only: derive_record, dyn_lbc, lbc_allocate
 implicit none
 character(len=*), parameter :: t0 = '2026-11-08_00:00:00'
 character(len=400) :: root, doctored
 character(len=:), allocatable :: dir, multi, p
 character(len=600) :: want_text(20)
 integer :: want_level(20), nwant = 0
 type(message_list) :: msgs
 type(lbc_request), allocatable :: q(:)
 type(lbc_reader) :: r, empty
 type(dyn_lbc_type) :: record, clean
 type(dyn_lbc) :: boundary_work
 type(dyn_mesh_type) :: m
 type(cold_inputs) :: inp
 integer(int64) :: valid_s
 real, allocatable :: u(:,:), ru(:,:), rho_zz(:,:), rtheta_m(:,:), scalars(:,:,:)
 integer :: i

 call get_command_argument(1, root)
 call get_command_argument(2, doctored)
 dir = trim(root)//'/data/cases/greece_60km/inputs'
 multi = trim(doctored)//'/lbc_multi.nc'

 ! lbc_other_levels: mesh sizes, including levels, govern the boundary stream.
 call set_mesh_sizes(2337, 7183, 4847, 55)
 call requests(1)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), '')
 call lbc_open(r, q, t0, msgs)
 call want(LOG_ERR, file(0)//': nVertLevels = 45; the mesh file has 55')
 call verify('lbc_other_levels')
 call check('lbc_other_levels: no records resolved', r%n == 0)
 call clear_mesh_sizes()

 ! seek_rules: five records in one file, no neighbouring file
 call requests(5)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, multi, '')
 call ask(2, 0_int64, LBC_EARLIEST_STRICTLY_AFTER, multi, '')
 call ask(3, 10800_int64, LBC_EARLIEST_STRICTLY_AFTER, multi, '')
 call ask(4, 25000_int64, LBC_EARLIEST_STRICTLY_AFTER, multi, '')
 call ask(5, 32400_int64, LBC_EARLIEST_STRICTLY_AFTER, multi, '')
 call lbc_open(r, q, t0, msgs)
 call verify('seek_rules')
 call check('seek_rules: records 1-5', r%n == 5 .and. all(r%record == [1, 2, 3, 4, 5]) .and. r%next == 1 .and. &
            all(r%valid_s == [0_int64, 10800_int64, 21600_int64, 32400_int64, 43200_int64]) .and. &
            all(r%at_s == [0_int64, 0_int64, 10800_int64, 25000_int64, 32400_int64]) .and. all(r%file == multi))
 call requests(2)
 call ask(1, 3600_int64, LBC_LATEST_BEFORE, multi, '')
 call ask(2, 3600_int64, LBC_EARLIEST_STRICTLY_AFTER, multi, '')
 call lbc_open(r, q, t0, msgs)
 call verify('seek_rules, between records')
 call check('seek_rules: (3600, LB) and (3600, ESA) give records 1 and 2', r%n == 2 .and. all(r%record == [1, 2]))
 ! several records before the wanted time: the latest of them, not the first (framework/mpas_io_streams.F:289-296)
 call requests(1)
 call ask(1, 25000_int64, LBC_LATEST_BEFORE, multi, '')
 call lbc_open(r, q, t0, msgs)
 call verify('seek_rules, three records before')
 call check('seek_rules: (25000, LB) gives record 3, the latest before', r%n == 1 .and. r%record(1) == 3 .and. &
            r%valid_s(1) == 21600)
 call ask(1, 50000_int64, LBC_LATEST_BEFORE, multi, '')
 call lbc_open(r, q, t0, msgs)
 call verify('seek_rules, every record before')
 call check('seek_rules: (50000, LB) gives record 5', r%n == 1 .and. r%record(1) == 5 .and. r%valid_s(1) == 43200)
 call ask(1, 21600_int64, LBC_LATEST_BEFORE, multi, '')
 call lbc_open(r, q, t0, msgs)
 call verify('seek_rules, a record at the time')
 call check('seek_rules: (21600, LB) gives record 3, the one at the time', r%n == 1 .and. r%record(1) == 3)

 ! retest_after, retest_before
 call requests(2)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), '')
 call ask(2, 0_int64, LBC_EARLIEST_STRICTLY_AFTER, file(0), file(3))
 call lbc_open(r, q, t0, msgs)
 call verify('retest_after')
 call check('retest_after: the second from lbc.03, valid 10800', r%n == 2 .and. trim(r%file(1)) == file(0) .and. &
            trim(r%file(2)) == file(3) .and. r%valid_s(2) == 10800 .and. r%record(2) == 1)
 call requests(1)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(3), file(0))
 call lbc_open(r, q, t0, msgs)
 call verify('retest_before')
 call check('retest_before: lbc.00, valid 0', r%n == 1 .and. trim(r%file(1)) == file(0) .and. r%valid_s(1) == 0)

 ! missing_primary: MPAS's stream line, then its boundary and core lines
 p = dir//'/lbc.2026-11-08_01.00.00.nc'
 call requests(1)
 call ask(1, 3600_int64, LBC_LATEST_BEFORE, p, '')
 call lbc_open(r, q, t0, msgs)
 call want(LOG_ERR, 'Stream ''lbc_in'' attempted to read non-existent file '''//p//'''')
 call want(LOG_ERR, 'Could not read from ''lbc_in'' stream on or before the current date to update lateral boundary tendencies')
 call want(LOG_ERR, 'Failed to process LBC data on or before 2026-11-08_01:00:00')
 call want(LOG_ERR, '    (time 2026-11-08_01:00:00; looked in '//p//')')
 call verify('missing_primary')

 ! hole: the set without its 06 UTC file
 p = trim(doctored)//'/hole/lbc.2026-11-08_'
 call requests(3)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, p//'00.00.00.nc', trim(doctored)//'/hole/lbc.2026-11-07_21.00.00.nc')
 call ask(2, 0_int64, LBC_EARLIEST_STRICTLY_AFTER, p//'00.00.00.nc', p//'03.00.00.nc')
 call ask(3, 10800_int64, LBC_EARLIEST_STRICTLY_AFTER, p//'03.00.00.nc', p//'06.00.00.nc')
 call lbc_open(r, q, t0, msgs)
 call want(LOG_ERR, 'Could not read from ''lbc_in'' stream after the current date to update lateral boundary tendencies')
 call want(LOG_ERR, 'Failed to process LBC data at next time after 2026-11-08_03:00:00')
 call want(LOG_ERR, '    (time 2026-11-08_03:00:00; looked in '//p//'03.00.00.nc and '//p//'06.00.00.nc)')
 call verify('hole')

 ! not_increasing
 call requests(3)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), '')
 call ask(2, 0_int64, LBC_EARLIEST_STRICTLY_AFTER, file(0), file(3))
 call ask(3, 3600_int64, LBC_EARLIEST_STRICTLY_AFTER, file(0), file(3))
 call lbc_open(r, q, t0, msgs)
 call want(LOG_ERR, 'lbc_in: the record read at 2026-11-08_01:00:00 (2026-11-08_03:00:00) is not after the previous one '// &
           '(2026-11-08_03:00:00)')
 call verify('not_increasing')

 ! nVertices is optional for MPAS's LBC fields, but a present value must match.
 p = trim(doctored)//'/lbc_vertices.nc'
 call ask(1, 0_int64, LBC_LATEST_BEFORE, p, '')
 call lbc_open(r, q, t0, msgs)
 call want(LOG_ERR, p//': nVertices = 7; the mesh file has 4847')
 call verify('lbc_other_vertices')
 call check('lbc_other_vertices: no seek', r%n == 0)

 ! the clean first record, then ice_scalars against it
 call requests(1)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), '')
 call lbc_open(r, q, t0, msgs)
 call lbc_next(r, clean, valid_s, msgs)
 call want_ice_warnings()
 call verify('clean record: the five ice-scalar warnings')
 p = trim(doctored)//'/lbc_ice.nc'
 call ask(1, 0_int64, LBC_LATEST_BEFORE, p, '')
 call lbc_open(r, q, t0, msgs)
 call lbc_next(r, record, valid_s, msgs)
 call verify('ice_scalars: no ice-scalar warning')
 call check('ice_scalars: the record holds them', record%qi(3,7) == 1.e-5 .and. record%qs(3,7) == 2.e-5 .and. &
            record%qg(3,7) == 3.e-5 .and. record%ni(3,7) == 4000. .and. record%nr(3,7) == 5000. .and. &
            count(record%qi /= 0.) == 1 .and. count(record%qs /= 0.) == 1 .and. count(record%qg /= 0.) == 1 .and. &
            count(record%ni /= 0.) == 1 .and. count(record%nr /= 0.) == 1)
 call check('ice_scalars: every other array equals the undoctored record', same(record%u, clean%u) .and. &
            same(record%w, clean%w) .and. same(record%rho, clean%rho) .and. same(record%theta, clean%theta) .and. &
            same(record%qv, clean%qv) .and. same(record%qc, clean%qc) .and. same(record%qr, clean%qr) .and. valid_s == 0)
 call read_mesh_inputs(trim(root)//'/data/cases/greece_60km/inputs/greece_60km.init.nc', m, inp, msgs)
 call verify('mesh for derive_record')
 call lbc_allocate(boundary_work)
 m%zz(:,nc1) = 1.0     ! as lbc_first sets it
 allocate(u(nz,ne1), ru(nz,ne1), rho_zz(nz,nc1), rtheta_m(nz,nc1), scalars(num_scalars,nz,nc1))
 call derive_record(record%u, record%rho, record%theta, record%qv, record%qc, record%qr, m%cellsOnEdge, m%zz, u, ru, rho_zz, &
                    rtheta_m, scalars, record%qi, record%qs, record%qg, record%ni, record%nr)
 call check('ice_scalars: derive_record''s scalars rows i_qi..i_nr equal them', all(scalars(i_qi,:,:) == record%qi) .and. &
            all(scalars(i_qs,:,:) == record%qs) .and. all(scalars(i_qg,:,:) == record%qg) .and. &
            all(scalars(i_ni,:,:) == record%ni) .and. all(scalars(i_nr,:,:) == record%nr) .and. scalars(i_qg,3,7) == 3.e-5 &
            .and. all(scalars(i_qv,:,:) == record%qv) .and. all(scalars(i_qc,:,:) == record%qc) .and. &
            all(scalars(i_qr,:,:) == record%qr))
 call derive_record(clean%u, clean%rho, clean%theta, clean%qv, clean%qc, clean%qr, m%cellsOnEdge, m%zz, u, ru, rho_zz, &
                    rtheta_m, scalars)
 call check('derive_record without them leaves those rows 0', all(scalars(i_qi:i_nr,:,:) == 0.) .and. &
            all(scalars(i_qv,:,:) == clean%qv))

 ! no_w_qc_qr: a boundary file without lbc_w, lbc_qc and lbc_qr. MPAS warns for each field its file lacks and goes on
 ! with zeros (build_stream ignores MPAS_streamAddField's error, framework/mpas_stream_manager.F:4360-4399;
 ! framework/mpas_io.F:875); lean does the same.
 p = trim(doctored)//'/lbc_no_w_qc_qr.nc'
 call ask(1, 0_int64, LBC_LATEST_BEFORE, p, '')
 call lbc_open(r, q, t0, msgs)
 call verify('no_w_qc_qr: open')
 call check('no_w_qc_qr: resolved', r%n == 1 .and. r%record(1) == 1)
 record%w = 1.
 record%qc = 1.
 record%qr = 1.
 call lbc_next(r, record, valid_s, msgs)
 call check('no_w_qc_qr: warnings alone', .not. inputs_failed(msgs))
 call want(LOG_WARN, 'Variable lbc_qc not in input file.')
 call want(LOG_WARN, 'Variable lbc_qr not in input file.')
 call want_ice_warnings()
 call want(LOG_WARN, 'Variable lbc_w not in input file.')
 call verify('no_w_qc_qr: MPAS''s WARN lines in the stream''s field order')
 call check('no_w_qc_qr: the three are zero, the others equal the undoctored record', all(record%w == 0.) .and. &
            all(record%qc == 0.) .and. all(record%qr == 0.) .and. same(record%u, clean%u) .and. &
            same(record%rho, clean%rho) .and. same(record%theta, clean%theta) .and. same(record%qv, clean%qv) .and. &
            all(record%qi == 0.))

 ! nan_record
 p = trim(doctored)//'/lbc_nan.nc'
 call ask(1, 0_int64, LBC_LATEST_BEFORE, p, '')
 call lbc_open(r, q, t0, msgs)
 call verify('nan_record: open')
 call lbc_next(r, record, valid_s, msgs)
 call check('nan_record: the caller must stop (inputs_failed)', inputs_failed(msgs))
 call want_ice_warnings()
 call want(LOG_CRIT, p//': lbc_theta: 1 non-finite values in the record of 2026-11-08_00:00:00')
 call verify('nan_record')

 ! lbc_next returns no status: its caller stops on ERR lines too (inputs_failed), not only on a CRIT.
 ! A file replaced after the startup check by one without the boundary fields (here the init file): ERR lines, no CRIT.
 call requests(1)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), '')
 call lbc_open(r, q, t0, msgs)
 call verify('replaced_file: open')
 call lbc_next(r, record, valid_s, msgs)
 call check('replaced_file: warnings alone do not stop the caller', msgs%n == 5 .and. .not. inputs_failed(msgs))
 msgs%n = 0
 r%next = 1
 r%file(1) = dir//'/greece_60km.init.nc'
 call lbc_next(r, record, valid_s, msgs)
 call check('replaced_file: ERR lines and no CRIT', .not. has_fatal(msgs) .and. &
            any(msgs%line(1:msgs%n)%level == LOG_ERR .and. msgs%line(1:msgs%n)%text == &
                dir//'/greece_60km.init.nc: no variable lbc_u (lbc_in stream)'))
 call check('replaced_file: the caller must stop (inputs_failed)', inputs_failed(msgs))
 msgs%n = 0

 ! due
 call requests(3)
 call ask(1, 0_int64, LBC_LATEST_BEFORE, file(0), file(-3))
 call ask(2, 0_int64, LBC_EARLIEST_STRICTLY_AFTER, file(0), file(3))
 call ask(3, 10800_int64, LBC_EARLIEST_STRICTLY_AFTER, file(3), file(6))
 call lbc_open(r, q, t0, msgs)
 call verify('due: open')
 call check('due: the first read is due at 0', lbc_due(r, 1))
 call lbc_next(r, record, valid_s, msgs)
 call lbc_next(r, record, valid_s, msgs)
 call check('due: after two reads, not at 0 or 10799, at 10800', .not. lbc_due(r, 1) .and. &
            .not. lbc_due(r, 90) .and. lbc_due(r, 91) .and. valid_s == 10800)
 call lbc_next(r, record, valid_s, msgs)
 call check('due: after the third, never', .not. lbc_due(r, huge(0)) .and. valid_s == 21600)
 call check('due: an empty reader is never due', .not. lbc_due(empty, huge(0)))
 msgs%n = 0

 ! xtime_seconds
 call check('xtime_seconds', xtime_seconds('2026-11-09_01:02:03', t0) == 90123 .and. &
            xtime_seconds('2026-11-07_23:59:59', t0) == -1 .and. xtime_seconds('garbage', t0) == -huge(0_int64) .and. &
            xtime_seconds(t0, 'garbage') == -huge(0_int64))
 ! lean_clock's parse_time, as MPAS reads a time: a date alone, fractional seconds (whole seconds are
 ! kept), days that roll over
 call check('xtime_seconds: MPAS''s time strings', xtime_seconds('2026-11-09', t0) == 86400 .and. &
            xtime_seconds('2026-11-08_00:00:07.5', t0) == 7 .and. &
            xtime_seconds('2026-02-31_00:00:00', '2026-03-03_00:00:00') == 0 .and. &
            xtime_seconds('2026-11-08_00:00', t0) == -huge(0_int64))
 call finish('lbc')

contains

 function file(hour) result(path)
 integer, intent(in) :: hour
 character(len=:), allocatable :: path
 character(len=2) :: hh
 if (hour < 0) then
  path = dir//'/lbc.2026-11-07_21.00.00.nc'
 else
  write(hh, '(i2.2)') hour
  path = dir//'/lbc.2026-11-08_'//hh//'.00.00.nc'
 end if
 end function

 subroutine requests(n)
 integer, intent(in) :: n
 if (allocated(q)) deallocate(q)
 allocate(q(n))
 end subroutine

 subroutine ask(k, at_s, whence, path, retest)
 integer, intent(in) :: k, whence
 integer(int64), intent(in) :: at_s
 character(*), intent(in) :: path, retest
 q(k)%step = int((at_s+119)/120)+1
 q(k)%at_s = at_s
 q(k)%whence = whence
 q(k)%file = path
 q(k)%retest = retest
 end subroutine

 logical function same(a, b)
 real, intent(in) :: a(:,:), b(:,:)
 same = all(shape(a) == shape(b))
 if (same) same = all(transfer(a, [0_int32]) == transfer(b, [0_int32]))
 end function

 subroutine want(level, text)
 integer, intent(in) :: level
 character(*), intent(in) :: text
 nwant = nwant + 1
 want_level(nwant) = level
 want_text(nwant) = text
 end subroutine

 subroutine want_ice_warnings()
 call want(LOG_WARN, 'Variable lbc_qi not in input file.')
 call want(LOG_WARN, 'Variable lbc_qs not in input file.')
 call want(LOG_WARN, 'Variable lbc_qg not in input file.')
 call want(LOG_WARN, 'Variable lbc_ni not in input file.')
 call want(LOG_WARN, 'Variable lbc_nr not in input file.')
 end subroutine

 ! The WARN, ERR and CRIT lines of msgs are exactly the wanted ones, in order. Both lists are then emptied.
 subroutine verify(name)
 character(*), intent(in) :: name
 integer :: k, n
 logical :: ok
 n = 0
 ok = .true.
 do k = 1, msgs%n
  if (msgs%line(k)%level == LOG_OUT) cycle
  n = n + 1
  if (n > nwant) then
   ok = .false.
  else if (msgs%line(k)%level /= want_level(n) .or. trim(msgs%line(k)%text) /= trim(want_text(n))) then
   ok = .false.
  end if
 end do
 if (n /= nwant) ok = .false.
 call check(name//': the WARN, ERR and CRIT lines', ok)
 if (.not. ok) then
  print '(a)', '   got:'
  do k = 1, msgs%n
   if (msgs%line(k)%level /= LOG_OUT) print '(a,i0,a)', '    ', msgs%line(k)%level, ' '//trim(msgs%line(k)%text)
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
