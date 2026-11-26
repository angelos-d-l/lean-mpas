! The namelist: reading it (the generated lean_namelist_options), resolving the physics suite as MPAS does, and the
! checks that stop a run lean_mpas cannot make or that MPAS 8.4.2 would make on storage it never allocated.
module lean_namelist
 use mpas_kind_types, only: RKIND, StrKIND
 use lean_namelist_options, only: run_config, read_records, check_options
 use lean_messages, only: message_list, add_message, int_text, LOG_OUT, LOG_WARN, LOG_ERR, LOG_CRIT
 implicit none
 private
 public :: run_config, read_namelist, resolve_suite, validate, dynamics_checks, check_scheme_set

 character(len=4), parameter, public :: lean_flavors(2) = ['mpas', 'lean']

contains

 subroutine read_namelist(path, cfg, msgs)
  character(len=*), intent(in) :: path
  type(run_config), intent(out) :: cfg
  type(message_list), intent(inout) :: msgs
  integer :: unit, ierr
  logical :: exists
  character(len=StrKIND) :: reason

  call add_message(msgs, LOG_OUT, 'Reading namelist from file '//trim(path))
  inquire(file=path, exist=exists)
  if (.not. exists) then
   call add_message(msgs, LOG_CRIT, 'Namelist file '//trim(path)//' does not exist.')
   return
  end if
  open(newunit=unit, file=path, status='old', action='read', iostat=ierr, iomsg=reason)
  if (ierr /= 0) then
   call add_message(msgs, LOG_CRIT, 'Cannot open namelist file '//trim(path)//': '//trim(reason))
   return
  end if
  call read_records(unit, cfg, msgs)
  close(unit)
 end subroutine

 ! MPAS mpas_atmphys_control.F:92-332; cfg replaces its pool pointers.
 subroutine resolve_suite(cfg, msgs)
  type(run_config), intent(inout) :: cfg
  type(message_list), intent(inout) :: msgs
  character(len=StrKIND) :: text

  ! the namelist's own value, before the MYNN forcing below (check_scheme_set needs it)
  cfg%sfclayer_off_written = trim(cfg%config_sfclayer_scheme) == 'off'
  call add_message(msgs, LOG_OUT, '')
  call add_message(msgs, LOG_OUT, '----- Setting up physics suite '''//trim(cfg%config_physics_suite)//''' -----')

  if (trim(cfg%config_physics_suite) == 'mesoscale_reference') then
   if (trim(cfg%config_microp_scheme) == 'suite') cfg%config_microp_scheme = 'mp_wsm6'
   if (trim(cfg%config_convection_scheme) == 'suite') cfg%config_convection_scheme = 'cu_ntiedtke'
   if (trim(cfg%config_pbl_scheme) == 'suite') cfg%config_pbl_scheme = 'bl_ysu'
   if (trim(cfg%config_gwdo_scheme) == 'suite') cfg%config_gwdo_scheme = 'bl_ysu_gwdo'
   if (trim(cfg%config_radt_lw_scheme) == 'suite') cfg%config_radt_lw_scheme = 'rrtmg_lw'
   if (trim(cfg%config_radt_sw_scheme) == 'suite') cfg%config_radt_sw_scheme = 'rrtmg_sw'
   if (trim(cfg%config_radt_cld_scheme) == 'suite') cfg%config_radt_cld_scheme = 'cld_fraction'
   if (trim(cfg%config_sfclayer_scheme) == 'suite') cfg%config_sfclayer_scheme = 'sf_monin_obukhov_rev'
   if (trim(cfg%config_lsm_scheme) == 'suite') cfg%config_lsm_scheme = 'sf_noah'
  else if (trim(cfg%config_physics_suite) == 'convection_permitting') then
   if (trim(cfg%config_microp_scheme) == 'suite') cfg%config_microp_scheme = 'mp_thompson'
   if (trim(cfg%config_convection_scheme) == 'suite') cfg%config_convection_scheme = 'cu_grell_freitas'
   if (trim(cfg%config_pbl_scheme) == 'suite') cfg%config_pbl_scheme = 'bl_mynn'
   if (trim(cfg%config_gwdo_scheme) == 'suite') cfg%config_gwdo_scheme = 'bl_ysu_gwdo'
   if (trim(cfg%config_radt_lw_scheme) == 'suite') cfg%config_radt_lw_scheme = 'rrtmg_lw'
   if (trim(cfg%config_radt_sw_scheme) == 'suite') cfg%config_radt_sw_scheme = 'rrtmg_sw'
   if (trim(cfg%config_radt_cld_scheme) == 'suite') cfg%config_radt_cld_scheme = 'cld_fraction'
   if (trim(cfg%config_sfclayer_scheme) == 'suite') cfg%config_sfclayer_scheme = 'sf_mynn'
   if (trim(cfg%config_lsm_scheme) == 'suite') cfg%config_lsm_scheme = 'sf_noah'
  else if (trim(cfg%config_physics_suite) == 'none') then
   if (trim(cfg%config_microp_scheme) == 'suite') cfg%config_microp_scheme = 'off'
   if (trim(cfg%config_convection_scheme) == 'suite') cfg%config_convection_scheme = 'off'
   if (trim(cfg%config_pbl_scheme) == 'suite') cfg%config_pbl_scheme = 'off'
   if (trim(cfg%config_gwdo_scheme) == 'suite') cfg%config_gwdo_scheme = 'off'
   if (trim(cfg%config_radt_lw_scheme) == 'suite') cfg%config_radt_lw_scheme = 'off'
   if (trim(cfg%config_radt_sw_scheme) == 'suite') cfg%config_radt_sw_scheme = 'off'
   if (trim(cfg%config_radt_cld_scheme) == 'suite') cfg%config_radt_cld_scheme = 'off'
   if (trim(cfg%config_sfclayer_scheme) == 'suite') cfg%config_sfclayer_scheme = 'off'
   if (trim(cfg%config_lsm_scheme) == 'suite') cfg%config_lsm_scheme = 'off'
  else
   write(text, '(A)') 'Unrecognized choice of physics suite: config_physics_suite = '''// &
                     trim(cfg%config_physics_suite)//''''
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_microp_scheme == 'off' .or. &
             cfg%config_microp_scheme == 'mp_kessler' .or. &
             cfg%config_microp_scheme == 'mp_thompson' .or. &
             cfg%config_microp_scheme == 'mp_thompson_aerosols' .or. &
             cfg%config_microp_scheme == 'mp_wsm6')) then
   write(text, '(A,A20)') 'illegal value for config_microp_scheme:', trim(cfg%config_microp_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_convection_scheme == 'off' .or. &
             cfg%config_convection_scheme == 'cu_grell_freitas' .or. &
             cfg%config_convection_scheme == 'cu_kain_fritsch' .or. &
             cfg%config_convection_scheme == 'cu_tiedtke' .or. &
             cfg%config_convection_scheme == 'cu_ntiedtke')) then
   write(text, '(A,A20)') 'illegal value for config_convection_scheme: ', trim(cfg%config_convection_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_pbl_scheme == 'off' .or. &
             cfg%config_pbl_scheme == 'bl_mynn' .or. &
             cfg%config_pbl_scheme == 'bl_ysu')) then
   write(text, '(A,A20)') 'illegal value for pbl_scheme: ', trim(cfg%config_pbl_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_gwdo_scheme == 'off' .or. &
             cfg%config_gwdo_scheme == 'bl_ysu_gwdo' .or. &
             cfg%config_gwdo_scheme == 'bl_ugwp_gwdo')) then
   write(text, '(A,A20)') 'illegal value for gwdo_scheme: ', trim(cfg%config_gwdo_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_radt_lw_scheme == 'off' .or. &
             cfg%config_radt_lw_scheme == 'cam_lw' .or. &
             cfg%config_radt_lw_scheme == 'rrtmg_lw')) then
   write(text, '(A,A20)') 'illegal value for longwave radiation scheme: ', trim(cfg%config_radt_lw_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_radt_sw_scheme == 'off' .or. &
             cfg%config_radt_sw_scheme == 'cam_sw' .or. &
             cfg%config_radt_sw_scheme == 'rrtmg_sw')) then
   write(text, '(A,A20)') 'illegal value for shortwave radiation _scheme: ', trim(cfg%config_radt_sw_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  if (.not. (cfg%config_radt_cld_scheme == 'off' .or. &
             cfg%config_radt_cld_scheme == 'cld_incidence' .or. &
             cfg%config_radt_cld_scheme == 'cld_fraction' .or. &
             cfg%config_radt_cld_scheme == 'cld_fraction_thompson')) then
   write(text, '(A,A20)') 'illegal value for calculation of cloud fraction: ', trim(cfg%config_radt_cld_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if
  if ((cfg%config_radt_lw_scheme /= 'off' .and. cfg%config_radt_cld_scheme == 'off') .or. &
      (cfg%config_radt_sw_scheme /= 'off' .and. cfg%config_radt_cld_scheme == 'off')) then
   call add_message(msgs, LOG_OUT, '')
   write(text, '(A,A20)') '    config_radt_cld_scheme is not set for radiation calculation'
   call add_message(msgs, LOG_OUT, trim(text))
   write(text, '(A,A20)') '    switch calculation of cloud fraction to config_radt_cld_scheme = cld_incidence'
   call add_message(msgs, LOG_OUT, trim(text))
   cfg%config_radt_cld_scheme = 'cld_incidence'
  end if

  if (.not. (cfg%config_sfclayer_scheme == 'off' .or. &
             cfg%config_sfclayer_scheme == 'sf_mynn' .or. &
             cfg%config_sfclayer_scheme == 'sf_monin_obukhov' .or. &
             cfg%config_sfclayer_scheme == 'sf_monin_obukhov_rev')) then
   write(text, '(A,A20)') 'illegal value for surface layer scheme: ', trim(cfg%config_sfclayer_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if
  if (cfg%config_pbl_scheme == 'bl_mynn') then
   cfg%config_sfclayer_scheme = 'sf_mynn'
  else if (cfg%config_pbl_scheme == 'bl_ysu') then
   if (cfg%config_sfclayer_scheme /= 'sf_monin_obukhov' .and. &
       cfg%config_sfclayer_scheme /= 'sf_monin_obukhov_rev') then
    write(text, '(A,A20)') 'wrong choice for surface layer scheme with YSU PBL: ', trim(cfg%config_sfclayer_scheme)
    call physics_error_fatal(msgs, text)
    return
   end if
  end if

  if (cfg%config_lsm_scheme /= 'off' .and. cfg%config_sfclayer_scheme == 'off') then
   call physics_error_fatal(msgs, 'land surface scheme: ' // &
                                 'set config_sfclayer_scheme different than off')
   return
  else if (.not. (cfg%config_lsm_scheme == 'off ' .or. &
                  cfg%config_lsm_scheme == 'sf_noah' .or. &
                  cfg%config_lsm_scheme == 'sf_noahmp')) then
   write(text, '(A,A20)') 'illegal value for land surface scheme: ', trim(cfg%config_lsm_scheme)
   call physics_error_fatal(msgs, text)
   return
  end if

  ! mpas_atmphys_control.F:310-318: no physics at all when none of these seven runs (the gwdo scheme does not count)
  cfg%moist_physics = .not. (cfg%config_microp_scheme == 'off' .and. cfg%config_convection_scheme == 'off' .and. &
                             cfg%config_lsm_scheme == 'off' .and. cfg%config_pbl_scheme == 'off' .and. &
                             cfg%config_radt_lw_scheme == 'off' .and. cfg%config_radt_sw_scheme == 'off' .and. &
                             cfg%config_sfclayer_scheme == 'off')

  call add_message(msgs, LOG_OUT, '')
  call add_message(msgs, LOG_OUT, '    config_microp_scheme       = '//trim(cfg%config_microp_scheme))
  call add_message(msgs, LOG_OUT, '    config_convection_scheme   = '//trim(cfg%config_convection_scheme))
  call add_message(msgs, LOG_OUT, '    config_pbl_scheme          = '//trim(cfg%config_pbl_scheme))
  call add_message(msgs, LOG_OUT, '    config_gwdo_scheme         = '//trim(cfg%config_gwdo_scheme))
  call add_message(msgs, LOG_OUT, '    config_radt_cld_scheme     = '//trim(cfg%config_radt_cld_scheme))
  call add_message(msgs, LOG_OUT, '    config_radt_lw_scheme      = '//trim(cfg%config_radt_lw_scheme))
  call add_message(msgs, LOG_OUT, '    config_radt_sw_scheme      = '//trim(cfg%config_radt_sw_scheme))
  call add_message(msgs, LOG_OUT, '    config_sfclayer_scheme     = '//trim(cfg%config_sfclayer_scheme))
  call add_message(msgs, LOG_OUT, '    config_lsm_scheme          = '//trim(cfg%config_lsm_scheme))
  call add_message(msgs, LOG_OUT, '')
 end subroutine

 subroutine validate(cfg, flavor, msgs)
  type(run_config), intent(in) :: cfg
  character(len=*), intent(in) :: flavor
  type(message_list), intent(inout) :: msgs
  integer :: n_fixed, n_scheme, n_problems, n_sets

  n_fixed = 0
  n_scheme = 0
  n_problems = 0
  ! MPAS mpas_atm_time_integration.F:1831-1832 logs both lines and goes on; lean has only SRK3 and stops.
  if (trim(cfg%config_time_integration) /= 'SRK3') then
   call add_message(msgs, LOG_ERR, 'Unknown time integration option '//trim(cfg%config_time_integration))
   call add_message(msgs, LOG_ERR, 'Currently, only ''SRK3'' is supported.')
   n_problems = n_problems + 1
  end if

  call check_options(cfg, msgs, n_fixed, n_scheme)
  if (cfg%config_noahmp_iopt_sfc /= 1 .and. cfg%config_noahmp_iopt_sfc /= 2) then
   call add_message(msgs, LOG_ERR, 'config_noahmp_iopt_sfc = '//int_text(cfg%config_noahmp_iopt_sfc)// &
                    ': lean_mpas supports only 1 or 2; MPAS options 3 and 4 are not supported')
   n_problems = n_problems + 1
  end if
  call check_scheme_set(cfg, msgs, n_sets)
  n_problems = n_problems + n_sets
  if (n_scheme > 0 .and. (trim(cfg%config_physics_suite) /= 'convection_permitting' .or. &
                         trim(cfg%config_lsm_scheme) /= 'sf_noahmp')) then
   call add_message(msgs, LOG_ERR, 'To run lean_mpas, set in &physics:')
   call add_message(msgs, LOG_ERR, '    config_physics_suite = ''convection_permitting''')
   call add_message(msgs, LOG_ERR, '    config_lsm_scheme = ''sf_noahmp''')
   call add_message(msgs, LOG_ERR, &
                    'and put NoahmpTable.TBL in the run directory. Noah-MP gives a different forecast from Noah.')
   call add_message(msgs, LOG_ERR, 'A scheme may also be ''off'' where MPAS 8.4.2 defines that (docs/fixes.md, ' // &
                    '"Scheme sets MPAS cannot run", lists the combinations that stop).')
  end if

  if (len_trim(cfg%config_lean_flavor) > 0) then
   if (.not. any(lean_flavors == trim(cfg%config_lean_flavor))) then
    call add_message(msgs, LOG_ERR, 'config_lean_flavor = '''//trim(cfg%config_lean_flavor)// &
                     ''': the flavors are ''mpas'' and ''lean''')
    n_problems = n_problems + 1
   else if (trim(cfg%config_lean_flavor) /= trim(flavor)) then
    call add_message(msgs, LOG_ERR, 'config_lean_flavor = '''//trim(cfg%config_lean_flavor)// &
                     ''', but this executable is the '''//trim(flavor)//''' flavor: rebuild with --flavor '// &
                     trim(cfg%config_lean_flavor))
    n_problems = n_problems + 1
   end if
  end if

  n_problems = n_problems + n_fixed + n_scheme
  if (n_problems > 0) then
   call add_message(msgs, LOG_CRIT, 'lean_mpas stops: '//int_text(n_problems)// &
                    ' namelist problem(s) listed above; docs/namelist.md lists what lean_mpas does with each option')
  end if
 end subroutine

 ! The scheme combinations in which MPAS 8.4.2 reads or writes storage it never allocated, on the resolved schemes;
 ! each is an error that validate counts. Every other combination has a defined behaviour in MPAS, and lean copies it.
 ! Without n_problems the closing line is added here.
 subroutine check_scheme_set(cfg, msgs, n_problems)
  type(run_config), intent(in) :: cfg
  type(message_list), intent(inout) :: msgs
  integer, intent(out), optional :: n_problems
  logical :: micro_off, conv_on, pbl_on, pbl_off, sfclayer_on, lsm_on, lw_on, sw_on
  character(len=96) :: names
  integer :: n

  n = 0
  micro_off = trim(cfg%config_microp_scheme) == 'off'
  conv_on = trim(cfg%config_convection_scheme) /= 'off'
  pbl_on = trim(cfg%config_pbl_scheme) /= 'off'
  pbl_off = .not. pbl_on
  sfclayer_on = trim(cfg%config_sfclayer_scheme) /= 'off'
  lsm_on = trim(cfg%config_lsm_scheme) /= 'off'
  lw_on = trim(cfg%config_radt_lw_scheme) /= 'off'
  sw_on = trim(cfg%config_radt_sw_scheme) /= 'off'

  ! microphysics off: mpas_atmphys_interface.F:316-335 reads the cloud species of a microphysics scheme that is not there
  if (micro_off .and. (conv_on .or. pbl_on .or. sfclayer_on .or. lsm_on .or. lw_on .or. sw_on)) then
   names = ''
   if (conv_on) call append(names, 'convection')
   if (pbl_on) call append(names, 'pbl')
   if (sfclayer_on) call append(names, 'sfclayer')
   if (lsm_on) call append(names, 'lsm')
   if (lw_on) call append(names, 'radt_lw')
   if (sw_on) call append(names, 'radt_sw')
   call add_message(msgs, LOG_ERR, "config_microp_scheme = 'off' with "//trim(names)// &
                    " on: MPAS 8.4.2 may finish such a run, but its microphysics-to-physics copy then reads cloud "// &
                    "species it has never set (mpas_atmphys_interface.F:316-335), so its numbers come from memory "// &
                    "outside its arrays. Set every scheme 'off' (config_physics_suite = 'none'), "// &
                    "or keep the microphysics.")
   n = n + 1
  end if
  ! surface layer off with MYNN: the written value, before resolve_suite's forcing (mpas_atmphys_control.F:280-281; Registry.xml:488-490)
  if (cfg%sfclayer_off_written .and. trim(cfg%config_pbl_scheme) == 'bl_mynn') then
   call add_message(msgs, LOG_ERR, "config_sfclayer_scheme = 'off' with config_pbl_scheme = 'bl_mynn': MPAS 8.4.2 "// &
                    "turns the surface layer back to sf_mynn (mpas_atmphys_control.F:280-281) after leaving its "// &
                    "fields unallocated (Registry.xml:488-490), and stops with a segmentation fault in its first step. "// &
                    "Leave config_sfclayer_scheme at 'suite' or 'sf_mynn'.")
   n = n + 1
  end if
  ! convection without PBL: mpas_atmphys_driver_convection.F:695-745 reads the PBL tendencies
  if (conv_on .and. pbl_off) then
   call add_message(msgs, LOG_ERR, "config_convection_scheme = '"//trim(cfg%config_convection_scheme)// &
                    "' with config_pbl_scheme = 'off': MPAS 8.4.2 stops there with a segmentation fault in its first "// &
                    "step: its convection reads PBL tendencies it has not allocated "// &
                    "(mpas_atmphys_driver_convection.F:695-745). Set config_convection_scheme = 'off' "// &
                    "too, or keep the PBL.")
   n = n + 1
  end if
  ! gravity-wave drag without PBL: mpas_atmphys_driver_gwdo.F:525-546 writes the PBL tendencies
  if (trim(cfg%config_gwdo_scheme) /= 'off' .and. pbl_off) then
   call add_message(msgs, LOG_ERR, "config_gwdo_scheme = '"//trim(cfg%config_gwdo_scheme)// &
                    "' with config_pbl_scheme = 'off': MPAS 8.4.2 stops there with a segmentation fault in its first "// &
                    "step: its gravity-wave drag writes PBL tendencies it has not allocated "// &
                    "(mpas_atmphys_driver_gwdo.F:525-546). Set config_gwdo_scheme = 'off' too, "// &
                    "or keep the PBL.")
   n = n + 1
  end if
  ! longwave without shortwave: with the longwave scheme on and the shortwave off, MPAS's longwave driver writes xlat_p
  ! (mpas_atmphys_driver_radiation_lw.F:519 and the rrtmg_lwrad call :871), which for RRTMG only the shortwave driver
  ! allocates (mpas_atmphys_driver_radiation_sw.F:125; the longwave one allocates it only for cam_lw, :162): a segmentation fault
  if (lw_on .and. .not. sw_on) then
   call add_message(msgs, LOG_ERR, "config_radt_sw_scheme = 'off' with config_radt_lw_scheme = '"// &
                    trim(cfg%config_radt_lw_scheme)//"': MPAS 8.4.2 stops there with a segmentation fault in its "// &
                    "first physics step: its longwave driver writes xlat_p, which only the shortwave driver allocates "// &
                    "for RRTMG (mpas_atmphys_driver_radiation_lw.F:162,519; mpas_atmphys_driver_radiation_sw.F:125). "// &
                    "Set both radiation schemes 'off', or both on.")
   n = n + 1
  end if
  if (present(n_problems)) then
   n_problems = n
  else if (n > 0) then
   call add_message(msgs, LOG_CRIT, 'lean_mpas stops: '//int_text(n)// &
                    ' namelist problem(s) listed above; docs/namelist.md lists what lean_mpas does with each option')
  end if

 contains
  subroutine append(list, word)
   character(len=*), intent(inout) :: list
   character(len=*), intent(in) :: word
   if (len_trim(list) > 0) then
    list = trim(list)//', '//word
   else
    list = word
   end if
  end subroutine
 end subroutine

 ! MPAS mpas_atm_time_integration.F:151-204, called after the input compatibility checks.
 subroutine dynamics_checks(cfg, msgs)
  type(run_config), intent(in) :: cfg
  type(message_list), intent(inout) :: msgs

  call add_message(msgs, LOG_OUT, '')
  call add_message(msgs, LOG_OUT, 'Checking consistency of dynamics settings...')
  ! MPAS mpas_atm_time_integration.F:162-170,181-199.
  if (cfg%config_positive_definite) then
   call add_message(msgs, LOG_WARN, 'The positive definite advection option is currently unimplemented, and')
   call add_message(msgs, LOG_WARN, 'setting config_positive_definite = true will enable monotonic advection.')
   call add_message(msgs, LOG_WARN, 'Please remove the specification of config_positive_definite from the')
   call add_message(msgs, LOG_WARN, '&nhyd_model namelist group.')
   call add_message(msgs, LOG_OUT, '')
  end if
  if (cfg%config_epssm /= 0.0_RKIND) then
   call add_message(msgs, LOG_WARN, 'The specification of the off-centering parameter for the vertically implicit')
   call add_message(msgs, LOG_WARN, 'acoustic integration using config_epssm in the &nhyd_model namelist group is')
   call add_message(msgs, LOG_WARN, 'no longer supported.')
   call add_message(msgs, LOG_WARN, 'Please use the namelist options')
   call add_message(msgs, LOG_WARN, '  config_epssm_minimum')
   call add_message(msgs, LOG_WARN, '  config_epssm_maximum')
   call add_message(msgs, LOG_WARN, '  config_epssm_transition_bottom_z')
   call add_message(msgs, LOG_WARN, '  config_epssm_transition_top_z')
   call add_message(msgs, LOG_WARN, 'in the &damping namelist group to specify level-dependent off-centering parameters.')
   call add_message(msgs, LOG_OUT, '')
  end if

  call add_message(msgs, LOG_OUT, ' ----- done checking dynamics settings -----')
  call add_message(msgs, LOG_OUT, '')
 end subroutine

 ! MPAS mpas_atmphys_utilities.F:55-58; the caller returns instead of aborting.
 subroutine physics_error_fatal(msgs, text)
  type(message_list), intent(inout) :: msgs
  character(len=*), intent(in) :: text

  call add_message(msgs, LOG_ERR, ' ')
  call add_message(msgs, LOG_ERR, '------------------------------ FATAL CALLED ------------------------------')
  call add_message(msgs, LOG_ERR, trim(text))
  call add_message(msgs, LOG_CRIT, 'MPAS core_physics abort')
 end subroutine

end module
