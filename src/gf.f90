! The Grell-Freitas convection driver for one column: cu_grell_freitas of MPAS 8.4.2 (physics_wrf/module_cu_gf.mpas.F:54-453),
! reading and writing the model state directly instead of the driver's 3-D arrays.
module grell_freitas
  use config,only:nz,dt,kte=>nz,kde=>nz1
  use model_state,only:column_view
  use gf_state,only:gf_column
  use lean_gf_core,only:cup_gf,cup_gf_sh,neg_check
  implicit none
  private
  public::advance_gf,advance_gf_columns
  contains
  ! The column of GF from a column view; advance_gf_columns takes the fields themselves (the card kernel passes the state's column
  ! sections and keeps the 7 KB view out of its frame).
  subroutine advance_gf(a,f,itimestep)
    type(column_view),intent(in)::a
    type(gf_column),intent(in)::f
    integer,intent(in)::itimestep
    call advance_gf_columns(f,itimestep,a%dx,a%kpbl,a%xland,a%dz,a%exner,a%p_hyd,a%qv,a%rho_moist,a%t_qv,a%t_theta,a%theta,a%u,a%v,a%w)
  end subroutine
  subroutine advance_gf_columns(f,itimestep,c_dx,c_kpbl,c_xland,c_dz,c_exner,c_p_hyd,c_qv,c_rho_moist,c_t_qv,c_t_theta,c_theta,c_u,c_v,c_w)
    real,intent(in)::c_dx,c_xland
    integer,intent(in)::c_kpbl
    real,intent(in)::c_dz(nz),c_exner(nz),c_p_hyd(nz),c_qv(nz),c_rho_moist(nz),c_t_qv(nz),c_t_theta(nz),c_theta(nz)
    real,intent(in)::c_u(nz),c_v(nz),c_w(nz+1)
    type(gf_column),intent(in)::f
    integer,intent(in)::itimestep
    integer,parameter::its=1,ite=1,jts=1,jte=1,kts=1,ide=2,jde=2
    integer,parameter::ichoice_deep=0,ichoice_shallow=8,ishallow_g3=1
    real,parameter::xlv=2.5e6,cp=1004.5,g=9.80616,r_v=461.6
    real::temperature(nz)
    integer, parameter:: autoconv = 1
    integer, parameter:: aeroevap = 1
    integer, parameter:: training = 0
    integer, parameter:: use_excess = 0
    integer, parameter:: use_excess_sh = 0
    integer, parameter:: maxiens = 1
    integer, parameter:: maxens = 1
    integer, parameter:: maxens2 = 1
    integer, parameter:: maxens3 = 16
    integer, parameter:: ensdim = 16
    integer, parameter:: iens = 1
    integer, parameter:: ens4_spread = 3
    integer, parameter:: ens4 = ens4_spread*ens4_spread
    real, parameter:: ccnclean = 250.
    real, parameter:: aodccn = 0.1
    real, parameter:: beta = 0.02
    real, parameter:: tcrit = 258.
    character(len=50)::ierrc
    character(len=50)::ierrcs
    integer::i,j,k,n
    integer::ipr,jpr
    integer::itf,jtf,ktf
    integer::ierr,ierrs
    integer::kbcon,ktop,k22s,kbcons,ktops
    real::dp,dq,pahfs,pgeoh,pqhfl,zkhvfl,zrho,zws
    real::tscl_kf
    real::xmb,xmbs
    real::ccn
    real::aaeq,cuten,psur,pret,ter11,zqexec,ztexec,pmean,umean,vmean
    real,dimension(1:kte)::zo,q2d,po,tn,qo,tshall,qshall
    real,dimension(1:kte)::subt,subq,outt,outq,outqc,outts,outqs,outqcs,subm,cupclw,cupclws,dhdt
    real::apri_gr,apri_w,apri_mc,apri_st,apri_as,apri_capma,apri_capme,apri_capmi
    real::edti_out,massi_flx
    real,dimension(1:16)::pri_ens,xfi_ens
    real,dimension(1:9) ::mconv
    real,dimension(1:kte,1:9) ::omeg
#ifdef LEAN_GF_DXMIN
    ! Lean departure (build switch LEAN_GF_DXMIN=metres, default off): no GF where the cell is finer than LEAN_GF_DXMIN, as with
    ! cu_physics = 0 on a fine WRF nest. The scale awareness of this GF stops at about 6 km: frh = pi*radius**2/dx**2 is capped
    ! at 0.7 (gf_core.f90, cup_gf), so sig = 0.09 of the closure mass flux in every cell of 3 km, 1 km or 500 m.
    ! Such a cell gets at once the outputs that prepare_column and publish_column leave when no plume runs: no rain, cloud base
    ! and top at their start values, zero tendencies and cloud water (publish_column's (0 + (0+0)*0)/exner is +0). The same
    ! values without the column preparation, which is nearly all of GF's cost on kilometre-scale meshes.
    if(c_dx < real(LEAN_GF_DXMIN)) then
      call quiet_column()
      return
    endif
#endif
    call prepare_column()
    call deep_column()
#ifdef LEAN_GF_NOSHALLOW
    ! Lean departure (build switch LEAN_GF_NOSHALLOW, default off): no GF shallow plume. MPAS hard-codes ishallow = 1
    ! (mpas_atmphys_vars.F:340) while MYNN's EDMF mass flux (config_mynn_edmf = 1) already carries the shallow cumulus;
    ! WRF stops on that pair (share/module_check_a_mundo.F: "bl_mynn_edmf > 0 requires both shcu_physics=0 & ishallow=0").
#else
    call shallow_column()
#endif
    call publish_column()
    contains
#ifdef LEAN_GF_DXMIN
    subroutine quiet_column()
      f%hbot = real(kte)
      f%htop = real(kts)
      f%xmb_total = 0.
      f%raincv = 0.
      f%pratec = 0.
      f%k22_shallow = 0
      f%kbcon_shallow = 0
      f%ktop_shallow = 0
      f%xmb_shallow = 0.
      do k = kts, kte
        f%qc_cu(k) = 0.
        f%qi_cu(k) = 0.
        f%rthcuten(k) = 0.
        f%rqvcuten(k) = 0.
        f%rqccuten(k) = 0.
        f%rqicuten(k) = 0.
      enddo
    end subroutine
#endif
    subroutine prepare_column()
      j=1
      temperature=c_theta*c_exner
      itf = min(ite,ide-1)
      ktf = min(kte,kde-1)
      jtf = min(jte,jde-1)
      ipr = ite
      jpr = jte
      f%hbot = real(kte)
      f%htop = real(kts)
      f%xmb_total = 0.
      f%raincv = 0.
      f%pratec = 0.
      f%k22_shallow = 0
      f%kbcon_shallow = 0
      f%ktop_shallow = 0
      f%xmb_shallow = 0.
      do k = jts, kte
        f%qc_cu(k) = 0.
      enddo
      do k = jts, kte
        f%qi_cu(k) = 0.
      enddo
      apri_gr = 0.
      apri_w = 0.
      apri_mc = 0.
      apri_st = 0.
      apri_as = 0.
      apri_capma = 0.
      apri_capme = 0.
      apri_capmi = 0.
      edti_out = 0.
      do n = 1, ensdim
        pri_ens(n) = 0.
        xfi_ens(n) = 0.
      enddo
      do n = 1, ens4
        do k = kts, ktf
          omeg(k,n) = 0.
        enddo
        mconv(n) = 0.
      enddo
      ierrc = " "
      ierr = 0
      kbcon = 0
      ktop = 0
      xmb = 0.
      ierrcs = " "
      ierrs = 0
      kbcons = 0
      ktops = 0
      k22s = 0
      xmbs = 0.
      tscl_kf = c_dx / 25.
      ter11 = max(0.,f%terrain)
      zo(kts) = ter11 + 0.5*c_dz(1)
      do k = kts+1, ktf
        zo(k) = zo(k-1) + 0.5*(c_dz(k-1)+c_dz(k))
      enddo
      psur = f%surface_pressure*.01
      ccn = 1500.
      cuten = 0.
      umean = 0.
      vmean = 0.
      pmean = 0.
      pret = 0.
      aaeq = 0.
      zqexec = 0.
      ztexec = 0.
      do k = kts, ktf
        q2d(k) = c_qv(k)
        if(q2d(k) .lt. 1.e-08) q2d(k) = 1.e-08
        tn(k) = temperature(k) + (f%dynamics_theta(k)+f%radiation_theta(k)+c_t_theta(k))*c_exner(k)*dt
        qo(k) = q2d(k) + (f%dynamics_qv(k)+c_t_qv(k))*dt
        if(tn(k) .lt. 200.) tn(k) = temperature(k)
        if(qo(k) .lt. 1.e-08) qo(k) = 1.e-08
        po(k) = c_p_hyd(k)*.01
        cupclw(k) = 0.
        outq(k) = 0.
        outqc(k) = 0.
        outt(k) = 0.
        subm(k) = 0.
        subq(k) = 0.
        subt(k) = 0.
        tshall(k) = temperature(k) + c_t_theta(k)*c_exner(k)*dt
        qshall(k) = q2d(k) + c_t_qv(k)*dt
        dhdt(k) = cp*c_t_theta(k)*c_exner(k) + xlv*c_t_qv(k)
        cupclws(k) = 0.
        outqcs(k) = 0.
        outqs(k) = 0.
        outts(k) = 0.
      enddo
      do n = 1, ens4
        do k = kts+1, ktf
          omeg(k,n) = -g*0.5*(c_rho_moist(k)+c_rho_moist(k-1))*c_w(k)
        enddo
        do k = kts+1, ktf
          dq = (q2d(k)-q2d(k-1))
          mconv(n) = mconv(n) + omeg(k,n)*dq/g
        enddo
        if(mconv(n) .lt. 0.) mconv(n) = 0.
      enddo
      do k = kts+1, kte-1
        if((po(1)-po(k)).gt.150. .and. po(k).gt.300.) then
          dp = -.5*(po(k+1)-po(k-1))
          umean = umean + c_u(k)*dp
          vmean = vmean + c_v(k)*dp
          pmean = pmean + dp
        endif
      enddo
    end subroutine
    subroutine deep_column()
      call cup_gf(zo,outqc,j,aaeq,temperature,q2d,ter11,subm,tn,qo,po,pret,po,outt,outq,dt,itimestep,psur, &
        & c_u,c_v,tcrit,iens,ztexec,zqexec,ccn,ccnclean,c_rho_moist,c_dx,f%area,mconv,omeg,maxiens,maxens, &
        & maxens2,maxens3,ensdim,apri_gr,apri_w,apri_mc,apri_st,apri_as,apri_capma,apri_capme,apri_capmi,kbcon, &
        & ktop,cupclw,xfi_ens,pri_ens,c_xland,f%shortwave,subt,subq,xlv,r_v,cp,g,ichoice_deep,ipr,jpr,ierrc,ens4, &
        & beta,autoconv,aeroevap,itf,jtf,ktf,training,xmb,use_excess,its,ite,jts,jte,kts,kte)
      call neg_check(j,subt,subq,dt,q2d,outq,outt,outqc,pret,its,ite,kts,kte,itf,ktf)
    end subroutine
    subroutine shallow_column()
      call cup_gf_sh(xmbs,zo,outqcs,j,aaeq,temperature,q2d,ter11,tshall,qshall,po,pret,po,outts,outqs,dt, &
        & itimestep,psur,c_u,c_v,tcrit,ztexec,zqexec,ccn,ccnclean,c_rho_moist,c_dx,f%area,dhdt,c_kpbl,kbcons, &
        & ktops,cupclws,k22s,c_xland,f%shortwave,tscl_kf,xlv,r_v,cp,g,ichoice_shallow,0,0,ierrs,ierrcs,autoconv, &
        & itf,jtf,ktf,use_excess_sh,its,ite,jts,jte,kts,kte)
    end subroutine
    subroutine publish_column()
      f%xmb_shallow = xmbs
      f%k22_shallow = k22s
      f%kbcon_shallow = kbcons
      f%ktop_shallow = ktops
      f%xmb_total = xmb
      if(pret .gt. 0.) then
        cuten = 1.
        f%pratec = pret
        f%raincv = pret*dt
        if(ktop > f%htop) f%htop = ktop + .001
        if(kbcon < f%hbot) f%hbot = kbcon + .001
      endif
      do k = kts, kte
        f%rthcuten(k) = (outts(k) + (subt(k) + outt(k))*cuten)/c_exner(k)
        f%rqvcuten(k) = outqs(k) + (subq(k) + outq(k))*cuten
        if(temperature(k) .lt. tcrit) then
          f%rqccuten(k) = 0.
          f%rqicuten(k) = outqcs(k) + outqc(k)*cuten
          f%qi_cu(k) = cupclws(k) + cupclw(k)*cuten
        else
          f%rqicuten(k) = 0.
          f%rqccuten(k) = outqcs(k) + outqc(k)*cuten
          f%qc_cu(k) = cupclws(k) + cupclw(k)*cuten
        endif
      enddo
    end subroutine
  end subroutine
end module
