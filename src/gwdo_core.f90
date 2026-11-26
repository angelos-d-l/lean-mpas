! The YSU orographic gravity-wave drag of MPAS 8.4.2 for one column: bl_gwdo_run (physics/physics_mmm/bl_gwdo.F90:55), which
! the WRF interface physics_wrf/module_bl_gwdo.F calls; the order of the vertical expressions is kept.
module lean_gwdo
  use config,only:nz,kte=>nz,kme=>nz1
  implicit none
  private
  public::gwdo_column
  integer,parameter::kind_phys=kind(1.)
  contains
  subroutine gwdo_column(sina, cosa, rublten,rvblten, dtaux3d,dtauy3d, dusfcg,dvsfcg, uproj, vproj, t1, q1, &
    & prsi, prsl, prslk, zl, var, oc1, oa2d1, oa2d2, oa2d3, oa2d4, ol2d1, ol2d2, ol2d3, ol2d4, g_, cp_, rd_, &
    & rv_, fv_, pi_, dxmeter, deltim, errmsg, errflg )
    implicit none
    integer, parameter :: kts = 1
    real(kind=kind_phys) , intent(in ) :: g_, pi_, rd_, rv_, fv_, cp_, deltim
    real(kind=kind_phys) , intent(in ) :: dxmeter
    real(kind=kind_phys), dimension(kte) , intent(inout) :: rublten, rvblten
    real(kind=kind_phys), dimension(kte) , intent( out) :: dtaux3d, dtauy3d
    real(kind=kind_phys) , intent( out) :: dusfcg, dvsfcg
    real(kind=kind_phys) , intent(in ) :: sina, cosa
    real(kind=kind_phys), dimension(kte) , intent(in ) :: uproj, vproj
    real(kind=kind_phys), dimension(kte) , intent(in ) :: t1, q1, prslk, zl
    real(kind=kind_phys), dimension(kte) , intent(in ) :: prsl
    real(kind=kind_phys), dimension(kme) , intent(in ) :: prsi
    real(kind=kind_phys) , intent(in ) :: var, oc1, oa2d1, oa2d2, oa2d3, oa2d4, ol2d1, ol2d2, ol2d3, ol2d4
    character(len=*) , intent( out) :: errmsg
    integer , intent( out) :: errflg
    real(kind=kind_phys), parameter :: ric = 0.25
    real(kind=kind_phys), parameter :: dw2min = 1.
    real(kind=kind_phys), parameter :: rimin = -100.
    real(kind=kind_phys), parameter :: bnv2min = 1.0e-5
    real(kind=kind_phys), parameter :: efmin = 0.0
    real(kind=kind_phys), parameter :: efmax = 10.0
    real(kind=kind_phys), parameter :: xl = 4.0e4
    real(kind=kind_phys), parameter :: critac = 1.0e-5
    real(kind=kind_phys), parameter :: gmax = 1.
    real(kind=kind_phys), parameter :: veleps = 1.0
    real(kind=kind_phys), parameter :: frc = 1.0
    real(kind=kind_phys), parameter :: ce = 0.8
    real(kind=kind_phys), parameter :: cg = 0.5
    integer,parameter :: kpblmin = 2
    integer :: kpblmax
    integer :: latd,lond
    integer :: i,k,lcap,lcapp1,nwd,idir, klcap,kp1,ikount,kk
    real(kind=kind_phys) :: fdir,cs,rcsks, wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks, wtkbj,tem,gfobnv,hd, &
      & fro,rim,temc,tem1,efact, temv,dtaux,dtauy
    real(kind=kind_phys), dimension(kts:kte) :: dudt, dvdt
    real(kind=kind_phys), dimension(kts:kte) :: dtaux2d, dtauy2d
    real(kind=kind_phys) :: dusfc, dvsfc
    logical :: ldrag, icrilv, flag,kloop1
    real(kind=kind_phys) :: coefm
    real(kind=kind_phys) :: taub, xn, yn, ubar, vbar, fr, ulow, rulow, bnv, oa, ol, rhobar, dtfac, brvf, &
      & xlinv, delks,delks1, zlowtop,cleff
    real(kind=kind_phys), dimension(kts:kte+1) :: taup
    real(kind=kind_phys), dimension(kts:kte-1) :: velco
    real(kind=kind_phys), dimension(kts:kte) :: bnv2, usqj, taud, rho, vtk, vtj
    real(kind=kind_phys), dimension(kts:kte) :: del
    real(kind=kind_phys), dimension(kts:kte) :: u1, v1
    real(kind=kind_phys), dimension(4) :: oa4, ol4
    integer :: kbl, klowtop
    integer, parameter :: mdir=8
    integer,parameter::nwdir(8)=[6,7,5,8,2,3,1,4]
    real(kind=kind_phys), parameter :: frmax = 10.
    real(kind=kind_phys), parameter :: olmin = 1.0e-5
    real(kind=kind_phys), parameter :: odmin = 0.1
    real(kind=kind_phys), parameter :: odmax = 10.
    real(kind=kind_phys) :: fbdcd
    real(kind=kind_phys) :: zblk, tautem
    real(kind=kind_phys) :: fbdpe, fbdke
    real(kind=kind_phys) :: delx, dely
    real(kind=kind_phys), dimension(4) :: dxy4, dxy4p
    real(kind=kind_phys), dimension(4) :: ol4p
    real(kind=kind_phys) :: dxy, dxyp, olp, od
    real(kind=kind_phys), dimension(kts:kte+1) :: taufb
    integer :: komax
    integer :: kblk
    call initialize_work()
    call layer_thermodynamics()
    call low_level_mean()
    call terrain_direction()
    call stability_and_critical_levels()
    call launch_stress()
    call propagate_stress()
    call flow_blocking()
    call apply_drag()
    contains
    subroutine initialize_work()
      lcap = kte
      lcapp1 = lcap + 1
      fdir = mdir / (2.0*pi_)
      errmsg = ''
      errflg = 0
      delx = dxmeter
      dely = dxmeter
      dxy4(1) = delx
      dxy4(2) = dely
      dxy4(3) = sqrt(delx**2. + dely**2.)
      dxy4(4) = dxy4(3)
      dxy4p(1) = dxy4(2)
      dxy4p(2) = dxy4(1)
      dxy4p(3) = dxy4(4)
      dxy4p(4) = dxy4(3)
      cleff = dxmeter
      ldrag = .false. ; icrilv = .false. ; flag = .true.
      klowtop = 0 ; kbl = 0
      dtaux = 0. ; dtauy = 0. ; xn = 0. ; yn = 0.
      ubar = 0. ; vbar = 0. ; rhobar = 0. ; ulow = 0.
      oa = 0. ; ol = 0. ; taub = 0.
      usqj = 0. ; bnv2 = 0. ; vtj = 0. ; vtk = 0.
      taup = 0. ; taud = 0. ; dtaux2d = 0. ; dtauy2d = 0.
      dtfac = 1.0 ; xlinv = 1.0/xl
      komax = 0
      taufb = 0.0
    end subroutine
    subroutine layer_thermodynamics()
      do k = kts,kte
        vtj(k) = t1(k) * (1.+fv_*q1(k))
        vtk(k) = vtj(k) / prslk(k)
        rho(k) = 1./rd_ * prsl(k) / vtj(k)
        del(k) = prsi(k) - prsi(k+1)
        u1(k) = uproj(k)*cosa - vproj(k)*sina
        v1(k) = uproj(k)*sina + vproj(k)*cosa
      enddo
    end subroutine
    subroutine low_level_mean()
      zlowtop = 2. * var
      kloop1 = .true.
      do k = kts+1,kte
        if(zlowtop .gt. 0.) then
          if(kloop1.and.zl(k)-zl(1).ge.zlowtop) then
            klowtop = k+1
            kloop1 = .false.
          endif
        endif
      enddo
      kpblmax = kte
      kbl = klowtop
      kbl = max(min(kbl,kpblmax),kpblmin)
      komax = kbl
      delks = 1.0 / (prsi(1) - prsi(kbl))
      delks1 = 1.0 / (prsl(1) - prsl(kbl))
      do k = kts,kpblmax
        if(k.lt.kbl) then
          rcsks = del(k) * delks
          rdelks = del(k) * delks
          ubar = ubar + rcsks * u1(k)
          vbar = vbar + rcsks * v1(k)
          rhobar = rhobar + rdelks * rho(k)
        endif
      enddo
    end subroutine
    subroutine terrain_direction()
      oa4(1) = oa2d1
      oa4(2) = oa2d2
      oa4(3) = oa2d3
      oa4(4) = oa2d4
      ol4(1) = ol2d1
      ol4(2) = ol2d2
      ol4(3) = ol2d3
      ol4(4) = ol2d4
      wdir = atan2(ubar,vbar) + pi_
      idir = mod(nint(fdir*wdir),mdir) + 1
      nwd = nwdir(idir)
      oa = (1-2*int((nwd-1)/4)) * oa4(mod(nwd-1,4)+1)
      ol = ol4(mod(nwd-1,4)+1)
      ol4p(1) = ol4(2)
      ol4p(2) = ol4(1)
      ol4p(3) = ol4(4)
      ol4p(4) = ol4(3)
      olp = ol4p(mod(nwd-1,4)+1)
      od = olp/max(ol,olmin)
      od = min(od,odmax)
      od = max(od,odmin)
      dxy = dxy4(MOD(nwd-1,4)+1)
      dxyp = dxy4p(MOD(nwd-1,4)+1)
    end subroutine
    subroutine stability_and_critical_levels()
      do k = kts,kte-1
        ti = 2.0 / (t1(k)+t1(k+1))
        rdz = 1./(zl(k+1) - zl(k))
        tem1 = u1(k) - u1(k+1)
        tem2 = v1(k) - v1(k+1)
        dw2 = tem1*tem1 + tem2*tem2
        shr2 = max(dw2,dw2min) * rdz * rdz
        bvf2 = g_*(g_/cp_+rdz*(vtj(k+1)-vtj(k))) * ti
        usqj(k) = max(bvf2/shr2,rimin)
        bnv2(k) = 2.0*g_*rdz*(vtk(k+1)-vtk(k))/(vtk(k+1)+vtk(k))
      enddo
      ulow = max(sqrt(ubar*ubar + vbar*vbar),1.0)
      rulow = 1./ulow
      do k = kts,kte-1
        velco(k) = 0.5 * ((u1(k)+u1(k+1)) * ubar + (v1(k)+v1(k+1)) * vbar)
        velco(k) = velco(k) * rulow
        if((velco(k).lt.veleps) .and. (velco(k).gt.0.)) then
          velco(k) = veleps
        endif
      enddo
      ldrag = velco(1).le.0.
      do k = kpblmin,kpblmax
        if(k .lt. kbl) ldrag = ldrag.or. velco(k).le.0.
      enddo
      wtkbj = (prsl(1)-prsl(2)) * delks1
      bnv2(1) = wtkbj * bnv2(1)
      usqj(1) = wtkbj * usqj(1)
      do k = kpblmin,kpblmax
        if(k .lt. kbl) then
          rdelks = (prsl(k)-prsl(k+1)) * delks1
          bnv2(1) = bnv2(1) + bnv2(k) * rdelks
          usqj(1) = usqj(1) + usqj(k) * rdelks
        endif
      enddo
      ldrag = ldrag .or. bnv2(1).le.0.0
      ldrag = ldrag .or. ulow.eq.1.0
      ldrag = ldrag .or. var .le. 0.0
      do k = kpblmin,kpblmax
        if(k .lt. kbl) usqj(k) = usqj(1)
      enddo
    end subroutine
    subroutine launch_stress()
      if(.not.ldrag) then
        bnv = sqrt(bnv2(1))
        fr = bnv * rulow * var * od
        fr = min(fr,frmax)
        xn = ubar * rulow
        yn = vbar * rulow
      endif
      if(.not. ldrag) then
        efact = (oa + 2.) ** (ce*fr/frc)
        efact = min(max(efact,efmin),efmax)
        coefm = (1. + ol) ** (oa+1.)
        xlinv = coefm / cleff
        tem = fr * fr * oc1
        gfobnv = gmax * tem / ((tem + cg)*bnv)
        taub = xlinv * rhobar * ulow * ulow * ulow * gfobnv * efact
      else
        taub = 0.0
        xn = 0.0
        yn = 0.0
      endif
      do k = kts,kpblmax
        if(k .le. kbl) taup(k) = taub
      enddo
    end subroutine
    subroutine propagate_stress()
      do k = kpblmin, kte-1
        kp1 = k + 1
        if(k .ge. kbl) then
          icrilv = icrilv .or. ( usqj(k) .lt. ric) .or. (velco(k) .le. 0.0)
          brvf = max(bnv2(k),bnv2min)
          brvf = sqrt(brvf)
        endif
        if(k .ge. kbl .and. (.not. ldrag)) then
          if(.not.icrilv .and. taup(k) .gt. 0.0) then
            temv = 1.0 / velco(k)
            tem1 = coefm/dxy*(rho(kp1)+rho(k))*brvf*velco(k)*0.5
            hd = sqrt(taup(k) / tem1)
            fro = brvf * hd * temv
            tem2 = sqrt(usqj(k))
            tem = 1. + tem2 * fro
            rim = usqj(k) * (1.-fro) / (tem * tem)
            if(rim .le. ric) then
              if((oa .le. 0.).or.(kp1 .ge. kpblmin )) then
                temc = 2.0 + 1.0 / tem2
                hd = velco(k) * (2.*sqrt(temc)-temc) / brvf
                taup(kp1) = tem1 * hd * hd
              endif
            else
              taup(kp1) = taup(k)
            endif
          endif
        endif
      enddo
      if(lcap.lt.kte) then
        do klcap = lcapp1,kte
          taup(klcap) = prsi(klcap) / prsi(lcap) * taup(lcap)
        enddo
      endif
    end subroutine
    subroutine flow_blocking()
      if(.not.ldrag) then
        kblk = 0
        fbdpe = 0.0
        fbdke = 0.0
        do k = kte, kpblmin, -1
          if(kblk.eq.0 .and. k.le.kbl) then
            fbdpe = fbdpe + bnv2(k)*(zl(kbl)-zl(k)) *del(k)/g_/rho(k)
            fbdke = 0.5*(u1(k)**2.+v1(k)**2.)
            if(fbdpe.ge.fbdke) then
              kblk = k
              kblk = min(kblk,kbl)
              zblk = zl(kblk)-zl(kts)
            endif
          endif
        enddo
        if(kblk.ne.0) then
          fbdcd = max(2.0-1.0/od,0.0)
          taufb(kts) = 0.5*rhobar*coefm/dxmeter**2*fbdcd*dxyp *olp*zblk*ulow**2
          tautem = taufb(kts)/real(kblk-kts)
          do k = kts+1, kblk
            taufb(k) = taufb(k-1) - tautem
          enddo
          taup(:) = taup(:) + taufb(:)
        endif
      endif
    end subroutine
    subroutine apply_drag()
      do k = kts,kte
        taud(k) = 1. * (taup(k+1) - taup(k)) * g_ / del(k)
      enddo
      do k = kts,kpblmax-1
        if(k .le. kbl) then
          if(taud(k).ne.0.) dtfac = min(dtfac,abs(velco(k)/(deltim*taud(k))))
        endif
      enddo
      dusfc = 0.
      dvsfc = 0.
      do k = kts,kte
        taud(k) = taud(k) * dtfac
        dtaux = taud(k) * xn
        dtauy = taud(k) * yn
        dtaux2d(k) = dtaux
        dtauy2d(k) = dtauy
        dudt(k) = dtaux
        dvdt(k) = dtauy
        dusfc = dusfc + dtaux * del(k)
        dvsfc = dvsfc + dtauy * del(k)
      enddo
      dusfc = (-1./g_) * dusfc
      dvsfc = (-1./g_) * dvsfc
      do k = kts,kte
        rublten(k) = rublten(k)+dudt(k)*cosa + dvdt(k)*sina
        rvblten(k) = rvblten(k)-dudt(k)*sina + dvdt(k)*cosa
        dtaux3d(k) = dtaux2d(k)*cosa + dtauy2d(k)*sina
        dtauy3d(k) =-dtaux2d(k)*sina + dtauy2d(k)*cosa
      enddo
      dusfcg = dusfc*cosa + dvsfc*sina
      dvsfcg =-dusfc*sina + dvsfc*cosa
      return
    end subroutine
  end subroutine
end module
