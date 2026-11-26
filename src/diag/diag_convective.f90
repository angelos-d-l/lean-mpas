! MPAS diagnostics/mpas_convective_diagnostics.F:148-190,227-493,528-1093.
! Original getcape: George H. Bryan, NCAR, version 1.02 (2008); MPAS BSD license.
module diag_convective
 use config, only: max_levels
 use mpas_kind_types, only: RKIND,R8KIND
 use dyn_config, only: rgas,rv,i_qv
 implicit none
 private
 public :: maxima_order,maxima_update,maxima_reset
 public :: convective_out,convective_compute,getcape,column_height_value,integral_zpoint,integral_zstaggered
 type convective_out
  real,allocatable :: values(:,:)
  logical :: need(12)=.true.
 end type
contains
 subroutine maxima_order(vertices_on_cell,degree,kite,order)
 ! MPAS numbers a block's vertices in the order a walk over its cells first meets them
 ! (framework/mpas_block_decomp.F:633-685, called from mpas_block_creator.F:318), not by
 ! the file's vertex number. Its vertex scatter (mpas_convective_diagnostics.F:166-174)
 ! therefore adds a cell's vertices in that order. One rank, cells in file order.
 integer,intent(in)::vertices_on_cell(:,:),degree(:),kite(:,:)
 integer,intent(out)::order(:,:)
 integer,allocatable::met(:)
 integer::c,i,j,t,v,n
 allocate(met(maxval(vertices_on_cell)));met=0;n=0
 do c=1,size(degree)
  do i=1,degree(c)
   v=vertices_on_cell(i,c)
   if(met(v)==0)then
    n=n+1;met(v)=n
   endif
  enddo
 enddo
 order=0
 do c=1,size(degree)
  do i=1,degree(c)
   order(i,c)=i
  enddo
  do i=2,degree(c)
   t=order(i,c);j=i-1
   do while(j>=1)
    if(met(vertices_on_cell(order(j,c),c))<=met(vertices_on_cell(t,c)))exit
    order(j+1,c)=order(j,c);j=j-1
   enddo
   order(j+1,c)=t
  enddo
 enddo
 end subroutine
 subroutine maxima_update(w,vorticity,u_zonal,u_meridional,zgrid,area,vertices_on_cell,order,kite,kite_area,degree, &
                          uh_max,w_max,wsp_max,do_uh,do_w,do_wsp)
 real,intent(in)::w(:,:),vorticity(:,:),u_zonal(:,:),u_meridional(:,:),zgrid(:,:),area(:),kite_area(:,:)
 integer,intent(in)::vertices_on_cell(:,:),order(:,:),kite(:,:),degree(:)
 real,intent(inout)::uh_max(:),w_max(:),wsp_max(:)
 logical,intent(in)::do_uh,do_w,do_wsp
 integer::c,k,i,j,v,n,nlev,ncol
 real::s,uph,peak,zb,zt,value
 nlev=size(w,1)-1;ncol=size(uh_max)
 !$omp parallel do schedule(dynamic,64) private(k,i,j,v,n,s,uph,peak,zb,zt,value)
 !$acc parallel loop gang vector default(present)
 do c=1,ncol
  if(do_w)then
   peak=w(1,c)
   !$acc loop seq
   do k=1,nlev
    peak=max(peak,w(k,c))
   enddo
   w_max(c)=max(w_max(c),peak)
  endif
  if(do_wsp)wsp_max(c)=max(wsp_max(c),sqrt(u_zonal(1,c)**2+u_meridional(1,c)**2))
  if(do_uh)then
   uph=0.
   !$acc loop seq
   do k=1,nlev
    s=0.
    ! The gather follows MPAS's first-met vertex scatter; kite identifies the cell within that vertex.
    !$acc loop seq
    do n=1,degree(c)
     i=order(n,c);v=vertices_on_cell(i,c);j=kite(i,c)
     s=s+kite_area(j,v)*vorticity(k,v)
    enddo
    value=max(0.,0.5*(w(k,c)+w(k+1,c)))*max(0.,s/area(c))
    zb=max(zgrid(k,c)-zgrid(1,c),2000.)
    zt=min(zgrid(k+1,c)-zgrid(1,c),5000.)
    uph=uph+value*max(0.,zt-zb)
   enddo
   uh_max(c)=max(uh_max(c),uph)
  endif
 enddo
 end subroutine
 subroutine maxima_reset(values)
 real,intent(out)::values(:)
 integer::c,n
 n=size(values)
 !$acc parallel loop gang vector default(present)
 do c=1,n
  values(c)=0.
 enddo
 end subroutine
 subroutine convective_compute(theta_m,scalars,exner,pressure_p,pressure_base,zgrid,u_zonal,u_meridional,out)
 real,intent(in)::theta_m(:,:),scalars(:,:,:),exner(:,:),pressure_p(:,:),pressure_base(:,:),zgrid(:,:)
 real,intent(in)::u_zonal(:,:),u_meridional(:,:)
 type(convective_out),intent(inout)::out
 integer::c
 !$omp parallel do schedule(dynamic,16)
 do c=1,size(out%values,2)
  call convective_column(theta_m(:,c),scalars(:,:,c),exner(:,c),pressure_p(:,c),pressure_base(:,c), &
   zgrid(:,c),u_zonal(:,c),u_meridional(:,c),out%need,out%values(:,c))
 enddo
 end subroutine
 recursive subroutine convective_column(theta_m,scalars,exner,pressure_p,pressure_base,height,uzonal,umeridional,need,out)
 real,intent(in)::theta_m(:),scalars(:,:),exner(:),pressure_p(:),pressure_base(:),height(:),uzonal(:),umeridional(:)
 logical,intent(in)::need(12)
 real,intent(out)::out(12)
 integer::k,nVertLevels,nVertLevelsP1
 integer,parameter::index_qv=i_qv
 real,parameter::rvord=rv/rgas
 real::temperature(max_levels),dewpoint(max_levels)
 real::dudz(max_levels),dvdz(max_levels),zp(max_levels),zrel(max_levels+1),srh(max_levels+1)
 real::p_in(max_levels),t_in(max_levels),td_in(max_levels),evp
        real (kind=RKIND), parameter :: dev_motion = 7.5, z_bunker_bot = 0., z_bunker_top = 6000.
        real (kind=RKIND)            :: u_storm, v_storm, u_srh_bot, v_srh_bot, u_srh_top, v_srh_top
        real (kind=RKIND)            :: u_mean, v_mean, u_shear, v_shear, shear_magnitude
        real (kind=RKIND)            :: b_term, cape_out, cin_out
 logical::need_cape,need_cin,need_srh_01km,need_srh_03km,need_uzonal_sfc,need_uzonal_1km,need_uzonal_6km,need_umerid_sfc,need_umerid_1km,need_umerid_6km,need_tsfc,need_tdsfc
 nVertLevels=size(theta_m);nVertLevelsP1=nVertLevels+1
 out=0.
 need_cape=need(1)
 need_cin=need(2)
 need_srh_01km=need(3)
 need_srh_03km=need(4)
 need_uzonal_sfc=need(5)
 need_uzonal_1km=need(6)
 need_uzonal_6km=need(7)
 need_umerid_sfc=need(8)
 need_umerid_1km=need(9)
 need_umerid_6km=need(10)
 need_tsfc=need(11)
 need_tdsfc=need(12)
            do k = 1,nVertLevels
                temperature(k) = (theta_m(k)/(1._RKIND+rvord*scalars(index_qv,k)))*exner(k)
                evp = 0.01_RKIND * (pressure_base(k) + pressure_p(k)) &
                                 * scalars(index_qv,k) / (scalars(index_qv,k) + 0.622_RKIND)
                evp = max(evp, 1.0e-8_RKIND)
                dewpoint(k) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
                dewpoint(k) = dewpoint(k) + 273.15
            enddo
              zp(1:nVertLevels) = 0.5*(height(1:nVertLevels)+height(2:nVertlevels+1)) - height(1)
              zrel(1:nVertLevels+1) = height(1:nVertLevels+1) - height(1)
              out(5) = uzonal(1)
              out(8) = umeridional(1)
              out(11) = temperature(1)
              out(12) = dewpoint(1)
              if (need_uzonal_1km) then
                  out(6)      = column_height_value(uzonal(1:nVertLevels),      zp, 1000.0_RKIND, nVertLevels)
              end if
              if (need_umerid_1km) then
                  out(9) = column_height_value(umeridional(1:nVertLevels), zp, 1000.0_RKIND, nVertLevels)
              end if
              if (need_uzonal_6km) then
                  out(7)      = column_height_value(uzonal(1:nVertLevels),      zp, 6000.0_RKIND, nVertLevels)
              end if
              if (need_umerid_6km) then
                  out(10) = column_height_value(umeridional(1:nVertLevels), zp, 6000.0_RKIND, nVertLevels)
              end if
               if (need_srh_01km .or. need_srh_03km) then
                   u_srh_bot = uzonal(1)
                   v_srh_bot = umeridional(1)
                   if(z_bunker_bot .gt. zp(1)) then
                     u_srh_bot = column_height_value( uzonal(1:nVertLevels), zp, z_bunker_bot, nVertLevels)
                     v_srh_bot = column_height_value( umeridional(1:nVertLevels), zp, z_bunker_bot, nVertLevels)
                   end if
                   u_srh_top = column_height_value( uzonal(1:nVertLevels), zp, z_bunker_top, nVertLevels)
                   v_srh_top = column_height_value( umeridional(1:nVertLevels), zp, z_bunker_top, nVertLevels)
                   u_shear = u_srh_top - u_srh_bot
                   v_shear = v_srh_top - v_srh_bot
                   u_mean = integral_zstaggered(uzonal(1:nVertLevels),zrel,z_bunker_bot,z_bunker_top,nVertLevels,nVertLevelsP1)/(z_bunker_top-z_bunker_bot)
                   v_mean = integral_zstaggered(umeridional(1:nVertLevels),zrel,z_bunker_bot,z_bunker_top,nVertLevels,nVertLevelsP1)/(z_bunker_top-z_bunker_bot)
                   shear_magnitude = max(0.0001,sqrt(u_shear**2 + v_shear**2))
                   u_storm = u_mean + dev_motion * v_shear/shear_magnitude
                   v_storm = v_mean - dev_motion * u_shear/shear_magnitude
                   do k=2, nVertLevels-1
                       dudz(k) = (uzonal(k)     -uzonal(k-1)     )/(0.5*(height(k+1)-height(k-1)))
                       dvdz(k) = (umeridional(k)-umeridional(k-1))/(0.5*(height(k+1)-height(k-1)))
                   end do
                   dudz(1) = dudz(2)
                   dvdz(1) = dvdz(2)
                   dudz(nVertLevels) = dudz(nVertLevels-1)
                   dvdz(nVertLevels) = dvdz(nVertLevels-1)
                   do k=2,nVertLevels
                     srh(k) = - (0.5*(uzonal(k)      + uzonal(k-1)      )-u_storm)*dvdz(k) &
                              + (0.5*(umeridional(k) + umeridional(k-1) )-v_storm)*dudz(k)
                   end do
                   srh(1) = - (uzonal(1)     - u_storm)*dvdz(1) &
                            + (umeridional(1)- v_storm)*dudz(1)
                   srh(nVertLevelsP1) = srh(nVertLevels)
                   do k=1, nVertLevels+1
                     srh(k) = max(0.,srh(k))  ! discounting negative SRH
                   end do
                   if (need_srh_01km) then
                       out(3) = integral_zpoint(srh, zrel, 0.0_RKIND, 1000.0_RKIND, nVertLevelsP1)
                   end if
                   if (need_srh_03km) then
                       out(4) = integral_zpoint(srh, zrel, 0.0_RKIND, 3000.0_RKIND, nVertLevelsP1)
                   end if
               end if
            if (need_cape .or. need_cin) then
                    p_in(1:nVertLevels) = (pressure_p(1:nVertLevels) + pressure_base(1:nVertLevels)) / 100.0_RKIND
                    t_in(1:nVertLevels) = temperature(1:nVertLevels) - 273.15_RKIND
                    td_in(1:nVertLevels) = dewpoint(1:nVertLevels) - 273.15_RKIND
                    call getcape( nVertLevels, p_in(:nVertLevels), t_in(:nVertLevels), td_in(:nVertLevels), cape_out, cin_out )
                    out(1) = cape_out
                    out(2) = cin_out
            end if
 end subroutine
      recursive real (kind=RKIND) function column_height_value( column_values, z, z_interp, n )
      implicit none
      integer n
      real (kind=RKIND) :: column_values(n), z(n), z_interp, wz, wzp1
      integer :: kz, k
      kz = 1
      do k=1,n
        if(z(k) <= z_interp) kz = k
      end do
      kz = min(kz,n-1)
      wz = (z(kz+1)-z_interp)/(z(kz+1)-z(kz))
      wzp1 = 1. - wz
      column_height_value = wz*column_values(kz) + wzp1*column_values(kz+1)
      end function column_height_value
      recursive real (kind=RKIND) function integral_zstaggered( column_values, z, zbot, ztop, n, np1 )
      implicit none
      integer n, np1
      real (kind=RKIND) :: column_values(n), z(np1), zbot, ztop
      real (kind=RKIND) :: zb, zt
      integer :: k
      integral_zstaggered = 0.
      do k=1,n
        zb = max(z(k), zbot)
        zt = min(z(k+1), ztop)
        integral_zstaggered = integral_zstaggered + column_values(k)*max(0.,(zt-zb))
      end do
      end function integral_zstaggered
      recursive real (kind=RKIND) function integral_zpoint( column_values, z, zbot, ztop, n )
      implicit none
      integer n
      real(kind=RKIND) :: column_values(n), z(n), zbot, ztop
      real(kind=RKIND) :: zb, zt, dz, zr_midpoint, midpoint_value
      integer :: k
      integral_zpoint = 0.
      do k=1,n-1
        zb = max(z(k), zbot)
        zt = min(z(k+1), ztop)
        dz = max(0.,zt-zb)
        zr_midpoint = (0.5*(zt+zb) - z(k))/(z(k+1)-z(k))
        midpoint_value = column_values(k) + (column_values(k+1)-column_values(k))*zr_midpoint
        integral_zpoint = integral_zpoint + dz*midpoint_value
      end do
      end function integral_zpoint
    recursive subroutine getcape( nk , p_in , t_in , td_in, cape , cin )
    implicit none
    integer, intent(in) :: nk
    real (kind=RKIND), dimension(nk), intent(in) :: p_in,t_in,td_in
    real (kind=RKIND), intent(out) :: cape,cin
    real (kind=RKIND), parameter :: pinc = 100.0   ! Pressure increment (Pa)
    integer, parameter :: source = 2    ! Source parcel:
    real (kind=RKIND), parameter :: ml_depth =  200.0  ! depth (m) of mixed layer
    integer, parameter :: adiabat = 1   ! Formulation of moist adiabat:
    logical :: doit,ice,cloud,not_converged
    integer :: k,kmax,n,nloop,i,orec
    real (kind=RKIND), dimension(max_levels) :: p,t,td,pi,q,th,thv,z,pt,pb,pc,pn,ptv
    real (kind=RKIND) :: the,maxthe,parea,narea,lfc
    real (kind=RKIND) :: th1,p1,t1,qv1,ql1,qi1,b1,pi1,thv1,qt,dp,dz,ps,frac
    real (kind=RKIND) :: th2,p2,t2,qv2,ql2,qi2,b2,pi2,thv2
    real (kind=RKIND) :: thlast,fliq,fice,tbar,qvbar,qlbar,qibar,lhv,lhs,lhf,rm,cpm
    real (kind=R8KIND) :: avgth,avgqv
    real (kind=RKIND), parameter :: g     = 9.81
    real (kind=RKIND), parameter :: p00   = 100000.0
    real (kind=RKIND), parameter :: cp    = 1005.7
    real (kind=RKIND), parameter :: rd    = 287.04
    real (kind=RKIND), parameter :: rv    = 461.5
    real (kind=RKIND), parameter :: xlv   = 2501000.0
    real (kind=RKIND), parameter :: xls   = 2836017.0
    real (kind=RKIND), parameter :: t0    = 273.15
    real (kind=RKIND), parameter :: cpv   = 1875.0
    real (kind=RKIND), parameter :: cpl   = 4190.0
    real (kind=RKIND), parameter :: cpi   = 2118.636
    real (kind=RKIND), parameter :: lv1   = xlv+(cpl-cpv)*t0
    real (kind=RKIND), parameter :: lv2   = cpl-cpv
    real (kind=RKIND), parameter :: ls1   = xls+(cpi-cpv)*t0
    real (kind=RKIND), parameter :: ls2   = cpi-cpv
    real (kind=RKIND), parameter :: rp00  = 1.0/p00
    real (kind=RKIND), parameter :: eps   = rd/rv
    real (kind=RKIND), parameter :: reps  = rv/rd
    real (kind=RKIND), parameter :: rddcp = rd/cp
    real (kind=RKIND), parameter :: cpdrd = cp/rd
    real (kind=RKIND), parameter :: cpdg  = cp/g
    real (kind=RKIND), parameter :: converge = 0.0002
    integer, parameter :: debug_level =   0
    do k=1,nk
        p(k) = 100.0*p_in(k)
        t(k) = 273.15+t_in(k)
       td(k) = 273.15+td_in(k)
       pi(k) = (p(k)*rp00)**rddcp
        q(k) = getqvs(p(k),td(k))
       th(k) = t(k)/pi(k)
      thv(k) = th(k)*(1.0+reps*q(k))/(1.0+q(k))
    enddo
    z(1) = 0.0
    do k=2,nk
      dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))
      z(k) = z(k-1) + dz
    enddo
  IF(source.eq.1)THEN
    kmax = 1
  ELSEIF(source.eq.2)THEN
    IF(p(1).lt.50000.0)THEN
      kmax = 1
      maxthe = getthe(p(1),t(1),td(1),q(1))
    ELSE
      maxthe = 0.0
      do k=1,nk
        if(p(k).ge.50000.0)then
          the = getthe(p(k),t(k),td(k),q(k))
          if( the.gt.maxthe )then
            maxthe = the
            kmax = k
          endif
        endif
      enddo
    ENDIF
  ELSEIF(source.eq.3)THEN
    IF( (z(2)-z(1)).gt.ml_depth )THEN
      avgth = th(1)
      avgqv = q(1)
      kmax = 1
    ELSEIF( z(nk).lt.ml_depth )THEN
      avgth = th(nk)
      avgqv = q(nk)
      kmax = nk
    ELSE
      avgth = 0.0
      avgqv = 0.0
      k = 2
      do while( (z(k).le.ml_depth) .and. (k.le.nk) )
        avgth = avgth + 0.5*(z(k)-z(k-1))*(th(k)+th(k-1))
        avgqv = avgqv + 0.5*(z(k)-z(k-1))*(q(k)+q(k-1))
        k = k + 1
      enddo
      th2 = th(k-1)+(th(k)-th(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))
      qv2 =  q(k-1)+( q(k)- q(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))
      avgth = avgth + 0.5*(ml_depth-z(k-1))*(th2+th(k-1))
      avgqv = avgqv + 0.5*(ml_depth-z(k-1))*(qv2+q(k-1))
      avgth = avgth/ml_depth
      avgqv = avgqv/ml_depth
      kmax = 1
    ENDIF
  ELSE
    return
  ENDIF
    narea = 0.0
  if( (source.eq.1).or.(source.eq.2) )then
    k    = kmax
    th2  = th(kmax)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = t(kmax)
    thv2 = thv(kmax)
    qv2  = q(kmax)
    b2   = 0.0
  elseif( source.eq.3 )then
    k    = kmax
    th2  = avgth
    qv2  = avgqv
    thv2 = th2*(1.0+reps*qv2)/(1.0+qv2)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = th2*pi2
    b2   = g*( thv2-thv(kmax) )/thv(kmax)
  endif
    ql2 = 0.0
    qi2 = 0.0
    qt  = qv2
    cape = 0.0
    cin  = 0.0
    lfc  = 0.0
    doit = .true.
    cloud = .false.
    if(adiabat.eq.1.or.adiabat.eq.2)then
      ice = .false.
    else
      ice = .true.
    endif
      the = getthe(p2,t2,t2,qv2)
      if(debug_level.ge.100)then
      endif
    do while( doit .and. (k.lt.nk) )
        k = k+1
       b1 =  b2
       dp = p(k-1)-p(k)
      if( dp.lt.pinc )then
        nloop = 1
      else
        nloop = 1 + int( dp/pinc )
        dp = dp/float(nloop)
      endif
      do n=1,nloop
         p1 =  p2
         t1 =  t2
        pi1 = pi2
        th1 = th2
        qv1 = qv2
        ql1 = ql2
        qi1 = qi2
        thv1 = thv2
        p2 = p2 - dp
        pi2 = (p2*rp00)**rddcp
        thlast = th1
        i = 0
        not_converged = .true.
        do while( not_converged )
          i = i + 1
          t2 = thlast*pi2
          if(ice)then
            fliq = max(min((t2-233.15)/(273.15-233.15),1.0),0.0)
            fice = 1.0-fliq
          else
            fliq = 1.0
            fice = 0.0
          endif
          qv2 = min( qt , fliq*getqvs(p2,t2) + fice*getqvi(p2,t2) )
          qi2 = max( fice*(qt-qv2) , 0.0 )
          ql2 = max( qt-qv2-qi2 , 0.0 )
          tbar  = 0.5*(t1+t2)
          qvbar = 0.5*(qv1+qv2)
          qlbar = 0.5*(ql1+ql2)
          qibar = 0.5*(qi1+qi2)
          lhv = lv1-lv2*tbar
          lhs = ls1-ls2*tbar
          lhf = lhs-lhv
          rm=rd+rv*qvbar
          cpm=cp+cpv*qvbar+cpl*qlbar+cpi*qibar
          th2=th1*exp(  lhv*(ql2-ql1)/(cpm*tbar)     &
                       +lhs*(qi2-qi1)/(cpm*tbar)     &
                       +(rm/cpm-rd/cp)*log(p2/p1) )
          if(i .gt. 100)then
            if (debug_level .gt. 0) then
            end if
            return
          endif
          if( abs(th2-thlast).gt.converge )then
            thlast=thlast+0.3*(th2-thlast)
          else
            not_converged = .false.
          endif
        enddo
        if( ql2.ge.1.0e-10 ) cloud = .true.
        IF(adiabat.eq.1.or.adiabat.eq.3)THEN
          qt  = qv2
          ql2 = 0.0
          qi2 = 0.0
        ELSEIF(adiabat.le.0.or.adiabat.ge.5)THEN
          return
        ENDIF
      enddo
      thv2 = th2*(1.0+reps*qv2)/(1.0+qv2+ql2+qi2)
        b2 = g*( thv2-thv(k) )/thv(k)
        dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))
      the = getthe(p2,t2,t2,qv2)
      if( (b2.ge.0.0) .and. (b1.lt.0.0) )then
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b2/(b2-b1)
        parea =  0.5*b2*dz*frac
        narea = narea-0.5*b1*dz*(1.0-frac)
        if(debug_level.ge.200)then
        endif
        cin  = cin  + narea
        narea = 0.0
      elseif( (b2.lt.0.0) .and. (b1.gt.0.0) )then
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b1/(b1-b2)
        parea =  0.5*b1*dz*frac
        narea = -0.5*b2*dz*(1.0-frac)
        if(debug_level.ge.200)then
        endif
      elseif( b2.lt.0.0 )then
        parea =  0.0
        narea = narea-0.5*dz*(b1+b2)
      else
        parea =  0.5*dz*(b1+b2)
        narea =  0.0
      endif
      cape = cape + max(0.0,parea)
      if(debug_level.ge.200)then
      endif
      if( (p(k).le.10000.0).and.(b2.lt.0.0) )then
        doit = .false.
      endif
    enddo
    return
    end subroutine getcape
    real (kind=RKIND) function getqvs(p,t)
    implicit none
    real (kind=RKIND) :: p,t,es
    real (kind=RKIND), parameter :: eps = 287.04/461.5
    es = 611.2*exp(17.67*(t-273.15)/(t-29.65))
    getqvs = eps*es/(p-es)
    return
    end function getqvs
    real (kind=RKIND) function getqvi(p,t)
    implicit none
    real (kind=RKIND) :: p,t,es
    real (kind=RKIND), parameter :: eps = 287.04/461.5
    es = 611.2*exp(21.8745584*(t-273.15)/(t-7.66))
    getqvi = eps*es/(p-es)
    return
    end function getqvi
    real (kind=RKIND) function getthe(p,t,td,q)
    implicit none
    real (kind=RKIND) :: p,t,td,q
    real (kind=RKIND) :: tlcl
    if( (td-t).ge.-0.1 )then
      tlcl = t
    else
      tlcl = 56.0 + ( (td-56.0)**(-1) + 0.00125*log(t/td) )**(-1)
    endif
    getthe=t*( (100000.0/p)**(0.2854*(1.0-0.28*q)) )   &
            *exp( ((3376.0/tlcl)-2.54)*q*(1.0+0.81*q) )
    return
    end function getthe
end module
