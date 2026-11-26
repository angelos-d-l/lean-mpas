! MPAS diagnostics/mpas_isobaric_diagnostics.F:550-1351.
! Host work only. The model never reads these arrays.
module diag_isobaric
 use config, only: max_levels
 use dyn_config, only: rv,rgas,i_qv
 use lean_log, only: log_write
 use lean_messages, only: LOG_ERR,real_text
 implicit none
 private
 public :: isobaric_out,isobaric_allocate,isobaric_compute,interp_column,compute_slp,compute_layer_mean
 type isobaric_out
  ! temperature, relhum, dewpoint, uzonal, umeridional, height, w; then vertex vorticity.
  real,allocatable :: cell(:,:,:),vorticity(:,:),mslp(:),t_isobaric(:,:),z_isobaric(:,:),meanT(:)
  real,allocatable :: pressure(:,:),temperature(:,:),press_in(:,:),field_in(:,:)
  real :: t_iso_levels(5)=0.
  real :: z_iso_levels(13)=0.
  logical :: need(12)=.true.
 end type
contains
 subroutine isobaric_allocate(out,nc,nv,nz)
 type(isobaric_out),intent(inout)::out
 integer,intent(in)::nc,nv,nz
 allocate(out%cell(8,nc,7),out%vorticity(8,nv),out%mslp(nc),out%t_isobaric(5,nc), &
  out%z_isobaric(13,nc),out%meanT(nc),out%pressure(nz,nc),out%temperature(nz,nc),out%press_in(nc,nz),out%field_in(nc,nz))
 end subroutine
 subroutine isobaric_compute(theta_m,scalars,exner,pressure_p,pressure_base,zgrid,w,u_zonal,u_meridional,relhum, &
                            vorticity,cells_on_vertex,kite_areas,area_triangle,out)
 real,intent(in)::theta_m(:,:),scalars(:,:,:),exner(:,:),pressure_p(:,:),pressure_base(:,:),zgrid(:,:),w(:,:)
 real,intent(in)::u_zonal(:,:),u_meridional(:,:),relhum(:,:),vorticity(:,:),kite_areas(:,:),area_triangle(:)
 integer,intent(in)::cells_on_vertex(:,:)
 type(isobaric_out),intent(inout)::out
 real,parameter::targets(8)=[50.,100.,200.,250.,500.,700.,850.,925.]
 real::p(max_levels+1),p2(max_levels+1),p_pa(max_levels+1),p2_pa(max_levels+1),t(max_levels),dew(max_levels),pv(max_levels),evp,z0,z1,z2,w1,w2
 integer::c,k,j,n,nc,v,cc,failed
 n=size(theta_m,1);nc=size(out%mslp)
 ! MPAS:562-584: coordinate pools are set only when their interpolated field is written.
 if(out%need(10))out%t_iso_levels=[30000.,35000.,40000.,45000.,50000.]
 if(out%need(11))out%z_iso_levels=[30000.,35000.,40000.,45000.,50000.,55000.,60000.,65000.,70000.,75000.,80000.,85000.,90000.]
 ! MPAS:550-652. Keep the hPa conversion, including the round trip for t_isobaric.
 do c=1,nc
  do k=1,n
   p(k)=(pressure_p(k,c)+pressure_base(k,c))/100.
   t(k)=(theta_m(k,c)/(1.+(rv/rgas)*scalars(i_qv,k,c)))*exner(k,c)
   evp=p(k)*scalars(i_qv,k,c)/(scalars(i_qv,k,c)+0.622)
   evp=max(evp,1.e-8)
   dew(k)=(243.5*log(evp/6.112))/(17.67-log(evp/6.112))
   dew(k)=dew(k)+273.15
   p_pa(k)=p(k)*100.
   out%press_in(c,n+1-k)=p_pa(k)
   out%field_in(c,n+1-k)=t(k)
  enddo
  out%pressure(:,c)=p(:n);out%temperature(:,c)=t(:n)
  if(out%need(1))call interp_column(n,8,p(n:1:-1),t(n:1:-1),targets,out%cell(:,c,1))
  if(out%need(2))call interp_column(n,8,p(n:1:-1),relhum(n:1:-1,c),targets,out%cell(:,c,2))
  if(out%need(3))call interp_column(n,8,p(n:1:-1),dew(n:1:-1),targets,out%cell(:,c,3))
  if(out%need(4))call interp_column(n,8,p(n:1:-1),u_zonal(n:1:-1,c),targets,out%cell(:,c,4))
  if(out%need(5))call interp_column(n,8,p(n:1:-1),u_meridional(n:1:-1,c),targets,out%cell(:,c,5))
  ! MPAS:596-626: logarithmic pressure on the interfaces, including both extrapolations.
  do k=1,n+1
   if(k==1)then
    z0=zgrid(k,c);z1=0.5*(zgrid(k,c)+zgrid(k+1,c));z2=0.5*(zgrid(k+1,c)+zgrid(k+2,c))
    w1=(z0-z2)/(z1-z2);w2=1.-w1
    p2(k)=exp(w1*log(p(k))+w2*log(p(k+1)))
   else if(k==n+1)then
    z0=zgrid(k,c);z1=0.5*(zgrid(k,c)+zgrid(k-1,c));z2=0.5*(zgrid(k-1,c)+zgrid(k-2,c))
    w1=(z0-z2)/(z1-z2);w2=1.-w1
    p2(k)=exp(w1*log(p(k-1))+w2*log(p(k-2)))
   else
    w1=(zgrid(k,c)-zgrid(k-1,c))/(zgrid(k+1,c)-zgrid(k-1,c))
    w2=(zgrid(k+1,c)-zgrid(k,c))/(zgrid(k+1,c)-zgrid(k-1,c))
    p2(k)=exp(w1*log(p(k))+w2*log(p(k-1)))
   endif
  enddo
  do k=1,n+1
   p2_pa(k)=p2(k)*100.
  enddo
  if(out%need(6))call interp_column(n+1,8,p2(n+1:1:-1),zgrid(n+1:1:-1,c),targets,out%cell(:,c,6))
  if(out%need(7))call interp_column(n+1,8,p2(n+1:1:-1),w(n+1:1:-1,c),targets,out%cell(:,c,7))
  if(out%need(10))call interp_column(n,5,p_pa(n:1:-1),t(n:1:-1),out%t_iso_levels,out%t_isobaric(:,c))
  if(out%need(11))call interp_column(n+1,13,p2_pa(n+1:1:-1),zgrid(n+1:1:-1,c),out%z_iso_levels,out%z_isobaric(:,c))
 enddo
 ! MPAS:631-642: this is a gather, in cellsOnVertex slot order (no block-order scatter).
 if(out%need(8))then
  do v=1,size(area_triangle)
   pv=0.
   do k=1,n
    do j=1,size(cells_on_vertex,1)
     cc=cells_on_vertex(j,v)
     if(cc>0.and.cc<=nc)pv(k)=pv(k)+kite_areas(j,v)*out%pressure(k,cc)
    enddo
    pv(k)=pv(k)/area_triangle(v)
   enddo
   call interp_column(n,8,pv(n:1:-1),vorticity(n:1:-1,v),targets,out%vorticity(:,v))
  enddo
 endif
 if(out%need(9))then
  call compute_slp(nc,n,size(scalars,1),out%temperature,zgrid(:,:nc),out%pressure,i_qv,scalars(:,:,:nc),out%mslp,failed)
  out%mslp=out%mslp*100.
 endif
 if(out%need(12))call compute_layer_mean(out%meanT,50000.,30000., &
  out%field_in,out%press_in,failed)
 end subroutine
 ! MPAS:1005-1091, one column retains its own bracket between targets.
 subroutine interp_column(nlev_in,nlev_out,p_in,f_in,p_out,f_out)
 integer,intent(in)::nlev_in,nlev_out
 real,intent(in)::p_in(:),f_in(:),p_out(:)
 real,intent(out)::f_out(:)
 integer::k,kk,upper
 real::dpu,dpl
 upper=1
 do k=1,nlev_out
  do kk=upper,nlev_in-1
   if(p_out(k)>p_in(kk).and.p_out(k)<=p_in(kk+1))then
    upper=kk
    exit
   endif
  enddo
  if(p_out(k)<p_in(1))then
   f_out(k)=f_in(1)*p_out(k)/p_in(1)
  else if(p_out(k)>p_in(nlev_in))then
   f_out(k)=f_in(nlev_in)
  else
   dpu=p_out(k)-p_in(upper);dpl=p_in(upper+1)-p_out(k)
   f_out(k)=(f_in(upper)*dpl+f_in(upper+1)*dpu)/(dpl+dpu)
  endif
 enddo
 end subroutine
    subroutine compute_slp(ncol,nlev_in,nscalars,t,height,p,index_qv,scalars,slp,failed)
       implicit none
       integer, intent(in) :: ncol, nlev_in, nscalars
       real, intent(in), dimension(:,:) :: p,t
       real, intent(in), dimension(:,:) :: height
       integer, intent(in) :: index_qv
       real, intent(in), dimension(:,:,:) :: scalars
       real, intent(out), dimension(:) :: slp
       integer :: icol, k, kcount
       integer :: klo, khi
       real :: gamma, rr, grav
       parameter (rr=287.0, grav=9.80616, gamma=0.0065)
       real :: tc, pconst
       parameter (tc=273.16+17.5, pconst=100.)
       logical mm5_test
       parameter (mm5_test=.true.)
       integer :: level, failed_col
       integer, optional, intent(out) :: failed
       real :: t_surf,t_msl
       real :: plo , phi , tlo, thi , zlo , zhi
       real :: p_at_pconst , t_at_pconst , z_at_pconst, z_half_lowest
       logical :: l1, l2, l3, found
       if(present(failed))failed=0
       do icol=1,ncol
        level=-1
        do k=1,nlev_in
         if(p(k,icol)<p(1,icol)-pconst)then
          level=k
          exit
         endif
        enddo
        if(level==-1)then
         slp=0.
         if(present(failed))then
          failed=1
          call log_write('Troubles finding level '//real_text(pconst)//' above ground.')
          call log_write('Problems first occur at ('//integer_text(icol)//')')
          call log_write('Surface pressure = '//real_text(p(1,icol))//' hPa.')
          call log_write('*** MSLP field will not be computed')
         endif
         return
        endif
          klo = max ( level - 1 , 1      )
          khi = min ( klo + 1        , nlev_in - 1 )
          if(klo==khi)then
           slp=0.
           if(present(failed))then
            failed=2
            call log_write('Trapping levels are weird.')
            call log_write('icol = '//integer_text(icol))
            call log_write('klo = '//integer_text(klo)//', khi = '//integer_text(khi)//': and they should not be equal.')
            call log_write('*** MSLP field will not be computed')
           endif
           return
          endif
          plo = p(klo,icol)
          phi = p(khi,icol)
          tlo = t(klo,icol) * (1. + 0.608 * scalars(index_qv,klo,icol))
          thi = t(khi,icol) * (1. + 0.608 * scalars(index_qv,khi,icol))
          zlo = 0.5*(height(klo,icol)+height(klo+1,icol))
          zhi = 0.5*(height(khi,icol)+height(khi+1,icol))
          p_at_pconst = p(1,icol) - pconst
          t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi)
          z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi)
          t_surf = t_at_pconst*(p(1,icol)/p_at_pconst)**(gamma*rr/grav)
          t_msl = t_at_pconst+gamma*z_at_pconst
        l1=t_msl<tc
        l2=t_surf<=tc
        l3=.not.l1
        if(l2.and.l3)then
         t_msl=tc
        else
         t_msl=tc-0.005*(t_surf-tc)**2
        endif
        z_half_lowest=0.5*(height(1,icol)+height(2,icol))
        slp(icol)=p(1,icol)*exp((2.*grav*z_half_lowest)/(rr*(t_msl+t_surf)))
       enddo
    end subroutine
    function integer_text(i) result(s)
     integer,intent(in)::i
     character(:),allocatable::s
     character(32)::b
     write(b,'(i0)')i
     s=trim(b)
    end function
    subroutine compute_layer_mean(layerMean, p1, p2, field_in, press_in,failed)
       implicit none
       real, dimension(:), intent(out) :: layerMean
       real, intent(in) :: p1, p2
       real, dimension(:,:), intent(in) :: field_in
       real, dimension(:,:), intent(in) :: press_in
       integer,optional,intent(out)::failed
       integer :: nCells, nVertLevels
       integer :: iCell, k
       integer :: k_bot, k_top
       real :: p_bot, p_top
       real :: wtop_p, wtop_m
       real :: wbot_p, wbot_m
       real :: wtotal, w
       real :: temp
       if(present(failed))failed=0
       nCells = size(field_in, 1)
       nVertLevels = size(field_in, 2)
       if (press_in(1,1) > press_in(1,nVertLevels)) then
           if(present(failed))then
            failed=1
            call log_write('Error in compute_layer_mean: pressure should increase with index',LOG_ERR)
           endif
           layerMean(:) = 0.0
           return
       end if
       if (p1 < p2) then
          p_top = p1
          p_bot = p2
       else
          p_top = p2
          p_bot = p1
       end if
       do iCell=1,nCells
          k_bot = -1
          k_top = -1
          do k=1,nVertLevels-1
             if (press_in(iCell,k) <= p_top .and. press_in(iCell,k+1) > p_top) then
                k_top = k
                wtop_p = (p_top - press_in(iCell,k)) / (press_in(iCell,k+1) - press_in(iCell,k))
                wtop_m = (press_in(iCell,k+1) - p_top) / (press_in(iCell,k+1) - press_in(iCell,k))
             end if
             if (press_in(iCell,k) <= p_bot .and. press_in(iCell,k+1) > p_bot) then
                k_bot = k
                wbot_m = (p_bot - press_in(iCell,k)) / (press_in(iCell,k+1) - press_in(iCell,k))
                wbot_p = (press_in(iCell,k+1) - p_bot) / (press_in(iCell,k+1) - press_in(iCell,k))
             end if
          end do
          if (k_top == -1 .or. k_bot == -1) then      ! Layer intersects top or bottom boundary
             layerMean(iCell) = 0.0
          else if (k_top == k_bot) then               ! Layer lies entirely within a single model layer
             layerMean(iCell) = wtop_m * field_in(iCell,k_top) + wtop_p * field_in(iCell,k_top+1)
             layerMean(iCell) = layerMean(iCell) + wbot_m * field_in(iCell,k_bot) + wbot_p * field_in(iCell,k_bot+1)
             layerMean(iCell) = 0.5 * layerMean(iCell)
          else
             wtotal = press_in(iCell,k_top+1) - p_top
             temp = wtop_m * field_in(iCell,k_top) + wtop_p * field_in(iCell,k_top+1)
             layerMean(iCell) = wtotal * 0.5 * (field_in(iCell,k_top+1) + temp)
             do k=k_top+1,k_bot-1
                w = press_in(iCell,k+1) - press_in(iCell,k)
                wtotal = wtotal + w
                layerMean(iCell) = layerMean(iCell) + w * 0.5 * (field_in(iCell,k) + field_in(iCell,k+1))
             end do
             w = p_bot - press_in(iCell,k_bot)
             wtotal = wtotal + w
             temp = wbot_m * field_in(iCell,k_bot) + wbot_p * field_in(iCell,k_bot+1)
             layerMean(iCell) = layerMean(iCell) + w * 0.5 * (field_in(iCell,k_bot) + temp)
             layerMean(iCell) = layerMean(iCell) / wtotal
          end if
       end do
    end subroutine compute_layer_mean
end module
