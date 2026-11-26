! MPAS 8.4.2 diagnostics/mpas_convective_diagnostics.F:166-190,548-566.
! Original scatter order, kept apart from the gather kernel under test. walk lists the
! vertices in MPAS's block order (first met by a walk over the cells, mpas_block_decomp.F:633-685).
module mpas_diag_oracle
 use diag_convective,only:convective_out
 use diag_isobaric, only: isobaric_out
 use dyn_config, only: i_qv,rv,rgas
 use mpas_kind_types, only: RKIND,R8KIND
 implicit none
 real,parameter::rvord=rv/rgas
contains
 subroutine scatter_uh(w,vorticity,zgrid,cells,kite_area,area,walk,uh)
 real,intent(in)::w(:,:),vorticity(:,:),zgrid(:,:),kite_area(:,:),area(:)
 integer,intent(in)::cells(:,:),walk(:)
 real,intent(out)::uh(:)
 real::column(size(vorticity,1),size(area)+1),z(size(w,1)),uph,zb,zt
 integer::v,j,c,k,nz,n
 nz=size(vorticity,1);column=0.;uh=0.
 do n=1,size(walk)
  v=walk(n)
  do j=1,size(cells,1)
   c=cells(j,v)
   if(c==0)c=size(area)+1
   column(:,c)=column(:,c)+kite_area(j,v)*vorticity(:,v)
  enddo
 enddo
 do c=1,size(area)
  do k=1,nz
   column(k,c)=max(0.,0.5*(w(k,c)+w(k+1,c)))*max(0.,column(k,c)/area(c))
  enddo
  z=zgrid(:,c)-zgrid(1,c);uph=0.
  do k=1,nz
   zb=max(z(k),2000.);zt=min(z(k+1),5000.)
   uph=uph+column(k,c)*max(0.,zt-zb)
  enddo
  uh(c)=max(uh(c),uph)
 enddo
 end subroutine
    subroutine oracle_interp(ncol,nlev_in,nlev_out,pres_in,field_in,pres_out,field_out)

    integer,intent(in):: ncol,nlev_in,nlev_out

    real(kind=RKIND),intent(in),dimension(ncol,nlev_in) :: pres_in,field_in
    real(kind=RKIND),intent(in),dimension(ncol,nlev_out):: pres_out

    real(kind=RKIND),intent(out),dimension(ncol,nlev_out):: field_out

    integer:: icol,k,kk
    integer:: kkstart,kount
    integer,dimension(ncol):: kupper

    real(kind=RKIND):: dpl,dpu



    do icol = 1, ncol
       kupper(icol) = 1
    enddo

    do k = 1, nlev_out

       kkstart = nlev_in
       do icol = 1, ncol
          kkstart = min0(kkstart,kupper(icol))
       enddo
       kount = 0

       do kk = kkstart, nlev_in-1
          do icol = 1, ncol
             if(pres_out(icol,k).gt.pres_in(icol,kk).and.pres_out(icol,k).le.pres_in(icol,kk+1)) then
                kupper(icol) = kk
                kount = kount + 1
             endif
          enddo

          if(kount.eq.ncol) then
             do icol = 1, ncol
                dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
                dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
                field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                                  + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
             end do
             goto 35
           end if
       enddo

       do icol = 1, ncol
          if(pres_out(icol,k) .lt. pres_in(icol,1)) then
             field_out(icol,k) = field_in(icol,1)*pres_out(icol,k)/pres_in(icol,1)
          elseif(pres_out(icol,k) .gt. pres_in(icol,nlev_in)) then
             field_out(icol,k) = field_in(icol,nlev_in)
          else
             dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
             dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
             field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                               + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
          endif
       enddo

    35 continue

    enddo

    end subroutine oracle_interp
    subroutine oracle_slp(ncol,nlev_in,nscalars,t,height,p,index_qv,scalars,slp)

       implicit none

       integer, intent(in) :: ncol, nlev_in, nscalars

       real(kind=RKIND), intent(in), dimension(nlev_in,ncol) :: p,t
       real(kind=RKIND), intent(in), dimension(nlev_in+1,ncol) :: height
       integer, intent(in) :: index_qv
       real(kind=RKIND), intent(in), dimension(nscalars,nlev_in,ncol) :: scalars

       real(kind=RKIND), intent(out), dimension(ncol) :: slp

       integer :: icol, k, kcount
       integer :: klo, khi

       real(kind=RKIND) :: gamma, rr, grav
       parameter (rr=287.0, grav=9.80616, gamma=0.0065)

       real(kind=RKIND) :: tc, pconst
       parameter (tc=273.16+17.5, pconst=100.)

       logical mm5_test
       parameter (mm5_test=.true.)

       integer, dimension(:), allocatable :: level
       real(kind=RKIND), dimension(:), allocatable :: t_surf, t_msl
       real(kind=RKIND) :: plo , phi , tlo, thi , zlo , zhi
       real(kind=RKIND) :: p_at_pconst , t_at_pconst , z_at_pconst, z_half_lowest

       logical :: l1, l2, l3, found


       if (.not.allocated(level))  allocate(level(ncol))
       if (.not.allocated(t_surf)) allocate(t_surf(ncol))
       if (.not.allocated(t_msl))  allocate(t_msl(ncol))

       do icol = 1 , ncol
          level(icol) = -1

          k = 1
          found = .false.
          do while ( (.not. found) .and. (k.le.nlev_in))
                if ( p(k,icol) .lt. p(1,icol)-pconst ) then
                   level(icol) = k
                   found = .true.
                end if
                k = k+1
          end do

          if ( level(icol) .eq. -1 ) then
             slp(:) = 0.0
             return
          end if

       end do


       do icol = 1 , ncol

          klo = max ( level(icol) - 1 , 1      )
          khi = min ( klo + 1        , nlev_in - 1 )

          if ( klo .eq. khi ) then
             slp(:) = 0.0
             return
          end if

          plo = p(klo,icol)
          phi = p(khi,icol)
          tlo = t(klo,icol) * (1. + 0.608 * scalars(index_qv,klo,icol))
          thi = t(khi,icol) * (1. + 0.608 * scalars(index_qv,khi,icol))
          zlo = 0.5*(height(klo,icol)+height(klo+1,icol))
          zhi = 0.5*(height(khi,icol)+height(khi+1,icol))

          p_at_pconst = p(1,icol) - pconst
          t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi)
          z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi)

          t_surf(icol) = t_at_pconst*(p(1,icol)/p_at_pconst)**(gamma*rr/grav)
          t_msl(icol) = t_at_pconst+gamma*z_at_pconst

       end do


       if ( mm5_test ) then
          kcount = 0
          do icol = 1 , ncol
                l1 = t_msl(icol) .lt. tc
                l2 = t_surf(icol) .le. tc
                l3 = .not. l1
                if ( l2 .and. l3 ) then
                   t_msl(icol) = tc
                else
                   t_msl(icol) = tc - 0.005*(t_surf(icol)-tc)**2
                   kcount = kcount+1
                end if
          end do
       end if

       do icol = 1 , ncol
          z_half_lowest=0.5*(height(1,icol)+height(2,icol))
          slp(icol) = p(1,icol) * exp((2.*grav*z_half_lowest)/ &
                                    (rr*(t_msl(icol)+t_surf(icol))))
       end do

       if (allocated(level))  deallocate(level)
       if (allocated(t_surf)) deallocate(t_surf)
       if (allocated(t_msl))  deallocate(t_msl)

    end subroutine oracle_slp
    subroutine oracle_layer_mean(layerMean, p1, p2, field_in, press_in)

       implicit none

       real(kind=RKIND), dimension(:), intent(out) :: layerMean
       real(kind=RKIND), intent(in) :: p1, p2
       real(kind=RKIND), dimension(:,:), intent(in) :: field_in
       real(kind=RKIND), dimension(:,:), intent(in) :: press_in

       integer :: nCells, nVertLevels
       integer :: iCell, k
       integer :: k_bot, k_top
       real(kind=RKIND) :: p_bot, p_top
       real(kind=RKIND) :: wtop_p, wtop_m
       real(kind=RKIND) :: wbot_p, wbot_m
       real(kind=RKIND) :: wtotal, w
       real(kind=RKIND) :: temp


       nCells = size(field_in, 1)
       nVertLevels = size(field_in, 2)


       if (press_in(1,1) > press_in(1,nVertLevels)) then
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

    end subroutine oracle_layer_mean
 subroutine oracle_isobaric(area_in,kite_in,exner_in,height_in,pb_in,pp_in,rh_in,theta_in,vort_in,um_in,uz_in,w_in,q_in,cells_in,out,temperature_only)
 type(isobaric_out),target,intent(inout)::out
 real,target,intent(in)::area_in(:)
 real,target,intent(in)::kite_in(:,:)
 real,target,intent(in)::exner_in(:,:)
 real,target,intent(in)::height_in(:,:)
 real,target,intent(in)::pb_in(:,:)
 real,target,intent(in)::pp_in(:,:)
 real,target,intent(in)::rh_in(:,:)
 real,target,intent(in)::theta_in(:,:)
 real,target,intent(in)::vort_in(:,:)
 real,target,intent(in)::um_in(:,:)
 real,target,intent(in)::uz_in(:,:)
 real,target,intent(in)::w_in(:,:)
 real,target,intent(in)::q_in(:,:,:)
 integer,target,intent(in)::cells_in(:,:)
 logical,optional,intent(in)::temperature_only
 logical::need_dewpoint,need_height,need_meant_500_300,need_mslp,need_relhum,need_t_isobaric,need_temp,need_umeridional,need_uzonal,need_vorticity,need_w,need_z_isobaric
        integer :: iCell,iVert,iVertD,k,kk
        integer :: nCells, nCellsSolve, nVertLevels, nVertices, vertexDegree, nIsoLevelsT, nIsoLevelsZ
        integer :: nVertLevelsP1
        integer :: index_qv, num_scalars
        integer, dimension(:,:), pointer :: cellsOnVertex
        real (kind=RKIND), dimension(:), pointer :: areaTriangle
        real (kind=RKIND), dimension(:,:), pointer :: kiteAreasOnVertex
        real (kind=RKIND), dimension(:,:), pointer :: exner, height
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:), pointer :: relhum, theta_m, vorticity
        real (kind=RKIND), dimension(:,:), pointer :: umeridional, uzonal, vvel
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
        real (kind=RKIND), dimension(:), pointer :: t_iso_levels
        real (kind=RKIND), dimension(:), pointer :: z_iso_levels
        real (kind=RKIND), dimension(:,:), pointer :: t_isobaric
        real (kind=RKIND), dimension(:,:), pointer :: z_isobaric
        real (kind=RKIND), dimension(:), pointer :: meanT_500_300
        real (kind=RKIND), dimension(:), pointer :: temperature_50hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_100hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_200hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_250hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_500hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_700hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_850hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_925hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_50hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_100hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_200hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_250hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_500hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_700hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_850hPa
        real (kind=RKIND), dimension(:), pointer :: relhum_925hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_50hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_100hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_200hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_250hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_500hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_700hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_850hPa
        real (kind=RKIND), dimension(:), pointer :: dewpoint_925hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_50hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_100hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_200hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_250hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_500hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_700hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_850hPa
        real (kind=RKIND), dimension(:), pointer :: uzonal_925hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_50hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_100hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_200hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_250hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_500hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_700hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_850hPa
        real (kind=RKIND), dimension(:), pointer :: umeridional_925hPa
        real (kind=RKIND), dimension(:), pointer :: height_50hPa
        real (kind=RKIND), dimension(:), pointer :: height_100hPa
        real (kind=RKIND), dimension(:), pointer :: height_200hPa
        real (kind=RKIND), dimension(:), pointer :: height_250hPa
        real (kind=RKIND), dimension(:), pointer :: height_500hPa
        real (kind=RKIND), dimension(:), pointer :: height_700hPa
        real (kind=RKIND), dimension(:), pointer :: height_850hPa
        real (kind=RKIND), dimension(:), pointer :: height_925hPa
        real (kind=RKIND), dimension(:), pointer :: w_50hPa
        real (kind=RKIND), dimension(:), pointer :: w_100hPa
        real (kind=RKIND), dimension(:), pointer :: w_200hPa
        real (kind=RKIND), dimension(:), pointer :: w_250hPa
        real (kind=RKIND), dimension(:), pointer :: w_500hPa
        real (kind=RKIND), dimension(:), pointer :: w_700hPa
        real (kind=RKIND), dimension(:), pointer :: w_850hPa
        real (kind=RKIND), dimension(:), pointer :: w_925hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_50hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_100hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_200hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_250hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_500hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_700hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_850hPa
        real (kind=RKIND), dimension(:), pointer :: vorticity_925hPa
        real (kind=RKIND) :: evp
        real (kind=RKIND), dimension(:), pointer :: mslp
        real (kind=RKIND), dimension(:,:), allocatable :: pressure, pressureCp1, pressure2, pressure_v, temperature
        real (kind=RKIND), dimension(:,:), allocatable :: dewpoint
        integer :: nIntP
        real (kind=RKIND) :: w1,w2,z0,z1,z2
        real (kind=RKIND), dimension(:,:), allocatable :: field_in,press_in
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp,press_interp
 need_dewpoint=.true.
 need_height=.true.
 need_meant_500_300=.true.
 need_mslp=.true.
 need_relhum=.true.
 need_t_isobaric=.true.
 need_temp=.true.
 need_umeridional=.true.
 need_uzonal=.true.
 need_vorticity=.true.
 need_w=.true.
 need_z_isobaric=.true.
 if(present(temperature_only))then
  if(temperature_only)then
   need_dewpoint=.false.
   need_height=.false.
   need_meant_500_300=.false.
   need_mslp=.false.
   need_relhum=.false.
   need_t_isobaric=.false.
   need_temp=.false.
   need_umeridional=.false.
   need_uzonal=.false.
   need_vorticity=.false.
   need_w=.false.
   need_z_isobaric=.false.
   need_t_isobaric=.true.;need_meanT_500_300=.true.
  endif
 endif
 nCells=size(theta_in,2)-1
 nCellsSolve=nCells
 nVertLevels=size(theta_in,1)
 nVertLevelsP1=nVertLevels+1
 nVertices=size(area_in)
 vertexDegree=size(cells_in,1)
 nIsoLevelsT=5;nIsoLevelsZ=13
 index_qv=i_qv;num_scalars=size(q_in,1)
 areaTriangle=>area_in
 kiteAreasOnVertex=>kite_in
 exner=>exner_in
 height=>height_in
 pressure_b=>pb_in
 pressure_p=>pp_in
 relhum=>rh_in
 theta_m=>theta_in
 vorticity=>vort_in
 umeridional=>um_in
 uzonal=>uz_in
 vvel=>w_in
 scalars=>q_in
 cellsOnVertex=>cells_in
 temperature_50hPa=>out%cell(1,:,1)
 temperature_100hPa=>out%cell(2,:,1)
 temperature_200hPa=>out%cell(3,:,1)
 temperature_250hPa=>out%cell(4,:,1)
 temperature_500hPa=>out%cell(5,:,1)
 temperature_700hPa=>out%cell(6,:,1)
 temperature_850hPa=>out%cell(7,:,1)
 temperature_925hPa=>out%cell(8,:,1)
 relhum_50hPa=>out%cell(1,:,2)
 relhum_100hPa=>out%cell(2,:,2)
 relhum_200hPa=>out%cell(3,:,2)
 relhum_250hPa=>out%cell(4,:,2)
 relhum_500hPa=>out%cell(5,:,2)
 relhum_700hPa=>out%cell(6,:,2)
 relhum_850hPa=>out%cell(7,:,2)
 relhum_925hPa=>out%cell(8,:,2)
 dewpoint_50hPa=>out%cell(1,:,3)
 dewpoint_100hPa=>out%cell(2,:,3)
 dewpoint_200hPa=>out%cell(3,:,3)
 dewpoint_250hPa=>out%cell(4,:,3)
 dewpoint_500hPa=>out%cell(5,:,3)
 dewpoint_700hPa=>out%cell(6,:,3)
 dewpoint_850hPa=>out%cell(7,:,3)
 dewpoint_925hPa=>out%cell(8,:,3)
 uzonal_50hPa=>out%cell(1,:,4)
 uzonal_100hPa=>out%cell(2,:,4)
 uzonal_200hPa=>out%cell(3,:,4)
 uzonal_250hPa=>out%cell(4,:,4)
 uzonal_500hPa=>out%cell(5,:,4)
 uzonal_700hPa=>out%cell(6,:,4)
 uzonal_850hPa=>out%cell(7,:,4)
 uzonal_925hPa=>out%cell(8,:,4)
 umeridional_50hPa=>out%cell(1,:,5)
 umeridional_100hPa=>out%cell(2,:,5)
 umeridional_200hPa=>out%cell(3,:,5)
 umeridional_250hPa=>out%cell(4,:,5)
 umeridional_500hPa=>out%cell(5,:,5)
 umeridional_700hPa=>out%cell(6,:,5)
 umeridional_850hPa=>out%cell(7,:,5)
 umeridional_925hPa=>out%cell(8,:,5)
 height_50hPa=>out%cell(1,:,6)
 height_100hPa=>out%cell(2,:,6)
 height_200hPa=>out%cell(3,:,6)
 height_250hPa=>out%cell(4,:,6)
 height_500hPa=>out%cell(5,:,6)
 height_700hPa=>out%cell(6,:,6)
 height_850hPa=>out%cell(7,:,6)
 height_925hPa=>out%cell(8,:,6)
 w_50hPa=>out%cell(1,:,7)
 w_100hPa=>out%cell(2,:,7)
 w_200hPa=>out%cell(3,:,7)
 w_250hPa=>out%cell(4,:,7)
 w_500hPa=>out%cell(5,:,7)
 w_700hPa=>out%cell(6,:,7)
 w_850hPa=>out%cell(7,:,7)
 w_925hPa=>out%cell(8,:,7)
 vorticity_50hPa=>out%vorticity(1,:)
 vorticity_100hPa=>out%vorticity(2,:)
 vorticity_200hPa=>out%vorticity(3,:)
 vorticity_250hPa=>out%vorticity(4,:)
 vorticity_500hPa=>out%vorticity(5,:)
 vorticity_700hPa=>out%vorticity(6,:)
 vorticity_850hPa=>out%vorticity(7,:)
 vorticity_925hPa=>out%vorticity(8,:)
 mslp=>out%mslp
 t_iso_levels=>out%t_iso_levels
 z_iso_levels=>out%z_iso_levels
 t_isobaric=>out%t_isobaric
 z_isobaric=>out%z_isobaric
 meanT_500_300=>out%meanT
        if(.not.allocated(pressure)    ) allocate(pressure(nVertLevels,nCells)      )
        if(.not.allocated(pressureCp1) ) allocate(pressureCp1(nVertLevels,nCells+1) )
        if(.not.allocated(pressure2)   ) allocate(pressure2(nVertLevelsP1,nCells)   )
        if(.not.allocated(pressure_v)  ) allocate(pressure_v(nVertLevels,nVertices) )
        if(.not.allocated(temperature) ) allocate(temperature(nVertLevels,nCells)   )
        if(.not.allocated(dewpoint) ) allocate(dewpoint(nVertLevels,nCells)   )
        if (need_t_isobaric) then
            t_iso_levels(1) = 30000.0
            t_iso_levels(2) = 35000.0
            t_iso_levels(3) = 40000.0
            t_iso_levels(4) = 45000.0
            t_iso_levels(5) = 50000.0
        end if
        if (need_z_isobaric) then
            z_iso_levels(1)  = 30000.0
            z_iso_levels(2)  = 35000.0
            z_iso_levels(3)  = 40000.0
            z_iso_levels(4)  = 45000.0
            z_iso_levels(5)  = 50000.0
            z_iso_levels(6)  = 55000.0
            z_iso_levels(7)  = 60000.0
            z_iso_levels(8)  = 65000.0
            z_iso_levels(9)  = 70000.0
            z_iso_levels(10) = 75000.0
            z_iso_levels(11) = 80000.0
            z_iso_levels(12) = 85000.0
            z_iso_levels(13) = 90000.0
       end if
        do iCell = 1, nCells
        do k = 1, nVertLevels
           pressure(k,iCell)    = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
           pressureCp1(k,iCell) = pressure(k,iCell)
        enddo
        enddo
        do iCell = nCells+1, nCells+1
        do k = 1, nVertLevels
           pressureCp1(k,iCell)   = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
        enddo
        enddo
        k = nVertLevelsP1
        do iCell = 1, nCells
           z0 = height(k,iCell)
           z1 = 0.5*(height(k,iCell)+height(k-1,iCell))
           z2 = 0.5*(height(k-1,iCell)+height(k-2,iCell))
           w1 = (z0-z2)/(z1-z2)
           w2 = 1.-w1
           pressure2(k,iCell) = exp(w1*log(pressure(k-1,iCell))+w2*log(pressure(k-2,iCell)))
        enddo
        do k = 2, nVertLevels
        do iCell = 1, nCells
           w1 = (height(k,iCell)-height(k-1,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
           w2 = (height(k+1,iCell)-height(k,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
           pressure2(k,iCell) = exp(w1*log(pressure(k,iCell))+w2*log(pressure(k-1,iCell)))
        enddo
        enddo
        k = 1
        do iCell = 1, nCells
           z0 = height(k,iCell)
           z1 = 0.5*(height(k,iCell)+height(k+1,iCell))
           z2 = 0.5*(height(k+1,iCell)+height(k+2,iCell))
           w1 = (z0-z2)/(z1-z2)
           w2 = 1.-w1
           pressure2(k,iCell) = exp(w1*log(pressure(k,iCell))+w2*log(pressure(k+1,iCell)))
        enddo
        do iVert = 1, nVertices
           pressure_v(:,iVert) = 0._RKIND
           do k = 1, nVertLevels
           do iVertD = 1, vertexDegree
              pressure_v(k,iVert) = pressure_v(k,iVert) &
                      + kiteAreasOnVertex(iVertD,iVert)*pressureCp1(k,cellsOnVertex(iVertD,iVert))
           enddo
           pressure_v(k,iVert) = pressure_v(k,iVert) / areaTriangle(iVert)
           enddo
        enddo
        if (NEED_TEMP .or. NEED_RELHUM .or. NEED_DEWPOINT .or. need_mslp .or. need_t_isobaric .or. need_meanT_500_300) then
            do iCell = 1,nCells
            do k = 1,nVertLevels
                temperature(k,iCell) = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)
                evp = pressure(k,iCell) * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
                evp = max(evp, 1.0e-8_RKIND)
                dewpoint(k,iCell) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
                dewpoint(k,iCell) = dewpoint(k,iCell) + 273.15
            enddo
            enddo
        end if
        nIntP = 8
        if(.not.allocated(field_interp)) allocate(field_interp(nCells,nIntP) )
        if(.not.allocated(press_interp)) allocate(press_interp(nCells,nIntP) )
        do iCell = 1, nCells
           press_interp(iCell,1) = 50.0_RKIND
           press_interp(iCell,2) = 100.0_RKIND
           press_interp(iCell,3) = 200.0_RKIND
           press_interp(iCell,4) = 250.0_RKIND
           press_interp(iCell,5) = 500.0_RKIND
           press_interp(iCell,6) = 700.0_RKIND
           press_interp(iCell,7) = 850.0_RKIND
           press_interp(iCell,8) = 925.0_RKIND
        enddo
        if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevels))
        do iCell = 1, nCells
        do k = 1, nVertLevels
           kk = nVertLevels+1-k
           press_in(iCell,kk) = pressure(k,iCell)
        enddo
        enddo
        if(.not.allocated(field_in)) allocate(field_in(nCells,nVertLevels))
        if (NEED_TEMP) then
            do iCell = 1, nCells
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = temperature(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            temperature_50hPa(1:nCells) = field_interp(1:nCells,1)
            temperature_100hPa(1:nCells) = field_interp(1:nCells,2)
            temperature_200hPa(1:nCells) = field_interp(1:nCells,3)
            temperature_250hPa(1:nCells) = field_interp(1:nCells,4)
            temperature_500hPa(1:nCells) = field_interp(1:nCells,5)
            temperature_700hPa(1:nCells) = field_interp(1:nCells,6)
            temperature_850hPa(1:nCells) = field_interp(1:nCells,7)
            temperature_925hPa(1:nCells) = field_interp(1:nCells,8)
        end if
        if (NEED_RELHUM) then
            do iCell = 1, nCells
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = relhum(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            relhum_50hPa(1:nCells) = field_interp(1:nCells,1)
            relhum_100hPa(1:nCells) = field_interp(1:nCells,2)
            relhum_200hPa(1:nCells) = field_interp(1:nCells,3)
            relhum_250hPa(1:nCells) = field_interp(1:nCells,4)
            relhum_500hPa(1:nCells) = field_interp(1:nCells,5)
            relhum_700hPa(1:nCells) = field_interp(1:nCells,6)
            relhum_850hPa(1:nCells) = field_interp(1:nCells,7)
            relhum_925hPa(1:nCells) = field_interp(1:nCells,8)
        end if
        if (NEED_DEWPOINT) then
            do iCell = 1, nCells
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = dewpoint(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            dewpoint_50hPa(1:nCells) = field_interp(1:nCells,1)
            dewpoint_100hPa(1:nCells) = field_interp(1:nCells,2)
            dewpoint_200hPa(1:nCells) = field_interp(1:nCells,3)
            dewpoint_250hPa(1:nCells) = field_interp(1:nCells,4)
            dewpoint_500hPa(1:nCells) = field_interp(1:nCells,5)
            dewpoint_700hPa(1:nCells) = field_interp(1:nCells,6)
            dewpoint_850hPa(1:nCells) = field_interp(1:nCells,7)
            dewpoint_925hPa(1:nCells) = field_interp(1:nCells,8)
        end if
        if (NEED_UZONAL) then
            do iCell = 1, nCells
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = uzonal(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            uzonal_50hPa(1:nCells) = field_interp(1:nCells,1)
            uzonal_100hPa(1:nCells) = field_interp(1:nCells,2)
            uzonal_200hPa(1:nCells) = field_interp(1:nCells,3)
            uzonal_250hPa(1:nCells) = field_interp(1:nCells,4)
            uzonal_500hPa(1:nCells) = field_interp(1:nCells,5)
            uzonal_700hPa(1:nCells) = field_interp(1:nCells,6)
            uzonal_850hPa(1:nCells) = field_interp(1:nCells,7)
            uzonal_925hPa(1:nCells) = field_interp(1:nCells,8)
        end if
        if (NEED_UMERIDIONAL) then
            do iCell = 1, nCells
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = umeridional(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            umeridional_50hPa(1:nCells) = field_interp(1:nCells,1)
            umeridional_100hPa(1:nCells) = field_interp(1:nCells,2)
            umeridional_200hPa(1:nCells) = field_interp(1:nCells,3)
            umeridional_250hPa(1:nCells) = field_interp(1:nCells,4)
            umeridional_500hPa(1:nCells) = field_interp(1:nCells,5)
            umeridional_700hPa(1:nCells) = field_interp(1:nCells,6)
            umeridional_850hPa(1:nCells) = field_interp(1:nCells,7)
            umeridional_925hPa(1:nCells) = field_interp(1:nCells,8)
        end if
        if(allocated(field_in)) deallocate(field_in)
        if(allocated(press_in)) deallocate(press_in)
        if (NEED_W .or. NEED_HEIGHT) then
            if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevelsP1))
            do iCell = 1, nCells
            do k = 1, nVertLevelsP1
               kk = nVertLevelsP1+1-k
               press_in(iCell,kk) = pressure2(k,iCell)
            enddo
            enddo
            if(.not.allocated(field_in)) allocate(field_in(nCells,nVertLevelsP1))
            do iCell = 1, nCells
            do k = 1, nVertLevelsP1
               kk = nVertLevelsP1+1-k
               field_in(iCell,kk) = height(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevelsP1,nIntP,press_in,field_in,press_interp,field_interp)
            height_50hPa(1:nCells) = field_interp(1:nCells,1)
            height_100hPa(1:nCells) = field_interp(1:nCells,2)
            height_200hPa(1:nCells) = field_interp(1:nCells,3)
            height_250hPa(1:nCells) = field_interp(1:nCells,4)
            height_500hPa(1:nCells) = field_interp(1:nCells,5)
            height_700hPa(1:nCells) = field_interp(1:nCells,6)
            height_850hPa(1:nCells) = field_interp(1:nCells,7)
            height_925hPa(1:nCells) = field_interp(1:nCells,8)
            do iCell = 1, nCells
            do k = 1, nVertLevelsP1
               kk = nVertLevelsP1+1-k
               field_in(iCell,kk) = vvel(k,iCell)
            enddo
            enddo
            call oracle_interp(nCells,nVertLevelsP1,nIntP,press_in,field_in,press_interp,field_interp)
            w_50hPa(1:nCells) = field_interp(1:nCells,1)
            w_100hPa(1:nCells) = field_interp(1:nCells,2)
            w_200hPa(1:nCells) = field_interp(1:nCells,3)
            w_250hPa(1:nCells) = field_interp(1:nCells,4)
            w_500hPa(1:nCells) = field_interp(1:nCells,5)
            w_700hPa(1:nCells) = field_interp(1:nCells,6)
            w_850hPa(1:nCells) = field_interp(1:nCells,7)
            w_925hPa(1:nCells) = field_interp(1:nCells,8)
            if(allocated(field_in)) deallocate(field_in)
            if(allocated(press_in)) deallocate(press_in)
        end if
        if(allocated(field_interp)) deallocate(field_interp)
        if(allocated(press_interp)) deallocate(press_interp)
        if (NEED_VORTICITY) then
            nIntP = 8
            if(.not.allocated(field_interp)) allocate(field_interp(nVertices,nIntP) )
            if(.not.allocated(press_interp)) allocate(press_interp(nVertices,nIntP) )
            do iVert = 1, nVertices
               press_interp(iVert,1) = 50.0_RKIND
               press_interp(iVert,2) = 100.0_RKIND
               press_interp(iVert,3) = 200.0_RKIND
               press_interp(iVert,4) = 250.0_RKIND
               press_interp(iVert,5) = 500.0_RKIND
               press_interp(iVert,6) = 700.0_RKIND
               press_interp(iVert,7) = 850.0_RKIND
               press_interp(iVert,8) = 925.0_RKIND
            enddo
            if(.not.allocated(press_in)) allocate(press_in(nVertices,nVertLevels))
            do iVert = 1, nVertices
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               press_in(iVert,kk) = pressure_v(k,iVert)
            enddo
            enddo
            if(.not.allocated(field_in)) allocate(field_in(nVertices,nVertLevels))
            do iVert = 1, nVertices
            do k = 1, nVertLevels
               kk = nVertLevels+1-k
               field_in(iVert,kk) = vorticity(k,iVert)
            enddo
            enddo
            call oracle_interp(nVertices,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
            vorticity_50hPa(1:nVertices) = field_interp(1:nVertices,1)
            vorticity_100hPa(1:nVertices) = field_interp(1:nVertices,2)
            vorticity_200hPa(1:nVertices) = field_interp(1:nVertices,3)
            vorticity_250hPa(1:nVertices) = field_interp(1:nVertices,4)
            vorticity_500hPa(1:nVertices) = field_interp(1:nVertices,5)
            vorticity_700hPa(1:nVertices) = field_interp(1:nVertices,6)
            vorticity_850hPa(1:nVertices) = field_interp(1:nVertices,7)
            vorticity_925hPa(1:nVertices) = field_interp(1:nVertices,8)
            if(allocated(field_interp)) deallocate(field_interp)
            if(allocated(press_interp)) deallocate(press_interp)
            if(allocated(field_in    )) deallocate(field_in)
            if(allocated(press_in    )) deallocate(press_in)
        end if
        if(allocated(pressureCp1) ) deallocate(pressureCp1 )
        if(allocated(pressure_v)  ) deallocate(pressure_v  )
        if (need_mslp) then
             call oracle_slp(nCells, nVertLevels, num_scalars, temperature, height, pressure, index_qv, scalars, mslp)
             mslp(:) = mslp(:) * 100.0   ! Convert from hPa to Pa
        end if
        if (need_t_isobaric .or. need_meanT_500_300) then
            allocate(field_in(nCells, nVertLevels))
            allocate(press_in(nCells, nVertLevels))
            allocate(field_interp(nCells, nIsoLevelsT))
            allocate(press_interp(nCells, nIsoLevelsT))
            do k=1,nIsoLevelsT
               press_interp(:,k) = t_iso_levels(k)
            end do
            do iCell=1,nCells
            do k=1,nVertLevels
               kk = nVertLevels+1-k
               field_in(iCell,kk) = temperature(k,iCell)
            end do
            end do
            do iCell=1,nCells
            do k=1,nVertLevels
               kk = nVertLevels+1-k
               press_in(iCell,kk) = pressure(k,iCell) * 100.0
            end do
            end do
            if (need_t_isobaric) then
                call oracle_interp(nCells, nVertLevels, nIsoLevelsT, press_in, field_in, press_interp, field_interp)
                do k=1,nIsoLevelsT
                   t_isobaric(k,1:nCells) = field_interp(1:nCells,k)
                end do
            end if
            if (need_meanT_500_300) then
                call oracle_layer_mean(meanT_500_300, 50000.0_RKIND, 30000.0_RKIND, field_in, press_in)
            end if
            deallocate(field_in)
            deallocate(field_interp)
            deallocate(press_in)
            deallocate(press_interp)
        end if
        if (need_z_isobaric) then
            allocate(field_in(nCells, nVertLevelsP1))
            allocate(press_in(nCells, nVertLevelsP1))
            allocate(field_interp(nCells, nIsoLevelsZ))
            allocate(press_interp(nCells, nIsoLevelsZ))
            do k=1,nIsoLevelsZ
               press_interp(:,k) = z_iso_levels(k)
            end do
            do iCell=1,nCells
            do k=1,nVertLevelsP1
               kk = nVertLevelsP1+1-k
               field_in(iCell,kk) = height(k,iCell)
            end do
            end do
            do iCell=1,nCells
            do k=1,nVertLevelsP1
               kk = nVertLevelsP1+1-k
               press_in(iCell,kk) = pressure2(k,iCell) * 100.0
            end do
            end do
            call oracle_interp(nCells, nVertLevelsP1, nIsoLevelsZ, press_in, field_in, press_interp, field_interp)
            do k=1,nIsoLevelsZ
               z_isobaric(k,1:nCells) = field_interp(1:nCells,k)
            end do
            deallocate(field_in)
            deallocate(field_interp)
            deallocate(press_in)
            deallocate(press_interp)
        end if
        if(allocated(temperature) ) deallocate(temperature )
        if(allocated(pressure2)   ) deallocate(pressure2   )
        if(allocated(pressure)    ) deallocate(pressure    )
        if(allocated(dewpoint)    ) deallocate(dewpoint )
 end subroutine
      real (kind=RKIND) function oracle_column_height_value( column_values, z, z_interp, n )
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
      oracle_column_height_value = wz*column_values(kz) + wzp1*column_values(kz+1)
      end function oracle_column_height_value
      real (kind=RKIND) function oracle_integral_zstaggered( column_values, z, zbot, ztop, n, np1 )
      implicit none
      integer n, np1
      real (kind=RKIND) :: column_values(n), z(np1), zbot, ztop
      real (kind=RKIND) :: zb, zt
      integer :: k
      oracle_integral_zstaggered = 0.
      do k=1,n
        zb = max(z(k), zbot)
        zt = min(z(k+1), ztop)
        oracle_integral_zstaggered = oracle_integral_zstaggered + column_values(k)*max(0.,(zt-zb))
      end do
      end function oracle_integral_zstaggered
      real (kind=RKIND) function oracle_integral_zpoint( column_values, z, zbot, ztop, n )
      implicit none
      integer n
      real(kind=RKIND) :: column_values(n), z(n), zbot, ztop
      real(kind=RKIND) :: zb, zt, dz, zr_midpoint, midpoint_value
      integer :: k
      oracle_integral_zpoint = 0.
      do k=1,n-1
        zb = max(z(k), zbot)
        zt = min(z(k+1), ztop)
        dz = max(0.,zt-zb)
        zr_midpoint = (0.5*(zt+zb) - z(k))/(z(k+1)-z(k))
        midpoint_value = column_values(k) + (column_values(k+1)-column_values(k))*zr_midpoint
        oracle_integral_zpoint = oracle_integral_zpoint + dz*midpoint_value
      end do
      end function oracle_integral_zpoint
    subroutine oracle_getcape( nk , p_in , t_in , td_in, cape , cin )
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
    real (kind=RKIND), dimension(nk) :: p,t,td,pi,q,th,thv,z,pt,pb,pc,pn,ptv
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
        q(k) = oracle_getqvs(p(k),td(k))
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
      maxthe = oracle_getthe(p(1),t(1),td(1),q(1))
    ELSE
      maxthe = 0.0
      do k=1,nk
        if(p(k).ge.50000.0)then
          the = oracle_getthe(p(k),t(k),td(k),q(k))
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
      the = oracle_getthe(p2,t2,t2,qv2)
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
          qv2 = min( qt , fliq*oracle_getqvs(p2,t2) + fice*oracle_getqvi(p2,t2) )
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
      the = oracle_getthe(p2,t2,t2,qv2)
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
    end subroutine oracle_getcape
    real (kind=RKIND) function oracle_getqvs(p,t)
    implicit none
    real (kind=RKIND) :: p,t,es
    real (kind=RKIND), parameter :: eps = 287.04/461.5
    es = 611.2*exp(17.67*(t-273.15)/(t-29.65))
    oracle_getqvs = eps*es/(p-es)
    return
    end function oracle_getqvs
    real (kind=RKIND) function oracle_getqvi(p,t)
    implicit none
    real (kind=RKIND) :: p,t,es
    real (kind=RKIND), parameter :: eps = 287.04/461.5
    es = 611.2*exp(21.8745584*(t-273.15)/(t-7.66))
    oracle_getqvi = eps*es/(p-es)
    return
    end function oracle_getqvi
    real (kind=RKIND) function oracle_getthe(p,t,td,q)
    implicit none
    real (kind=RKIND) :: p,t,td,q
    real (kind=RKIND) :: tlcl
    if( (td-t).ge.-0.1 )then
      tlcl = t
    else
      tlcl = 56.0 + ( (td-56.0)**(-1) + 0.00125*log(t/td) )**(-1)
    endif
    oracle_getthe=t*( (100000.0/p)**(0.2854*(1.0-0.28*q)) )   &
            *exp( ((3376.0/tlcl)-2.54)*q*(1.0+0.81*q) )
    return
    end function oracle_getthe
 subroutine oracle_convective(theta_m,scalars,exner,pressure_p,pressure_base,height,uzonal,umeridional,out)
 type(convective_out),target,intent(inout)::out
 real,intent(in)::theta_m(:,:),scalars(:,:,:),exner(:,:),pressure_p(:,:),pressure_base(:,:),height(:,:),uzonal(:,:),umeridional(:,:)
 integer::iCell,k,nCells,nCellsSolve,nVertLevels,nVertLevelsP1
 integer,parameter::index_qv=i_qv
 real,pointer::cape(:)
 real,pointer::cin(:)
 real,pointer::srh_0_1km(:)
 real,pointer::srh_0_3km(:)
 real,pointer::uzonal_surface(:)
 real,pointer::uzonal_1km(:)
 real,pointer::uzonal_6km(:)
 real,pointer::umeridional_surface(:)
 real,pointer::umeridional_1km(:)
 real,pointer::umeridional_6km(:)
 real,pointer::temperature_surface(:)
 real,pointer::dewpoint_surface(:)
        real (kind=RKIND), parameter :: dev_motion = 7.5, z_bunker_bot = 0., z_bunker_top = 6000.
        real (kind=RKIND)            :: u_storm, v_storm, u_srh_bot, v_srh_bot, u_srh_top, v_srh_top
        real (kind=RKIND)            :: u_mean, v_mean, u_shear, v_shear, shear_magnitude
        real (kind=RKIND)            :: b_term, cape_out, cin_out
        real (kind=RKIND), dimension(:), allocatable :: dudz, dvdz, zp
        real (kind=RKIND), dimension(:), allocatable :: zrel, srh
        real (kind=RKIND), dimension(:), allocatable :: p_in, t_in, td_in
        real (kind=RKIND), dimension(:,:), allocatable :: temperature, dewpoint
        real (kind=RKIND) :: evp
        logical :: need_cape, need_cin, need_lcl, need_lfc, need_srh_01km, need_srh_03km, need_uzonal_sfc, need_uzonal_1km, &
                   need_uzonal_6km, need_umerid_sfc, need_umerid_1km, need_umerid_6km, need_tsfc, need_tdsfc
        logical :: need_any_diags
 nCells=size(out%values,2);nCellsSolve=nCells;nVertLevels=size(theta_m,1);nVertLevelsP1=nVertLevels+1
 need_any_diags=.true.
 need_cape=.true.
 need_cin=.true.
 need_lcl=.true.
 need_lfc=.true.
 need_srh_01km=.true.
 need_srh_03km=.true.
 need_tdsfc=.true.
 need_tsfc=.true.
 need_umerid_1km=.true.
 need_umerid_6km=.true.
 need_umerid_sfc=.true.
 need_uzonal_1km=.true.
 need_uzonal_6km=.true.
 need_uzonal_sfc=.true.
 cape=>out%values(1,:)
 cin=>out%values(2,:)
 srh_0_1km=>out%values(3,:)
 srh_0_3km=>out%values(4,:)
 uzonal_surface=>out%values(5,:)
 uzonal_1km=>out%values(6,:)
 uzonal_6km=>out%values(7,:)
 umeridional_surface=>out%values(8,:)
 umeridional_1km=>out%values(9,:)
 umeridional_6km=>out%values(10,:)
 temperature_surface=>out%values(11,:)
 dewpoint_surface=>out%values(12,:)
            allocate(temperature(nVertLevels,nCells))
            allocate(dewpoint(nVertLevels,nCells))
            allocate(dudz(nVertLevels))
            allocate(dvdz(nVertLevels))
            allocate(zp(nVertLevels))
            allocate(zrel(nVertLevels+1))
            allocate(srh(nVertLevels+1))
            allocate(p_in(nVertLevels))
            allocate(t_in(nVertLevels))
            allocate(td_in(nVertLevels))
            do iCell = 1,nCells
            do k = 1,nVertLevels
                temperature(k,iCell) = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)
                evp = 0.01_RKIND * (pressure_base(k,iCell) + pressure_p(k,iCell)) &
                                 * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
                evp = max(evp, 1.0e-8_RKIND)
                dewpoint(k,iCell) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
                dewpoint(k,iCell) = dewpoint(k,iCell) + 273.15
            enddo
            enddo
            do iCell=1,nCellsSolve
              zp(1:nVertLevels) = 0.5*(height(1:nVertLevels,iCell)+height(2:nVertlevels+1,iCell)) - height(1,iCell)
              zrel(1:nVertLevels+1) = height(1:nVertLevels+1,iCell) - height(1,iCell)
              uzonal_surface(iCell) = uzonal(1,iCell)
              umeridional_surface(iCell) = umeridional(1,iCell)
              temperature_surface(iCell) = temperature(1,iCell)
              dewpoint_surface(iCell) = dewpoint(1,iCell)
              if (need_uzonal_1km) then
                  uzonal_1km(iCell)      = oracle_column_height_value(uzonal(1:nVertLevels,iCell),      zp, 1000.0_RKIND, nVertLevels)
              end if
              if (need_umerid_1km) then
                  umeridional_1km(iCell) = oracle_column_height_value(umeridional(1:nVertLevels,iCell), zp, 1000.0_RKIND, nVertLevels)
              end if
              if (need_uzonal_6km) then
                  uzonal_6km(iCell)      = oracle_column_height_value(uzonal(1:nVertLevels,iCell),      zp, 6000.0_RKIND, nVertLevels)
              end if
              if (need_umerid_6km) then
                  umeridional_6km(iCell) = oracle_column_height_value(umeridional(1:nVertLevels,iCell), zp, 6000.0_RKIND, nVertLevels)
              end if
               if (need_srh_01km .or. need_srh_03km) then
                   u_srh_bot = uzonal(1,iCell)
                   v_srh_bot = umeridional(1,iCell)
                   if(z_bunker_bot .gt. zp(1)) then
                     u_srh_bot = oracle_column_height_value( uzonal(1:nVertLevels,iCell), zp, z_bunker_bot, nVertLevels)
                     v_srh_bot = oracle_column_height_value( umeridional(1:nVertLevels,iCell), zp, z_bunker_bot, nVertLevels)
                   end if
                   u_srh_top = oracle_column_height_value( uzonal(1:nVertLevels,iCell), zp, z_bunker_top, nVertLevels)
                   v_srh_top = oracle_column_height_value( umeridional(1:nVertLevels,iCell), zp, z_bunker_top, nVertLevels)
                   u_shear = u_srh_top - u_srh_bot
                   v_shear = v_srh_top - v_srh_bot
                   u_mean = oracle_integral_zstaggered(uzonal(1:nVertLevels,iCell),zrel,z_bunker_bot,z_bunker_top,nVertLevels,nVertLevelsP1)/(z_bunker_top-z_bunker_bot)
                   v_mean = oracle_integral_zstaggered(umeridional(1:nVertLevels,iCell),zrel,z_bunker_bot,z_bunker_top,nVertLevels,nVertLevelsP1)/(z_bunker_top-z_bunker_bot)
                   shear_magnitude = max(0.0001,sqrt(u_shear**2 + v_shear**2))
                   u_storm = u_mean + dev_motion * v_shear/shear_magnitude
                   v_storm = v_mean - dev_motion * u_shear/shear_magnitude
                   do k=2, nVertLevels-1
                       dudz(k) = (uzonal(k,iCell)     -uzonal(k-1,iCell)     )/(0.5*(height(k+1,iCell)-height(k-1,iCell)))
                       dvdz(k) = (umeridional(k,iCell)-umeridional(k-1,iCell))/(0.5*(height(k+1,iCell)-height(k-1,iCell)))
                   end do
                   dudz(1) = dudz(2)
                   dvdz(1) = dvdz(2)
                   dudz(nVertLevels) = dudz(nVertLevels-1)
                   dvdz(nVertLevels) = dvdz(nVertLevels-1)
                   do k=2,nVertLevels
                     srh(k) = - (0.5*(uzonal(k,iCell)      + uzonal(k-1,iCell)      )-u_storm)*dvdz(k) &
                              + (0.5*(umeridional(k,iCell) + umeridional(k-1,iCell) )-v_storm)*dudz(k)
                   end do
                   srh(1) = - (uzonal(1,iCell)     - u_storm)*dvdz(1) &
                            + (umeridional(1,iCell)- v_storm)*dudz(1)
                   srh(nVertLevelsP1) = srh(nVertLevels)
                   do k=1, nVertLevels+1
                     srh(k) = max(0.,srh(k))  ! discounting negative SRH
                   end do
                   if (need_srh_01km) then
                       srh_0_1km(iCell) = oracle_integral_zpoint(srh, zrel, 0.0_RKIND, 1000.0_RKIND, nVertLevelsP1)
                   end if
                   if (need_srh_03km) then
                       srh_0_3km(iCell) = oracle_integral_zpoint(srh, zrel, 0.0_RKIND, 3000.0_RKIND, nVertLevelsP1)
                   end if
               end if
            end do
            if (need_cape .or. need_cin) then
                do iCell=1, nCellsSolve
                    p_in(1:nVertLevels) = (pressure_p(1:nVertLevels,iCell) + pressure_base(1:nVertLevels,iCell)) / 100.0_RKIND
                    t_in(1:nVertLevels) = temperature(1:nVertLevels,iCell) - 273.15_RKIND
                    td_in(1:nVertLevels) = dewpoint(1:nVertLevels,iCell) - 273.15_RKIND
                    call oracle_getcape( nVertLevels, p_in, t_in, td_in, cape_out, cin_out )
                    cape(iCell) = cape_out
                    cin(iCell) = cin_out
                end do
            end if
            deallocate(temperature)
            deallocate(dewpoint)
            deallocate(dudz)
            deallocate(dvdz)
            deallocate(zp)
            deallocate(zrel)
            deallocate(srh)
            deallocate(p_in)
            deallocate(t_in)
            deallocate(td_in)
 end subroutine
end module
