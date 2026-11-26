   subroutine summarize_timestep(domain)

       use ieee_arithmetic, only : ieee_is_nan

       implicit none

       type (domain_type), intent(inout) :: domain

       real (kind=RKIND), parameter :: pi_const = 2.0_RKIND*asin(1.0_RKIND)

       logical, pointer :: config_print_global_minmax_vel
       logical, pointer :: config_print_detailed_minmax_vel
       logical, pointer :: config_print_global_minmax_sca

       integer :: iCell, k, iEdge, iScalar
       integer, pointer :: num_scalars_ptr, nCellsSolve_ptr, nEdgesSolve_ptr, nVertLevels_ptr
       integer :: num_scalars, nCellsSolve, nEdgesSolve, nVertLevels

       type (mpas_pool_type), pointer :: state
       type (mpas_pool_type), pointer :: diag
       type (mpas_pool_type), pointer :: mesh

       real (kind=RKIND) :: scalar_min, scalar_max
       real (kind=RKIND) :: global_scalar_min, global_scalar_max

       real (kind=RKIND), dimension(:), pointer :: latCell
       real (kind=RKIND), dimension(:), pointer :: lonCell
       real (kind=RKIND), dimension(:), pointer :: latEdge
       real (kind=RKIND), dimension(:), pointer :: lonEdge
       integer, dimension(:), pointer :: indexToCellID
       integer :: indexMax, indexMax_global
       integer :: kMax, kMax_global
       integer :: offset_1d ! Offset into a multi-dimensional array, as if it were a contiguous 1-d array
       real (kind=RKIND) :: latMax, latMax_global
       real (kind=RKIND) :: lonMax, lonMax_global
       real (kind=RKIND), dimension(5) :: localVals, globalVals

       real (kind=RKIND), dimension(:,:), allocatable :: spd
       !$acc declare create(spd)
       real (kind=RKIND), dimension(:,:), pointer :: w
       real (kind=RKIND), dimension(:,:), pointer :: u, v, uReconstructZonal, uReconstructMeridional, uReconstructX, uReconstructY, uReconstructZ
       real (kind=RKIND), dimension(:,:,:), pointer :: scalars, scalars_1, scalars_2

       logical :: found_NaN

       call mpas_pool_get_config(block % configs, 'config_print_global_minmax_vel', config_print_global_minmax_vel)
       call mpas_pool_get_config(block % configs, 'config_print_detailed_minmax_vel', config_print_detailed_minmax_vel)
       call mpas_pool_get_config(block % configs, 'config_print_global_minmax_sca', config_print_global_minmax_sca)

      call mpas_pool_get_subpool(block % structs, 'state', state)
      call mpas_pool_get_subpool(block % structs, 'diag', diag)
      call mpas_pool_get_array(state, 'w', w, 2)
      call mpas_pool_get_array(state, 'u', u, 2)
      call mpas_pool_get_array(diag, 'v', v)
      call mpas_pool_get_array(state, 'scalars', scalars, 2)
      call mpas_pool_get_dimension(state, 'nCellsSolve', nCellsSolve_ptr)
      call mpas_pool_get_dimension(state, 'nEdgesSolve', nEdgesSolve_ptr)
      call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels_ptr)
      call mpas_pool_get_dimension(state, 'num_scalars', num_scalars_ptr)
      nCellsSolve = nCellsSolve_ptr
      nEdgesSolve = nEdgesSolve_ptr
      nVertLevels = nVertLevels_ptr
      num_scalars = num_scalars_ptr


      if (config_print_detailed_minmax_vel) then
         call mpas_log_write('')

         call mpas_pool_get_subpool(block % structs, 'mesh', mesh)

         call mpas_pool_get_array(mesh, 'indexToCellID', indexToCellID)
         call mpas_pool_get_array(mesh, 'latCell', latCell)
         call mpas_pool_get_array(mesh, 'lonCell', lonCell)
         call mpas_pool_get_array(mesh, 'latEdge', latEdge)
         call mpas_pool_get_array(mesh, 'lonEdge', lonEdge)

         allocate(spd(nVertLevels,nEdgesSolve))

         scalar_min = 1.0e20
         offset_1d = huge(1)
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:scalar_min)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, w(k,iCell))
         end do
         end do
         !$acc end parallel

         ! This second loop using offset_1d ensures the same (kMax,indexMax) are reported with the scalar_min
         ! Especially when using OpenACC
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:offset_1d)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (w(k,iCell) == scalar_min) then
               ! In case 2 locations tie, only save the minimum value
               offset_1d = min(offset_1d, (k-1) + size(w,1)*(iCell-1))
            end if
         end do
         end do
         !$acc end parallel
         if (offset_1d /= huge(1)) then
            kMax = mod(offset_1d, size(w,1)) + 1
            indexMax = (offset_1d / size(w,1)) + 1 ! Integer divide
            latMax = latCell(indexMax)
            lonMax = lonCell(indexMax)
         end if
         localVals(1) = scalar_min
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_minattributes_real(domain % dminfo, scalar_min, localVals, globalVals)
         global_scalar_min = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global min w: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_min, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         offset_1d = huge(1)
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(max:scalar_max)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            scalar_max = max(scalar_max, w(k,iCell))
         end do
         end do
         !$acc end parallel

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:offset_1d)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (w(k,iCell) == scalar_max) then
               offset_1d = min(offset_1d, (k-1) + size(w,1)*(iCell-1))
            end if
         end do
         end do
         !$acc end parallel
         if (offset_1d /= huge(1)) then
            kMax = mod(offset_1d, size(w,1)) + 1
            indexMax = (offset_1d / size(w,1)) + 1
            latMax = latCell(indexMax)
            lonMax = lonCell(indexMax)
         end if
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max w: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         scalar_min = 1.0e20
         offset_1d = huge(1)
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:scalar_min)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, u(k,iEdge))
         end do
         end do
         !$acc end parallel

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:offset_1d)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (u(k,iEdge) == scalar_min) then
               offset_1d = min(offset_1d, (k-1) + size(u,1)*(iEdge-1))
            end if
         end do
         end do
         !$acc end parallel
         if (offset_1d /= huge(1)) then
            kMax = mod(offset_1d, size(u,1)) + 1
            indexMax = (offset_1d / size(u,1)) + 1
            latMax = latEdge(indexMax)
            lonMax = lonEdge(indexMax)
         end if
         localVals(1) = scalar_min
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_minattributes_real(domain % dminfo, scalar_min, localVals, globalVals)
         global_scalar_min = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global min u: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_min, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         offset_1d = huge(1)
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(max:scalar_max)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            scalar_max = max(scalar_max, u(k,iEdge))
         end do
         end do
         !$acc end parallel

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:offset_1d)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (u(k,iEdge) == scalar_max) then
               offset_1d = min(offset_1d, (k-1) + size(u,1)*(iEdge-1))
            end if
         end do
         end do
         !$acc end parallel
         if (offset_1d /= huge(1)) then
            kMax = mod(offset_1d, size(u,1)) + 1
            indexMax = (offset_1d / size(u,1)) + 1
            latMax = latEdge(indexMax)
            lonMax = lonEdge(indexMax)
         end if
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max u: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         offset_1d = huge(1)
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(max:scalar_max)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            spd(k,iEdge) = sqrt(u(k,iEdge)*u(k,iEdge) + v(k,iEdge)*v(k,iEdge))
            scalar_max = max(scalar_max, spd(k,iEdge))
         end do
         end do
         !$acc end parallel

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(min:offset_1d)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (spd(k,iEdge) == scalar_max) then
               offset_1d = min(offset_1d, (k-1) + size(spd,1)*(iEdge-1))
            end if
         end do
         end do
         !$acc end parallel
         if (offset_1d /= huge(1)) then
            kMax = mod(offset_1d, size(spd,1)) + 1
            indexMax = (offset_1d / size(spd,1)) + 1
            latMax = latEdge(indexMax)
            lonMax = lonEdge(indexMax)
         end if
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max wsp: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         !
         ! Check for NaNs
         !
         found_NaN = .false.

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(.or.:found_NaN)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (ieee_is_nan(w(k,iCell))) then
               found_NaN = .true.
            end if
         end do
         end do
         !$acc end parallel
         if (found_NaN) then
            call mpas_log_write('NaN detected in ''w'' field.', messageType=MPAS_LOG_CRIT)
         end if

         found_NaN = .false.

         !$acc parallel default(present)
         !$acc loop collapse(2) gang vector reduction(.or.:found_NaN)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (ieee_is_nan(u(k,iEdge))) then
               found_NaN = .true.
            end if
         end do
         end do
         !$acc end parallel
         if (found_NaN) then
            call mpas_log_write('NaN detected in ''u'' field.', messageType=MPAS_LOG_CRIT)
         end if

         deallocate(spd)

      else if (config_print_global_minmax_vel) then
         call mpas_log_write('')

         scalar_min = 0.0
         scalar_max = 0.0
         !$acc parallel default(present)
         !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, w(k,iCell))
            scalar_max = max(scalar_max, w(k,iCell))
         end do
         end do
         !$acc end parallel
         call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
         call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
         call mpas_log_write('global min, max w $r $r', realArgs=(/global_scalar_min, global_scalar_max/))

         scalar_min = 0.0
         scalar_max = 0.0
         !$acc parallel default(present)
         !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, u(k,iEdge))
            scalar_max = max(scalar_max, u(k,iEdge))
         end do
         end do
         !$acc end parallel
         call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
         call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
         call mpas_log_write('global min, max u $r $r', realArgs=(/global_scalar_min, global_scalar_max/))
      end if

      if (config_print_global_minmax_sca) then
         if (.not. (config_print_global_minmax_vel .or. config_print_detailed_minmax_vel)) then
            call mpas_log_write('')
         end if

         do iScalar = 1, num_scalars
            scalar_min = 0.0
            scalar_max = 0.0
            !$acc parallel default(present)
            !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
            do iCell = 1, nCellsSolve
            do k = 1, nVertLevels
               scalar_min = min(scalar_min, scalars(iScalar,k,iCell))
               scalar_max = max(scalar_max, scalars(iScalar,k,iCell))
            end do
            end do
            !$acc end parallel
            call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
            call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
            call mpas_log_write(' global min, max scalar $i $r $r', intArgs=(/iScalar/), realArgs=(/global_scalar_min, global_scalar_max/))
         end do

      end if

   end subroutine summarize_timestep
