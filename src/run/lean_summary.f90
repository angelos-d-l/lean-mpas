! Serial MPAS extrema: core_atmosphere/dynamics/mpas_atm_time_integration.F:8509-8931.
! The loop bounds, min/max argument order, tie offsets and OpenACC reductions follow MPAS verbatim.
module lean_summary
 use mpas_kind_types, only: RKIND
 use lean_messages, only: int_text, real_text
 use lean_log, only: log_write, log_fatal
 use ieee_arithmetic, only: ieee_is_nan
 implicit none
 private
 public :: summarize_timestep
contains
 subroutine summarize_timestep(print_vel, print_detailed, print_sca, w, u, v, scalars, &
                               lat_cell, lon_cell, lat_edge, lon_edge, ncells, nedges, nz)
 logical, intent(in) :: print_vel, print_detailed, print_sca
 real(kind=RKIND), intent(in) :: w(:,:), u(:,:), v(:,:), scalars(:,:,:)
 real(kind=RKIND), intent(in) :: lat_cell(:), lon_cell(:), lat_edge(:), lon_edge(:)
 integer, intent(in) :: ncells, nedges, nz
 real(kind=RKIND), parameter :: pi_const = 2.0_RKIND*asin(1.0_RKIND)
 real(kind=RKIND) :: scalar_min, scalar_max, latMax, lonMax
 real(kind=RKIND), allocatable :: spd(:,:)
 !$acc declare create(spd)
 integer :: iCell, iEdge, k, iScalar, offset_1d, indexMax, kMax
 logical :: found_NaN

 ! MPAS :8615-8618,8664-8667,8713-8716,8762-8765,8812-8815: guard the unset location.
 ! With no match (e.g. all NaNs), huge(1) is a sentinel, not an array index; see docs/fixes.md.
 if (print_detailed) then
    call log_write('')

    allocate(spd(nz,nedges))

    scalar_min = 1.0e20
    offset_1d = huge(1)
    indexMax = -1
    kMax = -1
    latMax = 0.0
    lonMax = 0.0
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:scalar_min)
    do iCell = 1, ncells
    do k = 1, nz
       scalar_min = min(scalar_min, w(k,iCell))
    end do
    end do
    !$acc end parallel

    ! This second loop using offset_1d ensures the same (kMax,indexMax) are reported with the scalar_min
    ! Especially when using OpenACC
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:offset_1d)
    do iCell = 1, ncells
    do k = 1, nz
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
       latMax = lat_cell(indexMax)
       lonMax = lon_cell(indexMax)
    end if
    latMax = latMax * 180.0_RKIND / pi_const
    lonMax = lonMax * 180.0_RKIND / pi_const
    if (lonMax > 180.0) then
       lonMax = lonMax - 360.0
    end if
    call log_write(' global min w: '//real_text(scalar_min)//' k='//int_text(kMax)// &
                   ', '//real_text(latMax)//' lat, '//real_text(lonMax)//' lon')

    scalar_max = -1.0e20
    offset_1d = huge(1)
    indexMax = -1
    kMax = -1
    latMax = 0.0
    lonMax = 0.0
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(max:scalar_max)
    do iCell = 1, ncells
    do k = 1, nz
       scalar_max = max(scalar_max, w(k,iCell))
    end do
    end do
    !$acc end parallel

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:offset_1d)
    do iCell = 1, ncells
    do k = 1, nz
       if (w(k,iCell) == scalar_max) then
          offset_1d = min(offset_1d, (k-1) + size(w,1)*(iCell-1))
       end if
    end do
    end do
    !$acc end parallel
    if (offset_1d /= huge(1)) then
       kMax = mod(offset_1d, size(w,1)) + 1
       indexMax = (offset_1d / size(w,1)) + 1
       latMax = lat_cell(indexMax)
       lonMax = lon_cell(indexMax)
    end if
    latMax = latMax * 180.0_RKIND / pi_const
    lonMax = lonMax * 180.0_RKIND / pi_const
    if (lonMax > 180.0) then
       lonMax = lonMax - 360.0
    end if
    call log_write(' global max w: '//real_text(scalar_max)//' k='//int_text(kMax)// &
                   ', '//real_text(latMax)//' lat, '//real_text(lonMax)//' lon')

    scalar_min = 1.0e20
    offset_1d = huge(1)
    indexMax = -1
    kMax = -1
    latMax = 0.0
    lonMax = 0.0
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:scalar_min)
    do iEdge = 1, nedges
    do k = 1, nz
       scalar_min = min(scalar_min, u(k,iEdge))
    end do
    end do
    !$acc end parallel

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:offset_1d)
    do iEdge = 1, nedges
    do k = 1, nz
       if (u(k,iEdge) == scalar_min) then
          offset_1d = min(offset_1d, (k-1) + size(u,1)*(iEdge-1))
       end if
    end do
    end do
    !$acc end parallel
    if (offset_1d /= huge(1)) then
       kMax = mod(offset_1d, size(u,1)) + 1
       indexMax = (offset_1d / size(u,1)) + 1
       latMax = lat_edge(indexMax)
       lonMax = lon_edge(indexMax)
    end if
    latMax = latMax * 180.0_RKIND / pi_const
    lonMax = lonMax * 180.0_RKIND / pi_const
    if (lonMax > 180.0) then
       lonMax = lonMax - 360.0
    end if
    call log_write(' global min u: '//real_text(scalar_min)//' k='//int_text(kMax)// &
                   ', '//real_text(latMax)//' lat, '//real_text(lonMax)//' lon')

    scalar_max = -1.0e20
    offset_1d = huge(1)
    indexMax = -1
    kMax = -1
    latMax = 0.0
    lonMax = 0.0
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(max:scalar_max)
    do iEdge = 1, nedges
    do k = 1, nz
       scalar_max = max(scalar_max, u(k,iEdge))
    end do
    end do
    !$acc end parallel

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:offset_1d)
    do iEdge = 1, nedges
    do k = 1, nz
       if (u(k,iEdge) == scalar_max) then
          offset_1d = min(offset_1d, (k-1) + size(u,1)*(iEdge-1))
       end if
    end do
    end do
    !$acc end parallel
    if (offset_1d /= huge(1)) then
       kMax = mod(offset_1d, size(u,1)) + 1
       indexMax = (offset_1d / size(u,1)) + 1
       latMax = lat_edge(indexMax)
       lonMax = lon_edge(indexMax)
    end if
    latMax = latMax * 180.0_RKIND / pi_const
    lonMax = lonMax * 180.0_RKIND / pi_const
    if (lonMax > 180.0) then
       lonMax = lonMax - 360.0
    end if
    call log_write(' global max u: '//real_text(scalar_max)//' k='//int_text(kMax)// &
                   ', '//real_text(latMax)//' lat, '//real_text(lonMax)//' lon')

    scalar_max = -1.0e20
    offset_1d = huge(1)
    indexMax = -1
    kMax = -1
    latMax = 0.0
    lonMax = 0.0
    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(max:scalar_max)
    do iEdge = 1, nedges
    do k = 1, nz
       spd(k,iEdge) = sqrt(u(k,iEdge)*u(k,iEdge) + v(k,iEdge)*v(k,iEdge))
       scalar_max = max(scalar_max, spd(k,iEdge))
    end do
    end do
    !$acc end parallel

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(min:offset_1d)
    do iEdge = 1, nedges
    do k = 1, nz
       if (spd(k,iEdge) == scalar_max) then
          offset_1d = min(offset_1d, (k-1) + size(spd,1)*(iEdge-1))
       end if
    end do
    end do
    !$acc end parallel
    if (offset_1d /= huge(1)) then
       kMax = mod(offset_1d, size(spd,1)) + 1
       indexMax = (offset_1d / size(spd,1)) + 1
       latMax = lat_edge(indexMax)
       lonMax = lon_edge(indexMax)
    end if
    latMax = latMax * 180.0_RKIND / pi_const
    lonMax = lonMax * 180.0_RKIND / pi_const
    if (lonMax > 180.0) then
       lonMax = lonMax - 360.0
    end if
    call log_write(' global max wsp: '//real_text(scalar_max)//' k='//int_text(kMax)// &
                   ', '//real_text(latMax)//' lat, '//real_text(lonMax)//' lon')

    !
    ! Check for NaNs
    !
    found_NaN = .false.

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(.or.:found_NaN)
    do iCell = 1, ncells
    do k = 1, nz
       if (ieee_is_nan(w(k,iCell))) then
          found_NaN = .true.
       end if
    end do
    end do
    !$acc end parallel
    if (found_NaN) then
       call log_fatal("NaN detected in 'w' field.")
    end if

    found_NaN = .false.

    !$acc parallel default(present)
    !$acc loop collapse(2) gang vector reduction(.or.:found_NaN)
    do iEdge = 1, nedges
    do k = 1, nz
       if (ieee_is_nan(u(k,iEdge))) then
          found_NaN = .true.
       end if
    end do
    end do
    !$acc end parallel
    if (found_NaN) then
       call log_fatal("NaN detected in 'u' field.")
    end if

    deallocate(spd)

 else if (print_vel) then
    call log_write('')

    scalar_min = 0.0
    scalar_max = 0.0
    !$acc parallel default(present)
    !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
    do iCell = 1, ncells
    do k = 1, nz
       scalar_min = min(scalar_min, w(k,iCell))
       scalar_max = max(scalar_max, w(k,iCell))
    end do
    end do
    !$acc end parallel
    call log_write('global min, max w '//real_text(scalar_min)//' '//real_text(scalar_max))

    scalar_min = 0.0
    scalar_max = 0.0
    !$acc parallel default(present)
    !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
    do iEdge = 1, nedges
    do k = 1, nz
       scalar_min = min(scalar_min, u(k,iEdge))
       scalar_max = max(scalar_max, u(k,iEdge))
    end do
    end do
    !$acc end parallel
    call log_write('global min, max u '//real_text(scalar_min)//' '//real_text(scalar_max))
 end if

 if (print_sca) then
    if (.not. (print_vel .or. print_detailed)) then
       call log_write('')
    end if

    do iScalar = 1, size(scalars,1)
       scalar_min = 0.0
       scalar_max = 0.0
       !$acc parallel default(present)
       !$acc loop gang vector collapse(2) reduction(min:scalar_min) reduction(max:scalar_max)
       do iCell = 1, ncells
       do k = 1, nz
          scalar_min = min(scalar_min, scalars(iScalar,k,iCell))
          scalar_max = max(scalar_max, scalars(iScalar,k,iCell))
       end do
       end do
       !$acc end parallel
       call log_write(' global min, max scalar '//int_text(iScalar)//' '// &
                      real_text(scalar_min)//' '//real_text(scalar_max))
    end do

 end if

 end subroutine
end module
