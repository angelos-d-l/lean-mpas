! One Thompson microphysics step on a column view, with MPAS's conversions around mp_thompson and its effective radii; the
! tendencies of the step are left in the view's t_* fields.
module physics
 use micro_pool, only: micro_pool_allocate
 use config
 use model_state
 use lean_thompson, only: mp_thompson, calc_effectRad, naIN1
 implicit none
contains
 subroutine advance_column(a)
 type(column_view), intent(in) :: a
 real,pointer,contiguous :: col_qv(:)
 real,pointer,contiguous :: col_qc(:)
 real,pointer,contiguous :: col_qi(:)
 real,pointer,contiguous :: col_qr(:)
 real,pointer,contiguous :: col_qs(:)
 real,pointer,contiguous :: col_qg(:)
 real,pointer,contiguous :: col_ni(:)
 real,pointer,contiguous :: col_nr(:)
 real,pointer,contiguous :: col_p(:)
 real,pointer,contiguous :: col_w(:)
 real,pointer,contiguous :: col_dz(:)
 real,pointer,contiguous :: col_rainprod(:)
 real,pointer,contiguous :: col_evapprod(:)
 real,pointer,contiguous :: col_re_cloud(:)
 real,pointer,contiguous :: col_re_ice(:)
 real,pointer,contiguous :: col_re_snow(:)
 real :: t(nz),rho(nz),nc(nz),nwfa(nz),nifa(nz),pr,ps,pg,pi
 integer :: k
 ! Local contiguous aliases prevent gfortran packing guards on pointer components.
 col_qv=>a%qv
 col_qc=>a%qc
 col_qi=>a%qi
 col_qr=>a%qr
 col_qs=>a%qs
 col_qg=>a%qg
 col_ni=>a%ni
 col_nr=>a%nr
 col_p=>a%p
 col_w=>a%w
 col_dz=>a%dz
 col_rainprod=>a%rainprod
 col_evapprod=>a%evapprod
 col_re_cloud=>a%re_cloud
 col_re_ice=>a%re_ice
 col_re_snow=>a%re_snow
 ! The tendency fields hold the old values until the end, where they become (new - old)/dt.
 a%t_qv=a%qv
 a%t_qc=a%qc
 a%t_qi=a%qi
 a%t_qr=a%qr
 a%t_qs=a%qs
 a%t_qg=a%qg
 a%t_ni=a%ni
 a%t_nr=a%nr
 a%t_theta=a%theta
 do k=1,nz
  t(k)=a%theta(k)*a%exner(k)
  rho(k)=0.622*a%p(k)/(287.04*t(k)*(a%qv(k)+0.622))
  nc(k)=a%nt/rho(k)
  nwfa(k)=11.1E6/rho(k)
  nifa(k)=naIN1*0.01/rho(k)
 enddo
 pr=0.; ps=0.; pg=0.; pi=0.
 a%rainprod=0.
 a%evapprod=0.
 call micro_pool_allocate(128)   ! a card build's mp_thompson keeps its work arrays there (no-op elsewhere)
 call mp_thompson(col_qv,col_qc,col_qi,col_qr,col_qs,col_qg, &
  col_ni,col_nr,nc,nwfa,nifa,t,col_p,col_w,col_dz,pr,ps,pg,pi, &
  col_rainprod,col_evapprod,dt,1,1,a%nt)
 a%rainncv=pr+ps+pg+pi
 a%rainnc=a%rainnc+pr+ps+pg+pi
 a%snowncv=ps+pi
 a%snownc=a%snownc+ps+pi
 a%graupelncv=pg
 a%graupelnc=a%graupelnc+pg
 a%sr=(ps+pg+pi)/(a%rainncv+1.e-12)
 do k=1,nz
  a%theta(k)=t(k)/a%exner(k)
  if(a%qv(k)<0.) then
   if(k<nz-2.and.k>2) then
    a%qv(k)=max(1.e-7,0.5*(a%qv(k-1)+a%qv(k+1)))
   else
    a%qv(k)=1.e-7
   endif
  endif
 enddo
 a%re_cloud=2.49e-6; a%re_ice=4.99e-6; a%re_snow=9.99e-6
 call calc_effectRad(t,col_p,col_qv,col_qc,nc,col_qi,col_ni,col_qs, &
  col_re_cloud,col_re_ice,col_re_snow,a%nt)
 a%re_cloud=max(2.49e-6,min(a%re_cloud,50.e-6))
 a%re_ice=max(4.99e-6,min(a%re_ice,125.e-6))
 a%re_snow=max(9.99e-6,min(a%re_snow,999.e-6))
 a%t_qv=(a%qv-a%t_qv)/dt
 a%t_qc=(a%qc-a%t_qc)/dt
 a%t_qi=(a%qi-a%t_qi)/dt
 a%t_qr=(a%qr-a%t_qr)/dt
 a%t_qs=(a%qs-a%t_qs)/dt
 a%t_qg=(a%qg-a%t_qg)/dt
 a%t_ni=(a%ni-a%t_ni)/dt
 a%t_nr=(a%nr-a%t_nr)/dt
 a%t_theta=(a%theta-a%t_theta)/dt
 end subroutine
end module
