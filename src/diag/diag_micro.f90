! MPAS physics/mpas_atmphys_driver_microphysics.F:736-787,815-844.
module diag_micro
 use lean_thompson, only: calc_refl10cm
 use mpas_atmphys_functions, only: rslf,rsif
 use config, only: max_levels
 implicit none
 private
 public :: micro_diagnostics
contains
 subroutine micro_diagnostics(film_in,zgrid,refl10cm,refl10cm_max,refl10cm_1km,refl10cm_1km_max,relhum)
 real,intent(in)::film_in(:,:,:),zgrid(:,:)
 real,intent(out)::refl10cm(:,:),refl10cm_max(:),refl10cm_1km(:),relhum(:,:)
 real,intent(inout)::refl10cm_1km_max(:)
 real::qv(max_levels),qc(max_levels),qr(max_levels),nr(max_levels),qs(max_levels),qg(max_levels)
 real::t(max_levels),p(max_levels),dbz(max_levels),zp(max_levels),qvs,w1,w2
 integer::c,k,kp,nlev,ncol
 nlev=size(film_in,2);ncol=size(refl10cm_max)
 !$omp parallel do schedule(dynamic,64) private(k,kp,qv,qc,qr,nr,qs,qg,t,p,dbz,zp,qvs,w1,w2)
 do c=1,ncol
  do k=1,nlev
   qv(k)=film_in(1,k,c);qc(k)=film_in(2,k,c);qr(k)=film_in(3,k,c);nr(k)=film_in(4,k,c)
   qs(k)=film_in(5,k,c);qg(k)=film_in(6,k,c);t(k)=film_in(7,k,c);p(k)=film_in(8,k,c)
   dbz(k)=-35.
   zp(k)=zgrid(k,c)-zgrid(1,c)+0.5*(zgrid(k+1,c)-zgrid(k,c))
  enddo
  call calc_refl10cm(qv(:nlev),qc(:nlev),qr(:nlev),nr(:nlev),qs(:nlev),qg(:nlev), &
   t(:nlev),p(:nlev),dbz(:nlev),1,nlev,c,1)
  kp=1
  do k=1,nlev
   dbz(k)=max(-35.,dbz(k));refl10cm(k,c)=dbz(k)
   if(zp(k)<1000.)kp=k
   qvs=rslf(p(k),t(k))
   if(t(k)-273.16<=0.)qvs=rsif(p(k),t(k))
   relhum(k,c)=qv(k)/qvs
   relhum(k,c)=relhum(k,c)*100.
  enddo
  refl10cm_max(c)=maxval(dbz(:nlev))
  w1=(zp(kp+1)-1000.)/(zp(kp+1)-zp(kp));w2=1.-w1
  refl10cm_1km(c)=w1*dbz(kp)+w2*dbz(kp+1)
  refl10cm_1km_max(c)=max(refl10cm_1km_max(c),refl10cm_1km(c))
 enddo
 end subroutine
end module
