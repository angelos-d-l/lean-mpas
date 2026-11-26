! diag_isobaric against MPAS's routines (tests/diag/mpas_oracle.f90): interp_column bit for bit on 50 synthetic columns,
! and the failure paths of compute_slp and compute_layer_mean (every cell zero). Run by tests/diag/run.sh.
program isobaric_tests
 use lean_log,only:log_open
 use diag_isobaric
 use mpas_diag_oracle, only: oracle_interp
 implicit none
 real::p(50,6),f(50,6),po(50,8),fo(50,8),got(8),slp(2),t(4,2),z(5,2),ps(4,2),q(1,4,2),mean(2)
 integer::c,k,failed,st
 character(:),allocatable::text
 call log_open(st,text)
 if(st/=0)stop 2
 do c=1,50
  do k=1,6
   p(c,k)=real(k*100+c);f(c,k)=sin(real(c*k))
  enddo
  po(c,:)=[10.,p(c,1),150.,250.,350.,450.,550.,800.]
 enddo
 call oracle_interp(50,6,8,p,f,po,fo)
 do c=1,50
  call interp_column(6,8,p(c,:),f(c,:),po(c,:),got)
  call require(all(transfer(got,[0],8)==transfer(fo(c,:),[0],8)),'interp_column bits')
 enddo
 t=280.;z=0.;ps=1000.;q=0.
 ps(:,2)=[1000.,950.,850.,700.]
 call compute_slp(2,4,1,t,z,ps,1,q,slp,failed)
 call require(failed==1.and.all(slp==0.),'slp_failure all cells zero')
 p(1,1)=1000.;p(1,6)=10.
 call compute_layer_mean(mean,500.,300.,f(:2,:),p(:2,:),failed)
 call require(failed==1.and.all(mean==0.),'layer_mean_order all cells zero')
 print '(a)','ISOBARIC UNITS OK'
contains
 subroutine require(ok,label)
 logical,intent(in)::ok
 character(*),intent(in)::label
 if(.not.ok)then
  print '(a)','FAIL '//label
  stop 1
 endif
 end subroutine
end program
