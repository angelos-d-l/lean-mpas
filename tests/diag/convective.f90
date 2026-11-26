! getcape and the column helpers of diag_convective (column_height_value, integral_zpoint, integral_zstaggered) against
! MPAS's routines (tests/diag/mpas_oracle.f90), bit for bit, on synthetic columns. Run by tests/diag/run.sh.
program convective_columns
 use diag_convective,only:getcape,column_height_value,integral_zpoint,integral_zstaggered
 use mpas_diag_oracle,only:oracle_getcape,oracle_column_height_value,oracle_integral_zpoint,oracle_integral_zstaggered
 implicit none
 real::p(5)=[1000.,850.,700.,500.,200.],t(5)=[25.,15.,5.,-15.,-55.],td(5)=[20.,10.,-5.,-20.,-60.]
 real::cape,cin,ocape,ocin,z(6),zp(5),wind(5),one,two,height
 integer::c,k,checked
 call getcape(5,p,t,td,cape,cin)
 call oracle_getcape(5,p,t,td,ocape,ocin)
 call require(all(transfer([cape,cin],[0],2)==transfer([ocape,ocin],[0],2)),'getcape synthetic bits')
 checked=0
 do c=1,50
  do k=1,6
   z(k)=real(k-1)**2*300.+real(c)
  enddo
  zp=0.5*(z(:5)+z(2:))
  do k=1,5
   wind(k)=sin(real(k*c))*10.
  enddo
  height=real(c-2)*200.
  one=column_height_value(wind,zp,height,5)
  two=oracle_column_height_value(wind,zp,height,5)
  call require(transfer(one,0)==transfer(two,0),'srh_and_levels height bits')
  one=integral_zstaggered(wind,z,0.,3000.,5,6)
  two=oracle_integral_zstaggered(wind,z,0.,3000.,5,6)
  call require(transfer(one,0)==transfer(two,0),'srh_and_levels staggered bits')
  one=integral_zpoint(wind,zp,0.,1000.,5)
  two=oracle_integral_zpoint(wind,zp,0.,1000.,5)
  call require(transfer(one,0)==transfer(two,0),'srh_and_levels point bits')
  checked=checked+3
 enddo
 call require(checked==150,'srh_and_levels columns exercised')
 print '(a)','GETCAPE COLUMNS OK; SRH AND LEVELS OK 150'
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
