! The updraft helicity sum order, the wind maximum, the maxima reset and the reflectivity columns of the diagnostics on a
! real mesh:  families ROOT MESH (greece_60km or x1_2562). Reads the mesh inputs and MPAS's reference history of
! data/cases/MESH; run by tests/diag/run.sh.
program diagnostic_families
 use lean_nc
 use diag_convective
 use diag_micro
 use mpas_diag_oracle
 use mpas_atmphys_functions, only: rslf,rsif
 use lean_thompson, only: calc_refl10cm,thompson_init
 implicit none
 character(2048)::root
 character(32)::mesh,reference,init_name
 integer::oracle_unit, nonexact, kp
 type(nc_file)::f
 integer::status,nc,nv,nz,me,vd,c,v,j,k,edges,changed
 real,allocatable::w(:,:),vort(:,:),z(:,:),ka(:,:),area(:),u(:,:),vv(:,:),uh(:),wm(:),wsp(:),expected(:)
 integer,allocatable::vertices(:,:),degree(:),cells(:,:),kite(:,:),order(:,:),walk(:),met(:)
 real,allocatable::mpas_uh(:)
 integer::far
 real::film(8,5,3),zg(6,3),refl(5,3),rmax(3),r1(3),r1max(3),rh(5,3),dbz(5),height(5),qsat,weight,one(3)
 call get_command_argument(1,root)
 call get_command_argument(2,mesh)
 if(mesh=='')mesh='greece_60km'
 reference='strict_12h';init_name='greece_60km.init.nc'
 if(mesh=='x1_2562')then
  reference='strict_24h';init_name='x1.2562.init.nc'
 endif
 status=nc_open(f,trim(root)//'/data/cases/'//trim(mesh)//'/inputs/'//trim(init_name));call require(status==0,'uh_order mesh open')
 status=nc_inq_dim(f,'nCells',nc);status=nc_inq_dim(f,'nVertices',nv)
 status=nc_inq_dim(f,'nVertLevels',nz);status=nc_inq_dim(f,'maxEdges',me);status=nc_inq_dim(f,'vertexDegree',vd)
 call require(((mesh=='greece_60km'.and.nc==2337).or.(mesh=='x1_2562'.and.nc==2562)).and.nz>0.and.nv>0.and.me>0.and.vd>0,'uh_order nonempty fixture')
 allocate(w(nz+1,nc),vort(nz,nv),z(nz+1,nc),ka(vd,nv),area(nc),u(nz,nc),vv(nz,nc),uh(nc),wm(nc),wsp(nc),expected(nc))
 allocate(vertices(me,nc),degree(nc),cells(vd,nv),kite(me,nc),order(me,nc))
 status=nc_get(f,'zgrid',z);call require(status==0,'uh_order zgrid')
 status=nc_get(f,'verticesOnCell',vertices);call require(status==0,'uh_order vertices')
 status=nc_get(f,'nEdgesOnCell',degree);status=nc_get(f,'cellsOnVertex',cells)
 status=nc_get(f,'kiteAreasOnVertex',ka);status=nc_get(f,'areaCell',area)
 status=nc_close(f)
 status=nc_open(f,trim(root)//'/data/cases/'//trim(mesh)//'/reference/'//trim(reference)//'/history.2026-11-08_00.00.00.nc')
 call require(status==0,'uh_order history open')
 status=nc_get(f,'w',w,record=1);call require(status==0,'uh_order w')
 status=nc_get(f,'vorticity',vort,record=1);call require(status==0,'uh_order vorticity')
 status=nc_get(f,'uReconstructZonal',u,record=1);status=nc_get(f,'uReconstructMeridional',vv,record=1)
 status=nc_close(f)
 kite=0
 do c=1,nc
  do j=1,degree(c)
   v=vertices(j,c)
   do k=1,vd
    if(cells(k,v)==c)kite(j,c)=k
   enddo
   call require(kite(j,c)>0,'uh_order kite found')
  enddo
 enddo
 call maxima_order(vertices,degree,kite,order)
 ! MPAS's block order of the vertices, built here apart from maxima_order: first met by a walk over the cells.
 allocate(walk(nv),met(nv),mpas_uh(nc));met=0;k=0
 do c=1,nc
  do j=1,degree(c)
   v=vertices(j,c)
   if(met(v)==0)then
    k=k+1;met(v)=k;walk(k)=v
   endif
  enddo
 enddo
 call require(k==nv,'uh_order walk meets every vertex')
 call scatter_uh(w,vort,z,cells,ka,area,walk,expected)
 ! The oracle is held to MPAS's own file. Built with gfortran (levels summed in source order) it differs from the
 ! file in one x1_2562 cell, by one last bit; built with nvfortran as the MPAS reference was (vectorized level sum) it
 ! differs in none (docs/fixes.md).
 status=nc_open(f,trim(root)//'/data/cases/'//trim(mesh)//'/reference/'//trim(reference)//'/diag.2026-11-08_00.00.00.nc')
 call require(status==0,'uh_order diag open')
 status=nc_get(f,'updraft_helicity_max',mpas_uh,record=1);call require(status==0,'uh_order mpas uh')
 status=nc_close(f)
 call require(count(mpas_uh>0.)>1000,'uh_order mpas uh active')
 far=count(transfer(mpas_uh,[0],nc)/=transfer(expected,[0],nc))
 print '(a,i0)','uh_order: oracle against MPAS file, differing cells ',far
#ifdef __GFORTRAN__
 call require((mesh=='greece_60km'.and.far==0).or.(mesh=='x1_2562'.and.far==1),'uh_order oracle equals MPAS file')
#else
 call require(far==0,'uh_order oracle equals MPAS file')
#endif
 open(newunit=oracle_unit,file=trim(root)//'/build/diag/oracle_uh.'//trim(mesh)//'.bin', &
  access='stream',form='unformatted',status='replace')
 write(oracle_unit)expected
 close(oracle_unit)
 uh=0.;wm=0.;wsp=0.
 call maxima_update(w,vort,u,vv,z,area,vertices,order,kite,ka,degree,uh,wm,wsp,.true.,.true.,.true.)
 changed=count(transfer(uh,[0],nc)/=transfer(expected,[0],nc))
 print '(a,i0,a,i0)','uh_order: ',nc,' cells, changed ',changed
#ifdef __GFORTRAN__
 call require(changed==0,'uh_order scatter bits')
#else
 ! nvfortran vectorizes the oracle's level sum as it did MPAS's, and not the kernel's: one x1 cell (docs/fixes.md).
 call require((mesh=='greece_60km'.and.changed==0).or.(mesh=='x1_2562'.and.changed==1),'uh_order scatter bits')
#endif
 ! The t=0 vertical velocity can vanish. This second input makes summation order observable.
 call require(all(wm==max(0.,maxval(w(:nz,:),dim=1))),'w maximum original fixture')
 w=1.;w(nz+1,:)=1.e6
 wm=0.
 call maxima_update(w,vort,u,vv,z,area,vertices,order,kite,ka,degree,uh,wm,wsp,.false.,.true.,.false.)
 call require(all(wm==1.),'w maximum excludes top')
 w=1.
 do v=1,nv
  vort(:,v)=sin(real(v)*0.123)*0.001
 enddo
 call scatter_uh(w,vort,z,cells,ka,area,walk,expected)
 uh=0.
 call maxima_update(w,vort,u,vv,z,area,vertices,order,kite,ka,degree,uh,wm,wsp,.true.,.false.,.false.)
 call require(count(expected>0.)>0,'uh_order active synthetic cells')
 changed=count(transfer(uh,[0],nc)/=transfer(expected,[0],nc))
 print '(a,i0)','uh_order: synthetic, changed ',changed
#ifdef __GFORTRAN__
 call require(changed==0,'uh_order synthetic scatter bits')
#else
 ! Measured with nvfortran 25.3: its vectorized oracle sum against the kernel's level order.
 call require((mesh=='greece_60km'.and.changed==0).or.(mesh=='x1_2562'.and.changed==1),'uh_order synthetic measured count')
#endif
 call require(all(wsp==sqrt(u(1,:)**2+vv(1,:)**2)),'wind maximum')
 call maxima_reset(uh);call require(all(uh==0.),'maxima_reset zero')
 ! Three Thompson input columns straddle the humidity phase switch.
 call thompson_init()
 film=0.;film(1,:,:)=0.005;film(3,:,:)=1.e-4;film(4,:,:)=1.e5
 film(7,:,1)=[274.,273.16,270.,260.,250.];film(7,:,2)=280.;film(7,:,3)=260.
 film(8,:,:)=80000.
 zg(:,1)=[100.,500.,900.,1300.,1700.,2100.]
 zg(:,2)=[100.,450.,900.,1500.,2100.,2800.]
 zg(:,3)=[200.,600.,1200.,1800.,2600.,3500.]
 do c=1,3
  film(3,:,c)=[1.e-8,1.e-6,1.e-4,1.e-3,2.e-3]
 enddo
 nonexact=0
 r1max=0.
 call micro_diagnostics(film,zg,refl,rmax,r1,r1max,rh)
 do c=1,3
  dbz=-35.
  call calc_refl10cm(film(1,:,c),film(2,:,c),film(3,:,c),film(4,:,c),film(5,:,c),film(6,:,c), &
   film(7,:,c),film(8,:,c),dbz,1,5,c,1)
  dbz=max(-35.,dbz)
  call require(all(refl(:,c)==dbz),'refl_column profile')
  call require(rmax(c)==maxval(dbz),'refl_column max')
  do k=1,5
   height(k)=zg(k,c)-zg(1,c)+0.5*(zg(k+1,c)-zg(k,c))
  enddo
  kp=1
  do k=1,5
   if(height(k)<1000.)kp=k
  enddo
  weight=(height(kp+1)-1000.)/(height(kp+1)-height(kp))
  if(c>1)then
   call require(weight>0..and.weight<1.,'refl_column nontrivial weights')
   call require(dbz(kp)/=dbz(kp+1),'refl_column varying reflectivity')
   nonexact=nonexact+1
  endif
  call require(r1(c)==weight*dbz(kp)+(1.-weight)*dbz(kp+1),'refl_column interpolation')
  call require(r1max(c)==max(0.,r1(c)),'refl_column running max')
  do k=1,5
   qsat=rslf(film(8,k,c),film(7,k,c))
   if(film(7,k,c)<=273.16)qsat=rsif(film(8,k,c),film(7,k,c))
   call require(rh(k,c)==(film(1,k,c)/qsat)*100.,'refl_column relhum phase')
  enddo
 enddo
 call require(nonexact==2,'refl_column two interpolated columns')
 print '(a)','DIAG FAMILIES OK'
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
