! MPAS's original array routines (the oracle) are held to MPAS's files first; then lean's column code is compared with
! the oracle.
program output_families
!$ use omp_lib,only:omp_set_num_threads
 use initial_winds,only:reconstruct_winds
 use config
 use lean_inputs
 use lean_messages
 use lean_nc
 use dyn_mesh_data,only:dyn_mesh_type
 use cold_dyn_data,only:cold_dyn_type
 use dyn_state_data,only:dyn_state_type
 use cold_dyn
 use dyn_step,only:dyn_grid,setup_grid
 use diag_convective,only:maxima_order,maxima_update,convective_out,convective_compute,getcape
 use diag_isobaric
 use mpas_diag_oracle
 implicit none
 type(dyn_mesh_type),target::m
 type(dyn_state_type),target::s
 type(dyn_grid)::g
 type(cold_dyn_type)::d
 type(cold_inputs)::inp
 type(message_list)::msgs
 type(nc_file)::h,f
 type(isobaric_out)::original,port,alone
 type(convective_out)::corig,cport
 real::started,finished,cp,ci
 real,allocatable::pp(:),tt(:),td(:)
 character(32),parameter::convnames(12)=[character(32)::'cape','cin','srh_0_1km','srh_0_3km','uzonal_surface','uzonal_1km','uzonal_6km','umeridional_surface','umeridional_1km','umeridional_6km','temperature_surface','dewpoint_surface']
 real,allocatable::u(:,:),v(:,:),vort(:,:),expected(:),file_w(:,:),uh(:),uh_ref(:),uh_port(:),wm(:),wsp(:)
 integer,allocatable::walk(:),met(:),order(:,:)
 integer::c,k,jj,vv,far
 character(2048)::root,path,ref
 character(32)::mesh,variant
 character(32),parameter::family(8)=[character(32)::'temperature','relhum','dewpoint','uzonal','umeridional','height','w','vorticity']
 character(3),parameter::level(8)=[character(3)::'50','100','200','250','500','700','850','925']
 integer::status,i,j,n,count_fields,unit,expected_count
 real(8)::expected_max,expected_relative,relative
 character(64)::expected_name
 call get_command_argument(1,root);call get_command_argument(2,mesh);call get_command_argument(3,variant)
 path=trim(root)//'/data/cases/'//trim(mesh)//'/inputs/'
 ref='strict_12h'
 if(mesh=='x1_2562')ref='strict_24h'
 if(variant=='l55')then
  path=trim(path)//'l55/'
  ref='strict_l55_diag1h'
 endif
 if(mesh=='greece_60km')then
  path=trim(path)//'greece_60km.init.nc'
 else
  path=trim(path)//'x1.2562.init.nc'
 endif
 call read_mesh_inputs(trim(path),m,inp,msgs)
 call read_state_inputs(trim(path),'2026-11-08_00:00:00',m,d,inp,msgs)
 call require(.not.inputs_failed(msgs),'output families inputs')
 call start_mesh_vectors(m)
 call setup_grid(m,s,g)
 call cold_dyn_state(d,m,g%sign_cell,g%zb_cell,g%zb3_cell,s)
 allocate(u(nz,nc1),v(nz,nc1),vort(nz,nvertices))
 u=0.;v=0.
 path=trim(root)//'/data/cases/'//trim(mesh)//'/reference/'//trim(ref)//'/'
 status=nc_open(h,trim(path)//'history.2026-11-08_00.00.00.nc');call require(status==0,'output history open')
 status=nc_get(h,'uReconstructZonal',u(:,:ncells),record=1);call require(status==0,'output u')
 status=nc_get(h,'uReconstructMeridional',v(:,:ncells),record=1);call require(status==0,'output v')
 status=nc_get(h,'vorticity',vort,record=1);call require(status==0,'output vort')
 allocate(file_w(nz+1,ncells))
 status=nc_get(h,'w',file_w,record=1);call require(status==0,'output w read')
 call require(all(file_w==s%w(:,:ncells)),'UH initial w equals MPAS history')
 status=nc_close(h)
 status=nc_open(f,trim(path)//'diag.2026-11-08_00.00.00.nc');call require(status==0,'output diag open')
 call isobaric_allocate(original,ncells,nvertices,nz)
 call isobaric_allocate(port,ncells,nvertices,nz)
 call isobaric_allocate(alone,ncells,nvertices,nz)
 call oracle_isobaric(m%areaTriangle(:nvertices),m%kiteAreasOnVertex(:,:nvertices),s%exner,m%zgrid,s%pressure_base, &
  s%pressure_p,inp%relhum,s%theta_m,vort,v,u,s%w,s%scalars,m%cellsOnVertex(:,:nvertices),original)
 ! tests/diag/oracle_*.<mesh>[_l55].txt: per field, in order, the measured number of values where the gfortran-built
 ! oracle differs from MPAS's file (MPAS's reference was built with nvfortran), the largest difference and, for the
 ! convective fields, the largest relative difference (used for cape and cin); an nvfortran build must match exactly.
 open(newunit=unit,file=trim(root)//'/tests/diag/oracle_isobaric.'//trim(mesh)// &
  trim(merge('_l55','    ',variant=='l55'))//'.txt',status='old')
 count_fields=0
 do j=1,8
  n=ncells
  if(j==8)n=nvertices
  allocate(expected(n))
  do i=1,8
   status=nc_get(f,trim(family(j))//'_'//trim(level(i))//'hPa',expected,record=1)
   call require(status==0,'output isobaric field read')
   if(j==8)then
    call report(trim(family(j))//'_'//trim(level(i))//'hPa',original%vorticity(i,:),expected)
   else
    call report(trim(family(j))//'_'//trim(level(i))//'hPa',original%cell(i,:,j),expected)
   endif
   count_fields=count_fields+1
  enddo
  deallocate(expected)
 enddo
 allocate(expected(ncells))
 status=nc_get(f,'mslp',expected,record=1);call require(status==0,'output mslp read');call report('mslp',original%mslp,expected)
 status=nc_get(f,'meanT_500_300',expected,record=1);call require(status==0,'output mean read');call report('meanT_500_300',original%meanT,expected)
 deallocate(expected);allocate(expected(5*ncells))
 block
 real,allocatable::values(:,:)
 allocate(values(5,ncells))
 status=nc_get(f,'t_isobaric',values,record=1);call require(status==0,'output t read')
 call report('t_isobaric',reshape(original%t_isobaric,[5*ncells]),reshape(values,[5*ncells]))
 deallocate(values);allocate(values(13,ncells))
 status=nc_get(f,'z_isobaric',values,record=1);call require(status==0,'output z read')
 call report('z_isobaric',reshape(original%z_isobaric,[13*ncells]),reshape(values,[13*ncells]))
 end block
 call require(count_fields==64,'output compared 64 level fields')
 status=nc_close(f)
 close(unit)
 ! The 55-level file gets its own measured check of MPAS's original UH scatter.
 allocate(walk(nvertices),met(nvertices),uh(ncells),uh_ref(ncells),uh_port(ncells),wm(ncells),wsp(ncells))
 met=0;k=0
 do c=1,ncells
  do jj=1,m%nEdgesOnCell(c)
   vv=m%verticesOnCell(jj,c)
   if(met(vv)==0)then
    k=k+1;met(vv)=k;walk(k)=vv
   endif
  enddo
 enddo
 call require(k==nvertices,'UH file-order walk complete')
 call scatter_uh(s%w(:,:ncells),vort,m%zgrid(:,:ncells),m%cellsOnVertex(:,:nvertices), &
  m%kiteAreasOnVertex(:,:nvertices),m%areaCell(:ncells),walk,uh)
 status=nc_open(f,trim(path)//'diag.2026-11-08_00.00.00.nc')
 status=nc_get(f,'updraft_helicity_max',uh_ref,record=1);call require(status==0,'UH file read')
 status=nc_close(f)
 far=count(transfer(uh,[0],ncells)/=transfer(uh_ref,[0],ncells))
 print '(a,i0,a,es24.16)','UH oracle file differing ',far,', max ',maxval(abs(real(uh,8)-real(uh_ref,8)))
#ifdef __GFORTRAN__
 k=0
 if(mesh=='x1_2562')k=1
 if(variant=='l55')k=283
 call require(far==k,'UH oracle file measured count')
#else
 ! Compiled as the MPAS reference build was (nvfortran), the original equals MPAS's file: both carry nvfortran's vectorized level sum.
 call require(far==0,'UH oracle file exact')
#endif
 allocate(order(size(m%verticesOnCell,1),nc1))
 call maxima_order(m%verticesOnCell,m%nEdgesOnCell,g%kite,order)
 uh_port=0.;wm=0.;wsp=0.
 call maxima_update(s%w,vort,u,v,m%zgrid,m%areaCell,m%verticesOnCell,order,g%kite,m%kiteAreasOnVertex, &
  m%nEdgesOnCell,uh_port,wm,wsp,.true.,.false.,.false.)
 far=count(transfer(uh,[0],ncells)/=transfer(uh_port,[0],ncells))
 print '(a,i0)','UH port against oracle, differing cells ',far
#ifdef __GFORTRAN__
 call require(far==0,'UH port equals oracle at active levels')
#else
 ! nvfortran vectorizes the original's level sum and not the port's (docs/fixes.md); the allowance is measured.
 k=0
 if(mesh=='x1_2562')k=1
 if(variant=='l55')k=283
 call require(far==k,'UH port against oracle measured count')
#endif
 call isobaric_compute(s%theta_m,s%scalars,s%exner,s%pressure_p,s%pressure_base,m%zgrid,s%w,u,v,inp%relhum, &
  vort,m%cellsOnVertex(:,:nvertices),m%kiteAreasOnVertex(:,:nvertices),m%areaTriangle(:nvertices),port)
 call require(all(transfer(port%cell,[0],size(port%cell))==transfer(original%cell,[0],size(original%cell))),'isobaric cell oracle bits')
 call require(all(transfer(port%vorticity,[0],size(port%vorticity))==transfer(original%vorticity,[0],size(original%vorticity))),'isobaric vertex oracle bits')
 call require(all(port%mslp==original%mslp),'isobaric slp oracle bits')
 call require(all(port%t_isobaric==original%t_isobaric),'isobaric t oracle bits')
 call require(all(port%z_isobaric==original%z_isobaric),'isobaric z oracle bits')
 far=count(transfer(port%meanT,[0],ncells)/=transfer(original%meanT,[0],ncells))
 print '(a,i0)','meanT port against oracle, differing cells ',far
#ifdef __GFORTRAN__
 call require(far==0,'isobaric mean oracle bits')
#else
 ! The same cause: the original's layer sum is vectorized by nvfortran, the port's is not; the allowance is measured.
 k=0
 if(mesh=='x1_2562')k=1
 if(variant=='l55')k=20
 call require(far==k,'isobaric mean against oracle measured count')
#endif
 alone%need=.false.;alone%need(10)=.true.;alone%need(12)=.true.
 call isobaric_compute(s%theta_m,s%scalars,s%exner,s%pressure_p,s%pressure_base,m%zgrid,s%w,u,v,inp%relhum, &
  vort,m%cellsOnVertex(:,:nvertices),m%kiteAreasOnVertex(:,:nvertices),m%areaTriangle(:nvertices),alone)
 call require(all(alone%t_isobaric==port%t_isobaric).and.all(alone%meanT==port%meanT),'t_isobaric_alone')
 ! Exactly the predicate fix in reference_fixes/mpas_isobaric_diagnostics.F, with only these fields requested.
 call oracle_isobaric(m%areaTriangle(:nvertices),m%kiteAreasOnVertex(:,:nvertices),s%exner,m%zgrid,s%pressure_base, &
  s%pressure_p,inp%relhum,s%theta_m,vort,v,u,s%w,s%scalars,m%cellsOnVertex(:,:nvertices),alone,temperature_only=.true.)
#ifdef __GFORTRAN__
 call require(all(alone%t_isobaric==port%t_isobaric).and.all(alone%meanT==port%meanT),'t_isobaric_alone patched reference')
#else
 call require(all(alone%t_isobaric==port%t_isobaric).and.count(transfer(alone%meanT,[0],ncells)/=transfer(port%meanT,[0],ncells))==k, &
  't_isobaric_alone patched reference')
#endif

 allocate(corig%values(12,ncells),cport%values(12,ncells))
 !$omp parallel
 call reconstruct_winds(s%u,m%coeffs_reconstruct,m%edgesOnCell,m%nEdgesOnCell,m%latCell,m%lonCell,u,v)
 !$omp end parallel
 call oracle_convective(s%theta_m,s%scalars,s%exner,s%pressure_p,s%pressure_base,m%zgrid,u,v,corig)
 status=nc_open(f,trim(path)//'diag.2026-11-08_00.00.00.nc')
 deallocate(expected);allocate(expected(ncells))
 open(newunit=unit,file=trim(root)//'/tests/diag/oracle_convective.'//trim(mesh)// &
  trim(merge('_l55','    ',variant=='l55'))//'.txt',status='old')
 do i=1,12
  status=nc_get(f,trim(convnames(i)),expected,record=1);call require(status==0,'convective field read')
  far=count(transfer(corig%values(i,:),[0],ncells)/=transfer(expected,[0],ncells))
  print '(a,a,a,i0,a,es24.16)','convective oracle ',trim(convnames(i)),': differing ',far,', max ', &
   maxval(abs(real(corig%values(i,:),8)-real(expected,8)))
  relative=maxval(abs(real(corig%values(i,:),8)-real(expected,8))/max(abs(real(expected,8)),real(tiny(1.),8)))
  if(i<=2)print '(a,a,a,es24.16)','convective relative ',trim(convnames(i)),': ',relative
  read(unit,*)expected_name,expected_count,expected_max,expected_relative
  call require(trim(expected_name)==trim(convnames(i)),'convective expected order')
#ifdef __GFORTRAN__
  call require(far==expected_count.and. &
   maxval(abs(real(corig%values(i,:),8)-real(expected,8)))<=expected_max,'convective file measurement '//trim(convnames(i)))
  if(i<=2)call require(relative<=expected_relative,'convective measured relative '//trim(convnames(i)))
#else
  call require(far==0,'convective file exact '//trim(convnames(i)))
#endif
  call require(any(abs(expected)>0.),'convective active '//trim(convnames(i)))
 enddo
 status=nc_close(f)
 close(unit)
 !$ call omp_set_num_threads(1)
 call cpu_time(started)
 call convective_compute(s%theta_m,s%scalars,s%exner,s%pressure_p,s%pressure_base,m%zgrid,u,v,cport)
 call cpu_time(finished)
 print '(a,f12.6,a,f12.6)','convective cost seconds/1000 ',(finished-started)*1000./ncells, &
  '; estimated seconds for the 46,251 cells of athens_3km ',(finished-started)*46251./ncells
 do i=1,12
  call require(all(transfer(corig%values(i,:),[0],ncells)==transfer(cport%values(i,:),[0],ncells)), &
   'convective oracle bits '//trim(convnames(i)))
 enddo
 allocate(pp(nz),tt(nz),td(nz))
 do c=1,ncells
  pp=(s%pressure_p(:,c)+s%pressure_base(:,c))/100.
  tt=port%temperature(:,c)-273.15
  do k=1,nz
   block
    real::evp
    evp=0.01*(s%pressure_base(k,c)+s%pressure_p(k,c))*s%scalars(1,k,c)/(s%scalars(1,k,c)+0.622)
    evp=max(evp,1.e-8)
    td(k)=(243.5*log(evp/6.112))/(17.67-log(evp/6.112))
    td(k)=(td(k)+273.15)-273.15
   end block
  enddo
  call getcape(nz,pp,tt,td,cp,ci)
  call require(all(transfer([cp,ci],[0],2)==transfer(corig%values(1:2,c),[0],2)),'getcape_columns')
 enddo
 print '(a)','OUTPUT FAMILIES OK '//trim(mesh)//' '//trim(variant)
contains
 subroutine report(name,a,b)
 character(*),intent(in)::name
 real,intent(in)::a(:),b(:)
 integer::changed,locations
 changed=count(transfer(a,[0],size(a))/=transfer(b,[0],size(b)))
 print '(a,a,a,i0,a,es24.16)','oracle ',trim(name),': differing ',changed,', max ',maxval(abs(real(a,8)-real(b,8)))
 locations=changed
 if(name=='t_isobaric')locations=count(any(reshape(transfer(a,[0],size(a))/=transfer(b,[0],size(b)),[5,size(a)/5]),dim=1))
 if(name=='z_isobaric')locations=count(any(reshape(transfer(a,[0],size(a))/=transfer(b,[0],size(b)),[13,size(a)/13]),dim=1))
 if(name=='t_isobaric'.or.name=='z_isobaric')print '(a,a,a,i0)','oracle differing cells ',trim(name),': ',locations

 read(unit,*)expected_name,expected_count,expected_max
 call require(trim(expected_name)==name,'oracle expected field order')
#ifdef __GFORTRAN__
 call require(changed==expected_count.and.maxval(abs(real(a,8)-real(b,8)))<=expected_max,'oracle file measurement '//name)
#else
 call require(changed==0,'oracle file exact '//name)
#endif
 call require(size(a)>0.and.any(abs(b)>0.),'oracle active '//trim(name))
 end subroutine
 subroutine require(ok,label)
 logical,intent(in)::ok
 character(*),intent(in)::label
 if(.not.ok)then
  print '(a)','FAIL '//label
  stop 1
 endif
 end subroutine
end program
