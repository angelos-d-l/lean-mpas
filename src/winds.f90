! The zonal and meridional winds at the cell centres from the normal winds of the edges, with the reconstruction
! coefficients of mesh_vectors.f90 (MPAS's mpas_reconstruct_2d and its zonal/meridional projection).
module initial_winds
 use config,only: nz,ncells,nedges,maxedges,max_levels
 implicit none
contains
 subroutine reconstruct_winds(edge_wind,coeff,edges,degree,lat,lon,u,v)
 real,intent(in) :: edge_wind(nz,nedges),coeff(3,maxedges,ncells),lat(ncells),lon(ncells)
 integer,intent(in) :: edges(maxedges,ncells),degree(ncells)
 real,intent(out) :: u(nz,ncells),v(nz,ncells)
 real :: x(nz),y(nz),z(nz),clat,slat,clon,slon
 integer :: c,e,k,edge
 !$omp do schedule(dynamic,64)
 do c=1,ncells
  x=0.;y=0.;z=0.
  do e=1,degree(c)
   edge=edges(e,c)
   do k=1,nz
    x(k)=x(k)+coeff(1,e,c)*edge_wind(k,edge)
    y(k)=y(k)+coeff(2,e,c)*edge_wind(k,edge)
    z(k)=z(k)+coeff(3,e,c)*edge_wind(k,edge)
   enddo
  enddo
  clat=cos(lat(c));slat=sin(lat(c));clon=cos(lon(c));slon=sin(lon(c))
  do k=1,nz
   u(k,c)=-x(k)*slon+y(k)*clon
   v(k,c)=-(x(k)*clon+y(k)*slon)*slat+z(k)*clat
  enddo
 enddo
 end subroutine
! The same reconstruction with the sine and cosine of the cell latitude and longitude given (computed once at startup, see setup_grid):
! the card has its own sin/cos, which differ from the host library in the last bit for a share of the arguments.
 subroutine reconstruct_winds_trig(edge_wind,coeff,edges,degree,clat,slat,clon,slon,u,v)
 real,intent(in) :: edge_wind(nz,nedges),coeff(3,maxedges,ncells),clat(ncells),slat(ncells),clon(ncells),slon(ncells)
 integer,intent(in) :: edges(maxedges,ncells),degree(ncells)
 real,intent(out) :: u(nz,ncells),v(nz,ncells)
 real :: x(max_levels),y(max_levels),z(max_levels)   ! capacity, not nz: a compile-time size keeps the gang-private arrays in shared memory
 integer :: c,e,k,edge
 !$acc parallel loop gang vector_length(64) default(present) private(x,y,z) async(1)
 do c=1,ncells
  x=0.;y=0.;z=0.
  do e=1,degree(c)
   edge=edges(e,c)
   !$acc loop vector
   do k=1,nz
    x(k)=x(k)+coeff(1,e,c)*edge_wind(k,edge)
    y(k)=y(k)+coeff(2,e,c)*edge_wind(k,edge)
    z(k)=z(k)+coeff(3,e,c)*edge_wind(k,edge)
   enddo
  enddo
  do k=1,nz
   u(k,c)=-x(k)*slon(c)+y(k)*clon(c)
   v(k,c)=-(x(k)*clon(c)+y(k)*slon(c))*slat(c)+z(k)*clat(c)
  enddo
 enddo
 end subroutine
end module
