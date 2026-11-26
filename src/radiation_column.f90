! One RRTMG call for one column on the host: Xu-Randall cloud fraction or cloud incidence (config_radt_cld_scheme), the
! ozone of the date, then SW and LW (radiation_sw.f90, radiation_lw.f90), with the outputs packed as the host radiation
! loop of lean_model.f90 stores them.
module radiation_column
 use config,only: nz,nz1
 use radiation_sizes,only: max_lw_layers,max_sw_layers
 use mpas_atmphys_constants,only: degrad
 use radiation_prepare,only: radiation_interfaces
 use radiation_cloud,only: radiation_cloud_fraction,radiation_cloud_incidence
 use schemes,only: cloud_scheme,CLOUD_INCIDENCE
 use radiation_climatology,only: ozone_at_time
 use radiation_sw,only: radiation_sw_column
 use radiation_lw,only: radiation_lw_column
 use radiation_scratch,only: lw_scratch,sw_scratch
 implicit none
 private
 public :: radiation_event
contains
 ! radiation_event keeps the large work arrays (radiation_scratch.f90) as locals and passes them to radiation_event_ws.
 ! Both are recursive so that the locals are on the stack of each OpenMP thread, not shared static storage.
 recursive subroutine radiation_event(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs, &
  re_cloud,re_ice,re_snow,skin,emiss,xland,xice,snow,albedo,latitude,longitude, &
  day,julian,xtime,gmt,declin,solcon,pin,monthly,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,do_lw,do_sw)
 real,intent(in) :: theta(nz),exner(nz),pressure(nz),rho(nz),dz(nz),z(nz+1)
 real,intent(in) :: qv(nz),qc(nz),qi(nz),qs(nz),re_cloud(nz),re_ice(nz),re_snow(nz)
 real,intent(in) :: skin,emiss,xland,xice,snow,albedo,latitude,longitude
 real,intent(in) :: julian,xtime,gmt,declin,solcon,pin(59),monthly(12,59)
 logical,intent(in) :: do_lw,do_sw
 integer,intent(in) :: day
#ifdef LEAN_NOAHMP_SWSPLIT
 real,intent(out) :: heating(nz,2),sw(17),lw(11),sw_flux(4,max_sw_layers+1),lw_flux(4,max_lw_layers+1)
#else
 real,intent(out) :: heating(nz,2),sw(14),lw(11),sw_flux(4,max_sw_layers+1),lw_flux(4,max_lw_layers+1)
#endif
 real,intent(out) :: radii(nz,3),cloud(nz),ozone(59)
 integer,intent(out) :: active
 type(lw_scratch) :: lw_ws
 type(sw_scratch) :: sw_ws
 call radiation_event_ws(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs, &
  re_cloud,re_ice,re_snow,skin,emiss,xland,xice,snow,albedo,latitude,longitude, &
  day,julian,xtime,gmt,declin,solcon,pin,monthly,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,lw_ws,sw_ws,do_lw,do_sw)
 end subroutine
 recursive subroutine radiation_event_ws(theta,exner,pressure,rho,dz,z,qv,qc,qi,qs, &
  re_cloud,re_ice,re_snow,skin,emiss,xland,xice,snow,albedo,latitude,longitude, &
  day,julian,xtime,gmt,declin,solcon,pin,monthly,heating,sw,lw,sw_flux,lw_flux,active,radii,cloud,ozone,lw_ws,sw_ws,do_lw,do_sw)
 real,intent(in) :: theta(nz),exner(nz),pressure(nz),rho(nz),dz(nz),z(nz+1)
 real,intent(in) :: qv(nz),qc(nz),qi(nz),qs(nz),re_cloud(nz),re_ice(nz),re_snow(nz)
 real,intent(in) :: skin,emiss,xland,xice,snow,albedo,latitude,longitude
 real,intent(in) :: julian,xtime,gmt,declin,solcon,pin(59),monthly(12,59)
 logical,intent(in) :: do_lw,do_sw
 integer,intent(in) :: day
#ifdef LEAN_NOAHMP_SWSPLIT
 real,intent(out) :: heating(nz,2),sw(17),lw(11),sw_flux(4,max_sw_layers+1),lw_flux(4,max_lw_layers+1)
#else
 real,intent(out) :: heating(nz,2),sw(14),lw(11),sw_flux(4,max_sw_layers+1),lw_flux(4,max_lw_layers+1)
#endif
 real,intent(out) :: radii(nz,3),cloud(nz),ozone(59)
 integer,intent(out) :: active
 real :: temperature(nz),interface_t(nz+1),hydro_layer(nz),hydro_interface(nz+1),water(nz,4)
#ifdef LEAN_NOAHMP_SWSPLIT
 real :: coszr,gsw,swcf,glw,olr,lwcf,beam(6),lat,lon
#else
 real :: coszr,gsw,swcf,glw,olr,lwcf,beam(3),lat,lon
#endif
 type(lw_scratch),intent(inout) :: lw_ws   ! large work arrays (radiation_scratch.f90)
 type(sw_scratch),intent(inout) :: sw_ws
 integer :: k
 ! temperature from theta, and the water contents limited to >= 0 as MPAS_to_phys does
 do k=1,nz
  temperature(k)=theta(k)*exner(k)
  water(k,1)=max(0.,qv(k));water(k,2)=max(0.,qc(k));water(k,3)=max(0.,qi(k))
  water(k,4)=max(0.,qs(k))
 enddo
 lat=latitude/degrad
 lon=longitude/degrad
 call radiation_interfaces(temperature,pressure,rho,dz,z,interface_t,hydro_layer,hydro_interface)
 ! config_radt_cld_scheme (mpas_atmphys_driver_cloudiness.F:200-211): cld_incidence from qc and qi, else cld_fraction
 if(cloud_scheme==CLOUD_INCIDENCE)then
  call radiation_cloud_incidence(cloud,water(:,2),water(:,3))
 else
  call radiation_cloud_fraction(cloud,temperature,pressure,water(:,1),water(:,2),water(:,3),water(:,4))
 endif
 call ozone_at_time(julian,monthly,ozone)
 ! MPAS runs SW before LW, before any surface/PBL/convection state change.
 if(do_sw)then
 call radiation_sw_column(hydro_layer,hydro_interface,exner,temperature,interface_t,dz, &
  water(:,1),water(:,2),water(:,3),water(:,4),cloud,skin,albedo,xland,xice,snow,lat,lon, &
  day,pin,ozone,re_cloud,re_ice,re_snow,xtime,gmt,declin,solcon,coszr,gsw,swcf,heating(:,2),sw_flux,beam, ws=sw_ws)
 endif
 if(do_lw)then
 call radiation_lw_column(hydro_layer,hydro_interface,exner,temperature,interface_t,dz, &
  water(:,1),water(:,2),water(:,3),water(:,4),cloud,skin,emiss,xland,xice,snow,lat, &
  day,pin,ozone,re_cloud,re_ice,re_snow,glw,olr,lwcf,heating(:,1),lw_flux,active,radii, ws=lw_ws)
 endif
 if(do_sw)then
 sw=[coszr,gsw,swcf,sw_flux(1,nz1+1),sw_flux(3,nz1+1),sw_flux(2,nz1+1), &
  sw_flux(4,nz1+1),sw_flux(1,1),sw_flux(3,1),sw_flux(2,1),sw_flux(4,1),beam]
 endif
 if(do_lw)then
 lw=[glw,olr,lwcf,lw_flux(1,active+1),lw_flux(3,active+1),lw_flux(2,active+1),lw_flux(4,active+1), &
  lw_flux(1,1),lw_flux(3,1),lw_flux(2,1),lw_flux(4,1)]
 endif
 end subroutine
end module
