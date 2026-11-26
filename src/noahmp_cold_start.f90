! Land start of a cold run: the NoahmpIO column MPAS builds before its first step. The init values, the soil layers of
! physics_registry_init, the landuse surface properties (landuse_startup.f90), then NoahmpInitMain with restart_flag
! false on land cells.
! The caller clears io first (clear_io): every NoahmpIO field not set here or by NoahmpInitMain (fluxes,
! accumulators, the atmospheric forcing, which the first step sets) starts at 0, as in MPAS. dz8w, which Noah-MP reads
! once for its reference height, is set below from the column's layer thicknesses.
module noahmp_cold_start
 use NoahmpIOVarType
 use NoahmpInitMainMod, only: NoahmpInitMain
 use config, only: nz
 use model_state, only: column_view
 use cold_column, only: cold_cell
 use landuse_startup, only: surface_table, landuse_cold
 implicit none
 private
 public :: cold_land_column
contains
 ! Also sets the column's znt, mavail and ust as landuse_init_forMPAS does for every cell. z: zgrid of the column.
 subroutine cold_land_column(io, r, a, t, julday, z)
 type(NoahmpIO_type), intent(inout) :: io
 type(cold_cell), intent(in) :: r
 type(column_view), intent(in) :: a
 type(surface_table), intent(in) :: t
 integer, intent(in) :: julday
 real, intent(in) :: z(nz+1)
 real :: albedo, emissivity, roughness, moisture, friction
 integer :: k
 io%ivgtyp = r%ivgtyp; io%isltyp = r%isltyp; io%tsk = r%skintemp; io%snow = r%snow; io%snowh = r%snowh
 io%snowc = r%snowc; io%tmn = r%tmn; io%xice = r%xice; io%xland = r%xland
 io%sh2o(1,:) = r%sh2o; io%smois(1,:) = r%smois; io%tslb(1,:) = r%tslb
 ! physics_registry_init (mpas_atmphys_control.F:375-378) sets the Noah-MP soil layers at every start
 io%dzs(1) = 0.10; io%dzs(2) = 0.30; io%dzs(3) = 0.60; io%dzs(4) = 1.00
 io%vegfra = r%vegfra; io%gvfmin = r%shdmin; io%gvfmax = r%shdmax
 io%xlat = r%latCell; io%xlong = r%lonCell; io%areaxy = r%areaCell
 call landuse_cold(t, julday, r%latCell, r%ivgtyp, r%sfc_albbck, r%snowc, r%snoalb, r%xice, &
                   albedo, emissivity, roughness, moisture, friction)
 io%albedo = albedo; io%emiss = emissivity; io%znt = roughness
 a%znt = roughness; a%mavail = moisture; a%ust = friction
 ! the layer thicknesses: Noah-MP takes its reference height and bottom-layer thickness from them once
 ! (ConfigVarInTransferMod.F90:84-85); the other forcing the first step sets
 do k = 1, nz
  io%dz8w(1,k) = z(k+1) - z(k)
 end do
 io%restart_flag = .false.; io%fndsnowh = .true.; io%itimestep = 1
 if (r%xland < 1.5) call NoahmpInitMain(io)
 end subroutine
end module
