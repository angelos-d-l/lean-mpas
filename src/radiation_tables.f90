! The RRTMG table reads, translated from MPAS 8.4.2 (physics/mpas_atmphys_rrtmg_lwinit.F and _swinit.F).
! Startup only; explicit big-endian I/O. No tables are read in a column call.
module radiation_tables
 implicit none
 private
 public :: initialize_radiation_tables
contains
 subroutine initialize_radiation_tables(directory)
 use module_ra_rrtmg_lw, only: rrtmg_lw_ini
 use module_ra_rrtmg_sw, only: rrtmg_sw_ini
 use mpas_atmphys_constants, only: cp
 character(*), intent(in) :: directory
 integer :: unit, status
 open(newunit=unit,file=directory//'/RRTMG_LW_DATA', &
  status='old',action='read',form='unformatted',convert='big_endian',iostat=status)
 if(status/=0) error stop 'RRTMG table open failed'
 call read_lw_kgb01(unit)
 call read_lw_kgb02(unit)
 call read_lw_kgb03(unit)
 call read_lw_kgb04(unit)
 call read_lw_kgb05(unit)
 call read_lw_kgb06(unit)
 call read_lw_kgb07(unit)
 call read_lw_kgb08(unit)
 call read_lw_kgb09(unit)
 call read_lw_kgb10(unit)
 call read_lw_kgb11(unit)
 call read_lw_kgb12(unit)
 call read_lw_kgb13(unit)
 call read_lw_kgb14(unit)
 call read_lw_kgb15(unit)
 call read_lw_kgb16(unit)
 close(unit)
 call rrtmg_lw_ini(cp)
 open(newunit=unit,file=directory//'/RRTMG_SW_DATA', &
  status='old',action='read',form='unformatted',convert='big_endian',iostat=status)
 if(status/=0) error stop 'RRTMG table open failed'
 call read_sw_kgb16(unit)
 call read_sw_kgb17(unit)
 call read_sw_kgb18(unit)
 call read_sw_kgb19(unit)
 call read_sw_kgb20(unit)
 call read_sw_kgb21(unit)
 call read_sw_kgb22(unit)
 call read_sw_kgb23(unit)
 call read_sw_kgb24(unit)
 call read_sw_kgb25(unit)
 call read_sw_kgb26(unit)
 call read_sw_kgb27(unit)
 call read_sw_kgb28(unit)
 call read_sw_kgb29(unit)
 close(unit)
 call rrtmg_sw_ini(cp)
 end subroutine
 subroutine read_lw_kgb01(unit)
 use rrlw_kg01, only : fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb01: read failed'
 end subroutine
 subroutine read_lw_kgb02(unit)
 use rrlw_kg02, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb02: read failed'
 end subroutine
 subroutine read_lw_kgb03(unit)
 use rrlw_kg03, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb03: read failed'
 end subroutine
 subroutine read_lw_kgb04(unit)
 use rrlw_kg04, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb04: read failed'
 end subroutine
 subroutine read_lw_kgb05(unit)
 use rrlw_kg05, only : fracrefao, fracrefbo, kao, kbo, kao_mo3, selfrefo, forrefo, ccl4o
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mo3, ccl4o, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb05: read failed'
 end subroutine
 subroutine read_lw_kgb06(unit)
 use rrlw_kg06, only : fracrefao, kao, kao_mco2, selfrefo, forrefo, cfc11adjo, cfc12o
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, kao, kao_mco2, cfc11adjo, cfc12o, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb06: read failed'
 end subroutine
 subroutine read_lw_kgb07(unit)
 use rrlw_kg07, only : fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb07: read failed'
 end subroutine
 subroutine read_lw_kgb08(unit)
 use rrlw_kg08, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mn2o, kao_mo3, kbo, kbo_mco2, kbo_mn2o, selfrefo, forrefo, cfc12o, cfc22adjo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, kao_mn2o, kbo_mn2o, kao_mo3, cfc12o, cfc22adjo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb08: read failed'
 end subroutine
 subroutine read_lw_kgb09(unit)
 use rrlw_kg09, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb09: read failed'
 end subroutine
 subroutine read_lw_kgb10(unit)
 use rrlw_kg10, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb10: read failed'
 end subroutine
 subroutine read_lw_kgb11(unit)
 use rrlw_kg11, only : fracrefao, fracrefbo, kao, kbo, kao_mo2, kbo_mo2, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, kao_mo2, kbo_mo2, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb11: read failed'
 end subroutine
 subroutine read_lw_kgb12(unit)
 use rrlw_kg12, only : fracrefao, kao, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, kao, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb12: read failed'
 end subroutine
 subroutine read_lw_kgb13(unit)
 use rrlw_kg13, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mco, kbo_mo3, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kao_mco2, kao_mco, kbo_mo3, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb13: read failed'
 end subroutine
 subroutine read_lw_kgb14(unit)
 use rrlw_kg14, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb14: read failed'
 end subroutine
 subroutine read_lw_kgb15(unit)
 use rrlw_kg15, only : fracrefao, kao, kao_mn2, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, kao, kao_mn2, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb15: read failed'
 end subroutine
 subroutine read_lw_kgb16(unit)
 use rrlw_kg16, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
 if(status/=0) error stop 'lw_kgb16: read failed'
 end subroutine
 subroutine read_sw_kgb16(unit)
 use rrsw_kg16, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat1, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat1, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb16: read failed'
 end subroutine
 subroutine read_sw_kgb17(unit)
 use rrsw_kg17, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb17: read failed'
 end subroutine
 subroutine read_sw_kgb18(unit)
 use rrsw_kg18, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb18: read failed'
 end subroutine
 subroutine read_sw_kgb19(unit)
 use rrsw_kg19, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb19: read failed'
 end subroutine
 subroutine read_sw_kgb20(unit)
 use rrsw_kg20, only : kao, kbo, selfrefo, forrefo, sfluxrefo, absch4o, rayl, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, layreffr, absch4o, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb20: read failed'
 end subroutine
 subroutine read_sw_kgb21(unit)
 use rrsw_kg21, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb21: read failed'
 end subroutine
 subroutine read_sw_kgb22(unit)
 use rrsw_kg22, only : kao, kbo, selfrefo, forrefo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb22: read failed'
 end subroutine
 subroutine read_sw_kgb23(unit)
 use rrsw_kg23, only : kao, selfrefo, forrefo, sfluxrefo, raylo, givfac, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  raylo, givfac, layreffr, kao, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb23: read failed'
 end subroutine
 subroutine read_sw_kgb24(unit)
 use rrsw_kg24, only : kao, kbo, selfrefo, forrefo, sfluxrefo, raylao, raylbo, abso3ao, abso3bo, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  raylao, raylbo, strrat, layreffr, abso3ao, abso3bo, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb24: read failed'
 end subroutine
 subroutine read_sw_kgb25(unit)
 use rrsw_kg25, only : kao, sfluxrefo, raylo, abso3ao, abso3bo, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  raylo, layreffr, abso3ao, abso3bo, kao, sfluxrefo
 if(status/=0) error stop 'sw_kgb25: read failed'
 end subroutine
 subroutine read_sw_kgb26(unit)
 use rrsw_kg26, only : sfluxrefo, raylo
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  raylo, sfluxrefo
 if(status/=0) error stop 'sw_kgb26: read failed'
 end subroutine
 subroutine read_sw_kgb27(unit)
 use rrsw_kg27, only : kao, kbo, sfluxrefo, raylo, scalekur, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  raylo, scalekur, layreffr, kao, kbo, sfluxrefo
 if(status/=0) error stop 'sw_kgb27: read failed'
 end subroutine
 subroutine read_sw_kgb28(unit)
 use rrsw_kg28, only : kao, kbo, sfluxrefo, rayl, strrat, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, strrat, layreffr, kao, kbo, sfluxrefo
 if(status/=0) error stop 'sw_kgb28: read failed'
 end subroutine
 subroutine read_sw_kgb29(unit)
 use rrsw_kg29, only : kao, kbo, selfrefo, forrefo, sfluxrefo, absh2oo, absco2o, rayl, layreffr
 integer, intent(in) :: unit
 integer :: status
 read(unit,iostat=status) &
  rayl, layreffr, absh2oo, absco2o, kao, kbo, selfrefo, forrefo, sfluxrefo
 if(status/=0) error stop 'sw_kgb29: read failed'
 end subroutine
end module
