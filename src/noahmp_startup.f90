! The start of one Noah-MP column: Noah-MP's own init and transfer routines (ConfigVarInitMod ... BiochemVarOutTransferMod
! of physics_noahmp/src and physics_noahmp/drivers/mpas) build a noahmp_type from the NoahmpIO record as MPAS's driver
! does, and install copies it member by member into the lean column (noahmp_state.f90).
module noahmp_startup
use noahmp_state
use NoahmpVarType
use NoahmpIOVarType
use ConfigVarInitMod
use ConfigVarInTransferMod
use ConfigVarOutTransferMod
use ForcingVarInitMod
use ForcingVarInTransferMod
use ForcingVarOutTransferMod
use EnergyVarInitMod
use EnergyVarInTransferMod
use EnergyVarOutTransferMod
use WaterVarInitMod
use WaterVarInTransferMod
use WaterVarOutTransferMod
use BiochemVarInitMod
use BiochemVarInTransferMod
use BiochemVarOutTransferMod
implicit none
private
public :: initialize_noahmp_column
contains
! the NoahmpIO fields MPAS's driver sets before the transfers (one column, i=j=1; soil step = model step, so the
! soil-step accumulators start at 0), the soil layer depths, then the init and in-transfer routines into o
subroutine prepare(io,o)
type(NoahmpIO_type),intent(inout) :: io
type(noahmp_type),intent(inout) :: o
integer :: k
io%i=1;io%j=1;io%ice=0;io%yearlen=365
io%soil_update_steps=1;io%calculate_soil=.true.
io%acc_ssoilxy=0.;io%acc_qinsurxy=0.;io%acc_qsevaxy=0.;io%acc_etranixy=0.
io%acc_dwaterxy=0.;io%acc_prcpxy=0.;io%acc_ecanxy=0.;io%acc_etranxy=0.;io%acc_edirxy=0.
io%zsoil(1)=-io%dzs(1)
do k=2,4
io%zsoil(k)=-io%dzs(k)+io%zsoil(k-1)
enddo
call ConfigVarInitDefault(o)
call ConfigVarInTransfer(o,io)
call ForcingVarInitDefault(o)
call ForcingVarInTransfer(o,io)
call EnergyVarInitDefault(o)
call EnergyVarInTransfer(o,io)
call WaterVarInitDefault(o)
call WaterVarInTransfer(o,io)
call BiochemVarInitDefault(o)
call BiochemVarInTransfer(o,io)
o%config%domain%IndicatorIceSfc=0
end subroutine
! every member of o into the lean column a
subroutine install(o,a)
type(noahmp_type),intent(in) :: o
type(noahmp_column),intent(in) :: a
a%forcing_spechumidityrefheight=o%forcing%spechumidityrefheight
a%forcing_temperatureairrefheight=o%forcing%temperatureairrefheight
a%forcing_windeastwardrefheight=o%forcing%windeastwardrefheight
a%forcing_windnorthwardrefheight=o%forcing%windnorthwardrefheight
a%forcing_radswdownrefheight=o%forcing%radswdownrefheight
a%forcing_radlwdownrefheight=o%forcing%radlwdownrefheight
a%forcing_pressureairrefheight=o%forcing%pressureairrefheight
a%forcing_pressureairsurface=o%forcing%pressureairsurface
a%forcing_precipconvrefheight=o%forcing%precipconvrefheight
a%forcing_precipnonconvrefheight=o%forcing%precipnonconvrefheight
a%forcing_precipshconvrefheight=o%forcing%precipshconvrefheight
a%forcing_precipsnowrefheight=o%forcing%precipsnowrefheight
a%forcing_precipgraupelrefheight=o%forcing%precipgraupelrefheight
a%forcing_preciphailrefheight=o%forcing%preciphailrefheight
a%forcing_temperaturesoilbottom=o%forcing%temperaturesoilbottom
a%config_domain_flagurban=o%config%domain%flagurban
a%config_domain_flagcropland=o%config%domain%flagcropland
a%config_domain_flagdynamiccrop=o%config%domain%flagdynamiccrop
a%config_domain_flagdynamicveg=o%config%domain%flagdynamicveg
a%config_domain_flagsoilprocess=o%config%domain%flagsoilprocess
a%config_domain_gridindexi=o%config%domain%gridindexi
a%config_domain_gridindexj=o%config%domain%gridindexj
a%config_domain_vegtype=o%config%domain%vegtype
a%config_domain_croptype=o%config%domain%croptype
a%config_domain_numsnowlayerneg=o%config%domain%numsnowlayerneg
a%config_domain_indexwaterpoint=o%config%domain%indexwaterpoint
a%config_domain_indexbarrenpoint=o%config%domain%indexbarrenpoint
a%config_domain_indexicepoint=o%config%domain%indexicepoint
a%config_domain_numdayinyear=o%config%domain%numdayinyear
a%config_domain_numsoiltimestep=o%config%domain%numsoiltimestep
a%config_domain_maintimestep=o%config%domain%maintimestep
a%config_domain_soiltimestep=o%config%domain%soiltimestep
a%config_domain_dayjulianinyear=o%config%domain%dayjulianinyear
a%config_domain_cossolarzenithangle=o%config%domain%cossolarzenithangle
a%config_domain_refheightabovesfc=o%config%domain%refheightabovesfc
a%config_domain_latitude=o%config%domain%latitude
a%config_domain_depthsoiltempbottom=o%config%domain%depthsoiltempbottom
if(allocated(o%config%domain%depthsoillayer)) then
a%config_domain_depthsoillayer=o%config%domain%depthsoillayer
else
a%config_domain_depthsoillayer=-9999
endif
if(allocated(o%config%domain%thicknesssnowsoillayer)) then
a%config_domain_thicknesssnowsoillayer=o%config%domain%thicknesssnowsoillayer
else
a%config_domain_thicknesssnowsoillayer=-9999
endif
if(allocated(o%config%domain%depthsnowsoillayer)) then
a%config_domain_depthsnowsoillayer=o%config%domain%depthsnowsoillayer
else
a%config_domain_depthsnowsoillayer=-9999
endif
a%energy_flux_heatlatentcanopy=o%energy%flux%heatlatentcanopy
a%energy_flux_heatlatenttransp=o%energy%flux%heatlatenttransp
a%energy_flux_heatlatentgrd=o%energy%flux%heatlatentgrd
a%energy_flux_heatlatentirrievap=o%energy%flux%heatlatentirrievap
a%energy_flux_heatprecipadvcanopy=o%energy%flux%heatprecipadvcanopy
a%energy_flux_heatprecipadvveggrd=o%energy%flux%heatprecipadvveggrd
a%energy_flux_heatprecipadvbaregrd=o%energy%flux%heatprecipadvbaregrd
a%energy_flux_heatprecipadvsfc=o%energy%flux%heatprecipadvsfc
a%energy_flux_heatsensiblecanopy=o%energy%flux%heatsensiblecanopy
a%energy_flux_heatlatentcanevap=o%energy%flux%heatlatentcanevap
a%energy_flux_heatsensibleveggrd=o%energy%flux%heatsensibleveggrd
a%energy_flux_heatsensiblesfc=o%energy%flux%heatsensiblesfc
a%energy_flux_heatlatentveggrd=o%energy%flux%heatlatentveggrd
a%energy_flux_heatlatentcantransp=o%energy%flux%heatlatentcantransp
a%energy_flux_heatgroundveggrd=o%energy%flux%heatgroundveggrd
a%energy_flux_heatsensiblebaregrd=o%energy%flux%heatsensiblebaregrd
a%energy_flux_heatlatentbaregrd=o%energy%flux%heatlatentbaregrd
a%energy_flux_heatgroundbaregrd=o%energy%flux%heatgroundbaregrd
a%energy_flux_heatgroundtot=o%energy%flux%heatgroundtot
a%energy_flux_heatgroundtotmean=o%energy%flux%heatgroundtotmean
a%energy_flux_heatfromsoilbot=o%energy%flux%heatfromsoilbot
a%energy_flux_heatcanstoragechg=o%energy%flux%heatcanstoragechg
a%energy_flux_heatgroundtotacc=o%energy%flux%heatgroundtotacc
a%energy_flux_radphotoactabssunlit=o%energy%flux%radphotoactabssunlit
a%energy_flux_radphotoactabsshade=o%energy%flux%radphotoactabsshade
a%energy_flux_radswabsveg=o%energy%flux%radswabsveg
a%energy_flux_radswabsgrd=o%energy%flux%radswabsgrd
a%energy_flux_radswabssfc=o%energy%flux%radswabssfc
a%energy_flux_radswreflsfc=o%energy%flux%radswreflsfc
a%energy_flux_radswreflveg=o%energy%flux%radswreflveg
a%energy_flux_radswreflgrd=o%energy%flux%radswreflgrd
a%energy_flux_radlwnetcanopy=o%energy%flux%radlwnetcanopy
a%energy_flux_radlwnetsfc=o%energy%flux%radlwnetsfc
a%energy_flux_radphotoactabscan=o%energy%flux%radphotoactabscan
a%energy_flux_radlwemitsfc=o%energy%flux%radlwemitsfc
a%energy_flux_radlwnetveggrd=o%energy%flux%radlwnetveggrd
a%energy_flux_radlwnetbaregrd=o%energy%flux%radlwnetbaregrd
if(allocated(o%energy%flux%radswabsvegdir)) then
a%energy_flux_radswabsvegdir=o%energy%flux%radswabsvegdir
else
a%energy_flux_radswabsvegdir=-9999
endif
if(allocated(o%energy%flux%radswabsvegdif)) then
a%energy_flux_radswabsvegdif=o%energy%flux%radswabsvegdif
else
a%energy_flux_radswabsvegdif=-9999
endif
if(allocated(o%energy%flux%radswdirtrangrddir)) then
a%energy_flux_radswdirtrangrddir=o%energy%flux%radswdirtrangrddir
else
a%energy_flux_radswdirtrangrddir=-9999
endif
if(allocated(o%energy%flux%radswdirtrangrddif)) then
a%energy_flux_radswdirtrangrddif=o%energy%flux%radswdirtrangrddif
else
a%energy_flux_radswdirtrangrddif=-9999
endif
if(allocated(o%energy%flux%radswdiftrangrddir)) then
a%energy_flux_radswdiftrangrddir=o%energy%flux%radswdiftrangrddir
else
a%energy_flux_radswdiftrangrddir=-9999
endif
if(allocated(o%energy%flux%radswdiftrangrddif)) then
a%energy_flux_radswdiftrangrddif=o%energy%flux%radswdiftrangrddif
else
a%energy_flux_radswdiftrangrddif=-9999
endif
if(allocated(o%energy%flux%radswreflvegdir)) then
a%energy_flux_radswreflvegdir=o%energy%flux%radswreflvegdir
else
a%energy_flux_radswreflvegdir=-9999
endif
if(allocated(o%energy%flux%radswreflvegdif)) then
a%energy_flux_radswreflvegdif=o%energy%flux%radswreflvegdif
else
a%energy_flux_radswreflvegdif=-9999
endif
if(allocated(o%energy%flux%radswreflgrddir)) then
a%energy_flux_radswreflgrddir=o%energy%flux%radswreflgrddir
else
a%energy_flux_radswreflgrddir=-9999
endif
if(allocated(o%energy%flux%radswreflgrddif)) then
a%energy_flux_radswreflgrddif=o%energy%flux%radswreflgrddif
else
a%energy_flux_radswreflgrddif=-9999
endif
if(allocated(o%energy%flux%radswdowndir)) then
a%energy_flux_radswdowndir=o%energy%flux%radswdowndir
else
a%energy_flux_radswdowndir=-9999
endif
if(allocated(o%energy%flux%radswdowndif)) then
a%energy_flux_radswdowndif=o%energy%flux%radswdowndif
else
a%energy_flux_radswdowndif=-9999
endif
if(allocated(o%energy%flux%radswpenetrategrd)) then
a%energy_flux_radswpenetrategrd=o%energy%flux%radswpenetrategrd
else
a%energy_flux_radswpenetrategrd=-9999
endif
a%energy_state_flagfrozencanopy=o%energy%state%flagfrozencanopy
a%energy_state_flagfrozenground=o%energy%state%flagfrozenground
a%energy_state_leafareaindeff=o%energy%state%leafareaindeff
a%energy_state_stemareaindeff=o%energy%state%stemareaindeff
a%energy_state_leafareaindex=o%energy%state%leafareaindex
a%energy_state_stemareaindex=o%energy%state%stemareaindex
a%energy_state_vegareaindeff=o%energy%state%vegareaindeff
a%energy_state_vegfrac=o%energy%state%vegfrac
a%energy_state_temperaturegrd=o%energy%state%temperaturegrd
a%energy_state_temperaturecanopy=o%energy%state%temperaturecanopy
a%energy_state_temperaturesfc=o%energy%state%temperaturesfc
a%energy_state_temperaturerootzone=o%energy%state%temperaturerootzone
a%energy_state_pressurevaporrefheight=o%energy%state%pressurevaporrefheight
a%energy_state_snowagefac=o%energy%state%snowagefac
a%energy_state_snowagenondim=o%energy%state%snowagenondim
a%energy_state_vegareaprojdir=o%energy%state%vegareaprojdir
a%energy_state_gapbtwcanopy=o%energy%state%gapbtwcanopy
a%energy_state_gapincanopy=o%energy%state%gapincanopy
a%energy_state_gapcanopydif=o%energy%state%gapcanopydif
a%energy_state_gapcanopydir=o%energy%state%gapcanopydir
a%energy_state_canopysunlitfrac=o%energy%state%canopysunlitfrac
a%energy_state_canopyshadefrac=o%energy%state%canopyshadefrac
a%energy_state_leafareaindsunlit=o%energy%state%leafareaindsunlit
a%energy_state_leafareaindshade=o%energy%state%leafareaindshade
a%energy_state_vappressatcanopy=o%energy%state%vappressatcanopy
a%energy_state_vappressatgrdveg=o%energy%state%vappressatgrdveg
a%energy_state_vappressatgrdbare=o%energy%state%vappressatgrdbare
a%energy_state_vappressatcantempd=o%energy%state%vappressatcantempd
a%energy_state_vappressatgrdvegtempd=o%energy%state%vappressatgrdvegtempd
a%energy_state_vappressatgrdbaretempd=o%energy%state%vappressatgrdbaretempd
a%energy_state_pressurevaporcanair=o%energy%state%pressurevaporcanair
a%energy_state_pressureatmosco2=o%energy%state%pressureatmosco2
a%energy_state_pressureatmoso2=o%energy%state%pressureatmoso2
a%energy_state_resistancestomatasunlit=o%energy%state%resistancestomatasunlit
a%energy_state_resistancestomatashade=o%energy%state%resistancestomatashade
a%energy_state_densityairrefheight=o%energy%state%densityairrefheight
a%energy_state_temperaturecanopyair=o%energy%state%temperaturecanopyair
a%energy_state_zeroplanedispsfc=o%energy%state%zeroplanedispsfc
a%energy_state_zeroplanedispgrd=o%energy%state%zeroplanedispgrd
a%energy_state_roughlenmomgrd=o%energy%state%roughlenmomgrd
a%energy_state_roughlenmomsfc=o%energy%state%roughlenmomsfc
a%energy_state_roughlenshcanopy=o%energy%state%roughlenshcanopy
a%energy_state_roughlenshveggrd=o%energy%state%roughlenshveggrd
a%energy_state_roughlenshbaregrd=o%energy%state%roughlenshbaregrd
a%energy_state_canopyheight=o%energy%state%canopyheight
a%energy_state_windspdcanopytop=o%energy%state%windspdcanopytop
a%energy_state_frictionvelveg=o%energy%state%frictionvelveg
a%energy_state_frictionvelbare=o%energy%state%frictionvelbare
a%energy_state_windextcoeffcanopy=o%energy%state%windextcoeffcanopy
a%energy_state_mostabparaundcan=o%energy%state%mostabparaundcan
a%energy_state_mostabparaabvcan=o%energy%state%mostabparaabvcan
a%energy_state_mostabparabare=o%energy%state%mostabparabare
a%energy_state_mostabparaveg2m=o%energy%state%mostabparaveg2m
a%energy_state_mostabparabare2m=o%energy%state%mostabparabare2m
a%energy_state_molengthundcan=o%energy%state%molengthundcan
a%energy_state_molengthabvcan=o%energy%state%molengthabvcan
a%energy_state_molengthbare=o%energy%state%molengthbare
a%energy_state_mostabcorrshundcan=o%energy%state%mostabcorrshundcan
a%energy_state_mostabcorrmomabvcan=o%energy%state%mostabcorrmomabvcan
a%energy_state_mostabcorrshabvcan=o%energy%state%mostabcorrshabvcan
a%energy_state_mostabcorrmomveg2m=o%energy%state%mostabcorrmomveg2m
a%energy_state_mostabcorrshveg2m=o%energy%state%mostabcorrshveg2m
a%energy_state_mostabcorrshbare=o%energy%state%mostabcorrshbare
a%energy_state_mostabcorrmombare=o%energy%state%mostabcorrmombare
a%energy_state_mostabcorrmombare2m=o%energy%state%mostabcorrmombare2m
a%energy_state_mostabcorrshbare2m=o%energy%state%mostabcorrshbare2m
a%energy_state_exchcoeffmomsfc=o%energy%state%exchcoeffmomsfc
a%energy_state_exchcoeffmomabvcan=o%energy%state%exchcoeffmomabvcan
a%energy_state_exchcoeffmombare=o%energy%state%exchcoeffmombare
a%energy_state_exchcoeffshsfc=o%energy%state%exchcoeffshsfc
a%energy_state_exchcoeffshabvcan=o%energy%state%exchcoeffshabvcan
a%energy_state_exchcoeffshbare=o%energy%state%exchcoeffshbare
a%energy_state_exchcoeffsh2mvegmo=o%energy%state%exchcoeffsh2mvegmo
a%energy_state_exchcoeffsh2mbaremo=o%energy%state%exchcoeffsh2mbaremo
a%energy_state_exchcoeffsh2mveg=o%energy%state%exchcoeffsh2mveg
a%energy_state_exchcoefflhabvcan=o%energy%state%exchcoefflhabvcan
a%energy_state_exchcoefflhtransp=o%energy%state%exchcoefflhtransp
a%energy_state_exchcoefflhevap=o%energy%state%exchcoefflhevap
a%energy_state_exchcoefflhundcan=o%energy%state%exchcoefflhundcan
a%energy_state_resistancemomundcan=o%energy%state%resistancemomundcan
a%energy_state_resistanceshundcan=o%energy%state%resistanceshundcan
a%energy_state_resistancelhundcan=o%energy%state%resistancelhundcan
a%energy_state_resistancemomabvcan=o%energy%state%resistancemomabvcan
a%energy_state_resistanceshabvcan=o%energy%state%resistanceshabvcan
a%energy_state_resistancelhabvcan=o%energy%state%resistancelhabvcan
a%energy_state_resistancemombaregrd=o%energy%state%resistancemombaregrd
a%energy_state_resistanceshbaregrd=o%energy%state%resistanceshbaregrd
a%energy_state_resistancelhbaregrd=o%energy%state%resistancelhbaregrd
a%energy_state_resistanceleafboundary=o%energy%state%resistanceleafboundary
a%energy_state_temperaturepotrefheight=o%energy%state%temperaturepotrefheight
a%energy_state_windspdrefheight=o%energy%state%windspdrefheight
a%energy_state_emissivityveg=o%energy%state%emissivityveg
a%energy_state_emissivitygrd=o%energy%state%emissivitygrd
a%energy_state_resistancegrdevap=o%energy%state%resistancegrdevap
a%energy_state_psychconstcanopy=o%energy%state%psychconstcanopy
a%energy_state_latheatvapcanopy=o%energy%state%latheatvapcanopy
a%energy_state_psychconstgrd=o%energy%state%psychconstgrd
a%energy_state_latheatvapgrd=o%energy%state%latheatvapgrd
a%energy_state_relhumiditygrd=o%energy%state%relhumiditygrd
a%energy_state_spechumiditysfc=o%energy%state%spechumiditysfc
a%energy_state_spechumiditysfcmean=o%energy%state%spechumiditysfcmean
a%energy_state_spechumidity2mveg=o%energy%state%spechumidity2mveg
a%energy_state_spechumidity2mbare=o%energy%state%spechumidity2mbare
a%energy_state_spechumidity2m=o%energy%state%spechumidity2m
a%energy_state_temperaturegrdveg=o%energy%state%temperaturegrdveg
a%energy_state_temperaturegrdbare=o%energy%state%temperaturegrdbare
a%energy_state_windstressewveg=o%energy%state%windstressewveg
a%energy_state_windstressnsveg=o%energy%state%windstressnsveg
a%energy_state_windstressewbare=o%energy%state%windstressewbare
a%energy_state_windstressnsbare=o%energy%state%windstressnsbare
a%energy_state_windstressewsfc=o%energy%state%windstressewsfc
a%energy_state_windstressnssfc=o%energy%state%windstressnssfc
a%energy_state_temperatureair2mveg=o%energy%state%temperatureair2mveg
a%energy_state_temperatureair2mbare=o%energy%state%temperatureair2mbare
a%energy_state_temperatureair2m=o%energy%state%temperatureair2m
a%energy_state_exchcoeffshleaf=o%energy%state%exchcoeffshleaf
a%energy_state_exchcoeffshundcan=o%energy%state%exchcoeffshundcan
a%energy_state_exchcoeffsh2mbare=o%energy%state%exchcoeffsh2mbare
a%energy_state_refheightabovegrd=o%energy%state%refheightabovegrd
a%energy_state_canopyfracsnowbury=o%energy%state%canopyfracsnowbury
a%energy_state_depthsoiltempbottosno=o%energy%state%depthsoiltempbottosno
a%energy_state_roughlenmomsfctoatm=o%energy%state%roughlenmomsfctoatm
a%energy_state_temperatureradsfc=o%energy%state%temperatureradsfc
a%energy_state_emissivitysfc=o%energy%state%emissivitysfc
a%energy_state_albedosfc=o%energy%state%albedosfc
a%energy_state_energybalanceerror=o%energy%state%energybalanceerror
a%energy_state_radswbalanceerror=o%energy%state%radswbalanceerror
if(allocated(o%energy%state%temperaturesoilsnow)) then
a%energy_state_temperaturesoilsnow=o%energy%state%temperaturesoilsnow
else
a%energy_state_temperaturesoilsnow=-9999
endif
if(allocated(o%energy%state%heatcapacvolsnow)) then
a%energy_state_heatcapacvolsnow=o%energy%state%heatcapacvolsnow
else
a%energy_state_heatcapacvolsnow=-9999
endif
if(allocated(o%energy%state%thermconductsnow)) then
a%energy_state_thermconductsnow=o%energy%state%thermconductsnow
else
a%energy_state_thermconductsnow=-9999
endif
if(allocated(o%energy%state%heatcapacvolsoil)) then
a%energy_state_heatcapacvolsoil=o%energy%state%heatcapacvolsoil
else
a%energy_state_heatcapacvolsoil=-9999
endif
if(allocated(o%energy%state%thermconductsoil)) then
a%energy_state_thermconductsoil=o%energy%state%thermconductsoil
else
a%energy_state_thermconductsoil=-9999
endif
if(allocated(o%energy%state%thermconductsoilsnow)) then
a%energy_state_thermconductsoilsnow=o%energy%state%thermconductsoilsnow
else
a%energy_state_thermconductsoilsnow=-9999
endif
if(allocated(o%energy%state%heatcapacsoilsnow)) then
a%energy_state_heatcapacsoilsnow=o%energy%state%heatcapacsoilsnow
else
a%energy_state_heatcapacsoilsnow=-9999
endif
if(allocated(o%energy%state%phasechgfacsoilsnow)) then
a%energy_state_phasechgfacsoilsnow=o%energy%state%phasechgfacsoilsnow
else
a%energy_state_phasechgfacsoilsnow=-9999
endif
if(allocated(o%energy%state%albedosnowdir)) then
a%energy_state_albedosnowdir=o%energy%state%albedosnowdir
else
a%energy_state_albedosnowdir=-9999
endif
if(allocated(o%energy%state%albedosnowdif)) then
a%energy_state_albedosnowdif=o%energy%state%albedosnowdif
else
a%energy_state_albedosnowdif=-9999
endif
if(allocated(o%energy%state%albedosoildir)) then
a%energy_state_albedosoildir=o%energy%state%albedosoildir
else
a%energy_state_albedosoildir=-9999
endif
if(allocated(o%energy%state%albedosoildif)) then
a%energy_state_albedosoildif=o%energy%state%albedosoildif
else
a%energy_state_albedosoildif=-9999
endif
if(allocated(o%energy%state%albedogrddir)) then
a%energy_state_albedogrddir=o%energy%state%albedogrddir
else
a%energy_state_albedogrddir=-9999
endif
if(allocated(o%energy%state%albedogrddif)) then
a%energy_state_albedogrddif=o%energy%state%albedogrddif
else
a%energy_state_albedogrddif=-9999
endif
if(allocated(o%energy%state%reflectanceveg)) then
a%energy_state_reflectanceveg=o%energy%state%reflectanceveg
else
a%energy_state_reflectanceveg=-9999
endif
if(allocated(o%energy%state%transmittanceveg)) then
a%energy_state_transmittanceveg=o%energy%state%transmittanceveg
else
a%energy_state_transmittanceveg=-9999
endif
if(allocated(o%energy%state%albedosfcdir)) then
a%energy_state_albedosfcdir=o%energy%state%albedosfcdir
else
a%energy_state_albedosfcdir=-9999
endif
if(allocated(o%energy%state%albedosfcdif)) then
a%energy_state_albedosfcdif=o%energy%state%albedosfcdif
else
a%energy_state_albedosfcdif=-9999
endif
a%energy_param_treecrownradius=o%energy%param%treecrownradius
a%energy_param_heightcanopytop=o%energy%param%heightcanopytop
a%energy_param_heightcanopybot=o%energy%param%heightcanopybot
a%energy_param_roughlenmomveg=o%energy%param%roughlenmomveg
a%energy_param_treedensity=o%energy%param%treedensity
a%energy_param_canopyorientindex=o%energy%param%canopyorientindex
a%energy_param_upscattercoeffsnowdir=o%energy%param%upscattercoeffsnowdir
a%energy_param_upscattercoeffsnowdif=o%energy%param%upscattercoeffsnowdif
a%energy_param_soilheatcapacity=o%energy%param%soilheatcapacity
a%energy_param_snowagefacbats=o%energy%param%snowagefacbats
a%energy_param_snowgrowvapfacbats=o%energy%param%snowgrowvapfacbats
a%energy_param_snowsootfacbats=o%energy%param%snowsootfacbats
a%energy_param_snowgrowfrzfacbats=o%energy%param%snowgrowfrzfacbats
a%energy_param_solarzenithadjbats=o%energy%param%solarzenithadjbats
a%energy_param_freshsnoalbvisbats=o%energy%param%freshsnoalbvisbats
a%energy_param_freshsnoalbnirbats=o%energy%param%freshsnoalbnirbats
a%energy_param_snoagefacdifvisbats=o%energy%param%snoagefacdifvisbats
a%energy_param_snoagefacdifnirbats=o%energy%param%snoagefacdifnirbats
a%energy_param_szafacdirvisbats=o%energy%param%szafacdirvisbats
a%energy_param_szafacdirnirbats=o%energy%param%szafacdirnirbats
a%energy_param_conductanceleafmin=o%energy%param%conductanceleafmin
a%energy_param_co2mmconst25c=o%energy%param%co2mmconst25c
a%energy_param_o2mmconst25c=o%energy%param%o2mmconst25c
a%energy_param_co2mmconstq10=o%energy%param%co2mmconstq10
a%energy_param_o2mmconstq10=o%energy%param%o2mmconstq10
a%energy_param_leafdimlength=o%energy%param%leafdimlength
a%energy_param_zilitinkevichcoeff=o%energy%param%zilitinkevichcoeff
a%energy_param_emissivitysnow=o%energy%param%emissivitysnow
a%energy_param_canopywindextfac=o%energy%param%canopywindextfac
a%energy_param_roughlenmomsnow=o%energy%param%roughlenmomsnow
a%energy_param_roughlenmomsoil=o%energy%param%roughlenmomsoil
a%energy_param_roughlenmomlake=o%energy%param%roughlenmomlake
a%energy_param_emissivityicesfc=o%energy%param%emissivityicesfc
a%energy_param_resistancesoilexp=o%energy%param%resistancesoilexp
a%energy_param_resistancesnowsfc=o%energy%param%resistancesnowsfc
a%energy_param_vegfracgreen=o%energy%param%vegfracgreen
a%energy_param_vegfracannmax=o%energy%param%vegfracannmax
a%energy_param_heatcapaccanfac=o%energy%param%heatcapaccanfac
if(allocated(o%energy%param%leafareaindexmon)) then
a%energy_param_leafareaindexmon=o%energy%param%leafareaindexmon
else
a%energy_param_leafareaindexmon=-9999
endif
if(allocated(o%energy%param%stemareaindexmon)) then
a%energy_param_stemareaindexmon=o%energy%param%stemareaindexmon
else
a%energy_param_stemareaindexmon=-9999
endif
if(allocated(o%energy%param%soilquartzfrac)) then
a%energy_param_soilquartzfrac=o%energy%param%soilquartzfrac
else
a%energy_param_soilquartzfrac=-9999
endif
if(allocated(o%energy%param%albedosoilsat)) then
a%energy_param_albedosoilsat=o%energy%param%albedosoilsat
else
a%energy_param_albedosoilsat=-9999
endif
if(allocated(o%energy%param%albedosoildry)) then
a%energy_param_albedosoildry=o%energy%param%albedosoildry
else
a%energy_param_albedosoildry=-9999
endif
if(allocated(o%energy%param%albedolakefrz)) then
a%energy_param_albedolakefrz=o%energy%param%albedolakefrz
else
a%energy_param_albedolakefrz=-9999
endif
if(allocated(o%energy%param%scattercoeffsnow)) then
a%energy_param_scattercoeffsnow=o%energy%param%scattercoeffsnow
else
a%energy_param_scattercoeffsnow=-9999
endif
if(allocated(o%energy%param%reflectanceleaf)) then
a%energy_param_reflectanceleaf=o%energy%param%reflectanceleaf
else
a%energy_param_reflectanceleaf=-9999
endif
if(allocated(o%energy%param%reflectancestem)) then
a%energy_param_reflectancestem=o%energy%param%reflectancestem
else
a%energy_param_reflectancestem=-9999
endif
if(allocated(o%energy%param%transmittanceleaf)) then
a%energy_param_transmittanceleaf=o%energy%param%transmittanceleaf
else
a%energy_param_transmittanceleaf=-9999
endif
if(allocated(o%energy%param%transmittancestem)) then
a%energy_param_transmittancestem=o%energy%param%transmittancestem
else
a%energy_param_transmittancestem=-9999
endif
if(allocated(o%energy%param%emissivitysoillake)) then
a%energy_param_emissivitysoillake=o%energy%param%emissivitysoillake
else
a%energy_param_emissivitysoillake=-9999
endif
a%water_flux_rainfallrefheight=o%water%flux%rainfallrefheight
a%water_flux_snowfallrefheight=o%water%flux%snowfallrefheight
a%water_flux_preciptotrefheight=o%water%flux%preciptotrefheight
a%water_flux_precipconvtotrefheight=o%water%flux%precipconvtotrefheight
a%water_flux_preciplargesclrefheight=o%water%flux%preciplargesclrefheight
a%water_flux_evapcanopynet=o%water%flux%evapcanopynet
a%water_flux_transpiration=o%water%flux%transpiration
a%water_flux_evapcanopyliq=o%water%flux%evapcanopyliq
a%water_flux_dewcanopyliq=o%water%flux%dewcanopyliq
a%water_flux_frostcanopyice=o%water%flux%frostcanopyice
a%water_flux_sublimcanopyice=o%water%flux%sublimcanopyice
a%water_flux_meltcanopyice=o%water%flux%meltcanopyice
a%water_flux_freezecanopyliq=o%water%flux%freezecanopyliq
a%water_flux_snowfallground=o%water%flux%snowfallground
a%water_flux_snowdepthincr=o%water%flux%snowdepthincr
a%water_flux_frostsnowsfcice=o%water%flux%frostsnowsfcice
a%water_flux_sublimsnowsfcice=o%water%flux%sublimsnowsfcice
a%water_flux_rainfallground=o%water%flux%rainfallground
a%water_flux_snowbotoutflow=o%water%flux%snowbotoutflow
a%water_flux_glacierexcessflow=o%water%flux%glacierexcessflow
a%water_flux_irrigationrateflood=o%water%flux%irrigationrateflood
a%water_flux_irrigationratemicro=o%water%flux%irrigationratemicro
a%water_flux_irrigationratesprinkler=o%water%flux%irrigationratesprinkler
a%water_flux_irrievaplosssprinkler=o%water%flux%irrievaplosssprinkler
a%water_flux_soilsfcinflow=o%water%flux%soilsfcinflow
a%water_flux_runoffsurface=o%water%flux%runoffsurface
a%water_flux_runoffsubsurface=o%water%flux%runoffsubsurface
a%water_flux_infilratesfc=o%water%flux%infilratesfc
a%water_flux_evapsoilsfcliq=o%water%flux%evapsoilsfcliq
a%water_flux_drainsoilbot=o%water%flux%drainsoilbot
a%water_flux_tiledrain=o%water%flux%tiledrain
a%water_flux_vaporizegrd=o%water%flux%vaporizegrd
a%water_flux_condensevapgrd=o%water%flux%condensevapgrd
a%water_flux_dewsoilsfcliq=o%water%flux%dewsoilsfcliq
a%water_flux_evapirrisprinkler=o%water%flux%evapirrisprinkler
a%water_flux_interceptcanopyrain=o%water%flux%interceptcanopyrain
a%water_flux_dripcanopyrain=o%water%flux%dripcanopyrain
a%water_flux_throughfallrain=o%water%flux%throughfallrain
a%water_flux_interceptcanopysnow=o%water%flux%interceptcanopysnow
a%water_flux_dripcanopysnow=o%water%flux%dripcanopysnow
a%water_flux_throughfallsnow=o%water%flux%throughfallsnow
a%water_flux_evapgroundnet=o%water%flux%evapgroundnet
a%water_flux_meltgroundsnow=o%water%flux%meltgroundsnow
a%water_flux_watertoatmostotal=o%water%flux%watertoatmostotal
a%water_flux_evapsoilsfcliqacc=o%water%flux%evapsoilsfcliqacc
a%water_flux_soilsfcinflowacc=o%water%flux%soilsfcinflowacc
a%water_flux_sfcwatertotchgacc=o%water%flux%sfcwatertotchgacc
a%water_flux_preciptotacc=o%water%flux%preciptotacc
a%water_flux_evapcanopynetacc=o%water%flux%evapcanopynetacc
a%water_flux_transpirationacc=o%water%flux%transpirationacc
a%water_flux_evapgroundnetacc=o%water%flux%evapgroundnetacc
a%water_flux_evapsoilsfcliqmean=o%water%flux%evapsoilsfcliqmean
a%water_flux_soilsfcinflowmean=o%water%flux%soilsfcinflowmean
if(allocated(o%water%flux%transpwatlosssoil)) then
a%water_flux_transpwatlosssoil=o%water%flux%transpwatlosssoil
else
a%water_flux_transpwatlosssoil=-9999
endif
if(allocated(o%water%flux%transpwatlosssoilacc)) then
a%water_flux_transpwatlosssoilacc=o%water%flux%transpwatlosssoilacc
else
a%water_flux_transpwatlosssoilacc=-9999
endif
if(allocated(o%water%flux%transpwatlosssoilmean)) then
a%water_flux_transpwatlosssoilmean=o%water%flux%transpwatlosssoilmean
else
a%water_flux_transpwatlosssoilmean=-9999
endif
if(allocated(o%water%flux%compactionsnowaging)) then
a%water_flux_compactionsnowaging=o%water%flux%compactionsnowaging
else
a%water_flux_compactionsnowaging=-9999
endif
if(allocated(o%water%flux%compactionsnowburden)) then
a%water_flux_compactionsnowburden=o%water%flux%compactionsnowburden
else
a%water_flux_compactionsnowburden=-9999
endif
if(allocated(o%water%flux%compactionsnowmelt)) then
a%water_flux_compactionsnowmelt=o%water%flux%compactionsnowmelt
else
a%water_flux_compactionsnowmelt=-9999
endif
if(allocated(o%water%flux%compactionsnowtot)) then
a%water_flux_compactionsnowtot=o%water%flux%compactionsnowtot
else
a%water_flux_compactionsnowtot=-9999
endif
a%water_state_irrigationcntsprinkler=o%water%state%irrigationcntsprinkler
a%water_state_irrigationcntmicro=o%water%state%irrigationcntmicro
a%water_state_irrigationcntflood=o%water%state%irrigationcntflood
a%water_state_canopytotalwater=o%water%state%canopytotalwater
a%water_state_canopywetfrac=o%water%state%canopywetfrac
a%water_state_snowfalldensity=o%water%state%snowfalldensity
a%water_state_canopyliqwater=o%water%state%canopyliqwater
a%water_state_canopyice=o%water%state%canopyice
a%water_state_canopyicemax=o%water%state%canopyicemax
a%water_state_canopyliqwatermax=o%water%state%canopyliqwatermax
a%water_state_snowdepth=o%water%state%snowdepth
a%water_state_snowwaterequiv=o%water%state%snowwaterequiv
a%water_state_snowwaterequivprev=o%water%state%snowwaterequivprev
a%water_state_pondsfcthinsnwmelt=o%water%state%pondsfcthinsnwmelt
a%water_state_pondsfcthinsnwcomb=o%water%state%pondsfcthinsnwcomb
a%water_state_pondsfcthinsnwtrans=o%water%state%pondsfcthinsnwtrans
a%water_state_irrigationfracflood=o%water%state%irrigationfracflood
a%water_state_irrigationamtflood=o%water%state%irrigationamtflood
a%water_state_irrigationfracmicro=o%water%state%irrigationfracmicro
a%water_state_irrigationamtmicro=o%water%state%irrigationamtmicro
a%water_state_irrigationfracsprinkler=o%water%state%irrigationfracsprinkler
a%water_state_irrigationamtsprinkler=o%water%state%irrigationamtsprinkler
a%water_state_watertabledepth=o%water%state%watertabledepth
a%water_state_soilicemax=o%water%state%soilicemax
a%water_state_soilliqwatermin=o%water%state%soilliqwatermin
a%water_state_soilimpervfracmax=o%water%state%soilimpervfracmax
a%water_state_soilmoisturetowt=o%water%state%soilmoisturetowt
a%water_state_rechargegwdeepwt=o%water%state%rechargegwdeepwt
a%water_state_soilsaturationexcess=o%water%state%soilsaturationexcess
a%water_state_tiledrainfrac=o%water%state%tiledrainfrac
a%water_state_waterstorageaquifer=o%water%state%waterstorageaquifer
a%water_state_waterstoragelake=o%water%state%waterstoragelake
a%water_state_waterheadsfc=o%water%state%waterheadsfc
a%water_state_irrigationfracgrid=o%water%state%irrigationfracgrid
a%water_state_precipareafrac=o%water%state%precipareafrac
a%water_state_snowcoverfrac=o%water%state%snowcoverfrac
a%water_state_soiltranspfacacc=o%water%state%soiltranspfacacc
a%water_state_frozenprecipfrac=o%water%state%frozenprecipfrac
a%water_state_waterstoragetotbeg=o%water%state%waterstoragetotbeg
a%water_state_waterbalanceerror=o%water%state%waterbalanceerror
a%water_state_waterstoragetotend=o%water%state%waterstoragetotend
if(allocated(o%water%state%indexphasechange)) then
a%water_state_indexphasechange=o%water%state%indexphasechange
else
a%water_state_indexphasechange=-9999
endif
if(allocated(o%water%state%snowice)) then
a%water_state_snowice=o%water%state%snowice
else
a%water_state_snowice=-9999
endif
if(allocated(o%water%state%snowliqwater)) then
a%water_state_snowliqwater=o%water%state%snowliqwater
else
a%water_state_snowliqwater=-9999
endif
if(allocated(o%water%state%snowicefracprev)) then
a%water_state_snowicefracprev=o%water%state%snowicefracprev
else
a%water_state_snowicefracprev=-9999
endif
if(allocated(o%water%state%snowicefrac)) then
a%water_state_snowicefrac=o%water%state%snowicefrac
else
a%water_state_snowicefrac=-9999
endif
if(allocated(o%water%state%soilicefrac)) then
a%water_state_soilicefrac=o%water%state%soilicefrac
else
a%water_state_soilicefrac=-9999
endif
if(allocated(o%water%state%snoweffporosity)) then
a%water_state_snoweffporosity=o%water%state%snoweffporosity
else
a%water_state_snoweffporosity=-9999
endif
if(allocated(o%water%state%soilliqwater)) then
a%water_state_soilliqwater=o%water%state%soilliqwater
else
a%water_state_soilliqwater=-9999
endif
if(allocated(o%water%state%soilice)) then
a%water_state_soilice=o%water%state%soilice
else
a%water_state_soilice=-9999
endif
if(allocated(o%water%state%soilmoisture)) then
a%water_state_soilmoisture=o%water%state%soilmoisture
else
a%water_state_soilmoisture=-9999
endif
if(allocated(o%water%state%soilimpervfrac)) then
a%water_state_soilimpervfrac=o%water%state%soilimpervfrac
else
a%water_state_soilimpervfrac=-9999
endif
if(allocated(o%water%state%soilwatconductivity)) then
a%water_state_soilwatconductivity=o%water%state%soilwatconductivity
else
a%water_state_soilwatconductivity=-9999
endif
if(allocated(o%water%state%soilwatdiffusivity)) then
a%water_state_soilwatdiffusivity=o%water%state%soilwatdiffusivity
else
a%water_state_soilwatdiffusivity=-9999
endif
if(allocated(o%water%state%soileffporosity)) then
a%water_state_soileffporosity=o%water%state%soileffporosity
else
a%water_state_soileffporosity=-9999
endif
if(allocated(o%water%state%soiltranspfac)) then
a%water_state_soiltranspfac=o%water%state%soiltranspfac
else
a%water_state_soiltranspfac=-9999
endif
if(allocated(o%water%state%snowicevol)) then
a%water_state_snowicevol=o%water%state%snowicevol
else
a%water_state_snowicevol=-9999
endif
if(allocated(o%water%state%snowliqwatervol)) then
a%water_state_snowliqwatervol=o%water%state%snowliqwatervol
else
a%water_state_snowliqwatervol=-9999
endif
if(allocated(o%water%state%soilsupercoolwater)) then
a%water_state_soilsupercoolwater=o%water%state%soilsupercoolwater
else
a%water_state_soilsupercoolwater=-9999
endif
if(allocated(o%water%state%soilmatpotential)) then
a%water_state_soilmatpotential=o%water%state%soilmatpotential
else
a%water_state_soilmatpotential=-9999
endif
a%water_param_numsoillayerroot=o%water%param%numsoillayerroot
a%water_param_irristopdaybfharvest=o%water%param%irristopdaybfharvest
a%water_param_canopyliqholdcap=o%water%param%canopyliqholdcap
a%water_param_snowcompactburdenfac=o%water%param%snowcompactburdenfac
a%water_param_snowcompactagingfac1=o%water%param%snowcompactagingfac1
a%water_param_snowcompactagingfac2=o%water%param%snowcompactagingfac2
a%water_param_snowcompactagingfac3=o%water%param%snowcompactagingfac3
a%water_param_snowcompactagingmax=o%water%param%snowcompactagingmax
a%water_param_snowviscositycoeff=o%water%param%snowviscositycoeff
a%water_param_snowliqfracmax=o%water%param%snowliqfracmax
a%water_param_snowliqholdcap=o%water%param%snowliqholdcap
a%water_param_snowliqreleasefac=o%water%param%snowliqreleasefac
a%water_param_irrifloodratefac=o%water%param%irrifloodratefac
a%water_param_irrimicrorate=o%water%param%irrimicrorate
a%water_param_soilinfilmaxcoeff=o%water%param%soilinfilmaxcoeff
a%water_param_soilimpervfraccoeff=o%water%param%soilimpervfraccoeff
a%water_param_soildrainslope=o%water%param%soildrainslope
a%water_param_waterstoragelakemax=o%water%param%waterstoragelakemax
a%water_param_snowateqvmaxglacier=o%water%param%snowateqvmaxglacier
a%water_param_irritriggerlaimin=o%water%param%irritriggerlaimin
a%water_param_soilwatdeficitallow=o%water%param%soilwatdeficitallow
a%water_param_irrifloodlossfrac=o%water%param%irrifloodlossfrac
a%water_param_irrisprinklerrate=o%water%param%irrisprinklerrate
a%water_param_irrifracthreshold=o%water%param%irrifracthreshold
a%water_param_irristopprecipthr=o%water%param%irristopprecipthr
a%water_param_snowfalldensitymax=o%water%param%snowfalldensitymax
a%water_param_snowmassfullcoverold=o%water%param%snowmassfullcoverold
a%water_param_soilmatpotentialwilt=o%water%param%soilmatpotentialwilt
a%water_param_snowmeltfac=o%water%param%snowmeltfac
a%water_param_snowcoverfac=o%water%param%snowcoverfac
if(allocated(o%water%param%soilmoisturesat)) then
a%water_param_soilmoisturesat=o%water%param%soilmoisturesat
else
a%water_param_soilmoisturesat=-9999
endif
if(allocated(o%water%param%soilmoisturewilt)) then
a%water_param_soilmoisturewilt=o%water%param%soilmoisturewilt
else
a%water_param_soilmoisturewilt=-9999
endif
if(allocated(o%water%param%soilmoisturefieldcap)) then
a%water_param_soilmoisturefieldcap=o%water%param%soilmoisturefieldcap
else
a%water_param_soilmoisturefieldcap=-9999
endif
if(allocated(o%water%param%soilwatdiffusivitysat)) then
a%water_param_soilwatdiffusivitysat=o%water%param%soilwatdiffusivitysat
else
a%water_param_soilwatdiffusivitysat=-9999
endif
if(allocated(o%water%param%soilwatconductivitysat)) then
a%water_param_soilwatconductivitysat=o%water%param%soilwatconductivitysat
else
a%water_param_soilwatconductivitysat=-9999
endif
if(allocated(o%water%param%soilexpcoeffb)) then
a%water_param_soilexpcoeffb=o%water%param%soilexpcoeffb
else
a%water_param_soilexpcoeffb=-9999
endif
if(allocated(o%water%param%soilmatpotentialsat)) then
a%water_param_soilmatpotentialsat=o%water%param%soilmatpotentialsat
else
a%water_param_soilmatpotentialsat=-9999
endif
a%biochem_flux_photosyntotal=o%biochem%flux%photosyntotal
a%biochem_flux_photosynleafsunlit=o%biochem%flux%photosynleafsunlit
a%biochem_flux_photosynleafshade=o%biochem%flux%photosynleafshade
a%biochem_state_plantgrowstage=o%biochem%state%plantgrowstage
a%biochem_state_indexgrowseason=o%biochem%state%indexgrowseason
a%biochem_state_nitrogenconcfoliage=o%biochem%state%nitrogenconcfoliage
a%biochem_param_dateplanting=o%biochem%param%dateplanting
a%biochem_param_dateharvest=o%biochem%param%dateharvest
a%biochem_param_quantumefficiency25c=o%biochem%param%quantumefficiency25c
a%biochem_param_carboxylratemax25c=o%biochem%param%carboxylratemax25c
a%biochem_param_carboxylratemaxq10=o%biochem%param%carboxylratemaxq10
a%biochem_param_photosynpathc3=o%biochem%param%photosynpathc3
a%biochem_param_slopeconducttophotosyn=o%biochem%param%slopeconducttophotosyn
a%biochem_param_temperatureminphotosyn=o%biochem%param%temperatureminphotosyn
a%biochem_param_nitrogenconcfoliagemax=o%biochem%param%nitrogenconcfoliagemax
end subroutine
subroutine initialize_noahmp_column(io,a)
type(NoahmpIO_type),intent(inout) :: io
type(noahmp_column),intent(in) :: a
type(noahmp_type) :: initial
if(io%nsoil/=4.or.io%nsnow/=3) error stop 'Noah-MP compile-time layer counts'
call prepare(io,initial)
call install(initial,a)
end subroutine
end module
