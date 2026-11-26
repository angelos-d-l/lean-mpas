! The Noah-MP state of all land cells and a view of one cell. noahmp_state_type holds one array per member of Noah-MP's
! noahmp_type (cells last), with the member path flattened (noahmp%energy%flux%HeatSensibleSfc becomes
! energy_flux_heatsensiblesfc), plus lean's coupling fields (net_shortwave, rain_convective, surface_albedo).
! noahmp_column points at one cell of it under the same names, so that the Noah-MP code of noahmp_core.f90 runs on a
! column with its own expressions. noahmp_opt_sfc is config_noahmp_iopt_sfc, set once per run.
module noahmp_state
 use config,only: ncells
 use Machine,only: kind_noahmp
 implicit none
 integer, protected, save :: noahmp_opt_sfc=1
 logical, private, save :: surface_option_set=.false.
 type noahmp_column
  real(kind=kind_noahmp),pointer :: forcing_spechumidityrefheight
  real(kind=kind_noahmp),pointer :: forcing_temperatureairrefheight
  real(kind=kind_noahmp),pointer :: forcing_windeastwardrefheight
  real(kind=kind_noahmp),pointer :: forcing_windnorthwardrefheight
  real(kind=kind_noahmp),pointer :: forcing_radswdownrefheight
  real(kind=kind_noahmp),pointer :: forcing_radlwdownrefheight
  real(kind=kind_noahmp),pointer :: forcing_pressureairrefheight
  real(kind=kind_noahmp),pointer :: forcing_pressureairsurface
  real(kind=kind_noahmp),pointer :: forcing_precipconvrefheight
  real(kind=kind_noahmp),pointer :: forcing_precipnonconvrefheight
  real(kind=kind_noahmp),pointer :: forcing_precipshconvrefheight
  real(kind=kind_noahmp),pointer :: forcing_precipsnowrefheight
  real(kind=kind_noahmp),pointer :: forcing_precipgraupelrefheight
  real(kind=kind_noahmp),pointer :: forcing_preciphailrefheight
  real(kind=kind_noahmp),pointer :: forcing_temperaturesoilbottom
  logical,pointer :: config_domain_flagurban
  logical,pointer :: config_domain_flagcropland
  logical,pointer :: config_domain_flagdynamiccrop
  logical,pointer :: config_domain_flagdynamicveg
  logical,pointer :: config_domain_flagsoilprocess
  integer,pointer :: config_domain_gridindexi
  integer,pointer :: config_domain_gridindexj
  integer,pointer :: config_domain_vegtype
  integer,pointer :: config_domain_croptype
  integer,pointer :: config_domain_numsnowlayerneg
  integer,pointer :: config_domain_indexwaterpoint
  integer,pointer :: config_domain_indexbarrenpoint
  integer,pointer :: config_domain_indexicepoint
  integer,pointer :: config_domain_numdayinyear
  integer,pointer :: config_domain_numsoiltimestep
  real(kind=kind_noahmp),pointer :: config_domain_maintimestep
  real(kind=kind_noahmp),pointer :: config_domain_soiltimestep
  real(kind=kind_noahmp),pointer :: config_domain_dayjulianinyear
  real(kind=kind_noahmp),pointer :: config_domain_cossolarzenithangle
  real(kind=kind_noahmp),pointer :: config_domain_refheightabovesfc
  real(kind=kind_noahmp),pointer :: config_domain_latitude
  real(kind=kind_noahmp),pointer :: config_domain_depthsoiltempbottom
  real(kind=kind_noahmp),pointer :: config_domain_depthsoillayer(:)
  real(kind=kind_noahmp),pointer :: config_domain_thicknesssnowsoillayer(:)
  real(kind=kind_noahmp),pointer :: config_domain_depthsnowsoillayer(:)
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentcanopy
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatenttransp
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentgrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentirrievap
  real(kind=kind_noahmp),pointer :: energy_flux_heatprecipadvcanopy
  real(kind=kind_noahmp),pointer :: energy_flux_heatprecipadvveggrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatprecipadvbaregrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatprecipadvsfc
  real(kind=kind_noahmp),pointer :: energy_flux_heatsensiblecanopy
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentcanevap
  real(kind=kind_noahmp),pointer :: energy_flux_heatsensibleveggrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatsensiblesfc
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentveggrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentcantransp
  real(kind=kind_noahmp),pointer :: energy_flux_heatgroundveggrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatsensiblebaregrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatlatentbaregrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatgroundbaregrd
  real(kind=kind_noahmp),pointer :: energy_flux_heatgroundtot
  real(kind=kind_noahmp),pointer :: energy_flux_heatgroundtotmean
  real(kind=kind_noahmp),pointer :: energy_flux_heatfromsoilbot
  real(kind=kind_noahmp),pointer :: energy_flux_heatcanstoragechg
  real(kind=kind_noahmp),pointer :: energy_flux_heatgroundtotacc
  real(kind=kind_noahmp),pointer :: energy_flux_radphotoactabssunlit
  real(kind=kind_noahmp),pointer :: energy_flux_radphotoactabsshade
  real(kind=kind_noahmp),pointer :: energy_flux_radswabsveg
  real(kind=kind_noahmp),pointer :: energy_flux_radswabsgrd
  real(kind=kind_noahmp),pointer :: energy_flux_radswabssfc
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflsfc
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflveg
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflgrd
  real(kind=kind_noahmp),pointer :: energy_flux_radlwnetcanopy
  real(kind=kind_noahmp),pointer :: energy_flux_radlwnetsfc
  real(kind=kind_noahmp),pointer :: energy_flux_radphotoactabscan
  real(kind=kind_noahmp),pointer :: energy_flux_radlwemitsfc
  real(kind=kind_noahmp),pointer :: energy_flux_radlwnetveggrd
  real(kind=kind_noahmp),pointer :: energy_flux_radlwnetbaregrd
  real(kind=kind_noahmp),pointer :: energy_flux_radswabsvegdir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswabsvegdif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdirtrangrddir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdirtrangrddif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdiftrangrddir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdiftrangrddif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflvegdir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflvegdif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflgrddir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswreflgrddif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdowndir(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswdowndif(:)
  real(kind=kind_noahmp),pointer :: energy_flux_radswpenetrategrd(:)
  logical,pointer :: energy_state_flagfrozencanopy
  logical,pointer :: energy_state_flagfrozenground
  real(kind=kind_noahmp),pointer :: energy_state_leafareaindeff
  real(kind=kind_noahmp),pointer :: energy_state_stemareaindeff
  real(kind=kind_noahmp),pointer :: energy_state_leafareaindex
  real(kind=kind_noahmp),pointer :: energy_state_stemareaindex
  real(kind=kind_noahmp),pointer :: energy_state_vegareaindeff
  real(kind=kind_noahmp),pointer :: energy_state_vegfrac
  real(kind=kind_noahmp),pointer :: energy_state_temperaturegrd
  real(kind=kind_noahmp),pointer :: energy_state_temperaturecanopy
  real(kind=kind_noahmp),pointer :: energy_state_temperaturesfc
  real(kind=kind_noahmp),pointer :: energy_state_temperaturerootzone
  real(kind=kind_noahmp),pointer :: energy_state_pressurevaporrefheight
  real(kind=kind_noahmp),pointer :: energy_state_snowagefac
  real(kind=kind_noahmp),pointer :: energy_state_snowagenondim
  real(kind=kind_noahmp),pointer :: energy_state_vegareaprojdir
  real(kind=kind_noahmp),pointer :: energy_state_gapbtwcanopy
  real(kind=kind_noahmp),pointer :: energy_state_gapincanopy
  real(kind=kind_noahmp),pointer :: energy_state_gapcanopydif
  real(kind=kind_noahmp),pointer :: energy_state_gapcanopydir
  real(kind=kind_noahmp),pointer :: energy_state_canopysunlitfrac
  real(kind=kind_noahmp),pointer :: energy_state_canopyshadefrac
  real(kind=kind_noahmp),pointer :: energy_state_leafareaindsunlit
  real(kind=kind_noahmp),pointer :: energy_state_leafareaindshade
  real(kind=kind_noahmp),pointer :: energy_state_vappressatcanopy
  real(kind=kind_noahmp),pointer :: energy_state_vappressatgrdveg
  real(kind=kind_noahmp),pointer :: energy_state_vappressatgrdbare
  real(kind=kind_noahmp),pointer :: energy_state_vappressatcantempd
  real(kind=kind_noahmp),pointer :: energy_state_vappressatgrdvegtempd
  real(kind=kind_noahmp),pointer :: energy_state_vappressatgrdbaretempd
  real(kind=kind_noahmp),pointer :: energy_state_pressurevaporcanair
  real(kind=kind_noahmp),pointer :: energy_state_pressureatmosco2
  real(kind=kind_noahmp),pointer :: energy_state_pressureatmoso2
  real(kind=kind_noahmp),pointer :: energy_state_resistancestomatasunlit
  real(kind=kind_noahmp),pointer :: energy_state_resistancestomatashade
  real(kind=kind_noahmp),pointer :: energy_state_densityairrefheight
  real(kind=kind_noahmp),pointer :: energy_state_temperaturecanopyair
  real(kind=kind_noahmp),pointer :: energy_state_zeroplanedispsfc
  real(kind=kind_noahmp),pointer :: energy_state_zeroplanedispgrd
  real(kind=kind_noahmp),pointer :: energy_state_roughlenmomgrd
  real(kind=kind_noahmp),pointer :: energy_state_roughlenmomsfc
  real(kind=kind_noahmp),pointer :: energy_state_roughlenshcanopy
  real(kind=kind_noahmp),pointer :: energy_state_roughlenshveggrd
  real(kind=kind_noahmp),pointer :: energy_state_roughlenshbaregrd
  real(kind=kind_noahmp),pointer :: energy_state_canopyheight
  real(kind=kind_noahmp),pointer :: energy_state_windspdcanopytop
  real(kind=kind_noahmp),pointer :: energy_state_frictionvelveg
  real(kind=kind_noahmp),pointer :: energy_state_frictionvelbare
  real(kind=kind_noahmp),pointer :: energy_state_windextcoeffcanopy
  real(kind=kind_noahmp),pointer :: energy_state_mostabparaundcan
  real(kind=kind_noahmp),pointer :: energy_state_mostabparaabvcan
  real(kind=kind_noahmp),pointer :: energy_state_mostabparabare
  real(kind=kind_noahmp),pointer :: energy_state_mostabparaveg2m
  real(kind=kind_noahmp),pointer :: energy_state_mostabparabare2m
  real(kind=kind_noahmp),pointer :: energy_state_molengthundcan
  real(kind=kind_noahmp),pointer :: energy_state_molengthabvcan
  real(kind=kind_noahmp),pointer :: energy_state_molengthbare
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrshundcan
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrmomabvcan
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrshabvcan
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrmomveg2m
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrshveg2m
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrshbare
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrmombare
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrmombare2m
  real(kind=kind_noahmp),pointer :: energy_state_mostabcorrshbare2m
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffmomsfc
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffmomabvcan
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffmombare
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffshsfc
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffshabvcan
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffshbare
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffsh2mvegmo
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffsh2mbaremo
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffsh2mveg
  real(kind=kind_noahmp),pointer :: energy_state_exchcoefflhabvcan
  real(kind=kind_noahmp),pointer :: energy_state_exchcoefflhtransp
  real(kind=kind_noahmp),pointer :: energy_state_exchcoefflhevap
  real(kind=kind_noahmp),pointer :: energy_state_exchcoefflhundcan
  real(kind=kind_noahmp),pointer :: energy_state_resistancemomundcan
  real(kind=kind_noahmp),pointer :: energy_state_resistanceshundcan
  real(kind=kind_noahmp),pointer :: energy_state_resistancelhundcan
  real(kind=kind_noahmp),pointer :: energy_state_resistancemomabvcan
  real(kind=kind_noahmp),pointer :: energy_state_resistanceshabvcan
  real(kind=kind_noahmp),pointer :: energy_state_resistancelhabvcan
  real(kind=kind_noahmp),pointer :: energy_state_resistancemombaregrd
  real(kind=kind_noahmp),pointer :: energy_state_resistanceshbaregrd
  real(kind=kind_noahmp),pointer :: energy_state_resistancelhbaregrd
  real(kind=kind_noahmp),pointer :: energy_state_resistanceleafboundary
  real(kind=kind_noahmp),pointer :: energy_state_temperaturepotrefheight
  real(kind=kind_noahmp),pointer :: energy_state_windspdrefheight
  real(kind=kind_noahmp),pointer :: energy_state_frictionvelvertveg
  real(kind=kind_noahmp),pointer :: energy_state_frictionvelvertbare
  real(kind=kind_noahmp),pointer :: energy_state_emissivityveg
  real(kind=kind_noahmp),pointer :: energy_state_emissivitygrd
  real(kind=kind_noahmp),pointer :: energy_state_resistancegrdevap
  real(kind=kind_noahmp),pointer :: energy_state_psychconstcanopy
  real(kind=kind_noahmp),pointer :: energy_state_latheatvapcanopy
  real(kind=kind_noahmp),pointer :: energy_state_psychconstgrd
  real(kind=kind_noahmp),pointer :: energy_state_latheatvapgrd
  real(kind=kind_noahmp),pointer :: energy_state_relhumiditygrd
  real(kind=kind_noahmp),pointer :: energy_state_spechumiditysfc
  real(kind=kind_noahmp),pointer :: energy_state_spechumiditysfcmean
  real(kind=kind_noahmp),pointer :: energy_state_spechumidity2mveg
  real(kind=kind_noahmp),pointer :: energy_state_spechumidity2mbare
  real(kind=kind_noahmp),pointer :: energy_state_spechumidity2m
  real(kind=kind_noahmp),pointer :: energy_state_temperaturegrdveg
  real(kind=kind_noahmp),pointer :: energy_state_temperaturegrdbare
  real(kind=kind_noahmp),pointer :: energy_state_windstressewveg
  real(kind=kind_noahmp),pointer :: energy_state_windstressnsveg
  real(kind=kind_noahmp),pointer :: energy_state_windstressewbare
  real(kind=kind_noahmp),pointer :: energy_state_windstressnsbare
  real(kind=kind_noahmp),pointer :: energy_state_windstressewsfc
  real(kind=kind_noahmp),pointer :: energy_state_windstressnssfc
  real(kind=kind_noahmp),pointer :: energy_state_temperatureair2mveg
  real(kind=kind_noahmp),pointer :: energy_state_temperatureair2mbare
  real(kind=kind_noahmp),pointer :: energy_state_temperatureair2m
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffshleaf
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffshundcan
  real(kind=kind_noahmp),pointer :: energy_state_exchcoeffsh2mbare
  real(kind=kind_noahmp),pointer :: energy_state_refheightabovegrd
  real(kind=kind_noahmp),pointer :: energy_state_canopyfracsnowbury
  real(kind=kind_noahmp),pointer :: energy_state_depthsoiltempbottosno
  real(kind=kind_noahmp),pointer :: energy_state_roughlenmomsfctoatm
  real(kind=kind_noahmp),pointer :: energy_state_temperatureradsfc
  real(kind=kind_noahmp),pointer :: energy_state_emissivitysfc
  real(kind=kind_noahmp),pointer :: energy_state_albedosfc
  real(kind=kind_noahmp),pointer :: energy_state_energybalanceerror
  real(kind=kind_noahmp),pointer :: energy_state_radswbalanceerror
  real(kind=kind_noahmp),pointer :: energy_state_temperaturesoilsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_heatcapacvolsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_thermconductsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_heatcapacvolsoil(:)
  real(kind=kind_noahmp),pointer :: energy_state_thermconductsoil(:)
  real(kind=kind_noahmp),pointer :: energy_state_thermconductsoilsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_heatcapacsoilsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_phasechgfacsoilsnow(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosnowdir(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosnowdif(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosoildir(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosoildif(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedogrddir(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedogrddif(:)
  real(kind=kind_noahmp),pointer :: energy_state_reflectanceveg(:)
  real(kind=kind_noahmp),pointer :: energy_state_transmittanceveg(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosfcdir(:)
  real(kind=kind_noahmp),pointer :: energy_state_albedosfcdif(:)
  real(kind=kind_noahmp),pointer :: energy_param_treecrownradius
  real(kind=kind_noahmp),pointer :: energy_param_heightcanopytop
  real(kind=kind_noahmp),pointer :: energy_param_heightcanopybot
  real(kind=kind_noahmp),pointer :: energy_param_roughlenmomveg
  real(kind=kind_noahmp),pointer :: energy_param_treedensity
  real(kind=kind_noahmp),pointer :: energy_param_canopyorientindex
  real(kind=kind_noahmp),pointer :: energy_param_upscattercoeffsnowdir
  real(kind=kind_noahmp),pointer :: energy_param_upscattercoeffsnowdif
  real(kind=kind_noahmp),pointer :: energy_param_soilheatcapacity
  real(kind=kind_noahmp),pointer :: energy_param_snowagefacbats
  real(kind=kind_noahmp),pointer :: energy_param_snowgrowvapfacbats
  real(kind=kind_noahmp),pointer :: energy_param_snowsootfacbats
  real(kind=kind_noahmp),pointer :: energy_param_snowgrowfrzfacbats
  real(kind=kind_noahmp),pointer :: energy_param_solarzenithadjbats
  real(kind=kind_noahmp),pointer :: energy_param_freshsnoalbvisbats
  real(kind=kind_noahmp),pointer :: energy_param_freshsnoalbnirbats
  real(kind=kind_noahmp),pointer :: energy_param_snoagefacdifvisbats
  real(kind=kind_noahmp),pointer :: energy_param_snoagefacdifnirbats
  real(kind=kind_noahmp),pointer :: energy_param_szafacdirvisbats
  real(kind=kind_noahmp),pointer :: energy_param_szafacdirnirbats
  real(kind=kind_noahmp),pointer :: energy_param_conductanceleafmin
  real(kind=kind_noahmp),pointer :: energy_param_co2mmconst25c
  real(kind=kind_noahmp),pointer :: energy_param_o2mmconst25c
  real(kind=kind_noahmp),pointer :: energy_param_co2mmconstq10
  real(kind=kind_noahmp),pointer :: energy_param_o2mmconstq10
  real(kind=kind_noahmp),pointer :: energy_param_leafdimlength
  real(kind=kind_noahmp),pointer :: energy_param_zilitinkevichcoeff
  real(kind=kind_noahmp),pointer :: energy_param_emissivitysnow
  real(kind=kind_noahmp),pointer :: energy_param_canopywindextfac
  real(kind=kind_noahmp),pointer :: energy_param_roughlenmomsnow
  real(kind=kind_noahmp),pointer :: energy_param_roughlenmomsoil
  real(kind=kind_noahmp),pointer :: energy_param_roughlenmomlake
  real(kind=kind_noahmp),pointer :: energy_param_emissivityicesfc
  real(kind=kind_noahmp),pointer :: energy_param_resistancesoilexp
  real(kind=kind_noahmp),pointer :: energy_param_resistancesnowsfc
  real(kind=kind_noahmp),pointer :: energy_param_vegfracgreen
  real(kind=kind_noahmp),pointer :: energy_param_vegfracannmax
  real(kind=kind_noahmp),pointer :: energy_param_heatcapaccanfac
  real(kind=kind_noahmp),pointer :: energy_param_leafareaindexmon(:)
  real(kind=kind_noahmp),pointer :: energy_param_stemareaindexmon(:)
  real(kind=kind_noahmp),pointer :: energy_param_soilquartzfrac(:)
  real(kind=kind_noahmp),pointer :: energy_param_albedosoilsat(:)
  real(kind=kind_noahmp),pointer :: energy_param_albedosoildry(:)
  real(kind=kind_noahmp),pointer :: energy_param_albedolakefrz(:)
  real(kind=kind_noahmp),pointer :: energy_param_scattercoeffsnow(:)
  real(kind=kind_noahmp),pointer :: energy_param_reflectanceleaf(:)
  real(kind=kind_noahmp),pointer :: energy_param_reflectancestem(:)
  real(kind=kind_noahmp),pointer :: energy_param_transmittanceleaf(:)
  real(kind=kind_noahmp),pointer :: energy_param_transmittancestem(:)
  real(kind=kind_noahmp),pointer :: energy_param_emissivitysoillake(:)
  real(kind=kind_noahmp),pointer :: water_flux_rainfallrefheight
  real(kind=kind_noahmp),pointer :: water_flux_snowfallrefheight
  real(kind=kind_noahmp),pointer :: water_flux_preciptotrefheight
  real(kind=kind_noahmp),pointer :: water_flux_precipconvtotrefheight
  real(kind=kind_noahmp),pointer :: water_flux_preciplargesclrefheight
  real(kind=kind_noahmp),pointer :: water_flux_evapcanopynet
  real(kind=kind_noahmp),pointer :: water_flux_transpiration
  real(kind=kind_noahmp),pointer :: water_flux_evapcanopyliq
  real(kind=kind_noahmp),pointer :: water_flux_dewcanopyliq
  real(kind=kind_noahmp),pointer :: water_flux_frostcanopyice
  real(kind=kind_noahmp),pointer :: water_flux_sublimcanopyice
  real(kind=kind_noahmp),pointer :: water_flux_meltcanopyice
  real(kind=kind_noahmp),pointer :: water_flux_freezecanopyliq
  real(kind=kind_noahmp),pointer :: water_flux_snowfallground
  real(kind=kind_noahmp),pointer :: water_flux_snowdepthincr
  real(kind=kind_noahmp),pointer :: water_flux_frostsnowsfcice
  real(kind=kind_noahmp),pointer :: water_flux_sublimsnowsfcice
  real(kind=kind_noahmp),pointer :: water_flux_rainfallground
  real(kind=kind_noahmp),pointer :: water_flux_snowbotoutflow
  real(kind=kind_noahmp),pointer :: water_flux_glacierexcessflow
  real(kind=kind_noahmp),pointer :: water_flux_irrigationrateflood
  real(kind=kind_noahmp),pointer :: water_flux_irrigationratemicro
  real(kind=kind_noahmp),pointer :: water_flux_irrigationratesprinkler
  real(kind=kind_noahmp),pointer :: water_flux_irrievaplosssprinkler
  real(kind=kind_noahmp),pointer :: water_flux_soilsfcinflow
  real(kind=kind_noahmp),pointer :: water_flux_runoffsurface
  real(kind=kind_noahmp),pointer :: water_flux_runoffsubsurface
  real(kind=kind_noahmp),pointer :: water_flux_infilratesfc
  real(kind=kind_noahmp),pointer :: water_flux_evapsoilsfcliq
  real(kind=kind_noahmp),pointer :: water_flux_drainsoilbot
  real(kind=kind_noahmp),pointer :: water_flux_tiledrain
  real(kind=kind_noahmp),pointer :: water_flux_vaporizegrd
  real(kind=kind_noahmp),pointer :: water_flux_condensevapgrd
  real(kind=kind_noahmp),pointer :: water_flux_dewsoilsfcliq
  real(kind=kind_noahmp),pointer :: water_flux_evapirrisprinkler
  real(kind=kind_noahmp),pointer :: water_flux_interceptcanopyrain
  real(kind=kind_noahmp),pointer :: water_flux_dripcanopyrain
  real(kind=kind_noahmp),pointer :: water_flux_throughfallrain
  real(kind=kind_noahmp),pointer :: water_flux_interceptcanopysnow
  real(kind=kind_noahmp),pointer :: water_flux_dripcanopysnow
  real(kind=kind_noahmp),pointer :: water_flux_throughfallsnow
  real(kind=kind_noahmp),pointer :: water_flux_evapgroundnet
  real(kind=kind_noahmp),pointer :: water_flux_meltgroundsnow
  real(kind=kind_noahmp),pointer :: water_flux_watertoatmostotal
  real(kind=kind_noahmp),pointer :: water_flux_evapsoilsfcliqacc
  real(kind=kind_noahmp),pointer :: water_flux_soilsfcinflowacc
  real(kind=kind_noahmp),pointer :: water_flux_sfcwatertotchgacc
  real(kind=kind_noahmp),pointer :: water_flux_preciptotacc
  real(kind=kind_noahmp),pointer :: water_flux_evapcanopynetacc
  real(kind=kind_noahmp),pointer :: water_flux_transpirationacc
  real(kind=kind_noahmp),pointer :: water_flux_evapgroundnetacc
  real(kind=kind_noahmp),pointer :: water_flux_evapsoilsfcliqmean
  real(kind=kind_noahmp),pointer :: water_flux_soilsfcinflowmean
  real(kind=kind_noahmp),pointer :: water_flux_transpwatlosssoil(:)
  real(kind=kind_noahmp),pointer :: water_flux_transpwatlosssoilacc(:)
  real(kind=kind_noahmp),pointer :: water_flux_transpwatlosssoilmean(:)
  real(kind=kind_noahmp),pointer :: water_flux_compactionsnowaging(:)
  real(kind=kind_noahmp),pointer :: water_flux_compactionsnowburden(:)
  real(kind=kind_noahmp),pointer :: water_flux_compactionsnowmelt(:)
  real(kind=kind_noahmp),pointer :: water_flux_compactionsnowtot(:)
  integer,pointer :: water_state_irrigationcntsprinkler
  integer,pointer :: water_state_irrigationcntmicro
  integer,pointer :: water_state_irrigationcntflood
  real(kind=kind_noahmp),pointer :: water_state_canopytotalwater
  real(kind=kind_noahmp),pointer :: water_state_canopywetfrac
  real(kind=kind_noahmp),pointer :: water_state_snowfalldensity
  real(kind=kind_noahmp),pointer :: water_state_canopyliqwater
  real(kind=kind_noahmp),pointer :: water_state_canopyice
  real(kind=kind_noahmp),pointer :: water_state_canopyicemax
  real(kind=kind_noahmp),pointer :: water_state_canopyliqwatermax
  real(kind=kind_noahmp),pointer :: water_state_snowdepth
  real(kind=kind_noahmp),pointer :: water_state_snowwaterequiv
  real(kind=kind_noahmp),pointer :: water_state_snowwaterequivprev
  real(kind=kind_noahmp),pointer :: water_state_pondsfcthinsnwmelt
  real(kind=kind_noahmp),pointer :: water_state_pondsfcthinsnwcomb
  real(kind=kind_noahmp),pointer :: water_state_pondsfcthinsnwtrans
  real(kind=kind_noahmp),pointer :: water_state_irrigationfracflood
  real(kind=kind_noahmp),pointer :: water_state_irrigationamtflood
  real(kind=kind_noahmp),pointer :: water_state_irrigationfracmicro
  real(kind=kind_noahmp),pointer :: water_state_irrigationamtmicro
  real(kind=kind_noahmp),pointer :: water_state_irrigationfracsprinkler
  real(kind=kind_noahmp),pointer :: water_state_irrigationamtsprinkler
  real(kind=kind_noahmp),pointer :: water_state_watertabledepth
  real(kind=kind_noahmp),pointer :: water_state_soilicemax
  real(kind=kind_noahmp),pointer :: water_state_soilliqwatermin
  real(kind=kind_noahmp),pointer :: water_state_soilimpervfracmax
  real(kind=kind_noahmp),pointer :: water_state_soilmoisturetowt
  real(kind=kind_noahmp),pointer :: water_state_rechargegwdeepwt
  real(kind=kind_noahmp),pointer :: water_state_soilsaturationexcess
  real(kind=kind_noahmp),pointer :: water_state_tiledrainfrac
  real(kind=kind_noahmp),pointer :: water_state_waterstorageaquifer
  real(kind=kind_noahmp),pointer :: water_state_waterstoragelake
  real(kind=kind_noahmp),pointer :: water_state_waterheadsfc
  real(kind=kind_noahmp),pointer :: water_state_irrigationfracgrid
  real(kind=kind_noahmp),pointer :: water_state_precipareafrac
  real(kind=kind_noahmp),pointer :: water_state_snowcoverfrac
  real(kind=kind_noahmp),pointer :: water_state_soiltranspfacacc
  real(kind=kind_noahmp),pointer :: water_state_frozenprecipfrac
  real(kind=kind_noahmp),pointer :: water_state_waterstoragetotbeg
  real(kind=kind_noahmp),pointer :: water_state_waterbalanceerror
  real(kind=kind_noahmp),pointer :: water_state_waterstoragetotend
  integer               ,pointer :: water_state_indexphasechange(:)
  real(kind=kind_noahmp),pointer :: water_state_snowice(:)
  real(kind=kind_noahmp),pointer :: water_state_snowliqwater(:)
  real(kind=kind_noahmp),pointer :: water_state_snowicefracprev(:)
  real(kind=kind_noahmp),pointer :: water_state_snowicefrac(:)
  real(kind=kind_noahmp),pointer :: water_state_soilicefrac(:)
  real(kind=kind_noahmp),pointer :: water_state_snoweffporosity(:)
  real(kind=kind_noahmp),pointer :: water_state_soilliqwater(:)
  real(kind=kind_noahmp),pointer :: water_state_soilice(:)
  real(kind=kind_noahmp),pointer :: water_state_soilmoisture(:)
  real(kind=kind_noahmp),pointer :: water_state_soilimpervfrac(:)
  real(kind=kind_noahmp),pointer :: water_state_soilwatconductivity(:)
  real(kind=kind_noahmp),pointer :: water_state_soilwatdiffusivity(:)
  real(kind=kind_noahmp),pointer :: water_state_soileffporosity(:)
  real(kind=kind_noahmp),pointer :: water_state_soiltranspfac(:)
  real(kind=kind_noahmp),pointer :: water_state_snowicevol(:)
  real(kind=kind_noahmp),pointer :: water_state_snowliqwatervol(:)
  real(kind=kind_noahmp),pointer :: water_state_soilsupercoolwater(:)
  real(kind=kind_noahmp),pointer :: water_state_soilmatpotential(:)
  integer,pointer :: water_param_numsoillayerroot
  integer,pointer :: water_param_irristopdaybfharvest
  real(kind=kind_noahmp),pointer :: water_param_canopyliqholdcap
  real(kind=kind_noahmp),pointer :: water_param_snowcompactburdenfac
  real(kind=kind_noahmp),pointer :: water_param_snowcompactagingfac1
  real(kind=kind_noahmp),pointer :: water_param_snowcompactagingfac2
  real(kind=kind_noahmp),pointer :: water_param_snowcompactagingfac3
  real(kind=kind_noahmp),pointer :: water_param_snowcompactagingmax
  real(kind=kind_noahmp),pointer :: water_param_snowviscositycoeff
  real(kind=kind_noahmp),pointer :: water_param_snowliqfracmax
  real(kind=kind_noahmp),pointer :: water_param_snowliqholdcap
  real(kind=kind_noahmp),pointer :: water_param_snowliqreleasefac
  real(kind=kind_noahmp),pointer :: water_param_irrifloodratefac
  real(kind=kind_noahmp),pointer :: water_param_irrimicrorate
  real(kind=kind_noahmp),pointer :: water_param_soilinfilmaxcoeff
  real(kind=kind_noahmp),pointer :: water_param_soilimpervfraccoeff
  real(kind=kind_noahmp),pointer :: water_param_soildrainslope
  real(kind=kind_noahmp),pointer :: water_param_waterstoragelakemax
  real(kind=kind_noahmp),pointer :: water_param_snowateqvmaxglacier
  real(kind=kind_noahmp),pointer :: water_param_irritriggerlaimin
  real(kind=kind_noahmp),pointer :: water_param_soilwatdeficitallow
  real(kind=kind_noahmp),pointer :: water_param_irrifloodlossfrac
  real(kind=kind_noahmp),pointer :: water_param_irrisprinklerrate
  real(kind=kind_noahmp),pointer :: water_param_irrifracthreshold
  real(kind=kind_noahmp),pointer :: water_param_irristopprecipthr
  real(kind=kind_noahmp),pointer :: water_param_snowfalldensitymax
  real(kind=kind_noahmp),pointer :: water_param_snowmassfullcoverold
  real(kind=kind_noahmp),pointer :: water_param_soilmatpotentialwilt
  real(kind=kind_noahmp),pointer :: water_param_snowmeltfac
  real(kind=kind_noahmp),pointer :: water_param_snowcoverfac
  real(kind=kind_noahmp),pointer :: water_param_soilmoisturesat(:)
  real(kind=kind_noahmp),pointer :: water_param_soilmoisturewilt(:)
  real(kind=kind_noahmp),pointer :: water_param_soilmoisturefieldcap(:)
  real(kind=kind_noahmp),pointer :: water_param_soilwatdiffusivitysat(:)
  real(kind=kind_noahmp),pointer :: water_param_soilwatconductivitysat(:)
  real(kind=kind_noahmp),pointer :: water_param_soilexpcoeffb(:)
  real(kind=kind_noahmp),pointer :: water_param_soilmatpotentialsat(:)
  real(kind=kind_noahmp),pointer :: biochem_flux_photosyntotal
  real(kind=kind_noahmp),pointer :: biochem_flux_photosynleafsunlit
  real(kind=kind_noahmp),pointer :: biochem_flux_photosynleafshade
  integer,pointer :: biochem_state_plantgrowstage
  real(kind=kind_noahmp),pointer :: biochem_state_indexgrowseason
  real(kind=kind_noahmp),pointer :: biochem_state_nitrogenconcfoliage
  integer,pointer :: biochem_param_dateplanting
  integer,pointer :: biochem_param_dateharvest
  real(kind=kind_noahmp),pointer :: biochem_param_quantumefficiency25c
  real(kind=kind_noahmp),pointer :: biochem_param_carboxylratemax25c
  real(kind=kind_noahmp),pointer :: biochem_param_carboxylratemaxq10
  real(kind=kind_noahmp),pointer :: biochem_param_photosynpathc3
  real(kind=kind_noahmp),pointer :: biochem_param_slopeconducttophotosyn
  real(kind=kind_noahmp),pointer :: biochem_param_temperatureminphotosyn
  real(kind=kind_noahmp),pointer :: biochem_param_nitrogenconcfoliagemax
  real,pointer :: net_shortwave,rain_convective,surface_albedo
#ifdef LEAN_NOAHMP_SWSPLIT
  real,pointer :: sw_frac_dir_vis
  real,pointer :: sw_frac_dir_nir
  real,pointer :: sw_frac_dif_vis
#endif
 end type
 type noahmp_state_type
  real(kind=kind_noahmp),allocatable :: forcing_spechumidityrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_temperatureairrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_windeastwardrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_windnorthwardrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_radswdownrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_radlwdownrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_pressureairrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_pressureairsurface(:)
  real(kind=kind_noahmp),allocatable :: forcing_precipconvrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_precipnonconvrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_precipshconvrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_precipsnowrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_precipgraupelrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_preciphailrefheight(:)
  real(kind=kind_noahmp),allocatable :: forcing_temperaturesoilbottom(:)
  logical,allocatable :: config_domain_flagurban(:)
  logical,allocatable :: config_domain_flagcropland(:)
  logical,allocatable :: config_domain_flagdynamiccrop(:)
  logical,allocatable :: config_domain_flagdynamicveg(:)
  logical,allocatable :: config_domain_flagsoilprocess(:)
  integer,allocatable :: config_domain_gridindexi(:)
  integer,allocatable :: config_domain_gridindexj(:)
  integer,allocatable :: config_domain_vegtype(:)
  integer,allocatable :: config_domain_croptype(:)
  integer,allocatable :: config_domain_numsnowlayerneg(:)
  integer,allocatable :: config_domain_indexwaterpoint(:)
  integer,allocatable :: config_domain_indexbarrenpoint(:)
  integer,allocatable :: config_domain_indexicepoint(:)
  integer,allocatable :: config_domain_numdayinyear(:)
  integer,allocatable :: config_domain_numsoiltimestep(:)
  real(kind=kind_noahmp),allocatable :: config_domain_maintimestep(:)
  real(kind=kind_noahmp),allocatable :: config_domain_soiltimestep(:)
  real(kind=kind_noahmp),allocatable :: config_domain_dayjulianinyear(:)
  real(kind=kind_noahmp),allocatable :: config_domain_cossolarzenithangle(:)
  real(kind=kind_noahmp),allocatable :: config_domain_refheightabovesfc(:)
  real(kind=kind_noahmp),allocatable :: config_domain_latitude(:)
  real(kind=kind_noahmp),allocatable :: config_domain_depthsoiltempbottom(:)
  real(kind=kind_noahmp),allocatable :: config_domain_depthsoillayer(:,:)
  real(kind=kind_noahmp),allocatable :: config_domain_thicknesssnowsoillayer(:,:)
  real(kind=kind_noahmp),allocatable :: config_domain_depthsnowsoillayer(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatenttransp(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentirrievap(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatprecipadvcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatprecipadvveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatprecipadvbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatprecipadvsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatsensiblecanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentcanevap(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatsensibleveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatsensiblesfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentcantransp(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatgroundveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatsensiblebaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatlatentbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatgroundbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatgroundtot(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatgroundtotmean(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatfromsoilbot(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatcanstoragechg(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_heatgroundtotacc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radphotoactabssunlit(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radphotoactabsshade(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswabsveg(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswabsgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswabssfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflveg(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radlwnetcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radlwnetsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radphotoactabscan(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radlwemitsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radlwnetveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radlwnetbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswabsvegdir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswabsvegdif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdirtrangrddir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdirtrangrddif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdiftrangrddir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdiftrangrddif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflvegdir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflvegdif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflgrddir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswreflgrddif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdowndir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswdowndif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_flux_radswpenetrategrd(:,:)
  logical,allocatable :: energy_state_flagfrozencanopy(:)
  logical,allocatable :: energy_state_flagfrozenground(:)
  real(kind=kind_noahmp),allocatable :: energy_state_leafareaindeff(:)
  real(kind=kind_noahmp),allocatable :: energy_state_stemareaindeff(:)
  real(kind=kind_noahmp),allocatable :: energy_state_leafareaindex(:)
  real(kind=kind_noahmp),allocatable :: energy_state_stemareaindex(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vegareaindeff(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vegfrac(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturegrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturecanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturesfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturerootzone(:)
  real(kind=kind_noahmp),allocatable :: energy_state_pressurevaporrefheight(:)
  real(kind=kind_noahmp),allocatable :: energy_state_snowagefac(:)
  real(kind=kind_noahmp),allocatable :: energy_state_snowagenondim(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vegareaprojdir(:)
  real(kind=kind_noahmp),allocatable :: energy_state_gapbtwcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_gapincanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_gapcanopydif(:)
  real(kind=kind_noahmp),allocatable :: energy_state_gapcanopydir(:)
  real(kind=kind_noahmp),allocatable :: energy_state_canopysunlitfrac(:)
  real(kind=kind_noahmp),allocatable :: energy_state_canopyshadefrac(:)
  real(kind=kind_noahmp),allocatable :: energy_state_leafareaindsunlit(:)
  real(kind=kind_noahmp),allocatable :: energy_state_leafareaindshade(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatgrdveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatgrdbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatcantempd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatgrdvegtempd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_vappressatgrdbaretempd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_pressurevaporcanair(:)
  real(kind=kind_noahmp),allocatable :: energy_state_pressureatmosco2(:)
  real(kind=kind_noahmp),allocatable :: energy_state_pressureatmoso2(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancestomatasunlit(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancestomatashade(:)
  real(kind=kind_noahmp),allocatable :: energy_state_densityairrefheight(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturecanopyair(:)
  real(kind=kind_noahmp),allocatable :: energy_state_zeroplanedispsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_zeroplanedispgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenmomgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenmomsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenshcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenshveggrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenshbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_canopyheight(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windspdcanopytop(:)
  real(kind=kind_noahmp),allocatable :: energy_state_frictionvelveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_frictionvelbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windextcoeffcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabparaundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabparaabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabparabare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabparaveg2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabparabare2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_molengthundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_molengthabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_molengthbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrshundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrmomabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrshabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrmomveg2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrshveg2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrshbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrmombare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrmombare2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_mostabcorrshbare2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffmomsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffmomabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffmombare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffshsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffshabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffshbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffsh2mvegmo(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffsh2mbaremo(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffsh2mveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoefflhabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoefflhtransp(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoefflhevap(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoefflhundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancemomundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistanceshundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancelhundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancemomabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistanceshabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancelhabvcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancemombaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistanceshbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancelhbaregrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistanceleafboundary(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturepotrefheight(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windspdrefheight(:)
  real(kind=kind_noahmp),allocatable :: energy_state_frictionvelvertveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_frictionvelvertbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_emissivityveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_emissivitygrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_resistancegrdevap(:)
  real(kind=kind_noahmp),allocatable :: energy_state_psychconstcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_latheatvapcanopy(:)
  real(kind=kind_noahmp),allocatable :: energy_state_psychconstgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_latheatvapgrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_relhumiditygrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_spechumiditysfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_spechumiditysfcmean(:)
  real(kind=kind_noahmp),allocatable :: energy_state_spechumidity2mveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_spechumidity2mbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_spechumidity2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturegrdveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturegrdbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressewveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressnsveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressewbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressnsbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressewsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_windstressnssfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperatureair2mveg(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperatureair2mbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperatureair2m(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffshleaf(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffshundcan(:)
  real(kind=kind_noahmp),allocatable :: energy_state_exchcoeffsh2mbare(:)
  real(kind=kind_noahmp),allocatable :: energy_state_refheightabovegrd(:)
  real(kind=kind_noahmp),allocatable :: energy_state_canopyfracsnowbury(:)
  real(kind=kind_noahmp),allocatable :: energy_state_depthsoiltempbottosno(:)
  real(kind=kind_noahmp),allocatable :: energy_state_roughlenmomsfctoatm(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperatureradsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_emissivitysfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosfc(:)
  real(kind=kind_noahmp),allocatable :: energy_state_energybalanceerror(:)
  real(kind=kind_noahmp),allocatable :: energy_state_radswbalanceerror(:)
  real(kind=kind_noahmp),allocatable :: energy_state_temperaturesoilsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_heatcapacvolsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_thermconductsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_heatcapacvolsoil(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_thermconductsoil(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_thermconductsoilsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_heatcapacsoilsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_phasechgfacsoilsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosnowdir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosnowdif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosoildir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosoildif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedogrddir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedogrddif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_reflectanceveg(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_transmittanceveg(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosfcdir(:,:)
  real(kind=kind_noahmp),allocatable :: energy_state_albedosfcdif(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_treecrownradius(:)
  real(kind=kind_noahmp),allocatable :: energy_param_heightcanopytop(:)
  real(kind=kind_noahmp),allocatable :: energy_param_heightcanopybot(:)
  real(kind=kind_noahmp),allocatable :: energy_param_roughlenmomveg(:)
  real(kind=kind_noahmp),allocatable :: energy_param_treedensity(:)
  real(kind=kind_noahmp),allocatable :: energy_param_canopyorientindex(:)
  real(kind=kind_noahmp),allocatable :: energy_param_upscattercoeffsnowdir(:)
  real(kind=kind_noahmp),allocatable :: energy_param_upscattercoeffsnowdif(:)
  real(kind=kind_noahmp),allocatable :: energy_param_soilheatcapacity(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snowagefacbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snowgrowvapfacbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snowsootfacbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snowgrowfrzfacbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_solarzenithadjbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_freshsnoalbvisbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_freshsnoalbnirbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snoagefacdifvisbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_snoagefacdifnirbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_szafacdirvisbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_szafacdirnirbats(:)
  real(kind=kind_noahmp),allocatable :: energy_param_conductanceleafmin(:)
  real(kind=kind_noahmp),allocatable :: energy_param_co2mmconst25c(:)
  real(kind=kind_noahmp),allocatable :: energy_param_o2mmconst25c(:)
  real(kind=kind_noahmp),allocatable :: energy_param_co2mmconstq10(:)
  real(kind=kind_noahmp),allocatable :: energy_param_o2mmconstq10(:)
  real(kind=kind_noahmp),allocatable :: energy_param_leafdimlength(:)
  real(kind=kind_noahmp),allocatable :: energy_param_zilitinkevichcoeff(:)
  real(kind=kind_noahmp),allocatable :: energy_param_emissivitysnow(:)
  real(kind=kind_noahmp),allocatable :: energy_param_canopywindextfac(:)
  real(kind=kind_noahmp),allocatable :: energy_param_roughlenmomsnow(:)
  real(kind=kind_noahmp),allocatable :: energy_param_roughlenmomsoil(:)
  real(kind=kind_noahmp),allocatable :: energy_param_roughlenmomlake(:)
  real(kind=kind_noahmp),allocatable :: energy_param_emissivityicesfc(:)
  real(kind=kind_noahmp),allocatable :: energy_param_resistancesoilexp(:)
  real(kind=kind_noahmp),allocatable :: energy_param_resistancesnowsfc(:)
  real(kind=kind_noahmp),allocatable :: energy_param_vegfracgreen(:)
  real(kind=kind_noahmp),allocatable :: energy_param_vegfracannmax(:)
  real(kind=kind_noahmp),allocatable :: energy_param_heatcapaccanfac(:)
  real(kind=kind_noahmp),allocatable :: energy_param_leafareaindexmon(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_stemareaindexmon(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_soilquartzfrac(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_albedosoilsat(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_albedosoildry(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_albedolakefrz(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_scattercoeffsnow(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_reflectanceleaf(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_reflectancestem(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_transmittanceleaf(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_transmittancestem(:,:)
  real(kind=kind_noahmp),allocatable :: energy_param_emissivitysoillake(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_rainfallrefheight(:)
  real(kind=kind_noahmp),allocatable :: water_flux_snowfallrefheight(:)
  real(kind=kind_noahmp),allocatable :: water_flux_preciptotrefheight(:)
  real(kind=kind_noahmp),allocatable :: water_flux_precipconvtotrefheight(:)
  real(kind=kind_noahmp),allocatable :: water_flux_preciplargesclrefheight(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapcanopynet(:)
  real(kind=kind_noahmp),allocatable :: water_flux_transpiration(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapcanopyliq(:)
  real(kind=kind_noahmp),allocatable :: water_flux_dewcanopyliq(:)
  real(kind=kind_noahmp),allocatable :: water_flux_frostcanopyice(:)
  real(kind=kind_noahmp),allocatable :: water_flux_sublimcanopyice(:)
  real(kind=kind_noahmp),allocatable :: water_flux_meltcanopyice(:)
  real(kind=kind_noahmp),allocatable :: water_flux_freezecanopyliq(:)
  real(kind=kind_noahmp),allocatable :: water_flux_snowfallground(:)
  real(kind=kind_noahmp),allocatable :: water_flux_snowdepthincr(:)
  real(kind=kind_noahmp),allocatable :: water_flux_frostsnowsfcice(:)
  real(kind=kind_noahmp),allocatable :: water_flux_sublimsnowsfcice(:)
  real(kind=kind_noahmp),allocatable :: water_flux_rainfallground(:)
  real(kind=kind_noahmp),allocatable :: water_flux_snowbotoutflow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_glacierexcessflow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_irrigationrateflood(:)
  real(kind=kind_noahmp),allocatable :: water_flux_irrigationratemicro(:)
  real(kind=kind_noahmp),allocatable :: water_flux_irrigationratesprinkler(:)
  real(kind=kind_noahmp),allocatable :: water_flux_irrievaplosssprinkler(:)
  real(kind=kind_noahmp),allocatable :: water_flux_soilsfcinflow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_runoffsurface(:)
  real(kind=kind_noahmp),allocatable :: water_flux_runoffsubsurface(:)
  real(kind=kind_noahmp),allocatable :: water_flux_infilratesfc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapsoilsfcliq(:)
  real(kind=kind_noahmp),allocatable :: water_flux_drainsoilbot(:)
  real(kind=kind_noahmp),allocatable :: water_flux_tiledrain(:)
  real(kind=kind_noahmp),allocatable :: water_flux_vaporizegrd(:)
  real(kind=kind_noahmp),allocatable :: water_flux_condensevapgrd(:)
  real(kind=kind_noahmp),allocatable :: water_flux_dewsoilsfcliq(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapirrisprinkler(:)
  real(kind=kind_noahmp),allocatable :: water_flux_interceptcanopyrain(:)
  real(kind=kind_noahmp),allocatable :: water_flux_dripcanopyrain(:)
  real(kind=kind_noahmp),allocatable :: water_flux_throughfallrain(:)
  real(kind=kind_noahmp),allocatable :: water_flux_interceptcanopysnow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_dripcanopysnow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_throughfallsnow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapgroundnet(:)
  real(kind=kind_noahmp),allocatable :: water_flux_meltgroundsnow(:)
  real(kind=kind_noahmp),allocatable :: water_flux_watertoatmostotal(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapsoilsfcliqacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_soilsfcinflowacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_sfcwatertotchgacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_preciptotacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapcanopynetacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_transpirationacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapgroundnetacc(:)
  real(kind=kind_noahmp),allocatable :: water_flux_evapsoilsfcliqmean(:)
  real(kind=kind_noahmp),allocatable :: water_flux_soilsfcinflowmean(:)
  real(kind=kind_noahmp),allocatable :: water_flux_transpwatlosssoil(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_transpwatlosssoilacc(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_transpwatlosssoilmean(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_compactionsnowaging(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_compactionsnowburden(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_compactionsnowmelt(:,:)
  real(kind=kind_noahmp),allocatable :: water_flux_compactionsnowtot(:,:)
  integer,allocatable :: water_state_irrigationcntsprinkler(:)
  integer,allocatable :: water_state_irrigationcntmicro(:)
  integer,allocatable :: water_state_irrigationcntflood(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopytotalwater(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopywetfrac(:)
  real(kind=kind_noahmp),allocatable :: water_state_snowfalldensity(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopyliqwater(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopyice(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopyicemax(:)
  real(kind=kind_noahmp),allocatable :: water_state_canopyliqwatermax(:)
  real(kind=kind_noahmp),allocatable :: water_state_snowdepth(:)
  real(kind=kind_noahmp),allocatable :: water_state_snowwaterequiv(:)
  real(kind=kind_noahmp),allocatable :: water_state_snowwaterequivprev(:)
  real(kind=kind_noahmp),allocatable :: water_state_pondsfcthinsnwmelt(:)
  real(kind=kind_noahmp),allocatable :: water_state_pondsfcthinsnwcomb(:)
  real(kind=kind_noahmp),allocatable :: water_state_pondsfcthinsnwtrans(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationfracflood(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationamtflood(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationfracmicro(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationamtmicro(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationfracsprinkler(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationamtsprinkler(:)
  real(kind=kind_noahmp),allocatable :: water_state_watertabledepth(:)
  real(kind=kind_noahmp),allocatable :: water_state_soilicemax(:)
  real(kind=kind_noahmp),allocatable :: water_state_soilliqwatermin(:)
  real(kind=kind_noahmp),allocatable :: water_state_soilimpervfracmax(:)
  real(kind=kind_noahmp),allocatable :: water_state_soilmoisturetowt(:)
  real(kind=kind_noahmp),allocatable :: water_state_rechargegwdeepwt(:)
  real(kind=kind_noahmp),allocatable :: water_state_soilsaturationexcess(:)
  real(kind=kind_noahmp),allocatable :: water_state_tiledrainfrac(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterstorageaquifer(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterstoragelake(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterheadsfc(:)
  real(kind=kind_noahmp),allocatable :: water_state_irrigationfracgrid(:)
  real(kind=kind_noahmp),allocatable :: water_state_precipareafrac(:)
  real(kind=kind_noahmp),allocatable :: water_state_snowcoverfrac(:)
  real(kind=kind_noahmp),allocatable :: water_state_soiltranspfacacc(:)
  real(kind=kind_noahmp),allocatable :: water_state_frozenprecipfrac(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterstoragetotbeg(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterbalanceerror(:)
  real(kind=kind_noahmp),allocatable :: water_state_waterstoragetotend(:)
  integer               ,allocatable :: water_state_indexphasechange(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowice(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowliqwater(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowicefracprev(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowicefrac(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilicefrac(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snoweffporosity(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilliqwater(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilice(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilmoisture(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilimpervfrac(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilwatconductivity(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilwatdiffusivity(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soileffporosity(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soiltranspfac(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowicevol(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_snowliqwatervol(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilsupercoolwater(:,:)
  real(kind=kind_noahmp),allocatable :: water_state_soilmatpotential(:,:)
  integer,allocatable :: water_param_numsoillayerroot(:)
  integer,allocatable :: water_param_irristopdaybfharvest(:)
  real(kind=kind_noahmp),allocatable :: water_param_canopyliqholdcap(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcompactburdenfac(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcompactagingfac1(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcompactagingfac2(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcompactagingfac3(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcompactagingmax(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowviscositycoeff(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowliqfracmax(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowliqholdcap(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowliqreleasefac(:)
  real(kind=kind_noahmp),allocatable :: water_param_irrifloodratefac(:)
  real(kind=kind_noahmp),allocatable :: water_param_irrimicrorate(:)
  real(kind=kind_noahmp),allocatable :: water_param_soilinfilmaxcoeff(:)
  real(kind=kind_noahmp),allocatable :: water_param_soilimpervfraccoeff(:)
  real(kind=kind_noahmp),allocatable :: water_param_soildrainslope(:)
  real(kind=kind_noahmp),allocatable :: water_param_waterstoragelakemax(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowateqvmaxglacier(:)
  real(kind=kind_noahmp),allocatable :: water_param_irritriggerlaimin(:)
  real(kind=kind_noahmp),allocatable :: water_param_soilwatdeficitallow(:)
  real(kind=kind_noahmp),allocatable :: water_param_irrifloodlossfrac(:)
  real(kind=kind_noahmp),allocatable :: water_param_irrisprinklerrate(:)
  real(kind=kind_noahmp),allocatable :: water_param_irrifracthreshold(:)
  real(kind=kind_noahmp),allocatable :: water_param_irristopprecipthr(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowfalldensitymax(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowmassfullcoverold(:)
  real(kind=kind_noahmp),allocatable :: water_param_soilmatpotentialwilt(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowmeltfac(:)
  real(kind=kind_noahmp),allocatable :: water_param_snowcoverfac(:)
  real(kind=kind_noahmp),allocatable :: water_param_soilmoisturesat(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilmoisturewilt(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilmoisturefieldcap(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilwatdiffusivitysat(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilwatconductivitysat(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilexpcoeffb(:,:)
  real(kind=kind_noahmp),allocatable :: water_param_soilmatpotentialsat(:,:)
  real(kind=kind_noahmp),allocatable :: biochem_flux_photosyntotal(:)
  real(kind=kind_noahmp),allocatable :: biochem_flux_photosynleafsunlit(:)
  real(kind=kind_noahmp),allocatable :: biochem_flux_photosynleafshade(:)
  integer,allocatable :: biochem_state_plantgrowstage(:)
  real(kind=kind_noahmp),allocatable :: biochem_state_indexgrowseason(:)
  real(kind=kind_noahmp),allocatable :: biochem_state_nitrogenconcfoliage(:)
  integer,allocatable :: biochem_param_dateplanting(:)
  integer,allocatable :: biochem_param_dateharvest(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_quantumefficiency25c(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_carboxylratemax25c(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_carboxylratemaxq10(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_photosynpathc3(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_slopeconducttophotosyn(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_temperatureminphotosyn(:)
  real(kind=kind_noahmp),allocatable :: biochem_param_nitrogenconcfoliagemax(:)
#ifdef LEAN_NOAHMP_SWSPLIT
  real,allocatable :: sw_frac_dir_vis(:)
  real,allocatable :: sw_frac_dir_nir(:)
  real,allocatable :: sw_frac_dif_vis(:)
#endif
  real,allocatable :: net_shortwave(:),rain_convective(:),surface_albedo(:)
 end type
contains
 subroutine set_noahmp_opt_sfc(n)
 integer, intent(in) :: n
 if(n/=1.and.n/=2)error stop 'Noah-MP surface exchange must be 1 or 2'
 if(surface_option_set)then
  if(n/=noahmp_opt_sfc)error stop 'Noah-MP surface exchange is set once per run'
  return
 endif
 noahmp_opt_sfc=n
 surface_option_set=.true.
 end subroutine
 ! every array of the state, sized for ncells (the soil and snow layers first where a member has them)
 subroutine allocate_land(s)
 type(noahmp_state_type),intent(inout) :: s
 allocate(s%forcing_spechumidityrefheight(ncells))
 allocate(s%forcing_temperatureairrefheight(ncells))
 allocate(s%forcing_windeastwardrefheight(ncells))
 allocate(s%forcing_windnorthwardrefheight(ncells))
 allocate(s%forcing_radswdownrefheight(ncells))
 allocate(s%forcing_radlwdownrefheight(ncells))
 allocate(s%forcing_pressureairrefheight(ncells))
 allocate(s%forcing_pressureairsurface(ncells))
 allocate(s%forcing_precipconvrefheight(ncells))
 allocate(s%forcing_precipnonconvrefheight(ncells))
 allocate(s%forcing_precipshconvrefheight(ncells))
 allocate(s%forcing_precipsnowrefheight(ncells))
 allocate(s%forcing_precipgraupelrefheight(ncells))
 allocate(s%forcing_preciphailrefheight(ncells))
 allocate(s%forcing_temperaturesoilbottom(ncells))
 allocate(s%config_domain_flagurban(ncells))
 allocate(s%config_domain_flagcropland(ncells))
 allocate(s%config_domain_flagdynamiccrop(ncells))
 allocate(s%config_domain_flagdynamicveg(ncells))
 allocate(s%config_domain_flagsoilprocess(ncells))
 allocate(s%config_domain_gridindexi(ncells))
 allocate(s%config_domain_gridindexj(ncells))
 allocate(s%config_domain_vegtype(ncells))
 allocate(s%config_domain_croptype(ncells))
 allocate(s%config_domain_numsnowlayerneg(ncells))
 allocate(s%config_domain_indexwaterpoint(ncells))
 allocate(s%config_domain_indexbarrenpoint(ncells))
 allocate(s%config_domain_indexicepoint(ncells))
 allocate(s%config_domain_numdayinyear(ncells))
 allocate(s%config_domain_numsoiltimestep(ncells))
 allocate(s%config_domain_maintimestep(ncells))
 allocate(s%config_domain_soiltimestep(ncells))
 allocate(s%config_domain_dayjulianinyear(ncells))
 allocate(s%config_domain_cossolarzenithangle(ncells))
 allocate(s%config_domain_refheightabovesfc(ncells))
 allocate(s%config_domain_latitude(ncells))
 allocate(s%config_domain_depthsoiltempbottom(ncells))
 allocate(s%config_domain_depthsoillayer(1:4,ncells))
 allocate(s%config_domain_thicknesssnowsoillayer(-2:4,ncells))
 allocate(s%config_domain_depthsnowsoillayer(-2:4,ncells))
 allocate(s%energy_flux_heatlatentcanopy(ncells))
 allocate(s%energy_flux_heatlatenttransp(ncells))
 allocate(s%energy_flux_heatlatentgrd(ncells))
 allocate(s%energy_flux_heatlatentirrievap(ncells))
 allocate(s%energy_flux_heatprecipadvcanopy(ncells))
 allocate(s%energy_flux_heatprecipadvveggrd(ncells))
 allocate(s%energy_flux_heatprecipadvbaregrd(ncells))
 allocate(s%energy_flux_heatprecipadvsfc(ncells))
 allocate(s%energy_flux_heatsensiblecanopy(ncells))
 allocate(s%energy_flux_heatlatentcanevap(ncells))
 allocate(s%energy_flux_heatsensibleveggrd(ncells))
 allocate(s%energy_flux_heatsensiblesfc(ncells))
 allocate(s%energy_flux_heatlatentveggrd(ncells))
 allocate(s%energy_flux_heatlatentcantransp(ncells))
 allocate(s%energy_flux_heatgroundveggrd(ncells))
 allocate(s%energy_flux_heatsensiblebaregrd(ncells))
 allocate(s%energy_flux_heatlatentbaregrd(ncells))
 allocate(s%energy_flux_heatgroundbaregrd(ncells))
 allocate(s%energy_flux_heatgroundtot(ncells))
 allocate(s%energy_flux_heatgroundtotmean(ncells))
 allocate(s%energy_flux_heatfromsoilbot(ncells))
 allocate(s%energy_flux_heatcanstoragechg(ncells))
 allocate(s%energy_flux_heatgroundtotacc(ncells))
 allocate(s%energy_flux_radphotoactabssunlit(ncells))
 allocate(s%energy_flux_radphotoactabsshade(ncells))
 allocate(s%energy_flux_radswabsveg(ncells))
 allocate(s%energy_flux_radswabsgrd(ncells))
 allocate(s%energy_flux_radswabssfc(ncells))
 allocate(s%energy_flux_radswreflsfc(ncells))
 allocate(s%energy_flux_radswreflveg(ncells))
 allocate(s%energy_flux_radswreflgrd(ncells))
 allocate(s%energy_flux_radlwnetcanopy(ncells))
 allocate(s%energy_flux_radlwnetsfc(ncells))
 allocate(s%energy_flux_radphotoactabscan(ncells))
 allocate(s%energy_flux_radlwemitsfc(ncells))
 allocate(s%energy_flux_radlwnetveggrd(ncells))
 allocate(s%energy_flux_radlwnetbaregrd(ncells))
 allocate(s%energy_flux_radswabsvegdir(1:2,ncells))
 allocate(s%energy_flux_radswabsvegdif(1:2,ncells))
 allocate(s%energy_flux_radswdirtrangrddir(1:2,ncells))
 allocate(s%energy_flux_radswdirtrangrddif(1:2,ncells))
 allocate(s%energy_flux_radswdiftrangrddir(1:2,ncells))
 allocate(s%energy_flux_radswdiftrangrddif(1:2,ncells))
 allocate(s%energy_flux_radswreflvegdir(1:2,ncells))
 allocate(s%energy_flux_radswreflvegdif(1:2,ncells))
 allocate(s%energy_flux_radswreflgrddir(1:2,ncells))
 allocate(s%energy_flux_radswreflgrddif(1:2,ncells))
 allocate(s%energy_flux_radswdowndir(1:2,ncells))
 allocate(s%energy_flux_radswdowndif(1:2,ncells))
 allocate(s%energy_flux_radswpenetrategrd(-2:4,ncells))
 allocate(s%energy_state_flagfrozencanopy(ncells))
 allocate(s%energy_state_flagfrozenground(ncells))
 allocate(s%energy_state_leafareaindeff(ncells))
 allocate(s%energy_state_stemareaindeff(ncells))
 allocate(s%energy_state_leafareaindex(ncells))
 allocate(s%energy_state_stemareaindex(ncells))
 allocate(s%energy_state_vegareaindeff(ncells))
 allocate(s%energy_state_vegfrac(ncells))
 allocate(s%energy_state_temperaturegrd(ncells))
 allocate(s%energy_state_temperaturecanopy(ncells))
 allocate(s%energy_state_temperaturesfc(ncells))
 allocate(s%energy_state_temperaturerootzone(ncells))
 allocate(s%energy_state_pressurevaporrefheight(ncells))
 allocate(s%energy_state_snowagefac(ncells))
 allocate(s%energy_state_snowagenondim(ncells))
 allocate(s%energy_state_vegareaprojdir(ncells))
 allocate(s%energy_state_gapbtwcanopy(ncells))
 allocate(s%energy_state_gapincanopy(ncells))
 allocate(s%energy_state_gapcanopydif(ncells))
 allocate(s%energy_state_gapcanopydir(ncells))
 allocate(s%energy_state_canopysunlitfrac(ncells))
 allocate(s%energy_state_canopyshadefrac(ncells))
 allocate(s%energy_state_leafareaindsunlit(ncells))
 allocate(s%energy_state_leafareaindshade(ncells))
 allocate(s%energy_state_vappressatcanopy(ncells))
 allocate(s%energy_state_vappressatgrdveg(ncells))
 allocate(s%energy_state_vappressatgrdbare(ncells))
 allocate(s%energy_state_vappressatcantempd(ncells))
 allocate(s%energy_state_vappressatgrdvegtempd(ncells))
 allocate(s%energy_state_vappressatgrdbaretempd(ncells))
 allocate(s%energy_state_pressurevaporcanair(ncells))
 allocate(s%energy_state_pressureatmosco2(ncells))
 allocate(s%energy_state_pressureatmoso2(ncells))
 allocate(s%energy_state_resistancestomatasunlit(ncells))
 allocate(s%energy_state_resistancestomatashade(ncells))
 allocate(s%energy_state_densityairrefheight(ncells))
 allocate(s%energy_state_temperaturecanopyair(ncells))
 allocate(s%energy_state_zeroplanedispsfc(ncells))
 allocate(s%energy_state_zeroplanedispgrd(ncells))
 allocate(s%energy_state_roughlenmomgrd(ncells))
 allocate(s%energy_state_roughlenmomsfc(ncells))
 allocate(s%energy_state_roughlenshcanopy(ncells))
 allocate(s%energy_state_roughlenshveggrd(ncells))
 allocate(s%energy_state_roughlenshbaregrd(ncells))
 allocate(s%energy_state_canopyheight(ncells))
 allocate(s%energy_state_windspdcanopytop(ncells))
 allocate(s%energy_state_frictionvelveg(ncells))
 allocate(s%energy_state_frictionvelbare(ncells))
 allocate(s%energy_state_windextcoeffcanopy(ncells))
 allocate(s%energy_state_mostabparaundcan(ncells))
 allocate(s%energy_state_mostabparaabvcan(ncells))
 allocate(s%energy_state_mostabparabare(ncells))
 allocate(s%energy_state_mostabparaveg2m(ncells))
 allocate(s%energy_state_mostabparabare2m(ncells))
 allocate(s%energy_state_molengthundcan(ncells))
 allocate(s%energy_state_molengthabvcan(ncells))
 allocate(s%energy_state_molengthbare(ncells))
 allocate(s%energy_state_mostabcorrshundcan(ncells))
 allocate(s%energy_state_mostabcorrmomabvcan(ncells))
 allocate(s%energy_state_mostabcorrshabvcan(ncells))
 allocate(s%energy_state_mostabcorrmomveg2m(ncells))
 allocate(s%energy_state_mostabcorrshveg2m(ncells))
 allocate(s%energy_state_mostabcorrshbare(ncells))
 allocate(s%energy_state_mostabcorrmombare(ncells))
 allocate(s%energy_state_mostabcorrmombare2m(ncells))
 allocate(s%energy_state_mostabcorrshbare2m(ncells))
 allocate(s%energy_state_exchcoeffmomsfc(ncells))
 allocate(s%energy_state_exchcoeffmomabvcan(ncells))
 allocate(s%energy_state_exchcoeffmombare(ncells))
 allocate(s%energy_state_exchcoeffshsfc(ncells))
 allocate(s%energy_state_exchcoeffshabvcan(ncells))
 allocate(s%energy_state_exchcoeffshbare(ncells))
 allocate(s%energy_state_exchcoeffsh2mvegmo(ncells))
 allocate(s%energy_state_exchcoeffsh2mbaremo(ncells))
 allocate(s%energy_state_exchcoeffsh2mveg(ncells))
 allocate(s%energy_state_exchcoefflhabvcan(ncells))
 allocate(s%energy_state_exchcoefflhtransp(ncells))
 allocate(s%energy_state_exchcoefflhevap(ncells))
 allocate(s%energy_state_exchcoefflhundcan(ncells))
 allocate(s%energy_state_resistancemomundcan(ncells))
 allocate(s%energy_state_resistanceshundcan(ncells))
 allocate(s%energy_state_resistancelhundcan(ncells))
 allocate(s%energy_state_resistancemomabvcan(ncells))
 allocate(s%energy_state_resistanceshabvcan(ncells))
 allocate(s%energy_state_resistancelhabvcan(ncells))
 allocate(s%energy_state_resistancemombaregrd(ncells))
 allocate(s%energy_state_resistanceshbaregrd(ncells))
 allocate(s%energy_state_resistancelhbaregrd(ncells))
 allocate(s%energy_state_resistanceleafboundary(ncells))
 allocate(s%energy_state_temperaturepotrefheight(ncells))
 allocate(s%energy_state_windspdrefheight(ncells))
 allocate(s%energy_state_frictionvelvertveg(ncells))
 allocate(s%energy_state_frictionvelvertbare(ncells))
 allocate(s%energy_state_emissivityveg(ncells))
 allocate(s%energy_state_emissivitygrd(ncells))
 allocate(s%energy_state_resistancegrdevap(ncells))
 allocate(s%energy_state_psychconstcanopy(ncells))
 allocate(s%energy_state_latheatvapcanopy(ncells))
 allocate(s%energy_state_psychconstgrd(ncells))
 allocate(s%energy_state_latheatvapgrd(ncells))
 allocate(s%energy_state_relhumiditygrd(ncells))
 allocate(s%energy_state_spechumiditysfc(ncells))
 allocate(s%energy_state_spechumiditysfcmean(ncells))
 allocate(s%energy_state_spechumidity2mveg(ncells))
 allocate(s%energy_state_spechumidity2mbare(ncells))
 allocate(s%energy_state_spechumidity2m(ncells))
 allocate(s%energy_state_temperaturegrdveg(ncells))
 allocate(s%energy_state_temperaturegrdbare(ncells))
 allocate(s%energy_state_windstressewveg(ncells))
 allocate(s%energy_state_windstressnsveg(ncells))
 allocate(s%energy_state_windstressewbare(ncells))
 allocate(s%energy_state_windstressnsbare(ncells))
 allocate(s%energy_state_windstressewsfc(ncells))
 allocate(s%energy_state_windstressnssfc(ncells))
 allocate(s%energy_state_temperatureair2mveg(ncells))
 allocate(s%energy_state_temperatureair2mbare(ncells))
 allocate(s%energy_state_temperatureair2m(ncells))
 allocate(s%energy_state_exchcoeffshleaf(ncells))
 allocate(s%energy_state_exchcoeffshundcan(ncells))
 allocate(s%energy_state_exchcoeffsh2mbare(ncells))
 allocate(s%energy_state_refheightabovegrd(ncells))
 allocate(s%energy_state_canopyfracsnowbury(ncells))
 allocate(s%energy_state_depthsoiltempbottosno(ncells))
 allocate(s%energy_state_roughlenmomsfctoatm(ncells))
 allocate(s%energy_state_temperatureradsfc(ncells))
 allocate(s%energy_state_emissivitysfc(ncells))
 allocate(s%energy_state_albedosfc(ncells))
 allocate(s%energy_state_energybalanceerror(ncells))
 allocate(s%energy_state_radswbalanceerror(ncells))
 allocate(s%energy_state_temperaturesoilsnow(-2:4,ncells))
 allocate(s%energy_state_heatcapacvolsnow(-2:0,ncells))
 allocate(s%energy_state_thermconductsnow(-2:0,ncells))
 allocate(s%energy_state_heatcapacvolsoil(1:4,ncells))
 allocate(s%energy_state_thermconductsoil(1:4,ncells))
 allocate(s%energy_state_thermconductsoilsnow(-2:4,ncells))
 allocate(s%energy_state_heatcapacsoilsnow(-2:4,ncells))
 allocate(s%energy_state_phasechgfacsoilsnow(-2:4,ncells))
 allocate(s%energy_state_albedosnowdir(1:2,ncells))
 allocate(s%energy_state_albedosnowdif(1:2,ncells))
 allocate(s%energy_state_albedosoildir(1:2,ncells))
 allocate(s%energy_state_albedosoildif(1:2,ncells))
 allocate(s%energy_state_albedogrddir(1:2,ncells))
 allocate(s%energy_state_albedogrddif(1:2,ncells))
 allocate(s%energy_state_reflectanceveg(1:2,ncells))
 allocate(s%energy_state_transmittanceveg(1:2,ncells))
 allocate(s%energy_state_albedosfcdir(1:2,ncells))
 allocate(s%energy_state_albedosfcdif(1:2,ncells))
 allocate(s%energy_param_treecrownradius(ncells))
 allocate(s%energy_param_heightcanopytop(ncells))
 allocate(s%energy_param_heightcanopybot(ncells))
 allocate(s%energy_param_roughlenmomveg(ncells))
 allocate(s%energy_param_treedensity(ncells))
 allocate(s%energy_param_canopyorientindex(ncells))
 allocate(s%energy_param_upscattercoeffsnowdir(ncells))
 allocate(s%energy_param_upscattercoeffsnowdif(ncells))
 allocate(s%energy_param_soilheatcapacity(ncells))
 allocate(s%energy_param_snowagefacbats(ncells))
 allocate(s%energy_param_snowgrowvapfacbats(ncells))
 allocate(s%energy_param_snowsootfacbats(ncells))
 allocate(s%energy_param_snowgrowfrzfacbats(ncells))
 allocate(s%energy_param_solarzenithadjbats(ncells))
 allocate(s%energy_param_freshsnoalbvisbats(ncells))
 allocate(s%energy_param_freshsnoalbnirbats(ncells))
 allocate(s%energy_param_snoagefacdifvisbats(ncells))
 allocate(s%energy_param_snoagefacdifnirbats(ncells))
 allocate(s%energy_param_szafacdirvisbats(ncells))
 allocate(s%energy_param_szafacdirnirbats(ncells))
 allocate(s%energy_param_conductanceleafmin(ncells))
 allocate(s%energy_param_co2mmconst25c(ncells))
 allocate(s%energy_param_o2mmconst25c(ncells))
 allocate(s%energy_param_co2mmconstq10(ncells))
 allocate(s%energy_param_o2mmconstq10(ncells))
 allocate(s%energy_param_leafdimlength(ncells))
 allocate(s%energy_param_zilitinkevichcoeff(ncells))
 allocate(s%energy_param_emissivitysnow(ncells))
 allocate(s%energy_param_canopywindextfac(ncells))
 allocate(s%energy_param_roughlenmomsnow(ncells))
 allocate(s%energy_param_roughlenmomsoil(ncells))
 allocate(s%energy_param_roughlenmomlake(ncells))
 allocate(s%energy_param_emissivityicesfc(ncells))
 allocate(s%energy_param_resistancesoilexp(ncells))
 allocate(s%energy_param_resistancesnowsfc(ncells))
 allocate(s%energy_param_vegfracgreen(ncells))
 allocate(s%energy_param_vegfracannmax(ncells))
 allocate(s%energy_param_heatcapaccanfac(ncells))
 allocate(s%energy_param_leafareaindexmon(1:12,ncells))
 allocate(s%energy_param_stemareaindexmon(1:12,ncells))
 allocate(s%energy_param_soilquartzfrac(1:4,ncells))
 allocate(s%energy_param_albedosoilsat(1:2,ncells))
 allocate(s%energy_param_albedosoildry(1:2,ncells))
 allocate(s%energy_param_albedolakefrz(1:2,ncells))
 allocate(s%energy_param_scattercoeffsnow(1:2,ncells))
 allocate(s%energy_param_reflectanceleaf(1:2,ncells))
 allocate(s%energy_param_reflectancestem(1:2,ncells))
 allocate(s%energy_param_transmittanceleaf(1:2,ncells))
 allocate(s%energy_param_transmittancestem(1:2,ncells))
 allocate(s%energy_param_emissivitysoillake(1:2,ncells))
 allocate(s%water_flux_rainfallrefheight(ncells))
 allocate(s%water_flux_snowfallrefheight(ncells))
 allocate(s%water_flux_preciptotrefheight(ncells))
 allocate(s%water_flux_precipconvtotrefheight(ncells))
 allocate(s%water_flux_preciplargesclrefheight(ncells))
 allocate(s%water_flux_evapcanopynet(ncells))
 allocate(s%water_flux_transpiration(ncells))
 allocate(s%water_flux_evapcanopyliq(ncells))
 allocate(s%water_flux_dewcanopyliq(ncells))
 allocate(s%water_flux_frostcanopyice(ncells))
 allocate(s%water_flux_sublimcanopyice(ncells))
 allocate(s%water_flux_meltcanopyice(ncells))
 allocate(s%water_flux_freezecanopyliq(ncells))
 allocate(s%water_flux_snowfallground(ncells))
 allocate(s%water_flux_snowdepthincr(ncells))
 allocate(s%water_flux_frostsnowsfcice(ncells))
 allocate(s%water_flux_sublimsnowsfcice(ncells))
 allocate(s%water_flux_rainfallground(ncells))
 allocate(s%water_flux_snowbotoutflow(ncells))
 allocate(s%water_flux_glacierexcessflow(ncells))
 allocate(s%water_flux_irrigationrateflood(ncells))
 allocate(s%water_flux_irrigationratemicro(ncells))
 allocate(s%water_flux_irrigationratesprinkler(ncells))
 allocate(s%water_flux_irrievaplosssprinkler(ncells))
 allocate(s%water_flux_soilsfcinflow(ncells))
 allocate(s%water_flux_runoffsurface(ncells))
 allocate(s%water_flux_runoffsubsurface(ncells))
 allocate(s%water_flux_infilratesfc(ncells))
 allocate(s%water_flux_evapsoilsfcliq(ncells))
 allocate(s%water_flux_drainsoilbot(ncells))
 allocate(s%water_flux_tiledrain(ncells))
 allocate(s%water_flux_vaporizegrd(ncells))
 allocate(s%water_flux_condensevapgrd(ncells))
 allocate(s%water_flux_dewsoilsfcliq(ncells))
 allocate(s%water_flux_evapirrisprinkler(ncells))
 allocate(s%water_flux_interceptcanopyrain(ncells))
 allocate(s%water_flux_dripcanopyrain(ncells))
 allocate(s%water_flux_throughfallrain(ncells))
 allocate(s%water_flux_interceptcanopysnow(ncells))
 allocate(s%water_flux_dripcanopysnow(ncells))
 allocate(s%water_flux_throughfallsnow(ncells))
 allocate(s%water_flux_evapgroundnet(ncells))
 allocate(s%water_flux_meltgroundsnow(ncells))
 allocate(s%water_flux_watertoatmostotal(ncells))
 allocate(s%water_flux_evapsoilsfcliqacc(ncells))
 allocate(s%water_flux_soilsfcinflowacc(ncells))
 allocate(s%water_flux_sfcwatertotchgacc(ncells))
 allocate(s%water_flux_preciptotacc(ncells))
 allocate(s%water_flux_evapcanopynetacc(ncells))
 allocate(s%water_flux_transpirationacc(ncells))
 allocate(s%water_flux_evapgroundnetacc(ncells))
 allocate(s%water_flux_evapsoilsfcliqmean(ncells))
 allocate(s%water_flux_soilsfcinflowmean(ncells))
 allocate(s%water_flux_transpwatlosssoil(1:4,ncells))
 allocate(s%water_flux_transpwatlosssoilacc(1:4,ncells))
 allocate(s%water_flux_transpwatlosssoilmean(1:4,ncells))
 allocate(s%water_flux_compactionsnowaging(-2:0,ncells))
 allocate(s%water_flux_compactionsnowburden(-2:0,ncells))
 allocate(s%water_flux_compactionsnowmelt(-2:0,ncells))
 allocate(s%water_flux_compactionsnowtot(-2:0,ncells))
 allocate(s%water_state_irrigationcntsprinkler(ncells))
 allocate(s%water_state_irrigationcntmicro(ncells))
 allocate(s%water_state_irrigationcntflood(ncells))
 allocate(s%water_state_canopytotalwater(ncells))
 allocate(s%water_state_canopywetfrac(ncells))
 allocate(s%water_state_snowfalldensity(ncells))
 allocate(s%water_state_canopyliqwater(ncells))
 allocate(s%water_state_canopyice(ncells))
 allocate(s%water_state_canopyicemax(ncells))
 allocate(s%water_state_canopyliqwatermax(ncells))
 allocate(s%water_state_snowdepth(ncells))
 allocate(s%water_state_snowwaterequiv(ncells))
 allocate(s%water_state_snowwaterequivprev(ncells))
 allocate(s%water_state_pondsfcthinsnwmelt(ncells))
 allocate(s%water_state_pondsfcthinsnwcomb(ncells))
 allocate(s%water_state_pondsfcthinsnwtrans(ncells))
 allocate(s%water_state_irrigationfracflood(ncells))
 allocate(s%water_state_irrigationamtflood(ncells))
 allocate(s%water_state_irrigationfracmicro(ncells))
 allocate(s%water_state_irrigationamtmicro(ncells))
 allocate(s%water_state_irrigationfracsprinkler(ncells))
 allocate(s%water_state_irrigationamtsprinkler(ncells))
 allocate(s%water_state_watertabledepth(ncells))
 allocate(s%water_state_soilicemax(ncells))
 allocate(s%water_state_soilliqwatermin(ncells))
 allocate(s%water_state_soilimpervfracmax(ncells))
 allocate(s%water_state_soilmoisturetowt(ncells))
 allocate(s%water_state_rechargegwdeepwt(ncells))
 allocate(s%water_state_soilsaturationexcess(ncells))
 allocate(s%water_state_tiledrainfrac(ncells))
 allocate(s%water_state_waterstorageaquifer(ncells))
 allocate(s%water_state_waterstoragelake(ncells))
 allocate(s%water_state_waterheadsfc(ncells))
 allocate(s%water_state_irrigationfracgrid(ncells))
 allocate(s%water_state_precipareafrac(ncells))
 allocate(s%water_state_snowcoverfrac(ncells))
 allocate(s%water_state_soiltranspfacacc(ncells))
 allocate(s%water_state_frozenprecipfrac(ncells))
 allocate(s%water_state_waterstoragetotbeg(ncells))
 allocate(s%water_state_waterbalanceerror(ncells))
 allocate(s%water_state_waterstoragetotend(ncells))
 allocate(s%water_state_indexphasechange(-2:4,ncells))
 allocate(s%water_state_snowice(-2:0,ncells))
 allocate(s%water_state_snowliqwater(-2:0,ncells))
 allocate(s%water_state_snowicefracprev(-2:0,ncells))
 allocate(s%water_state_snowicefrac(-2:0,ncells))
 allocate(s%water_state_soilicefrac(1:4,ncells))
 allocate(s%water_state_snoweffporosity(-2:0,ncells))
 allocate(s%water_state_soilliqwater(1:4,ncells))
 allocate(s%water_state_soilice(1:4,ncells))
 allocate(s%water_state_soilmoisture(1:4,ncells))
 allocate(s%water_state_soilimpervfrac(1:4,ncells))
 allocate(s%water_state_soilwatconductivity(1:4,ncells))
 allocate(s%water_state_soilwatdiffusivity(1:4,ncells))
 allocate(s%water_state_soileffporosity(1:4,ncells))
 allocate(s%water_state_soiltranspfac(1:4,ncells))
 allocate(s%water_state_snowicevol(-2:0,ncells))
 allocate(s%water_state_snowliqwatervol(-2:0,ncells))
 allocate(s%water_state_soilsupercoolwater(-2:4,ncells))
 allocate(s%water_state_soilmatpotential(1:4,ncells))
 allocate(s%water_param_numsoillayerroot(ncells))
 allocate(s%water_param_irristopdaybfharvest(ncells))
 allocate(s%water_param_canopyliqholdcap(ncells))
 allocate(s%water_param_snowcompactburdenfac(ncells))
 allocate(s%water_param_snowcompactagingfac1(ncells))
 allocate(s%water_param_snowcompactagingfac2(ncells))
 allocate(s%water_param_snowcompactagingfac3(ncells))
 allocate(s%water_param_snowcompactagingmax(ncells))
 allocate(s%water_param_snowviscositycoeff(ncells))
 allocate(s%water_param_snowliqfracmax(ncells))
 allocate(s%water_param_snowliqholdcap(ncells))
 allocate(s%water_param_snowliqreleasefac(ncells))
 allocate(s%water_param_irrifloodratefac(ncells))
 allocate(s%water_param_irrimicrorate(ncells))
 allocate(s%water_param_soilinfilmaxcoeff(ncells))
 allocate(s%water_param_soilimpervfraccoeff(ncells))
 allocate(s%water_param_soildrainslope(ncells))
 allocate(s%water_param_waterstoragelakemax(ncells))
 allocate(s%water_param_snowateqvmaxglacier(ncells))
 allocate(s%water_param_irritriggerlaimin(ncells))
 allocate(s%water_param_soilwatdeficitallow(ncells))
 allocate(s%water_param_irrifloodlossfrac(ncells))
 allocate(s%water_param_irrisprinklerrate(ncells))
 allocate(s%water_param_irrifracthreshold(ncells))
 allocate(s%water_param_irristopprecipthr(ncells))
 allocate(s%water_param_snowfalldensitymax(ncells))
 allocate(s%water_param_snowmassfullcoverold(ncells))
 allocate(s%water_param_soilmatpotentialwilt(ncells))
 allocate(s%water_param_snowmeltfac(ncells))
 allocate(s%water_param_snowcoverfac(ncells))
 allocate(s%water_param_soilmoisturesat(1:4,ncells))
 allocate(s%water_param_soilmoisturewilt(1:4,ncells))
 allocate(s%water_param_soilmoisturefieldcap(1:4,ncells))
 allocate(s%water_param_soilwatdiffusivitysat(1:4,ncells))
 allocate(s%water_param_soilwatconductivitysat(1:4,ncells))
 allocate(s%water_param_soilexpcoeffb(1:4,ncells))
 allocate(s%water_param_soilmatpotentialsat(1:4,ncells))
 allocate(s%biochem_flux_photosyntotal(ncells))
 allocate(s%biochem_flux_photosynleafsunlit(ncells))
 allocate(s%biochem_flux_photosynleafshade(ncells))
 allocate(s%biochem_state_plantgrowstage(ncells))
 allocate(s%biochem_state_indexgrowseason(ncells))
 allocate(s%biochem_state_nitrogenconcfoliage(ncells))
 allocate(s%biochem_param_dateplanting(ncells))
 allocate(s%biochem_param_dateharvest(ncells))
 allocate(s%biochem_param_quantumefficiency25c(ncells))
 allocate(s%biochem_param_carboxylratemax25c(ncells))
 allocate(s%biochem_param_carboxylratemaxq10(ncells))
 allocate(s%biochem_param_photosynpathc3(ncells))
 allocate(s%biochem_param_slopeconducttophotosyn(ncells))
 allocate(s%biochem_param_temperatureminphotosyn(ncells))
 allocate(s%biochem_param_nitrogenconcfoliagemax(ncells))
#ifdef LEAN_NOAHMP_SWSPLIT
 allocate(s%sw_frac_dir_vis(ncells))
 allocate(s%sw_frac_dir_nir(ncells))
 allocate(s%sw_frac_dif_vis(ncells))
#endif
 allocate(s%net_shortwave(ncells))
 allocate(s%rain_convective(ncells))
 allocate(s%surface_albedo(ncells))
 end subroutine
 ! the view of cell c: every member of noahmp_column points at that cell's element (or layer column) of the state
 function noahmp_view(state,c) result(a)
 type(noahmp_state_type),target,intent(inout) :: state
 integer,intent(in) :: c
 type(noahmp_column) :: a
 a%forcing_spechumidityrefheight=>state%forcing_spechumidityrefheight(c)
 a%forcing_temperatureairrefheight=>state%forcing_temperatureairrefheight(c)
 a%forcing_windeastwardrefheight=>state%forcing_windeastwardrefheight(c)
 a%forcing_windnorthwardrefheight=>state%forcing_windnorthwardrefheight(c)
 a%forcing_radswdownrefheight=>state%forcing_radswdownrefheight(c)
 a%forcing_radlwdownrefheight=>state%forcing_radlwdownrefheight(c)
 a%forcing_pressureairrefheight=>state%forcing_pressureairrefheight(c)
 a%forcing_pressureairsurface=>state%forcing_pressureairsurface(c)
 a%forcing_precipconvrefheight=>state%forcing_precipconvrefheight(c)
 a%forcing_precipnonconvrefheight=>state%forcing_precipnonconvrefheight(c)
 a%forcing_precipshconvrefheight=>state%forcing_precipshconvrefheight(c)
 a%forcing_precipsnowrefheight=>state%forcing_precipsnowrefheight(c)
 a%forcing_precipgraupelrefheight=>state%forcing_precipgraupelrefheight(c)
 a%forcing_preciphailrefheight=>state%forcing_preciphailrefheight(c)
 a%forcing_temperaturesoilbottom=>state%forcing_temperaturesoilbottom(c)
 a%config_domain_flagurban=>state%config_domain_flagurban(c)
 a%config_domain_flagcropland=>state%config_domain_flagcropland(c)
 a%config_domain_flagdynamiccrop=>state%config_domain_flagdynamiccrop(c)
 a%config_domain_flagdynamicveg=>state%config_domain_flagdynamicveg(c)
 a%config_domain_flagsoilprocess=>state%config_domain_flagsoilprocess(c)
 a%config_domain_gridindexi=>state%config_domain_gridindexi(c)
 a%config_domain_gridindexj=>state%config_domain_gridindexj(c)
 a%config_domain_vegtype=>state%config_domain_vegtype(c)
 a%config_domain_croptype=>state%config_domain_croptype(c)
 a%config_domain_numsnowlayerneg=>state%config_domain_numsnowlayerneg(c)
 a%config_domain_indexwaterpoint=>state%config_domain_indexwaterpoint(c)
 a%config_domain_indexbarrenpoint=>state%config_domain_indexbarrenpoint(c)
 a%config_domain_indexicepoint=>state%config_domain_indexicepoint(c)
 a%config_domain_numdayinyear=>state%config_domain_numdayinyear(c)
 a%config_domain_numsoiltimestep=>state%config_domain_numsoiltimestep(c)
 a%config_domain_maintimestep=>state%config_domain_maintimestep(c)
 a%config_domain_soiltimestep=>state%config_domain_soiltimestep(c)
 a%config_domain_dayjulianinyear=>state%config_domain_dayjulianinyear(c)
 a%config_domain_cossolarzenithangle=>state%config_domain_cossolarzenithangle(c)
 a%config_domain_refheightabovesfc=>state%config_domain_refheightabovesfc(c)
 a%config_domain_latitude=>state%config_domain_latitude(c)
 a%config_domain_depthsoiltempbottom=>state%config_domain_depthsoiltempbottom(c)
 a%config_domain_depthsoillayer(1:)=>state%config_domain_depthsoillayer(:,c)
 a%config_domain_thicknesssnowsoillayer(-2:)=>state%config_domain_thicknesssnowsoillayer(:,c)
 a%config_domain_depthsnowsoillayer(-2:)=>state%config_domain_depthsnowsoillayer(:,c)
 a%energy_flux_heatlatentcanopy=>state%energy_flux_heatlatentcanopy(c)
 a%energy_flux_heatlatenttransp=>state%energy_flux_heatlatenttransp(c)
 a%energy_flux_heatlatentgrd=>state%energy_flux_heatlatentgrd(c)
 a%energy_flux_heatlatentirrievap=>state%energy_flux_heatlatentirrievap(c)
 a%energy_flux_heatprecipadvcanopy=>state%energy_flux_heatprecipadvcanopy(c)
 a%energy_flux_heatprecipadvveggrd=>state%energy_flux_heatprecipadvveggrd(c)
 a%energy_flux_heatprecipadvbaregrd=>state%energy_flux_heatprecipadvbaregrd(c)
 a%energy_flux_heatprecipadvsfc=>state%energy_flux_heatprecipadvsfc(c)
 a%energy_flux_heatsensiblecanopy=>state%energy_flux_heatsensiblecanopy(c)
 a%energy_flux_heatlatentcanevap=>state%energy_flux_heatlatentcanevap(c)
 a%energy_flux_heatsensibleveggrd=>state%energy_flux_heatsensibleveggrd(c)
 a%energy_flux_heatsensiblesfc=>state%energy_flux_heatsensiblesfc(c)
 a%energy_flux_heatlatentveggrd=>state%energy_flux_heatlatentveggrd(c)
 a%energy_flux_heatlatentcantransp=>state%energy_flux_heatlatentcantransp(c)
 a%energy_flux_heatgroundveggrd=>state%energy_flux_heatgroundveggrd(c)
 a%energy_flux_heatsensiblebaregrd=>state%energy_flux_heatsensiblebaregrd(c)
 a%energy_flux_heatlatentbaregrd=>state%energy_flux_heatlatentbaregrd(c)
 a%energy_flux_heatgroundbaregrd=>state%energy_flux_heatgroundbaregrd(c)
 a%energy_flux_heatgroundtot=>state%energy_flux_heatgroundtot(c)
 a%energy_flux_heatgroundtotmean=>state%energy_flux_heatgroundtotmean(c)
 a%energy_flux_heatfromsoilbot=>state%energy_flux_heatfromsoilbot(c)
 a%energy_flux_heatcanstoragechg=>state%energy_flux_heatcanstoragechg(c)
 a%energy_flux_heatgroundtotacc=>state%energy_flux_heatgroundtotacc(c)
 a%energy_flux_radphotoactabssunlit=>state%energy_flux_radphotoactabssunlit(c)
 a%energy_flux_radphotoactabsshade=>state%energy_flux_radphotoactabsshade(c)
 a%energy_flux_radswabsveg=>state%energy_flux_radswabsveg(c)
 a%energy_flux_radswabsgrd=>state%energy_flux_radswabsgrd(c)
 a%energy_flux_radswabssfc=>state%energy_flux_radswabssfc(c)
 a%energy_flux_radswreflsfc=>state%energy_flux_radswreflsfc(c)
 a%energy_flux_radswreflveg=>state%energy_flux_radswreflveg(c)
 a%energy_flux_radswreflgrd=>state%energy_flux_radswreflgrd(c)
 a%energy_flux_radlwnetcanopy=>state%energy_flux_radlwnetcanopy(c)
 a%energy_flux_radlwnetsfc=>state%energy_flux_radlwnetsfc(c)
 a%energy_flux_radphotoactabscan=>state%energy_flux_radphotoactabscan(c)
 a%energy_flux_radlwemitsfc=>state%energy_flux_radlwemitsfc(c)
 a%energy_flux_radlwnetveggrd=>state%energy_flux_radlwnetveggrd(c)
 a%energy_flux_radlwnetbaregrd=>state%energy_flux_radlwnetbaregrd(c)
 a%energy_flux_radswabsvegdir(1:)=>state%energy_flux_radswabsvegdir(:,c)
 a%energy_flux_radswabsvegdif(1:)=>state%energy_flux_radswabsvegdif(:,c)
 a%energy_flux_radswdirtrangrddir(1:)=>state%energy_flux_radswdirtrangrddir(:,c)
 a%energy_flux_radswdirtrangrddif(1:)=>state%energy_flux_radswdirtrangrddif(:,c)
 a%energy_flux_radswdiftrangrddir(1:)=>state%energy_flux_radswdiftrangrddir(:,c)
 a%energy_flux_radswdiftrangrddif(1:)=>state%energy_flux_radswdiftrangrddif(:,c)
 a%energy_flux_radswreflvegdir(1:)=>state%energy_flux_radswreflvegdir(:,c)
 a%energy_flux_radswreflvegdif(1:)=>state%energy_flux_radswreflvegdif(:,c)
 a%energy_flux_radswreflgrddir(1:)=>state%energy_flux_radswreflgrddir(:,c)
 a%energy_flux_radswreflgrddif(1:)=>state%energy_flux_radswreflgrddif(:,c)
 a%energy_flux_radswdowndir(1:)=>state%energy_flux_radswdowndir(:,c)
 a%energy_flux_radswdowndif(1:)=>state%energy_flux_radswdowndif(:,c)
 a%energy_flux_radswpenetrategrd(-2:)=>state%energy_flux_radswpenetrategrd(:,c)
 a%energy_state_flagfrozencanopy=>state%energy_state_flagfrozencanopy(c)
 a%energy_state_flagfrozenground=>state%energy_state_flagfrozenground(c)
 a%energy_state_leafareaindeff=>state%energy_state_leafareaindeff(c)
 a%energy_state_stemareaindeff=>state%energy_state_stemareaindeff(c)
 a%energy_state_leafareaindex=>state%energy_state_leafareaindex(c)
 a%energy_state_stemareaindex=>state%energy_state_stemareaindex(c)
 a%energy_state_vegareaindeff=>state%energy_state_vegareaindeff(c)
 a%energy_state_vegfrac=>state%energy_state_vegfrac(c)
 a%energy_state_temperaturegrd=>state%energy_state_temperaturegrd(c)
 a%energy_state_temperaturecanopy=>state%energy_state_temperaturecanopy(c)
 a%energy_state_temperaturesfc=>state%energy_state_temperaturesfc(c)
 a%energy_state_temperaturerootzone=>state%energy_state_temperaturerootzone(c)
 a%energy_state_pressurevaporrefheight=>state%energy_state_pressurevaporrefheight(c)
 a%energy_state_snowagefac=>state%energy_state_snowagefac(c)
 a%energy_state_snowagenondim=>state%energy_state_snowagenondim(c)
 a%energy_state_vegareaprojdir=>state%energy_state_vegareaprojdir(c)
 a%energy_state_gapbtwcanopy=>state%energy_state_gapbtwcanopy(c)
 a%energy_state_gapincanopy=>state%energy_state_gapincanopy(c)
 a%energy_state_gapcanopydif=>state%energy_state_gapcanopydif(c)
 a%energy_state_gapcanopydir=>state%energy_state_gapcanopydir(c)
 a%energy_state_canopysunlitfrac=>state%energy_state_canopysunlitfrac(c)
 a%energy_state_canopyshadefrac=>state%energy_state_canopyshadefrac(c)
 a%energy_state_leafareaindsunlit=>state%energy_state_leafareaindsunlit(c)
 a%energy_state_leafareaindshade=>state%energy_state_leafareaindshade(c)
 a%energy_state_vappressatcanopy=>state%energy_state_vappressatcanopy(c)
 a%energy_state_vappressatgrdveg=>state%energy_state_vappressatgrdveg(c)
 a%energy_state_vappressatgrdbare=>state%energy_state_vappressatgrdbare(c)
 a%energy_state_vappressatcantempd=>state%energy_state_vappressatcantempd(c)
 a%energy_state_vappressatgrdvegtempd=>state%energy_state_vappressatgrdvegtempd(c)
 a%energy_state_vappressatgrdbaretempd=>state%energy_state_vappressatgrdbaretempd(c)
 a%energy_state_pressurevaporcanair=>state%energy_state_pressurevaporcanair(c)
 a%energy_state_pressureatmosco2=>state%energy_state_pressureatmosco2(c)
 a%energy_state_pressureatmoso2=>state%energy_state_pressureatmoso2(c)
 a%energy_state_resistancestomatasunlit=>state%energy_state_resistancestomatasunlit(c)
 a%energy_state_resistancestomatashade=>state%energy_state_resistancestomatashade(c)
 a%energy_state_densityairrefheight=>state%energy_state_densityairrefheight(c)
 a%energy_state_temperaturecanopyair=>state%energy_state_temperaturecanopyair(c)
 a%energy_state_zeroplanedispsfc=>state%energy_state_zeroplanedispsfc(c)
 a%energy_state_zeroplanedispgrd=>state%energy_state_zeroplanedispgrd(c)
 a%energy_state_roughlenmomgrd=>state%energy_state_roughlenmomgrd(c)
 a%energy_state_roughlenmomsfc=>state%energy_state_roughlenmomsfc(c)
 a%energy_state_roughlenshcanopy=>state%energy_state_roughlenshcanopy(c)
 a%energy_state_roughlenshveggrd=>state%energy_state_roughlenshveggrd(c)
 a%energy_state_roughlenshbaregrd=>state%energy_state_roughlenshbaregrd(c)
 a%energy_state_canopyheight=>state%energy_state_canopyheight(c)
 a%energy_state_windspdcanopytop=>state%energy_state_windspdcanopytop(c)
 a%energy_state_frictionvelveg=>state%energy_state_frictionvelveg(c)
 a%energy_state_frictionvelbare=>state%energy_state_frictionvelbare(c)
 a%energy_state_windextcoeffcanopy=>state%energy_state_windextcoeffcanopy(c)
 a%energy_state_mostabparaundcan=>state%energy_state_mostabparaundcan(c)
 a%energy_state_mostabparaabvcan=>state%energy_state_mostabparaabvcan(c)
 a%energy_state_mostabparabare=>state%energy_state_mostabparabare(c)
 a%energy_state_mostabparaveg2m=>state%energy_state_mostabparaveg2m(c)
 a%energy_state_mostabparabare2m=>state%energy_state_mostabparabare2m(c)
 a%energy_state_molengthundcan=>state%energy_state_molengthundcan(c)
 a%energy_state_molengthabvcan=>state%energy_state_molengthabvcan(c)
 a%energy_state_molengthbare=>state%energy_state_molengthbare(c)
 a%energy_state_mostabcorrshundcan=>state%energy_state_mostabcorrshundcan(c)
 a%energy_state_mostabcorrmomabvcan=>state%energy_state_mostabcorrmomabvcan(c)
 a%energy_state_mostabcorrshabvcan=>state%energy_state_mostabcorrshabvcan(c)
 a%energy_state_mostabcorrmomveg2m=>state%energy_state_mostabcorrmomveg2m(c)
 a%energy_state_mostabcorrshveg2m=>state%energy_state_mostabcorrshveg2m(c)
 a%energy_state_mostabcorrshbare=>state%energy_state_mostabcorrshbare(c)
 a%energy_state_mostabcorrmombare=>state%energy_state_mostabcorrmombare(c)
 a%energy_state_mostabcorrmombare2m=>state%energy_state_mostabcorrmombare2m(c)
 a%energy_state_mostabcorrshbare2m=>state%energy_state_mostabcorrshbare2m(c)
 a%energy_state_exchcoeffmomsfc=>state%energy_state_exchcoeffmomsfc(c)
 a%energy_state_exchcoeffmomabvcan=>state%energy_state_exchcoeffmomabvcan(c)
 a%energy_state_exchcoeffmombare=>state%energy_state_exchcoeffmombare(c)
 a%energy_state_exchcoeffshsfc=>state%energy_state_exchcoeffshsfc(c)
 a%energy_state_exchcoeffshabvcan=>state%energy_state_exchcoeffshabvcan(c)
 a%energy_state_exchcoeffshbare=>state%energy_state_exchcoeffshbare(c)
 a%energy_state_exchcoeffsh2mvegmo=>state%energy_state_exchcoeffsh2mvegmo(c)
 a%energy_state_exchcoeffsh2mbaremo=>state%energy_state_exchcoeffsh2mbaremo(c)
 a%energy_state_exchcoeffsh2mveg=>state%energy_state_exchcoeffsh2mveg(c)
 a%energy_state_exchcoefflhabvcan=>state%energy_state_exchcoefflhabvcan(c)
 a%energy_state_exchcoefflhtransp=>state%energy_state_exchcoefflhtransp(c)
 a%energy_state_exchcoefflhevap=>state%energy_state_exchcoefflhevap(c)
 a%energy_state_exchcoefflhundcan=>state%energy_state_exchcoefflhundcan(c)
 a%energy_state_resistancemomundcan=>state%energy_state_resistancemomundcan(c)
 a%energy_state_resistanceshundcan=>state%energy_state_resistanceshundcan(c)
 a%energy_state_resistancelhundcan=>state%energy_state_resistancelhundcan(c)
 a%energy_state_resistancemomabvcan=>state%energy_state_resistancemomabvcan(c)
 a%energy_state_resistanceshabvcan=>state%energy_state_resistanceshabvcan(c)
 a%energy_state_resistancelhabvcan=>state%energy_state_resistancelhabvcan(c)
 a%energy_state_resistancemombaregrd=>state%energy_state_resistancemombaregrd(c)
 a%energy_state_resistanceshbaregrd=>state%energy_state_resistanceshbaregrd(c)
 a%energy_state_resistancelhbaregrd=>state%energy_state_resistancelhbaregrd(c)
 a%energy_state_resistanceleafboundary=>state%energy_state_resistanceleafboundary(c)
 a%energy_state_temperaturepotrefheight=>state%energy_state_temperaturepotrefheight(c)
 a%energy_state_windspdrefheight=>state%energy_state_windspdrefheight(c)
 a%energy_state_frictionvelvertveg=>state%energy_state_frictionvelvertveg(c)
 a%energy_state_frictionvelvertbare=>state%energy_state_frictionvelvertbare(c)
 a%energy_state_emissivityveg=>state%energy_state_emissivityveg(c)
 a%energy_state_emissivitygrd=>state%energy_state_emissivitygrd(c)
 a%energy_state_resistancegrdevap=>state%energy_state_resistancegrdevap(c)
 a%energy_state_psychconstcanopy=>state%energy_state_psychconstcanopy(c)
 a%energy_state_latheatvapcanopy=>state%energy_state_latheatvapcanopy(c)
 a%energy_state_psychconstgrd=>state%energy_state_psychconstgrd(c)
 a%energy_state_latheatvapgrd=>state%energy_state_latheatvapgrd(c)
 a%energy_state_relhumiditygrd=>state%energy_state_relhumiditygrd(c)
 a%energy_state_spechumiditysfc=>state%energy_state_spechumiditysfc(c)
 a%energy_state_spechumiditysfcmean=>state%energy_state_spechumiditysfcmean(c)
 a%energy_state_spechumidity2mveg=>state%energy_state_spechumidity2mveg(c)
 a%energy_state_spechumidity2mbare=>state%energy_state_spechumidity2mbare(c)
 a%energy_state_spechumidity2m=>state%energy_state_spechumidity2m(c)
 a%energy_state_temperaturegrdveg=>state%energy_state_temperaturegrdveg(c)
 a%energy_state_temperaturegrdbare=>state%energy_state_temperaturegrdbare(c)
 a%energy_state_windstressewveg=>state%energy_state_windstressewveg(c)
 a%energy_state_windstressnsveg=>state%energy_state_windstressnsveg(c)
 a%energy_state_windstressewbare=>state%energy_state_windstressewbare(c)
 a%energy_state_windstressnsbare=>state%energy_state_windstressnsbare(c)
 a%energy_state_windstressewsfc=>state%energy_state_windstressewsfc(c)
 a%energy_state_windstressnssfc=>state%energy_state_windstressnssfc(c)
 a%energy_state_temperatureair2mveg=>state%energy_state_temperatureair2mveg(c)
 a%energy_state_temperatureair2mbare=>state%energy_state_temperatureair2mbare(c)
 a%energy_state_temperatureair2m=>state%energy_state_temperatureair2m(c)
 a%energy_state_exchcoeffshleaf=>state%energy_state_exchcoeffshleaf(c)
 a%energy_state_exchcoeffshundcan=>state%energy_state_exchcoeffshundcan(c)
 a%energy_state_exchcoeffsh2mbare=>state%energy_state_exchcoeffsh2mbare(c)
 a%energy_state_refheightabovegrd=>state%energy_state_refheightabovegrd(c)
 a%energy_state_canopyfracsnowbury=>state%energy_state_canopyfracsnowbury(c)
 a%energy_state_depthsoiltempbottosno=>state%energy_state_depthsoiltempbottosno(c)
 a%energy_state_roughlenmomsfctoatm=>state%energy_state_roughlenmomsfctoatm(c)
 a%energy_state_temperatureradsfc=>state%energy_state_temperatureradsfc(c)
 a%energy_state_emissivitysfc=>state%energy_state_emissivitysfc(c)
 a%energy_state_albedosfc=>state%energy_state_albedosfc(c)
 a%energy_state_energybalanceerror=>state%energy_state_energybalanceerror(c)
 a%energy_state_radswbalanceerror=>state%energy_state_radswbalanceerror(c)
 a%energy_state_temperaturesoilsnow(-2:)=>state%energy_state_temperaturesoilsnow(:,c)
 a%energy_state_heatcapacvolsnow(-2:)=>state%energy_state_heatcapacvolsnow(:,c)
 a%energy_state_thermconductsnow(-2:)=>state%energy_state_thermconductsnow(:,c)
 a%energy_state_heatcapacvolsoil(1:)=>state%energy_state_heatcapacvolsoil(:,c)
 a%energy_state_thermconductsoil(1:)=>state%energy_state_thermconductsoil(:,c)
 a%energy_state_thermconductsoilsnow(-2:)=>state%energy_state_thermconductsoilsnow(:,c)
 a%energy_state_heatcapacsoilsnow(-2:)=>state%energy_state_heatcapacsoilsnow(:,c)
 a%energy_state_phasechgfacsoilsnow(-2:)=>state%energy_state_phasechgfacsoilsnow(:,c)
 a%energy_state_albedosnowdir(1:)=>state%energy_state_albedosnowdir(:,c)
 a%energy_state_albedosnowdif(1:)=>state%energy_state_albedosnowdif(:,c)
 a%energy_state_albedosoildir(1:)=>state%energy_state_albedosoildir(:,c)
 a%energy_state_albedosoildif(1:)=>state%energy_state_albedosoildif(:,c)
 a%energy_state_albedogrddir(1:)=>state%energy_state_albedogrddir(:,c)
 a%energy_state_albedogrddif(1:)=>state%energy_state_albedogrddif(:,c)
 a%energy_state_reflectanceveg(1:)=>state%energy_state_reflectanceveg(:,c)
 a%energy_state_transmittanceveg(1:)=>state%energy_state_transmittanceveg(:,c)
 a%energy_state_albedosfcdir(1:)=>state%energy_state_albedosfcdir(:,c)
 a%energy_state_albedosfcdif(1:)=>state%energy_state_albedosfcdif(:,c)
 a%energy_param_treecrownradius=>state%energy_param_treecrownradius(c)
 a%energy_param_heightcanopytop=>state%energy_param_heightcanopytop(c)
 a%energy_param_heightcanopybot=>state%energy_param_heightcanopybot(c)
 a%energy_param_roughlenmomveg=>state%energy_param_roughlenmomveg(c)
 a%energy_param_treedensity=>state%energy_param_treedensity(c)
 a%energy_param_canopyorientindex=>state%energy_param_canopyorientindex(c)
 a%energy_param_upscattercoeffsnowdir=>state%energy_param_upscattercoeffsnowdir(c)
 a%energy_param_upscattercoeffsnowdif=>state%energy_param_upscattercoeffsnowdif(c)
 a%energy_param_soilheatcapacity=>state%energy_param_soilheatcapacity(c)
 a%energy_param_snowagefacbats=>state%energy_param_snowagefacbats(c)
 a%energy_param_snowgrowvapfacbats=>state%energy_param_snowgrowvapfacbats(c)
 a%energy_param_snowsootfacbats=>state%energy_param_snowsootfacbats(c)
 a%energy_param_snowgrowfrzfacbats=>state%energy_param_snowgrowfrzfacbats(c)
 a%energy_param_solarzenithadjbats=>state%energy_param_solarzenithadjbats(c)
 a%energy_param_freshsnoalbvisbats=>state%energy_param_freshsnoalbvisbats(c)
 a%energy_param_freshsnoalbnirbats=>state%energy_param_freshsnoalbnirbats(c)
 a%energy_param_snoagefacdifvisbats=>state%energy_param_snoagefacdifvisbats(c)
 a%energy_param_snoagefacdifnirbats=>state%energy_param_snoagefacdifnirbats(c)
 a%energy_param_szafacdirvisbats=>state%energy_param_szafacdirvisbats(c)
 a%energy_param_szafacdirnirbats=>state%energy_param_szafacdirnirbats(c)
 a%energy_param_conductanceleafmin=>state%energy_param_conductanceleafmin(c)
 a%energy_param_co2mmconst25c=>state%energy_param_co2mmconst25c(c)
 a%energy_param_o2mmconst25c=>state%energy_param_o2mmconst25c(c)
 a%energy_param_co2mmconstq10=>state%energy_param_co2mmconstq10(c)
 a%energy_param_o2mmconstq10=>state%energy_param_o2mmconstq10(c)
 a%energy_param_leafdimlength=>state%energy_param_leafdimlength(c)
 a%energy_param_zilitinkevichcoeff=>state%energy_param_zilitinkevichcoeff(c)
 a%energy_param_emissivitysnow=>state%energy_param_emissivitysnow(c)
 a%energy_param_canopywindextfac=>state%energy_param_canopywindextfac(c)
 a%energy_param_roughlenmomsnow=>state%energy_param_roughlenmomsnow(c)
 a%energy_param_roughlenmomsoil=>state%energy_param_roughlenmomsoil(c)
 a%energy_param_roughlenmomlake=>state%energy_param_roughlenmomlake(c)
 a%energy_param_emissivityicesfc=>state%energy_param_emissivityicesfc(c)
 a%energy_param_resistancesoilexp=>state%energy_param_resistancesoilexp(c)
 a%energy_param_resistancesnowsfc=>state%energy_param_resistancesnowsfc(c)
 a%energy_param_vegfracgreen=>state%energy_param_vegfracgreen(c)
 a%energy_param_vegfracannmax=>state%energy_param_vegfracannmax(c)
 a%energy_param_heatcapaccanfac=>state%energy_param_heatcapaccanfac(c)
 a%energy_param_leafareaindexmon(1:)=>state%energy_param_leafareaindexmon(:,c)
 a%energy_param_stemareaindexmon(1:)=>state%energy_param_stemareaindexmon(:,c)
 a%energy_param_soilquartzfrac(1:)=>state%energy_param_soilquartzfrac(:,c)
 a%energy_param_albedosoilsat(1:)=>state%energy_param_albedosoilsat(:,c)
 a%energy_param_albedosoildry(1:)=>state%energy_param_albedosoildry(:,c)
 a%energy_param_albedolakefrz(1:)=>state%energy_param_albedolakefrz(:,c)
 a%energy_param_scattercoeffsnow(1:)=>state%energy_param_scattercoeffsnow(:,c)
 a%energy_param_reflectanceleaf(1:)=>state%energy_param_reflectanceleaf(:,c)
 a%energy_param_reflectancestem(1:)=>state%energy_param_reflectancestem(:,c)
 a%energy_param_transmittanceleaf(1:)=>state%energy_param_transmittanceleaf(:,c)
 a%energy_param_transmittancestem(1:)=>state%energy_param_transmittancestem(:,c)
 a%energy_param_emissivitysoillake(1:)=>state%energy_param_emissivitysoillake(:,c)
 a%water_flux_rainfallrefheight=>state%water_flux_rainfallrefheight(c)
 a%water_flux_snowfallrefheight=>state%water_flux_snowfallrefheight(c)
 a%water_flux_preciptotrefheight=>state%water_flux_preciptotrefheight(c)
 a%water_flux_precipconvtotrefheight=>state%water_flux_precipconvtotrefheight(c)
 a%water_flux_preciplargesclrefheight=>state%water_flux_preciplargesclrefheight(c)
 a%water_flux_evapcanopynet=>state%water_flux_evapcanopynet(c)
 a%water_flux_transpiration=>state%water_flux_transpiration(c)
 a%water_flux_evapcanopyliq=>state%water_flux_evapcanopyliq(c)
 a%water_flux_dewcanopyliq=>state%water_flux_dewcanopyliq(c)
 a%water_flux_frostcanopyice=>state%water_flux_frostcanopyice(c)
 a%water_flux_sublimcanopyice=>state%water_flux_sublimcanopyice(c)
 a%water_flux_meltcanopyice=>state%water_flux_meltcanopyice(c)
 a%water_flux_freezecanopyliq=>state%water_flux_freezecanopyliq(c)
 a%water_flux_snowfallground=>state%water_flux_snowfallground(c)
 a%water_flux_snowdepthincr=>state%water_flux_snowdepthincr(c)
 a%water_flux_frostsnowsfcice=>state%water_flux_frostsnowsfcice(c)
 a%water_flux_sublimsnowsfcice=>state%water_flux_sublimsnowsfcice(c)
 a%water_flux_rainfallground=>state%water_flux_rainfallground(c)
 a%water_flux_snowbotoutflow=>state%water_flux_snowbotoutflow(c)
 a%water_flux_glacierexcessflow=>state%water_flux_glacierexcessflow(c)
 a%water_flux_irrigationrateflood=>state%water_flux_irrigationrateflood(c)
 a%water_flux_irrigationratemicro=>state%water_flux_irrigationratemicro(c)
 a%water_flux_irrigationratesprinkler=>state%water_flux_irrigationratesprinkler(c)
 a%water_flux_irrievaplosssprinkler=>state%water_flux_irrievaplosssprinkler(c)
 a%water_flux_soilsfcinflow=>state%water_flux_soilsfcinflow(c)
 a%water_flux_runoffsurface=>state%water_flux_runoffsurface(c)
 a%water_flux_runoffsubsurface=>state%water_flux_runoffsubsurface(c)
 a%water_flux_infilratesfc=>state%water_flux_infilratesfc(c)
 a%water_flux_evapsoilsfcliq=>state%water_flux_evapsoilsfcliq(c)
 a%water_flux_drainsoilbot=>state%water_flux_drainsoilbot(c)
 a%water_flux_tiledrain=>state%water_flux_tiledrain(c)
 a%water_flux_vaporizegrd=>state%water_flux_vaporizegrd(c)
 a%water_flux_condensevapgrd=>state%water_flux_condensevapgrd(c)
 a%water_flux_dewsoilsfcliq=>state%water_flux_dewsoilsfcliq(c)
 a%water_flux_evapirrisprinkler=>state%water_flux_evapirrisprinkler(c)
 a%water_flux_interceptcanopyrain=>state%water_flux_interceptcanopyrain(c)
 a%water_flux_dripcanopyrain=>state%water_flux_dripcanopyrain(c)
 a%water_flux_throughfallrain=>state%water_flux_throughfallrain(c)
 a%water_flux_interceptcanopysnow=>state%water_flux_interceptcanopysnow(c)
 a%water_flux_dripcanopysnow=>state%water_flux_dripcanopysnow(c)
 a%water_flux_throughfallsnow=>state%water_flux_throughfallsnow(c)
 a%water_flux_evapgroundnet=>state%water_flux_evapgroundnet(c)
 a%water_flux_meltgroundsnow=>state%water_flux_meltgroundsnow(c)
 a%water_flux_watertoatmostotal=>state%water_flux_watertoatmostotal(c)
 a%water_flux_evapsoilsfcliqacc=>state%water_flux_evapsoilsfcliqacc(c)
 a%water_flux_soilsfcinflowacc=>state%water_flux_soilsfcinflowacc(c)
 a%water_flux_sfcwatertotchgacc=>state%water_flux_sfcwatertotchgacc(c)
 a%water_flux_preciptotacc=>state%water_flux_preciptotacc(c)
 a%water_flux_evapcanopynetacc=>state%water_flux_evapcanopynetacc(c)
 a%water_flux_transpirationacc=>state%water_flux_transpirationacc(c)
 a%water_flux_evapgroundnetacc=>state%water_flux_evapgroundnetacc(c)
 a%water_flux_evapsoilsfcliqmean=>state%water_flux_evapsoilsfcliqmean(c)
 a%water_flux_soilsfcinflowmean=>state%water_flux_soilsfcinflowmean(c)
 a%water_flux_transpwatlosssoil(1:)=>state%water_flux_transpwatlosssoil(:,c)
 a%water_flux_transpwatlosssoilacc(1:)=>state%water_flux_transpwatlosssoilacc(:,c)
 a%water_flux_transpwatlosssoilmean(1:)=>state%water_flux_transpwatlosssoilmean(:,c)
 a%water_flux_compactionsnowaging(-2:)=>state%water_flux_compactionsnowaging(:,c)
 a%water_flux_compactionsnowburden(-2:)=>state%water_flux_compactionsnowburden(:,c)
 a%water_flux_compactionsnowmelt(-2:)=>state%water_flux_compactionsnowmelt(:,c)
 a%water_flux_compactionsnowtot(-2:)=>state%water_flux_compactionsnowtot(:,c)
 a%water_state_irrigationcntsprinkler=>state%water_state_irrigationcntsprinkler(c)
 a%water_state_irrigationcntmicro=>state%water_state_irrigationcntmicro(c)
 a%water_state_irrigationcntflood=>state%water_state_irrigationcntflood(c)
 a%water_state_canopytotalwater=>state%water_state_canopytotalwater(c)
 a%water_state_canopywetfrac=>state%water_state_canopywetfrac(c)
 a%water_state_snowfalldensity=>state%water_state_snowfalldensity(c)
 a%water_state_canopyliqwater=>state%water_state_canopyliqwater(c)
 a%water_state_canopyice=>state%water_state_canopyice(c)
 a%water_state_canopyicemax=>state%water_state_canopyicemax(c)
 a%water_state_canopyliqwatermax=>state%water_state_canopyliqwatermax(c)
 a%water_state_snowdepth=>state%water_state_snowdepth(c)
 a%water_state_snowwaterequiv=>state%water_state_snowwaterequiv(c)
 a%water_state_snowwaterequivprev=>state%water_state_snowwaterequivprev(c)
 a%water_state_pondsfcthinsnwmelt=>state%water_state_pondsfcthinsnwmelt(c)
 a%water_state_pondsfcthinsnwcomb=>state%water_state_pondsfcthinsnwcomb(c)
 a%water_state_pondsfcthinsnwtrans=>state%water_state_pondsfcthinsnwtrans(c)
 a%water_state_irrigationfracflood=>state%water_state_irrigationfracflood(c)
 a%water_state_irrigationamtflood=>state%water_state_irrigationamtflood(c)
 a%water_state_irrigationfracmicro=>state%water_state_irrigationfracmicro(c)
 a%water_state_irrigationamtmicro=>state%water_state_irrigationamtmicro(c)
 a%water_state_irrigationfracsprinkler=>state%water_state_irrigationfracsprinkler(c)
 a%water_state_irrigationamtsprinkler=>state%water_state_irrigationamtsprinkler(c)
 a%water_state_watertabledepth=>state%water_state_watertabledepth(c)
 a%water_state_soilicemax=>state%water_state_soilicemax(c)
 a%water_state_soilliqwatermin=>state%water_state_soilliqwatermin(c)
 a%water_state_soilimpervfracmax=>state%water_state_soilimpervfracmax(c)
 a%water_state_soilmoisturetowt=>state%water_state_soilmoisturetowt(c)
 a%water_state_rechargegwdeepwt=>state%water_state_rechargegwdeepwt(c)
 a%water_state_soilsaturationexcess=>state%water_state_soilsaturationexcess(c)
 a%water_state_tiledrainfrac=>state%water_state_tiledrainfrac(c)
 a%water_state_waterstorageaquifer=>state%water_state_waterstorageaquifer(c)
 a%water_state_waterstoragelake=>state%water_state_waterstoragelake(c)
 a%water_state_waterheadsfc=>state%water_state_waterheadsfc(c)
 a%water_state_irrigationfracgrid=>state%water_state_irrigationfracgrid(c)
 a%water_state_precipareafrac=>state%water_state_precipareafrac(c)
 a%water_state_snowcoverfrac=>state%water_state_snowcoverfrac(c)
 a%water_state_soiltranspfacacc=>state%water_state_soiltranspfacacc(c)
 a%water_state_frozenprecipfrac=>state%water_state_frozenprecipfrac(c)
 a%water_state_waterstoragetotbeg=>state%water_state_waterstoragetotbeg(c)
 a%water_state_waterbalanceerror=>state%water_state_waterbalanceerror(c)
 a%water_state_waterstoragetotend=>state%water_state_waterstoragetotend(c)
 a%water_state_indexphasechange(-2:)=>state%water_state_indexphasechange(:,c)
 a%water_state_snowice(-2:)=>state%water_state_snowice(:,c)
 a%water_state_snowliqwater(-2:)=>state%water_state_snowliqwater(:,c)
 a%water_state_snowicefracprev(-2:)=>state%water_state_snowicefracprev(:,c)
 a%water_state_snowicefrac(-2:)=>state%water_state_snowicefrac(:,c)
 a%water_state_soilicefrac(1:)=>state%water_state_soilicefrac(:,c)
 a%water_state_snoweffporosity(-2:)=>state%water_state_snoweffporosity(:,c)
 a%water_state_soilliqwater(1:)=>state%water_state_soilliqwater(:,c)
 a%water_state_soilice(1:)=>state%water_state_soilice(:,c)
 a%water_state_soilmoisture(1:)=>state%water_state_soilmoisture(:,c)
 a%water_state_soilimpervfrac(1:)=>state%water_state_soilimpervfrac(:,c)
 a%water_state_soilwatconductivity(1:)=>state%water_state_soilwatconductivity(:,c)
 a%water_state_soilwatdiffusivity(1:)=>state%water_state_soilwatdiffusivity(:,c)
 a%water_state_soileffporosity(1:)=>state%water_state_soileffporosity(:,c)
 a%water_state_soiltranspfac(1:)=>state%water_state_soiltranspfac(:,c)
 a%water_state_snowicevol(-2:)=>state%water_state_snowicevol(:,c)
 a%water_state_snowliqwatervol(-2:)=>state%water_state_snowliqwatervol(:,c)
 a%water_state_soilsupercoolwater(-2:)=>state%water_state_soilsupercoolwater(:,c)
 a%water_state_soilmatpotential(1:)=>state%water_state_soilmatpotential(:,c)
 a%water_param_numsoillayerroot=>state%water_param_numsoillayerroot(c)
 a%water_param_irristopdaybfharvest=>state%water_param_irristopdaybfharvest(c)
 a%water_param_canopyliqholdcap=>state%water_param_canopyliqholdcap(c)
 a%water_param_snowcompactburdenfac=>state%water_param_snowcompactburdenfac(c)
 a%water_param_snowcompactagingfac1=>state%water_param_snowcompactagingfac1(c)
 a%water_param_snowcompactagingfac2=>state%water_param_snowcompactagingfac2(c)
 a%water_param_snowcompactagingfac3=>state%water_param_snowcompactagingfac3(c)
 a%water_param_snowcompactagingmax=>state%water_param_snowcompactagingmax(c)
 a%water_param_snowviscositycoeff=>state%water_param_snowviscositycoeff(c)
 a%water_param_snowliqfracmax=>state%water_param_snowliqfracmax(c)
 a%water_param_snowliqholdcap=>state%water_param_snowliqholdcap(c)
 a%water_param_snowliqreleasefac=>state%water_param_snowliqreleasefac(c)
 a%water_param_irrifloodratefac=>state%water_param_irrifloodratefac(c)
 a%water_param_irrimicrorate=>state%water_param_irrimicrorate(c)
 a%water_param_soilinfilmaxcoeff=>state%water_param_soilinfilmaxcoeff(c)
 a%water_param_soilimpervfraccoeff=>state%water_param_soilimpervfraccoeff(c)
 a%water_param_soildrainslope=>state%water_param_soildrainslope(c)
 a%water_param_waterstoragelakemax=>state%water_param_waterstoragelakemax(c)
 a%water_param_snowateqvmaxglacier=>state%water_param_snowateqvmaxglacier(c)
 a%water_param_irritriggerlaimin=>state%water_param_irritriggerlaimin(c)
 a%water_param_soilwatdeficitallow=>state%water_param_soilwatdeficitallow(c)
 a%water_param_irrifloodlossfrac=>state%water_param_irrifloodlossfrac(c)
 a%water_param_irrisprinklerrate=>state%water_param_irrisprinklerrate(c)
 a%water_param_irrifracthreshold=>state%water_param_irrifracthreshold(c)
 a%water_param_irristopprecipthr=>state%water_param_irristopprecipthr(c)
 a%water_param_snowfalldensitymax=>state%water_param_snowfalldensitymax(c)
 a%water_param_snowmassfullcoverold=>state%water_param_snowmassfullcoverold(c)
 a%water_param_soilmatpotentialwilt=>state%water_param_soilmatpotentialwilt(c)
 a%water_param_snowmeltfac=>state%water_param_snowmeltfac(c)
 a%water_param_snowcoverfac=>state%water_param_snowcoverfac(c)
 a%water_param_soilmoisturesat(1:)=>state%water_param_soilmoisturesat(:,c)
 a%water_param_soilmoisturewilt(1:)=>state%water_param_soilmoisturewilt(:,c)
 a%water_param_soilmoisturefieldcap(1:)=>state%water_param_soilmoisturefieldcap(:,c)
 a%water_param_soilwatdiffusivitysat(1:)=>state%water_param_soilwatdiffusivitysat(:,c)
 a%water_param_soilwatconductivitysat(1:)=>state%water_param_soilwatconductivitysat(:,c)
 a%water_param_soilexpcoeffb(1:)=>state%water_param_soilexpcoeffb(:,c)
 a%water_param_soilmatpotentialsat(1:)=>state%water_param_soilmatpotentialsat(:,c)
 a%biochem_flux_photosyntotal=>state%biochem_flux_photosyntotal(c)
 a%biochem_flux_photosynleafsunlit=>state%biochem_flux_photosynleafsunlit(c)
 a%biochem_flux_photosynleafshade=>state%biochem_flux_photosynleafshade(c)
 a%biochem_state_plantgrowstage=>state%biochem_state_plantgrowstage(c)
 a%biochem_state_indexgrowseason=>state%biochem_state_indexgrowseason(c)
 a%biochem_state_nitrogenconcfoliage=>state%biochem_state_nitrogenconcfoliage(c)
 a%biochem_param_dateplanting=>state%biochem_param_dateplanting(c)
 a%biochem_param_dateharvest=>state%biochem_param_dateharvest(c)
 a%biochem_param_quantumefficiency25c=>state%biochem_param_quantumefficiency25c(c)
 a%biochem_param_carboxylratemax25c=>state%biochem_param_carboxylratemax25c(c)
 a%biochem_param_carboxylratemaxq10=>state%biochem_param_carboxylratemaxq10(c)
 a%biochem_param_photosynpathc3=>state%biochem_param_photosynpathc3(c)
 a%biochem_param_slopeconducttophotosyn=>state%biochem_param_slopeconducttophotosyn(c)
 a%biochem_param_temperatureminphotosyn=>state%biochem_param_temperatureminphotosyn(c)
 a%biochem_param_nitrogenconcfoliagemax=>state%biochem_param_nitrogenconcfoliagemax(c)
#ifdef LEAN_NOAHMP_SWSPLIT
 a%sw_frac_dir_vis=>state%sw_frac_dir_vis(c)
 a%sw_frac_dir_nir=>state%sw_frac_dir_nir(c)
 a%sw_frac_dif_vis=>state%sw_frac_dif_vis(c)
#endif
 a%net_shortwave=>state%net_shortwave(c)
 a%rain_convective=>state%rain_convective(c)
 a%surface_albedo=>state%surface_albedo(c)
 end function
end module
