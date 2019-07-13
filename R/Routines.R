
#' Astronomical computations routine
#'
#' Routine Astro calculates day length, some intermediate variables for the
#' calculation of the solar elevation, the integral of the solar elevation
#' over a day and the fraction of diffuse radiation.
#'
#' @param w Dataframe. Daily weather table
#' @param t Integer. Time steps (days) passed from emergence (dvs==0)
#' @param lat Numeric. Latitude
#' @return Named list of length = 8:
#' d:      DAYL      Astronomical daylength (base = 0 degrees)     h
#' dlp:    DAYLP     Photosynthetic daylength (base =-4 degrees)   h
#' sinld:  SINLD     Seasonal offset of sine of solar height       -
#' cosld:  COSLD     Amplitude of sine of solar height             -
#' dp:     DIFPP     Diffuse irradiation perpendicular to
#'                   direction of light                         J/m2*s
#' tatm:   ATMTR     Daily atmospheric transmission                -
#' sinbm:  DSINBE    Daily total of effective solar height         s
#' sod:    ANGOT     Angot radiation at top of atmosphere       J/m2*d
#'
#' @export
#'
Astro<- function(w, t, lat){

  # return:
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # R:      Python:   Description:
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # d:      DAYL      Astronomical daylength (base = 0 degrees)     h
  # dlp:    DAYLP     Photosynthetic daylength (base =-4 degrees)   h
  # sinld:  SINLD     Seasonal offset of sine of solar height       -
  # cosld:  COSLD     Amplitude of sine of solar height             -
  # dp:     DIFPP     Diffuse irradiation perpendicular to
  #                   direction of light                         J/m2*s
  # tatm:   ATMTR     Daily atmospheric transmission                -
  # sinbm:  DSINBE    Daily total of effective solar height         s
  # sod:    ANGOT     Angot radiation at top of atmosphere       J/m2*d
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Julian day
  if (class(w@DAY[t])=="Date") {td<- format(w@DAY[t], "%j")} else
  {stop('Dates must be of class "Date"')}
  td<- as.numeric(td)

  # daily global radiation
  sgd<- w@IRRAD[t]

  # solar declination
  sold<- sol_dec(td)

  # solar constant at the top of the atmosphere
  scd<- sol_const(td)

  # day length
  d<- day_length(lat, sold)

  # day length for photosynthtic activity
  dlp<- day_length_corr(lat, sold)

  # integral of solar height
  sinbdth<- int_sol_hgt(lat, sold, d)

  # Integral of effective solar height
  sinbm<- int_effsol_hgt(d, lat, sold)

  # daily extra-terrestrial radiation
  sod<- day_extrad(scd, sinbdth)

  # atmospheric transmission
  if (d <= 0){tatm<- 0} else {
  tatm<- atm_tra(sgd, sod)
  }

  # diffuse vs. global irradiance ratio
  SdfSg<- df_g_ratio(sgd, sod)

  # diffuse radiation perpendicular to the direction of light
  dp<- diff_perp(SdfSg, tatm, scd)

  return(list(
           'sod'=sod,
           'tatm'=tatm,
           'cosld'=cosd(lat)*cos(sold),
           'd'=d,
           'dlp'=dlp,
           'dp'=dp,
           'sinbm'=sinbm,
           'sinld'=sind(lat)*sin(sold),
           'sgd'=sgd
           ))

}


#' CO2 assimilation routine
#'
#' Computes "pgass" (Potential daily CO2 assimilation rate).
#' Calls functions "totass()" and "assim()"
#'
#' @param tday Average daytime temperature
#' @param d From Astro. Astronomic day length
#' @param sinld From Astro. Seasonal offset of sine of solar height
#' @param cosld From Astro. Amplitude of sine of solar height
#' @param lat Latitude
#' @param sgd Daily global radiation
#' @param sinbm From Astro. Integral of effective solar height
#' @param dp From Astro. Diffuse radiation perpendicular to the direction light
#' @param lai Leaf area index of whole canopy
#' @param dvs Development stage
#' @param t Day in the loop
#' @param AMAXTB Afgen table (inst. gross assimilation rate at light saturation
#'               vs. development stage)
#' @param TMPFTB Afgen table (inst. gross assimilation rate at light saturation
#'               vs. daytime temperature)
#' @param TMNFTB Afgen table (inst. gross assimilation rate at light saturation
#'               vs. 7 days running minumum temperature)
#' @param EFFTB Afgen table (light use efficiency vs. daily mean temp),
#'              crop-specific [kg ha-1 hr-1 j-1 m2 s]
#' @export
#'
Assimilation<- function(
                        AMAXTB,TMPFTB,KDIFTB,EFFTB,TMNFTB,
                        d, lai, sgd, dp, sinbm, sinld, cosld, dvs,
                        tday, w, t, tmins
                        ){

  # 7-day running average of TMIN
  if (t <= length(w@DAY)){ # necessary to have model running last row of test.
    tmins<- c(w@TMIN[t], tmins)
    tmins<- tmins[1:min(7,length(tmins))]
  }
  tlow<- week_tmin_av(tmins)

  # gross assimilation and correction for sub-optimum average day temperature
  amax<- afgen(dvs, AMAXTB)
  amax<- amax* afgen(tday, TMPFTB)
  kdif<- afgen(dvs, KDIFTB)
  eff<- afgen(tday, EFFTB)
  dtga<- totass(d, amax, eff, lai, kdif, sgd,
                dp, sinbm, sinld, cosld)

  # correction for low minimum temperature potential
  dtga<- dtga*afgen(tlow, TMNFTB)

  # assimilation in kg CH2O per ha
  pgass<- dtga * 30/44

  return(list('pgass'=pgass,
              'tmins'=tmins))

}


#' Assimilation of CO2 throughout the day
#'
#' This routine calculates the daily total gross CO2 assimilation by
#' performing a Gaussian integration over time. At three different times of
#' the day, irradiance is computed and used to calculate the instantaneous
#' canopy assimilation, whereafter integration takes place. More information
#' on this routine is given by Spitters et al. (1988).
#'
#' Called by Assimilation(). Calls Assim().
#'
#' @export
#'
totass<- function(d, amax, eff, lai, kdif, sgd,
                  dp, sinbm, sinld, cosld){

  # Gauss points and weights
  xgauss<- c(0.1127017, 0.5000000, 0.8872983)
  wgauss<- c(0.2777778, 0.4444444, 0.2777778)

  # calculation of assimilation is done only when it will not be zero
  # (amax >0, lai >0, d >0)
  dtga<- 0
  if (amax > 0 & lai > 0 & d > 0){
    for (i in 1:3){
      hour<- 12 + 0.5*d*xgauss[i]
      sinb<-  max(0, sinld + cosld*cos(2*pi*(hour + 12)/24))
      parr<- 0.5*sgd*sinb*(1 + 0.4*sinb)/sinbm
      pardif<- min(parr,sinb*dp)
      pardir<- parr - pardif
      fgros<- assim(amax,eff,lai,kdif,sinb,pardir,pardif)
      dtga<- dtga + fgros*wgauss[i]
    }
  }
  dtga<- dtga*d

  return(dtga)

}


#' Assimilation of CO2 throughout the canopy
#'
#' This routine calculates the gross CO2 assimilation rate of
#' the whole crop, FGROS, by performing a Gaussian integration
#' over depth in the crop canopy. At three different depths in
#' the canopy, (i.e. for different values of LAI) the
#' assimilation rate is computed for given fluxes of photosynthe-
#' tically active radiation, whereafter integration over depth
#' takes place. More information on this routine is given by
#' Spitters et al. (1988). The input variables SINB, PARDIR
#' and PARDIF are calculated in routine TOTASS.
#'
#' Called by totas()
#'
#' @export
#'
assim<- function(amax,eff,lai,kdif,sinb,pardir,pardif){

  # Gauss points and weights
  xgauss<- c(0.1127017, 0.5000000, 0.8872983)
  wgauss<- c(0.2777778, 0.4444444, 0.2777778)

  scv<- 0.2

  # Extinction coefficients kdirbl & kdirt
  refh<- (1 - sqrt(1 - scv))/(1 + sqrt(1 - scv))
  refs<- refh*2/(1 + 1.6*sinb)
  kdirbl<- (0.5/sinb)*kdif/(0.8*sqrt(1 - scv))
  kdirt<- kdirbl*sqrt(1 - scv)

  # Three-point Gaussian integration over lai
  fgros<- 0
  for (i in 1:3){

    laic<- lai*xgauss[i]

    # Absorbed diffuse radiation (visdf),light from direct
    # origine (vist) and direct light (visd)
    visdf<- (1 - refs)*pardif*kdif*exp(-kdif*laic)
    vist<- (1 - refs)*pardir*kdirt*exp(-kdirt*laic)
    visd<- (1 - scv)*pardir*kdirbl*exp(-kdirbl*laic)

    # absorbed flux in W/m2 for shaded leaves and assimilation
    visshd<- visdf + vist - visd
    fgrsh<- amax*(1 - exp(-visshd*eff/max(2, amax)))

    # direct light absorbed by leaves perpendicular to direct
    # beam and assimilation of sunlit leaf area
    vispp<- (1 - scv)*pardir/sinb
    if (vispp <= 0){
      fgrsun<- fgrsh
    } else {
      fgrsun<- amax*(1 - (amax - fgrsh)*
                       (1 - exp(-vispp*eff/max(2, amax)))/(eff*vispp))
    }

    # fraction of sunlit leaf area (fslla) and local
    # assimilation rate (fgl)
    fslla<- exp(-kdirbl*laic)
    fgl<- fslla*fgrsun + (1 - fslla)*fgrsh

    # integration
    fgros<- fgros + fgl*wgauss[i]

  }

  fgros<- fgros*lai

  return(fgros)

}


#' Evapotranspiration routine
#'
#' Routine Evapotranspiration computes evapotranspiration rates from crop
#' and soil input parameters
#'
#' @param dvs Numeric. Development stage
#' @param w Weather table
#' @param lai Numeric. Leaf Area Index LAI
#' @param sm Numeric. Volumetric soil moisture content
#' @param t Integar. Time step of the model
#' @param DEPNR Crop group number
#' @param SMFCF Soil moisture content at field capacity [cm3/cm3]
#' @param IAIRDU Indicates presence (1) or absence (0) of airducts in the plant
#' @param IOX Flag; controlling calculation of potential (0) or
#'            water limited yield (1)
#' @param CFET Correction parameter of potential evapotranspiration rate
#' @param SMW Soil moisture content at wilting point
#' @param CRAIRC Critical air content for root aeration
#' @param KDIFTB Extinction coefficient for diffuse visible radiation as
#'               function of DVS
#' @param dsos Days since oxygen stress, computed in classic waterbalance,
#'             accumulates the number of consecutive days of oxygen stress
#' @export
#'
Evapotranspiration<- function(dvs,w,lai,sm,t,dsos,SM0,DEPNR,SMFCF,IAIRDU,IOX,
                              CFET,SMW,CRAIRC,KDIFTB){


  # helper variable for counting total days with water and oxygen
  # stress (idwst, idost)
  .idwst<- 0
  .idost<- 0

  kglob<- 0.75*afgen(dvs, KDIFTB)

  # crop specific correction on potential transpiration rate
  et0_crop<- max(0, CFET*w@ET0[t])

  # maximum evaporation and transpiration rates
  ekl<- exp(-kglob*lai)
  evwmx<- w@E0[t]*ekl
  evsmx<- max(0, w@ES0[t]*ekl)
  tramx<- et0_crop*(1 - ekl)

  # Soil Water Easily Available Fraction
  swdep<- sweaf(et0_crop, DEPNR)

  # Critical soil moisture
  smcr<- (1 - swdep)*(SMFCF - SMW) + SMW

  # Reduction factor for transpiration in case of water shortage (rfws)
  rfws<- limit(0, 1, (sm - SMW)/(smcr - SMW))

  # reduction in transpiration in case of oxygen shortage (RFOS)
  # for non-rice crops, and possibly deficient land drainage
  rfos<- 1
  if (IAIRDU == 0 & IOX == 1){
    print('Oxygen shortage')
    rfosmx<- limit(0, 1, (SM0 - sm)/CRAIRC)
    # maximum reduction reached after 4 days
    rfos<- rfosmx + (1 - min(dsos, 4)/4)*(1 - rfosmx)
  }

  # Transpiration rate multiplied with reduction factors for oxygen and water
  rftra<- rfos * rfws
  tra<- tramx * rftra

  # Counting stress days
  if (rfws < 1){
    .idwst<- .idwst + 1
  }

  if (rfos < 1){
    .idost<- .idost + 1
  }

  return (list(
               'evsmx'= evsmx,
               'evwmx'= evwmx,
               'tra'=   tra,
               'tramx'= tramx,
               'rftra'= rftra
               ))

}

#' Wofost Potential Production
#'
#' Computes potential production as per Wofost crop growth algorithm
#'
#' @param crop CorpObject. Object of class CropObject containing the specific
#' crop parameters. See ?CropObject.
#' @param w WeatherObject. Object of class CropObject containing the climatic
#' driving variables. See ?WeatherObject.
#' @param waterlimited Logical. If TRUE waterlimited production is modelled,
#' potential production is modelled by default (waterlimited = F)
#' @param finishType Variable describing the conditionst riggering
#' the end of the simulation.
#' Can be any one of the following:
#' "maturity": The model is terminated 7 days after maturity is reached
#' Integer [1:365]: Maximum number of days for which the model is run.
#' @param varReturn Character vector specifying which variables to output.
#' Potentially, any of the variables produced inside the Wofost function
#' can be returned. However the use of carReturn = NULL (default) is
#' encouraged. By default returning variables described in "Returns".
#'
#' @export
#'
Wofost<- function(
  crop, w ,
  waterlimited= F,
  finishType= 'maturity',
  startType= 'sowing',
  varReturn= NULL
){

  # CROP PARAMETERS ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Following commented out variables not used in testing PP.

  # CROP_END_TYPE = crop@CROP_END_TYPE
  # CO2 = crop@CO2
  # CRPNAM = crop@CRPNAM
  # IFUNRN = crop@IFUNRN
  # K0 = crop@K0
  # KSUB = crop@KSUB
  # NOTINF = crop@NOTINF
  # SMLIM = crop@SMLIM
  # SSMAX = crop@SSMAX
  # WAV = crop@WAV
  # SSI = crop@SSI
  # CRAIRC = crop@CRAIRC  # Soil
  # SMW = crop@SMW  # Soil
  # SM0 = crop@SM0  # Soil
  # SMFCF = crop@SMFCF  # Soil
  # RDMSOL = crop@RDMSOL  # Soil

  RDMSOL=100 # TEMPORARY!! THIS NEEDS TO BE PROVIDED IN SOIL OBJECT !!!
  crop_start_type = startType
  AMAXTB = crop@AMAXTB
  CFET = crop@CFET
  CVL = crop@CVL
  CVO = crop@CVO
  CVR = crop@CVR
  CVS = crop@CVS
  DEPNR = crop@DEPNR
  DLC = crop@DLC
  DLO = crop@DLO
  DTSMTB = crop@DTSMTB
  DVSEND = crop@DVSEND
  DVSI = crop@DVSI
  EFFTB = crop@EFFTB
  FLTB = crop@FLTB
  FOTB = crop@FOTB
  FRTB = crop@FRTB
  FSTB = crop@FSTB
  IAIRDU = crop@IAIRDU
  IDSL = crop@IDSL
  IOX = crop@IOX
  KDIFTB = crop@KDIFTB
  PERDL = crop@PERDL
  Q10 = crop@Q10
  RDI = crop@RDI
  RDMCR = crop@RDMCR
  RDRRTB = crop@RDRRTB
  RDRSTB = crop@RDRSTB
  RFSETB = crop@RFSETB
  RGRLAI = crop@RGRLAI
  RML = crop@RML
  RMO = crop@RMO
  RMR = crop@RMR
  RMS = crop@RMS
  RRI = crop@RRI
  SLATB = crop@SLATB
  SPA = crop@SPA
  SPAN = crop@SPAN
  SSATB = crop@SSATB
  TBASE = crop@TBASE
  TBASEM = crop@TBASEM
  TDWI = crop@TDWI
  TEFFMX = crop@TEFFMX
  TMNFTB = crop@TMNFTB
  TMPFTB = crop@TMPFTB
  TSUM1 = crop@TSUM1
  TSUM2 = crop@TSUM2
  TSUMEM = crop@TSUMEM
  VERNRTB = crop@VERNRTB
  VERNDVS = crop@VERNDVS
  VERNBASE = crop@VERNBASE
  VERNSAT = crop@VERNSAT

  # Latitude
  lat<- w@LAT[1]


  # VARIABLE DECLARATIONS ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # loop control parameters
  STOP<- FALSE
  t<- 1

  # output containers for "varReturn = NULL"
  if(is.null(varReturn)){

    dvs_out<- NULL     # phenology
    dvr_out<- NULL     # phenology
    lai_out<- NULL     # leaf dynamics
    twlv_out<- NULL    # leaf dynamics
    tagp_out<- NULL    # total above groud dry matter
    twrt_out<- NULL    # root dry matter
    twso_out<- NULL    # storage organ dry matter
    twst_out<- NULL    # stems dry matter
    gass_out<- NULL    # gross assimilation rate of canopy
    astro_out<- list() # astro
    rmt_out<- NULL     # respiration
    rd_out<- NULL      # root depth
    tra_out<- NULL     # transpiration

  # output containers for user-defined output
  } else if (class(varReturn) == 'character'){

    OUT<- vector('list',length(varReturn))
    names(OUT)<- varReturn

  } else { # Problem: unacceptable varReturn

    stop('"varReturn" must be either NULL or of class "character"')

  }


  # Helper variables
  gass<- 0
  astro<- 0
  rmt<- 0
  tra<- 0
  rd<- 0
  stopInSeven<- 7


  # LOOP STARTS ####
  #~~~~~~~~~~~~~~~~~

  while (isFALSE(STOP)){ # main loop.
    # 't' counts the days, one iteration per day.



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > INITIALISATION ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t == 1){ # first iteration of the model


      # >> PHENOLOGY ####

      # Set initial values for phenology
      tsum<- 0
      tsume<- 0
      vern<- 0
      isvernalised<- FALSE
      force_vernalisation<- FALSE
      dov<- NULL # date of vernalisation

      # Initialise nighttime 7 days running minimum temperature
      tmins<- NULL

      # Define initial stage type (emergence/sowing)
      if (crop_start_type == "emergence"){
        stage<- "vegetative"
        dvs<- DVSI
      } else if (crop_start_type == "sowing"){
        stage<- "emerging"
        dvs<- -0.1
      } else {
        stop('unknown "crop_start_type" value.')
      }


      # >> PARTITIONING ####

      # Set partitioning values
      fr<- afgen(dvs, FRTB)
      fl<- afgen(dvs, FLTB)
      fs<- afgen(dvs, FSTB)
      fo<- afgen(dvs, FOTB)

      checksum<- fr + (fl + fs + fo)*(1 - fr) - 1
      if (abs(checksum) >= 0.0001){
        stop(paste0('Error in partitioning. Parameter "checksum" = ',checksum))
      }


      # >> ASSIMILATION ####
      # No initialisation seems necessary for the assimilation module.

      # >> RESPIRATION ####
      # No initialisation seems necessary for the respiration module.

      # >> EVAPOTRANSPIRATION ####
      # No initialisation seems necessary for the evapotranspiration module.


      # >> ROOT DYNAMICS ####

      # Initial root depth
      rdmax<- max(RDI, min(RDMCR, RDMSOL))
      rdm<- rdmax
      rd<- RDI

      # initial root biomass
      wrt<- TDWI*fr
      dwrt<- 0
      twrt<- wrt + dwrt


      # >> STEM DYNAMICS ####

      # initial stem biomass
      wst<- (TDWI*(1-fr))*fs
      dwst<- 0
      twst<- wst + dwst

      # Initial Stem Area Index
      sai<- wst*afgen(dvs,SSATB)


      # >> STORAGE ORGAN DYNAMICS ####

      # initial storage organ biomass
      wso<- (TDWI*(1-fr))*fo
      dwso<- 0
      twso<- wso + dwso

      # Initial Pod Area Index
      pai<- wso*SPA


      # >> LEAF DYNAMICS ####

      # Set initial dataframe with all living leaves
      leaves<- data.frame(matrix(ncol=4, nrow=1))
      names(leaves)<- c('paget','slat','dwlv','dwnlv')
      leaves[1,]<- c(0,0,0,0)

      # Initial leaf biomass
      wlv<- (TDWI*(1-fr))*fl
      dwlv<- 0
      twlv<- wlv + dwlv

      # First leaf class (sla, age and weight)
      slat<- afgen(dvs, SLATB)
      paget<- 0
      leaves$dwlv[1]<- wlv
      leaves$dwnlv[1]<- leaves$dwlv
      leaves$slat[1]<- slat
      leaves$paget[1]<- paget

      # Initial values for leaf area
      LAIEM<- wlv*slat
      lasum<- LAIEM
      laiexp<- LAIEM
      laimax<- LAIEM
      lai<- lasum + sai + pai


      # >> WHOLE CROP DYNAMICS ####

      # Initial total (living+dead) above-ground biomass of the crop
      tagp<- twlv + twst + twso

      gasst<- 0        # total gross assimilation rate of the canopy
      mrest<- 0        # summation of the maintenance respiration
      ctrat<- 0         # total crop transpiration

      # Check partitioning of TDWI over plant organs
      checksum<- TDWI - tagp - twrt
      if (abs(checksum) >= 0.0001){
        stop('Error in partitioning of initial biomass (TDWI)')
      }

    } # end of first iteration of the model



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > RATES COMPUTATIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # >> TEMPERATURE ####

    # average temperature and average daytemperature
    temp<- (w@TMIN[t] + w@TMAX[t])/2
    tday<- (w@TMAX[t] + temp)/2


    # >> PHENOLOGY ####

    # Day length sensitivity
    dvred<- 1
    if (IDSL >= 1){ # if pre-anthesis development depends on daylength
      astro<- Astro(w,t,lat)
      if(DLC==-99 | DLO==-99){stop('DLC and/or DLO is set to -99.')}
      if (stage == 'vegetative'){
        dvred<- dayl_red(dlp= astro$dlp, DLC, DLO)
      }
    }

    # Vernalisation
    vernfac<- 1
    if (IDSL >= 2){
      if (stage == 'vegetative'){
        # here starts PCSE's "vernalisation.calc_rates"..................#|
        if (isFALSE(isvernalised)){                                      #|
          if (dvs < VERNDVS){ # Vernalisation requirements reached.      #|
            vernr<- afgen(temp,VERNRTB)                                  #|
            r<- (vern - VERNBASE)/(VERNSAT - VERNBASE)                   #|
            vernfac<- limit(0, 1, r)                                     #|
          } else {                                                       #|
            vernr<- 0                                                    #|
            vernfac<- 1                                                  #|
            force_vernalisation<- TRUE                                   #|
          }                                                              #|
        } else {                                                         #|
          vernr<- 0                                                      #|
          vernfac<- 1                                                    #|
        }                                                                #|
        # here finishes PCSE's "vernalisation.calc_rates"................#|
      }
    }

    # Development rates
    if (stage == "emerging"){
      dtsume<- effect_temp(TEFFMX, TBASEM, temp)
      dtsum<- 0
      dvr<- 0.1 * dtsume/TSUMEM
    } else if (stage == 'vegetative'){
      dtsume<- 0
      dtsum<- afgen(temp, DTSMTB) * vernfac * dvred
      dvr = dtsum/TSUM1
    } else if (stage == 'reproductive'){
      dtsume<- 0
      dtsum<- afgen(temp, DTSMTB)
      dvr<- dtsum/TSUM2
    } else if (stage == 'mature'){
      dtsume<- 0
      dtsum<- 0
      dvr<- 0
    } else {  # Problem: no stage defined.
      stop("Unrecognized 'stage' defined in phenology submodule")
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Before emergence there is no need to continue
    # because only the phenology is running.
    if (stage != "emerging"){


      # >> POTENTIAL ASSIMILATION ####

      if (IDSL < 1){ # if astro was not computed for day length sensitivity
        astro<- Astro(w,t,lat)
      }

      # for (v in 1:length(astro)){assign(names(astro)[v], astro[[v]])}
      out<- Assimilation(
        AMAXTB,TMPFTB,KDIFTB,EFFTB,TMNFTB,
        d=astro$d, lai=lai, sgd=astro$sgd, dp=astro$dp,
        sinbm=astro$sinbm,
        sinld=astro$sinld, cosld=astro$cosld, dvs=dvs,
        tday=tday, w=w, t=t, tmins=tmins
      )
      tmins<- out$tmins # updated tmins to feed into next cycle
      pgass<- out$pgass


      # >> EVAPOTRANSPIRATION ####

      if (waterlimited == T){
        sm<- SM[t]
        evtra<- Evapotranspiration(dvs,w,lai,sm,t,dsos=0,SM0,DEPNR,SMFCF,
                                   IAIRDU,IOX,
                                   CFET,SMW,CRAIRC,KDIFTB)
        for (v in 1:length(evtra)){assign(names(evtra)[v], evtra[[v]])}
      } else {
        rftra<- 1
        tra<- 0
      }

      # Water stress reduction
      gass<- pgass*rftra


      # >> RESPIRATION ####

      # Maintenance coefficient of given organ i
      cmi<- c(RML, RMO, RMS, RMR)
      wi<- c(wlv,wso,wst,wrt)

      # Maintanance respiration rate at reference temperature of 25 degrees
      rmr<- maint_resp(cmi, wi)
      rmr<- rmr*afgen(dvs, RFSETB)

      # Maintanance respiration rate at temperature t
      rmt<- maint_resp_t(rmr, Q10, temp, tr=25)
      rmt<- min(gass,rmt) # Maintenance respiration cannot exceed assimilation

      # Net available assimilates
      asrc<- gass - rmt


      # >> PARTITIONING AND CARB CONVERSION ####

      fr<-  afgen (dvs, FRTB)
      fl<-  afgen (dvs, FLTB)
      fs<-  afgen (dvs, FSTB)
      fo<-  afgen (dvs, FOTB)

      # Average conversion efficiency of assimilates into dry matter
      cvf<- 1/((fl/CVL + fs/CVS + fo/CVO)*(1 - fr) + fr/CVR)

      # Dry matter increase
      dmi<- cvf*asrc


      # Organ partitioning check
      if (isFALSE(org_part_check(fl,fo,fs,fr))){
        stop(paste0('Organ partitioning check not passed on day ',
                    w@DAY[t],'. fl=',fl,' fo=',fo,' fs=',fs,' fr=',fr))
      }

      # Carbon balance check
      if (isFALSE(carb_bal_check(fl,fo,fs,fr,gass,rmt,dmi,cvf))){
        stop(paste('Carbon balance check not passed on day ',
                   w@DAY[t],'. fl=',fl,' fo=',fo,' fs=',fs,' fr=',fr,
                   ' gass=',gass,' rmt=',rmt,' dmi=',dmi,' cvf=',cvf))
      }


      # >> GROWTH OF PLANT ORGANS ####

      # Growth of roots
      grrt<- fr*dmi                  # Growth rate of roots
      drrt<- wrt*afgen(dvs, RDRRTB)  # Death rate of roots
      gwrt<- grrt - drrt             # Dry weight of living roots
      # Increase in root depth
      rr<- min((rdm - rd), RRI)
      # Do not let the roots grow if partioning to the roots (fr) is zero
      if (fr == 0){rr<- 0}

      # Above ground dry matter increase
      admi<- (1 - fr)*dmi
      # Growth of stems
      grst<- fs*admi
      drst<- afgen(dvs, RDRSTB)*wst
      gwst<- grst - drst

      # Growth of storage organs
      grso<- fo*admi
      drso<- 0
      gwso<- grso - drso

      # Leaf dynamics
      # Rate of increase of leaf dry matter
      grlv<- fl*admi


      # Death of leaves due to water stress
      dw1d<- wlv*(1 - rftra)*PERDL

      # Death of leaves due to high LAI
      kdf<- afgen(dvs, KDIFTB)
      laic<- crit_lai(kdf)
      dw2d<- lai_death(wlv, lai, laic)

      # Leaf death equals maximum of water stress and shading
      wd<- max(dw1d, dw2d)

      # Leaf biomass that will have to die. Note that the actual leaf death
      # is imposed on the array LV during the state integration step.
      dalv<- sum(leaves[leaves$paget> SPAN, 'dwnlv'])

      # Dry weight of dead leaves
      drlv<- max(wd, dalv)

      # Phisiologic ageing factor for leaves
      frai<- ag_fac(temp, TBASE)

      # Specific leaf area of leaves per time step
      slat<- afgen(dvs, SLATB)

      # Leaf area not to exceed exponential growth curve
      if (laiexp< 6){
        # exponential growth of lai
        dteff<- max(0, temp - TBASE)
        glaiex<- laiexp*RGRLAI*dteff
        # source-limited grwth of lai
        glasol<- grlv*slat
        # final growth rate of lai
        gla<- min(glaiex, glasol)
        # adjustment of specific leaf area index of youngest leaf class
        if (grlv> 0) {slat<- gla/grlv}
      }
    } # End of post-emergence section

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > TEST FINISH CONDITIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(finishType == 'maturity'){ # finishType = 'maturity'
      if (stopInSeven == 0) {STOP<- TRUE}
    } else if (class(finishType) == 'numeric'){
      if (t == finishType) {STOP<- TRUE}
    } else { # Problem: unacceptable finishType
      stop('"finishType" must be either "maturity" or an integer.
           See ?Wofost')
    }

    if (isFALSE(STOP)){ # continue only if finish conditions are
      # not reached.


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > COLLECT OUTPUT ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Default varReturn value
      if(is.null(varReturn)){
        dvs_out[t]<-     dvs     # phenology
        dvr_out[t]<-     dvr     # phenology
        lai_out[t]<-     lai     # leaf dynamics
        twlv_out[t]<-    twlv    # leaf dynamics
        tagp_out[t]<-    tagp    # total above groud dry matter
        twrt_out[t]<-    twrt    # root dry matter
        twso_out[t]<-    twso    # storage organ dry matter
        twst_out[t]<-    twst    # stems dry matter
        gass_out[t]<-    gass   # gross assimilation rate of canopy
        astro_out[[t]]<- astro   # astro
        rmt_out[t]<-     rmt     # respiration
        rd_out[t]<-      rd      # root depth
        tra_out[t]<-     tra     # transpiration

      # User-defined varReturn value
      } else if (class(varReturn) == 'character'){

        for (l in 1:length(varReturn)){
          OUT[[l]][t]<- get(valReturn[l])
        }

      }



      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > TIMER UPDATE ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      t<- t + 1


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > INTEGRATION ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      # Crop stage before integration
      old_crop_stage<- stage

      # >> PHENOLOGY ####

      # Integrate vernalisation module
      if (IDSL >= 2){
        if (stage == 'vegetative'){
          # here starts PCSE's "vernalisation.integrate".................#|
          vern<- vern + vernr                                            #|
          if (vern >= VERNSAT){  # Vernalisation requirements reached    #|
            isvernalised<- TRUE                                          #|
            if (is.null(dov)){                                           #|
              dov<- t                                                    #|
            }                                                            #|
          } else if (isTRUE(force_vernalisation)){                       #|
            isvernalised<- TRUE                                          #|
            warning(paste0(                                              #|
              'Critical DVS for vernalisation (VERNDVS) reached
             at day ',t,
              ' but vernalisation requirements not yet fulfilled.',
              'forcing vernalisation now (vern= ',vern,').'))
          } else {                                                       #|
            isvernalised<- FALSE                                         #|
          }                                                              #|
        }                                                                #|
        # here finishes PCSE's "vernalisation.integrate".................#|
      }

      # Integrate phenologic states
      tsume<- tsume + dtsume
      dvs<- dvs + dvr
      tsum<- tsum + dtsum

      # Check if a new stage is reached
      if (stage == "emerging"){
        if (dvs >= 0){
          stage<- "vegetative"
          dvs<- 0
        }
      }else if (stage == 'vegetative'){
        if (dvs >= 1){
          stage<- 'reproductive'
          dvs<- 1.0
        }
      }else if (stage == 'reproductive'){
        if (dvs >= DVSEND){
          stage<- 'mature'
          dvs<- DVSEND
        }
      }
      else if (stage == 'mature'){
        stopInSeven<- stopInSeven - 1
      } else { # Problem: no stage defined
        stop("No 'stage' defined in phenology submodule")
      }

      # Before emergence there is no need to continue
      # because only the phenology is running.
      if (old_crop_stage != "emerging"){ # After emergence

        # >> GROWTH OF PLANT ORGANS ####

        # New roots biomass
        wrt<- wrt + gwrt
        dwrt<- dwrt + drrt
        twrt<- wrt + dwrt

        # New root depth
        rd<- rd + rr

        # New storage organ biomass
        wso<- wso + gwso
        dwso<- dwso + drso
        twso<- wso + dwso

        # Calculate Pod Area Index (pai)
        pai<- wso*SPA

        # New stem biomass
        wst<- wst + gwst
        dwst<- dwst + drst
        twst<- wst + dwst

        # Calculate Stem Area Index (sai)
        sai<- wst*afgen(dvs,SSATB)

        # Leaf dynamics
        # remove weight of dead leaves from water stress and high LAI
        if (wd==0) {leaves[1,'dwnlv']<- leaves[1,'dwlv']}
        i<-  nrow(leaves)
        while (wd > 0){
          leaves[i,'dwnlv']<- max(0, leaves[i,'dwlv'] - wd)
          wd<- max(0, wd - leaves[i,'dwlv'])
          i<- i - 1
        }

        # remove leaves older than the specific life spans
        leaves<- leaves[leaves$paget<= SPAN,]

        # physiologic ages
        leaves[,'paget']<- phy_age(leaves[,'paget'], frai)

        # add new leaves born in current iteration
        leaves<- rbind(c(0,slat,grlv,grlv),leaves)

        # New leaf area
        lasum<- sum(leaves$dwnlv*leaves$slat)  # total living leaf area
        lai<- lasum + sai + pai
        laimax<- max(lai, laimax)

        # Exponential growth curve
        laiexp<- laiexp + glaiex

        # New leaf biomass
        wlv<- sum(leaves$dwnlv)  # weight of living leaves
        dwlv<- dwlv + drlv  # accumulated dry weight of dead leaves
        twlv<- wlv + dwlv  # accumulated dry weight of dead and leaving leaves

        # Update leaf weight after leaf death
        leaves$dwlv<- leaves$dwnlv

        # Combined dead an living dry weight of plant organs
        tagp = twlv + twst + twso

        # Total gross assimilation and maintenance respiration
        gasst<- gasst + gass
        mrest<- mrest + rmt

        # Total crop transpiration
        ctrat<- ctrat + tra

      } # end of post-emergence section
    } # end of post-finish-conditions-test section
  } # end of daily cycles

  # LOOP ENDS ####
  #~~~~~~~~~~~~~~~

  # FINALISATION ####
  #~~~~~~~~~~~~~~~~~~

  # Default varReturn value
  if(is.null(varReturn)){

  return(list(
    'dvs'=dvs_out,     # phenology
    'lai'=lai_out,     # leaf dynamics
    'rd'=rd_out,       #root depth
    'tagp'=tagp_out,   # total above groud dry matter
    'tra'=tra_out,     # transpiration
    'twlv'=twlv_out,   # leaf dynamics
    'twrt'=twrt_out,   # root dry matter
    'twso'=twso_out,   # storage organ dry matter
    'twst'=twst_out,   # stems dry matter
    'gass'=gass_out   # gross assimilation rate of canopy
    # 'astro'=do.call(rbind,astro_out),   # astro
    # 'rmt'=rmt_out,     # respiration
    # 'dvr'=dvr_out      # phenology
  ))

  # User-defined varReturn value
  } else if (class(varReturn) == 'character'){
    return(OUT)
  }

}
