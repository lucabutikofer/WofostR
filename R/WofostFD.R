# Water-limited production ####

#' Wofost Free-Draining, Water-Limited Production
#'
#' Computes water-limited production on freely draining soils with not
#' water table.
#'
#' @param crop CorpObject. Object of class CropObject containing the specific
#' crop parameters. See ?CropObject.
#' @param w WeatherObject. Object of class CropObject containing the climatic
#' driving variables. See ?WeatherObject.
#' @param soil SoilObject. Object of class SoilObject containing the soil
#' parameters. See ?SoilObject.
#' @param startType Development stage at which the simulation is started.
#' Either "sowing" or "emergence".
#' @param finishType Variable describing the conditionst triggering
#' the end of the simulation.
#' Can be either "maturity" -The model is terminated 7 days after maturity is
#' reached - or
#' an integer [1:365] -Maximum number of days for which the model is run.
#' @param varReturn Character vector specifying which variables to output.
#' Potentially, any of the variables produced inside the Wofost function
#' can be returned. However the use of carReturn = NULL (default) is
#' encouraged. By default returning variables described in "Returns".
#'
#' @export
#'
WofostFD<- function(
  crop, w , soil,
  startType= 'sowing',
  finishType= 'maturity',
  varReturn= NULL
){

  # SOIL PARAMETERS ####
  #~~~~~~~~~~~~~~~~~~~~~

  # K0 = soil@K0
  KSUB = soil@KSUB
  NOTINF = soil@NOTINF
  SMLIM = soil@SMLIM
  SSMAX = soil@SSMAX
  WAV = soil@WAV
  SSI = soil@SSI
  CRAIRC = soil@CRAIRC
  SMW = soil@SMW
  SM0 = soil@SM0
  SMFCF = soil@SMFCF
  RDMSOL = soil@RDMSOL
  IFUNRN = soil@IFUNRN
  SOPE = soil@SOPE


  # CROP PARAMETERS ####
  #~~~~~~~~~~~~~~~~~~~~~

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

  # WEATHER PARAMETERS ####
  #~~~~~~~~~~~~~~~~~~~~~

  # Commented-out variables are not used but specified in .yaml test sets.

  # ELEV = w@ELEV
  # ET0 = w@ET0
  # IRRAD = w@IRRAD
  # LAT = w@LAT
  # LON = w@LON
  # SNOWDEPTH = w@SNOWDEPTH
  # TEMP = w@TEMP
  # VAP = w@VAP
  # WIND = w@WIND
  RAIN = w@RAIN
  DAY = w@DAY
  E0 = w@E0
  ES0 = w@ES0
  TMAX = w@TMAX
  TMIN = w@TMIN


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
  # rd<- 0
  stopInSeven<- 7


  # LOOP STARTS ####
  #~~~~~~~~~~~~~~~~~

  while (isFALSE(STOP)){ # main loop.
    # 't' counts the days, one iteration per day.



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > INITIALISATION ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t == 1){ # first iteration of the model


      # >> SOIL ####

      # Check validity of maximum soil moisture amount in topsoil (SMLIM)
      SMLIM<- limit(SMW, SM0, SMLIM)
      if (SMLIM != soil@SMLIM){
        stop("SMLIM not in valid range. Must be between SMW AND SM0")
      }

      # set default rd to 10 cm, also derive maximum depth and
      # old rooting depth
      rd<- 10
      RDM<- max(rd, RDMSOL)
      RDold<- rd

      # Initial surface storage
      SS<- SSI

      # Initial soil moisture content (W) and amount of water in rooted zone
      # (sm), limited by SMLIM. Save initial value (WI)
      sm<- limit(SMW, SMLIM, SMW + WAV/rd)
      W<- sm * rd
      WI<- W

      # initial amount of soil moisture between current root zone and maximum
      # rootable depth (WLOW). Save initial value (WLOWI)
      WLOW<- limit(0, SM0*(RDM - rd), (WAV + RDM*SMW - W))
      WLOWI<- WLOW

      # Total water depth in soil column (root zone + subsoil)
      WWLOW<- W + WLOW

      # soil evaporation, days since last rain (DSLR) set to 1 if the
      # soil is wetter than halfway between SMW and SMFCF, else DSLR=5.
      if (sm >= (SMW + 0.5*(SMFCF - SMW))){
        DSLR<- 1
      } else {
        DSLR<- 5
      }

      # Initialize some remaining helper variables
      RINold<- 0
      NINFTB<- make.afgen(c(0,0,0.5,0,1.5,1))

      # Initialize model state variables.
      wtrat=0
      EVST=0
      EVWT=0
      TSR=0
      RAINT=0
      WDRT=0
      TOTINF=0
      TOTIRR=0
      DSOS=0
      PERCT=0
      LOSST=0
      WBALRT=-999
      WBALTT=-999


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
    temp<- (TMIN[t] + TMAX[t])/2
    tday<- (TMAX[t] + temp)/2


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

      evtra<- Evapotranspiration(dvs,w,lai,sm,t,dsos=0,SM0,DEPNR,SMFCF,
                                 IAIRDU,IOX,
                                 CFET,SMW,CRAIRC,KDIFTB)
      evsmx<- evtra$evsmx
      evwmx<- evtra$evwmx
      tra<- evtra$tra
      rftra<- evtra$rftra

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
                    DAY[t],'. fl=',fl,' fo=',fo,' fs=',fs,' fr=',fr))
      }

      # Carbon balance check
      if (isFALSE(carb_bal_check(fl,fo,fs,fr,gass,rmt,dmi,cvf))){
        stop(paste('Carbon balance check not passed on day ',
                   DAY[t],'. fl=',fl,' fo=',fo,' fs=',fs,' fr=',fr,
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


    # >> SOIL ####

    # Rate of irrigation (RIRR)
    RIRR<- 0

    # Transpiration and maximum soil and surface water evaporation rates
    # are calculated by the crop evapotranspiration module.
    # However, if the crop is not yet emerged then set tra=0 and use
    # the potential soil/water evaporation rates directly because there is
    # no shading by the canopy.
    if (stage == 'emerging'){
      wtra = 0
      evwmx<- E0[t]
      evsmx<- ES0[t]
    } else {
      wtra<- tra
      evwmx<- evtra$evwmx
      evsmx<- evtra$evsmx
    }

    # Actual evaporation rates
    EVW<- 0
    EVS<- 0
    if (SS > 1){
      # If surface storage > 1cm then evaporate from water layer on
      # soil surface
      EVW<- evwmx
    } else {
      # else assume evaporation from soil surface
      if (RINold >= 1){
        # If infiltration >= 1cm on previous day assume maximum soil
        # evaporation
        EVS<- evsmx
        DSLR<- 1
      } else {
        # Else soil evaporation is a function days-since-last-rain (DSLR)
        EVSMXT<- evsmx*(sqrt(DSLR + 1) - sqrt(DSLR))
        EVS<- min(evsmx, EVSMXT + RINold)
        DSLR<- DSLR + 1
      }
    }

    # Potentially infiltrating rainfall
    if (IFUNRN == 0){
      RINPRE<- (1 - NOTINF)*RAIN[t]
    } else {
      # infiltration is function of storm size (NINFTB)
      RINPRE<- (1 - NOTINF*NINFTB(RAIN[t]))*RAIN[t]
    }

    # Second stage preliminary infiltration rate (RINPRE)
    # including surface storage and irrigation
    RINPRE<- RINPRE + RIRR + SS
    if (SS > 0.1){
      # with surface storage, infiltration limited by SOPE
      AVAIL<- RINPRE + RIRR - EVW
      RINPRE<- min(SOPE, AVAIL)
    }

    # equilibrium amount of soil moisture in rooted zone
    WE<- SMFCF * rd

    # percolation from rooted zone to subsoil equals amount of
    # excess moisture in rooted zone, not to exceed maximum percolation rate
    # of root zone (SOPE)
    PERC1<- limit(0, SOPE, (W - WE) - wtra - EVS)

    # loss of water at the lower end of the maximum root zone
    # equilibrium amount of soil moisture below rooted zone
    WELOW<- SMFCF*(RDM - rd)
    LOSS<- limit(0, KSUB, (WLOW - WELOW + PERC1))

    # percolation not to exceed uptake capacity of subsoil
    PERC2<- ((RDM - rd)*SM0 - WLOW) + LOSS
    PERC<- min(PERC1, PERC2)

    # adjustment of infiltration rate
    RIN<- min(RINPRE, (SM0 - sm)*rd + wtra + EVS + PERC)
    RINold<- RIN

    # rates of change in amounts of moisture W and WLOW
    DW<- RIN - wtra - EVS - PERC
    DWLOW<- PERC - LOSS

    # Check if DW creates a negative value of W
    # If so, reduce EVS to reach W == 0
    Wtmp<- W + DW
    if (Wtmp < 0){
      EVS<- EVS + Wtmp
      if (EVS < 0){
        stop(paste0('Negative soil evaporation rate on day ',t,'.'))
      }
      DW<- -W
    }

    # Computation of rate of change in surface storage and surface runoff
    # SStmp is the layer of water that cannot infiltrate and that can potentially
    # be stored on the surface. Here we assume that RAIN_NOTINF automatically
    # ends up in the surface storage (and finally runoff).
    SStmp<- RAIN[t] + RIRR - EVW - RIN
    # rate of change in surface storage is limited by SSMAX - SS
    DSS<- min(SStmp, (SSMAX - SS))
    # Remaining part of SStmp is send to surface runoff
    DTSR<- SStmp - DSS
    # incoming rainfall rate
    DRAINT<- RAIN[t]


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > TEST FINISH CONDITIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(finishType == 'maturity'){ # finishType = 'maturity'
      if (stopInSeven == 0) {STOP<- TRUE}
    } else if (class(finishType) == 'numeric' |
               class(finishType) == 'integer'){
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
          OUT[[l]][t]<- get(varReturn[l])
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


      # >> SOIL ####

      delt<- 1

      # total transpiration
      wtrat<- wtrat + wtra*delt

      # total evaporation from surface water layer and/or soil
      EVWT<- EVWT + EVW*delt
      EVST<- EVST + EVS*delt

      # totals for rainfall, irrigation and infiltration
      RAINT<- RAINT + DRAINT*delt
      TOTINF<- TOTINF + RIN*delt
      TOTIRR<- TOTIRR + RIRR*delt

      # Update surface storage and total surface runoff (TSR)
      SS<- SS + DSS*delt
      TSR<- TSR + DTSR*delt

      # amount of water in rooted zone
      W<- W + DW*delt
      if(W < 0){
        stop('Negative amount of water in root zone on day', t,'.')
      }

      # total percolation and loss of water by deep leaching
      PERCT<- PERCT + PERC*delt
      LOSST<- LOSST + LOSS*delt

      # amount of water in unrooted, lower part of rootable zone
      WLOW<- WLOW + DWLOW*delt
      # total amount of water in the whole rootable zone
      WWLOW<- W + WLOW*delt

      # CHANGE OF ROOTZONE SUBSYSTEM BOUNDARY

      # First get the actual rooting depth
      RDchange<- rd - RDold

      # Redistribute water between the root zone and the lower zone
      # (Redistribution of water is needed when roots grow during the
      # growing season and when the crop is finished and the root zone
      # shifts back from the crop rooted depth to the default depth of the
      # upper (rooted) layer of the water balance.
      # Or when the initial rooting depth of a crop is different from the
      # default one used by the water balance module (10 cm).)

      WDR<- 0
      if (RDchange > 0.001){
        # roots grow down by more than 0.001 cm
        # move water from previously unrooted zone and add to new rooted zone
        WDR<- WLOW*RDchange/(RDMSOL - RDold)
        # Take minimum of WDR and WLOW to avoid negative WLOW due to rounding
        WDR<- min(WLOW, WDR)
      } else {
        # roots disappear upwards by more than 0.001 cm
        # especially when crop disappears)
        # move water from previously rooted zone and add to new unrooted zone
        WDR<- W*RDchange/RDold
      }

      if (WDR != 0){
        # reduce amount of water in subsoil
        WLOW<- WLOW - WDR
        # increase amount of water in root zone
        W<- W + WDR
        # total water add to rootzone by root zone reset
        WDRT<- WDRT + WDR
      }

      # mean soil moisture content in rooted zone
      sm<- W/rd

      # Accumulate days since oxygen stress, but only if a crop is present
      if (sm >= (SM0 - CRAIRC)){  # and self.in_crop_cycle:
        DSOS<- DSOS + 1
      } else {
        DSOS<- 0
      }

      # save rooting depth
      RDold<- rd

      # Checksums waterbalance for systems without groundwater
      # for rootzone (WBALRT) and whole system (WBALTT)
      WBALRT<- TOTINF + WI + WDRT - EVST - wtrat - PERCT - W
      WBALTT<- SSI + RAINT + TOTIRR + WI - W + WLOWI -
        WLOW - wtrat - EVWT - EVST - TSR - LOSST - SS

      if (abs(WBALRT) > 0.0001){
        stop('Water balance for root zone does not close.')
      }

      if (abs(WBALTT) > 0.0001){
        stop('Water balance for complete soil profile does not close.')

      }
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
