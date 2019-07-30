
#' Phenology module
#'
#' Computes the phenological development of the crop
#'
#' @param crop CorpObject. Object of class CropObject containing the specific
#' crop parameters. See ?CropObject.
#' @param w WeatherObject. Object of class CropObject containing the climatic
#' driving variables. See ?WeatherObject.
#' @param startType Development stage at which the simulation is started.
#' Either "sowing" or "emergence".
#' @param finishType Variable describing the conditionst riggering
#' the end of the simulation.
#' Can be either "maturity" -The model is terminated 7 days after maturity is
#' reached - or
#' an integer [1:365] -Maximum number of days for which the model is run.
#'
#' @export
#'
Phenology<- function(
  crop, w,
  startType= 'sowing',
  finishType= 'maturity'
){

  crop_start_type = startType
  DLC = crop@DLC
  DLO = crop@DLO
  DTSMTB = crop@DTSMTB
  DVSEND = crop@DVSEND
  DVSI = crop@DVSI
  IDSL = crop@IDSL
  TBASEM = crop@TBASEM
  TEFFMX = crop@TEFFMX
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
  dvr_out<- NULL
  dvs_out<- NULL
  vern_out<- NULL
  vernfac_out<- NULL
  vernr_out<- NULL

  # Helper variables
  astro<- 0
  stopInSeven<- 7
  vernr<- 0

  # LOOP STARTS ####
  #~~~~~~~~~~~~~~~~~

  while (isFALSE(STOP)){ # main loop.
    # 't' counts the days, one iteration per day.

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > INITIALISATION ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t == 1){ # first iteration of the model

      # Set initial values for phenology
      tsum<- 0
      tsume<- 0
      vern<- 0
      isvernalised<- FALSE
      force_vernalisation<- FALSE
      dov<- NULL # date of vernalisation

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

    } # end of first iteration of the model

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > RATES COMPUTATIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # average temperature and average daytemperature
    temp<- (w@TMIN[t] + w@TMAX[t])/2

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
      vernfac<- 0
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

      dvs_out[t]<-     dvs
      dvr_out[t]<-     dvr
      vern_out[t]<-    vern
      vernfac_out[t]<- vernfac
      vernr_out[t]<-   vernr


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > TIMER UPDATE ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      t<- t + 1

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > INTEGRATION ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    } # end of post-finish-conditions-test section
  } # end of daily cycles

  # LOOP ENDS ####
  #~~~~~~~~~~~~~~~

  # FINALISATION ####
  #~~~~~~~~~~~~~~~~~~

    return(list(
      'dvr'= dvr_out,
      'dvs'= dvs_out,
      'vern'= vern_out,
      'vernfac'= vernfac_out,
      'vernr'= vernr_out
    ))
}


