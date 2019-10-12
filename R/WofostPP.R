# Potential production ####

#' Wofost Potential Production
#'
#' Computes potential production as per Wofost crop growth algorithm
#'
#' @param crop CorpObject. Object of class CropObject containing the specific
#' crop parameters. See ?CropObject.
#' @param w WeatherObject. Object of class CropObject containing the climatic
#' driving variables. See ?WeatherObject.
#' @param startType Development stage at which the simulation is started.
#' Either "sowing" or "emergence".
#' @param finishType Variable describing the conditionst triggering
#' the end of the simulation.
#' Can be either "maturity" -The model is terminated at - or 7 days after -
#' maturity is reached, or an integer [1:365] representing the maximum
#' number of days for which the model is run.
#' @param varReturn Character vector specifying which variables to output.
#' Potentially, any of the variables produced inside the Wofost function
#' can be returned. However the use of carReturn = NULL (default) is
#' encouraged. By default returning variables described in "Returns".
#' @param activate.verndvs Logical. If TRUE, allows the use of variable
#' "VERNDVS". A critical development stage (VERNDVS) is used to stop the effect
#' of vernalisation when this DVS is reached. This is done to improve model
#' stability in order to avoid that Anthesis is never reached due to a
#' somewhat too high VERNSAT. Nevertheless, a warning is written to the log
#' file, if this happens.
#' @param activate.stopInSeven Logical. If TRUE, the simulation stops seven
#' days after maturity is reached. If FALSE, the simulation terminates when
#' maturity is reached. Ignored if "finishType" is numeric.
#' @export
#'
WofostPP<- function(
  crop, w,
  startType= 'sowing',
  finishType= 'maturity',
  varReturn= NULL,
  activate.verndvs= TRUE,
  activate.stopInSeven= FALSE

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
  RDMSOL = 100  # Max rootable soil depth would be from SoilObject
                # but only affect the water balance, so is set to 1m here.

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
      vernalisation.warning<- "on" # switch ensuring the vernalisation warning
                                   # is printed only once

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

    # Stop if WeatherObject is too short
    if (t > length(w@DAY)){
      stop(paste0('Reached last day of WeatherObject (', w@DAY[t],
                  ') but "finishType" condition not reached yet (dvs = ',
                  dvs, '"finishType" value = ', t,'/', finishType,').'))
    }

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
    if (IDSL >= 2){ # if pre-anthesis development depends on daylength
      # and vernalisation
      vernfac<- 0
      if (stage == 'vegetative'){
        if (isFALSE(isvernalised)){
          # if vernalisation requirements not reached yet
          if (dvs < VERNDVS){
            vernr<- afgen(temp,VERNRTB)
            r<- (vern - VERNBASE)/(VERNSAT - VERNBASE)
            vernfac<- limit(0, 1, r)
          } else if (isTRUE(activate.verndvs)){
            vernr<- 0
            vernfac<- 1
            force_vernalisation<- TRUE
          } else if (isFALSE(activate.verndvs)){
            vernr<- afgen(temp,VERNRTB)
            r<- (vern - VERNBASE)/(VERNSAT - VERNBASE)
            vernfac<- limit(0, 1, r)
          }
        } else { # if vernalisation requirement are fullfilled
          vernr<- 0
          vernfac<- 1
        }
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

      # Water stress reduction (not happening in PP)
      gass<- pgass


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
      dw1d<- 0

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

    } else if (class(finishType) == 'numeric' | # finishType = No. of days
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
          vern<- vern + vernr
          if (vern >= VERNSAT){  # Vernalisation requirements reached
            isvernalised<- TRUE
            if (is.null(dov)){
              dov<- w@DAY[t]
            }
          } else if (isTRUE(force_vernalisation)){
            isvernalised<- TRUE
            if (vernalisation.warning == 'on'){
              warning(paste0(
                'Critical DVS for vernalisation (VERNDVS) reached at day ',
                w@DAY[t],
                ' but vernalisation requirements not yet fulfilled.','\n',
                'Forcing vernalisation now (vern/VERNSAT = ',
                round(vern), '/', VERNSAT,').'))
            }
            vernalisation.warning <- 'off' # switch of the warning
          } else {
            isvernalised<- FALSE
          }
        }
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
        if (isTRUE(activate.stopInSeven)){
          stopInSeven<- stopInSeven - 1
        } else {stopInSeven<- 0}

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


