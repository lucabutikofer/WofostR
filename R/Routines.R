
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


Assimilation<- function(
                        AMAXTB,TMPFTB,KDIFTB,EFFTB,TMNFTB,
                        d, lai, sgd, dp, sinbm, sinld, cosld, dvs,
                        tday, w, t, tmins
                        ){

  # CO2 assimilation routine
  #
  # Computes "pgass" (Potential daily CO2 assimilation rate).
  # Calls functions "totass()" and "assim()"
  #
  # @param tday Average daytime temperature
  # @param d From Astro. Astronomic day length
  # @param sinld From Astro. Seasonal offset of sine of solar height
  # @param cosld From Astro. Amplitude of sine of solar height
  # @param lat Latitude
  # @param sgd Daily global radiation
  # @param sinbm From Astro. Integral of effective solar height
  # @param dp From Astro. Diffuse radiation perpendicular to the direction light
  # @param lai Leaf area index of whole canopy
  # @param dvs Development stage
  # @param t Day in the loop
  # @param AMAXTB Afgen table (inst. gross assimilation rate at light saturation
  #               vs. development stage)
  # @param TMPFTB Afgen table (inst. gross assimilation rate at light saturation
  #               vs. daytime temperature)
  # @param TMNFTB Afgen table (inst. gross assimilation rate at light saturation
  #               vs. 7 days running minumum temperature)
  # @param EFFTB Afgen table (light use efficiency vs. daily mean temp),
  #              crop-specific [kg ha-1 hr-1 j-1 m2 s]
  #


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


totass<- function(d, amax, eff, lai, kdif, sgd,
                  dp, sinbm, sinld, cosld){

  # Assimilation of CO2 throughout the day
  #
  # This routine calculates the daily total gross CO2 assimilation by
  # performing a Gaussian integration over time. At three different times of
  # the day, irradiance is computed and used to calculate the instantaneous
  # canopy assimilation, whereafter integration takes place. More information
  # on this routine is given by Spitters et al. (1988).
  #
  # Called by Assimilation(). Calls Assim().
  #
  #


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


assim<- function(amax,eff,lai,kdif,sinb,pardir,pardif){

  # Assimilation of CO2 throughout the canopy
  #
  # This routine calculates the gross CO2 assimilation rate of
  # the whole crop, FGROS, by performing a Gaussian integration
  # over depth in the crop canopy. At three different depths in
  # the canopy, (i.e. for different values of LAI) the
  # assimilation rate is computed for given fluxes of photosynthe-
  # tically active radiation, whereafter integration over depth
  # takes place. More information on this routine is given by
  # Spitters et al. (1988). The input variables SINB, PARDIR
  # and PARDIF are calculated in routine TOTASS.
  #
  # Called by totas()
  #
  #


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
#' @param SM0 Soil porosity
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
