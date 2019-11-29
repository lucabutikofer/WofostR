
#' Evapo(transpiration) following Penman's formula
#'
#' Computes and adds E0, ES0, ET0 to a WeatherObject where these are absent
#' using Penmans formulation.
#'
#' @details This routine calculates the potential evapo(transpi)ration rates
#' from a free water surface (E0), a bare soil surface (ES0), and a crop canopy
  #' (ET0) in mm/d. For these calculations the analysis by Penman is followed
  #' (Frere and Popov, 1979;Penman, 1948, 1956, and 1963).
#'
#' @param w WeatherObject where E0, ES0 and ET0 are not computed yet.
#' @return WeatherObject w with added E0, ES0 and ET0 slots.
#' @export
#'
#' @examples
#'
#' w <- randomWeather
#' w@E0 = NULL
#' w@ES0 = NULL
#' w@ET0 = NULL
#' w <- Penman(w)
#' w
Penman <- function(w){


#   Input variables from w:
#
#       LAT     -  Latitude of the site                        degrees
#       ELEV    -  Elevation above sea level                      m
#       TMIN    -  Minimum temperature                            C
#       TMAX    -  Maximum temperature                            C
#       AVRAD   -  Daily shortwave radiation                   J m-2 d-1
#       VAP     -  24 hour average vapour pressure               hPa
#       WIND2   -  24 hour average windspeed at 2 meter          m/s
#
#   Output:
#
#       E0      -  Penman potential evaporation from a free
#                  water surface [mm/d]
#       ES0     -  Penman potential evaporation from a moist bare
#                  soil surface [mm/d]
#       ET0     -  Penman potential transpiration from a
#                  crop canopy [mm/d]


  # prepare outputs
  E0 <- NULL; ES0 <- NULL; ET0 <- NULL

  for (t in 1:length(w@DAY)){

    # weather variables
    TMIN <- w@TMIN[t]
    TMAX <- w@TMAX[t]
    AVRAD <- w@IRRAD[t]
    VAP <- w@VAP[t]
    WIND2 <- w@WIND[t]
    ELEV <- w@ELEV[t]
    LAT <- w@LAT[1]

    # Angstrom formula constants
    ANGSTA <- 0.4885 - 0.0052*LAT
    ANGSTB <- 0.1563 + 0.0074*LAT


    # psychrometric instrument constant (mbar/Celsius-1)
    # albedo for water surface, soil surface and canopy
    # latent heat of evaporation of water (J/kg=J/mm)
    # Stefan Boltzmann constant (in J/m2/d/K4, e.g multiplied by 24*60*60)
    PSYCON = 0.67; REFCFW = 0.05; REFCFS = 0.15; REFCFC = 0.25
    LHVAP = 2.45e6; STBC =  5.670373e-8 * 24*60*60 # (=4.9E-3)

    # preparatory calculations
    # mean daily temperature and temperature difference (Celsius)
    # coefficient Bu in wind function, dependent on temperature
    # difference
    TMPA <- (TMIN + TMAX)/2
    TDIF <- TMAX - TMIN
    BU <- 0.54 + 0.35 * limit(0, 1, (TDIF - 12)/4)

    # barometric pressure (mbar)
    # psychrometric constant (mbar/Celsius)
    PBAR <- 1013*exp(-0.034*ELEV/(TMPA + 273))
    GAMMA <- PSYCON*PBAR/1013

    # saturated vapour pressure according to equation of Goudriaan
    # (1977) derivative of SVAP with respect to temperature, i.e.
    # slope of the SVAP-temperature curve (mbar/Celsius);
    # measured vapour pressure not to exceed saturated vapour pressure

    SVAP <- 6.10588 * exp(17.32491*TMPA/(TMPA + 238.102))
    DELTA <- 238.102*17.32491*SVAP/(TMPA + 238.102)^2
    VAP <- min(VAP, SVAP)

    # the expression n/N (RELSSD) from the Penman formula is estimated
    # from the Angstrom formula: RI=RA(A+B.n/N) -> n/N=(RI/RA-A)/B,
    # where RI/RA is the atmospheric transmission obtained by a CALL
    # to ASTRO:

    r <- Astro(w = w, t = t, lat = LAT)
    RELSSD <- limit(0, 1, (r$tatm - abs(ANGSTA))/abs(ANGSTB))

    # Terms in Penman formula, for water, soil and canopy

    # net outgoing long-wave radiation (J/m2/d) acc. to Brunt (1932)
    RB <- STBC*((TMPA + 273)^4)*(0.56 - 0.079*sqrt(VAP))*(0.1 + 0.9*RELSSD)

    # net absorbed radiation, expressed in mm/d
    RNW <- (AVRAD*(1 - REFCFW) - RB)/LHVAP
    RNS <- (AVRAD*(1 - REFCFS) - RB)/LHVAP
    RNC <- (AVRAD*(1 - REFCFC) - RB)/LHVAP

    # evaporative demand of the atmosphere (mm/d)
    EA <- 0.26 * max(0, (SVAP - VAP)) * (0.5 + BU*WIND2)
    EAC <- 0.26 * max(0, (SVAP - VAP)) * (1 + BU*WIND2)

    # Penman formula (1948)
    e0 <- (DELTA*RNW + GAMMA*EA)/(DELTA + GAMMA)
    es0 <- (DELTA*RNS + GAMMA*EA)/(DELTA + GAMMA)
    et0 <- (DELTA*RNC + GAMMA*EAC)/(DELTA + GAMMA)

    # Ensure reference evaporation >= 0.
    E0[t] <- max(0, e0)
    ES0[t] <- max(0, es0)
    ET0[t] <- max(0, et0)
  }

  w@E0 <- E0/10  # "*10" is added to transform output from mm/day to cm/day
  w@ES0 <- ES0/10
  w@ET0 <- ET0/10

  return(w)

}
