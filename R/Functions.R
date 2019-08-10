

#### SUPPORT FUNCTIONS ####
#~*~*~*~*~*~*~*~*~*~*~*~*~#


# Sine in decimal degrees (personal function)

sind<- function(degrees) {

  #sine: the sine of an angle expressed in decimal degrees.
  #degrees: angle in decimal degrees (either number or numeric vector)

  sine<- sin(degrees*pi/180)
  return(sine)
}


# Cosine in decimal degrees (personal function)

cosd<- function(degrees) {

  #cosd: the cosine of an angle expressed in decimal degrees.
  #degrees: angle in decimal degrees (either number or numeric vector)

  cosine<- cos(degrees*pi/180)
  return(cosine)
}


# Tan in decimal degrees (personal function)

tand<- function(degrees) {

  #tand: the tangent of an angle expressed in decimal degrees.
  #degrees: angle in decimal degrees (either number or numeric vector)

  tangent<- tan(degrees*pi/180)
  return(tangent)
}


# Arc-sine in decimal degrees (personal function)

asind<- function(x) {

  #asind: the arc-sine of an angle expressed in decimal degrees.
  #x: sine to be converted in degrees (either number or numeric vector)

  arcsine<- asin(x)*(180/pi)
  return(arcsine)
}


# Arbitrary Function GENerator (Appendix 2)

afgen<- function(intx, afg){

  #afgen stands for Arbitrary Function GENerator
  #intx: x value for which to interpolate the function
  #inty: y value to obtain from the interpolation of the function
  #afg: specific afgen table from which to retrieve x and y
  #x: independent variable of the arbitrary function (vector)
  #y: dependent variable of the arbitrary function (vector)

  x<- afg[,1]
  y<- afg[,2]

  if (intx<= min(x)){inty<- y[1]}
  else if (intx>= max(x)){inty<- y[length(y)]}
  else if (intx> min(x) & intx<max(x)){
    xl<- max(x[x<intx])  # x values left of intx
    xr<- min(x[x>=intx])  # x values right of intx
    yl<- y[which(x == xl)]  # y values left of inty
    yr<- y[which(x == xr)]  # y values right of inty

    inty<- yl + ((intx-xl)*(yr-yl)/(xr-xl))
  }
  return(inty)
}


# Limit within range

limit<- function(min,max,x){

  if(min>max){stop('"min" must be smaller than "max"')}
  else if(x<=min){return(min)}
  else if(x>=max){return(max)}
  else if(x>min & x<max){return(x)}
}


#### ASTRONOMICAL FUNCTIONS ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*#

# Solar declination (4.22)

sol_dec<- function(td){

  #sold: solar declination [RADIANS]
  #td: number of the day since 1st of Jan

  sold<- -asin(sind(23.45) * cos(2*pi*(td + 10)/365))
  # line above modified from supit 1994 to work in radians
  # like in FORTRAN Wofost 7.1.7
  # original was: sold<- -(23.45) * cos(2*pi*(td + 10)/365)
  return(sold)
}


# Solar constant at the top of the atmosphere (4.23)

sol_const<- function(td){

  #scd: solar constant at the top of the atmosphere
  #td: number of day since 1st of Jan

  scd<- 1370*(1 + 0.033*cos(2*pi*td/365))
  return(scd)
}


# Day length (4.25)

day_length<- function(lat, sold){

  #lat: latitude
  #sold: solar declination

  sinld<- sind(lat)*sin(sold)
  cosld<- cosd(lat)*cos(sold)
  aob<- sinld/cosld

  if (abs(aob) <= 1){ # for aob between -1 and 1
    d<- 12*(1+2*asin(aob)/pi)
  } else if (aob > 1){
    d<- 24
  } else if (aob < -1){
    d<- 0
  }

  return(d)
}


# photosynthetic day length (for photosynthetic activity)

day_length_corr<- function(lat, sold){

  #lat: latitude
  #sold: solar declination

  sinld<- sind(lat)*sin(sold)
  cosld<- cosd(lat)*cos(sold)
  aobcorr<- (-sind(-4) + sinld)/cosld

  if (abs(aobcorr) <= 1){ # for aobcorr between -1 and 1
    dlp<- 12*(1+2*asin(aobcorr)/pi)
  } else if (aobcorr > 1){
    dlp<- 24
  } else if (aobcorr < -1){
    dlp<- 0
  }

  return(dlp)
}


# Integral of solar height (4.26)

int_sol_hgt<- function(lat, sold, d){

  #lat: latitude
  #sold: solar declination
  #d: day length

  sinld<- sind(lat)*sin(sold)
  cosld<- cosd(lat)*cos(sold)
  aob<- sinld/cosld

  if (abs(aob) <= 1){ # for aob between -1 and 1
    sinbdth<- 3600*(d*sinld + 24*cosld*sqrt(1-aob^2)/pi)
  } else if (abs(aob) > 1){ # for aob bigger than 1 or smaller than -1
    sinbdth<- 3600*(d*sinld)
  }

  return(sinbdth)
}


# Daily exrta-terrestrial radiation (4.27)

day_extrad<- function(scd, sinbdth){

  #scd: solar constant at the top of the atmosphere
  #sinbdth: integral of solar height

  sod<- scd*sinbdth
  return(sod)
}


# Integral of effective solar height (4.28)

int_effsol_hgt<- function(d, lat, sold){

  #d: day length
  #lat: latitude
  #sold: solar declination

  sinld<- sind(lat)*sin(sold)
  cosld<- cosd(lat)*cos(sold)
  aob<- sinld/cosld

  if (abs(aob)<=1){ # for aob between -1 and 1
    sinbm<- 3600*(d*(sinld+0.4*(sinld^2 + cosld^2*0.5))+
                  12*cosld*(2+3*0.4*sinld)*sqrt(1-aob^2)/pi)
  } else if (abs(aob) > 1){ # for aob bigger than 1 or smaller than -1
    sinbm<- 3600*(d*(sinld+0.4*(sinld^2 + cosld^2*0.5)))
  }

  return(sinbm)
}


# Atmospheric transmission (4.29)

atm_tra<- function(sgd, sod){

  #tatm: atmospheric transmission
  #sgd: daily global radiation
  #sod: daily exrta-terrestrial radiation

  tatm<- sgd/sod
  return(tatm)
}


# Diffuse vs. global irradiance ratio (4.30)

df_g_ratio<- function(sgd, sod){

  #SdfSg: diffuse vs. global irradiance ratio
  #sod: daily exrta-terrestrial radiation
  #sgd: daily global radiation

  if (sgd/sod <= 0.07){SdfSg<- 1}
  else if (sgd/sod > 0.07 & sgd/sod <= 0.35){
    SdfSg<- 1-(2.3*(sgd/sod - 0.07)^2)
  }
  else if(sgd/sod > 0.35 & sgd/sod <= 0.75){
    SdfSg<- 1.33 - (1.46*sgd/sod)
  }
  else if(sgd/sod > 0.75){SdfSg<- 0.23}
  return(SdfSg)
}


# Diffuse radiation perpendicular to the direction of light

diff_perp<- function(SdfSg, tatm, scd){

  #dp: diffuse radiation perpendicular to the direction of light
  #SdfSg: diffuse vs. global irradiance ratio
  #tatm: atmospheric transmission
  #scd: solar constant at the top of the atmosphere

  dp<- SdfSg*tatm*0.5*scd
  return(dp)
}


#### METEOROLOGICAL FUNCTIONS ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*#

# Average daytime temperature (5.32)

av_dayt<- function(tmax, tmin){

  #tday: average daytime temperature
  #tmax: maximum daytime temperature
  #tmin: minimum daytime temperature

  tday<- (tmax + ((tmax + tmin) / 2)) / 2
  return(tday)
}


# 7 days average of minimum temperature (5.33)

week_tmin_av<- function(tmins){

  #tlow: 7 days average of minimum temperature
  #tmins: vector of daily minimum temp. for the week

  tlow<- sum(tmins) / length(tmins)
  return(tlow)
}


#### PHENOLOGICAL DEVELOPMENT ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*#

# Growth Rate (5.1)

growth_rate<- function(ce, a, rm){

  #gr: growth rate [kg dry matter / d * ha]
  #ce: conversion efficiency [kg dry matter / kg CH2O]
  #a: gross assimilation [kg CH2O /  d * ha]
  #rm: maintenance respiration  [kg CH2O /  d * ha]

  gr<- (ce*(a-rm))
  return(gr)
}


# Effective temperature (5.2)

effect_temp<- function(TEFFMX, TBASEM, temp){

  #te: effective temperature [°C]
  #TEFFMX: maximum temperature beyond which  activity does not increase [°C]
  #TBASEM: base temperature below which phenological development stops [°C]
  #temp: average temperature [°C]

  te<- limit(0, TEFFMX - TBASEM, temp - TBASEM)
  return(te)
}


# Development rate (5.3)

dev_rate<- function(dt, ts, tday, DTSMTB){

  #dvr: development rate
  #dt: temperature dependent correction factor (DTSMTB, AFGEN table)
  #ts: temperature sum required to complete development stage (TSUM1 or TSUM2)
  #tday: average temperature for the time step
  #DTSMTB: AFGEN table (temperature vs. temperature correction factor)

  dt<- afgen(tday,DTSMTB)
  dvr<- dt/ts
  return(dvr)
}


# Development stage (5.4)

dev_stage<- function(dvr, pds, tstep=1){

  #dvs: development stage
  #pds: previous development stage
  #dvr: development rate
  #tstep: time step (one day)

  dvs<- pds + dvr*tstep
  return(dvs)
}


# Reduction factor for day length sensityvity (5.5)

dayl_red<- function(dlp,dlc,dlo){

  #dvred: reduction factor of dev. rate (to be multiplied with 5.3 in
  #      photosensitive species)
  #dlp: photoperiodic day length
  #dlc: critical (lower) day length threshold for development
  #dlo: optimum (upper) day length value for development

  dvred<- limit(0, 1, ((dlp - dlc) / (dlo - dlc)))
  return(dvred)
}


#### DAILY ASSIMILATION ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*#

# Selection of the hours of the day for the Gaussian (5.6)
# integration of daily assimilation

hod_sel<- function(d){

  #th: hour of the day
  #d: day leght (4.25)
  #p: Gaussian integration points

  p<- c(-1,0,1)
  th<- 12 + 0.5*d*(0.5 + p*sqrt(0.15))
  return(th)
}


# Solar height (5.7)

sol_hgt<- function(sold, th, lat){

  #sinb: solar height (sine of b, b being solar elevation)
  #sold: solar declination
  #th: hour of the day (5.6)
  #lat: latitude

  sinld<- sind(lat)*sin(sold)
  cosld<- cosd(lat)*cos(sold)

  sinb<- sinld + (cosld*cos(2*pi*((th + 12) / 24)))
  sinb[sinb<0]<- 0  # changes negative values to 0
  return(sinb)
}


# Photosynthetically Active Radiation (PAR) flux (5.8)

par_flux<- function(sgd, sinb, sinbm){

  #i0: PAR flux
  #sgd: daily global radiation
  #sinb: solar height (sine of b, b being solar elevation) (5.7)
  #sinbm: corrected integral of solar height over the day (4.28)

  i0<- 0.5*sgd*(sinb*(1 + 0.4*sinb) / sinbm)
  return(i0)
}


# Diffuse photosynthetically Active Radiation (PAR) flux (5.9)

diff_par_flux<- function(dp, sinb){

  #i0df: diffuse part of the photosynthetically active radiation flux
  #      at top of the canopy
  #dp: diffuse radiation perpendicular to the direction light (4.31)
  #sinb: solar height (sine of b, b being solar elevation) (5.7)

  i0df<- dp*sinb
  return(i0df)
}


# Direct photosynthetically Active Radiation (PAR) flux (5.10)

dir_par_flux<- function(i0, i0df){

  #i0dr: direct part of the photosynthetically active radiation flux
  #      at top of the canopy
  #i0df: diffuse part of the photosynthetically active radiation flux (5.9)
  #      at top of the canopy
  #i0: PAR flux

  i0dr<- i0 - i0df
  return(i0dr)
}


# Total gross assimilation rate (whole canopy for a day) (5.11)

tot_day_asm<- function(d, ac){

  #ad: total gross assimilation rate (whole canopy for a day)[kg/ha*d]
  #d: day length (4.25)
  #ac: vector of length=3. Total instantaneous gross assimilation rate for
  #    the whole canopy (5.31). ac[1], ac[2] and ac[3] refer to p= -1,0,1
  #    in the Gaussian integration over time.

  ad<- d*((ac[1] + 1.6*ac[2] + ac[3]) / 3.6)
  return(ad)
}


# Canopy radiation reflection coefficient (5.12)

ref_coef<- function(s,sinb){

  #p: reflection coefficient
  #s: scattering coefficient fraction (transmission and reflection) (=0.2)
  #sinb: sin of solar elevation (5.7)

  p<- ((1 - sqrt(1-s)) / (1 + sqrt(1-s))) * (2 / (1 + 1.6*sinb))
  return(p)
}


# Black leaves extinction coefficient (5.14)

bl_coef<- function(sinb){

  #kbl: extinction coefficient for the direct radiation flux
  #sinb: solar elevation [degrees]

  kbl<- 0.5 / sinb
  return(kbl)
}


# Diffuse radiation extinction coefficient (5.15)

# NOTE: In the model, the extinction coefficient for the diffuse radiation flux,
# κdf (acronym: KDIF), is not computed but should be provided by the user.
# It can be measured directly under diffuse sky conditions.

df_coef<- function(s, kbl){

  #kdf: extinction coefficient for the diffuse radiation flux
  #s: scattering coefficient fraction of single leaves for visible radiation
  #kbl: extinction coefficient for the direct radiation flux

  kdf<- kbl * sqrt(1 - s)
  return(kdf)
}


# Cluster factor for leaf area distribution (5.16)

clus_fac<- function(kdf,s){

  #cf: cluster factor
  #kdf: extinction coefficient for diffuse radiation flux
  #s: scattering coefficient fraction of single leaves for visible radiation

  cf<- kdf / (0.8 * sqrt(1 - s))
  return(cf)
}


# Extinction coefficient for the direct component of direct radiation (5.17)

drbl_coef<- function(sinb, cf){

  #kdrbl: extinction coefficient for the direct component of direct radiation
  #b: solar elevation [degrees]
  #cd: cluster factor (5.16)

  kdrbl<- cf * 0.5 / sinb
  return(kdrbl)
}


# Direct radiation extinction coefficient (5.18)

drt_coef<- function(kdrbl, s){

  #kdrt: Extinction coefficient for total direct radiation flux
  #s: scattering coefficient
  #kdrbl: extinction coefficient for the direct radiation flux

  kdrt<- kdrbl * sqrt(1 - s)
  return(kdrt)
}


#### ABSORPTION ####
#~*~*~*~*~*~*~*~*~*#

# Leaf area index at given horizon (5.19)

lai_horiz<- function(lai,p=c(-1,0,1)){

  #lail: leaf area index at given horizon
  #p: selected horizon
  #lai: leaf area index (see chapter 5.4.5 in Supit 1994)

  lail<- (0.5 + p * sqrt(0.15)) * lai
  return(lail)
}


# Absorbed radiation at given canopy horizon (5.20)

abs_rad<- function(i0, k, p, lail){

  #ialdf: absorbed diffuse radiation at relative depth l [J / m^2*s]
  #i0df: diffuse PAR flux at top of the canopy [J / m^2*s]
  #k: extinction coefficient for the PAR flux
  #p: reflection coefficient of the canopy (see eq. 5.12)
  #lail: leaf area index at depth l

  ia<- k * (1 - p) * i0 * exp(-k * lail)
  return(ia)
}


# Absorbed diffuse radiation at given canopy horizon (5.21a)

abs_diff_rad<- function(i0df, kdf, p, lail){

  #ialdf: absorbed diffuse radiation at relative depth l [J / m^2*s]
  #i0df: diffuse PAR flux at top of the canopy [J / m^2*s]
  #k: extinction coefficient for the specific radiations (see 5.15, -17, -18)
  #p: reflection coefficient of the canopy (see eq. 5.12)
  #lail: leaf area index at depth l

  iadf<- kdf * (1 - p) * i0df * exp(-kdf * lail)
  return(iadf)
}


# Absorbed total direct radiation at given canopy horizon (5.21b)

abs_dirt_rad<- function(i0dr, kdrt, p, lail){

  #iadrt: absorbed total direct radiation at relative depth l [J / m^2*s]
  #i0dr: direct PAR flux at top of the canopy [J / m^2*s]
  #k: extinction coefficient for the specific radiations (see 5.15, -17, -18)
  #p: reflection coefficient of the canopy (see eq. 5.12)
  #lail: leaf area index at depth l

  iadrt<- kdrt * (1 - p) * i0dr * exp(-kdrt * lail)
  return(iadrt)
}


# Absorbed direct component of direct radiation at given canopy horizon (5.21c)

abs_dirdr_rad<- function(i0dr, kdrbl, s, lail){

  #iadrdr: absorbed direct component of direct radiation at relative depth l
  #        [J / m^2*s]
  #i0dr: direct PAR flux at top of the canopy [J / m^2*s]
  #k: extinction coefficient for the specific radiations (see 5.15, -17, -18)
  #s: scattering coefficient
  #lail: leaf area index at depth l

  iadrdr<- kdrbl * (1 - s) * i0dr * exp(-kdrbl * lail)
  return(iadrdr)
}


# Absorbed radiation for shaded leaves (5.22)

abs_sh<- function(iadf, iadrt, iadrdr){

  #iash: absorbed amount of total radiation flux by shaded leaves [J / m*s]
  #iadf: absorbed amount of diffuse radiation flux*
  #iadrt: absorbed amount of total direct radiation flux*
  #iadrdr: absorbed amount of direct component of the direct radiation flux*
  #*: values obtained by using different k values in abs_rad()

  iash<- iadf + iadrt - iadrdr
  return(iash)
}


# Instantaneous gross assimilation (5.23)

inst_gras<- function(am, ia, e){

  #al: inst. gross assimilation rate at  depth l per unit leaf area [kg / ha*h]
  #am: inst. gross assimilation rate at light saturation (AMAXTB) [kg / ha*h]
  #e: initial light use efficiency [(kg / ha*h) / (J / m^2*s)]
  #ia: absorbed amount of the total radiation flux [J / m^2*s]

  al<- am * (1 - exp((-e * ia) / am))
  return(al)
}


# Instantaneous gross assimilation rate for shaded leaves (5.24)

inst_gras_sh<- function(am, iash, e){

  #ash: inst. gross assimilation rate for shaded leaves [kg / ha*h]
  #am: inst. gross assimilation rate at light saturation (AMAXTB) [kg / ha*h]
  #e: initial light use efficiency [(kg / ha*h) / (J / m^2*s)]
  #iash: absorbed amount of total radiation flux by shaded leaves [J / m*s]

  ash<- am * (1 - exp((-e * iash) / max(2,am)))
  # max(2,...) was copied from FORTRAN wofost 7.1.7
  return(ash)
}


# Absorbed radiation by leaves perpendicular to direct beam (5.25)

abs_perp<- function(i0dr, s, sinb){

  #iadrsl: Absorbed radiation by leaves perpendicular to direct beam
  #        [J / m^2*s]
  #i0dr: direct flux of visible radiation at the top of the canopy [J / m^2*s]
  #s: scattering coefficient [-]
  #b: solar elevation [degrees]

  iadrsl<- ((1 - s) * i0dr) / sinb
  return(iadrsl)
}


# Instantaneous gross assimilation for direct light on sunlit leaves (5.26)

inst_gras_dir<- function(ash, am, iadrsl, e){

  #asl: inst. gross assimilation rate for sunlit leaves [kg / ha*h]
  #ash: inst. gross assimilation rate for shaded leaves [kg / ha*h] (5.24)
  #am: inst. gross assimilation rate at light saturation (AMAXTB) [kg / ha*h]
  #iadrsl: Absorbed radiation by leaves perpendicular to direct beam
  #        [J / m^2*s] (5.25)
  #e: initial light use efficiency [(kg / ha*h) / (J / m^2*s)]

  asl<- am * (1 - (am - ash) *
        (1 - exp((-iadrsl * e) / max(2,am))) / (e * iadrsl))
  # max(2,...) was copied from FORTRAN wofost 7.1.7
  return(asl)
}


# Fraction of sunlit area at a given horizon (5.27)

fr_sl<- function(kdrbl, lail){

  #fsl: fraction of sunlit leaf area at given horizon,
  #     each list element is at a different time of the day.
  #kdrbl: extinction coefficient for direct component of direct radiation (5.17)
  #       length of 3, each element refers to a different time of the day.
  #lail: cumulative leaf area index at relative depth l in canopy [-]

  fsl<- exp(-kdrbl * lail)
  return(fsl)
}


# Total instantaneous assimilation rate at horizon l (5.28)

inst_gras_tot<- function(asl, ash, fsl){

  #atl: total instantaneous assimilation rate at horizon l [kg / ha*h]
  #asl: inst. gross assimilation rate for sunlit leaves [kg / ha*h] (5.26)
  #ash: inst. gross assimilation rate for shaded leaves [kg / ha*h] (5.24)
  #fsl: fraction sunlit leaf area [-]

  atl<- fsl * asl + ((1 - fsl) * ash)
  return(atl)
}


# Leaf area index at horizon l (5.29) is same as lai_horiz() (5.19)


# Total instantaneous canopy assimilation (5.31)

can_totas<- function(acl, lai){

  #ac: total inst. gross assimilation rate for the whole canopy
  #acl: total inst. gross assimilation rate per unit leaf area (5.30)
  #lai: total leaf area of the grop (5.4.5)

  ac<- acl * lai
  return(ac)
}

# Note that the green parts of the stems and the storage organs (like panicles)
# may absorb a substantial amount of radiation. Therefore, the green area index
# of these organs is added to the total leaf area. The green area index of the
# stems and storage organs can be calculated by multiplying the dry weight of
# the organ with respectively the specific stem area and the specific pod area
# (see also eq. 5.45 and eq. 5.54). The specific stem area and specific pod area
# are crop specific and should be provided by the user.


#### CROP GROWTH FUNCTIONS ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~#

# Gross daily CH20 assimilation rate (5.36)

grcarb_asrate_day<- function(ad){

  #rd: gross daily CH2O assimilation rate
  #ad: Gross daily CO2 assimilation rate (5.11)

  rd1<- ad * 30/40
  return(rd1)
}


# Gross daily CH20 assimilation rate if water-streessed (5.37)

grcarb_asrate_day_w<- function(rd1,ta,tp){

  #rd: gross daily CH2O assimilation rate corrected for water stress
  #rd1: gross daily CH2O assimilation rate not corrected for water stress (5.32)
  #ta: actual transpiration [cm / d]
  #tp: potential transpiration [cm / d]

  rd<- rd1 * ta / tp
  return(rd)
}


# Maintanance respiration rate (5.39)

maint_resp<- function(cmi, wi){

  #rmr: maintanance respiration rate at 25 degrees [kg / ha*d]
  #cmi: maintenance coefficient of given organ i
  #wi: dry matter weight of organ i (5.49)
  #i: leaves (lv), storage organs (so), stems (st) or roots (rt)

  rmr<- sum(cmi*wi)
  return(rmr)
}


# Maintanance respiration rate at temperature t (5.40)

maint_resp_t<- function(rmr, Q10, temp, tr=25){

  #rmt: maintenance respiration rate at temperature t
  #rmr: maintenance respiration rate at reference temperature of 25°C (5.39)
  #Q10: relative increase of the respiration rate per 10°C temperature increase
  #temp: average temperature
  #tr: reference temperature [=25°C in the model]

  rmt<- rmr * Q10^((temp - tr)/10)
  return(rmt)
}


# Growth respiratio rate (5.41)

gr_rate<- function(rd, rmt){

  #rg: growth respiratio rate [kg / ha*d]
  #rd: actual daily CH2O assimilation rate (5.37)
  #rmt: maintenance respiration rate at temperature t (5.40)

  rg<- rd - rmt
  return(rg)
}


# Conversion efficiency of assimilates into dry matter

conv_eff<- function(cei,pci){

  #ce: conversion efficiency factor of assimilates [kg / kg]
  #cei: conversion efficiency factor of the assimilates of organ i [kg / kg]
  #     vector of length 4
  #     CVL leaves (lv), CVO storage organs (so), CVS stems (st), CVR roots (rt)
  #pci: partitioning factor of organ i [kg / kg]. vector of length 4
  #     FLTB leaves (lv), FOTB storage organs (so),
  #     FSTB stems (st), FRTB roots (rt)

  pcrt<- pci[4]
  cert<- cei[4]
  ce<- 1 / (sum(pci / cei) * (1 - pcrt) + (pcrt / cert))
  return(ce)
}


# Dry matter growth rate for total crop (5.43)

dry_grate<- function(ce, rg){

  #dw: dry matte growth rate for total crop [kg / ha*d]
  #ce: conversion efficiency factor of assimilates for total crop (5.42)
  #    [kg / kg]
  #rg: growth respiratio rate (5.41) [kg / ha*d]

  dw<-ce * rg
  return(dw)
}


# Dry matter partitioning root (5.44a)

part_rt<- function(pci, dw){

  #dwrt: dry matter growth rate for roots
  #pci: partitioning factor of organ i [kg / kg]. vector of length 4
  #     FLTB leaves (lv), FOTB storage organs (so),
  #     FSTB stems (st), FRTB roots (rt)
  #dw: dry matter growth rate for total crop (5.43)

  pcrt<-pci[4]
  dwrt<- pcrt * dw
  return(dwrt)
}


# Dry matter partitioning shoots (5.44b)

part_sh<- function(pci, dw){

  #wsh: dry matter growth rate for shoots
  #pci: partitioning factor of organ i [kg / kg]. vector of length 4
  #     FLTB leaves (lv), FOTB storage organs (so),
  #     FSTB stems (st), FRTB roots (rt)
  #dw: dry matter growth rate for total crop (5.43)

  pcrt<-pci[4]
  dwsh<- (1 - pcrt) * dw
  return(dwsh)
}


# Dry matter partitioning leaves, stems and storage organs (5.45)

part_i<- function(dwsh, pci){

  #dwi: dry matter growth rate for organ i
  #dwsh: dry matter growth rate for shoots (5.44b)
  #pci: partitioning factor of organ i
  #i: leaves (lv), storage organs (so), stems (st)

  pci<- pci[1:3] # remove pc roots as accounted in 5.44a
  dwi<- pci * dwsh
  return(dwi)
}


# Organ partitioning check (5.46)

org_part_check<- function(FL,FO,FS,FR){

  # At any development stage the following relation must be valid,
  # if not, the simulation will be stopped.

  return(isTRUE(abs(FR+(FL+FS+FO)*(1-FR)-1) <= 0.0001))
}


# Carbon balance check (5.47)

carb_bal_check<- function(FL,FO,FS,FR,pgass,rmt,dmi,cvf){

  # At any development stage the following relation must be valid,
  # if not, the simulation will be stopped.

  return(isTRUE(
    abs(pgass-rmt-(FR+(FL+FS+FO)*(1-FR))*dmi/cvf)/max(0.0001,pgass) <= 0.0001))
}


# Dry matter weight of stems (5.49a)

dry_weigth_st<- function(dwnst, pwi, tstep=1){

  #wst: dry matter weight of stems
  #pwi: past dry matter weight of organ i (time step t-1)
  #dwnst: net dry matter growth rate of stems
  #tstep: time step (one day)

  wst<- pwi[3] + dwnst * tstep
  return(wst)
}


# Dry matter weight of roots (5.49b)

dry_weigth_rt<- function(dwnrt, pwi, tstep=1){

  #wrt: dry matter weight of roots
  #pwi: past dry matter weight of organ i (time step t-1)
  #dwnrt: net dry matter growth rate of roots
  #tstep: time step (one day)

  wrt<- pwi[4] + dwnrt * tstep
  return(wrt)
}


# Dry matter weight of leaves (5.49c) <- ! FINISH !!!

dry_weigth_lv<- function(dwnlv, pwi, tstep=1){

  #wlv: dry matter weight of leaves
  #pwi: past dry matter weight of organ i (time step t-1)
  #dwnlv: net dry matter growth rate of leaves
  #tstep: time step (one day)

  wlv<- pwi[1] + dwnlv * tstep
  return(wlv)
}


# Accumulated LAI at time t during unlimited growth (5.51)

laigr_acc<- function(lexpt, plai, tstep=1){

  #lai: leaf area index at time step t
  #plai: leaf area index at time step t - 1
  #lexpt: growth rate of the leaf area index at time step t during exponential
  #       growth stage (5.50)
  #tstep: time step (one day)

  lai<- plai + lexpt * tstep
  return(lai)
}


# LAI growth rate during source-limited growth (5.52)

laisc_rate<- function(dvs, dwnlv, SLATB){

  #lsct: growth rate of the leaf area index at time step t during source-limited
  #       growth stage
  #dwnlv: net dry matter growth of leaes at time step t
  #sla: specific leaf area at time step t (SLATB) [ha / kg]

  sla<- afgen(dvs, SLATB)
  lsct<- dwnlv * sla
  return(lsct)
}


# Accumulated source-limited LAI at time t (5.53)

laisc_acc<- function(plai, lsct, tstep=1){

  #lai: leaf area index at time step t
  #plai: leaf area index at time step t - 1
  #lsct: growth rate of the leaf area index at time step t during source-limited
  #       growth stage (5.51)
  #t: time step (one day)

  lai<- plai + sum(lsct * tstep)
  return(lai)
}


# Stems and storage organs green area index (5.54)

gai<- function(ssi, wi){

  #gaii: green area index of organ i
  #ssi: specific green area of organ i
  #wi: dry matter of organ i
  #i: stems (st), storage organs (so)

  gaii<- ssi * wi[3:2]
  return(gaii)
}


# Specific leaf are at time t during exponential growth (5.55)

spla_exp<- function(lexpt, dwlv){

  #sexpt: specific leaf area at time step t during the exponential growth stage
  #lextp: growth rate of the leaf area index at time step t during
  #       exponential growth stage (5.51)
  #dwlv: dry matter growth rate of leaves (5.45)

  sexpt<- lexpt / dwlv
  return(sexpt)
}


#### LEAF SCENESCENCE ####
#~*~*~*~*~*~*~*~*~*~*~*~*#

# Physiological ageing factor fro leaves (5.56)

ag_fac<- function(temp, TBASE){

  #frai: physiologic ageing factor for leaf age increase
  #temp: average temperature
  #TBASE: lower threshold temperature for physiologic ageing

  frai<- max(0, (temp - TBASE) / (35 - TBASE))
  return(frai)
}


# Physiologic leaf age at time step t (5.57)

phy_age<- function(ppaget, frai, tstep=1){

  #paget: physiologic age at time step t
  #ppaget: physiologic age at time step t - 1
  #frai: physiologic ageing factor for leaf age increase
  #tstep: time step (one day)

  paget<- ppaget + frai*tstep
  return(paget)
}


# Potential leaves death rate due to water stress (5.58)

water_death<- function(wlv, PERDL, ta, tp){

  #dw1d: potential death rate of leaves due to water stress [kg / ha*d]
  #wlv: dry matter weight of the leaves (5.49)
  #PERDL: maximum relative death rate of leaves due to water stress (PERDL)
  #ta: actual transpiration
  #tp: potential transpiration

  if (ta==0 | tp==0){dw1d<- wlv*PERDL}
  else {dw1d<- wlv*(1 - (ta / tp))*PERDL}
  return(dw1d)
}


# Potential death rate of leaves due to high LAI (5.59)

lai_death<- function(wlv, lai, laic){

  #dw2d: potential death rate of leaves due to high LAI [kg / ha*d]
  #wlv: dry matter weight of the leaves (5.49)
  #lai: leaf area index
  #laic: critical leaf area index

  dw2d<- wlv* limit(0, 0.03, 0.03*((lai - laic) / laic))
  return(dw2d)
}


# Critical lai (5.60)

crit_lai<- function(kdf){

  #laic: critical leaf area index
  #kdf: extinction coefficient for the diffuse radiation flux

  laic<- 3.2 / kdf
  return(laic)
}


# Weight of leaves that have died during current time step (5.61)

dead_weight<- function(dw1d, dw2d, tstep=1){

  #dw2d: potential death rate of leaves due to high LAI [kg / ha*d]
  #dw1d: potential death rate of leaves due to water stress [kg / ha*d]
  #tstep: time step (one day)

  wd<- max(c(dw1d,dw2d))*tstep
  return(wd)
}


#### EVAPOTRANSPIRATION ####
#~*~*~*~*~*~*~*~*~*~*~*~*~*#

sweaf<- function(et0, DEPNR){

  # The fraction of easily available soil water between field capacity and
  # wilting point is a function of the potential evapotranspiration rate
  # (for a closed canopy) in cm/day, ET0, and the crop group number, DEPNR
  # (from 1 (=drought-sensitive) to 5 (=drought-resistent)). The function
  # SWEAF describes this relationship given in tabular form by Doorenbos &
  #   Kassam (1979) and by Van Keulen & Wolf (1986; p.108, table 20)
  # http://edepot.wur.nl/168025.

  #et0: Evapotranpiration from a reference crop
  #DEPNR: Crop dependency number

  A= 0.76
  B= 1.5

  # curve for CGNR 5, and other curves at fixed distance below it
  sweaf= 1/(A+B*et0) - (5 - DEPNR)*0.10

  # Correction for lower curves (CGNR less than 3)
  if (DEPNR < 3){
    sweaf<- sweaf + (et0 - 0.6)/(DEPNR*(DEPNR + 3))
  }

  swdep<- limit(0.10, 0.95, sweaf)

  return(swdep)
}
