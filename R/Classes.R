

# >> INTERNAL, UNDOCUMENTED METHODS AND CLASSES ####

# TestCropObject class definition #####
# S4 Class "TestCropObject"
#
# Contains crop parameters
#
# @export TestCropObject
#
TestCropObject <- setClass('TestCropObject', slots = c(

  'AMAXTB',
  'CFET',
  'CRAIRC',
  'CROP_START_TYPE',
  'CVL',
  'CVO',
  'CVR',
  'CVS',
  'DEPNR',
  'DLC',
  'DLO',
  'DTSMTB',
  'DVSEND',
  'DVSI',
  'EFFTB',
  'FLTB',
  'FOTB',
  'FRTB',
  'FSTB',
  'IAIRDU',
  'IDSL',
  'IOX',
  'KDIFTB',
  'LAIEM',
  'PERDL',
  'Q10',
  'RDI',
  'RDMCR',
  'RDMSOL',
  'RDRRTB',
  'RDRSTB',
  'RFSETB',
  'RGRLAI',
  'RML',
  'RMO',
  'RMR',
  'RMS',
  'RRI',
  'SLATB',
  'SMFCF',
  'SMW',
  'SPA',
  'SPAN',
  'SSATB',
  'TBASE',
  'TBASEM',
  'TDWI',
  'TEFFMX',
  'TMNFTB',
  'TMPFTB',
  'TSUM1',
  'TSUM2',
  'TSUMEM',
  'VERNRTB',
  'SM0',
  'VERNDVS',
  'VERNBASE',
  'VERNSAT'

)
)


# TestCropObject() method for "list" class ####
# S4 Method for generic "TestCropObject()", list
#
# @export
#
setMethod(f='TestCropObject', signature='list',
          definition= function(...){
            TestCropObject(

              AMAXTB = ...$AMAXTB,
              CFET = ...$CFET,
              CRAIRC = ...$CRAIRC,
              CROP_START_TYPE = ...$CROP_START_TYPE,
              CVL = ...$CVL,
              CVO = ...$CVO,
              CVR = ...$CVR,
              CVS = ...$CVS,
              DEPNR = ...$DEPNR,
              DLC = ...$DLC,
              DLO = ...$DLO,
              DTSMTB = ...$DTSMTB,
              DVSEND = ...$DVSEND,
              DVSI = ...$DVSI,
              EFFTB = ...$EFFTB,
              FLTB = ...$FLTB,
              FOTB = ...$FOTB,
              FRTB = ...$FRTB,
              FSTB = ...$FSTB,
              IAIRDU = ...$IAIRDU,
              IDSL = ...$IDSL,
              IOX = ...$IOX,
              KDIFTB = ...$KDIFTB,
              LAIEM = ...$LAIEM,
              PERDL = ...$PERDL,
              Q10 = ...$Q10,
              RDI = ...$RDI,
              RDMCR = ...$RDMCR,
              RDMSOL = ...$RDMSOL,
              RDRRTB = ...$RDRRTB,
              RDRSTB = ...$RDRSTB,
              RFSETB = ...$RFSETB,
              RGRLAI = ...$RGRLAI,
              RML = ...$RML,
              RMO = ...$RMO,
              RMR = ...$RMR,
              RMS = ...$RMS,
              RRI = ...$RRI,
              SLATB = ...$SLATB,
              SMFCF = ...$SMFCF,
              SMW = ...$SMW,
              SPA = ...$SPA,
              SPAN = ...$SPAN,
              SSATB = ...$SSATB,
              TBASE = ...$TBASE,
              TBASEM = ...$TBASEM,
              TDWI = ...$TDWI,
              TEFFMX = ...$TEFFMX,
              TMNFTB = ...$TMNFTB,
              TMPFTB = ...$TMPFTB,
              TSUM1 = ...$TSUM1,
              TSUM2 = ...$TSUM2,
              TSUMEM = ...$TSUMEM,
              VERNRTB = ...$VERNRTB,
              SM0 = ...$SM0,
              VERNDVS = ...$VERNDVS,
              VERNBASE = ...$VERNBASE,
              VERNSAT = ...$VERNSAT

            )
          }
)


# >> EXPORTED, DOCUMENTED METHODS AND CLASSES ####

# > CROP OBJECT ####

# CropObject class definition #####
#' S4 Class "CropObject"
#'
#' Contains crop parameters
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the CropObject to be created. Alternatively slots can be filled
#' individually (e.g. with "CropObject(CVR = 2, SPA = 7)").
#' @importFrom methods new slot slotNames
#' @export CropObject
#'
CropObject <- setClass('CropObject', slots = c(

  'CROPNAME',
  'VARNAME',
  'CO2EFFTB',
  'CO2TRATB',
  'CO2AMAXTB',
  'TBASEM',
  'TEFFMX',
  'TSUMEM',
  'IDSL',
  'DLO',
  'DLC',
  'TSUM1',
  'TSUM2',
  'DTSMTB',
  'DVSI',
  'DVSEND',
  'VERNBASE',
  'VERNSAT',
  'VERNDVS',
  'VERNRTB',
  'TDWI',
  'RGRLAI',
  'SLATB',
  'SPA',
  'SSATB',
  'SPAN',
  'TBASE',
  'KDIFTB',
  'EFFTB',
  'AMAXTB',
  'TMPFTB',
  'TMNFTB',
  'CVL',
  'CVO',
  'CVR',
  'CVS',
  'Q10',
  'RML',
  'RMO',
  'RMR',
  'RMS',
  'RFSETB',
  'FRTB',
  'FLTB',
  'FSTB',
  'FOTB',
  'PERDL',
  'RDRRTB',
  'RDRSTB',
  'CFET',
  'DEPNR',
  'IAIRDU',
  'IOX',
  'RDI',
  'RRI',
  'RDMCR',
  'NMAXLV_TB',
  'NMAXRT_FR',
  'NMAXST_FR',
  'NMAXSO',
  'NCRIT_FR',
  'NRESIDLV',
  'NRESIDST',
  'NRESIDRT',
  'TCNT',
  'NFIX_FR',
  'PMAXLV_TB',
  'PMAXRT_FR',
  'PMAXST_FR',
  'PMAXSO',
  'PCRIT_FR',
  'PRESIDLV',
  'PRESIDST',
  'PRESIDRT',
  'TCPT',
  'KMAXLV_TB',
  'KMAXRT_FR',
  'KMAXST_FR',
  'KMAXSO',
  'KCRIT_FR',
  'KRESIDLV',
  'KRESIDST',
  'KRESIDRT',
  'TCKT',
  'DVS_NPK_STOP',
  'DVS_NPK_TRANSL',
  'NLAI_NPK',
  'NSLA_NPK',
  'NPART',
  'NLUE_NPK',
  'NPK_TRANSLRT_FR',
  'RDRLV_NPK'

  )
)


# show() method for CropObject class ####
#' S4 Method for generic "show()", CropObject
#'
#' Prints a summary of the simulation object when typed or when
#' "show(object_name)" is called
#' @param object CropObject S4 object
#' @export
#'
setMethod('show', 'CropObject',
          function(object){
            es<-NULL # empty slots
            for(i in 1:length(slotNames(object))){
              es[i]<- !is.null(slot(object,slotNames(object)[i]))
            }
            fs<-sum(es) # full slots
            cat('\n', 'WofostR Crop Object:', '\n')
            cat(' >>', 'Crop name:', object@CROPNAME, '\n')
            cat(' >>','Variety name:', object@VARNAME, '\n')
            cat(' >>',fs, 'crop parameters out of',
                length(slotNames(object)),
                'are specified.','\n', '\n')
          }
          )


# CropObject() method for "list" class ####
#' S4 Method for generic "CropObject()", list
#'
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the CropObject to be created.
#' @export
#'
setMethod(f='CropObject', signature='list',
          definition= function(...){
            CropObject(

              CROPNAME = ...$CROPNAME,
              VARNAME = ...$VARNAME,
              CO2EFFTB = ...$CO2EFFTB,
              CO2TRATB = ...$CO2TRATB,
              CO2AMAXTB = ...$CO2AMAXTB,
              TBASEM = ...$TBASEM,
              TEFFMX = ...$TEFFMX,
              TSUMEM = ...$TSUMEM,
              IDSL = ...$IDSL,
              DLO = ...$DLO,
              DLC = ...$DLC,
              TSUM1 = ...$TSUM1,
              TSUM2 = ...$TSUM2,
              DTSMTB = ...$DTSMTB,
              DVSI = ...$DVSI,
              DVSEND = ...$DVSEND,
              VERNBASE = ...$VERNBASE,
              VERNSAT = ...$VERNSAT,
              VERNDVS = ...$VERNDVS,
              VERNRTB = ...$VERNRTB,
              TDWI = ...$TDWI,
              RGRLAI = ...$RGRLAI,
              SLATB = ...$SLATB,
              SPA = ...$SPA,
              SSATB = ...$SSATB,
              SPAN = ...$SPAN,
              TBASE = ...$TBASE,
              KDIFTB = ...$KDIFTB,
              EFFTB = ...$EFFTB,
              AMAXTB = ...$AMAXTB,
              TMPFTB = ...$TMPFTB,
              TMNFTB = ...$TMNFTB,
              CVL = ...$CVL,
              CVO = ...$CVO,
              CVR = ...$CVR,
              CVS = ...$CVS,
              Q10 = ...$Q10,
              RML = ...$RML,
              RMO = ...$RMO,
              RMR = ...$RMR,
              RMS = ...$RMS,
              RFSETB = ...$RFSETB,
              FRTB = ...$FRTB,
              FLTB = ...$FLTB,
              FSTB = ...$FSTB,
              FOTB = ...$FOTB,
              PERDL = ...$PERDL,
              RDRRTB = ...$RDRRTB,
              RDRSTB = ...$RDRSTB,
              CFET = ...$CFET,
              DEPNR = ...$DEPNR,
              IAIRDU = ...$IAIRDU,
              IOX = ...$IOX,
              RDI = ...$RDI,
              RRI = ...$RRI,
              RDMCR = ...$RDMCR,
              NMAXLV_TB = ...$NMAXLV_TB,
              NMAXRT_FR = ...$NMAXRT_FR,
              NMAXST_FR = ...$NMAXST_FR,
              NMAXSO = ...$NMAXSO,
              NCRIT_FR = ...$NCRIT_FR,
              NRESIDLV = ...$NRESIDLV,
              NRESIDST = ...$NRESIDST,
              NRESIDRT = ...$NRESIDRT,
              TCNT = ...$TCNT,
              NFIX_FR = ...$NFIX_FR,
              PMAXLV_TB = ...$PMAXLV_TB,
              PMAXRT_FR = ...$PMAXRT_FR,
              PMAXST_FR = ...$PMAXST_FR,
              PMAXSO = ...$PMAXSO,
              PCRIT_FR = ...$PCRIT_FR,
              PRESIDLV = ...$PRESIDLV,
              PRESIDST = ...$PRESIDST,
              PRESIDRT = ...$PRESIDRT,
              TCPT = ...$TCPT,
              KMAXLV_TB = ...$KMAXLV_TB,
              KMAXRT_FR = ...$KMAXRT_FR,
              KMAXST_FR = ...$KMAXST_FR,
              KMAXSO = ...$KMAXSO,
              KCRIT_FR = ...$KCRIT_FR,
              KRESIDLV = ...$KRESIDLV,
              KRESIDST = ...$KRESIDST,
              KRESIDRT = ...$KRESIDRT,
              TCKT = ...$TCKT,
              DVS_NPK_STOP = ...$DVS_NPK_STOP,
              DVS_NPK_TRANSL = ...$DVS_NPK_TRANSL,
              NLAI_NPK = ...$NLAI_NPK,
              NSLA_NPK = ...$NSLA_NPK,
              NPART = ...$NPART,
              NLUE_NPK = ...$NLUE_NPK,
              NPK_TRANSLRT_FR = ...$NPK_TRANSLRT_FR,
              RDRLV_NPK = ...$RDRLV_NPK

            )
          }
)

# > WEATHER OBJECT ####

# WeatehrObject class definition ####
#' S4 Class "WeatherObject"
#'
#' Contains meteorological driving variables.
#'
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the WeatherObject to be created. Alternatively slots can be filled
#' individually (e.g. with "WeaterObject(CVR = 2, SPA = 7)").
#' @importFrom methods new slot slotNames
#' @export WeatherObject
#'
WeatherObject <- setClass('WeatherObject', slots = c(

  'DAY',
  'E0',
  'ELEV',
  'ES0',
  'ET0',
  'IRRAD',
  'LAT',
  'LON',
  'RAIN',
  'SNOWDEPTH',
  'TEMP',
  'TMAX',
  'TMIN',
  'VAP',
  'WIND'
  )
)


# show() method for WeatherObject class ####
#' S4 Method for generic "show()", WeatherObject
#'
#' Prints a summary of the weather object when typed or when
#' "show(object_name)" is called
#' @param object CropObject S4 object
#' @export
#'
setMethod('show', 'WeatherObject',
          function(object){
            sl<-NULL # slot length
            for(i in 1:length(slotNames(object))){
              sl[i]<- length(slot(object,slotNames(object)[i]))
            }

            out<- cbind('VARIABLES'=paste0('@',slotNames(object)),
                        'LENGTH'=sl)
            rownames(out)<- 1:length(slotNames(object))

            cat('\n', 'WofostR Weather Object:', '\n', '\n')
            cat(' >>', 'From date:',
                as.character(min(object@DAY)), '\n')
            cat(' >>', 'To date:',
                as.character(max(object@DAY)), '\n', '\n')
            print(out,quote=F)
            cat('\n')
          }
)


# WeatherObject() method for list class ####
#' S4 Method for generic "WeatherObject()", list
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the WatherObject to be created.

#' @export
#'
setMethod(f='WeatherObject', signature='list',
          definition= function(...){
            WeatherObject(

              DAY = ...$DAY,
              E0 = ...$E0,
              ELEV = ...$ELEV,
              ES0 = ...$ES0,
              ET0 = ...$ET0,
              IRRAD = ...$IRRAD,
              LAT = ...$LAT,
              LON = ...$LON,
              RAIN = ...$RAIN,
              SNOWDEPTH = ...$SNOWDEPTH,
              TEMP = ...$TEMP,
              TMAX = ...$TMAX,
              TMIN = ...$TMIN,
              VAP = ...$VAP,
              WIND = ...$WIND

            )
          }
)


# > SOIL OBJECT ####

# SoilObject class definition #####
#' S4 Class "SoilObject"
#'
#' Contains soil parameters
#'
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the SoilObject to be created. Alternatively slots can be filled
#' individually (e.g. with "SoilObject(CVR = 2, SPA = 7)").
#' @importFrom methods new slot slotNames
#' @export SoilObject
#'
SoilObject <- setClass('SoilObject', slots = c(
  # ~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~  ~~~~~~~~~~
  # Name     Description                                     Type     Unit
  # ~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~  ~~~~~~~~~~
  # SMFCF     Field capacity of the soil                       SSo     -
  # SM0       Porosity of the soil                             SSo     -
  # SMW       Wilting point of the soil                        SSo     -
  # CRAIRC    Soil critical air content (waterlogging)         SSo     -
  # SOPE      maximum percolation rate root zone               SSo    |cmday-1|
  # KSUB      maximum percolation rate subsoil                 SSo    |cmday-1|
  # K0        hydraulic conductivity of saturated soil         SSo    |cmday-1|
  # RDMSOL    Soil rootable depth                              SSo     cm
  # IFUNRN    Indicates whether non-infiltrating fraction of   SSi    -
  #         rain is a function of storm size (1)
  #         or not (0)
  # SSMAX     Maximum surface storage                          SSi     cm
  # SSI       Initial surface storage                          SSi     cm
  # WAV       Initial amount of water in total soil            SSi     cm
  #         profile
  # NOTINF    Maximum fraction of rain not-infiltrating into   SSi     -
  #         the soil
  # SMLIM     Initial maximum moisture content in initial      SSi     -
  #         rooting depth zone.
  # ~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~  ~~~~~~~~~~

  'SMFCF',
  'SM0',
  'SMW',
  'CRAIRC',
  'SOPE',
  'KSUB',
  'K0',
  'RDMSOL',
  'IFUNRN',
  'SSMAX',
  'SSI',
  'WAV',
  'NOTINF',
  'SMLIM'

)
)


# show() method for SoilObject class ####
#' S4 Method for generic "show()", SoilObject
#'
#' Prints a summary of the simulation object when typed or when
#' "show(object_name)" is called
#' @param object CropObject S4 object
#' @export
#'
setMethod('show', 'SoilObject',
          function(object){
            es<-NULL # empty slots
            for(i in 1:length(slotNames(object))){
              es[i]<- !is.null(slot(object,slotNames(object)[i]))
            }
            fs<-sum(es) # full slots
            cat('\n', 'WofostR Soil Object:', '\n')
            cat(' >>',fs, 'soil parameters out of',
                length(slotNames(object)),
                'are specified.','\n', '\n')
          }
)


# SoilObject() method for "list" class ####
#' S4 Method for generic "SoilObject()", list
#' @param ... Named list where each named element corresponds to an omonimous
#' slot in the SoilObject to be created.
#' @export
#'
setMethod(f='SoilObject', signature='list',
          definition= function(...){
            SoilObject(

              SMFCF = ...$SMFCF,
              SM0 = ...$SM0,
              SMW = ...$SMW,
              CRAIRC = ...$CRAIRC,
              SOPE = ...$SOPE,
              KSUB = ...$KSUB,
              K0 = ...$K0,
              RDMSOL = ...$RDMSOL,
              IFUNRN = ...$IFUNRN,
              SSMAX = ...$SSMAX,
              SSI = ...$SSI,
              WAV = ...$WAV,
              NOTINF = ...$NOTINF,
              SMLIM = ...$SMLIM

            )
          }
)


# > MANAGER OBJECT ####

# ManagerObject class definition #####
#' S4 Class "ManagerObject"
#'
#' Contains managements instructions
#'
#' @param  cropVarSequence: Character vector containing the names of the
#' crop varieties in the desired succession. Must be in the format
#' "cropName-varName". Type "cropVarList" for a list of crop and variety names.
#' @param cropStartTypes: Character vector of same length as cropSequence
#' containing the crop start type (i.e. "sowing" or "emergence")
#' @param cromStartDate: Character vecotr of same length as croRotation
#' containing start Dates for each crop. The first element is ignored and
#' the first crop will start at "sequenceStart".
#' @param cropFinish: Character vector of same length as cropRotation containing
#' either a crop stage (e.g. "maturity") or a date in the format "YYYY/MM/DD".
#' Where specified, date will take priority over corp stage and can be used to
#' regulate long rotation series.
#' @param spacing: Numeric vector of same lenaght as cropSequence.
#' number of days after previous step when new crop is started.
#' The first crop will be started spacing[1] days after sequenceStart.
#' In most cases spacing[1] = 0.
#' @importFrom methods new slot slotNames
#'
#' @export ManagerObject
#'
#' @example
#' ManagerObject(
#'   cropSequence = c(
#'     'barley-Spring_barley_301',
#'     'millet-Millet_VanHeemst_1988',
#'     'maize-Grain_maize_201'),
#'   sequenceStart = '2010/04/16',
#'   sequenceFinish = '2012/01/01',
#'   cropStartType = c('sowing', 'sowing', 'sowing'),
#'   cropStartDate = c(NA, '2011/04/16', NA),
#'   cropFinish = c('maturity','2011/05/16','maturity'),
#'   spacing = c(0, NA, 10 )
#' )

#'
ManagerObject <- setClass('ManagerObject', slots = c(

  'cropSequence',
  'sequenceStart',
  'sequenceFinish',
  'cropStartType',
  'cropStartDate',
  'cropFinish',
  'spacing'

))


# show() method for ManagerObject class ####
#' S4 Method for generic "show()", ManagerObject
#'
#' Prints a summary of the manager object when typed or when
#' "show(object_name)" is called
#' @param object ManagerObject S4 object
#' @export
#'
setMethod('show', 'ManagerObject',
          function(object){
            cat('\n', 'WofostR Manager Object:', '\n')
            cat(' >>', 'Starting sequence:', object@sequenceStart, '\n')
            cat(' >>','Finishing sequence:', object@sequenceFinish, '\n')
            cat(' >>','Crop rotation:','\n','\n')
            print(cbind('Crops' = object@cropSequence,
                        'Start_types' = object@cropStartType,
                        'Start_dates' = object@cropStartDate,
                        'Finish' = object@cropFinish,
                        'Spacing' = object@spacing),
                  quote = FALSE, row_numbers = FALSE)
          }
)


# > SIMULATION OBJECT ####

# SimulationObject class definition #####
#' S4 Class "SimulationObject"
#'
#' Contains the output of function Wofost()
#' @param description: Dataframe containing metadata of the simulation
#' @param variables: List of length equal to nrow("description") containing
#' the output variables. Each top level element of "variables" represents a
#' crop.
#'
#' @importFrom methods new slot slotNames
#'
#' @export SimulationObject
#'
#' @example
#'
SimulationObject <- setClass('SimulationObject', slots = c(

  'description',
  'variables'

))


# show() method for SimulationObject class ####
#' S4 Method for generic "show()", SimulationObject
#'
#' Prints a summary of the manager object when typed or when
#' "show(object_name)" is called
#' @param object SimulationObject S4 object
#' @export
#'
setMethod('show', 'SimulationObject',
          function(object){
            cat('\n', 'WofostR Simulation Object:', '\n', '\n')
            print(object@description,
                  quote = FALSE)
            cat('\n', '\n')
          }
)

# plot() method for SimulationObject class ####
#' S4 Method for generic "plot()", SimulationObject
#'
#' Plots an overview of the output of function Wofost
#' @param x SimulationObject S4 object
#' @param var Character vector of variable names that will be plotted.
#' @export
#'
setMethod('plot', 'SimulationObject',
          function(x, var = 'twso'){

    if (nrow(x@description) > 1){ # if SimulationObject contains multiple
                                  # simulations

      # set new par() values.
      l <- nrow(x@description) * length(var)
      if (l < 4){
        op <- par(mfrow = c(1, l))
        on.exit(par(op,no.readonly = T))
      } else if (round(sqrt(l)) == sqrt(l)){ # if l is a square number
        op <- par(mfrow = c(sqrt(l), sqrt(l)))
        on.exit(par(op, no.readonly = T))
      } else {
        op <- par(mfrow = c(round(sqrt(l)), round(sqrt(l)) + 1))
        on.exit(par(op,no.readonly = T))
      }

      for (i in 1:nrow(x@description)){ # for each crop
        time <- seq(x@description$startDate[i],
                    x@description$finishDate[i],
                    1)
        for (v in 1:length(var)){ # for each variable in "var"
          ind <- which(names(x@variables[[i]]) == var[v])
          plot(time, x@variables[[i]][ind][[1]],
               type='l', col = 4, lwd = 2,
               xlab = 'Date',
               ylab = var[v],
               main = names(x@variables)[i])
        }
      }
    } else { # if SimulationObject contains a single crop

      # set new par() values.
      l <- length(x@variables[[1]])
      if (l < 4){
        op <- par(mfrow = c(1, l))
        on.exit(par(op,no.readonly = T))
      } else if (round(sqrt(l)) == sqrt(l)){ # if l is a square number
        op <- par(mfrow = c(sqrt(l), sqrt(l)))
        on.exit(par(op, no.readonly = T))
      } else {
        op <- par(mfrow = c(round(sqrt(l)), round(sqrt(l)) + 1))
        on.exit(par(op,no.readonly = T))
      }

      time <- seq(x@description$startDate[1],
                  x@description$finishDate[1],
                  1)
      for (i in 1:l){
        plot(time, x@variables[[1]][[i]],
             type='l', col = 4, lwd = 2,
             xlab = 'Date',
             ylab = names(x@variables[[1]])[i]
             )
      }
    }
  }
)

