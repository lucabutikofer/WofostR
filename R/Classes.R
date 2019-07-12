
# CropObject class definition #####

#' S4 Class "CropObject"
#'
#' Contains crop parameters
#'
#' @importFrom methods new
#' @export CropObject
#'
CropObject <- setClass('CropObject', slots = c(

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


# "show()" method for CropObject class ####

#' S4 Method for generic "show()", CropObject
#'
#' Prints a summary of the simulation object when typed or when
#' "show(object_name)" is called
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
            cat('>>',fs, 'crop parameters out of',
                length(slotNames(object)),
                'are specified.','\n', '\n')
          }
          )


# "CropObject()" method for list class ####

#' S4 Method for generic "CropObject()", list
#'
#' @export
#'
setMethod(f='CropObject', signature='list',
          definition= function(...){
            CropObject(

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


# WeatehrObject class definition ####

#' S4 Class "WeatherObject"
#'
#' Contains meteorological driving variables.
#'
#' @importFrom methods new
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


# "WeatherObject()" method for list class ####

#' S4 Method for generic "WeatherObject()", list
#'
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


# "show()" method for WeatherObject class ####

#' S4 Method for generic "show()", WeatherObject
#'
#' Prints a summary of the weather object when typed or when
#' "show(object_name)" is called
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
            print(out,quote=F)
            cat('\n')
          }
)
