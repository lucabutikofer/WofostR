

#' S4 Class "CropObject"
#'
#' Contains crop parameters, rates and states for the current time step in
#' the simulation.
#'
#' @importFrom methods new
#' @export CropObject
#'
CropObject <- setClass('CropObject', slots = c(

  'AMAXTB',
  'CFET',
  'CO2',
  'CRAIRC',
  'CROP_END_TYPE',
  'CROP_START_TYPE',
  'CRPNAM',
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
  'IFUNRN',
  'IOX',
  'K0',
  'KDIFTB',
  'KSUB',
  'LAIEM',
  'NOTINF',
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
  'SM0',
  'SMFCF',
  'SMLIM',
  'SMW',
  'SOPE',
  'SPA',
  'SPAN',
  'SSATB',
  'SSI',
  'SSMAX',
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
  'WAV',
  'VERNDVS',
  'VERNBASE',
  'VERNSAT'


  )
)


#' S4 Method for generic "show()"
#'
#' Prints a summary of the simulation object when typed or when
#' "show(object_name)" is called
#' @export
#'
setMethod('show', 'CropObject',
          function(object){
            cat('WofostR Crop Object:', '\n')
            cat('>>',length(object), 'crop parameters specified','\n')
          }
          )


#' S4 Method for generic "CropObject()"
#'
#' @export
#'
setMethod(f='CropObject', signature='list',
          definition= function(...){
            CropObject(
              AMAXTB = ...$AMAXTB,
              CFET = ...$CFET,
              CO2 = ...$CO2,
              CRAIRC = ...$CRAIRC,
              CROP_END_TYPE = ...$CROP_END_TYPE,
              CROP_START_TYPE = ...$CROP_START_TYPE,
              CRPNAM = ...$CRPNAM,
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
              IFUNRN = ...$IFUNRN,
              IOX = ...$IOX,
              K0 = ...$K0,
              KDIFTB = ...$KDIFTB,
              KSUB = ...$KSUB,
              LAIEM = ...$LAIEM,
              NOTINF = ...$NOTINF,
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
              SM0 = ...$SM0,
              SMFCF = ...$SMFCF,
              SMLIM = ...$SMLIM,
              SMW = ...$SMW,
              SOPE = ...$SOPE,
              SPA = ...$SPA,
              SPAN = ...$SPAN,
              SSATB = ...$SSATB,
              SSI = ...$SSI,
              SSMAX = ...$SSMAX,
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
              WAV = ...$WAV,
              VERNDVS = ...$VERNDVS,
              VERNBASE = ...$VERNBASE,
              VERNSAT = ...$VERNSAT

            )
          }
)
