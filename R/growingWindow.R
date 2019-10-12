#' Find growing window
#'
#' Determins if a crop can reach maturity when grown at a specific location
#'
#' @param crop Character identifying the name of the crop to be
#' downlowaded with function dwn.crop().
#' @param variety Character.The variety name. See omonimous variable in
#' dwn.crop().
#' @param w WeatherObject encompassing the testing period. Should end with
#' one extra year to allow for testing the sowing of december of second-to-last
#' year.
#' @param allVar Logical. If TRUE all crop-variety combinations available
#' on "https://github.com/ajwdewit/WOFOST_crop_parameters.git" will be run.
#' @param crLocal Character vector. If crops are stored locally, this is
#' the path where all crop yaml files are stored.
#' @export
#'
growingWindow <- function(crop = "potato", variety = "Potato_701",
                          w, allVar = FALSE, crLocal = NULL,
                          activate.verndvs= FALSE){

  # Add "/" at the end of "crLocal" if specified and not ending in "/" already
  if (!is.null(crLocal)){
    if (substr(crLocal, nchar(crLocal), nchar(crLocal)) != '/'){
      crLocal <- paste0(crLocal,'/')
    }
  }

  # Run "growingWindowAll" if "allVar==TRUE"
  if (isTRUE(allVar)){out <- growingWindowAll(w, crLocal, activate.verndvs)}

  # Run "growingWindowONE" if "allVar==FALSE"
  if (isFALSE(allVar)){out <- growingWindowOne(crop, variety, w, crLocal,
                                               activate.verndvs)}

  return(out)

}


growingWindowAll <- function(w, crLocal, activate.verndvs){

  # Create progress bar
  pb <- txtProgressBar(min = 1, max = nrow(cropVarList), style = 3)

  outAll <- NULL
  for (i in 1:nrow(cropVarList)){

    crop <- as.character(cropVarList$crop[i])
    variety <- as.character(cropVarList$variety[i])

    outAll[[i]] <- growingWindowOne(crop, variety, w, crLocal, activate.verndvs)

    # Update progress bar
    setTxtProgressBar(pb, i)

  }
  names(outAll) <- paste(as.character(cropVarList[,1]),
                         as.character(cropVarList[,2]),
                         sep = '/')

  return(outAll)

}


growingWindowOne <- function(crop, variety, w, crLocal, activate.verndvs){


  #=================#
  # MAKE CropObject #
  #=================#

  # Retrieve crop data either online or locally
  if (is.null(crLocal)){   # Download crop from the web
    cr <- dwn.crop(cropName = crop, variety = variety)
  } else {               # Read crLocal file
    y<- yaml::read_yaml(paste0(crLocal, crop, '.yaml'))

    # Select variety
    yv<- y[["CropParameters"]][["Varieties"]][[variety]]

    # Reorganise list structure
    yv0<- NULL
    for(i in 1:length(yv)){
      yv0[[i]]<- yv[[i]][[1]]
    }
    names(yv0)<- names(yv)

    # Make matrixes out of afgen vectors
    for (i in 1:length(yv0)){
      if(length((yv0[[i]])) > 1){
        yv0[[i]]<- make.afgen(yv0[[i]])
      }
    }

    # Make CropObject
    cr<- CropObject(yv0)
    cr@CROPNAME<- crop
    cr@VARNAME<- variety
  }


  #========================================#
  # SETUP TIME INTERVALS AND RUN PHENOLOGY #
  #========================================#

  # Generate vector of first of the month dates for the time span in w
  firsts <- seq.Date(from = w@DAY[1], to = w@DAY[length(w@DAY)], by = 'month')

  # Run the phenology component starting each month
  phen <- NULL
  for (i in 1:(length(firsts) - 12)){

    # Subset weather object
    wsub <- subsetObj(w, c(firsts[i], w@DAY[length(w@DAY)]))

    # Run phenology module
    phen[[i]] <- Phenology (crop = cr, w = wsub,
                       startType= 'sowing',
                       finishType= 'maturity',
                       activate.verndvs= activate.verndvs
                       )
  }


  #=============================#
  # REORGANISE OUTPUT STRUCTURE #
  #=============================#

  # Extract the maximum development stage reached for each starting month,
  # no. of days to reach it, and vernalisation factors
  # (only for winter wheat)
  dvs <- NULL; days<- NULL; vern <- NULL; vernfacNF <- NULL
  for (i in 1:length(phen)){
    dvs[i] <- max(phen[[i]]$dvs)
    days[i]<- length(phen[[i]]$dvs)
    vern[i] <- max(phen[[i]]$vern)
    vernfacNF[i] <- max(phen[[i]]$vernfacNF)
  }

  if(crop != 'wheat'){
    out <- data.frame(startDate = firsts[1:(length(firsts) - 12)],
                      dvs = dvs,
                      days = days,
                      vern = rep("NA", length(dvs)),
                      vernfacNF = rep("NA", length(dvs))
                      )
  }

  if(crop == 'wheat'){
    out <- data.frame(startDate = firsts[1:(length(firsts) - 12)],
                      dvs = round(dvs, digits = 2),
                      days = days,
                      vern = round(vern, digits = 2),
                      vernfacNF = round(vernfacNF, digits = 2))
  }

  return(out)

}
