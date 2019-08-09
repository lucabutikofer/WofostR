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
#' @export
#'
growingWindow <- function(crop = "potato", variety = "Potato_701",
                          w, allVar = FALSE){

  if (isTRUE(allVar)){out <- growingWindowAll(w)}

  if (isFALSE(allVar)){out <- growingWindowOne(crop, variety, w)}

  return(out)

}


growingWindowAll <- function(w){

  outAll <- NULL
  for (i in 1:nrow(cropVarList)){

    crop <- as.character(cropVarList$crop[i])
    variety <- as.character(cropVarList$variety[i])

    cat(paste0(round(i/nrow(cropVarList)*100), '% ', crop, ' ', variety, '\n'))

    outAll[[i]] <- growingWindowOne(crop, variety, w)

  }
  names(outAll) <- paste(as.character(cropVarList[,1]),
                         as.character(cropVarList[,2]),
                         sep = '/')

  return(outAll)

}


growingWindowOne <- function(crop, variety, w){

  # Download crop from the web
  cr <- dwn.crop(cropName = crop, variety = variety)

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
                       finishType= 'maturity'
                       )
  }

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
                      days = days)
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
