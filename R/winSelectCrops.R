#' Select crops have viable growing window
#'
#' Takes the output of growingWindow() and returns a selection of
#' crop/varieties that reached maturity with their corresponding sowing
#' dates. This is useful for running Wofost on those with function
#' "winSelectWofost()".
#' @param gw Growing Window. The output of growingWindow() function.
#'
#' @export
#'
#' @examples
#' # Single crop growing window analysis
#' out <- growingWindow(w = randomWeatherLong,
#'                      crop = "potato",
#'                      variety = "Potato_701")
#'
#' # Select viable crops
#' gwOut <- winSelectCrops(out)
#'
#'
winSelectCrops <- function(gw){

  gwOut <- gw
  rate <- NULL
  for (i in 1:length(gw)){
    sgw <-gw[[i]]  # subset of gw, i-th crop
    rate[i] <- length(sgw$dvs[sgw$dvs >= 2])/nrow(sgw)
    sgw2 <- sgw[sgw$dvs >= 2, 1]
    gwOut[[i]] <- sgw2
  }

  # subset of gw with only crops that made it at least once
  gwOut <- gwOut[which(rate != 0)]

  return(gwOut)
}

#' Runs wofost for winSelectCrops() output
#'
#' Deprecated! This function has been superseeded by cropSuitWofost().
#' Takes the output of winSelectCrops() and runs a Wofost() run for each
#' sowing date for each selected crop.
#'
#' @param gwOut Selected crops as outputted from winSelectCrops().
#' @param w WeatherObject. Must encompass the period from the earliest
#' sowing date to 730 days after the last sowing date.
#' @param s SoilObject.
#' @param crLocal Character vector. If crops are stored locally, this is
#' the path where all crop yaml files are stored.
#' @param waterLimited Logical. If FALSE (default) runs potential production.
#' If TRUE, free draining water-limited production.
#' @param outDir Directory path (to be created) where the output is saved.
#'
#' @export
#'
winSelectWofost <- function(gwOut, w, s,
                            crLocal = NULL, waterLimited = FALSE,
                            outDir = ''){

  # Names of corps and varieties
  nm <- names(gwOut)
  nm <- strsplit(nm, '/')

  # Number of crop-time combinations
  n <- 0
  for(i in 1:length(nm)){
    n <- n + length(gwOut[[i]])
  }

  # Make directory for output
  outDir <- paste0('WofostOutput_', min(w@DAY), '_', max(w@DAY),
                   '_', outDir)
  dir.create(outDir, recursive = T)

  # Create progress bar
  pb <- txtProgressBar(min = 1, max = n, style = 3)

  # Start loop
  out <- NULL; count <- 1
  for (i in 1:length(gwOut)){ # for each crop
    cnm <- nm[[i]][1]  # crop name
    vnm <- nm[[i]][2]  # variety name

    for (j in 1:length(gwOut[[i]])){ # for each time
      st <- gwOut[[i]][j] # sowing time

      # subset of WeatherObject
      # 731 days in case there is a leap year. However finish date cannot
      # exceed latest day in WeatherObject
      sw <- subsetObj(w, c(st, min(st + 731, max(w@DAY))))

      # Make crop object
      if (is.null(crLocal)){ # if crop is to be downloaded from the web
        cro <- dwn.crop(cropName = cnm, variety = vnm)
      } else { # if crops are stored locally
        cro <- load.crop(cropName = cnm, variety = vnm, crLocal = crLocal)
      }


      # Run Wofost waterlimited production
      # !!! "try(..., silent = T)" kills all error signals !!!

      if(cnm == 'tobacco'){ # Tobacco require start at emergence
                            # Will need to fix this in Wofost()
        out[[j]] <- try(WofostFD(crop = cro, w = sw, soil = s,
                             startType = "emergence",
                             finishType = "maturity",
                             activate.verndvs = F), silent = T)
      } else {
        out[[j]] <- try(WofostFD(crop = cro, w = sw, soil = s,
                           startType = "sowing",
                           finishType = "maturity",
                           activate.verndvs = F), silent = T)
      }
      names(out)[j] <- as.character(st)

      # Update progress bar
      setTxtProgressBar(pb, count)
      # print(paste(cnm, vnm, count, i, j))
      count <- count + 1

    }

    # Save crop output
    saveRDS(out, paste0(outDir, '/',
                        nm[[i]][1], '_', nm[[i]][2],
                        '.rds'))
    out <- NULL

  }
}




#' Runs a crop suitability analysis.
#'
#' Performs a crop suitability analysis for the location and period described
#' in a weatherObject. First determins for which of the input crops and for
#' which sowing date maturity is reached. Then runs a Wofost() simulation for
#' each of the viable crop-sowing-time combinations.
#' Alternatively, the same result can be reached by running in sequence
#' functions growingWindow() > winSelectCrops() > cropSuitWofost().
#' maturity with function growingWindow()
#' Takes the output of winSelectCrops() and runs a Wofost() run for each
#' sowing date for each selected crop.
#'
#' @param gwOut Selected crops as outputted from winSelectCrops().
#' @param crop Character identifying the name of the crop to be
#' downlowaded with function dwn.crop().
#' @param variety Character.The variety name. See omonimous variable in
#' dwn.crop().
#' @param allVar Logical. If TRUE all crop-variety combinations available
#' on "https://github.com/ajwdewit/WOFOST_crop_parameters.git" or on the
#' local directory crLocal will be run.
#' @param w WeatherObject. Must encompass the period from the earliest
#' sowing date to 730 days after the last sowing date.
#' @param s SoilObject.
#' @param crLocal Character vector. If crops are stored locally, this is
#' the path where all crop yaml files are stored.
#' @param waterLimited Logical. If FALSE (default) runs potential production.
#' If TRUE, free draining water-limited production.
#' @param activate.verndvs Logical. If TRUE, allows the use of variable
#' "VERNDVS". A critical development stage (VERNDVS) is used to stop the
#' effect of vernalisation when this development stage is reached.
#' This is done to improve model stability in order to avoid that anthesis
#' is never reached due to a somewhat too high VERNSAT.
#' A warning is released when VERNDVS is reached without
#' completing vernalisation.
#' @param outDir Directory path (to be created) where the output is saved.
#' @return This function saves its output in the working directory
#' as .rds files.
#' .rds files are grouped in folders named
#' "WofostOutput_SimulationStartDate_SimulationEndDate_outDir.
#' Each .rds file contains a named list of SimulationObjects.
#' Names are the sowing date.
#' Since computation is serial and typically across a high number of
#' iterations, error messages are suppressed and a log file
#' ("WofostErrorLog.txt")
#' is printed in the working directory reporting errors instead.
#'
#' @export
#'
#' @examples
#' # Load WeatherObject and SoilObject
#' w <- randomWeatherLong
#' s <- randomSoil
#'
#' # Run single crop variety
#' cropSuitWofost(crop = "potato", variety = "Potato_701",
#'                w = w, s = s,
#'                waterLimited = TRUE)
#'
#' # Run independent functions in series
#' out <- growingWindow(w = w, crop = "potato", variety = "Potato_701")
#' gwOut <- winSelectCrops(out)
#' cropSuitWofost(gwOut = gwOut,
#'                w = w, s = s,
#'                waterLimited = TRUE)
#'
#' # Load and plot output
#' wo <- readRDS('WofostOutput_2010-04-16_2013-11-06/potato_Potato_701.rds')
#' plot(wo[[1]])
#' unlink('WofostOutput_2010-04-16_2013-11-06/potato_Potato_701.rds')
#'
#'
#' # Run all crop varieties
#' # Not run, takes several minutes.
#' # cropSuitWofost(allVar = T,
#' #                w = w, s = s,
#' #                waterLimited = TRUE)
#'
#'
cropSuitWofost <- function(gwOut = NULL,
                           crop = NULL,
                           variety = NULL,
                           allVar = FALSE,
                           w, s,
                           crLocal = NULL,
                           waterLimited = FALSE,
                           activate.verndvs = FALSE,
                           outDir = NULL
                           ){

  # Run growing window if necessary
  if(is.null(gwOut)){ # if gwOut is to be computed

    # Run growing window analysis
    cat('\n  Running growing window analysis\n')
    out <- growingWindow(crop, variety,
                         w, allVar, crLocal,
                         activate.verndvs)
    # Run crop selection
    gwOut <- winSelectCrops(out)
  }

  cat('\n  Running Wofost simulations\n')
  # Names of corps and varieties
  nm <- names(gwOut)
  nm <- strsplit(nm, '/')

  # Number of crop-time combinations
  n <- 0
  for(i in 1:length(nm)){
    n <- n + length(gwOut[[i]])
  }

  # Make directory for output
  if(is.null(outDir)){ # if outDir is not specified
    outDir <- paste0('WofostOutput_', min(w@DAY), '_', max(w@DAY))
  } else {
    outDir <- paste0('WofostOutput_', min(w@DAY), '_', max(w@DAY),
                     '_', outDir)
  }
  dir.create(outDir, recursive = T)

  # Create progress bar
  pb <- txtProgressBar(min = 1, max = n, style = 3)

  # Start loop
  out <- list(); count <- 1
  for (i in 1:length(gwOut)){ # for each crop
    cnm <- nm[[i]][1]  # crop name
    vnm <- nm[[i]][2]  # variety name

    # Make crop object
    if (is.null(crLocal)){ # if crop is to be downloaded from the web
      cro <- dwn.crop(cropName = cnm, variety = vnm)
    } else { # if crops are stored locally
      cro <- load.crop(cropName = cnm, variety = vnm, crLocal = crLocal)
    }

    for (j in 1:length(gwOut[[i]])){ # for each sowing time
      st <- gwOut[[i]][j] # sowing time

      # subset of WeatherObject
      # 731 days in case there is a leap year. However finish date cannot
      # exceed latest day in WeatherObject
      sw <- subsetObj(w, c(st, min(st + 731, max(w@DAY))))

      # Tobacco requires start at emergence
      if(cnm == 'tobacco'){
        startType = 'emergence'
      } else {
        startType = 'sowing'
      }

      # Run Wofost
      out[[j]] <- try(Wofost(crop = cro, w = sw, soil = s,
                         startType = startType,
                         finishType = "maturity",
                         activate.verndvs = activate.verndvs,
                         waterLimited = waterLimited),
                      outFile = 'WofostErrorLog.txt')
      names(out)[j] <- as.character(st)

      # Update progress bar
      setTxtProgressBar(pb, count)
      # print(paste(cnm, vnm, count, i, j))
      count <- count + 1

    }

    # Save crop output
    saveRDS(out, paste0(outDir, '/',
                        nm[[i]][1], '_', nm[[i]][2],
                        '.rds'))
    out <- list()

  }
  cat('\n')
}
