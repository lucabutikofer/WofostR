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
      if(cnm == 'tobacco'){ # Tobacco require start at emergence
                            # Will need to fix this in Wofost()
        out[[j]] <- WofostFD(crop = cro, w = sw, soil = s,
                             startType = "emergence",
                             finishType = "maturity",
                             activate.verndvs = F)
      } else {
        out[[j]] <- WofostFD(crop = cro, w = sw, soil = s,
                           startType = "sowing",
                           finishType = "maturity",
                           activate.verndvs = F)
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
