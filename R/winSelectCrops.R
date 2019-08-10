#' Select crops have viable growing window
#'
#' Takes the output of growingWindow() and returns a selection of
#' crop/varieties that reached maturity with their corresponding sowing
#' dates. This is useful for running WofostXX() on those.
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
#' @param waterlimited Logical. If FALSE (default) runs potential production.
#' If TRUE, free draining water-limited production.
#' @param w WeatherObject. Must encompass the period from the earliest
#' sowing date to 730 days after the last sowing date.
#'
#' @export
#'
winSelectWofost <- function(gwOut, w, waterlimited = FALSE){

  nm <- names(gwOut)
  nm <- strsplit(nm, '/')

  out <- NULL
  for (i in 1:length(gwOut)){
    crn <- nm[[i]][1]  # crop name
    vnm <- nm[[i]][2]  # variety name

    for (j in 1:length(gwOut[[i]])){
      st <- gwOut[[i]][j] # sowing time
      sw <- subsetObj(w, c(st, st + 730))  # subset of w
      cro <- dwn.crop(cropName = crn, variety = vnm)
      out[[i]][[j]] <- WofostPP(crop = cro, w = sw)
    }
    names(out[[i]]) <- names(gwOut[[i]])
  }
  names(out) <- names(gwOut)
}
