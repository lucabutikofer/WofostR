
# Wofost crop simulation ####

#' Wofost Wofost crop simulation
#'
#' Computes potential or water-limited production on freely draining soils
#' without water table.
#'
#' @param crop CorpObject.
#' Object of class CropObject containing the specific
#' crop parameters. See ?CropObject. To specify a crop sequence (rotation)
#' use "manager".
#' @param w WeatherObject. Object of class CropObject containing the climatic
#' driving variables. See ?WeatherObject.
#' @param soil SoilObject. Object of class SoilObject containing the soil
#' parameters. See ?SoilObject.
#' @param manager Optional ManagerObject. If present "startType" and
#' "finishType will be ignored".
#' @param waterLimited Logial. Wether to simulate potential production
#' ("= FALSE", default), or water limited production ("= TRUE")
#' @param startType Development stage at which the simulation is started.
#' Either "sowing" or "emergence". Ignored if "manager" is specified.
#' @param finishType Variable describing the conditionst triggering
#' the end of the simulation.
#' Can be either "maturity" -The model is terminated 7 days after maturity is
#' reached - or
#' an integer [1:365] -Maximum number of days for which the model is run.
#' Ignored if "manager" is specified.
#'
#' @param cropDir Character vector containing the directory where crop files
#' are stored as .yaml files. If not specified and a CropObject is not
#' assigned to parameter "crop", crop files will be downloaded from
#' "https://github.com/ajwdewit/WOFOST_crop_parameters".
#' @param varReturn Character vector specifying which variables to output.
#' Potentially, any of the variables produced inside the Wofost function
#' can be returned. However the use of carReturn = NULL (default) is
#' encouraged. By default returning variables described in "Returns".
#' @param activate.verndvs Logical. If TRUE, allows the use of variable
#' "VERNDVS". A critical development stage (VERNDVS) is used to stop the effect
#' of vernalisation when this DVS is reached. This is done to improve model
#' stability in order to avoid that Anthesis is never reached due to a
#' somewhat too high VERNSAT. Nevertheless, a warning is written to the log
#' file, if this happens.
#' @param activate.stopInSeven Logical. If TRUE, the simulation stops seven
#' days after maturity is reached. If FALSE, the simulation terminates when
#' maturity is reached. Ignored if "finishType" is numeric.
#' @export
#'
#' @examples
#'
#' # Run Potential Production
#' cr <- dwn.crop()
#' outPP <- Wofost(crop = cr, w = randomWeather)
#' plot(outPP)
#'
#' # Run Water-Limited Production
#' cr <- dwn.crop()
#' outFD <- Wofost(crop = cr, w = randomWeather, soil = randomSoil,
#'                 waterLimited = TRUE)
#' plot(outFD)
#'
#' # Run crop rotation with ManagerObject
#' outMN <- Wofost(w = randomWeatherLong, manager = exampleManager)
#' plot(outMN)
#'
#'
Wofost<- function(
  w , crop = NULL, soil = NULL, manager = NULL,
  waterLimited = FALSE,
  startType= 'sowing',
  finishType= 'maturity',
  activate.verndvs= TRUE,
  activate.stopInSeven= FALSE,
  cropDir= NULL,
  varReturn= NULL
){


  #  IF MANAGER IS SPECIFIED ####

  if (!is.null(manager)){
    WofostR:::manager.check(manager)

    OUT <- NULL  # stores outputs for all Wofost runs
    SD <- as.Date(manager@sequenceStart)  # keeps track of start dates
    FD <- as.Date(manager@sequenceStart)  # keeps track of finish dates

    crops <- strsplit(manager@cropSequence, '-')
    crnames <- NULL; vrnames <- NULL
    for (j in 1:length(crops)) {
      crnames[j] <- crops[[j]][1]
      vrnames[j] <- crops[[j]][2]
    }

    for (m in 1:length(crops)){ # for each crop to run

      # MAKE CROP OBJECT

      if(!is.null(cropDir)){ # if crops are available locally

        y <- yaml::read_yaml(paste0(cropDir, '/', crops[[m]][1],'.yaml'))
        yv <- y[["CropParameters"]][["Varieties"]][[crops[[m]][2]]]

        yv0<- NULL
        for(i in 1:length(yv)){
          yv0[[i]]<- yv[[i]][[1]]
        }
        names(yv0)<- names(yv)

        for (i in 1:length(yv0)){ # make matrixes out of afgen vectors
          if(length((yv0[[i]])) > 1){
            yv0[[i]]<- make.afgen(yv0[[i]])
          }
        }
        cr<- CropObject(yv0)
      } else {  # if crops are to be downloaded
        cr <- dwn.crop(cropName = crops[[m]][1], variety = crops[[m]][2])
      }

      # DETERMINE CROP STARTING DATE

      # start date
      if (m == 1){
        SD <- as.Date(manager@sequenceStart) + manager@spacing[m]
      } else if (m > 1) {
        if (is.na(manager@cropStartDate[m])){ # if cropStartDate is NA
          SD[m] <- FD[m - 1] + manager@spacing[m]
        } else { # if cropStartDate is specifiend
          SD[m] <- manager@cropStartDate[m]
        }
      }

      # maximum duration
      if (grepl('/', manager@cropFinish[m])){ # if cropFinish is a date
        # maximum number of days for the model to run
        maxlen <- as.numeric(as.Date(manager@cropFinish[m]) - SD[m])
      } else if (!grepl('/', manager@cropFinish[m])){ # if is crop stage
        maxlen <- manager@cropFinish[m]
      }


      # SUBSET WEATHER FILE

      # subset period
      if ((as.Date(SD[m]) + 365) > max(w@DAY)){
        tf <- c(as.Date(SD[m]), max(w@DAY)) # timeframe
      } else {
        tf <- c(as.Date(SD[m]), as.Date(SD[m]) + 365) # timeframe
      }
      wr <- subsetObj(obj = w, interval = tf)

      # RUN WOFOST
      if (!isTRUE(waterLimited)){  # potential production
        OUT[[m]] <- WofostPP(crop = cr, w = wr,
                             varReturn = varReturn,
                             startType = manager@cropStartType[m],
                             finishType = maxlen)
      } else if (isTRUE(waterLimited)){  # water-limited production
        OUT[[m]] <- WofostFD(crop = cr, w = wr, soil = soil,
                             varReturn = varReturn,
                             startType = manager@cropStartType[m],
                             finishType = maxlen)
      }
      # save finish date
      FD[m] <- SD[m] + length(OUT[[m]][[1]]) - 1
    }

    names(OUT) <- paste0(crnames, '_', as.character(SD))

    # Finish Type
    if (!is.null(OUT[[1]]['dvs'][[1]])){ # if variable "dvs" is present
      ft <- NULL
      for (j in 1:length(OUT)) {
        ft[j] <- max(OUT[[j]]['dvs'][[1]]) # finish type
      }
      ft[ft == 2] <- 'mature'
      ft[ft < 2 & ft >= 1] <- 'reproductive'
      ft[ft < 1 & ft >= 0] <- 'vegetative'
      ft[ft < 0] <- 'emerging'
    } else {ft <- rep('unknown', length(OUT))}

    dsc <- data.frame('crop' = crnames, 'variety' = vrnames,
                      'startDate' = SD, 'finishDate' = FD,
                      'startType' = manager@cropStartType,
                      'finishType' = ft)
    so <- SimulationObject(description = dsc, variables = OUT)
    return(so)


    #  IF MANAGER IS NOT SPECIFIED ####

  } else if (is.null(manager)){
    # RUN WOFOST
    if (!isTRUE(waterLimited)){  # potential production

      OUT <- WofostPP(crop = crop, w = w,
                           varReturn = varReturn,
                           startType = startType,
                           finishType = finishType,
                           activate.verndvs= TRUE,
                           activate.stopInSeven= FALSE)

    } else if (isTRUE(waterLimited)){  # water-limited production

      OUT <- WofostFD(crop = crop, w = w, soil = soil,
                           varReturn = varReturn,
                           startType = startType,
                           finishType = finishType,
                           activate.verndvs= TRUE,
                           activate.stopInSeven= FALSE)
    }
    OUT <- list(OUT)
    names(OUT) <- paste0(crop@CROPNAME, '_', as.character(w@DAY[1]))
    dsc <- data.frame('crop' = crop@CROPNAME, 'variety' = crop@VARNAME,
                      'startDate' = w@DAY[1],
                      'finishDate' = w@DAY[1] + length(OUT[[1]][[1]]) - 1,
                      'startType' = startType,
                      'finishType' = finishType)
    so <- SimulationObject(description = dsc, variables = OUT)
    return(so)
  }
}
