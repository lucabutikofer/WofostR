# INTERNAL, UNDOCUMENTED FUNCTIONS ####

get.wofost.results <- function(direc,name,variable,section='results'){

  # Extract test file variables
  #
  # Extracts any variable from yaml test files.
  #
  # @param direc Directory where the relevant yaml file is saved
  # @param name Name of test file (including extension)
  # @param variable Name of variable to retrieve
  # @param section Section of yaml file where variable is stored
  #                 can be either 'results' (default) or 'externals'.
  #

  if(section == 'externals'){
    section<- 2
  } else if (section == 'results'){
    section<- 3
  }

  file<- paste0(direc,'/',name)
  yam<- read.test.yaml(file)
  varb<- yam[[section]][[variable]]
  varb<- as.numeric(levels(varb))[varb]
  return(varb)
}


#' @importFrom grDevices rgb
#' @importFrom graphics abline legend lines par plot polygon
#'
plot.wofost.test <- function(output,control,weather,precision = FALSE,
                             keypress = TRUE){
  # Plot Wofost test results
  #
  # Plots graphs of outputs of test trials
  #
  # @param output Dataframe of variables outputted by the function wofots.test.
  #               Column order must be the same as in "control".
  # @param control Control section of yaml test file as returned by function
  #                make.afgen
  # @param weather weather section of yaml test file as returned by function
  #                make.afgen
  # @param precision Precision section of yaml test file as returned by function
  #                make.afgen
  # @param keypress Logical. If TRUE, plots one graph at the time after a key is
  #                pressed. if FALSE, returns them all together, par(mfrow())
  #                can be used to place all plots in the same image.
  #
  #
  # @return dataframe of variables outputted by the code to test. Column
  #         order must be the same as in "control".


  if(names(control)[1] == "DAY"){control <- control[2:ncol(control)]}

  unacceptable <- NULL

  for(i in 1:ncol(control)){

    par(mfrow=c(2,1))

    # Scatterplot of my output and control
    plot(weather@DAY, output[,i],col=4,
         main = names(output)[i], ylab = names(output)[i], xlab = 'Date')
    lines(weather@DAY,control[,i],col = 2)
    legend('topright',c('My output','Control'),col=c(4,2),pch = c(1,4),bty='n')

    # Difference between my output and control
    dif <- output[,i] - control[,i]

    # Plot of distribution of the difference between my output and control
    plot(weather@DAY, dif, type = 'n',
         main = 'Precision (my output - control),
         black line should be contained in green area.')

    # Area of acceptable results
    if (!isFALSE(precision)){
      x <- c(weather@DAY[1], weather@DAY[length(weather@DAY)],
             weather@DAY[length(weather@DAY)], weather@DAY[1])
      y <- c(precision[1,i],precision[1,i],-1*precision[1,i],-1*precision[1,i])
      polygon(x, y, col = rgb(0,1,0,.1))

      # Number of output estimation exceeding acceptable imprecision
      wrong<- rep(0,length(dif))
      wrong[(abs(output[,i] - control[,i]) - precision[1,i]) > precision[1,i]] <-
        1
      unacceptable[[i]] <- sum(wrong)
    }

    # Finish plot of distribution of the differences
    abline(h=0,col=4)
    lines(weather@DAY, dif)

    if(isTRUE(keypress)){
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }

  # Print a message saying which variables are rightly estimated
  # and which are wrong.
  if (!isFALSE(precision)){
    unacceptable <- unlist(unacceptable)
    passed <- NULL
    passed[unacceptable > 0]<- 'WRONG'
    passed[unacceptable <= 0]<- 'RIGHT'
    print(rbind(variables=names(control),passed))
  }
}


# EXPORTED, DOCUMENTED FUCTIONS ####

#' Afgen vector to table
#'
#' Renders afgen tables expressed as "vectors" into a "matrix" format.
#'
#' @param tab Afgen table expressed as vector.
#' @export
make.afgen<- function(tab){

  tab<- as.numeric(tab)
  x<- tab[seq(1,length(tab),2)]
  y<- tab[seq(2,length(tab),2)]

  # remove zeros at the end
  zeros<- 1:(length(x)-1)
  for(j in 1:length(x)){zeros[j]<- (x[j+1]-x[j])}
  if (min(zeros, na.rm = T) < 0){
    x<- x[1:which(zeros<0)]
    y<- y[1:which(zeros<0)]
  }

  # combine in matrix
  afgen.matrix<- cbind(x,y)

  return(afgen.matrix)

}


#' Reads .yaml test file
#'
#' Reads .yaml test files published in De Wit et al. 2019,
#' "25 years of the WOFOST cropping systems model", and returns R list.
#' @param yamlFile Path to the .yaml file or list object returned by function
#' yaml::read_yaml.
#' @return List version of yaml file.
#' @export
#'
read.test.yaml<- function(yamlFile){

  # Read .yaml file with "yaml" package
  if(class(yamlFile) == 'character'){
    ryam<- yaml::read_yaml(yamlFile)
  } else if (class(yamlFile) == 'list'){
    ryam<- yamlFile
  }

  # External states
  ext<- ryam[["ExternalStates"]]
  EXT<- NULL
  for(i in 1:length(ext)){
    EXT[[i]]<- unlist(ext[[i]])
  }
  if (!is.null(ext)){ext<- data.frame(do.call(rbind, EXT))}


  # Model parameters
  parm<- ryam[["ModelParameters"]]
  parmClass<-NULL
  if (!is.null(parm)){
    for (i in 1:length(parm)){
      if(length((parm[[i]])) > 1){
        parm[[i]]<- make.afgen(parm[[i]])
      }
      parmClass[[i]]<- class(parm[[i]])
    }
  }
  if ('list' %in% parmClass){
    parm[[which(parmClass == 'list')]]<- NULL
  }


  # Model Results
  res<- ryam[["ModelResults"]]
  RES<- NULL
  for(i in 1:length(res)){
    RES[[i]]<- unlist(res[[i]])
  }
  res<- data.frame(do.call(rbind, RES))

  # Weather Variables
  wea<- ryam[["WeatherVariables"]]
  WEA<- NULL
  for(i in 1:length(wea)){
    WEA[[i]]<- unlist(wea[[i]])
  }
  wea<- data.frame(do.call(rbind, WEA))

  # Precision
  pre<- ryam[["Precision"]]
  pre<- data.frame(t(do.call(rbind, pre)))


  # Combine in a list and return it

  return(list("parameters"   = parm,
              "externals"    = ext,
              "results"      = res,
              "weather"      = wea,
              "agro"         = ryam[['AgroManagement']][[1]][[1]][[1]],
              "precision"    = pre))

}


#' Tests afgen() function
#'
#' Performs a graphical test to check that afgen() function performs
#' correcly. Also useful to visualise the shape of the function represented
#' by an afgen table.
#'
#' @importFrom graphics points
#' @param at atrix. Afgen table
#' @export
#'
test.afgen<- function(at){

  rg<- diff(range(at[,'x']))
  xt<- sort(c(sample(seq(min(at[,'x']),max(at[,'x']),length.out=100),20),
              min(at[,'x'])-.2*rg, max(at[,'x'])+.2*rg))
  yt<-NULL
  for (i in 1:length(xt)) yt[i]<- afgen(xt[i], at)
  plot(at,type='b',col=4,xlim= c(min(xt),max(xt)),cex=2,lwd=2)
  points(xt,yt,col=2,pch=4,cex=1,lwd=2)
}


#' Test Wofost
#'
#' Tests different Wofost modules by comparison with the testing sets
#' published by De Wit et al. (2018).
#'
#' @param component Character. Name of module to test among:
#' "astro","phenology","transpiration","potentialproduction",
#' "waterlimitedproduction".
#'
#' If not specified will run all available components.
#'
#' @param complete Logical. If FALSE only a subset of 3 files per set will be
#' used for testing.
#'
#' @details Although testing sets for more modules have been made available in
#' De Wit et al. 2018, it does not make sense to test some available modules
#' since they have not been implemente in WofostR as independent components,
#' but rather they have been hard-coded into other functions.
#' Examples of these are 'partitioning', 'root dynamics', 'leaf dynamics',
#' and others that do not exhist as independet R functions.
#'
#' "phenology" tests the stand-alone module
#' Phenology() used by function growingWindow(). Phenological processes
#' are hardcoded within in Wofost() and can only be tested through
#' "potentialproduction" and "waterlimitedproduction".

#' @export
#'
test<- function(component= "All", complete= FALSE){

  allcomp<- c('astro','phenology','transpiration',
              'potentialproduction','waterlimitedproduction')

  if(component != "All"){
    if(isFALSE(component %in% allcomp)){
      stop(paste0('component ',component,' not available for testing.'))
    }
    testall<- FALSE
  } else {testall<- TRUE}

  # Retrieve list of test files names saved in "R/sysdata.rda"
  fls<- testFilesNames
  spl<- strsplit(fls,'_')
  gr<- NULL # groups of files pertaining the same tests
  for (i in 1:length(spl)){
    gr[i]<- spl[[i]][2]
  }

  # Astro
  if (component == "astro" | testall == TRUE){
    cat('\n','Testing Astro','\n','\n')
    xfls<- fls[gr == 'astro']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)]}
    for (i in 1:length(xfls)){
      print(i)

      request <- httr::GET(paste0(
        'https://raw.github.com/lucabutikofer/WofostR_testData/master/',
        xfls[i]
      ))
      httr::stop_for_status(request)
      handle <- textConnection(httr::content(request, as = 'text'))
      yamlFile<- yaml::read_yaml(handle)
      close(handle)

      test.Astro(yamlFile, keypress=F)
    }
  }

  # Phenology
  if (component == "phenology" | testall == TRUE){
    cat('\n','Testing Phenology','\n','\n')
    xfls<- fls[gr == 'phenology']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)]}
    for (i in 1:length(xfls)){
      print(i)

      request <- httr::GET(paste0(
        'https://raw.github.com/lucabutikofer/WofostR_testData/master/',
        xfls[i]
      ))
      httr::stop_for_status(request)
      handle <- textConnection(httr::content(request, as = 'text'))
      yamlFile<- yaml::read_yaml(handle)
      close(handle)

      test.Phenology(yamlFile, keypress=F)
    }
  }


  # Transpiration
  if (component == "transpiration" | testall == TRUE){
    cat('\n','Testing Transpiration','\n','\n')
    xfls<- fls[gr == 'transpiration']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)

      request <- httr::GET(paste0(
        'https://raw.github.com/lucabutikofer/WofostR_testData/master/',
        xfls[i]
      ))
      httr::stop_for_status(request)
      handle <- textConnection(httr::content(request, as = 'text'))
      yamlFile<- yaml::read_yaml(handle)
      close(handle)

      test.Evapotranspiration(yamlFile, keypress=F)
    }
  }

  # Potential Production
  if (component == "potentialproduction" | testall == TRUE){
    cat('\n','Testing Potential Production','\n','\n')
    xfls<- fls[gr == 'potentialproduction']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)

      request <- httr::GET(paste0(
        'https://raw.github.com/lucabutikofer/WofostR_testData/master/',
        xfls[i]
        ))
      httr::stop_for_status(request)
      handle <- textConnection(httr::content(request, as = 'text'))
      yamlFile<- yaml::read_yaml(handle)
      close(handle)

      test.PotentialProduction(yamlFile, keypress=F)
    }
  }

  # Water-Limited Production
  if (component == "waterlimitedproduction" | testall == TRUE){
    cat('\n','Testing Water-Limited Production','\n','\n')
    xfls<- fls[gr == 'waterlimitedproduction']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)

      request <- httr::GET(paste0(
        'https://raw.github.com/lucabutikofer/WofostR_testData/master/',
        xfls[i]
      ))
      httr::stop_for_status(request)
      handle <- textConnection(httr::content(request, as = 'text'))
      yamlFile<- yaml::read_yaml(handle)
      close(handle)

      test.WaterLimitedProduction(yamlFile, keypress=F)
    }
  }


}



#' Download crop parameters
#'
#' Downloads crop parameter files from
#' "https://github.com/ajwdewit/WOFOST_crop_parameters.git"
#'
#' @param cropName Character identifying the name of the crop to be
#' downlowaded. See list on link above.
#' @param variety Character.The variety name as mentioned in they yaml file
#' on link above.
#' @return CropObject
#' @export
#'
dwn.crop<- function(cropName='potato', variety='Potato_701'){

  request <- httr::GET(paste0(
    'https://raw.github.com/ajwdewit/WOFOST_crop_parameters/master/',
    cropName,
    '.yaml'
  ))
  httr::stop_for_status(request)
  handle <- textConnection(httr::content(request, as = 'text'))
  on.exit(close(handle))
  y<- yaml::read_yaml(handle)


  yv<- y[["CropParameters"]][["Varieties"]][[variety]]

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
  cr@CROPNAME<- cropName
  cr@VARNAME<- variety

  return(cr)

}

#' Subsets weather object
#'
#' Returns a section of the WeatherObject based on dates
#' @ param obj WeatherObject
#' @param interval Either a vector of integers representing the indices of the
#' days to extract or a vector of class="Date" and length=2 containing the
#' initial and final date of the period to extract.
#' @return WeatherObject of the selected period
#' @export
#'
subsetObj <- function(obj, interval){

  # Convert dates into indices
  if (class(interval) == 'Date'){
    interval1 <- which(w@DAY == interval[1])
    interval2 <- which(w@DAY == interval[2])
    interval <- seq(interval1, interval2, 1)
  }

  tempObj <- obj

  tempObj@DAY <- tempObj@DAY[interval]
  tempObj@E0 <- tempObj@E0[interval]
  tempObj@ELEV <- tempObj@ELEV[interval]
  tempObj@ES0 <- tempObj@ES0[interval]
  tempObj@ET0 <- tempObj@ET0[interval]
  tempObj@IRRAD <- tempObj@IRRAD[interval]
  tempObj@LAT <- tempObj@LAT[interval]
  tempObj@LON <- tempObj@LON[interval]
  tempObj@RAIN <- tempObj@RAIN[interval]
  tempObj@SNOWDEPTH <- tempObj@SNOWDEPTH[interval]
  tempObj@TEMP <- tempObj@TEMP[interval]
  tempObj@TMAX <- tempObj@TMAX[interval]
  tempObj@TMIN <- tempObj@TMIN[interval]
  tempObj@VAP <- tempObj@VAP[interval]
  tempObj@WIND <- tempObj@WIND[interval]

  return(tempObj)

}













