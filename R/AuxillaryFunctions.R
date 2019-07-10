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
#' @param yamlFile Path to the .yaml file
#' @export
read.wofost.yaml<- function(yamlFile){

  # Read .yaml file with "yaml" package
  ryam<- yaml::read_yaml(yamlFile)

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
  for (i in 1:length(parm)){
    if(length((parm[[i]])) > 1){
      parm[[i]]<- make.afgen(parm[[i]])
    }
    parmClass[[i]]<- class(parm[[i]])
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


#' Plot Wofost test results
#'
#' Plots graphs of outputs of test trials
#'
#' @param output Dataframe of variables outputted by the function wofots.test.
#'               Column order must be the same as in "control".
#' @param control Control section of yaml test file as returned by function
#'                make.afgen
#' @param weather wather section of yaml test file as returned by function
#'                make.afgen
#' @param precision Precision section of yaml test file as returned by function
#'                make.afgen
#'@param keypress Logical. If TRUE, plots one graph at the time after a key is
#'                pressed. if FALSE, returns them all together, par(mfrow())
#'                can be used to place all plots in the same image.
#'
#' @importFrom grDevices rgb
#' @importFrom graphics abline legend lines par plot polygon
#'
plot.wofost.test <- function(output,control,weather,precision = FALSE,
                             keypress = TRUE){

  # Output: dataframe of variables outputted by the code to test. Column
  #         order must be the same as in "control".


  if(names(control)[1] == "DAY"){control <- control[2:ncol(control)]}

  unacceptable <- NULL

  for(i in 1:ncol(control)){

    par(mfrow=c(2,1))

    # Scatterplot of my output and control
    plot(weather$DAY, output[,i],col=4,
         main = names(output)[i], ylab = names(output)[i], xlab = 'Date')
    lines(weather$DAY,control[,i],col = 2)
    legend('topright',c('My output','Control'),col=c(4,2),pch = c(1,4),bty='n')

    # Difference between my output and control
    dif <- output[,i] - control[,i]

    # Plot of distribution of the difference between my output and control
    plot(weather$DAY, dif, type = 'n',
         main = 'Precision (my output - control),
         black line should be contained in green area.')

    # Area of acceptable results
    if (!isFALSE(precision)){
      x <- c(weather$DAY[1], weather$DAY[nrow(weather)],
            weather$DAY[nrow(weather)], weather$DAY[1])
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
    lines(weather$DAY, dif)

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


#' Extract test file variables
#'
#' Extracts any variable from yaml test files.
#'
#' @param direc Directory where the relevant yaml file is saved
#' @param name Name of test file (including extension)
#' @param variable Name of variable to retrieve
#' @param section Section of yaml file where variable is stored
#                 can be either 'results' (default) or 'externals'.

get.wofost.results <- function(direc,name,variable,section='results'){

  if(section == 'externals'){
    section<- 2
  } else if (section == 'results'){
    section<- 3
  }

  file<- paste0(direc,'/',name)
  yam<- read.wofost.yaml(file)
  varb<- yam[[section]][[variable]]
  varb<- as.numeric(levels(varb))[varb]
  return(varb)
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
#' Tests different components of this R implementation of Wofost by
#' comparison with the testing sets published in
#' De Wit 2018 (Agricultural Systems journal).
#'
#' @param component Character. Either one of the testing sets released
#' in De Wit 2018. Any of "assimilation", "astro", "leafdynamics",
#' "partitioning", "phenology", "potentialproduction", "respiration",
#' "rootdynamics", "transpiration", "waterlimitedproduction". If not specified
#' will run all available components.
#' However, currently limited to PP, LD, A.
#' @param complete Logical. If FALSE only a subset of 3 files per set will be
#' used for testing.
#' @export
#'
test<- function(component= "All", complete= FALSE){

  allcomp<- c("assimilation", "astro", "leafdynamics",
             "partitioning", "phenology", "potentialproduction",
             "respiration", "rootdynamics", "transpiration",
             "waterlimitedproduction")

  if(component != "All"){
    if(isFALSE(component %in% allcomp)){
      stop(paste0('component ',component,' not available for testing.'))
    }
    testall<- FALSE
  } else {testall<- TRUE}

  fls<- list.files('../WofostR/inst/extdata/')
  spl<- strsplit(fls,'_')
  gr<- NULL # groups of files pertaining the same tests
  for (i in 1:length(spl)){
    gr[i]<- spl[[i]][2]
  }

  # Potential Production
  if (component == "potentialproduction" | testall == TRUE){
    cat('\n','Testing Potential Production','\n','\n')
    xfls<- fls[gr == 'potentialproduction']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)
      yamlFile=paste0('../WofostR/inst/extdata/',xfls[i])
      test.PotentialProduction(yamlFile, keypress=F)
    }
  }

  # Leaf Dynamics
  if (component == "leafdynamics" | testall == TRUE){
    cat('\n','Testing Leaf Dynamics','\n','\n')
    xfls<- fls[gr == 'leafdynamics']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)
      yamlFile=paste0('../WofostR/inst/extdata/',xfls[i])
      test.LeafDynamics(yamlFile, keypress=F)
    }
  }

  # Assimilation
  if (component == "assimilation" | testall == TRUE){
    cat('\n','Testing Assimilation','\n','\n')
    xfls<- fls[gr == 'assimilation']
    if (isFALSE(complete)){xfls<- xfls[sample(1:length(xfls),3)] }
    for (i in 1:length(xfls)){
      print(i)
      yamlFile=paste0('../WofostR/inst/extdata/',xfls[i])
      test.Assimilation(yamlFile, keypress=F)
    }
  }

}




















