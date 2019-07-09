


#' Astro routine testing
#'
#' Tests the Astro routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param rtn Logical. If TRUE returns a list (see below)
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.Astro<- function(yamlFile, plt = TRUE, rtn = FALSE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }
  lat<- w$LAT[1]

  out<-NULL
  for (t in 1:nrow(w)){out[[t]]<- Astro(w=w,t=t,lat=lat)}
  for (t in 1:length(out)){out[[t]] <- unlist(out[[t]])}
  out <- data.frame(do.call(rbind,out))

  contr<- yam$results

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w, ...)
  }

  if(isTRUE(rtn)) return(list('output'=out,'control'=contr))

}

#' Assimilation routine testing
#'
#' Tests the Assimilation routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.Assimilation<- function(yamlFile, plt = TRUE, rtn = FALSE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }
  lat<- w$LAT[1]

  # Externals
  ext<- yam$externals
  ext<-yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  for (v in 1:length(yam[["externals"]])){
    assign(names(ext)[v], ext[,v])}

  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<-NULL
  for (t in 1:nrow(w)){

    lai<- LAI[t]
    dvs<- DVS[t]
    sgd<- w$IRRAD[t]

    # average temperature and average daytemperature
    if (t <= nrow(w)){ # necessary to have model to run till last row of test.
      temp<- (w$TMIN[t] + w$TMAX[t])/2
      tday<- (w$TMAX[t] + temp)/2
    }

    # Nighttime 7 days running minimum temperature
    if (t <= nrow(w)){ # necessary to have model to run till last row of test.
      tmins<- w$TMIN[(max(t-6,1)):t]
    }
    tlow<- week_tmin_av(tmins)

    # Before emergence there is no need to continue
    # because only the phenology is running.
    if (dvs >= 0){

      # Astro
      if (t <= nrow(w)){ # necessary to have model running last row of test.
        astro<- Astro(w,t,lat)
      }
      # for (v in 1:length(astro)){assign(names(astro)[v], astro[[v]])}

      # Assimilation
      out[[t]]<- Totas_Assim(tday,astro$d,astro$sold,lat,astro$sgd,
                             astro$sinbm,astro$dp,s=0.2,lai,
                             dvs,t,tlow,
                             KDIFTB,
                             AMAXTB,
                             TMPFTB,
                             TMNFTB,
                             EFFTB)
    } else { out[[t]] <- 0}
  }
  out <- data.frame(out)

  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  contr[,2]<- as.numeric(levels(contr[,2]))[contr[,2]]

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  if(isTRUE(rtn)) return(list('output'=out,'control'=contr))

}


#' Respiration routine testing
#'
#' Tests the Respiration routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.Respiration<- function(yamlFile, plt = TRUE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }

  # Externals
  ext<- yam$externals
  ext<-yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  for (v in 1:length(yam[["externals"]])){
    assign(names(ext)[v], ext[,v])}

  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<-NULL
  for (t in 1:nrow(w)){

    dvs<- DVS[t]
    wlv<- WLV[t]
    wso<- WSO[t]
    wst<- WST[t]
    wrt<- WRT[t]

    # average temperature and average daytemperature
    if (t <= nrow(w)){ # necessary to have model to run till last row of test.
      temp<- (w$TMIN[t] + w$TMAX[t])/2
    }

    # Before emergence there is no need to continue
    # because only the phenology is running.
    if (dvs >= 0){

      # Maintenance coefficient of given organ i
      cmi<- c(RML, RMO, RMS, RMR)
      wi<- c(wlv,wso,wst,wrt)

      # Maintanance respiration rate at reference temperature of 25 degrees
      rmr<- maint_resp(cmi, wi)
      rmr<- rmr*afgen(dvs, RFSETB)

      # Maintanance respiration rate at temperature t
      rmt<- maint_resp_t(rmr, Q10, temp, tr=25)

      out[[t]]<- rmt
    } else {out[[t]]<- 0}
  }
  out <- data.frame(out)

  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  contr[,2]<- as.numeric(levels(contr[,2]))[contr[,2]]

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  return(list('output'=out,'control'=contr))

}

#' Partitioning routine testing
#'
#' Tests the Partitioning routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.Partitioning<- function(yamlFile, plt = TRUE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }

  # Externals
  ext<- yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  for (v in 1:length(yam[["externals"]])){
    assign(names(ext)[v], ext[,v])}

  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<-NULL
  for (t in 1:nrow(w)){

    dvs<- DVS[t]

    fr<-  afgen (dvs, FRTB)
    fl<-  afgen (dvs, FLTB)
    fs<-  afgen (dvs, FSTB)
    fo<-  afgen (dvs, FOTB)

    out[[t]]<- c('fl'=fl,'fo'=fo,'fr'=fr,'fs'=fs)
  }
  out<- data.frame(do.call(rbind,out))

  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  for(i in 2:ncol(contr)){
    contr[,i]<- as.numeric(levels(contr[,i]))[contr[,i]]
  }

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  return(list('output'=out,'control'=contr))

}

#' Root Dynamics routine testing
#'
#' Tests the Root Dynamics routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.RootDynamics<- function(yamlFile, plt = TRUE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }

  # Externals
  ext<- yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  for (v in 1:length(yam[["externals"]])){
    assign(names(ext)[v], ext[,v])}


  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<- RootDynamics(RDI,RDMCR,RDMSOL,RDRRTB,RRI,TDWI,
                          w,ext)

  out<- data.frame(cbind(out$rd,out$twrt))
  names(out)<-c('rd','twrt')

  # results
  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  for(i in 2:ncol(contr)){
    contr[,i]<- as.numeric(levels(contr[,i]))[contr[,i]]
  }

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  return(list('output'=out,'control'=contr))

}

#' Leaf Dynamics routine testing
#'
#' Tests the Leaf Dynamics routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.LeafDynamics<- function(yamlFile, plt = TRUE, rtn = FALSE,...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }

  # Externals
  ext<- yam$externals
  ext<- ext[c(2,1,3:ncol(ext))]
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  names(ext)<- c('DAY','admi','dvs','fl','fr','pai','sai','tra','tramx')

  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<- LeafDynamics(KDIFTB,PERDL,RGRLAI,SLATB,SPAN,TBASE,TDWI,
                     w,ext)

  out<- data.frame(cbind(out$lai,out$twlv))
  names(out)<-c('lai','twlv')

  # results
  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  for(i in 2:ncol(contr)){
    contr[,i]<- as.numeric(levels(contr[,i]))[contr[,i]]
  }

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  if(isTRUE(rtn)) return(list('output'=out,'control'=contr))

}

#' Evapotranspiration routine testing
#'
#' Tests the Evapotranspiration routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.Evapotranspiration<- function(yamlFile, plt = TRUE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }

  # Externals
  # Externals
  ext<- yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  for (v in 1:length(yam[["externals"]])){
    assign(names(ext)[v], ext[,v])}

  # Parameters
  for (v in 1:length(yam[["parameters"]])){
    assign(names(yam[["parameters"]])[v], yam[["parameters"]][[v]])}

  # Run model
  out<- NULL
  for (t in 1:nrow(w)){
    lai<- LAI[t]
    sm<- SM[t]
    dvs<- DVS[t]

    if(dvs >= 0){
      out[[t]]<- Evapotranspiration(dvs,w,lai,sm,t,dsos=0,SM0,DEPNR,SMFCF,
                                    IAIRDU,IOX,
                                    CFET,SMW,CRAIRC)
    } else {out[[t]]<- list('evsmx'=0,'evwmx'=0,'tra'=0,'tramx'=0,'rftra'=0)}
  }
  OUT<- NULL
  for (i in 1:length(out)){
    OUT[[i]]<-as.numeric(out[[i]])
  }
  out<- data.frame(do.call(rbind,OUT))
  names(out)<- c('evsmx','evwmx','tra','tramx','rftra')

  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  for (i in 2:ncol(contr)){
    contr[,i]<- as.numeric(levels(contr[,i]))[contr[,i]]
  }

  if (isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  return(list('output'=out,'control'=contr))

}


#' Potential Production routine testing
#'
#' Tests the Potential Production routine with the testing data of
#' De Wit et al. 2018
#'
#' @param yamlFile Path to yaml test file
#' @param plt Logical. If TRUE prints plots of results with plot.wofost.test
#' @param rtn Logical. If TRUE returns a list (see below)
#' @param ... Arguments passed to function plot.wofost.test
#' @return List of 2 elements. 'output': the variables values produced by the
#'         script 'control': the variables values saved in the yaml file.
#' @export
#'
test.PotentialProduction<- function(yamlFile, plt = TRUE, rtn = FALSE, ...){

  yam<- read.wofost.yaml(yamlFile = yamlFile)

  # Weather
  w<- yam$weather
  w[,1]<- as.Date(w[,1])
  for(i in 2:ncol(w)){
    w[,i]<- as.numeric(levels(w[,i]))[w[,i]]
  }
  lat<- w$LAT[1]

  # parameters
  prmt<- yam[["parameters"]]
  simo<- SimulationObject(parameters = prmt)

  # Externals
  ext<- yam$externals
  ext[,1]<- as.Date(ext[,1])
  for(i in 2:ncol(ext)){
    ext[,i]<- as.numeric(levels(ext[,i]))[ext[,i]]
  }
  # for (v in 1:length(yam[["externals"]])){
  #   assign(names(ext)[v], ext[,v])}

  # run model
  out<- Potential_production(
                             crop=simo, w=w,lat=lat,ext=ext,
                             waterlimited= T
                             )
  for (i in 1:length(out)){out[[i]]<-as.numeric(out[[i]])}
  out<- data.frame(do.call(cbind,out))

  # results
  contr<- yam$results
  contr[,1]<- as.Date(contr[,1])
  for(i in 2:ncol(contr)){
    contr[,i]<- as.numeric(levels(contr[,i]))[contr[,i]]
  }

  if(isTRUE(plt)){
    plot.wofost.test(control = contr, output = out, weather = w,
                     precision = yam[["precision"]], ...)
  }

  if(isTRUE(rtn)) return(list('output'=out,'control'=contr))

}
