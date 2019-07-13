
RootDynamics<- function(RDI,RDMCR,RDMSOL,RDRRTB,RRI,TDWI,
                        w,ext
                        ){


  # loop control parameters
  STOP<- FALSE
  t<- 1
  # outputs
  twrt_out<- NULL    # root dry matter
  rd_out<- NULL

  rd<- 0


  # LOOP STARTS ####
  #~~~~~~~~~~~~~~~~~

  while (isFALSE(STOP)){ # main loop.
    # 't' counts the days, one iteration per day.

    if (t <= nrow(w)){
      extt<- ext[t,]
      for (v in 1:ncol(extt)){assign(names(extt)[v], extt[1,v])}
    }
    fr<-FR
    dvs<-DVS
    dmi<-DMI

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > INITIALISATION ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t == 1){ # first iteration of the model

      # >> ROOT DYNAMICS ####

      # Initial root depth
      rdmax<- max(RDI, min(RDMCR, RDMSOL))
      rdm<- rdmax
      rd<- RDI

      # initial root biomass
      wrt<- TDWI*fr
      dwrt<- 0
      twrt<- wrt + dwrt

    } # end of first iteration of the model



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > COMPUTATIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Before emergence there is no need to continue
    # because only the phenology is running.
    if (dvs >= 0){


      # >> GROWTH OF PLANT ORGANS ####

      # Growth of roots
      grrt<- fr*dmi                  # Growth rate of roots
      drrt<- wrt*afgen(dvs, RDRRTB)  # Death rate of roots
      gwrt<- grrt - drrt             # Dry weight of living roots
      # Increase in root depth
      rr<- min((rdm - rd), RRI)
      # Do not let the roots grow if partioning to the roots (fr) is zero
      if (fr == 0){rr<- 0}
    } # End of post-emergence section

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > TEST FINISH CONDITIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t > nrow(w)) {STOP<- TRUE}
    if (isFALSE(STOP)){ # continue only if finish conditions are
      # not reached.


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > COLLECT OUTPUT ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      twrt_out[t]<-    twrt    # root dry matter
      rd_out[t]<-      rd      # root depth


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > TIMER UPDATE ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      t<- t + 1


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > INTEGRATION ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Before emergence there is no need to continue
      # because only the phenology is running.
      if (dvs >=0){ # After emergence

        # >> GROWTH OF PLANT ORGANS ####

        # New roots biomass
        wrt<- wrt + gwrt
        dwrt<- dwrt + drrt
        twrt<- wrt + dwrt

        # New root depth
        rd<- rd + rr

      } # end of post-emergence section
    } # end of post-finish-conditions-test section
  } # end of daily cycles


  # FINALISATION ####
  #~~~~~~~~~~~~~~~~~~

  return(list(
    'rd'=rd_out,       #root depth
    'twrt'=twrt_out   # root dry matter
  ))

}
