
LeafDynamics<- function(KDIFTB,PERDL,RGRLAI,SLATB,SPAN,TBASE,TDWI,
                        w,ext){


  # VARIABLE DECLARATIONS ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # loop control parameters
  STOP<- FALSE
  t<- 1
  # outputs
  lai_out<- NULL     # leaf dynamics
  twlv_out<- NULL    # leaf dynamics


  # LOOP STARTS ####
  #~~~~~~~~~~~~~~~~~

  while (isFALSE(STOP)){ # main loop.
    # 't' counts the days, one iteration per day.

    if (t <= length(w@DAY)){
      extt<- ext[t,]
      for (v in 1:ncol(extt)){assign(names(extt)[v], extt[1,v])}
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > INITIALISATION ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t == 1){ # first iteration of the model


      # >> LEAF DYNAMICS ####

      # Set initial dataframe with all living leaves
      leaves<- data.frame(matrix(ncol=4, nrow=1))
      names(leaves)<- c('paget','slat','dwlv','dwnlv')
      leaves[1,]<- c(0,0,0,0)

      # Initial leaf biomass
      wlv<- ((1-fr)*TDWI)*fl
      dwlv<- 0
      twlv<- wlv + dwlv

      # First leaf class (sla, age and weight)
      slat<- afgen(dvs, SLATB)
      paget<- 0
      leaves$dwlv[1]<- wlv
      leaves$dwnlv[1]<- leaves$dwlv
      leaves$slat[1]<- slat
      leaves$paget[1]<- paget

      # Initial values for leaf area
      LAIEM<- wlv*slat
      lasum<- LAIEM
      laiexp<- LAIEM
      laimax<- LAIEM
      lai<- lasum + sai + pai

    } # end of first iteration of the model



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > COMPUTATIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # >> TEMPERATURE ####

    # average temperature and average daytemperature
    if (t <= length(w@DAY)){ # necessary to have model to run till last row of test.
      temp<- (w@TMIN[t] + w@TMAX[t])/2
    }

    # Before emergence there is no need to continue
    # because only the phenology is running.
    if (dvs >= 0){

      # Leaf dynamics
      # Rate of increase of leaf dry matter
      grlv<- fl*admi

      # # Death of leaves due to water stress
      # dw1d<- wlv*(1 - rftra)*PERDL
      dw1d<- 0

      # Death of leaves due to high LAI
      kdf<- afgen(dvs, KDIFTB)
      laic<- crit_lai(kdf)
      dw2d<- lai_death(wlv, lai, laic)

      # Leaf death equals maximum of water stress and shading
      wd<- max(dw1d, dw2d)

      # Leaf biomass that will have to die. Note that the actual leaf death
      # is imposed on the array LV during the state integration step.
      dalv<- sum(leaves[leaves$paget> SPAN, 'dwnlv'])

      # Dry weight of dead leaves
      drlv<- max(wd, dalv)

      # Phisiologic ageing factor for leaves
      frai<- ag_fac(temp, TBASE)

      # Specific leaf area of leaves per time step
      slat<- afgen(dvs, SLATB)

      # Leaf area not to exceed exponential growth curve
      if (laiexp< 6){
        # exponential growth of lai
        dteff<- max(0, temp - TBASE)
        glaiex<- laiexp*RGRLAI*dteff
        # source-limited grwth of lai
        glasol<- grlv*slat
        # final growth rate of lai
        gla<- min(glaiex, glasol)
        # adjustment of specific leaf area inded of youngest leaf class
        if (grlv> 0) {slat<- gla/grlv}
      }
    } # End of post-emergence section

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > TEST FINISH CONDITIONS ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (t > length(w@DAY)) {STOP<- TRUE}
    if (isFALSE(STOP)){ # continue only if finish conditions are
      # not reached.


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > COLLECT OUTPUT ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # print(paste('t=',t,'lai=',lai,'twlv'=twlv))
      lai_out[t]<-     lai     # leaf dynamics
      twlv_out[t]<-    twlv    # leaf dynamics


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > TIMER UPDATE ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      t<- t + 1


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # > INTEGRATION ####
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Before emergence there is no need to continue
      # because only the phenology is running.
      if (dvs >=  0){ # After emergence


        # Leaf dynamics
        # remove weight of dead leaves from water stress and high LAI
        if (wd==0) {leaves[1,'dwnlv']<- leaves[1,'dwlv']}
        i<-  nrow(leaves)
        while (wd > 0){
          leaves[i,'dwnlv']<- max(0, leaves[i,'dwlv'] - wd)
          wd<- max(0, wd - leaves[i,'dwlv'])
          i<- i - 1
        }

        # remove leaves older than the specific life spans
        leaves<- leaves[leaves$paget<= SPAN,]

        # physiologic ages
        leaves[,'paget']<- phy_age(leaves[,'paget'], frai)

        # add new leaves born in current iteration
        leaves<- rbind(c(0,slat,grlv,grlv),leaves)

        # New leaf area
        lasum<- sum(leaves$dwnlv*leaves$slat)  # total living leaf area
        lai<- lasum + sai + pai
        laimax<- max(lai, laimax)

        # Exponential growth curve
        laiexp<- laiexp + glaiex

        # New leaf biomass
        wlv<- sum(leaves$dwnlv)  # weight of living leaves
        dwlv<- dwlv + drlv  # accumulated dry weight of dead leaves
        twlv<- wlv + dwlv  # accumulated dry weight of dead and leaving leaves

        # Update leaf weight after leaf death
        leaves$dwlv<- leaves$dwnlv


      } # end of post-emergence section
    } # end of post-finish-conditions-test section
  } # end of daily cycles


  # FINALISATION ####
  #~~~~~~~~~~~~~~~~~~

  return(list(
              'lai'=lai_out,     # leaf dynamics
              'twlv'=twlv_out    # leaf dynamics
  ))

}
