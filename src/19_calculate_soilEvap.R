calculate_soilEvaporation<-function(P,          #precipitation on day d
                                    irrigation, #irrigation applied on day d
                                    infil,      #infiltration on day d
                                    d,          #day of the year
                                    doS,        #day of the cropSeason
                                    crop.season,#name of crop which is being grown
                                    CCadj,      #adjusted canopy cover on day d
                                    ETo,        #reference evapotranspiration on day d
                                    surface.Storage,#surface storage on day d
                                    soil,       # soil information
                                    ST,
                                    Wsurf,
                                    EvapZ,
                                    Stage2,
                                    Wstage2)
{
  
  #variables used 
  #Wsurf           storage in surface evaporation layer
  #Wevap
  #Wstage2         proportional water content for start of stage 2 evaporation
  #REW             readily evaporable water
  #EsPot           potential soil evaporation rate
  #EsAct           actual soil evaporation rate
  #ToExtract       available atmospheric demand after one round of evaporation
  #ExtractPotStg1  total water to be extracted in stage1
  #comp_sto        soil compartment covered by evaporation layer
  #comp            counter variable for soil compartment
  #l               soil layer
  #Wdry            Water storage at dry air (mm)
  #W               Available water (mm)
  #AvW             water available for extraction (mm)
  #Wupper          Get water storage (mm) at start of stage 2 evaporation
  #Wlower          Get water storage (mm) when there is no evaporation
  #Wrel            Get relative depletion of evaporation storage in stage 2
  #Kr              Get stage 2 evaporation reduction coefficient
  #ToExtractStg2   Get water to extract (mm) in stage2
  
  EvapZmin  <- 0.15 # Minimum thickness of full soil surface evaporation layer (m)
  EvapZsurf <- 0.04 # Thickness of soil surface skin evaporation layer (m)
  Kex       <- 1.1  # Maximum soil evaporation coefficient
  EvapZmax  <- 0.30 # Maximum thickness of full soil surface evaporation layer (m)
  fWrelExp  <- 0.4  # Proportional value of Wrel at which soil evaporation layer expands
  fevap     <- 4    # Shape factor describing reduction in soil evaporation in stage 2.
  
  REW   <- round(1000*(soil$soil.layer$FC[1]-(0.5*soil$soil.layer$WP[1]))*EvapZsurf)
  
  # Prepare stage 2 evaporation (REW gone) Only do this if it is first 
  # day of simulation, or if it is first day of growing season 
  if( d==1 |  (is.na(doS)==F & doS==1) )
  {
    Wsurf  <- 0        # Reset storage in surface soil layer to zero
    EvapZ  <- EvapZmin # Set evaporation depth to minimum
    Stage2 <- T        # Trigger stage 2 evaporation
    # Get relative water content for start of stage 2 evaporation
    Wevap   <- calculate_EvapLayerWaterContent(soil,EvapZ,ST)
    Wstage2 <- (Wevap$Act-(Wevap$Fc-REW))/(Wevap$Sat-(Wevap$Fc-REW))
    Wstage2 <- round((100*Wstage2))/100
    if(Wstage2 < 0)
      Wstage2 <- 0
  }

  #preparing soil evaporation (Stage1)
  if(P>0 | irrigation>0) #only when rainfall occurs of irrigation is triggered
  {
    if(infil>0)
    {
      Wsurf <- infil # update surface storage for incoming infiltration
      if(Wsurf>REW)  # surface storage evaporation layer cannot exceed REW
        Wsurf <- REW
      #reset variables
      Wstage2 <- NULL 
      EvapZ   <- EvapZmin
      Stage2  <- F
    }
  }
  
  #calculate potential soil evaporation rate (mm/day)
  {
    if(!is.na(crop.season)) #check for growing season
    {
      var   <- 1 - CCadj  # (1-CC.) :adjusted fraction of the non-covered soil surface
      var   <- ifelse(var<=0,0,var)
      EsPot <- Kex*var*ETo
    }
    else
      EsPot <- Kex*ETo      #no canopy cover during non-growing season
  }
  
  #Surface evaporation----------------------------------------------------------
  EsAct <- 0 #actual evaporation
  if(surface.Storage > 0)
  {
    EsPot <- Kex*ETo
    if(surface.Storage > EsPot)
    {
      EsAct <- EsPot  #All potential soil evaporation can be supplied by surface storage
      surface.Storage <- surface.Storage-EsAct #update surface storage
    }
    else
    {
      EsAct <- surface.Storage #Surface storage is not sufficient to meet all potential soil evaporation
      surface.Storage <- 0     #updating surface storage
      Wsurf           <- REW
      Wstage2         <- NULL
      EvapZ           <- EvapZmin
      Stage2          <- F
    }
  }
  
  #Stage1 evaporation-----------------------------------------------------------
  ToExtract      <- EsPot-EsAct          #Determine total water to be extracted
  ExtractPotStg1 <- min(ToExtract,Wsurf) #total water to be extracted in stage one
  if(ExtractPotStg1>0)
  {
    comp_sto <- which(cumsum(soil$soil.comp$del.z)<EvapZmin) + 1 #soil compartment covered by evaporation layer
    comp     <- 0                                                #counter variable for soil compartment
    while( ExtractPotStg1>0 & comp<comp_sto)
    {
      comp <- comp+1                     #increment of compartment counter
      l    <- soil$soil.comp$layer[comp] #specify soil layer
      
      #Determine proportion of compartment in evaporation layer
      if( sum(soil$soil.comp$del.z[1:comp]) > EvapZmin )
        factor <- 1 - ( ( sum(soil$soil.comp$del.z[1:comp]) - EvapZmin ) / soil$soil.comp$del.z[comp] )
      else
        factor <- 1
      
      Wdry <- 1000*(0.5*soil$soil.layer$WP[l])*soil$soil.comp$del.z[comp] #Water storage at dry air (mm)
      W    <- 1000*ST[comp]*soil$soil.comp$del.z[comp]                    #Available water (mm)
      AvW  <- (W-Wdry)*factor                                             #water available for extraction (mm)
      if(AvW<0) 
        AvW <- 0
      if(AvW >= ExtractPotStg1)
      {
        EsAct          <- EsAct+ExtractPotStg1     # Update actual evaporation
        W              <- W-ExtractPotStg1         # Update depth of water in current compartment
        ToExtract      <- ToExtract-ExtractPotStg1 # Update total water to be extracted
        ExtractPotStg1 <- 0                        # Update water to be extracted from surface layer (stage 1)
      }
      else
      {
        EsAct          <- EsAct+AvW           # Update actual evaporation
        ExtractPotStg1 <- ExtractPotStg1-AvW  # Update water to be extracted from surface layer (stage 1)
        ToExtract      <- ToExtract-AvW       # Update total water to be extracted
        W              <- W-AvW               # Update depth of water in current compartment
      }
      
      #Update water content
      ST[comp] <- W/(1000*soil$soil.comp$del.z[comp])
    }
    
    #Update surface evaporation layer water balance
    Wsurf <- Wsurf - EsAct   ####CHECK THIS LINE
    if (Wsurf < 0 | ExtractPotStg1 > 0.0001)
      Wsurf = 0
    
    #If surface storage completely depleted, prepare stage 2
    if(Wsurf < 0.0001)
    {
      Wevap   <- calculate_EvapLayerWaterContent(soil,EvapZ,ST)     #get water contents (mm)
      Wstage2 <- (Wevap$Act-(Wevap$Fc-REW))/(Wevap$Sat-(Wevap$Fc-REW)) 
      Wstage2 <- round((100*Wstage2))/100
      if(Wstage2 < 0)
        Wstage2 <- 0
    }
  }
  
  #Stage 2 evaporation
  #Extract water
  if(ToExtract > 0)
  {
    Stage2 <- T                       #Start stage 2
    Edt    <- ToExtract/20            # Get sub-daily evaporative demand
    for(jj in 1:20)                   # Loop sub-daily steps (as per Aquacrop module)
    {
      Wevap  <- calculate_EvapLayerWaterContent(soil,EvapZ,ST)    # Get current water storage (mm)
      Wupper <- Wstage2*(Wevap$Sat-(Wevap$Fc-REW))+(Wevap$Fc-REW) # Get water storage (mm) at start of stage 2 evaporation
      Wlower <- Wevap$Dry                                         # Get water storage (mm) when there is no evaporation
      Wrel   <- (Wevap$Act-Wlower)/(Wupper-Wlower)                # Get relative depletion of evaporation storage in stage 2
      if(EvapZmax > EvapZmin)                                     # Check if need to expand evaporation layer
      {
        Wcheck <- fWrelExp*((EvapZmax-EvapZ)/(EvapZmax-EvapZmin))
        while( (Wrel < Wcheck) & (EvapZ < EvapZmax) )
        {
          EvapZ  <- EvapZ+0.001                                    # Expand evaporation layer by 1 mm
          Wevap  <- calculate_EvapLayerWaterContent(soil,EvapZ,ST) # Update water storage (mm) in evaporation layer
          Wupper <- Wstage2*(Wevap$Sat-(Wevap$Fc-REW))+(Wevap$Fc-REW)
          Wlower <- Wevap$Dry
          Wrel   <- (Wevap$Act-Wlower)/(Wupper-Wlower)             # Update relative depletion of evaporation storage
          Wcheck <- fWrelExp*((EvapZmax-EvapZ)/(EvapZmax-EvapZmin))
        }
      }
      
      Kr <- (exp(fevap*Wrel)-1)/(exp(fevap)-1)                     # Get stage 2 evaporation reduction coefficient
      if(Kr > 1)
        Kr <- 1
      
      ToExtractStg2 <- Kr*Edt                                      # Get water to extract (mm)
      
      comp_sto <- which(cumsum(soil$soil.comp$del.z)<EvapZ)[1] + 1    # Extract water from compartments
      comp     <- 0     
      while((ToExtractStg2 > 0) & (comp < comp_sto))
      {
        comp   <- comp+1                            # Increment compartment counter
        layeri <- soil$soil.comp$layer[comp]        # Specify layer number
        #Determine proportion of compartment in evaporation layer
        if( sum(soil$soil.comp$del.z[1:comp]) > EvapZ )
          factor <- 1 - ( ( sum(soil$soil.comp$del.z[1:comp]) - EvapZmin ) / soil$soil.comp$del.z[comp] )
        else
          factor <- 1
                  
        
        Wdry <- 1000*(0.5*soil$soil.layer$WP[layeri])*soil$soil.comp$del.z[comp] #Water storage at dry air (mm)
        W    <- 1000*ST[comp]*soil$soil.comp$del.z[comp]                         #Available water (mm)
        AvW  <- (W-Wdry)*factor                                                  #water available for extraction (mm)
        if(AvW >= ToExtractStg2)
        {
          EsAct <- EsAct+ToExtractStg2            # Update actual evaporation
          W     <- W-ToExtractStg2                # Update depth of water in current compartment
          ToExtract <- ToExtract-ToExtractStg2    # Update total water to be extracted
          ToExtractStg2 <- 0                      # Update water to be extracted from surface layer (stage 1)
        }
        else
        {
          EsAct <- EsAct+AvW                 # Update actual evaporation
          W     <- W-AvW                     # Update depth of water in current compartment
          ToExtractStg2 <- ToExtractStg2-AvW # Update water to be extracted from surface layer (stage 1)
          ToExtract     <- ToExtract-AvW     # Update total water to be extracted
        }
        
        ST[comp] <- W/(1000*soil$soil.comp$del.z[comp]) # Update total water to be extracted
      } 
    }
  }
  
  
  return(list(ST    = ST,
              EsAct = EsAct,
              EsPot = EsPot,
              surface.Storage = surface.Storage,
              Wsurf = Wsurf,
              EvapZ = EvapZ,
              Stage2=Stage2,
              Wstage2=Wstage2))
}


















  