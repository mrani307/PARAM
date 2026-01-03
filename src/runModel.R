runModel<-function(crop.info,
                   grid.crop,
                   crop.param,
                   P.grid,
                   Temp.grid,
                   RA.grid,
                   WS.grid,
                   DPT.grid,
                   CO2.grid,
                   ETo.grid,
                   PET.grid,
                   soil,
                   start.year,
                   end.year,
                   no.of.days,
                   z,
                   lat,
                   doY,
                   bund.height,
                   irr.grid,
                   start_of.season,
                   end_of.season,
                   Irr.AppEff.irrGrid,
                   Irr.AppEff.nonIrrGrid)
{

  no.of.comp <- length(soil$soil.comp$layer)
  
  #FOR STORING OUTPUTS-------------------------------------------------------- 
  soil.water <-matrix(nrow = no.of.days, ncol = no.of.comp) #defining a matrix to store the daily soil water information of the grid (m3/m3)
  irrigation <- numeric(length = no.of.days) #irrigation provided (mm)
  surface.Storage <- numeric(length = no.of.days) #surface storage(mm)
  Transpiration   <- numeric(length = no.of.days) #Transpiration from crop (mm)
  EsAct           <- numeric(length = no.of.days) #soil evaporation (mm)
  DP   <- numeric(length = no.of.days) #deep percolation (mm)
  R    <- numeric(length = no.of.days)  #runoff (mm)
  LAI  <- numeric(length = no.of.days) #leaf area index (-)
  DM   <- numeric(length = no.of.days) # crop biomass (t ha-1)
  RWT  <- matrix(0,nrow = no.of.days, ncol = no.of.comp) #crop root weight (t ha-1)
  B.ag <- numeric(length = no.of.days) #above ground biomass (t ha-1)
  YLD  <- numeric(length = length(start.year:end.year)) #yeild (t ha-1)
  REG  <- numeric(length = no.of.days) #REG
  
  Sg  <- numeric(length = no.of.days) # green content of soil water (mm)
  Sb  <- numeric(length = no.of.days) # green content of soil water (mm)
  ETb <- numeric(length = no.of.days) # blue ET (mm)
  ETg <- numeric(length = no.of.days) # green ET (mm)
  
  #initialization-----------------------------------------------------------------
  #assuming the soil water content for all compartments on the beginning of the 1st day at FC  
  #ST : present soil moisture
  ST <- vapply( 1:no.of.comp, function(comp) soil$soil.layer$FC[soil$soil.comp$layer[comp]],numeric(1))
  
  Sg[1] <- calculate_rootZoneWater(1.99,ST,soil)$thRZ.Act*1000 
  Sb[1] <- 0
  Sbg   <- Sg[1]+Sb[1]
  
  #MODEL--------------------------------------------------------------------------
  
  #running the model for the entire time period
  for(d in 1:no.of.days)
  {
    
    #extracting the crop season for that particular day
    crop.season <- crop.info$season[d] 
    
    {
      if(!is.na(crop.season))
      {
        cropName <- crop.info$crop_name[d]
      }
    }
    
    #CHECK BUNDS
    check.bunds    <- F
    if( !is.na(crop.season) )
    {
      #set bunds as T if crop is rice during the growing season
      if( cropName == "RICE" )
      {
        #bunds set for Rice crop
        check.bunds <- T
      }
    }
    
    #DRAINAGE
    res.update_drainage <- update_drainage(ST.o = ST,soil = soil)
    Flux.out <- res.update_drainage$Flux.out 
    ST       <- res.update_drainage$ST.i   
    DP[d]    <- res.update_drainage$DeepPerc
    rm(res.update_drainage)
    
    #RAINFALL PARTITION
    res.rainfall_partition <- rainfall_partition( bunds = check.bunds,
                                                  soil  = soil, 
                                                  ST    = ST,
                                                  P     = P.grid[d] )
    infil <- res.rainfall_partition$infil
    R[d]  <- res.rainfall_partition$Runoff
    rm(res.rainfall_partition)
    
    {
      if(d==1) 
      {
        surf.Storage <- 0
        EPot         <- 0
        TPot         <- 0
        
        Wsurf    <- 0
        EvapZ    <- 0
        Stage2   <- T
        Wstage2  <- 0
        
      }
      else     
        surf.Storage <- surface.Storage[d-1]
    }
    
    #IRRIGATION
    if( !is.na(crop.season) )
    {
      if(irr.grid==(-1))  #for non-irrigated croplands
        irrigation[d]<-0
      else                #for cancal-irrigated and non-canal irrigated cropland
      {
        irrigation[d] <- calculate_irrigation( rootdepth = crop.info$RD[d], 
                                               ST        = ST, 
                                               soil      = soil, 
                                               EPot      = EPot ,
                                               TPot      = TPot,
                                               Rain      = P.grid[d], 
                                               Runoff    = R[d], 
                                               cropName  = cropName,
                                               crop.param = crop.param,
                                               irr.grid  = irr.grid, #check here
                                               Irr.AppEff.irrGrid, 
                                               Irr.AppEff.nonIrrGrid )
        
        if(cropName == "RICE" & surf.Storage<50 & irrigation[d]>0) #limiting surface storage to 5 cm
        {
          irrigation[d]<-irrigation[d]+(50-surf.Storage) 
        }
      }
    }
    
    #INFILTRATION
    res.infiltration <- infiltration( irrigation      = irrigation[d],
                                      infil           = infil, 
                                      bunds           = check.bunds, 
                                      surface.Storage = surf.Storage, 
                                      bund.height     = bund.height,
                                      Flux.out        = Flux.out,
                                      ST.o            = ST,
                                      soil            = soil)
    ST    <- res.infiltration$ST.i 
    R[d]  <- R[d] + res.infiltration$Runoff 
    DP[d] <- DP[d] + res.infiltration$DeepPerc
    surface.Storage[d] <- res.infiltration$surface.Storage #get output
    
    #CROP GROWTH MODEL----------------------------------------------------------
    if(!is.na(crop.season)) 
    {
      
      {
        if( d == start_of.season[crop.season] )
        {
          DM[d-1]  <- 0.002
          LAI[d-1] <- 0.1
          crop.info$HUF[d-1] <- 1e-15
        }
        
      }
      
      #ROOT GROWTH STRESS FACTOR (RGF)
      RGF <- calculate_RGF(no.of.comp,soil )
      
      #ROOT WATER UPTAKE (TRANSPIRATION)
      
      CC    <- DM[d-1] / (DM[d-1] + exp(1.175-1.748*DM[d-1]))
      CCadj <- 1.72*CC - CC^2 + 0.30*CC^3
      TPot  <- PET.grid[d]*CCadj
      
      #root water uptake
      S <- calculate_WU( EP.i = TPot ,
                         RD.i = crop.info$RD[d] ,
                         ST   = ST , 
                         p    = crop.param[[cropName]][["p"]] , 
                         soil = soil, 
                         no.of.comp = no.of.comp) 
      S  <- S * RGF
      
      #updating the soil moisture content due to removal of water by root
      ST <- ST - ( S/( 1000 * soil$soil.comp$del.z ) )
      
      #transpiration on present day
      Transpiration[d] <- sum(S)        
      
      #soil evaporation
      res.evap <- calculate_soilEvaporation(P          = P.grid[d],         
                                            irrigation = irrigation[d],
                                            infil      = infil,
                                            d          = d,    
                                            doS        = crop.info$doS[d],  
                                            crop.season= crop.season,
                                            CCadj      = CCadj,      
                                            ETo        = ETo.grid[d],      
                                            surface.Storage = surface.Storage[d],
                                            soil       = soil,
                                            ST         = ST,
                                            Wsurf      = Wsurf,
                                            EvapZ      = EvapZ,
                                            Stage2     = Stage2,
                                            Wstage2    = Wstage2)
      
      ST       <- res.evap$ST
      EsAct[d] <- res.evap$EsAct
      EPot     <- res.evap$EsPot
      Wsurf    <- res.evap$Wsurf
      EvapZ    <- res.evap$EvapZ
      Stage2   <- res.evap$Stage2
      Wstage2  <- res.evap$Wstage2
      surface.Storage[d] <- res.evap$surface.Storage
      
      #CROP GROWTH REGULATION FACTOR ( REG,stress parameter )
      REG[d] <- calculate_REG( AEP.i = Transpiration[d],
                               EP.i  = TPot,
                               TMP.i = Temp.grid[d],
                               TBSC  = crop.param[[cropName]][["TBS"]],
                               TOPC  = crop.param[[cropName]][["TOP"]],
                               ST.i  = ST,
                               soil  = soil,
                               CAF   = crop.param[[cropName]][["CAF"]])
      
      #LEAF AREA INDEX
      XLAI   <- crop.param[[cropName]][["DMLA"]][crop.info$year[d]-start.year+1]
      LAI[d] <- calculate_LAI(  DLAI      = crop.param[[cropName]][["DLAI"]],
                                HUF.o     = crop.info$HUF[d-1], 
                                HUF.i     = crop.info$HUF[d], 
                                LAI.o     = LAI[d-1], 
                                XLAI      = XLAI,  #crop.param[[cropName]][["DMLA"]], 
                                REG.i     = REG[d],
                                HUI.i     = crop.info$HUI[d],
                                ad        = crop.param[[cropName]][["RLAD"]])
      
      #CROP BIOMASS
      #calculating the potential increase in biomass of crop on present day
      DDM <- calculate_DDM( RA.i  = RA.grid[d],
                            LAI.i = LAI[d],
                            RUE.i = crop.info$RUE[d],
                            X1.i  = crop.info$X1[d],
                            WAVP  = crop.param[[cropName]][["WAVP"]],
                            EXT   = 0.45)
      #calculating the actual increase in biomass of crop on present day
      DDM     <- DDM * REG[d]
      #total biomass on i'th day
      DM[d]   <- DM[d-1] + DDM   
      #root weight on i'th day
      RWT[d,] <- calculate_root_growth( DM.i  = DM[d] , 
                                        HUI.i = crop.info$HUI[d], 
                                        ar1   = crop.param[[cropName]][["RWPC1"]], 
                                        ar2   = crop.param[[cropName]][["RWPC2"]], 
                                        UW    = S, 
                                        RWT.o = RWT[d-1,] )
      #above ground biomass on i'th day
      B.ag[d] <- DM[d] - sum(RWT[d,])
      
      #CROP YEILD (on the last day of cropping season)
      
      if( d == end_of.season[crop.season] )
      {
        #calculating the simulated water use during the growing season ( HUI>0.5 )
        df <- data.frame( crop.info, Transpiration)
        
        df  <- df[which(df$season==crop.season),]
        SWH <- sum(df[which(df$HUI>0.5),"Transpiration"])
        
        rm(df)
        
        #calculate yield
        HI <- crop.param[[cropName]][["HI"]][crop.info$year[d]-start.year+1]
        YLD[crop.season] <- calculate_yeild( SWH   = SWH,
                                             HUI.i = crop.info$HUI[d],  
                                             HI    = HI, #crop.param[[cropName]][["HI"]], 
                                             WSYF  = crop.param[[cropName]][["WSYF"]], 
                                             STL.i = B.ag[d])
      }
    }
    else #grass grown during the fallow period
    {
      
      #ROOT GROWTH STRESS FACTOR (RGF)
      RGF <- calculate_RGF(no.of.comp,soil )
      
      #ROOT WATER UPTAKE (TRANSPIRATION)
      S <- calculate_WU( EP.i = ETo.grid[d] ,
                         RD.i = 0.15 ,
                         ST   = ST , 
                         p    = 0.5 , 
                         soil = soil, 
                         no.of.comp = no.of.comp) 
      S  <- S * RGF
      
      #updating the soil moisture content due to removal of water by root
      ST <- ST - ( S/( 1000 * soil$soil.comp$del.z ) )
      
      #transpiration on present day
      Transpiration[d] <- sum(S) 
      
      #soil evaporation
      CCadj    <- 0
      res.evap <- calculate_soilEvaporation(P          = P.grid[d],         
                                            irrigation = irrigation[d],
                                            infil      = infil,
                                            d          = d,    
                                            doS        = crop.info$doS[d],  
                                            crop.season= crop.season,
                                            CCadj      = CCadj,      
                                            ETo        = (ETo.grid[d]-sum(S)),      
                                            surface.Storage = surface.Storage[d],
                                            soil       = soil,
                                            ST         = ST,
                                            Wsurf      = Wsurf,
                                            EvapZ      = EvapZ,
                                            Stage2     = Stage2,
                                            Wstage2    = Wstage2)
      
      ST       <- res.evap$ST
      EsAct[d] <- res.evap$EsAct
      EPot     <- res.evap$EsPot
      Wsurf    <- res.evap$Wsurf
      EvapZ    <- res.evap$EvapZ
      Stage2   <- res.evap$Stage2
      Wstage2  <- res.evap$Wstage2
      surface.Storage[d] <- res.evap$surface.Storage
      
    }
    
    #updating soil water content at the end of the day
    soil.water[d,]  <- ST
	
	#blue green water accounting 
    frac.g <- ifelse((P.grid[d]+irrigation[d])==0,0,(P.grid[d]/(irrigation[d]+P.grid[d])))
    frac.b <- ifelse((P.grid[d]+irrigation[d])==0,0,(irrigation[d]/(irrigation[d]+P.grid[d])))
    
  	{
  		if(d==1)
  		{
  			Sg[d]  <- Sg[1] + P.grid[d]     - (DP[d]+Transpiration[d]+EsAct[d])*(Sg[1]/Sbg) - R[d]*frac.g
  			Sb[d]  <- Sb[1] + irrigation[d] - (DP[d]+Transpiration[d]+EsAct[d])*(Sb[1]/Sbg) - R[d]*frac.b
  			
  			ETb[d] <- (Transpiration[d]+EsAct[d])*(Sb[1]/Sbg)
  			ETg[d] <- (Transpiration[d]+EsAct[d])*(Sg[1]/Sbg)
  		}
  		else
  		{
  			Sg[d]  <- Sg[d-1] + P.grid[d]     - (DP[d]+Transpiration[d]+EsAct[d])*(Sg[d-1]/Sbg) - R[d]*frac.g
  			Sb[d]  <- Sb[d-1] + irrigation[d] - (DP[d]+Transpiration[d]+EsAct[d])*(Sb[d-1]/Sbg) - R[d]*frac.b
  			
  			ETb[d] <- (Transpiration[d]+EsAct[d])*(Sb[d-1]/Sbg)
  			ETg[d] <- (Transpiration[d]+EsAct[d])*(Sg[d-1]/Sbg)
  		}			
  	}
    Sbg <- Sg[d] + Sb[d]
  }
  
  #storing results----------------------------------------------------------------
  
  outputs<-data.frame(crop.info[,c(1:5,9,10)],Transpiration,EsAct,irrigation,surface.Storage,ETg,ETb)
  
  Yield <- numeric(length = length(start.year:end.year))
  Yield[which(!is.na(grid.crop))] <- YLD[YLD!=0]
  names(Yield)<-names(grid.crop)

  return(list(output=na.omit(outputs),
         Yield = Yield))
}



