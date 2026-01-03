calculate_EvapLayerWaterContent<-function(soil,EvapZ,ST)
{
  #Function to get water contents in the evaporation layer
  
  # Determine actual water content (mm) %%
  comp_sto <- which(cumsum(soil$soil.comp$del.z)<EvapZ)[1] + 1  # Find soil compartments covered by evaporation layer
  Wevap    <- list(Sat = 0,Fc  = 0,Wp  = 0,Dry = 0,Act = 0)  # Initialize variables
    
  for(ii in 1:comp_sto)
  {
    layeri <- soil$soil.comp$layer[ii] #Specify layer number
    
    #Determine fraction of soil compartment covered by evaporation layer
    if( cumsum(soil$soil.comp$del.z[1:ii])[ii] > EvapZ )
      factor <- 1- ( (cumsum(soil$soil.comp$del.z[1:ii])[ii]-EvapZ)/soil$soil.comp$del.z[ii] )
    else
      factor <- 1
    
    # Actual water storage in evaporation layer (mm)
    Wevap$Act <- Wevap$Act +(factor*1000*ST[ii]*soil$soil.comp$del.z[ii])
    # Water storage in evaporation layer at saturation (mm)
    Wevap$Sat <- Wevap$Sat +(factor*1000*soil$soil.layer$SAT[layeri]*soil$soil.comp$del.z[ii])
    # Water storage in evaporation layer at field capacity (mm)
    Wevap$Fc  <- Wevap$Fc  +(factor*1000*soil$soil.layer$FC[layeri]*soil$soil.comp$del.z[ii])
    # Water storage in evaporation layer at permanent wilting point (mm)
    Wevap$Wp  <- Wevap$Wp  +(factor*1000*soil$soil.layer$WP[layeri]*soil$soil.comp$del.z[ii])
    # Water storage in evaporation layer at air dry (mm)
    Wevap$Dry <- Wevap$Dry +(factor*1000*0.5*soil$soil.layer$WP[layeri]*soil$soil.comp$del.z[ii])    
  }
  
  if(Wevap$Act < 0)
    Wevap$Act <- 0
  
  return(Wevap)
}
