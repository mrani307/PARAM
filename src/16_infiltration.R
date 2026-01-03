#' Infiltration module
#'
#' @param irrigation resultant irrigation after adjusting for application efficiency for particular day (in mm)
#' @param infil precipitation minus runoff of a particular day (in mm)
#' @param bunds bunds present in field for that day or not (logical)
#' @param surface.Storage standing surface storage from previous day (mm)
#' @param bund.height height of bund (in m)
#' @param Flux.out water flux leaving each soil compartment after drainage (mm)
#' @param ST.o soil water content at the start of infiltration process (m3/m3)
#' @param soil soil information (list)
#'
#' @return a list containing the a) updated water content b) runoff c) deep percolation due to infiltration process
#' @export
#'
#' @examples
infiltration<-function(irrigation, 
                       infil, 
                       bunds, 
                       surface.Storage, 
                       bund.height, 
                       Flux.out, 
                       ST.o,
                       soil)
{
  #' ToStore    amount of water infiltrating into the soil (mm)
  #' R.Ini      runoff while trying to enter the topmost soil layer (mm)
  #' comp       counter variable for soil compartment(-)
  #' R          total runoff during the infiltration process (not rainfall partitioning) (mm) 
  #' no.of.comp number of compartments of the soil layer (-)
  #' l          layer of the soil (-)
  #' dSTdt.S    drainage ability of soil when soil water is at saturation (m3/m3 day-1)
  #' drg.fac    drainage factor (-)
  #' dSTdt.O    drainage ability required to drain 'ToStore' from any soil compartment (m3/m3 day-1)
  #' ST.x       water content required to meet drainage ability dSTdt.O (m3/m3)
  #' D          drainage from compartment (mm)
  #' D.max      maximum drainage from compartment (mm)
  #' diff       difference between threshold and current water contents (mm)
  #' ST.i       soil water content at the end of the infiltration process (m3/m3)
  #' excess     excess water which is redistributed to above compartments (mm)
  #' precomp    counter for compartment in soil layer, used in distribution of excess water (-)
  #' DeepPerc   deep percolation due to infiltration (mm)
  
  no.of.comp <- length( soil$soil.comp$layer )
  ST.i       <- ST.o
 
  #Update infiltration rate for irrigation 
  #Note: irrigation amount adjusted for specified application efficiency
  if( irrigation > 0)
    infil <- infil + irrigation
  
  
  # Determine surface storage (if bunds are present) 
  if(bunds == T)
  {
    #total infiltation is infiltration of the present day and the standing storage from previous day
    infil.total <- infil + surface.Storage 
  
    if( infil.total > 0)
    {
      #Update surface storage and infiltration storage  
      if( infil.total >  soil$soil.layer$SSKS[1] )
      {
        # Infiltration limited by saturated hydraulic conductivity
        # of surface soil layer
        ToStore <- soil$soil.layer$SSKS[1] 
        
        # Additional water ponds on surface
        surface.Storage <- infil.total - soil$soil.layer$SSKS[1]
      }
      else
      {
        # All water infiltrates
        ToStore <- infil.total
        
        # Reset surface storage depth to zero
        surface.Storage <- 0
      }
      
      # Calculate additional runoff
      if( surface.Storage > ( bund.height * 1000) ) 
      {
        # Water overtops bunds and runs off
        R.Ini <- surface.Storage - ( bund.height * 1000 )
        
        # Surface storage equal to bund height
        surface.Storage <- ( bund.height * 1000 )
      }
      else
      {
        # No overtopping of bunds
        R.Ini <- 0
      }
    }
    else 
    {
      # No storage or runoff
      ToStore <- 0
      R.Ini   <- 0
    }
  }
  else
  {
    # No bunds on field
    if( infil > soil$soil.layer$SSKS[1] )
    {
      # Infiltration limited by saturated hydraulic conductivity of top
      # soil layer
      ToStore <- soil$soil.layer$SSKS[1] 
      
      # Additional water runs off
      R.Ini <- infil - soil$soil.layer$SSKS[1]
    }
    else
    {
      # All water infiltrates
      ToStore <- infil
      R.Ini   <- 0
    }
    
    # Add any water remaining behind bunds to surface runoff (needed for
    # days when bunds are removed to maintain water balance)
    R.Ini <- R.Ini + surface.Storage
    
    # Update surface storage
    surface.Storage <- 0
  }
  
  
  #Initialise counters 
  comp <- 0
  R    <- 0
  
  #Infiltrate incoming water 
  if( ToStore > 0 )
  {
    while( ( ToStore > 0 ) & ( comp < no.of.comp  ) )
    {
      # Update compartment counter
      comp <- comp + 1
      
      # Get soil layer
      l <- soil$soil.comp$layer[comp]
      
      # Calculate saturated drainage ability
      dSTdt.S <- soil$soil.layer$tau[l] * ( soil$soil.layer$SAT[l] - soil$soil.layer$FC[l]  )
        
      # Calculate drainage factor
      drg.fac <- soil$soil.layer$SSKS[l]/( dSTdt.S * 1000 * soil$soil.comp$del.z[comp] )
      
      # Calculate drainage ability required
      dSTdt.O <- ToStore/( 1000 * soil$soil.comp$del.z[comp] )
      
      # Check drainage ability and calculate STx
      if( dSTdt.O < dSTdt.S )
      {
        # Calculate water content, ST.x, needed to meet drainage dSTdt.O
        {
          if( dSTdt.O <= 0 )
          ST.x <- soil$soil.layer$FC[l]
          else
          {
            
            A <- 1 + ( ( dSTdt.O * ( exp(soil$soil.layer$SAT[l]-soil$soil.layer$FC[l]) - 1 ) )/
                     ( soil$soil.layer$tau[l] * (soil$soil.layer$SAT[l]-soil$soil.layer$FC[l]) ) )
            ST.x <- soil$soil.layer$FC[l] + log(A)
          }
        }
        #limiting ST.x between saturation and field capacity
        {
          if(ST.x > soil$soil.layer$SAT[l])
            ST.x <- soil$soil.layer$SAT[l]
          else if( ST.x < soil$soil.layer$FC[l])
          {
            ST.x    <- soil$soil.layer$FC[l]
            dSTdt.O <-0
          }
        }
      }
      else
      {
        # Limit water content and drainage to saturation
        ST.x   <- soil$soil.layer$SAT[l]
        dSTdt.O <- dSTdt.S
      }
  
      # Calculate maximum water flow through compartment comp
      D.max <- drg.fac * dSTdt.O * 1000 * soil$soil.comp$del.z[comp]
      
      # Calculate total drainage from compartment comp
      D <- D.max + Flux.out[comp]
      
      # Limit maximum drainage to saturated hydraulic conductivity
      if( D > soil$soil.layer$SSKS[l] )
        D.max <- soil$soil.layer$SSKS[l] - Flux.out[comp]
      
      # Calculate difference between threshold and current water contents
      diff <- ST.x - ST.o[comp] #CHANGE DONE HERE
      
      if( diff > 0 )
      {
        # Increase water content of compartment ii
        ST.i[comp] <- ST.o[comp] + ( ToStore/( 1000 * soil$soil.comp$del.z[comp] ) ) #CHANGE DONE HERE
        
        if( ST.i[comp] > ST.x )
        {
          # Water remaining that can infiltrate to compartments below
          ToStore    <- ( ST.i[comp] - ST.x ) * 1000 * soil$soil.comp$del.z[comp]
          ST.i[comp] <- ST.x
        }
        else
        {
          # All infiltrating water has been stored in this compartment
          ToStore = 0
        }
      }
      
      # Update outflow from current compartment (drainage + infiltration
      # flows)
      Flux.out[comp] <- Flux.out[comp] + ToStore
      
      # Calculate back-up of water into compartments above
      excess <- ToStore - D.max
      
      if( excess < 0 )
        excess <- 0
      
      # Update water to store
      ToStore <- ToStore - excess
      
      # Redistribute excess to compartments above
      if( excess > 0 )
      {
        precomp <- comp + 1
        
        while( (excess>0) & (precomp!=1) )
        {
          # Keep storing in compartments above until soil surface is reached
          # Update compartment counter
          precomp <- precomp-1
          
          # Update layer number
          l <- soil$soil.comp$layer[precomp]
          
          # Update outflow from compartment
          Flux.out[precomp] <- Flux.out[precomp] - excess
          
          # Update water content
          ST.i[precomp] <- ST.i[precomp] + ( excess/( soil$soil.comp$del.z[precomp] * 1000 ) )
          
          # Limit water content to saturation
          if( ST.i[precomp] > soil$soil.layer$SAT[l] )
          {
            # Update excess to store
            excess <- ( ST.i[precomp] - soil$soil.layer$SAT[l]) * 1000 * soil$soil.comp$del.z[precomp]
            
            # Set water content to saturation
            ST.i[precomp] <- soil$soil.layer$SAT[l]
          }
          else
          {
            # All excess stored
            excess <- 0
          }
        }
        if( excess > 0 )
        {
          # Any leftover water not stored becomes runoff
          R <- R + excess
        }
      }
    }
    
    # Infiltration left to store after bottom compartment becomes deep
    # percolation (mm)
    DeepPerc <- ToStore
  }
  else
  {
    #No infiltration
    DeepPerc <- 0
    R        <-0
  }
  
  #Update total runoff %%
  R <- R + R.Ini
  
  # Update surface storage (if bunds are present) %%
  if( R > R.Ini )
  {
    if(bunds)
    {
      # Increase surface storage
      surface.Storage = surface.Storage + ( R - R.Ini )
      
      # Limit surface storage to bund height
      if( surface.Storage > ( bund.height * 1000 ) )
      {
        # Additional water above top of bunds becomes runoff
        R <- R.Ini + ( surface.Storage - ( bund.height * 1000 ) )
         
        # Set surface storage to bund height
        surface.Storage <-  bund.height * 1000 
      }
      else
      {
        # No additional over topping of bunds
        R <- R.Ini
      }
    }  
  }
  
  return( list( ST.i     = ST.i,
                DeepPerc = DeepPerc,
                Runoff   = R,
                surface.Storage = surface.Storage))
}  
  
  
  
  
  
  
