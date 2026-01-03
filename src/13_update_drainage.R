#' Computed the drainage through the soil compartments and calculates the updated soil moisute content
#'
#' @param ST.o previous day soil water content (m3/m3)  
#' @param soil soil information (list)
#'
#' @return a list of a) updated soil moisture b) deep percolation and c) water flux removed from soil
#'         compartment; due to the drainage process
#' @export
#'
#' @examples
update_drainage<-function( ST.o, soil )
{
  
  #' variables used :
  #' D.sum     cumulative drainage from above compartments (mm)
  #' excess    excess water into a compartment which is distributed to upward compartments(mm)
  #' ST.i      final water content of the compartment (m3/m3)
  #' Flux.out  total amount of water draining from each compartment on a given day (mm)
  #' comp      counter variable used for soil compartments(-)
  #' l         layer to which the soil compartment is a part of (-)
  #' dSTdt     drainage ability of a soil compartment (m3/m3/day)
  #' D         drainage from a soil compartment (mm)
  #' STx       water content required for drainage ability to be equal to cumulative drainage (m3/m3)
  #' checkRes  check for drainage of a compartment to its saturated hydraulic conductivity (list)
  #' precomp   counter variable used for soil compartment, to distribute the excess water (-)
  #' DeepPerc  deep percolation on the given day (mm)
  
  #' functions used : 
  #' calculate_dSTsdt    calculate the drainage ability of a soil compartment
  #' check_drainability  check the drainability of a soil compartment
  #' check_for_D.sum     check for drainage of a compartment to its saturated hydraulic conductivity
  
  no.of.comp <- length(soil$soil.comp$layer)
  D.sum      <- 0
  excess     <- 0
  ST.i       <- numeric( length = no.of.comp )
  Flux.out   <- numeric( length = no.of.comp )
  
  for(comp in 1:no.of.comp)
  {
    l<- soil$soil.comp$layer[comp]
  
    #calculate drainage ability of the compartment 'comp'
    dSTdt <- calculate_dSTsdt( ST   = ST.o[comp] ,
                               FC   = soil$soil.layer$FC[l] ,
                               SAT  = soil$soil.layer$SAT[l] ,
                               tau  = soil$soil.layer$tau[l] )
    
    #drainage from compartment 'comp'
    D <- dSTdt * soil$soil.comp$del.z[comp] * 1000
    
    #check for drainability
    excess       <- 0 
    drainability <- check_drainability( comp , dSTdt , D.sum , soil)
    
    #drain component
    if(drainability)
    {
      #no water is stored in this component
      ST.i[comp] <- ST.o[comp] - dSTdt
      
      #update cumulative drainage
      D.sum <- D.sum + D
      
      #restrict cumulative drainage to saturated hydraulic
      #conductivity and adjust excess drainage flow
      checkRes <- check_for_D.sum( l, D.sum, soil)
      excess   <- excess + checkRes$excess
      D.sum    <- checkRes$D.sum
    }
    else
    {
      #equivalent drainage ability of the above soil layers
      dSTdt <- D.sum / ( 1000 * sum( soil$soil.comp$del.z[1:(comp-1)] ) )
      
      #calculate STx (water content required for drainage ability to be equal to cumulative drainage)
      ST.x <- calculate_STx( l , dSTdt , soil)
      
      #increase in storage of compartment 'comp' due to cumulative drainage
      ST.i[comp] <- ST.o[comp] + D.sum/( 1000 * soil$soil.comp$del.z[comp] )
      
      #check for STx against hydraulic properties of current soil layer
      if( ST.x <= soil$soil.layer$SAT[l] )
      {
        #check updated water content against STx
        if( ST.i[comp] > ST.x )
        {
          # Cumulative drainage is the drainage difference
          # between ST.x and ST.i plus drainage ability
          # at ST.x
          D.sum <- ( ST.i[comp] - ST.x ) * 1000 * soil$soil.comp$del.z[comp]
          
          # Calculate drainage ability for ST.x
          dSTdt <- calculate_dSTsdt( ST   = ST.x ,
                                     FC   = soil$soil.layer$FC[l] ,
                                     SAT  = soil$soil.layer$SAT[l] ,
                                     tau  = soil$soil.layer$tau[l] )
          
          # Update drainage total
          D.sum <- D.sum + ( dSTdt * 1000 * soil$soil.comp$del.z[comp] )
          
          #restrict cumulative drainage to saturated hydraulic
          #conductivity and adjust excess drainage flow
          checkRes <- check_for_D.sum( l, D.sum, soil )
          excess   <- excess + checkRes$excess
          D.sum    <- checkRes$D.sum
          
          # Update water content
          ST.i[comp] <- ST.x - dSTdt
        }
        else if( ST.i[comp] > soil$soil.layer$FC[l] )
        {
          # Calculate drainage ability for updated water content
          dSTdt <- calculate_dSTsdt( ST   = ST.i[comp] ,
                                     FC   = soil$soil.layer$FC[l] ,
                                     SAT  = soil$soil.layer$SAT[l] ,
                                     tau  = soil$soil.layer$tau[l] )
          
          # Update water content in compartment l
          ST.i[comp] <- ST.i[comp] - dSTdt
          
          # Update cumulative drainage
          D.sum <- dSTdt * 1000 * soil$soil.comp$del.z[comp]
          
          # Restrict cumulative drainage to saturated hydraulic
          # conductivity and adjust excess drainage flow
          checkRes <- check_for_D.sum( l, D.sum, soil )
          excess   <- excess + checkRes$excess
          D.sum    <- checkRes$D.sum
          
        }
        else
        {
          # Drainage and cumulative drainage are zero as water
          # content has not risen above field capacity in
          # compartment 'comp'.
          D.sum <- 0 
        }
      }
      else
      {
        # Check new water content against hydraulic properties of soil
        # layer
        if( ST.i[comp] <= soil$soil.layer$SAT[l])
        {
          if( ST.i[comp] > soil$soil.layer$FC[l] ) 
          {
            # Calculate new drainage ability
            dSTdt <- calculate_dSTsdt( ST   = ST.i[comp] ,
                                       FC   = soil$soil.layer$FC[l] ,
                                       SAT  = soil$soil.layer$SAT[l] ,
                                       tau  = soil$soil.layer$tau[l] )
            
            # Update water content in compartment 'comp'
            ST.i[comp] <- ST.i[comp] - dSTdt
            
            # Update cumulative drainage
            D.sum <- dSTdt * 1000 * soil$soil.comp$del.z[comp]
            
            # Restrict cumulative drainage to saturated hydraulic
            # conductivity and adjust excess drainage flow
            checkRes <- check_for_D.sum( l, D.sum, soil )
            excess   <- excess + checkRes$excess
            D.sum    <- checkRes$D.sum
          }
          else
            D.sum <- 0
        }
        else if( ST.i[comp]  > soil$soil.layer$SAT[l] ) 
        {
          # Calculate excess drainage above saturation
          excess <- ( ST.i[comp] - soil$soil.layer$SAT[l] ) * 1000 * soil$soil.comp$del.z[comp]
          
          # Calculate drainage ability for updated water content
          dSTdt <- calculate_dSTsdt( ST   = ST.i[comp] ,#CHECK HERE 
                                     FC   = soil$soil.layer$FC[l] ,
                                     SAT  = soil$soil.layer$SAT[l] ,
                                     tau  = soil$soil.layer$tau[l] )
          
          # Update water content in compartment 'comp'
          ST.i[comp] <- soil$soil.layer$SAT[l] - dSTdt
          
          # Update drainage from compartment 'comp'
          D <- dSTdt * 1000 * soil$soil.comp$del.z[comp]
          
          # Update maximum drainage
          D.max <- dSTdt * 1000 * sum( soil$soil.comp$del.z[1:(comp-1)] )
          
          # Update excess drainage
          if( D.max > excess )        #ask
            D.max <- excess           #ask
          
          excess = excess - D.max     #ask
          
          # Update drainsum and restrict to saturated hydraulic
          # conductivity of soil layer
          D.sum    <- D + D.max
          checkRes <- check_for_D.sum( l, D.sum, soil )
          excess   <- excess + checkRes$excess
          D.sum    <- checkRes$D.sum
        }
      }
    }
      
    #Store output flux from layer i
    Flux.out[comp] <- D.sum
    
    #Redistribute excess in layers above
    if (excess > 0) 
    {
      precomp <- comp + 1
      while ( (excess > 0) & (precomp != 1) ) 
      {
        # Update compartment counter
        precomp <- precomp - 1
        
        # Update layer counter
        l <- soil$soil.comp$layer[precomp]
        
        # Update flux from compartment
        if ( precomp < comp )                                    
          Flux.out[precomp] <- Flux.out[precomp] - excess        
        
        # Increase water content to store excess
        ST.i[precomp] <- ST.i[precomp] + (excess / (1000 * soil$soil.comp$del.z[precomp]))
        
        # Limit water content to saturation and adjust excess counter
        if ( ST.i[precomp] > soil$soil.layer$SAT[l] ) 
        {
          excess        <- ( ST.i[precomp] - soil$soil.layer$SAT[l] ) * 1000 * soil$soil.comp$del.z[precomp]
          ST.i[precomp] <- soil$soil.layer$SAT[l]
        } 
        else 
          excess <- 0
      }
    }
  }
  
  # Update conditions and outputs %%
  # Total deep percolation (mm)
  DeepPerc = D.sum
  
  return(list( ST.i     = ST.i,
               Flux.out = Flux.out,
               DeepPerc = DeepPerc))
  
}

  
    