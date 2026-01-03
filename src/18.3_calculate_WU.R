#' Calculates the actual soil water uptake from each layer of the soil on i'th day (mm)
#' A LINEAR ROOT WATER UPTAKE MODEL : PRASAD et al., 1986
#'
#' @param EP.i Potential Water Use (not potential evapo-transpiration) on i'th day (in mm)
#' @param RD.i Root depth of the crop on i'th day (in m). Refer to [func(calculate_RD)]
#' @param ST Soil Moisture content of each layer of soil after drainage on i'th day (in m3/m3)
#' @param p [CROP PARAMETER] allowable depletion factor
#' @param FC [GRID PARAMETER] Soil moisture corresponding to Field Capacity for layer l (m3/m3)
#' @param WP [GRID PARAMETER] Soil moisture corresponding to Wilting Point  for layer l (m3/m3)
#' @param del.z depth of each layer of soil (in m)
#' @param no.of.comp number of layers of soil (-)
#'
#' @return Actual soil water uptake from each layer of soil on i'th day (mm)
#' @export
#'
#' @examples
calculate_WU<-function( EP.i, RD.i, ST, p, soil, no.of.comp)
{
  
  #to store the outputs 
  
  Smax     <- numeric( length = no.of.comp )
  S        <- numeric( length = no.of.comp )
  
  #layer up-to which root is present of the given day
  
  RD.comp  <- findInterval( RD.i, c( 0 ,cumsum(soil$soil.comp$del.z) ))      
   
  # soil water uptake 
  
  for( comp in 1:RD.comp )
  {
    #soil layer corresponding to the soil compartment
    
    l <- soil$soil.comp$layer[comp]
      
    #selecting upper depth and lower depth of layer l
    
    { 
      #upper depth of the sil compartment
      if( comp == 1 ) 
        up.comp_depth <- 0        
      else     
        up.comp_depth <- cumsum( soil$soil.comp$del.z[comp-1] )  
      
      #lower depth of the layer
      lw.comp_depth <- cumsum( soil$soil.comp$del.z[comp] )      
    }
        
    #calculating potential soil water uptake by the root at a given soil compartment comp (mm)       
    
    {
      if( RD.comp >  comp )  Smax[comp] <- RWU_model( EP.i, RD.i, lw.comp_depth,up.comp_depth)
      if( RD.comp == comp )  Smax[comp] <- EP.i - RWU_model( EP.i, RD.i, lw.comp_depth = up.comp_depth ,up.comp_depth=0) 
      if( RD.comp <  comp )  Smax[comp] <- 0
    }
    
    #calculating soil water stress factor for the given soil compartment
    
    RAW   <- soil$soil.layer$WP[l] + p * ( soil$soil.layer$FC[l] - soil$soil.layer$WP[l] )  
    
    alpha <- calculate_SWS ( ST_l  = ST[comp], 
                             RAW_l = RAW , 
                             WP_l  = soil$soil.layer$WP[l] ) 
      
    #calculating actual soil water uptake (mm)
    
    S[comp]  <- alpha * Smax[comp]
    
  } 
  
  return(S)
}
