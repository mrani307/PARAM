#' Runoff model 
#'
#' @param bunds bunds present in field for that day or not (logical)
#' @param soil soil information (list)
#' @param ST soil water content at the start of rainfall partition process (m3/m3)
#'
#' @return a list containing a)runoff(mm) b)infiltration(mm)
#' @export
#'
#' @examples
rainfall_partition<-function(bunds, soil, ST, P )
{
  
  # Calculate runoff and infiltration
  if (bunds == F) 
  {
    CN <- soil$CN   # CN is not adjusted for field management practices
      
    #Adjusting Curve number to antecedent moisture
    {
      # Calculating upper(AMC I) and lower(AMC II) bounds for CN
      CNbot <- round(1.4 * (exp(-14 * log(10))) + (0.507 * CN) - (0.00374 * CN^2) + (0.0000867 * CN^3))
      CNtop <- round(5.6 * (exp(-14 * log(10))) + (2.33 * CN) - (0.0209 * CN^2) + (0.000076 * CN^3))
      
      # Determine which soil compartment covers the top layer for CN adjustment
      comp_sto <- min(which( cumsum(soil$soil.comp$del.z) >= soil$zCN))
      
      # Calculate weighting factors for soil compartments
      xx <- 0
      wrel <- rep(0, comp_sto)
      for (comp in 1:comp_sto) 
      {
        wx <- 1.016 * (1 - exp(-4.16 * ( cumsum(soil$soil.comp$del.z)[comp] / soil$zCN )))
        wrel[comp] <- wx - xx
        if (wrel[comp] < 0) wrel[comp] <- 0
        if (wrel[comp] > 1) wrel[comp] <- 1
        xx <- wx
      }
      
      # Calculate relative wetness of top soil
      wet_top <- 0
      for (comp in 1:comp_sto) 
      {
        l  <- soil$soil.comp$layer[comp]
        th <- max( soil$soil.layer$WP[l], ST[comp] )
        wet_top <- wet_top + (wrel[comp] * (( th - soil$soil.layer$WP[l]) / 
                                            ( soil$soil.layer$FC[l] - soil$soil.layer$WP[l] ) ))
      }
      
      wet_top <- min( max(wet_top,0), 1)
      
      # Adjust curve number
      CN <- round( CNbot + (CNtop - CNbot) * wet_top )
    }
    
    # Calculate runoff and infiltration (mm)
    S    <- (25400 / CN) - 254
    term <- P - (5 / 100) * S
    
    if (term <= 0) 
    {
      R     <- 0
      infil <- P
    } 
    else 
    {
      R     <- (term^2) / (P + (1 - (5 / 100)) * S)
      infil <- P - R
    }
  } 
  else 
  {
    # No runoff due to bunds
    R <- 0
    infil <- P
  }
  
  # Return the results as a list
  return( list(Runoff = R, 
               infil = infil) )
}
