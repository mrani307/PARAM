#' Potential Root Water Uptake from a given soil layer of a particular day using linear root water uptake model
#' A LINEAR ROOT WATER UPTAKE MODEL : PRASAD et al., 1986
#'
#' @param EP.i Potential Water Use (not potential evapotranspiration) on i'th day (in mm)
#' @param RD.i Root depth of the crop on i'th day (in m). Refer to [func(calculate_RD)]
#' @param lw.comp_depth upper depth of a given soil layer l (in m)
#' @param up.comp_depth upper depth of a given soil layer l (in m)
#'
#' @return Root water uptake (RWU) from layer l on i'th day (in mm)
#' @export
#'
#' @examples
RWU_model <- function( EP.i,
                       RD.i,
                       lw.comp_depth,
                       up.comp_depth
                     )
{
  
  #LINEAR ROOT WATER UPTAKE MODEL
  
    RWU <- 2 * (EP.i/RD.i) * ( (lw.comp_depth-up.comp_depth) - ((lw.comp_depth^2-up.comp_depth^2) / (2*RD.i)) )
    return(RWU)
}


