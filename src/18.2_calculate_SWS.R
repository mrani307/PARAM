#' Calculate the soil water stress factor (alpha) for calculating actual root water uptake under moisture limiting condition
#' A LINEAR ROOT WATER UPTAKE MODEL : PRASAD et al., 1986
#'
#' @param ST_l Soil moisture of layer l on i'th day after drainage has occourded (m3/m3)
#' @param RAW_l [GRID PARAMETER] Soil moisture corresponding to Readily available water for layer l (m3/m3)
#' @param WP_l [GRID PARAMETER] Soil moisture corresponding to Wilting Point for layer l (m3/m3)
#'
#' @return soil water stress factor (alpha) for layer l on i'th day (-)
#' @export
#'
#' @examples
calculate_SWS <- function(ST_l,RAW_l,WP_l)
{
  #calculate SOIL WATER STRESS
  
  if (ST_l >= RAW_l)    alpha <- 1
  else if (ST_l > WP_l) alpha <- (ST_l - WP_l) / (RAW_l - WP_l)
  else                  alpha <- 0
  
  return(alpha)
}