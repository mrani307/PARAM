#' Calculation of daily Root Depth for each cropping season for the whole time period (in m)
#' EPIC CROP MODEL : Williams et al., 1989
#'
#' @param HUI daily Heat Unit Index (HUI) time series for the whole time period (in degree Celsius) . Calculated using [func(calculate_HUI)].
#' @param RDMX [CROP PARAMETER] maximum crop depth of a specific crop (in m)
#'
#' @return time series of Root Depth of crop for each cropping season for the whole time period (in m)
#' @export
#'
#' @examples
calculate_RD<-function( HUI, RDMX)
{
  RD <- data.frame(rd = 2.5*RDMX*HUI,
                   RDMX)
  RD$RD <- apply( RD, 1, function(x) min(x[1],x[2]))
  
  return(RD$RD)
}
