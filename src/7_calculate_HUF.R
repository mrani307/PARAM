#' Calculates the daily Heat Unit Factor (HUF) for all the seasons within the whole time period (-).
#'
#' @param HUI daily Heat Unit Index (HUI) time series for the whole time period (-) . Calculated using [func(calculate_HUI)].
#' @param ah1 [CROP PARAMETER] first point of optimal leaf area development curve
#' @param ah2 [CROP PARAMETER] second point of optimal leaf area development curve
#'
#' @return Daily Heat Unit Factor (-) for all the seasons withing the whole time period 
#' @export
#'
#' @examples
  calculate_HUF<-function(HUI, ah1, ah2)
  {
    HUF <- numeric(length = length(HUI)) # stores the daily HUI during each cropping period throughout the whole time series 
    
    HUF <- (HUI)/(HUI+exp(ah1-ah2*HUI))
    
    return(HUF)
  }
