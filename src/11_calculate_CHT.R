
#' calculate crop height for all the seasons within the whole time period (m)
#'
#' @param HUF  Heat Unit Factor (HUF)
#' @param HMX [CROP PARAMETER] maximum height of crop (m)
#'
#' @return
#' @export
#'
#' @examples
calculate_CHT<-function(HUF,HMX)
{
  CHT <- sqrt(HUF)*HMX
  return(CHT)
}