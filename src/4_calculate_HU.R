#' Calculate the daily heat units for the whole time series (in degree Celsius ). 
#' EPIC CROP MODEL : Williams et al., 1989
#'
#' @param TMP daily temperature for the whole time series (in degree Celsius)
#' @param TBSC [ CROP PARAMETER ] crop specific base temperature, that is no growth occurs below this temperature (in degree Celsius)
#'
#' @return daily heat units units for the whole time series (in degree Celsius )
#' @export  
#'
#' @examples
calculate_HU<-function(TMP,TBSC)
{
  HU <- TMP-TBSC
  return(HU)
}
