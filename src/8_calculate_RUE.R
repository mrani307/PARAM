#' Calculation of Radiation Use Efficiency Factor (RUE) for converting energy into biomass (kg ha-1)/(MJ m-2)
#'
#' @param CO2 daily atmospheric CO2 level time series for the whole time period (in ppm)
#' @param WAC2 [CROP PARAMETER] 
#' @param WA [CROP PARAMETER] biomass energy ratio , when CO2 = 330ppm
#'
#' @return Daily Radiation Use Efficiency Factor (RUE) time series for the whole time period (kg ha-1)/(MJ m-2)
#' @export
#'
#' @examples
  calculate_RUE<-function( WAC2, WA, CO2 )
  {
    #parameters such that CO2@330ppm RUE=WA and CO2@660ppm RUE=WAX
    bc1 <-25.82621
    bc2 <-0.03975867
    
    WAX <- 100 * (WAC2%%1)
    RUE <- (WAX-WA)* CO2/(CO2+exp(bc1-bc2*CO2)) + WA
    
    return(RUE)
  }