#' Calculation of X1 time series to calculation of daily potential increase in biomass (-)
#' APEX CROP MODEL 
#'
#' @param TMP daily temperature for the whole time series (in degree Celsius)
#' @param RH daily relative humidity for the whole time series ( in %)
#'
#' @return daily X1 time series for whole time period (-)
#' @export
#'
#' @examples
  calculate_X1<-function(TMP,TDEW)
  {
    TAVG  <- TMP  #Average daily temperature (in Celcius)
    
    SAT.VP<-0.6108*exp(17.27*TAVG/(TAVG+237.3)) #Daily Saturation Vapor Pressure (in kPA)
    
    ACT.VP<-0.6108*exp(17.27*TDEW/(TDEW+237.3))  #Daily Actual Vapor Pressure (in kPA)
    
    VPD   <- SAT.VP - ACT.VP #daily Vapor Pressure Deficit (in kPA)
  
    VPD.1 <- VPD-1
    
    X1    <- unlist(lapply(VPD.1, function(vpd.1) max(vpd.1,-0.5)))
    
    return(X1)
  }
  
  
  