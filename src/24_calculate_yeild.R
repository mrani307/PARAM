#' calculates crop yeild
#'
#' @param SWH simulated water use during the fraction of growing season
#' @param HUI.i HUI on the day of harvest
#' @param HI [CROP PARAMETER] optimal harvest index
#' @param WSYF [CROP PARAMETER] minimal harvest index
#' @param STL.i standing live biomass on the day of harvest (above ground biomass in t ha-1)
#'
#' @return yeild (amount of the crop removed from the field) in t ha-1
#' @export
#'
#' @examples
calculate_yeild<-function( SWH, HUI.i, HI, WSYF, STL.i)
{
  f    <- SWH/( SWH + exp(5.563-0.0315*SWH) )
  
  X2   <- 100 * HUI.i
  
  AJHI <- HI * X2 / ( X2 + exp(11.11-0.1*X2) )

  HIA  <- f * ( AJHI-WSYF) + WSYF
  
  YLD  <- HIA * STL.i
  
  return(YLD)
}