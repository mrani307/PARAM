#' calculates the root weight growth 
#'
#' @param DM.i total biomass of crop on given day (t ha-1)
#' @param HUI.i heat unit index of the given day (-)
#' @param ar1 [CROP PARAMETER]
#' @param ar2 [CROP PARAMETER]
#' @param RWT.o previous day root weight of the crop for each layer of soil (t ha-1)
#' @param UW water uptake from each layer of soil (mm)
#'
#' @return
#' @export
#'
#' @examples
calculate_root_growth<-function( DM.i , HUI.i, ar1 , ar2, UW, RWT.o  )
{
  
  RW.o  <- sum(RWT.o)
  
  RW.i  <- DM.i * ( ar1*(1-HUI.i) + ar2*HUI.i )
  
  DRW   <- RW.i - RW.o
  
  UTO   <- UW/sum(UW)
  
  RWT.i <- RWT.o + DRW*UTO
  
  return(RWT.i)
}
  
