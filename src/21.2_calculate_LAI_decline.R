#' calculate the LAI of crop on i'th day during the 
#' period of  leaf decline to end of growing season
#'
#' @param LAI.o LAI on previous (i-1th) day (-)
#' @param HUI.i HUI on i'th day (-)
#' @param HUI.d value of HUI when LAI starts declining
#' @param ad [CROP PARAMETER] parameter which governs LAI decline rate
#'
#' @return LAI of crop on i'th day during the period of  leaf decline to end of growing season
#' @export
#'
#' @examples
calculate_LAI.decline<-function(LAI.o, HUI.i, HUI.d, ad)
{
  LAI.i <- LAI.o * ((1-HUI.i)/(1-HUI.d))^ad
  
  return(LAI.i)
}