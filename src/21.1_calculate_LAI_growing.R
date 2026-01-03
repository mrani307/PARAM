#' calculates the increase in LAI of crop on i'th day , when the crop lies within
#' emergence and start of leaf decline  (-)
#'
#' @param HUF.o HUF value on i-1 th day (-)
#' @param HUF.i HUF value on ith day (-)
#' @param XLAI [CROP PARAMETER] maximum possible value of LAI for a specific crop(-)
#' @param LAI.o LAI value on i-1th day (-)
#' @param REG.i minimum crop stress factor on ith day (-)
#'
#' @return LAI of crop on i'th day (-)
#' @export
#'
#' @examples
  del.LAI_growing<-function( HUF.o, HUF.i, LAI.o, XLAI, REG.i)
  {
    dHUF  <- HUF.i-HUF.o
    LAI.i <- LAI.o + dHUF*XLAI*sqrt(REG.i)
    return(LAI.i)
  }