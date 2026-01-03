#' Calculate LAI of crop on the present day
#'
#' @param DLAI [CROP PARAMETER] fraction of growing season where leaf area declines (-)
#' @param HUF.o HUF value on i-1 th day (-)
#' @param HUF.i HUF value on ith day (-)
#' @param LAI.o LAI value on i-1th day (-)
#' @param XLAI [CROP PARAMETER] maximum possible value of LAI for a specific crop(-)
#' @param REG.i minimum crop stress factor on ith day (-)
#' @param HUI.i HUI on i'th day (-)
#' @param ad [CROP PARAMETER] parameter which governs LAI decline rate
#'
#' @return
#' @export
#'
#' @examples
calculate_LAI<-function(  DLAI,
                          HUF.o, 
                          HUF.i, 
                          LAI.o, 
                          XLAI, 
                          REG.i,
                          HUI.i, 
                          ad )
{
  
  #calculate LAI
    
    #check of LAI growing or decling stage
    if( HUI.i < DLAI)
      LAI.i <- del.LAI_growing( HUF.o, HUF.i, LAI.o, XLAI, REG.i)
    else
      LAI.i <- calculate_LAI.decline( LAI.o, HUI.i, HUI.d = DLAI, ad )
    
    return(LAI.i)
}