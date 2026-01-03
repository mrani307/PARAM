#' Calculate potential increase in biomass of crop on i'th day
#'
#' @param RA.i solar radiation (in MJ m-2 d-1)
#' @param LAI.i Leaf Area Index on i'th day (-). 
#' @param RUE.i Radiation Use Efficiency Factor (RUE) on i'th day (kg ha-1)/(MJ m-2). . Calculated using [func(calculate_RUE)].
#' @param X1.i X1 value onb i'th day (-). . Calculated using [func(calculate_X1)].
#' @param WAVP [CROP PARAMETER] parameter relating RUE and VPD
#' @param EXT [GRID PARAMETER]extinction coefficient (-)
#'
#' @return potential increase in biomass of crop on i'th day ( in t ha-1 d-1)
#' @export
#'
#' @examples
  calculate_DDM<-function(RA.i, LAI.i, RUE.i, WAVP, EXT, X1.i)
  {
    
    PAR.i <- 0.5 * RA.i * (1-exp(-EXT*LAI.i)) #intercepted photosynthetic active radiation
    
    DDM.i <- 0.001 * PAR.i * (RUE.i - WAVP*X1.i) #potential increase in biomass for the given day
    
    return(DDM.i)
    
  }