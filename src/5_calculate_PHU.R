#' Calculates the Potential Heat Units (PHU) require for maturity of a crop for each season (in degree Celsius).
#' It is calculated as the accumulated Head Units (HU) for each cropping season.
#' EPIC CROP GROWTH MODEL : Williams et al., 1989
#' 
#' @param crop.info crop.info dataframe
#' @param HU daily heat units time series for the whole time period (in degree Celsius). Calculated using [func(calculate_HU)].   
#'
#' @return Potential Heat Units (PHU) require for maturity of a crop for each season (in degree Celsius)
#' @export
#'
#' @examples
  calculate_PHU<-function(crop.info)
  {
    vapply( na.omit(unique(crop.info$season)),
            function(season) sum( crop.info$HU[ which( crop.info$season == season ) ] ),
            numeric(1)) -> PHU
    
    return(PHU)
  }
  