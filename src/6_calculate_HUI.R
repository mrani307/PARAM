#' Calculates the daily Heat unit index (HUI) for all the seasons within the whole time period (-).
#' EPIC CROP GROWTH MODEL : Williams et al., 1989
#'
#' @param crop.info crop.info data frame 
#' @param PHU Potential Heat Units (PHU) for all the cropping seasons within the whole time period (in degree Celcius) . Calculated using [func(calculate_PHU)]  
#'
#' @return Daily heat unit index (-) for all the seasons withing the whole time period 
#' @export 
#'
#' @examples
  calculate_HUI<-function( crop.info, PHU)
  {
    # counts the number of cropping seasons within the whole time period
    length(unique(na.omit(crop.info$season)))-> no.of.seasons
    
    #calculating HUI
    crop.info$HUI <-  NA
    for(s in seq_len(no.of.seasons))
      crop.info$HUI[which(crop.info$season==s)] <- cumsum(crop.info$HU[which(crop.info$season==s)])/PHU[s]
    
    return(crop.info$HUI)
}