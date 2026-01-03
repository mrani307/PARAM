#' Determine irrigation depth (mm/day) to be applied 
#'
#' @param rootdepth root depth of crop on the present day
#' @param Irr.AppEff.irrGrid irrigation application efficiency on grids with canal irrigation
#' @param Irr.AppEff.nonIrrGrid irrigation application efficiency on grids without canal irrigation
#' @param SMT.irrGrid soil moisture threshold to trigger irrigation on grids with canal irrigation
#' @param SMT.nonIrrGrid soil moisture threshold to trigger irrigation on grids without canal irrigation
#' @param irr.grid check if the present grid is irrigated or not
#' @param ST soil moisture content at each compartment of soil (m3/m3)
#' @param soil soil information (list)
#' @param EPot potential soil evaporation on present day (mm)
#' @param TPot potential crop evaporation on present day (mm)
#' @param Rain rainfall on present day(mm)
#' @param Runoff runoff on present day(mm)
#' 
#' @return
#' @export
#'
#' @examples
calculate_irrigation<-function( rootdepth, ST, soil, EPot , TPot, Rain , Runoff, cropName, crop.param,
                                irr.grid,Irr.AppEff.irrGrid, Irr.AppEff.nonIrrGrid)
{
  
  #' variables used : 
  #' AbvFc   water excess of field capacity at root zone ( in mm )
  #' IrrThr  threshold to initiate irrigation(mm)
  #' IrrReq  Irrigation required (mm)
  #' EffAdj  used for calculation of irrigation provided after considering application frequency (-)
  #' Irr     irrigation which shall be provided (mm)
 
  #' functions used :
  #' calculate_rootZoneWater 
  

  #setting irrigation application efficiency for Rice crop under flooded irrigation
  if(cropName == "RICE")
  {
    Irr.AppEff.irrGrid    <- 50
    Irr.AppEff.nonIrrGrid <- 50
  }
    
  # Calculate root zone water content and depletion
  rootZoneWater.res <- calculate_rootZoneWater(rootdepth, ST, soil)
  
  # Use root zone depletion and TAW only for triggering irrigation
  Dr       <- rootZoneWater.res$Dr.Rz
  TAW      <- rootZoneWater.res$TAW.Rz
  thRZ.Act <- rootZoneWater.res$thRZ.Act
  thRZ.FC  <- rootZoneWater.res$thRZ.FC
  
  
  # Determine adjustment for inflows and outflows on current day %
  if( thRZ.Act > thRZ.FC )
    AbvFc <- ( thRZ.Act - thRZ.FC ) * 1000 * rootdepth
  else
    AbvFc <- 0
  
  WCadj <- EPot + TPot - Rain + Runoff - AbvFc
  
  # Run irrigation depth calculation
    
    # Determine threshold to initiate irrigation 
    IrrThr <- ( 1 - (crop.param[[cropName]][["SMT"]]/100) ) * TAW
    
    # Adjust depletion for inflows and outflows today
    Dr <- Dr + WCadj 
    
    if( Dr < 0 )
      Dr <- 0
    
    # Check if depletion exceeds threshold
    if( Dr > IrrThr )
    {
      # Irrigation will occur
      IrrReq <- max( 0 , Dr )
      
      # Adjust irrigation requirements for application efficiency
      {
        if(irr.grid==1)
          EffAdj <- ( ( 100-Irr.AppEff.irrGrid ) + 100 ) / 100
        else
          EffAdj <- ( ( 100-Irr.AppEff.nonIrrGrid ) + 100 ) / 100
      }
      IrrReq <- IrrReq * EffAdj
      
      # Irrigation to be applied
      Irr <- IrrReq
    }
    else
    {
      # No irrigation
      Irr <- 0
    }
    
    return(Irr)
}  
