# Function to calculate actual and total available water in the root
# zone at current time step


# Calculate root zone water content and available water %%
#Compartments covered by the root zone


#' Title
#'
#' @param rootdepth root depth of crop on the current day (in m)
#' @param ST soil water content in each compartment on the present day(m3/m3)
#' @param soil soil information (list)
#'
#' @return
#' @export
#'
#' @examples
calculate_rootZoneWater<-function(rootdepth, ST, soil)
{
  #' comp      counter variable for soil compartment (-)
  #' comp_sto  compartment in which root tip lies on the present day (-)
  #' l         layer of soil to which the soil compartment belongs to (-)
  #' factor    Fraction of compartment covered by root zone (-)
  #' WrAct     Actual water storage in root zone (mm)
  #' WrS       Water storage in root zone at saturation (mm) 
  #' WrFC      Water storage in root zone at field capacity (mm)
  #' WrWP      Water storage in root zone at permanent wilting point (mm)
  #' TAW.Rz    Calculate total available water (m3/m3)
  #' Dr.Rz     Calculate soil water depletion (mm)
  #' thRZ.Act  Actual root zone water content (m3/m3)
  #' thRZ.S    Root zone water content at saturation (m3/m3)
  #' thRZ.FC   Root zone water content at field capacity (m3/m3)
  #' thRZ.WP   Root zone water content at permanent wilting point (m3/m3)
  
  
  comp_sto <- findInterval( rootdepth,  c(0,cumsum(soil$soil.comp$del.z)  ) )
  
  # Initialise counters
  WrAct <- 0
  WrS   <- 0
  WrFC  <- 0
  WrWP  <- 0
  
  for( comp in 1:comp_sto)
  {
    #Specify layer
    l <- soil$soil.comp$layer[comp] 
    
    # Fraction of compartment covered by root zone
    if( sum(soil$soil.comp$del.z[1:comp]) > rootdepth )
      factor <- 1 - ( ( sum(soil$soil.comp$del.z[1:comp]) - rootdepth ) / soil$soil.comp$del.z[comp] )
    else
      factor <- 1
    
    # Actual water storage in root zone (mm)
    WrAct <- WrAct + ( factor * 1000 * ST[comp] * soil$soil.comp$del.z[comp] )
    # Water storage in root zone at saturation (mm) 
    WrS   <- WrS   + ( factor * 1000 * soil$soil.layer$SAT[l] * soil$soil.comp$del.z[comp] )
    # Water storage in root zone at field capacity (mm)
    WrFC  <- WrFC  + ( factor * 1000 * soil$soil.layer$FC[l] * soil$soil.comp$del.z[comp] )
    # Water storage in root zone at permanent wilting point (mm)
    WrWP  <- WrWP  + ( factor * 1000 * soil$soil.layer$WP[l] * soil$soil.comp$del.z[comp] ) 
  }
  
  if( WrAct < 0 )
    WrAct <- 0
  
  # Calculate total available water (m3/m3)
  TAW.Rz <- max( WrFC-WrWP, 0 )
  # Calculate soil water depletion (mm)
  Dr.Rz  <- min( WrFC-WrAct, TAW.Rz )
  
  # Actual root zone water content (m3/m3)
  thRZ.Act <- WrAct/( rootdepth * 1000 )
  # Root zone water content at saturation (m3/m3)
  thRZ.S   <- WrS/( rootdepth * 1000 )
  # Root zone water content at field capacity (m3/m3)
  thRZ.FC  <- WrFC/( rootdepth * 1000 )
  # Root zone water content at permanent wilting point (m3/m3)
  thRZ.WP  <- WrWP/( rootdepth * 1000 )
  
  return( list( TAW.Rz   = TAW.Rz,
                Dr.Rz    = Dr.Rz,
                thRZ.Act = thRZ.Act,
                thRZ.S   = thRZ.S,
                thRZ.FC  = thRZ.FC,
                thRZ.WP  = thRZ.WP
              ))
}