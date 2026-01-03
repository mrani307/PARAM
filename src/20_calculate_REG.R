
#' Calculate the crop growth regulating factor (REG) on i'th day
#' It is used to adjust the potential daily biomass growth
#'
#' @param AEP.i actual plant water use on i'th day (mm)
#' @param EP.i potential plant water use on i'th day
#' @param TMP.i temperature on i'th day (degree C)
#' @param TBSC [CROP PARAMERTER] base temperature of the crop (degree C)
#' @param TOPC [CROP PARAMETER] optimal temperature of the crop (degree C)
#' @param ST.i water content on i'th day (in mm)
#' @param soil [GRID PARAMETER] soil information (list)
#' @param CAF [CROP PARAMETER] critical aeration factor for crop (-)
#'
#' @return crop regulating growth factor (REG) on i'th day (-)
#' @export
#'
#' @examples
calculate_REG<-function(AEP.i, EP.i, TMP.i, TBSC,TOPC,ST.i,soil,CAF)
{  
  #water stress factor

  WS.i <- AEP.i/EP.i
    
  #temperature stress factor
    
  RTO <- (TMP.i - TBSC)/(TOPC-TBSC)
  
  if(RTO<0 | RTO>2) TS <- 0
  else              TS <- sin(1.5708*RTO)
    
  #aeration stress
  
  res.calculate_rootZoneWater <- calculate_rootZoneWater(rootdepth = 1 , 
                                                         ST        = ST.i, 
                                                         soil      = soil)
  SW1 <- res.calculate_rootZoneWater$thRZ.Act
  PO1 <- res.calculate_rootZoneWater$thRZ.S
    
  if(CAF==1) CAF<- 0.99
  
  SAT <- 100*( SW1/PO1 - CAF )/( 1 - CAF )
    
  if(SAT>0)  AS <- 1 - SAT/(SAT+exp(-1.291-56.1*SAT))
  else       AS <- 1
    
  #crop growth regulating factor
    
  REG <- c(WS.i,TS,AS)
  REG[REG==0]<-NA
  REG <- min(na.omit(REG))
    
  return(REG)
}