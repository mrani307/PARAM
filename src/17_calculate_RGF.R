#' Calculating of root growth stress factor (RGF)
#'
#' @param SAN percentage sand in the each layer of soil (%)
#' @param BDi bulk density of soil in (t m-3)
#' param BD3 bulk density of each layer of soil at 33kPA water content (t m-3)
#' param BDD oven dry bulk density (t m-3)
#' param FC water content at field capacity at each layer of soil (m3/m3)
#' param WP water content at wilting point at each layer of soil (m3/m3)
#' param SW.i present day water content at each layer of soil (m3/m3) 
#'
#' @return root growth stress factor (RGF) at each layer of soil
#' @export
#'
#' @examples
calculate_RGF<-function(no.of.comp,soil)
{
  
  SS<-c()
  
  #soil strength factor
  for( comp in 1:no.of.comp)
  {
    
    l <- soil$soil.comp$layer[comp]
    
    SAN <- soil$soil.layer$SAN[l]
    BD.i <- soil$soil.layer$BD[l]
    
    BDL <- 1.15 + 0.00445 * SAN  #bulk density near lower boundary
    BDU <- 1.5  + 0.05    * SAN  # bulk density near upper boundary
    bt2 <-(log(0.112*BDL)-log(8*BDU)) / (BDL-BDU)
    bt1 <-log(0.0112*BDL)-bt2*BDL
    
    #BD.i<- BD3 + (BDD-BD3)*( (FC-SW.i) / (FC-WP*(4.0833-3.33*BDD^(1/3))) ) #water content adjusted bulk density for the given day
    SS[comp]  <-0.1+( 0.9*BD.i/(BD.i+exp(bt1+bt2*BD.i)))
  }
  #root growth stress factor
    
    RGF<- SS
    
  return(RGF)
}