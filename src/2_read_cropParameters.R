#read crop parameters 
read_cropParameters<-function(crop.param.file,crop)
{
  crop  <- toupper(crop)
 
  #extract the row in which the crop parameter exists
  crop.row<-which(crop.param.file$NAME==crop)
  
  #reading the crop parameters
  
  crop.param<-list( WA    = crop.param.file$WA[crop.row],    #Biomass Energy Ratio @ CO2=330ppm
                    HI    = crop.param.file$HI[crop.row],    #Harvest Index
                    TOP   = crop.param.file$TOP[crop.row],   #Optimal Temperature for plant growth
                    TBS   = crop.param.file$TBS[crop.row],   #minimum temperature for plant growth
                    DMLA  = crop.param.file$DMLA[crop.row],  #maximum potential leaf area index
                    DLAI  = crop.param.file$DLAI[crop.row],  #fraction of growing season where leaf area declines
                    DLAP1 = crop.param.file$DLAP1[crop.row], #first point of optimal leaf area development curve
                    DLAP2 = crop.param.file$DLAP2[crop.row], #second point of optimal leaf area development curve
                    RLAD  = crop.param.file$RLAD[crop.row],  #leaf area decline parameter
                    CAF   = crop.param.file$CAF[crop.row],   #critical aeration factor
                    RDMX  = crop.param.file$RDMX[crop.row],  #maximum root depth
                    WAC2  = crop.param.file$WAC2[crop.row],  #CO2/resulting WA value 
                    WSYF  = crop.param.file$WSYF[crop.row],  #lower limit of harvest index
                    WAVP  = crop.param.file$WAVP[crop.row],  #parameter relating vapor pressure deficit to WA
                    RWPC1 = crop.param.file$RWPC1[crop.row], #fraction of root weight at emergence
                    RWPC2 = crop.param.file$RWPC2[crop.row], #fraction of root weight at maturity
                    HMX   = crop.param.file$HMX[crop.row],   #maximum crop height
                    GSI   = crop.param.file$GSI[crop.row],   #maximum stomatal conductance
                    VPD2  = crop.param.file$RWPC2[crop.row], #VPD value
                    VPTH  = crop.param.file$RWPC2[crop.row], #threshold VPD value
                    SMT   = crop.param.file$SMT[crop.row],
                    p     = 0.5                              #depletion fraction
                  )
  
  rm(crop.param.file,crop.row)
  return(crop.param)
}