
calculate_PET<-function(crop.info,epic.cropNames,z,ts.temp,ts.dpt,ts.ssrd,lat,ts.doY,ts.wind)
{
  #reading the Kc information of each crop (from Allen et. al., 1998)
  cropPET<-read.csv("data_India/cropPET.csv")
  rownames(cropPET)<-cropPET[,1]
  cropPET<-cropPET[,-1]
  cropPET[2,5]<-1.45
  
  #number of cropping seasons in the whole timeperiod
  seasons<-unique(na.omit(crop.info$season))
  
  #setting crop growing stages based for each croping season
  crop.info$doS.P<-NA
  crop.info$cropGrowthStage<-NA
  for(s in seasons)
  {
    #selecting a single croping season
    pos<-which(crop.info$season==s)
    
    #sertting cumulative length of croping stage of the crop
    doS<-crop.info$doS[pos]
    crop.info$doS.P[pos]<-doS/length(doS)
    
    #assigning crop growing state of crop based on the cumulative length of growing stage
    cropName<-crop.info$crop_name[pos[1]]
    cut(crop.info$doS.P[pos], 
        breaks = unlist(c(0, cropPET[cropName,c(1,2,3)] ,1)),
        include.lowest = TRUE, 
        labels = c("ini","dev","mid","late")) -> crop.info$cropGrowthStage[pos]
  }
  
  #Kc values of each crop based on its crop growing stage
  crop.info$Kc<-NA
  for(cropName in epic.cropNames)
  {
    #Kc values for crop initial and mid-season phase (with constant Kc values)
    crop.info$Kc[which(crop.info$crop_name==cropName & crop.info$cropGrowthStage==1)]<-cropPET[cropName,4]
    crop.info$Kc[which(crop.info$crop_name==cropName & crop.info$cropGrowthStage==3)]<-cropPET[cropName,5]
  }
  for( s in seasons)
  {
    #Kc values for crop development and late season stage 
    s1End.pos <- tail(which(crop.info$cropGrowthStage==1 & crop.info$season==s),1)
    s3St.pos  <- head(which(crop.info$cropGrowthStage==3 & crop.info$season==s),1)
    s3End.pos <- tail(which(crop.info$cropGrowthStage==3 & crop.info$season==s),1)
    s4End.pos <- tail(which(crop.info$season==s),1)
    
    cropName  <- crop.info$crop_name[which(crop.info$season==s)][1]
    
    s1.Kc <- cropPET[cropName,"st1.Kc"]
    s3.Kc <- cropPET[cropName,"st3.Kc"]
    s4.Kc <- cropPET[cropName,"stEnd.Kc"]
    
    vapply((s1End.pos+1):(s3St.pos-1), 
           function(x) s1.Kc + ((s3.Kc-s1.Kc)/(s3St.pos-s1End.pos))*(x-s1End.pos),
           numeric(1)) -> crop.info$Kc[(s1End.pos+1):(s3St.pos-1)]
    
    vapply((s3End.pos+1):(s4End.pos-1), 
           function(x) s3.Kc + ((s4.Kc-s3.Kc)/(s4End.pos-s3End.pos))*(x-s3End.pos),
           numeric(1)) -> crop.info$Kc[(s3End.pos+1):(s4End.pos-1)]
    
    crop.info$Kc[s4End.pos]<-s4.Kc
  }
  
  #estimation of reference evapotranspiration using PM-FAO56 (Allen et al.,1998)
  crop.info$ETo <- calculate_ETo(z  = z,
                                 temp = ts.temp,
                                 dpt  = ts.dpt,
                                 Ra   = ts.ssrd,
                                 lat  = lat,
                                 doY  = ts.doY,
                                 u10  = ts.wind)
  #PET 
  crop.info$PET  <- vapply(1:nrow(crop.info),
                           function(d) ifelse(is.na(crop.info$Kc[d]),crop.info$ETo[d],crop.info$ETo[d]*crop.info$Kc[d]),
                           numeric(1))
  
  return(crop.info)
  
}
  
  
  
  
  
  
  
  
