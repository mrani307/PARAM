#function-----------------------------------------------------------------------

extract_soil_information<-function(start,end)
{
  
  #' inputs are the start and ending column number for the specified variable
  
  coord  <-list()
  var    <-list()
  
  count<-1
  
  for(row in seq(1,526071,13))
  {
    #coordinates
    
    row.val<-d[(row+2),]
    
    c(substr(row.val,37,42) %>% as.numeric,
      substr(row.val,28,33) %>% as.numeric) -> coord[[count]]
    
    #soil parameters
    
    var.height<-c()
    
    for(h in 6:11)
    {
      row.val<-d[(row+h),]
      var.height[h-5]<-substr(row.val,start,end) %>% as.numeric
    }
    
    var[[count]]<-var.height
    
    count<-count+1
    
  }
  
  df<-cbind.data.frame(do.call(rbind,coord),do.call(rbind,var))
  colnames(df)<-c("long","lat","d_0-5cm","d_5-15cm","d_15-30cm","d_30-60cm","d_60-100cm","d_100-200cm")
  
  return(df)
}

prepare_soil_information_for_dist<-function(var.df,ref.coords)
{
  crop.df <- foreach(x = 1:nrow(ref.coords),.combine='rbind') %dopar%
  {
    library(sp)
    library(dplyr)
    
    spDistsN1(var.df[,1:2] %>% as.matrix(),
              ref.coords[x,] %>% as.matrix(),
              longlat = TRUE)  %>% 
      which.min() %>% 
      var.df[.,3:8]
  }
  return(crop.df)
} 
 
#reading soil data--------------------------------------------------------------

  d<-read.delim("data_India/IN.SOL", header = F)
  
  cropArea   <- readRDS(sprintf("%s/utils/cropArea.RDS",dir.save))
  ref.coords <- cropArea[,1:2]
  rm(cropArea)
  
#preparing soil data------------------------------------------------------------

  #start parallel
  my.cluster <- parallel::makeCluster(48,type = "PSOCK")
  print(my.cluster)
  doParallel::registerDoParallel(cl = my.cluster)
  
  #wilting point
  wilting.point          <- extract_soil_information(14,18)
  wilting.point.cropArea <- prepare_soil_information_for_dist(wilting.point,ref.coords)
  write_fst(wilting.point.cropArea, sprintf("%s/utils/wiltingPoint.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(wilting.point,wilting.point.cropArea)
  
  #field capacity
  field.capacity          <- extract_soil_information(20,24)
  field.capacity.cropArea <- prepare_soil_information_for_dist(field.capacity,ref.coords)
  write_fst(field.capacity.cropArea, sprintf("%s/utils/fieldCapacity.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(field.capacity,field.capacity.cropArea)
  
  #saturation capacity
  saturation.capacity          <- extract_soil_information(26,30)
  saturation.capacity.cropArea <- prepare_soil_information_for_dist(saturation.capacity,ref.coords)
  write_fst(saturation.capacity.cropArea, sprintf("%s/utils/saturationCapacity.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(saturation.capacity,saturation.capacity.cropArea) 
  
  #hydraulic conductivity
  hydraulic.conductivity          <- extract_soil_information(39,42)
  hydraulic.conductivity.cropArea <- prepare_soil_information_for_dist(hydraulic.conductivity,ref.coords)
  write_fst(hydraulic.conductivity.cropArea, sprintf("%s/utils/hydraulicConductivity.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(hydraulic.conductivity,hydraulic.conductivity.cropArea)
  
  # % sand
  per.silt <- extract_soil_information(62,66) #silt
  per.clay <- extract_soil_information(56,60) #clay
  per.sand <- per.clay
  for(c in 3:8) per.sand[,c]<-100-(per.clay[,c]+per.silt[,c])
  per.sand.cropArea<- prepare_soil_information_for_dist(per.sand,ref.coords)
  write_fst(per.sand.cropArea, sprintf("%s/utils/perSand.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(per.sand,per.sand.cropArea,per.silt,per.clay)
  
  #bulk density
  bulk.density   <-extract_soil_information(45,48)
  bulk.density.cropArea<- prepare_soil_information_for_dist(bulk.density,ref.coords)
  write_fst(bulk.density.cropArea, sprintf("%s/utils/bulkDensity.fst",dir.save), 
            compress = 0,uniform_encoding = TRUE)
  rm(bulk.density,bulk.density.cropArea)

  #end parallel
  foreach::registerDoSEQ()
  parallel::stopCluster(cl = my.cluster)
  
rm(extract_soil_information,prepare_soil_information_for_dist,d)
