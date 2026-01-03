read_iped_precipitation<-function(year)
{
  library(ncdf4)
  library(dplyr)
  library(raster)
  
  dir<-"data_India/precipitation"
  
  nc<-nc_open(paste0(dir,"/IPED_mean_",year,".nc"))
  nc_data<-ncvar_get(nc)
  len <- nc[["dim"]][["time"]][["len"]]+2
  
  rain<-matrix(nrow = 89072, ncol = len)
  
  c<-1
  for(long in 1:293)
  {
    for(lat in 1:304)
    {
      rain[c,1]<-nc[["dim"]][["lon"]][["vals"]][long]
      rain[c,2]<-nc[["dim"]][["lat"]][["vals"]][lat]
      rain[c,3:len]<-nc_data[,long,lat] 
      c<-c+1
    }
  }
  
  nc_close(nc)
  
  coord<-rain[,1:2]  
  lapply(3:len,function(d) data.frame(coord,rain[,d]) %>% rasterFromXYZ()) %>% stack() -> r
  crs(r)<-"+proj=longlat +datum=WGS84"
  
  return(r)
}  

#reading district shapefile
paste0("data_India/India_district_boundary/DISTRICT_BOUNDARY.shp") %>%
  read_sf() %>%  st_transform(.,4326) %>%  subset(.,District== .$District[which(.$District==dist.name %>% toupper())]) -> dist.shp 

cropArea   <- readRDS(sprintf("%s/utils/cropArea.RDS",dir.save))
ref.coords <- cropArea[,1:2]
rm(cropArea)

for(year in start.year:end.year )
{
  print(year)
  
  #reading downscale annual precipitation
  annual.SSP.ds<-raster(sprintf("%s/utils/annualPrecip%d.tif",dir.save,year))
  
  #reading precipitation data
  SPP.daily.ownRes  <- read_iped_precipitation(year) %>% crop(., st_buffer(dist.shp,20000))
  SPP.annual.ownRes <- SPP.daily.ownRes %>% sum 
  ratio.SSP         <- SPP.daily.ownRes/SPP.annual.ownRes
  rm(SPP.daily.ownRes,SPP.annual.ownRes)
  
  #NGB interpolation of the ratio
  lapply(1:nlayers(ratio.SSP),function(l) resample(ratio.SSP[[l]],annual.SSP.ds)) %>%  stack-> ratio.SSP.ds
  
  #calculating downscaled daily precipitation
  ratio.SSP.ds*annual.SSP.ds -> daily.SSP.ds
  
  #extracting precipitaiton data for the required crop grids
  daily.SSP.ds_cropGrids <- raster::extract(daily.SSP.ds,ref.coords) %>% as.data.frame()
  
  write_fst(daily.SSP.ds_cropGrids, sprintf("%s/utils/precip%d.fst",dir.save,year),compress = 0,uniform_encoding = TRUE)
  
  file.remove(sprintf("%s/utils/annualPrecip%d.tif",dir.save,year))
  rm(ratio.SSP,annual.SSP.ds,ratio.SSP.ds,daily.SSP.ds,daily.SSP.ds_cropGrids)
}

rm(year,read_iped_precipitation,dist.shp,ref.coords)
  