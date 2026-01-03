#reading inputs-----------------------------------------------------------------
ref.grids  <- raster(sprintf("%s/utils/cropArea.nc",dir.save))
cropArea   <- readRDS(sprintf("%s/utils/cropArea.RDS",dir.save))
ref.coords <- cropArea[,1:2]
rm(cropArea)


paste0("data_India/India_district_boundary/DISTRICT_BOUNDARY.shp") %>%
  read_sf() %>%
  st_transform(.,4326) %>%
  subset(.,District== .$District[which(.$District==dist.name %>% toupper())]) -> dist.shp  

#preparing data for district----------------------------------------------------


for(var in c("temp","dpt","ssrd","wind"))
{
  for(year in start.year:end.year)
  {
    var.year <- stack(sprintf("data_India/%s/%s_%d.nc",var,var,year)) %>% crop(.,st_buffer(dist.shp,30000))
    var.year <- resample(var.year,ref.grids,method="bilinear")
    
    var.year.df <- raster::extract(var.year,ref.coords) %>% as.data.frame()
  
    write_fst(var.year.df, sprintf("%s/utils/%s%d.fst",dir.save,var,year), 
              compress = 0,uniform_encoding = TRUE)
  }
}

rm(var,year,var.year,ref.grids,var.year.df)

#elevation----------------------------------------------------------------------

stack("data_India/SRTMGL3_NC.003_90m_aid0001.nc",varname="SRTMGL3_DEM") %>% crop(.,dist.shp)-> terrain

z <- raster::extract(terrain,ref.coords) %>% as.numeric()
saveRDS(z,sprintf("%s/utils/elevation.RDS",dir.save))
rm(terrain,z,dist.shp,ref.coords)
