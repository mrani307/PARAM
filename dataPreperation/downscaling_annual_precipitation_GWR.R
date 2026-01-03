#functions----------------------------------------------------------------------
  
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
  
  aggregate_raster<-function(from,to,fun=mean)
  {
    fact <- round(res(to) / res(from)) %>% .[1]
    fine_avg <- aggregate(from, fact = fact, fun = fun, na.rm = TRUE)
    fine_avg <- resample(fine_avg, to, method = "ngb")
    
    return(fine_avg)
  }
  
  fillNAcells<-function(r)
  {
    # Create a mask of NA cells
    na_mask <- is.na(r)
    
    # Temporarily replace NA with 0 (or any value)
    r_temp <- r
    values(r_temp)[is.na(values(r_temp))] <- 0
    
    # Smooth it using a moving window (mean)
    w <- matrix(1, 3, 3)
    r_smooth <- focal(r_temp, w, fun = mean, na.policy = "omit", na.rm = TRUE)
    
    # Replace only NA cells in original raster
    r_filled <- r
    values(r_filled)[is.na(values(r_filled))] <- values(r_smooth)[is.na(values(r_filled))]
    
    return(r_filled)
  }
  
  
#reading inputs-----------------------------------------------------------------
  
  #reading district shapefile
  paste0("data_India/India_district_boundary/DISTRICT_BOUNDARY.shp") %>%
    read_sf() %>%  st_transform(.,4326) %>%  subset(.,District== .$District[which(.$District==dist.name %>% toupper())]) -> dist.shp 
  
  #reading co-variates
  stack("data_India/MOD13A1.061_500m_aid0001.nc",varname="_500m_16_days_NDVI") %>% crop(.,st_buffer(dist.shp,20000)) -> ndvi 
  stack("data_India/SRTMGL3_NC.003_90m_aid0001.nc",varname="SRTMGL3_DEM") %>% crop(.,st_buffer(dist.shp,20000))      -> terrain
  
  #rescaling terrain to SPP and required resolution
  p <- read_iped_precipitation(2005)
  terrain %>% aggregate_raster(.,p %>%  crop(., st_buffer(dist.shp,20000)))  -> terrain.ownRes
  terrain %>% aggregate_raster(.,ndvi) ->  terrain.ds
  rm(terrain,p)
  
  
#main code----------------------------------------------------------------------
  
  #ch.range<-list()
  #ch.NA   <-c()

  for(year in start.year:end.year)
  {
    print(year)
  
    #preparing annual precipitation at its original resolution (0.1x0.1)
    SPP.annual.ownRes <- read_iped_precipitation(year) %>% sum %>% crop(., st_buffer(dist.shp,20000))
    
    #rescaling the annual NDVI for the year (at resolution of SSP and 500m)
    ndvi %>% names %>% substr(.,2,5) %>% as.numeric %>% is_in(year) %>%  
      which %>% ndvi[[.]] -> ndvi.year
    lapply(1:12, function(m) {
      ndvi.year %>% names %>% substr(.,7,8) %>% as.numeric %>% is_in(m) %>% which %>% ndvi.year[[.]] -> ndvi.month
      if(nlayers(ndvi.month)>1) return(ndvi.month %>% max())
      else return(ndvi.month)
    }) %>% stack() %>% mean() -> ndvi.annual.ds
    rm(ndvi.year) 
    
    ndvi.annual.ownRes <- aggregate_raster(ndvi.annual.ds,SPP.annual.ownRes)
    
  #GWR data preperation --------------------------------------------------------
         
    #preparing dataframe for GWR operation
    stack(SPP.annual.ownRes,ndvi.annual.ownRes,terrain.ownRes) %>% 
      crop(.,st_buffer(dist.shp,10000)) %>% as.data.frame(.,xy=T) -> ownRes.df
    colnames(ownRes.df)<-c("x","y","precipitation","ndvi","terrain") 
  
    ownRes.df      <-ownRes.df %>% na.omit
    ownRes.df$long <-ownRes.df$x   #including longitude as co-variate
    ownRes.df$lat  <-ownRes.df$y   #including latitude  as co-variate
    shp.ownRes<-ownRes.df          #converting into shape-file
    coordinates(shp.ownRes)=~x+y
      
  #GWR operation----------------------------------------------------------------
    DM <- gw.dist(dp.locat=coordinates(shp.ownRes) ,longlat = T)
    bw <- bw.gwr(precipitation~ndvi+terrain+long+lat, data=shp.ownRes, kernel = "gaussian",dMat=DM) 
    gwr.res<-gwr.basic(precipitation~ndvi+terrain+long+lat, data=shp.ownRes, 
                       bw=bw, kernel="gaussian",dMat=DM)[["SDF"]]
      
    #rm(DM,bw,ownRes.df,#SPP.annual.ownRes,ndvi.annual.ownRes,shp.ownRes)
      
  #interpolation of GWR intercepts, co-efficients and residuals to high resolution (bilinear interpolation----------
      
    stack(ndvi.annual.ds,terrain.ds) %>%  crop(.,dist.shp) %>%  as.data.frame(.,xy=T)  -> ds.df
    colnames(ds.df)[c(3:4)]<-c("ndvi","terrain")
    ds.df$long <-ds.df$x   #including longitude as co-variate
    ds.df$lat  <-ds.df$y   #including latitude  as co-variate
    
    shp.ds<-ds.df          #converting into shape-file
    coordinates(shp.ds)=~x+y
    ndvi.ds.coeff     <-idw(ndvi ~ 1,      gwr.res , newdata=shp.ds, idp = 2.0)
    intercept.ds.coeff<-idw(Intercept ~ 1, gwr.res , newdata=shp.ds, idp = 2.0)
    terrain.ds.coeff  <-idw(terrain ~ 1,   gwr.res , newdata=shp.ds, idp = 2.0) 
    long.ds.coeff     <-idw(long ~ 1,      gwr.res , newdata=shp.ds, idp = 2.0)
    lat.ds.coeff      <-idw(lat ~ 1,       gwr.res , newdata=shp.ds, idp = 2.0)
    residual.ds.coeff <-idw(residual ~ 1,  gwr.res , newdata=shp.ds, idp = 2.0)
    data.frame(ndvi.ds.coeff@coords,
               intercept.ds.coeff$var1.pred,
               ndvi.ds.coeff$var1.pred,
               terrain.ds.coeff$var1.pred,
               long.ds.coeff$var1.pred,
               lat.ds.coeff$var1.pred,
               residual.ds.coeff$var1.pred) -> ds.coeff
    
    rain.ds<-ds.coeff$intercept.ds.coeff.var1.pred+
             ds.df$ndvi   *ds.coeff$ndvi.ds.coeff.var1.pred+
             ds.df$terrain*ds.coeff$terrain.ds.coeff.var1.pred+
             ds.df$x      *ds.coeff$long.ds.coeff.var1.pred+
             ds.df$y      *ds.coeff$lat.ds.coeff.var1.pred+
             ds.coeff$residual.ds.coeff.var1.pred
    #converting the downscaled annual precipitation into raster
    data.frame(ds.coeff[,1:2],rain.ds) %>% rasterFromXYZ -> rain.ds
    crs(rain.ds)<-"+proj=longlat +datum=WGS84"
    
    rain.ds[rain.ds<0]<-NA
    rain.ds<-fillNAcells(rain.ds)
    
    #ch.NA   <-c(ch.NA,length(which(is.na(as.vector(rain.ds)))))
    #ch.range[[year-2000]]<-glue("ds : {round(range(as.vector(rain.ds),na.rm = T),2)} , org : {round(range(as.vector(SPP.annual.ownRes),na.rm = T))}")
    
    #print(length(which(as.vector)))
    #print(glue("ds : {round(range(as.vector(rain.ds),na.rm = T),2)} , org : {round(range(as.vector(SPP.annual.ownRes),na.rm = T))}"))
    
    writeRaster(rain.ds,sprintf("%s/utils/annualPrecip%d.tif",dir.save,year))
      
    rm(ds.df,ndvi.annual.ds,gwr.res,rain.ds,intercept.ds.coeff,ndvi.ds.coeff,long.ds.coeff,
       lat.ds.coeff,residual.ds.coeff,terrain.ds.coeff,ds.coeff,SPP.annual.ownRes,ownRes.df,shp.ds,shp.ownRes,DM,ndvi.annual.ownRes,bw)
  }
  
  rm(terrain.ds,terrain.ownRes, ndvi, dist.shp,read_iped_precipitation,year,aggregate_raster,fillNAcells)
      
     