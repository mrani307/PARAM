#reading district shapefile
  paste0("data_India/India_district_boundary/DISTRICT_BOUNDARY.shp") %>%
    read_sf() %>%
    st_transform(.,4326) %>%
    subset(.,District== .$District[which(.$District==dist.name %>% toupper())]) -> dist.shp  

#reading yearly modis lulc data (500m) from 2001 to 2020
  "data_India/MCD12Q1.061_500m_aid0001.nc" %>% 
    stack(.,varname="LC_Type1") %>% 
    .[[1:20]] %>%
    crop(., st_buffer(dist.shp,dist=10000)) -> modis.lulc
  modis.cropLand.pixVal <- c(12,14) #the pixel values which are to be considered as cropland

#reading gridded CROP physical area
  
  suppressMessages(suppressWarnings({
    lapply(gridData.cropNames, 
         function(crop) raster(paste0("data_India/spam2020V1r0_global_physical_area/spam2020_v1r0_global_A_",crop,"_A.tif"))) %>%
    stack -> gridded.cropPhyData
  }))

#reading crop statistics data for whole India
  suppressMessages( india.cropStats<- read_excel("data_India/crop_statistics_kharif.xlsx") )

#reading command area (major and medium irrigation projects) shapefile India
  read_sf("data_India/command_area_India/WRPCOMMAND.shp") -> commandArea
  
#preparing dynamic crop maps ---------------------------------------------------
  
  #function to extract the crop statistics (Kharif) for the required required crops, district & time period
  prepare_dist.cropInfo<-function(india.cropStats, start.year,end.year,rep.cropNames,dist.name,state.name)
  {
    #extracting the name of the states from the above file
    lapply(india.cropStats[,1] %>% unlist %>% na.omit(), 
           function(x) if( substr(x,1,1) %>% utf8ToInt() %in% 48:57 ) return(x)) %>% 
      unlist -> states
    
    states %>% 
      sub("^[^. ]*\\. ", "", .) %>% 
      is_in(state.name) %>%
      which %>%
      states[[.]] -> state.name
    
    #extracting data for each state-----------------------------------------------
    
    state.cropStats <- list()
    
    for(s in 2:(length(states)+1))
    {
      if(s <= length(states))
      {
        st.1 <- states[s-1]
        st.2 <- states[s]
        
        pos.1 <- which(india.cropStats[,1]==st.1)
        pos.2 <- which(india.cropStats[,1]==st.2)
        
        state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
      }
      else
      {
        st.1 <- states[s-1]
        
        pos.1 <- which(india.cropStats[,1]==st.1)
        pos.2 <- nrow(india.cropStats)
        
        state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
      }
    }
    
    rm(india.cropStats,st.1,st.2,pos.1,pos.2,s,states)
    
    #extracting data for a particular state-----------------------------------
    
    cropStats <- state.cropStats[[state.name]]
    
    row.no <- which(!is.na(cropStats[,2]))
    row.no <- c(row.no,(nrow(cropStats)+1))
    
    cropStats.dist<-list()
    
    for(c in 1:(length(row.no)-1))
    {
      start <- row.no[c]
      end   <- row.no[c+1]-1
      dist_name <- cropStats[start,2] %>% unlist
      crop.stat <- cropStats[(start):(end),]
      colnames(crop.stat)<-colnames(cropStats)
      
      cropStats.dist[[dist_name]] <- crop.stat
    }
    
    rm(state.cropStats,cropStats, row.no, c,dist_name, start,end,crop.stat,state.name)
    
    #extracting crop stats for the required district--------------------------
    
    cropStats.dist %>% 
      names %>% 
      sub("^[^. ]*\\. ", "", .) %>% 
      is_in(dist.name) %>%
      which %>%
      cropStats.dist[[.]] -> cropData.dist
    
    rm(cropStats.dist)
    
    #extracting required crop data--------------------------------------------
    
    lapply(rep.cropNames ,function(crop.name) colnames(cropData.dist) %>% is_in(crop.name) %>% which) %>% unlist() -> col
    
    avail.years <- cropData.dist$Year %>% substr(.,1,4) %>% as.numeric()
    if(avail.years[1]>start.year | avail.years[length(avail.years)]<end.year)
    {
      print(paste0("Crop statistics for ",dist.name," is available from ",avail.years[1]," to ",avail.years[length(avail.years)]))
      stop("Please chaange the start.year and end.year")
    }
    
    rows <- which(avail.years %in% c(start.year:end.year))
    
    lapply(col, function(c) cropData.dist[rows,(c:(c+2))] %>% sapply(., as.numeric) %>% as.data.frame() ) -> dist.cropInfo
    lapply(dist.cropInfo, function(df) {
      colnames(df) <- c("Area","Production","Yeild")
      rownames(df) <- c(start.year:end.year)
      return(df)
    }) -> dist.cropInfo
    names(dist.cropInfo) <- rep.cropNames
    
    rm(cropData.dist,col,rows,avail.years)
    
    return(dist.cropInfo)  
  }
  
  check_and_fill_missing_data<-function(df)
  {
    if(nrow(na.omit(df))!=nrow(df))
    {
      x<-rownames(na.omit(df)) %>% as.numeric()
      y<-df$Area[which(!is.na(df$Area))]
      x1<-rownames(df)[which(is.na(df$Area))] %>% as.numeric()
      df$Area[which(is.na(df$Area))]<-interp1(x,y,x1,"nearest")
      return(df)
    }
    else
      return(df)
  }

  #masking the SPAM2020V1 physical area to our study area (district)
  mask_SPAM_to_district<-function( dist.shp, gridded.cropPhyData)
  {
    #counting the number of crops used in the study
    crop.len   <- gridded.cropPhyData %>% nlayers()
    
    #cropping the SPAM raster stack to the district boundary
    gridded.cropPhyData <- gridded.cropPhyData %>% crop(.,st_buffer(dist.shp,dist=10000))
    
    #masking the gridded crop Physical raster to the district boundary, after 
    #taking weighted areal values for the boundary pixels
    mask.info <- terra::extract(terra::rast(gridded.cropPhyData),
                                terra::vect(dist.shp), 
                                weights=T,
                                exact=T,
                                xy=T)
    
    #weight values for each pixels (fraction of area of that pixel inside the district boundary)
    weights    <- mask.info$weight
    
    #re-arranging the columns
    mask.info  <- mask.info[,c(crop.len+2,crop.len+3,(1:crop.len)+1)] 
    
    #area averaged value of the masked raster pixels
    mask.info  <- data.frame( mask.info[,1:2], mask.info[,3:(2+crop.len)]*weights ,weights)
    mask.info[is.na(mask.info)] <- 0
    
    mask.info %>% .[,-ncol(.)] -> mask.info
    
    return(mask.info)
  }
  
  calculate_MODcrop_pixel <- function( spam.dist, dist.cropArea, start.year, end.year, modis.pixArea)
  {
    #reading the crop physical area(ha) for small millet (ragi), maize, rice and groundnut from SPAM2020
    spam.data.dist <- spam.dist
    
    #reading observed data of physical crop area for that particular district
    obs.area.dist <- dist.cropArea
    
    #to store the number of MODIS cropland pixels corresponding to each SPAM grid for a district for each year
    dist.year.cropInfo<-list()
    
    for(year in start.year:end.year)
    {
      #reported crop area for crops on that year
      obs.area<-obs.area.dist[as.character(year),]
      
      #calculating total area of crops from SPAM2000 data
      spam.area<-apply(spam.data.dist[,-c(1:2)],2,sum)
      
      #difference between reported and SPAM crop areas
      diff.area<-obs.area-spam.area
      
      #'correcting the spam crop area, by increasing/decreasing the area of each pixel by 
      #'correction factor for a specific crop
      for(c in 3:(ncol(spam.data.dist)))
      {
        crop.area          <- spam.data.dist[,c]               #area of crop in each pixel
        spam.data.dist[,c] <- diff.area[c-2]*crop.area/sum(crop.area) + spam.data.dist[,c]
      }
      
      rm(crop.area, diff.area,spam.area,c)
      
      #calculating the number of MODIS pixels to be allocated for each SPAM pixel
      
      #to store the number of modis cropland pixels corresponding to each SPAM grid 
      modis.pixels <- spam.data.dist
      
      #converting the crop area into MODIS pixels
      modis.pixels[,3:ncol(spam.data.dist)] <- modis.pixels[,3:ncol(spam.data.dist)]/modis.pixArea 
      
      error<-numeric(length = length(3:ncol(spam.data.dist)))
      
      for(c in 3:ncol(spam.data.dist))
      {
        #total number of modis cropland pixels to be assigned to that crop for each SPAM grid
        modis.pix_perGrid <- modis.pixels[,c]
        
        #rounding the number of pixels
        modis.pix_perGrid_rounded  <- round(modis.pix_perGrid)
        
        #error in total area of crop within the district due to rounding off
        total.diff_Area <- obs.area[c-2] - sum(modis.pix_perGrid_rounded) * modis.pixArea 
        
        #number of pixels corresponding to the difference in area calculated above
        total.diff_modis.Pix <- round(total.diff_Area/modis.pixArea)
        
        if(total.diff_modis.Pix > 0 )
        {
          #'if there is an error of 'n' pixels (where n=total.diff_modis.Pix)
          #'more more modis pixel is added/subtracted from n SPAM grids having maximum number of modis pixels
          pos <- order(modis.pix_perGrid_rounded,decreasing = T)[1:abs(total.diff_modis.Pix)] 
          modis.pix_perGrid_rounded[pos] <- modis.pix_perGrid_rounded[pos] + 1*(total.diff_modis.Pix/abs(total.diff_modis.Pix)) 
          
          if(any(modis.pix_perGrid_rounded<0))
            stop("Error!")
          
          error[c-2] <- sum(modis.pix_perGrid_rounded)*modis.pixArea - obs.area[c-2]
          rm(pos)
        }
        
        modis.pixels[,c] <- modis.pix_perGrid_rounded
        
      } 
      
      #print(year)
      #print(error)
      
      rm(c,modis.pix_perGrid,modis.pix_perGrid_rounded,total.diff_Area,total.diff_modis.Pix)
      
      dist.year.cropInfo[[as.character(year)]] <- modis.pixels
    }
    
    return(dist.year.cropInfo)
  }
  
  calculate_availableCroplands<-function(dist.shp, spam.dist,modis.lulc, modis.cropLand.pixVal)
  {
    #converting the SPAM raster into a spatial polygon
    spam.shp <- as( rasterFromXYZ(spam.dist[,1:3]), 'SpatialPolygons')
    
    #modis masked to study area
    modis.lulc.masked <- modis.lulc %>% mask(.,dist.shp)
    
    grid.croplands <- list()
    
    for(i in 1:length(spam.shp))
    {  
      #print(i)
      
      #extracting modis LULC info for a specific SPAM grid
      df.grid<-spam.shp@polygons[[i]]@Polygons[[1]]@coords
      colnames(df.grid)<-c("x","y")
      bb <- as(raster::extent(min(df.grid[,"x"]), max(df.grid[,"x"]), min(df.grid[,"y"]), max(df.grid[,"y"])), "SpatialPolygons")
      proj4string(bb) <- "+proj=longlat  +datum=WGS84 +no_defs"
      #plot(bb,add=T, col="red")
      
      bb.lulc <- crop(modis.lulc.masked, bb)
      bb.df   <- as.data.frame(bb.lulc)
      
      grid.croplands[[i]] <- apply(bb.df,2,function(x) length(which(x %in% modis.cropLand.pixVal)) )
    }
    
    grid.available.croplands <- grid.croplands %>% do.call(rbind,.)
    
    return(grid.available.croplands)
  }
  
  find_neighbouring_grid <- function( spam.grids, g,available.croplands.year, check.grids)
  {
    df <- data.frame(spam.grids,
                     dist = sqrt( (spam.grids[,1]-spam.grids[check.grids[g],1])^2 + (spam.grids[,2]-spam.grids[check.grids[g],2])^2 ) )
    df$sl <- 1:nrow(df)
    df$available.grids <- available.croplands.year
    df <- df[-check.grids,]
    df <- df[order(df$dist),]
    pos<-which(df$available.grids==0)
    if(length(pos)>0)
      df <- df[-pos,]
    #neighbor.grid <- df$sl[which(df$available.grids > correction.g)[1]]
    
    return(df)
  }
  
  # Function to perform rounding while maintaining the sum
  round_preserve_sum <- function(fn) 
  {
    # Initialize a temp array to store results
    tempArr <- data.frame(
      result = floor(fn),                    # Lower bound
      difference = fn - floor(fn),            # Roundoff error
      index = seq_along(fn)                   # Original index
    )
    
    # Calculate the expected sum
    arraySum <- sum(fn)
    
    # Calculate the lower sum (sum of floored values)
    lowerSum <- sum(tempArr$result)
    
    # Sort the temp array based on roundoff error (difference)
    tempArr <- tempArr[order(tempArr$difference, decreasing = TRUE), ]
    
    # Difference between the actual sum and the lower sum
    difference <- arraySum - lowerSum
    
    # Adjust the results by adding 1 to the elements with the largest roundoff errors
    tempArr$result[1:difference] <- tempArr$result[1:difference] + 1
    
    # Sort the temp array back based on the original indices
    tempArr <- tempArr[order(tempArr$index), ]
    
    # Return the adjusted result
    return(tempArr$result)
  }
  
  correct_cropArea_to_MODIS_cropland <- function( dist.cropPixels , spam.dist, grid.avail.croplands, start.year, end.year)
  {
    
    lapply(1:length(start.year:end.year), 
           function(y) grid.avail.croplands[,y]-apply( dist.cropPixels[[y]][,-c(1:2)], 1, sum)) %>% do.call(cbind,.) -> avail.cropland
    
    if(any(apply(avail.cropland,2,sum)<0))
      stop("Not enough cropland")
    
    #spam grids 
    spam.grids <- spam.dist[,1:2]
    
    dist.cropPixels_c <- list()
    
    for(y in 1:length(start.year:end.year))
    {
      #total number of croplands available in each grid in present year
      available.croplands.year <- avail.cropland[,y]
      
      #croplands for each crop in each grid in present year
      cropland.year <- dist.cropPixels[[y]][,-c(1:2)]
      
      #grids in which cropland assigned is more than required
      check.grids <- which(available.croplands.year<0 )
      
      if(length(check.grids)>0)
      {
        for(g in 1:length(check.grids))
        {
          correction.g <- abs(available.croplands.year[check.grids[g]]) #correction required in that grid
          crops.g      <- cropland.year[check.grids[g],] %>% unlist # cropland assigned to that grid
          
          neighbor.grid.df <- find_neighbouring_grid( spam.grids, g,available.croplands.year, check.grids)
          
          if(any(neighbor.grid.df$available.grids > correction.g))
          {
            neighbor.grid <- neighbor.grid.df$sl[which(neighbor.grid.df$available.grids > correction.g)[1]]
            
            available.croplands.year[neighbor.grid] <- available.croplands.year[neighbor.grid] - correction.g
            
            correction.g.eachCrop <- correction.g * crops.g/sum(crops.g)
            
            crops.g_corrected <- crops.g - correction.g.eachCrop
            
            if( sum(crops.g_corrected %% 1) > 0 )
              crops.g_corrected      <- round_preserve_sum( crops.g_corrected )
            
            cropland.year[check.grids[g],] <- crops.g_corrected
            cropland.year[neighbor.grid,]  <- cropland.year[neighbor.grid,] + (crops.g-crops.g_corrected)
            
            #avail.cropland[check.grids[g],y] <-  0
          }
          else
          {
            neighbor.grid <- neighbor.grid.df$sl[1:which(cumsum(neighbor.grid.df$available.grids)>correction.g)[1]]
            
            correction.done <- 0
            
            for(n in 1:length(neighbor.grid))
            {
              if(n!=length(neighbor.grid))
              {
                to.correct <- available.croplands.year[neighbor.grid[n]]
                correction.done <- correction.done + to.correct
              }
              else
              {
                to.correct <-  correction.g - correction.done
              }
              
              available.croplands.year[neighbor.grid[n]] <- available.croplands.year[neighbor.grid[n]] - to.correct
              
              correction.g.eachCrop <- to.correct * crops.g/sum(crops.g)
              
              crops.g_corrected <- crops.g - correction.g.eachCrop
              
              if( sum(crops.g_corrected %% 1) > 0 )
                crops.g_corrected      <- round_preserve_sum( crops.g_corrected )
              
              cropland.year[neighbor.grid[n],]  <- cropland.year[neighbor.grid[n],] + (crops.g-crops.g_corrected)
              crops.g <- crops.g_corrected
            }
            
            cropland.year[check.grids[g],] <- crops.g_corrected
          }
        }  
      }
      
      dist.cropPixels_c[[y]] <- data.frame(spam.grids,cropland.year)
    }
    
    return(dist.cropPixels_c )
    
  } 
  
  prepare_modis_cropland <- function( dist.shp, modis.lulc, modis.cropLand.pixVal, modisCropland_SPAMgrids, start.year,end.year )
  {
    #converting the SPAM raster into a spatial polygon
    spam.shp <- as( rasterFromXYZ(modisCropland_SPAMgrids[[1]]), 'SpatialPolygons')
    
    #modis lulc grid coordinates
    modis.lulc <- modis.lulc %>% crop( ., st_buffer(dist.shp,dist=10000)) %>% as.data.frame(.,xy=T)
    coordinates( modis.lulc ) =~ x + y
    crs(modis.lulc) <- "+proj=longlat +datum=WGS84"
    
    modis.lulc_dist <- terra::mask( terra::vect(modis.lulc), 
                                    terra::vect(dist.shp)) %>%  as(.,"Spatial")
    rm(modis.lulc)
    
    grid.croplands <- list()
    
    for(i in 1:length(spam.shp))
    {
      #print(i)
      
      #extracting modis LULC info for a specific SPAM grid
      df.grid           <- spam.shp@polygons[[i]]@Polygons[[1]]@coords
      colnames(df.grid) <- c("x","y")
      bb              <- as(raster::extent(min(df.grid[,"x"]), max(df.grid[,"x"]), min(df.grid[,"y"]), max(df.grid[,"y"])), "SpatialPolygons")
      proj4string(bb) <- "+proj=longlat  +datum=WGS84 +no_defs"
      
      bb.lulc    <- terra::mask( terra::vect(modis.lulc_dist), terra::vect(bb)) 
      
      if(  lapply(bb.lulc,function(x) x) %>% unlist %>% length > 0)
      {
        #plot(bb,col="red", lwd=2,add=T)
        bb.lulc    <- bb.lulc %>%   as(.,"Spatial") 
        bb.lulc.df <- bb.lulc@data
        
        #rm(df.grid,bb,bb.lulc)
        
        spam.grid <- lapply( modisCropland_SPAMgrids, function(df) df[i,-c(1:2)]) %>% do.call(rbind,.)
        
        for(c in 1:ncol(spam.grid))
        {
          for(y in 1:length(start.year:end.year))
          {
            pos <- sample(which(bb.lulc.df[,y] %in% modis.cropLand.pixVal),spam.grid[y,c])
            bb.lulc.df[pos,y] <- 100+c
          }
        }
        
        grid.croplands[[i]] <- data.frame(bb.lulc@coords, bb.lulc.df)
      }
    }
    
    cropMap <-  lapply(1:length(start.year:end.year),function(year) 
      lapply(grid.croplands, 
             function(df) df[,c(1,2,(year+2))]) %>% do.call(rbind,.)) 
    
    cropArea<- data.frame( cropMap[[1]][,1:2], lapply( cropMap, function(df)  df[,3]) %>% do.call(cbind,.))
    
    return(cropArea)
  }
  
  prepare_irrigation_map<-function(dist.shp,commandArea,cropArea)
  {
    dist.shp %>%
      st_transform(.,crs(commandArea)) -> dist.shp2
    
    st_agr(dist.shp2) = "constant"
    st_agr(commandArea) = "constant"
    
    st_intersection(commandArea,st_buffer(dist.shp2,dist=10000)) %>%
      st_transform(.,4326) -> commandArea
    
    df<-data.frame(cropArea[,c(1:2)],1)
    
    if(nrow(commandArea)!=0)
    {
      df %>%
        rasterFromXYZ(.,crs = 4326) %>%
        mask(.,commandArea) %>%
        as.data.frame(.,xy=T) ->df
      
      df$coord<-paste0(df$x %>% round(.,5),",",df$y %>% round(.,5))
      cAcoord <-paste0(cropArea$x %>% round(.,5),",",cropArea$y %>% round(.,5))
      
      lapply(1:nrow(cropArea),function(i) which(df$coord==cAcoord[i])) -> pos
      
      df <- df[unlist(pos),]
    }
    else
    {
      df[,3]<-NA
    }
    
    return(df$X1)
  }
  
  #crop statistics for the required required crops(Kharif), district & time period
  dist.cropInfo <- prepare_dist.cropInfo(india.cropStats= india.cropStats,
                                         start.year   =start.year,
                                         end.year     =end.year,
                                         rep.cropNames=rep.cropNames,
                                         dist.name    =dist.name,
                                         state.name   =state.name)
  
  dist.cropInfo <- lapply(dist.cropInfo,check_and_fill_missing_data)
  saveRDS(dist.cropInfo,sprintf("%s/utils/distCropInfo.RDS",dir.save))
  
  #Total copped area(Kharif) for each crop for the provided district (in hectares) for entire time period
  lapply(dist.cropInfo,function(df) df$Area) %>% do.call(rbind,.) %>% t -> dist.cropArea
  colnames(dist.cropArea)<-rep.cropNames 
  rownames(dist.cropArea)<-c(start.year:end.year)
  
  #SPAM2020V1 crop physical area masked to our study area (district)
  spam.dist  <- mask_SPAM_to_district( dist.shp = dist.shp,
                                       gridded.cropPhyData = gridded.cropPhyData)
  modis.lulc %>% 
    .[[1]] %>% 
    crop(., dist.shp) %>% 
    mask(.,dist.shp) %>% 
    raster::area() %>% 
    as.vector %>% 
    mean(.,na.rm=T) *100 -> modis.pixArea #area of each MODIS pixel in ha 
  saveRDS(modis.pixArea,sprintf("%s/utils/gridArea.RDS",dir.save))
  
  #calculate number of MODIS cropland pixels to be assigned for a specific crop in a given year 
  dist.cropPixels <- calculate_MODcrop_pixel( spam.dist     = spam.dist,
                                              dist.cropArea = dist.cropArea,
                                              start.year    = start.year,
                                              end.year      = end.year,
                                              modis.pixArea = modis.pixArea )
  
  #available MODIS cropland pixels in each SPAM grid
  grid.avail.croplands <- calculate_availableCroplands(dist.shp   = dist.shp, 
                                                       spam.dist  = spam.dist,
                                                       modis.lulc = modis.lulc, 
                                                       modis.cropLand.pixVal = modis.cropLand.pixVal)
  #corrected cropland pixels 
  modisCropland_SPAMgrids <- correct_cropArea_to_MODIS_cropland( dist.cropPixels , 
                                                                 spam.dist, 
                                                                 grid.avail.croplands, 
                                                                 start.year, 
                                                                 end.year)
  #dynamic crop maps
  cropArea <- prepare_modis_cropland( dist.shp = dist.shp, 
                                      modis.lulc = modis.lulc ,
                                      modis.cropLand.pixVal = modis.cropLand.pixVal,
                                      modisCropland_SPAMgrids ,
                                      start.year, 
                                      end.year)
  
  df <- cropArea[,-c(1:2)]
  df[df<100] <- NA
  cropArea   <- data.frame( cropArea[,1:2], df) 
  writeRaster(cropArea[,1:3] %>% rasterFromXYZ(),sprintf("%s/utils/cropArea.nc",dir.save))
  
  #irrigation maps
  irrigation.grid <- prepare_irrigation_map(dist.shp,
                                            commandArea,
                                            cropArea)
  
  #removing cropland grids without any growing period
  f<-apply(cropArea[,-c(1:2)],1,function(x) if(length(na.omit(x))>0) return(T) else return(F))
  cropArea<-cropArea[which(f),]
  irrigation.grid<-irrigation.grid[which(f)]
  
  saveRDS(cropArea,       sprintf("%s/utils/cropArea.RDS",dir.save))
  saveRDS(irrigation.grid,sprintf("%s/utils/irrigation.RDS",dir.save))
  
  #check
  print("Relative Error (%) : Rep Crop Area vs Prepared Crop Area")
  rel.err <- (lapply( 100+(1:length(rep.cropNames)), function(c) apply(cropArea[,-c(1:2)],2,function(x) length(which(x==c)))) %>% do.call(cbind,.) * modis.pixArea - dist.cropArea)/dist.cropArea*100
  colnames(rel.err)<-rep.cropNames
  rownames(rel.err)<-start.year:end.year
  print(rel.err)
  
  rm(modis.lulc,dist.shp,gridded.cropPhyData,commandArea,cropArea,irrigation.grid,modis.cropLand.pixVal,modis.pixArea,f,check_and_fill_missing_data,dist.cropArea)
  rm( df,rel.err,prepare_modis_cropland,modisCropland_SPAMgrids,spam.dist,
      correct_cropArea_to_MODIS_cropland,prepare_dist.cropInfo,
      round_preserve_sum,find_neighbouring_grid,grid.avail.croplands,
      calculate_availableCroplands,dist.cropPixels,calculate_MODcrop_pixel,
      dist.cropInfo,india.cropStats,prepare_irrigation_map,mask_SPAM_to_district) 
  
#plots--------------------------------------------------------------------------
  
  
  