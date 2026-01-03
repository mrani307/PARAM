dir.code<-"C:/Users//merul/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/PARAM_code"
setwd(dir.code)
rm(dir.code)
start.year <- 2001
end.year   <- 2019
dist.name  <- "Mysore"
epic.cropNames <- c("RICE","GRSG", "PMIL","CORN", "PNUT",     "COTP")
dir.save       <- sprintf("C:/Users//merul/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/Districts/%s",dist.name)
# 
# dir.code<-"D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/PARAM_code"
# setwd(dir.code)
# rm(dir.code)
# start.year <- 2001
# end.year   <- 2019
# dist.name  <- "Mysore"
# epic.cropNames <- c("RICE","GRSG", "PMIL","CORN", "PNUT", "COTP")
# dir.save       <- sprintf("D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/Districts/%s",dist.name)
# grid<-100

###-------------------------------------------------------------------------------

run_eachGrid<-function(grid,epic.cropNames,dir.code,dir.save,start.year,end.year)
{
  
  setwd(dir.code)
#crop model for a grid for the entire time period
  
#*******************************************************************************
#**********************reading inputs*******************************************
#*******************************************************************************
  
  cropArea <- readRDS(sprintf("%s/utils/cropArea.RDS",dir.save))
  
  #reading the crops grown for kharif season of each year
  grid.crop  <-  unlist(cropArea[grid,3:ncol(cropArea),drop=T]) #represented by numbers : in order of epic.cropNames
  names(grid.crop)<-start.year:end.year
  
  #reading irrigation information for the grid
  grid.irrigation <- readRDS(sprintf("%s/utils/irrigation.RDS",dir.save))[grid] # 1: canal irrigation available ; 0 : no canal irrigation available ; -1 : no irrigation available
  grid.irrigation[is.na(grid.irrigation)]<-0
  
  #for ETo calculation
  lat    <- cropArea[grid,2] #latutude 
  ts     <- seq(as.Date(paste0(start.year,"-01-01")),as.Date(paste0(end.year,"-12-31")),1)
  ts.doY <- as.POSIXlt(ts)$yday + 1
  z      <- readRDS(sprintf("%s/utils/elevation.RDS",dir.save))[grid]
  
  #reading input variable for entire time period (time series inputs)
  read_var_timeseries <- function(varname) 
    do.call(c, lapply(start.year:end.year, function(year) unname(unlist(read.fst(sprintf("%s/utils/%s%d.fst", dir.save, varname, year), from=grid, to=grid)))) )
  
  ts.precip <- read_var_timeseries("precip")
  ts.temp   <- read_var_timeseries("temp")
  ts.ssrd   <- read_var_timeseries("ssrd")
  ts.dpt    <- read_var_timeseries("dpt")
  ts.wind   <- read_var_timeseries("wind")
  
  ts.temp   <- ts.temp - 273.15 #converting Kelvin to Celcius
  ts.dpt    <- ts.dpt - 273.15  #converting Kelvin to Celcius
  ts.ssrd   <- ts.ssrd/10^6     #converting Jm-2 to MJm-2
  
  source("src/1_prepare_CO2_timeseries.R")
  ts.co2    <- prepare_CO2_timeseries(read.csv("data_India/co2_mm_gl.csv",sep = ",")[265:504,],ts)
  
  #reading soil input (static input)
  soil.layer     <- list(SAT   = unlist(read.fst(sprintf("%s/utils/saturationCapacity.fst",dir.save),    from=grid,to=grid)), 
                         SSKS  = unlist(read.fst(sprintf("%s/utils/hydraulicConductivity.fst",dir.save), from=grid,to=grid)) * 240, #converting cm hr-1 to mm d-1
                         FC    = unlist(read.fst(sprintf("%s/utils/fieldCapacity.fst",dir.save),         from=grid,to=grid)),  
                         WP    = unlist(read.fst(sprintf("%s/utils/wiltingPoint.fst",dir.save),          from=grid,to=grid)),
                         SAN   = unlist(read.fst(sprintf("%s/utils/perSand.fst",dir.save),               from=grid,to=grid)),
                         BD    = unlist(read.fst(sprintf("%s/utils/bulkDensity.fst",dir.save),           from=grid,to=grid)),
                         del.z = c(5,10,15,30,40,100)/100  ) #soil depths of the each layer
  soil.layer$tau <- 0.0866 * soil.layer$SSKS^0.35            #dimensionless drainage characteristices (AquaCrop ; Barrios Gonzales, 1999)
  soil           <- list(soil.layer = soil.layer,    
                         soil.comp  = list(layer = c(1,2,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,6), 
                                           del.z = rep(0.1,20)), #soil compartments (for soil drainage)
                         zCN        = 0.3,  #depth of soil top layer
                         CN         = 75 )  #run-off curve number
  
#*******************************************************************************
#*******************reading crop information************************************
#*******************************************************************************
  
  #reading the crop parameters 
  source("src/2_read_cropParameters.R")
  source(sprintf("%s/crop_param_generated.R",dir.save))
  
  #if the root length is greater than 2m setting it to 1.99m
  for(i in 1:length(crop.param))
    if(crop.param[[i]][["RDMX"]]>=2)
      crop.param[[i]][["RDMX"]]<-1.99
  
  #reading the crop Yield timeseries for each crop for the district (for scaling HI and DMLA parameters)
  dist.CropInfo  <- readRDS(sprintf("%s/utils/distCropInfo.RDS",dir.save))
  dist.cropYeild <- do.call(cbind,lapply(dist.CropInfo,function(df) df[,"Yeild"])) 
  
  #gapfilling the missing crop yield values using nearest neighbour method
  fill_na_nearest <- function(x) {
    if (!anyNA(x)) return(x)
    
    # Find indices of NA and non-NA
    na_idx <- which(is.na(x))
    non_na_idx <- which(!is.na(x))
    
    # Loop over NA positions and assign nearest non-NA value
    for (i in na_idx) {
      # Compute distances to all non-NA values
      distances <- abs(non_na_idx - i)
      nearest <- non_na_idx[which.min(distances)]
      x[i] <- x[nearest]
    }
    
    return(x)
  }
  dist.cropYeild <- apply( dist.cropYeild,2,fill_na_nearest)
  
  #scaling the parameters (DMLA and HI) using observed crop yield
  scaleParameters <- function(data, a, b)   {
    x_min <- min(data)
    x_max <- max(data)
    scaled_data <- a + ((data - x_min) / (x_max - x_min)) * (b - a)
    return(scaled_data)
  }
  for(i in 1:length(epic.cropNames))
  {
    crop.param[[i]][["DMLA"]] <- scaleParameters(dist.cropYeild[,i],crop.param[[i]][["a_DMLA"]],crop.param[[i]][["b_DMLA"]])
    crop.param[[i]][["HI"]]   <- scaleParameters(dist.cropYeild[,i],crop.param[[i]][["a_HI"]],  crop.param[[i]][["b_HI"]])
  }
  
  for(i in 1:length(crop.calender)) names(crop.calender)[i]<-as.character(100+i)
  cropName_fromID <- epic.cropNames
  for(i in 1:length(crop.calender)) names(cropName_fromID)[i]<-as.character(100+i)
  
#*******************************************************************************
#*******************pre-processing for crop model ******************************
#*******************************************************************************
  
  #preparing a data.frame with crop ID and corresponding day of the season for whole time-period
  source("src/3_prepare_grid.LU.R")
  lu        <- prepare_grid.LU( grid.crop,crop.calender,start.year,end.year )  
  crop.info <- data.frame ( year    = as.numeric(format(ts,"%Y")),
                            crop_id = floor(lu) ,
                            doS     = (lu %% 1)*1000 )
  #assigning crop name to crop ID  
  crop.info$crop_name<- cropName_fromID[as.character(crop.info$crop_id)]
  
  #identifying cropping seasons
  rle_vals <- rle(!is.na(crop.info$crop_id))
  ends     <- cumsum(rle_vals$lengths)
  starts   <- ends - rle_vals$lengths + 1
  start_of.season <- starts[which(rle_vals$values)]
  end_of.season   <- ends[which(rle_vals$values)]
  crop.info$season<-NA
  for(l in 1:length(start_of.season))
    crop.info$season[start_of.season[l]:end_of.season[l]]<-l
  
  #calculate PET
  source("src/12.1_calculate_Rso.R")
  source("src/12.2_calculate_ETo.R")
  source("src/12.3_calculate_PET.R")
  
  crop.info <- calculate_PET(crop.info,epic.cropNames,z,ts.temp,ts.dpt,ts.ssrd,lat,ts.doY,ts.wind)
  
  #EPIC crop model(those which aren't stress dependent)---------------------------
  
  source("src/4_calculate_HU.R")
  source("src/5_calculate_PHU.R")
  source("src/6_calculate_HUI.R")
  source("src/7_calculate_HUF.R")
  source("src/8_calculate_RUE.R")
  source("src/9_calculate_X1.R")
  source("src/10_calculate_RD.R")
  source("src/11_calculate_CHT.R")
  
  read_cropPARAM_for_eachDay<-function(paramName)
    vapply(crop.info$crop_name, 
           function(cropName) ifelse(is.na(cropName), NA_real_,crop.param[[cropName]][[paramName]]), 
           numeric(1))
  
  #calculate Heat Units
  calculate_HU(TMP  = ts.temp,
               TBSC = read_cropPARAM_for_eachDay("TBS")) -> crop.info$HU  
  
  
  #calculate PHU (potential heat units required for maturity)
  PHU <- calculate_PHU(crop.info = crop.info) 
  names(PHU)<-vapply(na.omit(grid.crop), function(cropId) cropName_fromID[which(as.numeric(names(cropName_fromID))==cropId)], character(1))
  
  #calculate HUI (heat unit index)
  calculate_HUI(crop.info=crop.info,PHU=PHU) -> crop.info$HUI 
  
  #calculate HUF
  calculate_HUF(HUI = crop.info$HUI, 
                ah1 = read_cropPARAM_for_eachDay("DLAP1"), 
                ah2 = read_cropPARAM_for_eachDay("DLAP2")) -> crop.info$HUF 
  
  #calculate RUE
  calculate_RUE(WAC2 = read_cropPARAM_for_eachDay("WAC2") ,
                WA   = read_cropPARAM_for_eachDay("WA"),
                CO2  = ts.co2) -> crop.info$RUE 
  
  #calculate X1  (for calculation of daily potential increase in biomass)
  calculate_X1(TMP  = ts.temp, 
               TDEW = ts.dpt) -> crop.info$X1  
  
  #calculate root depth (m)
  calculate_RD(HUI  = crop.info$HUI, 
               RDMX = read_cropPARAM_for_eachDay("RDMX")) -> crop.info$RD
  
  #calculate root depth (m)
  calculate_CHT(HUF  = crop.info$HUF, 
                HMX =  read_cropPARAM_for_eachDay("HMX")) -> crop.info$CHT

  #runModel-----------------------------------------------------------------------
  
  source("src/13.1_other_drainage_functions.R")
  source("src/13_update_drainage.R")
  source("src/14_rainfall_partition.R")
  source("src/15.1_rootZone.R")                       
  source("src/15_calculate_irrigation.R")              
  source("src/16_infiltration.R")                     
  source("src/17_calculate_RGF.R")                     
  source("src/18.1_RWU_model.R")                      
  source("src/18.2_calculate_SWS.R")                  
  source("src/18.3_calculate_WU.R")                   
  source("src/19.1_calculate_EvapLayerWaterContent.R") 
  source("src/19_calculate_soilEvap.R")               
  source("src/20_calculate_REG.R")                    
  source("src/21.1_calculate_LAI_growing.R")           
  source("src/21.2_calculate_LAI_decline.R")          
  source("src/21_calculate_LAI.R")                     
  source("src/22_calculate_DDM.R")                    
  source("src/23_calculate_root_growth.R")             
  source("src/24_calculate_yeild.R") 
  source("src/runModel.R")
  
  
  grid.output <- runModel(crop.info   = crop.info,
                          grid.crop   = grid.crop, 
                          crop.param  = crop.param,
                          P.grid      = ts.precip,
                          Temp.grid   = ts.temp,
                          RA.grid     = ts.ssrd,
                          WS.grid     = ts.wind,
                          DPT.grid    = ts.dpt,
                          CO2.grid    = ts.co2,
                          ETo.grid    = crop.info$ETo,
                          PET.grid    = crop.info$PET,
                          soil        = soil,
                          start.year  = start.year,
                          end.year    = end.year,
                          no.of.days  = length(ts),
                          z           = z,
                          lat         = lat,
                          doY         = ts.doY,
                          bund.height = 0.20,
                          irr.grid    = grid.irrigation,
                          start_of.season = start_of.season,
                          end_of.season   = end_of.season,
                          Irr.AppEff.irrGrid    = 80,
                          Irr.AppEff.nonIrrGrid = 80)
  return(grid.output)
}
                          
  
  
  # crop.info   = crop.info
  # crop.param  = crop.param
  # P.grid      = ts.precip
  # Temp.grid   = ts.temp
  # RA.grid     = ts.ssrd
  # WS.grid     = ts.wind
  # DPT.grid    = ts.dpt
  # CO2.grid    = ts.co2
  # ETo.grid    = crop.info$ETo
  # PET.grid    = crop.info$PET
  # soil        = soil
  # start.year  = start.year
  # end.year    = end.year
  # no.of.days  = length(ts)
  # z           = z
  # lat         = lat
  # doY         = ts.doY
  # calc.PHU    = T
  # bund.height = 0.20
  # irr.grid    = 1
  # start_of.season = start_of.season
  # end_of.season   = end_of.season
  # Irr.AppEff.irrGrid    = 80
  # Irr.AppEff.nonIrrGrid = 80
