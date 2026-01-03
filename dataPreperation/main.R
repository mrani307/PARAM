library(sf)
library(raster)
library(dplyr)
library(lubridate)
library(magrittr)
library(fst)
library(readxl)
library(pracma)
library(GWmodel)
library(ncdf4)
library(gstat)
library(glue)
library(terra)

library(parallel)
library(doParallel)
library(foreach)

#set directory------------------------------------------------------------------

  dir.code<-"D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/PARAM_code"
  setwd(dir.code)

#read inputs--------------------------------------------------------------------
  
  #study time period
  start.year<- 2010
  end.year  <- 2019
  
  #name of the district (as per govt statistics)
  dist.name <- "Dahod"    #Hoshiarpur  
  state.name<- "Gujarat"
  
  #name of crops to be modelled
  
  rep.cropNames      <- c("Rice","Jowar", "Maize","Bajra","Groundnut","Cotton(lint)","Sesamum","Soyabean")[c(1,3,8)]  # as per govt report
  gridData.cropNames <- c("RICE", "SORG",  "MAIZ", "PMIL",     "GROU",        "COTT",   "SESA",   "SOYB")[c(1,3,8)]   # as per SPAM2020
  epic.cropNames     <- c("RICE", "GRSG",  "CORN", "PMIL",     "PNUT",        "COTP",   "SESA",    "SOYB")[c(1,3,8)]  # as per EPIC parameter table
  
  dir.save<-sprintf("D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/Districts/%s",dist.name)
  dir.create(dir.save)
  dir.create(sprintf("%s/utils",dir.save))
  
#prepare data inputs for the model----------------------------------------------
  
  source("dataPreperation/prepare_input_file.R")
  source("dataPreperation/prepare_crop_map.R") 
  source("dataPreperation/downscaling_annual_precipitation_GWR.R")
  source("dataPreperation/downscaling_daily_precipitation_RATIO.R")
  source("dataPreperation/prepare_ET_variables.R")
  source("dataPreperation/prepare_soil_information.R")
  source("dataPreperation/prepare_cropParameter_Rfile.R")



