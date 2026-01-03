#'PARAM : Pixel based Agro-hydrological model for water Accounting and Management
#'
#'developed by Aniruddha Saha, 2025
#'
#'this chunk of code runs the PARAM model on Linux interface with HPC using OpenMPI 


suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(parallel)))

#SET MANUALLY-------------------------------------------------------------------

  #PARAM source code locations (src folder)
  dir.code <- "D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/PARAM_code" 
  #location where the prepared model inputs are stored
  dir.save <- "D:/Aniruddha/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/Districts/Sehore"

#RUN MODEL----------------------------------------------------------------------  
    
  #function to run model on each grid
  PARAM <- function(grid)
  {
    #reading model inputs
    source(paste0(dir.save,"/inputs.R"))
    
    #RUN MODEL
    setwd(dir.code)
    source("src/run_eachGrid.R")
    PARAM.res<-run_eachGrid(grid=grid,dir.save=dir.save,dir.code=dir.code,
                            start.year=start.year,end.year=end.year,
                            epic.cropNames = epic.cropNames)
    
    return(PARAM.res)
  }
  
  N          <- nrow(readRDS(sprintf("%s/utils/cropArea.RDS",dir.save)))
  crop_grids <- 1:N
  chunks     <- split(crop_grids, cut(seq_along(crop_grids), 500, labels = FALSE))
  
  #start parallel
  my.cluster <- parallel::makeCluster(50,type = "PSOCK")
  print(my.cluster)
  doParallel::registerDoParallel(cl = my.cluster)
  
    sim.res<-foreach::foreach(ch = 1:length(chunks), .combine = 'c' ) %dopar% 
      {
        
        suppressWarnings(suppressMessages(library(dplyr)))
        suppressWarnings(suppressMessages(library(magrittr)))
        suppressWarnings(suppressMessages(library(fst)))
        suppressWarnings(suppressMessages(library(readxl)))
        
        my_chunk    <- chunks[[ch]]
        result_list <- lapply(my_chunk, PARAM)
        saveRDS(result_list, file = sprintf("%s/Results/gridOutput_%02d.RDS",dir.save,ch))
      }
  
  #end parallel
  foreach::registerDoSEQ()
  parallel::stopCluster(cl = my.cluster)
  
