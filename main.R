#'PARAM : Pixel based Agro-hydrological model for water Accounting and Management
#'
#'developed by Aniruddha Saha, 2025
#'
#'this chunk of code runs the PARAM model on Linux interface with HPC using OpenMPI 


library(pbdMPI)

# Initialize MPI
init()

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(fst)))
suppressWarnings(suppressMessages(library(readxl)))

#SET MANUALLY-------------------------------------------------------------------

  #PARAM source code locations (src folder)
  dir.code <- "C:/Users/merul/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/PARAM_code" 
  #location where the prepared model inputs are stored
  dir.save <- "C:/Users/merul/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/Districts/Mysore"

#RUN MODEL----------------------------------------------------------------------  
    
  #function to run model on each grid
  PARAM <- function(grid)
  {
    #reading model inputs
    source(paste0(dir.save,"/inputs.R"))
    
    #RUN MODEL
    source("src/run_eachGrid.R")
    PARAM.res<-run_eachGrid(grid=grid,dir.save=dir.save,dir.code=dir.code,
                            start.year=start.year,end.year=end.year,
                            epic.cropNames = epic.cropNames)
    
    return(PARAM.res)
  }
  
  # Full input range
  N <- nrow(readRDS(sprintf("%s/utils/cropArea.RDS",dir.save)))
  crop_grids <- 1:N 
  rank <- comm.rank()
  size <- comm.size()
  
  # Split the workload among ranks
  chunks   <- split(crop_grids, cut(seq_along(crop_grids), size, labels = FALSE))
  my_chunk <- chunks[[rank + 1]]  # +1 because R is 1-based
  
  # Apply the function to your chunk
  result_list <- lapply(my_chunk, PARAM)
  
  # Save the result
  saveRDS(result_list, file = sprintf("%s/Results/gridOutput_%02d.RDS",dir.save,rank))
  
#PREPARE RESULTS----------------------------------------------------------------
  
# Finalize MPI
finalize()
  
