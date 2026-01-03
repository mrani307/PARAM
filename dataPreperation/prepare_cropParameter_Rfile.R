generate_crop_param_file <- function(crops,
                                     output_file = sprintf("%s/crop_param_generated.R",dir.save)) 
  {
  
  # Initialize lines
  lines <- c()
  
  # Header
  lines <- c(lines, "#crop calender (specify the day of the year)------------------------------------", "")
  
  # Crop calendar block
  lines <- c(lines, "crop.calender  <- list(")
  cal_entries <- sapply(seq_along(crops), function(i) {
    crop <- crops[i]
    paste0('  "', crop, '" = NA', ifelse(i < length(crops), ",", ""))
  })
  lines <- c(lines, cal_entries, ")", "")
  
  # Crop parameters read block
  lines <- c(lines,
             "#crop parameters----------------------------------------------------------------",
             "",
             "#read crop parameter file",
             'crop.param.file <- read.csv("data_India/cropParameters.csv")',
             "",
             "crop.param <- list(")
  param_entries <- sapply(seq_along(crops), function(i) {
    crop <- crops[i]
    paste0('  "', crop, '" = read_cropParameters(crop.param.file,"', crop, '")',
           ifelse(i < length(crops), ",", ""))
  })
  lines <- c(lines, param_entries, ")", "", "#edit crop param", "")
  
  # Helper to write parameter assignments
  add_param_block <- function(param_name, crops) {
    sapply(crops, function(crop) {
      paste0('  crop.param[["', crop, '"]][["', param_name, '"]] <- NA')
    })
  }
  
  # Add blocks
  lines <- c(lines, "#DMLA", "  #a", add_param_block("a_DMLA", crops))
  lines <- c(lines, "  #b", add_param_block("b_DMLA", crops))
  
  lines <- c(lines, "#HI", "  #a", add_param_block("a_HI", crops))
  lines <- c(lines, "  #b", add_param_block("b_HI", crops))
  
  lines <- c(lines, "#WSYF", add_param_block("WSYF", crops))
  
  lines <- c(lines, "#RDMX", add_param_block("RDMX", crops))
  
  lines <- c(lines, "#PHU", add_param_block("PHU", crops))
  
  # Write to file
  writeLines(lines, con = output_file)
  message("File written to ", output_file)
}

generate_crop_param_file(crops=epic.cropNames)

