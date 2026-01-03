# Format crop name vectors as R character vectors
rep_crops      <- paste(sprintf('"%s"', rep.cropNames), collapse = ", ")
gridData_crops <- paste(sprintf('"%s"', gridData.cropNames), collapse = ", ")
epic_crops     <- paste(sprintf('"%s"', epic.cropNames), collapse = ", ")

# Escape backslashes in dir.save
dir.save.escaped <- gsub("\\\\", "/", dir.save)

# Generate the R script content
script_content <- sprintf('
#study time period
start.year <- %d
end.year   <- %d

#name of the district (as per govt statistics)
dist.name      <- "%s"     
state.name     <- "%s"

#name of crops to be modelled
rep.cropNames      <- c(%s)  # as per govt report
gridData.cropNames <- c(%s)  # as per SPAM2020
epic.cropNames     <- c(%s)  # as per EPIC parameter table

dir.save <- "%s"
',start.year,end.year,dist.name, state.name,rep_crops,gridData_crops,epic_crops,dir.save.escaped)

# Write to the file
writeLines(script_content,sprintf("%s/inputs.R",dir.save))

