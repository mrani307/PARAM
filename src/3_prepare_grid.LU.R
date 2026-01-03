prepare_grid.LU <- function(grid.crop,crop.calender,start.year,end.year)
{
  unlist(
    lapply(start.year:end.year, function(year) 
      {
        crop.year<-grid.crop[[as.character(year)]]
        df<-data.frame( ts = seq( as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), 1 ),
                        lu = NA )
        if(!is.na(crop.year))
        {
          doY <- crop.calender[[as.character(crop.year)]]
          df$lu[ doY  ] <- crop.year + (1:length(doY))/1000  #saving as 100.001 ; crop no : 100, dayofseason =1
        }
        return(df$lu)
      }
      )
    )
}