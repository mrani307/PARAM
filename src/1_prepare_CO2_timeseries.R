prepare_CO2_timeseries<-function(co2.monthly,ts)
{
  
  #converting monthly CO2 concentration to daily; we use the same value for each day of the month
  co2.daily<-data.frame(date = ts,
                        year = as.numeric(format(ts,"%Y")),
                        month= as.numeric(format(ts,"%m")),
                        co2=NA)
  
  for(i in 1:nrow(co2.monthly))
    co2.daily$co2[which(co2.daily$year==co2.monthly$year[i] & co2.daily$month == co2.monthly$month[i])] <- co2.monthly$average[i]
  
  return(co2.daily$co2)
}


