calculate_Rso<-function(lat,DoY,z){
  #unit conversion of inputs
  lat<-lat*pi/180 #converting latitude in radian
  #inverse relative distance
  dr<-1+0.033*cos(2*pi/365*DoY)
  #solar declination
  del<-0.409*sin(2*pi*DoY/365-1.39)
  #sunset hour angle
  ws<-acos(-tan(lat)*tan(del))
  #extraterrestial radiation (Ra)
  Ra<-24*60*0.0820*dr/pi*(ws*sin(lat)*sin(del)+cos(lat)*cos(del)*sin(ws))
  
  #clear sky radiation (Rso in MJ m-2 day-1)
  Rso<-(0.75+2*10^(-5)*z)*Ra
  
  #return
  return(Rso)
}