
#' calculates PET
#'
#' @param z    elevation (in m)
#' @param temp temperature (in C)
#' @param dpt  dew point temperature (in C)
#' @param Ra   incoming solar radiation (MJ m-2 d-1)
#' @param u10  wind speed at 10m (ms-1)
#'
#' @return ETo on a given day (mm)
calculate_ETo<-function(z,
                        temp,
                        dpt,
                        Ra,
                        lat,
                        doY,
                        u10)
{
  pressure <- 101.3*((293-0.0065*z)/293)^5.26
  psych.c  <- 0.665*10^(-3)*pressure
  
  es  <- 0.6108*exp((17.27*temp)/(temp+237.3))
  ea  <- 0.6108*exp((17.27*dpt) /(dpt+237.3))
  VPD <- es-ea
  del <- 4098*es/((temp+237.3)^2)
  
  Rso <- calculate_Rso(lat,doY,z)
  Rn  <- Ra*(1-0.23) - (4.903*10^(-9)*(temp+273.15)^4)*(0.34-0.14*sqrt(ea))*(1.35*Ra/Rso-0.35)
  
  u2  <- 0.75*u10
  PET <- ((0.408*del*Rn) + psych.c*(900/(temp+273))*u2*VPD)/(del + psych.c*(1+0.34*u2) )
  
  return(PET)
}
