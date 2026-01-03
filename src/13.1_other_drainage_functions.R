calculate_dSTsdt <- function( ST , FC , SAT , tau )
{
  {
    if( ST <= FC )    
      dSTdt <- 0
    else
    {
      if( ST >= SAT )
        dSTdt <- tau * ( SAT - FC ) * ( ( exp ( SAT - FC ) - 1 ) / ( exp( SAT - FC ) - 1 ) )    
      else  
        dSTdt <- tau * ( SAT - FC ) * ( ( exp ( ST - FC ) - 1 ) / ( exp( SAT - FC ) - 1 ) )  
      
      {
        if( (ST - dSTdt) < FC)
          dSTdt <- ST - FC
      }
    }
  }
  
  return(dSTdt)
}

check_drainability <- function( comp , dSTdt , D.sum , soil)
{
  {
    if(comp==1) prethick <- 0
    else        prethick <- sum( soil$soil.comp$del.z[1:(comp-1)] )
  }
  D.max <- dSTdt * 1000 * prethick
  if( D.sum <= D.max ) return( T )
  else                 return( F )
}

check_for_D.sum<-function( l, D.sum, soil )
{
  if( D.sum > soil$soil.layer$SSKS[l] )
  {
    excess <- D.sum - soil$soil.layer$SSKS[l]
    D.sum  <- soil$soil.layer$SSKS[l]
  }
  else
    excess <- 0
  
  return( list( excess = excess,
                D.sum  = D.sum ) )
}

calculate_STx<-function( l , dSTdt , soil)
{
  if( dSTdt <= 0 )
    STx <- soil$soil.layer$FC[l]
  else if ( soil$soil.layer$tau[l] > 0 )
  {
    A = 1+( ( dSTdt *( exp( soil$soil.layer$SAT[l] - soil$soil.layer$FC[l] ) - 1 ) )/
              ( soil$soil.layer$tau[l] * ( soil$soil.layer$SAT[l] - soil$soil.layer$FC[l] ) ) )
    STx = soil$soil.layer$FC[l] + log(A)
  }
  else
    STx = soil$soil.layer$SAT[l] + 0.01
  
  return(STx)
}