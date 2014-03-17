n.divisors <- function(n) {
  
  # returns the number of positive divisors of n >= 1
  
  if ( n == 1 ) return(1)
  
  source("prime.factors.R")
  factors <- prime.factors(n)
  ndiv <- 1
  for ( i in factors$exp )
    ndiv <- ndiv * (i+1)
  return(ndiv)
}
