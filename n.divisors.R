n.divisors <- function(n) {
  
  # returns the number of positive divisors of n >= 1

  source("prime.factors.R")
  source("sieve.R")
  if ( n == 0 ) return(0)
  if ( n <= 3 ) return(1)
  sieve <- sieve(floor(sqrt(n)))
  factors <- prime.factors(n, sieve)
  ndiv <- 1
  for ( i in factors$exp )
    ndiv <- ndiv * (i+1)
  return(ndiv)
}
