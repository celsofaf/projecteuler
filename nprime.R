nprime <- function(n){
  
  # calculates the n-th prime number
 
  if ( n == 1 ) return(2)
  if ( n == 2 ) return(3)
  if ( n == 3 ) return(5)
  if ( n == 4 ) return(7)
  
  source("sieve.R")
  primes <- sieve(2*n)
  candidate <- primes[length(primes)] + 2
  repeat {
    np <- length(primes)
    if ( np == n ) return(primes[np])
    if ( prod(candidate %% primes) != 0 )
      primes <- append(primes, candidate)
    candidate <- candidate + 2
  }
}
