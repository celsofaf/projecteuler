prime.factors <- function(n, sieve=NULL) {
  
  # Returns a list containing two vectors:
  # list$primes : prime divisors of n
  # list$exp    : multiplicative index of the corresponding prime divisor
  #
  # May receive an already calculated sieve as input
  
  if ( is.null(sieve) ) {
    source("sieve.R")
    sieve <- sieve(n)
  }
  list <- list()
  list$primes <- sieve[n %% sieve == 0]
  list$exp <- numeric()
  for ( p in list$primes ){
    i <- 0
    k <- n
    repeat {
      k <- k / p
      i <- i + 1
      if ( k %% p != 0 ) break
    }
    list$exp <- c(list$exp, i)
  }
  return(list)
}
