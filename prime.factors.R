prime.factors <- function(n, sieve=NULL) {
  
  # Returns a list containing two vectors:
  # list$primes : prime divisors of n
  # list$exp    : multiplicative index of the corresponding prime divisor
  #
  # May receive an already calculated sieve as input.
  # It's enough to use the sieve up to sqrt(n), as there is at most one prime divisor
  # larger than that.
  
  source("sieve.R")
  
  if ( is.null(sieve) ) sieve <- sieve(sqrt(floor(n)))
  if ( max(sieve) < sqrt(floor(n)) ) sieve <- sieve(sqrt(floor(n)))
  list <- list()
  list$primes <- sieve[n %% sieve == 0]
  list$exp <- numeric()
  for ( p in list$primes ){
    i <- 0
    k <- n
    while ( k %% p == 0 ) {
      k <- k / p
      i <- i + 1
    }
    list$exp <- c(list$exp, i)
  }
  pow <- list$primes^list$exp
  if ( length(pow) == 0 ) return(list(primes = n, exp = 1))  # is a prime number
  if ( n != prod(pow) ) {
    n <- n / prod(pow)   # now, either n is prime or a power of a prime
    p <- sieve(n)
    p <- p[n %% p == 0]  # there is only 1
    i <= 1
    while ( n != 1 ) {
      n <- n / p
      i <- i + 1
    }
    list$primes <- c(list$primes, p)
    list$exp <- c(list$exp, i)
  }
  return(list)
}
