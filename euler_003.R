euler_003 <- function() {
  source("sieve.R")
  N <- 600851475143
  factors <- prime.factors(N)
  return(max(factors$primes))
}
