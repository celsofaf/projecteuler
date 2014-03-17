euler_010 <- function() {
  source("sieve.R")
  return(sum(sieve(2e6)))
}
