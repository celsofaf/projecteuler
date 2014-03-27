euler_021_2 <- function() {
  # a clever method
  
  max_n <- 10000
  source("sieve.R")
  source("prime.factors.R")
  thesieve <- sieve(max_n)
  
  sum.divisors <- function(n) {  # based on the sigma function of number theory
    if ( n <= 1 ) return(1)
    p <- prime.factors(n, thesieve)
    sigma <- 1
    for ( i in 1:length(p$primes) ) {
      sum <- 1
      for ( j in 1:p$exp[i] )
        sum <- sum + p$primes[i]^j
      sigma <- sigma*sum
    }
    return(sigma - n)
  }
  
  sum <- 0
  for ( i in 1:max_n ) {
    di <- sum.divisors(i)
    if ( di > 1 && di <= max_n && sum.divisors(di) == i && di > i)
      sum <- sum + i + di
  }
  return(sum)
}
