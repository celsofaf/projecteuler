euler_021_2 <- function() {
  # a (possibly more) clever method
  
  MAX_N <- 10000
  source("sieve.R")
  source("prime.factors.R")
  thesieve <- sieve(floor(sqrt(MAX_N)))
  
  sum.divisors <- function(n) {  # based on the sigma function of number theory
    if ( n <= 1 || n %in% thesieve ) return(1)
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
  sum.div <- sapply(1:MAX_N, sum.divisors)
  for ( i in 1:MAX_N )
    if ( sum.div[i] > 1 && sum.div[i] <= MAX_N && sum.div[i] > i )
      if ( sum.div[sum.div[i]] == i )
        sum <- sum + i + sum.div[i]
  return(sum)
}
