euler_023 <- function() {
  # brute force method
  
  sum.divisors <- function(n) {
    if ( n <= 3 ) return(1)
    sum <- 1
    for ( i in 2:floor(sqrt(n)) ) {
      if ( n %% i == 0 ) {
        sum <- sum + i
        sum <- sum + n / i
        if ( i == sqrt(n) )  # correction
          sum <- sum - i
      }
    }
    return(sum)
  }
  
  is.abundant <- function(n)
    return(sum.divisors(n) > n)
  
  MAX_N <- 28123
  abundants <- which(sapply(1:MAX_N, is.abundant))
  sum.of.abundants <- integer()
  for ( i in abundants )
    sum.of.abundants <- unique(c(sum.of.abundants, i + abundants))
  sum.of.abundants <- sort(sum.of.abundants[sum.of.abundants <= MAX_N])
  return(sum(setdiff(1:MAX_N, sum.of.abundants)))
}
