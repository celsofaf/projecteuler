euler_021_1 <- function() {
  #brute force method

  MAX_N <- 10000
  
  sum.divisors <- function(n) {
    if ( n %in% c(1, 2, 3, 5, 7) ) return(1)
    sum <- 1
    for ( i in seq(2+n%%2, floor(sqrt(n)), by=1+n%%2) ) {
      if ( n %% i == 0 ) {
        sum <- sum + i
        sum <- sum + n / i
        if ( i == sqrt(n) )  # correction
          sum <- sum - i
      }
    }
    return(sum)
  }
  
  sum <- 0
  for ( i in 1:MAX_N ) {
    di <- sum.divisors(i)
    if ( di > 1 && di <= MAX_N && sum.divisors(di) == i && di > i )
      sum <- sum + i + di
  }
  return(sum)
}
