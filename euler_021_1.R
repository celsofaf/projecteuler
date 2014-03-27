euler_021_1 <- function() {
  #brute force method
  
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
  
  max_n <- 10000
  sum <- 0
  for ( i in 1:max_n ) {
    di <- sum.divisors(i)
    if ( di > 1 && di <= max_n && sum.divisors(di) == i && di > i )
      sum <- sum + i + di
  }
  return(sum)
}
