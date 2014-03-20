mult.v <- function(a, b) {
  
  # Multiplies (positive) integers a and b, represented as vectors of digits.
  # Returns a*b as a vector of digits.
  
  source("sum.v.R")
  a <- rev(a)
  b <- rev(b)
  la <- length(a)
  lb <- length(b)
  x <- integer()
  for ( j in 1:lb ) {
    xtmp <- integer()
    carry <- 0
    for ( i in 1:la ) {
      if ( a[i] * b[j] + carry < 10 ) {
        xtmp <- c(xtmp, a[i] * b[j] + carry)
        carry <- 0
      } else {
        xtmp <- c(xtmp, (a[i] * b[j] + carry) %% 10)
        carry <- (a[i] * b[j] + carry) %/% 10
        if ( i == la && carry > 0 )
          xtmp <- c(xtmp, carry)
      }
    }
    zeroes <- j - 1
    while ( zeroes > 0 ) {
      xtmp <- c(0, xtmp)
      zeroes <- zeroes - 1
    }
    x <- sum.v(x, rev(xtmp))
  }
  return(x)
}
