sum.v <- function(a, b) {
  
  # Sums (positive) integers a and b, represented as vectors of digits.
  # Returns a*b as a vector of digits.
  
  while ( length(a) < length(b) )
    a <- c(0, a)
  while ( length(b) < length(a) )
    b <- c(0, b)
  la <- length(a)  # they are "the same"
  a <- rev(a)
  b <- rev(b)
  x <- integer()
  carry <- 0
  for ( i in 1:la ) {
    if ( a[i] + b[i] + carry < 10 ) {
      x <- c(x, a[i] + b[i] + carry)
      carry <- 0
    } else {
      x <- c(x, a[i] + b[i] + carry - 10)
      carry <- 1
    }
  }
  if ( carry == 1 )
    x <- c(x, 1)
  return(rev(x))
}
