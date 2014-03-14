fib <- function(n) {
  
  # Generates the n-th term of the Fibonacci sequence.
  # Assuming n >= 0, n integer
  
  if ( n < 2 ) return(n)
  else {
    a1 <- 1
    a2 <- 1
    for ( i in 2:n ) {
      f <- a1 + a2
      a1 <- a2
      a2 <- f
    }
    return(f)
  }
}
