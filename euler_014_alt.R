euler_014_alt <- function() {
  next.collatz <- function(n) {
    if ( n %% 2 == 0 )
      return(n/2)
    else
      return(3*n + 1)
  }
  
  max.n <- 1e6
  collatz <- c(1, rep(0, times=max.n-1))
  for ( n in 2:max.n ){
    i <- 0
    j <- 0
    k <- n
    repeat {
      i <- i + 1
      k <- next.collatz(k)
      if ( k < max.n )
        j <- collatz[k]
      if ( j > 0 ){
        i <- i + j
        collatz[n] <- i
        break
      }
    }
  }
  max.collatz <- max(collatz)
  return(match(max.collatz, collatz))
}
