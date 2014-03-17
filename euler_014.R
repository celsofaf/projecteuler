euler_014 <- function() {
  next.collatz <- function(n) {
    if ( n %% 2 == 0 )
      return(n/2)
    else
      return(3*n + 1)
  }
  
  collatz.count <- function(n) {
    i <- 1
    while ( n > 1 ) {
      n <- next.collatz(n)
      i <- i + 1
    }
    return(i)
  }
  
  library("parallel")
  max_start <- 1e6
  start <- seq(from=1, to=max_start, by=2)
  counts <- mcmapply(collatz.count, start)
  max.count <- max(counts)
  collatz <- -1 + 2 * which(counts == max.count)
  print(collatz)
}
