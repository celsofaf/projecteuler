euler_024 <- function(){
  p <- 0:9
  n <- length(p)
  
  next.p <- function(p) {
    k <- 0
    for ( i in 1:(n-1) )
      if ( p[i] < p[i+1] && i >= k )
        k <- i
    if ( k == 0 ) return(NULL)  # last permutation
    for ( i in (k+1):n )
      if ( p[k] < p[i] )
        l <- i
    p[c(k, l)] <- p[c(l, k)]
    p[(k+1):n] <- rev(p[(k+1):n])
    return(p)
  }
  
  for ( i in 2:1e6 )
    p <- next.p(p)
  return(p)
}
