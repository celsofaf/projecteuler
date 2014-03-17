euler_009 <- function() {
  for ( a in 1:332 ) {
    for ( b in (a+1):499 ) {
      c <- 1000 - a - b
      if ( c < b || c < a ) break
      if ( a^2 + b^2 == c^2 )
        return(a*b*c)
    }
  }
}
