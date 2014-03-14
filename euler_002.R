euler_002 <- function() {
  a1 <- 1
  a2 <- 1
  sum <- 0
  repeat {
    b <- a1 + a2
    if ( b > 4e6 ) break
    if ( b %% 2 == 0) sum <- sum + b
    a1 <- a2
    a2 <- b
  }
  return(sum)
}
