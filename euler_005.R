euler_005 <- function() {
  TARGET <- 20
  sieve <- sieve(TARGET)
  prime.powers <- numeric()
  for ( n in sieve ) {
    p <- 1
    while ( p * n <= TARGET ) p <- p * n
    prime.powers <- c(prime.powers, p)
  }
  return(prod(prime.powers))
}
