euler_020 <- function() {
  source("mult.v.R")
  fact <- 2
  for ( n in 3:9 )
    fact <- mult.v(fact, n)
  for ( n in 10:99 )
    fact <- mult.v(fact, c(n%/%10, n%%10))
  fact <- mult.v(fact, c(1,0,0))
  return(sum(fact))
}
