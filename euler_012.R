euler_012 <- function(){
  source("n.divisors.R")
  triang <- 0
  i <- 0
  ndiv.odd <- 1
  ndiv.even <- 1 # "even" is actualy half the even number
  ndiv <- 1
  while ( ndiv <= 500 ) {
    i <- i + 1
    if ( i %% 2 == 0 ) {
      ndiv.odd <- n.divisors(i+1)
    } else {
      ndiv.even <- n.divisors((i+1)/2)
    }
    triang <- triang + i
    ndiv <- ndiv.odd * ndiv.even
  }
  return(triang)
}
