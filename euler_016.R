euler_016 <- function() {
  source("mult.v.R")
  MAX_EXP <- 1000
  number <- 2
  for ( i in 2:MAX_EXP )
    number <- mult.v(number, 2)
  print(rev(number))
  return(sum(number))
}
