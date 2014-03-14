euler_004 <- function(){
  source("is.palindrome.R")
  largest <- 0
  for ( i in 100:999 ) {
    for ( j in 100:i ) {
      prod <- i*j
      if ( prod > largest && is.palindrome(prod) ) largest <- prod
    }
  }
  return(largest)
}
