sieve <- function(n) {
  
  # Returns a list with the prime numbers up to n.
  # Assuming n >= 2.
  
  if ( n <= 3 ) return(2:n)
  nums <- c(0, 2:n)
  for ( i in 2:floor(sqrt(n)) )
    if ( nums[i] )
      for ( j in 2:(n %/% i) )
        nums[i * j] <- 0
  return(nums[nums])
}
