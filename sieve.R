sieve <- function(n) {
  
  # Returns a list with the prime numbers up to n.
  # Assuming n >= 2.
  
  if ( n <= 3 ) return(2:n)
  nums <- seq(from=3, to=n, by=2)
  list <- c(2)
  while ( length(nums) > 0 ) {
    j <- nums[1]
    nums <- nums[nums %% j != 0]
    list <- c(list, j)
  }
  return(list)
}
