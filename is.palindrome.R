is.palindrome <- function(n) {
  
  # Checks whether n (integer) is palindrome or not
  
  num <- n
  rev = 0;
  while ( num > 0 ) {
    dig = num %% 10;
    rev = rev * 10 + dig;
    num = num %/% 10;
  }
  return ( n == rev )
}
