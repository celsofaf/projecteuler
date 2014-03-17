euler_008 <-function() {
  number <- readLines("number_008.txt")
  n <- character()
  for ( i in number )
    n <- paste(n, i, sep="")
  prod <- 0
  for ( i in 5:nchar(n) ) {
    p <- prod(as.numeric(strsplit(substr(n, i-4, i), "")[[1]]))
    if ( p > prod )
      prod <- p
  }
  return(prod)
}
