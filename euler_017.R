euler_017 <- function() {  
  library("stringr")
  names <- character(1000)
  names[1:10] <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  names[11:19] <- c("eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  names[c(20, 30, 40, 50, 60, 70, 80, 90)] <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  for ( i in 21:99 )
    if ( i %% 10 != 0 )
      names[i] <- paste(names[i - i %% 10], names[i %% 10])
  for ( i in 100:999 ){
    names[i] <- paste(names[i %/% 100], "hundred")
    if ( i %% 100 != 0 )
      names[i] <- paste(names[i], "and", names[i %% 100])
  }
  names[1000] <- "one thousand"
  names <- str_replace_all(names, " ", "")
  return(sum(nchar(names)))
}
