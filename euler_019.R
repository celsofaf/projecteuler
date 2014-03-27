euler_019 <- function() {
  firsts <- c()
  for ( year in 1901:2000 )
    for ( month in 1:12 )
      firsts <- c(firsts, paste(as.character(year), as.character(month), "01", sep="-"))
  firsts <- weekdays(as.Date(firsts))
  n_sundays <- length(firsts[firsts == "domingo"])
  return(n_sundays)
}
