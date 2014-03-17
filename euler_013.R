euler_013 <- function() {
  nums <- readLines("number_013.txt")
  nums <- substr(nums, 1, 11)
  nums <- as.character(sum(as.numeric(nums)))
  return(substr(nums, 1, 10))
}
