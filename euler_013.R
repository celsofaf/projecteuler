euler_013 <- function() {
  nums <- readLines("number_013.txt")
  nums <- as.numeric(substr(nums, 1, 11))
  nums <- as.character(sum(nums))
  return(substr(nums, 1, 10))
}
