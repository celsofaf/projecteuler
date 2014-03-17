euler_011 <- function() {
  grid <- read.csv("grid_011.txt", header=FALSE, sep=" ")
  prod <- 0
  
  # horizontal
  for ( i in 1:nrow(grid) )
    for ( j in 1:(ncol(grid)-3) )
      prod <- max(prod, grid[i, j] * grid[i, j+1] * grid[i, j+2] * grid[i, j+3])
  
  # vertical
  for ( i in 1:(nrow(grid)-3) )
    for ( j in 1:ncol(grid) )
      prod <- max(prod, grid[i, j] * grid[i+1, j] * grid[i+2, j] * grid[i+3, j])
  
  # diagonal 1
  for ( i in 1:(nrow(grid)-3) )
    for ( j in 1:(ncol(grid)-3) )
      prod <- max(prod, grid[i, j] * grid[i+1, j+1] * grid[i+2, j+2] * grid[i+3, j+3])
  
  # diagonal 2
  for ( i in 1:(nrow(grid)-3) )
    for ( j in 4:ncol(grid) )
      prod <- max(prod, grid[i, j] * grid[i+1, j-1] * grid[i+2, j-2] * grid[i+3, j-3])
  
  return(prod)
}
