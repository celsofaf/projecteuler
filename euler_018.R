euler_018 <- function(){
  library("stringr")
  triangle <- readLines("triangle_018.txt")
  triangle <- str_split(triangle, " ")
  for ( i in 1:length(triangle) )
    triangle[[i]] <- as.numeric(triangle[[i]])
  triangle[[2]][1] <- triangle[[2]][1] + triangle[[1]][[1]]
  triangle[[2]][2] <- triangle[[2]][2] + triangle[[1]][[1]]
  for ( i in 2:(length(triangle)-1) ) {
    #extreme left
    triangle[[i+1]][1] <- triangle[[i+1]][1] + triangle[[i]][1]
    for( j in 2:length(triangle[[i]]) ) {
      #extreme right
      if ( j == length(triangle[[i]]) )
        triangle[[i+1]][j+1] <- triangle[[i+1]][j+1] + triangle[[i]][[j]]
      #all in the middle
      triangle[[i+1]][[j]] <- triangle[[i+1]][[j]] + max(triangle[[i]][[j-1]], triangle[[i]][[j]])
    }
  }
  return(max(triangle[[length(triangle)]]))
}
