library("stringr")
names <- readLines("names_022.txt")
names <- str_replace_all(names, "\"", "")
names <- str_split(names, ",")
names <- sort(names[[1]])
scoresum <- 0
for ( i in 1:length(names) ) {
  score <- 0
  for ( j in 1:str_length(names[i]) )
    score <- score + which(LETTERS == str_sub(names[i], j, j))
  scoresum <- scoresum + score * i
}
print(scoresum)
