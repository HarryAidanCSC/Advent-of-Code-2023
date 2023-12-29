
# Advent of Code 2023 - Day 14 --------------------------------------------
# Author: Harry Mancinelli
# Date: 29/12/2023

# Packages 
library(tidyverse)

# Get Input
input <- readLines("C:/Users/hxzzm/Desktop/git/Advent-of-Code-2023/14/input.txt")

# Turn into df

df <- data.frame(matrix(nrow = nchar(input[1]), 
                    ncol = length(input)))

for(i in 1:length(input)){
  df[i,] <- str_split_1(input[i], "")
}


# Initalise vector
summary <- vector()

# Run for loop 
for(i in 1:ncol(df)){
 
col1 <- df[,i]
    
round <- which(col1 == "O") 
square <- which(col1 == "#") + 1
roll_possible <- unique(c(1,square))

# Roll desintation 
blind_roll <- findInterval(round, roll_possible)

adjusted_roll <- roll_possible[pmax(1, blind_roll)] + 
  ave(seq_along(ave(seq_along(blind_roll))), blind_roll, FUN = function(x) seq_along(x) - 1)



}

# Part 1 Answer
(101-summary) %>% sum()


# Part 2 ------------------------------------------------------------------
rotate <- function(x) t(apply(x, 2, rev))



counter = 0
mat <- as.matrix(df)

repeat{
  
for(i in 1:ncol(df)){

  col1 <- mat[,i]
  
  round <- which(col1 == "O") 
  square <- which(col1 == "#") + 1
  roll_possible <- unique(c(1,square))
  
  # Roll desintation 
  blind_roll <- findInterval(round, roll_possible)
  
  adjusted_roll <- roll_possible[pmax(1, blind_roll)] + 
    ave(seq_along(ave(seq_along(blind_roll))), blind_roll, FUN = function(x) seq_along(x) - 1)
  
 col1[col1 == "O"] <- "."
 col1[adjusted_roll] <- "O" 
 
 mat[,i] <- col1
}
  counter = counter + 1
  
  if(counter == 1000000000*4){
    break
  }
  
  mat <- rotate(mat)

  
}

# Part 2 Answer

rowSums(mat == "O") * (101 - seq_along(rowSums(mat == "O")))
