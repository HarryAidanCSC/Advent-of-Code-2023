
# Advent of Code 2023 - Day 14 --------------------------------------------
# Author: Harry Mancinelli
# Date: 29/12/2023

# Packages 
library(tidyverse)

# Get Input
dirname(rstudioapi::getActiveDocumentContext()$path)
input <- readLines(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/input.txt"))

# Turn into df

df <- data.frame(matrix(nrow = nchar(input[1]), 
                    ncol = length(input)))

for(i in 1:length(input)){
  df[i,] <- str_split_1(input[i], "")
}


# Initalise vector
summary <- 0

# Run for loop 
for(i in 1:ncol(df)){
 
col1 <- df[,i]
    
round <- which(col1 == "O") 
square <- which(col1 == "#") + 1
roll_possible <- unique(c(1,square))

# Roll destination 
blind_roll <- findInterval(round, roll_possible)

adjusted_roll <- roll_possible[pmax(1, blind_roll)] + 
  ave(seq_along(ave(seq_along(blind_roll))), blind_roll, FUN = function(x) seq_along(x) - 1)

value = sum(101 - adjusted_roll)
summary = summary + value

}

# Part 1 Answer
summary


# Part 2 ------------------------------------------------------------------

# Part 2 ------------------------------------------------------------------
rotate <- function(x) t(apply(x, 2, rev))
empty_lit = list()


counter = 0
mat <- as.matrix(df)

#Find break

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
  
  if(counter %% 4){
    empty_lit <- c(empty_lit, list(mat))
    
    predefined_matrix <- empty_lit[[1]]
    
    is_equal <- sapply(empty_lit, function(mat) identical(mat, predefined_matrix))
    
    if(sum(is_equal) > 1){
      print(counter)
      break
    }
    
  }
  
  
  
  
  
  if(counter == 4000000000){
    break
  }
  mat <- rotate(mat)
  
  
}



predefined_matrix <- empty_lit[[3]]


# Check which matrices in the list are equal to the predefined matrix
is_equal <- sapply(empty_lit, function(mat) identical(mat, predefined_matrix))

which(is_equal == TRUE)
sum(is_equal)

