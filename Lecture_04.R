rm(list=ls()) #clean, clc, close all
# ***************
# R version 4.4.2 / win
# author: Nikola Jordánová
# *************

# Path
setwd('V:/MPA-PRG/exercise_04') # set working directory

# Task 1
# Dodělat doma

#Task 2
# Dodělat doma

# Task 3
Chocolate <- function(M, r, c){
  number_of_rows = (dim(M))[1]
   if (r == number_of_rows){
     return (M[r, c])
   }
   else{
     bars <- M[r, c]
     down <- Chocolate(M, r + 1, c)
     diagonal <- Chocolate(M, r + 1, c + 1)
     return (max(down, diagonal) + bars)
}
}

m <- matrix(c(3, 0, 0, 0, 1, 4, 0, 0, 5, 3, 0, 0, 1, 2, 6, 7), nrow = 4, byrow = TRUE)
print(m)

Chocolate(m,1,1)

# Task 4
HanoiTowers <- function(n, fromPeg, toPeg){
   if (n == 1){
     output <- "Move disc from peg fromPeg to peg toPeg"
     return
   }
   unusedPeg <- 6 - fromPeg - toPeg
   HanoiTowers(n - 1, fromPeg, unusedPeg)
   output <- "Move disc from peg fromPeg to peg toPeg"
   HanoiTowers(n - 1, emptyPeg, toPeg)
   return
}

HanoiTowers(3, 1,3)
