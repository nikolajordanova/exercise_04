rm(list=ls()) #clean, clc, close all
# ***************
# R version 4.4.2 / win
# author: Nikola Jordánová
# *************

# Path
setwd('V:/MPA-PRG/exercise_04') # set working directory

# Task 1
ReturnCoins <- function(M){
  # M - money
  pd <- 0 # 50
  dc <- 0 # 20
  ds <- 0 # 10
  p <- 0 # 5
  d <- 0 # 2
  j <- 0 # 1
  
  pd <- M %/% 50
  M <- M - (pd * 50)
  
  dc <- M %/% 20
  M <- M - (dc * 20)
  
  ds <- M %/% 10
  M <- M - (ds * 10)
  
  p <- M %/% 5
  M <- M - (p * 5)

  d <- M %/% 2
  M <- M - (d * 2)
  
  j <- M %/% 1
  M <- M - (j * 1)
  
  number_of_coins <- pd + dc + ds + p + d+ j
  return (number_of_coins)
  
}

ReturnCoins(137)

#Task 2

UniversalReturnCoins <- function(M, coins){
  number_of_coins_vector <- c()
  for (coin in coins){
    number <- M %/% coin
    M <- M - (number * coin)
    number_of_coins_vector <- c(number_of_coins_vector, number)
    
  }
  
  
  return(number_of_coins_vector)
}
  
  

UniversalReturnCoins(137, c(50, 20, 10, 5, 2, 1))


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
     cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
     return ()
   }
   unusedPeg <- 6 - fromPeg - toPeg
   HanoiTowers(n - 1, fromPeg, unusedPeg)
   cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
   HanoiTowers(n - 1, unusedPeg, toPeg)
}

HanoiTowers(3, 1,3)
