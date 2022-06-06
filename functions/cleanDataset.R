library(rethinking)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(grid)

#dataset <- weat1Glove
#nrow(dataset)

cleanDataset <- function (dataset) {
  
  colnames(dataset) <- c("pw","word","stereotype", "distance", "similarity", "connection")
  dataset$pw <- as.factor(dataset$pw)
  dataset$word <- as.factor(dataset$word)
  dataset$stereotype <- as.factor(dataset$stereotype)
  dataset$connection <- as.factor(dataset$connection)
  
  
  
  ifelse(sum( dataset$similarity < -1 | dataset$distance < -1) != 0, 
         print(
           paste("WARNING:  ",  sum( dataset$similarity < -1 | dataset$distance < -1), 
                 " out of ", nrow(dataset), " (", sum( dataset$similarity < -1 | dataset$distance < -1)/nrow(dataset) * 100, 
                 "%) missing comparisons have been removed!", sep = "")
         ),
         print("No word removal needed.")
  )
  
  
  dataset <- dataset[  dataset$similarity >= -1 | dataset$distance >= -1,]
  
  dataset$pw <- droplevels(dataset$pw)
  dataset$word <- droplevels(dataset$word)
  dataset$stereotype <- droplevels(dataset$stereotype)
  dataset$connection <- droplevels(dataset$connection)
  
  
  dataset$associated <- as.integer(dataset$connection == "associated")
  dataset$different <- as.integer(dataset$connection == "different")
  dataset$human <- as.integer(dataset$connection == "human")
  dataset$none <- as.integer(dataset$connection == "none")
  dataset$pwi <- as.integer(dataset$pw)
  

  return(dataset)
}
