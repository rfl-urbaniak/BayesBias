library(rethinking)
library(tidyverse)
library(stringr)
library(ggthemes)


#clean dataset
cleanDataset <- function(dataset, levelsWordClass){
  
  colnames(dataset) <- c("protectedWord","wordToCompare","wordClass",
                         "cosineDistance","cosineSimilarity","connection")
  levels(dataset$wordClass) <- levelsWordClass
  
  dataset$conFactor <- factor(paste0(dataset$connection))
  dataset$con <- as.integer(dataset$conFactor)
  
  dataset$pw <- as.factor(dataset$protectedWord)
  dataset$pw <- as.integer(dataset$pw)
  
  dataset$pwConFactor <- factor(paste0(dataset$protectedWord, "-", dataset$connection))
  dataset$pwConIndex <- as.integer(dataset$pwConFactor)
  #   dataset$associated <- ifelse(dataset$connection == "associated", 1, 0)
  #   dataset$different <- ifelse(dataset$connection == "different", 1, 0)
  #   dataset$human <- ifelse(dataset$connection == "human", 1, 0)
  return(dataset)
}


#build model
buildModel <- function(dataset, mm = 1, sm =.5, mco =0 , sco =.5){
  options(buildtools.check = function(action) TRUE )
  d <- list()
  d$cosineDistance <- data2$cosineDistance
  d$pw <- data2$pw
  d$con <- data2$con
  model <- ulam(
    alist(
      cosineDistance ~ dnorm(mu,sigma),
      mu <- m[pw] + co[con],
      m[pw] ~ dnorm(1,.5),
      co[con] ~dnorm(0,.5),
      sigma ~ dcauchy(0,1)
    ),
    data = d,
    chains=2 , iter=80000 , warmup=1000, 
    log_lik = TRUE
  )
  return(model)
}



#test on a small dataset

religionReddit <- read.csv("./datasets/religionReddit.csv")[-1]
religionReddit <- cleanDataset(religionReddit,c("christian","human","jewish","muslim","neutral"))

head(religionReddit)

set.seed(123)
data2 <- sample_n(religionReddit, 300)
data2$pw

model <- buildModel(data2)

precis(model, depth = 2)



