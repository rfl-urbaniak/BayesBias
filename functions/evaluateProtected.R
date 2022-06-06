library(ggplot2)
library(ggthemes)

library(tidyverse)
library(dplyr)
library(rethinking)

options(buildtools.check = function(action) TRUE )

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("cosineAnalysis/functions/cleanDataset.R")


extractPars <- function(typeModel) {
  return(data.frame(n = precis(typeModel)["no","mean"], 
                    nlo = precis(typeModel)["no","5.5%"],
                    nhigh = precis(typeModel)["no","94.5%"],
                    a = precis(typeModel)["a","mean"],
                    alo = precis(typeModel)["a","5.5%"],
                    ahigh =  precis(typeModel)["a","94.5%"],
                    d = precis(typeModel)["d","mean"],
                    dlo = precis(typeModel)["d","5.5%"],
                    dhigh =  precis(typeModel)["d","94.5%"],
                    h = precis(typeModel)["h","mean"],
                    hlo = precis(typeModel)["h","5.5%"],
                    hhigh = precis(typeModel)["h","94.5%"])
  )
}



dataset <- religion

parTable <- function(dataset){

type.full.st  <- map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + a * associated  + d * different + h * human,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    d ~ dnorm(0,1),
    h ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)), data = dataset,
  chains=2 , iter=6000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3),
  control = list(max_treedepth = 11)
)


full.stats <- extractPars(type.full.st)
rownames(full.stats) <- "full dataset"

protectedWords <-unique(dataset$protectedWord)

stats <- list()

for(word in 1:length(protectedWords)){
  
  bunch <- dataset[dataset$protectedWord == protectedWords[word],]
  type  <- rethinking::map(
    alist(
      cosineDistance ~ dnorm(mu,sigma),
      mu <- no + a * associated  + d * different + h * human,
      no ~ dnorm(1,.5),
      a ~ dnorm(0,1),
      d ~ dnorm(0,1),
      h ~ dnorm(0,1),
      sigma ~ dcauchy(0,1)
    ),
    data = bunch,
    start=list(no = 1, a = 0, d = 0, sigma= .3)
  )
  stats[[word]] <- extractPars(type)
  rownames(stats[[word]]) <- protectedWords[word]
}

statsTable <-  bind_rows(stats, .id = "column_label")[,-1]
statsTable <- rbind(statsTable,full.stats)
statsTable <-  cbind(rownames(statsTable), data.frame(statsTable, row.names=NULL))
colnames(statsTable)[1] <- "protectedClass"
statsTable <- statsTable[order(statsTable$a),]
return(statsTable)
}


#genderOriginalTable <- parTable(genderClean)

#raceOriginalTable <- parTable(raceClean)

#religionOriginalTable <- parTable(religionClean)

#religionOriginalTable

#getwd()

#raceOriginalTable

#visualiseTable(raceOriginalTable)
#save(religionOriginalTable,file = "cosineAnalysis/datasets/religionOriginalTable.RData")  
#save(genderOriginalTable,file = "cosineAnalysis/datasets/genderOriginalTable.RData")  
#save(raceOriginalTable,file = "cosineAnalysis/datasets/raceOriginalTable.RData")  

#visualiseTable(genderOriginalTable)

