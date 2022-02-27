#gender <- read.csv("cosineAnalysis/datasets/genderOriginal.csv")[,-1]
#genderClean <- cleanDataset(gender, c("man","neutral","woman"))



cleanDataset <- function(dataset, levelsWordClass){

    colnames(dataset) <- c("protectedWord","wordToCompare","wordClass",
                      "cosineDistance","cosineSimilarity","connection")

    levels(dataset$wordClass) <- levelsWordClass

    dataset$con <- as.integer(dataset$connection)
    dataset$pw <- as.integer(dataset$protectedWord)
    dataset$pwFactor <- factor(paste0(dataset$protectedWord, "-", dataset$connection))
    dataset$pwIndex <- as.integer(dataset$pwFactor)
#    dataset$associated <- ifelse(dataset$connection == "associated", 1, 0)
#    dataset$different <- ifelse(dataset$connection == "different", 1, 0)
#    dataset$human <- ifelse(dataset$connection == "human", 1, 0)
    return(dataset)
    }
