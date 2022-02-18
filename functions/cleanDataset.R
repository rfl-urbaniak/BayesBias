cleanDataset <- function(dataset, levelsWordClass){

    colnames(dataset) <- c("protectedWord","wordToCompare","wordClass",
                      "cosineDistance","cosineSimilarity","connection")

    levels(dataset$wordClass) <- levelsWordClass

    dataset$conFactor <- factor(paste0(dataset$connection))
    dataset$con <- as.integer(dataset$conFactor)
    
    dataset$pwFactor <- factor(paste0(dataset$protectedWord, "-", dataset$connection))
    dataset$pwIndex <- as.integer(dataset$pwFactor)
#    dataset$associated <- ifelse(dataset$connection == "associated", 1, 0)
#    dataset$different <- ifelse(dataset$connection == "different", 1, 0)
#    dataset$human <- ifelse(dataset$connection == "human", 1, 0)
    return(dataset)
    }



religionReddit <- read.csv("./datasets/religionReddit.csv")[-1]
religionReddit <- cleanDataset(religionReddit,c("christian","human","jewish","muslim","neutral"))
write.csv(religionReddit, "./datasets/cleanedDatasets/religionRedditCleaned.csv")

genderReddit <- read.csv("./datasets/genderReddit.csv")[-1] 
genderReddit <- cleanDataset(genderReddit,c("human","man","neutral","woman"))
write.csv(genderReddit, "./datasets/cleanedDatasets/genderRedditCleaned.csv")

raceReddit <- read.csv("./datasets/raceReddit.csv")[-1]
raceReddit <- cleanDataset(raceReddit,c("asian","black","caucasian","human","neutral"))
write.csv(raceReddit, "./datasets/cleanedDatasets/raceRedditCleaned.csv")

religionGoogle <- read.csv("./datasets/religionGoogle.csv")[-1]
religionGoogle <- cleanDataset(religionGoogle,c("christian","human","jewish","muslim","neutral"))
write.csv(religionGoogle, "./datasets/cleanedDatasets/religionGoogleCleaned.csv")

genderGoogle <- read.csv("./datasets/genderGoogle.csv")[-1] 
genderGoogle <- cleanDataset(genderGoogle,c("human","man","neutral","woman"))
write.csv(genderGoogle, "./datasets/cleanedDatasets/genderGoogleCleaned.csv")

raceGoogle <- read.csv("./datasets/raceGoogle.csv")[-1]
raceGoogle <- cleanDataset(raceGoogle,c("asian","black","caucasian","human","neutral"))
write.csv(raceGoogle, "./datasets/cleanedDatasets/raceGoogleCleaned.csv")






