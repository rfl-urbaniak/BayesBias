# Standard deviation for cosine similarity from Google datasets

# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 

getStatistics <- function(data, connection, group) {
  sdValue = sd(data)
  meanValue = mean(data)
  paste(group,connection,"sd:",sdValue,"mean:",meanValue)
}


# Read data

race = read.csv("./datasets/cleanedDatasets/raceGoogleCleaned.csv")
religion = read.csv("./datasets/cleanedDatasets/religionGoogleCleaned.csv")
gender = read.csv("./datasets/cleanedDatasets/genderGoogleCleaned.csv")

# RACE 
raceDifferent = race %>% filter(race$connection == "different")
raceAssociated = race %>% filter(race$connection == "associated")
raceNeutral = race %>% filter(race$connection == "none")
raceHuman = race %>% filter(race$connection == "human")

getStatistics(raceDifferent$cosineSimilarity, "different", "race")
getStatistics(raceAssociated$cosineSimilarity, "associated", "race")
getStatistics(raceNeutral$cosineSimilarity, "neutral", "race")
getStatistics(raceHuman$cosineSimilarity, "human", "race")

# GENDER

genderDifferent = gender %>% filter(gender$connection == "different")
genderAssociated = gender %>% filter(gender$connection == "associated")
genderNeutral = gender %>% filter(gender$connection == "none")
genderHuman = gender %>% filter(gender$connection == "human")

getStatistics(genderDifferent$cosineSimilarity, "different", "gender")
getStatistics(genderAssociated$cosineSimilarity, "associated", "gender")
getStatistics(genderNeutral$cosineSimilarity, "neutral", "gender")
getStatistics(genderHuman$cosineSimilarity, "human", "gender")


