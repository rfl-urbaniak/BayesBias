library(rethinking)


getwd()
source("functions/cleanDataset.R")



##GENDER
#genderGlove <- read.csv("./datasets/macWeatDatasets/gender_group_glove_dataset.csv")[,-1]
genderGoogle <- cleanDataset(read.csv("./datasets/macWeatDatasets/gender_group_google_dataset.csv")[,-1])
genderReddit <-  cleanDataset(read.csv("./datasets/macWeatDatasets/gender_group_reddit_dataset.csv")[,-1])


modelGenderGoogle <- buildModel(genderGoogle)
precisGenderGoogle <- extractPrecis(modelGenderGoogle, name = "GenderGoogle")



modelGenderReddit <- buildModel(genderReddit)
precisGenderReddit  <- extractPrecis(modelGenderReddit, name = "GenderReddit")



##RACE
raceGlove <-  cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_glove_dataset.csv")[,-1])
#"WARNING:  18 out of 5868 (0.306748466257669%) missing comparisons have been removed!"

raceGoogle <- cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_google_dataset.csv")[,-1])
raceReddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_reddit_dataset.csv")[,-1])


modelRaceGlove <- buildModel(raceGlove)
precisRaceGlove  <- extractPrecis(modelRaceGlove, name = "RaceGlove")



modelRaceGoogle <- buildModel(raceGoogle)
precisRaceGoogle <- extractPrecis(modelRaceGoogle, name = "RaceGoogle")



modelRaceReddit <- buildModel(raceReddit)
precisRaceReddit  <- extractPrecis(modelRaceReddit, name = "RaceReddit")





##RELIGION
religionGlove <- cleanDataset(read.csv("./datasets/macWeatDatasets/religion_group_glove_dataset.csv")[,-1])
#"WARNING:  15 out of 4830 (0.31055900621118%) missing comparisons have been removed!"

religionGoogle <- cleanDataset(read.csv("./datasets/macWeatDatasets/religion_group_google_dataset.csv")[,-1])

#religionReddit <- read.csv("./datasets/macWeatDatasets/religion_group_reddit_dataset.csv")[,-1]


modelReligionGlove <- buildModel(religionGlove)
precisReligionGlove  <- extractPrecis(modelReligionGlove, name = "ReligionGlove")


modelReligionGoogle <- buildModel(religionGoogle)
precisReligionGoogle  <- extractPrecis(modelReligionGoogle, name = "ReligionGoogle")

##WEAT


#weat1Glove <- read.csv("./datasets/macWeatDatasets/weat_1_glove.csv")[,-1]
weat1Google <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_1_google.csv")[,-1])
#"WARNING:  361 out of 18050 (2%) missing comparisons have been removed!"

weat1Reddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_1_reddit.csv")[,-1])
# "WARNING:  6890 out of 18050 (38.1717451523546%) missing comparisons have been removed!"

modelWeat1Google <- buildModel(weat1Google)
precisWeat1Google  <- extractPrecis(modelWeat1Google, name = "Weat1Google")

modelWeat1Reddit <- buildModel(weat1Reddit)
precisWeat1Reddit  <- extractPrecis(modelWeat1Reddit, name = "Weat1Reddit")


weat7Glove <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_glove.csv")[,-1])
#"WARNING:  16 out of 5232 (0.305810397553517%) missing comparisons have been removed!"

weat7Google <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_google.csv")[,-1])
weat7Reddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_reddit.csv")[,-1])


modelWeat7Glove <- buildModel(weat7Glove)
precisWeat7Glove  <- extractPrecis(modelWeat7Glove, name = "Weat7Glove")


modelWeat7Google <- buildModel(weat7Google)
precisWeat7Google  <- extractPrecis(modelWeat7Google, name = "Weat7Google")

modelWeat7Reddit <- buildModel(weat7Reddit)
precisWeat7Reddit  <- extractPrecis(modelWeat7Reddit, name = "Weat7Reddit")









dataset <- cleanDataset(religionReddit)

