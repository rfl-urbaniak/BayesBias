

source("functions/cleanDataset.R")
source("functions/plotFromPrecis.R")




weat7Google <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_google.csv")[,-1])
precisWeat7Google <- readRDS("resultsDFs/Weat7GoogleDF.rds")
resultsWeat7Google <- plotFromPrecis(precis = precisWeat7Google,
                                     dataset = weat7Google,
                                     list = "Weat 7", 
                                     embedding = "Google")


weat7Glove <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_glove.csv")[,-1])
precisWeat7Glove <- readRDS("resultsDFs/Weat7GloveDF.rds")
resultsWeat7Glove <- plotFromPrecis(precis = precisWeat7Glove,
                                     dataset = weat7Glove,
                                     list = "Weat 7", 
                                     embedding = "Glove")


weat7Reddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_7_reddit.csv")[,-1])
precisWeat7Reddit <- readRDS("resultsDFs/Weat7RedditDF.rds")
resultsWeat7Reddit <- plotFromPrecis(precis = precisWeat7Reddit,
                                    dataset = weat7Reddit,
                                    list = "Weat 7", 
                                    embedding = "Reddit", ylims = c(.5,1.3))


weat1Google <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_1_google.csv")[,-1])
precisWeat1Google <- readRDS("resultsDFs/Weat1GoogleDF.rds")
resultsWeat1Google <- plotFromPrecis(precis = precisWeat1Google,
                                     dataset = weat1Google,
                                     list = "Weat 1", 
                                     embedding = "Google")


weat1Glove <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_1_glove.csv")[,-1])
precisWeat1Glove <- readRDS("resultsDFs/Weat1GloveDF.rds")
resultsWeat1Glove <- plotFromPrecis(precis = precisWeat1Glove,
                                     dataset = weat1Glove,
                                     list = "Weat 1", 
                                     embedding = "Glove")


weat1Reddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/weat_1_reddit.csv")[,-1])
precisWeat1Reddit <- readRDS("resultsDFs/Weat1RedditDF.rds")
resultsWeat1Reddit <- plotFromPrecis(precis = precisWeat1Reddit,
                                     dataset = weat1Reddit,
                                     list = "Weat 1", 
                                     embedding = "Reddit")


religionGoogle <- cleanDataset(read.csv("./datasets/macWeatDatasets/religion_group_google_dataset.csv")[,-1])
precisReligionGoogle <- readRDS("resultsDFs/ReligionGoogleDF.rds")
resultsReligionGoogle <- plotFromPrecis(precis = precisReligionGoogle,
                                     dataset = religionGoogle,
                                     list = "Religion", 
                                     embedding = "Google")




religionGlove <- cleanDataset(read.csv("./datasets/macWeatDatasets/religion_group_glove_dataset.csv")[,-1])
precisReligionGlove <- readRDS("resultsDFs/ReligionGloveDF.rds")
resultsReligionGlove <- plotFromPrecis(precis = precisReligionGlove,
                                        dataset = religionGlove,
                                        list = "Religion", 
                                        embedding = "Glove")



religionReddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/religion_group_reddit_dataset.csv")[,-1])
precisReligionReddit <- readRDS("resultsDFs/ReligionRedditDF.rds")
resultsReligionReddit <- plotFromPrecis(precis = precisReligionReddit,
                                       dataset = religionReddit,
                                       list = "Religion", 
                                       embedding = "Reddit")




raceGoogle <-  cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_google_dataset.csv")[,-1])
precisRaceGoogle <- readRDS("resultsDFs/RaceGoogleDF.rds")
resultsRaceGoogle <- plotFromPrecis(precis = precisRaceGoogle,
                                        dataset = raceGoogle,
                                        list = "Race", 
                                        embedding = "Google")




raceGlove <-  cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_glove_dataset.csv")[,-1])
precisRaceGlove <- readRDS("resultsDFs/RaceGloveDF.rds")
resultsRaceGlove <- plotFromPrecis(precis = precisRaceGlove,
                                    dataset = raceGlove,
                                    list = "Race", 
                                    embedding = "Glove")





raceReddit <-  cleanDataset(read.csv("./datasets/macWeatDatasets/race_group_reddit_dataset.csv")[,-1])
precisRaceReddit <- readRDS("resultsDFs/RaceRedditDF.rds")
resultsRaceReddit <- plotFromPrecis(precis = precisRaceReddit,
                                    dataset = raceReddit,
                                    list = "Race", 
                                    embedding = "Reddit")




genderGoogle <- cleanDataset(read.csv("./datasets/macWeatDatasets/gender_group_google_dataset.csv")[,-1])
precisGenderGoogle <- readRDS("resultsDFs/GenderGoogleDF.rds")
resultsGenderGoogle <- plotFromPrecis(precis = precisGenderGoogle,
                                    dataset = genderGoogle,
                                    list = "Gender", 
                                    embedding = "Google")




genderGlove <- cleanDataset(read.csv("./datasets/macWeatDatasets/gender_group_glove_dataset.csv")[,-1])
precisGenderGlove <- readRDS("resultsDFs/GenderGloveDF.rds")
resultsGenderGlove <- plotFromPrecis(precis = precisGenderGlove,
                                      dataset = genderGlove,
                                      list = "Gender", 
                                      embedding = "Glove")





genderReddit <- cleanDataset(read.csv("./datasets/macWeatDatasets/gender_group_reddit_dataset.csv")[,-1])
precisGenderReddit <- readRDS("resultsDFs/GenderRedditDF.rds")

resultsGenderReddit <- plotFromPrecis(precis = precisGenderReddit,
                                     dataset = genderReddit,
                                     list = "Gender", 
                                     embedding = "Reddit",  ylims = c(.4,1.2))





#results

resultsWeat7Google$plotJoint

resultsWeat7Glove$plotJoint

resultsWeat7Reddit$plotJoint

resultsWeat1Google$plotJoint

resultsWeat1Glove$plotJoint

resultsWeat1Reddit$plotJoint

resultsReligionGoogle$plotJoint

resultsReligionGlove$plotJoint

resultsReligionReddit$plotJoint

resultsRaceGoogle$plotJoint

resultsRaceGlove$plotJoint

resultsRaceReddit$plotJoint

resultsGenderGoogle$plotJoint

resultsGenderGlove$plotJoint

resultsGenderReddit$plotJoint


