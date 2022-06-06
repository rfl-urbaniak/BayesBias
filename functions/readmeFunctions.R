#First, cleaning the dataset. We'll use religionReddit as an example


source("functions/cleanDataset.R")

#example
religionReddit <- read.csv("./datasets/macWeatDatasets/religion_group_reddit_dataset.csv")[,-1]
dataset <- cleanDataset(religionReddit)



#then, on the example of religion we compare three model structures and their performance
file.edit("functions/modelComparison.R")

#it turns out that hierarhical modeling with variable sigma makes most sense; we'll use it to define the buildModel function
#the training takes a bit of time, so we play a nice song when done

source("functions/buildModel.R")

#now apply to our example
modelReligionReddit <- buildModel(dataset)

#extract and save the model precis 
source("functions/extractPrecis.R")

#apply to our model
precisReligionReddit <- extractPrecis(modelReligionReddit, name = "ReligionReddit")

#now we clean up the summaries and prepare plots based on a precis and a dataset
source("functions/plotFromPrecis.R")


#example
resultsReligionReddit <- plotFromPrecis(precis = precisReligionReddit,
                                        dataset = dataset
                                          )

#take a look
names(resultsReligionReddit)

resultsReligionReddit$DFindividual
resultsReligionReddit$DFoverall
resultsReligionReddit$plotIndividual
resultsReligionReddit$plotOverall
resultsReligionReddit$plotJoint


#now, let's try do to everything for a different dataset
genderGlove <- read.csv("./datasets/macWeatDatasets/gender_group_glove_dataset.csv")[,-1]
genderGlove <- cleanDataset(genderGlove)
modelGenderGlove <- buildModel(genderGlove)
precisGenderGlove <- extractPrecis(modelGenderGlove, name = "GenderGlove")
resultsGenderGlove <- plotFromPrecis(precis = precisGenderGlove,
                                        dataset = genderGlove)

resultsGenderGlove$plotJoint


