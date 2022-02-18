# Standard deviation for cosine similarity from Google datasets


# Read data

race = read.csv("./datasets/cleanedDatasets/raceGoogleCleaned.csv")
religion = read.csv("./datasets/cleanedDatasets/religionGoogleCleaned.csv")
gender = read.csv("./datasets/cleanedDatasets/genderGoogleCleaned.csv")

# RACE 
raceDifferent = race %>% filter(race$connection == "different")
raceAssociated = race %>% filter(race$connection == "associated")
raceNeutral = race %>% filter(race$connection == "none")
raceHuman = race %>% filter(race$connection == "human")

print(paste0("Std for different: ", sd(raceDifferent$cosineSimilarity)))
print(paste0("Std for associated: ", sd(raceAssociated$cosineSimilarity)))
print(paste0("Std for neutral: ", sd(raceNeutral$cosineSimilarity)))
print(paste0("Std for human: ", sd(raceHuman$cosineSimilarity)))

# GENDER

genderDifferent = gender %>% filter(gender$connection == "different")
genderAssociated = gender %>% filter(gender$connection == "associated")
genderNeutral = gender %>% filter(gender$connection == "none")
genderHuman = gender %>% filter(gender$connection == "human")

print(paste0("Std for different: ", sd(genderDifferent$cosineSimilarity)))
print(paste0("Std for associated: ", sd(genderAssociated$cosineSimilarity)))
print(paste0("Std for neutral: ", sd(genderNeutral$cosineSimilarity)))
print(paste0("Std for human: ", sd(genderHuman$cosineSimilarity)))

