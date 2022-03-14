#  cosine similarity histogram for all groups

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(kableExtra)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(ggforce)
library(latex2exp)

religionReddit <- read.csv("./datasets/religionReddit.csv")[-1]
raceReddit <- read.csv("./datasets/raceReddit.csv")[-1]
genderReddit <- read.csv("./datasets/genderReddit.csv")[-1]


# --------------------------- RELIGION DATASET ------------------------------------------------

cosAssReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'associated'),]
cosDiffReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'different'),]
cosNoneReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'none'),]

# ------------ Associated
par(mfrow=c(1,3))
ggplot()+geom_histogram(aes(x=cosAssReligion, y = ..density..), alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosAssReligion)>0.1)) / length(cosAssReligion)

# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffReligion, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosDiffReligion)>0.1)) / length(cosDiffReligion)

# ------------- Neutral and Human

ggplot()+geom_histogram(aes(x=cosNoneReligion, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosNoneReligion)>0.1)) / length(cosNoneReligion)


# ------------------------------ GENDER DATASET ---------------------------------------------

cosAssGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'associated'),]
cosDiffGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'different'),]
cosNoneGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'none'),]

# ------------ Associated

par(mfrow=c(1,3))
ggplot()+geom_histogram(aes(x=cosAssGender, y = ..density..), alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosAssGender)>0.1)) / length(cosAssGender)


# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffGender, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosDiffGender)>0.1)) / length(cosDiffGender)


# ------------- Neutral and Human

ggplot()+geom_histogram(aes(x=cosNoneGender, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosNoneGender)>0.1)) / length(cosNoneGender)





# ------------------------------- RACE DATASET ----------------------------------------------


cosAssRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'associated'),]
cosDiffRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'different'),]
cosNoneRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'none'),]

# ------------ Associated

par(mfrow=c(1,3))
ggplot()+geom_histogram(aes(x=cosAssRace, y = ..density..), alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosAssRace)>0.1)) / length(cosAssRace)


# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffRace, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosDiffRace)>0.1)) / length(cosDiffRace)


# ------------- Neutral and Human

ggplot()+geom_histogram(aes(x=cosNoneRace, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("bias") + geom_vline(xintercept = .1) + geom_vline(xintercept = -.1) 

(sum(abs(cosNoneRace)>0.1)) / length(cosNoneRace)




