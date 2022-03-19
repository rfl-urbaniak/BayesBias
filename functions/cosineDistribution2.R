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

getwd()
religionReddit <- read.csv("../datasets/religionReddit.csv")[-1]
raceReddit <- read.csv("../datasets/raceReddit.csv")[-1]
genderReddit <- read.csv("../datasets/genderReddit.csv")[-1]


# --------------------------- RELIGION DATASET ------------------------------------------------

cosAssReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'associated'),]
cosDiffReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'different'),]
cosNoneReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'none'),]
cosHumReligion <- religionReddit['cosine_similarity'][which(religionReddit$connection == 'human'),]


ggplot(religionReddit, aes(x = connection, y = cosine_similarity, fill = connection, color = connection))+
  geom_violin(alpha = .5)+coord_flip()+
  geom_vline(xintercept = .14) + geom_hline(yintercept = -.14, color = "grey") +
  ylab("similarity")+theme_tufte()+ geom_hline(yintercept = .14, color = "grey")+
  ggtitle("Religion: empirical distribution of cosine similarity")+
  annotate(geom = "label", x = 4, y = 0, label = "56%")+
  annotate(geom = "label", x = 3, y = 0, label = "55%")+
  annotate(geom = "label", x = 2, y = 0, label = "43%")+
  annotate(geom = "label", x = 1, y = 0, label = "36%")




# ------------ Associated
length(cosAssReligion)
ggplot()+geom_histogram(aes(x=cosAssReligion, y = ..density..),
                alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
                theme_tufte()+labs(title="Cosine similarity distribution")+
                xlab("similarity") + geom_vline(xintercept = .14) + geom_vline(xintercept = -.14)

1-(sum(abs(cosAssReligion)>0.14)) / length(cosAssReligion)

# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffReligion, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .14) + geom_vline(xintercept = -.14) 

1-(sum(abs(cosDiffReligion)>0.14)) / length(cosDiffReligion)

# ------------- Neutral

ggplot()+geom_histogram(aes(x=cosNoneReligion, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .14) + geom_vline(xintercept = -.14) 

(sum(abs(cosNoneReligion)>0.14)) / length(cosNoneReligion)

1-mean(abs(cosNoneReligion)>0.14)
mean(cosNoneReligion)


cosRelNoneMeans <- numeric(100)
for(i in 1:100){
  sample <- sample(cosNoneReligion,size = 165)
  cosRelNoneMeans[i] <- mean(sample)
}
rethinking::dens(cosRelNoneMeans)
mean(abs(cosRelNoneMeans)>0.14)


(sum(abs(cosNoneReligion)>0.375)) / length(cosNoneReligion)

quantile(cosNoneReligion, c(0.025, 0, 0.975))







#------------  Human

ggplot()+geom_histogram(aes(x=cosHumReligion, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .14) + geom_vline(xintercept = -.14) 

1- (sum(abs(cosHumReligion)>0.14)) / length(cosHumReligion)


(sum(abs(cosHumReligion)>0.375)) / length(cosHumReligion)

quantile(cosHumReligion, c(0.025, 0, 0.975))



mean(cosHumReligion)




cosRelHumMeans <- numeric(100)
for(i in 1:100){
  sample <- sample(cosHumReligion,size = 165)
  cosRelHumMeans[i] <- mean(sample)
}
rethinking::dens(cosRelHumMeans)
mean(abs(cosRelHumMeans)>0.14)

length(c(cosAssReligion,cosDiffReligion))



# ------------------------------ GENDER DATASET ---------------------------------------------

cosAssGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'associated'),]
cosDiffGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'different'),]
cosNoneGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'none'),]
cosHumGender <- genderReddit['cosine_similarity'][which(genderReddit$connection == 'human'),]

# ------------ Associated

par(mfrow=c(1,3))
ggplot()+geom_histogram(aes(x=cosAssGender, y = ..density..), alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") +
  geom_vline(xintercept = .38) + geom_vline(xintercept = -.38) 

(sum(abs(cosAssGender)>0.38)) / length(cosAssGender)


# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffGender, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .38) + geom_vline(xintercept = -.38) 

(sum(abs(cosDiffGender)>0.38)) / length(cosDiffGender)


# ------------- Neutral 

ggplot()+geom_histogram(aes(x=cosNoneGender, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .38) + geom_vline(xintercept = -.38) 

(sum(abs(cosNoneGender)>0.38)) / length(cosNoneGender)



# ------------- Human 

ggplot()+geom_histogram(aes(x=cosHumGender, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .38) + geom_vline(xintercept = -.38) 

(sum(abs(cosHumGender)>0.38)) / length(cosHumGender)




# ------------------------------- RACE DATASET ----------------------------------------------


cosAssRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'associated'),]
cosDiffRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'different'),]
cosNoneRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'none'),]
cosHumRace <- raceReddit['cosine_similarity'][which(raceReddit$connection == 'human'),]



# ------------ Associated

ggplot()+geom_histogram(aes(x=cosAssRace, y = ..density..), alpha = 0.8, bins=20, color="lightgreen", fill="lightgreen")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .11) + geom_vline(xintercept = -.11) 

(sum(abs(cosAssRace)>0.11)) / length(cosAssRace)


# ------------ Different

ggplot()+geom_histogram(aes(x=cosDiffRace, y = ..density..), alpha = 0.8, bins=20, color="pink", fill="pink")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .11) + geom_vline(xintercept = -.11) 

(sum(abs(cosDiffRace)>0.11)) / length(cosDiffRace)


# ------------- Neutral

ggplot()+geom_histogram(aes(x=cosNoneRace, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .11) + geom_vline(xintercept = -.11) 

(sum(abs(cosNoneRace)>0.11)) / length(cosNoneRace)



# ------------- Human

ggplot()+geom_histogram(aes(x=cosHumRace, y = ..density..), alpha = 0.8, bins=20, color="lightblue", fill="lightblue")+
  theme_tufte()+labs(title="Cosine similarity distribution")+ xlab("similarity") + geom_vline(xintercept = .11) + geom_vline(xintercept = -.11) 

(sum(abs(cosHumRace)>0.11)) / length(cosHumRace)

