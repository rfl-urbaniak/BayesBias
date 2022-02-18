library(rethinking)
library(tidyverse)
library(stringr)
library(ggthemes)

buildModel <- function(dataset, mm = 1, sm =.5, mco =0 , sco =.5){
  options(buildtools.check = function(action) TRUE )
  model <- ulam(
    alist(
      cosineDistance ~ dnorm(mu,sigma),
      mu <- m[pw] + co[con],
      m[pw] ~ dnorm(1,.5),
      co[con] ~dnorm(0,.5),
      sigma ~ dcauchy(0,1)
    ),
    data = dataset,
    chains=2 , iter=8000 , warmup=1000,
    log_lik = TRUE
  )
  return(model)
}

# General coefficients
religion <- read.csv("./datasets/cleanedDatasets/religionRedditCleaned.csv")[-1]
religionCoefs <- buildModel(religion)
saveRDS(religionCoefs, file = "./datasets/coefficients/religionCoefs.rds")







# TODO: refactor rest if it works

religionCoefs <- readRDS("cosineAnalysis/datasets/religionCoefs.rds")
labels <- paste("co[",1:4,"]:", levels(religion$connection), sep = "")
plot(precis(religionCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Religion (Reddit)")







# gender <- read.csv("cosineAnalysis/datasets/genderReddit.csv")[-1] 
# gender <- cleanDataset(gender,c("human","man","neutral","woman"))


gender$pwFactor <- factor(paste0(gender$protectedWord, "-", gender$connection))
gender$pwIndex <- as.integer(gender$pwFactor)


options(buildtools.check = function(action) TRUE )
genderCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = gender,
  chains=2 , iter=8000 , warmup=1000, 
  log_lik = TRUE
)


saveRDS(genderCoefs, file = "genderCoefs.rds")


 


labels <- paste("co[",1:4,"]:", levels(gender$connection), sep = "")
plot(precis(genderCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Gender (Reddit)")


# race <- read.csv("cosineAnalysis/datasets/raceReddit.csv")[-1]
# race <- cleanDataset(race,c("asian","black","caucasian","human","neutral"))



race$pwFactor <- factor(paste0(race$protectedWord, "-", race$connection))
race$pwIndex <- as.integer(race$pwFactor)

options(buildtools.check = function(action) TRUE )
raceCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = race,
  chains=2 , iter=8000 , warmup=1000, 
  log_lik = TRUE
)


saveRDS(raceCoefs, file = "raceCoefs.rds")



raceCoefs <- readRDS("cosineAnalysis/datasets/raceCoefs.rds")
labels <- paste("co[",1:4,"]:", levels(race$connection), sep = "")
plot(precis(raceCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Race (Reddit)")






labels <- paste("co[",1:4,"]:", levels(race$connection), sep = "")
plot(precis(raceCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Race (Reddit)")


#coefficients for google


religionGoogle <- read.csv("cosineAnalysis/datasets/religionGoogle.csv")[-1]
religionGoogle <- cleanDataset(religionGoogle,c("christian","human","jewish","muslim","neutral"))

religionGoogleCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religionGoogle,
  chains=2 , iter=12000 , warmup=1000, 
  log_lik = TRUE, control = list(max_treedepth = 15)
)



saveRDS(religionGoogleCoefs, file = "religionGoogleCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(religionGoogle$connection), sep = "")
plot(precis(religionGoogleCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Religion (Google)")





genderGoogle <- read.csv("cosineAnalysis/datasets/genderGoogle.csv")[-1]
genderGoogle <- cleanDataset(genderGoogle,c("human","man","neutral","woman"))

genderGoogleCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = genderGoogle,
  chains=2 , iter=12000 , warmup=1000, 
  log_lik = TRUE, control = list(max_treedepth = 15)
)



saveRDS(genderGoogleCoefs, file = "genderGoogleCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(genderGoogle$connection), sep = "")
plot(precis(genderGoogleCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Gender (Google)")








genderGoogle <- read.csv("cosineAnalysis/datasets/genderGoogle.csv")[-1]
genderGoogle <- cleanDataset(genderGoogle,c("human","man","neutral","woman"))

genderGoogleCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = genderGoogle,
  chains=2 , iter=12000 , warmup=1000, 
  log_lik = TRUE, control = list(max_treedepth = 15)
)



saveRDS(genderGoogleCoefs, file = "genderGoogleCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(genderGoogle$connection), sep = "")
plot(precis(genderGoogleCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Gender (Google)")



raceGoogle <- read.csv("cosineAnalysis/datasets/raceGoogle.csv")[-1]
raceGoogle <- cleanDataset(raceGoogle,c("asian","black","caucasian","human","neutral"))




raceGoogleCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = raceGoogle,
  chains=2 , iter=10000 , warmup=1000,
  control = list(max_treedepth = 12),
  cores = 2
)



saveRDS(raceGoogleCoefs, file = "raceGoogleCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(raceGoogle$connection), sep = "")
plot(precis(raceGoogleCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Race (Google)")










#coefficients for debiased





debiasedReligionReddit <- read.csv("cosineAnalysis/datasets/debiasedReligionReddit.csv")[-1]
debiasedReligionReddit <- cleanDataset(debiasedReligionReddit,c("christian","human","jewish","muslim","neutral"))

debiasedReligionRedditCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedReligionReddit,
  chains=2 , iter=8000 , warmup=1000, 
  log_lik = TRUE
)


saveRDS(debiasedReligionRedditCoefs, file = "debiasedReligionRedditCoefs.rds")

debiasedReligionRedditCoefs <- readRDS("debiasedReligionRedditCoefs.rds")

labels <- paste("co[",1:4,"]:", levels(debiasedReligionReddit$connection), sep = "")
plot(precis(debiasedReligionRedditCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Religion (Reddit, debiased)")




debiasedGenderReddit <- read.csv("cosineAnalysis/datasets/debiasedGenderReddit.csv")[-1]
debiasedGenderReddit <- cleanDataset(debiasedGenderReddit,c("human","man","neutral","woman"))

debiasedGenderRedditCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedGenderReddit,
  chains=2 , iter=10000 , warmup=1000, 
  log_lik = TRUE
)



saveRDS(debiasedGenderRedditCoefs, file = "debiasedGenderRedditCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(debiasedGenderReddit$connection), sep = "")
plot(precis(debiasedGenderRedditCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Gender (Reddit, debiased)")







debiasedRaceReddit <- read.csv("cosineAnalysis/datasets/debiasedRaceReddit.csv")[-1]
debiasedRaceReddit <- cleanDataset(debiasedRaceReddit,c("asian","black","caucasian","human","neutral"))




debiasedRaceRedditCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedRaceReddit,
  chains=2 , iter=10000 , warmup=1000,
  cores = 2
)



saveRDS(debiasedRaceRedditCoefs, file = "debiasedRaceRedditCoefs.rds")


labels <- paste("co[",1:4,"]:", levels(debiasedRaceReddit$connection), sep = "")
plot(precis(debiasedRaceRedditCoefs, depth = 2, pars = "co"), labels = labels, main = 
       "Race (Reddit, debiased)")



#________________________________________


labels <- paste("co[",1:4,"]:", levels(religion$connection), sep = "")
plot(precis(religionCoefs, depth = 2, pars = "co"), labels = labels)




religionCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=10000 , warmup=1000, cores = 4,
  log_lik = TRUE
)

saveRDS(religionCoefs, file = "religionCoefs.rds")




religion.PW <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- none[pw],
    none[pw] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=4000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3),
  log_lik = TRUE
)


precis(religion.PW, depth = 2)


religionCoefficients <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=10000 , warmup=1000, cores = 2,
  log_lik = TRUE
)



precis(religion.PW2, depth = 2)


#PW2 comes out on top, neat
compare(religion.PW, religion.PW2)





#now moving to the level of protected words 



options(buildtools.check = function(action) TRUE ) #removes install pop-up request

religionSeparate <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)





precis(religionCoefs)


muslimWords <- c("imam","islam","mosque","muslim","quran")
muslim <- religion %>% filter(protectedWord %in% muslimWords)

head(muslim)




religion <- read.csv("cosineAnalysis/datasets/religionReddit.csv")[-1]
colnames(religion) <- c("protectedWord","wordToCompare","wordClass",
                        "cosineDistance","cosineSimilarity","connection")
levels(religion$wordClass) <- c("christian","human","jewish","muslim","neutral")
muslimWords <- c("imam","islam","mosque","muslim","quran")
muslim <- religion %>% filter(protectedWord %in% muslimWords)

muslim$protectedWord <- droplevels(muslim$protectedWord)

muslim$pw <- as.integer(muslim$protectedWord)
muslim$con <- as.integer(muslim$connection)
muslim$pwFactor <- factor(paste0(muslim$protectedWord, muslim$connection))
muslim$pwIndex <- as.integer(muslim$pwFactor)


muslim$pw


islamCoefs <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- m[pw] + co[con],
    m[pw] ~ dnorm(1,.5),
    co[con] ~dnorm(0,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = muslim,
  chains=2 , iter=10000 , warmup=1000, cores = 4,
  log_lik = TRUE
)


saveRDS(islamCoefs, file = "muslimCoefs.rds")

precis(islamCoefs, depth = 2)


islamCoefs <- readRDS("cosineAnalysis/datasets/islamCoefs.rds")
labelsIslam <- paste("co[",1:4,"]:", levels(muslim$connection), sep = "")
plot(precis(islamCoefs, depth = 2, pars = "co"), labels = labelsIslam, main = "Islam (Reddit)")



precis(religion.separate, depth = 2)

compare(religion.PW,religion.PW2,religion.separate)


options(buildtools.check = function(action) TRUE ) #removes install pop-up request

religion$pwFactor <- factor(paste0(religion$protectedWord, "-", religion$connection))
religion$pwIndex <- as.integer(religion$pwFactor)


religionSeparate <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)



saveRDS(religionSeparate, file = "religionSeparate.rds")



religionSeparate <- readRDS("cosineAnalysis/datasets/religionSeparate.rds")









religionGoogle <- read.csv("cosineAnalysis/datasets/religionGoogle.csv")[-1]

religionGoogle <- cleanDataset(religionGoogle,c("christian","human","jewish","muslim","neutral"))


religionGoogleModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = religionGoogle,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)


library(rethinking)

saveRDS(religionGoogleModel, file = "religionGoogleModel.rds")


genderGoogle <- read.csv("cosineAnalysis/datasets/genderGoogle.csv")[-1]
genderGoogle <- cleanDataset(genderGoogle,c("human","man","neutral","woman"))


genderGoogleModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = genderGoogle,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)



saveRDS(genderGoogleModel, file = "genderGoogleModel.rds")


library(rethinking)

raceGoogle <- read.csv("cosineAnalysis/datasets/raceGoogle.csv")[-1]
raceGoogle <- cleanDataset(raceGoogle,c("asian","black","caucasian","human","neutral"))


raceGoogleModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = raceGoogle,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)





saveRDS(raceGoogleModel, file = "raceGoogleModel.rds")









library(rethinking)



visualisePWestimates <- function(data, model){
  pr <- as.data.frame(precis(model, depth = 2))
  pr$tags <- c(levels(data$pwFactor), "sigma")
  tags <- pr$tags
  pr <- cbind(pr,str_split_fixed(tags, "-", 2))
  pr <- pr[-nrow(pr),]
  colnames(pr) <- c("mean", "sd", "low", "high", "neff", "rhat", "tags", "pw", "connection")
  dataset <- pr
  
  associated <-  dataset[dataset$connection == "associated",]
  different <-   dataset[dataset$connection == "different",]
  human <-   dataset[dataset$connection == "human",]
  none <- dataset[dataset$connection == "none",]
  
  colors <- c("associated"="orangered","different"="chartreuse4","human"="skyblue", "none" ="grey")
  
  plot <-   ggplot(dataset, aes(x = pw, y = mean, color = connection))+
    geom_point(data= associated, aes(x = pw, y = mean, color = "associated"))+
    geom_segment(data = associated, aes(x=pw,xend=pw,y=low,yend=high,color = "associated"), alpha = 0.3
    ) +
    geom_point(data= different, aes(x = pw, y = mean, color = "different"), 
               position = position_nudge(0.15))+
    geom_segment(data = different, aes(x=pw,xend=pw,y=low,yend=high, color = "different"), alpha = 0.3,
                 position = position_nudge(0.15))+
    geom_point(data= human, aes(x = pw, y = mean, color = "human"), position = position_nudge(-0.15))+
    geom_segment(data = human, aes(x=pw,xend=pw,y=low,yend=high, color = "human"), alpha = 0.3,
                 position = position_nudge(-0.15))+
    geom_point(data= none, aes(x = pw, y = mean, 
                               color = "none"),  position = position_nudge(0.3))+
    geom_segment(data = none, aes(x=pw,xend=pw,y=low,yend=high,
                                  color = "none"),
                 alpha = 0.3, position = position_nudge(0.3))+
    coord_flip()+theme_tufte()+xlab("protected class")+ylab("connection coefficient")+ 
    labs(color = "connection")+ scale_color_manual(name = "connection", values = colors)
  
  return(plot)
    }



visualisePWestimates(data = religion, model = religionSeparate)









#dataset %>% mutate(protectedClass = fct_reorder(protectedClass, a)) %>%
 
ggplot()+ geom_point(aes(x = protectedClass,y=a,color = "associated"))+
    geom_segment(aes(x=protectedClass,xend=protectedClass,y=alo,yend=ahigh), alpha = 0.3,
                 color = "chartreuse4")+
    geom_segment(data = dataset[dataset$protectedClass == "full dataset",],aes(x=protectedClass, xend = protectedClass,y=min(dataset$alo),yend = max(dataset$ahigh)), size = 9, color = "red",
                 alpha = 0.1)+geom_hline(yintercept = 0, size = 0.15, lty = 2)+
    geom_point(aes(x=protectedClass, y = d,color = "different"),position = position_nudge(0.15))+
    geom_segment(aes(x=protectedClass,xend=protectedClass,y=dlo,yend=dhigh, color ="different"), alpha = 0.3,
                 position = position_nudge(0.15))+
    geom_point(aes(x=protectedClass, y = h,color = "human"),position = position_nudge(-0.15) )+
    geom_segment(aes(x=protectedClass,xend=protectedClass,y=hlo,yend=hhigh, color = "human"), alpha = 0.3,    position = position_nudge(-0.15))+
    scale_color_manual(values = c("chartreuse4","orangered4","grey"))+
    coord_flip()+theme_tufte()+xlab("protected class")+ylab("class coefficient")+ 
    labs(color = "connection")
  print(parameterPlot)
}




### Now gender


gender <- read.csv("cosineAnalysis/datasets/genderReddit.csv")[-1]
gender <- cleanDataset(gender,c("human","man","neutral","woman"))

genderSeparate <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = gender,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)



saveRDS(genderSeparate, file = "genderSeparate.rds")


#gender <- read.csv("cosineAnalysis/datasets/genderReddit.csv")[-1]
gender <- read.csv("cosineAnalysis/datasets/genderReddit.csv")[-1]

#gender <- cleanDataset(gender,c("human","man","neutral","woman"))
#genderSeparate <- readRDS("cosineAnalysis/datasets/genderSeparate.rds")
visualisePWestimates(data = gender, model = genderSeparate)




## Now race


race <- read.csv("cosineAnalysis/datasets/raceReddit.csv")[-1]
race <- cleanDataset(race,c("asian","black","caucasian","human","neutral"))

raceSeparate <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = race,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3), log_lik =  TRUE
)



saveRDS(raceSeparate, file = "raceSeparate.rds")


# now debiased 




debiasedReligionReddit <- read.csv("cosineAnalysis/datasets/debiasedReligionReddit.csv")[-1]

debiasedReligionReddit <- cleanDataset(debiasedReligionReddit,c("christian","human","jewish","muslim","neutral"))


debiasedReligionRedditModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedReligionReddit,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3))


saveRDS(debiasedReligionRedditModel, file = "debiasedReligionRedditModel.rds")






debiasedGenderReddit <- read.csv("cosineAnalysis/datasets/debiasedGenderReddit.csv")[-1]
debiasedGenderReddit <- cleanDataset(debiasedGenderReddit,c("human","man","neutral","woman"))

debiasedGenderRedditModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedGenderReddit,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)



saveRDS(debiasedGenderRedditModel, file = "debiasedGenderRedditModel.rds")









debiasedRaceReddit <- read.csv("cosineAnalysis/datasets/debiasedRaceReddit.csv")[-1]
debiasedRaceReddit <- cleanDataset(debiasedRaceReddit,c("asian","black","caucasian","human","neutral"))

debiasedRaceRedditModel <- ulam(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- c[pwIndex],
    c[pwIndex] ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = debiasedRaceReddit,
  chains=2 , iter=10000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)



saveRDS(debiasedRaceRedditModel, file = "debiasedRaceRedditModel.rds")











#OUTDATED



Now, let's construct a multilevel model with the following form:
  
  \begin{align*}
S_i & \sim Bin(N_i, p_i)\\
logit(p_i) & = \alpha_{tank[i]}\\
\alpha_j & \sim N(\bar{\alpha}, \sigma)\\
\bar \alpha & \sim N(0, 1.5)\\
\sigma & \sim Exp(1)
\end{align*}



\vspace{1mm}
\footnotesize
```{r,echo=TRUE,eval=TRUE,fig.align = "center",cache=TRUE, fig.show = "hold", out.width = "100%", results = "hide"}
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar,sigma),
    a_bar ~ dnorm(0,1.5),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, log_lik = TRUE
)
```
\normalsize

\vspace{1mm}
\footnotesize
```{r,echo=TRUE,eval=TRUE,fig.align = "center",cache=TRUE, fig.show = "hold", out.width = "100%"}
print(compare(m13.1, m13.2))
```
\normalsize
