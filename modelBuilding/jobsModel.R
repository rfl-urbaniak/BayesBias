library(tidyr)
library(rethinking)
library(ggthemes)
library(precis)
library(ggplot2)
library(rethinking)

genderProjections <-  read.csv("datasets/genderProjections.csv")[,-1]
head(genderProjections)
colnames(genderProjections)[4] <- "stereotype" 
head(genderProjections)
levels(genderProjections$stereotype) <- c("male", "female")

ggplot(genderProjections, aes(x=he,y=she, color = stereotype, shape = stereotype))+geom_point(size = 6, alpha = 0.75)+theme_tufte(base_size = 27)+labs(title ="GloVe  on Wikipedia 2014 and Gigaword 5th ed.")+scale_color_manual(values =c("orangered4","chartreuse4"))+ 
  theme(legend.position = c(0.8,.15)) +
  scale_shape_manual(values=c(15, 19)) 

genderLong <- gather(genderProjections, gender, similarity, c("he", "she"),factor_key=TRUE)
genderLong$stereotype <- as.factor(genderLong$stereotype)
genderLong$stereotypeID <- as.integer(genderLong$stereotype)
genderLong$genderID <- as.integer(genderLong$gender)

genderLong$treatmentID <- (genderLong$stereotypeID == genderLong$genderID)+1
#note:
#1: opposite
#2: same


model <- quap(
  alist(
    similarity ~ dnorm(mu,sigma),
    mu <- m[treatmentID],
    m[treatmentID] ~ dnorm(10,5),
    sigma ~ dexp(1)
  ),
  data = genderLong
)

#postcheck(model)


precis(model, depth = 2, pars = "m")

plot(precis(model, depth = 2, pars="m"), labels = c("opposite", "same") )




