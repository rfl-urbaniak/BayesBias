
library(rethinking)
source("cosineAnalysis/functions/cleanDataset.R")


religion <- read.csv("cosineAnalysis/datasets/religionReddit.csv")[-1]

#religion <- cleanDataset(religion,c("christian","human","jewish","muslim","neutral"))





religion.adh.st <- map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + a * associated  + d * different + h * human,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    d ~ dnorm(0,1),
    h ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = religion,
  chains=2 , iter=4000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)






religion$pw <- coerce_index(religion$protectedWord)



religion.Separate <- map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + a[pw] * associated,
    a[pw] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    no ~ dnorm(1,.5)
  ),
  data = religion,
  chains=2 , iter=4000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)





#+ a * associated,
# d[pw] ~ dnorm(0,1),
# h[pw] ~ dnorm(0,1),
# + d * different + h * human,
#,