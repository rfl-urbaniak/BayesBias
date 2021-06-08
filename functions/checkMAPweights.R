library(ggplot2)
library(ggthemes)
library(rethinking)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

religion <- read.csv("../datasets/religion_cosine_table.csv")


dataset <- religion

dataset$associated <- ifelse(dataset$connection == "associated", 1, 0)
dataset$different <- ifelse(dataset$connection == "different", 1, 0)

#build models
null  <- map(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu ~  dnorm(1,.5),
    sigma  ~ dcauchy(0,1)
  ),
  data = dataset,
  start=list(mu = 1, sigma= .3)
)


associated  <- map(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu <- no + a * associated,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = dataset,
  start=list(no = 1, a = 0, sigma= .3)
)


type  <- map(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu <- no + a * associated  + d * different,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    d ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = dataset,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)


compare(null, associated, type)




associated.st  <- map2stan(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu <- no + a * associated,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = dataset,
  chains=2 , iter=4000 , warmup=1000,
  start=list(no = 1, a = 0, sigma= .3)
)


type.st  <- map2stan(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu <- no + a * associated  + d * different,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    d ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = dataset,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)



compare(associated.st, type.st)


#extract samples
post.type.st <- extract.samples (type.st, n = 1e4 )

#calculate mu for the type model
mu.type.no <- post.type.st$no 
mu.type.associated <- post.type.st$no + post.type.st$a
mu.type.different <- post.type.st$no + post.type.st$d

#put the means together for plotting
mu.type.no.plotting <- data.frame(mu = mu.type.no, connection = "none")
mu.type.associated.plotting <- data.frame(mu = mu.type.associated, connection = "associated")
mu.type.different.plotting <- data.frame(mu = mu.type.different, connection = "different")

mu.plotting <- rbind(mu.type.no.plotting, mu.type.associated.plotting, mu.type.different.plotting)



ggplot(dataset, aes(x=connection,
                  y = cosine_distance))+geom_point(size=1, alpha = 0.4)+theme_tufte()+xlab("connection") +geom_point(data = mu.plotting,aes(x = connection, y = mu), color = "azure2", alpha = 0.01, size = 2) +geom_point(aes(x="associated",y = HPDI( mu.type.associated )[1]), shape =2, size = 3, color = "orange2")+geom_point(aes(x="associated",y = HPDI( mu.type.associated )[2]), shape = 6, size = 3, color = "orange2")+
  geom_point(aes(x="different",y = HPDI( mu.type.different )[1]), shape =2, size = 3, color = "orange2")+geom_point(aes(x="different",y = HPDI( mu.type.different )[2]), shape = 6, size = 3, color = "orange2")+
  geom_point(aes(x="none",y = HPDI( mu.type.no )[1]), shape =2, size = 3, color = "orange2")+geom_point(aes(x="none",y = HPDI( mu.type.no )[2]), shape = 6, size = 3, color = "orange2")+ylab("cosine distance")
