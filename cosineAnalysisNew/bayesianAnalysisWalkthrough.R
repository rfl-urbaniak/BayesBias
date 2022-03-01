library(ggplot2)
library(ggthemes)
library(rethinking)



#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

religion <- read.csv("cosineAnalysis/datasets/religionOriginal.csv")[-1]

colnames(religion) <- c("protectedWord","wordToCompare","wordClass",
                        "cosineDistance","cosineSimilarity","connection")

dataset <- religion
protected <- "muslim"
bunch <- dataset[dataset$protectedWord == protected,]
bunch$associated <- ifelse(bunch$connection == "associated", 1, 0)
bunch$different <- ifelse(bunch$connection == "different", 1, 0)



#plot priors and think about them
sim <- seq(-1,1,by = 0.001)
dis <- 1-sim
max(dis)

p <- ggplot(data = data.frame(distance = dis), 
            mapping = aes(x = distance))+theme_tufte()


#mean(bunch$cosine_distance[bunch$connection == "none"])

sd(bunch$cosine_distance[bunch$connection == "none"])

#mean(dataset$cosine_distance[dataset$connection == "none"])




none <- function(x) dnorm(x,1,.5)
cau <- function(x) dcauchy(x,0,1)
par <- function(x) dnorm(x,0,1)

p + stat_function(fun = none)+xlim(c(-0.5,2.5))
p + stat_function(fun = cau)+xlim(c(0,2))
p + stat_function(fun = par)+xlim(c(-2,2))+xlab("distance change")


#build map and stan models

null  <- map(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu ~  dnorm(1,.5),
    sigma  ~ dcauchy(0,1)
  ),
  data = bunch,
  start=list(mu = 1, sigma= .3)
)


null.st <- map2stan(
  alist(
    cosine_distance ~ dnorm(mu,sigma),
    mu ~ dnorm(1,.5),
    sigma ~ dcauchy(0,1)
  ),
  data = bunch,
#  start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000,
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
  data = bunch,
  start=list(no = 1, a = 0, sigma= .3)
)



associated.st  <- map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + a * associated,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = bunch,
  chains=2 , iter=4000 , warmup=1000,
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
  data = bunch,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)


type.st <- map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + a * associated  + d * different,
    no ~ dnorm(1,.5),
    a ~ dnorm(0,1),
    d ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = bunch,
  chains=2 , iter=4000 , warmup=1000,
  start=list(no = 1, a = 0, d = 0, sigma= .3)
)

library(rstan)
devtools::install_github("rmcelreath/rethinking", force = TRUE)
plot(associated.st)
library(rethinking)
plot(type.st)


str(type.st)

type.st

type.st

devtools::install_github("rmcelreath/rethinking", force = TRUE)


# diagnostics
library(rstan)
library(rethinking)
rstan::plot(null.st)

plot(null.st)

precis_plot(precis(null.st))
plot(precis(associated.st))

plot(precis(type.st))

plot(type.st)

## look at parameters
colnames(precis(null))
str(as.data.frame(precis(null)))


dimnames(precis(null))

precis(null)

nullDf <- cbind(precis(null)[1],precis(null)[2],precis(null)[3],precis(null)[4])

precisToDf <- function (model){
  modelDf <- cbind(precis(model)[1],precis(model)[2],precis(model)[3],precis(model)[4])
  return(modelDf)
}

precisToDf(null)

precis(null) %>% kable(format = "latex",booktabs=T,
      linesep = "",  escape = FALSE, 
      caption = "Precis of the baseline model.",
      col.names = 1:7
) 

precis(null.st)

plot(precis(null.st))

precis(associated)
precis(associated.st)

precis(type)
precis(type.st)


plot(precis(null.st))
plot(precis(type.st))


# think about choosing models


#- filled points are in sample deviances
#- open points are Waics
#- se for WAIC are lines through open points
#- se and estimate for the difference between the top and the others are the triangles.



modelsMAP <- compare( null , associated , type )

str(compare( null , associated , type ))

modelsMAP



par("mfrow"=1)

plot( modelsMAP , SE=TRUE , dSE=TRUE )

modelsSTAN <- compare (associated.st, type.st)


modelsSTAN

?par

par(mfrow=c(1,1)) 

plot( modelsSTAN , SE=TRUE , dSE=TRUE )



#this is useful
coeftab(null.st , associated.st , type.st)

plot(coeftab(null.st , associated.st , type.st))





#extracting sampled parameter values
post.type.st <- as.data.frame(extract.samples (type.st, n = 1e4 ))



str(post.type.st)
head(post.type.st)

#calculate mu for the type model
mu.type.no <- post.type.st$no 
mu.type.associated <- post.type.st$no + post.type.st$a
mu.type.different <- post.type.st$no + post.type.st$d


#look at the means
precis( data.frame(mu.type.no,mu.type.associated,mu.type.different) )


#percentile interval
PI(mu.type.no)
PI(mu.type.associated)
PI(mu.type.different)

#HPDI
HPDI( mu.type.no )
HPDI( mu.type.associated )
HPDI( mu.type.different )

precis(data.frame(mu.type.no,mu.type.associated,mu.type.different))


#put the means together for plotting
mu.type.no.plotting <- data.frame(mu = mu.type.no, connection = "none")
mu.type.associated.plotting <- data.frame(mu = mu.type.associated, connection = "associated")
mu.type.different.plotting <- data.frame(mu = mu.type.different, connection = "different")




mu.plotting <- rbind(mu.type.no.plotting, mu.type.associated.plotting, mu.type.different.plotting)



head(mu.plotting)

#plot
#plot( cosine_distance ~ associated , data=bunch )
#abline( a=coef(associated)["no"] , b=coef(associated)["a"] )
#calculate mu from parameters
#predictive plot
#associated.seq <- c(0,1)
#different.seq <- c(0,1)

#mu <- link(type.st, data= data.frame(associated = associated.seq, different = different.seq))

#str(mu)

#mu.mean <- apply( mu , 2 , mean )
#mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

ggplot(bunch, aes(x=connection,
                  y = cosine_distance))+geom_point(size=2, alpha = 0.8)+theme_tufte()+xlab("connection") +geom_point(data = mu.plotting,aes(x = connection, y = mu), color = "azure2", alpha = 0.01, size = 5) +geom_point(aes(x="associated",y = HPDI( mu.type.associated )[1]), shape =2, size = 5, color = "orange2")+geom_point(aes(x="associated",y = HPDI( mu.type.associated )[2]), shape = 6, size = 5, color = "orange2")+
  geom_point(aes(x="different",y = HPDI( mu.type.different )[1]), shape =2, size = 5, color = "orange2")+geom_point(aes(x="different",y = HPDI( mu.type.different )[2]), shape = 6, size = 5, color = "orange2")+
  geom_point(aes(x="none",y = HPDI( mu.type.no )[1]), shape =2, size = 5, color = "orange2")+geom_point(aes(x="none",y = HPDI( mu.type.no )[2]), shape = 6, size = 5, color = "orange2")+ylab("cosine distance")

  
  
  




#model averaging

#joint <- ensemble( null,associated,type, data = bunch)

#mu <- apply( joint$link , 2 , mean )
#mu.PI <- apply( joint$link , 2 , PI )


#joint.en <- ensemble ( associated.st, type.st, data = bunch )

