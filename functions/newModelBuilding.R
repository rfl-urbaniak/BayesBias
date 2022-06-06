library(rethinking)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(grid)


religionReddit <- read.csv("./datasets/macWeatDatasets/religion_group_reddit_dataset.csv")[,-1]



dataset <- religionReddit


cleanDataset <- function (dataset) {

colnames(dataset) <- c("pw","word","stereotype", "distance", "similarity", "connection")
dataset$pw <- as.factor(dataset$pw)
dataset$word <- as.factor(dataset$word)
dataset$stereotype <- as.factor(dataset$stereotype)
dataset$connection <- as.factor(dataset$connection)



ifelse(sum( dataset$similarity < -1 | dataset$distance < -1) != 0, 
       print(
         paste("WARNING:  ",  sum( dataset$similarity < -1 | dataset$distance < -1), 
               " out of ", nrow(dataset), " (", sum( dataset$similarity < -1 | dataset$distance < -1)/nrow(dataset) * 100, 
               "%) missing comparisons have been removed!", sep = "")
       ),
       print("No word removal needed.")
)


dataset <- dataset[  dataset$similarity >= -1 | dataset$distance >= -1,]


dataset$associated <- as.integer(dataset$connection == "associated")
dataset$different <- as.integer(dataset$connection == "different")
dataset$human <- as.integer(dataset$connection == "human")
dataset$none <- as.integer(dataset$connection == "none")
dataset$pwi <- as.integer(dataset$pw)
return(dataset)
}


dataset <- cleanDataset(religionReddit)



#full hierarhical
modelResult <- ulam(
  alist(
    distance ~ dnorm(mu,sigma),
    mu <- d[pwi] * different + a[pwi] * associated + h[pwi] * human + n[pw] * none,
    d[pwi] ~ dnorm(dbar, dsigmabar),
    a[pwi] ~ dnorm(abar, asigmabar),
    h[pwi] ~ dnorm(hbar, hsigmabar),
    n[pwi] ~ dnorm(nbar, nsigmabar),
    dbar ~ dnorm(1,.3),
    abar ~ dnorm(1,.3),
    hbar ~ dnorm(1,.3),
    nbar ~ dnorm(1,.3),
    dsigmabar ~ dexp(2),
    asigmabar ~ dexp(2),
    hsigmabar ~ dexp(2),
    nsigmabar ~ dexp(2),
    sigma ~ dexp(2)
  ),   data = dataset, chains=2 , iter=8000 , warmup=1000,  log_lik = TRUE, cores = 4
)

head(dataset)

#relativized sigma
modelSigma <- ulam(
  alist(
    distance ~ dnorm(mu,sigma),
    mu <- d[pwi] * different + a[pwi] * associated + h[pwi] * human + n[pw] * none,
    d[pwi] ~ dnorm(dbar, dsigmabar),
    a[pwi] ~ dnorm(abar, asigmabar),
    h[pwi] ~ dnorm(hbar, hsigmabar),
    n[pwi] ~ dnorm(nbar, nsigmabar),
    dbar ~ dnorm(1,.3),
    abar ~ dnorm(1,.3),
    hbar ~ dnorm(1,.3),
    nbar ~ dnorm(1,.3),
    dsigmabar ~ dexp(2),
    asigmabar ~ dexp(2),
    hsigmabar ~ dexp(2),
    nsigmabar ~ dexp(2),
    sigma <- s[connection],
    s[connection] ~  dexp(2)
  ),   data = dataset, chains=2 , iter=8000 , warmup=1000,  log_lik = TRUE, cores = 4
)


head(dataset)

modelNoPooling <- ulam(
  alist(
    distance ~ dnorm(mu,sigma),
    mu <- d[pwi] * different + a[pwi] * associated + h[pwi] * human + n[pwi] * none,
    d[pwi] ~ dnorm(1, .3),
    a[pwi] ~ dnorm(1, .3),
    h[pwi] ~ dnorm(1, .3),
    n[pwi] ~ dnorm(1,.3),
    sigma ~ dexp(5)
  ),   data = dataset, chains=2 , iter=8000 , warmup=1000,  log_lik = TRUE, cores = 4
)






buildModel <- function(dataset){
options(buildtools.check = function(action) TRUE )
startTime <- Sys.time()
modelResult <- ulam(
  alist(
    distance ~ dnorm(mu,sigma),
    mu <- m +  d[pwi] * different + a[pwi] * associated + h[pwi] * human + n[pw] * none,
    m ~ dnorm(1,.5),
    d[pwi] ~ dnorm(dbar, dsigmabar),
    a[pwi] ~ dnorm(abar, asigmabar),
    h[pwi] ~ dnorm(hbar, hsigmabar),
    n[pwi] ~ dnorm(nbar, nsigmabar),
    dbar ~ dnorm(0,.2),
    abar ~ dnorm(0,.2),
    hbar ~ dnorm(0,.2),
    nbar ~ dnorm(0,.2),
    dsigmabar ~ dexp(5),
    asigmabar ~ dexp(5),
    hsigmabar ~ dexp(5),
    nsigmabar ~ dexp(5),
    sigma ~ dexp(5)
  ),   data = dataset, chains=2 , iter=8000 , warmup=1000,  log_lik = TRUE, cores = 4
)
endTime <- Sys.time()
time <- endTime - startTime
browseURL('https://www.youtube.com/watch?v=Lx1iH8DgrLE&list=RDLx1iH8DgrLE&start_radio=1')
print(time)
return(modelResult)
}


postcheck(modelResult)


savePrecis <- function(modelResult,name){
precisResult <- precis(modelResult, depth = 2)
saveRDS(precisResult,file = paste("resultsDFs/", name, "DF.rds", sep = ""))
}


savePrecis(modelResult, "religionReddit")

precis <- precisResult



precisSigma <- precis(modelSigma, depth = 2)

precisSigma

precis <- precisSigma

plotFromPrecis <- function (precis, dataset){

DFoverall <- rbind(head(precis,1),head(tail(precis, 9),4))
DFindividual <- head(precis[-1,], -9)

DFoverall <- DFoverall[-1,]
DFoverall$type <- c("different", "associated", "human", "none")
colnames(DFoverall) <- c("mean", "sd", "low", "high", "neff", "rhat", "type")



ifelse(nrow(DFindividual)/4 != nlevels(dataset$pw),
  print("WARNING: dataset and precis don't match!"),
  print("Dataset and precis match in size"))

DFindividual$pw <- as.factor(levels(dataset$pw)[rep((1:(nrow(DFindividual)/4)),4)])
DFindividual$type <- as.factor(c(rep("different", nrow(DFindividual)/4),rep("associated", 
                      nrow(DFindividual)/4),rep("human", nrow(DFindividual)/4), 
                       rep("none", nrow(DFindividual)/4)))

pwPlot <- ggplot() + geom_point(data = DFindividual, aes( x = reorder(pw, desc(pw)), y = mean, color = type),
                       position = position_nudge((as.integer(DFindividual$type)-1)*0.15), size = 1)+
  coord_flip()+theme_tufte()+expand_limits(x= c(-1, nrow(DFindividual)/4 +1))+
                   scale_color_manual(values = c("associated" ="orangered4", 
                                                 "different" = "chartreuse4",
                                                 "human" = "skyblue",
                                                 "none" = "grey")) +xlab("protected word")+
                  ylab("distance from overall baseline")+
  geom_segment(data = DFindividual,aes(x=reorder(pw, desc(pw)),xend=reorder(pw, desc(pw)), y  = `5.5%`, yend= `94.5%`,
                                       color = type),
               position = position_nudge((as.integer(DFindividual$type)-1)*0.15), size = .4, alpha = .4)+
  geom_hline(yintercept = 0, lty = 2, size = .2, alpha = .3)+ylim(-.5,.5) +ggtitle("Protected-word-relative coefficients")+ 
  theme(legend.position='none')



overallPlot <- ggplot() + geom_point(data = DFoverall, aes( x = type, y = mean, color = type), size = 1, position = position_dodge2(width = -1))+
  coord_flip()+theme_tufte() +  
  geom_segment(data = DFoverall,aes(x=type,xend = type, y  = low, yend= high,
                                                                   color = type))+ylim(-.5,.5)+
  scale_color_manual(values = c("associated" ="orangered4", 
                                "different" = "chartreuse4",
                                "human" = "skyblue",
                                "none" = "grey"))+ theme(legend.position = c(0.2, 0.6))+ggtitle("Overall coefficients")+
  ylab("distance from overall baseline")+
  geom_hline(yintercept = 0, lty = 2, size = .2, alpha = .3)


plotModel <- grid.arrange(overallPlot,pwPlot, ncol = 1)

return(plotModel)
}


plotFromPrecis(precis, dataset)






