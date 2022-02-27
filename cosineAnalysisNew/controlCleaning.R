controlReddit <- read.csv("cosineAnalysis/datasets/controlReddit.csv")[-1]
controlGoogle <- read.csv("cosineAnalysis/datasets/controlGoogle.csv")[-1]


controlRedditClean <- cleanDataset(controlReddit, c("asian", "black", "caucasian", "christian", "human", "jew", "man", "muslim", "neutral", "woman"))

controlGoogleClean <- cleanDataset(controlGoogle, c("asian", "black", "caucasian", "christian", "human", "jew", "man", "muslim", "neutral", "woman"))

humans <- controlRedditClean %>% filter(connection =="human")
humanWords <- droplevels(unique(humans$protectedWord))


reddit <- controlRedditClean[,1:(length(controlRedditClean)-4)]
google <- controlGoogleClean[,1:(length(controlGoogleClean)-4)]


reddit$humanProtected <- ifelse(reddit$protectedWord %in% humanWords, 1, 0)
google$humanProtected <- ifelse(reddit$protectedWord %in% humanWords, 1, 0)


reddit$humanPredicate <- ifelse(reddit$wordClass == "human", 1, 0)
reddit$activePredicate <- ifelse(reddit$wordClass != "neutral" & reddit$wordClass != "human", 1, 0)
reddit$asianPredicate <- ifelse(reddit$wordClass == "asian", 1, 0)
reddit$blackPredicate <- ifelse(reddit$wordClass == "black", 1, 0)
reddit$caucasianPredicate <- ifelse(reddit$wordClass == "caucasian", 1, 0)
reddit$christianPredicate <- ifelse(reddit$wordClass == "christian", 1, 0)
reddit$jewPredicate <- ifelse(reddit$wordClass == "jew", 1, 0)
reddit$manPredicate <- ifelse(reddit$wordClass == "man", 1, 0)
reddit$muslimPredicate <- ifelse(reddit$wordClass == "muslim", 1, 0)
reddit$womanPredicate <- ifelse(reddit$wordClass == "woman", 1, 0)


google$humanPredicate <- ifelse(google$wordClass == "human", 1, 0)
google$activePredicate <- ifelse(google$wordClass != "neutral" & google$wordClass != "human", 1, 0)
google$asianPredicate <- ifelse(google$wordClass == "asian", 1, 0)
google$blackPredicate <- ifelse(google$wordClass == "black", 1, 0)
google$caucasianPredicate <- ifelse(google$wordClass == "caucasian", 1, 0)
google$christianPredicate <- ifelse(google$wordClass == "christian", 1, 0)
google$jewPredicate <- ifelse(google$wordClass == "jew", 1, 0)
google$manPredicate <- ifelse(google$wordClass == "man", 1, 0)
google$muslimPredicate <- ifelse(google$wordClass == "muslim", 1, 0)
google$womanPredicate <- ifelse(google$wordClass == "woman", 1, 0)


controlReddit.model <- rethinking::map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + H * humanProtected  + hp * humanPredicate + ac * activePredicate,
    no ~ dnorm(1,1),
    H ~ dnorm(0,1),
    hp ~ dnorm(0,1),
    ac ~ dnorm(0,1),
    sigma ~ dcauchy(0,.5)
  ),
  data = reddit,
  start=list(no = 1, H = 0, hp = 0, ac = 0, sigma= .3)
)

controlGoogle.model <- rethinking::map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + H * humanProtected  + hp * humanPredicate + ac * activePredicate,
    no ~ dnorm(1,1),
    H ~ dnorm(0,1),
    hp ~ dnorm(0,1),
    ac ~ dnorm(0,1),
    sigma ~ dcauchy(0,.5)
  ),
  data = google,
  start=list(no = 1, H = 0, hp = 0, ac = 0, sigma= .3)
)




controlReddit.fullModel <- rethinking::map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + H * humanProtected  + hp * humanPredicate + asian * asianPredicate +
      bl * blackPredicate + ca * caucasianPredicate + chr * christianPredicate + 
      wo * womanPredicate + j * jewPredicate + man * manPredicate + mus * muslimPredicate,
    no ~ dnorm(1,.5),
    H ~ dnorm(0,1),
    hp ~ dnorm(0,1),
    asian ~ dnorm(0,1),
    bl ~ dnorm(0,1),
    ca ~ dnorm(0,1),
    chr ~ dnorm(0,1),
    wo ~ dnorm(0,1),
    j ~ dnorm(0,1),
    man ~ dnorm(0,1),
    mus ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = reddit,
  start=list(no = 1, H = 0, hp = 0, asian = 0, bl = 0, ca = 0, chr = 0, wo = 0, j = 0, man = 0, mus = 0, sigma= .3)
)


controlGoogle.fullModel <- rethinking::map2stan(
  alist(
    cosineDistance ~ dnorm(mu,sigma),
    mu <- no + H * humanProtected  + hp * humanPredicate + asian * asianPredicate +
      bl * blackPredicate + ca * caucasianPredicate + chr * christianPredicate + 
      wo * womanPredicate + j * jewPredicate + man * manPredicate + mus * muslimPredicate,
    no ~ dnorm(1,.5),
    H ~ dnorm(0,1),
    hp ~ dnorm(0,1),
    asian ~ dnorm(0,1),
    bl ~ dnorm(0,1),
    ca ~ dnorm(0,1),
    chr ~ dnorm(0,1),
    wo ~ dnorm(0,1),
    j ~ dnorm(0,1),
    man ~ dnorm(0,1),
    mus ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data = google,
  start=list(no = 1, H = 0, hp = 0, asian = 0, bl = 0, ca = 0, chr = 0, wo = 0, j = 0, man = 0, mus = 0, sigma= .3)
)


precis(controlReddit.model)
precis(controlGoogle.model)

precis(controlReddit.fullModel)
precis(controlGoogle.fullModel)


compare(controlReddit.fullModel,controlReddit.model)
compare(controlGoogle.fullModel,controlGoogle.model)




plot(coeftab(controlReddit.fullModel))


#now google
