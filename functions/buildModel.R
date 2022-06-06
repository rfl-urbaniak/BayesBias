library(rethinking)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(grid)

buildModel <- function(dataset){
  options(buildtools.check = function(action) TRUE )
  startTime <- Sys.time()
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
      sigma <- s[connection],
      s[connection] ~  dexp(2)
    ),   data = dataset, chains=2 , iter=8000 , warmup=1000,  log_lik = TRUE, cores = 4
  )
  endTime <- Sys.time()
  time <- endTime - startTime
  browseURL('https://www.youtube.com/watch?v=Lx1iH8DgrLE&list=RDLx1iH8DgrLE&start_radio=1')
  print(time)
  return(modelResult)
}
