library(bnlearn)
library(Rgraphviz)
library(knitr)
library(kableExtra)

cancer1 <- empty.graph(nodes = c("PS","SH","S","C"))
cancer1.arcs <- matrix(c("PS", "SH",
                         "PS", "S",
                         "SH", "C",
                         "S", "C"),
                       byrow = TRUE, ncol = 2,
                       dimnames = list(NULL, c("from", "to")))
arcs(cancer1) = cancer1.arcs
graphviz.plot(cancer1)


#alternatively
cancer2DAG <- model2network("[SH|PS][PS][S|PS][C|SH:S]")

graphviz.plot(cancer2DAG)



source("cptCreate.R")


PSprob <- priorCPT("PS",prob1 = .3) 

Sprob <-  singleCPT(eNode = "S", hNode = "PS", probEifHS1 = .4 , probEifHS2 =  .2)


SHprob <- singleCPT(eNode = "SH", hNode = "PS", probEifHS1 = .8, probEifHS2 = .3)

Cprob <- doubleCPT(eNode= "C", h1Node = "S", h2Node = "SH",
                   probEifH1S1H2S1 = .6,
                   probEifH1S1H2S2 = .4,
                   probEifH1S2H2S1 = .1,
                   probEifH1S2H2S2 = .01)

cancerCPT <- list(PS = PSprob, S = Sprob, SH = SHprob, C = Cprob)

cancerBN <- custom.fit(cancer2DAG,cancerCPT)


graphviz.chart(cancerBN, grid = TRUE, bg = "beige", bar.col = "black")


graphviz.chart(cancerBN, grid = FALSE, type = "barprob", layout = "neato", scale = c(1,1.2), 
               main="marginal probabilities for the cancer BN")



