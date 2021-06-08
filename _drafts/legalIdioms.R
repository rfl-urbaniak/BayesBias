#Packages
library(bnlearn)
library(Rgraphviz)
library(gRain)


#Wrapper functions (the file needs to be in your working directory)
source("cptCreate.R")




# Hypothesis - evidence
HE <- model2network("[Evidence|Hypothesis][Hypothesis]")

graphviz.plot(HE)

Hypothesis.prob <-  array(c(0.01, 0.99), dim = 2, dimnames = list(HP =  c("guilty","not guilty")))

Evidence.prob <- array(c( 0.99999, 0.00001, 0, 1), dim = c(2,2),dimnames = list(Evidence = c("DNA match","no match"), Hypothesis = c("guilty","not guilty")))

HEcpt <- list(Hypothesis=Hypothesis.prob,Evidence=Evidence.prob)
HEbn = custom.fit(HE,HEcpt)


# Hypothesis - two pieces of evidence

HEEdag <- model2network("[H][W|H][DNA|H]")
graphviz.plot(HEE.dag)

Hprob <- array(c(0.01, 0.99), dim = 2, 
                dimnames = list(h = c("murder","nomurder")))

Wprob <- array(c( 0.7, 0.3, 0.4, 0.6), dim = c(2,2),dimnames = list(W= c("seen","notseen"), H = c("murder","nomurder")))

DNAprob <- array(c( 1, 0, 0.001, 0.999), dim = c(2,2),
                  dimnames = list(DNA =c("dnamatch","nomatch"),
                                  H = c("murder","nomurder")))



HEEcpt <- list(H=Hprob,W=Wprob,DNA = DNAprob)

HEEbn <- custom.fit(HEEdag,HEEcpt)

# Calculating probabilities in HEE with exact inference 
library(gRain)



#convert to a junction tree for calculations
junction <- compile(as.grain(HEEbn))

#update with match and seen
junctionMS <- setEvidence(junction, nodes = c("DNA","W"), states = c("dnamatch","seen") )
querygrain(junctionMS)$H


#update with match but not seen
junctionMN <- setEvidence(junction, nodes = c("DNA","W"), states = c("dnamatch","notseen"))
querygrain(junction.mn)$H



#update with no match
junctionNOMATCH <- setEvidence(junction, nodes = c("DNA"), states = c("nomatch"))
querygrain(junctionNOMATCH)$H


#convert to a BNs with propagation
HEEms <- as.bn.fit(junctionMS, including.evidence = TRUE) 
HEEmn <- as.bn.fit(junctionMN, including.evidence = TRUE) 
HEEnomatch <- as.bn.fit(junctionNOMATCH, including.evidence = TRUE) 



#plot updated BNs
graphviz.chart(HEEms, grid = FALSE, type = "barprob",  scale = c(1.5,1.5), 
               main="DNA match and  witness evidence")



graphviz.chart(HEEmn, grid = FALSE, type = "barprob",  scale = c(1.5,1.5), 
               main="DNA match and negative witness evidence")



graphviz.chart(HEEnomatch, grid = FALSE, type = "barprob",  scale = c(1.5,1.5), 
               main="DNA match and  no witness evidence")


#cause - consequence
CauseCon <-  model2network("[Cause][Consequence|Cause]")
graphviz.plot(CauseCon)

#measurement
Measurement <- model2network("[Accuracy][Actual value][Observed value|Accuracy:Actual value]")
graphviz.plot(Measurement)

#synthesis
Definitional <- model2network("[Distance][Time][Velocity|Distance:Time]")
graphviz.plot(Definitional)

#evidence accuracy
evidenceAccuracy <-  model2network("[Accuracy of evidence][Excess alcohol level][Evidence for excess|Accuracy of evidence:Excess alcohol level]")
graphviz.plot(evidenceAccuracy)


#opportunity
opportunity <- model2network("[H1][H2|H1][A1][A2][E1|H1:A1][E2|H1:A2]")
graphviz.plot(opportunity)


#dependency
cameras <- model2network("[H][D][C1|H][C2|H:C1:D]")
graphviz.plot(cameras, layout = "neato")


#alibi
alibi <- model2network("[S present][S guilty|S present][Alibi accuracy|S guilty][Alibi|Alibi accuracy:S present]")
graphviz.plot(alibi)


#mixed DNA
DNA789 <- model2network("[S is C1][S is C2][Genotype of C1|S is C1][Genotype of C2|S is C2][S is the source|S is C1:S is C2][(7,8,9) found|Genotype of C1:Genotype of C2]")
graphviz.plot(DNA789)


#Sally Clark


#define the structure of the Sally Clark BN
SallyClarkDAG <- model2network("[Abruising|Acause][Adisease|Acause][Bbruising|Bcause][Bdisease|Bcause][Acause][Bcause|Acause][NoMurdered|Acause:Bcause][Guilty|NoMurdered]")

#plot 
graphviz.plot(SallyClarkDAG)



#CPTs as used in Fenton & al.

AcauseProb <-prior.CPT("Acause","SIDS","Murder",0.921659)
AbruisingProb <- single.CPT("Abruising","Acause","Yes","No","SIDS","Murder",0.01,0.05)
AdiseaseProb <- single.CPT("Adisease","Acause","Yes","No","SIDS","Murder",0.05,0.001)
BbruisingProb <- single.CPT("Bbruising","Bcause","Yes","No","SIDS","Murder",0.01,0.05)
BdiseaseProb <- single.CPT("Bdisease","Bcause","Yes","No","SIDS","Murder",0.05,0.001)
BcauseProb <- single.CPT("Bcause","Acause","SIDS","Murder","SIDS","Murder",0.9993604,1-0.9998538)

#E goes first; order: last variable through levels, second last, then first
NoMurderedProb <- array(c(0, 0, 1, 0, 1, 0, 0,1,0,1,0,0), dim = c(3, 2, 2),dimnames = list(NoMurdered = c("both","one","none"),Bcause = c("SIDS","Murder"), Acause = c("SIDS","Murder")))

#this one is definitional
GuiltyProb <-  array(c( 1,0, 1,0, 0,1), dim = c(2,3),dimnames = list(Guilty = c("Yes","No"), NoMurdered = c("both","one","none")))

# Put CPTs together
SallyClarkCPT <- list(Acause=AcauseProb,Adisease = AdiseaseProb,
                      Bcause = BcauseProb,Bdisease=BdiseaseProb,
                      Abruising = AbruisingProb,Bbruising = BbruisingProb,
                      NoMurdered = NoMurderedProb,Guilty=GuiltyProb)

# join with the DAG to get a BN
SallyClarkBN <- custom.fit(SallyClarkDAG,SallyClarkCPT)

graphviz.chart(SallyClarkBN,type="barprob", scale = c(0.7,1.3), main = "Priors in the Sally Clark case")



#convert to a junction tree
SallyClarkJN <- compile(as.grain(SallyClarkBN))

#the prior of guilt 
querygrain(SallyClarkJN, node = "Guilty")


#say bruising was found on child A:
SallyClarkJNbra <- setEvidence(SallyClarkJN, nodes = c("Abruising"), states = c("Yes"))
querygrain(SallyClarkJNbra, node = "Guilty")

#say bruising is also found on child B:
SallyClarkJNbrab <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising"),
                                states = c("Yes","Yes"))
querygrain(SallyClarkJNbrab, node = "Guilty")


#if no sings of disease in children:
SallyClarkJNbrabNoDisease <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                                states = c("Yes","Yes", "No", "No"))
querygrain(SallyClarkJNbrabNoDisease, node = "Guilty")


# disease in Child A
SallyClarkJNdiseaseA <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("Yes","Yes", "Yes", "No"))
querygrain(SallyClarkJNdiseaseA, node = "Guilty")


SallyClarkFinalBN <- as.bn.fit(SallyClarkJNdiseaseA, including.evidence = TRUE)

graphviz.chart(SallyClarkFinalBN,type="barprob", scale = c(0.7,1.3), main = "Appeal posteriors in the Sally Clark case")


#disease in both children
SallyClarkJNdiseaseAB <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("Yes","Yes", "Yes", "Yes"))
querygrain(SallyClarkJNdiseaseAB, node = "Guilty")


#a simple scenario
ScenarioBN <- model2network("[Scenario][State/event 1|Scenario][State/event 2|Scenario:State/event 1][State/event 3|Scenario:State/event 2][Guilt|Scenario][Evidence 1|State/event 1][Evidence 2|State/event 2][Evidence 3|State/event 3]")
graphviz.plot(ScenarioBN)


#constraint
constraint <- model2network("[Constraint|Node 1:Node 2][Node 1][Node 2]")

#merging
ScenarioMerged <- model2network("[Scenario 1][Scenario 2][Event 1|Scenario 1][Event 2|Scenario 1:Event 1][Event 3|Scenario 2][Event 4|Scenario 2][Guilt|Scenario 1:Scenario 2][Evidence 1|Event 1][Evidence 2|Event 2][Evidence 3|Event 3][Evidence 4|Event 4]")
graphviz.plot(ScenarioMerged)

