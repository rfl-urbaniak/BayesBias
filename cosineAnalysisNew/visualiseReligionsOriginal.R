setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions/visualisationTools.R")

religion <- read.csv("datasets/religion_cosine_table.csv")

religionVisualisations <- allVisualisations(religion)

religionVisualisations
