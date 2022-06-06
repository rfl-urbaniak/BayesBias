library(rethinking)


extractPrecis <- function(modelResult, name, location = "resultsDFs/"){
  precisResult <- precis(modelResult, depth = 2)
  saveRDS(precisResult,file = paste(location, name, "DF.rds", sep = ""))
  return(precisResult)
}