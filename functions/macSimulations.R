# 2 protected groups in a class, eg. gender -> man, woman
# calculate the average inside the group and then average all groups


knitr::opts_knit$set(root.dir = '../')
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)

# BASIC SIMULATION -> samples=5, mean=0, st=0.08

# X = {t1,t2,t3,t4,t5,t6,t7,t8}, Y = {t9,t10,t11,t12,t13,t14,t15,t16}


# does it hide existing difference between the class and attribute?

# visualize systematic difference
# calculate simulation
# save p-value (check also p-value when no difference) -> two variants

set.seed(123456)
# X
t1 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t2 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t3 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t4 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t5 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t6 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t7 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))

# Y
t8 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t9 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t10 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t11 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t12 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t13 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))
t14 <- data.frame(A  = rnorm(8,1,0.08), B = rnorm(8,1,0.08))


# Mean values for X
wholeX = rbind(t1,t2,t3,t4,t5,t6,t7)
meanX = mean(c(wholeX$A,wholeX$B))
meanX

# Mean values for Y
wholeY = rbind(t8,t9,t10,t11,t12,t13,t14)
meanY = mean(c(wholeY$A,wholeY$B))
meanY

# Final mean
finalMean = mean(c(meanX, meanY))
finalMean

# look at p-values






# CALCULATE BIAS FROM SIMULATION
# 
# s <- function (table){ mean(table$A) - mean(table$B)}
# whole <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16)
# # st from all samples
# rawsd <- sd(c(whole$A,whole$B))
# # st from the means 
# factor <- sd(c(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8),s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16)))
# rawsd/factor
# numerator <-  mean(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8)) - mean(s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16))
# print(list(factor = factor,numerator = numerator, bias = numerator / factor))
# 
# 
biasesD0 <- numeric(1000)

for(i in 1:10000){
  t1 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t2 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t3 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t4 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t5 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t6 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t7 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t8 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t9 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t10 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t11 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t12 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t13 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t14 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t15 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))
  t16 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,0,0.08))

  factorD0 <- sd(c(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8),s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16)))
  numeratorD0 <-  mean(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8)) - mean(s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16))
  biasesD0[i] <- numeratorD0/factorD0
}


ggplot()+geom_histogram(aes(x=biasesD0, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for same means and sd =.08")+ xlab("bias")












