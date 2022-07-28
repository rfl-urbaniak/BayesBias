# Simulation with more examples 

# Based on the WEAT 7 example:
# Math: math, algebra, geometry, calculus, equations, computation, numbers, addition.
# Arts: poetry, art, dance, literature, novel, symphony, drama, sculpture.
# Male terms: male, man, boy, brother, he, him, his, son.
# Female terms: female, woman, girl, sister, she, her, hers, daughter.

knitr::opts_knit$set(root.dir = '../')
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)

# BASIC SIMULATION -> samples=5, mean=0, st=0.08

# X = {t1,t2,t3,t4,t5,t6,t7,t8}, Y = {t9,t10,t11,t12,t13,t14,t15,t16}

set.seed(123456)
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

xlimits <- c(-.3,.3)

t1Plot <- ggplot(t1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t1 in X")+xlim(xlimits)
t2Plot <- ggplot(t2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t2 in X")+xlim(xlimits)
t3Plot <- ggplot(t8)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t3 in Y")+xlim(xlimits)
t4Plot <- ggplot(t9)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t4 in Y")+xlim(xlimits)
# grid.arrange(t1Plot,t2Plot, t3Plot, t4Plot, ncol=2)

t1Plot
t2Plot
t3Plot
t4Plot

# CALCULATE BIAS FROM SIMULATION

s <- function (table){ mean(table$A) - mean(table$B)}
whole <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16)
# st from all samples
rawsd <- sd(c(whole$A,whole$B))
# st from the means 
factor <- sd(c(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8),s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16)))
rawsd/factor
numerator <-  mean(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8)) - mean(s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16))
print(list(factor = factor,numerator = numerator, bias = numerator / factor))


# SIMULATION FOR SAME MEANS AND SMALL STD
biasesD0 <- numeric(10000)

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


quantile(biasesD0, c(0.025, 0.975))
mean(abs(biasesD0)>=1.82)



# SIMULATION FOR DIFFERENT MEANS (0.1) AND SMALL STD DEVIATION 
set.seed(123456)
biasesD1 <- numeric(10000)

for(i in 1:10000){
  t1 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t2 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t3 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t4 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t5 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t6 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t7 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t8 <- data.frame(A  = rnorm(8,.1,0.08), B = rnorm(8,0,0.08))
  t9 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t10 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t11 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t12 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t13 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t14 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t15 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  t16 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.1,0.08))
  
  factorD1 <- sd(c(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8),s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16)))
  numeratorD1 <-  mean(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8)) - mean(s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16))
  biasesD1[i] <- numeratorD1/factorD1
}

ggplot()+geom_histogram(aes(x=biasesD1, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for different means 0.1 and sd =.08")+ xlab("bias")

quantile(biasesD1, c(0.025, 0.975))
mean(abs(biasesD1)>=1.82)


# SIMULATION FOR DIFFERENT MEANS (0.4) AND SMALL STD DEVIATION 
set.seed(123456)
biasesD4 <- numeric(10000)

for(i in 1:10000){
  t1 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t2 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t3 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t4 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t5 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t6 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t7 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t8 <- data.frame(A  = rnorm(8,.4,0.08), B = rnorm(8,0,0.08))
  t9 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t10 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t11 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t12 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t13 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t14 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t15 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  t16 <- data.frame(A  = rnorm(8,0,0.08), B = rnorm(8,.4,0.08))
  
  factorD4 <- sd(c(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8),s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16)))
  numeratorD4 <-  mean(s(t1),s(t2),s(t3),s(t4),s(t5),s(t6),s(t7),s(t8)) - mean(s(t9),s(t10),s(t11),s(t12),s(t13),s(t14),s(t15),s(t16))
  biasesD4[i] <- numeratorD4/factorD4
}

ggplot()+geom_histogram(aes(x=biasesD4, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for different means 0.4 and sd =.08")+ xlab("bias")

quantile(biasesD4, c(0.025, 0.975))
mean(abs(biasesD4)>=1.82)


