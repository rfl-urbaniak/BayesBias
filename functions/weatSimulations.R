knitr::opts_knit$set(root.dir = '../')
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)

# BASIC SIMULATION -> samples=5, mean=0, st=0.05

set.seed(123)
t1 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
t2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
t3 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
t4 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))

xlimits <- c(-.3,.3)

t1Plot <- ggplot(t1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t1 in X")+xlim(xlimits)
t2Plot <- ggplot(t2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t2 in X")+xlim(xlimits)
t3Plot <- ggplot(t3)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t3 in Y")+xlim(xlimits)
t4Plot <- ggplot(t4)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t4 in Y")+xlim(xlimits)
# grid.arrange(t1Plot,t2Plot, t3Plot, t4Plot, ncol=2)

t1Plot
t2Plot
t3Plot
t4Plot

# CALCULATE BIAS FROM SIMULATION

s <- function (table){ mean(table$A) - mean(table$B)}
whole <- rbind(t1,t2,t3,t4)
# st from all samples
rawsd <- sd(c(whole$A,whole$B))
# st from the means 
factor <- sd(c(s(t1),s(t2),s(t3),s(t4)))
rawsd/factor
numerator <-  mean(s(t1),s(t2)) - mean(s(t3),s(t4))
print(list(factor = factor,numerator = numerator, bias = numerator / factor))


# VISUALISE 1000 x BASIC SIMULATION
biasesNull <- numeric(10000)
for(i in 1:10000){
  t1 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
  t2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
  t3 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
  t4 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,0,0.05))
  
  factor <- sd(c(s(t1),s(t2),s(t3),s(t4)))
  numerator <-  mean(s(t1),s(t2)) - mean(s(t3),s(t4))
  biasesNull[i]  <- numerator / factor
}
ggplot()+geom_histogram(aes(x=biasesNull, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for identical means and sd =.05")+ xlab("bias")


# SIMULATION WITH SMALLER STD
set.seed(124)
t1v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
t2v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
t3v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
t4v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))

xlimits <- c(-.3,.3)


t1vPlot <- ggplot(t1v)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t1 in X")+xlim(xlimits)
t2vPlot <- ggplot(t2v)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t2 in X")+xlim(xlimits)
t3vPlot <- ggplot(t3v)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t3 in Y")+xlim(xlimits)
t4vPlot <- ggplot(t4v)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t4 in Y")+xlim(xlimits)
# grid.arrange(t1vPlot,t2vPlot, t3vPlot, t4vPlot, ncol=2)

t1vPlot
t2vPlot
t3vPlot
t4vPlot

# BIAS FOR SIMULATION WITH SMALLER STD

factorV <- sd(c(s(t1v),s(t2v),s(t3v),s(t4v)))
numeratorV <-  mean(s(t1v),s(t2v)) - mean(s(t3v),s(t4v))
print(list(factor = factorV,numerator = numeratorV, bias = numeratorV / factorV))

# VISUALIZE 1000 SIMULATION WITH SMALLER STD

set.seed(124)

biasesLowVariance <- numeric(10000)
for(i in 1:10000){
  t1v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
  t2v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
  t3v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
  t4v <- data.frame(A  = rnorm(5,0,0.001), B = rnorm(5,0,0.001))
  
  factorV <- sd(c(s(t1v),s(t2v),s(t3v),s(t4v)))
  
  numeratorV <-  mean(s(t1v),s(t2v)) - mean(s(t3v),s(t4v))
  
  biasesLowVariance[i] <- numeratorV / factorV
}
ggplot()+geom_histogram(aes(x=biasesLowVariance, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for identical means and sd =.001")+ xlab("bias")


# SIMULATION WITH DIFFERENCES IN MEANS 
set.seed(766)
t1d2 <- data.frame(A  = rnorm(5,.1,0.05), B = rnorm(5,0,0.05))
t2d2 <- data.frame(A  = rnorm(5,.1,0.05), B = rnorm(5,0,0.05))
t3d2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,.1,0.05))
t4d2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,.1,0.05))

t1d2Plot <- ggplot(t1d2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t1 in X")+xlim(xlimits)

t2d2Plot <- ggplot(t2d2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t2 in X")+xlim(xlimits)

t3d2Plot <- ggplot(t3d2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t3 in Y")+xlim(xlimits)

t4d2Plot <- ggplot(t4d2)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t4 in Y")+xlim(xlimits)

# grid.arrange(t1d2Plot,t2d2Plot, t3d2Plot, t4d2Plot, ncol=2,   
#              top = textGrob("different means, sd=0.05",gp=gpar(fontsize=15,font=1)))

# BIAS FOR DIFFERENT MEANS
factorD2 <- sd(c(s(t1d2),s(t2d2),s(t3d2),s(t4d2)))
numeratorD2 <-  mean(s(t1d2),s(t2d2)) - mean(s(t3d2),s(t4d2))
biasD2 <- numeratorD2 / factorD2
biasD2


# VISUALIZATION OF 1000 SIMULATION FOR DIFFERENT MEANS 
biasesD2 <- numeric(10000)
for(i in 1:10000){
  t1d2 <- data.frame(A  = rnorm(5,.1,0.05), B = rnorm(5,0,0.05))
  t2d2 <- data.frame(A  = rnorm(5,.1,0.05), B = rnorm(5,0,0.05))
  t3d2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,.1,0.05))
  t4d2 <- data.frame(A  = rnorm(5,0,0.05), B = rnorm(5,.1,0.05))
  
  factorD2 <- sd(c(s(t1d2),s(t2d2),s(t3d2),s(t4d2)))
  numeratorD2 <-  mean(s(t1d2),s(t2d2)) - mean(s(t3d2),s(t4d2))
  biasesD2[i] <- numeratorD2/factorD2
}

ggplot()+geom_histogram(aes(x=biasesD2, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for different means and sd =.05")+ xlab("bias")


# SIMULATION WITH STD HIGHER = 0.1
set.seed(766)
t1d1 <- data.frame(A  = rnorm(5,.1,0.1), B = rnorm(5,0,0.1))
t2d1 <- data.frame(A  = rnorm(5,.1,0.1), B = rnorm(5,0,0.1))
t3d1 <- data.frame(A  = rnorm(5,0,0.1), B = rnorm(5,.1,0.1))
t4d1 <- data.frame(A  = rnorm(5,0,0.1), B = rnorm(5,.1,0.1))

t1d1Plot <- ggplot(t1d1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t1 in X")+xlim(xlimits)
t2d1Plot <- ggplot(t2d1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t2 in X")+xlim(xlimits)
t3d1Plot <- ggplot(t3d1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t3 in Y")+xlim(xlimits)
t4d1Plot <- ggplot(t4d1)+geom_point(aes(y = 0, x = A), size = 1, alpha = 0.5)+geom_point(aes(y = 0.1, x = B), col = "skyblue", size = 1, alpha = 0.5)+theme_tufte()+xlab("similarity")+scale_y_continuous(limits = c(-.05,.15),breaks = c(0,0.1), labels = c("A","B"))+ylab("group") + geom_vline(aes(xintercept = mean(A)), size = 0.3) + geom_vline(aes(xintercept = mean(B)), size = 0.3, col = "skyblue") +ggtitle("Similarities for term t4 in Y")+xlim(xlimits)

# grid.arrange(t1d1Plot,t2d1Plot, t3d1Plot, t4d1Plot, ncol=2)
t1d1Plot
t2d1Plot
t3d1Plot
t4d1Plot

# BIAS FOR STD = 0.1
factorD1 <- sd(c(s(t1d1),s(t2d1),s(t3d1),s(t4d1)))
numeratorD1 <-  mean(s(t1d1),s(t2d1)) - mean(s(t3d1),s(t4d1))
biasD1 <- numeratorD1 / factorD1
biasD1

# VISUALIZE SIMULATION 1000 FOR STD = 0.1
biasesD1 <- numeric(10000)

for(i in 1:10000){
  t1d1 <- data.frame(A  = rnorm(5,.1,0.1), B = rnorm(5,0,0.1))
  t2d1 <- data.frame(A  = rnorm(5,.1,0.1), B = rnorm(5,0,0.1))
  t3d1 <- data.frame(A  = rnorm(5,0,0.1), B = rnorm(5,.1,0.1))
  t4d1 <- data.frame(A  = rnorm(5,0,0.1), B = rnorm(5,.1,0.1))
  
  factorD1 <- sd(c(s(t1d1),s(t2d1),s(t3d1),s(t4d1)))
  numeratorD1 <-  mean(s(t1d1),s(t2d1)) - mean(s(t3d1),s(t4d1))
  biasesD1[i] <- numeratorD1/factorD1
}

ggplot()+geom_histogram(aes(x=biasesD1, y = ..density..), alpha = 0.6, bins=50)+
  theme_tufte()+labs(title="10k biases for different means and sd =.001")+ xlab("bias")



