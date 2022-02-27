library(dplyr)
library(forcats)

library(stringr)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load("cosineAnalysis/datasets/religionOriginalTable.RData")  


#dataset <- religionOriginalTable


visualiseTable <- function(dataset, orderVector = a){
parameterPlot <-   dataset %>% mutate(protectedClass = fct_reorder(protectedClass, a)) %>%
  ggplot()+
  geom_point(aes(x = protectedClass,y=a,color = "associated"))+
  geom_segment(aes(x=protectedClass,xend=protectedClass,y=alo,yend=ahigh), alpha = 0.3,
               color = "chartreuse4")+
  geom_segment(data = dataset[dataset$protectedClass == "full dataset",],aes(x=protectedClass, xend = protectedClass,y=min(dataset$alo),yend = max(dataset$ahigh)), size = 9, color = "red",
                alpha = 0.1)+geom_hline(yintercept = 0, size = 0.15, lty = 2)+
  geom_point(aes(x=protectedClass, y = d,color = "different"),position = position_nudge(0.15))+
  geom_segment(aes(x=protectedClass,xend=protectedClass,y=dlo,yend=dhigh, color ="different"), alpha = 0.3,
                position = position_nudge(0.15))+
  geom_point(aes(x=protectedClass, y = h,color = "human"),position = position_nudge(-0.15) )+
  geom_segment(aes(x=protectedClass,xend=protectedClass,y=hlo,yend=hhigh, color = "human"), alpha = 0.3,    position = position_nudge(-0.15))+
  scale_color_manual(values = c("chartreuse4","orangered4","grey"))+
  coord_flip()+theme_tufte()+xlab("protected class")+ylab("class coefficient")+ 
  labs(color = "connection")
print(parameterPlot)
}


visualisePWestimates <- function(data, model){
  pr <- as.data.frame(precis(model, depth = 2))
  pr$tags <- c(levels(data$pwFactor), "sigma")
  tags <- pr$tags
  pr <- cbind(pr,str_split_fixed(tags, "-", 2))
  pr <- pr[-nrow(pr),]
  colnames(pr) <- c("mean", "sd", "low", "high", "neff", "rhat", "tags", "pw", "connection")
  dataset <- pr
  
  associated <-  dataset[dataset$connection == "associated",]
  different <-   dataset[dataset$connection == "different",]
  human <-   dataset[dataset$connection == "human",]
  none <- dataset[dataset$connection == "none",]
  
  colors <- c("associated"="orangered","different"="chartreuse4","human"="skyblue", "none" ="grey")
  
  plot <-   ggplot(dataset, aes(x = pw, y = mean, color = connection))+
    geom_point(data= associated, aes(x = pw, y = mean, color = "associated"))+
    geom_segment(data = associated, aes(x=pw,xend=pw,y=low,yend=high,color = "associated"), alpha = 0.3
    ) +
    geom_point(data= different, aes(x = pw, y = mean, color = "different"), 
               position = position_nudge(0.15))+
    geom_segment(data = different, aes(x=pw,xend=pw,y=low,yend=high, color = "different"), alpha = 0.3,
                 position = position_nudge(0.15))+
    geom_point(data= human, aes(x = pw, y = mean, color = "human"), position = position_nudge(-0.15))+
    geom_segment(data = human, aes(x=pw,xend=pw,y=low,yend=high, color = "human"), alpha = 0.3,
                 position = position_nudge(-0.15))+
    geom_point(data= none, aes(x = pw, y = mean, 
                               color = "none"),  position = position_nudge(0.3))+
    geom_segment(data = none, aes(x=pw,xend=pw,y=low,yend=high,
                                  color = "none"),
                 alpha = 0.3, position = position_nudge(0.3))+
    coord_flip()+theme_tufte()+xlab("protected class")+ylab("connection coefficient")+ 
    labs(color = "connection")+ scale_color_manual(name = "connection", values = colors)
  
  return(plot)
}


#head(dataset)

#visualiseTable(religionOriginalTable)
