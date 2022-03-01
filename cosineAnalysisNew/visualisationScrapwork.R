library(ggplot2)
library(ggthemes)

#religion <- read.csv("datasets//religion_cosine_table.csv")

#head(religion)


#dataset <- religion
#protected <- "judaism"

visualiseProtected <- function(dataset, protected){
  bunch <- dataset[dataset$protected_word == protected,]
  bunch <- bunch[order(bunch$cosine_distance),]
  bunch$xaxis <- rev(seq(0.01,sqrt(max(bunch$cosine_distance^2/40)),length.out = nrow(bunch)))
  bunch$yaxis <-  sqrt(bunch$cosine_distance^2 - bunch$xaxis^2)

#check if x and y yield the right distances
#check <- bunch[bunch$yaxis^2+bunch$xaxis^2 != bunch$cosine_distance^2,]
#approximate <- check$xaxis^2 + check$yaxis^2
#squaredDistance <- check$cosine_distance^2
#cbind(approximate,squaredDistance)
#ok, just rounding errors

#closest to the right

closestx <- bunch$xaxis[1]
closesty <- bunch$yaxis[1]
closestDistance <- bunch$cosine_distance[1]

furthestx <- bunch$xaxis[nrow(bunch)]
furthesty <- bunch$yaxis[nrow(bunch)]
furthestDistance <- bunch$cosine_distance[nrow(bunch)]

plot <- ggplot(bunch) + geom_point(aes(x=0, y=0), colour="black", size =10,alpha=0.02)+
          geom_point(aes(x=xaxis,y=yaxis, color = connection, size = cosine_similarity), alpha = 0.3)+ 
  geom_segment(aes(x= 0, y = 0, xend = closestx,yend=closesty),alpha= 0.01, size = 0.15)+
  geom_segment(aes(x= 0, y = 0, xend = furthestx,yend=furthesty),alpha=0.01,size = 0.15)+
  annotate("text", x = 0.5*closestx, y = 0.5*closesty-0.05, label = round(closestDistance,3), size = 3)+
  annotate("text", x = 1.5*furthestx, y = 0.5*furthesty, label = round(furthestDistance,3), size = 3)+
  annotate("text", x = -0.01, y = -0.1, label = protected, size = 4)+
  geom_text(aes(label = word_to_compare,x=xaxis+0.05,y=yaxis+0.03,color = connection), size = 3)+
  theme_void()+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  scale_x_continuous(limits = c(-0.2,1.5*max(bunch$xaxis)))+
  scale_y_continuous(limits = c(-0.2,1.05*max(bunch$cosine_distance)))+
  scale_color_manual(values=c("chartreuse4", "orangered4", "gray"))+
  labs(size = "cosine similarity",color = "connection type")

return(plot)
}


allVisualisations <- function(dataset){
protectedWords <-unique(dataset$protected_word)
visualisations <- list()
for(i in 1:length(protectedWords)){
visualisations[[i]] <-   visualiseProtected(dataset,protectedWords[i])
}
return(visualisations)
}





