library(rethinking)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(grid)

# precis


plotFromPrecis <- function (precis, dataset, ylims = c(.5,1.2), list, embedding){
  
  
  # precis = precisWeat7Google
  # dataset = weat7Google
  # list = "Weat 7"
  # embedding = "Google"
  
  
  results <- list()
  DFoverall <-  precis[grep(c("^dbar|^abar|^hbar|^nbar"), rownames(precis)),]
  DFindividual <- precis[grep(c("^d\\[|^a\\[|^h\\[|^n\\["), rownames(precis)),]
  DFoverall$type <- c("different", "associated", "human", "none")
  colnames(DFoverall) <- c("mean", "sd", "low", "high", "neff", "rhat", "type")
  
  nrow(DFindividual)
    
  dataset
  
  ifelse(nrow(DFindividual)/4 != nlevels(dataset$pw),
         print("WARNING: dataset and precis don't match!"),
         print("Dataset and precis match in size"))
  
  
  DFindividual$pw <- as.factor(levels(dataset$pw)[rep((1:(nrow(DFindividual)/4)),4)])
  DFindividual$type <- as.factor(c(rep("different", nrow(DFindividual)/4),rep("associated", 
                                                                              nrow(DFindividual)/4),rep("human", nrow(DFindividual)/4), 
                                   rep("none", nrow(DFindividual)/4)))
  
  
  results$DFindividual <- DFindividual
  results$DFoverall <- DFoverall
  
  
  results$plotIndividual <- ggplot() + geom_point(data = DFindividual, aes( x = reorder(pw, desc(pw)), y = mean, color = type),
                                  position = position_nudge((as.integer(DFindividual$type)-1)*0.15), size = 1)+
    coord_flip()+theme_tufte()+expand_limits(x= c(-1, nrow(DFindividual)/4 +1))+
    scale_color_manual(values = c("associated" ="orangered4", 
                                  "different" = "chartreuse4",
                                  "human" = "skyblue",
                                  "none" = "grey")) +xlab("protected word")+
    ylab("cosine distance")+
    geom_segment(data = DFindividual,aes(x=reorder(pw, desc(pw)),xend=reorder(pw, desc(pw)), y  = `5.5%`, yend= `94.5%`,
                                         color = type),
                 position = position_nudge((as.integer(DFindividual$type)-1)*0.15), size = .4)+
    geom_hline(yintercept = 1, lty = 2, size = .2, alpha = .3) +labs(title = paste(list, ", ", embedding, ", cosine distances, by protected-word", sep = ""), subtitle = "Means with 89% highest posterior density intervals")+ 
    theme(legend.position='none', plot.title.position = "plot") + ylim(ylims)
  
  
  
  results$plotOverall  <- ggplot() + geom_point(data = DFoverall, aes( x = type, y = mean, color = type), size = 1) +
    coord_flip()+theme_tufte() +  
    geom_segment(data = DFoverall,aes(x=type,xend = type, y  = low, yend= high,
                                      color = type))+ylim(ylims)+
    scale_color_manual(values = c("associated" ="orangered4", 
                                  "different" = "chartreuse4",
                                  "human" = "skyblue",
                                  "none" = "grey"))+ theme(legend.position = c(0.9, 0.8), plot.title.position = "plot")+
    labs(title = paste(list, ", ", embedding, ",  cosine distances, by connection type", sep = ""), 
       subtitle = "Means with 89% highest posterior density intervals")+
    ylab("cosine distance")+
    geom_hline(yintercept = 1, lty = 2, size = .2, alpha = .3)
  
  
  
  grobIndividual <- ggplotGrob(results$plotIndividual+theme_tufte(base_size = 6) + theme(legend.position='none'))
  grobOverall <- ggplotGrob(results$plotOverall+theme_tufte(base_size = 6)+ theme(legend.position = c(0.9, 0.7)))
  
  
  results$plotJoint <- ggplot(data.frame(a=1)) + xlim(1, 20) + ylim(10, 60)+theme_void()+
    annotation_custom(grobIndividual, xmin = 1, xmax = 20, ymin = 10, ymax = 50)+
    annotation_custom(grobOverall, xmin = 1, xmax = 20, ymin = 50, ymax = 60)  
  
  return(results)
}


