words <- c("logic", "Nietzsche", "analytic", "Hegel", "Russell", "continental", "burger", "sandwich" 
           )

distances <- c(.47, .32, .13, .29, .08, .08, .04, .04)

philosophy <- data.frame(words, distances)


library(ggplot2)
library(ggthemes)
library(forcats)


fct_reorder(words, distances)
ggplot(philosophy, aes(x = fct_reorder(words, distances), y = distances))+
  geom_point(size = 3)+coord_flip()+theme_tufte(base_size = 7)

