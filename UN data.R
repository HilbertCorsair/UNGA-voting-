library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(animation)

Idealpoints <- read.delim("IdealpointsPublished.tab")
a1 <- Idealpoints %>% gather(country, perc_agree, 7:12)
ggplot(a1, aes(x=perc_agree, fill = country))+
  geom_density(alpha = .5)+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  scale_y_continuous(breaks=NULL)+
  xlab("Ratio of agerrment")