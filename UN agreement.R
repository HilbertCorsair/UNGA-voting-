library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(animation)
library(broom)
Idealpoints <- read.delim("IdealpointsPublished.tab")
a1 <- Idealpoints %>%  filter(year != 1964) %>% 
  gather(country, perc_agree, 7:12)
a1sub <- a1[,c(1,14,15)]
years_of_interest <- c(1946, seq(from = 1955, to = 2015, by = 10)  )
  for(i in yerar_of_interest) {
    data <- filter(a1sub, year == i) %>%
      ggplot(aes(perc_agree, fill = country, col = country))+
      geom_density(alpha = .5)+
      theme_classic()+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.key.size = unit(2.5, 'lines'),
            text = element_text(size = 30, face = "bold", colour = "black"))+
      scale_y_continuous(breaks=NULL)+
      scale_x_continuous(name = "Ratio of agerrment", limits = c(0.00, 1.00),
                         breaks = seq(from = 0.00, to = 1.00, by = 0.10))+
      ggtitle(i)
    ggsave(paste0("00", i,".png"), plot = last_plot(),
           width=30, height=10, dpi=300)
    print(data)
  }


#png(name_test, units="cm", width=30, height=10, res=300)
#p1_yes_votes
#dev.off()

of_interest <- c("United States of America", "China", "Romania", "France", "Russia", "Romania")
idp <- arrange(Idealpoints, desc(Idealpoint )) %>% 
  select(year, CountryName,Idealpoint) %>% 
  group_by(CountryName, year)

main <- Idealpoints %>% filter(CountryName %in% of_interest)%>%
  ggplot(aes(x=year, y = Idealpoint, col = CountryName))+
  theme_classic()+
  geom_point(alpha= .6)+
  geom_line()
main

# creating a list of linear models for the idealpoint of each country and filtering for relevant p values
lm_list <- Idealpoints%>% 
  filter(year != 1964) %>%                                            #no voting in 1964
  select(year, Idealpoint, CountryName) %>%                           #selecting relevant columns 
  nest(-CountryName) %>%                                              #nesting data for each country
  mutate(model_raw = map(data, ~lm(Idealpoint ~ year, data = .))) %>% #creating linear model table of corficients for each country
  mutate(model_tbl = map(model_raw , tidy)) %>%                    #transforming the linear model table into a tible that can be used in dplyr manipulations
  unnest(model_tbl)%>%                                            #unnesting to make data available 
  filter(term == "year")%>%                                          # selecting the slope 
  mutate(p_adjusted = p.adjust(p.value))%>%                          #adjustint the p value and filtering for statisclal signifficance (p>.05)
  filter(p_adjusted > .05)

#plotting all countries all years 
ggplot(Idealpoints, aes(x=year, y = Idealpoint, col = CountryName))+
  theme_classic()+
  theme(legend.position="none")+
  geom_line()

ggplot(Idealpoints, aes( x = Idealpoint))+
  geom_density(width = 1)

idps_post90 <- Idealpoints %>% 
  filter(year>=1990) %>%
  select(year, CountryName, Idealpoint) %>% 
  group_by(CountryName)%>%
  summarise(mean_idp = mean(Idealpoint))
idps_post90 %>% arrange(desc(mean_idp)) -> ordered
head(ordered, n = 20)
tail(ordered, n = 20)



hclust_model <- dist(idps_post90) %>% 
  hclust(method = "complete")
cut <- cutree(hclust_model, k = 3 )

ggplot(ordered, aes(x = mean_idp, y = CountryName))+
  geom_point()+
  scale_x_continuous(limits = c(-3, 3), minor_breaks = seq(from = -3, to = 3, by = 0.5))

median_90 <- Idealpoints %>% 
  filter(year>=1990) %>%
  select(year, CountryName, Idealpoint) %>% 
  group_by(CountryName)%>%
  summarise(medianIP = median(Idealpoint))
median_ordered <- arrange(median_90, desc(medianIP))
comparison <- data.frame(ordered, median_ordered)

# plotting all countries since 90s 
IP_since90s <- Idealpoints %>% 
  filter(year>=1990) %>%
  select(year, CountryName, Idealpoint) %>% 
  group_by(CountryName)

ggplot(median_90, aes(x=medianIP))+
  geom_histogram(binwidth=0.3, fill = "skyblue")+
  theme_classic()+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank())+
  scale_x_continuous(name = "Ideal point", limits = c(-3,3))+
  scale_y_continuous(expand=c(0,0))

                    


