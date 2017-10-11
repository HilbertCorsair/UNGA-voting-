library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(data.table)
library(countrycode)
library(broom)
library(gridExtra)
# reading voting data 
UNvotes <- read_tsv("UNVotesPublished.tab")

# cleanning the data 
relevant_votes <- UNvotes %>% 
filter(vote <= 3) %>% # yes, no & abstein votes only
mutate(country = countrycode(ccode, "cown", "country.name")) #adds a country column with names according to codes in the "ccode" column

# analising yes vote ratio  
YES_votes <- relevant_votes %>% filter(year != 1964) %>%
  group_by(year)  %>% 
  summarize(total = n(),
            ratio_yes = mean(vote == 1, na.rm = T))
p1_yes_votes <- YES_votes %>%
                ggplot(aes(x = year, y = ratio_yes))+
                  geom_point()         #plotting total ratio 

by_year_country <- relevant_votes %>%  #by year and country yes ratio
  group_by(year, country)
countries <- by_year_country %>% filter(country %in% c("United States of America", "China", "Romania", "Germany", "France", "Korea, Democratic People's Republic of", "Russian Federation", "United Arab Emirates", "Romania") & year !=1964)%>%
  summarize(total = n(),
            ratio_yes = mean(vote == 1, na.rm = T))
ggplot(countries, aes(x = year, y= ratio_yes, col = country))+
  geom_point(alpha = 0.6)+
  theme_classic()+
  ylab(label = "Yes vote ratio")+
  facet_wrap(~topic)

# Linear fitt of the voting trend
sub_data <- relevant_votes[c("vote", "country", "year")]
yes_ratio_yc <- group_by(sub_data, year, country) %>%
  summarize(total = n(),
            ratio_yes = mean(vote == 1))

# creating a list of linear models for each country 
lm_list <- yes_ratio_yc%>% ungroup() %>% 
  nest(-country) %>%                                                 #nesting data for each country
  mutate(model_raw = map(data, ~lm(ratio_yes ~ year, data = .))) %>% #creating linear model table of corficients for each country
  mutate(model_tidied = map(model_raw , tidy))%>%                    #transforming the linear model table into a tible that can be used in dplyr manipulations
  unnest(model_tidied)%>%                                            #unnesting to make data available 
  filter(term == "year")%>%                                          # selecting the slope 
  mutate(p_adjusted = p.adjust(p.value))%>%                          #adjustint the p value and filtering for statisclal signifficance (p>.05)
  filter(p_adjusted > .05)

ascending <- lm_list %>% arrange(desc(estimate))
descending <-lm_list %>% arrange(estimate)

of_interest <- c("United States of America", "China",  "France", "Russia", "Saudi Arabia")
trends <- yes_ratio_yc %>%
  filter(country %in% of_interest )
ggplot(trends, aes(x = year, y = ratio_yes, col = country))+
  #geom_point(alpha = .4)+
  geom_smooth(method='lm')+
  theme_classic()

#introducing the vote description dataset 
descriptions <- read_excel("descriptions1-70latestversion.xls")
votes_topic<- relevant_votes %>% gather(topic, has_topic, me:ec) %>% # makeing the topic variable
  filter(has_topic ==1)
votes_topic$topic <- recode(votes_topic$topic,
         "me" = "Israel-Palestine conflict", 
         "nu" = "Nuclear: wepons & materials", 
         "di" = "Disarmament",
         "co" = "Colonialism", 
         "hr" = "Human rights", 
         "ec" = "Economic developement")
votes_topic_sumary <- votes_topic %>%
  group_by(year, country, topic) %>%
  summarize(total = n(),
            ratio_yes = mean(vote == 1, na.rm = T))                               # summarise on topic 
trnds_on_topic <- filter(votes_topic_sumary, country %in% c("United States of America", "China",  "France", "Russian Federation", "Saudi Arabia"))# filtering for countries of interest
trnds_on_topic$topic <- factor(trnds_on_topic$topic)
trnds_on_topic$topic <- recode(trnds_on_topic$topic,
       "me" = "Israel-Palestine conflict", 
       "nu" = "Nucl.: wepons & materials", 
       "di" = "Disarmament",
       "co" = "Colonialism", 
       "hr" = "Human rights", 
       "ec" = "Economic developement")

g1 <- ggplot(trnds_on_topic, aes(x = year, y = ratio_yes, col = country))+
 geom_point(alpha = 0.6)+
  geom_line()+
  facet_wrap(~topic)+
  theme_classic()+
  scale_y_continuous(name = "Ratio of yes votes")
g1

lm_list_topic <- votes_topic_sumary%>% ungroup() %>% 
  nest(-country, -topic) %>%                                                 #nesting data for each country and topic
  mutate(model_raw = map(data, ~lm(ratio_yes ~ year, data = .))) %>% #creating linear model table of corficients for each country
  mutate(model_tidied = map(model_raw , tidy))%>%                    #transforming the linear model table into a tible that can be used in dplyr manipulations
  unnest(model_tidied)%>%                                            #unnesting to make data available 
  filter(term == "year")%>%                                          # selecting the slope 
  mutate(p_adjusted = p.adjust(p.value))%>%                          #adjustint the p value and filtering for statisclal signifficance (p>.05)
  filter(p_adjusted > .05)


# RO on human rights 
filter(lm_list_topic, country == "Sirya")
RO_HR <- filter(votes_topic_sumary, country == "Sirya" & topic == "Human rights")
LM_HR_RO <- lm(ratio_yes ~ year, data = RO_HR)
LM_HR_RO
ggplot(RO_HR, aes(x = year, y= ratio_yes))+
  geom_point()


# country profile 
profile <- votes_topic_sumary %>% ungroup() %>% group_by(country, topic) #  %>% summarise(yes_vote = mean(ratio_yes))
pof <- filter(profile, country %in% c("United States of America", "China",  "Russia", "United Arab Emirates") )
ggplot(pof, aes(x= topic, y= ratio_yes, col = country))+
  geom_boxplot()


