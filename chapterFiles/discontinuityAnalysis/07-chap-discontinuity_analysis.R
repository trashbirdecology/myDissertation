# Here is the data you requested. I've also attached the "constant power table" for you to reference. Column descriptions:
# species = common name of bird species detected at a BBS route
# log.mass = log-transformed mean body mass of species
# d.value = value computed by "discontinuity detector" (i.e., Gap Rarity Index modified per Barichievy et al., 2018) for delineating discontinuities
# aggs = body mass aggregation identified via "discontinuity detector." Low numbers indicate aggregations with small species (e.g., hummingbirds, etc).
# Lati = latitude
# Longi = longitude
# Year = year of BBS survey

# Setup -------------------------------------------------------------------
# rm(list=ls())
library(tidyverse)

# Load and munge data -----------------------------------------------------
data <- read.csv(here::here("chapterFiles/discontinuityAnalysis/cpr_DA_transectData.csv")) %>% 
  as_tibble()

glimpse(data)

unique(data$Longi) %>% length()
unique(data$Lati) %>% length()


pts <- data %>% distinct(Longi,Lati,Year) %>% 
  group_by(Lati,Longi) %>% 
  summarise(nYear= n_distinct(Year))

head(pts)

# Map ---------------------------------------------------------------------
ggplot(pts, aes(Longi, Lati))+
  geom_point(aes(size=nYear, color=as.factor(Lati)), show.legend=FALSE)+
  geom_hline(aes(yintercept=(Lati)))


# DD ----------------------------------------------------------------------
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_discontinuityDetectorBarichievy2018Functions.R"))

test2010 <- data %>% filter(Year==2010, 
                        Lati == 48.28636,
                        Longi == -93.55119)
test2011 <- data %>% filter(Year==2011, 
                            Lati == 48.28636,
                            Longi == -93.55119)
setdiff(test2011$species, test2010$species)
setdiff(test2010$species, test2011$species)



hnull<-Neutral.Null(test$log.mass,resolution)
# Bootstrap.gaps<-DD(log10.data,hnull,Sample.N)

