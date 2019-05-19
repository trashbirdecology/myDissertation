# Here is the data you requested. I've also attached the "constant power table" for you to reference. Column descriptions:
# species = common name of bird species detected at a BBS route
# log.mass = log-transformed mean body mass of species
# d.value = value computed by "discontinuity detector" (i.e., Gap Rarity Index modified per Barichievy et al., 2018) for delineating discontinuities
# aggs = body mass aggregation identified via "discontinuity detector." Low numbers indicate aggregations with small species (e.g., hummingbirds, etc).
# Lati = latitude
# Longi = longitude
# Year = year of BBS survey

# Setup -------------------------------------------------------------------
## Clear mem
# rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

## Source the script with Barichievy functions...
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_discontinuityDetectorBarichievy2018Functions.R"))


# Load and munge data -----------------------------------------------------
origData <- read.csv(here::here("chapterFiles/discontinuityAnalysis/cpr_DA_transectData.csv")) %>% 
  as_tibble() %>% 
  rename(Lat = Lati, Long = Longi)

# Munge original data -----------------------------------------------------
data <- origData   %>% 
  mutate(loc= as.factor(group_indices(origData, Lat, Long))) %>% # creates a number for each lat-long pair..
  # add a variable to filter by amount of annual data avialable
  group_by(loc) %>%
  mutate(nYear= n_distinct(Year)) %>% 
  ungroup()

# Filter the data by nYear
hist((data %>% distinct(loc, nYear))$nYear)

data <- data %>% 
  filter(nYear >=10)
hist((data %>% distinct(loc, nYear))$nYear)

# Get spatial locations of sampling points --------------------------------
pts <- data %>% distinct(loc,Year, .keep_all=TRUE) 
  
# Map of sampling locations (unique lat lon pts..) ---------------------------------------------------------------------
# ggplot(pts, aes(Long, Lat))+
#   geom_point(aes(size=nYear, color=as.factor(loc)), show.legend=FALSE)

# Get species turnover ----------------------------------------------------------------------
## Get lag-1 year turnover
turnover <- data %>% 
  group_by(loc, Year) %>% 
  summarise(nSpp=n_distinct(species)) %>% 
  group_by(loc) %>% 
  mutate(nSppDiff = nSpp-lag(nSpp)) %>%   
  ungroup()

hist(turnover$nSpp)
hist(turnover$nSppDiff)

## Just plot the mean and SD since there are way too many locs to plot idnividual turnover rates
temp <- turnover %>%
  group_by(Year) %>% 
  summarise(mean = mean(nSppDiff, na.rm=TRUE), 
         sd = sd(nSppDiff, na.rm=TRUE)) %>% 
  mutate(
         upper    = 1.96*sd + mean,
         lower    = mean - 1.96*sd)

ggplot(data=temp)+
  geom_line(aes(x=Year, y=mean))+
  ylab("mean annual turnover")+
  geom_ribbon(aes(x = Year, ymax = lower, ymin = upper), alpha=0.30)


# Discontinuity Analysis using Barichievy Methods...-------------------------------------------------
loc.vec  <- unique(data$loc) 
year.vec <- unique(data$Year) 

for(i in seq_along(year.vec)){
  results <- tibble()
  for(j in seq_along(loc.vec)){
  if(j == 1 & i == 1) dir.create(here::here("chapterFiles/discontinuityAnalysis/results/"))

analyData <- data %>% 
  filter(Year == year.vec[i],
         loc == loc.vec[j])

if(nrow(analyData)==0)next()

hnull <- Neutral.Null(analyData$log.mass,resolution=4000)
gaps  <- DD(analyData$log.mass,hnull,Sample.N=1000, thresh=0.95)

results <- gaps %>% 
  mutate(Year = as.integer(year.vec[i]),
         loc = as.integer(loc.vec[j])) %>% 
  bind_rows(results)

print(paste("end i=loop ", i , "/", length(year.vec), " & j-loop ", j, "/", length(loc.vec)))
  } # end j-loop

  fn <-
    paste0(
      here::here("chapterFiles/discontinuityAnalysis/results/discontResults_"),
      'year',
      year.vec[i],
      ".RDS"
    )
  
  saveRDS(results, file=fn)
  if(exists("results"))rm(results)

} # end i-loop




# Visualize results -------------------------------------------------------

p3 <- ggplot(data=gaps, aes(x=rank, y = log10.data))+
  geom_point(aes(color=isGap, shape=isGap))+
  scale_color_manual(values=c("yes"="red","no"="black"))+
  scale_shape_manual(values=c("yes"=17,"no"=16))+
  theme_bw()+
  ylab("log(body mass)g)")+
  xlab("rank order")+
  labs(caption = "")

p1
p2
p3
library(cowplot)
plot_grid(p1,p2,p3, ncol=1)
