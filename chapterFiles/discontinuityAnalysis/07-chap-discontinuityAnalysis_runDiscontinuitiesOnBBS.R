# Setup -------------------------------------------------------------------
## Clear mem
rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

## Source the script that returns the obkect `feathers.subset`, whcih si the BBS data + sampling gridded subsetted data...
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_getMungeBBSdata.R"))

## Source the helper functions
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_helperFunctions.R"))


## Source the script with Barichievy functions...
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_discontinuityDetectorBarichievy2018Functions.R"))
        ## Thsi data was obtained from CP Roberts in 2019.              
              # species = common name of bird species detected at a BBS route
              # log.mass = log-transformed mean body mass of species
              # d.value = value computed by "discontinuity detector" (i.e., Gap Rarity Index modified per Barichievy et al., 2018) for delineating discontinuities
              # aggs = body mass aggregation identified via "discontinuity detector." Low numbers indicate aggregations with small species (e.g., hummingbirds, etc).
              # Lati = latitude
              # Longi = longitude
              # Year = year of BBS survey

## Source helper funs for plotting spatial data
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R")

# Define, create directories -------------------------------------
## discontinuity results
reultsDir <- here::here("chapterFiles/discontinuityAnalysis/results/")

## figures
figDirTemp <- here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <- here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(figDirTemp))


# Visualize the spatial sampling grid -------------------------------------

# Get the us state map data from ggplot
us_states <- ggplot2::map_data("state")
ca_states <- ggplot2::map_data("world", "Canada") 
ca_us_states <- map_data("world", c("usa", "Canada")) %>% 
  filter(long < -55, lat > 22.5)
rm(us_states, ca_states)

usBaseMap <-  ggplot() +
  geom_polygon(
    data = ca_us_states,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey80"
  ) +
  coord_fixed(1.3) +
  ggthemes::theme_map()+
  coord_map(xlim = c(-135, -60),ylim = c(25, 60))

routesMap <- usBaseMap +
  geom_point(
    data = routes_gridList$routes_grid,
    aes(x = long, y = lat),
    color = "black",
    size = .75) 

## Get the military bases.
milBases <- getMilBases() %>% 
   as.data.frame() %>%
  rename(lat = coords.x2, long = coords.x1)

# milBasesMap <- usBaseMap +
#   geom_point(data = milBases,
#              aes(x = long, y = lat),
#              color = "red",
#              size = 0.75)

riley <-
  data.frame(long = -96.788448,
             lat = 39.199983,
             base = "riley")


routesMap +
  geom_point(
    data = riley,
    aes(x = long, y = lat),
    color = "darkred",
    shape = 16,
    size = 3.5
  ) +
  geom_text(
    data = riley,
    aes(x = long, y = lat, label = 'Ft. Riley'),
    nudge_x = 0,
    nudge_y = 3,
    color = "darkred",
    size = 6
  ) +
  theme.margin
saveFig(last_plot(), fn = "rileyMap", figDir)


# Further munge/subset/merge BBS data by spatial locations ----------------

glimpse(bbsData)





# Discontinuity Analysis using Barichievy Methods...-------------------------------------------------
loc.vec  <- unique(data$loc) 
year.vec <- unique(data$Year) 



###############STOPPED HERE##############
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


# END RUN -----------------------------------------------------------------


