# Setup -------------------------------------------------------------------
## Clear mem
# rm(list=ls())

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


# Visualize the spatial sampling grid & BBS data locations ---------------------------------
# Get the us state map data from ggplot
us_states <- ggplot2::map_data("county")
# ca_states <- ggplot2::map_data("world", "Canada") 
ca_us_states <- ggplot2::map_data("world", c("usa", "Canada")) %>% 
  filter(long < -55, lat > 22.5)


## basemap of the use and all route location in US/CA
p.usBaseMap <-  ggplot() +
  geom_polygon(
    data = ca_us_states,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey80"
  ) +
  coord_fixed(1.3) +
  ggthemes::theme_map()+
  coord_map(xlim = c(-135, -60),ylim = c(25, 60))

p.allRoutes <- p.usBaseMap +
  geom_point(
    data = routes_gridList$routes_grid,
    aes(x = long, y = lat),
    color = "black",
    size = .75)

## Fort Riley map in entire us/ca
riley <-
  data.frame(long = -96.788448,
             lat = 39.199983,
             base = "Fort Riley")


konza <-
  data.frame(long = -96.583961,
             lat = 39.091720,
             base = "Konza Prairie")



# Kansas maps by County 
pts <- bbsData.forAnalysis %>% 
    filter(year %in% c(1975, 2015)) %>%
    distinct(countrynum, statenum, route, lat, long, colID, rowID, year) %>% 
  # keep only the routes that were sampled in both years
  group_by(countrynum, statenum, route) %>% 
  filter(n()>1) %>% 
  ungroup()#safety first
  
box <- pts%>% 
  summarise(y.min = min(lat, na.rm=TRUE),
            y.max = max(lat, na.rm=TRUE),
            x.min = min(long, na.rm=TRUE),
            x.max = max(long, na.rm=TRUE))


p.kansas <-
  ggplot() +
  geom_polygon(
    data = us_states %>% filter(region=="kansas") ,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey90"
  ) +
  coord_fixed(1.3) +
  ggthemes::theme_map()+
  geom_point(data=pts, aes(x=long, y=lat), size=2, show.legend=FALSE)+
  geom_text(data=pts, aes(x=long, y=lat,  label=route), size=3.3,hjust=.5, vjust=-.75)+
  coord_map(xlim = c(box$x.min-.5, box$x.max+.5),
            ylim = c(box$y.min-.5, box$y.max+.5))+
  # geom_point(data=riley, aes(x=long,y=lat), color="red",pch=21, fill=NA, size=25)+
  geom_text(data=riley, aes(x=long,y=lat), label="Ft. Riley", vjust=-.7, hjust=.3,fontface="bold", size=5 ) +
  geom_point(data=riley, aes(x=long,y=lat), color="red", size=3)+
  geom_point(data=konza, aes(x=long,y=lat), color="red", size=3)+
  geom_text(data=konza, aes(x=long,y=lat), label="Konza", vjust=1.4, hjust=.3,fontface="bold", size=5)

p.kansas

saveFig(p.kansas,"kansasBBSpts_1975and2010", dir=figDir)


########################## BEGIN ANALYSIS #################################

# Discontinuity Analysis using Barichievy Methods...-------------------------------------------------
data <- bbsData.forAnalysis %>% 
  filter(
    year %in% unique(pts$year),
    route %in% unique(pts$route)) %>% 
  mutate(loc = as.factor(paste(countrynum, statenum, route, sep="_")))


loc.vec  <- unique(data$loc) 
year.vec <- unique(data$year)


## Run over all year vecs and loc vecs
for(i in seq_along(year.vec)){
  results <- tibble()
  for(j in seq_along(loc.vec)){
  if(j == 1 & i == 1) dir.create(here::here("chapterFiles/discontinuityAnalysis/results/"))

analyData <- data %>% 
  filter(year == year.vec[i],
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


