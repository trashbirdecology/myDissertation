# Setup -------------------------------------------------------------------
## Clear mem
rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

# Load BBS data into mem -------------------------------------------------
## Source the script that returns the obkect `feathers.subset`, which si the BBS data + sampling gridded subsetted data
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discont_01_getBBSdata.R"))
# takes about thirty to sixty seconds...

## This will return a few objects:
### 1. bbsData - containes the subsetted data and munged species/aou and body masses. This df also includes presence absence data for 3-year aggregates
### 2. grassSpecies - some grassland obligate spp of interest
### 3. routes_gridList - the grid design assosciation with rowID and colID on the bbsData

### THIS SECTION NEEDS TO BE SOURCED BEFORE ANY OTHER DATA OR FUNCTIONS ARE ADDED TO MEMORY!


# Source helper functions -------------------------------------------------
(to.source <- list.files(here::here("/chapterFiles/discontinuityAnalysis/"), 
                         pattern="helper",  
                         full.names=TRUE))
for(i in 1:length(to.source)) source(to.source[i])

## Source helper funs for plotting spatial data located in another chapter!
source(here::here("/chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R"))


# Define, create directories -------------------------------------
## discontinuity results
reultsDir <- here::here("chapterFiles/discontinuityAnalysis/results/")
## figures
figDirTemp <- here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <- here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(figDirTemp))


# Define some things for plotting and subsetting data ---------------------
pts <- bbsData %>% 
  # filter(year %in% c(1970, 1985, 2000, 2015)) %>%
  distinct(countrynum, statenum, route, lat, long, colID, rowID, year) %>% 
  # keep only the routes that were sampled in all the years of interest
  group_by(countrynum, statenum, route) %>% 
  # filter(n()==4) %>% 
  ungroup() %>%  #safety first
  mutate(loc = as.factor(paste(countrynum, statenum, route, sep="_")))

# see # routes per year
pts %>% group_by(year) %>% summarise(z=n_distinct(loc))->t;ggplot(t, aes(x=year, y=z))+geom_line()

box <- pts %>% 
  summarise(y.min = min(lat, na.rm=TRUE),
            y.max = max(lat, na.rm=TRUE),
            x.min = min(long, na.rm=TRUE),
            x.max = max(long, na.rm=TRUE))

# Visualize the spatial sampling grid & BBS data locations ---------------------------------
# Get the us state map data from ggplot
us_states <- ggplot2::map_data("county")
# ca_states <- ggplot2::map_data("world", "Canada") 
ca_us_states <- ggplot2::map_data("world", c("usa", "Canada"))


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

## Fort Riley mb
riley <-
  data.frame(long = -96.788448,
             lat = 39.199983,
             base = "Fort Riley")
### Konza prairie
konza <-
  data.frame(long = -96.583961,
             lat = 39.091720,
             base = "Konza Prairie")



# Create county maps 
(p.studyarea <-
    ggplot() +
    geom_polygon(
      data = us_states %>% filter(region %in% tolower(states.of.interest)),
      aes(x = long, y = lat, group = group),
      colour = "black",
      fill = "grey90"
    ) +
    coord_fixed(1.3) +
    # ggthemes::theme_map()+
    geom_point(data=pts, aes(x=long, y=lat), size=1.3, show.legend=FALSE)+
    # geom_text(data=pts, aes(x=long, y=lat,  label=route), size=3.3,hjust=.5, vjust=-.75)+
    # coord_map(xlim = c(box$x.min-1, box$x.max+.5),
    #           ylim = c(box$y.min-1, box$y.max+.5))+
    # geom_point(data=riley, aes(x=long,y=lat), color="red",pch=21, fill=NA, size=25)+
    geom_text(data=riley, aes(x=long,y=lat), label="Ft. Riley", vjust=-.9, 
              hjust=.3,fontface="bold", size=2 , color="red") +
    geom_point(data=riley, aes(x=long,y=lat), color="red", size=2)
  # geom_point(data=konza, aes(x=long,y=lat), color="red", size=3)+
  # geom_text(data=konza, aes(x=long,y=lat), label="Konza", vjust=1.4, hjust=.3,fontface="bold", size=5)
)

# saveFig(p.studyarea,"bbsRoutes", dir=figDir)

cpr.regimes <- tibble(year = c(1970,
                               1985,
                               2000,
                               2015),
                      lat = c(-39, -39.5,-40, -40.5))

(p.studyarea2 = p.studyarea +
  geom_hline(aes(yintercept=39, color="1970"), size=1, alpha=.7, show.legend=TRUE)+
    geom_hline(aes(yintercept=39.5, color="1985"), size=1, alpha=.7, show.legend=TRUE)+
    geom_hline(aes(yintercept=40, color="2000"), size=1, alpha=.7, show.legend=TRUE)+
    geom_hline(aes(yintercept=40.5, color="2015"), size=1, alpha=.7, show.legend=TRUE)+
    ggthemes::scale_color_colorblind(name="assumed spatial \nregime boundaries")+
    theme(legend.position="left")
)

saveFig(p.studyarea2,"routes_spatRegimeLines2", dir=figDir)


########################## BEGIN ANALYSIS #################################
# Discontinuity Analysis using Barichievy Methods...-------------------------------------------------

## Filter the bbsData by retaining relevant routes and years
bbsData <- bbsData %>% 
  mutate(loc = as.factor(paste(countrynum, statenum, route, sep="_"))) %>%
  filter(
    year %in% unique(pts$year), 
    loc %in% unique(pts$loc))  

year.vec <- unique(bbsData$year)

## Run over all year vecs and loc vecs
for(i in seq_along(year.vec)){
  
  # Subset data to a single year
  bbsData.subset <- bbsData %>% filter(year == year.vec[i])
  loc.vec  <- unique(bbsData.subset$loc) %>% droplevels()
  
  for(j in seq_along(loc.vec)){
    # create new data frame for saving annual results (will add all locations onto the df)
    if(j==1)results <- tibble()
    # create folder to store results if (a) i and j ==1 AND (b) results dir DNE
    if(j == 1 & i == 1) suppressWarnings(dir.create(here::here("chapterFiles/discontinuityAnalysis/results/")))
    
    # Subset the data by location 
    analyData <- bbsData.subset %>% 
      filter(loc %in% loc.vec[j])  %>%
      # remove species if they were not present, we don't want to include them in the census!
      filter(pa.3year!=0) %>% 
      distinct(loc, year, aou, .keep_all=TRUE) %>% 
      # sort by body mass  
      arrange(log10.mass)
    
    
    # skip
    if(nrow(analyData)<5)next()
    # throw error
    if(nrow(analyData %>% 
            filter(is.na(log10.mass)))>0)stop(message("body masses include NA. please check bbsData for missing log10.mass values..."))
    
    
    if(length(analyData$log10.mass)==0)next()
    
    hnull <- Neutral.Null(analyData$log10.mass,resolution=1000)
    gaps  <- DD(analyData$log10.mass,hnull=hnull,Sample.N=1000) %>% 
      as.data.frame() %>% 
      rename(log10.mass = log10.data, 
             gap.percentile = V2)
    
    results.new <- full_join(analyData, gaps)
    results <- results %>% bind_rows(results.new)
    
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
  
  if( i == max(seq_along(year.vec)) & i == max(seq_along(loc.vec))) message("um hello i am done running this analysis for you...")
} # end i-loop

# END RUN -----------------------------------------------------------------
