# # Setup -------------------------------------------------------------------
# ## Clear mem
rm(list=ls())
# 
# ## Load pkgs
# library(cowplot)
# library(tidyverse)
# 
# ## Source the script that returns the obkect `feathers.subset`, whcih si the BBS data + sampling gridded subsetted data
# source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discont_01_getBBSdata.R"))
## This will return a few objects:
  ### 1. bbsData.forAnalysis - containes the subsetted data and munged species/aou and body masses. This df also includes presence absence data for 3-year aggregates
  ### 2. grassSpecies - some grassland obligate spp of interest
  ### 3. routes_gridList - the grid design assosciation with rowID and colID on the bbsData.forAnalysis
# 
# ## Source the helper functions
# (to.source <- list.files(here::here("/chapterFiles/discontinuityAnalysis/"), 
#             pattern="helper",  
#            full.names=TRUE))
# for(i in 1:length(to.source)) source(to.source[i])
# 
# ## Source helper funs for plotting spatial data located in another chapter!
# source(here::here("/chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R"))

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



# Kansas maps by County 
pts <- bbsData.forAnalysis %>% 
    filter(year %in% c(1970, 1985, 2000, 2015)) %>%
    distinct(countrynum, statenum, route, lat, long, colID, rowID, year) %>% 
  # keep only the routes that were sampled in all the years of interest
  group_by(countrynum, statenum, route) %>% 
  filter(n()==4) %>% 
  ungroup()#safety first
  
box <- pts %>% 
  summarise(y.min = min(lat, na.rm=TRUE),
            y.max = max(lat, na.rm=TRUE),
            x.min = min(long, na.rm=TRUE),
            x.max = max(long, na.rm=TRUE))


p.kansas <-
  ggplot() +
  geom_polygon(
    data = us_states %>% filter(region %in% c("nebraska","kansas")),
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey90"
  ) +
  coord_fixed(1.3) +
  ggthemes::theme_map()+
  geom_point(data=pts, aes(x=long, y=lat), size=2, show.legend=FALSE)+
  geom_text(data=pts, aes(x=long, y=lat,  label=route), size=3.3,hjust=.5, vjust=-.75)+
  coord_map(xlim = c(box$x.min-1, box$x.max+.5),
            ylim = c(box$y.min-1, box$y.max+.5))+
  # geom_point(data=riley, aes(x=long,y=lat), color="red",pch=21, fill=NA, size=25)+
  geom_text(data=riley, aes(x=long,y=lat), label="Ft. Riley", vjust=-.7, hjust=.3,fontface="bold", size=5 ) +
  geom_point(data=riley, aes(x=long,y=lat), color="red", size=3)+
  geom_point(data=konza, aes(x=long,y=lat), color="red", size=3)+
  geom_text(data=konza, aes(x=long,y=lat), label="Konza", vjust=1.4, hjust=.3,fontface="bold", size=5)

p.kansas
# saveFig(p.kansas,"routes3years", dir=figDir)

cpr.regimes <- tibble(year = c(1970,
                               1985,
                               2000,
                               2015),
                      lat = c(-39, -39.5,-40, -40.5))

p.kansas2 = p.kansas +
  geom_hline(aes(yintercept=39, color="1970"), size=3, alpha=.3, show.legend=TRUE)+
  geom_hline(aes(yintercept=39.5, color="1985"), size=3, alpha=.3, show.legend=TRUE)+
  geom_hline(aes(yintercept=40, color="2000"), size=3, alpha=.3, show.legend=TRUE)+
  geom_hline(aes(yintercept=40.5, color="2015"), size=3, alpha=.3, show.legend=TRUE)+
  ggthemes::scale_color_colorblind(name="spatial regime location per Roberts et. al")+
  theme(legend.position="bottom")
p.kansas2
saveFig(p.kansas2,"routes3years_spatRegimeLines", dir=figDir)



########################## BEGIN ANALYSIS #################################
# Discontinuity Analysis using Barichievy Methods...-------------------------------------------------

## Filter the bbsData by retaining relevant routes and years
bbsData.forAnalysis <- bbsData.forAnalysis %>% 
  filter(
    year %in% unique(pts$year),
    route %in% unique(pts$route)) %>% 
  mutate(loc = as.factor(paste(countrynum, statenum, route, sep="_")))

loc.vec  <- unique(bbsData.forAnalysis$loc) 
year.vec <- unique(bbsData.forAnalysis$year)

## Run over all year vecs and loc vecs
for(i in seq_along(year.vec)){
  results <- tibble()
  for(j in seq_along(loc.vec)){
  if(j == 1 & i == 1) dir.create(here::here("chapterFiles/discontinuityAnalysis/results/"))

analyData <- bbsData.forAnalysis %>% 
  filter(year == year.vec[i],
         loc == loc.vec[j])  %>% 
  arrange(log10.mass)

if(nrow(analyData %>% 
          filter(is.na(log10.mass)))>0)stop(message("body masses include NA. please check bbsData.forAnalysis for missing log10.mass values..."))


if(length(analyData$log10.mass)==0)next()

hnull <- Neutral.Null(analyData$log10.mass,resolution=1000)
gaps  <- DD(analyData$log10.mass,hnull=hnull,Sample.N=1000) %>% 
  as.data.frame() %>% 
  rename(log10.mass = log10.data, 
         gap.percentile = V2)


results <- full_join(analyData, gaps)
if(j!=1)  results <- results %>% bind_rows(results)

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