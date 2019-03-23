# This script creates some basemaps, and defines directories for plotting and a couple of helper functions.
# source helper functions
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R")

######################## DEFINE & CREATE DIRECTORIES ########################
resultsDir <- "./chapterFiles/fisherSpatial/myResults"
dir.create(resultsDir)

figDir <- "./chapterFiles/fisherSpatial/figures"
dir.create(figDir)

figDissDir <- "./chapterFiles/fisherSpatial/figures/figsCalledInDiss"
dir.create(figDissDir)

rObjs <-
  "./chapterFiles/fisherSpatial/rObjs" # where i will store random objects

animDir <- "./chapterFiles/fisherSpatial/figures/animations"
dir.create(animDir)

rowEx.ind <- 12
######################## IMPORT SAMPLING GRID #######################
sampGrid <- readRDS(paste0(rObjs, "/samplingGrid.RDS"))

sampGridCoords <-
  cbind(sampGrid$sp_grd@data,
        coordinates(sampGrid$sp_grd)) %>%
  rename(lat = s2,
         long = s1,
         cellID  = id)

################## CREATE BASE MAPS ##################
# This section will create numerous base maps off of which we will build results visualizations.

canada <- c(
  "Yukon",  
  "British Columbia", "BC",  
  "Northwest Territories", "NW",  
  "Alberta", "AB",  
  "Nunavut", "NU",  
  "Saskatchewan", "SK",  
  "Manitoba", "MB", 
  "Ontario", "ON",  
  "Quebec", "QC",  
  "Prince Edward Island", "PE",  
  "New Brunswick", "NB", 
  "Newfoundland and Labrador", "NL", 
  "Nova Scotia", "NS" 
)

# Get the us state map data frim ggplot
us_states <- ggplot2::map_data("state")
ca_states <- ggplot2::map_data("world", "Canada") 
ca_us_states <- map_data("world", c("usa", "Canada")) %>% 
  filter(long < -55, lat > 22.5)



# BASEMAP: US STATES  -------------------------------- 
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


# BASEMAP: US STATES  + BBS ROUTES --------------------------------------------------------
routesMap <- usBaseMap +
  geom_point(
    data = sampGrid$routes_grid,
    aes(x = long, y = lat),
    color = "black",
    size = .75) 

# BASEMAP: GRID SAMPLING DESIGN -------------------------------------------

# I would really like to get a fig of sampling grid with outlines of e.g., one row. But I cannot find a way to integrate with ggplot, so I will resort to plotting single dirIDs on the route map.
# spplot(sampGrid$sp_grd)
routesMapRowEx <- usBaseMap +
  geom_point(
    data = sampGrid$routes_grid %>% filter(rowID == rowEx.ind),
    aes(x = long, y = lat),
    color = "red",
    size = .8
  )

routesMapRowEx2 <- routesMapRowEx +
geom_point(
  data = sampGrid$routes_grid %>% filter(rowID == rowEx.ind+1),
aes(x = long, y = lat),
color = "black",
size = .8) 

allRoutesUsed <- usBaseMap +
  geom_point(data = sampGrid$routes_grid %>% filter(rowID %in% 11:15),
    aes(x = long, y = lat, color = as.factor(rowID)), size = .8)+
  scale_color_viridis_d()+
  theme(legend.position = "none")+
  theme.margin


# BASEMAP: MILITARY BASES -------------------------------------------------
milBases <- getMilBases()
milBases.df <- milBases %>% as.data.frame() %>%
  rename(lat = coords.x2, long = coords.x1)

milBasesMap <- usBaseMap +
  geom_point(data = milBases.df,
             aes(x = long, y = lat),
             color = "red",
             size = 0.75)

milBasesRoutesMap <- routesMap +
  geom_point(data = milBases.df,
             aes(x = long, y = lat),
             color = "red",
             size = 1) 

# define approx. loc. of bases of interest.
rileyApprox <-
  data.frame(long = -96.788448,
             lat = 39.199983,
             base = "riley")
eglinApprox <-
  data.frame(long = -86.554,
             lat = 30.458,
             base = "riley")

# hopefully correct locations for bases.
basesOfInterest <- rbind(
  closestSite(milBases.df, rileyApprox, ndeg = 3, by = 0.1) %>% mutate(name = "Fort Riley")
  # ,
  # closestSite(milBases.df, eglinApprox, ndeg = 3, by = 0.1) %>%  mutate(name = "Eglin AFB")
)

rm(rileyApprox)
rm(eglinApprox)

basesOfIntMap <- usBaseMap +
  geom_point(
    data = basesOfInterest,
    aes(x = long, y = lat),
    color = "darkred",
    shape = 18 ,
    size = 4
  ) +
  geom_text(
    data = basesOfInterest,
    aes(x = long, y = lat, label = name),
    nudge_x = 0,
    nudge_y = 2,
    color = "darkred",
    size = 5
  ) +
  theme.margin


# Ecoregions --------------------------------------------------------------

eco_poly <- getEcoregions()

# get ecoregion for sp_grd
temp = over(sp_grd, eco_poly) # get which fall into which 
t<- merge(temp, sp_grd)




# Save basemaps -----------------------------------------------------------



# Save the basemaps to file
ggsave(filename = paste0(figDissDir, "/milBases.png"), plot = milBasesMap)
ggsave(filename = paste0(figDissDir, "/milBasesAndRoutes.png"), plot = milBasesRoutesMap)
ggsave(filename = paste0(figDissDir, "/basesOfInterestMap.png"), plot = basesOfIntMap)
ggsave(filename = paste0(figDissDir, "/bbsRoutesUsed.png"), plot = routesMap)
ggsave(filename = paste0(figDissDir, "/transectSamplingEx_1row", ".png"), plot = routesMapRowEx)
ggsave(filename = paste0(figDissDir, "/transectSamplingEx_2rows"  ,".png"), plot = routesMapRowEx2)
ggsave(filename = paste0(figDissDir, "/transectSamplingAllRoutesUsed"  ,".png"), plot = allRoutesUsed)

# # Make a list of the available objects for printing to console, just as a remidner!
# baseObjects <- rbind(
#   "usBaseMap: ggObj, states outlined",
#   "routesMap: ggObj, usBaseMap + black pts for routes",
#   "routesMapRowEx: ggObj, East-West running transect in red pts over usBaseMap",
#   "milBasesMap:  ggObj, all mil bases in red over usBaseMap",
#   "basesOfIntMap:  ggObj, Eglin and Riley in large font",
#   "basesOfInterest: data.frame; lat long bases of interest"
# )


################## END RUN ################## 