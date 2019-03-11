# This script creates some basemaps, and defines directories for plotting and a couple of helper functions.

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


######################## IMPORT SAMPLING GRID #######################
sampGrid <- readRDS(paste0(rObjs, "/samplingGrid.RDS"))

sampGridCoords <-
  cbind(sampGrid$sp_grd@data,
        coordinates(sampGrid$sp_grd)) %>%
  rename(lat = s2,
         long = s1,
         cellID  = id)
################## IMPORT & MUNGE RDM RESULTS ##################
# Import the calculated metrics (results) & merge with sampling grid --------------------------------------------
# a. Import EWSs
if (to.plot == "ews") {
  print("importing EWSs (e.g., FI, VI, CV)")
  ## FYI: varibles will likely be missing (NA) for metricTypes FI and VI, because these are calculated across ALL variables at a time...
  results <-
    bbsRDM::importResults(resultsDir = resultsDir,
                          myPattern = 'ews',
                          subset.by = direction) %>%
    # assign the end of the window as the cellID
    mutate(cellID = cellID_max)
  
  # join with spatial grid
  results <- full_join(sampGridCoords,
                       results_ews) %>%
    na.omit(metricType) %>%
    dplyr::select(-cellID_min, -cellID_max, -winStart  , -winStop)
   ## PLEASE NOTE:
    ### a full join of the sampling grid and the results for EWS will likely produce many cells with NO results data..
    ### however, NO lat or long should == NA!
  
  }

# b. Import distance results
if (to.plot == "distances") {
  print("importing distances")
  results <-
    bbsRDM::importResults(resultsDir = resultsDir,
                          myPattern = 'distances',
                          subset.by = direction)
  
  # join with spatial grid
  results <- full_join(sampGridCoords,
                       results) %>%
    na.omit(metricType)
}

 ## Set coordinate system and projection for both data sets! (the same)
coordinates(results) <- c("long", "lat")
sp::proj4string(results) <- sp::CRS("+proj=longlat +datum=WGS84")

################## CREATE BASE MAPS ##################
# This section will create numerous base maps off of which we will build results visualizations.

# Get the us state map data frim ggplot
us_states <- ggplot2::map_data("state")

# BASEMAP: US STATES  -------------------------------- 

usBaseMap <-  ggplot() +
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey80"
  ) +
  coord_fixed(1.3) +
  ggthemes::theme_map()

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
    size = 1
  )

# BASEMAP: MILITARY BASES -------------------------------------------------
milBases <- getMilBases()
milBases.df <- milBases %>% as.data.frame() %>%
  rename(lat = coords.x2, long = coords.x1)

milBasesMap <- usBaseMap +
  geom_point(data = milBases.df,
             aes(x = long, y = lat),
             color = "red",
             size = 0.75) +
  xlim(c(
    min(sampGrid$routes_grid$long),
    max(sampGrid$routes_grid$long)
  )) +
  ylim(c(
    min(sampGrid$routes_grid$lat),
    max(sampGrid$routes_grid$lat)
  ))


milBasesRoutesMap <- routesMap +
  geom_point(data = milBases.df,
             aes(x = long, y = lat),
             color = "red",
             size = 1) +
  xlim(c(
    min(sampGrid$routes_grid$long),
    max(sampGrid$routes_grid$long)
  )) +
  ylim(c(
    min(sampGrid$routes_grid$lat),
    max(sampGrid$routes_grid$lat)
  ))

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
  closestSite(milBases.df, rileyApprox, ndeg = 3, by = 0.1) %>% mutate(name = "Fort Riley"),
  closestSite(milBases.df, eglinApprox, ndeg = 3, by = 0.1) %>%  mutate(name = "Eglin AFB")
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


# Save the basemaps to file
ggsave(filename = paste0(figDissDir, "/milBases.png"), plot = milBasesMap)
ggsave(filename = paste0(figDissDir, "/milBasesAndRoutes.png"), plot = milBasesRoutesMap)
ggsave(filename = paste0(figDissDir, "/basesOfInterestMap.png"), plot = basesOfIntMap)
ggsave(filename = paste0(figDissDir, "/bbsRoutesUsed.png"), plot = routesMap)
ggsave(filename = paste0(figDissDir, "/transectSamplingEx_row18.png"), plot = routesMapRowEx)


# Make a list of the available objects for printing to console, just as a remidner!
baseObjects <- rbind(
  "usBaseMap: ggObj, states outlined",
  "routesMap: ggObj, usBaseMap + black pts for routes",
  "routesMapRowEx: ggObj, East-West running transect in red pts over usBaseMap",
  "milBasesMap:  ggObj, all mil bases in red over usBaseMap",
  "basesOfIntMap:  ggObj, Eglin and Riley in large font",
  "basesOfInterest: data.frame; lat long bases of interest"
)


################## END RUN ################## 