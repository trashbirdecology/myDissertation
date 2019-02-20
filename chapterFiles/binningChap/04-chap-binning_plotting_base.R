# GGPLOT custom defs ------------------------------------------------------
theme1 <-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

theme.margin <-theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Helper Functions --------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- stats::quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y <- na.omit(y)
  return(y)
}

# Create function to identify closest military base to those of interst to me.
closestSite <- function(mbs, site, ndeg = 5, by = 0.1){
  y = 1:999
  ndeg = ndeg + by
  
  for(i in 1:1000){
    
    ndeg = ndeg - by
    if(ndeg < 0){break("could not find a single closest site. try new deg and/or by values.")}
    
    y = mbs %>% filter(mbs$lat <  site$lat+ ndeg & 
                         mbs$lat > site$lat - ndeg   & 
                         mbs$long  > site$long - ndeg & 
                         mbs$long < site$long + ndeg)
    
    if(length(y)==1){break}
  }
  
  print(paste0("Closest base is within ", paste(ndeg), " degrees lat and long of your site."))
  
  return(y)
  
}

# Directories: define -----------------------------------------------------
if (!exists("resultsDir")) {
  resultsDir <- paste0(here::here(),
                       "/chapterFiles/binningChap/myResults")
}

figDir <- paste0(here::here(), "/chapterFiles/binningChap/figures")

figDissDir <- paste0(here::here(), "/chapterFiles/binningChap/figures/figsCalledInDiss")


rObjs <- paste0(here::here(),
                "/chapterFiles/binningChap/rObjs")

animDir <-
  paste0(here::here(),
         "/chapterFiles/binningChap/figures/animations")

# direction = "South-North"
direction = "East-West"

# Import the calculated metrics (results) ------------------------------------------------------------
results_ews <-
  importResults(resultsDir = resultsDir,
                myPattern = 'ews',
                subset.by = direction) %>%
  # assign the end of the window as the cellID
  mutate(cellID = cellID_max)

## FYI: varibles will likely be missing (NA) for metricTypes FI and VI, because these are calculated across ALL variables at a time...

# b. Import distance results
results_dist <-
  importResults(resultsDir = resultsDir,
                myPattern = 'distances',
                subset.by = direction)


# Import spatial sampling grid + join with results --------------------------------------------------

#Import the sampling grid
sampGrid <- readRDS(paste0(rObjs, "/samplingGrid.RDS"))

sampGridCoords <-
  cbind(sampGrid$sp_grd@data,
        coordinates(sampGrid$sp_grd)) %>%
  rename(lat = s2,
         long = s1,
         cellID  = id)


## Join coords_grd with results
# note: a full join will likely produce many cells with NO results data..
# but NO lat or long should == NA!
results_dist <-
  full_join(sampGridCoords,
            results_dist) %>%
  na.omit(metricType)

results_ews <-
  full_join(sampGridCoords,
            results_ews) %>%
  na.omit(metricType) %>%
  dplyr::select(-cellID_min,-cellID_max, -winStart  , -winStop)

## Set coordinate system and projection for both data sets! (the same)
coordinates(results_dist) <-  coordinates(results_ews) <- c("long", "lat")
sp::proj4string(results_dist) <- sp::proj4string(results_ews) <-
  sp::CRS("+proj=longlat +datum=WGS84")

# spTransform(results_dist) <- spTransform(results_ews) <-
#   sp::CRS("+proj=longlat +datum=WGS84")


# BASEMAP: US STATES  --------------------------------------------------------
us_states <- map_data("state")

usBaseMap <-  ggplot() +
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "grey")+
    theme1+
  theme.margin+
  coord_fixed(1.3)

# usBaseMap

# BASEMAP: US STATES  + BBS ROUTES --------------------------------------------------------
routesMap <- usBaseMap +
  geom_point(data = sampGrid$routes_grid,
             aes(x=long, y = lat), color = "black", size = .75)+
  theme_bw()+
  theme1+
  theme.margin
# routesMap

ggsave(filename = paste0(figDissDir, "/bbsRoutesUsed.png"), plot = routesMap)

# BASEMAP: GRID SAMPLING DESIGN -------------------------------------------

# I would really like to get a fig of sampling grid with outlines of e.g., one row. But I cannot find a way to integrate with ggplot, so I will resort to plotting single dirIDs on the route map.
# spplot(sampGrid$sp_grd)
if(direction == "East-West"){
routesMapRowEx <- usBaseMap +
  geom_point(data = sampGrid$routes_grid %>% filter(rowID == 18),
             aes(x=long, y = lat), color = "red", size = 1)+
  theme_bw()+
  theme1+
  theme.margin
ggsave(filename = paste0(figDissDir, "/transectSamplingEx_row18.png"), plot = routesMapRowEx)

}


# BASEMAP: MILITARY BASES -------------------------------------------------

milBases <- getMilBases()
milBases.df<- milBases %>% as.data.frame()%>% 
  rename(lat = coords.x2, long = coords.x1)

milBasesMap <- usBaseMap +
  geom_point(data = milBases.df, aes(x = long, y = lat), color = "red", size = 0.75)+
  xlim(c(min(sampGrid$routes_grid$long), max(sampGrid$routes_grid$long)))+
  ylim(c(min(sampGrid$routes_grid$lat), max(sampGrid$routes_grid$lat)))+
  theme.margin
  
ggsave(filename = paste0(figDissDir, "/milBases.png"), plot = milBasesMap)

milBasesRoutesMap <- routesMap +
  geom_point(data = milBases.df, aes(x = long, y = lat), color = "red", size = 0.75)+
  xlim(c(min(sampGrid$routes_grid$long), max(sampGrid$routes_grid$long)))+
  ylim(c(min(sampGrid$routes_grid$lat), max(sampGrid$routes_grid$lat)))

ggsave(filename = paste0(figDissDir, "/milBasesAndRoutes.png"), plot = milBasesRoutesMap)


  
# define approx. loc. of bases of interest. 
rileyApprox <- data.frame(long= -96.788448, lat = 39.199983, base = "riley")
eglinApprox <- data.frame(long= -86.554, lat = 30.458, base = "riley")

# hopefully correct locations for bases.
basesOfInterest <- rbind(
  closestSite(milBases.df, rileyApprox, ndeg = 3, by = 0.1) %>% mutate(name = "Fort Riley"),
  closestSite(milBases.df, eglinApprox, ndeg = 3, by = 0.1) %>%  mutate(name = "Eglin AFB")
)

rm(rileyApprox); rm(eglinApprox)

basesOfIntMap <- usBaseMap + 
  geom_point(data = basesOfInterest, aes(x = long, y = lat), color = "darkred", shape=18 , size = 4)+
  geom_text(data = basesOfInterest, aes(x =long, y = lat, label = name), nudge_x = 0, nudge_y = 2, color = "darkred", size = 5)+
  theme.margin

ggsave(filename = paste0(figDissDir, "/basesOfInterestMap.png"), plot = basesOfIntMap)

               
