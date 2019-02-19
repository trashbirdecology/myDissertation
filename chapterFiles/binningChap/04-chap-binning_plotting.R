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

# Directories: define -----------------------------------------------------
if (!exists("resultsDir")) {
  resultsDir <- paste0(here::here(),
                       "/chapterFiles/binningChap/myResults")
}
figDir <- paste0(here::here(), "/chapterFiles/binningChap/figures")
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


# Spatial sampling grid - munging with results --------------------------------------------------

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
coordinates(results_dist) <-
  coordinates(results_ews) <- c("long", "lat")
sp::proj4string(results_dist) <- sp::proj4string(results_ews) <-
  sp::CRS("+proj=longlat +datum=WGS84")



# User-defined plotting parameters ------------------------------------------------------

to.plot <- "distances"
# to.plot <- "ews"

metric.ind <- c('dsdt', "s") # the metrics to print


plotResults = results_dist

# Plotting paramters ------------------------------------------------------

year.ind <- unique(plotResults@data$year) %>% sort()

sortVar.lab <-
  ifelse(unique(plotResults@data$direction) == "South-North",
         "latitude",
         "longitude")

# Define the plotting data based on to.plot arg
if (to.plot == "distances") {
  plotResults = results_dist
}
if (to.plot == "ews") {
  plotResults = results_ews
}


# BASEMAP: US STATES  --------------------------------------------------------
library(maps)

us_states <- map_data("state")

usBaseMap <-  ggplot() +
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = "white",
    alpha = 0
  )

# BASEMAP: MILITARY BASES -------------------------------------------------
milBases <- getMilBases()


milBases.df <- milBases %>% 
  as.data.frame() %>% 
  rename(lat = coords.x2, long = coords.x1)


usBaseMap +
  geom_point(data = milBases.df, aes(x = long, y = lat, color = ))


riley <- data.frame(long= -96.788448, lat = 39.199983)
eglin <- data.frame(long= -86.554, lat = 30.458)

closestSite <- function(mbs, site, coords, ndeg = 5, by = 1e-2){
  y = NULL
  while(length(y != 1) & ndeg > 0){
  ndeg = ndeg - by  
      y = mbs %>% filter(mbs$lat <  site$lat+ ndeg & 
                   mbs$lat > site$lat - ndeg   & 
                   mbs$long  > site$long - ndeg & 
                   mbs$long < site$long + ndeg
      )
  print(paste0("too many sites within ", ndeg, " degrees"))
  }
  
  y
  
}
  
mbs = milBases.df; site = riley
closestMB(x = milBases.df$lat, your.number = riley$lat, coords = milBases.df)
closestMB(x = milBases.df$long, your.number = riley$long, coords = milBases.df)
closestMB(x = milBases.df$lat, your.number = riley$lat, coords = milBases.df)

                      
# BEGIN NON-SPATIAL PLOTTING ----------------------------------------------

# Plot indiviudal transects -----------------------------------------------
for (i in 1:length(unique(plotResults$dirID))) {
  for (j in 1:length(unique(plotResults$direction))) {
    for (k in 1:length(unique(metric.ind))) {
      dirID.ind = unique(plotResults$dirID)[i]
      direction = unique(plotResults$direction)[j]
      metric =    unique(metric.ind)[k]
      
      p <- sort.year.line(
        plotResults,
        metric.ind = metric,
        year.ind =  year.ind,
        dirID.ind = dirID.ind,
        scale = T,
        center = T,
        direction = direction
      )
      
      fn <- paste0(figDir,
                   "/transect_",
                   dirID.ind,
                   "_",
                   direction,
                   "_metric_",
                   metric,
                   ".png")
      
      ggsave(filename = fn, plot = p)
      
    }
  }
}


# END NON-SPATIAL PLOTTING ------------------------------------------------

# Plot one metric across multiple years facet on US map -------------------
# BEGIN SPATIAL PLOTTING --------------------------------------------------

## plot all transects on USA map, each metric
for (j in 1:length(unique(plotResults$direction))) {
  for (k in 1:length(unique(metric.ind))) {
      direction = unique(plotResults$direction)[j]
      metric =    unique(metric.ind)[k]

      temp <- plotResults %>%
        as.data.frame() %>%
        filter(metricType == metric)
      
      p <-
        ggplot(aes(x = long, y = lat, fill = metricValue), data = temp) + geom_tile() +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          colour = "black",
          fill = "white",
          alpha = 0
        ) +
        coord_equal() +
        facet_wrap( ~ year, ncol = 1) +
        guides(fill = guide_legend(title = paste(metric))) +
        theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
        xlim(c(-125 ,-65))
      
      
      if (metric.ind[k] == "dsdt") {
        outlierLimits <- remove_outliers(temp$metricValue)
        
        ## Remove outliers for plotting purposes
        p <- p +
          scale_fill_gradient2(
            low = "red",
            midpoint = 0,
            high = "red",
            na.value = "transparent",
            limits = c(min(outlierLimits), max(outlierLimits))
          )
        
        
      }
      
      if (metric.ind[k] == "s") {
        p <-        p +
          scale_fill_gradient2(rainbow(2))
      }
      
      
      
      fn <-
        paste0(
          figDir,
          "/usaAllTsects_",
          direction,
          "_metric_",
          metric,
          ".png"
        )
      
      ggsave(
        filename = fn,
        plot = p,
        height = 8,
        width = 11
      )
      
    }
  }


## plot around Fort Riley on USA map, each metric
for (j in 1:length(unique(plotResults$direction))) {
  for (k in 1:length(unique(metric.ind))) {
    for (l in 1:length(unique(plotResults$year))) {
      direction = unique(plotResults$direction)[j]
      metric =    unique(metric.ind)[k]
      year = unique(plotResults$year)[l]
      
      temp <- plotResults %>%
        as.data.frame() %>%
        filter(metricType == metric)
      
      
      
      p <-
        ggplot(aes(x = long, y = lat, fill = metricValue), data = temp) + geom_tile() +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          colour = "black",
          fill = "white",
          alpha = 0
        ) +
        coord_equal() +
        facet_wrap( ~ year, ncol = 1) +
        guides(fill = guide_legend(title = paste(metric))) +
        theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
        xlim(c(-125 ,-65))
      
      
      if (metric.ind[k] == "dsdt") {
        outlierLimits <- remove_outliers(temp$metricValue)
        
        ## Remove outliers for plotting purposes
        p <- p +
          scale_fill_gradient2(
            low = "red",
            midpoint = 0,
            high = "red",
            na.value = "transparent",
            limits = c(min(outlierLimits), max(outlierLimits))
          )
        
        
      }
      
      if (metric.ind[k] == "s") {
        p <-        p +
          scale_fill_gradient2(rainbow(2))
      }
      
      
      
      fn <-
        paste0(
          figDir,
          "/usaAllTsects_",
          plotResults$year[l],
          direction,
          "_metric_",
          metric,
          ".png"
        )
      
      ggsave(
        filename = fn,
        plot = p,
        height = 8,
        width = 11
      )
      
    }
  }
}



# END SPATIAL-PLOTTING ----------------------------------------------------




# END RUN -----------------------------------------------------------------






# Plot the BBS routes, mil bases ------------------------------------------

plot_routes <- usplot +
  xlim(min(routePts$long) - 2, max(routePts$long)) +
  ylim(min(routePts$lat), max(routePts$lat) + 2) +
  geom_point(
    data = routePts,
    aes(x = long, y = lat, color = "bbsRoutes"),
    size = .4,
    show.legend = TRUE
  ) +
  xlab ("longitude") + ylab("latitude") +
  theme(legend.position = "bottom") +
  # theme(legend.position= c(-75,35))+
  scale_colour_manual(name = "",
                      values = c(bbsRoutes = "black"))#, myline2="red"))
plot_routes # plot of all bbs routes, simple map
plot_routes_filename <- paste0(figDir, "/plot_routes.png")

plot_routes_milbases <- plot_routes +
  geom_point(
    data = coordinates(milBases) %>% as.data.frame(),
    aes(
      x = milBases@coords[, 1],
      y = milBases@coords[, 2],
      color = "milBase"
    ),
    size = .8,
    show.legend = TRUE
  ) +
  xlab ("longitude") + ylab("latitude") +
  theme(legend.position = "bottom") +
  scale_colour_manual(name = "",
                      values = c(bbsRoutes = "black", milBase  = "red")) +
  xlim(-125, -69)

plot_routes_milbases
plot_routes_milbases_filename <-
  paste0(figDir, "/plot_routes_milbases.png")

## Save the figures to file
ggsave(filename = plot_routes_filename,
       plot_routes,
       width = 12,
       height = 9)
ggsave(
  filename = plot_routes_milbases_filename,
  plot_routes_milbases,
  width = 12,
  height = 9
)


# Plot example transect COL ----------------------------------------------

tmp <- routes_grid %>%
  distinct(lat, long, .keep_all = T) %>%
  filter(colID == 54)

tmpMilBases <- milBases@coords %>%
  as_tibble() %>%
  filter(coords.x1 > min(tmp$long) & coords.x1 < max(tmp$long)) %>%
  filter(coords.x2 > min(tmp$lat) & coords.x2 < max(tmp$lat))

plot_tsect <- usPlot +
  geom_point(
    data = tmp,
    aes(x = long, y = lat, color = "bbsRoutes"),
    size = .6,
    show.legend = TRUE
  ) +
  xlab ("longitude") + ylab("latitude") +
  theme(legend.position = "bottom") +
  scale_colour_manual(name = "", values = c(bbsRoutes = "black"))#, myline2="red"))

plot_tsect # plot of all bbs routes, simple map
plot_filename <- paste0(figDir, "/plot_tsect_colEx.png")
## Save the figures to file
ggsave(filename = plot_filename,
       plot_tsect,
       width = 12,
       height = 9)


# Plot transect example with military bases -------------------------------
tmpMilBases <- milBases@coords %>%
  as_tibble() %>%
  filter(coords.x1 > min(tmp$long) & coords.x1 < max(tmp$long)) %>%
  filter(coords.x2 > min(tmp$lat) & coords.x2 < max(tmp$lat))

plot_tsect_mb <- plot_tsect +
  geom_point(
    data = tmpMilBases,
    aes(x = coords.x1, y = coords.x2, color = "mb"),
    size = .6,
    show.legend = TRUE
  ) +
  xlab ("longitude") + ylab("latitude") +
  theme(legend.position = "bottom") +
  # theme(legend.position= c(-75,35))+
  scale_colour_manual(name = "",
                      values = c(mb = "red", bbsRoutes = "black"))#, myline2="red"))


plot_tsect_mb # plot of all bbs routes, simple map
plot_filename <- paste0(figDir, "/plot_tsect_colEx_mb.png")

## Save the figures to file
ggsave(filename = plot_filename,
       plot_tsect_mb,
       width = 12,
       height = 9)


# Plot example transect ROW ----------------------------------------------
tmp <- routes_grid %>%
  distinct(lat, long, .keep_all = T) %>%
  filter(rowID == 19)

plot_tsect <- ggplot()  +
  geom_polygon(
    data = us_states,
    mapping = aes(x = long, y = lat,
                  group = group),
    fill = "white",
    color = "black"
  ) +
  xlim(min(routePts$long) - 2, max(routePts$long)) +
  ylim(min(routePts$lat), max(routePts$lat) + 2) +
  geom_point(
    data = tmp,
    aes(x = long, y = lat, color = "bbsRoutes"),
    size = .6,
    show.legend = TRUE
  ) +
  xlab ("longitude") + ylab("latitude") +
  theme(legend.position = "bottom") +
  # theme(legend.position= c(-75,35))+
  scale_colour_manual(name = "",
                      values = c(bbsRoutes = "black"))#, myline2="red"))

plot_tsect # plot of all bbs routes, simple map
plot_tsect_filename <- paste0(figDir, "/plot_tsect_rowEx.png")

## Save the figures to file
ggsave(filename = plot_tsect_filename,
       plot_tsect,
       width = 12,
       height = 9)
