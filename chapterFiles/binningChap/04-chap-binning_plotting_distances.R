# SOURCE BASEMAPS AND DATA ------------------------------------------------
# source(paste0(here(), "/chapterFiles/binningChap/04-chap-binning_plotting_base.R" ))


# User-defined plotting parameters ------------------------------------------------------

## FOR DISTANCES
to.plot <- "distances"
metric.ind <- c('dsdt', "s") # the metrics to print


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
          limits = c(min(outlierLimits), max(outlierLimits))+
            theme.margin
        )
      
      
    }
    
    if (metric.ind[k] == "s") {
      p <-        p +
        scale_fill_gradient2(rainbow(2))+
        theme.margin
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
      plot = p
    )
    
  }
}


## plot around Fort Riley on USA map, each metric
for(i in 1:nrow(basesOfInterest)){
  for (j in 1:length(unique(plotResults$direction))) {
    for (k in 1:length(unique(metric.ind))) {
      for (l in 1:length(unique(plotResults$year))) {
        direction = unique(plotResults$direction)[j]
        metric =    unique(metric.ind)[k]
        year = unique(plotResults$year)[l]
        
        base = basesOfInterest[i,]
        
        temp <- plotResults %>%
          as.data.frame() %>%
          filter(metricType == metric)
        
        
        p <-
          ggplot(aes(x = long, y = lat, fill = metricValue), data = temp) + geom_tile() +
          coord_equal() +
          facet_wrap( ~ year, ncol = 1) +
          guides(fill = guide_legend(title = paste(metric))) +
          theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
          xlim(c(base$long- 4, base$long + 4))+
          ylim(c(base$lat- 4, base$lat + 4))+
          title(paste0("Military base: ", base$COMPONENT, " ", base$name))+
          theme.margin
        
        
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
            )+
            theme.margin
          
          
        }
        
        if (metric.ind[k] == "s") {
          p <-        p +
            scale_fill_gradient2(rainbow(2))+
            theme.margin
        }
        
        
        fn <-
          paste0(
            figDir,
            "/base_",
            base$name,
            "_",
            direction,
            "_metric_",
            metric,
            ".png"
          )
        
        ggsave(
          filename = fn,
          plot = p)
        
      }
    }
  }
}


# END SPATIAL-PLOTTING ----------------------------------------------------



