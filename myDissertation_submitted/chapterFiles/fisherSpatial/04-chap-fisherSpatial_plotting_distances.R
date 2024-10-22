## This script is sources from fisherSpatial_analysis.R

# Define some parameters --------------------------------------------------
year.ind <- unique(results@data$year) %>% sort()

sortVar.lab <-
  ifelse(unique(results@data$direction) == "South-North",
         "latitude",
         "longitude")

##################### BEGIN NON-SPATIAL PLOTTING #####################
# Plot indiviudal transects with a single metric per figure -----------------------------------------------
for (i in 1:length(unique(results$dirID))) {
  for (j in 1:length(unique(results$direction))) {
    for (k in 1:length(unique(metric.ind))) {
      dirID.ind = unique(results$dirID)[i]
      direction = unique(results$direction)[j]
      metric =    unique(metric.ind)[k]
    
      
      p <- sort.year.line(
        results,
        metric.ind = metric,
        year.ind =  year.ind,
        dirID.ind = dirID.ind,
        scale = T,
        center = T,
        direction = direction # make sure we only have one direction of data!
      ) +
        theme(plot.title = element_text(size=8))+
        theme(legend.position = "none")+
        scale_color_grey()
                     

      my.fn <- paste0(figDir,
                   "/transect_",
                   dirID.ind,
                   "_",
                   direction,
                   "_metric_",
                   metric,
                   ".png")
      
      ggsave(filename = my.fn, plot = p)
      
    }
  }
}

##################### BEGIN SPATIAL PLOTTING #####################
###############################################################
## I AM HAVING ISSUES PLOTTING SPATIAL DATA RIGHT NOW..#######
###############################################################
## plot all transects on USA map, each metric for a single year
for(i in 1:length(unique(results$year))){
for (j in 1:length(unique(results$direction))) {
  for (k in 1:length(unique(metric.ind))) {
    direction = unique(results$direction)[j]
    metric =    unique(metric.ind)[k]
   year = unique(results$year[i]) 
    
    temp <- results %>%
      as.data.frame() %>%
      filter(metricType == metric) %>% 
      filter(year == year)
      
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
      facet_wrap( ~ year, ncol = 2) +
      guides(fill = guide_legend(title = paste(metric))) +
      xlim(c(-125 ,-65))+
      ggthemes::theme_map()+
      theme(legend.position = "none")+
      theme(strip.background = element_blank(), 
            strip.text = element_text(size=10))+
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
          limits = c(min(outlierLimits), max(outlierLimits)))
    }
    
    if (metric.ind[k] == "s") {
      p <-  p +
        scale_fill_gradient2(rainbow(2))
    }
    
    my.fn <-
      paste0(
        figDir,
        "/usaAllTsects_",
        direction,
        "_metric_",
        metric,
        ".png"
      )
    
    ggsave(
      filename = my.fn,
      plot = p
    )
    
  }
}}


## plot around Fort Riley on USA map, each metric
for(i in 1:nrow(basesOfInterest)){
  for (j in 1:length(unique(results$direction))) {
    for (k in 1:length(unique(metric.ind))) {
      for (l in 1:length(unique(results$year))) {
        direction = unique(results$direction)[j]
        metric =    unique(metric.ind)[k]
        year = unique(results$year)[l]
        
        base = basesOfInterest[i,]
        
        temp <- results %>%
          as.data.frame() %>%
          filter(metricType == metric)
        
        
        p <-
          ggplot(aes(x = long, y = lat, fill = metricValue), data = temp) + geom_tile() +
          coord_equal() +
          facet_wrap( ~ year, ncol = 2) +
          guides(fill = guide_legend(title = paste(metric))) +
          xlim(c(base$long- 4, base$long + 4))+
          ylim(c(base$lat- 6, base$lat + 6))+
          title(paste0("Military base: ", base$COMPONENT, " ", base$name))+
          # ggthemes::theme_map()+
          theme(legend.position = "none")+
          theme(strip.background = element_blank(), 
                strip.text = element_text(size=10))+
          theme.margin
        
        p
        
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
        
        
        my.fn <-
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
          filename = my.fn,
          plot = p)
        
      }
    }
  }
}

###################### END SPATIAL-PLOTTING #####################