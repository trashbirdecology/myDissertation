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
      direction.ind = unique(results$direction)[j]
      metric =  unique(metric.ind)[k]
      

      if("FI" %in% metric) metric = c(metric, 'FI', 'FI_Eqn7.12', 'FI_Eqn7.2c')
      
      plot.dat <- results %>% as_tibble() %>% 
        filter(dirID == dirID.ind, 
               direction == direction.ind,
               year %in% year.ind, 
               metricType %in% metric)
      
      if(nrow(plot.dat) < 3 ) next("not enough data to plot, skipping dir ID ", dirID, " metric ", metric)
      
      if(sortVar.lab == "latitude") plot.dat$sortVar = plot.dat$lat
      if(sortVar.lab == "longitude") plot.dat$sortVar = plot.dat$long
      # Only works if more than one year of data...

      p <-
        ggplot(data = plot.dat, aes(x = sortVar, y = metricValue , color = year)) +
            geom_line( size = 0.8)+
        scale_color_grey(breaks = c(1980, 1990, 2000, 2010))+
        # theme_few()+
        xlab(sortVar.lab)+
        ylab(metric)+
        envalysis::theme_publish()+
        # theme(legend.position = "none")+
        geom_rug(na.rm = TRUE, sides = "b")
      p
      
      my.fn <- paste0(figDir,
                      "/transect_",
                      dirID.ind,
                      "_",
                      direction,
                      "_metric_",
                      unique(plot.dat$metricType),
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
  for (j in 1:length(unique(results$direction))) {
    for (k in 1:length(unique(metric.ind))) {
      direction = unique(results$direction)[j]
      metric =    unique(metric.ind)[k]
      yr.temp <- unique(results$year) %>% as.character() %>% as.integer()
      yr.temp <- unique(yr.temp)[which(unique(yr.temp)%%10==0)] 
      
      
      if(metric == "FI") metric = c('FI', 'FI_Eqn7.12', 'FI_Eqn7.2c')
      
      temp <- results %>%
        as.data.frame() %>%
        filter(metricType %in% metric) %>% 
        filter(year %in% yr.temp)
      
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
        # xlim(c(-125 ,-65))+
        # envalysis::theme_publish()+
        ggthemes::theme_map()+
        theme(legend.position = "bottom")+
        theme(strip.background = element_blank(), 
              strip.text = element_text(size=10))+
        theme.margin +
        scale_fill_gradient(low="blue", high="red", breaks = c(min(temp$metricValue), max(temp$metricValue)), 
                             labels=(c("min.", "max")))
      
      
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
      
      
      
      my.fn <-
        paste0(
          figDissDir,
          "/usaAllTsects_",
          direction,
          "_metric_",
          metric[1],
          ".png"
        )
      
      ggsave(
        filename = my.fn,
        plot = p
      )
      
    }
  }


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
          filter(metricType %in% metric)
        
        
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
            figDissDir,
            "/base_",
            base$name,
            "_",
            direction,
            "_metric_",
            metric[1],
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