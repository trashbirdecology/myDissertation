plotLoop <- function(plotResults, metric.Ind, gg)

for(i in 1:length(unique(plotResults$dirID))){
  for(j in 1:length(unique(plotResults$direction))){
    for(k in 1:length(unique(metric.ind))){
      
      dirID.ind = unique(plotResults$dirID)[i]
      direction = unique(plotResults$direction)[j]
      metric =    unique(metric.ind)[k]
      
    
      fn <- paste0(figDir, "/transect_", dirID.ind,"_", direction,
                   "_metric_", metric, 
                   ".png")
      
      ggsave(filename = fn, plot = gg)
      
    }
  }
}


plotLoop(plotResults, metric.Ind = , gg = )