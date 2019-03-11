## Source the file 04-chap-binning_plotting_base.R before saving these to file. 

## These figures should only be created once -- they are base maps that shouldn't really change with the results and are called directly in the dissertation from file.


source("./chapterFiles/fisherSpatial/04-chap-binning_plotting_base.R")
       
ggsave(filename = paste0(figDissDir, "/milBases.png"), plot = milBasesMap)
ggsave(filename = paste0(figDissDir, "/milBasesAndRoutes.png"), plot = milBasesRoutesMap)
ggsave(filename = paste0(figDissDir, "/basesOfInterestMap.png"), plot = basesOfIntMap)
ggsave(filename = paste0(figDissDir, "/bbsRoutesUsed.png"), plot = routesMap)
ggsave(filename = paste0(figDissDir, "/transectSamplingEx_row18.png"), plot = routesMapRowEx)
