# Simulate a simple, two variable system 

# Vary the mean value of x_1, constant variance ---------------------------
dir.create("./chapterFiles/velocity/simResults/")
mean.vec <- seq(25,100, by = 5)
for(j in 1:1e4){ # this will take ~ 2 minutes per 100 j. 
dist.sim <- NULL
  for(i in seq_along(mean.vec)){
    x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = mean.vec[i], sd = 5, n=50))
    x_2 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 50, sd = 5, n=50))
    t = 1:length(x_1)
    df.wide = data.frame(t, x_1, x_2) 
    df  = data.frame(t, x_1, x_2) %>% 
      tidyr::gather(key = "variable", value = "value", -t) 
    dist <- df %>% 
      mutate(cellID = 1) %>% 
      rename(sortVar = t) %>% 
      regimeDetectionMeasures::calculate_distanceTravelled() %>% 
      dplyr::select(-cellID) %>% 
      rename(t = sortVar) %>% 
      mutate(mean.sim = (mean.vec[i]-25), 
             sim.num = j)
    dist.sim <- suppressMessages(bind_rows(dist.sim, dist))
  }
  print(paste('j = ', j))
feather::write_feather(x = dist.sim, path = paste0("./chapterFiles/velocity/simResults/mean_x1_1e5sims_",j ,".feather"))
}





