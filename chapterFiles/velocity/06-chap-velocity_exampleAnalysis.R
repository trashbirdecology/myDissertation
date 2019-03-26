# Simulate a simple, two variable system 
if(!exists("mean.analy")) mean.analy=FALSE
if(!exists("var.analy")) var.analy=FALSE

# Vary the mean value of x_1, constant variance ---------------------------
if(mean.analy){
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
}


# Vary the variance -------------------------------------------------------
if(var.analy){
dir.create("./chapterFiles/velocity/simResults/")
var.vec <- seq(1,25, by = 1)
for(j in 1:1e4){ # this will take some time. 
  dist.sim <- NULL
  for(i in seq_along(var.vec)){
    x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 100, sd = var.vec[i], n=50))
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
      mutate(var.sim = (var.vec[i]), 
             sim.num = j)
    dist.sim <- suppressMessages(bind_rows(dist.sim, dist))
  }
  print(paste('j = ', j))
  feather::write_feather(x = dist.sim, path = paste0("./chapterFiles/velocity/simResults/var_x1_1e5sims_",j ,".feather"))
}
}



# Approx results fun ------------------------------------------------------
approx_dist.sim <- function(t.mult){
  
  dist<-list()
  x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 100, sd = 5, n=50))
  x_2 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 50, sd = 5, n=50))
  t = 1:length(x_1)
  dist[[1]] <- data.frame(t, x_1, x_2) %>% 
    gather(key="variable", value="value", -t)
  
  x_1 = approx(t, x_1, n = t.mult*length(t))
  x_2 = approx(t, x_2, n = t.mult*length(t))
  t=x_1$x
  
  df.wide = data.frame(t, x_1$y, x_2$y)
  
  df  = data.frame(t, x_1, x_2) %>% 
    tidyr::gather(key = "variable", value = "value", -t) 
  dist[[2]] <- df %>% 
    mutate(cellID = 1) %>% 
    rename(sortVar = t) %>% 
    regimeDetectionMeasures::calculate_distanceTravelled() %>% 
    dplyr::select(-cellID) %>% 
    rename(t = sortVar)
  
  return(dist)
}

# End Run -----------------------------------------------------------------

