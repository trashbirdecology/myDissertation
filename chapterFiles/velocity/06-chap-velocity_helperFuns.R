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
