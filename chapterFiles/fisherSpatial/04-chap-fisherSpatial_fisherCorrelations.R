# By now we should have already imported the results. 

if("FI" %in% metric.ind){
  
  results.fi <-  results %>% as_tibble() %>% 
    filter(metricType == "FI_Eqn7.12")
  
  results.temp <-   results.fi %>% 
      filter(year %in%  2000:2010, direction == "East-West", dirID %in% c(9:15)) %>% 
      arrange(long) %>% 
      dplyr::select(long,metricValue, dirID, year) %>% distinct() 
  
  ggplot(results.temp, aes(x = long, y = metricValue, group = dirID))+#, color =dirID )+
    geom_smooth(aes(color = as.factor(dirID)),method = "lm")+ 
    scale_color_viridis_d()+
    geom_rug(sides = "b")+
    facet_wrap(~year, ncol  = 1, scales= "free_y")
  
  

  plot.dat <- results %>% as_tibble() %>% 
  filter(dirID == dirID.ind, 
         direction == direction.ind,
         year %in% year.ind, 
         metricType %in% metric)



}