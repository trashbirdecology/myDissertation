# By now we should have already imported the results. 

if("FI" %in% metric.ind){
  
  results.fi <-  results %>% as_tibble() %>% 
    filter(metricType == "FI_Eqn7.12")
  
  ggplot(results.fi, aes(x = long, y = metricValue, group = dirID))+#, color =dirID )+
    geom_smooth(aes(color = as.factor(dirID)))+#,method = "lm")+ 
    scale_color_viridis_d()+
    geom_rug(sides = "b")+
    facet_wrap(~year, ncol  = 1, scales= "free_y")
  
  
  sd.dat <- results.fi %>% as_tibble() %>% 
    filter(dirID %in% dirID.ind, 
           direction == direction,
           year %in% year.ind) %>% 
    group_by(year, dirID, direction, metricType) %>% 
    mutate(mean = mean(metricValue, na.rm=T), 
           sd   = sd(metricValue)) %>% 
    ungroup() %>% 
    mutate(flag1sd = ifelse(metricValue < mean-sd | metricValue > mean + sd, "out1SD", "in1SD"),
           flag2sd = ifelse(metricValue < mean-2*sd | metricValue > mean + 2*sd, "out2SD", "in2SD")) 
  
  
  
  x = sd.dat %>% filter(flag1sd == "out2SD", metricType =="FI") %>% distinct(year, dirID, long, lat,  metricType) 
  
  ggplot(x, aes(x = long, y = lat, group = long, color = metricType))+
    geom_point()+
    facet_wrap(~year)
  
  
  # not run
    do(fit = lm(metricValue~long, data = .))

# get the coefficients by group in a tidy data_frame
lm.dat <- broom::tidy(lm.dat, fit)


}