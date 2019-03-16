# By now we should have already imported the results. 

# First, we need to join the cellID/colID/rowID grid data to the r --------

cellID <- routes_grid %>% dplyr::select(cellID, rowID, colID)
results2 <- full_join(results %>% as_tibble(), cellID) 

# Join with all the points tin inclufe cells that were not sampled
results2 <- full_join(sp_grd %>% as_tibble() %>% rename(cellID = id), results2)


# Filter by metric --------------------------------------------------------
if("FI" %in% metric.ind){
    results.plot <-  results2 %>% 
    filter(metricType %in% c("FI_Eqn7.12", NA))
}



# Identify "significant" cahnges in FI within a transect x year  ----------

temp = results2 %>% 
  group_by(year, rowID) %>% 
  mutate(mean.met  = mean(metricValue, na.rm=TRUE), 
         sd.met    = sd(metricValue, na.rm=TRUE)
                 ) %>% 
  ungroup() %>% 
  mutate(
    met.highlight = as.factor(ifelse((metricValue > mean.met+1.96*sd.met | metricValue < mean.met-1.96*sd.met), 
                           "out2SD", "in2SD"
                           )))

ecoregions <- getEcoregions() 
ggplot(data = ecoregions, aes(x = long, y = lat, group = group)) + geom_path()


usBaseMap+
  geom_point(data = temp %>% filter(met.highlight == "out2SD"), 
             aes(x = long, y = lat, color = year, size =1, alpha = .7))+
  scale_color_viridis_d()

usBaseMap+
  geom_point(data = temp %>% filter(met.highlight == "out2SD", rowID %in% 15:16), 
             aes(x = long, y = lat, color = year, size =1, alpha = .7))+
  scale_color_viridis_d()




# Plot fi for a signle transect -------------------------------------------

glimpse(results.plot)

df <- results.plot %>% 
  filter(year == 2010)
  
t1 <- df[df$rowID == 11, ]
t2 <- df[df$rowID == 12, ]

cor(t1$metricValue, t2$metricValue)
  
  summarise(cor.fi  = cor(metricValue) )

cor(results.plot %>% filter(dirID ==)

  cor(t1, t2)
  summarise(cor.fi = cor(metricValue, method = "pearson"))

View(cor.results)



# Plot the pair-wise FI values --------------------------------------------

ggplot(myResults, aes(x = long, y = metricValue)) + 
  geom_smooth()+
  facet_wrap(~rowID)

# Calculate correlations
  
  

## messing around 
results.temp <-   results.fi %>% 
  filter(
    year %in%  2000:2003,
    direction == "East-West", dirID %in% c(15:20)) %>% 
  arrange(long) %>% 
  dplyr::select(long,metricValue, dirID, year) %>% distinct() 

ggplot(results.temp, aes(x = long, y = metricValue, group = dirID))+
  geom_smooth(aes(color = as.factor(dirID)),method = "loess")+ 
  scale_color_viridis_d()+
  geom_rug(sides = "b")+
  facet_wrap(~year, ncol  = 1, scales= "free_y")