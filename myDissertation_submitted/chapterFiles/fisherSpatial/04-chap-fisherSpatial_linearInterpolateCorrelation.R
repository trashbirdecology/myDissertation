# I am using lienar interpolation to visualize adjacent to eachother in space.
## If Fisher Information is in fact identifying regime shifts in space, then similar signals should be autocorrelated.

interpAndCor <- function(results, metricType.ind = "FI_Eqn7.12"){
temp <- results %>%
  as_tibble() %>%
  filter(metricType == metricType.ind)

tempLatLong <- temp %>% group_by(dirID) %>% 
  summarise(minLat =  min(lat), # get the max of the minimum latitudes for each route so as to not extrapolate
            maxLat = max(lat), 
            minLong = min(long), 
            maxLong = max(long)
          ) 


xout <- seq(max(tempLatLong$minLong), min(tempLatLong$maxLong), length.out = 50)

results.interp <-
  temp %>%
  group_by(year, dirID, direction, metricType) %>%
  filter(n() > 1) %>% 
  nest()  %>%
  mutate(newX = purrr::map(data, ~ approx(.$long, .$metricValue, xout = xout)$x, rule = 1),
         newY = purrr::map(data, ~ approx(.$long, .$metricValue, xout = xout)$y, rule = 1)) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  mutate(year = as.integer(as.character(year)))

# Calculate correlations among transects ----------------------------------
temp.ID <- unique(results.interp$dirID)  
year.ID <- unique(results.interp$year)

cor.interp.results <- tibble()

for(j in seq_along(year.ID)) {
  for (i in seq_along(temp.ID)) {
    if (exists("cor.temp"))rm(cor.temp)
    
    if(i == max(seq_along(temp.ID)))next()
    
    # First transect interpolated results
    df1 = results.interp %>% filter(year == year.ID[j], dirID %in% temp.ID[i])  
    # Second transect interpolated results
    df2 =  results.interp %>% filter(year == year.ID[j], dirID %in% temp.ID[(i + 1)]) 
    
    if(nrow(df1)!=0 & nrow(df2)!=0){ cor.interp = cor(df1$newY, df2$newY, "complete.obs") %>% as.numeric() %>% round(3)}else(cor.interp = NULL)
    cor.interp = rbind(df1 %>% mutate(cor.interp = cor.interp), 
          df2 %>% mutate(cor.interp = cor.interp)) %>% 
      mutate(tsect.pair = paste0(temp.ID[i], ":" , temp.ID[i + 1]))
    
    cor.interp.results <- bind_rows(cor.interp.results, cor.interp)
  }
}  

return(cor.interp.results)
}

