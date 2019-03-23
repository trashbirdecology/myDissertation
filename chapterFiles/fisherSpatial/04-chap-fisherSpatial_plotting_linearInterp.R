# Source the FUNCTION to run linear interpolation and calculate correlations among interpolated FI results
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_linearInterpolateCorrelation.R")

cor.interp.results <- interpAndCor(results, metricType.ind = "FI_Eqn7.12") %>% 
  filter(tsect.pair %in%  paste0(dir.use, ":", lead(dir.use)))


# Plot select transect-pairs ----------------------------------------------
yr.temp <- unique(cor.interp.results$year)[which(unique(cor.interp.results$year)%%10==0)] 
cor.lab <- cor.interp.results %>% filter(year %in% yr.temp) %>%  group_by(tsect.pair, cor.interp, year) %>% 
  summarise(newX = min(newX), newY = max(newY))

cor.p1 <-
  ggplot(data = cor.interp.results %>% filter(year %in% yr.temp), 
       aes(x = newX, y = newY, color = as.factor(dirID)))+
  geom_line()+
  facet_grid(year~tsect.pair, scales = "free_y")+
  theme_bw()+ 
  scale_color_viridis_d() +
  # scale_color_grey()+
  theme.margin+
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "inside")+
geom_text(inherit.aes = FALSE,
  data    = cor.lab,
  mapping = aes(x = -Inf, y = Inf, 
                label = paste0("r^2 ==",cor.interp)), parse = TRUE,
  hjust   = -.1,
  vjust   = 2.5, 
  size = 2,
  fontface = "bold")+
  xlab("longitude")+ylab("FI (interpolated)")
cor.p1

ggsave(filename = paste0(
    figDissDir,
    "/interpolated_FI_corplotSelectTransects_",
    unique(cor.interp.results$direction),
    ".png"
  ),
  plot = cor.p1)

cor.p1.log <-
  ggplot(data = cor.interp.results %>% filter(year %in% yr.temp), 
         aes(x = newX, y = log(newY+1), color = as.factor(dirID)))+
  geom_line()+
  facet_grid(year~tsect.pair, scales = "free_y")+
  theme_bw()+ 
  scale_color_viridis_d() +
  # scale_color_grey()+
  theme.margin+
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "inside")+
  geom_text(inherit.aes = FALSE,
            data    = cor.lab,
            mapping = aes(x = -Inf, y = Inf, 
                          label = paste0("r^2 ==",cor.interp)), parse = TRUE,
            hjust   = -.1,
            vjust   = 1.95, 
            size = 2,
            fontface = "bold")+
  xlab("longitude")+ylab("log FI (interpolated)")
cor.p1.log

ggsave(filename = paste0(
  figDissDir,
  "/interpolated_FI_corplotSelectTransects_",
  unique(cor.interp.results$direction),
  "_log.png"
),
plot = cor.p1.log)


# Plot a single transect pair over time (FI) -------------------------------------------------------
cor.p3 <- 
  ggplot(data = cor.interp.results %>% filter(dirID %in% 12:13) %>% 
           mutate(year = as.factor(as.character(year))), 
         aes(x = newX, y = newY, group = year, color = year))+
  geom_line(size=1)+
  facet_wrap(~dirID, scales = "free_y", ncol=1)+
  theme_bw()+ 
  scale_color_viridis_d()+
  theme(strip.background = element_blank(), strip.placement = "inside")+
  xlab("longitude")+ylab("FI (interpolated)")

cor.p3
ggsave(filename = paste0(
  figDissDir,
  "/interp_FI_singlePair_corPlot_",
  unique(cor.interp.results$direction),
  ".png"
),
plot = cor.p3)


# plot a single transect Fi value over space @ year 2010 (expensive!) ------------------------------
temp <-sampGrid$routes_grid %>% as.data.frame() %>% filter(rowID %in% dir.use) %>%  group_by(rowID) %>% summarise(lat = mean(lat)) %>% rename(dirID = rowID)
tempDat <- cor.interp.results %>% filter(tsect.pair %in% paste0(dir.use, ":", lead(dir.use)), year == 2010) %>% 
  group_by(year, dirID) %>% 
    mutate(newY = base::scale(newY)) %>% 
  mutate(lat = if_else(dirID == 11, temp$lat[1], if_else(dirID == 12, temp$lat[2], temp$lat[3])))

cor.p4 <- eco_poly_basemap +
  coord_map(xlim = c(min(tempDat$newX - 2), max(tempDat$newX + 2)),
            ylim = c(min(temp$lat)-7, max(temp$lat) + 7))+
  geom_point(data=tempDat, aes(x=newX, y= lat , size = newY, group = dirID)) 


bbox <- data.frame(x1 = -106,x2 =-102 ,y1 = 39.5,y2 = 43.5)
cor.p4.5 <- cor.p4 +
  geom_rect(data = bbox, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=NA, color="red", alpha=0.5)

## Zoom in to rockies
cor.p5 <- eco_poly_basemap +
  coord_map(xlim = c(bbox$x1, bbox$x2), ylim=c(bbox$y1, bbox$y2))+
  geom_point(data=tempDat, aes(x=newX, y= lat ,  size = newY))

ggsave(filename = paste0(
  figDissDir,
  "/scaledFiInterpolated_year2010_zoom_",
  unique(cor.interp.results$direction),
  ".png"
),
plot = gridExtra::grid.arrange(cor.p4.5,cor.p5, ncol=1))


# plot a single transect Fi value over space @ year 2000 (expensive!) ------------------------------
temp <-sampGrid$routes_grid %>% as.data.frame() %>% filter(rowID %in% dir.use) %>%  group_by(rowID) %>% summarise(lat = mean(lat)) %>% rename(dirID = rowID)
tempDat <- cor.interp.results %>% filter(tsect.pair %in% paste0(dir.use, ":", lead(dir.use)), year == 2000) %>% 
  group_by(year, dirID) %>% 
  mutate(newY = base::scale(newY)) %>% 
  mutate(lat = if_else(dirID == 11, temp$lat[1], if_else(dirID == 12, temp$lat[2], temp$lat[3])))

cor.p4 <- eco_poly_basemap +
  coord_map(xlim = c(min(tempDat$newX - 2), max(tempDat$newX + 2)),
            ylim = c(min(temp$lat)-7, max(temp$lat) + 7))+
  geom_point(data=tempDat, aes(x=newX, y= lat , size = newY, group = dirID)) 

ggsave(filename = paste0(
  figDissDir,
  "/scaledFiInterpolated_year2000_",
  unique(cor.interp.results$direction),
  ".png"
),
plot = cor.p4)

# END RUN -----------------------------------------------------------------





