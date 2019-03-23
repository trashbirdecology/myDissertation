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


# plot a single transect Fi value over space ------------------------------
temp <-sampGrid$routes_grid %>% as.data.frame() %>% filter(rowID == 12) %>% summarise(mean(lat)) %>% as.numeric()
tempDat <- cor.interp.results %>% filter(tsect.pair =="11:12")

cor.p4 <- eco_poly_basemap +
  coord_map(xlim = c(min(tempDat$newX - .5), max(tempDat$newX +5)),
            ylim = c(temp-2, temp +2))+
  geom_point(data=tempDat, aes(x=newX, y=temp, color = newY))+
  scale_color_viridis_c()

cor.p4

ggsave(filename = paste0(
  figDissDir,
  "/interp_FI_singlePair_corPlot_",
  unique(cor.interp.results$direction),
  ".png"
),
plot = cor.p3)

ggsave(filename = paste0(
  figDissDir,
  "/interp_FI_singlePair_corPlot_",
  unique(cor.interp.results$direction),
  ".png"
),
plot = cor.p3)


  
