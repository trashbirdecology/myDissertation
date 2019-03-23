# Source the FUNCTION to run linear interpolation and calculate correlations among interpolated FI results
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_linearInterpolateCorrelation.R")

cor.interp.results <- interpAndCor(results, metricType.ind = "FI_Eqn7.12")

# Plot select transect-pairs ----------------------------------------------
yr.temp <- unique(cor.interp.results$year)[which(unique(cor.interp.results$year)%%10==0)] 
cor.lab <- cor.interp.results %>% filter(year %in% yr.temp) %>%  group_by(tsect.pair, cor.interp, year) %>% 
  summarise(newX = min(newX), newY = max(newY))

cor.p1 <-
  ggplot(data = cor.interp.results %>% filter(year %in% yr.temp), 
       aes(x = newX, y = newY, color = as.factor(dirID)))+
  geom_line()+
  facet_grid(year~tsect.pair, scales = "free_y")+
  theme_bw()+ scale_color_grey()+
  theme.margin+
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "inside")+
geom_text(inherit.aes = FALSE,
  data    = cor.lab,
  mapping = aes(x = -Inf, y = Inf, 
                label = paste0("r^2 ==",cor.interp)), parse = TRUE,
  hjust   = -.1,
  vjust   = 1.5, 
  size = 2)+
  xlab("longitude")+ylab("FI (interpolated)")


ggsave(filename = paste0(
    figDissDir,
    "/interpolated_FI_corplotSelectTransects_",
    unique(cor.interp.results$direction),
    ".png"
  ),
  plot = cor.p1)

cor.p1.log<-
  ggplot(data = cor.interp.results %>% filter(year %in% yr.temp), 
         aes(x = newX, y = log(newY+1), color = as.factor(dirID)))+
  geom_line()+
  facet_grid(year~tsect.pair, scales = "free_y")+
  theme_bw()+ scale_color_grey()+
  theme.margin+
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "inside")+
  geom_text(inherit.aes = FALSE,
            data    = cor.lab,
            mapping = aes(x = -Inf, y = Inf, 
                          label = paste0("r^2 ==",cor.interp)), parse = TRUE,
            hjust   = -.1,
            vjust   = 1.5, 
            size = 2)+
  xlab("longitude")+ylab("log FI (interpolated)")

ggsave(filename = paste0(
  figDissDir,
  "/interpolated_FI_corplotSelectTransects_",
  unique(cor.interp.results$direction),
  "_log.png"
),
plot = cor.p1.log)




# # Plot interpolated results -----------------------------------------------
# # Keep only a few years
# my.years  <-
#   unique(cor.interp.results$year)[which(unique(cor.interp.results$year) %% 10 == 0)]
# 
# interp.line <- ggplot(
#   cor.interp.results %>%
#     filter(year %in% my.years),
#   aes(
#     x = newX,
#     y = newY,
#     group = dirID,
#     color = as.factor(dirID)
#   )
# ) +
#   geom_line() +
#   scale_color_viridis_d() +
#   theme_bw() +
#   theme.margin +
#   theme(legend.position = "bottom") +
#   xlab(paste(ifelse(
#     unique(cor.interp.results$direction) == 'East-West',
#     'longitude',
#     'latitude'
#   ))) +
#   # ylab(paste(unique(cor.interp.results$metricType)))
#   ylab("FI") +
#   guides(color = guide_legend(title = "transect")) +
#   facet_wrap(~ year, scales = "free_y")
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = interp.line)
# 
# 
# ##
# interp.line2 <-
#   ggplot(
#   cor.interp.results %>%
#     filter(year %in% my.years),
#   aes(
#     x = newX,
#     y = newY,
#     group = year,
#     color = as.factor(year)
#   )
# ) +
#   geom_line() +
#   scale_color_grey() +
#   theme_bw() +
#   theme.margin +
#   theme(legend.position = "bottom") +
#   xlab(paste(ifelse(
#     unique(cor.interp.results$direction) == 'East-West',
#     'longitude',
#     'latitude'
#   ))) +
#   # ylab(paste(unique(cor.interp.results$metricType)))
#   ylab("FI") +
#   guides(color = guide_legend(title = "year")) +
#   facet_wrap(~ dirID, scales = "free_y")
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI2",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = interp.line2)
# 
# # Plot correlations -------------------------------------------------------
# cor.plot1 <- ggplot(cor.results %>% filter(cor.interp < -0.5 | cor.interp > 0.5), aes(x = year, y = tsect.pair), group = year)+
#   geom_point(aes(size = cor.interp))+
#   scale_color_viridis_c()+
#   geom_rug(data = cor.results, sides = "b")+
#   ggtitle("correlations < -0.5 and > 0.5")+ ylab("transect-pair")+
#   guides(size = guide_legend(title = "correlation")) +
#   theme_bw()+theme.margin
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI_corplot1_",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = cor.plot1)
# 
# cor.plot2 <- ggplot(cor.results %>% filter( cor.interp < -0.75 | cor.interp > 0.75), aes(x = year, y = tsect.pair), group = year)+
#   geom_point(aes(size = cor.interp))+
#   scale_color_viridis_d()+
#   geom_rug(data = cor.results, sides = "b")+
#   ggtitle("correlations < -0.75 and > 0.75")+
#   ylab("transect-pair")+
#   guides(size = guide_legend(title = "correlation")) +
#   theme_bw()+theme.margin
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI_corplot2_",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = cor.plot2)
# 
# 
# 
# cor.plot3 <- ggplot(cor.results %>%  filter(year == 2010), aes(x = year, y = tsect.pair), group = year)+
#   geom_point(aes( color = cor.interp, size = 1))+
#   scale_color_gradient2( low = "red", mid = "white",
#                      high = "black", midpoint = 0)+
#   theme_bw()+ theme.margin + 
#   ylab("transect-pair") +
#   guides(color = guide_legend(title = "correlation"), 
#          size = guide_legend("none")) 
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI_corplot3_",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = cor.plot3)
# 
# 
# 
# # Single-pair plots -------------------------------------------------------
# year.temp = 2010
# temp1 =   cor.interp.results %>%
#   filter(
#     year %in% year.temp,
#          dirID %in% c(15, 16))
# temp2 =   cor.results %>%
#   filter(
#     year %in% year.temp,
#          tsect.pair %in% c("15:16"))
# lab.x = min(temp1$newX ,na.rm = T)
# lab.y = max(temp1$newY ,na.rm = T)
# 
# corpair.fi.p1 <- ggplot() +
#   geom_line( data = temp1, aes(x = newX, y = newY,  color = as.factor(dirID))) +
#   scale_color_grey() +
#   theme_bw() +
#   annotate("text", x = lab.x, y = lab.y, label = paste0("R = ", round(temp2$cor.interp, 3)), hjust=0)+
#   xlab(paste(ifelse(
#     unique(cor.interp.results$direction) == 'East-West',
#     'longitude',
#     'latitude'
#   ))) +
#   ylab("FI") +
#   guides(color = guide_legend(title = "transect"))+
#   theme.margin +
#   theme(legend.position = "bottom") 
# 
# 
# ggsave(filename = paste0(
#   figDir,
#   "/interpolated_FI_withCor_pairplot_year", year.temp,"_",
#   unique(cor.interp.results$direction),
#   ".png"
# ),
# plot = corpair.fi.p1)
# 
# 

