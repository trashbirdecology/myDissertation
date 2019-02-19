# install.packages(c("magick", "gganimate", "gifski", "transformr"))
library(transformr)
library(gapminder)
library(magick)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)


# Create temp df for anim plot
temp = results_dist %>% as_data_frame() %>% 
  filter(
          metricType %in% c('s' , 'dsdt'),  
         # metricType == "s", 
         # dirID %in% c(13)) %>% 
         dirID == 13) %>%
  mutate(year = as.integer(year)) %>% 
  na.omit(metricValue)

p <- ggplot(temp, aes(long, metricValue))+ #, color = as.factor(dirID))) +
  geom_line(alpha = 0.7, show.legend = FALSE)+
  theme(legend.position = "bottom")+facet_wrap(~metricType, scales = "free_y", ncol=1) 
# +
#     geom_vline(aes(xintercept = -96.8), color = "grey", linetype = 2)
  
p.anim <- p + 
    transition_time(year)#+
    # ggtitle('Distance travelled for year {frame_time}')


p.anim

anim_save(path = animDir)
