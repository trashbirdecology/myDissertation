# # Chunk defaults
set.seed(123)
# knitr::opts_chunk$set(
# cache = TRUE, cache.path = "_cache/",cache.lazy = FALSE,  message = FALSE, warning=FALSE, eval=TRUE, echo=FALSE, error=FALSE
# )
## load pkgs
library(gridExtra)
# library(magrittr)
library(tidyverse)

figDir<-(here::here("chapterFiles/velocity/figsCalledInDiss"))


## source code for theme_Publications
source(here::here("chapterFiles/velocity/06-chap-velocity_helperFuns.R"))

# Source some helper funs
source("./chapterFiles/velocity/06-chap-velocity_helperFuns.R")

## Define a plotting function for plotting simulated and dist results
sPlot <- function(df, dist, div = 10){
  if("sortVar" %in% names(dist))dist <- dist %>% rename(t = sortVar)
  ggplot()+
    geom_line(data = df, aes(t, value, linetype=variable), color='black')+
    geom_line(data = dist, aes(x = t, y = s/div), color = "red", size =1)+
    scale_y_continuous(sec.axis = sec_axis(~.*div, name = "s"))+
    theme_bw()+
    theme(legend.position = "bottom")}

dsdtPlot <- function(df, dist, div = 10){
  if("sortVar" %in% names(dist))dist <- dist %>% rename(t = sortVar)
  ggplot()+
    geom_line(data = df, aes(t, value, linetype=variable), color='black')+
    geom_line(data = dist, aes(x = t, y = dsdt/div), color = "red", size =1)+
    scale_y_continuous(sec.axis = sec_axis(~.*div, name = "v"))+ 
    theme_bw()+
    theme(legend.position = "bottom")}




# pltos -------------------------------------------------------------------

x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean =100, sd = 5, n=50))
x_2 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean =100, sd = 5, n=50))
t = 1:length(x_1)
df.wide = data.frame(t, x_1, x_2) 
df  = data.frame(t, x_1, x_2) %>% 
  tidyr::gather(key = "variable", value = "value", -t) 

dist <- regimeDetectionMeasures::calculate_distanceTravelled(df %>% mutate(cellID = 1) %>% rename(sortVar = t)) %>% rename(t = sortVar) %>% 
  dplyr::select(-cellID)

# join results with the orig data
df.wide <- left_join(df.wide, dist) %>% 
  # add column for change in variable
  mutate(dx_1 = x_1-lag(x_1),
         dx_2 = x_2-lag(x_2), 
         dt = t - lag(t)) %>%
  dplyr::select(t, x_1, x_2, dx_1, dx_2, dt, everything(), -d2sdt2)

# change names of df.wide for table in latex
colnames(df.wide) <- c("t", 
                       "$x_1$",
                       "$x_2$", 
                       "$\\Delta x_1$",
                       "$\\Delta x_2$",
                       "$\\Delta t$",
                       "$\\sqrt(\\sum_{i=1}^N \\Delta x_i^2) $",
                       "$s$",
                       "$V$")
# "$s = \\sum_{t=1}^j(\\sqrt(\\sum_{i=1}^N(\\Delta x_i^2))) $",
# "$ velocity = \\frac{\\Delta s_{j,j+1}}{\\Delta t}$")

saveRDS(df.wide, file = here::here("/chapterFiles/velocity/dfwide_table.RDS"))

p.sysEx <-ggplot(data =df, aes(x = t, y = value,  linetype=variable, color=variable))+
  geom_line(color="black", size=1)+#data = df, aes(t, value, linetype=variable), color='black')+
  theme(legend.position='bottom')+
  theme_bw()+
  scale_linetype_discrete(
    labels=c(expression(x[1]), expression(x[2])))

saveFig(p.sysEx, fn ="sysEx")



p.sysEx2 <- p.sysEx +
  geom_point(size=4, show.legend = FALSE)+
  xlim(c(1,2))+ylim(c(20,28))+
  scale_color_manual(values=c("black","black"))+
  theme(legend.position = 'bottom')+
  theme_bw()

saveFig(p.sysEx2, fn ="sysEx2")


temp <- df %>%
  group_by(variable) %>% 
  arrange(t) %>% 
  mutate(dx2 = ((value)-lag(value))^2) %>% 
  ungroup() %>% 
  group_by(t) %>% 
  mutate(ds = sqrt(sum(dx2))) %>% 
  distinct(t, ds) %>% 
  ungroup() %>%
  arrange(t)
temp[1,"ds"]=0
temp <- temp %>% 
  mutate(s = cumsum(ds))

sysExs<-ggplot(temp, aes(x=t, y=s))+
  geom_line(size=1)+
  theme_Publication()+
  geom_line(aes(x=50), color="grey40", linetype=2)+
  xlab("t")
saveFig(sysExs, fn ="sysExs")




x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean =100, sd = 5, n=50))
x_2 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean =100, sd = 5, n=50))
t = 1:length(x_1)
df.wide = data.frame(t, x_1, x_2)
df  = data.frame(t, x_1, x_2) %>%
  tidyr::gather(key = "variable", value = "value", -t)

dist <- df %>%
  mutate(cellID = 1) %>%
  rename(sortVar = t) %>%
  regimeDetectionMeasures::calculate_distanceTravelled()

p<- dsdtPlot(dist=dist, df=df, div = 1)+
  ggtitle(
  expression(paste("changing means, constant variance"))
)+
     scale_linetype_discrete(
                          labels=c(expression(x[1]), expression(x[2])))
ggsave(p, filename=paste0(here::here("chapterFiles/velocity/figsCalledInDiss/velocitySysEx1.png")))


p<- ggplot(data = dist.sim %>% filter(sim.num==1, t==51),
       aes(x = as.integer(mean.sim), y = dsdt))+
  geom_line()+
  theme_bw()+
  ylab(expression(paste("velocity, ",italic("v"), " at ", italic("t = 51"))))+
  xlab(expression(paste("total change in ", bar("x")[1], " at ", italic("t = 50"))))
ggsave(p, filename=paste0(figDir,"simVplot1.png"))




# DIST SIM ----------------------------------------------------------------

# files <- list.files("./chapterFiles/velocity/simResults", pattern= "var_x1")
# if(!exists("quickRun")) quickRun <- NULL
# if(exists("files")& !is.null(files)) quickRun=FALSE
# if(quickRun) files <- files[1:500] # when knitting thesis for minor edits...
#
# if(length(files) ==0){
#   var.analy = TRUE
#   source("./chapterFiles/velocity/06-chap-velocity_exampleAnalysis.R")
# }
# if(quickRun){
# dist.sim <- do.call("rbind", lapply(paste0("./chapterFiles/velocity/simResults/",files),
# 		feather::read_feather))
# dist.sim <- saveRDS(dist.sim, "./chapterFiles/velocity/simResults/distSim_varx1.RDS")
# }

# dist.sim2 <- readRDS( "./chapterFiles/velocity/simResults/distSim_varx1.RDS")
# dist.sim.reduced2 <- dist.sim2 %>% filter(t==51) %>%
#          group_by(var.sim) %>%
#          mutate(dsdt.sim.mean  = mean(dsdt),
#                 sd.sim    = sd(dsdt),
#                 lower     = dsdt.sim.mean - 1.96*sd.sim,
#                 upper     = dsdt.sim.mean + 1.96*sd.sim
#                 ) %>%
#          distinct(var.sim, sd.sim, lower, upper, dsdt.sim.mean) %>%
#   ungroup() %>%
#   mutate(var.sim = as.integer(var.sim))

dist.sim.reduced <- dist.sim %>% filter(t==51) %>%
         group_by(mean.sim) %>%
         mutate(dsdt.sim.mean  = mean(dsdt),
                sd.sim    = sd(dsdt),
                lower     = dsdt.sim.mean - 1.96*sd.sim,
                upper     = dsdt.sim.mean + 1.96*sd.sim
                ) %>%
         distinct(mean.sim, sd.sim, lower, upper, dsdt.sim.mean) %>%
  ungroup() %>%
  mutate(mean.sim = as.integer(mean.sim))

p<- ggplot(data = dist.sim.reduced)+
 geom_ribbon(aes(x = mean.sim, ymin = lower , ymax = upper), fill = "grey70") +
    geom_line(aes(x = mean.sim,  y = dsdt.sim.mean))+
    theme_bw()+
  ylab(expression(paste("velocity, ",italic("v"), " at ", italic("t = 51"))))+
  xlab(expression(paste("total change in ", bar("x")[1], " between ", italic("t = 50"), " and ", italic("t = 51"))))+
  ggtitle(expression(paste("Mean ",  italic("v")," (±2SD) over ", 10,000, " iterations")))
ggsave(p, filename=paste0(figDir,"simVplot2.png"))


# p<- ggplot(data = dist.sim2 %>% filter(sim.num==1, t==51), 
#        aes(x = as.integer(var.sim), y = dsdt))+
#   geom_line()+
#   theme_bw()+
#   ylab(expression(paste("velocity, ",italic("v"), " at ", italic("t = 51"))))+
#   xlab(expression(paste(
#     # italic("W"), 
#     "total change in ", "sigma"[1], " at ", italic("t = 50"))))
# ggsave(p, filename=paste0(figDir,"simVarPlot.png"))


# RUN PALEO DATA PLOTS ----------------------------------------------------

library(tvdiff)
library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Load data
regimeDetectionMeasures::munge_orig_dat()
data("paleo")

# Linear interpolate data 
xout <- seq(min(paleo$sortVar), max(paleo$sortVar), length.out = 700)
paleoInterp <- 
  paleo %>%
  dplyr::select(-cellID, -site) %>% 
  rename(t = sortVar, y = value) %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(
    t = purrr::map(data, ~ approx(.$t, .$y, xout = xout)$x, rule = 1),
    y = purrr::map(data, ~ approx(.$t, .$y, xout = xout)$y, rule = 1)
  ) %>%
  dplyr::select(-data) 

paleoInterp <- paleoInterp %>%
  unnest()


# Calculate distance
paleoDistance <- 
  paleoInterp %>% 
  group_by(variable) %>% 
  mutate(dy = lead(y) - y,
         dt = lead(t) - t) %>% 
  group_by(t) %>% 
  summarize(ds = sqrt(sum(dy^2))) %>% 
  mutate(s = cumsum(ds)) %>% 
  dplyr::select(-ds) %>% 
  na.omit()

# Build dataframe
paleoEx <-
  paleoDistance %>% 
  mutate(dt = lead(t) - t) %>% 
  # Estimate derivative
  mutate(dsdt =
           TVRegDiffR(
             data = s,
             iter = 2e3,
             alph = 100,
             scale = "small",
             ep = 1e-6,
             dx = dt[1]
           )[-1]) %>% 
  # Simple numerical integration
  mutate(pred = s[1] + cumsum(dsdt*dt)) %>% 
  # Collect in long form
  gather(key, value, -t, -dt)



topSpecies <-
  paleo %>%
  group_by(variable) %>%
  summarize(q95 = quantile(value, 0.95)) %>%
  arrange(desc(q95))
topSpecies <- topSpecies[1:10,]
plotData <-
  paleo %>%
  mutate(variable = if_else(variable %in% topSpecies$variable, variable, "Other (n = 99)")) %>%
  mutate(variable = factor(variable, levels = c(sort(topSpecies$variable), "Other (n = 99)"))) %>%
  group_by(sortVar, variable) %>%
  summarize(value = sum(value)) %>%
  mutate(year.plot = -1*sortVar)

# Generate top species abundance plot to see turnover
p <-
  ggplot(data = plotData,
         aes(x = year.plot, y = value, fill = variable)) +
  geom_area() +
  geom_rug(sides = "b") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw()+
  theme(legend.position = "top") +
  labs(x = "years before 1950", y = "relative abundance", fill = "variable")+
  scale_x_reverse()
ggsave(p, filename=paste0(figDir,"paleoTurnover.png"))


# plotData <- regimeDetectionMeasures::calculate_distanceTravelled(paleo) %>% na.omit(d2sdt2) %>%
#   rename(acceleration= d2sdt2,
#          v = dsdt) %>%
#   gather(key ="key", value="value", -cellID,-sortVar) %>%
#   filter(!key %in% c("acceleration","ds"))
#
# p<- ggplot(plotData, aes(x = sortVar*-1, y = value))+
#   geom_line()+
#   ylab("")+
#   facet_wrap(~key, ncol=1, scales="free_y", strip.position="left")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black"))+
#         labs(x = "years before 1950")+
#      scale_x_reverse()+
#   geom_vline(xintercept = 1300, color="red",size=.8, linetype=2)+
#   geom_vline(xintercept = c(1383, 2150, 4800), color="blue",linetype=3, size=.8)
# ggsave(p, filename=paste0(figDir,"paleoVelocity.png"))

# p1=ggplot(plotData %>% filter(sortVar < -5000, key=="v"), aes(x = sortVar*-1, y = value))+
#   geom_line()+
#   ylab("")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black"))+
#         labs(x = "years before 1950")+
#  annotate("text", x=6800, y=0.10, size=4.5, label= expression(bold("")))+ 
#      scale_x_reverse()
# 
# p2=ggplot(plotData%>% filter(sortVar < -1250, sortVar > -2200, key=="v"), aes(x = sortVar*-1, y = value))+
#   geom_line()+
#   ylab("")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black"))+
#         labs(x = "years before 1950")+
#  annotate("text", x=2125, y=0.20, size=4.5, label= expression(bold("")))+ 
#      scale_x_reverse()
# p<- cowplot::plot_grid(p1, p2, ncol=1, labels="AUTO")
# 
# ggsave(p, filename=paste0(figDir,"paleoRegime1and3.png"))

# Plot observed vs predicted
# p=ggplot(paleoEx %>% 
#          filter(key %in% c("s", "pred")), 
#        aes(x = t, y = value, color = key, linetype = key)) +
#   geom_line(size = 1.2) +
#   labs(x = "time",
#        y = "cumulative change in state (s)")
# ggsave(p, filename=paste0(figDir,"paleoObsPred.png"))


# Plot derivative
# p=ggplot(paleoEx %>% 
#          filter(key == "dsdt"), 
#        aes(x = t*-1, y = value)) +
#   geom_point() +
#   labs(x = "time", 
#        y = expression(italic("v")))+
#   scale_x_reverse()

# ggsave(p, filename=paste0(figDir,"paleoV.png"))



# VELOCSYSEX2 -------------------------------------------------------------


x_1 = c(rnorm(mean = 25, sd = 2, n=50), rnorm(mean =100, sd = 10, n=50))
x_2 = c(rnorm(mean = 50, sd = 5, n=50), rnorm(mean =10, sd = 10, n=50))
t = 1:length(x_1)
df  = data.frame(t, x_1, x_2) %>%
  tidyr::gather(key = "variable", value = "value", -t)
dist <- df %>%
  mutate(cellID = 1) %>%
  rename(sortVar = t) %>%
  regimeDetectionMeasures::calculate_distanceTravelled()
p <- dsdtPlot(dist=dist, df=df, div = 1)+
  ggtitle(
    expression(paste("change in mean & variance of x"[1], "and  x"[2]))
  )
ggsave(p, filename=paste0(figDir,"velocSysEx2.png"))



x_1 = c(rnorm(mean = 25, sd = 2, n=50), rnorm(mean =25, sd = 12, n=50))
x_2 = c(rnorm(mean = 50, sd = 5, n=50), rnorm(mean =50, sd = 5, n=50))
t = 1:length(x_1)
df  = data.frame(t, x_1, x_2) %>%
  tidyr::gather(key = "variable", value = "value", -t)
dist <- df %>%
  mutate(cellID = 1) %>%
  rename(sortVar = t) %>%
  regimeDetectionMeasures::calculate_distanceTravelled()
p <- dsdtPlot(dist=dist, df=df, div = 1)+
  ggtitle(
    expression(paste("constant means, increased variance of x"[1]))
  )

ggsave(p, filename=paste0(figDir,"velocSysEx3.png"))



x_1 = c(rnorm(mean = 25, sd = 25, n=50), rnorm(mean =50, sd = 50, n=50))
x_2 = c(rnorm(mean = 15, sd = 15, n=50), rnorm(mean =150, sd = 150, n=50))
t = 1:length(x_1)
df  = data.frame(t, x_1, x_2) %>%
  tidyr::gather(key = "variable", value = "value", -t)
dist <- df %>%
  mutate(cellID = 1) %>%
  rename(sortVar = t) %>%
  regimeDetectionMeasures::calculate_distanceTravelled()
p<- dsdtPlot(dist=dist, df=df, div = 1)+
  ggtitle(
    expression(paste("changing means, variance = mean"))
  )

ggsave(p, filename=paste0(figDir,"velocSysEx4.png"))
