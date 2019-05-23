# Setup -------------------------------------------------------------------
## Clear memory
rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

# Source files and functions ----------------------------------------------
## Source the script that returns the obkect `feathers.subset`, whcih si the BBS data + sampling gridded subsetted data...
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_getMungeBBSdata.R"))
    ## This ^ will return a few objects:
    ### 1. bbsData.forAnalysis - containes the subsetted data and munged species/aou and body masses. This df also includes presence absence data for 3-year aggregates
    ### 2. grassSpecies - some grassland obligate spp of interest
    ### 3. routes_gridList - the grid design assosciation with rowID and colID on the bbsData.forAnalysis

## Source the helper functions
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_helperFunctions.R"))

## Source helper funs for plotting spatial data
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R")


# Define, create directories -------------------------------------
## discontinuity results location
reultsDir <- here::here("chapterFiles/discontinuityAnalysis/results/")

## figures
figDirTemp <- here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <- here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(figDirTemp))

# Merge results and bbs route and other information -----------------------------------------------------------
# Import the discontinuity analysis results
gaps <- loadResultsDiscont()

# Join the results with the locations of the BBS routes
gaps<- 
  left_join(gaps, bbsData.forAnalysis)


# Species turnover within locations  ----------------------------------------------------------------------
## Get lag-1 year turnover  
turnover <- bbsData.forAnalysis %>% 
    group_by(countrynum, statenum, route, year) %>% 
    summarise(nSpp=n_distinct(species)) %>%  
    spread(key="year", value="nSpp") %>% 
    gather(key="year", value="nSpp", -countrynum, -statenum, -route) %>% 
    group_by(countrynum, statenum, route) %>%
    mutate(nSppDiff = nSpp-lag(nSpp), 
           year = as.integer(year)) %>%
    ungroup()
head(turnover)

## Histogram  - distribution of turnover ranges
par(mfrow=c(1,2))
hist(turnover$nSpp)
hist(turnover$nSppDiff)
par(mfrow=c(1,1))

## Lineplot of mean turnover and lag-1 turnover across routes
temp <- turnover %>%
    group_by(year) %>% 
    summarise(mean = mean(nSppDiff, na.rm=TRUE), 
              sd = sd(nSppDiff, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(
        upper    = 1.96*sd + mean,
        lower    = mean - 1.96*sd) 


p.turn <- ggplot(data=temp)+
    geom_line(aes(x=year, y=mean))+
    ylab(expression(mu*" annual species turnover (\u00B1% CI)"))+
    # ylab(expression(mu*" annual species turnover (?95% CI)"))+
    geom_ribbon(aes(x = year, ymax = lower, ymin = upper), alpha=0.30)+
    theme_Publication()
p.turn
# saveFig(p=p.turn, fn = "meanAnnualTurnover")


# # Run gam on species turnover ---------------------------------------------
# require(mgcv)
# ## run a gam on species richness over time 
# turnover.gam <- mgcv::gam(data = turnover,
#                     formula = nSpp ~  s(year, by = route))
# summary(turnover.gam)
# 
# plot(turnover.gam)
# 
# 
# # Run gam on stopTotal.3year grass species ----------------------------------------------
# temp <- bbsData.forAnalysis %>% filter(aou %in% unique(grassSpecies$aou)) %>% 
#                 mutate(commonName = as.factor(commonName))
# 
# grass.gam <- mgcv::gam(data = temp,
#                             formula = stoptotal  ~  factor(route) + s(year, by = commonName))
# gam.check(grass.gam)
# summary(grass.gam)
# plot(grass.gam)
# 
# 
# spp <- unique(grassSpecies$commonName)[10]
# dat <- temp %>% filter(commonName == spp)
# if(nrow(dat)>0){
#   if(length(unique(dat$route))>1) gam <- mgcv::gam(data=dat,
#                              formula = stoptotal  ~  factor(route) + s(year)) 
#   gam <- mgcv::gam(data=dat,
#                       formula = stoptotal  ~   s(year))
# gam.check(gam)
# plot(gam, main=spp)
#   # par(mfrow=c(1,2))
# # plot(gam,main=paste0(spp, "\n\nw/route"));plot(gam2,main=paste0(spp, "\n\nno route effect"))
# }else(dev.off())
# 
# 
# Plot the discontinuity results ------------------------------------------

thresh=0.95 # define threshold
year.ind <- c(1975, 2015)
for(i in 1:unique(gaps$route)){
route.ind <- unique(gaps$route)[i]
temp <- gaps %>% 
  filter(countrynum ==840, statenum == 38, 
         route==route.ind
         # , year==2015
         ) %>% 
  group_by(year) %>% 
  arrange(log10.mass) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  mutate(isGap = ifelse(gap.percentile>=thresh, "yes","no"))

p1 <-
ggplot(data=temp %>% filter(year == year.ind[1]), aes(x=rank, y = log10.mass))+
    geom_point(aes(color=isGap, shape=isGap))+
    scale_color_manual(values=c("yes"="red","no"="black"))+
    scale_shape_manual(values=c("yes"=17,"no"=16))+
    theme_bw()+
    ylab("log mass")+
    xlab("")+
    labs(caption = "")

p2 <-
  ggplot(data=temp %>% filter(year == year.ind[2]), aes(x=rank, y = log10.mass))+
  geom_point(aes(color=isGap, shape=isGap))+
  scale_color_manual(values=c("yes"="red","no"="black"))+
  scale_shape_manual(values=c("yes"=17,"no"=16))+
  theme_bw()+
  ylab("log mass")+
  xlab("rank order")+
  labs(caption = "")


prow <-
  cowplot::plot_grid(p1+ theme(legend.position="none"), 
                        p2+ theme(legend.position="none"), 
                        ncol=1, 
                        labels = c("1975","2015"), 
                        hjust = -1.3, vjust=1.8)
# labels = "AUTO")

legend_b <- get_legend(p2 + theme(legend.position="bottom", 
                                  legend.spacing.x = unit(.3, 'cm')
                                  ))

p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
fn <- paste0("ddBothYears_route", route.ind)
saveFig(p = p, fn=fn, dir=figDirTemp)
}

