# Setup -------------------------------------------------------------------
## Clear memory
rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

## Source helper functions
source(here::here("chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_helperFunctions.R"))

# Define, create directories -------------------------------------
## discontinuity results
reultsDir <- here::here("chapterFiles/discontinuityAnalysis/results/")

## figures
figDirTemp <- here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <- here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(figDirTemp))

# Load Datasets -----------------------------------------------------------
## Import the discontinuity analysis results
gaps <- loadResultsDiscont()

## Load BBS route information 
routeInfo <- bbsRDM::getRouteInfo(routesFile = "routes.zip",
                                  routesDir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/",
                                  RouteTypeID = 1, # one or more of c(1,2,3)
                                  Stratum = NULL,
                                  BCR = NULL) %>% 
  rename(Lat=latitude, Long=longitude) %>% 
  dplyr::select(Lat,Long,countrynum,statenum, route,bcr) %>% 
  mutate(Lat = sprintf("%.5f",round(Lat, 5)),   
         Long = sprintf("%.5f",round(Long, 5) )        
         ) # round the Lat and Long for merging with original data

## Load the data that was analyzed in discontinuity analyses
data <- readRDS(here::here("chapterFiles/discontinuityAnalysis/analyzedData.RDS"))%>% 
  mutate(Lat = sprintf("%.5f",round(Lat,5)),   
         Long = sprintf("%.5f",round(Long, 5)))# round the Lat and Long for merging with route data
## obj `data` is the same as the original righ tnow, but with two additional columns for `loc`=unique lat longs and `nYear`=#years sampled

## merge with BBS route information
data <- left_join(data%>% dplyr::select(loc,Lat,Long,Year), routeInfo)

## Add the BBS route information to the results
memory.limit(4000)
gaps <- full_join(gaps, data)



# View spatial location of locs (sampled pts) -----------------------------
## keep only unique points analyzed
pts <- data %>% distinct(loc,Year, .keep_all=TRUE) 

## Map of sampling locations (unique lat lon pts..) 
ggplot(pts, aes(Long, Lat))+
  geom_point(aes(size=nYear, color=as.factor(loc)), show.legend=FALSE)


# Species turnover within locations  ----------------------------------------------------------------------
## Get lag-1 year turnover  
turnover <- data %>% 
    group_by(loc, Year) %>% 
    summarise(nSpp=n_distinct(species)) %>% 
    dplyr::select(nSpp,loc, Year) %>% 
    spread(key="Year", value="nSpp") %>% 
    gather(key="Year", value="nSpp", -loc) %>% 
    group_by(loc) %>%
    mutate(nSppDiff = nSpp-lag(nSpp), 
           Year = as.integer(Year)) %>%
    ungroup()

## Histogram  - distribution of turnover ranges
par(mfrow=c(1,2))
hist(turnover$nSpp)
hist(turnover$nSppDiff)


## Lineplot - mean turnover and lag-1 turnover
temp <- turnover %>%
    group_by(Year) %>% 
    summarise(mean = mean(nSppDiff, na.rm=TRUE), 
              sd = sd(nSppDiff, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(
        upper    = 1.96*sd + mean,
        lower    = mean - 1.96*sd)   


p.turn <- ggplot(data=temp)+
    geom_line(aes(x=Year, y=mean))+
    ylab(expression(mu*" annual species turnover (\u00B1% CI)"))+
    # ylab(expression(mu*" annual species turnover (?95% CI)"))+
    geom_ribbon(aes(x = Year, ymax = lower, ymin = upper), alpha=0.30)+
    theme_Publication()
p.turn
saveFig(p=p.turn, fn = "meanAnnualTurnover")



# Run gam on species turnover ---------------------------------------------
# require(mgcv)
## run a gam on species richness over time 
# turnover.gam <- gam(data = turnover,
#                     formula = nSpp ~ <-  s(Year, by = loc)))



# Plot the discontinuity results ------------------------------------------

test <- gaps %>% 
  filter(loc==25, Year==2000)

# p3 <- 
  ggplot(data=test, aes(x=rank, y = log10.data))+
    geom_point(aes(color=isGap, shape=isGap))+
    scale_color_manual(values=c("yes"="red","no"="black"))+
    scale_shape_manual(values=c("yes"=17,"no"=16))+
    theme_bw()+
    ylab("log(body mass)g)")+
    xlab("rank order")+
    labs(caption = "")


plot_grid(p1,p2,p3, ncol=1)
