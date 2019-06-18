# Setup -------------------------------------------------------------------
## Clear memory
rm(list=ls())

## Load pkgs
library(cowplot)
library(tidyverse)

# Source files and functions ----------------------------------------------
## Source the script that returns the object `feathers.subset`, whcih si the BBS data + sampling gridded subsetted data...
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_getMungeBBSdata.R"))
    ## This ^ will return a few objects:
    ### 1. bbsData.forAnalysis - containes the subsetted data and munged species/aou and body masses. This df also includes presence absence data for 3-year aggregates
    ### 2. grassSpecies - some grassland obligate spp of interest
### 3. routes_gridList - the grid design assosciation with rowID and colID on the bbsData.forAnalysis
### 4. decliningSpecies - species wtih sign. declining trends from 1966-2015 with HIGH (blue) credibility ratings according to bbs (https://www.mbr-pwrc.usgs.gov/cgi-bin/atlasa15.pl?KAN&2&15&csrfmiddlewaretoken=3YKakk7LxT2ki6NSpl4mstudYCqdW02C)

## Source the helper functions
source(here::here("/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_helperFunctions.R"))

## Source helper funs for plotting spatial data
source(here::here("/chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R"))

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
gaps <- 
  left_join(gaps, bbsData.forAnalysis) 


## Load the GRI 'constant power table'
pwr <- read_csv(here::here("chapterFiles/discontinuityAnalysis/griConstantPowerTable.csv"))

## Use linear apprxoimation to get richness for every integer between 20 and 300
pwr.approx <- approx(x=pwr$richness,y=pwr$threshold, xout = seq(from = 20,to= 300)) %>% 
  as_tibble() %>% 
  rename(richness=x, powerConstant =y)


## Append species richenss to gap data
gaps.bbs <- gaps %>% 
  group_by(countrynum, statenum, route, year) %>% 
  mutate(richness=n_distinct(species)) %>% ungroup() %>% 
  left_join(pwr.approx) %>% 
  ungroup() %>% 
## Add new column for GRI constant pwoer threshold level%>% 
  mutate(isGap.powerConstant = ifelse(gap.percentile >= powerConstant, "yes","no"))%>% 
  mutate(isGap.percentile = ifelse(gap.percentile >= 0.90, "yes","no"))

# Species turnover within locations  ----------------------------------------------------------------------
## Get lag-1 year turnover  
# turnover <- bbsData.forAnalysis %>% 
#     group_by(countrynum, statenum, route, year) %>% 
#     summarise(nSpp=n_distinct(scientificName)) %>%  
#     spread(key="year", value="nSpp") %>% 
#     gather(key="year", value="nSpp", -countrynum, -statenum, -route) %>% 
#     # group_by(countrynum, statenum, route, year) %>%
#     mutate(nSppDiff = nSpp-lag(nSpp), 
#            year = as.integer(year)) %>%
#     ungroup()


## Histogram  - distribution of turnover ranges
# par(mfrow=c(1,2))
# hist(turnover$nSpp, xlab="species richness", main="")
# hist(turnover$nSppDiff, xlab="turnover", main="")
# par(mfrow=c(1,1))

## Lineplot of mean turnover and lag-1 turnover across routes
# temp <- turnover %>%
#     group_by(year) %>% 
#     summarise(mean = mean(nSppDiff, na.rm=TRUE), 
#               sd = sd(nSppDiff, na.rm=TRUE)) %>% 
#     ungroup() %>% 
#     mutate(
#         upper    = 1.96*sd + mean,
#         lower    = mean - 1.96*sd) 

# 
# p.turn <- ggplot(data=temp)+
#     geom_line(aes(x=year, y=mean))+
#     ylab(expression(mu*" annual species turnover (\u00B1% CI)"))+
#     # ylab(expression(mu*" annual species turnover (?95% CI)"))+
#     geom_ribbon(aes(x = year, ymax = lower, ymin = upper), alpha=0.30)+
#     theme_Publication()
# p.turn
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
# require(mgcv)
# temp <- bbsData.forAnalysis %>% filter(aou %in% unique(grassSpecies$aou)) %>%
#                 mutate(commonName = as.factor(commonName))
# 
# grass.gam <- mgcv::gam(data = temp,
#                             formula = stoptotal  ~  commonName + s(year ,by=route))
# gam.check(grass.gam)
# summary(grass.gam)
# plot(grass.gam)
# 
# 
# spp <- unique(grassSpecies$commonName)[4]
# dat <- temp %>% filter(commonName == spp)
# if(nrow(dat)>0){
# gam <- mgcv::gam(data=dat,
#                       formula = stoptotal  ~   s(year, by=route))
# gam.check(gam)
# summary(gam)
# plot(gam, main=spp)
# }


# Plot the discontinuity results ------------------------------------------
thresh=0.90 # define threshold
year.ind <- unique(gaps.bbs$year)
# gap.stat <- "isGap.powerConstant" ## whihc gap stat to plot
gap.stat <- "isGap.percentile" ## whihc gap stat to plot

for(j in seq_along(unique(gaps.bbs$statenum))){
state.ind = unique(gaps.bbs$statenum)[j]
temp1 <- gaps.bbs %>% 
  filter(year %in% year.ind, 
         statenum ==state.ind) 

for(i in seq_along(unique(temp1$route))){
route.ind <- unique(temp1$route)[i]
temp <- temp1 %>% 
  filter(countrynum ==840, 
         route==route.ind) %>% 
  group_by(year,countrynum, statenum, route) %>% 
  arrange(year, countrynum, statenum, route, log10.mass) %>% 
  mutate(rank = 1:n(), 
         edgeSpp  = ifelse(lag(!!sym(gap.stat))=="yes"| !!sym(gap.stat) == "yes" , "yes", "no"), 
         edgeSpp  = ifelse(log10.mass == min(log10.mass) | log10.mass == max(log10.mass),"yes" , edgeSpp)
  ) %>% 
 ungroup() 
# View(temp)
## Define xlim for aligning the plot_grid plots
ylims <- c(min(temp$log10.mass), max(temp$log10.mass))

p1 <-
ggplot(data=temp %>% filter(year == year.ind[1]), aes(x=rank, y = log10.mass))+
    geom_point(aes(color=edgeSpp, shape=edgeSpp))+
    scale_color_manual(values=c("yes"="red","no"="black"))+
    scale_shape_manual(values=c("yes"=17,"no"=16))+
    theme_bw()+
    ylab("")+
    xlab("")+
  ylim(c(ylims))+
  labs(caption = "")
# p1
p2 <-
  ggplot(data=temp %>% filter(year == year.ind[2]), aes(x=rank, y = log10.mass))+
  geom_point(aes(color=edgeSpp, shape=edgeSpp))+
  scale_color_manual(values=c("yes"="red","no"="black"))+
  scale_shape_manual(values=c("yes"=17,"no"=16))+
  theme_bw()+
  ylab("")+
  xlab("")+
  ylim(c(ylims))+
  labs(caption = "")
# p2

p3 <-
  ggplot(data=temp %>% filter(year == year.ind[3]), aes(x=rank, y = log10.mass))+
  geom_point(aes(color=edgeSpp, shape=edgeSpp))+
  scale_color_manual(values=c("yes"="red","no"="black"))+
  scale_shape_manual(values=c("yes"=17,"no"=16))+
  theme_bw()+
  ylim(c(ylims))+
  ylab("")+
  xlab("")+
  labs(caption = "")

p4 <-
  ggplot(data=temp %>% filter(year == year.ind[3]), aes(x=rank, y = log10.mass))+
  geom_point(aes(color=edgeSpp, shape=edgeSpp))+
  scale_color_manual(values=c("yes"="red","no"="black"))+
  scale_shape_manual(values=c("yes"=17,"no"=16))+
  theme_bw()+
  ylim(c(ylims))+
  ylab("")+
  xlab("")+
  labs(caption = "")


require(grid)
require(gridExtra)
prow <-
  cowplot::plot_grid(p1+ theme(legend.position="none"), 
                      p2+ theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     p4 + theme(legend.position="none"),
                        ncol=2,
                        labels = c(year.ind), 
                        hjust = -1.3, vjust=1.8, 
                     align="v")


y.grob <- textGrob("log mass", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12), rot=90)

x.grob <- textGrob("rank", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12))

p <- plot_grid(prow,
                # legend_b, 
                ncol = 1, rel_heights = c(1, .4))

#add to plot
p <- grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob))


fn <- paste0(gap.stat, "_state", state.ind, "_rte", route.ind )
saveFig(p = p, fn=fn, dir=figDirTemp)

## Add grassland obligate species symbols to the plots
  test <- temp %>% filter(aou %in% grassSpecies$aou) %>% 
    dplyr::select(year, commonName, aou, log10.mass, rank)
  
  prow <-
    cowplot::plot_grid(addGrassSppLabels(p1) + theme(legend.position="none"), 
                       addGrassSppLabels(p2) + theme(legend.position="none"),
                       addGrassSppLabels(p3)  + theme(legend.position="none"),
                       addGrassSppLabels(p4)  + theme(legend.position="none"),
                       ncol=2,
                       labels = c(year.ind), 
                       hjust = -1.3, vjust=1.8, 
                       align="v")
  
  
  y.grob <- textGrob("log mass", 
                     gp=gpar(fontface="bold", col="blue", fontsize=12), rot=90)
  
  x.grob <- textGrob("rank", 
                     gp=gpar(fontface="bold", col="blue", fontsize=12))
  
  p <- plot_grid(prow,
                 # legend_b, 
                 ncol = 1, rel_heights = c(1, .4))
  
  #add to plot
  p <- grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob))
  
  
  fn <- paste0(gap.stat, "_withGrassObligates_state", state.ind, "_rte", route.ind )
  saveFig(p = p, fn=fn, dir=figDirTemp)
  rm(p, p1,p2,p3,p4,prow)
  
  
}
}

# Analysis based on Roberts et al.spatial regimes shift paper -------------------------------
## year and location of their proposed regime shifts
rs.loc <- data.frame(year = as.factor(c(1970,1985, 2000, 2015)), 
                     lat = c(39, 39.5, 40, 40.5))


rtesOfInterest <- paste0("840_38_", c(25,28,29,31))




select.gaps.bbs <- gaps.bbs %>% 
  filter(loc %in% rtesOfInterest) %>%
  group_by(year,countrynum, statenum, route) %>% 
  arrange(year, countrynum, statenum, route, log10.mass) %>% 
  mutate(rank = 1:n(), 
         edgeSpp  = ifelse(lag(isGap.percentile)=="yes"| isGap.percentile == "yes" , "yes", "no"), 
         edgeSpp  = ifelse(log10.mass == min(log10.mass) | log10.mass == max(log10.mass),"yes" , edgeSpp)
  ) %>% 
  ungroup() 


### Identify the aggregation numbers 
loc.ind <- unique(select.gaps.bbs$loc)
for (i in seq_along(loc.ind)) {
if(i==1) results<- NULL
  ## go along each country/state/route location
  temp1 <- select.gaps.bbs %>%
    filter(loc == loc.ind[i])
  
  year.ind <- unique(temp1$year)

    df.out <- NULL
  for (h in seq_along(year.ind)) {
    temp <-  temp1 %>%
      filter(year == year.ind[h])
    
    # setup for j loop
    x = temp$edgeSpp
    agg.vec  = rep(1, length.out = length(x))
    counter <- 1
    
    ## Create a vector of aggregation numbers for all rows in temp 
    for (j in 2:length(x)) {
      # browser()
      agg.vec[j] <- counter
      
      if(j!= length(x) & x[j] == "yes" &  x[j + 1] == "yes") counter <- counter + 1
      
      if(j==length(x)) agg.vec[j] = counter  # ensure the last one takes on current counter...
      } # end j loop
    
    temp$aggNumber = agg.vec
    df.out <- bind_rows(temp, df.out)  
  } # end h loop
  
  results <- bind_rows(results, df.out)  
  
  } # end i-loop
   
if(nrow(select.gaps.bbs)!=nrow(results))stop("results arent same size as selectgapsbbs")

results <- results %>%
  group_by(year, loc, aggNumber) %>% 
  mutate(
    distEdge.left = abs(log10.mass - min(log10.mass)), 
    distEdge.right = abs(log10.mass - max(log10.mass))
        ) %>% 
  ungroup() %>% 
  mutate(distEdge = ifelse(distEdge.left < distEdge.right, distEdge.left, distEdge.right)) %>% 
  group_by(year, countrynum, statenum, route) %>% 
  mutate(nAggs = n_distinct(aggNumber), 
         nSpp  = n_distinct(scientificName)
         ) %>% 
  ungroup() 



# PLot # aggs in each route/year ------------------------------------------
ggplot(results %>% distinct(loc, year, nAggs))+
  geom_boxplot(aes(x=factor(year), y = nAggs))+
  facet_wrap(~loc)

ggplot(results %>% filter(isGap.percentile=="yes"))+
  geom_point(aes(x=year, y = log10.mass,color=loc))


# Plot distance to edge  ----------------------------------------------
require(ggridges)
results <- results %>% mutate(year = as.factor(year))

ggplot(results, aes(x = log10.mass, y = year, group = year))+
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges()+
  facet_wrap(~loc)+
  labs(title="density mass by route")

(p <- ggplot(results, aes(x = distEdge, y = year, group = year))+
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges()+
  facet_wrap(~loc)+
  xlab("distance to edge")+ylab("year")
)
saveFig(p,fn="distEdgePerRouteYear",dir = figDirTemp)

(p<-ggplot(results)+
  geom_density_ridges(aes(x = distEdge, y = year, group = year, fill=paste(year,factor(edgeSpp))),
                      alpha = .8, color = "white")+
  theme_ridges()+
  facet_wrap(~loc)+
  xlab("distance to edge")+ylab("year")
)
saveFig(p,fn="distEdgePerRouteYear2",dir = figDirTemp)

(p<-ggplot(results %>% filter(aou %in% grassSpecies$aou))+
  geom_line(aes(x = as.integer(as.character(year)), y = nSpp, color=loc),show.legend=FALSE)+
  xlab("year")+ylab("spp richness")+ggtitle("richness per route") 
)
saveFig(p,fn="richnessPerRoute",dir = figDirTemp)



# Get results for all the routes ------------------------------------------
all.gaps.bbs <- gaps.bbs %>% 
  group_by(year,countrynum, statenum, route) %>% 
  arrange(year, countrynum, statenum, route, log10.mass) %>% 
  mutate(rank = 1:n(), 
         edgeSpp  = ifelse(lag(isGap.percentile)=="yes"| isGap.percentile == "yes" , "yes", "no"), 
         edgeSpp  = ifelse(log10.mass == min(log10.mass) | log10.mass == max(log10.mass),"yes" , edgeSpp)
  ) %>% 
  ungroup() 

### Identify the aggregation numbers 
loc.ind <- unique(all.gaps.bbs$loc)
for (i in seq_along(loc.ind)) {
if(i==1) results <- NULL
  ## go along each country/state/route location
  temp1 <- all.gaps.bbs %>%
    filter(loc == loc.ind[i])
  
  year.ind <- unique(temp1$year)
  
  df.out <- NULL
  for (h in seq_along(year.ind)) {
    temp <-  temp1 %>%
      filter(year == year.ind[h])
    
    # setup for j loop
    x = temp$edgeSpp
    agg.vec  = rep(1, length.out = length(x))
    counter <- 1
    
    ## Create a vector of aggregation numbers for all rows in temp 
    for (j in 2:length(x)) {
      # browser()
      agg.vec[j] <- counter
      
      if(j!= length(x) & x[j] == "yes" &  x[j + 1] == "yes") counter <- counter + 1
      
      if(j==length(x)) agg.vec[j] = counter  # ensure the last one takes on current counter...
    } # end j loop
    
    temp$aggNumber = agg.vec
    df.out <- bind_rows(temp, df.out)  
  } # end h loop
  
  results <- bind_rows(results, df.out)  
  
} # end i-loop

if(nrow(all.gaps.bbs)!=nrow(results))stop("results arent same size as selectgapsbbs")

results <- results %>%
  group_by(year, loc, aggNumber) %>% 
  mutate(
    distEdge.left = abs(log10.mass - min(log10.mass)), 
    distEdge.right = abs(log10.mass - max(log10.mass))
  ) %>% 
  ungroup() %>% 
  mutate(distEdge = ifelse(distEdge.left < distEdge.right, distEdge.left, distEdge.right)) %>% 
  group_by(year, countrynum, statenum, route) %>% 
  mutate(nAggs = n_distinct(aggNumber), 
         nSpp  = n_distinct(scientificName)
  ) %>% 
  ungroup() %>% 
  mutate(
    rs.1970 = ifelse(lat < rs.loc$lat[1], "below","above"), 
      rs.1985 = ifelse(lat < rs.loc$lat[2], "below","above"), 
      rs.2000 = ifelse(lat < rs.loc$lat[3], "below","above"), 
      rs.2015 = ifelse(lat < rs.loc$lat[4], "below","above"),
    
    rs.all = ifelse((rs.1970==rs.1985) & (rs.2000==rs.1985) & (rs.2000==rs.2015), rs.1970, "different" )
    )

declining.results <- results %>% 
  filter(commonName %in% decliningSpecies$commonName)

grass.results <- results %>% 
  filter(commonName %in% grassSpecies$commonName)

grassDeclining.results <- results %>% 
  filter(commonName %in% grassSpecies$commonName, 
         commonName %in% decliningSpecies$commonName)



(p1 <- ggplot(declining.results, aes(x=factor(year), y =distEdge, color = ))+ 
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Declining species in all routes"))
saveFig(p=p1, fn = "distEdge_decliningSpp_allRoutes", dir = figDirTemp)

(p2 <- ggplot(declining.results %>% filter(loc %in% rtesOfInterest), aes(x=factor(year), y =distEdge))+#, color=log10.mass))+ 
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Declining species in select routes"))
saveFig(p=p2, fn = "distEdge_decliningSpp_selectRoutes", dir = figDirTemp)

(p11 <- ggplot(grass.results, aes(x=factor(year), y =distEdge))+ 
    geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Grassland obligates in all routes"))
saveFig(p=p11, fn = "distEdge_grassSpp_allRoutes", dir = figDirTemp)

(p22 <- ggplot(grass.results %>% filter(loc %in% rtesOfInterest), aes(x=factor(year), y =distEdge))+#, color=log10.mass))+ 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Grassland obligates in select routes"))
saveFig(p=p22, fn = "distEdge_grassSpp_selectRoutes", dir = figDirTemp)

(p111 <- ggplot(grassDeclining.results, aes(x=factor(commonName), y =distEdge, color=factor(year)))+ 
    geom_point(outlier.shape = 2) + 
    geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Declining grassland obligates in all routes"))
saveFig(p=p111, fn = "distEdge_grassDeclinSpp_allRoutes", dir = figDirTemp)

(p222 <- ggplot(grassDeclining.results %>% filter(loc %in% rtesOfInterest), aes(x=factor(commonName), y =distEdge, color=factor(year)))+#, color=log10.mass))+ 
    geom_boxplot(outlier.shape = 2) + 
    # geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("Declining grassland obligates in select routes"))
saveFig(p=p222, fn = "distEdge_grassDeclinSpp_selectRoutes", dir = figDirTemp)

(p3 <- ggplot(results %>% filter(loc %in% rtesOfInterest), aes(x=factor(year), y =distEdge))+#, color=log10.mass))+ 
    geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("All species in select routes"))
saveFig(p=p3, fn = "distEdge_allSpp_selectRoutes", dir = figDirTemp)

(p33 <- ggplot(results %>% filter(loc %in% rtesOfInterest) %>% mutate(is.grass = ifelse(commonName %in% grassSpecies$commonName,"yes","no")), aes(x=factor(year), y =distEdge, color=is.grass))+
    geom_boxplot(outlier.shape = NA) + #geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("All species in select routes"))
saveFig(p=p33, fn = "distEdge_allSpp_selectRoutes_grassYN", dir = figDirTemp)

(p33 <- ggplot(results %>% filter(loc %in% rtesOfInterest) %>% mutate(is.declining= ifelse(commonName %in% decliningSpecies$commonName,"yes","no")), aes(x=factor(year), y =distEdge, color=is.declining))+
    geom_boxplot(outlier.shape = NA) + #geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("All species in select routes"))
saveFig(p=p33, fn = "distEdge_allSpp_selectRoutes_decliningYN", dir = figDirTemp)


(p4 <- ggplot(results, aes(x=factor(year), y =distEdge))+ 
    geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)+
    ylab("distance to edge")+xlab("year")+ggtitle("All species in all routes"))
saveFig(p=p4, fn = "distEdge_allSpp_allRoutes", dir = figDirTemp)



# Correlation of distance to edge with body mass --------------------------
cor(results$distEdge, results$log10.mass, method="pearson")
cor(declining.results$distEdge, declining.results$log10.mass, method="pearson")
cor(grass.results$distEdge, grass.results$log10.mass, method="pearson")
cor(grassDeclining.results$distEdge, grassDeclining.results$log10.mass, method="pearson")


# END RUN -----------------------------------------------------------------


