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
(prow <-
  cowplot::plot_grid(p1+ theme(legend.position="none"), 
                      p2+ theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     p4 + theme(legend.position="none"),
                        ncol=2,
                        labels = c(year.ind), 
                        hjust = -1.3, vjust=1.8, 
                     align="v"))


y.grob <- textGrob("log mass", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12), rot=90)

x.grob <- textGrob("rank", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12))

# legend_b <- get_legend(p1 + theme(legend.position="bottom", 
#                                   legend.spacing.y = unit(.3, 'cm')
#                                   ))

(p <- plot_grid(prow,
                # legend_b, 
                ncol = 1, rel_heights = c(1, .4)))


#add to plot
p <- grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob))


fn <- paste0(gap.stat, "_state", state.ind, "_rte", route.ind )
saveFig(p = p, fn=fn, dir=figDirTemp)
rm(p, p1,p2,p3,p4,prow)
}
}

# Analysis based on Roberts et al.spatial regimes shift paper -------------------------------

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
results<- NULL
for (i in seq_along(loc.ind)) {
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
  group_by(year, countrynum, statenum, route, aggNumber) %>% 
  mutate(distEdge = abs(min(log10.mass - min(log10.mass),
                            log10.mass - max(log10.mass)))) %>% 
  group_by(year, countrynum, statenum, route) %>% 
  mutate(nAggs = n_distinct(aggNumber), 
         nSpp  = n_distinct(scientificName)
         ) %>% 
  ungroup()

# visualize species shit --------------------------------------------------
ggplot(data=results %>%  filter(year == 2015, loc== '840_38_29'), 
       aes(x=rank, y = log10.mass))+
  # geom_point(aes(group=as.factor(as.character(aggNumber))), show.legend = FALSE)
  geom_point(aes(color=factor(aggNumber)), show.legend = FALSE)



# PLot # aggs in each route/year ------------------------------------------
ggplot(results %>% distinct(loc, year, nAggs))+
  geom_boxplot(aes(x=factor(year), y = nAggs))+
  facet_wrap(~loc)

ggplot(results %>% filter(isGap.percentile=="yes"))+
  geom_point(aes(x=year, y = log10.mass,color=loc))



# Plot distance to edge crap ----------------------------------------------
require(ggridges)
results <- results %>% mutate(year = as.factor(year))

ggplot(results, aes(x = log10.mass, y = year, group = year))+
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges()+
  facet_wrap(~loc)+
  labs(title="density mass by route")

p<-ggplot(results, aes(x = distEdge, y = year, group = year))+
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges()+
  facet_wrap(~loc)
saveFig(p,fn="distEdgePerRouteYear",dir = figDirTemp)

p<-ggplot(results)+
  geom_density_ridges(aes(x = distEdge, y = year, group = year, fill=paste(year,factor(edgeSpp))),
                      alpha = .8, color = "white")+
  theme_ridges()+
  facet_wrap(~loc)
saveFig(p,fn="distEdgePerRouteYear2",dir = figDirTemp)

p<-ggplot(results %>% filter(aou %in% grassSpecies$aou))+
  geom_line(aes(x = as.integer(as.character(year)), y = nSpp, color=loc),show.legend=FALSE)+
  labs(xlab='year',ylab="spp richness",main='richness per route') 
saveFig(p,fn="richnessPerRoute",dir = figDirTemp)



# END RUN -----------------------------------------------------------------
# select.gaps.bbs %>% 
#   group_by(year, countrynum, statenum, route, aggNumber) %>%
#   mutate(distEdge = if_else(
#     log_avg_wt <= mean(log_avg_wt),
#     abs(min(log_avg_wt) - log_avg_wt),
#     abs(max(log_avg_wt) - log_avg_wt)
#   )) %>%
#   ungroup()