# Init -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(ggthemes)
# install.packages("reghelper")
library(reghelper)


# source some functions
to.source <- list.files(
  here::here("/chapterFiles/discontinuityAnalysis/"),
  pattern = "helper",
  full.names = TRUE
)
for (i in 1:length(to.source))source(to.source[i])

# Set plotting theme
theme_set(theme_Publication())

# 0. Define dirs ----------------------------------------------------------
resultsDir <-
  here::here("chapterFiles/discontinuityAnalysis/results/")
figDirTemp <-
  here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <-
  here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
tabDir <-
  here::here("chapterFiles/discontinuityAnalysis/tabsCalledInDiss/")
suppressWarnings(dir.create(tabDir))

# 1. LOAD the results used to feed lme ---------------------------------------------
## data used to buld model
dat <- readRDS(file=paste0(resultsDir,"/mixedModel-dataUsed.RDS"))


# 2. Calculate additional things re: distance to edge.. -------------------

## Roberts et al calculated
  ## agg size (nAgg) # already calculated
dat <- dat %>% 
  group_by(year, loc, aggNumber) %>% 
  ## agg width (aggWidth)
  mutate(aggWidth = max(log10.mass)- min(log10.mass),
         ## agg location (minimum mass of each agg)
         aggLoc   = min(log10.mass)
         ) %>% 
  ungroup() 

# They remove routes with only a few species.....i won't do that...

# 3. HIerarhical clustering analysis per roberts et al. ------------------------------------------------
library(rioja)

##
clusters <- list()
for(i in seq_along(unique(dat$year))){
  df  <- dat %>% 
    filter(
      year %in% unique(dat$year)[i],
      nSpp >= 40) %>% 
    distinct(loc, aggNumber, aggWidth, aggLoc, lat) %>% 
    arrange(lat, aggNumber)
  
  y.scale=df$lat
  
  mat <- as.matrix(df %>% dplyr::select(-lat, -loc))
  
  row.names(mat) <- df$loc
  
  # get dissimilarity matrix...
  d <- vegan::vegdist(mat, method="bray")
  # Perform hierarch cluster analsysi as they did on the matrix for each year
  clust <- chclust(d, method="coniss")

  ## Let bstick choose # clusters....
  (stick <- bstick(clust))
  summary(stick)
  plot(clust)
  bstick(clust) 
  

  p.clust <- strat.plot(d = mat, clust = clust, yvar=y.scale, y.tks=y.scale, y.rev=FALSE, 
               ytop=max(y.scale), ybottom=min(y.scale),
             plot.line=FALSE, 
             plot.poly=FALSE, 
             plot.bar=TRUE, 
             lwd.bar=8,
             scale.percent=FALSE, 
             xSpace=0.01, x.pc.lab=TRUE, x.pc.omit0=TRUE, 
             las=2)

  
    }

