# Init -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(ggthemes)
library(reghelper)
library(nlme)


# META -------------------------------------------------------------------
# About dataset:
## a discontinuity analysis of avian body mass disitributions (using Barichievy et al. 2018 methods)
## testing the hypothesis of some collaborators re: 1) existence of "spatial regimes" and 2) that sensitive (here, grassland obligates and declining birds) birds are closer to the edge of a body mass distribution (i.e. LOWER `distEdge``) in areas undergoing 'regime shifts' (`regime`, `regimeShift`)

# Vars of interest/used in analysis
## `distEdge` = response of interest; a measure
## `distEdge.scaled`` = distEdge when scaled and centered when each `loc` and `year` combination
## `regime` = classified as South or North at each year `year` (by a paper in prep)
## `year` = year
## `is.declining`= LOGICAL. is the species declining (according to BBS pop trend estimates)
## `is.grassland`= LOGICAL. grassland obligate
## `loc` = bbs route location (countrynum_statenum_routenum)

# Other vars of interest
## `regimeShift` =  one of c(south, north, regimeShift). Did the location (`loc`; BBS route) undergo a 'regime shift' according to authors? If so, categorized as regimeShift. If not, categorized as either norht or south regime, corresponding to `regime`


# 0. Load anlaysis data ------------------------------------------------------
dat <-
  readRDS(here::here(
    "/chapterFiles/discontinuityAnalysis/results/datForAnova.RDS"
  )) %>% 
  mutate(regimeShift = as.factor(regimeShift)) %>% 
  mutate(regime = as.factor(regime)) %>%
  # center teh year variable...
  mutate(year.center = year - round(mean(year)))%>% 
  mutate(
    sppGroup = "allOthers",
    sppGroup = ifelse(is.declining=="yes","declining",sppGroup),
    sppGroup = ifelse(is.grassland=="yes","grassObli",sppGroup),
    sppGroup = as.factor(ifelse(is.grassDeclining=="yes","grassDeclining", sppGroup) )
  ) %>%
  # arrange data frame prior to analysis to account for year as a factor and as a rep. measure
  arrange(loc, aou, year)

dat$year.center %>% range()
dat$year        %>% range()

# 1. Munge analysis data -------------------------------------------------
# Reorder factor levels for easy interp
if(levels(dat$regime)[1]=="North") dat$regime = factor(dat$regime,levels(dat$regime)[c(2,1)])
if(!levels(dat$regimeShift)[1]=="no") dat$regimeShift = factor(dat$regimeShift,levels(dat$regimeShift)[c(2,1)])
levels(dat$regimeShift)
levels(dat$regime)

# 2. Source functions --------------------------------------------------------
to.source <- list.files(
  here::here("/chapterFiles/discontinuityAnalysis/"),
  pattern = "helper",
  full.names = TRUE
)
for (i in 1:length(to.source))source(to.source[i])

# 3. Define, create directories -------------------------------------
## discontinuity results location
resultsDir <-
  here::here("chapterFiles/discontinuityAnalysis/results/")

## figures
figDirTemp <-
  here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <-
  here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
tabDir <-
  here::here("chapterFiles/discontinuityAnalysis/tabsCalledInDiss/")
suppressWarnings(dir.create(figDirTemp))
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(tabDir))


# 4. Species trends -------------------------------------------------------
dat <- dat %>% 
  mutate(stoptotal.3year.scaled = scale(stoptotal.3year)) 


hist(dat$stoptotal.3year.scaled)

ggplot(dat, aes(x=year, y = stoptotal.3year.scaled,color=sppGroup))+
  geom_point()+
  geom_smooth()

library(nlme)      


    model <- lme(data = dat, 
    stoptotal.3year.scaled ~ year*sppGroup + bcr,
    random = ~1|loc/aou
    )
  
    summary(model)
    plot(model)
    
    
# 6. Make precitions ------------------------------------------------------
newdat <- expand.grid(year=unique(dat$year), 
                      stoptotal.3year=0, 
                      sppGroup = unique(dat$sppGroup),
                      bcr = unique(dat$bcr)
                      )

predict(model, newdat)


# Plot sum spp totals -----------------------------------------------------

dat2 <- dat %>%
  filter(bcr %in% c(11,19,17,  22)) %>%
  group_by(bcr, year, sppGroup) %>% 
  summarise(nTot = sum(stoptotal), 
            meanTot  = mean(stoptotal), 
            sdTot    = sd(stoptotal)
            ) %>% 
  mutate(
    lwr=meanTot + 1.96*sdTot,
    upper=meanTot- 1.96*sdTot, 
    cv = sdTot/abs(meanTot)*100
  )


bcr.labs <- c(`11` = "Prairie Potholes", `22` = "Eastern Tallgrass Prairie", `19` = "Central Mixed Grass", 
              `18` = "Shortgrass Prairie", `17`= "Badlands & Prairies")

ggplot(dat2, aes( x=year, y = log(nTot), color=sppGroup))+
  geom_smooth()+
  geom_point()+
  facet_wrap(~bcr, 
             labeller = labeller(bcr = bcr.labs), 
             scales="free_y") +
  ylab("sum stop totals (log)")+
  scale_color_colorblind(
    name = "Species group",
    labels = c(
      "All others",
      "Declining",
      "Declining \nGrassland\nObligates",
      "Grassland\nObligates"
    )) 
saveFig(last_plot(), fn="stopTotal-byBCR-logSum", figDir)

ggplot(dat2, aes( x=year, y = meanTot, color=sppGroup))+
  geom_line()+
  facet_wrap(~bcr, 
             labeller = labeller(bcr = bcr.labs)) +
  ylab("average stop total")+
  scale_color_colorblind(
    name = "Species group",
    labels = c(
      "All others",
      "Declining",
      "Declining \nGrassland\nObligates",
      "Grassland\nObligates"
    ))
saveFig(last_plot(), fn="stopTotal-byBCR-mean", figDir)

ggplot(dat2, aes( x=year, y = cv, color=sppGroup))+
  # geom_smooth()+
  geom_point()+
  geom_hline(aes(yintercept=100), color="gray30", linetype=4, size=1.5)+
  facet_wrap(~bcr, 
             labeller = labeller(bcr = bcr.labs)) +
  ylab("% CV (stop total)")+
  scale_color_colorblind(
    name = "Species group",
    labels = c(
      "All others",
      "Declining",
      "Declining \nGrassland\nObligates",
      "Grassland\nObligates"
    ))
saveFig(last_plot(), fn="stopTotal-byBCR-CV", figDir)
  
