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

# 1. LOAD ANOVA RESULTS and data ---------------------------------------------
## Load the lme
mod <- readRDS(file=paste0(resultsDir,"/mixedModelResults.RDS"))
## data used to buld model
dat <- readRDS(file=paste0(resultsDir,"/mixedModel-dataUsed.RDS"))

# 2. Model summaries ------------------------------------------------------
summary(mod)
coefficients(mod) %>% glimpse()
mod$coefficients$fixed

# 3. Plot model results using reghelper pkg ------------------------------
#graphing categorical interactions
graph_model(mod,
            y = distEdge.scaled,
            x = sppGroup,
            lines = regime) + scale_color_colorblind() + ylab("distance to edge (scaled)") +
  xlab("") +
  scale_fill_discrete(name = "Regime", labels = c("South", "North")) +
  scale_x_discrete(
    labels = c(
      "All others",
      "Declining",
      "Declining \nGrassland\nObligates",
      "Grassland\nObligates"
    )
  )

saveFig(p=last_plot(), fn="intrxnPlot_regime1", dir=figDir)

graph_model(mod,
            y = distEdge.scaled,
            x = regime,
            lines = sppGroup) +
  ylab("distance to edge (scaled)") +
  xlab("") +
  scale_color_discrete(
    name = "Species group",
    labels = c(
      "All others",
      "Declining",
      "Declining \nGrassland\nObligates",
      "Grassland\nObligates"
    )
  ) +
  scale_color_colorblind()  

saveFig(p=last_plot(), fn="intrxnPlot_regime", dir=figDir)

# MakePredictions/visualize effects -------------------------------------------
newdat <- expand.grid(regime=unique(dat$regime),
                      sppGroup=unique(dat$sppGroup),
                      year.center=unique(dat$year.center), 
                      distEdge.scaled=0)



ggplot(dat, aes(x=regime, y=distEdge.scaled, colour=year.center)) +
  geom_point(size=3, alpha=.2) +
  geom_jitter()+
  geom_line(aes(y=predict(mod), group=regime)) +
  geom_line(data=newdat, aes(y=predict(mod, level=0, newdata=newdat), size="Population"))+
  facet_wrap(~sppGroup)


# 5. Route-level turnover and spp. of interest ----------------------------
names(dat)

dat.summary <- dat %>% 
  group_by(year, loc, sppGroup) %>% 
  summarise(nSppOfInterest = n_distinct(aou))%>%   
  ## add back the spp ricness var
  left_join(dat %>% distinct(year,year.center, loc, nSpp, lat, long, bcr, regime, regimeShift)) 

ggplot(dat.summary, aes(x=year, y = nSppOfInterest,color=sppGroup))+
  geom_smooth()+
  ylab("number of species per route")+
  geom_point(show.legend=FALSE)+
  scale_color_discrete( name = "Species group",
  labels = c(
  "All others",
  "Declining",
  "Declining \nGrassland\nObligates",
  "Grassland\nObligates"))+
  scale_color_colorblind()

saveFig(last_plot(),fn="nSppOfIntTime", figDir)


# library(mgcv)
# gam1 <- mgcv::gam(data = dat.summary, formula =  nSppOfInterest ~ s(lat, long, by=year.center) + sppGroup*regime)
# summary(gam1)
# plot(gam1, rug=T, residuals = T, se = T, all.terms = TRUE)




