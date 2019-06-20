# Init -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(ggthemes)


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
  mutate(year = as.factor(year)) %>% 
  mutate(regimeShift = as.factor(regimeShift)) %>% 
  mutate(regime = as.factor(regime)) %>%
  mutate(year.int = as.integer(as.character(year)), 
         year.int = year.int - min(year.int))%>% 
  mutate(
    sppGroup = "allOthers",
    sppGroup = ifelse(is.declining=="yes","declining",sppGroup),
    sppGroup = ifelse(is.grassland=="yes","grassObli",sppGroup),
    sppGroup = as.factor(ifelse(is.grassDeclining=="yes","grassDeclining", sppGroup) )
  ) 

glimpse(dat)

# arrange data frame prior to analysis to account for year as a factor and as a rep. measure
dat <- dat %>%
  arrange(loc, aou, year)


# 1. Munge analysis data -------------------------------------------------
# Reorder factor levels for easy interp
levels(dat$regimeShift)
dat$regimeShift = factor(dat$regimeShift,levels(dat$regimeShift)[c(3,2,1)])
levels(dat$regimeShift)

if(levels(dat$regime)[1]=="North") dat$regime = factor(dat$regime,levels(dat$regime)[c(2,1)])

## Set year ind for simple plotting 
year.ind <- c(1970, 1985, 2000, 2015)

## Reduced dataset for simpler plotting
dat2 <- dat %>% filter(year %in% year.ind) %>% 
  mutate(year = droplevels(year)) 

# 2. Source functions --------------------------------------------------------
to.source <- list.files(
  here::here("/chapterFiles/discontinuityAnalysis/"),
  pattern = "helper",
  full.names = TRUE
)
for (i in 1:length(to.source))source(to.source[i])

# 3. Define, create directories -------------------------------------
## discontinuity results location
reultsDir <-
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

################################################################################
## A. DATA EXPLORATION of  response variable
################################################################################
# 4. Check for normality beyond QQ plots and Boxplots ------------------------
# Skewness
PerformanceAnalytics::skewness(dat$distEdge.scaled)
## 0.752 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
# Kurtosis
PerformanceAnalytics::kurtosis(dat$distEdge.scaled)
## 0.177 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....

# 5. Visualize the original and scaled response  ---------------------------------------
h1 <- ggplot(dat, aes(x = distEdge)) +
  geom_histogram()
h2 <- ggplot(dat, aes(x = distEdge.scaled)) +
  geom_histogram()
cowplot::plot_grid(h1, h2)

b1 <- ggplot(dat, aes(y = distEdge)) +
  geom_boxplot()
b2 <- ggplot(dat, aes(y = distEdge.scaled)) +
  geom_boxplot()

cowplot::plot_grid(b1, b2)

## Q-Q plot of orig dat
ggplot(dat, aes(sample = distEdge)) +
  geom_qq_line() + stat_qq() +
  xlab("standard normal quantiles") +
  ylab("data quantiles")
## shit
ggplot(dat, aes(sample = distEdge.scaled)) +
  geom_qq_line() + stat_qq() +
  xlab("standard normal quantiles") +
  ylab("data quantiles")
## better but not amazing



# 6. Visualize dist to edge  --------------------------------------------------------------

## Determine whether we need random intercepts/slopes for loc and aou
ggplot(dat2) +
  geom_boxplot(aes(x = loc, y = distEdge.scaled)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("NABBS route")+ ylab("scaled distance to edge")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  ## eh.. not too much difference among locations....
  saveFig(last_plot(), dir=figDir, fn="randEffect_loc")

ggplot(dat2) +
  geom_boxplot(aes(x = as.factor(aou), y = distEdge.scaled)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("species")+ ylab("scaled distance to edge")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
## eh.. not too much difference among locations....
saveFig(last_plot(), dir=figDir, fn="randEffect_aou")
  ## definitely need to account for AOU/species ID

ggplot(dat2) +
  geom_boxplot(aes(x = sppGroup, y = distEdge.scaled, color = regime)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("species group")+ ylab("scaled distance to edge")+
  theme(axis.text.x = element_text(angle = 90))+
  ggthemes::scale_color_colorblind()
saveFig(last_plot(), dir=figDir, fn="randEffect_sppGroupByRegime")



ggplot(dat2 %>% group_by(year, loc, sppGroup) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y, group=loc), show.legend=F)+
  facet_wrap(~sppGroup)

ggplot(dat2 %>% group_by(year, sppGroup, regime) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y,group= regime), show.legend=F)+
  facet_wrap(~sppGroup)

ggplot(dat2 %>% group_by(year, aou, sppGroup) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y, group=aou), show.legend=F)+
  facet_wrap(~sppGroup)



# 7. Correlation of nAggs with distEdge --------------------------------------
ggplot(dat2, aes(x = nAggs, y = distEdge)) +
  geom_point() +
  facet_wrap(year ~ regime, ncol = 2)+
  geom_smooth(method="lm", se = T)
## unsurprisingly dist edge decreases with # aggs. so that's a good sign.

# 8. Interaction plots -------------------------------------------------------
## Nothing too interesting here..
interaction.plot(response = dat2$distEdge,
                 dat2$regime,
                 dat2$year)
interaction.plot(response = dat2$distEdge,
                 dat2$year,
                 dat2$regime)
interaction.plot(response = dat2$distEdge,
                 dat2$regime,
                 dat2$is.grassland)
## Below, however, we see something goin gon...
saveFig(interaction.plot(response = dat2$distEdge,
                         dat2$year,
                         dat2$regimeShift, xlab = "Year", ylab="Mean distance-to-edge",
                         trace.label = "Regime\n   shift",  # label for legend
                         xpd = FALSE) ,#,  # 'clip' legend at border
        fn="intrxnRegimeShift", dir=figDir)


saveFig(interaction.plot(response = dat2$distEdge,
                         dat2$regimeShift,
                         dat2$is.declining,
                         fun = "mean",
                         xlab="Regime shift",
                         ylab="Mean distance-to-edge",
                         trace.label = "Declining\n   species",  # label for legend
                         xpd = FALSE) ,#,  # 'clip' legend at border
        fn="intrxnRegimeShiftDeclinSpp", dir=figDir)


################################################################################
## REGIME SHIFT lcoations according to roberts et al
################################################################################
# 10. Roberts et al regime shift locations -----------------------------------------------------------------------
## these are the locations and years of the regime shifts as presented in roberts dissertation.
rs.loc <- data.frame(year = as.factor(c(1970, 1985, 2000, 2015)),
                     lat = c(39, 39.5, 40, 40.5))


################################################################################
## ANOVA time
################################################################################
# 11. Ensure factors and best level order for interpretation purposes ----------------------------------------------
dat <- dat %>% 
  mutate(
    sppGroup = "allOthers",
    sppGroup = ifelse(is.declining=="yes","declining",sppGroup),
    sppGroup = ifelse(is.grassland=="yes","grassObli",sppGroup),
    sppGroup = as.factor(ifelse(is.grassDeclining=="yes","grassDeclining", sppGroup) )
  ) 

levels(dat$regime)
levels(dat$is.declining)
levels(dat$is.grassland)
levels(dat$is.grassDeclining)
levels(dat$sppGroup)

# ensure year is a factor
str(dat$year)

# 12. Run ANOVA ---------------------------------------------------------------
library(nlme)
# Set contrasts
options(contrasts = c("contr.sum", "contr.poly")) ## type 3


M.mixed <-
  nlme::lme(
    distEdge ~ 
      regime * is.declining + # declining species X whether the route underwent shift 
      regime * is.grassland + # grassland oblig. species X whether the route underwent shift
      is.grassland * is.declining + # grass X declining
      regime * year , # south or north regime X year
    random = ~ 1 | loc / aou,  
    correlation = corAR1(form = ~ 1 | loc / aou),
    data = dat,
    method = "REML"
  )

plot(M.mixed)
(M.mixed.aov <- nlme::anova.lme(M.mixed))
summary(M.mixed)
VarCorr(M.mixed)

saveTab(tab=M.mixed.aov, dir=tabDir, fn="aov-table-lme")


M.mixed <-
  nlme::lme(
    distEdge ~ 
      regime * is.declining + # declining species X whether the route underwent shift 
      regime * is.grassland + # grassland oblig. species X whether the route underwent shift
      is.grassland * is.declining + # grass X declining
      regime * year , # south or north regime X year
    random = ~ 1 | loc / aou,  
    correlation = corAR1(form = ~ 1 | loc / aou),
    data = dat,
    method = "REML"
  )


# My new model....

M.mixed2 <-
  nlme::lme(
    distEdge.scaled ~ year.int + regime*sppGroup, 
    random = ~year.int + 1|loc/aou,  
    correlation = corAR1(form = ~ 1 | loc / aou ),
    data = dat,
    method = "REML")

## Does intercept approximately equal the mean Y?
M.mixed2$coefficients$fixed[1]
mean(dat$distEdge)

plot(M.mixed2)
(M.mixed2.aov <- nlme::anova.lme(M.mixed2))
summary(M.mixed2)

coef.pa.time.nlme <- coef(pa.na.time.nlme)
round(coef.pa.time.nlme, 2)


m.randint <- nlme::lme(fixed = distEdge.scaled ~ 
               regime * sppGroup + year.int, 
             random = ~ 1 | loc/aou, 
             correlation = corAR1(form = ~ 1 | loc / aou ),
             data = dat)
m.randslope <- update(m.randint, random = ~ year|loc/aou)

  ## Yes..


# Using LME4 lmer ---------------------------------------------------------


lmer.full <- lme4::lmer(distEdge ~ 
                          regime * sppGroup + 
                          (1 + regime*sppGroup | loc) + 
                          (1 + year            | loc) + 
                          (1 + year            | aou), 
                        dat3)

nlme.full <- nlme::lme(data   = distEdge, 
                       fixed  =     , 
                       random =  
                       )


# GEEE GLm model -------------------------------------------
# install.packages("geepack")
library("geepack")

df <- dat3 %>% mutate(id = paste0(loc,"_" ,aou))
mf <- formula(distEdge ~ year + regime + sppGroup)
geeInd <- geeglm(mf, id=id, data=df, family=beta, corstr="ind")
summary(geeInd)


##########################################################################
#### Interpret Model 
##########################################################################

# Check residuals
E2 <- resid(M.mixed, type = "normalized")
F2 <- fitted(M.mixed)
op <- par(mfrow = c(2, 2))
plot(F2, E2)
boxplot(E2 ~ regime,
        main = "regime",
        ylab = "residuals",
        data = dat)
boxplot(E2 ~ is.declining,
        main = "is.declining",
        ylab = "residuals",
        data = dat)
boxplot(E2 ~ is.grassland,
        main = "is.grassland",
        ylab = "residuals",
        data = dat)
par(op)
# MakePredictions/visualize effects -------------------------------------------
newdat <- expand.grid(regime=unique(dat$regime),
                      is.grassland=unique(dat$is.grassland),
                      is.declining=unique(dat$is.declining),
                      year=unique(dat$year), 
                      distEdge.scaled=0)



p <- ggplot(dat, aes(x=regime, y=distEdge.scaled, colour=year)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(M.mixed), group=loc, size="locs")) +
  geom_line(data=newdat, aes(y=predict(M.mixed, level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("locs"=0.5, "Text"=3)) +
  theme_bw(base_size=22) 
p
