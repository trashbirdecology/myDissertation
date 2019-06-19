# Init -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


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


# 1. Load anlaysis data ------------------------------------------------------
dat <-
  readRDS(here::here(
    "/chapterFiles/discontinuityAnalysis/results/anovaData.rds"
  )) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(regimeShift = as.factor(regimeShift)) %>% 
mutate(regime = as.factor(regime))

glimpse(dat)

# Reorder factor levels for easy interp
levels(dat$regimeShift)
dat$regimeShift = factor(dat$regimeShift,levels(dat$regimeShift)[c(3,2,1)])

if(levels(dat$regime)[1]=="North") dat$regime = factor(dat$regime,levels(dat$regime)[c(2,1)])


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
## 0.766 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
# Kurtosis
PerformanceAnalytics::kurtosis(dat$distEdge.scaled)
## 0.117 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....

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

# 6. Check for normality beyond QQ plots and Boxplots ------------------------
# Skewness
PerformanceAnalytics::skewness(dat$distEdge.scaled)
## 0.766 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
PerformanceAnalytics::kurtosis(dat$distEdge.scaled)
## 0.117 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....


# 7. Visualize dist to edge  --------------------------------------------------------------
ggplot(dat) +
  geom_boxplot(aes(x = regimeShift, y = distEdge.scaled)) +
  facet_wrap( ~ year)

ggplot(dat) +
  geom_boxplot(aes(x = regime, y = distEdge.scaled)) +
  facet_wrap(year ~ is.grassland, ncol = 2)


ggplot(dat) +
  geom_boxplot(aes(x = regime, y = distEdge.scaled)) +
  facet_wrap(year ~ is.declining, ncol = 2)

ggplot(dat) +
  geom_boxplot(aes(x = regime, y = distEdge.scaled)) +
  facet_wrap(year ~ nAggs, ncol = 7)

ggplot(dat) +
  geom_boxplot(aes(x = is.grassland, y = distEdge.scaled)) +
  facet_wrap(year ~ regime, ncol = 2)

ggplot(dat) +
  geom_boxplot(aes(x = is.declining, y = distEdge.scaled)) +
  facet_wrap(year ~ regime, ncol = 2)

ggplot(dat) +
  geom_boxplot(aes(x = is.grassDeclining, y = distEdge.scaled)) +
  facet_wrap(year ~ regime, ncol = 2)

# 8. Correlation of nAggs with distEdge --------------------------------------
ggplot(dat) +
  geom_point(aes(x = nAggs, y = distEdge.scaled)) +
  facet_wrap(year ~ regime, ncol = 2)


# 9. Interaction plots -------------------------------------------------------
interaction.plot(response = dat$distEdge.scaled,
                 dat$regime,
                 dat$year)
interaction.plot(response = dat$distEdge.scaled,
                 dat$year,
                 dat$regime)
interaction.plot(response = dat$distEdge.scaled,
                 dat$year,
                 dat$regimeShift)
interaction.plot(response = dat$distEdge,
                 dat$regime,
                 dat$is.grassland)
interaction.plot(response = dat$distEdge.scaled,
                 dat$regime,
                 dat$is.declining)


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
# 11. Ensure factors and best levels ----------------------------------------------
levels(dat$regime)
levels(dat$is.declining)
levels(dat$is.grassland)
levels(dat$is.grassDeclining)
# ensure year is a factor
str(dat$year)

# 12. Run ANOVA ---------------------------------------------------------------
library(nlme)
# Set contrasts
options(contrasts = c("contr.sum", "contr.poly")) ## type 3

# arrange data frame prior to analysis to account for year as a factor and as a rep. measure
dat <- dat %>% 
  arrange(loc, aou, year) 

M.mixed <-
  nlme::lme(
    distEdge.scaled ~ 
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

saveTab(tab=M.mixed.aov, dir=tabDir, fn="aov-table-lme")

VarCorr(M.mixed)

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
